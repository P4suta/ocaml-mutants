# Architecture

## Invariants

Four invariants shape the implementation:

1. The source workspace is read-only. Every build, instrumentation edit, and
   test happens in a disposable snapshot that excludes `.git`, `_build`,
   `_opam`, caches, and dependency trees.
2. Candidate generation begins with a compiler Typedtree. A mutation is emitted
   only for a non-ghost typed expression whose source location maps exactly to
   byte offsets in an implementation file reported by Dune.
3. Instrumentation happens once. Every mutant in a file is composed into a
   nested interval tree, and `OCAML_MUTANTS_ACTIVE` selects one replacement at
   runtime. The target project needs no runtime library or Dune stanza.
4. Only validated mutants execute, cache hits are revalidated against the
   original source digest and identity inputs, and every fatal error carries a
   closed phase/cause classification with cleanup failures preserved.

## Boundaries

`Ocaml_mutants_core` is private and pure. It owns
source ranges, stable mutant identities, operators, outcomes, catalog summaries,
exit policy, and interval-tree instrumentation.

`Ocaml_mutants_engine` is also private and owns effects behind narrow modules:

- `Dune_adapter` is the workspace boundary. It uses the `csexp` library and a
  typed decoder for `dune describe workspace --format csexp --lang 0.1`;
  experimental test descriptions stay isolated from this stable decoder. The
  resolved Dune driver sorts individual `@DIR/runtest-NAME` aliases and retains
  `@runtest` as the exhaustive proof for unclassified/custom rules.
- `Ocaml_frontend` reads `.cmt` files with compiler-libs. It validates resolved
  Stdlib identifiers for operator mutations, typed branch compatibility supplied
  by the compiler, non-ghost locations, and exact source slices. The typed
  existential `Operator.Spec` registry is the single production writer for all
  40 rules: each typed visit site evaluates the registry once and commits the
  candidates validated against the exact source bytes, with no second
  generation path.
- `Workspace_snapshot` derives copying and fingerprinting from one sorted
  manifest and rejects escaping links, junctions, and special files. Its
  cleanup is a release-critical adversarial race boundary. A supported release
  must use the shared native directory-capability substrate and may not ship a
  path-based fallback.
- `Process_supervisor` uses `posix_spawn` to enter a single-threaded helper that
  establishes a POSIX process group before releasing the target. On Windows the
  helper is created in a console process group distinct from the CLI, and the
  same handshake assigns it to a kill-on-close Job Object before target
  execution. A targeted `CTRL_BREAK_EVENT` can therefore reach the CLI without
  directly racing the supervised test tree. The pinned MinGW OCaml runtime's
  `Sys.signal` path delegates to the CRT `signal()` entry point, while its
  console-control implementation is a separate, unreferenced runtime symbol;
  an unhandled targeted break consequently exits with
  `STATUS_CONTROL_C_EXIT`. The engine installs a last-registered native console
  handler that only atomically latches a process-lifetime event. An ordinary
  OCaml worker drains and rearms that event, then dispatches to the currently
  active in-memory subscription; the Windows console-dispatch thread never
  calls OCaml or enters the runtime. POSIX installs the equivalent runtime
  handlers once. Per-command subscription deactivation is total and the OS
  router remains installed, so `Application.Make` keeps cancellation active
  through atomic report publication without a restore/publish interrupt gap.
  A cancellation observed immediately before `commit_reserved` becomes the
  authoritative interrupted resolution. That check is the report-decision
  linearization point; a later interrupt is claimed safely but cannot rewrite
  a report whose atomic publication may already have committed.
  The supervised tree then stops through cancellation and Job ownership. The
  real interrupt lifecycle contract distinguishes exit 130
  from `STATUS_CONTROL_C_EXIT` explicitly and skips Windows only when native Job
  assignment or targeted console delivery returns a proved
  unsupported-environment error. Other native failures fail the contract. If
  Job ownership cannot be established, execution fails closed; there is no
  process-enumeration, PowerShell, or `taskkill` fallback.
  Because the helper is the CLI executable itself, a one-shot internal
  environment marker disables mutation activation and hit recording during
  helper initialization. The marker is removed before the real target starts,
  and mutant attempts remove inherited readiness hit files because coverage is
  collected only by the dedicated readiness pass. Each runtime also embeds a
  deterministic catalog ownership token and records hits only when the owner of
  the active hit file matches. These boundaries prevent doubly nested mutation
  runs from mixing outer and inner catalog IDs.
- `Application.Make` owns one cancellation token and deterministic
  process-lifetime signal subscriptions. The initial OS-router installation can
  fail before work begins; after subscription, deactivation is an in-memory,
  no-fail handoff performed only after report commit. The live TUI supplies a
  caller-owned token through the same reservation/snapshot/commit lifecycle and
  translates Matrix keys into `Cancel.request` without installing a competing
  signal subscription. Privacy-bounded settled results travel on the callback
  sink for immediate evidence/diff display while JSONL retains its compact v1
  projection. A streaming monochrome adapter removes only ordinary SGR output,
  retaining Matrix terminal protocols for `NO_COLOR`. `Runner` composes these
  private services; no OCaml service or operator ABI is public in 1.0.
- `Run_store` namespaces reports by canonical workspace identity and writes
  outcomes and reports to the OS cache directory. The shared
  directory-capability contract and in-memory namespace-swap suite define:
  single-leaf cache-root materialization,
  owner-private directory/file creation, exact v2 ownership establishment,
  root-relative shared/exclusive locks, captured reads, exact stage and
  reservation-marker deletion, no-replace report publication, atomic
  latest-index replacement. Implemented primitives bind names through live
  handles and sharing exclusion on Windows; POSIX pins inodes, retains
  capture-time components, and
  re-verifies the name-to-inode binding at each commit (`mkdirat` creation,
  `renameat2`/`renameatx_np` no-replace publication, verified-name `unlinkat`
  deletion with link-count release evidence). Any remaining `Unsupported` in a
  required primitive is a release hold, never a path-based fallback. Several
  required native operations still return `Unsupported`, so this gate is not
  yet closed.
  Outcome-cache writes, GC/clean, journal cleanup, and recursive cache and
  snapshot cleanup still use guarded path operations; migrating them to the
  same captured capability substrate is therefore an explicit 1.0 release
  hold.

## Historical outcome reuse

Run journals and checkpoints are always enabled and are not historical cache
reuse. Historical reuse defaults to `off`; `exact` requires the complete input
fingerprint, while `estimated` is visibly marked and rejected by the default
policy. Fingerprint failure disables reuse for that run. The fingerprint covers
the workspace, resolved configuration, complete environment, executable,
OCaml/Dune versions, opam dependency closure, catalog IDs, declared external
inputs, resolved stage executables, compiled PPX artifacts, and test binaries.
Dune uses a private `enabled-except-user-rules` compiler-artifact cache to supply
isolated worker build directories without sharing user test rules. Cross-OS and
adversarial validation of that boundary remains a release gate.

## Pipeline

```text
Dune describe -> snapshot -> analysis build -> baseline tests
                                      |
                                      v
                          .cmt Typedtree discovery
                                      |
                                      v
                    interval-tree instrumentation
                                      |
                                      v
                         instrumented baseline
                                      |
                                      v
                  isolated worker build directories
                                      |
                                      v
                         cache + terminal/JSON report
```

The analysis `.cmt` files are created before source instrumentation so candidate
locations always describe the original source. Instrumented builds use separate
Dune build directories. Snapshot deletion is attempted after success or failure
and before authoritative report publication. Cleanup failures remain in the
reportable resolution and may leave an auditable residual; only after clean
teardown are the compact persisted run/cache artifacts expected to remain.

## Stable identity

A mutant ID is the full SHA-256 over a length-prefixed canonical Csexp containing
the normalized relative path, concrete rule and version, source byte range,
source digest, and original/replacement digests. The CLI displays a 20-hex
prefix; the catalog rejects a prefix collision instead of silently overwriting.
Absolute paths and snapshot locations do not participate.

## Compatibility and reporting boundary

Compiler-libs is deliberately private to the executable implementation because
Typedtree APIs can change between OCaml releases. Windows, Linux, and macOS with
OCaml 5.4–5.5 are the target matrix. Local Windows component, fixture, schema,
catalog-determinism, and live event-to-TUI cancellation gates are evidenced;
the complete Balanced self-run, pinned corpus, directory-capability migration,
three-OS PTY/ConPTY validation, and cross-OS gates remain release gates. No
dynamic operator ABI is offered in 1.0.

The engine boundary is OCaml-native. Typedtree witnesses, resolved identifiers,
Dune workspace evidence, mutation rendering, process supervision, and cache
proofs are not abstractions shared with another language implementation.
Language-independent interoperability is instead provided at the report
boundary.

`ocaml-mutants.run-report-v2` is the authoritative record of the complete
modeled engine state for a run; process stdout and stderr remain intentionally
bounded captures. It drives exit policy, expectation checks, diagnostics,
replay, and cache decisions. A Stryker Mutation Testing Report Schema v2
document is a deterministic, one-way projection for compatible report viewers
and consumers. It is emitted locally on standard output by
`run --stryker-json` after the native run report has been stored. It does not
replace the native report and is never read back as cache or resume state.

The compatibility gate verifies the vendored official v2.0.5 schema bytes and
provenance, then validates both the checked-in projection fixture and real CLI
output against that schema and the stricter local emitted-surface schema.

Projection is a checked reporting step, not a second source of truth. Source
files are read once per file and must match both the digest and original source
slices recorded by the native ledger. Stored line and byte-column coordinates
must also match positions derived from the recorded byte offsets. Invalid spans,
globally duplicate mutant IDs, inconsistent coordinates, or unavailable or
changed source abort projection instead of emitting a report that appears
authoritative. Files and mutants are ordered deterministically. The required
high and low score thresholds are supplied explicitly, so the engine does not
invent reporting policy.

This compatibility target covers the shared report schema only. It does not
promise compatibility with a Stryker CLI, configuration file, plugin API,
mutation kernel, operator implementation, or execution protocol. See
[Stryker report ecosystem compatibility](stryker-compatibility.md) for the
detailed boundary.
