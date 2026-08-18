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
  experimental test descriptions stay isolated from this stable decoder.
- `Ocaml_frontend` reads `.cmt` files with compiler-libs. It validates resolved
  Stdlib identifiers for operator mutations, typed branch compatibility supplied
  by the compiler, non-ghost locations, and exact source slices. The typed
  existential `Operator.Spec` registry is the single production writer for all
  40 rules: each typed visit site evaluates the registry once and commits the
  candidates validated against the exact source bytes, with no second
  generation path.
- `Workspace_snapshot` derives copying and fingerprinting from one sorted
  manifest and rejects escaping links, junctions, and special files. Its
  current path-based cleanup is not yet the final adversarial race boundary;
  migration to the shared native directory-capability substrate is a
  pre-release gate.
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
- `Application.Make` owns one cancellation token and deterministic
  process-lifetime signal subscriptions. The initial OS-router installation can
  fail before work begins; after subscription, deactivation is an in-memory,
  no-fail handoff performed only after report commit. `Runner` currently
  composes the concrete workspace, Dune, Git, process, and store modules; the
  full service-algebra migration is a pre-release acceptance item rather than a
  documented capability.
- `Run_store` namespaces reports by canonical workspace identity and writes
  outcomes and reports to the OS cache directory. The shared
  directory-capability contract and in-memory namespace-swap suite are backed
  natively on every platform: single-leaf cache-root materialization,
  owner-private directory/file creation, exact v2 ownership establishment,
  root-relative shared/exclusive locks, captured reads, exact stage and
  reservation-marker deletion, no-replace report publication, and atomic
  latest-index replacement. Windows binds names through live handles and
  sharing exclusion; POSIX pins inodes, retains capture-time components, and
  re-verifies the name-to-inode binding at each commit (`mkdirat` creation,
  `renameat2`/`renameatx_np` no-replace publication, verified-name `unlinkat`
  deletion with link-count release evidence). Mutant outcome-cache I/O and
  GC/clean traversal and deletion remain path-based, and production recursive
  deletion authority with mount-boundary proof plus the complete
  `Workspace_snapshot` migration are still pre-release gates. Automatic
  outcome caching remains disabled until both that authority boundary and the
  complete input proof are closed.

## Remaining requirements for automatic outcome caching

`cache.mode = "auto"` stays proof-gated off. The cache key is already
conservative — it includes the complete environment, the toolchain versions,
the tool's own executable digest, the workspace digest, every catalog ID, and
the rule/instrumentation/cache ABIs — so the open items are authority and
input-proof gaps, not key strength:

1. Authority boundary. Mutant outcome I/O and the GC/clean traversal must move
   from path-based operations onto the owner-verified directory-capability
   substrate (whose primitives are now native on Windows and POSIX both), and
   `Workspace_snapshot` cleanup must complete the same migration. Until then a
   shared cache directory is not adversarially safe.
2. Complete input proof. The opam switch's dependency closure is not yet part
   of the key, so upgrading a library or PPX in place could produce a stale
   hit; and a failed executable-digest read currently degrades to a shared
   `unavailable` constant instead of disabling caching for that run
   (fail-open). Inputs that cannot be proved — network access, absolute-path
   reads, time, randomness — need an explicit user declaration equivalent to
   `test.parallel_safe` before `auto` may trust a run.
3. Semantics and acceptance. The `auto` policy needs a documented definition
   (an `on` whose save failures degrade to warnings), a per-subcommand
   read/write matrix, and green cross-OS directory-capability suites.

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
and catalog-determinism gates are evidenced; the complete Balanced self-run,
pinned corpus, directory-capability migration, and cross-OS gates remain
outstanding. No dynamic operator ABI is offered in 0.1.

The engine boundary is OCaml-native. Typedtree witnesses, resolved identifiers,
Dune workspace evidence, mutation rendering, process supervision, and cache
proofs are not abstractions shared with another language implementation.
Language-independent interoperability is instead provided at the report
boundary.

`ocaml-mutants.run-report-v1` is the authoritative record of the complete
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
