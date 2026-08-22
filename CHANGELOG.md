# Changelog

All notable changes are documented here. The project follows Semantic Versioning
for its CLI, TOML schema, and JSON schema.

## [Unreleased]

## [0.1.0] - 2026-08-20

### Fixed

- `ocaml-mutants -- <command>` works: the `--` command tail now reaches the
  implicit default `run` invocation instead of failing to parse, matching the
  documented behavior. The implicit form is options-only — cmdliner's group
  dispatch reads a first positional as a subcommand name, so a workspace path
  still requires the explicit `run`. Other subcommands keep `--` as an
  ordinary end-of-options token, and a bare trailing `--` stays a usage error.
  The cmdliner lower bound rises to 2.0, whose exact-name subcommand dispatch
  the argv pre-split mirrors.
- The mutation score no longer counts unconfirmed timeouts as detected.
  `summary.detected` is now `killed + timeout - unconfirmed_timeouts`, with the
  new `unconfirmed_timeouts` counter recorded in the summary and surfaced in
  the terminal totals. Unconfirmed timeouts exist only in interrupted runs,
  where cancellation skips the serial confirmation retry; completed runs are
  unaffected. Pre-release `run-report-v1` schema change: previously stored
  local reports no longer decode (fail-closed); clear them with
  `ocaml-mutants cache clean`.
- The first cross-OS, cross-compiler CI run in the project's history found and
  fixed four latent problems: OCaml 5.4 never compiled
  (`Shape.Uid.Local_opaque_item` is 5.5-only; uid ownership now goes through
  a version-selected `Shape_uid_compat` module), macOS never compiled
  (`_POSIX_C_SOURCE` hides Darwin's `st_mtimespec`; the stubs opt back in via
  `_DARWIN_C_SOURCE`), CRLF checkouts silently changed every mutant ID
  (`.gitattributes` now forbids EOL conversion so checkouts are byte-exact by
  contract), and the fmt gate needed the pinned ocamlformat installed in CI.
- The `init` starter file no longer narrows `mutation.operators` to three
  families: it now leaves the key omitted (every family enabled, matching the
  built-in defaults) with a commented example that lists all nine names.
- `--mutant` is no longer rejected when the configuration narrows
  `mutation.operators`. Explicit IDs already resolve against the complete
  catalog, exactly as they do for rules outside the selected profile.
- `report` and `cache stats|gc|clean` now load the workspace configuration and
  resolve the same store as `run`, so a configured `cache.directory` applies to
  every subcommand. Both accept `--path PATH` (default `.`) to name the
  workspace. As a consequence, a malformed `.ocaml-mutants.toml` is now a usage
  error for these commands instead of being silently ignored.
- Alias-based test stages actually rerun per mutant: every
  `@dogfood-fast`/`@dogfood-full` rule now declares
  `(deps (env_var OCAML_MUTANTS_ACTIVE))`. Dune does not track environment
  variables unless a rule declares them, so after the first successful mutant
  an alias build was satisfied from cache and the fast stage silently became a
  no-op — every mutant fell through to the slow `--force` stage. Detection
  stayed correct (the full stage reruns everything), but the staged speedup
  was lost. The `test.stages` documentation now states this requirement for
  user projects.

### Changed

- The opam metadata is generated from `dune-project`
  (`generate_opam_files true`), ending the hand-maintained duplication that
  had already let the two descriptions drift. The maintainer and author
  fields carry a real contact address, `x-maintenance-intent` declares
  `(latest)`, and the unused `ocaml-compiler-libs` dependency is dropped —
  the libraries use the compiler-shipped `compiler-libs.common`, which is not
  that opam package.
- `dune runtest` is pure OCaml: the Python-driven schema-validation and
  dogfood-verifier rules moved off the `runtest` alias onto the new
  `@schema-validation` alias (and the existing `@dogfood-fast`/`@dogfood-full`
  copies). CI and the release gates build `@schema-validation` explicitly, so
  coverage is unchanged while the opam with-test build no longer needs Python
  — the opam-repository sandbox provides none.
- The `balanced`, `strong`, and `all` profiles are now real, monotonically
  inclusive tiers instead of aliases for one rule set: `balanced` (default)
  drops `if-branch` and `sequence-deletion`, `strong` adds `if-branch`, and
  `all` adds `sequence-deletion`. Mutant IDs are profile-independent and
  unchanged; only the default catalog membership shrinks (the two noisiest
  families move to the opt-in tiers). Explicit `--mutant` selection still
  resolves against the complete catalog. `rule.abi` bumps 4 → 5 because rule
  metadata changed, invalidating prior cache keys.
- The migration-era shadow oracle is removed from the analysis path: the typed
  existential `Operator.Spec` registry is now the single production writer,
  ending the double generation and parity comparison of every candidate. The
  committed objects were already the Spec-materialized ones, so catalogs, IDs,
  ordering, and the skip ledger are byte-for-byte unchanged (verified across
  all fixtures); `rule.abi` and `instrumentation.abi` are unbumped. Inputs
  whose typed evidence does not own their source bytes now skip as imprecise
  mappings instead of failing the whole analysis. The parity suites retire
  with the oracle; their semantic guarantees move to Spec-level registry
  contracts and serialized-CMT discovery contracts.

### Added

- The directory-capability store is native on Linux and macOS: the six
  deliberately fail-closed POSIX primitives close. Directory creation commits
  with `mkdirat(0700)` and verifies kind, exact mode, effective-uid ownership,
  and same-device parentage; captured deletion is a verified-name `unlinkat`
  with post-commit release evidence. Linux no-replace publication binds the
  destination straight to the still-open source inode (`linkat` over
  `/proc/self/fd`), leaving no source name to race; where hard links are
  unavailable, on Darwin, and for replacement, publication re-verifies the
  retained capture-time component and commits with one native rename
  (`renameat2`/`renameatx_np`/`renameat`). Windows-only sharing guarantees are now documented per-OS: POSIX
  capabilities pin inodes, not names, and a same-effective-uid namespace
  writer stays outside hard guarantees on both platforms. The full test
  suite — store contracts, fixture E2E, CLI lifecycle, and interrupt included
  — runs on Windows, Linux, and macOS in CI, and the byte-exact
  preprocessor reverse-mapping bug that hid every `.pp.ml` mutant on Linux is
  fixed along the way.
- CI now enforces the pinned quality gates it previously only documented. A
  lint job runs typos, taplo, actionlint, and committed (via mise, so CI and
  local runs share one pinned toolchain); every matrix job runs
  `@dogfood-fast`; a `dogfood-list` job proves Balanced self-catalog
  determinism; Python is pinned through `requirements-dev.txt` everywhere; the
  installed-schema check covers all three schemas; and every action is pinned
  by commit SHA. A tag-triggered release workflow verifies the
  tag/`dune-project`/CLI version agreement, replays the gates, and drafts a
  GitHub release — publishing stays manual behind the release checklist.
  Locally, `mise run check` gains the same spelling/TOML/workflow gates, a
  commit-msg hook runs committed (subject hard cap 80, aim 72, per
  `committed.toml`), and `justfile` works on POSIX shells instead of requiring
  pwsh. Dependabot watches the pinned actions and Python requirement.
- Two operator families for OCaml's pattern-matching core, both in the
  Balanced tier. `match-arm` (8 rules, `match-arm-unit@1` …
  `match-arm-none@1`) replaces every match/try arm RHS with the neutral value
  of its typed result, sharing the return-replacement evidence; arms never
  disappear, so exhaustiveness and pattern-binding warnings cannot fire.
  `constructor-replacement` (`some-to-none@1`, `cons-to-nil@1`) swaps typed
  Stdlib `Some`/cons applications for `None`/`[]`. Both sit at the tail of the
  deduplication precedence, so every pre-existing exact-edit winner keeps its
  ID; `rule.abi` is unchanged. The registry grows from 30 to 40 rules, and the
  new `fixtures/match` E2E pins the combined catalog and proves the
  instrumented tree compiles under the fatal-warning dev profile.
- A derived mutation score in the terminal summary and the native run report:
  `summary.detected` (kills plus serially confirmed timeouts) and
  `summary.score`
  (`100 × detected / (detected + unexpected_survivors)`, `null` when the
  denominator is zero). Expected survivors and infrastructure results stay out
  of the denominator, so a score of 100 coincides with exit code 0. The score
  is diagnostic only; no threshold policy is attached to it. Pre-release
  `run-report-v1` schema change: previously stored local reports no longer
  decode (fail-closed); clear them with `ocaml-mutants cache clean`.
- Type-aware mutation discovery from Dune-produced Typedtrees.
- Isolated workspace snapshots and single-pass interval-tree instrumentation.
- Domain worker scheduler with POSIX process groups and Windows Job Objects.
- Baseline verification before and after instrumentation.
- Versioned strict TOML configuration, native run/catalog JSON schemas, and a
  one-way Mutation Testing Report Schema v2 projection validated against the
  pinned official v2.0.5 schema and a stricter local emitted-surface schema.
- Staged baseline/test execution, reasoned survivor expectations, full-ID
  selection, partial baseline evidence, and serial timeout confirmation with
  both attempts retained.
- Canonical Csexp SHA-256 mutation IDs and proof-framed cache ABI v3.
- Explicit `--cache-mode auto|on|off` policy override and a resumable external
  stage-0 Balanced dogfood gate with native-schema and workspace-manifest proof.
- Bounded head/tail process output and fail-closed POSIX process-group/Windows
  Job ownership.
- A process-lifetime interrupt router with total in-memory per-command
  deactivation, cancellation kept active through atomic report publication,
  and an explicit pre-publication report-decision linearization point.
- Cleanup-before-publication run transactions, maintenance/publication leases,
  and non-authoritative post-commit advisories.
- Structural timestamp/PID/monotonic-sequence run reservation IDs with
  exclusive-create collision authority, full ownership proofs, and no fixed
  retry limit.
- A structural complete/partial native report witness and strict decoding of
  summary, failure, expectation, baseline, retry, capture, and mutant identity
  evidence.
- A directory-capability contract and deterministic namespace-swap race suite,
  with Windows-native single-leaf cache-root materialization, owner-private
  directory/file creation, exact v2 ownership bootstrap, root-relative locking,
  captured stage and reservation-marker deletion, atomic no-replace report
  publication, and atomic latest-index replacement. POSIX missing-root and
  directory creation, publication and captured deletion, plus complete native
  recursive cache/snapshot cleanup authority remain pre-release gates.
- An authoritative typed existential `Operator.Spec` production writer for all
  40 versioned rules, validated at every typed visit site against the exact
  source bytes it rewrites.
- `run`, `list`, `report`, `doctor`, `init`, and cache maintenance commands.
- Windows, Linux, and macOS local development and dogfood tasks, with the
  complete test suite green on every CI platform row.

[Unreleased]: https://github.com/P4suta/ocaml-mutants/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/P4suta/ocaml-mutants/releases/tag/v0.1.0
