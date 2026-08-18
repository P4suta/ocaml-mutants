# Changelog

All notable changes are documented here. The project follows Semantic Versioning
for its CLI, TOML schema, and JSON schema.

## [Unreleased]

### Fixed

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

### Added

- A derived mutation score in the terminal summary and the native run report:
  `summary.detected` (`killed + timeout`) and `summary.score`
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
  30 versioned rules, with a separately materialized compatibility oracle that
  proves exact rule, range, source, replacement, digest, and full-ID parity.
- `run`, `list`, `report`, `doctor`, `init`, and cache maintenance commands.
- Windows-native local development and dogfood tasks; Linux and macOS remain
  target platforms pending cross-OS acceptance evidence.
