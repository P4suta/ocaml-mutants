# Release checklist

- [ ] Run `mise install` and `mise run bootstrap`, confirm Python is exactly
  3.13.7 and `jsonschema==4.26.0`, and run `python -m pip check`
- [ ] `opam lint ocaml-mutants.opam`
- [ ] `dune build @all`
- [ ] `dune runtest` (pure OCaml; must pass without Python on PATH, matching
  the opam-repository sandbox)
- [ ] `dune build @schema-validation`
- [ ] Verify `opam install --with-test .` inside a sandboxed Linux opam before
  submission: the sandbox mounts `$HOME` read-only, so the e2e suites must pin
  their store and snapshots to writable temporary directories
- [ ] `dune build @fmt`
- [ ] `dune build @doc`
- [ ] Confirm the CI lint job (typos, taplo over every tracked TOML file,
  actionlint over every workflow, committed over the commit range) is green
  for the release commit; `mise run check` runs the same spelling/TOML/workflow
  gates locally
- [ ] Confirm the CI `dogfood-list` determinism job and per-matrix
  `@dogfood-fast` are green for the release commit
- [ ] Build and install the opam package in a clean switch
- [ ] Confirm the installed `ocaml-mutants --version`, `dune-project` version,
  release notes, tag, and opam release version all agree exactly
- [ ] Run fixture E2E tests on Windows, Ubuntu, and macOS
- [ ] Confirm timeout and Ctrl-C leave no child or grandchild process
- [ ] Inject snapshot and reservation cleanup failures, plus signal-router
  initialization failure before work, and confirm the authoritative report is
  committed once with the same reportable failure and exit decision returned
  by the CLI
- [ ] Interrupt during report commit and prove the process-lifetime
  subscription remains active until publication completes, then deactivates
  without an OS-handler restoration step
- [ ] Confirm TTY, redirected, `CI=true`, and `NO_COLOR` output
- [x] Connect the no-argument TUI to the live event stream, start runs with a
  caller-owned cancel token, show phase/progress/warnings/settled state, and
  reload the immutable final report without changing explicit `run` output
- [x] Carry sufficient settled-result payloads to show result, diff, test,
  output, evidence, and expectation detail before final report publication
- [ ] Run keyboard, resize, Unicode, Ctrl-C, exception, and terminal-restoration
  contracts through real POSIX PTYs and Windows ConPTY at narrow and wide
  sizes; any backend failure is a release hold
- [ ] Validate real CLI output against `schema/run-report-v2.schema.json`,
  `schema/catalog-v2.schema.json`, `schema/check-report-v1.schema.json`,
  `schema/event-v1.schema.json`, `schema/shard-plan-v1.schema.json`, and
  `schema/mutation-testing-report-v2.schema.json`
- [ ] Verify the vendored upstream schema at
  `test/vendor/stryker-mutator/mutation-testing-report-schema-v2.json` remains
  pinned to tag `v2.0.5`, commit
  `8c3f6c7d34953aa2758a514728e78866e7b9c269`, and SHA-256
  `404575e9264686dbf85c399f6531fdb489bebc3146e7e1d793b8083bc6a041ae`;
  retain its exact provenance and license evidence, and validate every Stryker
  projection against both that official schema and the local exact-surface
  schema
- [ ] Generate the current repository's Balanced catalog twice through an
  external stage-0 binary and compare canonical JSON plus the ordered full-ID
  sequence
- [ ] Compare the fixed `fixtures/basic` and `fixtures/match` Balanced
  catalogs with their reviewed full-ID goldens; use these source-stable gates,
  not a cross-version self-catalog hash, to detect operator semantic drift
- [ ] Run the registry and discovery contracts for all 40 `Operator.Spec`
  rules: exactly one definition per registered rule, the pinned
  family-to-profile tier map, and the serialized-CMT discovery suite
- [ ] Run against a dirty workspace and compare every file digest and Git status
- [x] Make `test.driver = auto|dune|command` control execution, synthesize the
  Dune 3.22 per-test alias inventory for `auto`/`dune`, preserve custom actions
  as aliases, and prove covering/killer-first ordering without survivor skips
- [x] Populate worker build directories from a private compiler-artifact cache,
  hash compiled PPX/test artifacts into the fingerprint, and prove that user
  test rules and mutable build state are never shared between workers
- [ ] Verify cache reuse, corrupt-entry misses, concurrent writes, and
  source/config/toolchain/environment invalidation
- [ ] Verify cache GC and clean cannot acquire an exclusive maintenance lease
  while any process holds a live run reservation
- [ ] Run the directory-capability race suite on POSIX and Windows, proving
  cache create/write/GC/clean never follows a swapped symlink or junction
- [ ] Run native single-leaf materialization, owner-private directory/file
  creation, captured file/empty-directory deletion, shared/exclusive locking,
  and atomic no-replace/replace publication fault and contention contracts on
  Windows
- [ ] Close and exercise every production `Dir_cap.System` `Unsupported`
  required by supported run/cache paths on POSIX, including missing-root and
  owner-private directory creation, captured deletion, atomic publication, and
  recursive deletion authority with mount-boundary proof; a remaining
  `Unsupported` is a release hold for that platform
- [ ] Confirm the install contains the CLI, all versioned public schemas,
  completions, manpage, and documentation, but no installed OCaml library API
- [ ] Run `mise run dogfood`; require a complete native report with no unexpected
  survivor, inconclusive/error/not-run result, or stale/unfulfilled/unevaluated
  expectation, and an unchanged source-workspace manifest
- [ ] Run `mise run corpus-check` and `mise run corpus-unit`, then run the full
  networked `mise run corpus` gate against every manifest-pinned revision
- [ ] Review Mutaml comparison and third-party acknowledgements
- [ ] Review `CHANGELOG.md` and `RELEASE_NOTES.md`: cut the release section
  from `Unreleased` with the release date, add the link references at the
  bottom, and drop the pre-release framing from the release notes
- [ ] Create and sign the `1.0.0` tag only after approval; confirm the release
  workflow publishes three OS archives, per-platform SPDX SBOMs, SHA-256 sums,
  Sigstore bundles, and GitHub provenance attestations
- [ ] Submit to opam-repository only after approval
