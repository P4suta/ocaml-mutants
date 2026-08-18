# ocaml-mutants 0.1.0 pre-release notes

This pre-release develops an isolated mutation testing workflow for Dune
projects. The tool builds the project normally, derives candidates from compiler
Typedtrees, instruments a disposable snapshot once, and uses isolated Dune build
directories while testing each mutation.

The target platform matrix is Windows, Linux, and macOS with OCaml 5.4–5.5 and
Dune 3.21–3.x. Component, contract, and fixture gates have local Windows
evidence; the complete Balanced self-run, pinned corpus, remaining
directory-capability migration, and Linux/macOS safety checks are outstanding.
The tree must not be described as production-ready yet. The candidate includes
strict TOML version 1, native run-report-v1 and catalog-v1 schemas, a Mutation
Testing Report Schema v2 projection checked against the pinned official v2.0.5
schema and a stricter local emitted-surface schema, full SHA-256 stable IDs with
20-character display prefixes, proof-gated caching, changed-file selection,
timeout accounting, descendant process cleanup, a process-lifetime interrupt
subscription that remains active through report publication, and an
authoritative 30-rule `Operator.Spec` writer guarded by exact
compatibility-oracle parity.

On Windows, cache-root bootstrap, ownership establishment, run-directory and
reservation-marker creation, marker cleanup, and report publication use
retained directory/file capabilities. Outcome-cache I/O, GC/clean traversal and
deletion, and `Workspace_snapshot` traversal and cleanup still require migration
before release.

The intended public stability promise covers the CLI, `.ocaml-mutants.toml`,
both native JSON schemas, and the ecosystem report projection. OCaml modules
and the mutation implementation are not installed as a library API in 0.1.

Before publishing, maintainers must complete every item in
`docs/release-checklist.md`. This repository intentionally contains no pushed
tag, opam-repository submission, or announcement.
