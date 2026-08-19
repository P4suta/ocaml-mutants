# ocaml-mutants 0.1.0 release notes

ocaml-mutants is a type-aware mutation testing CLI for Dune projects. The tool
builds the project normally, derives candidates from compiler Typedtrees,
instruments a disposable snapshot once, and uses isolated Dune build
directories while testing each mutation. The source workspace is never
modified.

The supported platform matrix is Windows, Linux, and macOS with OCaml 5.4–5.5
and Dune 3.21–3.x. CI runs the complete test suite — store-backed directory-
capability contracts, fixture end-to-end runs, CLI lifecycle, and interrupt
contracts included — on all six OS/compiler rows, plus a Dune-floor row on
Ubuntu and Windows. The directory-capability store is native everywhere:
Windows binds names through handles and sharing exclusion, POSIX pins inodes
and re-verifies retained capture-time names at each commit
(`renameat2`/`renameatx_np` no-replace publication, verified-name `unlinkat`
deletion with release evidence).

The release includes strict TOML version 1, native run-report-v1 and
catalog-v1 schemas, a Mutation Testing Report Schema v2 projection checked
against the pinned official v2.0.5 schema and a stricter local
emitted-surface schema, full SHA-256 stable IDs with 20-character display
prefixes, proof-gated caching, changed-file selection, timeout accounting
with serial confirmation, descendant process cleanup, a process-lifetime
interrupt subscription that remains active through report publication, and an
authoritative 40-rule `Operator.Spec` writer validated at every typed visit
site against the exact source bytes it rewrites. The complete Balanced
self-dogfood found and fixed real instrumentation defects before this release
(punned-argument and synthesized-`Some` sites are reported as unsupported
rather than mutated).

Known limitations in 0.1: `cache.mode = "auto"` stays proof-gated off until
the outcome-cache authority and input-proof gaps documented in
`docs/architecture.md` close; mutant outcome-cache I/O and GC/clean traversal
remain path-based; publication requires a filesystem with a native no-replace
rename (NFS and some FUSE filesystems are refused fail-closed); and cache
roots reached through symlinked prefixes are canonicalized at the ambient
boundary only.

The public stability promise covers the CLI, `.ocaml-mutants.toml`, both
native JSON schemas, and the ecosystem report projection. OCaml modules and
the mutation implementation are not installed as a library API in 0.1.

Before tagging, maintainers must complete every item in
`docs/release-checklist.md`.
