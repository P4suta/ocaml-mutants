# ocaml-mutants 1.0.0 release notes

These are pre-release target notes. Version 1.0 remains held until every item
in `docs/release-checklist.md`, including native directory capabilities,
three-OS PTY/ConPTY coverage, and full-screen no-color output, is complete.

Version 1.0 separates measurement from policy: `run` stores authoritative
evidence and exits 0 for any complete measurement, while `check` returns 0 for
pass, 1 for a policy violation on valid evidence, and 2 for unusable evidence.

The release introduces configuration v2; native run-report-v2 and catalog-v2;
check, event, and shard-plan schemas; deterministic CI sharding and strict
merge; an always-on checksummed crash journal; explicit executed, resume,
exact-cache, and estimated evidence; offline HTML, Markdown, SARIF, native JSON,
terminal, and Stryker reporting; and the Mosaic/Matrix full-screen UI.

The no-argument UI consumes the same live event stream as plain and JSONL
reporters, starts a run with `r`, cancels through the application token, and
shows each privacy-bounded settled result with its diff, test, output, evidence,
coverage, and expectation state before publication. `NO_COLOR` and
`ui --color never` retain full-screen keyboard behavior without SGR styling.
The UI reloads the immutable report after publication. The Dune driver inventories
per-test aliases, retains an exhaustive strict fallback, fingerprints compiled
test/PPX artifacts, and uses a private compiler cache that excludes user rules.

Nested mutation runs are isolated at both process boundaries: an instrumented
CLI cannot activate or record sites while serving as the supervisor helper, and
inner mutant attempts cannot inherit an outer readiness hit file. Each generated
runtime also embeds a deterministic catalog ownership token, so an outer
instrumented test executable cannot write its IDs into an inner readiness file.
Cache keys use instrumentation ABI 5 so older artifacts are rejected.

The survivor workflow includes `mutant show`, strict child `rerun`, guarded
atomic `apply`/`revert` with private undo data, and comment-preserving
full-ID/reason expectation edits. Ordinary commands keep the source workspace
unchanged and never invoke Git or send data over the network.

Supported toolchains are OCaml 5.4–5.5 and Dune 3.22–3.x on Windows, Linux, and
macOS. Release archives include checksums, SBOM, provenance, completions, and a
manpage in addition to the opam package.
