# End-to-end fixtures

These workspaces exercise behavior at the process/Dune boundary. Release checks
copy each directory before invoking `ocaml-mutants`; a test must compare all
source-workspace file digests and `git status --porcelain=v1` before and after.

- `basic`: killed and survived arithmetic, comparison, condition, and boolean
  mutations in a library.
- `baseline-failure`: tests fail before discovery, so no mutation may run.
- `timeout`: an activated mutation leaves a descendant alive unless supervision
  terminates the complete process tree.
- `custom-command`: uses `.ocaml-mutants.toml` argv and sequential execution.
- `ppx`: proves byte-exact preprocessing can be reverse-mapped to the
  original source.
- `ppx-imprecise`: proves transformed PPX output is explained and skipped
  unless its locations can be reverse-mapped byte-for-byte.
- `private-library`: reaches code through a private Dune library.
- `executable`: mutates implementation code owned by an executable stanza.
- `root-test`: proves a root-level test stanza does not classify neighboring
  production modules as tests.

Inline-test fixtures require the corresponding PPX packages in their own opam
test switch and are kept out of this repository's normal `dune runtest` graph.
