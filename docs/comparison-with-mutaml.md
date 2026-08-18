# Relationship to Mutaml

[Mutaml](https://github.com/jmid/mutaml) is an important prior OCaml mutation
testing project. It demonstrated a practical PPX-based workflow, helped make
mutation testing approachable in the OCaml ecosystem, and informed both the
problem vocabulary and several usability goals of this project. We are grateful
to its maintainers and contributors.

`ocaml-mutants` is a new implementation, not a fork. Its principal design choice
is to keep the target repository unchanged: it reads Typedtrees produced by a
normal Dune build, copies the workspace, instruments the copied source in one
pass, and selects mutations through an internal environment variable. The target
does not add a PPX, runtime dependency, or Dune stanza.

Other deliberate differences are:

- discovery is gated by resolved Typedtree identifiers, inferred types, non-ghost
  locations, and exact original-source byte mapping;
- all overlapping mutations are composed through an interval tree before tests;
- worker processes use independent Dune build directories and cross-platform
  descendant supervision;
- configuration, JSON, cache keys, skip reasons, and exit policy are explicit
  versioned CLI surfaces;
- Windows native opam, OCaml 5 Domains, and OCaml 5.4–5.5 are first-class support
  targets.

These are different engineering tradeoffs rather than a claim that one workflow
fits every project. Mutaml remains the relevant reference for teams that prefer
its PPX integration and established behavior.

