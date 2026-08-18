(lang dune 3.21)
(name ocaml-mutants)
(version 0.1.0)
(generate_opam_files false)
(source (github P4suta/ocaml-mutants))
(license "MIT OR Apache-2.0")
(authors "ocaml-mutants contributors")
(maintainers "ocaml-mutants contributors")

(package
 (name ocaml-mutants)
 (synopsis "Type-aware mutation testing for Dune projects")
 (description
  "ocaml-mutants discovers mutations from compiler Typedtrees and tests them in an isolated workspace snapshot."))

