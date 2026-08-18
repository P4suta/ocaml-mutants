# Contributing

Thank you for improving `ocaml-mutants`. Changes should preserve the read-only
workspace invariant and keep compiler-version details behind `Ocaml_frontend`.

## Development setup

On the dotfiles-managed Windows environment:

```console
mise trust
mise install
mise run bootstrap
mise run check
mise run hooks
```

On other systems, create an OCaml 5.4 or 5.5 opam switch and run `opam install .
--deps-only --with-test --with-doc`.

## Expectations

- Run `dune build @all`, `dune runtest`, `dune build @fmt`, and `dune build @doc`.
- Add unit tests for domain behavior and property tests for source transforms.
- Add a fixture for process, Dune, PPX, or cache behavior.
- Never mutate an original target workspace, including dirty and untracked files.
- Treat new config and JSON fields as compatibility work and update the schemas.
- Do not add an operator without Typedtree/type evidence and a stable version.

Keep commits focused and use imperative, conventional commit messages. By
contributing, you agree that your work may be distributed under
`MIT OR Apache-2.0`.

