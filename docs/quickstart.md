# Quick start

Run `ocaml-mutants doctor` at a Dune workspace root, then run
`ocaml-mutants config init` to inspect the detected v2 defaults. Add `--write`
only when you want to create `.ocaml-mutants.toml`.

`ocaml-mutants run` stores a complete measurement and returns 0 even when a
mutant survives. Use `ocaml-mutants check` as the CI policy gate. The latest
stored run is selected by default; every command also accepts an explicit run
ID where applicable.

The default driver inventories Dune tests, tries known covering
`@DIR/runtest-NAME` aliases first, and retains a forced global `@runtest` proof
for strict survivors. Custom Dune actions remain aliases. A custom command is
passed after `--` and is executed as an argv vector; it is never parsed by a
shell. Custom commands are serial unless `parallel_safe = true` is declared.

With an interactive stdin/stdout, bare `ocaml-mutants` opens the live UI. Press
`r` to start and `q`, Escape, or Ctrl-C to cancel an active run. In redirected
or CI use, invoke `run` or `check` explicitly.

For machine-readable progress use `run --events jsonl`. For an offline report
use `report --format html --output report.html`.
