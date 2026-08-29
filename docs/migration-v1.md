# Migrating from v1

v1 TOML remains readable with a warning. Inspect the normalized result with
`ocaml-mutants config show`, preview a textual migration with
`ocaml-mutants config migrate`, and apply it atomically with
`ocaml-mutants config migrate --write`.

Native v1 run reports remain read-only inputs for `report`. New runs always
write `run-report-v2`; new catalogs always use `catalog-v2`. CI should validate
new artifacts against the v2 schemas and use `check` for policy rather than the
old run exit code.

The no-argument non-TTY invocation and implicit custom-command form were
removed. Use explicit `run` or `check`, and write `run -- COMMAND ...`.
