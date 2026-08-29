# ocaml-mutants 1.0

`ocaml-mutants` is a type-aware mutation-testing CLI for Dune workspaces on
Windows, Linux, and macOS. It discovers mutations from compiler Typedtrees,
instruments one isolated snapshot, executes tests under supervised process
trees, and stores an authoritative evidence report. Ordinary `run`, `check`,
and `report` operations never edit the source workspace or use the network.

The public compatibility surface is the CLI, `.ocaml-mutants.toml` version 2,
and the versioned JSON schemas. The OCaml implementation modules and custom
operator ABI are private.

## Requirements

- OCaml `>= 5.4, < 5.6`
- Dune `>= 3.22, < 4.0`
- opam 2.2 or newer
- Windows, Linux, or macOS

Mosaic and Matrix are pinned to 0.1.0 for the full-screen terminal UI. HTML
reports and the UI need neither Node.js nor network access.

## Quick start

```console
opam pin add ocaml-mutants .
cd my-dune-workspace
ocaml-mutants run
ocaml-mutants check
ocaml-mutants report --format html --output mutation-report.html
```

With no arguments, `ocaml-mutants` opens the UI only when both stdin and stdout
are interactive terminals. In redirected or CI use it exits 2 and asks for an
explicit `run` or `check`. Press `r` in the live UI to start a run; while it is
active, `q`, Escape, or Ctrl-C requests cancellation and keeps the UI open until
the interrupted report has been published. Settled mutants are immediately
browsable with their diff and evidence. `NO_COLOR` or `ui --color never` keeps
the full-screen keyboard UI monochrome; terminal report output remains the
plain-text fallback.

`run` measures and stores evidence. Survivors do not make it fail: complete
measurement exits 0, infrastructure or incomplete evidence exits 2, and an
interrupt exits 130. `check [RUN_ID]` applies policy later: pass is 0, a policy
violation on valid evidence is 1, and broken/incomplete/disallowed estimated
evidence is 2.

```console
ocaml-mutants run --events jsonl
ocaml-mutants check latest --against PREVIOUS_RUN
ocaml-mutants report latest --format terminal --format html --format sarif --output artifacts
ocaml-mutants ui latest
```

Custom commands are argv vectors, never shell strings:

```console
ocaml-mutants run -- dune exec test/my_suite.exe -- --quick
```

## Survivor workflow

```console
ocaml-mutants mutant show FULL_ID
ocaml-mutants mutant rerun FULL_ID
ocaml-mutants mutant apply FULL_ID
ocaml-mutants mutant revert FULL_ID
ocaml-mutants mutant expect FULL_ID --reason "Equivalent under documented invariant"
```

`rerun` creates a strict child run. `apply` and `revert` revalidate the full
source digest and byte range, show a diff, write atomically, and never invoke
Git. Undo material is owner-private. `expect` requires a reason and appends a
full-ID v2 TOML entry without rewriting existing comments or ordering.

## CI sharding

```console
ocaml-mutants plan --shards 4 --output plan.json
ocaml-mutants run --shard-plan plan.json --shard 0
ocaml-mutants merge plan.json RUN_0 RUN_1 RUN_2 RUN_3
ocaml-mutants check
```

Plans are deterministic and bind the complete catalog, resolved configuration,
workspace, and toolchain fingerprint. Merge rejects a missing or duplicate
shard, a foreign plan/fingerprint, and results outside the assigned IDs.

## Reports and privacy

Native `run-report-v2` is authoritative. `report` also renders terminal text,
a self-contained offline HTML file, Markdown, SARIF 2.1.0, and a lossy Stryker
v2 projection. HTML has no CDN or external scripts and includes a restrictive
CSP. Captures are bounded; source embedding and redaction are configured under
`[privacy]`. Nothing is uploaded automatically.

## Documentation

- [Quick start](docs/quickstart.md)
- [Configuration v2](docs/configuration.md)
- [TUI controls](docs/tui.md)
- [Strict and fast evidence](docs/evidence.md)
- [CI sharding](docs/ci-sharding.md)
- [v1 migration](docs/migration-v1.md)
- [Expected mutants](docs/expectations.md)
- [Privacy](docs/privacy.md)
- [Troubleshooting](docs/troubleshooting.md)
- [JSON schemas](docs/json-schema.md)
- [Architecture](docs/architecture.md)

## Development

```console
opam switch create . ocaml-base-compiler.5.5.0 --no-install
opam install . --deps-only --with-test --with-doc
opam exec -- dune build @all
opam exec -- dune runtest
opam exec -- dune build @schema-validation
```

## License

MIT or Apache-2.0, at your option.
