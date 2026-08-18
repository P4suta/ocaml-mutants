# ocaml-mutants

[![CI](https://github.com/P4suta/ocaml-mutants/actions/workflows/ci.yml/badge.svg)](https://github.com/P4suta/ocaml-mutants/actions/workflows/ci.yml)

`ocaml-mutants` is a type-aware mutation testing CLI for modern Dune projects.
Run it at the root of a project: it performs a normal Dune build, reads compiler
Typedtrees, creates an isolated source snapshot, instruments all selected
mutations once, and switches the active mutation for each test process. The
source workspace and its Dune files are never modified.

This repository is the pre-release `0.1.0` working tree. The CLI, TOML v1, the native
`run-report-v1` and `catalog-v1` JSON schemas, and the Mutation Testing Report
Schema v2 projection are the public compatibility surfaces for 0.1. OCaml
library modules are not installed.

CI builds the tree on Windows, Linux, and macOS across OCaml 5.4 and 5.5,
runs the complete store-backed test suites on Windows, and replays the
Balanced self-catalog determinism gate on Linux. The POSIX
`Run_store`/`Dir_cap` migration is in progress, so the store-backed suites do
not run on Linux/macOS yet; the complete Balanced self-run and the pinned
corpus gate are also still outstanding. This tree is therefore not
production-ready yet.

The engine is deliberately OCaml-native: mutation discovery, rendering, Dune
integration, execution, and safety are built around OCaml compiler evidence.
Interoperability with the wider mutation-testing ecosystem belongs at the
report boundary. `--stryker-json` emits a deterministic Mutation Testing Report
Schema v2 projection for Stryker-ecosystem report consumers; it does not make
the CLI, config, or mutation engine Stryker-compatible. The stored native
`run-report-v1` document remains the authoritative record of complete modeled
engine state; stdout and stderr captures are intentionally bounded.

## Target requirements

- Windows, Linux, or macOS
- OCaml 5.4 or 5.5
- Dune 3.21 or newer (but below Dune 4)
- opam 2.2 or newer

Windows development uses the official native opam distribution. This repository
pins opam through `mise.toml`, while opam creates a project-local `_opam` switch.
No OCaml runtime is added to the dotfiles-managed global mise tool list.
Development and release gates additionally use the repository-pinned Python
3.13.7 and `jsonschema==4.26.0`; Python is not a runtime dependency of the
installed CLI. `mise run bootstrap` installs the pinned Python requirement from
`requirements-dev.txt`.

```console
mise trust
mise install
mise run bootstrap
mise run check
mise run dogfood-list
mise run dogfood
```

`dogfood-list` proves that two external-stage-0 Balanced catalogs are bytewise
canonical and ID-order deterministic. `dogfood` runs the complete Balanced
self-test with explicit resumable caching, independently verifies the native
report against its public schema and acceptance counts, and proves that every
source-workspace entry outside the generated `_build` and `_opam` roots is
unchanged.

The equivalent portable setup is:

```console
python -m pip install --requirement requirements-dev.txt
opam switch create . ocaml-base-compiler.5.5.0 --no-install
opam install . --deps-only --with-test --with-doc
opam exec -- dune build @all
opam exec -- dune runtest
```

## Quick start

This pre-release is not on the opam repository yet: install it by pinning this
tree (`opam pin add ocaml-mutants .`). Then enter any Dune workspace and run:

```console
ocaml-mutants
```

The default test command is `dune runtest --force`. A custom command is passed
as an argv vector after `--`; it is never evaluated as a shell string.

```console
ocaml-mutants run . --jobs 4 --timeout 30 -- dune exec test/my_suite.exe -- --quick
ocaml-mutants list --operator comparison
ocaml-mutants run --profile strong
ocaml-mutants run --changed
ocaml-mutants report latest --json
ocaml-mutants run --stryker-json --threshold-high 80 --threshold-low 60 -- dune runtest --force
```

Stryker-compatible output is written only to standard output. Both score
thresholds are explicit required inputs; ocaml-mutants neither uploads the
report nor performs network publication. See the
[compatibility boundary](docs/stryker-compatibility.md) for the exact, lossy
status mapping and validation rules.

Use `ocaml-mutants init` to create `.ocaml-mutants.toml`. Configuration values
are applied in this order: built-in defaults, TOML, then CLI flags. Unknown TOML
keys are errors with file, line, and column information.

## Safety and execution model

Before mutation starts, both the analysis build and the configured baseline
tests must pass. After instrumentation, the baseline runs again with no active
mutant to verify semantic preservation. Each default Dune test worker receives
an independent build directory, and Dune's shared compilation cache is
explicitly disabled during mutation runs so artifacts cannot leak between
mutants. Custom commands run sequentially unless `test.parallel_safe = true`.

On POSIX, test commands run in their own session and the complete process group
is terminated on timeout. On Windows, a private helper waits for its parent to
complete Job Object assignment before starting the target; the Job uses
`JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE`. If Job ownership cannot be established,
execution fails closed. The first timeout is retried serially; only a second
timeout is a confirmed detection, while an unconfirmable result is
inconclusive. Ctrl-C returns exit code 130.

## Exit codes

| Code | Meaning |
| ---: | --- |
| 0 | No unexpected survivor, inconclusive result, or expectation failure |
| 1 | At least one unexpected mutant survived |
| 2 | Infrastructure failure, inconclusive result, not-run mutant, or stale/unfulfilled expectation |
| 130 | Interrupted |

The terminal summary and native JSON report also carry a mutation score:
detected mutants (kills plus confirmed timeouts) over detected plus unexpected
survivors, so 100% coincides with exit code 0. Declared expected survivors and
infrastructure results are excluded from the denominator. The engine reports
the score as a diagnostic and applies no threshold policy to it.

## Documentation

- [Architecture](docs/architecture.md)
- [Operators](docs/operators.md)
- [Configuration](docs/configuration.md)
- [JSON report schema](docs/json-schema.md)
- [Stryker report ecosystem compatibility](docs/stryker-compatibility.md)
- [Pinned local corpus gate](docs/pinned-corpus.md)
- [Comparison with Mutaml](docs/comparison-with-mutaml.md)
- [Contributing](CONTRIBUTING.md)
- [0.1 release notes](RELEASE_NOTES.md)

## License

Licensed under either the MIT License or Apache License 2.0, at your option.
