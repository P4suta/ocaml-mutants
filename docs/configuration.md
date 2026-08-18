# Configuration

`.ocaml-mutants.toml` is optional. If present, it must contain `version = 1` at
the root. The parser rejects unknown sections and keys as likely typos.

```toml
version = 1

[mutation]
include = ["lib/**/*.ml", "bin/**/*.ml"]
exclude = ["**/test/**", "**/_build/**"]
operators = ["boolean-literal", "condition-negation", "comparison"]
profile = "balanced"

[[mutation.expect]]
id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
reason = "Equivalent because this input is fixed by the public contract."

[test]
timeout = 30.0
baseline_runs = 3
parallel_safe = false

[[test.stages]]
name = "fast"
command = ["dune", "build", "@dogfood-fast"]

[[test.stages]]
name = "full"
command = ["dune", "runtest", "--force"]

[execution]
jobs = 4

[cache]
mode = "auto"
directory = "team-cache"
```

## Fields

- `mutation.include`: string glob array. At least one pattern must match.
- `mutation.exclude`: string glob array applied after includes.
- `mutation.operators`: names from the operator reference.
- `mutation.profile`: `"balanced"`, `"strong"`, or `"all"`. The default and
  `init` output both use `"balanced"`.
- `mutation.expect`: repeated strict table rows containing a unique full
  64-character mutant `id` and a non-empty `reason`. Expected mutants still run
  on every invocation and never use the outcome cache.
- `test.command`: non-empty argv array. No shell parsing is performed. This is
  the compatibility spelling for one stage and cannot be combined with
  `test.stages`.
- `test.stages`: ordered, uniquely named command rows. Stages fail fast for each
  mutant.
- `test.timeout`: positive seconds. If omitted, the timeout is
  `max(10 seconds, slowest observed baseline × 5)`. An explicit timeout at or
  below the slowest observed baseline is rejected before mutation execution.
- `test.baseline_runs`: positive integer, default 3. Every stage is measured
  this many times; the complete per-stage observations remain in the native
  report.
- `test.parallel_safe`: permits concurrent custom test commands. The default
  Dune command is always safe because each worker has its own build directory.
- `execution.jobs`: positive worker count.
- `cache.mode`: `"auto"`, `"on"`, or `"off"`. `auto` is proof-gated off until
  every cache input is represented. `on` is an explicit opt-in; its write
  failures are fatal. Errors, cancellations, inconclusive outcomes, unconfirmed
  timeouts, and expected mutants are never stored as reusable outcomes.
- `cache.directory`: optional cache directory. Relative paths resolve from the
  OS cache root, never from the source workspace. Existing directories require
  the ocaml-mutants ownership marker; workspace paths, ancestors, and symlink
  escapes are rejected.

CLI options override TOML. Repeating `--include`, `--exclude`, or `--operator`
replaces the corresponding TOML array. `--profile` overrides the profile, and
`--cache-mode auto|on|off` overrides only `cache.mode` while retaining the
configured cache directory.
Repeatable `--mutant FULL_OR_UNIQUE_PREFIX` resolves against the complete
catalog, including rules outside the selected profile, and cannot be combined
with changed-file or operator filters. `--fresh` bypasses existing outcomes but
stores newly proven cacheable outcomes. Values after `--` replace the configured
stages with one argv-vector stage.

Unknown keys are rejected at every table depth. Duplicate expectation IDs and
duplicate stage names are errors rather than last-value-wins overrides.

An expectation is evidence to check, not a skip list. Its mutant runs on every
selected invocation: survival fulfills the expectation, while a kill or a
confirmed timeout makes it unfulfilled and returns exit 2. An error or an
unconfirmable result remains an infrastructure/inconclusive failure. A full
run also returns exit 2 when an expected full ID is no longer in the catalog;
a partial run records expectations outside its selection as `not-evaluated`
without changing the exit decision. Terminal output and the native JSON ledger
show these states explicitly.

Globs use `/` as the portable separator. `*` matches within a path component,
`?` matches one non-separator byte, and `**` crosses directory boundaries.
