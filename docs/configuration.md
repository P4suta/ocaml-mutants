# Configuration v2

The optional workspace file is `.ocaml-mutants.toml`. Unknown keys, duplicate
IDs/stage names, malformed durations, and empty commands are errors. Built-in
defaults, file values, then CLI overrides are applied in that order.

```toml
version = 2

[mutation]
profile = "balanced"
include = ["**/*.ml"]
exclude = ["**/test/**", "**/tests/**", "**/_build/**"]
operators = ["boolean", "comparison", "arithmetic"]

[[mutation.expect]]
id = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
reason = "Equivalent under the documented invariant"

[test]
driver = "auto" # auto | dune | command
command = ["dune", "runtest", "--force"]
timeout = "auto"
baseline_runs = 3
parallel_safe = false
external_inputs = []
reproducible = true

# For several ordered stages, replace command with repeated tables:
# [[test.stages]]
# name = "unit"
# command = ["dune", "build", "@unit"]

[execution]
mode = "strict" # strict | fast
jobs = "auto"

[cache]
historical_reuse = "off" # off | exact | estimated
# directory = "/private/cache/root"

[policy]
require_complete = true
max_unexpected_survivors = 0
# minimum_score = 90.0
# maximum_score_drop = 2.0
allow_estimated = false

[report]
formats = ["terminal", "json"]
# directory = "artifacts"

[privacy]
stdout_limit_bytes = 65536
stderr_limit_bytes = 65536
redactions = []
source_embedding = "context" # context | all | none
```

Zero configuration detects a Dune workspace and uses strict execution,
automatic jobs/timeout, no historical reuse, complete-evidence policy, and
bounded source context. The crash journal is always enabled independently of
historical reuse.

`driver = "auto"` selects the Dune driver when every configured stage is
Dune-managed; a custom argv stage selects the command driver. `driver = "dune"`
uses Dune 3.22's deterministic `@DIR/runtest-NAME` aliases for individually
described tests, followed by one forced global `@runtest` proof. Custom, cram,
inline, and otherwise unclassified rules remain behind that global alias and
are never rewritten as direct executable calls. Strict mode prioritizes known
covering aliases but runs the global proof before recording a survivor. Fast
mode may omit known non-covering aliases and marks the result as estimated.

Dune builds set `DUNE_CACHE=enabled-except-user-rules` with an owner-private
cache below the snapshot. This lets compiler artifacts populate isolated worker
build directories without sharing user test rules or their mutable outputs.
Resolved stage executables, test binaries, and PPX artifacts are digested into
the reuse fingerprint; any resolution or hash failure disables reuse.

Version 1 files remain readable with a warning. Use `config migrate` to preview
and `config migrate --write` to atomically write canonical v2.
