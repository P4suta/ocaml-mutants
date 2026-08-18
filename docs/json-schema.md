# JSON v1 contracts

ocaml-mutants publishes two native JSON document types and one interoperable,
lossy report projection. Consumers of a native document must check both
`document_type` and `schema_version` before decoding it.

## `ocaml-mutants.run-report-v1`

Produced by `run --json` and `report --json`. The normative schema is
`run-report-v1.schema.json`. It records completed, interrupted, and failed runs;
start and finish timestamps; workspace, toolchain, selection, test, and cache
metadata; the selected `balanced`, `strong`, or `all` mutation profile as a
field independent of the selection description; a complete or partial summary;
executed mutants; explicit `not_run` mutants; structured phase/cause failures;
and skip counts with sorted, unique, concrete source examples.

Each executed mutant has a concrete versioned rule, separate stdout and stderr
captures, total byte counts, and truncation flags. IDs include the 20-hex display
prefix and the full SHA-256 identity. A timeout retry retains the initial timeout
and serial retry as separate attempts, including each attempt's outcome, stage
timings, duration, and captured output. The top-level expectation ledger uses
explicit `fulfilled`, killed/timeout unfulfilled, `inconclusive`, `error`,
`stale`, and partial-selection `not-evaluated` states.

## `ocaml-mutants.catalog-v1`

Produced only by `list --json`. The normative schema is
`catalog-v1.schema.json`. A catalog is not a run report: it has no outcomes,
summary, test output, or run ID. It records the mutation profile separately
from the changed/full/explicit-mutant selection description.

The two native schemas, the exact emitted surface of Mutation Testing Report
Schema v2, and this English reference are installed in the package share
directory. The native schema files use JSON Schema Draft 2020-12; the installed
`mutation-testing-report-v2.schema.json` compatibility schema uses Draft 7.
All three are validated against report fixtures and real CLI output in the
release test suite.

## Native contract and ecosystem projection

`run-report-v1` is not a private intermediate format. It preserves the complete
modeled engine state used for ocaml-mutants-specific behavior, including stage
baselines and timings, timeout confirmation, expectations, partial runs,
structured infrastructure failures, suppressed cleanup errors, bounded process
output with truncation metadata, warnings, and cache provenance. The bounded
stdout and stderr observations are intentionally not unlimited transcripts.

If a baseline stage fails or is cancelled, the failed report retains every
completed earlier stage and every successful repetition of the active stage.
Its slowest duration is derived only from those measurements; an explicit
configured timeout remains present even though mutation execution did not
begin.

A Mutation Testing Report Schema v2 projection is produced with:

```console
ocaml-mutants run --stryker-json --threshold-high HIGH --threshold-low LOW -- COMMAND ARG...
```

Both thresholds are required integer percentages from 0 through 100, and
`LOW` must not exceed `HIGH`. The report is written only to standard output;
the native `run-report-v1` ledger is stored first and remains authoritative.
The projection is necessarily lossy because the shared schema has a smaller
outcome and operational model. Consumers that need to diagnose, resume, or
audit a run must retain `run-report-v1`.

The compatibility document uses `schemaVersion: "2"`, not the native
`document_type`/`schema_version` discriminator. Its exact status mapping,
deterministic ordering, source validation, and local-only security boundary are
documented in
[Stryker report ecosystem compatibility](https://github.com/ocaml-mutants/ocaml-mutants/blob/main/docs/stryker-compatibility.md).
