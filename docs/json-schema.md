# Public JSON schemas

Installed schemas live under `share/ocaml-mutants`:

- `run-report-v2.schema.json`: authoritative immutable run evidence, including
  resolved config, input fingerprint, baseline/test inventory, attempts,
  coverage, checkpoint/cache provenance, completeness, warnings, and errors.
- `catalog-v2.schema.json`: deterministic full/lineage IDs and discovery data.
- `check-report-v1.schema.json`: policy decision and stable 0/1/2 result.
- `event-v1.schema.json`: JSONL progress events with monotonic sequence numbers.
- `shard-plan-v1.schema.json`: deterministic catalog partition and fingerprint.
- `mutation-testing-report-v2.schema.json`: strict emitted surface of the lossy
  Stryker Mutation Testing Report Schema v2 projection.

Schemas reject unknown fields. Writers use canonical field and collection
ordering, and codec contract tests require lossless decode/encode round trips.
The complete SHA mutant ID is authoritative. `lineage_id` is only a display and
manual rebind hint.

`run-report-v1` and `catalog-v1` remain installed solely for read-only migration
and historical validation. New output never uses them.

HTML, Markdown, SARIF, and Stryker output are projections from native v2. Store
the native document whenever evidence must be audited or checked later.
