# Privacy

ocaml-mutants is local-first. It performs no report upload, telemetry, CDN
fetch, or other automatic network transmission. Stryker JSON and SARIF are
files/stdout projections only.

`[privacy]` limits retained stdout/stderr, applies configured non-empty literal
redactions as equal-length `*` masks before checkpointing, and selects
`context`, `all`, or `none` source embedding. Offline HTML embeds only
the report data and its own CSS/JavaScript, carries a restrictive CSP, strips
control characters, and makes no network request. Full-source embedding must
be selected explicitly.

Native reports preserve the number and ordering of configured redaction rules,
but serialize every rule itself as `<redacted>`. The input fingerprint still
depends on the real resolved configuration, so changing a rule invalidates
reuse without disclosing the literal.

Undo records created by `mutant apply` live under `.ocaml-mutants/undo` with
owner-private permissions and can contain the original source needed to revert.
