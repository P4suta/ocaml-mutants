# Stryker report ecosystem compatibility

## Scope

ocaml-mutants aims to be an OCaml-native mutation engine that can participate in
the Stryker reporting ecosystem. The compatibility point is the
[Mutation Testing Report Schema v2.0.5](https://github.com/stryker-mutator/mutation-testing-elements/blob/8c3f6c7d34953aa2758a514728e78866e7b9c269/packages/report-schema/src/mutation-testing-report-schema.json),
not a shared mutation kernel. The schema is designed for reports from mutation
testing frameworks, and compatible reports can be consumed by tools such as
[Mutation Testing Elements](https://github.com/stryker-mutator/mutation-testing-elements)
and, if a user separately chooses to upload one, the
[Stryker Dashboard](https://stryker-mutator.io/docs/General/dashboard/).

This project is independent of the Stryker project. It is not affiliated with,
endorsed by, or maintained by the Stryker team. “Stryker” is used here only to
identify the report schema and ecosystem compatibility target.

The compatibility gate vendors that official schema at immutable commit
`8c3f6c7d34953aa2758a514728e78866e7b9c269` under
`test/vendor/stryker-mutator`, together with its Apache-2.0 license and
machine-checked provenance. The pinned schema SHA-256 is
`404575e9264686dbf85c399f6531fdb489bebc3146e7e1d793b8083bc6a041ae`.
Both the checked-in projection fixture and a real CLI projection must validate
against the official schema and the stricter local emitted-surface schema. The
vendored upstream files are test inputs only and are not part of the installed
package surface.

## What remains OCaml-native

Report compatibility does not constrain the mutation engine. These facilities
remain native to OCaml and Dune:

- Typedtree-based discovery, UID and environment evidence, and exact source
  identity checks;
- operator applicability proofs and OCaml-specific rendering semantics;
- Dune workspace analysis, snapshot isolation, and instrumented builds;
- cancellation, process-tree ownership, timeout confirmation, and staged test
  execution; and
- stable mutant identity, expectation policy, cache proofs, and exit policy.

There is no intended compatibility with Stryker command-line options,
configuration formats, plugins, mutator implementations, runtime protocol, or
cache representation.

## Two reports, two responsibilities

The native `ocaml-mutants.run-report-v1` document is the authoritative ledger.
It preserves the complete modeled engine state and records information that is
not faithfully represented by the shared report schema. Command output is an
intentional exception: stdout and stderr are bounded observations rather than
unlimited transcripts.

- per-stage baselines, results, and timings;
- confirmed versus unconfirmed timeouts and inconclusive outcomes;
- executed expected survivors, fulfilled expectations, and stale or unfulfilled
  expectations;
- partial runs and explicit `not_run` entries;
- structured phase and cause data, primary and suppressed cleanup failures;
- bounded stdout and stderr captures, truncation and total-byte metadata;
- warnings, selection details, workspace identity, and cache provenance.

The Stryker-schema report is a deterministic one-way projection of that ledger.
It is intended for visualization, score aggregation, and generic report
tooling. It is not used as cache state, as a resume journal, or as input to
expectation and exit-code decisions. Keeping the native report alongside the
projected report is therefore required for operational diagnosis and audit.

## Projection policy

Run the local projection with explicit score thresholds:

```console
ocaml-mutants run --stryker-json --threshold-high HIGH --threshold-low LOW -- COMMAND ARG...
```

`HIGH` and `LOW` must both be integer percentages from 0 through 100, with
`LOW <= HIGH`. They are required because choosing score policy is a caller
decision, not an engine default. `--stryker-json` cannot be combined with
`--json` or `--quiet`. The projection is written only to standard output after
the native run report has been stored.

The status mapping is fixed as follows:

| Native result | Projected status | `statusReason` |
| --- | --- | --- |
| Killed | `Killed` | An attached expectation is reported as unfulfilled. |
| Unexpected survivor | `Survived` | Omitted. |
| Fulfilled expected survivor | `Ignored` | Includes the configured expectation reason. |
| Confirmed timeout | `Timeout` | An attached expectation is reported as unfulfilled. |
| Unconfirmed timeout | `RuntimeError` | Identifies the result as inconclusive because timeout confirmation failed. |
| Inconclusive | `RuntimeError` | Preserves the inconclusive reason. |
| Native `Error` outcome | `RuntimeError` | Preserves the error reason. |
| Not run | `Pending` | States that ocaml-mutants did not run the mutant. |

`NoCoverage` and `CompileError` are valid shared-schema statuses but are not
emitted by this projection. The native outcome and expectation state remain
authoritative where a generic consumer cannot display the distinction.

The execution policy currently treats a nonzero test-command exit or a signal
as a failed test, so its native outcome is `Killed` and the projection preserves
that classification. Only a failure represented by the engine as a native
`Error` becomes `RuntimeError`. Distinguishing a test assertion from a test
runner crash would require richer typed stage evidence and remains an explicit
compatibility gap; the report projection does not guess after the fact.

Each projected file contains its normalized workspace-relative path, language,
full original source, and mutants sorted by their 64-character full IDs; file
paths are sorted as well. Executed mutants include duration in milliseconds.
Locations use one-based lines and columns as required by the shared schema.
OCaml compiler locations count source bytes, so a projected column is the
native zero-based byte column plus one. In a UTF-8 file, a multi-byte code point
therefore advances the column by its byte width. The projection does not invent
a Unicode-scalar or UTF-16-code-unit conversion. This preserves the byte-exact
engine location; a consumer that assumes a different column unit may display a
shifted caret after non-ASCII text. Mutator names map as follows:

| OCaml operator family | `mutatorName` |
| --- | --- |
| Boolean literal | `BooleanLiteral` |
| Condition negation, if branch, match arm | `ConditionalExpression` |
| Boolean connective | `LogicalOperator` |
| Comparison | `EqualityOperator` |
| Integer or float arithmetic | `ArithmeticOperator` |
| Sequence deletion | `BlockStatement` |
| Return replacement | `ReturnValue` |
| Constructor replacement | `ConstructorReplacement` |

Before emission, each source file is read exactly once. Its digest must equal
the source digest in every corresponding native mutant, and every recorded byte
span must slice to the mutant's original text. The recorded start and end
line/byte-column coordinates must also exactly equal the positions derived from
those byte offsets in the source. An unavailable source, digest, slice, or
coordinate mismatch, invalid span, or duplicate full mutant ID anywhere in the
run aborts the projection. This prevents changed, instrumented, or internally
inconsistent source evidence from being presented as the source against which
the stored outcome was established.

## Local-only stance

The compatibility output is local and stdout-only. ocaml-mutants does not
upload reports, contact the Stryker Dashboard, read dashboard API keys, or
enable network publication implicitly. A user may redirect the JSON to a local
file; any later dashboard upload is a separate user-controlled operation outside
the current product boundary.

This distinction matters because a shared-schema report contains source text as
well as mutation results. Any future built-in upload feature would require an
explicit product decision, opt-in configuration, credential handling, and a
documented disclosure model. Report-schema compatibility alone must never cause
network access.
