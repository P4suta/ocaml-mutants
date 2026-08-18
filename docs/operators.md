# Mutation operators

All operators are versioned. The operator version is part of a mutant's stable
identity. `--operator NAME` and `mutation.operators` select operators by the
names below.

| Name | Examples | Profile | Type-safety condition |
| --- | --- | --- | --- |
| `boolean-literal` | `true` → `false` | balanced | Typed constructor is `bool` |
| `condition-negation` | `if p` → `if not (p)` | balanced | Typed condition is `bool` |
| `boolean-connective` | `&&` ↔ `||` | balanced | Resolved Stdlib identifier |
| `comparison` | `=` ↔ `<>`, `<` ↔ `<=`, `>` ↔ `>=` | balanced | Resolved Stdlib identifier and bool result |
| `integer-arithmetic` | `+` ↔ `-`, `*` ↔ `/` | balanced | Resolved Stdlib operator with int arguments/result |
| `float-arithmetic` | `+.` ↔ `-.`, `*.` ↔ `/.` | balanced | Resolved Stdlib operator with float arguments/result |
| `if-branch` | replace either branch with the other | strong | Both branches share the Typedtree result type |
| `sequence-deletion` | `first; second` → `second` | all | Typed sequence result is the right expression type |
| `return-replacement` | typed function result → neutral value | balanced | Stable function-body root with a supported result type |

Profiles are monotonically inclusive tiers over these families. `balanced`
(the default) runs every family marked balanced; `strong` adds `if-branch`,
whose edits often duplicate condition and Boolean-literal mutations at the
same `if` site; `all` adds `sequence-deletion`, whose deleted effects (for
example logging) are the classic source of equivalent mutants. `--mutant`
still resolves explicit IDs against the complete catalog regardless of the
selected profile.

The 0.1 frontend is conservative. It does not mutate tests selected by exclusion
rules, ghost/generated locations, Reason source, custom operators, or preprocessor
output that cannot be mapped byte-for-byte to an original `.ml` file. The report
counts every skip occurrence and records the complete sorted set of distinct
source or source-range examples for each reason. Return replacement is limited
to stable function-body
roots whose result is `bool`, `int`, `float`, `string`, `unit`, `list`, or
`option`; it never falls back to an untyped textual rewrite. Boundary
comparison rules intentionally target off-by-one failures instead of duplicating
whole-condition negation. Condition negation covers `if`, `while`, and typed
match/exception guards.

When two families describe the exact same source edit, the catalog keeps one
deterministically and reports the rest as duplicates. More local rules take
precedence (for example, `true-to-false@1` over a branch or return replacement),
so users do not pay for running semantically identical mutants under different
IDs.

The typed existential `Operator.Spec` registry is the single production writer
for all 30 versioned rules. At each typed visit site the frontend evaluates the
registry, validates every candidate against the exact source bytes, and commits
the validated mutants directly; there is no second generation path. A candidate
whose typed evidence does not own its source slice is skipped as an imprecise
mapping, while a candidate that cannot be validated against the source it was
derived from is an analysis invariant failure, not a skip. The traversal
preserves the reviewed skip precedence and exact-edit family dominance. Spec
semantic keys are audit evidence and do not drive production deduplication yet.

Adding an operator requires a new constructor, a stable public name, a version,
Typedtree evidence, source-preservation tests, and focused documentation. There
is no dynamic plugin ABI in 0.1.
