# Strict and fast evidence

`execution.mode = "strict"` is the default. Known covering per-test Dune aliases
and historical killers run first, but every potential survivor then executes a
forced global `@runtest` proof. That fallback retains cram, inline, custom, and
otherwise unclassified rules, so a hit map cannot create a survivor skip. A
kill can stop safely because it is observed executed evidence. Exact checkpoint
recovery and exact historical reuse are marked separately from direct execution.

`execution.mode = "fast"` permits declared optimizations. Any result that
depends on omitted non-covering tests carries `fast-estimated` provenance;
estimated historical reuse carries `historical-estimated`. Resuming either
kind remains `checkpoint-estimated` and can never upgrade the evidence to
exact. An observed kill remains `execution` evidence even in fast mode. The
default policy has `allow_estimated = false`, so `check` refuses estimated
evidence with exit 2. Observed kills remain executed evidence.

Evidence is never inferred from the structural lineage ID. Only the complete
SHA identity participates in execution, expectations, caching, and policy.
