# CI sharding

Create a deterministic plan once and distribute its zero-based assignments:

```console
ocaml-mutants plan --shards 4 --output plan.json
ocaml-mutants run --shard-plan plan.json --shard "$SHARD"
```

`--durations-from RUN_ID` uses previous observed durations for greedy balancing;
without them, stable full-ID hashing assigns mutants. The plan binds catalog,
resolved config, workspace, and toolchain inputs. A shard run rechecks that
fingerprint before executing.

Download all authoritative run reports, then:

```console
ocaml-mutants merge --path . plan.json shard-0.json shard-1.json shard-2.json shard-3.json
ocaml-mutants check
ocaml-mutants report --format html --format sarif --output artifacts
```

Merge produces a complete report only when every assignment appears exactly
once and contains exactly its planned IDs. See the
[complete GitHub Actions example](github-actions-sharding.yml).
