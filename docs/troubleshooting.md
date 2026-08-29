# Troubleshooting

Start with `ocaml-mutants doctor`; use `doctor --deep` to exercise an isolated
snapshot, analysis build, test inventory, baseline, instrumentation, and
readiness without running mutants.

- Exit 2 from `run`: read the stable error code, phase, cause, and `next`
  remediation.
- Exit 2 from `check`: evidence is incomplete, corrupt, or estimated while the
  policy forbids estimates. Re-run rather than weakening policy accidentally.
- No-argument invocation exits 2: stdin/stdout are not both TTYs; use `run`,
  `check`, `report`, or `--events jsonl`.
- Shard fingerprint mismatch: regenerate the plan after source, config,
  toolchain, environment, or declared external-input changes.
- Apply/revert conflict: no overwrite occurred. Review the shown patch and
  regenerate or resolve manually.
- Interrupted run: repeat the same exact input. The checksum journal restores
  settled results and executes only unfinished mutants.
