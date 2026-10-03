# Terminal UI

Run `ocaml-mutants ui [RUN_ID]` to browse immutable stored reports. Invoke
`ocaml-mutants` with no arguments in an interactive terminal for the live UI;
press `r` there to start a normal mutation run. Explicit `run` always keeps its
stable stream output. The default list contains survivors, not-covered or
inconclusive results, and errors; killed mutants are aggregated.

- Up/Down or `k`/`j`: move
- PageUp/PageDown: move ten rows
- Left/Right or `h`/`l`: move to the older/newer stored run
- Tab: actionable/all/killed filter
- `r`: start a run (live UI only)
- `q` or Escape: cancel the active run; otherwise quit
- Ctrl-C: cancel the active run; otherwise quit

The detail pane shows the full and lineage IDs, evidence level and origin,
coverage, expectation state/reason, source diff, every executed stage, the
killing/last stage, and bounded stdout/stderr. Each checkpointed result arrives
on the live callback before final report publication, so the same detail is
browsable while a run is active. Layout stacks vertically on narrow terminals.
The header shows the run ID, phase, completed/total counts, workers,
cache/resume counts, elapsed time, ETA, latest settled mutant, and warnings.
Completion reloads the immutable report and keeps the UI open; an interrupt is
therefore visible as an interrupted saved run.

Invalid stored reports are omitted from interactive history with an ID-bearing
warning. This does not weaken evidence validation: `check`, `report`, and
explicit run loading continue to reject the same report.

The Windows backend uses Matrix 0.1.0's public `attach` interface, restores the
original Console input mode and code page, polls ConPTY resize, and feeds bytes
through the Matrix Unicode parser. POSIX uses Matrix's standard backend for
colored output and its public `attach` interface for monochrome output.
`NO_COLOR`, `ui --color never`, and `ui --no-color` retain the full-screen and
keyboard behavior while removing ordinary SGR styling; terminal protocol,
cursor, and alternate-screen control sequences remain. `--color always`
overrides `NO_COLOR`. `report --format terminal --no-color` is the stable
line-oriented plain-text alternative.

Automated narrow/wide keyboard, resize, Unicode, Ctrl-C, exception, and
restoration coverage through real PTYs/ConPTY on all three operating systems is
still a release hold.
