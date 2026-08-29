# Expected mutants

Expectations use a complete 64-character mutant ID and a non-empty reason.
They are evidence annotations, not exclusions: the mutant is still executed.

Use `ocaml-mutants mutant expect ID --reason TEXT`. The command shows the TOML
diff, requires confirmation, preserves existing text/comments/order, validates
the result, rechecks the file before commit, and writes atomically.

A stale exact ID is never rebound automatically. The structural lineage ID is
display-only and may help a human find a candidate after source movement.
Review the source and explicitly create a new expectation.
