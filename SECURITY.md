# Security policy

## Supported versions

ocaml-mutants is a 0.1.0 pre-release; only the latest `main` receives fixes.

## Reporting a vulnerability

Use GitHub's private vulnerability reporting on this repository
(Security → Report a vulnerability). Please do not open a public issue for
security reports.

Relevant scope: ocaml-mutants runs local builds and tests inside disposable
workspace snapshots and never performs network I/O at runtime. Reports about
escaping the snapshot boundary, writing to an original target workspace,
following symlinks/junctions out of owned directories, or executing content
that the tool should treat as data are all in scope.
