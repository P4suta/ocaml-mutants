# Pinned local corpus gate

The local corpus gate exercises the OCaml-native engine against three reviewed
upstream revisions. It is an acceptance input, not a source of mutation rules
or a substitute for the native run ledger.

The normative input is [`scripts/corpus-v1.toml`](../scripts/corpus-v1.toml).
Repository URLs, versions, full Git commits, the Balanced profile, selection
policy, per-rule sample count, OCaml compiler, commands, process budgets,
per-mutant timeout, output bounds, and allowed artifact roots all live there.
The runner has no fallback timeout or sample count.

## Pinned inputs

| Project | Version | Commit | Selection |
| --- | --- | --- | --- |
| Cmdliner | 2.1.1 | `d8eb07b7879432636e6ecf4057b16b30b4095cda` | Every Balanced full ID |
| Alcotest | 1.9.1 | `bcb1466eb13f2512049252d374938680cc20b87e` | First five full IDs in lexicographic order for each rule |
| Ppxlib | 0.38.0 | `09660e9d9e153d2dbe47ac621e695d7de717eb65` | First five full IDs in lexicographic order for each rule |

The sample count is read from `sample_count_per_rule = 5`; selection code does
not contain a separate numeric policy. Each catalog is generated twice. The
gate compares both canonical JSON and the ordered full-ID sequence before it
runs a mutant.

## Isolation and ownership

`mise run corpus` uses an owned directory below the platform cache location:

- Windows: `%LOCALAPPDATA%/ocaml-mutants/pinned-corpus-v1`
- macOS: `~/Library/Caches/ocaml-mutants/pinned-corpus-v1`
- other POSIX systems: `$XDG_CACHE_HOME/ocaml-mutants/pinned-corpus-v1`, or the
  standard `~/.cache` base when `XDG_CACHE_HOME` is unset

An exact ownership marker is required whenever that root already exists.
Repositories, the isolated opam root, the dedicated path switch, stage-0 build,
engine caches, and evidence have separate children below it. Cache namespaces
also differ by project.

Temporary clone staging is created only under an owned session directory. A
session is removed only when its resolved parent, name prefix, and unique marker
all match. The runner has no repository, switch, cache, or evidence deletion
operation. An unmarked root, foreign marker, reparse point, dirty source entry,
or incomplete switch fails closed and requires operator inspection.

The stage-0 executable is built from this repository with Dune's build directory
outside the source tree, then atomically copied to the owned cache. The corpus
repositories are never used as the stage-0 build location. Opam state is kept
in the corpus root rather than the user's normal opam root.

## Source and report proofs

Every repository must have the configured origin. The runner fetches, verifies
that the pinned object is a commit, checks it out detached, and compares `HEAD`
with all 40 configured hexadecimal characters. Before dependency preparation,
it records every source directory, regular file digest, mode, and internal link.
It repeats that scan after acceptance, including on failure.

Git metadata is not source input. The only excluded worktree roots are the
manifest-declared `_build` or `_opam` artifact kinds; arbitrary exclusions are
rejected and an excluded root may not contain tracked files. Absolute links,
escaping links, symlinked artifact roots, and reparse-point roots are rejected.

Catalogs are checked against `catalog-v2`. Every native run report is checked
against `run-report-v2`, and must have:

- completed, failure-free infrastructure state;
- exactly the selected full IDs, with `not_run = 0`;
- no `error` or `inconclusive` result;
- no unconfirmed timeout.

Run exits 0 after complete measurement even when an upstream corpus contains
real survivors. Exit 2, interruption, a partial report, or a missing selected
result is an infrastructure failure. Accepted catalogs, reports, and a compact summary
remain below the marker-owned `evidence` directory.

## Commands

These two checks are local and do not create the OS-cache root or contact a
remote:

```console
mise run corpus-check
mise run corpus-unit
```

The full command initializes the isolated opam state and fetches the configured
repositories, so it requires network access on a cold cache:

```console
mise run corpus
```

Review the manifest diff before changing a pin, command, budget, artifact root,
or sampling policy. A successful local corpus run is one release gate; it does
not by itself establish cross-OS production readiness.
