# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

from __future__ import annotations

import argparse
from pathlib import Path, PurePosixPath


PACKAGE = "ocaml-mutants"
FORBIDDEN_LIBRARY_SUFFIXES = {
    ".a",
    ".cma",
    ".cmi",
    ".cmx",
    ".cmxa",
    ".dll",
    ".dylib",
    ".lib",
    ".ml",
    ".mli",
    ".so",
}


def relative_files(root: Path) -> set[PurePosixPath]:
    return {
        PurePosixPath(path.relative_to(root).as_posix())
        for path in root.rglob("*")
        if path.is_file()
    }


def expected_files(executable: PurePosixPath) -> set[PurePosixPath]:
    return {
        executable,
        PurePosixPath(f"lib/{PACKAGE}/META"),
        PurePosixPath(f"lib/{PACKAGE}/dune-package"),
        PurePosixPath(f"lib/{PACKAGE}/opam"),
        PurePosixPath(f"doc/{PACKAGE}/CHANGELOG.md"),
        PurePosixPath(f"doc/{PACKAGE}/LICENSE-APACHE"),
        PurePosixPath(f"doc/{PACKAGE}/LICENSE-MIT"),
        PurePosixPath(f"doc/{PACKAGE}/README.md"),
        # Emitted by dune for generate_opam_files-managed packages: the odoc 3
        # documentation configuration. Metadata, not an OCaml library surface.
        PurePosixPath(f"doc/{PACKAGE}/odoc-config.sexp"),
        PurePosixPath(f"share/{PACKAGE}/catalog-v1.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/catalog-v2.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/check-report-v1.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/event-v1.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/json-schema.md"),
        PurePosixPath(
            f"share/{PACKAGE}/mutation-testing-report-v2.schema.json"
        ),
        PurePosixPath(f"share/{PACKAGE}/run-report-v1.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/run-report-v2.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/shard-plan-v1.schema.json"),
        PurePosixPath(f"share/{PACKAGE}/completions/ocaml-mutants.bash"),
        PurePosixPath(f"share/{PACKAGE}/completions/_ocaml-mutants"),
        PurePosixPath(f"share/{PACKAGE}/completions/ocaml-mutants.fish"),
        PurePosixPath(f"share/{PACKAGE}/completions/ocaml-mutants.ps1"),
        PurePosixPath("man/man1/ocaml-mutants.1"),
        *{
            PurePosixPath(f"share/{PACKAGE}/docs/{name}")
            for name in (
                "quickstart.md",
                "configuration.md",
                "tui.md",
                "evidence.md",
                "ci-sharding.md",
                "github-actions-sharding.yml",
                "migration-v1.md",
                "expectations.md",
                "privacy.md",
                "troubleshooting.md",
            )
        },
    }


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Verify the intentionally narrow installed artifact surface."
    )
    parser.add_argument("install_root", type=Path)
    args = parser.parse_args()

    root = args.install_root.resolve(strict=True)
    files = relative_files(root)
    executable_candidates = {
        PurePosixPath(f"bin/{PACKAGE}"),
        PurePosixPath(f"bin/{PACKAGE}.exe"),
    }
    executables = files & executable_candidates
    if len(executables) != 1:
        rendered = ", ".join(sorted(map(str, executables))) or "none"
        raise SystemExit(f"expected exactly one installed CLI, found: {rendered}")

    forbidden = sorted(
        path
        for path in files
        if path.parts[:2] == ("lib", PACKAGE)
        and path.suffix.lower() in FORBIDDEN_LIBRARY_SUFFIXES
    )
    if forbidden:
        raise SystemExit(
            "installed private OCaml library artifacts: "
            + ", ".join(map(str, forbidden))
        )

    expected = expected_files(next(iter(executables)))
    missing = sorted(expected - files)
    unexpected = sorted(files - expected)
    if missing or unexpected:
        details = []
        if missing:
            details.append("missing: " + ", ".join(map(str, missing)))
        if unexpected:
            details.append("unexpected: " + ", ".join(map(str, unexpected)))
        raise SystemExit("install layout mismatch; " + "; ".join(details))

    print(
        "install-layout: CLI, versioned schemas, docs, completions, manpage, "
        "and package metadata only"
    )


if __name__ == "__main__":
    main()
