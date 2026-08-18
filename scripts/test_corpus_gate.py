#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Pure contracts for the pinned corpus manifest and selection logic."""

from __future__ import annotations

import dataclasses
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True

import corpus_gate as gate  # noqa: E402


HERE = Path(__file__).resolve().parent
MANIFEST = HERE / "corpus-v1.toml"


class ManifestContracts(unittest.TestCase):
    def test_requested_pins_and_policy_are_exact(self) -> None:
        manifest = gate.load_manifest(MANIFEST)
        self.assertEqual("balanced", manifest.profile)
        self.assertEqual(2, manifest.catalog_repetitions)
        self.assertEqual(5, manifest.sample_count_per_rule)
        self.assertEqual(gate.REQUIRED_TIMEOUTS, set(manifest.command_timeouts_seconds))
        self.assertEqual(
            [
                (
                    "cmdliner",
                    "2.1.1",
                    "d8eb07b7879432636e6ecf4057b16b30b4095cda",
                    "all",
                ),
                (
                    "alcotest",
                    "1.9.1",
                    "bcb1466eb13f2512049252d374938680cc20b87e",
                    "first-per-rule",
                ),
                (
                    "ppxlib",
                    "0.38.0",
                    "09660e9d9e153d2dbe47ac621e695d7de717eb65",
                    "first-per-rule",
                ),
            ],
            [
                (
                    project.name,
                    project.version,
                    project.commit,
                    project.selection,
                )
                for project in manifest.projects
            ],
        )

    def test_missing_timeout_is_rejected_without_a_default(self) -> None:
        contents = MANIFEST.read_text(encoding="utf-8")
        contents = contents.replace("git_clone = 1800.0\n", "", 1)
        with tempfile.TemporaryDirectory(prefix="corpus-manifest-contract-") as raw:
            path = Path(raw) / "manifest.toml"
            path.write_text(contents, encoding="utf-8")
            with self.assertRaisesRegex(gate.CorpusError, "missing required keys"):
                gate.load_manifest(path)

    def test_arbitrary_source_exclusion_is_rejected(self) -> None:
        contents = MANIFEST.read_text(encoding="utf-8")
        contents = contents.replace(
            'corpus_excluded_roots = ["_build"]',
            'corpus_excluded_roots = ["_build", "src"]',
            1,
        )
        with tempfile.TemporaryDirectory(prefix="corpus-manifest-contract-") as raw:
            path = Path(raw) / "manifest.toml"
            path.write_text(contents, encoding="utf-8")
            with self.assertRaisesRegex(gate.CorpusError, "only owned Dune/opam"):
                gate.load_manifest(path)


def mutant(rule: str, number: int) -> dict[str, str]:
    return {"rule": rule, "full_id": f"{number:064x}"}


class SelectionContracts(unittest.TestCase):
    def test_all_preserves_catalog_order(self) -> None:
        mutants = [mutant("z@1", 3), mutant("a@1", 1), mutant("z@1", 2)]
        self.assertEqual(
            tuple(item["full_id"] for item in mutants),
            gate.select_mutants(mutants, "all", 5),
        )

    def test_first_five_are_lexicographic_per_rule(self) -> None:
        mutants = [
            mutant(rule, number)
            for number, rule in (
                (107, "z-rule@1"),
                (4, "a-rule@1"),
                (102, "z-rule@1"),
                (6, "a-rule@1"),
                (101, "z-rule@1"),
                (7, "a-rule@1"),
                (103, "z-rule@1"),
                (5, "a-rule@1"),
                (105, "z-rule@1"),
                (1, "a-rule@1"),
                (106, "z-rule@1"),
                (3, "a-rule@1"),
                (104, "z-rule@1"),
                (2, "a-rule@1"),
            )
        ]
        expected = tuple(
            [f"{number:064x}" for number in range(1, 6)]
            + [f"{number:064x}" for number in range(101, 106)]
        )
        self.assertEqual(
            expected,
            gate.select_mutants(mutants, "first-per-rule", 5),
        )

    def test_duplicate_full_id_is_rejected(self) -> None:
        mutants = [mutant("a@1", 1), mutant("b@1", 1)]
        with self.assertRaisesRegex(gate.CorpusError, "duplicate mutant ID"):
            gate.select_mutants(mutants, "first-per-rule", 5)


class CatalogContracts(unittest.TestCase):
    def test_comparison_checks_order_and_canonical_document(self) -> None:
        first = gate.Catalog({"value": 1}, b'{"value":1}', ("a", "b"))
        same = dataclasses.replace(first)
        self.assertIs(first, gate.compare_catalogs([first, same]))
        with self.assertRaisesRegex(gate.CorpusError, "ordered full-ID"):
            gate.compare_catalogs(
                [first, gate.Catalog(first.document, first.canonical, ("b", "a"))]
            )
        with self.assertRaisesRegex(gate.CorpusError, "canonical JSON"):
            gate.compare_catalogs(
                [first, gate.Catalog({"value": 2}, b'{"value":2}', ("a", "b"))]
            )


if __name__ == "__main__":
    unittest.main()
