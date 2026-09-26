# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(sys.argv.pop(1)).resolve()


class PackageReleaseContract(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory(
            prefix="ocaml-mutants-package-release-"
        )
        self.addCleanup(self.temporary.cleanup)
        self.parent = Path(self.temporary.name)
        self.install = self.parent / "install"
        executable = self.install / "bin" / (
            "ocaml-mutants.exe" if os.name == "nt" else "ocaml-mutants"
        )
        executable.parent.mkdir(parents=True)
        executable.write_bytes(b"release fixture\n")
        executable.chmod(0o755)

    def package(self, archive_format: str, output: Path) -> subprocess.CompletedProcess:
        return subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--install-root",
                str(self.install),
                "--output-dir",
                str(output),
                "--version",
                "1.0.0",
                "--target",
                "contract",
                "--format",
                archive_format,
            ],
            check=False,
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            env={**os.environ, "SOURCE_DATE_EPOCH": "0"},
        )

    def test_supported_tree_packages_in_both_formats(self) -> None:
        for archive_format in ("tar.gz", "zip"):
            with self.subTest(archive_format=archive_format):
                output = self.parent / ("out-" + archive_format.replace(".", "-"))
                completed = self.package(archive_format, output)
                self.assertEqual(completed.returncode, 0, completed.stderr)
                archive = Path(completed.stdout.strip())
                self.assertTrue(archive.is_file())

    def test_output_must_not_be_inside_install_tree(self) -> None:
        output = self.install / "release-assets"
        completed = self.package("zip", output)
        self.assertNotEqual(completed.returncode, 0)
        self.assertIn("outside the install root", completed.stderr)
        self.assertFalse(output.exists())

    def test_links_are_rejected_for_both_formats(self) -> None:
        target = self.install / "bin" / (
            "ocaml-mutants.exe" if os.name == "nt" else "ocaml-mutants"
        )
        link = self.install / "linked-cli"
        try:
            link.symlink_to(target)
        except OSError as error:
            self.skipTest(f"symbolic links are unavailable: {error}")
        for archive_format in ("tar.gz", "zip"):
            with self.subTest(archive_format=archive_format):
                output = self.parent / ("links-" + archive_format.replace(".", "-"))
                completed = self.package(archive_format, output)
                self.assertNotEqual(completed.returncode, 0)
                self.assertIn(
                    "symbolic link or reparse point",
                    completed.stderr,
                )
                self.assertFalse(output.exists())


if __name__ == "__main__":
    unittest.main()
