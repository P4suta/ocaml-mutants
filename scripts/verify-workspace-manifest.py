#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Create or verify a content-addressed source-workspace manifest."""

from __future__ import annotations

import argparse
import hashlib
import io
import json
import os
import stat
import sys
import tempfile
from pathlib import Path, PurePosixPath
from typing import Any


FORMAT_VERSION = 1


class ManifestError(ValueError):
    pass


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ManifestError(f"duplicate JSON object key: {key!r}")
        result[key] = value
    return result


def canonical_root(path: Path) -> Path:
    try:
        root = path.resolve(strict=True)
    except OSError as error:
        raise ManifestError(f"cannot resolve workspace root {path}: {error}") from error
    if not root.is_dir():
        raise ManifestError(f"workspace root is not a directory: {root}")
    return root


def validate_excluded_roots(values: list[str]) -> list[str]:
    excluded: set[str] = set()
    for value in values:
        if (
            not value
            or value in {".", ".."}
            or Path(value).name != value
            or "/" in value
            or "\\" in value
        ):
            raise ManifestError(
                f"excluded root must be one direct child name, got {value!r}"
            )
        excluded.add(value)
    return sorted(excluded)


def is_reparse_point(metadata: os.stat_result) -> bool:
    attributes = getattr(metadata, "st_file_attributes", 0)
    reparse_flag = getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0)
    return bool(attributes & reparse_flag)


def digest_file(path: Path) -> str:
    digest = hashlib.sha256()
    try:
        with path.open("rb") as source:
            for chunk in iter(lambda: source.read(io.DEFAULT_BUFFER_SIZE), b""):
                digest.update(chunk)
    except OSError as error:
        raise ManifestError(f"cannot hash {path}: {error}") from error
    return digest.hexdigest()


def relative_name(parent: PurePosixPath, name: str) -> PurePosixPath:
    return PurePosixPath(name) if str(parent) == "." else parent / name


def scan_workspace(root: Path, excluded_roots: list[str]) -> list[dict[str, Any]]:
    excluded = set(excluded_roots)
    entries: list[dict[str, Any]] = []

    def walk(directory: Path, relative_parent: PurePosixPath) -> None:
        try:
            with os.scandir(directory) as iterator:
                children = sorted(iterator, key=lambda child: child.name)
        except OSError as error:
            raise ManifestError(f"cannot scan {directory}: {error}") from error

        for child in children:
            path = Path(child.path)
            relative = relative_name(relative_parent, child.name)
            relative_text = relative.as_posix()
            try:
                metadata = child.stat(follow_symlinks=False)
            except OSError as error:
                raise ManifestError(f"cannot inspect {path}: {error}") from error

            reparse = is_reparse_point(metadata)
            if str(relative_parent) == "." and child.name in excluded:
                if not stat.S_ISDIR(metadata.st_mode) or reparse:
                    raise ManifestError(
                        "refusing to exclude a generated root that is not an "
                        f"ordinary directory: {path}"
                    )
                continue

            mode = stat.S_IMODE(metadata.st_mode)
            if child.is_symlink() or reparse:
                try:
                    target = os.readlink(path)
                except OSError as error:
                    raise ManifestError(
                        f"cannot read link or reparse target {path}: {error}"
                    ) from error
                entries.append(
                    {
                        "path": relative_text,
                        "kind": "link",
                        "mode": mode,
                        "target": target,
                    }
                )
            elif stat.S_ISDIR(metadata.st_mode):
                entries.append(
                    {"path": relative_text, "kind": "directory", "mode": mode}
                )
                walk(path, relative)
            elif stat.S_ISREG(metadata.st_mode):
                entries.append(
                    {
                        "path": relative_text,
                        "kind": "file",
                        "mode": mode,
                        "size": metadata.st_size,
                        "sha256": digest_file(path),
                    }
                )
            else:
                raise ManifestError(f"unsupported workspace entry type: {path}")

    walk(root, PurePosixPath("."))
    return entries


def manifest(root: Path, excluded_roots: list[str]) -> dict[str, Any]:
    return {
        "format_version": FORMAT_VERSION,
        "workspace_root": str(root),
        "excluded_roots": excluded_roots,
        "entries": scan_workspace(root, excluded_roots),
    }


def path_is_within(path: Path, root: Path) -> bool:
    try:
        common = os.path.commonpath([str(path), str(root)])
    except ValueError:
        return False
    return os.path.normcase(common) == os.path.normcase(str(root))


def write_manifest(path: Path, value: dict[str, Any]) -> None:
    destination = path.resolve(strict=False)
    destination.parent.mkdir(parents=True, exist_ok=True)
    temporary: str | None = None
    try:
        with tempfile.NamedTemporaryFile(
            mode="w",
            encoding="utf-8",
            newline="\n",
            prefix=destination.name + ".",
            suffix=".pending",
            dir=destination.parent,
            delete=False,
        ) as output:
            temporary = output.name
            json.dump(
                value,
                output,
                ensure_ascii=False,
                allow_nan=False,
                sort_keys=True,
                separators=(",", ":"),
            )
            output.write("\n")
            output.flush()
            os.fsync(output.fileno())
        os.replace(temporary, destination)
        temporary = None
    except OSError as error:
        raise ManifestError(f"cannot write manifest {destination}: {error}") from error
    finally:
        if temporary is not None:
            try:
                os.unlink(temporary)
            except OSError:
                pass


def read_manifest(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(
            path.read_text(encoding="utf-8-sig"), object_pairs_hook=unique_object
        )
    except (OSError, UnicodeError, json.JSONDecodeError, ManifestError) as error:
        raise ManifestError(f"cannot decode manifest {path}: {error}") from error
    if not isinstance(value, dict):
        raise ManifestError(f"manifest root must be an object: {path}")
    return value


def first_difference(
    expected: list[dict[str, Any]], actual: list[dict[str, Any]]
) -> str:
    expected_by_path = {entry.get("path"): entry for entry in expected}
    actual_by_path = {entry.get("path"): entry for entry in actual}
    for path in sorted(set(expected_by_path) | set(actual_by_path)):
        if path not in actual_by_path:
            return f"removed entry {path!r}"
        if path not in expected_by_path:
            return f"added entry {path!r}"
        if expected_by_path[path] != actual_by_path[path]:
            return f"changed entry {path!r}"
    return "manifest metadata changed"


def create(args: argparse.Namespace) -> None:
    root = canonical_root(args.root)
    excluded_roots = validate_excluded_roots(args.exclude_root)
    destination = args.manifest.resolve(strict=False)
    if path_is_within(destination, root):
        raise ManifestError("manifest output must be outside the workspace root")
    value = manifest(root, excluded_roots)
    write_manifest(destination, value)
    print(
        f"workspace-manifest: recorded {len(value['entries'])} entries from {root}"
    )


def verify(args: argparse.Namespace) -> None:
    root = canonical_root(args.root)
    recorded = read_manifest(args.manifest)
    if recorded.get("format_version") != FORMAT_VERSION:
        raise ManifestError("unsupported workspace manifest format version")
    if recorded.get("workspace_root") != str(root):
        raise ManifestError(
            "workspace root does not match the root recorded in the manifest"
        )
    raw_excluded = recorded.get("excluded_roots")
    if not isinstance(raw_excluded, list) or not all(
        isinstance(value, str) for value in raw_excluded
    ):
        raise ManifestError("manifest excluded_roots must be a string array")
    excluded_roots = validate_excluded_roots(raw_excluded)
    if raw_excluded != excluded_roots:
        raise ManifestError("manifest excluded_roots are not canonical")
    expected_entries = recorded.get("entries")
    if not isinstance(expected_entries, list) or not all(
        isinstance(entry, dict) for entry in expected_entries
    ):
        raise ManifestError("manifest entries must be an object array")

    actual = manifest(root, excluded_roots)
    if recorded != actual:
        raise ManifestError(
            "workspace changed after dogfood: "
            + first_difference(expected_entries, actual["entries"])
        )
    print(f"workspace-manifest: unchanged ({len(expected_entries)} entries)")


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    create_parser = commands.add_parser("create")
    create_parser.add_argument("root", type=Path)
    create_parser.add_argument("manifest", type=Path)
    create_parser.add_argument(
        "--exclude-root",
        action="append",
        default=[],
        help="exclude one ordinary generated directory directly under ROOT",
    )
    create_parser.set_defaults(action=create)
    verify_parser = commands.add_parser("verify")
    verify_parser.add_argument("root", type=Path)
    verify_parser.add_argument("manifest", type=Path)
    verify_parser.set_defaults(action=verify)
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        args.action(args)
    except ManifestError as error:
        print(f"workspace-manifest: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
