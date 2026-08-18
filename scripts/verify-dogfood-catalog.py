#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Verify that two emitted mutation catalogs are exactly reproducible."""

from __future__ import annotations

import hashlib
import json
import re
import sys
from pathlib import Path
from typing import Any


FULL_ID = re.compile(r"[0-9a-f]{64}\Z")


class CatalogError(ValueError):
    pass


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise CatalogError(f"duplicate JSON object key: {key!r}")
        result[key] = value
    return result


def reject_constant(value: str) -> None:
    raise CatalogError(f"non-finite JSON number: {value}")


def read_catalog(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(
            path.read_text(encoding="utf-8-sig"),
            object_pairs_hook=unique_object,
            parse_constant=reject_constant,
        )
    except (OSError, UnicodeError, json.JSONDecodeError, CatalogError) as error:
        raise CatalogError(f"cannot decode {path}: {error}") from error
    if not isinstance(value, dict):
        raise CatalogError(f"{path}: catalog root must be an object")
    if value.get("document_type") != "ocaml-mutants.catalog-v1":
        raise CatalogError(f"{path}: unexpected document_type")
    if value.get("schema_version") != 1:
        raise CatalogError(f"{path}: unexpected schema_version")
    return value


def full_ids(path: Path, catalog: dict[str, Any]) -> list[str]:
    mutants = catalog.get("mutants")
    if not isinstance(mutants, list):
        raise CatalogError(f"{path}: mutants must be an array")
    if not mutants:
        raise CatalogError(f"{path}: Balanced catalog is empty")

    ids: list[str] = []
    for index, mutant in enumerate(mutants):
        if not isinstance(mutant, dict):
            raise CatalogError(f"{path}: mutants[{index}] must be an object")
        full_id = mutant.get("full_id")
        if not isinstance(full_id, str) or FULL_ID.fullmatch(full_id) is None:
            raise CatalogError(
                f"{path}: mutants[{index}].full_id is not 64 lowercase hex characters"
            )
        ids.append(full_id)

    if len(ids) != len(set(ids)):
        raise CatalogError(f"{path}: duplicate full mutant IDs")
    return ids


def canonical_json(catalog: dict[str, Any]) -> bytes:
    return json.dumps(
        catalog,
        ensure_ascii=False,
        allow_nan=False,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")


def main() -> int:
    if len(sys.argv) != 3:
        print(
            "usage: verify-dogfood-catalog.py FIRST.json SECOND.json",
            file=sys.stderr,
        )
        return 2

    first_path, second_path = map(Path, sys.argv[1:])
    try:
        first = read_catalog(first_path)
        second = read_catalog(second_path)
        first_ids = full_ids(first_path, first)
        second_ids = full_ids(second_path, second)
        if first_ids != second_ids:
            mismatch = next(
                (
                    index
                    for index, (left, right) in enumerate(
                        zip(first_ids, second_ids)
                    )
                    if left != right
                ),
                min(len(first_ids), len(second_ids)),
            )
            raise CatalogError(
                "ordered full_id sequences differ "
                f"at index {mismatch} ({len(first_ids)} vs {len(second_ids)} IDs)"
            )

        first_canonical = canonical_json(first)
        second_canonical = canonical_json(second)
        if first_canonical != second_canonical:
            raise CatalogError(
                "canonical JSON documents differ although ordered full_id sequences match"
            )
    except CatalogError as error:
        print(f"dogfood-list: {error}", file=sys.stderr)
        return 1

    digest = hashlib.sha256(first_canonical).hexdigest()
    print(
        f"dogfood-list: deterministic Balanced catalog: "
        f"{len(first_ids)} mutants, sha256={digest}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
