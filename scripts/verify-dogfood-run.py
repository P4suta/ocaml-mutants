#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Verify the authoritative native report from a complete Balanced dogfood run."""

from __future__ import annotations

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path
from typing import Any

import jsonschema


FULL_ID = re.compile(r"[0-9a-f]{64}\Z")
OUTCOMES = {"killed", "survived", "timeout", "inconclusive", "error"}


class ReportError(ValueError):
    pass


def unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise ReportError(f"duplicate JSON object key: {key!r}")
        result[key] = value
    return result


def reject_constant(value: str) -> None:
    raise ReportError(f"non-finite JSON number: {value}")


def read_report(path: Path) -> dict[str, Any]:
    try:
        value = json.loads(
            path.read_text(encoding="utf-8-sig"),
            object_pairs_hook=unique_object,
            parse_constant=reject_constant,
        )
    except (OSError, UnicodeError, json.JSONDecodeError, ReportError) as error:
        raise ReportError(f"cannot decode {path}: {error}") from error
    if not isinstance(value, dict):
        raise ReportError("report root must be an object")
    return value


def validate_schema(report: dict[str, Any], schema_path: Path) -> None:
    try:
        schema = json.loads(
            schema_path.read_text(encoding="utf-8-sig"),
            object_pairs_hook=unique_object,
            parse_constant=reject_constant,
        )
        if not isinstance(schema, dict):
            raise ReportError("schema root must be an object")
        validator_type = jsonschema.validators.validator_for(schema)
        validator_type.check_schema(schema)
        validator_type(schema).validate(report)
    except (
        OSError,
        UnicodeError,
        json.JSONDecodeError,
        jsonschema.exceptions.SchemaError,
        jsonschema.exceptions.ValidationError,
    ) as error:
        raise ReportError(
            f"native report does not validate against {schema_path}: {error}"
        ) from error


def object_field(value: dict[str, Any], name: str) -> dict[str, Any]:
    field = value.get(name)
    if not isinstance(field, dict):
        raise ReportError(f"{name} must be an object")
    return field


def array_field(value: dict[str, Any], name: str) -> list[Any]:
    field = value.get(name)
    if not isinstance(field, list):
        raise ReportError(f"{name} must be an array")
    return field


def integer_field(value: dict[str, Any], name: str) -> int:
    field = value.get(name)
    if isinstance(field, bool) or not isinstance(field, int) or field < 0:
        raise ReportError(f"{name} must be a non-negative integer")
    return field


def mutant_id(result: dict[str, Any], index: int) -> str:
    mutant = object_field(result, "mutant")
    full_id = mutant.get("full_id")
    if not isinstance(full_id, str) or FULL_ID.fullmatch(full_id) is None:
        raise ReportError(f"mutants[{index}] has an invalid full mutant ID")
    return full_id


def verify_report(report: dict[str, Any], exit_code: int) -> tuple[int, int, int]:
    if report.get("document_type") != "ocaml-mutants.run-report-v1":
        raise ReportError("unexpected document_type")
    if report.get("schema_version") != 1:
        raise ReportError("unexpected schema_version")
    run_id = report.get("run_id")
    if not isinstance(run_id, str) or not run_id:
        raise ReportError("run_id must be a non-empty string")
    if report.get("status") != "completed" or report.get("failure") is not None:
        raise ReportError("dogfood did not produce a completed failure-free report")
    if report.get("profile") != "balanced":
        raise ReportError("dogfood report did not record the Balanced profile")
    if object_field(report, "cache").get("mode") != "on":
        raise ReportError("dogfood report did not record the explicit cache=on policy")
    if exit_code != 0:
        raise ReportError(f"dogfood CLI exited {exit_code}, expected 0")

    summary = object_field(report, "summary")
    if summary.get("kind") != "complete":
        raise ReportError("dogfood summary is partial")
    mutants = array_field(report, "mutants")
    not_run = array_field(report, "not_run")
    expectations = array_field(report, "expectations")
    if not mutants:
        raise ReportError("Balanced dogfood executed no mutants")
    if not_run:
        raise ReportError(f"dogfood left {len(not_run)} mutants not run")

    counts: Counter[str] = Counter()
    seen_ids: set[str] = set()
    unexpected: list[str] = []
    rejected_expectations: list[str] = []
    cached = 0
    for index, raw_result in enumerate(mutants):
        if not isinstance(raw_result, dict):
            raise ReportError(f"mutants[{index}] must be an object")
        full_id = mutant_id(raw_result, index)
        if full_id in seen_ids:
            raise ReportError(f"duplicate result for mutant {full_id}")
        seen_ids.add(full_id)
        outcome = raw_result.get("outcome")
        if outcome not in OUTCOMES:
            raise ReportError(f"mutant {full_id} has invalid outcome {outcome!r}")
        counts[outcome] += 1
        if raw_result.get("cached") is True:
            cached += 1
        elif raw_result.get("cached") is not False:
            raise ReportError(f"mutant {full_id} has a non-boolean cached field")
        expected = raw_result.get("expected_survivor")
        if not isinstance(expected, bool):
            raise ReportError(f"mutant {full_id} has invalid expected_survivor")
        expectation = raw_result.get("expectation")
        if expectation is not None and not isinstance(expectation, dict):
            raise ReportError(f"mutant {full_id} has invalid expectation evidence")
        expectation_status = (
            expectation.get("status") if isinstance(expectation, dict) else None
        )
        if expected:
            if outcome != "survived" or expectation_status != "fulfilled":
                rejected_expectations.append(full_id)
        elif outcome == "survived":
            unexpected.append(full_id)
        if expectation_status not in {None, "fulfilled"}:
            rejected_expectations.append(full_id)
        if outcome == "timeout" and raw_result.get("timeout_confirmed") is not True:
            raise ReportError(f"mutant {full_id} has an unconfirmed timeout")

    ledger_failures: list[str] = []
    for index, raw_expectation in enumerate(expectations):
        if not isinstance(raw_expectation, dict):
            raise ReportError(f"expectations[{index}] must be an object")
        full_id = raw_expectation.get("mutant_id")
        if not isinstance(full_id, str) or FULL_ID.fullmatch(full_id) is None:
            raise ReportError(f"expectations[{index}] has an invalid mutant_id")
        if raw_expectation.get("status") != "fulfilled":
            ledger_failures.append(full_id)

    detected = counts["killed"] + counts["timeout"]
    summary_expected = {
        "total": len(mutants),
        "executed": len(mutants),
        "not_run": 0,
        "killed": counts["killed"],
        "survived": counts["survived"],
        "timeout": counts["timeout"],
        "inconclusive": counts["inconclusive"],
        "error": counts["error"],
        "expected_survivors": sum(
            1 for result in mutants if result.get("expected_survivor") is True
        ),
        "unexpected_survivors": len(unexpected),
        "unfulfilled_expectations": 0,
        "detected": detected,
    }
    for name, expected in summary_expected.items():
        actual = integer_field(summary, name)
        if actual != expected:
            raise ReportError(
                f"summary.{name}={actual} contradicts derived value {expected}"
            )
    scoreable = detected + len(unexpected)
    score = summary.get("score")
    if scoreable == 0:
        if score is not None:
            raise ReportError(
                f"summary.score={score!r} contradicts the empty scoreable set"
            )
    else:
        derived_score = 100.0 * detected / scoreable
        if (
            isinstance(score, bool)
            or not isinstance(score, (int, float))
            or abs(score - derived_score) > 1e-9
        ):
            raise ReportError(
                f"summary.score={score!r} contradicts derived value {derived_score}"
            )

    if counts["inconclusive"]:
        raise ReportError(f"dogfood has {counts['inconclusive']} inconclusive results")
    if counts["error"]:
        raise ReportError(f"dogfood has {counts['error']} mutant errors")
    if unexpected:
        raise ReportError(
            f"dogfood has {len(unexpected)} unexpected survivors; first={unexpected[0]}"
        )
    if rejected_expectations:
        raise ReportError(
            "dogfood has unfulfilled or contradictory expectation evidence; "
            f"first={rejected_expectations[0]}"
        )
    if ledger_failures:
        raise ReportError(
            "dogfood has a stale, unevaluated, or unfulfilled expectation; "
            f"first={ledger_failures[0]}"
        )

    for index, raw_skip in enumerate(array_field(report, "skips")):
        if not isinstance(raw_skip, dict):
            raise ReportError(f"skips[{index}] must be an object")
        count = integer_field(raw_skip, "count")
        examples = array_field(raw_skip, "examples")
        if not examples or not all(isinstance(example, str) for example in examples):
            raise ReportError(f"skips[{index}] lacks concrete string examples")
        if examples != sorted(set(examples)):
            raise ReportError(f"skips[{index}] examples are not sorted and unique")
        if count < len(examples):
            raise ReportError(f"skips[{index}] count is smaller than its evidence set")

    warnings = array_field(report, "warnings")
    return len(mutants), cached, len(warnings)


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("report", type=Path)
    result.add_argument("--schema", type=Path, required=True)
    result.add_argument("--exit-code", type=int, required=True)
    return result


def main() -> int:
    args = parser().parse_args()
    report: dict[str, Any] | None = None
    try:
        report = read_report(args.report)
        validate_schema(report, args.schema)
        mutants, cached, warnings = verify_report(report, args.exit_code)
    except ReportError as error:
        run_id = report.get("run_id") if report is not None else None
        label = f"dogfood run {run_id}" if isinstance(run_id, str) else "dogfood"
        print(f"{label}: {error}", file=sys.stderr)
        if isinstance(run_id, str) and run_id:
            print(
                f"dogfood: inspect with `ocaml-mutants report {run_id} --json`",
                file=sys.stderr,
            )
        return 1
    print(
        f"dogfood: accepted run {report.get('run_id')}: "
        f"{mutants} mutants, {cached} cache hits, {warnings} warnings"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
