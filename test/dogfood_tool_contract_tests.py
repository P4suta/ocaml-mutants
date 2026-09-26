#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Black-box contracts for the local dogfood verification tools."""

from __future__ import annotations

import copy
import json
import subprocess
import sys
import tempfile
from pathlib import Path


def run(*arguments: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, *arguments],
        check=False,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )


def require_success(label: str, completed: subprocess.CompletedProcess[str]) -> None:
    if completed.returncode != 0:
        raise AssertionError(
            f"{label} failed ({completed.returncode})\n"
            f"stdout:\n{completed.stdout}\nstderr:\n{completed.stderr}"
        )


def require_failure(
    label: str, completed: subprocess.CompletedProcess[str], fragment: str
) -> None:
    if completed.returncode == 0 or fragment not in completed.stderr:
        raise AssertionError(
            f"{label} did not fail with {fragment!r}\n"
            f"exit={completed.returncode}\nstdout:\n{completed.stdout}\n"
            f"stderr:\n{completed.stderr}"
        )


def workspace_contract(manifest_tool: str) -> None:
    with tempfile.TemporaryDirectory(prefix="ocaml-mutants-manifest-contract-") as raw:
        parent = Path(raw)
        workspace = parent / "workspace"
        workspace.mkdir()
        source = workspace / "source.ml"
        source.write_text("let value = true\n", encoding="utf-8")
        generated = workspace / "_build"
        generated.mkdir()
        (generated / "state").write_text("before\n", encoding="utf-8")
        manifest = parent / "manifest.json"

        require_success(
            "manifest create",
            run(
                manifest_tool,
                "create",
                str(workspace),
                str(manifest),
                "--exclude-root",
                "_build",
                "--exclude-root",
                "_opam",
            ),
        )
        require_success(
            "unchanged manifest verify",
            run(manifest_tool, "verify", str(workspace), str(manifest)),
        )

        source.write_text("let value = false\n", encoding="utf-8")
        require_failure(
            "source mutation",
            run(manifest_tool, "verify", str(workspace), str(manifest)),
            "changed entry 'source.ml'",
        )
        source.write_text("let value = true\n", encoding="utf-8")

        (generated / "state").write_text("after\n", encoding="utf-8")
        require_success(
            "generated root mutation",
            run(manifest_tool, "verify", str(workspace), str(manifest)),
        )

        leaked_cache = workspace / ".ocaml-mutants"
        leaked_cache.mkdir()
        (leaked_cache / "outcome").write_text("leak\n", encoding="utf-8")
        require_failure(
            "workspace-local cache leak",
            run(manifest_tool, "verify", str(workspace), str(manifest)),
            "added entry '.ocaml-mutants'",
        )


def captured() -> dict[str, object]:
    return {"contents": "", "truncated": False, "total_bytes": 0}


def valid_report() -> dict[str, object]:
    full_id = "a" * 64
    mutant = {
        "id": "a" * 20,
        "full_id": full_id,
        "path": "source.ml",
        "range": {
            "start_byte": 12,
            "end_byte": 16,
            "start_line": 1,
            "start_column": 12,
            "end_line": 1,
            "end_column": 16,
        },
        "family": "boolean-literal",
        "rule": "boolean-literal-true-to-false@1",
        "original": "true",
        "replacement": "false",
        "source_digest": "b" * 64,
        "lineage_id": "d" * 64,
    }
    stage = {"name": "fast", "status": "exited 1", "duration_seconds": 0.1}
    attempt = {
        "outcome": "killed",
        "error": None,
        "duration_seconds": 0.1,
        "stages": [stage],
        "stdout": captured(),
        "stderr": captured(),
    }
    return {
        "document_type": "ocaml-mutants.run-report-v2",
        "schema_version": 2,
        "run_id": "contract-run",
        "status": "completed",
        "started_at": "2026-01-01T00:00:00Z",
        "finished_at": "2026-01-01T00:00:01Z",
        "workspace": {"digest": "c" * 64, "toolchain": "contract"},
        "resolved_config": {
            "version": 2,
            "mutation": {
                "profile": "balanced",
                "include": ["**/*.ml"],
                "exclude": ["_build/**"],
                "operators": [],
                "expectations": [],
            },
            "test": {
                "driver": "command",
                "command": ["dune", "build"],
                "stages": [
                    {
                        "name": "fast",
                        "command": ["dune", "build"],
                    }
                ],
                "timeout_seconds": 10.0,
                "baseline_runs": 1,
                "parallel_safe": False,
                "external_inputs": [],
                "reproducible": True,
            },
            "execution": {"mode": "strict", "jobs": 1},
            "cache": {
                "mode": "on",
                "directory": None,
                "historical_reuse": "off",
            },
            "policy": {
                "require_complete": True,
                "max_unexpected_survivors": 0,
                "minimum_score": None,
                "maximum_score_drop": None,
                "allow_estimated": False,
            },
            "report": {"formats": ["terminal", "json"], "directory": None},
            "privacy": {
                "stdout_limit_bytes": 1048576,
                "stderr_limit_bytes": 1048576,
                "redactions": [],
                "source_embedding": "context",
            },
        },
        "input_fingerprint": {
            "digest": "e" * 64,
            "workspace_digest": "c" * 64,
            "toolchain": "contract",
            "config_digest": "f" * 64,
        },
        "profile": "balanced",
        "selection": {"description": "all"},
        "test": {
            "command": ["dune", "build"],
            "baseline_duration_seconds": 0.1,
            "timeout_seconds": 10.0,
            "stages": [
                {
                    "name": "fast",
                    "command": ["dune", "build"],
                    "baseline_runs_seconds": [0.1],
                    "slowest_baseline_seconds": 0.1,
                }
            ],
            "inventory": {"state": "stage-level", "tests": ["fast"], "hit_map": []},
        },
        "cache": {"mode": "on", "key": "unavailable"},
        "summary": {
            "kind": "complete",
            "total": 1,
            "executed": 1,
            "not_run": 0,
            "killed": 1,
            "survived": 0,
            "timeout": 0,
            "unconfirmed_timeouts": 0,
            "inconclusive": 0,
            "error": 0,
            "expected_survivors": 0,
            "unexpected_survivors": 0,
            "unfulfilled_expectations": 0,
            "detected": 1,
            "score": 100.0,
        },
        "evidence": {
            "level": "executed",
            "complete": True,
            "executed": 1,
            "exact_cache": 0,
            "estimated": 0,
            "checkpointed": 1,
            "resumed": 0,
        },
        "mutants": [
            {
                "mutant": mutant,
                "outcome": "killed",
                "error": None,
                "duration_seconds": 0.1,
                "cached": False,
                "evidence": {
                    "level": "executed",
                    "origin": "execution",
                    "estimated": False,
                },
                "attempts": [attempt],
                "killing_test": "fast",
                "coverage": "covered",
                "checkpoint": {"settled": True, "resumed": False},
                "stages": [stage],
                "timeout_confirmed": False,
                "timeout_retry": None,
                "expected_survivor": False,
                "expectation": None,
                "stdout": captured(),
                "stderr": captured(),
            }
        ],
        "not_run": [],
        "expectations": [],
        "failure": None,
        "skips": [
            {
                "reason": "contract evidence",
                "count": 1,
                "examples": ["source.ml:1:0-1:1"],
            }
        ],
        "warnings": [],
    }


def report_contract(report_tool: str, schema: str) -> None:
    with tempfile.TemporaryDirectory(prefix="ocaml-mutants-report-contract-") as raw:
        report_path = Path(raw) / "run.json"
        report = valid_report()
        report_path.write_text(json.dumps(report), encoding="utf-8")
        require_success(
            "accepted report",
            run(
                report_tool,
                str(report_path),
                "--schema",
                schema,
                "--exit-code",
                "0",
            ),
        )

        survivor = copy.deepcopy(report)
        survivor["mutants"][0]["outcome"] = "survived"
        survivor["mutants"][0]["coverage"] = "unknown"
        summary = survivor["summary"]
        summary["killed"] = 0
        summary["survived"] = 1
        summary["unexpected_survivors"] = 1
        summary["detected"] = 0
        summary["score"] = 0.0
        report_path.write_text(json.dumps(survivor), encoding="utf-8")
        require_failure(
            "unexpected survivor",
            run(
                report_tool,
                str(report_path),
                "--schema",
                schema,
                "--exit-code",
                "0",
            ),
            "unexpected survivors",
        )


def main() -> None:
    if len(sys.argv) != 4:
        raise SystemExit(
            "usage: dogfood_tool_contract_tests.py MANIFEST_TOOL REPORT_TOOL SCHEMA"
        )
    workspace_contract(sys.argv[1])
    report_contract(sys.argv[2], sys.argv[3])
    print("dogfood tool contracts: ok")


if __name__ == "__main__":
    main()
