import hashlib
import json
import os
import subprocess
import sys
import tempfile
from pathlib import Path

import jsonschema


# This gate validates emitted document shapes, not timeout calibration. The
# first isolated Dune execution may include a cold compiler/antivirus start on
# Windows, so keep its explicit mutation timeout comfortably above that cost.
FIXTURE_TIMEOUT_SECONDS = "60"


PINNED_STRYKER_SCHEMA = {
    "name": "mutation-testing-report-schema",
    "version": "2.0.5",
    "repository": "https://github.com/stryker-mutator/mutation-testing-elements",
    "tag": "v2.0.5",
    "commit": "8c3f6c7d34953aa2758a514728e78866e7b9c269",
    "schema": {
        "path": "packages/report-schema/src/mutation-testing-report-schema.json",
        "source": (
            "https://raw.githubusercontent.com/stryker-mutator/"
            "mutation-testing-elements/"
            "8c3f6c7d34953aa2758a514728e78866e7b9c269/"
            "packages/report-schema/src/mutation-testing-report-schema.json"
        ),
        "sha256": (
            "404575e9264686dbf85c399f6531fdb489bebc3146e7e1d793b8083bc6a041ae"
        ),
    },
    "license": {
        "spdx": "Apache-2.0",
        "path": "LICENSE",
        "source": (
            "https://raw.githubusercontent.com/stryker-mutator/"
            "mutation-testing-elements/"
            "8c3f6c7d34953aa2758a514728e78866e7b9c269/LICENSE"
        ),
        "sha256": (
            "c71d239df91726fc519c6eb72d318ec65820627232b2f796219e87dcf35d0ab4"
        ),
    },
}


def sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def checked_validator(schema):
    validator_type = jsonschema.validators.validator_for(schema)
    validator_type.check_schema(schema)
    return validator_type(schema)


def read_pinned_stryker_schema(schema_path, license_path, provenance_path):
    provenance = json.loads(provenance_path.read_text(encoding="utf-8"))
    if provenance != PINNED_STRYKER_SCHEMA:
        raise AssertionError(
            f"unexpected official Stryker schema provenance in {provenance_path}"
        )
    if sha256(schema_path) != provenance["schema"]["sha256"]:
        raise AssertionError(
            f"official Stryker schema checksum mismatch: {schema_path}"
        )
    if sha256(license_path) != provenance["license"]["sha256"]:
        raise AssertionError(
            f"official Stryker license checksum mismatch: {license_path}"
        )
    return json.loads(schema_path.read_text(encoding="utf-8"))


def run(command, cwd, env, accepted_returncodes=(0,)):
    completed = subprocess.run(
        command,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if completed.returncode not in accepted_returncodes:
        raise AssertionError(
            f"command failed ({completed.returncode}): {' '.join(command)}\n"
            f"stdout:\n{completed.stdout}\nstderr:\n{completed.stderr}"
        )
    return json.loads(completed.stdout), completed.returncode


def run_jsonl(command, cwd, env):
    completed = subprocess.run(
        command,
        cwd=cwd,
        env=env,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if completed.returncode != 0:
        raise AssertionError(
            f"command failed ({completed.returncode}): {' '.join(command)}\n"
            f"stdout:\n{completed.stdout}\nstderr:\n{completed.stderr}"
        )
    return [json.loads(line) for line in completed.stdout.splitlines() if line]


def main():
    cli = str(Path(sys.argv[1]).resolve())
    fixture_project = Path(sys.argv[2]).resolve()
    fixture = str(fixture_project.parent)
    catalog_schema = json.loads(Path(sys.argv[3]).read_text(encoding="utf-8"))
    report_schema = json.loads(Path(sys.argv[4]).read_text(encoding="utf-8"))
    check_schema = json.loads(Path(sys.argv[5]).read_text(encoding="utf-8"))
    event_schema = json.loads(Path(sys.argv[6]).read_text(encoding="utf-8"))
    shard_schema = json.loads(Path(sys.argv[7]).read_text(encoding="utf-8"))
    stryker_surface_schema = json.loads(
        Path(sys.argv[8]).read_text(encoding="utf-8")
    )
    official_stryker_schema = read_pinned_stryker_schema(
        Path(sys.argv[9]), Path(sys.argv[10]), Path(sys.argv[11])
    )
    stryker_fixture = json.loads(Path(sys.argv[12]).read_text(encoding="utf-8"))
    catalog_validator = checked_validator(catalog_schema)
    report_validator = checked_validator(report_schema)
    check_validator = checked_validator(check_schema)
    event_validator = checked_validator(event_schema)
    shard_validator = checked_validator(shard_schema)
    stryker_surface_validator = checked_validator(stryker_surface_schema)
    official_stryker_validator = checked_validator(official_stryker_schema)
    stryker_validators = (
        official_stryker_validator,
        stryker_surface_validator,
    )

    progress_event = {
        "document_type": "ocaml-mutants.event-v1",
        "schema_version": 1,
        "sequence": 0,
        "timestamp": "20260101T000000Z",
        "type": "progress",
        "payload": {
            "phase": "mutation",
            "completed": 1,
            "total": 2,
            "workers": 1,
            "cache_hits": 0,
            "resume_hits": 0,
            "elapsed_seconds": 0.5,
            "eta_seconds": 0.5,
        },
    }
    event_validator.validate(progress_event)
    invalid_events = [
        {
            **progress_event,
            "payload": {**progress_event["payload"], "unexpected": True},
        },
        {
            **progress_event,
            "payload": {
                key: value
                for key, value in progress_event["payload"].items()
                if key != "workers"
            },
        },
        {**progress_event, "type": "warning"},
    ]
    for invalid_event in invalid_events:
        try:
            event_validator.validate(invalid_event)
        except jsonschema.exceptions.ValidationError:
            pass
        else:
            raise AssertionError("event-v1 accepted a non-strict payload")

    for validator in stryker_validators:
        validator.validate(stryker_fixture)

    # The upstream schema deliberately accepts ecosystem metadata that this
    # emitter does not produce. Keep the local emitted-surface contract strict.
    official_only = {**stryker_fixture, "framework": {"name": "schema witness"}}
    official_stryker_validator.validate(official_only)
    try:
        stryker_surface_validator.validate(official_only)
    except jsonschema.exceptions.ValidationError:
        pass
    else:
        raise AssertionError(
            "the local Stryker emitted-surface schema is no longer strict"
        )

    with tempfile.TemporaryDirectory(prefix="ocaml-mutants-schema-cache-") as cache:
        env = os.environ.copy()
        env["LOCALAPPDATA"] = cache
        env["XDG_CACHE_HOME"] = cache
        catalog, catalog_exit = run(
            [cli, "list", "--json", "--no-color"], fixture, env
        )
        assert catalog_exit == 0
        catalog_validator.validate(catalog)

        report, report_exit = run(
            [
                cli,
                "run",
                "--fresh",
                "--jobs",
                "1",
                "--timeout",
                FIXTURE_TIMEOUT_SECONDS,
                "--json",
                "--no-color",
                "--",
                "dune",
                "exec",
                "./check.exe",
            ],
            fixture,
            env,
        )
        assert report_exit == 0
        report_validator.validate(report)
        summary = report["summary"]
        assert summary["total"] == summary["executed"] + summary["not_run"]
        # This fixture run completes, so every recorded timeout is confirmed.
        assert summary["unconfirmed_timeouts"] == 0
        assert summary["detected"] == summary["killed"] + summary["timeout"] - summary["unconfirmed_timeouts"]
        scoreable = summary["detected"] + summary["unexpected_survivors"]
        if scoreable == 0:
            assert summary["score"] is None
        else:
            derived = 100.0 * summary["detected"] / scoreable
            assert abs(summary["score"] - derived) < 1e-9

        check, check_exit = run(
            [cli, "check", "--json"],
            fixture,
            env,
            accepted_returncodes=(0, 1, 2),
        )
        check_validator.validate(check)
        if report["evidence"]["level"] == "estimated":
            expected_check_exit = 2
        elif summary["unexpected_survivors"] > 0:
            expected_check_exit = 1
        else:
            expected_check_exit = 0
        assert check["exit_code"] == expected_check_exit
        assert check_exit == expected_check_exit

        plan, plan_exit = run([cli, "plan", "--shards", "2"], fixture, env)
        assert plan_exit == 0
        shard_validator.validate(plan)

        events = run_jsonl(
            [
                cli,
                "run",
                "--fresh",
                "--jobs",
                "1",
                "--timeout",
                FIXTURE_TIMEOUT_SECONDS,
                "--events",
                "jsonl",
                "--",
                "dune",
                "exec",
                "./check.exe",
            ],
            fixture,
            env,
        )
        assert events
        for sequence, event in enumerate(events):
            event_validator.validate(event)
            assert event["sequence"] == sequence

        stryker_report, stryker_exit = run(
            [
                cli,
                "run",
                "--fresh",
                "--jobs",
                "1",
                "--timeout",
                FIXTURE_TIMEOUT_SECONDS,
                "--stryker-json",
                "--threshold-high",
                "80",
                "--threshold-low",
                "60",
                "--no-color",
                "--",
                "dune",
                "exec",
                "./check.exe",
            ],
            fixture,
            env,
        )
        assert stryker_exit == 0
        for validator in stryker_validators:
            validator.validate(stryker_report)
        paths = list(stryker_report["files"])
        assert paths == sorted(paths)
        for file_report in stryker_report["files"].values():
            mutant_ids = [mutant["id"] for mutant in file_report["mutants"]]
            assert mutant_ids == sorted(mutant_ids)

        native_after_projection, native_exit = run(
            [cli, "report", "--json", "--no-color"], fixture, env
        )
        assert native_exit == 0
        report_validator.validate(native_after_projection)
        native_ids = {
            result["mutant"]["full_id"]
            for result in native_after_projection["mutants"]
        } | {
            mutant["full_id"] for mutant in native_after_projection["not_run"]
        }
        projected_ids = {
            mutant["id"]
            for file_report in stryker_report["files"].values()
            for mutant in file_report["mutants"]
        }
        assert native_ids == projected_ids


if __name__ == "__main__":
    main()
