#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

"""Run the manifest-pinned, local ocaml-mutants corpus acceptance gate.

``check`` is side-effect free. Only ``run`` creates the owned OS-cache root,
initializes opam, or contacts the configured Git remotes.
"""

from __future__ import annotations

import argparse
import collections
import dataclasses
import hashlib
import io
import json
import os
import re
import shlex
import shutil
import signal
import stat
import string
import subprocess
import sys
import tempfile
import threading
import uuid
from pathlib import Path, PurePosixPath
from typing import Any, Iterable, Mapping, Sequence

try:
    import tomllib
except ModuleNotFoundError as error:  # pragma: no cover
    raise SystemExit("corpus: Python 3.11 or newer is required") from error

try:
    import jsonschema
except ModuleNotFoundError as error:  # pragma: no cover
    raise SystemExit("corpus: the Python jsonschema package is required") from error


FULL_ID = re.compile(r"[0-9a-f]{64}\Z")
COMMIT = re.compile(r"[0-9a-f]{40}\Z")
SAFE_NAME = re.compile(r"[a-z0-9](?:[a-z0-9.-]*[a-z0-9])?\Z")
VERSION = re.compile(r"[0-9]+(?:\.[0-9]+)+(?:[-+][0-9A-Za-z.-]+)?\Z")
ALLOWED_ARTIFACT_ROOTS = frozenset({"_build", "_opam"})
SELECTIONS = frozenset({"all", "first-per-rule"})
OUTCOMES = frozenset({"killed", "survived", "timeout", "inconclusive", "error"})
REQUIRED_TIMEOUTS = frozenset(
    {
        "git_clone",
        "git_fetch",
        "git_inspect",
        "git_checkout",
        "opam_init",
        "opam_switch_create",
        "opam_inspect",
        "stage0_dependencies",
        "stage0_build",
        "corpus_dependencies",
        "catalog",
        "mutation_run",
        "process_termination",
    }
)


class CorpusError(RuntimeError):
    """A fail-closed corpus configuration or execution error."""


@dataclasses.dataclass(frozen=True)
class MutationConfig:
    jobs: int
    test_timeout_seconds: float
    cache_mode: str


@dataclasses.dataclass(frozen=True)
class Commands:
    opam_init: tuple[str, ...]
    opam_init_windows_extra: tuple[str, ...]
    opam_switch_create: tuple[str, ...]
    stage0_dependencies: tuple[str, ...]
    stage0_build: tuple[str, ...]
    corpus_dependencies: tuple[str, ...]


@dataclasses.dataclass(frozen=True)
class StageZero:
    built_executable: str
    installed_name: str


@dataclasses.dataclass(frozen=True)
class Project:
    name: str
    version: str
    repository: str
    commit: str
    selection: str
    cache_namespace: str
    include: tuple[str, ...]
    test_command: tuple[str, ...]


@dataclasses.dataclass(frozen=True)
class Manifest:
    format_version: int
    profile: str
    catalog_repetitions: int
    sample_count_per_rule: int
    cache_namespace: str
    ownership_id: str
    compiler_package: str
    diagnostic_capture_bytes: int
    structured_output_bytes: int
    mutation: MutationConfig
    command_timeouts_seconds: Mapping[str, float]
    engine_excluded_roots: tuple[str, ...]
    corpus_excluded_roots: tuple[str, ...]
    commands: Commands
    stage0: StageZero
    projects: tuple[Project, ...]


@dataclasses.dataclass(frozen=True)
class ProcessResult:
    returncode: int
    stdout: bytes | None
    stdout_diagnostic: bytes
    stderr_diagnostic: bytes


@dataclasses.dataclass(frozen=True)
class Catalog:
    document: Mapping[str, Any]
    canonical: bytes
    ordered_ids: tuple[str, ...]


def _object(value: Any, label: str) -> dict[str, Any]:
    if not isinstance(value, dict):
        raise CorpusError(f"{label} must be a table")
    return value


def _keys(table: Mapping[str, Any], label: str, expected: set[str]) -> None:
    actual = set(table)
    missing = sorted(expected - actual)
    unknown = sorted(actual - expected)
    if missing:
        raise CorpusError(f"{label} is missing required keys: {', '.join(missing)}")
    if unknown:
        raise CorpusError(f"{label} has unknown keys: {', '.join(unknown)}")


def _string(value: Any, label: str) -> str:
    if not isinstance(value, str) or not value or value.strip() != value:
        raise CorpusError(f"{label} must be a non-empty, trimmed string")
    return value


def _safe_name(value: Any, label: str) -> str:
    result = _string(value, label)
    if SAFE_NAME.fullmatch(result) is None:
        raise CorpusError(f"{label} is not a safe cache component: {result!r}")
    return result


def _positive_int(value: Any, label: str) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise CorpusError(f"{label} must be a positive integer")
    return value


def _positive_number(value: Any, label: str) -> float:
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise CorpusError(f"{label} must be a positive finite number")
    result = float(value)
    if not result > 0.0 or result == float("inf"):
        raise CorpusError(f"{label} must be a positive finite number")
    return result


def _string_array(
    value: Any, label: str, *, allow_empty: bool = False
) -> tuple[str, ...]:
    if not isinstance(value, list) or (not value and not allow_empty):
        qualifier = "an array" if allow_empty else "a non-empty array"
        raise CorpusError(f"{label} must be {qualifier} of strings")
    return tuple(
        _string(item, f"{label}[{index}]") for index, item in enumerate(value)
    )


def _artifact_roots(value: Any, label: str) -> tuple[str, ...]:
    result = _string_array(value, label, allow_empty=True)
    if len(result) != len(set(result)):
        raise CorpusError(f"{label} contains duplicate roots")
    unsupported = sorted(set(result) - ALLOWED_ARTIFACT_ROOTS)
    if unsupported:
        raise CorpusError(
            f"{label} may exclude only owned Dune/opam roots; got {unsupported!r}"
        )
    return tuple(sorted(result))


def _command(value: Any, label: str) -> tuple[str, ...]:
    return _string_array(value, label)


def _validate_template(command: Iterable[str], label: str, allowed: set[str]) -> None:
    formatter = string.Formatter()
    for argument in command:
        try:
            fields = [field for _, field, _, _ in formatter.parse(argument) if field]
        except ValueError as error:
            raise CorpusError(f"{label} has an invalid template: {error}") from error
        unknown = sorted(set(fields) - allowed)
        if unknown:
            raise CorpusError(f"{label} has unknown template fields: {unknown!r}")


def _project(value: Any, index: int) -> Project:
    label = f"projects[{index}]"
    table = _object(value, label)
    _keys(
        table,
        label,
        {
            "name",
            "version",
            "repository",
            "commit",
            "selection",
            "cache_namespace",
            "include",
            "test_command",
        },
    )
    name = _safe_name(table["name"], f"{label}.name")
    version = _string(table["version"], f"{label}.version")
    if VERSION.fullmatch(version) is None:
        raise CorpusError(f"{label}.version is not a pinned release version")
    repository = _string(table["repository"], f"{label}.repository")
    if not repository.startswith("https://") or not repository.endswith(".git"):
        raise CorpusError(f"{label}.repository must be an explicit HTTPS .git URL")
    commit = _string(table["commit"], f"{label}.commit")
    if COMMIT.fullmatch(commit) is None:
        raise CorpusError(
            f"{label}.commit must be 40 lowercase hexadecimal characters"
        )
    selection = _string(table["selection"], f"{label}.selection")
    if selection not in SELECTIONS:
        raise CorpusError(f"{label}.selection must be one of {sorted(SELECTIONS)!r}")
    include = _string_array(table["include"], f"{label}.include")
    if len(include) != len(set(include)):
        raise CorpusError(f"{label}.include contains duplicate patterns")
    return Project(
        name=name,
        version=version,
        repository=repository,
        commit=commit,
        selection=selection,
        cache_namespace=_safe_name(
            table["cache_namespace"], f"{label}.cache_namespace"
        ),
        include=include,
        test_command=_command(table["test_command"], f"{label}.test_command"),
    )


def load_manifest(path: Path) -> Manifest:
    try:
        with path.open("rb") as source:
            raw = tomllib.load(source)
    except (OSError, tomllib.TOMLDecodeError) as error:
        raise CorpusError(f"cannot read corpus manifest {path}: {error}") from error
    root = _object(raw, "manifest")
    _keys(
        root,
        "manifest",
        {
            "format_version",
            "profile",
            "catalog_repetitions",
            "sample_count_per_rule",
            "cache_namespace",
            "ownership_id",
            "compiler_package",
            "diagnostic_capture_bytes",
            "structured_output_bytes",
            "mutation",
            "command_timeouts_seconds",
            "artifacts",
            "commands",
            "stage0",
            "projects",
        },
    )
    format_version = _positive_int(root["format_version"], "format_version")
    if format_version != 1:
        raise CorpusError(
            f"unsupported corpus manifest format_version {format_version}"
        )
    profile = _string(root["profile"], "profile")
    if profile != "balanced":
        raise CorpusError("the pinned corpus acceptance profile must be balanced")
    repetitions = _positive_int(root["catalog_repetitions"], "catalog_repetitions")
    if repetitions < 2:
        raise CorpusError("catalog_repetitions must be at least two")

    mutation_table = _object(root["mutation"], "mutation")
    _keys(mutation_table, "mutation", {"jobs", "test_timeout_seconds", "cache_mode"})
    cache_mode = _string(mutation_table["cache_mode"], "mutation.cache_mode")
    if cache_mode != "on":
        raise CorpusError("corpus acceptance requires mutation.cache_mode = 'on'")
    mutation = MutationConfig(
        jobs=_positive_int(mutation_table["jobs"], "mutation.jobs"),
        test_timeout_seconds=_positive_number(
            mutation_table["test_timeout_seconds"], "mutation.test_timeout_seconds"
        ),
        cache_mode=cache_mode,
    )

    timeout_table = _object(
        root["command_timeouts_seconds"], "command_timeouts_seconds"
    )
    _keys(timeout_table, "command_timeouts_seconds", set(REQUIRED_TIMEOUTS))
    timeouts = {
        name: _positive_number(value, f"command_timeouts_seconds.{name}")
        for name, value in timeout_table.items()
    }

    artifacts = _object(root["artifacts"], "artifacts")
    _keys(
        artifacts,
        "artifacts",
        {"engine_excluded_roots", "corpus_excluded_roots"},
    )
    commands_table = _object(root["commands"], "commands")
    command_names = {
        "opam_init",
        "opam_init_windows_extra",
        "opam_switch_create",
        "stage0_dependencies",
        "stage0_build",
        "corpus_dependencies",
    }
    _keys(commands_table, "commands", command_names)
    commands = Commands(
        opam_init=_command(commands_table["opam_init"], "commands.opam_init"),
        opam_init_windows_extra=_string_array(
            commands_table["opam_init_windows_extra"],
            "commands.opam_init_windows_extra",
            allow_empty=True,
        ),
        opam_switch_create=_command(
            commands_table["opam_switch_create"], "commands.opam_switch_create"
        ),
        stage0_dependencies=_command(
            commands_table["stage0_dependencies"], "commands.stage0_dependencies"
        ),
        stage0_build=_command(
            commands_table["stage0_build"], "commands.stage0_build"
        ),
        corpus_dependencies=_command(
            commands_table["corpus_dependencies"], "commands.corpus_dependencies"
        ),
    )
    _validate_template(commands.opam_init, "commands.opam_init", set())
    _validate_template(
        commands.opam_init_windows_extra,
        "commands.opam_init_windows_extra",
        set(),
    )
    template_fields = {"switch", "compiler_package", "stage0_build_dir"}
    for label, command in (
        ("commands.opam_switch_create", commands.opam_switch_create),
        ("commands.stage0_dependencies", commands.stage0_dependencies),
        ("commands.stage0_build", commands.stage0_build),
        ("commands.corpus_dependencies", commands.corpus_dependencies),
    ):
        _validate_template(command, label, template_fields)

    stage0_table = _object(root["stage0"], "stage0")
    _keys(stage0_table, "stage0", {"built_executable", "installed_name"})
    stage0 = StageZero(
        built_executable=_string(
            stage0_table["built_executable"], "stage0.built_executable"
        ),
        installed_name=_safe_name(
            stage0_table["installed_name"], "stage0.installed_name"
        ),
    )
    _validate_template(
        (stage0.built_executable,), "stage0.built_executable", template_fields
    )

    project_values = root["projects"]
    if not isinstance(project_values, list) or not project_values:
        raise CorpusError("projects must be a non-empty array of tables")
    projects = tuple(
        _project(value, index) for index, value in enumerate(project_values)
    )
    for field, values in (
        ("name", [project.name for project in projects]),
        ("repository", [project.repository for project in projects]),
        ("cache_namespace", [project.cache_namespace for project in projects]),
    ):
        if len(values) != len(set(values)):
            raise CorpusError(f"projects contain duplicate {field} values")

    return Manifest(
        format_version=format_version,
        profile=profile,
        catalog_repetitions=repetitions,
        sample_count_per_rule=_positive_int(
            root["sample_count_per_rule"], "sample_count_per_rule"
        ),
        cache_namespace=_safe_name(root["cache_namespace"], "cache_namespace"),
        ownership_id=_safe_name(root["ownership_id"], "ownership_id"),
        compiler_package=_string(root["compiler_package"], "compiler_package"),
        diagnostic_capture_bytes=_positive_int(
            root["diagnostic_capture_bytes"], "diagnostic_capture_bytes"
        ),
        structured_output_bytes=_positive_int(
            root["structured_output_bytes"], "structured_output_bytes"
        ),
        mutation=mutation,
        command_timeouts_seconds=timeouts,
        engine_excluded_roots=_artifact_roots(
            artifacts["engine_excluded_roots"], "artifacts.engine_excluded_roots"
        ),
        corpus_excluded_roots=_artifact_roots(
            artifacts["corpus_excluded_roots"], "artifacts.corpus_excluded_roots"
        ),
        commands=commands,
        stage0=stage0,
        projects=projects,
    )


def render_command(command: Sequence[str], values: Mapping[str, str]) -> list[str]:
    try:
        return [argument.format_map(values) for argument in command]
    except KeyError as error:
        raise CorpusError(
            f"command references unavailable template field {error}"
        ) from error


def select_mutants(
    mutants: Sequence[Mapping[str, Any]], selection: str, sample_count: int
) -> tuple[str, ...]:
    """Return deterministic full IDs for one manifest selection policy."""

    if selection not in SELECTIONS:
        raise CorpusError(f"unsupported selection policy {selection!r}")
    if sample_count <= 0:
        raise CorpusError("sample_count must be positive")
    by_rule: dict[str, list[str]] = collections.defaultdict(list)
    catalog_order: list[str] = []
    seen: set[str] = set()
    for index, mutant in enumerate(mutants):
        if not isinstance(mutant, Mapping):
            raise CorpusError(f"mutants[{index}] must be an object")
        full_id = mutant.get("full_id")
        rule = mutant.get("rule")
        if not isinstance(full_id, str) or FULL_ID.fullmatch(full_id) is None:
            raise CorpusError(f"mutants[{index}].full_id is invalid")
        if not isinstance(rule, str) or not rule:
            raise CorpusError(f"mutants[{index}].rule is invalid")
        if full_id in seen:
            raise CorpusError(f"catalog contains duplicate mutant ID {full_id}")
        seen.add(full_id)
        catalog_order.append(full_id)
        by_rule[rule].append(full_id)
    if not catalog_order:
        raise CorpusError("Balanced catalog is empty")
    if selection == "all":
        return tuple(catalog_order)
    selected: list[str] = []
    for rule in sorted(by_rule):
        selected.extend(sorted(by_rule[rule])[:sample_count])
    return tuple(selected)


def _unique_object(pairs: list[tuple[str, Any]]) -> dict[str, Any]:
    result: dict[str, Any] = {}
    for key, value in pairs:
        if key in result:
            raise CorpusError(f"duplicate JSON object key {key!r}")
        result[key] = value
    return result


def _reject_constant(value: str) -> None:
    raise CorpusError(f"non-finite JSON number {value}")


def decode_json(contents: bytes, label: str) -> Mapping[str, Any]:
    try:
        value = json.loads(
            contents.decode("utf-8-sig"),
            object_pairs_hook=_unique_object,
            parse_constant=_reject_constant,
        )
    except (UnicodeError, json.JSONDecodeError, CorpusError) as error:
        raise CorpusError(f"cannot decode {label}: {error}") from error
    if not isinstance(value, dict):
        raise CorpusError(f"{label} root must be an object")
    return value


def canonical_json(value: Mapping[str, Any]) -> bytes:
    return json.dumps(
        value,
        ensure_ascii=False,
        allow_nan=False,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")


def catalog_from_bytes(
    contents: bytes, label: str, schema: Mapping[str, Any]
) -> Catalog:
    document = decode_json(contents, label)
    try:
        jsonschema.validators.validator_for(schema)(schema).validate(document)
    except jsonschema.exceptions.ValidationError as error:
        raise CorpusError(
            f"{label} does not satisfy catalog-v2: {error.message}"
        ) from error
    if document.get("document_type") != "ocaml-mutants.catalog-v2":
        raise CorpusError(f"{label} has the wrong document_type")
    if document.get("schema_version") != 2:
        raise CorpusError(f"{label} has the wrong schema_version")
    if document.get("profile") != "balanced":
        raise CorpusError(f"{label} is not a Balanced catalog")
    mutants = document.get("mutants")
    if not isinstance(mutants, list):
        raise CorpusError(f"{label}.mutants must be an array")
    ids = select_mutants(mutants, "all", 1)
    return Catalog(
        document=document,
        canonical=canonical_json(document),
        ordered_ids=ids,
    )


def compare_catalogs(catalogs: Sequence[Catalog]) -> Catalog:
    if len(catalogs) < 2:
        raise CorpusError("at least two catalogs are required for determinism proof")
    reference = catalogs[0]
    for index, candidate in enumerate(catalogs[1:], start=2):
        if candidate.ordered_ids != reference.ordered_ids:
            raise CorpusError(
                f"catalog {index} has a different ordered full-ID sequence"
            )
        if candidate.canonical != reference.canonical:
            raise CorpusError(f"catalog {index} has different canonical JSON")
    return reference


def validate_report(
    contents: bytes,
    schema: Mapping[str, Any],
    selected_ids: Sequence[str],
    returncode: int,
) -> Mapping[str, Any]:
    report = decode_json(contents, "native run report")
    try:
        jsonschema.validators.validator_for(schema)(schema).validate(report)
    except jsonschema.exceptions.ValidationError as error:
        raise CorpusError(
            f"native report does not satisfy run-report-v2: {error.message}"
        ) from error
    if returncode != 0:
        raise CorpusError(
            f"mutation run exited {returncode}; expected complete measurement exit 0"
        )
    if report.get("document_type") != "ocaml-mutants.run-report-v2":
        raise CorpusError("native report has the wrong document_type")
    if report.get("schema_version") != 2 or report.get("profile") != "balanced":
        raise CorpusError("native report has the wrong schema version or profile")
    if report.get("status") != "completed" or report.get("failure") is not None:
        raise CorpusError("native report records an infrastructure failure")
    summary = report.get("summary")
    if not isinstance(summary, dict) or summary.get("kind") != "complete":
        raise CorpusError("native report summary is partial")
    for field in ("error", "inconclusive", "not_run"):
        if summary.get(field) != 0:
            raise CorpusError(f"native report summary.{field} must be zero")
    not_run = report.get("not_run")
    if not isinstance(not_run, list) or not_run:
        raise CorpusError("native report contains not-run mutants")
    raw_results = report.get("mutants")
    if not isinstance(raw_results, list):
        raise CorpusError("native report mutants must be an array")
    actual_ids: list[str] = []
    for index, raw_result in enumerate(raw_results):
        if not isinstance(raw_result, dict):
            raise CorpusError(f"native report mutants[{index}] must be an object")
        mutant = raw_result.get("mutant")
        if not isinstance(mutant, dict):
            raise CorpusError(
                f"native report mutants[{index}].mutant must be an object"
            )
        full_id = mutant.get("full_id")
        if not isinstance(full_id, str) or FULL_ID.fullmatch(full_id) is None:
            raise CorpusError(
                f"native report mutants[{index}] has an invalid full ID"
            )
        outcome = raw_result.get("outcome")
        if outcome not in OUTCOMES:
            raise CorpusError(
                f"native report mutant {full_id} has invalid outcome {outcome!r}"
            )
        if outcome in {"error", "inconclusive"}:
            raise CorpusError(f"native report mutant {full_id} is {outcome}")
        if outcome == "timeout" and raw_result.get("timeout_confirmed") is not True:
            raise CorpusError(
                f"native report mutant {full_id} has an unconfirmed timeout"
            )
        actual_ids.append(full_id)
    if len(actual_ids) != len(set(actual_ids)):
        raise CorpusError("native report contains duplicate mutant results")
    if set(actual_ids) != set(selected_ids) or len(actual_ids) != len(selected_ids):
        missing = sorted(set(selected_ids) - set(actual_ids))
        extra = sorted(set(actual_ids) - set(selected_ids))
        raise CorpusError(
            "native report does not cover the exact selection "
            f"(missing={missing[:1]!r}, extra={extra[:1]!r})"
        )
    if (
        summary.get("total") != len(selected_ids)
        or summary.get("executed") != len(selected_ids)
    ):
        raise CorpusError("native report summary does not cover the exact selection")
    return report


class _Capture:
    def __init__(self, diagnostic_limit: int, full_limit: int | None) -> None:
        self._head_limit = diagnostic_limit // 2
        self._tail_limit = diagnostic_limit - self._head_limit
        self._head = bytearray()
        self._tail = bytearray()
        self._full_limit = full_limit
        self._full = bytearray() if full_limit is not None else None
        self.total = 0
        self.overflow = False

    def append(self, chunk: bytes) -> None:
        self.total += len(chunk)
        if self._full is not None:
            assert self._full_limit is not None
            if len(self._full) + len(chunk) <= self._full_limit:
                self._full.extend(chunk)
            else:
                self.overflow = True
        head_room = self._head_limit - len(self._head)
        consumed = min(max(head_room, 0), len(chunk))
        if consumed:
            self._head.extend(chunk[:consumed])
        if self._tail_limit > 0:
            self._tail.extend(chunk[consumed:])
            excess = len(self._tail) - self._tail_limit
            if excess > 0:
                del self._tail[:excess]

    def diagnostic(self) -> bytes:
        if self.total <= len(self._head) + len(self._tail):
            return bytes(self._head) + bytes(self._tail)
        omitted = max(0, self.total - len(self._head) - len(self._tail))
        marker = f"\n... {omitted} bytes omitted ...\n".encode("ascii")
        return bytes(self._head) + marker + bytes(self._tail)

    def full(self) -> bytes | None:
        if self._full is None or self.overflow:
            return None
        return bytes(self._full)


class CommandRunner:
    def __init__(self, manifest: Manifest, session: Path) -> None:
        self.manifest = manifest
        self.session = session

    def _timeout(self, operation: str) -> float:
        try:
            return self.manifest.command_timeouts_seconds[operation]
        except KeyError as error:
            raise CorpusError(
                f"operation {operation!r} has no configured timeout"
            ) from error

    @staticmethod
    def _display(argv: Sequence[str]) -> str:
        if os.name == "nt":
            return subprocess.list2cmdline(argv)
        return shlex.join(argv)

    def _terminate_tree(self, process: subprocess.Popen[bytes]) -> None:
        if process.poll() is not None:
            return
        if os.name == "nt":
            try:
                subprocess.run(
                    ["taskkill", "/PID", str(process.pid), "/T", "/F"],
                    check=False,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                    timeout=self._timeout("process_termination"),
                )
            except (OSError, subprocess.TimeoutExpired):
                process.kill()
        else:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                return
        try:
            process.wait(timeout=self._timeout("process_termination"))
        except subprocess.TimeoutExpired as error:
            raise CorpusError(
                f"process tree {process.pid} did not terminate"
            ) from error

    def run(
        self,
        operation: str,
        argv: Sequence[str],
        *,
        cwd: Path,
        env: Mapping[str, str],
        structured_stdout: bool = False,
        accepted_codes: set[int] | None = None,
    ) -> ProcessResult:
        if not argv:
            raise CorpusError(f"{operation}: empty command")
        print(f"corpus: {operation}: {self._display(argv)}")
        creationflags = (
            subprocess.CREATE_NEW_PROCESS_GROUP if os.name == "nt" else 0
        )
        try:
            process = subprocess.Popen(
                list(argv),
                cwd=cwd,
                env=dict(env),
                stdin=subprocess.DEVNULL,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                creationflags=creationflags,
                start_new_session=os.name != "nt",
            )
        except OSError as error:
            raise CorpusError(f"{operation}: cannot start command: {error}") from error
        stdout_capture = _Capture(
            self.manifest.diagnostic_capture_bytes,
            self.manifest.structured_output_bytes if structured_stdout else None,
        )
        stderr_capture = _Capture(self.manifest.diagnostic_capture_bytes, None)
        reader_errors: list[BaseException] = []

        def drain(stream: Any, capture: _Capture) -> None:
            try:
                while True:
                    chunk = stream.read(io.DEFAULT_BUFFER_SIZE)
                    if not chunk:
                        return
                    capture.append(chunk)
            except BaseException as error:
                reader_errors.append(error)

        assert process.stdout is not None and process.stderr is not None
        readers = [
            threading.Thread(
                target=drain,
                args=(process.stdout, stdout_capture),
                daemon=True,
            ),
            threading.Thread(
                target=drain,
                args=(process.stderr, stderr_capture),
                daemon=True,
            ),
        ]
        for reader in readers:
            reader.start()
        try:
            process.wait(timeout=self._timeout(operation))
        except subprocess.TimeoutExpired as error:
            self._terminate_tree(process)
            raise CorpusError(
                f"{operation}: command exceeded its manifest timeout "
                f"({self._timeout(operation):g}s)"
            ) from error
        finally:
            for reader in readers:
                reader.join(self._timeout("process_termination"))
            process.stdout.close()
            process.stderr.close()
        if any(reader.is_alive() for reader in readers):
            raise CorpusError(f"{operation}: output drain did not terminate")
        if reader_errors:
            raise CorpusError(
                f"{operation}: output drain failed: {reader_errors[0]}"
            )
        if stdout_capture.overflow:
            raise CorpusError(
                f"{operation}: structured stdout exceeded the manifest limit "
                f"({self.manifest.structured_output_bytes} bytes)"
            )
        result = ProcessResult(
            returncode=process.returncode,
            stdout=stdout_capture.full(),
            stdout_diagnostic=stdout_capture.diagnostic(),
            stderr_diagnostic=stderr_capture.diagnostic(),
        )
        if accepted_codes is not None and result.returncode not in accepted_codes:
            diagnostics = (result.stderr_diagnostic or result.stdout_diagnostic).decode(
                "utf-8", errors="replace"
            )
            raise CorpusError(
                f"{operation}: command exited {result.returncode}\n"
                f"{diagnostics.rstrip()}"
            )
        return result

    def text(
        self,
        operation: str,
        argv: Sequence[str],
        *,
        cwd: Path,
        env: Mapping[str, str],
    ) -> str:
        result = self.run(
            operation,
            argv,
            cwd=cwd,
            env=env,
            structured_stdout=True,
            accepted_codes={0},
        )
        assert result.stdout is not None
        try:
            return result.stdout.decode("utf-8").strip()
        except UnicodeDecodeError as error:
            raise CorpusError(f"{operation}: command output is not UTF-8") from error


def _is_reparse(metadata: os.stat_result) -> bool:
    attributes = getattr(metadata, "st_file_attributes", 0)
    flag = getattr(stat, "FILE_ATTRIBUTE_REPARSE_POINT", 0)
    return bool(attributes & flag)


def _reject_link(path: Path, label: str) -> None:
    try:
        metadata = path.lstat()
    except OSError as error:
        raise CorpusError(f"cannot inspect {label} {path}: {error}") from error
    if stat.S_ISLNK(metadata.st_mode) or _is_reparse(metadata):
        raise CorpusError(
            f"refusing {label} through a symlink or reparse point: {path}"
        )


def os_cache_root(
    manifest: Manifest, environ: Mapping[str, str] | None = None
) -> Path:
    env = os.environ if environ is None else environ
    if os.name == "nt":
        raw_base = env.get("LOCALAPPDATA")
        if not raw_base:
            raise CorpusError("LOCALAPPDATA is required for the Windows corpus cache")
        base = Path(raw_base)
    elif sys.platform == "darwin":
        base = Path.home() / "Library" / "Caches"
    else:
        base = Path(env.get("XDG_CACHE_HOME", str(Path.home() / ".cache")))
    if not base.is_absolute():
        raise CorpusError(f"OS cache base is not absolute: {base}")
    return base / "ocaml-mutants" / manifest.cache_namespace


def _marker_bytes(manifest: Manifest) -> bytes:
    return (
        json.dumps(
            {
                "cache_namespace": manifest.cache_namespace,
                "format_version": manifest.format_version,
                "owner": manifest.ownership_id,
            },
            sort_keys=True,
            separators=(",", ":"),
        )
        + "\n"
    ).encode("utf-8")


def atomic_write(path: Path, contents: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    pending = path.parent / f"{path.name}.{uuid.uuid4().hex}.pending"
    try:
        with pending.open("xb") as output:
            output.write(contents)
            output.flush()
            os.fsync(output.fileno())
        os.replace(pending, path)
    except OSError as error:
        raise CorpusError(f"cannot atomically write {path}: {error}") from error
    finally:
        try:
            pending.unlink()
        except FileNotFoundError:
            pass


def ensure_owned_cache_root(root: Path, manifest: Manifest) -> None:
    marker = root / ".ocaml-mutants-corpus-owner"
    expected = _marker_bytes(manifest)
    if root.exists():
        _reject_link(root, "corpus cache root")
        if not root.is_dir():
            raise CorpusError(f"corpus cache root is not a directory: {root}")
        try:
            actual = marker.read_bytes()
        except OSError as error:
            raise CorpusError(
                f"refusing unmarked corpus cache root {root}: {error}"
            ) from error
        if actual != expected:
            raise CorpusError(
                f"refusing corpus cache root with a foreign marker: {root}"
            )
        return
    parent = root.parent
    parent.mkdir(parents=True, exist_ok=True)
    _reject_link(parent, "corpus cache parent")
    try:
        root.mkdir()
    except OSError as error:
        raise CorpusError(f"cannot create corpus cache root {root}: {error}") from error
    atomic_write(marker, expected)


def create_owned_session(root: Path, manifest: Manifest) -> tuple[Path, bytes]:
    sessions = root / "sessions"
    sessions.mkdir(exist_ok=True)
    _reject_link(sessions, "session parent")
    path = Path(tempfile.mkdtemp(prefix="corpus-session-", dir=sessions))
    token = uuid.uuid4().hex
    marker = (
        json.dumps(
            {"owner": manifest.ownership_id, "token": token},
            sort_keys=True,
            separators=(",", ":"),
        )
        + "\n"
    ).encode("utf-8")
    atomic_write(path / ".ocaml-mutants-session-owner", marker)
    return path, marker


def remove_owned_session(root: Path, session: Path, marker_contents: bytes) -> None:
    sessions = (root / "sessions").resolve(strict=True)
    _reject_link(session, "session")
    resolved = session.resolve(strict=True)
    if resolved.parent != sessions or not resolved.name.startswith("corpus-session-"):
        raise CorpusError(f"refusing to remove out-of-scope session {resolved}")
    marker = resolved / ".ocaml-mutants-session-owner"
    try:
        actual = marker.read_bytes()
    except OSError as error:
        raise CorpusError(
            f"refusing to remove unmarked session {resolved}: {error}"
        ) from error
    if actual != marker_contents:
        raise CorpusError(
            f"refusing to remove session with a foreign marker: {resolved}"
        )
    shutil.rmtree(resolved)


def _relative(parent: PurePosixPath, name: str) -> PurePosixPath:
    if str(parent) == ".":
        return PurePosixPath(name)
    return parent / name


def _file_digest(path: Path) -> str:
    digest = hashlib.sha256()
    try:
        with path.open("rb") as source:
            for chunk in iter(lambda: source.read(io.DEFAULT_BUFFER_SIZE), b""):
                digest.update(chunk)
    except OSError as error:
        raise CorpusError(f"cannot hash source entry {path}: {error}") from error
    return digest.hexdigest()


def source_manifest(
    root: Path, excluded_roots: Sequence[str]
) -> tuple[tuple[Any, ...], ...]:
    """Hash source entries, excluding VCS metadata and declared artifacts."""

    root = root.resolve(strict=True)
    if not root.is_dir():
        raise CorpusError(f"source root is not a directory: {root}")
    excluded = set(excluded_roots)
    entries: list[tuple[Any, ...]] = []

    def walk(directory: Path, parent: PurePosixPath) -> None:
        try:
            children = sorted(os.scandir(directory), key=lambda child: child.name)
        except OSError as error:
            raise CorpusError(
                f"cannot enumerate source directory {directory}: {error}"
            ) from error
        for child in children:
            path = Path(child.path)
            relative = _relative(parent, child.name)
            relative_text = relative.as_posix()
            try:
                metadata = child.stat(follow_symlinks=False)
            except OSError as error:
                raise CorpusError(
                    f"cannot inspect source entry {path}: {error}"
                ) from error
            if str(parent) == "." and child.name == ".git":
                if stat.S_ISLNK(metadata.st_mode) or _is_reparse(metadata):
                    raise CorpusError(
                        f"Git metadata is a link or reparse point: {path}"
                    )
                continue
            if str(parent) == "." and child.name in excluded:
                if not stat.S_ISDIR(metadata.st_mode) or _is_reparse(metadata):
                    raise CorpusError(
                        f"owned artifact root is not an ordinary directory: {path}"
                    )
                continue
            mode = stat.S_IMODE(metadata.st_mode)
            if stat.S_ISLNK(metadata.st_mode) or _is_reparse(metadata):
                try:
                    target = os.readlink(path)
                except OSError as error:
                    raise CorpusError(
                        f"cannot read source link {path}: {error}"
                    ) from error
                if os.path.isabs(target):
                    raise CorpusError(
                        f"source contains an absolute link: {path} -> {target}"
                    )
                resolved_target = (path.parent / target).resolve(strict=False)
                try:
                    resolved_target.relative_to(root)
                except ValueError as error:
                    raise CorpusError(
                        f"source link escapes the workspace: {path} -> {target}"
                    ) from error
                entries.append((relative_text, "link", mode, target))
            elif stat.S_ISDIR(metadata.st_mode):
                entries.append((relative_text, "directory", mode))
                walk(path, relative)
            elif stat.S_ISREG(metadata.st_mode):
                entries.append(
                    (
                        relative_text,
                        "file",
                        mode,
                        metadata.st_size,
                        _file_digest(path),
                    )
                )
            else:
                raise CorpusError(f"unsupported source entry type: {path}")

    walk(root, PurePosixPath("."))
    return tuple(entries)


def first_manifest_difference(
    before: Sequence[tuple[Any, ...]], after: Sequence[tuple[Any, ...]]
) -> str:
    before_map = {entry[0]: entry for entry in before}
    after_map = {entry[0]: entry for entry in after}
    for path in sorted(set(before_map) | set(after_map)):
        if path not in after_map:
            return f"removed source entry {path!r}"
        if path not in before_map:
            return f"added source entry {path!r}"
        if before_map[path] != after_map[path]:
            return f"changed source entry {path!r}"
    return "source manifest metadata changed"


def require_unchanged(
    root: Path,
    excluded_roots: Sequence[str],
    before: Sequence[tuple[Any, ...]],
) -> None:
    after = source_manifest(root, excluded_roots)
    if tuple(before) != after:
        raise CorpusError(
            f"source workspace changed: {first_manifest_difference(before, after)}"
        )


def _base_environment() -> dict[str, str]:
    env = dict(os.environ)
    env["NO_COLOR"] = "1"
    env["OPAMCOLOR"] = "never"
    env["OPAMYES"] = "1"
    return env


def _opam_environment(opam_root: Path) -> dict[str, str]:
    env = _base_environment()
    env["OPAMROOT"] = str(opam_root)
    return env


def _engine_environment(opam_root: Path, cache_root: Path) -> dict[str, str]:
    env = _opam_environment(opam_root)
    cache_root.parent.mkdir(parents=True, exist_ok=True)
    _reject_link(cache_root.parent, "engine cache parent")
    cache_root.mkdir(parents=True, exist_ok=True)
    _reject_link(cache_root, "engine cache namespace")
    if os.name == "nt":
        env["LOCALAPPDATA"] = str(cache_root)
    else:
        env["XDG_CACHE_HOME"] = str(cache_root)
    return env


def _read_schema(path: Path, label: str) -> Mapping[str, Any]:
    try:
        contents = path.read_bytes()
    except OSError as error:
        raise CorpusError(f"cannot read {label} schema {path}: {error}") from error
    schema = decode_json(contents, f"{label} schema")
    try:
        validator = jsonschema.validators.validator_for(schema)
        validator.check_schema(schema)
    except jsonschema.exceptions.SchemaError as error:
        raise CorpusError(f"invalid {label} schema: {error.message}") from error
    return schema


def _ensure_repository_clean(
    runner: CommandRunner,
    repository: Path,
    excluded_roots: Sequence[str],
    env: Mapping[str, str],
) -> None:
    status = runner.run(
        "git_inspect",
        ["git", "status", "--porcelain=v1", "-z", "--untracked-files=all"],
        cwd=repository,
        env=env,
        structured_stdout=True,
        accepted_codes={0},
    )
    assert status.stdout is not None
    allowed = set(excluded_roots)
    for raw in status.stdout.split(b"\0"):
        if not raw:
            continue
        try:
            entry = raw.decode("utf-8")
        except UnicodeDecodeError as error:
            raise CorpusError("git status emitted a non-UTF-8 path") from error
        if len(entry) < 4:
            raise CorpusError(f"cannot decode git status entry {entry!r}")
        state, path = entry[:2], entry[3:]
        first = path.replace("\\", "/").split("/", 1)[0]
        if state == "??" and first in allowed:
            continue
        raise CorpusError(
            f"repository has a non-owned worktree change: {entry!r}"
        )
    for artifact in excluded_roots:
        tracked = runner.run(
            "git_inspect",
            ["git", "ls-files", "-z", "--", artifact],
            cwd=repository,
            env=env,
            structured_stdout=True,
            accepted_codes={0},
        )
        assert tracked.stdout is not None
        if tracked.stdout:
            raise CorpusError(
                f"declared artifact root {artifact!r} contains tracked files"
            )


def ensure_repository(
    runner: CommandRunner,
    root: Path,
    session: Path,
    project: Project,
    excluded_roots: Sequence[str],
    env: Mapping[str, str],
) -> Path:
    repositories = root / "repositories"
    repositories.mkdir(exist_ok=True)
    _reject_link(repositories, "repository parent")
    destination = repositories / project.name
    if not os.path.lexists(destination):
        staged = session / f"clone-{project.name}"
        runner.run(
            "git_clone",
            [
                "git",
                "clone",
                "--no-checkout",
                "--origin",
                "origin",
                project.repository,
                str(staged),
            ],
            cwd=session,
            env=env,
            accepted_codes={0},
        )
        if os.path.lexists(destination):
            raise CorpusError(
                f"repository destination appeared during clone: {destination}"
            )
        os.rename(staged, destination)
    _reject_link(destination, "corpus repository")
    if not destination.is_dir():
        raise CorpusError(f"repository path is not a directory: {destination}")
    origin = runner.text(
        "git_inspect",
        ["git", "remote", "get-url", "origin"],
        cwd=destination,
        env=env,
    )
    if origin != project.repository:
        raise CorpusError(
            f"{project.name}: origin mismatch; expected "
            f"{project.repository!r}, got {origin!r}"
        )
    _ensure_repository_clean(runner, destination, excluded_roots, env)
    runner.run(
        "git_fetch",
        ["git", "fetch", "--force", "--tags", "origin"],
        cwd=destination,
        env=env,
        accepted_codes={0},
    )
    object_type = runner.text(
        "git_inspect",
        ["git", "cat-file", "-t", project.commit],
        cwd=destination,
        env=env,
    )
    if object_type != "commit":
        raise CorpusError(f"{project.name}: pinned object is not a commit")
    runner.run(
        "git_checkout",
        ["git", "checkout", "--detach", "--force", project.commit],
        cwd=destination,
        env=env,
        accepted_codes={0},
    )
    head = runner.text(
        "git_inspect",
        ["git", "rev-parse", "HEAD"],
        cwd=destination,
        env=env,
    )
    if head != project.commit:
        raise CorpusError(
            f"{project.name}: checkout mismatch; expected {project.commit}, got {head}"
        )
    _ensure_repository_clean(runner, destination, excluded_roots, env)
    return destination


def ensure_switch(
    runner: CommandRunner,
    manifest: Manifest,
    root: Path,
    engine_root: Path,
) -> tuple[Path, Path, dict[str, str]]:
    opam_root = root / "opam-root"
    switch = root / "switch"
    env = _opam_environment(opam_root)
    if opam_root.exists() and not (opam_root / "config").is_file():
        raise CorpusError(f"refusing incomplete isolated opam root: {opam_root}")
    if not opam_root.exists():
        command = list(manifest.commands.opam_init)
        if os.name == "nt":
            command.extend(manifest.commands.opam_init_windows_extra)
        runner.run(
            "opam_init", command, cwd=engine_root, env=env, accepted_codes={0}
        )
    switch_config = switch / "_opam" / ".opam-switch" / "switch-config"
    values = {
        "switch": str(switch),
        "compiler_package": manifest.compiler_package,
        "stage0_build_dir": str(root / "stage0" / "build"),
    }
    if not switch_config.is_file():
        if switch.exists():
            raise CorpusError(
                f"refusing incomplete local opam switch directory: {switch}"
            )
        runner.run(
            "opam_switch_create",
            render_command(manifest.commands.opam_switch_create, values),
            cwd=engine_root,
            env=env,
            accepted_codes={0},
        )
    compiler = runner.text(
        "opam_inspect",
        [
            "opam",
            "list",
            f"--switch={switch}",
            "--installed",
            "--short",
            manifest.compiler_package,
        ],
        cwd=engine_root,
        env=env,
    )
    if compiler.splitlines() != [manifest.compiler_package]:
        raise CorpusError(
            f"local switch does not contain exactly {manifest.compiler_package!r}"
        )
    return opam_root, switch, env


def build_stage_zero(
    runner: CommandRunner,
    manifest: Manifest,
    root: Path,
    engine_root: Path,
    switch: Path,
    env: Mapping[str, str],
) -> Path:
    before = source_manifest(engine_root, manifest.engine_excluded_roots)
    values = {
        "switch": str(switch),
        "compiler_package": manifest.compiler_package,
        "stage0_build_dir": str(root / "stage0" / "build"),
    }
    primary: BaseException | None = None
    try:
        runner.run(
            "stage0_dependencies",
            render_command(manifest.commands.stage0_dependencies, values),
            cwd=engine_root,
            env=env,
            accepted_codes={0},
        )
        runner.run(
            "stage0_build",
            render_command(manifest.commands.stage0_build, values),
            cwd=engine_root,
            env=env,
            accepted_codes={0},
        )
    except BaseException as error:
        primary = error
    try:
        require_unchanged(engine_root, manifest.engine_excluded_roots, before)
    except BaseException as cleanup_error:
        if primary is None:
            raise
        raise CorpusError(
            f"{primary}; suppressed workspace error: {cleanup_error}"
        ) from primary
    if primary is not None:
        raise primary
    built = Path(
        manifest.stage0.built_executable.format_map(values)
    ).resolve(strict=True)
    build_root = (root / "stage0" / "build").resolve(strict=True)
    try:
        built.relative_to(build_root)
    except ValueError as error:
        raise CorpusError(
            f"stage-0 output escaped its owned build root: {built}"
        ) from error
    if not built.is_file():
        raise CorpusError(
            f"stage-0 build did not produce an executable: {built}"
        )
    installed_dir = root / "stage0" / "bin"
    installed_dir.mkdir(parents=True, exist_ok=True)
    installed = installed_dir / manifest.stage0.installed_name
    pending = installed_dir / f"{installed.name}.{uuid.uuid4().hex}.pending"
    try:
        shutil.copyfile(built, pending)
        pending.chmod(built.stat().st_mode)
        os.replace(pending, installed)
    except OSError as error:
        raise CorpusError(
            f"cannot publish stage-0 executable {installed}: {error}"
        ) from error
    finally:
        try:
            pending.unlink()
        except FileNotFoundError:
            pass
    return installed


def _cli_prefix(switch: Path, stage0: Path) -> list[str]:
    return ["opam", "exec", f"--switch={switch}", "--", str(stage0)]


def _selection_arguments(project: Project, selected: Sequence[str]) -> list[str]:
    if project.selection == "all":
        return []
    arguments: list[str] = []
    for full_id in selected:
        arguments.extend(["--mutant", full_id])
    return arguments


def run_project(
    runner: CommandRunner,
    manifest: Manifest,
    root: Path,
    session: Path,
    evidence: Path,
    stage0: Path,
    switch: Path,
    opam_root: Path,
    project: Project,
    catalog_schema: Mapping[str, Any],
    report_schema: Mapping[str, Any],
) -> Mapping[str, Any]:
    base_env = _opam_environment(opam_root)
    repository = ensure_repository(
        runner,
        root,
        session,
        project,
        manifest.corpus_excluded_roots,
        base_env,
    )
    before = source_manifest(repository, manifest.corpus_excluded_roots)
    cache_root = root / "engine-cache" / project.cache_namespace
    env = _engine_environment(opam_root, cache_root)
    values = {
        "switch": str(switch),
        "compiler_package": manifest.compiler_package,
        "stage0_build_dir": str(root / "stage0" / "build"),
    }
    project_evidence = evidence / project.name
    project_evidence.mkdir(parents=True)
    primary: BaseException | None = None
    result_summary: Mapping[str, Any] | None = None
    try:
        runner.run(
            "corpus_dependencies",
            render_command(manifest.commands.corpus_dependencies, values),
            cwd=repository,
            env=env,
            accepted_codes={0},
        )
        common = [
            str(repository),
            "--profile",
            manifest.profile,
            "--json",
            "--no-color",
        ]
        for pattern in project.include:
            common.extend(["--include", pattern])
        catalogs: list[Catalog] = []
        for repetition in range(1, manifest.catalog_repetitions + 1):
            emitted = runner.run(
                "catalog",
                _cli_prefix(switch, stage0) + ["list"] + common,
                cwd=repository,
                env=env,
                structured_stdout=True,
                accepted_codes={0},
            )
            assert emitted.stdout is not None
            atomic_write(
                project_evidence / f"catalog-{repetition}.json",
                emitted.stdout,
            )
            catalogs.append(
                catalog_from_bytes(
                    emitted.stdout,
                    f"{project.name} catalog {repetition}",
                    catalog_schema,
                )
            )
        catalog = compare_catalogs(catalogs)
        raw_mutants = catalog.document.get("mutants")
        assert isinstance(raw_mutants, list)
        selected = select_mutants(
            raw_mutants,
            project.selection,
            manifest.sample_count_per_rule,
        )
        run_arguments = [
            "run",
            str(repository),
            "--profile",
            manifest.profile,
            "--cache-mode",
            manifest.mutation.cache_mode,
            "--jobs",
            str(manifest.mutation.jobs),
            "--timeout",
            format(manifest.mutation.test_timeout_seconds, "g"),
            "--json",
            "--no-color",
        ]
        for pattern in project.include:
            run_arguments.extend(["--include", pattern])
        run_arguments.extend(_selection_arguments(project, selected))
        run_arguments.append("--")
        run_arguments.extend(project.test_command)
        completed = runner.run(
            "mutation_run",
            _cli_prefix(switch, stage0) + run_arguments,
            cwd=repository,
            env=env,
            structured_stdout=True,
        )
        assert completed.stdout is not None
        atomic_write(project_evidence / "run-report.json", completed.stdout)
        report = validate_report(
            completed.stdout,
            report_schema,
            selected,
            completed.returncode,
        )
        result_summary = {
            "catalog_sha256": hashlib.sha256(catalog.canonical).hexdigest(),
            "catalog_mutants": len(catalog.ordered_ids),
            "commit": project.commit,
            "name": project.name,
            "report_run_id": report.get("run_id"),
            "selected_mutants": len(selected),
            "selection": project.selection,
            "version": project.version,
        }
        atomic_write(
            project_evidence / "acceptance.json",
            canonical_json(result_summary) + b"\n",
        )
    except BaseException as error:
        primary = error
    try:
        require_unchanged(repository, manifest.corpus_excluded_roots, before)
        head = runner.text(
            "git_inspect",
            ["git", "rev-parse", "HEAD"],
            cwd=repository,
            env=base_env,
        )
        if head != project.commit:
            raise CorpusError(
                f"{project.name}: HEAD changed during corpus run ({head})"
            )
    except BaseException as workspace_error:
        if primary is None:
            raise
        raise CorpusError(
            f"{primary}; suppressed workspace verification error: {workspace_error}"
        ) from primary
    if primary is not None:
        raise primary
    assert result_summary is not None
    return result_summary


def execute(manifest_path: Path) -> None:
    manifest = load_manifest(manifest_path)
    engine_root = Path(__file__).resolve().parent.parent
    root = os_cache_root(manifest)
    ensure_owned_cache_root(root, manifest)
    session, session_marker = create_owned_session(root, manifest)
    primary: BaseException | None = None
    try:
        runner = CommandRunner(manifest, session)
        catalog_schema = _read_schema(
            engine_root / "schema" / "catalog-v2.schema.json",
            "catalog-v2",
        )
        report_schema = _read_schema(
            engine_root / "schema" / "run-report-v2.schema.json",
            "run-report-v2",
        )
        opam_root, switch, opam_env = ensure_switch(
            runner, manifest, root, engine_root
        )
        stage0 = build_stage_zero(
            runner,
            manifest,
            root,
            engine_root,
            switch,
            opam_env,
        )
        evidence = root / "evidence" / f"run-{uuid.uuid4().hex}"
        evidence.mkdir(parents=True)
        atomic_write(
            evidence / ".ocaml-mutants-evidence-owner",
            _marker_bytes(manifest),
        )
        summaries = [
            run_project(
                runner,
                manifest,
                root,
                session,
                evidence,
                stage0,
                switch,
                opam_root,
                project,
                catalog_schema,
                report_schema,
            )
            for project in manifest.projects
        ]
        final = {
            "cache_namespace": manifest.cache_namespace,
            "format_version": manifest.format_version,
            "profile": manifest.profile,
            "projects": summaries,
        }
        atomic_write(
            evidence / "acceptance.json",
            canonical_json(final) + b"\n",
        )
        print(
            f"corpus: accepted {len(summaries)} pinned projects; "
            f"evidence={evidence}"
        )
    except BaseException as error:
        primary = error
    try:
        remove_owned_session(root, session, session_marker)
    except BaseException as cleanup_error:
        if primary is None:
            raise
        raise CorpusError(
            f"{primary}; suppressed session cleanup error: {cleanup_error}"
        ) from primary
    if primary is not None:
        raise primary


def check(manifest_path: Path) -> None:
    manifest = load_manifest(manifest_path)
    pins = ", ".join(
        f"{project.name} {project.version}@{project.commit}"
        for project in manifest.projects
    )
    print(
        "corpus: manifest accepted without cache or network access: "
        f"profile={manifest.profile}, "
        f"sample_count_per_rule={manifest.sample_count_per_rule}, "
        f"projects=[{pins}]"
    )


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    commands = result.add_subparsers(dest="command", required=True)
    for name, help_text in (
        (
            "check",
            "validate the manifest without filesystem or network mutation",
        ),
        (
            "run",
            "execute the pinned corpus gate in its owned OS-cache root",
        ),
    ):
        command = commands.add_parser(name, help=help_text)
        command.add_argument(
            "--manifest",
            type=Path,
            required=True,
            help="path to the corpus TOML manifest",
        )
    return result


def main() -> int:
    args = parser().parse_args()
    try:
        if args.command == "check":
            check(args.manifest)
        else:
            execute(args.manifest)
    except KeyboardInterrupt:
        print("corpus: interrupted", file=sys.stderr)
        return 130
    except CorpusError as error:
        print(f"corpus: {error}", file=sys.stderr)
        return 2
    except OSError as error:
        print(f"corpus: filesystem operation failed: {error}", file=sys.stderr)
        return 2
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
