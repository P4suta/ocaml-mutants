#!/bin/sh
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
temp_root=$(CDPATH= cd -- "${TMPDIR:-/tmp}" && pwd -P)
temp_dir=$(mktemp -d "$temp_root/ocaml-mutants-dogfood-XXXXXXXX")
owner_marker="$temp_dir/.ocaml-mutants-dogfood-owner"
owner_token=$(basename -- "$temp_dir")
printf 'owner=ocaml-mutants-dogfood\ntoken=%s\n' "$owner_token" >"$owner_marker"

manifest_created=0

cleanup() {
  if [ ! -e "$temp_dir" ]; then
    return 0
  fi
  resolved_temp=$(CDPATH= cd -- "$temp_dir" && pwd -P) || return 1
  parent=$(dirname -- "$resolved_temp")
  base=$(basename -- "$resolved_temp")
  case "$base" in
    ocaml-mutants-dogfood-*) ;;
    *)
      printf '%s\n' "dogfood: refusing to remove unverified temporary directory: $resolved_temp" >&2
      return 1
      ;;
  esac
  if [ "$parent" != "$temp_root" ]; then
    printf '%s\n' "dogfood: temporary directory is outside the system temp root: $resolved_temp" >&2
    return 1
  fi
  expected_marker=$(printf 'owner=ocaml-mutants-dogfood\ntoken=%s' "$owner_token")
  if [ ! -f "$owner_marker" ] || [ "$(cat "$owner_marker")" != "$expected_marker" ]; then
    printf '%s\n' "dogfood: refusing to remove temporary directory without its ownership marker: $resolved_temp" >&2
    return 1
  fi
  rm -rf -- "$resolved_temp"
}

finish() {
  original_status=$?
  trap - EXIT INT TERM HUP
  verification_status=0
  cleanup_status=0
  if [ "$manifest_created" -eq 1 ]; then
    "$python_command" "$repo_root/scripts/verify-workspace-manifest.py" \
      verify "$repo_root" "$temp_dir/workspace-manifest.json" || verification_status=$?
  fi
  cleanup || cleanup_status=$?
  if [ "$original_status" -ne 0 ]; then
    exit "$original_status"
  fi
  if [ "$verification_status" -ne 0 ]; then
    exit "$verification_status"
  fi
  exit "$cleanup_status"
}
trap finish EXIT
trap 'exit 130' INT TERM HUP

if command -v python3 >/dev/null 2>&1; then
  python_command=python3
elif command -v python >/dev/null 2>&1; then
  python_command=python
else
  printf '%s\n' 'dogfood: Python 3 is required for report and workspace verification' >&2
  exit 2
fi

cd "$repo_root"
opam exec -- dune build bin/main.exe

stage_zero="$temp_dir/ocaml-mutants-stage0.exe"
cp "$repo_root/_build/default/bin/main.exe" "$stage_zero"
chmod +x "$stage_zero"

"$python_command" "$repo_root/scripts/verify-workspace-manifest.py" \
  create "$repo_root" "$temp_dir/workspace-manifest.json" \
  --exclude-root _build --exclude-root _opam
manifest_created=1

set +e
opam exec -- "$stage_zero" run "$repo_root" \
  --profile balanced --cache-mode on --json --no-color \
  >"$temp_dir/run-report.json" 2>"$temp_dir/run.stderr"
run_status=$?
set -e

if [ -s "$temp_dir/run.stderr" ]; then
  cat "$temp_dir/run.stderr" >&2
fi
"$python_command" "$repo_root/scripts/verify-dogfood-run.py" \
  "$temp_dir/run-report.json" \
  --schema "$repo_root/schema/run-report-v2.schema.json" \
  --exit-code "$run_status"
