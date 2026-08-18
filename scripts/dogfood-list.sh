#!/bin/sh
# SPDX-FileCopyrightText: 2026 ocaml-mutants contributors
# SPDX-License-Identifier: MIT OR Apache-2.0

set -eu

repo_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd -P)
temp_root=${TMPDIR:-/tmp}
temp_dir=$(mktemp -d "$temp_root/ocaml-mutants-dogfood-list-XXXXXXXX")
owner_marker="$temp_dir/.ocaml-mutants-dogfood-list-owner"
printf '%s\n' 'owner=ocaml-mutants-dogfood-list' >"$owner_marker"

cleanup() {
  case "$temp_dir" in
    "$temp_root"/ocaml-mutants-dogfood-list-*) ;;
    *)
      printf '%s\n' "dogfood-list: refusing to remove unverified temporary directory: $temp_dir" >&2
      return 1
      ;;
  esac
  if [ ! -f "$owner_marker" ] ||
     [ "$(cat "$owner_marker")" != 'owner=ocaml-mutants-dogfood-list' ]; then
    printf '%s\n' "dogfood-list: refusing to remove temporary directory without its ownership marker: $temp_dir" >&2
    return 1
  fi
  rm -rf -- "$temp_dir"
}
trap cleanup EXIT
trap 'exit 130' INT TERM HUP

cd "$repo_root"
opam exec -- dune build bin/main.exe

stage_zero="$temp_dir/ocaml-mutants-stage0.exe"
cp "$repo_root/_build/default/bin/main.exe" "$stage_zero"
chmod +x "$stage_zero"

opam exec -- "$stage_zero" list "$repo_root" \
  --profile balanced --json --no-color \
  >"$temp_dir/catalog-1.json" 2>"$temp_dir/catalog-1.stderr"
opam exec -- "$stage_zero" list "$repo_root" \
  --profile balanced --json --no-color \
  >"$temp_dir/catalog-2.json" 2>"$temp_dir/catalog-2.stderr"

if command -v python3 >/dev/null 2>&1; then
  python_command=python3
elif command -v python >/dev/null 2>&1; then
  python_command=python
else
  printf '%s\n' 'dogfood-list: Python 3 is required to canonicalize and verify catalog JSON' >&2
  exit 2
fi

"$python_command" "$repo_root/scripts/verify-dogfood-catalog.py" \
  "$temp_dir/catalog-1.json" "$temp_dir/catalog-2.json"
