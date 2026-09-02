#!/usr/bin/env bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

find_repo() {
  local dir=$script_dir
  while [[ "$dir" != / ]]; do
    if [[ -f "$dir/.github/workflows/ci.yml" ]]; then
      printf '%s\n' "$dir"
      return
    fi
    dir=$(dirname -- "$dir")
  done
  echo "EverParse benchmark policy: unable to locate the repository root" >&2
  return 1
}

repo=${1:-$(find_repo)}
workflow=$repo/.github/workflows/ci.yml

if ! grep -Eq \
  '^[[:space:]]*([^#[:space:]].*)?@test/bench/runtest([[:space:]]|$)' \
  "$workflow"; then
  echo \
    "EverParse benchmark policy: CI does not run @test/bench/runtest" \
    >&2
  exit 1
fi
