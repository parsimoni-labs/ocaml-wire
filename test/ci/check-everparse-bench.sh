#!/usr/bin/env bash
set -euo pipefail

repo=${1:-$(git rev-parse --show-toplevel)}
workflow=$repo/.github/workflows/ci.yml

if ! grep -Eq \
  '^[[:space:]]*([^#[:space:]].*)?@test/bench/runtest([[:space:]]|$)' \
  "$workflow"; then
  echo \
    "EverParse benchmark policy: CI does not run @test/bench/runtest" \
    >&2
  exit 1
fi
