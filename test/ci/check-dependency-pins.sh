#!/usr/bin/env bash
set -euo pipefail

repo=${1:-$(git rev-parse --show-toplevel)}
workflow=$repo/.github/workflows/ci.yml
manifest=$repo/.github/ci-dependencies.env
installer=$repo/.github/scripts/install-archive
updater=$repo/.github/scripts/update-ci-dependencies
dependabot=$repo/.github/dependabot.yml
failed=0

fail() {
  echo "dependency-pin policy: $*" >&2
  failed=1
}

action_refs=$(sed -n \
  's/^[[:space:]]*- uses: [^@[:space:]]*@\([^[:space:]#]*\).*/\1/p' \
  "$workflow")
if [[ -z "$action_refs" ]]; then
  fail "no GitHub Actions references found"
else
  while IFS= read -r ref; do
    if [[ ! "$ref" =~ ^v[0-9]+$ ]]; then
      fail "GitHub Actions reference @$ref is not a major version tag"
    fi
  done <<< "$action_refs"
fi

if grep -Eq 'curl[^|]*\|[[:space:]]*tar' "$workflow"; then
  fail "workflow pipes a download directly into tar"
fi

for path in "$manifest" "$installer" "$updater" "$dependabot"; do
  if [[ ! -f "$path" ]]; then
    fail "missing $path"
  fi
done
for path in "$installer" "$updater"; do
  if [[ -f "$path" && ! -x "$path" ]]; then
    fail "$path is not executable"
  fi
done
if [[ -x "$updater" ]] && "$updater" >/dev/null 2>&1; then
  fail "archive updater does not require explicit release tags"
fi

if [[ -f "$manifest" ]]; then
  # shellcheck disable=SC1090
  source "$manifest"
  for var in EVERPARSE_VERSION EVERPARSE_LINUX_X86_64_URL \
    EVERPARSE_LINUX_X86_64_SHA256 BINARYEN_VERSION \
    BINARYEN_LINUX_X86_64_URL BINARYEN_LINUX_X86_64_SHA256; do
    if [[ -z "${!var:-}" ]]; then
      fail "manifest does not set $var"
    fi
  done
  if [[ ! "${EVERPARSE_LINUX_X86_64_SHA256:-}" =~ ^[0-9a-f]{64}$ ]]; then
    fail "EverParse SHA-256 is not a full lowercase digest"
  fi
  if [[ ! "${BINARYEN_LINUX_X86_64_SHA256:-}" =~ ^[0-9a-f]{64}$ ]]; then
    fail "Binaryen SHA-256 is not a full lowercase digest"
  fi
  if [[ "${EVERPARSE_LINUX_X86_64_URL:-}" != *"/${EVERPARSE_VERSION:-missing}/"* ]]; then
    fail "EverParse URL does not contain its pinned version"
  fi
  if [[ "${BINARYEN_LINUX_X86_64_URL:-}" != *"/${BINARYEN_VERSION:-missing}/"* ]]; then
    fail "Binaryen URL does not contain its pinned version"
  fi
fi

if [[ -f "$installer" ]]; then
  grep -q -- '--fail' "$installer" || fail "archive installer omits curl --fail"
  grep -q -- '--location' "$installer" || fail "archive installer omits redirect handling"
  grep -q 'sha256sum' "$installer" || fail "archive installer omits SHA-256 verification"

  scratch=$(mktemp -d "${TMPDIR:-/tmp}/wire-ci-policy.XXXXXX")
  trap 'rm -rf -- "$scratch"' EXIT
  mkdir -p "$scratch/source"
  printf 'verified\n' > "$scratch/source/payload"
  tar czf "$scratch/fixture.tar.gz" -C "$scratch/source" payload
  if command -v sha256sum >/dev/null 2>&1; then
    fixture_sha=$(sha256sum "$scratch/fixture.tar.gz")
  else
    fixture_sha=$(shasum -a 256 "$scratch/fixture.tar.gz")
  fi
  fixture_sha=${fixture_sha%% *}
  if [[ ${fixture_sha:0:1} == 0 ]]; then
    wrong_sha=1${fixture_sha:1}
  else
    wrong_sha=0${fixture_sha:1}
  fi
  if bash "$installer" "file://$scratch/fixture.tar.gz" \
    "$wrong_sha" "$scratch/rejected" >/dev/null 2>&1; then
    fail "archive installer accepts a wrong digest"
  fi
  if [[ -e "$scratch/rejected/payload" ]]; then
    fail "archive installer extracts before checking the digest"
  fi
  if ! bash "$installer" "file://$scratch/fixture.tar.gz" \
    "$fixture_sha" "$scratch/accepted" >/dev/null 2>&1; then
    fail "archive installer rejects the correct digest"
  elif [[ ! -f "$scratch/accepted/payload" ]]; then
    fail "archive installer did not extract the verified payload"
  elif [[ $(< "$scratch/accepted/payload") != verified ]]; then
    fail "archive installer did not extract the verified payload"
  fi
fi

if [[ -f "$dependabot" ]]; then
  grep -Eq 'package-ecosystem:[[:space:]]*["'\'']?github-actions' "$dependabot" \
    || fail "Dependabot does not update GitHub Actions"
fi

grep -q 'ci-dependencies.env' "$workflow" \
  || fail "workflow does not load the dependency manifest"
grep -q 'install-archive' "$workflow" \
  || fail "workflow does not use the verified archive installer"

exit "$failed"
