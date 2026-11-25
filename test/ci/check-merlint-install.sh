#!/usr/bin/env bash
# A restored setup-ocaml switch must refresh installed overlay dependencies
# before compiling merlint, even when their opam version is still "dev".
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
  echo "merlint install: unable to locate the repository root" >&2
  return 1
}

installer=${1:-$(find_repo)/.github/scripts/install-merlint}
scratch=$(mktemp -d "${TMPDIR:-/tmp}/wire-merlint-test.XXXXXX")
trap 'rm -rf -- "$scratch"' EXIT
export OPAM_TEST_STATE=$scratch
export PATH="$scratch:$PATH"

cat > "$scratch/opam" <<'OPAM'
#!/usr/bin/env bash
set -euo pipefail
state=$OPAM_TEST_STATE
case "$*" in
  'repository add tangled git+https://tangled.org/gazagnaire.org/opam-overlay.git --rank=-1')
    touch "$state/added" ;;
  'update tangled')
    test -f "$state/added"
    test "${FAIL_AT:-}" != update
    touch "$state/updated" ;;
  'list --installed --short --repos=tangled')
    test -f "$state/updated"
    test "${FAIL_AT:-}" != list
    if [[ "$CACHE" == warm ]]; then
      echo project-index
      echo merlint
    fi ;;
  'upgrade project-index merlint')
    test -f "$state/updated"
    test "${FAIL_AT:-}" != upgrade
    touch "$state/runner-field" ;;
  'install merlint')
    test -f "$state/updated"
    # Without the upgrade, the cached project-index lacks source_stanza.runner.
    if [[ "$CACHE" == warm ]]; then test -f "$state/runner-field"; fi
    touch "$state/installed" ;;
  *) echo "Unexpected opam command: $*" >&2; exit 1 ;;
esac
OPAM
chmod +x "$scratch/opam"

for cache in warm cold; do
  rm -f "$scratch/added" "$scratch/updated" "$scratch/runner-field" "$scratch/installed"
  CACHE=$cache bash "$installer"
  test -f "$scratch/installed"
done

# Refresh and solver failures must stop installation, rather than silently
# compiling merlint against the stale dependency or upgrading the whole switch.
for fail_at in update list upgrade; do
  rm -f "$scratch/added" "$scratch/updated" "$scratch/runner-field" "$scratch/installed"
  if CACHE=warm FAIL_AT=$fail_at bash "$installer"; then
    echo "Install ignored opam $fail_at failure" >&2
    exit 1
  fi
  test ! -f "$scratch/installed"
done
