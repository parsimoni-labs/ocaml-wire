#!/usr/bin/env bash
# merlint installs in a switch of its own, never in the project switch where
# wire is pinned to the tree under test, and a switch kept from an earlier
# run refreshes its overlay dependencies before compiling merlint, even when
# their opam version is still "dev".
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
  'switch list --short')
    echo default
    if [[ "$CACHE" == warm || -f "$state/created" ]]; then echo merlint; fi ;;
  'exec -- ocamlc -version')
    echo 5.4.1 ;;
  'switch create merlint ocaml-base-compiler.5.4.1 --no-switch --repositories=default,tangled=git+https://tangled.org/gazagnaire.org/opam-overlay.git')
    test "$CACHE" == cold
    test ! -f "$state/created"
    test "${FAIL_AT:-}" != create
    touch "$state/created" ;;
  'update tangled')
    if [[ "$CACHE" == cold ]]; then test -f "$state/created"; fi
    test "${FAIL_AT:-}" != update
    touch "$state/updated" ;;
  'list --switch=merlint --installed --short --repos=tangled')
    test -f "$state/updated"
    test "${FAIL_AT:-}" != list
    if [[ "$CACHE" == warm ]]; then
      echo project-index
      echo merlint
    fi ;;
  'upgrade --switch=merlint project-index merlint')
    test -f "$state/updated"
    test "${FAIL_AT:-}" != upgrade
    touch "$state/runner-field" ;;
  'install --switch=merlint merlint')
    test -f "$state/updated"
    # Without the upgrade, the kept project-index lacks source_stanza.runner.
    if [[ "$CACHE" == warm ]]; then test -f "$state/runner-field"; fi
    touch "$state/installed" ;;
  # Any other command, an install in the project switch among them, fails.
  *) echo "Unexpected opam command: $*" >&2; exit 1 ;;
esac
OPAM
chmod +x "$scratch/opam"

for cache in warm cold; do
  rm -f "$scratch/created" "$scratch/updated" "$scratch/runner-field" "$scratch/installed"
  CACHE=$cache bash "$installer"
  test -f "$scratch/installed"
done

# Creation, refresh and solver failures must stop installation, rather than
# falling back to the project switch, silently compiling merlint against the
# stale dependency or upgrading the whole switch.
for case in cold:create warm:update warm:list warm:upgrade; do
  cache=${case%%:*} fail_at=${case#*:}
  rm -f "$scratch/created" "$scratch/updated" "$scratch/runner-field" "$scratch/installed"
  if CACHE=$cache FAIL_AT=$fail_at bash "$installer"; then
    echo "Install ignored opam $fail_at failure" >&2
    exit 1
  fi
  test ! -f "$scratch/installed"
done
