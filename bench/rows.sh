#!/bin/sh
# rows.sh CMD... : runs each row of a benchmark harness in a process of its
# own. CMD is the harness command line without a row name (bench.exe run FILE
# STRIDE RUNS, wire-bench-rust FILE 64 RUNS, ...): CMD --list prints the names
# of its rows, then CMD ROW runs the row ROW alone and prints its RESULT line.
# No row runs after another in the same process, so none starts on the heap,
# the caches or the address space another row left behind. Fails when CMD
# lists no row or a row fails.
set -eu
rows=$("$@" --list)
[ -n "$rows" ] || {
  echo "rows.sh: $* lists no row" >&2
  exit 1
}
printf '%s\n' "$rows" | while IFS= read -r row; do
  "$@" "$row"
done
