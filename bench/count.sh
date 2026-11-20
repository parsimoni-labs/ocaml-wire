#!/bin/sh
# count.sh ITEMS LABEL CMD... : the instructions and cycles one row of a
# benchmark spends per item. CMD is the harness command line for that one row:
# it ends with the row's exact name and has the literal word RUNS where its
# number of timed passes goes. count.sh runs CMD with 1 timed pass and with 21,
# each after the harness's warm-up pass, and divides the difference by 20
# passes of ITEMS items, which cancels starting the process, reading the
# input, building the tables, the warm-up pass and printing the result (a write
# row hashes its whole output then). It prints LABEL, the instructions and the
# cycles per item and the row's result, and fails unless both runs exit 0,
# print one RESULT line each with the same result, and count some instructions
# and cycles. macOS reads the counts from /usr/bin/time -l, Linux from perf
# stat.
set -eu
items=$1
label=$2
shift 2
out=$(mktemp "${TMPDIR:-/tmp}/count.out.XXXXXX")
err=$(mktemp "${TMPDIR:-/tmp}/count.err.XXXXXX")
trap 'rm -f "$out" "$err"' EXIT

fail() {
  echo "count.sh: $label: $*" >&2
  exit 1
}

# run PASSES CMD... prints the instructions, the cycles and the result of CMD
# with PASSES timed passes.
run() {
  passes=$1
  shift
  for a do
    shift
    if [ "$a" = RUNS ]; then set -- "$@" "$passes"; else set -- "$@" "$a"; fi
  done
  case $(uname) in
  Darwin)
    /usr/bin/time -l "$@" >"$out" 2>"$err" ||
      fail "$passes passes exited with status $?: $(head -n 1 "$err")"
    counts=$(awk '/instructions retired/ { i = $1 } /cycles elapsed/ { c = $1 }
                  END { print i + 0 "\t" c + 0 }' "$err")
    ;;
  *)
    perf stat -x, -e instructions,cycles "$@" >"$out" 2>"$err" ||
      fail "$passes passes exited with status $?: $(head -n 1 "$err")"
    counts=$(awk -F, '$3 ~ /^instructions/ { i = $1 } $3 ~ /^cycles/ { c = $1 }
                      END { print i + 0 "\t" c + 0 }' "$err")
    ;;
  esac
  results=$(grep -c '^RESULT' "$out" || true)
  [ "$results" = 1 ] ||
    fail "$passes passes printed $results RESULT lines, not 1"
  case $counts in
  0"	"* | *"	"0) fail "$passes passes: no instruction or cycle count" ;;
  esac
  printf '%s\t%s\n' "$counts" "$(awk -F '\t' '/^RESULT/ { print $NF }' "$out")"
}

a=$(run 1 "$@")
b=$(run 21 "$@")
printf '%s\t%s\n' "$a" "$b" | awk -F '\t' -v n="$items" -v l="$label" '
  $3 != $6 { print "count.sh: " l ": the runs gave " $3 " and " $6 > "/dev/stderr"; exit 1 }
  { printf "%s\t%.1f\t%.1f\t%s\n", l, ($4 - $1) / (20 * n), ($5 - $2) / (20 * n), $3 }'
