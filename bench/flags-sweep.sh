#!/bin/sh
# flags-sweep.sh SWITCH CANDIDATE... : the instructions and cycles per item of
# the rows of bench/apps and bench/rust, built once per CANDIDATE. SWITCH is an
# opam switch, or - for the compiler on PATH; each CANDIDATE is one argument
# holding the ocamlopt flags dune's release profile adds to every unit, the
# libraries a row calls as well as the harness ("" for none). With SWITCH
# cargo, each CANDIDATE is a RUSTFLAGS value for the two cargo projects
# instead. Every row each harness lists is counted REPEATS times (default 3) by
# bench/count.sh: on each workload of bench/apps, reading and writing, and on
# the fixed64-random and mix128-random corpora of bench/rust. Builds and
# inputs land in _build/flags. Prints one line per count: switch, flags, row,
# instructions and cycles per item, result; reports a candidate that does not
# build and goes on; fails unless every row of a workload or corpus gives the
# same result.
set -eu
[ $# -ge 2 ] || {
  echo "usage: flags-sweep.sh SWITCH CANDIDATE..." >&2
  exit 2
}
sw=$1
shift
root=$(cd "$(dirname "$0")/.." && pwd)
out=$root/_build/flags
in=$out/inputs
repeats=${REPEATS:-3}
apps=100000
packets=200000
mkdir -p "$out"

# The generators, and the inputs, which do not depend on the flags.
dune build --root "$root" --profile=bench bench/apps/gen.exe \
  bench/rust/gen.exe bench/apps/bench.exe bench/rust/bench.exe 2>/dev/null
if [ ! -f "$in/routing.bin" ]; then
  mkdir -p "$in"
  "$root/_build/default/bench/apps/bench.exe" inputs "$in" $apps $apps $apps
  "$root/_build/default/bench/rust/bench.exe" corpus "$in" $packets
fi

# count ITEMS LABEL CMD... : REPEATS counts of one row (bench/count.sh).
count() {
  i=0
  while [ $i -lt "$repeats" ]; do
    sh "$root/bench/count.sh" "$@" </dev/null | sed "s/^/$sw	$flags	/"
    i=$((i + 1))
  done
}

# rows APPS HEADERS : every row of the bench/apps harness APPS and of the
# bench/rust harness HEADERS (its arguments up to the input file).
rows() {
  a=$1
  shift
  for w in routing gateway clcw; do
    for m in run write; do
      $a $m "$in/$w.bin" $apps 1 --list | while IFS= read -r r; do
        count $apps "$w $m $r" $a $m "$in/$w.bin" $apps RUNS "$r"
      done
    done
  done
  for c in fixed64-random:64 mix128-random:128; do
    f=$in/${c%:*}.bin
    "$@" "$f" "${c#*:}" 1 --list | while IFS= read -r r; do
      count $packets "${c%:*} $r" "$@" "$f" "${c#*:}" RUNS "$r"
    done
  done
}

for flags do
  tag=$(printf '%s' "${flags:-none}" | tr -c 'A-Za-z0-9' _)
  if [ "$sw" = cargo ]; then
    t=$out/cargo/$tag
    for p in apps rust; do
      mkdir -p "$root/bench/$p/cargo/src/gen"
      "$root/_build/default/bench/$p/gen.exe" --rust \
        "$root/bench/$p/cargo/src/gen"
      RUSTFLAGS=$flags CARGO_TARGET_DIR=$t/$p cargo build --quiet --release \
        --manifest-path "$root/bench/$p/cargo/Cargo.toml"
    done
    rows "$t/apps/release/wire-bench-apps" "$t/rust/release/wire-bench-rust"
    continue
  fi
  b=$out/$sw/$tag
  mkdir -p "$b"
  cat >"$b.ws" <<EOF
(lang dune 3.21)
(context default)
(env (release (ocamlopt_flags (:standard $flags))))
EOF
  if [ "$sw" = - ]; then ex=; else ex="opam exec --switch $sw --"; fi
  if ! $ex dune build --root "$root" --workspace "$b.ws" --build-dir "$b" \
    --profile=release bench/apps/bench.exe bench/rust/bench.exe \
    >"$b.log" 2>&1; then
    echo "flags-sweep.sh: $sw $flags: build failed, see $b.log" >&2
    continue
  fi
  rows "$b/default/bench/apps/bench.exe" "$b/default/bench/rust/bench.exe" run
done | awk -F '\t' '
  { print; fflush(); split($3, w, " "); k = w[1] " " w[2]
    if (w[1] ~ /^fixed64|^mix128/) k = w[1]
    if (k in r && r[k] != $6) { bad = bad k ": " r[k] " and " $6 "\n" }
    r[k] = $6 }
  END { if (bad != "") { printf "flags-sweep.sh: rows disagree on %s", bad > "/dev/stderr"; exit 1 } }'
