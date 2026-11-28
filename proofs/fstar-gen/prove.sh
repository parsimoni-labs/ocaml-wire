#!/bin/sh
# The F* lane of make prove, run by the rules of ./dune from the build
# directory, which holds the generated modules, lib/ and ../fstar/.
#
#   prove.sh lib FILE...          check the fixed library, in order
#   prove.sh codec NAME           3d.exe on NAME.3d, then every module of
#                                 NAME (MODULES below) must verify
#   prove.sh refused NAME SED WALK [walk]
#                                 SED applied to NAME.WALK.fst (Step, Frames,
#                                 Index, or each with Ox): the walk and Flat
#                                 verify, its proof must not; with walk, the
#                                 walk itself must not
#   prove.sh prune USED...        remove from the cache all but the entries
#                                 the USED files list
#
# lib writes used.lib, codec and refused used.NAME: the cache entries the
# run read or wrote, relative to WIRE_FSTAR_CACHE. prune runs once they are
# all written, so the cache holds only what the current sources need.
#
# F*, Z3 and 3d.exe come with EverParse (EVERPARSE, default
# ~/.local/everparse). scripts/fstar-gen/prove.exe finds 3d.exe there as
# Wire_3d does, under the name everparse-3d.exe, and runs each tool.
# Checked modules go to WIRE_FSTAR_CACHE, one directory per F* version, and
# 3d.exe's output to a directory named by the digest of the .3d; prune
# removes every other version and digest. The default is fstar-cache in the
# build directory, beside default/: each tree has its own, so a prune never
# touches another tree's, and dune neither digests nor removes it (dune
# clean does). Each module gets WIRE_PROVE_TIMEOUT seconds (default 900); a
# module that runs out fails, so a mutant refused by a timeout counts as
# refused.
set -eu

EP=${EVERPARSE:-$HOME/.local/everparse}
FSTAR=$EP/bin/fstar.exe
Z3DIR=$(ls -d "$EP"/lib/fstar/z3-* | tail -1)
VERSION=$("$FSTAR" --version | sed -n 's/^F\* //p')
HERE=$(pwd)
ROOT=${WIRE_FSTAR_CACHE:-$(cd "$HERE/../../.." && pwd)/fstar-cache}
CACHE=$ROOT/$VERSION
TIMEOUT=${WIRE_PROVE_TIMEOUT:-900}
PROVE=$HERE/../../scripts/fstar-gen/prove.exe
if [ "$1" = prune ]; then
  shift
  exec "$PROVE" prune "$ROOT" "$@"
fi
THREED=$("$PROVE" 3d "$EP")
mkdir -p "$CACHE"

# use ENTRY: ENTRY of the cache, relative to CACHE, is one this run needs.
use() {
  echo "$VERSION/$1" >>"$USED"
}

# verify FILE DIR...: F* checks FILE with the modules of each DIR in scope,
# from the directory of FILE: F* reads the modules of its working directory
# first, and those of the build directory are not the ones under proof. Its
# messages are the human ones, which refused reads, wherever it runs: under
# GITHUB_ACTIONS, F* writes GitHub annotations by default.
verify() {
  file=$1
  shift
  includes=""
  for d in "$@"; do includes="$includes --include $d"; done
  # shellcheck disable=SC2086
  (cd "$(dirname "$file")" && "$PROVE" run "$TIMEOUT" "$(basename "$file")" \
    "$FSTAR" --z3version "${Z3DIR##*z3-}" --smt "$Z3DIR/bin/z3" \
    --message_format human \
    --already_cached 'Prims,LowStar,FStar,LowParse,C,PulseCore,Pulse,EverParse3d.*,Spec' \
    --include "$EP/lib/everparse/lowparse" --include "$EP/lib/krml" \
    --include "$EP/lib/everparse/3d" \
    $includes --include "$HERE/lib" --include "$HERE/../fstar" \
    --cache_checked_modules --cache_dir "$CACHE" "$file") || return
  use "$(basename "$file").checked"
}

# schema NAME: the directory of 3d.exe's F* module for NAME.3d, written and
# checked once per digest of the .3d.
schema() {
  digest=$(cksum <"$HERE/$1.3d" | tr ' ' '-')
  out=$CACHE/3d/$1-$digest
  if [ ! -f "$out/done" ]; then
    rm -rf "$out"
    mkdir -p "$out"
    cp "$HERE/$1.3d" "$out/"
    "$PROVE" run "$TIMEOUT" "$1.3d" \
      "$THREED" --batch --odir "$out" "$out/$1.3d" >"$out/3d.log" 2>&1 ||
      { cat "$out/3d.log" >&2; exit 1; }
    touch "$out/done"
  fi
  use "3d/$1-$digest"
  echo "$out"
}

# The modules of a codec, in the order F* checks them: the reading, then
# the step functions and their proofs, then the walks of a frame and theirs.
MODULES="Flat.fsti Flat.fst Visitor.fsti
  Step.fst StepProof.fsti StepProof.fst
  OxStep.fst OxStepProof.fsti OxStepProof.fst
  Frames.fst FramesProof.fsti FramesProof.fst Index.fst IndexProof.fsti IndexProof.fst
  OxFrames.fst OxFramesProof.fsti OxFramesProof.fst
  OxIndex.fst OxIndexProof.fsti OxIndexProof.fst"

# work NAME SED WALK: a directory of NAME's modules, SED applied to its WALK.
work() {
  dir=$(mktemp -d "${TMPDIR:-/tmp}/wire-prove.XXXXXX")
  for m in $MODULES; do
    cp "$HERE/$1.$m" "$dir/"
  done
  chmod u+w "$dir"/*
  if [ -n "$2" ]; then
    sed "$2" "$HERE/$1.$3.fst" >"$dir/$1.$3.fst"
    if cmp -s "$HERE/$1.$3.fst" "$dir/$1.$3.fst"; then
      echo "FAIL $1: the mutation does not apply" >&2
      exit 1
    fi
  fi
  echo "$dir"
}

case $1 in
lib)
  shift
  USED=$HERE/used.lib
  : >"$USED"
  for m in "$@"; do verify "$HERE/$m"; done
  ;;
codec)
  USED=$HERE/used.$2
  : >"$USED"
  out=$(schema "$2")
  dir=$(work "$2" "" Step)
  # An interface is checked on its own, so that its checked file is kept.
  for m in $MODULES; do
    verify "$dir/$2.$m" "$dir" "$out"
  done
  rm -rf "$dir"
  ;;
refused)
  USED=$HERE/used.$2
  : >"$USED"
  out=$(schema "$2")
  walk=$4
  case $walk in
  Step | OxStep | Frames | OxFrames | Index | OxIndex)
    proof=${walk}Proof
    needed="Visitor.fsti $walk.fst Flat.fsti Flat.fst $proof.fsti"
    ;;
  *)
    echo "prove.sh: unknown walk $walk" >&2
    exit 2
    ;;
  esac
  # With "walk", the mutated walk itself must not verify: a load its type
  # does not allow, which F* refuses before any proof.
  if [ "${5:-}" = walk ]; then
    needed=$(echo "$needed" | sed "s/ *$walk.fst//; s/ *$proof.fsti//")
    refused=$walk
  else
    refused=$proof
  fi
  dir=$(work "$2" "$3" "$walk")
  for m in $needed; do
    verify "$dir/$2.$m" "$dir" "$out"
  done
  if verify "$dir/$2.$refused.fst" "$dir" "$out" >"$dir/log" 2>&1; then
    cat "$dir/log"
    echo "FAIL $2: the mutant verifies" >&2
    exit 1
  fi
  echo "refused $2: $(grep -m1 -E '^\* Error|timeout' "$dir/log")"
  grep "^$2.$refused.fst: wall" "$dir/log" >&2
  rm -rf "$dir"
  ;;
*)
  echo "prove.sh: unknown command $1" >&2
  exit 2
  ;;
esac
