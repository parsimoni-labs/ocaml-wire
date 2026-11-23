#!/bin/sh
# fstar.sh DIR3D FILE: check FILE against the 3d.exe output in DIR3D.
EP=${EVERPARSE:-$HOME/.local/everparse}
Z3DIR=$(ls -d $EP/lib/fstar/z3-* | tail -1)
CACHE=${CACHE:-${TMPDIR:-/tmp}/wire-fstar-gen-cache}
mkdir -p "$CACHE"
dir3d=$1; shift
exec "$EP/bin/fstar.exe" --z3version "${Z3DIR##*z3-}" --smt "$Z3DIR/bin/z3" \
  --already_cached 'Prims,LowStar,FStar,LowParse,C,PulseCore,Pulse,EverParse3d.*,Spec' \
  --include "$EP/lib/everparse/lowparse" --include "$EP/lib/krml" --include "$EP/lib/krml/obj" \
  --include "$EP/lib/everparse/3d" --include "$dir3d" --include "$(dirname "$0")" --include "$(dirname "$0")/../fstar" \
  --cache_checked_modules --cache_dir "$CACHE" "$@"
