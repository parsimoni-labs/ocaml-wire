#!/bin/sh
set -eu
: "${HW_MITER:?set HW_MITER to generated stream properties Verilog}"
: "${HW_OUT:?set HW_OUT to a writable output directory}"
command -v yosys >/dev/null
mkdir -p "$HW_OUT"
if [ ! "$HW_MITER" -ef "$HW_OUT/stream.v" ]; then
  cp "$HW_MITER" "$HW_OUT/stream.v"
fi
cd "$HW_OUT"
case "${HW_PROPERTY:-control}" in
  control)
    hw_commands='read_verilog stream.v; prep -top wire_stream_properties -flatten; dffunmap; sat -timeout 300 -seq 2 -tempinduct -maxsteps 12 -set-at 1 reset 1 -prove proof_ok 1 -verify'
    ;;
  keep)
    hw_commands='read_verilog stream.v; prep -top wire_stream_keep_properties -flatten; sat -timeout 300 -prove proof_ok 1 -verify'
    ;;
  *) echo "invalid HW_PROPERTY" >&2; exit 1 ;;
esac
yosys -p "$hw_commands" > stream-proof.log 2>&1
