#!/bin/sh
set -eu
: "${HW_MITER:?set HW_MITER to the generated padding miter Verilog}"
: "${HW_OUT:?set HW_OUT to a writable output directory}"
command -v yosys >/dev/null
mkdir -p "$HW_OUT"
if [ ! "$HW_MITER" -ef "$HW_OUT/padding.v" ]; then
  cp "$HW_MITER" "$HW_OUT/padding.v"
fi
cd "$HW_OUT"
yosys -p 'read_verilog padding.v; prep -top wire_padding_noninterference -flatten; sat -timeout 300 -prove proof_ok 1 -verify' > padding-proof.log 2>&1
