#!/bin/sh
set -eu
: "${HW_MITER:?set HW_MITER to the generated pipeline miter Verilog}"
: "${HW_OUT:?set HW_OUT to a writable output directory}"
command -v yosys >/dev/null
mkdir -p "$HW_OUT"
if [ ! "$HW_MITER" -ef "$HW_OUT/pipeline.v" ]; then
  cp "$HW_MITER" "$HW_OUT/pipeline.v"
fi
cd "$HW_OUT"
yosys -p 'read_verilog pipeline.v; prep -top wire_pipeline_equivalence -flatten; dffunmap; sat -timeout 300 -seq 2 -tempinduct -maxsteps 8 -set-at 1 reset 1 -prove proof_ok 1 -verify' > pipeline-proof.log 2>&1
