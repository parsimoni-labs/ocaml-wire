#!/bin/sh
set -eu
: "${HW_TOP:?set HW_TOP to the generated wire_stack_64 Verilog file}"
: "${HW_OUT:?set HW_OUT to a writable output directory}"
hw_module=${HW_MODULE:-wire_stack_64}
case "$hw_module" in
  ""|[0-9]*|*[!A-Za-z0-9_]*) echo "invalid top module" >&2; exit 1 ;;
esac
hw_frequency=${HW_FREQ:-250}
hw_seed=${HW_SEED:-1}
command -v yosys >/dev/null
command -v nextpnr-ecp5 >/dev/null
mkdir -p "$HW_OUT"
if [ ! "$HW_TOP" -ef "$HW_OUT/core.v" ]; then
  cp "$HW_TOP" "$HW_OUT/core.v"
fi
cd "$HW_OUT"
yosys -V > toolchain.txt
nextpnr-ecp5 --version >> toolchain.txt 2>&1
printf 'target_mhz=%s\nseed=%s\n' "$hw_frequency" "$hw_seed" >> toolchain.txt
# Core-only timing uses registered boundaries and virtual IO. Experimental
# out-of-context routing excludes board IO and global clock integration.
# Retain a measured slower result when the requested target is not met.
yosys -p "read_verilog core.v; synth_ecp5 -top $hw_module -json core.json" > yosys.log 2>&1
nextpnr-ecp5 --85k --package CABGA381 --speed 8 --out-of-context \
  --freq "$hw_frequency" --seed "$hw_seed" --threads 2 --timing-allow-fail --json core.json \
  --write routed.json --report timing.json \
  > nextpnr.log 2>&1
