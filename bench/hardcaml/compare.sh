#!/bin/sh
set -eu
: "${HW_REFERENCE:?set HW_REFERENCE to the saved reference Verilog}"
: "${HW_CANDIDATE:?set HW_CANDIDATE to the candidate Verilog}"
: "${HW_OUT:?set HW_OUT to a writable output directory}"
case "${HW_KIND:-decoder}" in
  decoder) module_name=wire_decoder_64 ;;
  checksum) module_name=wire_checksum_64 ;;
  *) echo 'HW_KIND must be decoder or checksum' >&2; exit 1 ;;
esac
command -v yosys >/dev/null
python3 - "$HW_REFERENCE" "$HW_CANDIDATE" "$HW_OUT" <<'PYTHON'
import hashlib
import os
import pathlib
import sys
reference, candidate, directory = map(pathlib.Path, sys.argv[1:])
if os.path.samefile(reference, candidate):
    raise SystemExit('save distinct reference and candidate snapshots')
# Read both before writing either output, including when inputs alias outputs.
snapshots = [('reference.v', reference.read_bytes()),
             ('candidate.v', candidate.read_bytes())]
directory.mkdir(parents=True, exist_ok=True)
for name, data in snapshots:
    (directory / name).write_bytes(data)
    print(hashlib.sha256(data).hexdigest(), name)
PYTHON
cd "$HW_OUT"
# The module name comes from the fixed enumeration above, not command text.
yosys -p "read_verilog reference.v; rename $module_name gold; read_verilog candidate.v; rename $module_name gate; miter -equiv -flatten gold gate wire_equiv; prep -top wire_equiv -flatten; sat -timeout 300 -prove trigger 0 -verify" > equivalence.log 2>&1
