"""Registered timing fixture and path-class budgets for the checked stream core.

The fixture samples all inputs and outputs every cycle. It is an OOC measurement
boundary, not an elastic AXI adapter and never an HDK deployment component.
"""

import argparse
import hashlib
import json
from pathlib import Path

from . import bundle


TOP = "wire_timing_boundary"
PERIOD_NS = 4.0
SETUP_UNCERTAINTY_NS = 0.2
# Planning allocations, not measured device delays. Actual CQ/setup, clock skew,
# logic and net delays are reported by Vivado rather than inferred from these.
BUDGET = dict(period_ns=4.0, uncertainty_ns=0.2,
              register_allowance_ns=0.4, logic_ns=2.0, routing_ns=1.4)
STAGES = {
    "input_boundary": ["*wire_boundary_input_*"],
    "ingress_keep_count": ["*wire_ingress_*"],
    "prefix_capture": ["*wire_capture_*", "*wire_frame_*"],
    "decode_extract": ["*wire_decode_extract_q_*"],
    "validation_accept": ["*wire_result_bank_*_q_accept*"],
    "result_bank": ["*wire_result_bank_*"],
    "output_boundary": ["*wire_boundary_output_*"],
}


def ports(schema):
    if "ports" in schema:
        return [(p["name"], p["width"], p["direction"]) for p in schema["ports"]]
    result = [(name, width, "input") for name, width in bundle.INPUTS.items()]
    result += [(name, 1, "output") for name in sorted(bundle.HANDSHAKE)]
    result += [(field["name"], field["width"], "output")
               for field in schema["fields"]]
    return result


def core_schema(manifest, rtl):
    """Timing accepts stream widths other than the SDE adapter's fixed 512 bits."""
    bundle.identifier(manifest["module"])
    seen, inputs, outputs = set(), {}, {}
    for port in manifest["ports"]:
        name = bundle.identifier(port["name"])
        width = bundle.integer(port["width"], name, 1)
        if name in seen or port["direction"] not in ("input", "output"):
            raise ValueError("duplicate or invalid timing port")
        seen.add(name)
        (inputs if port["direction"] == "input" else outputs)[name] = width
    fixed = {n: w for n, w in bundle.INPUTS.items() if n not in ("input_data", "input_keep")}
    if {n: w for n, w in inputs.items() if n not in ("input_data", "input_keep")} != fixed:
        raise ValueError("invalid stream timing inputs")
    if not 1 <= inputs.get("input_keep", 0) <= 1024:
        raise ValueError("invalid timing beat width")
    if inputs.get("input_data") != 8 * inputs["input_keep"]:
        raise ValueError("data and KEEP widths differ")
    if any(outputs.get(n) != 1 for n in bundle.HANDSHAKE):
        raise ValueError("invalid timing handshake outputs")
    result = dict(manifest)
    result["rtl_sha256"] = hashlib.sha256(rtl).hexdigest()
    result["manifest_sha256"] = hashlib.sha256(json.dumps(
        manifest, sort_keys=True, separators=(",", ":")).encode()).hexdigest()
    return result


def boundary(schema):
    if schema["module"] == TOP:
        raise ValueError("core module collides with timing fixture")
    interface = ports(schema)
    lines = ["// Timing fixture only: not an elastic transport adapter.",
             "module " + TOP + "("]
    def size(width):
        return "" if width == 1 else f"[{width - 1}:0] "
    lines.append(",\n".join(f" {direction} wire {size(width)}{name}"
                             for name, width, direction in interface) + ");")
    bindings = []
    for name, width, direction in interface:
        if name == "clock":
            bindings.append(".clock(clock)")
            continue
        prefix = "input" if direction == "input" else "output"
        register = f"wire_boundary_{prefix}_{name}"
        # Preserve the measurement boundaries, not the internal implementation.
        lines.append(f' (* keep = "true", dont_touch = "true" *) reg {size(width)}{register};')
        if direction == "input":
            lines.append(f" always @(posedge clock) {register} <= {name};")
            bindings.append(f".{name}({register})")
        else:
            wire = "core_" + name
            lines.append(f" wire {size(width)}{wire};")
            lines.append(f" always @(posedge clock) {register} <= {wire};")
            lines.append(f" assign {name} = {register};")
            bindings.append(f".{name}({wire})")
    lines += [' (* keep_hierarchy = "yes" *) ' + schema["module"] + " core(",
              ",\n".join(" " + binding for binding in bindings) + ");",
              "endmodule", ""]
    return "\n".join(lines)


def stages(*, registered_boundary=True):
    return {name: patterns for name, patterns in STAGES.items()
            if registered_boundary or "boundary" not in name}


def stage_tcl(*, registered_boundary=True):
    return "\n".join("wire_stage " + name + " {" + " ".join(patterns) + "}"
                     for name, patterns in stages(registered_boundary=registered_boundary).items()) + "\n"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path)
    parser.add_argument("output", type=Path)
    args = parser.parse_args()
    schema = bundle.read_schema(args.bundle)
    with args.output.open("x") as output:
        output.write(boundary(schema))


if __name__ == "__main__":
    main()
