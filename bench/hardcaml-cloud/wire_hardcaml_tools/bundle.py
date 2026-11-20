#!/usr/bin/env python3
"""Package a checked streaming circuit for the pinned AWS SDE interface.

This command generates local files. It never launches an instance, uploads an
AFI, loads hardware, or reads cloud credentials.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path


INPUTS = {
    "clock": 1, "reset": 1, "input_valid": 1, "input_data": 512,
    "input_keep": 64, "input_last": 1, "input_error": 1, "output_ready": 1,
}
STATUS = {"frame_id", "frame_length", "transport_error", "accepted",
          "in_scope", "consumed"}
HANDSHAKE = {"input_ready", "output_valid"}
HDK_REVISION = "c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82"
COUNTERS = ["received", "delivered", "accepted", "cycles",
            "input_stalls", "output_stalls", "last_input_user", "bad_writes"]


def integer(value, name, minimum=0):
    if type(value) is not int or value < minimum:
        raise ValueError(f"{name}: expected an integer >= {minimum}")
    return value


def identifier(value):
    if not isinstance(value, str) or not re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", value):
        raise ValueError("invalid RTL identifier")
    if value in {"module", "wire", "logic", "input", "output", "reg", "cl_sde_srm"}:
        raise ValueError("reserved RTL identifier")
    return value


def layout(manifest, rtl):
    if not isinstance(manifest, dict):
        raise ValueError("circuit manifest must be an object")
    identifier(manifest.get("module"))
    capture = integer(manifest.get("capture_bytes"), "capture_bytes", 1)
    maximum = integer(manifest.get("max_length"), "max_length")
    prefix = integer(manifest.get("read_prefix"), "read_prefix")
    latency = integer(manifest.get("latency_cycles"), "latency_cycles", 1)
    if any(value > 0xffffffff for value in (capture, maximum, prefix, latency)):
        raise ValueError("configuration exceeds the 32-bit SDE registers")
    if prefix > capture or manifest.get("packing") != "full_nonfinal":
        raise ValueError("SDE requires a proven capture bound and full nonfinal beats")
    ports = manifest.get("ports")
    if not isinstance(ports, list):
        raise ValueError("missing circuit ports")
    seen, inputs, outputs, fields = set(), {}, {}, []
    offset = 0
    for port in ports:
        if not isinstance(port, dict):
            raise ValueError("circuit port must be an object")
        name = identifier(port.get("name"))
        width = integer(port.get("width"), name + ".width", 1)
        compact = integer(port.get("wire_width"), name + ".wire_width", 1)
        signed = port.get("signed")
        if name in seen or type(signed) is not bool or compact > width:
            raise ValueError("duplicate port or invalid compact representation")
        seen.add(name)
        if port.get("direction") == "input":
            inputs[name] = width
        elif port.get("direction") == "output":
            outputs[name] = width
            if name in HANDSHAKE:
                if width != 1 or compact != 1 or signed:
                    raise ValueError("invalid handshake port")
                continue
            if name not in STATUS and not name.startswith("field_"):
                raise ValueError("unknown metadata port: " + name)
            if width > 64 or compact > 64:
                raise ValueError("metadata scalar exceeds 64 bits")
            fields.append(dict(name=name, width=width, wire_width=compact,
                               signed=signed, offset=offset))
            offset += compact
        else:
            raise ValueError("invalid port direction")
    if inputs != INPUTS or not HANDSHAKE <= outputs.keys() or not STATUS <= outputs.keys():
        raise ValueError("stream circuit does not implement the required interface")
    if offset > 512:
        raise ValueError("metadata exceeds one SDE beat; select fewer outputs")
    by_name = {field["name"]: field for field in fields}
    for name, bound in [("frame_length", maximum + 1), ("consumed", maximum)]:
        if (by_name[name]["signed"] or
                by_name[name]["wire_width"] < max(1, bound.bit_length())):
            raise ValueError(name + " cannot represent the configured extent")
    for name, width in {"accepted": 1, "in_scope": 1, "transport_error": 3,
                        "frame_id": 64}.items():
        if (by_name[name]["width"] != width or
                by_name[name]["wire_width"] != width or by_name[name]["signed"]):
            raise ValueError("invalid status representation: " + name)
    if by_name["frame_id"]["wire_width"] != 64 or by_name["frame_id"]["signed"]:
        raise ValueError("frame_id must be unsigned 64-bit")
    result = dict(abi_version=1, record_bytes=64, metadata_bits=offset,
                  module=manifest["module"], capture_bytes=capture,
                  max_length=maximum, read_prefix=prefix, latency_cycles=latency,
                  packing=manifest["packing"], fields=fields,
                  rtl_sha256=hashlib.sha256(rtl).hexdigest(), hdk_revision=HDK_REVISION)
    canonical = json.dumps(result, sort_keys=True, separators=(",", ":")).encode()
    result["layout_sha256"] = hashlib.sha256(canonical).hexdigest()
    return result


def encode_record(schema, values):
    if set(values) != {field["name"] for field in schema["fields"]}:
        raise ValueError("metadata field set differs from the ABI")
    word = 0
    for field in schema["fields"]:
        value = values[field["name"]]
        if type(value) is not int:
            raise ValueError("metadata values must be integers")
        bits = field["wire_width"]
        low = -(1 << (bits - 1)) if field["signed"] else 0
        high = (1 << (bits - int(field["signed"]))) - 1
        if not low <= value <= high:
            raise ValueError("metadata value is outside its wire range")
        word |= (value & ((1 << bits) - 1)) << field["offset"]
    return word.to_bytes(schema["record_bytes"], "little")


def decode_record(schema, data):
    if len(data) != schema["record_bytes"]:
        raise ValueError("truncated metadata record")
    word = int.from_bytes(data, "little")
    if word >> schema["metadata_bits"]:
        raise ValueError("nonzero metadata padding")
    values = {}
    for field in schema["fields"]:
        bits = field["wire_width"]
        value = (word >> field["offset"]) & ((1 << bits) - 1)
        if field["signed"] and value & (1 << (bits - 1)):
            value -= 1 << bits
        values[field["name"]] = value
    return values


def render(schema):
    lines = ["// Generated from the checked circuit manifest; see metadata.json.",
             "module cl_sde_srm(",
             " input wire clk, rst_n,",
             " input wire [11:0] cfg_srm_addr,",
             " input wire cfg_srm_wr, cfg_srm_rd,",
             " input wire [31:0] cfg_srm_wdata,",
             " output reg srm_cfg_ack, output reg [31:0] srm_cfg_rdata,",
             " input wire ins_valid, input wire [511:0] ins_data,",
             " input wire [63:0] ins_keep, ins_user, input wire ins_last,",
             " output wire ins_ready, output wire ots_valid,",
             " output wire [511:0] ots_data, output wire [63:0] ots_keep, ots_user,",
             " output wire ots_last, input wire ots_ready);",
             " wire core_input_ready, core_output_valid;",
             " wire [511:0] packed_metadata; wire range_fault;",
             " reg abi_fault;"]
    for field in schema["fields"]:
        lines.append(f" wire [{field['width'] - 1}:0] core_{field['name']};")
    bindings = dict(clock="clk", reset="!rst_n", input_valid="ins_valid && rst_n",
                    input_data="ins_data", input_keep="ins_keep", input_last="ins_last",
                    input_error="1'b0", output_ready="ots_ready && rst_n",
                    input_ready="core_input_ready", output_valid="core_output_valid")
    bindings.update({f["name"]: "core_" + f["name"] for f in schema["fields"]})
    lines.append(" " + schema["module"] + " parser(")
    lines.append(",\n".join(f" .{name}({value})" for name, value in bindings.items()) + ");")
    faults = []
    for field in schema["fields"]:
        name, bits, width = field["name"], field["wire_width"], field["width"]
        value = f"core_{name}"
        if bits < width:
            extension = value + f"[{bits - 1}]" if field["signed"] else "1'b0"
            faults.append(f"({value}[{width - 1}:{bits}] != {{{width - bits}{{{extension}}}}})")
        part = value if bits == width else value + f"[{bits - 1}:0]"
        if name.startswith("field_") or name == "consumed":
            part = f"(!core_accepted || range_fault ? {bits}'d0 : {part})"
        elif name in {"accepted", "in_scope"}:
            part = f"(range_fault ? {bits}'d0 : {part})"
        lines.append(f" assign packed_metadata[{field['offset']} +: {bits}] = {part};")
    if schema["metadata_bits"] < 512:
        lines.append(f" assign packed_metadata[511:{schema['metadata_bits']}] = 0;")
    lines.append(" assign range_fault = " + (" | ".join(faults) if faults else "1'b0") + ";")
    lines += [" assign ins_ready = rst_n && core_input_ready;",
              " assign ots_valid = rst_n && core_output_valid;",
              " assign ots_data = packed_metadata;",
              " assign ots_keep = 64'hffffffffffffffff;",
              " assign ots_last = 1'b1; assign ots_user = 64'd0;"]
    for counter in COUNTERS:
        lines.append(f" reg [63:0] {counter}, snapshot_{counter};")
    lines += [" always @(posedge clk) begin", "  if (!rst_n) begin",
              "   srm_cfg_ack <= 0; srm_cfg_rdata <= 0; abi_fault <= 0;"]
    for counter in COUNTERS:
        lines.append(f"   {counter} <= 0; snapshot_{counter} <= 0;")
    lines += ["  end else begin",
              "   srm_cfg_ack <= cfg_srm_wr || cfg_srm_rd;",
              "   cycles <= cycles + 1;",
              "   if (ins_valid && ins_ready && ins_last) begin",
              "    received <= received + 1; last_input_user <= ins_user; end",
              "   if (ots_valid && ots_ready) begin",
              "    delivered <= delivered + 1;",
              "    if (core_accepted && !range_fault) accepted <= accepted + 1; end",
              "   if (ins_valid && !ins_ready) input_stalls <= input_stalls + 1;",
              "   if (ots_valid && !ots_ready) output_stalls <= output_stalls + 1;",
              "   if (core_output_valid && range_fault) abi_fault <= 1;",
              "   if (cfg_srm_wr) begin",
              "    if (cfg_srm_addr == 12'h060 && cfg_srm_wdata == 1) begin"]
    for counter in COUNTERS:
        lines.append(f"     snapshot_{counter} <= {counter};")
    lines += ["    end else bad_writes <= bad_writes + 1; end",
              "   if (cfg_srm_rd) case (cfg_srm_addr)"]
    registers = {0: 0x57495245, 4: 1, 8: schema["metadata_bits"], 12: 64,
                 16: schema["max_length"], 20: schema["capture_bytes"],
                 24: schema["latency_cycles"], 28: schema["read_prefix"]}
    for address, value in registers.items():
        lines.append(f"    12'h{address:03x}: srm_cfg_rdata <= 32'h{value:08x};")
    for index, counter in enumerate(COUNTERS):
        address = 0x100 + index * 8
        lines += [f"    12'h{address:03x}: srm_cfg_rdata <= snapshot_{counter}[31:0];",
                  f"    12'h{address + 4:03x}: srm_cfg_rdata <= snapshot_{counter}[63:32];"]
    digest = bytes.fromhex(schema["layout_sha256"])
    for index in range(8):
        value = int.from_bytes(digest[4 * index:4 * index + 4], "little")
        lines.append(f"    12'h{0x40 + 4 * index:03x}: srm_cfg_rdata <= 32'h{value:08x};")
    lines += ["    12'h064: srm_cfg_rdata <= {31'd0, abi_fault};",
              "    default: srm_cfg_rdata <= 32'hbad10001;",
              "   endcase", "  end", " end", "endmodule", ""]
    return "\n".join(lines)


def package(manifest_path, rtl_path, destination):
    rtl = rtl_path.read_bytes()
    schema = layout(json.loads(manifest_path.read_text()), rtl)
    content = {"wire_stream.v": rtl, "cl_sde_srm.sv": render(schema).encode(),
               "metadata.json": (json.dumps(schema, indent=2) + "\n").encode()}
    destination.mkdir(parents=True, exist_ok=True)
    for name, data in content.items():
        path = destination / name
        if path.exists() and path.read_bytes() != data:
            raise ValueError("refusing to replace a different artifact: " + str(path))
    for name, data in content.items():
        (destination / name).write_bytes(data)
    return schema


def read_schema(directory):
    """Recheck the ABI seal and all offsets before consuming device records."""
    schema = json.loads((directory / "metadata.json").read_text())
    if not isinstance(schema, dict) or not isinstance(schema.get("fields"), list):
        raise ValueError("invalid metadata schema")
    if not all(isinstance(field, dict) for field in schema["fields"]):
        raise ValueError("invalid metadata fields")
    ports = [dict(name=n, width=w, wire_width=w, signed=False, direction="input")
             for n, w in INPUTS.items()]
    ports += [dict(name=n, width=1, wire_width=1, signed=False, direction="output")
              for n in sorted(HANDSHAKE)]
    ports += [dict(field, direction="output") for field in schema["fields"]]
    source = dict(schema, ports=ports)
    actual = layout(source, (directory / "wire_stream.v").read_bytes())
    if schema != actual:
        raise ValueError("metadata schema or RTL differs from its sealed ABI")
    if (directory / "cl_sde_srm.sv").read_text() != render(schema):
        raise ValueError("SDE adapter differs from this ABI implementation")
    return schema


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("manifest", type=Path)
    parser.add_argument("rtl", type=Path)
    parser.add_argument("destination", type=Path)
    args = parser.parse_args()
    try:
        result = package(args.manifest, args.rtl, args.destination)
    except (OSError, ValueError, TypeError, KeyError) as error:
        parser.exit(1, str(error) + "\n")
    print(json.dumps(dict(metadata_bits=result["metadata_bits"],
                          layout_sha256=result["layout_sha256"])))


if __name__ == "__main__":
    main()
