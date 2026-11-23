#!/usr/bin/env python3
"""Replay the packaged SDE component in RTL against existing codec references."""

import argparse
import json
import shutil
import subprocess
from pathlib import Path

from . import bundle, oracle_cache, verify


def frames(channel, slot):
    while True:
        prefix = channel.read(slot or 4)
        if not prefix:
            return
        if len(prefix) != (slot or 4):
            raise ValueError("truncated corpus slot/length")
        if slot:
            yield prefix
        else:
            length = int.from_bytes(prefix, "little")
            if length > 1024 * 1024:
                raise ValueError("local RTL replay limits each frame to 1 MiB")
            data = channel.read(length)
            if len(data) != length:
                raise ValueError("truncated corpus frame")
            yield data


def testbench(schema, beats, records, accepted):
    limit = (beats + records + 128) * 32
    if not 0 < beats or not 0 < records or limit >= 1 << 31:
        raise ValueError("empty or oversized RTL replay")
    lines = ["module tb;", " reg clk = 0; always #5 clk = !clk;",
             " reg rst_n = 0, cfg_srm_wr = 0, cfg_srm_rd = 0;",
             " reg [11:0] cfg_srm_addr = 0; reg [31:0] cfg_srm_wdata = 0;",
             " wire srm_cfg_ack; wire [31:0] srm_cfg_rdata;",
             " reg ins_valid = 0, ins_last = 0, ots_ready = 0;",
             " reg [511:0] ins_data = 0; reg [63:0] ins_keep = 0;",
             " reg [63:0] ins_user = 64'h0123456789abcdef;",
             " wire ins_ready, ots_valid, ots_last;",
             " wire [511:0] ots_data; wire [63:0] ots_keep, ots_user;",
             " cl_sde_srm dut(.*);",
             f" reg [576:0] stimulus [0:{beats - 1}];",
             f" reg [511:0] expected [0:{records - 1}];",
             " integer sent = 0, received = 0, cycles = 0;",
             " integer warm_sent = 0, warm_cycle;",
             " integer capture, byte_index;",
             " reg input_fired = 0, was_stalled = 0; reg [511:0] stalled_data;",
             " task automatic read_csr(input [11:0] addr, input [31:0] value);",
             "  @(negedge clk); cfg_srm_addr = addr; cfg_srm_rd = 1;",
             "  @(posedge clk); #1;",
             "  if (!srm_cfg_ack || srm_cfg_rdata !== value)",
             "   $fatal(1, \"CSR %h: got %h expected %h\", addr, srm_cfg_rdata, value);",
             "  @(negedge clk); cfg_srm_rd = 0; endtask",
             " task automatic snapshot;",
             "  @(negedge clk); cfg_srm_addr = 12'h060; cfg_srm_wdata = 1; cfg_srm_wr = 1;",
             "  @(posedge clk); #1; if (!srm_cfg_ack) $fatal(1, \"snapshot not acknowledged\");",
             "  @(negedge clk); cfg_srm_wr = 0; endtask",
             " initial begin",
             "  capture = $fopen(\"device-records.bin\", \"wb\");",
             "  if (capture == 0) $fatal(1, \"cannot open capture\");",
             "  $readmemh(\"stimulus.hex\", stimulus); $readmemh(\"expected.hex\", expected);",
             "  repeat (3) @(posedge clk); @(negedge clk); rst_n = 1;",
             "  for (warm_cycle = 0; warm_cycle < 32; warm_cycle = warm_cycle + 1) begin",
             "   @(negedge clk);",
             "   if (!ins_valid || input_fired) begin",
             f"    ins_valid = warm_sent < {min(beats, 4)};",
             "    if (ins_valid) begin",
             "     ins_data = stimulus[warm_sent][511:0]; ins_keep = stimulus[warm_sent][575:512];",
             "     ins_last = stimulus[warm_sent][576]; end end",
             "   @(posedge clk); input_fired = ins_valid && ins_ready;",
             "   if (input_fired) warm_sent = warm_sent + 1; end",
             "  @(negedge clk); rst_n = 0; ins_valid = 0; input_fired = 0;",
             "  repeat (3) @(posedge clk); #1;",
             "  if (ots_valid || ins_ready) $fatal(1, \"stream active during reset\");",
             "  @(negedge clk); rst_n = 1;",
             "  read_csr(12'h000, 32'h57495245); read_csr(12'h004, 1);",
             f"  read_csr(12'h008, {schema['metadata_bits']}); read_csr(12'h00c, 64);"]
    for address, key in [(0x10, "max_length"), (0x14, "capture_bytes"),
                         (0x18, "latency_cycles"), (0x1C, "read_prefix")]:
        lines.append(f"  read_csr(12'h{address:03x}, {schema[key]});")
    digest = bytes.fromhex(schema["layout_sha256"])
    for index in range(8):
        value = int.from_bytes(digest[4 * index:4 * index + 4], "little")
        lines.append(f"  read_csr(12'h{0x40 + 4 * index:03x}, 32'h{value:08x});")
    lines += ["  read_csr(12'h001, 32'hbad10001); read_csr(12'h064, 0);",
              f"  while (received < {records} && cycles < {limit}) begin",
              "   @(negedge clk);",
              "   if (!ins_valid || input_fired) begin",
              f"    ins_valid = sent < {beats} && cycles % 5 != 1;",
              "    if (ins_valid) begin",
              "     ins_data = stimulus[sent][511:0]; ins_keep = stimulus[sent][575:512];",
              "     ins_last = stimulus[sent][576]; end end",
              "   ots_ready = cycles % 97 >= 32 && cycles % 7 != 2;",
              "   @(posedge clk);",
              "   if (was_stalled && (!ots_valid || ots_data !== stalled_data ||",
              "                       !ots_last || ots_keep !== 64'hffffffffffffffff || ots_user !== 0))",
              "    $fatal(1, \"metadata changed during backpressure\");",
              "   was_stalled = ots_valid && !ots_ready; stalled_data = ots_data;",
              "   input_fired = ins_valid && ins_ready; if (input_fired) sent = sent + 1;",
              "   if (ots_valid && ots_ready) begin",
              "    if (!ots_last || ots_keep !== 64'hffffffffffffffff || ots_user !== 0)",
              "     $fatal(1, \"invalid SDE metadata framing\");",
              "    if (ots_data !== expected[received])",
              "     $fatal(1, \"record %0d got %h expected %h\", received, ots_data, expected[received]);",
              "    for (byte_index = 0; byte_index < 64; byte_index = byte_index + 1)",
              "     $fwrite(capture, \"%c\", ots_data[8 * byte_index +: 8]);",
              "    received = received + 1; end",
              "   cycles = cycles + 1; end",
              f"  if (sent != {beats} || received != {records}) $fatal(1, \"RTL replay timed out\");",
              "  @(negedge clk); ins_valid = 0; ots_ready = 1;",
              "  repeat (5) begin @(posedge clk); if (ots_valid) $fatal(1, \"extra metadata\"); end",
              "  snapshot();"]
    for address, value in [(0x100, records), (0x108, records), (0x110, accepted)]:
        lines += [f"  read_csr(12'h{address:03x}, 32'h{value & 0xffffffff:08x});",
                  f"  read_csr(12'h{address + 4:03x}, 32'h{value >> 32:08x});"]
    lines += ["  read_csr(12'h130, 32'h89abcdef); read_csr(12'h134, 32'h01234567);",
              "  read_csr(12'h138, 0); read_csr(12'h13c, 0); read_csr(12'h064, 0);",
              "  $fclose(capture);",
              f"  $display(\"PASS: {records} reference records, %0d cycles\", cycles);",
              "  $finish; end", "endmodule", ""]
    return "\n".join(lines)


def run(directory, corpus, references, slot, destination, verilator):
    schema = bundle.read_schema(directory)
    if slot < 0 or slot > 1024 * 1024:
        raise ValueError("invalid corpus slot")
    destination.mkdir(parents=True, exist_ok=True)
    beat_count = count = accepted = 0
    with corpus.open("rb") as source, references.open() as expected, \
            (destination / "stimulus.hex").open("w") as stimulus, \
            (destination / "expected.hex").open("w") as results:
        for count, frame in enumerate(frames(source, slot), 1):
            line = expected.readline()
            if not line:
                raise ValueError("corpus has more frames than the reference")
            record = json.loads(line)
            if record["frame_id"] != count - 1:
                raise ValueError("reference frame IDs are not ordered from zero")
            encoded = bundle.encode_record(schema, record)
            results.write(f"{int.from_bytes(encoded, 'little'):0128x}\n")
            accepted += record["accepted"]
            parts = [frame[i:i + 64] for i in range(0, len(frame), 64)] or [b""]
            for index, part in enumerate(parts):
                word = int.from_bytes(part.ljust(64, b"\xa5"), "little")
                word |= ((1 << len(part)) - 1) << 512
                word |= int(index == len(parts) - 1) << 576
                stimulus.write(f"{word:0145x}\n")
                beat_count += 1
        if expected.read(1):
            raise ValueError("reference has more frames than the corpus")
    (destination / "tb.sv").write_text(testbench(schema, beat_count, count, accepted))
    command = [verilator, "--binary", "--timing", "--assert", "--timescale", "1ns/1ps",
               "--top-module", "tb", "-j", "2", "-Mdir", str(destination / "obj_dir"),
               "-o", "replay", str(destination / "tb.sv"),
               str(directory / "cl_sde_srm.sv"), str(directory / "wire_stream.v")]
    with (destination / "build.log").open("w") as log:
        subprocess.run(command, cwd=destination, stdout=log, stderr=subprocess.STDOUT, check=True)
    result = subprocess.run([str(destination / "obj_dir/replay")], cwd=destination,
                            text=True, capture_output=True)
    (destination / "simulation.log").write_text(result.stdout + result.stderr)
    result.check_returncode()
    with references.open() as expected, (destination / "device-records.bin").open("rb") as records:
        verify.verify(directory, expected, records)
    print(result.stdout, end="")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("bundle", type=Path)
    parser.add_argument("corpus", type=Path)
    parser.add_argument("reference", type=Path)
    parser.add_argument("destination", type=Path)
    parser.add_argument("--slot", type=int, default=0)
    parser.add_argument("--oracle-receipt", type=Path,
                        help="verify frozen oracle provenance without running EverParse")
    parser.add_argument("--verilator", default=shutil.which("verilator"))
    args = parser.parse_args()
    if not args.verilator:
        parser.error("Verilator is required")
    try:
        if args.oracle_receipt:
            schema = bundle.read_schema(args.bundle)
            oracle_cache.validate(args.oracle_receipt, args.corpus, args.reference,
                                  maximum=schema["max_length"], slot=args.slot)
        run(args.bundle.resolve(), args.corpus.resolve(), args.reference.resolve(), args.slot,
            args.destination.resolve(), args.verilator)
    except (OSError, ValueError, KeyError, subprocess.CalledProcessError) as error:
        parser.exit(1, str(error) + "\n")


if __name__ == "__main__":
    main()
