#!/usr/bin/env python3
"""Inspect the registered fixture in an existing synth_xilinx XCUP JSON.

Depth counts mapped cells, not nanoseconds. Multi-output combinational cells
conservatively depend on all input pins. Every FF boundary stops traversal.
"""

import argparse
from collections import Counter, defaultdict
from functools import lru_cache
import fnmatch
import hashlib
import json
import re
from pathlib import Path

from . import timing


COMBINATIONAL = {"LUT1", "LUT2", "LUT3", "LUT4", "LUT5", "LUT6", "LUT6_2",
                 "MUXF7", "MUXF8", "MUXF9", "CARRY4", "CARRY8", "INV", "GND", "VCC"}
SEQUENTIAL = {"FDRE", "FDSE", "FDCE", "FDPE"}


def numeric(bits):
    return {bit for bit in bits if isinstance(bit, int)}


def matches(name, patterns):
    return any(fnmatch.fnmatchcase(name, pattern) for pattern in patterns)


class Netlist:
    def __init__(self, module, rtl):
        self.module = module
        self.cells = module["cells"]
        self.drivers = {}
        self.loads = defaultdict(list)
        self.aliases = defaultdict(list)
        self.q = {}
        self.ff_bits = set()
        self.register_targets = {}
        sources = {str(path.resolve()): path.read_text().splitlines() for path in rtl}
        for name, net in module["netnames"].items():
            for index, bit in enumerate(net["bits"]):
                if isinstance(bit, int):
                    self.aliases[bit].append(f"{name}[{index}]")
        for name, cell in self.cells.items():
            kind = cell["type"]
            if kind == "$scopeinfo":
                continue
            if kind not in COMBINATIONAL | SEQUENTIAL:
                raise ValueError(f"unsupported mapped cell {kind}: {name}")
            for port, bits in cell["connections"].items():
                direction = cell["port_directions"][port]
                for bit in (bit for bit in bits if isinstance(bit, int)):
                    if direction == "output":
                        previous = self.drivers.setdefault(bit, (name, port))
                        if previous != (name, port):
                            raise ValueError(f"multiple drivers for bit {bit}")
                    else:
                        self.loads[bit].append((name, port))
            if kind in SEQUENTIAL:
                self.q[name] = numeric(cell["connections"]["Q"])
                self.ff_bits.update(self.q[name])
                targets = set()
                for location in cell.get("attributes", {}).get("src", "").split("|"):
                    match = re.fullmatch(r"(.+):(\d+)\.\d+-(\d+)\.\d+", location)
                    if match and str(Path(match[1]).resolve()) in sources:
                        lines = sources[str(Path(match[1]).resolve())]
                        fragment = "\n".join(lines[int(match[2])-1:int(match[3])])
                        targets.update(re.findall(r"\b([A-Za-z_][A-Za-z_0-9]*)\s*<=", fragment))
                self.register_targets[name] = targets
                for bit in self.q[name]:
                    self.aliases[bit].extend("source:" + target for target in targets)


    def label(self, bit):
        names = self.aliases[bit]
        named = [name for name in names if "wire_" in name]
        return min(named or names, default=str(bit))

    def group(self, spec):
        names = [
            name for name in self.module["netnames"]
            if matches(name, spec.get("nets", []))
            and not matches(name, spec.get("exclude", []))
        ]
        bits = set()
        for name in names:
            bits.update(numeric(self.module["netnames"][name]["bits"]))
        registers = {name for name, q in self.q.items() if q & bits}
        provenance = {name for name, targets in self.register_targets.items()
                      if any(matches(target, spec.get("nets", []))
                             and not matches(target, spec.get("exclude", []))
                             for target in targets)}
        if not spec.get("probe", False):
            registers.update(provenance)
            for name in provenance:
                bits.update(self.q[name])

        if not spec.get("probe", False):
            bits &= self.ff_bits
            names = [name for name in names
                     if numeric(self.module["netnames"][name]["bits"]) & bits]
        return {"nets": names, "bits": bits, "registers": registers, "provenance_registers": provenance}

    def fanout(self, bits):
        bit = max(bits, key=lambda b: len(self.loads[b]), default=None)
        if bit is None:
            return {"max_input_pin_loads": 0}
        return {"max_input_pin_loads": len(self.loads[bit]),
                "net": self.label(bit)}

    def depth(self, sources):
        active = set()

        @lru_cache(None)
        def walk(bit):
            if bit in sources:
                return (0, bit, ())
            if bit in self.ff_bits or bit not in self.drivers:
                return None
            name, _ = self.drivers[bit]
            cell = self.cells[name]
            if cell["type"] in {"GND", "VCC"}:
                return None
            if bit in active:
                raise ValueError("combinational cycle in mapped graph")
            active.add(bit)
            incoming = []
            for port, values in cell["connections"].items():
                if cell["port_directions"][port] == "input":
                    # O5 ignores I5; treating it as a dependency can invent a
                    # cycle when a dual-output LUT feeds its own other half.
                    if cell["type"] == "LUT6_2" and self.drivers[bit][1] == "O5" and port == "I5":
                        continue
                    incoming.extend(walk(value) for value in numeric(values))
            active.remove(bit)
            reached = [item for item in incoming if item is not None]
            if not reached:
                return None
            depth, origin, cells = max(reached, key=lambda item: item[0])
            return (depth + 1, origin, cells + (name,))

        return walk

    def endpoints(self, registers):
        for name in sorted(registers):
            cell = self.cells[name]
            for port, values in cell["connections"].items():
                if cell["port_directions"][port] == "input" and port not in {"C", "CLK"}:
                    for bit in numeric(values):
                        yield f"{name}.{port}", bit

    def path(self, sources, registers, to_outputs=False):
        walk = self.depth(sources)
        endpoints = list(self.endpoints(registers))
        if to_outputs:
            for name, port in self.module["ports"].items():
                if port["direction"] == "output":
                    endpoints.extend((name, bit) for bit in numeric(port["bits"]))
        reached = [(name, walk(bit)) for name, bit in endpoints]
        reached = [(name, path) for name, path in reached if path is not None]
        if not reached:
            return {"maximum_cell_depth": None, "reached_endpoint_pins": 0}
        name, (depth, origin, cells) = max(reached, key=lambda item: item[1][0])
        return {"maximum_cell_depth": depth, "reached_endpoint_pins": len(reached),
                "source": self.label(origin), "endpoint": name,
                "cell_types_on_longest_path": dict(Counter(self.cells[c]["type"] for c in cells))}


def analyse(path, timing, rtl):
    data = path.read_bytes()
    module = json.loads(data)["modules"][timing.TOP]
    graph = Netlist(module, rtl)
    groups = {name: graph.group({"nets": patterns})
              for name, patterns in timing.STAGES.items()}
    for name, group in groups.items():
        if not group["registers"]:
            raise ValueError(f"missing stage FF anchors: {name}; analyse the registered fixture")
    paths = [
        ("ingress_keep_count", ["input_boundary", "ingress_keep_count"], ["ingress_keep_count"]),
        ("prefix_capture", ["input_boundary", "ingress_keep_count", "prefix_capture"], ["prefix_capture"]),
        ("decode_extract", ["prefix_capture"], ["decode_extract"]),
        ("validation_accept", ["decode_extract"], ["validation_accept"]),
        ("result_store", ["decode_extract"], ["result_bank"]),
        ("result_readout", ["result_bank"], ["output_boundary"]),
    ]
    report = {
        "netlist_sha256": hashlib.sha256(data).hexdigest(),
        "module": timing.TOP,
        "rtl_sha256": {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in rtl},
        "analysis": "XCUP mapped-cell dependency upper bounds; no physical delays",
        "planning_budget_ns": timing.BUDGET,
        "stage_selectors": timing.STAGES,
        "cell_counts": dict(Counter(cell["type"] for cell in graph.cells.values()
                                    if cell["type"] != "$scopeinfo")),
        "groups": {}, "paths": {}, "probes": {},
        "sequential_cells": len(graph.q),
        "classified_sequential_cells": len(set().union(*(g["registers"] for g in groups.values()))),
    }
    for name, group in groups.items():
        report["groups"][name] = {
            "named_vectors": len(group["nets"]),
            "unique_q_bits": len(group["bits"]),
            "registers": len(group["registers"]),
            "source_provenance_registers": len(group["provenance_registers"]),
            **graph.fanout(group["bits"]),
        }
    for name, source_groups, target_groups in paths:
        sources = set().union(*(groups[name]["bits"] for name in source_groups))
        registers = set().union(*(groups[name]["registers"] for name in target_groups))
        report["paths"][name] = {
            "target_period_ns": timing.PERIOD_NS,
            "achieved_delay_ns": None,
            **graph.path(sources, registers),
        }
    probes = {
        "keep": ["*wire_ingress_keep_*", "*wire_ingress_beat_bytes"],
        "length_sum": ["*wire_ingress_length_sum", "*wire_ingress_overflow"],
        "validation": ["*wire_decode_accept", "*wire_decode_consumed", "*wire_decode_scope_next"],
        "result_head": ["*wire_result_head_*"],
    }
    for name, patterns in probes.items():
        probe = graph.group({"nets": patterns, "probe": True})
        report["probes"][name] = {"matched_vectors": probe["nets"], **graph.fanout(probe["bits"])}
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("netlist", type=Path)
    parser.add_argument("--rtl", type=Path, nargs="+", required=True,
                        help="exact emitted RTL files read to produce the mapped netlist")
    args = parser.parse_args()
    print(json.dumps(analyse(args.netlist, timing, args.rtl), indent=2))


if __name__ == "__main__":
    main()
