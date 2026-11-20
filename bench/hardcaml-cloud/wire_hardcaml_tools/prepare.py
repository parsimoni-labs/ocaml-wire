#!/usr/bin/env python3
"""Prepare local Vivado/F2 build inputs; never provision or upload resources."""

import argparse
import json
import re
import shlex
import shutil
import subprocess
from pathlib import Path

from . import bundle, timing


# AWS HDK v2.3.5 build_all.tcl, DEVICE_TYPE. clk_main_a0 is fixed at 250 MHz.
PART = "xcvu47p-fsvh2892-2-e"
VIVADO = "2025.2"
PERIOD_NS = 4.0


def check_checkpoints(directory, tag):
    """The upstream packager also accepts timing-VIOLATED checkpoints."""
    if not re.fullmatch(r"[A-Za-z0-9_-]+", tag):
        raise ValueError("invalid build tag")
    directory = Path(directory)
    failed = directory / f"cl_sde.{tag}.post_route.VIOLATED.dcp"
    routed = directory / f"cl_sde.{tag}.post_route.dcp"
    archive = directory / f"{tag}.Developer_CL.tar"
    if failed.exists():
        raise ValueError("timing-violated checkpoint is not deployable")
    if not routed.is_file() or not archive.is_file():
        raise ValueError("missing routed checkpoint or deployment archive")
    if not routed.stat().st_size or not archive.stat().st_size:
        raise ValueError("empty build artifact")


def fresh(directory):
    directory = directory.resolve()
    directory.mkdir(parents=True, exist_ok=False)
    return directory


def ooc(source, destination, *, adapter=False, part=PART):
    schema = bundle.read_schema(source)
    files = {name: source / name for name in
             ("wire_stream.v", "cl_sde_srm.sv", "metadata.json")}
    return ooc_inputs(schema, files, destination, adapter=adapter, part=part)


def core_ooc(manifest, rtl, destination, *, part=PART):
    schema = timing.core_schema(json.loads(manifest.read_text()), rtl.read_bytes())
    return ooc_inputs(schema, {"wire_stream.v": rtl, "core-manifest.json": manifest},
                      destination, part=part)


def ooc_inputs(schema, files, destination, *, adapter=False, part=PART):
    if not re.fullmatch(r"x[A-Za-z0-9_-]+", part):
        raise ValueError("invalid target part")
    destination = fresh(destination)
    for name, path in files.items():
        shutil.copyfile(path, destination / name)
    (destination / "wire_timing_boundary.v").write_text(timing.boundary(schema))
    shutil.copyfile(Path(__file__).with_name("timing_reports.tcl"),
                    destination / "timing_reports.tcl")
    (destination / "stages.tcl").write_text(timing.stage_tcl(registered_boundary=not adapter))
    (destination / "stage-budgets.json").write_text(json.dumps(dict(
        kind="planning_allocation_not_measured_delay", allocation=timing.BUDGET,
        stages=timing.stages(registered_boundary=not adapter),
        note="path classes overlap and are not additional pipeline stages"),
        indent=2) + "\n")
    # All paths inside Tcl are relative to its own directory, avoiding Tcl
    # interpolation of command-line paths. External timing is an explicit model.
    script = '''cd [file dirname [file normalize [info script]]]
if {[version -short] ne "2025.2"} { error "requires Vivado 2025.2" }
create_project -in_memory -part @PART@
read_verilog -sv {@SOURCES@}
synth_design -mode out_of_context -flatten_hierarchy none -top @TOP@
create_clock -name core -period 4.000 [get_ports @CLOCK@]
set_clock_uncertainty -setup 0.200 [get_clocks core]
set inputs [remove_from_collection [all_inputs] [get_ports @CLOCK@]]
set_input_delay -clock core -max 0.250 $inputs
set_input_delay -clock core -min 0.000 $inputs
set_output_delay -clock core -max 0.250 [all_outputs]
set_output_delay -clock core -min 0.000 [all_outputs]
source timing_reports.tcl
wire_reports post_synth
opt_design
place_design -directive Explore
phys_opt_design -directive AggressiveExplore
wire_reports post_place
route_design -directive Explore
wire_reports post_route
report_drc -file drc.rpt
write_checkpoint -force routed.dcp
set setup [get_timing_paths -delay_type max -max_paths 1]
set hold [get_timing_paths -delay_type min -max_paths 1]
if {[llength $setup] != 1 || [llength $hold] != 1} { error "missing timing paths" }
set wns [get_property SLACK $setup]
set whs [get_property SLACK $hold]
set complete [expr {$wire_stage_missing == 0 ? "true" : "false"}]
set out [open metrics.json w]
puts $out [format {{"vivado":"2025.2","part":"@PART@","period_ns":4.0,"setup_uncertainty_ns":0.2,"setup_slack_ns":%s,"hold_slack_ns":%s,"stage_coverage_complete":%s}} $wns $whs $complete]
close $out
if {$wns < 0 || $whs < 0} { error "timing target missed; do not claim line rate" }
if {$wire_stage_missing != 0} { error "stage timing coverage incomplete; inspect stages.tsv" }
'''
    top, clock = ("cl_sde_srm", "clk") if adapter else (timing.TOP, "clock")
    (destination / "build.tcl").write_text(
        script.replace("@TOP@", top).replace("@CLOCK@", clock).replace("@PART@", part)
        .replace("@SOURCES@", "wire_stream.v wire_timing_boundary.v"
                 + (" cl_sde_srm.sv" if adapter else "")))
    (destination / "build.sh").write_text(
        '#!/usr/bin/env bash\nset -euo pipefail\n'
        'cd -- "$(dirname -- "$0")"\n'
        'exec vivado -mode batch -source build.tcl -log vivado.log -journal vivado.jou\n')
    command = ["bash", str(destination / "build.sh")]
    (destination / "provenance.json").write_text(json.dumps(dict(
        mode="out_of_context", part=part, vivado=VIVADO, period_ns=PERIOD_NS,
        top=top, registered_boundary=not adapter, setup_uncertainty_ns=0.2,
        external_delay_ns=0.25, layout_sha256=schema.get("layout_sha256"),
        rtl_sha256=schema["rtl_sha256"], manifest_sha256=schema.get("manifest_sha256"),
        command=command), indent=2) + "\n")
    return shlex.join(command)


def hdk(source, checkout, destination):
    schema = bundle.read_schema(source)
    checkout = checkout.resolve()
    revision = subprocess.check_output(
        ["git", "-C", str(checkout), "rev-parse", "HEAD"], text=True).strip()
    if revision != bundle.HDK_REVISION:
        raise ValueError("AWS HDK revision differs from the pinned ABI")
    subprocess.run(["git", "-C", str(checkout), "diff", "--quiet", "HEAD", "--"],
                   check=True)
    destination = destination.resolve()
    if destination.name != "cl_sde" or destination.exists():
        raise ValueError("destination must be a new directory named cl_sde")
    # Copy tracked sources only: a pinned checkout can also contain untracked
    # checkpoints from older builds. Those must never become current results.
    prefix = Path("hdk/cl/examples/cl_sde")
    tracked = subprocess.check_output(
        ["git", "-C", str(checkout), "ls-files", "-z", "--", str(prefix)])
    destination.mkdir(parents=True, exist_ok=False)
    for entry in tracked.decode().split("\0"):
        if not entry:
            continue
        path = Path(entry)
        target = destination / path.relative_to(prefix)
        target.parent.mkdir(parents=True, exist_ok=True)
        # Resolve the example's common-script symlinks; moving their relative
        # paths into an external build directory would break the build.
        shutil.copy2(checkout / path, target)
    for name in ("wire_stream.v", "cl_sde_srm.sv"):
        shutil.copyfile(source / name, destination / "design" / name)
    shutil.copyfile(source / "metadata.json", destination / "wire-metadata.json")
    q = shlex.quote
    script = "\n".join([
        "#!/usr/bin/env bash", "set -euo pipefail",
        "# Source the pinned HDK hdk_setup.sh in this shell before running.",
        'test "${VIVADO_TOOL_VERSION:-}" = "2025.2"',
        f"export CL_DIR={q(str(destination))}",
        '# One-shot build: do not reuse stale results after a failed invocation.',
        'mkdir "$CL_DIR/.wire-build-started"',
        'cd "$CL_DIR/build/scripts"',
        "python3 aws_build_dcp_from_cl.py --cl cl_sde --mode small_shell "
        "--flow BuildAll --tag wire_checked",
        'python3 -m wire_hardcaml_tools prepare check '
        '"$CL_DIR/build/checkpoints" wire_checked', "",
    ])
    (destination / "build-wire.sh").write_text(script)
    (destination / "wire-provenance.json").write_text(json.dumps(dict(
        hdk_revision=revision, vivado=VIVADO, period_ns=PERIOD_NS,
        layout_sha256=schema["layout_sha256"],
        note="build only; AFI registration and hardware replay remain separate"),
        indent=2) + "\n")
    return "bash " + q(str(destination / "build-wire.sh"))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest="command", required=True)
    for command in ("ooc", "hdk"):
        sub = commands.add_parser(command)
        sub.add_argument("bundle", type=Path)
        if command == "hdk":
            sub.add_argument("checkout", type=Path)
        else:
            sub.add_argument("--adapter", action="store_true",
                             help="time the SDE adapter with external I/O delays instead")
            sub.add_argument("--part", default=PART,
                             help="target part (default: pinned F2 device)")
        sub.add_argument("destination", type=Path)
    core = commands.add_parser("core-ooc", help="time a bare stream manifest at any supported width")
    core.add_argument("manifest", type=Path)
    core.add_argument("rtl", type=Path)
    core.add_argument("destination", type=Path)
    core.add_argument("--part", default=PART)
    check = commands.add_parser("check")
    check.add_argument("directory", type=Path)
    check.add_argument("tag")
    args = parser.parse_args()
    try:
        if args.command == "ooc":
            print(ooc(args.bundle, args.destination, adapter=args.adapter, part=args.part))
        elif args.command == "core-ooc":
            print(core_ooc(args.manifest, args.rtl, args.destination, part=args.part))
        elif args.command == "hdk":
            print(hdk(args.bundle, args.checkout, args.destination))
        else:
            check_checkpoints(args.directory, args.tag)
    except (OSError, ValueError, subprocess.CalledProcessError) as error:
        parser.exit(1, str(error) + "\n")


if __name__ == "__main__":
    main()
