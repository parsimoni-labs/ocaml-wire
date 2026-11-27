# Bounded Hardcaml decoder

Experimental. Target-device timing is not measured yet.

This experimental backend lowers the existing checked scanner plan to hardware.
`Packet_stack.codec` remains the protocol definition; no separate protocol RTL
or parser oracle is maintained. Byte zero occupies the low eight input bits.
The measurement circuit accepts a 512-bit frame word and a 0–64-byte length.
Larger frames are outside this interface, rather than codec rejections.

The backend retains prefix checks, bounded regions, dispatch, optionals and
fragment rules. DSL arithmetic keeps native 63-bit semantics. Bounded cursors
cannot publish an overflowing result. Conditional cursors reuse the OCaml
scanner's branch facts, allowing mutually exclusive alternatives to share
header extraction. One optional register cut separates extraction from
validation; the same lowering builds combinational and pipelined decoders.

The backend supports capacities 1–256 bytes and the shared scanner's supported
scalar shapes, up to 32-bit storage. Unsupported expressions or incomplete
validation projections fail generation. This is a decoder prototype with a
complete-frame input, not a streaming interface or coverage of every Wire
combinator.

## Setup

Use a stock OCaml 5.4.1 development switch. Hardcaml and Base are declared
optional dependencies in `dune-project` and `wire.opam`; install them explicitly
for this opt-in experiment. The normal Wire package does not require hardware
tools. From a fresh checkout:

```sh
opam switch create . ocaml-base-compiler.5.4.1
opam install . --deps-only --with-test
opam install hardcaml.v0.17.1
hw() {
  opam exec -- env BUILD_HARDCAML=1 "$@"
}
mkdir -p _build/hardcaml-artifacts
```

Synthesis needs `yosys` and `nextpnr-ecp5` on PATH. The ARM64 macOS experiment
uses [YosysHQ OSS CAD Suite 2026-11-10](https://github.com/YosysHQ/oss-cad-suite-build/releases/tag/2026-11-10),
archive `oss-cad-suite-darwin-arm64-20261110.tgz`, SHA-256
`168a0e93a8065bf4bfb3ffb58c7f41928d8b6f50bcc5fe11a4665ddbbfa48edd`.
Versions are Yosys 0.70+368, nextpnr 0.11.1-89 and Verilator 5.055.
For isolated execution, temporary PATH links to the native `libexec/yosys`,
`libexec/yosys-abc` and `libexec/nextpnr-ecp5` avoid the distribution wrappers'
home-directory preferences. Verilator uses its `bin/verilator` entrypoint.

## Correctness

The hardware lanes extend `Diff_ocaml.check_scan` and reuse `Diff_codecs`
corpora, generated OCaml readers, normal/general Codec and the actual
Wire_3d → [EverParse 3D](https://project-everest.github.io/everparse/3d.html)
C oracle. All 20 structured scan cases run by default. Capacity skips are
counted explicitly. Tests vary unused physical bits, invalid physical bounds and
capture enables, checking every scalar/status output while registers hold.
The recorded run, made before the port onto the current plan, covered the 19
cases of that time with 20,000 inputs each in combinational, registered and
internally pipelined modes.

```sh
hw env BUILD_EVERPARSE=1 WIRE_3D_JOBS=1 WIRE_FUZZ_REPEAT=20000 \
  dune build --root . --build-dir _build_hw --profile release -j 2 \
  @fuzz/diff/hardcaml/runtest
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/rust/bench.exe -- corpus _build/hardcaml-artifacts 200000
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml/test.exe -- _build/hardcaml-artifacts/fixed64-random.bin
```

`WIRE_HARDCAML_CASE` selects one case; `WIRE_HARDCAML_CAPACITY` changes the
fuzzer capacity. Both are Dune action dependencies. Boundary capacities
1, 2, 3, 4, 7, 8, 15, 16, 31, 32, 63, 64, 65, 127, 128, 129, 255 and 256
are exercised using the same driver.

The registered circuit includes flow control, reset recovery and the same
field checksum as the CPU benchmark. This is benchmark work, not a network
checksum or Ethernet CRC. The checksum consumer uses zero-default fields and
a bounded balanced sum; its emitted RTL was SAT-equivalent to the original
conditional 64-bit expression for every frame and length.

```sh
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml/gen.exe -- --padding _build/hardcaml-artifacts/padding.v
HW_MITER="$PWD/_build/hardcaml-artifacts/padding.v" HW_OUT="$PWD/_build/hardcaml-artifacts/padding-proof" \
  bench/hardcaml/prove-padding.sh
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml/gen.exe -- --pipeline-proof _build/hardcaml-artifacts/pipeline.v
HW_MITER="$PWD/_build/hardcaml-artifacts/pipeline.v" HW_OUT="$PWD/_build/hardcaml-artifacts/pipeline-proof" \
  bench/hardcaml/prove-pipeline.sh
```

The padding miter proves unused physical/trailing bytes cannot affect results.
Temporal induction compares the pipelined decoder with a registered
combinational decoder, including arbitrary capture enables after reset.
`gen --decoder` and `gen --checksum` export combinational comparison circuits;
`compare.sh` checks saved reference/candidate snapshots and records hashes.
These are relative hardware properties. EverParse's proof verifies its C
oracle and does not transfer to RTL. Nested scalar values use Codec as their
reference; C independently checks acceptance and consumed length.

## Timing experiment

```sh
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml/gen.exe -- _build/hardcaml-artifacts/core.v
HW_TOP="$PWD/_build/hardcaml-artifacts/core.v" HW_OUT="$PWD/_build/hardcaml-artifacts/routing" \
  HW_FREQ=125 HW_SEED=1 bench/hardcaml/synth.sh
```

Routing targets LFE5U-85F, CABGA381, speed grade 8, using nextpnr's experimental
[out-of-context mode](https://github.com/YosysHQ/nextpnr/blob/main/ecp5/main.cc).
The script retains slower results when the requested frequency is not met.
It records exact tool versions, frequency target and placement seed.

The initial two-stage core reached 51.15 MHz. The optimized four-stage core
reaches 129.35–134.66 MHz across placement seeds 1, 2 and 3, meeting the
requested 125 MHz in each run, with 2,215 TRELLIS_COMB cells and 1,258
flip-flops. Simulation verifies initiation interval
one: at 125 MHz this models 125 million frames/s (8 ns/frame), with four
cycles of transfer latency. The 200,000-frame workload checksum is
`837559888487357`.

This is registered core timing, excluding board I/O and global clock
integration. It is not measured FPGA or host/NIC throughput. A production
comparison needs integration timing, transfer costs and multicore CPU
throughput. Simulator runtime is not hardware speed;
[Cyclesim models clock cycles](https://docs.hardcaml.org/hardcaml-docs/simulating-circuits/simulation/).
