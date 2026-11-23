# FPGA validation from an Apple Silicon host

The streaming backend is experimental. Local simulation, formal checks and
routing are useful evidence; deployment on F2, sustained PCIe throughput and a
matched state-of-the-art comparison have **not** been demonstrated. Renting an
FPGA is the next hardware validation step. No cloud resources are provisioned
by the scripts in this directory.

## Local validation

The compiler uses the same checked plan as the software generator. It captures
the scalar prefix and counts the entire frame, so a long payload does not require
an equally wide register. The packet-stack plan proves a 94-byte read prefix;
the streaming configuration captures 128 bytes and accepts transport lengths up
to 65,549 bytes. Codec validity, lengths, IPv4 options, fragments and transport
dispatch remain part of the codec. This is a metadata sink, not a packet
forwarder, and it does not add checksums absent from the codec.
The current hardware lowering supports scalar storage through 32 bits and
refuses unsupported shapes.

The cloud interface uses 64-byte beats, contiguous low-byte KEEP on the final
beat and full KEEP on every earlier beat. It returns one 64-byte metadata record
per frame, including rejected frames. A 64-bit sequence number starts at zero
after reset and wraps modulo 2^64. Reset flushes in-flight frames and starts a
new sequence; captures from different reset epochs must be kept separate.

Generate and seal the component:

```sh
BUILD_HARDCAML=1 dune exec bench/hardcaml-stream/gen.exe -- core.v core.json
python3 bench/hardcaml-cloud/tools.py bundle core.json core.v artifacts/component
```

The seal binds RTL and metadata layout for compatibility checking. It is not a
trusted hardware attestation. `metadata.json` gives every offset and signedness;
records are packed from bit zero and transmitted in little-endian byte order.
Nonzero padding, truncated results, missing or extra records and stale adapters
are rejected. The canonical stack record occupies 507 bits, including its ID.

Export expectations through the existing differential framework. A slot of zero
means u32 little-endian length followed by frame bytes; a positive slot means
fixed-size frames. The exporter checks the normal codec against the existing
generated implementations. Build with `BUILD_EVERPARSE=1` to include the actual
3D-generated C oracle; the exporter reports whether that oracle is enabled.

```sh
BUILD_HARDCAML=1 dune exec bench/hardcaml-stream/corpus.exe -- --all frames.bin
BUILD_HARDCAML=1 BUILD_EVERPARSE=1 dune exec \
  bench/hardcaml-cloud/reference.exe -- \
  scan_packet 65549 0 frames.bin expected.jsonl
python3 bench/hardcaml-cloud/tools.py rtl-test \
  artifacts/component frames.bin expected.jsonl artifacts/rtl-replay
python3 bench/hardcaml-cloud/tools.py verify \
  artifacts/component expected.jsonl device-records.bin
```

For repeated timing candidates, build the native C-enabled reference once and
freeze its records independently of RTL:

```sh
BUILD_HARDCAML=1 BUILD_EVERPARSE=1 dune build --profile release \
  bench/hardcaml-cloud/reference.exe
oracle_entry=$(python3 bench/hardcaml-cloud/tools.py oracle-cache \
  _build/default/bench/hardcaml-cloud/reference.exe frames.bin artifacts/oracle-cache)
python3 bench/hardcaml-cloud/tools.py rtl-test \
  artifacts/component frames.bin "$oracle_entry/expected.jsonl" artifacts/rtl-replay \
  --oracle-receipt "$oracle_entry/receipt.json"
```

The cache key includes the corpus, native executable, codec case, logical maximum
and framing format. A changed codec/reference must rebuild that executable;
its changed digest invalidates the cache. Receipt-based replay needs neither
the executable nor EverParse installed. Altered records/corpora, absent C-oracle
confirmation and mismatched extents are rejected. Checksums detect accidental
staleness; they are not an attestation against a malicious cache editor.

The RTL replay executes the emitted circuit and SDE adapter under Verilator,
with input bubbles and recurring 32-cycle output stalls. Before the checked
replay, it resets pending traffic while the sink is stalled. It compares every
metadata bit, checks stalled output stability, sequence order, counters and ABI
registers.
The shared `--all` corpus currently contains 15,512 frames; its C-backed
reference and strict emitted-RTL replay pass. A separate 163-frame corpus also
checks sample traffic and transport overflow. This is simulation evidence,
not a physical FPGA measurement.
Transport mask, reset, error and overflow tests also run against the core. The
formal transport checks prove their elaborated small configurations; they are
not a proof of every possible bus width or the complete F2 shell.

## Reproducible F2 build

[AWS F2](https://aws.amazon.com/ec2/instance-types/f2/) provides rentable FPGAs.
The smallest instance, f2.6xlarge, has one FPGA, 24 vCPUs and 256 GiB host RAM.
Its host network is limited to 12.5 Gb/s, so it cannot establish 100 Gb/s network
throughput. PCIe replay and core timing are separate measurements. The FPGA
Developer AMI includes Vivado without an additional software charge; instance,
storage and other AWS charges still apply.

Build inputs pin AWS HDK v2.3.5, commit
`c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82`, and Vivado 2025.2. The FPGA part
`xcvu47p-fsvh2892-2-e` comes from that revision's
[`build_all.tcl`](https://github.com/aws/aws-fpga/blob/c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82/hdk/common/shell_stable/build/scripts/build_all.tcl).
The fixed main clock is 250 MHz, according to the
[clock guide](https://github.com/aws/aws-fpga/blob/c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82/hdk/docs/Clock_Recipes_User_Guide.md).

The first timing measurement needs an x86 host with Vivado 2025.2 and support
for this part; it does not need an F2 instance or an FPGA board. Prepare the
portable registered-core OOC job locally and copy its directory to that host:

```sh
python3 bench/hardcaml-cloud/tools.py prepare ooc artifacts/component artifacts/ooc
# On the Vivado host, after sourcing settings64.sh:
bash artifacts/ooc/build.sh
```

The default top registers all non-clock inputs and all outputs, preserving only
these measurement boundaries. This includes the output mux, masks and length
clamp. It is a timing fixture, not an elastic AXI adapter or deployable component.
`--adapter` selects the separate SDE integration OOC measurement with external
delays. `core-ooc CORE.json CORE.v DEST --part PART` accepts a bare stream
manifest, including 256/320-bit literature profiles, without forcing the SDE's
512-bit interface. The default part remains the pinned F2 device.

Each job records RTL/manifest provenance, the 4 ns period, 0.2 ns setup
uncertainty and a 0.25 ns external-delay model. `post_synth`, `post_place` and
`post_route` contain per-stage paths, logic levels, slack, fanout and utilization.
Planning allocations are explicitly separate from measured delays. Empty stage
queries flag incomplete coverage; negative setup/hold slack or incomplete final
coverage fails the job after preserving reports. See
[the stage budget and scope audit](../bench/hardcaml-stream/TIMING.md).

Only after OOC measurement, prepare an isolated shell build from the pinned HDK:

```sh
git clone https://github.com/aws/aws-fpga.git aws-fpga
git -C aws-fpga checkout c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82
python3 bench/hardcaml-cloud/tools.py prepare hdk \
  artifacts/component aws-fpga artifacts/f2/cl_sde
```

These commands prepare files and print build commands; they do not execute
Vivado. HDK staging copies tracked sources only, resolving common-script links
and excluding old build products. Each generated shell build is a one-shot job;
prepare a fresh destination to retry a failed build. The F2 flow replaces the existing SDE metadata
component and uses the upstream small-shell build. Source `hdk_setup.sh` first,
install the [portable host tools](../bench/hardcaml-cloud/README.md) in the
active Python environment,
then run the generated `build-wire.sh`. It refuses the upstream packager's
timing-violated checkpoint path, even if the packager produced an archive.
Neither generated flow has been executed under Vivado yet.

The SDE input USER signal is opaque metadata, not a MAC checksum-error flag.
The adapter ties the core's input-error signal low. Core error injection tests
do not establish that a network ingress will propagate MAC errors correctly.
Before replay, disable the SDE loopback bypass through its global control
register as specified in the pinned
[SDE guide](https://github.com/aws/aws-fpga/blob/c34f88e97eaf8a9b6ea7a3516ab65ee64690ab82/sdk/apps/virtual-ethernet/doc/SDE_HW_Guide.md).
The upstream loopback application alone is not a parser validation. A host DMA
runner that receives this metadata format still needs integration and testing.

Before renting, confirm the region's current on-demand price and F2 quota, set
an explicit duration and budget, and arrange termination plus removal of
temporary EBS volumes and S3 artifacts. A billing alert is not a spending cap;
stopping an instance does not remove storage charges. AFI registration, loading
and hardware replay follow successful timing and shell checks. None has been
performed by this change.

## Performance acceptance

At 250 MHz, a 512-bit bus has a raw capacity of 128 Gb/s. That is a core bus
budget, not an achieved network rate. Measure sustained throughput, packet rate,
latency distribution, backpressure, rejected traffic and host overhead using
minimum, mixed and long frame lengths. Preserve the corpus hash, checksum,
device, tool versions, clock constraints and resource reports with each result.

The earlier ECP5 complete-frame decoder cannot predict the timing of this
streaming circuit. The streaming core's matched ECP5 placement seeds reach
120.67–125.91 MHz (mean 123.74 MHz), following the changes recorded in the
[optimization audit](hardcaml-optimization.md). These are register-to-register
estimates, excluding physical I/O. F2 timing is unknown until routed with its
shell; the 250 MHz integration target remains unverified.

Published parsers are useful comparison targets, but an extraction-only parser
is doing less work than a validating codec. Compare identical field sets,
protocol paths, malformed-input behavior, device, tools and constraints before
claiming a win. For example,
[Patterns to Parsers](https://arxiv.org/abs/2607.16058) uses sparse field-offset
multiplexing and reports high synthesis throughput; its published configuration
and the Wire stack have different protocol and validation coverage. We have not
reused its implementation or established a matched comparison. No SOTA win is
claimed.
