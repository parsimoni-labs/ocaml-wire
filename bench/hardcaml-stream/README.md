# Streaming decoder candidate

Experimental. Target-device timing is not measured yet, and local correctness
tests do not establish production readiness or the 250 MHz target.

This metadata-only AXI4-Stream profile retains the shared checked scanner plan.
The reference top `wire_stack_stream_64` accepts 512-bit beats, stores a
128-byte prefix, and counts frames through 65,549 bytes (14-byte Ethernet
header plus IPv4's maximum total length; MAC FCS excluded). The existing
analysis proves that this codec reads scalars only within the first 94 bytes.
Opaque options and payload spans retain every logical size and region check
without occupying frame memory. A plan whose reads cannot be proven captured
is refused during elaboration. Scalar storage is currently limited to 32 bits.

The public decoder keeps native 63-bit arithmetic and scalar semantics. The
JSON manifest derives each scalar's conservative wire width and signedness
from the same analysis. Its ordered output ports, excluding handshake signals,
pack into 507 bits, including the unsigned 64-bit `frame_id`. The host must
sign-extend signed fields after unpacking; absent and rejected fields are zero.
Input and output widths in the manifest describe the actual emitted circuit.
Hardcaml can prune unused inputs in constant plans; integration adapters must
refuse a missing required port rather than silently invent its semantics.

## Transport contract

`Full_beats` requires full nonfinal TKEEP and a contiguous low mask on TLAST;
it captures fixed byte lanes. `Contiguous` also accepts partial intermediate
beats, packing bytes without gaps. Neither profile supports sparse TKEEP.
Zero TKEEP is allowed on TLAST, representing an empty final beat or frame.
Byte zero occupies the low input bits. Transfer occurs only on VALID and READY.

Input errors, unsupported masks and length overflow remain sticky through
TLAST. Overflow saturates the length at maximum plus one. The circuit drains
that frame and reports one rejected result, then starts a fresh frame. It
never guesses a missing boundary: absent TLAST requires upstream recovery or
reset. `transport_error` bits are input error (0), mask error (1), overflow (2).
Codec rejection remains distinct from these transport errors and from scope.

With no earlier pending result, metadata appears three cycles after TLAST.
A slot from a pool of four is reserved at each accepted TLAST; the decoder stages
advance independently of output READY. Metadata remains stable while stalled.
Ingress stops when every slot is reserved and resumes one cycle after an
output transfer releases credit. With a continuously ready sink, the core
accepts one beat per cycle, including back-to-back single-beat frames.
Synchronous active-high reset aborts partial frames and pending metadata.
A completed-frame ID increments on each accepted
TLAST, including rejected frames, wraps modulo 2^64 and resets to zero. It does
not depend on output transfers. Payload forwarding is outside this core.

## Reproduce

Use the standard opam setup in [the bounded experiment](../hardcaml/README.md).
From the repository root:

```sh
mkdir -p _build/hardcaml-artifacts
hw env BUILD_EVERPARSE=1 WIRE_DIFF_JOBS=1 WIRE_FUZZ_REPEAT=1000 \
  dune build --root . --build-dir _build_hw --profile release -j 2 \
  @fuzz/diff/hardcaml/runtest @fuzz/diff/hardcaml-stream/runtest \
  @test/hardcaml/runtest
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml-stream/gen.exe -- _build/hardcaml-artifacts/stream.v _build/hardcaml-artifacts/stream.json
HW_TOP="$PWD/_build/hardcaml-artifacts/stream.v" HW_MODULE=wire_stack_stream_64 \
  HW_OUT="$PWD/_build/hardcaml-artifacts/stream-routing" HW_FREQ=250 \
  bench/hardcaml/synth.sh
```

The shared long-frame fixtures can also be exported without hardware packages:

```sh
dune exec --root . --profile release -j 2 bench/hardcaml-stream/corpus.exe -- \
  _build/hardcaml-artifacts/stream-long.bin
```

The file contains repeated unsigned 32-bit little-endian lengths followed by
frame bytes, with no count header; its reference case is `scan_packet`.
Pass `--all` before the output path to include the existing stack corpus's
truncation, option and fragment cases as well as the long-frame fixtures.

`gen --contiguous` emits the flexible packing profile and its manifest.
`gen --proof` and `gen --proof-contiguous` emit a small counted-payload
transport instance for the SAT control checks in `prove.sh`.
The contiguous-mask helper also has an exact combinational check against its
arithmetic specification, covering every mask at widths 1–64 and 128:

```sh
hw dune exec --root . --build-dir _build_hw --profile release -j 2 \
  bench/hardcaml-stream/gen.exe -- --keep-proof _build/hardcaml-artifacts/keep-proof.v
HW_PROPERTY=keep HW_MITER="$PWD/_build/hardcaml-artifacts/keep-proof.v" \
  HW_OUT="$PWD/_build/hardcaml-artifacts/keep-proof" bench/hardcaml-stream/prove.sh
```

The stream differential lane extends `Diff_ocaml.check_scan`, using the normal
and general Codec paths, existing generated OCaml scanners, and actual verified
EverParse C. All 20 structured scan cases run through both packing profiles.
Generic fixture lanes have an explicit 256-byte maximum; the stack lane covers 65,549
bytes. Scope skips are counted. Nested scalar snapshots use Codec/generated
OCaml; C independently checks acceptance and consumed prefix. The long-frame
corpus is shared in `Diff_codecs`, rather than maintained in another parser
or oracle.

Focused transport tests cover every first-beat split and final mask, beat
widths from 1 to 64 bytes, long payloads/options/fragments/truncations,
back-to-back frames, deterministic gaps and stalls, malformed masks, input
errors, overflow recovery, reset cancellation, ordered IDs, zero rejected
metadata and capture-proof refusal. Seventeen focused tests include four
pending results under a long sink stall, reset cancellation of all four, and
a final-beat carry after an earlier overflow. Deliberately truncating that
carry makes the regression detect a false acceptance. A configuration matrix
covers logical maxima smaller than a beat; fuzzed transport faults check rejection and
recovery using the same simulator adapter. The control and 64-bit ID induction
checks are universal in time for the small elaborated counted-payload instance,
separately for both packing profiles. Occupied-slot and in-flight invariants
prove credits, queue contents, IDs, framing and accepted-result bounds. The
unstable `Stream.Private` view reuses actual state without adding observation
registers or production ports. These checks do not prove every capacity or the
full 512-bit circuit. EverParse's C proof does not transfer to RTL.

The retained full-beat registered core routed at 120.67, 125.91 and 124.64 MHz
with 6,328 TRELLIS_COMB cells and 4,009 flip-flops on ECP5-85F/CABGA381,
speed grade 8, placement seeds 1–3. The requested 250 MHz was not met.
The queue raises the three-seed mean from 122.36 to 123.74 MHz and reduces
the logical output-READY cone from 2,482 register enables to eight, at the
cost of 1,419 logic cells and 1,527 registers. This connectivity count is
independent of timing closure.

These out-of-context frequencies cover register-to-register paths. The
output queue mux, rejected-result masks and reported-length clamp also need
complete interface timing against their consumer. See the
[optimization audit](../../docs/hardcaml-optimization.md) for tested and
untested techniques, retained results and discarded candidates.

A one-beat-per-cycle interface is an architectural throughput property.
100 Gb/s needs the complete reference design to close timing on its target,
with transport, framing overhead, payload forwarding and sink bandwidth
included. ECP5 out-of-context timing excludes physical I/O and global clock
integration; simulation runtime is not FPGA speed. Cloud synthesis and board
runs require separate validation and authorization before billable work.
