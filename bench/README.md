# Wire Benchmarks

All benchmarks derive from the same Wire DSL definition. The field-level
demo benchmark (`bench/demo/`) compares three tiers:

1. **Pure OCaml** -- `Wire.Codec.get/set` (zero-copy field access)
2. **EverParse C** -- EverParse-generated verified C validator with field extraction in a tight C loop
3. **OCaml->C FFI** -- calling the EverParse C validator from OCaml

The application benchmarks (`bench/clcw/`, `bench/routing/`, `bench/gateway/`)
compare two tiers: Pure OCaml vs EverParse C. `bench/rust/` and `bench/apps/`
compare the OCaml, OxCaml and Rust code generators across languages.

Metrics: ns/op, allocation (words), ratio vs C, throughput.

The C validator checks the schema as it extracts fields. A staged OCaml
`Codec.get` reads the requested field and does not validate the whole record;
call `Codec.validate_exn` separately when that is required. The timing ratio
therefore includes different amounts of validation work.

## Rules

- **No hand-written C parsers.** All field extraction in C must go through
  EverParse-generated validators using the default `<Name>_Fields` plug:
  stack-allocate `<Name>Fields`, pass `(WIRECTX *)&ctx` to the validator,
  read named struct members. No manual bitfield manipulation, no index
  arrays, no hand-rolled `WIRECTX`.
- Both tiers must operate on the same data and produce the same results.
- **Loops are what a user writes.** Every optimisation lives in the code
  generator or the runtime, never in the harness: plain checked indexing,
  no `unsafe_get`, masks, hand unboxing, inlining attributes or intrinsics,
  and SIMD only through an API the generator emits. The rows of one
  workload in one language share one loop shape and differ only in the API
  they call; a crate row calls the crate as its documentation shows. A
  harness trick is a defect: its win belongs in the generator.

## Field-level codec (`bench/demo/`)

Per-field read/write performance across all Wire DSL constructs:

- Integer types: uint8, uint16be, uint32be, uint64be
- Bitfields: bf_uint8, bf_uint16be, bf_uint32be
- Type combinators: `bit`, `map`, `lookup`, `enum`, `where`
- Nested protocols: 3-layer Ethernet->IPv4->TCP traversal
- Real protocols: CLCW (CCSDS), TCP/IP

```
make bench-demo           # requires EverParse (everparse-3d.exe in PATH)
```

## Application benchmarks

Throughput on realistic workloads. The C loop calls the EverParse validator for
field extraction, then runs application logic. The OCaml loop does the same
work with `Codec.read_word` and `Codec.of_word` (CLCW: `Codec.load_word`
and `Codec.extract`), and writes with `Codec.writer`.

| Target | Scenario | Protocols |
|---|---|---|
| `make bench-routing` | APID demux (10M variable-size packets) | Space Packet |
| `make bench-gateway` | TM frame reassembly (1M frames) | TMFrame + Space Packet |
| `make bench-clcw` | COP-1 CLCW polling (1M words, polled 10M times) | CLCW |

Each workload lives in `<app>_workload.ml` in its directory, with no C, so
that the cross-language rows below run the same code.

## Cross-language applications (`bench/apps/`)

The same three workloads on OCaml, on OCaml with flambda, on OxCaml, in
Rust, and over released crates that read and write the same headers, each
pass over the whole input its generator writes. Each workload has a reading side, what a ground
station does with the stream, and a writing side, what the spacecraft or a
relay does to produce it:

| Workload | Input | Work per item | Result |
|---|---|---|---|
| `routing` | 10M space packet headers of 14 to 262 bytes, the last packet in the buffer truncated so that the reader wraps to the first for its last 1 in 64 | read APID and data length, count the APID's route, skip the packet | the four route counts |
| `routing_write` | the same packets | read APID and data length, then write the packet's header at the same offset of an output buffer with the next sequence count of its route: the forwarding rewrite of a router that re-numbers each route's packets | FNV-1a hash of the output |
| `gateway` | 1M TM frames of 1115 bytes, 1 in 64 idle (first header pointer 0x7FE, no packet starts in it) | read the frame's VCID and first header pointer, then the APID and sequence count of each 70-byte packet starting in it; fold them into an FNV-1a hash | the hash |
| `gateway_write` | none, 1M frames out | build each frame from a packet stream: write its primary header (VCID, master and virtual channel counts, first header pointer), then the header of each 70-byte packet starting in it, laid out as the `gateway` input without its idle frames | FNV-1a hash of the output |
| `clcw` | 1M CLCW words, with wait, retransmit and lockout set on words of their own and a report value skipped now and then | load the word, take lockout, wait, retransmit and report value, count an anomaly | the anomaly count |
| `clcw_write` | none, 1M words out | write the CLCW a FARM reports with each frame: report value, lockout, wait and retransmit from its state | FNV-1a hash of the output |

| Row | Implementation | API |
|---|---|---|
| `Wire.Codec` | the OCaml workload (OCaml 5, OxCaml) | `Codec.read_word` per header, then `Codec.of_word` per field; for CLCW `Codec.load_word` once, then `Codec.extract` per field. Writing: `Codec.writer` of the default header and the fields that vary, one call per header |
| `wire.ocaml` | generated OCaml, `Wire_ocaml.ml` of `Space.packet_codec`, `Space.tm_frame_codec`, `Space.clcw_codec`: default target on OCaml 5, ``~target:`OxCaml`` on OxCaml | `Get.<field>` per field; for CLCW `Get.lockout_word` once, then `<field>_of_word` per field. Writing: `write_int` per header |
| `wire.rust` | generated Rust, `Wire_rust.rs` of the same codecs | `Valid::at` per header, then its accessors, for routing and gateway; for CLCW the functions `wire.ocaml` calls. Writing: `write` per header |
| `spacepackets` 0.19 (routing, gateway packets) | hand-written, zerocopy-based | `SpHeader::from_be_bytes`, then `apid()`, `data_len()`, `seq_count()`. Writing: `SpHeader::new_from_fields`, then `write_to_slice` |
| `ccsds_primary_header` 0.15 (routing) | hand-written | `PrimaryHeader::from_slice`, then `control.apid()` and `length.length_field()`. Writing: the setters of its three words, then their bytes |
| `gaia-ccsds-c2a` 1.2 (routing, CLCW) | `modular-bitfield` structs over zerocopy | `PrimaryHeader` through `LayoutVerified::new_unaligned_from_prefix`, as its `SpacePacket` reads it; `CLCW::from_bytes`. Writing: the `with_<field>` builders, then `into_bytes` |
| `chronus-gateway` 1.0.0-rc.1 (gateway frames) | hand-written | `TmFrameHeader::parse`, with `spacepackets` for the packets. Writing: `TmFrameHeader::to_bytes`, the header parsed once and its fields set per frame, as the type is `#[non_exhaustive]` |

The `Wire.Codec` rows run `Routing_workload.run_packets`,
`Gateway_workload.checksum` and `Clcw_workload.run_all`, the code the
EverParse comparisons time, and on the writing side
`Routing_workload.restamp`, `Gateway_workload.frame` and
`Clcw_workload.report`. The other rows run the same application logic,
written as a user of each library writes it, with plain checked indexing,
and every row gives the same result. `bench/apps/loops.ml` writes the loops
over the generated OCaml, the same source for OCaml 5 and for OxCaml.
`bench/apps/cargo/src/main.rs` writes them over the generated Rust, whose
`Valid::at` checks a header once before its accessors read it, and over the
crates; the Rust rows of a workload share one loop and differ only in the
header reader and writer they call.
No EverParse row runs here: those stay in `make bench-routing`,
`bench-gateway` and `bench-clcw`.

Crates: `spacepackets`, `ccsds_primary_header` and `gaia-ccsds-c2a` read
and write the space packet primary header; `chronus-gateway` the CCSDS TM
transfer frame primary header (CCSDS 132.0), with `spacepackets` for the
packets it carries; `gaia-ccsds-c2a` the CLCW (CCSDS 232.0). Each crate row
reads exactly the fields the other rows read, through the crate's own
types. `chronus-gateway` is a gateway application whose library pulls in
its server's dependencies (tokio, axum and more): it takes `cargo/Cargo.lock`
from 27 packages to 188, of which 131 come from it and 27 from
`gaia-ccsds-c2a`. Not used: `ccsds` 0.1.0-beta.25 decodes the AOS (CCSDS
732.0) VCDU header, whose layout differs from a TM frame's, and
`spacepackets` 0.19 decodes USLP frames and keeps the operational control
field as a raw `u32`.

Timing and reporting are those of `bench/rust`: one warm-up pass, which gives
the result, then the timed passes, each reading the wall clock and the
process CPU time (`getrusage`) around the call, in `Bench_lib.measure` on the
OCaml side and in `bench/rust/cargo/src/timing.rs` on the Rust side; the
report prints a table per workload and fails unless every row agrees. Each
row runs in a process of its own (`bench/rows.sh`: the harness lists its
rows with `--list`, then runs each by name), so no row starts on the heap,
the caches or the gigabytes of address space the rows before it touched.
The cargo project has the release profile of `bench/rust/cargo`, and
`cargo/Cargo.lock` pins the crates.

```
make bench-apps-rust      # needs cargo; fetches the crates the first time
```

Instructions / cycles per item, as `make bench-apps-count` reports them with
its defaults (inputs of 100,000 items), the median of five runs, measured
at commit `86864e63` (the parent of the commit that adds these numbers,
which changes only READMEs) on an ARM64 macOS host under a load average
of 20 to 43: OCaml 5.4.1 with `-inline 50`, OCaml 5.4.1 with flambda (switch
`5.4.1-flambda`, `-O2`), OxCaml 5.4.0+ox (switch `5.4.0-ox9`, `-O4`), each
on every unit as [the sweep](#compiler-flags) picks them, and rustc 1.94.1
with the crates `cargo/Cargo.lock` pins. `bench/count.sh` runs the row alone
with 1 and with 21 timed passes, each after the warm-up pass, and divides the
difference by 20 passes, which cancels starting the process, the input read,
the output allocation and the warm-up pass; it fails unless both runs print
the row's `RESULT` line with the same result and the counters read some
instructions. The cycles move with the load and the core a row lands on, the
instructions do not.

| Row | routing | gateway | clcw | routing_write | gateway_write | clcw_write |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| wire.rust | 26.1 / 13.5 | 459 / 131 | 12.2 / 3.2 | 43.1 / 18.1 | 355 / 56.3 | 5.2 / 2.1, vectorised |
| spacepackets 0.19 | 27.2 / 16.2 | - | - | 43.2 / 15.5 | - | - |
| ccsds_primary_header 0.15 | 27.3 / 17.0 | - | - | 40.1 / 17.2 | - | - |
| gaia-ccsds-c2a 1.2 | 24.1 / 13.5 | - | 12.2 / 2.9 | 43.5 / 17.1 | - | 4.3 / 1.3, vectorised |
| chronus-gateway 1.0.0-rc.1 + spacepackets 0.19 | - | 356 / 131 | - | - | 354 / 67.6 | - |
| wire.ocaml, OxCaml target | 44.1 / 13.3 | 656 / 193 | 42.1 / 9.1 | 72.7 / 20.3 | 556 / 91.4 | 51.2 / 11.1 |
| NEON lanes, OxCaml | - | - | 10.7 / 1.7 | - | - | 6.9 / 0.90 |
| wire.ocaml, OCaml flambda | 56.3 / 16.2 | 1009 / 197 | 46.8 / 10.6 | 105 / 20.7 | 890 / 123 | 66.0 / 11.3 |
| wire.ocaml, OCaml | 56.2 / 15.7 | 793 / 199 | 47.1 / 8.8 | 105 / 22.5 | 890 / 137 | 66.2 / 7.8 |
| Wire.Codec, OxCaml | 107 / 49.4 | 1206 / 224 | 119 / 20.4 | 197 / 36.8 | 1997 / 300 | 136 / 22.7 |
| Wire.Codec, OCaml flambda | 103 / 53.4 | 1363 / 222 | 139 / 21.3 | 208 / 33.0 | 2169 / 264 | 142 / 16.5 |
| Wire.Codec, OCaml | 100 / 55.6 | 1239 / 222 | 146 / 17.7 | 206 / 39.0 | 2194 / 295 | 142 / 17.7 |

The two Rust `clcw_write` rows are autovectorised: LLVM turns their loops
into NEON code that builds several words per instruction, where every other
cell but the NEON lanes row runs one item at a time, so they do not compare
loop for loop with the OCaml rows.

Every row gave `(hk=40039 sci=34986 diag=19980 idle=4995)` on routing,
`203423907276323163` on gateway and 3879 on clcw, and wrote buffers hashing
to `34573b9d7391d744` (routing), `1228726ff2ca7adb` (gateway) and
`3bd5c8be11c13745` (clcw). The inputs hold one of each kind of anomaly the
readers test for, so every row takes each branch: the routing stream ends in
a truncated packet after which the reader wraps, one gateway frame in 64 is
idle, and CLCW words carry wait, retransmit and lockout and skip report
values; writing sets lockout, wait and retransmit too, and the crate's CCSDS
232.0 words and the codec's hash the same.

Writing is where the generated OCaml is furthest from Rust. A
`write_int` checks every field value against its width on every call,
where LLVM proves from the loop that `(p % 2048) as u16 >> 11` is 0 and drops
the check; stock OCaml assembles a 32-bit word on tagged integers, with an
`int32` sign normalisation and a `sxtw` on each side of the `rev` that do
nothing, where OxCaml builds it untagged; and each packet polls for a
safepoint. On gateway that is 556 instructions per frame on OxCaml against
355 in Rust. LLVM also vectorises the CLCW writer, 5.2 instructions per
word. `Wire.Codec` writes a header with one call to a staged
`Codec.writer`, which puts the default header and the fields that vary
together in an `int` and stores it. These headers are at most 6 bytes,
so no row stores a 64-bit word: the generated writes are `str w` and `strh`,
and the only `dmb ishld` OxCaml puts in a write loop is the one before the
per-route sequence count's `int array` store of `routing_write`. The OxCaml rows
trail Rust on the latency chain each item carries: on routing the next
offset is loaded from the packet, and OxCaml spends about eight dependent
operations on it where Rust spends five (untagging, and the sign extension
and mask `%bswap16` leaves after `rev16`), and every checked read compares
a tagged offset.

Every hash the workloads compute (the gateway checksum and the hash of a
written buffer) is FNV-1a modulo 2^63: OCaml's native `int`, whose
products wrap modulo 2^63, and in Rust and C a 64-bit word masked to its
low 63 bits after each product, so every row agrees. A 64-bit hash would be
an `int64` in OCaml, boxed wherever it crosses a call the compiler leaves
out of line, which measures the language's `int64` rather than the codec.

`BENCH_APPS_PACKETS`, `BENCH_APPS_FRAMES` and `BENCH_APPS_WORDS` size the
inputs, `BENCH_APPS_RUNS` the number of timed passes, and `FLAMBDA_SWITCH`
and `OXCAML_SWITCH` name the switches of OCaml with flambda and of OxCaml,
as for `make bench-rust`. Each harness runs a
workload's writing side with `write`, and runs only the row a last argument
names exactly, failing when no row has that name, or prints its row names
with `--list`: `bench.exe (run | write) FILE ITEMS RUNS [ROW | --list]` or
`wire-bench-apps [run | write] FILE ITEMS RUNS [ROW | --list]`.
`make bench-apps-count` runs `bench/count.sh` on each row the harnesses
list, with the row's exact name.

## Cross-language headers (`bench/rust/`)

The code generators against released Rust parsers: Ethernet, IPv4 and TCP or
UDP headers, read from the corpora below. Each row uses the API a user would
pick for exactly this work:

| Row | Implementation | API |
|---|---|---|
| `Wire.Codec stack decode` | interpreted codec (OCaml 5, OxCaml) | `Codec.decode` of one bounded stack codec |
| `Wire.Codec check/read, linked headers` | interpreted codecs of the fixed headers (OCaml 5, OxCaml) | `Codec.check` and `Codec.ok` per header, then a staged `Codec.read` per field; the harness links the layers |
| `wire.ocaml fused stack`, `selected outputs` | generated OCaml, default target on OCaml 5, ``~target:`OxCaml`` on OxCaml | the scanner of the stack: `decode`, `fold` and `fold_frames` (batch), on OxCaml also their unboxed forms; `selected outputs` hands out only the checksum's fields |
| `wire.ocaml View.check/view, linked headers` | generated OCaml record modules of the fixed headers | `View.check`/`View.ok` per header, then a `view_<field>` per field; the harness links the layers, as the `wire.rust, linked headers` row does |
| `wire.rust fused stack`, `selected outputs` | generated Rust, `Wire_rust.scanner` of the stack | `scan` per frame; `selected outputs` hands out only the checksum's fields |
| `wire.rust, linked headers` | generated Rust of the fixed headers | `Valid::at` per header, then a read per field; the harness links the layers |
| EverParse C (`BUILD_EVERPARSE=1`) | the verified validator EverParse generates from `Packet_stack.codec` | `EthernetStackValidateWireEthernetStack` per slot, then the members of the default `EthernetStack_Fields` plug, zeroed per slot |
| `etherparse` 0.21 | hand-written | `SlicedPacket::from_ethernet`, its one-call path |
| `smoltcp` 0.14 | hand-written | `new_checked` per layer (`EthernetFrame`, `Ipv4Packet`, `TcpPacket`/`UdpPacket`) |
| `pnet_packet` 0.35 | generated by its `#[packet]` derive | `new` per layer, then the field accessors |

The crates are the most used maintained crates on crates.io for parsing
these headers, one per implementation style (hand-written zero-copy slices,
hand-written `wire` module of a network stack, derive-generated accessors).

Every row does the work `SlicedPacket::from_ethernet` does on each packet:

1. the Ethernet header is checked to fit and its EtherType read; a packet
   that is not IPv4 stops there;
2. the IPv4 header: version 4, a header length of at least 5 words, a total
   length that covers the header and lies inside the packet, so the payload
   is bounded by the total length; addresses, TTL, protocol and total length
   read;
3. unless the packet is a fragment (more fragments, or a fragment offset),
   the TCP or UDP header at the header length: a TCP data offset of at least
   5 words inside the payload, or a UDP length of at least 8 inside it; ports
   and the other fields read.

The stack rows use `examples/net/packet_stack.ml`: one codec owns Ethernet
and protocol dispatch, fragment handling, options, and the nested payload
bounds. `Wire_ocaml.scanner` and `Wire_rust.scanner` write those rules out
from one plan as a fused scanner; the harness only folds returned scalar
fields into the checksum. Its
`off`/`len` arguments bound each frame even inside a larger corpus buffer.
`Codec.decode` uses the same stack inside a framing region of one corpus
slot and constructs its ordinary nested values. Byte fields are zero-copy
slices.

The frame loop of the batch rows (`fold_frames` and its unboxed and step
forms) walks four frames per iteration on OxCaml only, so that it polls
for signals once per four frames. Each iteration first computes where its
four frames start and loads the field their transport header is chosen
on, the IPv4 protocol, then walks the frames: a mispredicted branch of one
frame does not hold back the next frame's dispatch by a load. On stock OCaml it walks one frame per
iteration, because four inlined walks cost the stock compiler more in
spilled registers than the polls they save. The generated Rust has no frame
loop: its rows call `scan` once per slot.

The `linked headers` rows, OCaml and Rust, use the
fixed header codecs in `headers.ml` with a harness that links the layers. The crate adapters apply the same
checks (`smoltcp` checks the bounds itself; `pnet_packet`'s adapter supplies
checks beyond its minimum header lengths). A packet counts only when every
header it has passes, as with `etherparse`. Every row folds the same fields
into a checksum, and the report fails if they disagree.

The existing `test/ocaml` and `fuzz/diff/ocaml` suites compare scanner
acceptance and scalar fields with `Codec`, including truncation at every
byte, nonzero frame offsets, IPv4/TCP options and arbitrary mutations.
Their `ocaml-ox` lanes also compare boxed and unboxed APIs and check
`decode_unboxed` with `[@zero_alloc]` in release builds. With
`BUILD_EVERPARSE=1`, the same differential driver compiles the projected
3D schemas through EverParse and compares C acceptance, consumed length and
the scalar members of the default plug, nested fields included.
Selected-output and validation-only scanners retain every validation check
([execution model](../docs/execution.md#generated-code)).

`bench.exe corpus DIR N` writes four corpora of N packets in fixed-stride
slots, the only framing the batch consumers (`fold_frames`) take:

| Corpus | Slots | Packets |
|---|---|---|
| `fixed64-random` | 64 bytes | three in four: 20-byte IPv4 and TCP or UDP headers without options, the packet filling the slot; the others one of the cases below; kinds in random order |
| `fixed64-sorted` | 64 bytes | the same packets grouped by kind |
| `mix128-random` | 128 bytes | IPv4 and TCP headers of 20, 24, 32 or 40 bytes (options), IPv4 total lengths from the headers to the end of the slot (packets of 54 to 128 bytes, then Ethernet padding); kinds in random order |
| `mix128-sorted` | 128 bytes | the same packets grouped by kind |

The packets of `fixed64` that are not well formed are spread evenly over
these cases, so each check meets packets that pass it and packets that fail
it, and the rows agree only if each makes every check:

| Check | Rejected | Accepted besides the well-formed |
|---|---|---|
| EtherType | | another EtherType, counted alone |
| IPv4 version | version 6 | |
| IPv4 header length | 4 words | 6 words, with options |
| IPv4 total length | 19; 22 under a 24-byte header; one byte past the packet | 6 bytes short of the packet |
| Fragment | | more fragments, or a fragment offset: IPv4 counted, no transport header read |
| Protocol | | neither TCP nor UDP: IPv4 counted alone |
| TCP data offset | 4 words; 15 words, past the payload | 6 words, with options |
| UDP length | 7; one byte past the payload | shorter than the payload |

The kind of a packet is what the parsers must decide: in `fixed64`, its case
above; in `mix128`, not IPv4, valid TCP, valid UDP, a fragment, a total
length past the slot, a TCP data offset past the payload or a UDP length past
it. In random order the branches on the kind mispredict as they would on
mixed traffic; in sorted order they are predictable, which isolates the
parsing work. The checksum is a sum, so both orders of a family give the
same one, and the report fails unless every row on every corpus of a family
agrees.

Both harnesses do the same thing, each row in a process of its own
(`bench/rows.sh`): one warm-up pass, which gives the checksum, then the
timed passes. Each pass reads the wall clock and the
process CPU time (user and system, `getrusage(RUSAGE_SELF)`: `Unix.times` in
OCaml, `libc::getrusage` in Rust) around the call. The report prints one
table per corpus, ranked by the best CPU time per packet, with its 10th
percentile, median and 90th percentile and the best and median wall time,
then the best CPU time of every row on every corpus. CPU time leaves out the
time the process waits descheduled, which on a loaded machine dominates the
wall time of the slower rows; it does not remove the cache and branch
predictor state other processes leave behind, so compare rows measured in
the same conditions. Every row takes the slot's stride at run time, from
the command line, as a reader of fixed-size frames that learns their size
from its input does: the Rust rows read the corpus with
`chunks_exact(slot)`, and no loop carries an inlining attribute. The Rust
side builds with `opt-level = 3`, `lto = true`, `codegen-units = 1` and
`panic = "abort"`, and without `target-cpu=native`, which the OCaml
compilers have no equivalent of. The OCaml side builds with
dune's `bench` profile (`dune-workspace`): the `release` profile with the
flags [the sweep](#compiler-flags) picks for each compiler on every unit,
the libraries the harness links (`wire`, `wire.runtime`, the workloads) as
well as the harness, so the compiler optimises the code a row calls as it
optimises the row.

```
make bench-rust           # needs cargo; fetches the crates the first time
```

`FLAMBDA_SWITCH` and `OXCAML_SWITCH` name the opam switches the runs on
OCaml with flambda and on OxCaml use (default `5.4.1-flambda` and
`5.4.0-ox9`; each skipped when absent), `BENCH_RUST_PACKETS` and
`BENCH_RUST_RUNS` size the corpora and the number of timed passes, and
`BENCH_RUST_CORPORA` lists the corpora to run as `name:stride`. Each harness
also takes a row's exact name last, so one row can run alone, and fails when
no row has that name; `--list` in its place prints the row names. On macOS,
`/usr/bin/time -l` then gives the instructions retired and the cycles of
that row, as `bench.exe run FILE 64 RUNS 'wire.ocaml fused stack'` or
`wire-bench-rust FILE 64 RUNS 'smoltcp 0.14'`; `bench/count.sh` does the
subtraction that leaves out the process's fixed costs, and `make
bench-rust-count` runs it on every row of every harness and corpus, as
`make bench-apps-count` does for the applications.

With `BUILD_EVERPARSE=1` (`make bench-rust BUILD_EVERPARSE=1`), the
EverParse C row runs too: `bench/rust/everparse` links the validator and
default plug that `gen_stubs.exe` archives with the other benchmark schemas
(compiled at `-O2`, as dune compiles C stubs), and its C loop calls the
validator once per slot, bounded by the slot, reads the IPv4, TCP and UDP
fields as named plug members (`ipv4_src`, `ipv4_tcp_present`, ...; see
[the FFI plug](../docs/projection.md#the-ffi-plug)) and folds them into the
same checksum. It times its passes in C as the Rust harness does. Every
field reaches the plug through an out-of-line setter call, 30 for a TCP
packet; the measurements do not separate that cost from the validator's.

The batch consumers take fixed-stride slots only, so a real capture, with
frames of every length, is measured per frame by the flow benchmark below.

## A real trace through a flow classifier (`make bench-flow`)

The corpora above are synthetic: their mix of options, fragments and kinds
of packet is the one `bench.exe corpus` draws, and the branch predictors
learn it as such. This benchmark runs the first packets of a real backbone
trace, held in memory, through a flow classifier on one core, so every row
meets the trace's own mix of IPv4 and IPv6, ports, flows and lengths.

### The trace

[MAWI](https://mawi.wide.ad.jp/mawi/) samplepoint-F, the public 15-minute
daily trace of the WIDE backbone (CAIDA's traces need an agreement and are
not used):

| | |
|---|---|
| URL | `http://mawi.wide.ad.jp/mawi/samplepoint-F/2025/202511151400.pcap.gz` |
| SHA-256 | `a99b91648ec296c1611558878f11828d2cb02d434e77f05153fa0020a6ce266c` |
| Size | 4,636,631,468 bytes compressed; 180,654,520 packets over 900 s (MAWI's summary) |
| Link type, snap length | Ethernet, 96 bytes |

`make bench-flow` downloads it into `_build/bench-flow` once, checks the
SHA-256 on every run, and cuts its first `BENCH_FLOW_PACKETS` packets
(default 1,000,000) into `_build/bench-flow/mawi.pcap` (71.6 MB for the
default). Each harness reads that file whole into one buffer and indexes the
offset and captured length of each frame before timing; the flow table has
the least power of two at least twice the packets in slots of 64 bytes, 128
MiB for the default. Nothing of the trace is committed.

`bench.exe flow-mix FILE` counts the mix byte by byte, independently of the
parsers; on the default 1,000,000 packets:

| | Packets | Share |
|---|---:|---:|
| IPv4 | 820,260 | 82.03% |
| IPv6 | 178,454 | 17.85% |
| neither (ARP and others) | 1,286 | 0.13% |
| IPv4 fragments | 393 | 0.04% |
| IPv4 with options, IPv4 AH, IPv6 extension headers, VLAN tags | 0 | 0% |
| captured length under the wire length | 776,000 | 77.60% |
| IPv4 total length past the capture | 617,735 | 61.77% |
| IPv6 payload length past the capture | 156,836 | 15.68% |

Captured lengths run from 34 to 96 bytes: MAWI keeps the headers and drops
every payload, so a TCP packet stops after its TCP header (54 bytes, 66 with
the usual 12 bytes of options) and a UDP packet after its UDP header (42
bytes). With no extension header in these packets, the IPv6 rows read the
fixed header and the TCP or UDP header its next header names; an extension
header ends the walk with no transport header, as in `Packet_stack.ipv6`.

### Truncated captures

On this trace the IPv4 total length runs past the bytes captured in 62% of
the packets, so the checks of the header benchmark, which bound the payload
by the total length, would reject most of them. etherparse's API for
truncated captures, `LaxSlicedPacket`, takes the payload to be what was
captured when a length runs past it. Every row here does the same, with the
checks of `Packet_stack.capture` (`examples/net`):

1. Ethernet: 14 bytes captured; EtherType `0x0800` is IPv4 and `0x86dd`
   IPv6, any other ends the packet without a flow;
2. IPv4: version 4, a header length of at least 5 words, the header and its
   options captured, a total length covering the header; IPv6: version 6 and
   the 40-byte header captured;
3. unless the packet is an IPv4 fragment, the TCP header (a data offset of at
   least 5 words, the header and options captured) or the UDP header (8
   bytes captured, a length of at least 8);
4. a packet counts only when every header it has passes, as in the header
   benchmark.

The IP and UDP length fields are read and bound nothing. Lax slicing also
cuts the payload at the IP length when that length ends before the capture;
`Packet_stack.capture` cannot, because a Wire codec cannot name the bytes
left in its input (3D has no such expression either: the validator would
have to take the captured length as a parameter, which the scanner
generators do not accept). The two rules differ only on a packet whose IP
length ends before the capture and whose transport header runs past that
length: `flow-mix` counts them, and there are none in the default packets,
so the etherparse row runs its lax API unchanged and agrees with the others.
Its adapter drops a packet whose TCP or UDP header stopped the slicing, and
makes the two checks lax slicing leaves out (a total length covering the
IPv4 header, a UDP length of at least 8; neither fails on these packets).

### The classifier

Per packet, every row parses the frame, builds the 5-tuple (both addresses,
IPv4 or IPv6, the protocol or next header, and the ports when a TCP or UDP
header was read, 0 otherwise), hashes it and counts the packet and its IP
length (the IPv4 total length, or 40 plus the IPv6 payload length) in an
open-addressing table with linear probing. The hash mixes the key's 32-bit
words, `h = (h xor w) * 0x9E3779B97F4A7C15`, and takes the slot from bits 32
and up; OCaml's 63-bit `int` keeps the low 63 bits of the same product, so
the OCaml, Rust and C tables (`flow_table.ml`, `cargo/src/flow.rs`,
`everparse/stack_c.c`) fill the same slots. A slot belongs to the pass whose
epoch it carries, so no pass clears the table. Each row prints the number of
flows and a checksum of every live slot (its place, key, packets and bytes),
read after the timed passes and not timed; the report fails unless every
row agrees. The default packets hold 342,506 flows.

| Row | Implementation | Parse |
|---|---|---|
| `Wire.Codec capture decode` | interpreted codec (OCaml 5, OxCaml) | `Codec.decode` of the pcap record around the frame, its `incl_len` bounding `Packet_stack.capture`, then the nested values |
| `wire.ocaml View.check/view, linked headers` | generated OCaml record modules of `headers.ml`, IPv6 included | `View.check`/`View.ok` per header, `view_<field>` per field; the harness links the layers and checks each header ends inside the capture |
| `wire.ocaml scanner, selected outputs, fold_step` | generated OCaml scanner of `Packet_stack.capture` | `fold_step` per frame, handing the classifier's fields to `Flow_step` (`Flow_parse` without the table) by name |
| `wire.rust scanner, selected outputs` | generated Rust scanner of `Packet_stack.capture` | `scan` per frame |
| `wire.rust, linked headers` | generated Rust of `headers.ml` | `Valid::at` per header, then a read per field; the harness links the layers |
| EverParse C (`BUILD_EVERPARSE=1`) | the verified validator of `Packet_stack.capture` | `CapturedStackValidateWireCapturedStack` per frame, then the members of the default `CapturedStack_Fields` plug, zeroed per frame |
| `etherparse 0.21, lax` | hand-written | `LaxSlicedPacket::from_ethernet` |
| `smoltcp 0.14` | hand-written | `EthernetFrame::new_checked`, `TcpPacket::new_checked`; the IPv4, IPv6 and UDP layers through `new_unchecked` after the adapter's checks, because `new_checked` bounds them by their length fields and rejects a header-only capture |
| `pnet_packet 0.35` | generated by its `#[packet]` derive | `new` per layer, then the field accessors; the adapter makes the checks beyond the minimum header lengths, as in the header benchmark |

The IPv6 fixed header of `Packet_stack.ipv6` and `Headers.ipv6` is the layout
of nox-ip's `Ip.V6.Header.codec`, with each address as four 32-bit words
because the scanners hand out scalars of at most 32 bits.

Each row runs in a process of its own, one warm-up pass then
`BENCH_FLOW_RUNS` (default 21) timed passes, timed as the header benchmark
times them (process CPU time and wall time around each pass). ns/packet is
the CPU time of a pass over the packets; packets per second is its inverse.

The methodology follows ixy, the user-space NIC driver written in C, Rust,
Go, C#, Java, OCaml, Haskell, Swift, JavaScript and Python (P. Emmerich et
al., "The Case for Writing Network Drivers in High-Level Programming
Languages", ANCS 2019; [ixy-languages](https://github.com/ixy-languages)):
one core, packets per second as the measure, and the same design in every
language, so the rows differ in the parser and not in the work around it.
ixy measured a NIC forwarding packets and swept the batch size, which
amortises the per-call cost of the driver; here the packets are already in
memory and every row walks them one frame at a time, which is the batch the
whole buffer gives, so there is no batch size to sweep.

```
make bench-flow                       # needs cargo, curl and the network the first time
make bench-flow BUILD_EVERPARSE=1     # adds the EverParse C row
```

`BENCH_FLOW_PACKETS` sets how many packets are cut from the trace and
`BENCH_FLOW_RUNS` the timed passes; `FLAMBDA_SWITCH` and `OXCAML_SWITCH` are
read as for `make bench-rust`.

### The parse and the table apart

Two more corpora split each row into its parse and its table. On `mawi
parse`, every row but EverParse C parses as on `mawi` and hands each key (the
arguments of `add4` or `add6`) to a fold of a few instructions instead of the
table (`flow_sum.ml`, `Sum` in `cargo/src/flow.rs`); the row `keys from an
array, parse only` folds the keys of the pass from an array, with no parse.
On `mawi table`, the row `flow table, keys from an array` hands the same keys
from that array to the table of the classifier. The keys are recorded once
before timing, 12 ints per key (`flow_keys.ml`, `Keys`), by the record modules
in OCaml and by the generated scanner in Rust; every row of a corpus gives
the same result, and the table row the result of `mawi`.

The OCaml rows that hand their keys to a sink are written once, in
`flow_rows.ml`, and copied by dune for the other sinks, so that every
compiler calls the sink directly (a functor over the sink would call it
through a closure on OCaml without flambda). The Rust parse rows run in a
binary of their own, `wire-bench-flow-parse`: a binary that builds a row for
two sinks gives a crate function, `Ipv6Packet::get_source` for one, two
callers, and LLVM then leaves it out of line, which costs `pnet_packet` 28
instructions per packet in its row of `mawi`.

### Results

Measured at commit `12adeef7` (the parent of the commit that adds this
section, which changes only this README) with the default 1,000,000 packets
and 21 timed passes, each row in a process of its own, on an Apple M5 Max
(18 cores, 128 GiB, macOS 27.3) shared with other work: `top` showed a load
average of 146 before the run and 94 after it, with no idle CPU. The CPU time
leaves out the time a row waited descheduled, not the caches and predictors
other processes share; compare rows of one run. OCaml 5.4.1 (`-inline 50`),
OCaml 5.4.1 with flambda (`-O2`) and OxCaml 5.4.0+ox (`-O4`) with dune's
`bench` profile; rustc 1.94.1 at `opt-level = 3` with LTO; EverParse 3d
v2026.10.27 with Apple clang 21.0.0 at `-O2`. The command is `make
bench-flow BUILD_EVERPARSE=1`, run as its recipe with dune rooted at this
checkout. Packets per second is the inverse of the best CPU time.

| Row | CPU ns/packet, best | median | Mpackets/s, best |
| --- | ---: | ---: | ---: |
| wire.rust, linked headers | 29.66 | 31.12 | 33.7 |
| pnet_packet 0.35 | 29.77 | 30.49 | 33.6 |
| wire.rust scanner, selected outputs | 31.01 | 32.27 | 32.2 |
| smoltcp 0.14 | 34.88 | 36.05 | 28.7 |
| wire.ocaml scanner, selected outputs, fold_step, OxCaml | 66.81 | 68.10 | 15.0 |
| wire.ocaml View.check/view, linked headers, OxCaml | 71.89 | 73.55 | 13.9 |
| wire.ocaml scanner, selected outputs, fold_step, OCaml flambda | 77.87 | 81.48 | 12.8 |
| wire.ocaml View.check/view, linked headers, OCaml | 84.19 | 86.84 | 11.9 |
| etherparse 0.21, lax | 91.80 | 94.40 | 10.9 |
| wire.ocaml View.check/view, linked headers, OCaml flambda | 92.89 | 97.18 | 10.8 |
| EverParse C, default plug | 95.62 | 99.07 | 10.5 |
| wire.ocaml scanner, selected outputs, fold_step, OCaml | 98.61 | 101.16 | 10.1 |
| Wire.Codec capture decode, OxCaml | 632.38 | 654.09 | 1.6 |
| Wire.Codec capture decode, OCaml | 681.18 | 711.91 | 1.5 |
| Wire.Codec capture decode, OCaml flambda | 712.21 | 739.20 | 1.4 |

Every row counted 342,506 flows and gave checksum `3901415651386128532`.

### Instructions per packet

The host was loaded, so the counts below are instructions, which another
process does not change, with cycles for scale. `bench/count.sh` runs a row
with 1 and with 21 timed passes and divides the difference by 20 passes of
the 1,000,000 packets, which cancels the start of the process, the read of
the capture, the recording of the keys and the warm-up pass:

```
sh bench/count.sh 1000000 ROW HARNESS flow _build/bench-flow/mawi.pcap RUNS ROW
sh bench/count.sh 1000000 ROW wire-bench-flow-parse _build/bench-flow/mawi.pcap RUNS ROW
```

Measured on the branch that adds the split rows, on the machine above with a
load average of 83 before and 55 after (the OCaml 5.4.1 rows ran earlier at
91), no idle CPU. Parse is the row on `mawi parse`, which folds its keys
instead of counting them; table is the row on `mawi` less its parse.

| Row | Total | Parse | Table | Cycles, total | Cycles, parse |
| --- | ---: | ---: | ---: | ---: | ---: |
| wire.rust scanner, selected outputs | 114.4 | 69.2 | 45.2 | 102.6 | 27.7 |
| wire.rust, linked headers | 109.9 | 66.1 | 43.8 | 97.1 | 26.8 |
| pnet_packet 0.35 | 109.5 | 67.3 | 42.2 | 97.0 | 27.3 |
| smoltcp 0.14 | 138.8 | 63.8 | 75.0 | 112.8 | 27.1 |
| etherparse 0.21, lax | 478.4 | 431.2 | 47.2 | 297.8 | 100.2 |
| wire.ocaml scanner, OxCaml | 327.0 | 117.5 | 209.5 | 197.4 | 38.8 |
| wire.ocaml View, OxCaml | 397.4 | 200.3 | 197.1 | 222.5 | 57.9 |
| Wire.Codec, OxCaml | 8456.7 | 8242.5 | 214.2 | 2125.8 | 1739.5 |
| wire.ocaml scanner, OCaml flambda | 396.4 | 151.8 | 244.6 | 245.4 | 46.3 |
| wire.ocaml View, OCaml flambda | 490.4 | 245.9 | 244.5 | 297.2 | 63.2 |
| Wire.Codec, OCaml flambda | 10286.8 | 10036.8 | 250.0 | 2229.0 | 1882.8 |
| wire.ocaml scanner, OCaml | 516.6 | 310.1 | 206.5 | 317.1 | 89.3 |
| wire.ocaml View, OCaml | 422.8 | 221.1 | 201.7 | 272.9 | 64.5 |
| Wire.Codec, OCaml | 9134.8 | 8934.3 | 200.5 | 2133.3 | 1851.4 |

The table alone, `flow table, keys from an array` less the fold of `keys
from an array, parse only`, costs 45.9 instructions per packet in Rust
(64.3 less 18.4), 202.3 on OxCaml (271.1 less 68.8), 243.8 on OCaml flambda
(319.0 less 75.2) and 206.3 on OCaml (274.2 less 67.9): the rows' table
column, but for smoltcp, whose row spends 29 instructions more than its
parse and the table do apart. Against `pnet_packet`, the OxCaml scanner
spends 218 instructions and 100 cycles more per packet; the table is 167 of
the instructions and 89 of the cycles.

Both tables lay their slots out alike, 64 bytes each in one buffer, and probe
alike; a `while` loop in place of the recursive probe costs 7 instructions
more on OxCaml. What the OxCaml table spends beyond the Rust one, read in
the disassembly of `Flow_table` and weighted by the trace (a key claims a
slot in 34.5% of the packets and finds its slot in 65.5%; 82% are IPv4):

1. checked `Bytes` accessors, about 67 instructions per packet: each access
   untags its offset, compares it with the length and branches (4), and each
   function recomputes the length from the block header and its last byte
   (8) and the bound of each access width (5 or 6), 51 instructions for an
   IPv4 key that finds its slot and 89 for one that claims a slot, where Rust
   checks the slot index once per probe (2);
2. the hash constant, built again for every multiplication, about 41: OCaml's
   tagged multiplication by `0x1E37_79B9_7F4A_7C15` takes the constant and
   its tag correction, 8 instructions per mix, 32 for the 4 mixes of an IPv4
   key and 80 for the 10 of an IPv6 key; Rust keeps the constant in a
   register for the whole row;
3. calls left out of line, about 38: `add4` tail-calls the probe with 9
   arguments, one through the domain state, the probe calls `claim`, each
   with a frame, a stack check and a poll, and the call to `claim` saves 5
   registers; Rust inlines `add4` and `add` (only `add6` stays a call);
4. tagging, about 23: a 32-bit read is retagged (2), `Int32.of_int` sign
   extends each 32-bit write (2), each xor of the hash is retagged (1);
5. the table's global state, about 6: the buffer, the mask and the epoch are
   loaded from a global record in every call, where Rust keeps them in
   registers;
6. a `dmb ishld` barrier before the 64-bit write of the byte count, 1, which
   OCaml 5 puts before a store on arm64.

The OxCaml scanner parses in 117.5 instructions where the Rust scanner takes
69.2: about 25 in the loop over the frames, which reloads the lengths of the
buffer (8) and of the array of lengths (3) on every frame and checks both
array reads (8), where Rust bounds the frame in 7; about 12 in the fold,
whose state is a global record written behind two barriers; the rest in
tagging the fields the scanner hands out.

`Wire.Codec` spends 8242.5 instructions and 1739.5 cycles per packet on the
parse on OxCaml. macOS `sample`, 10 s of the parse row (7514 samples), puts
21.1% of its time in closure application (`caml_apply4` 16.9%), 28.0% in
`Codec_seal` (`apply_fwd`, the record constructors, 12.2%), 14.0% in
`Codec_compile` (the optional nested sections and the variable-length
fields), 12.2% in `Codec_repr` (`read_slot`, the field values read again for
the constraints, 9.0%), 9.9% in the compiled constraint expressions, 7.4% in
`Codec_layout` (offsets computed per decode) and 7.4% elsewhere. Allocation
costs little: 51.1 minor words (409 bytes) per packet on OCaml 5.4.1, none
promoted, and no GC function of the runtime reaches 5 samples.

## Compiler flags

The `bench` profile of `dune-workspace` adds the flags of
`bench/flags.sexp` to every unit, the libraries a row calls as well as the
row; `bench/dune` writes that file for the compiler that builds. The flags
are measured with `bench/flags-sweep.sh`, which builds `bench/apps` and
`bench/rust` once per candidate and counts every row of both, the six
application workloads and the `fixed64-random` and `mix128-random` corpora,
three times with `bench/count.sh`:

```
sh bench/flags-sweep.sh - '' -O3 -unboxed-types '-inline 25' '-inline 50' \
  '-inline 100' '-inline 200'
sh bench/flags-sweep.sh 5.4.1-flambda '' -O2 -O3 '-O3 -unbox-closures' \
  '-O3 -inline 200' '-O3 -remove-unused-arguments' -Oclassic \
  -unbox-closures -remove-unused-arguments
sh bench/flags-sweep.sh 5.4.0-ox9 '' -O3 -O4 '-O3 -cfg-block-layout' \
  '-O3 -regalloc irc' '-O3 -regalloc ls' '-O3 -regalloc gi' '-O3 -vectorize' \
  '-O4 -cfg-block-layout' '-O4 -regalloc irc' \
  '-O4 -cfg-block-layout -regalloc irc' \
  '-O4 -cfg-block-layout -regalloc irc -vectorize'
sh bench/flags-sweep.sh cargo '' '-C target-cpu=native'
```

A row's count is the median instructions per item of its three counts. A
candidate is adopted over the flags already adopted when every row gives the
same result, the geometric mean over the rows falls, and no row rises by more
than 1%; `-O3` on stock OCaml, which ignores it, moves no row by more than
0.4%. The mean is against the build without flags; the verdict names the
rows that rise by more than 1% against the flags adopted before:

| Compiler | Candidate | Mean | Verdict |
| --- | --- | ---: | --- |
| OCaml 5.4.1 | `-inline 50` | -2.1% | adopted |
| | `-inline 25` | -1.9% | `Wire.Codec stack decode` +2% |
| | `-inline 100`, `-inline 200` | -2.0% | the two `fold` rows +4% |
| | `-unboxed-types` | | `wire` does not build |
| OCaml 5.4.1 flambda | `-O2` | -1.7% | adopted |
| | `-O3`, alone or with `-unbox-closures`, `-inline 200` or `-remove-unused-arguments` | -2.8% | routing `Wire.Codec` +2% |
| | `-Oclassic` | -0.8% | four `Wire.Codec` rows, up to +6% |
| | `-unbox-closures`, `-remove-unused-arguments` | +0.2% | mean rises |
| OxCaml 5.4.0 | `-O3` | -12.1% | adopted, then replaced |
| | `-O4` | -12.3% | adopted |
| | `-O4 -regalloc irc` | -12.3% | mean unchanged |
| | `-O4 -cfg-block-layout`, with or without `-regalloc irc` or `-vectorize` | -12.5% | gateway `wire.ocaml` +2% |
| | `-O3 -vectorize` | -12.0% | mean rises |
| | `-O3 -regalloc ls` | -7.6% | 16 rows, up to +26% |
| | `-O3 -regalloc gi` | -4.0% | 24 rows, up to +49% |
| Rust 1.94 | `-C target-cpu=native` | -0.0% | mean unchanged |

The flags adopted are `-inline 50` on OCaml, `-O2` on OCaml with flambda
and `-O4` on OxCaml, which over `-O3` spends 0.2% fewer instructions and
2.4% fewer cycles with no row up by more than 0.7%. Rust keeps its release
profile: `target-cpu=native` leaves the instructions unchanged and saves
cycles only within noise, and the OCaml compilers have no such flag on
arm64.

## Profiling

```
make prof                 # CPU profile with Instruments (macOS)
make memtrace             # allocation hotspots via memtrace
```

## EverParse pipeline

`make bench` triggers the EverParse code generation pipeline:

1. `gen_stubs.exe` generates `.3d` files from Wire DSL definitions
2. EverParse (`everparse-3d.exe`) compiles `.3d` to verified C validators
3. `Wire_stubs` generates C/OCaml FFI stubs
