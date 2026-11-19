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
make bench-demo           # requires EverParse (3d.exe in PATH)
```

## Application benchmarks

Throughput on realistic workloads. The C loop calls the EverParse validator for
field extraction, then runs application logic. The OCaml loop does the same
work with `Codec.get`/`Codec.set`.

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
| `Wire.Codec` | the OCaml workload (OCaml 5, OxCaml) | `Codec.get` per field; for CLCW `Codec.load_word` once, then `Codec.extract` per field. Writing: `Codec.writer` of the default header and the fields that vary, one call per header |
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

Each row on its own, with the inputs `make bench-apps-rust` writes, on an
ARM64 macOS host under a load average near 100, so the times are
indicative; OCaml 5.4.1 (no flambda), OxCaml 5.4.0+ox (switch `5.4.0-ox9`,
`-O3` on the harness only), rustc 1.94.1, with the crates
`cargo/Cargo.lock` pins. Instructions per item are what `make
bench-apps-count` reports: `bench/count.sh` runs the row alone with 1 and
with 21 timed passes, each after the warm-up pass, and divides the
difference by 20 passes, which cancels starting the process, the input
read, the output allocation and the warm-up pass; it fails unless both runs
print the row's `RESULT` line with the same result and the counters read
some instructions. The time is the best CPU time per item of the timed
passes. Instructions / ns per item (stale: measured as the difference
between 4 timed passes and 1, on inputs without anomalies, before the
`Wire.Codec` gateway row lost its closure per frame,
with `-O3` on the harness only):

| Row | routing | gateway | clcw | routing_write | gateway_write | clcw_write |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| wire.rust | 12 / 4.39 | 332 / 49.7 | 11 / 1.06 | 31 / 6.40 | 215 / 26.0 | 3.4 / 0.55, vectorised |
| spacepackets 0.19 | 29 / 5.46 | - | - | 33 / 5.93 | - | - |
| ccsds_primary_header 0.15 | 28 / 5.65 | - | - | 34 / 5.63 | - | - |
| gaia-ccsds-c2a 1.2 | 25 / 4.15 | - | 12 / 0.76 | 51 / 5.99 | - | 3.6 / 0.51, vectorised |
| chronus-gateway 1.0.0-rc.1 + spacepackets 0.19 | - | 378 / 48.9 | - | - | 222 / 26.7 | - |
| wire.ocaml, OxCaml target | 41 / 6.37 | 727 / 57.9 | 40 / 3.16 | 78 / 9.37 | 811 / 59.1 | 64 / 4.52 |
| wire.ocaml, OCaml | 63 / 6.71 | 864 / 69.7 | 55 / 4.29 | 124 / 9.96 | 1215 / 79.0 | 70 / 5.69 |
| Wire.Codec, OxCaml | 135 / 13.8 | 2089 / 132 | 120 / 7.05 | 540 / 33.4 | 6867 / 410 | 619 / 39.4 |
| Wire.Codec, OCaml | 164 / 11.8 | 3345 / 214 | 146 / 8.86 | 657 / 39.3 | 8510 / 496 | 859 / 52.3 |

The two Rust `clcw_write` rows are autovectorised: LLVM turns their loops
into NEON code that builds several words per instruction, where every other
cell runs one item at a time, so they do not compare loop for loop with the
OCaml rows.

Every row gave `(hk=4000000 sci=3500000 diag=2000000 idle=500000)` on
routing, `16569803807358438373` on gateway and 0 on clcw, and wrote buffers
hashing to `c2824049e5408e67` (routing), `42b48f14215237fe` (gateway) and
`7fd27aeb79936a55` (clcw), on inputs that held no anomaly. The inputs now
hold one of each kind the readers test for, so every row takes each branch:
the routing stream ends in a truncated packet after which the reader wraps,
one gateway frame in 64 is idle, and CLCW words carry wait, retransmit and
lockout and skip report values; writing sets lockout, wait and retransmit
too, and the crate's CCSDS 232.0 words and the codec's hash the same.

Writing is where the generated OCaml is furthest from Rust. A
`write_int` checks every field value against its width on every call,
where LLVM proves from the loop that `(p % 2048) as u16 >> 11` is 0 and drops
the check; the OCaml assembles a 32-bit word on tagged integers, with an
`int32` sign normalisation and a `sxtw` on each side of the `rev` that do
nothing, where OxCaml builds it untagged; and each packet polls for a
safepoint. On gateway that is 557 instructions per frame on OxCaml against
354 in Rust. LLVM also vectorises the CLCW writer, 4.9 instructions per
word. `Wire.Codec` writes a header with one call to a staged
`Codec.writer`, which puts the default header and the fields that vary
together in an `int` and stores it; the table above predates it, when the
row copied the encoded default and called a staged `Codec.set` per field,
about 150 instructions per field. These headers are at most 6 bytes,
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
| `Wire.Codec valid/read, linked headers` | interpreted codecs of the fixed headers (OCaml 5, OxCaml) | `Codec.valid` per header, then a staged `Codec.read` per field; the harness links the layers |
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

A real capture (length-prefixed frames) is not measured: it has frames of
every length, which the batch consumers cannot take without a second,
per-frame code path per row, and padding each frame to a fixed slot would
change what the parsers accept.

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
2. EverParse (`3d.exe`) compiles `.3d` to verified C validators
3. `Wire_stubs` generates C/OCaml FFI stubs
