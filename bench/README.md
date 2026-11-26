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

The C validator checks the schema as it extracts fields. The reader OCaml
`Codec.get c f` makes reads the requested field and does not validate the whole record;
call `Codec.validate` separately when that is required. The timing ratio
therefore includes different amounts of validation work.

## Rules

- **No hand-written C parsers.** All field extraction in C must go through
  EverParse-generated validators using the default `<Name>_Fields` visitor:
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
work with `Codec.Word.check` and `Codec.Word.read`, and writes with
`Codec.writer`.

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
| `Wire.Codec` | the OCaml workload (OCaml 5, OxCaml) | `Codec.Word.check`, `ok` and `get` per header, then `Codec.Word.read` per field, for CLCW too. Writing: `Codec.writer` of the default header and the fields that vary, one call per header |
| `wire.ocaml` | generated OCaml, the module `Wire_ocaml.v` declares for `Space.packet_codec`, `Space.tm_frame_codec`, `Space.clcw_codec`: default target on OCaml 5, ``~target:`OxCaml`` on OxCaml | `check b len off` per header, the check of a codec of constant size, which takes no window, then an accessor per field; for CLCW `view_lockout_word` once, then `<field>_of_word` per field. Writing: `write_at`, or `write_int_at` where a field's value is computed, per header |
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

The released crates are the reference these rows aim to beat. Against the
crate row with the fewest instructions on each workload, the OxCaml
`wire.ocaml` row spends 1.4 to 3.7 times the instructions where both work
one item at a time, and about 11 times on `clcw_write`, whose crate loop LLVM
vectorises. The counts below are the median of three runs of
`bench/count.sh` on each row, with the inputs of `make bench-apps-count`
(100,000 items), at the commit before the one that adds them, on the ARM64
macOS host above under a load average of 150 to 280. The counts of one row
still moved by up to 5% between runs at that load, so a change of a few
instructions per item is read from the loop, not from the count. The loop
lengths are read from `objdump -d` of the binaries that run counts,
`_build/oxcaml/default/bench/apps/bench.exe` (`-O4`) and
`bench/apps/cargo/target/release/wire-bench-apps`, with the Rust rebuilt
once with line tables (`CARGO_PROFILE_RELEASE_DEBUG=line-tables-only`, the
same code) to find each row's inlined loop. A loop length counts the
instructions on the path a well-formed item takes:

| Workload | Crate row | Instructions per item, crate / OxCaml | Loop, crate / OxCaml |
| --- | --- | ---: | ---: |
| `routing` | `gaia-ccsds-c2a` 1.2 | 24.6 / 41.4 | 24 / 47 |
| `routing_write` | `ccsds_primary_header` 0.15 | 38.8 / 68.7 | 40 / 71 |
| `gateway` | `chronus-gateway` + `spacepackets` | 356 / 612 per frame | 22 / 40 per packet |
| `gateway_write` | `chronus-gateway` + `spacepackets` | 354 / 511 per frame | 21 / 30 per packet |
| `clcw` | `gaia-ccsds-c2a` 1.2 | 11.5 / 42.1 | 12 / 43 |
| `clcw_write` | `gaia-ccsds-c2a` 1.2 | 4.5 / 51.5 | 68 per 16 words / 51 |

Each instruction the OxCaml loop runs beyond the crate's falls in one of
four kinds: what `Wire_ocaml` generates, what the harness writes, what the
compiler does with both, and what the OCaml 5 runtime asks of every loop
(a safepoint poll, a barrier before a store to a mutable block). The
generated reads and writes are not where the gap is: each field is one
load, a swap and a mask, or one merged store, as in the crates. Two
differences came from the harness, where the OCaml loops differed from the
Rust ones, and are gone: the gateway loops took the packet size from
`Gateway_workload.pkt_size`, which `Codec.min_size` computes when the
program starts, where the Rust rows use the constant `space_packet::SIZE +
64`, so OxCaml added it as a variable and built the data length's bytes
again per packet (`asr`, `rev16`, `uxth`); and `routing_write` kept each
route's sequence count as an `int`, so `write_at`'s carrier checked its
range per packet (`lsr #14`, `orr`, `cmp`, `b.eq`), where it now keeps the
count in its carrier and wraps it with `seqcount_low`, as the Rust rows
wrap it with `& 0x3FFF`. The rest is the compiler and the runtime, with the
open OxCaml issue of each compiler item where one exists:

| Workload | Instructions per item beyond the crate's loop, and the issue |
| --- | --- |
| `routing` | route table address reloaded (`adrp`, `add`, `ldr`) and `len - size` recomputed, 4: oxcaml/oxcaml#8175; the 11-bit APID's index into the 2048-entry table checked, 3: oxcaml/oxcaml#2436; the offset untagged and retagged around each load, 2 to 3: oxcaml/oxcaml#8171; the check's answer moved through a join (`mov`, `mov`) before its test, 2: oxcaml/oxcaml#8182; an extra `b` on the back edge, 1: oxcaml/oxcaml#8177; the poll (`ldr`, `cmp`, `b.ls`), 3, and the `dmb ishld` before the count's store, 1: the runtime |
| `routing_write` | the routing items above; the write's `off < 0` tested again after the read's check of the same offset, 2: oxcaml/oxcaml#8182; the 32-bit store's swap as `rev x` then `asr #32`, 1, and `uxth` after `rev16`, 1: oxcaml/oxcaml#8184; the data length tagged then untagged for its store, 2: oxcaml/oxcaml#8173 |
| `gateway` | per packet, the FNV prime and basis rebuilt by `mov`/`movk`, 8: oxcaml/oxcaml#8183 and oxcaml/oxcaml#8175; the `ok` flag of the `while` taken through `cset` and `cbnz` twice, 4, and the `or #1` of `hash_int` on a tagged `int`, 2: no issue yet; the poll, 3: the runtime |
| `gateway_write` | per packet, the store addresses computed from the tagged offset, 2: oxcaml/oxcaml#8171; the 32-bit store's swap, 1: oxcaml/oxcaml#8184; APID and count masked and untagged from the tagged packet number, 4: tagging; `off + 70` and the frame end recomputed, 1: oxcaml/oxcaml#8175; the poll, 3: the runtime. Per frame, the range test of the four computed header fields `write_int_at` takes, 8: oxcaml/oxcaml#2436 |
| `clcw` | each of three flags as `lsr`, `orr`, `and`, `cmp`, `b.ne` where `gaia-ccsds-c2a` has one `tst` and one `tbnz` for all three, 12: oxcaml/oxcaml#8172; the word zero-extended again after `rev w`, 1: oxcaml/oxcaml#8184; the word tagged, 2; the bounds of each word checked, which `chunks_exact` proves, 5: oxcaml/oxcaml#2436; the offset tagged, 3: oxcaml/oxcaml#8171; the poll, 3: the runtime |
| `clcw_write` | LLVM vectorises the crate's loop, OxCaml vectorises none (the NEON lanes row is that loop written by hand); one word at a time, the range test of the four computed fields, 7: oxcaml/oxcaml#2436; each field untagged before its shift, 4: tagging; the 32-bit store's swap, 1: oxcaml/oxcaml#8184 |

A flag tested as `w land mask <> 0` instead of `(w lsr k) land 1 <> 0`
takes one instruction fewer on OxCaml, `mov`, `and`, `cmp`, `b.ne`, as the
tagged mask is not an arm64 logical immediate, and is no `tst` or `tbnz`
until oxcaml/oxcaml#8172 is fixed: the `clcw` loop goes from 43 to 40. The generated
`<field>_of_word` returns the field's `int`, so that form would need a
boolean reader of a 1-bit field, which no codec here declares (`Wire.bit`
goes through a named map).

On routing, LLVM reads `gaia-ccsds-c2a`'s `PrimaryHeader` with two `ldrb`
for the APID and one `ldrh` and `rev16` for the data length; the APID has 11
bits, so it indexes the 2048-entry route table unchecked and keeps one
check, `cmp x8, #0x3`, on the route. The OxCaml loop reads each field with a
32-bit `ldrsw`, a `rev` and a mask, reloads the table's address (`adrp`,
`add`, `ldr`) and checks both array indices, puts a `dmb ishld` before the
count's `int array` store, polls for a safepoint, and keeps the offset and
the counter tagged. The chain from one packet's offset to the next is eight
dependent instructions in both, made of different parts: `gaia-ccsds-c2a`
spends `add`, `cmp` and `csel` on the wrap to offset 0, then `add`, `ldrh`,
`rev16`, `add`, `add`; OxCaml takes the wrap as a predicted branch, and spends
`asr` to untag, a `mov`, `add`, `ldursw`, `rev`, `and #0xffff` and two `add`s
on the tagged offset. Its cycles in this run are close, 16.0 per packet
against 14.6. `spacepackets` and `ccsds_primary_header` read the header as a
32-bit and a 16-bit load merged with `orr` and `lsr`, eleven instructions on
the same chain.

On writing, the crates never branch on a field's value:
`ccsds_primary_header`'s setters and `chronus-gateway`'s
`TmFrameHeader::to_bytes` mask each field (`seq_count & 0x3FFF`), and
`spacepackets`' `u11::new` and `u14::new` assert, which LLVM proves cannot
fire, so no range branch is left in their loops. The generated OCaml checks
a value against its field's width wherever the compiler cannot prove it
fits: on `gateway_write`, every field of the frame header `write_int_at`
takes; on `clcw_write`, the four fields that vary per word. On
`routing_write` no range is tested: LLVM proves the `[u16; 4]` counts stay
below 2^14 and drops `ccsds_primary_header`'s mask, and the OxCaml loop keeps
them in `Value.seqcount`, whose type proves it. Per packet of
`gateway_write`, `spacepackets` stores the data length from a register set
before the loop and addresses both stores from one base, where OxCaml stores
the constant data length with `orr` and `strh`, computes the second store's
address from the tagged offset, masks and untags the APID and count from the
tagged packet number, and polls.
`gaia-ccsds-c2a`'s `with_<field>` builders vectorise: 68 NEON instructions
build 16 CLCW words, with the byte order folded into the shifts. Stock OCaml
also assembles the 32-bit word on tagged integers, with a `sxtw` on each
side of the `rev` that does nothing (`sxtw x3, w2`, `rev w4, w3`,
`sxtw x5, w4`, `str w5`), where OxCaml builds it untagged. `Wire.Codec`
writes a header with one call to the writer `Codec.writer` makes, which puts the
default header and the fields that vary together in an `int` and stores it.
These headers are at most 6 bytes, so no row stores a 64-bit word: the
generated writes are `str w` and `strh`, and the only `dmb ishld` OxCaml puts
in a write loop is the one before the per-route sequence count's `int array`
store of `routing_write`.

On gateway, each packet costs OxCaml 40 instructions against 22: it builds
the two 64-bit constants each `hash_int` multiplies and adds with again with `mov` and `movk`
pairs inside the loop, 8 instructions per packet, and a `hash_int` on a
tagged `int` is `eor`, `orr #1` and `madd`, where LLVM's is `eor` and `mul`
with the 63-bit mask moved out to once per frame; the loop's `ok` flag goes
through `cset` and `cbnz`. On clcw, `gaia-ccsds-c2a`'s `CLCW` getters compile
to two byte loads (`ldurb` of the flags byte, `ldrb` of the report value),
one `tst #0x30` and one `tbnz #3` for the three flags, and no bounds check,
which `chunks_exact` proves; OxCaml loads the word (`ldrsw`, `rev`), tags it,
takes each flag with `lsr`, `orr`, `and`, `cmp` and `b.ne`, checks the word's
bounds and polls. The NEON lanes row reads a CLCW word in 10.5 instructions,
below the crate's 11.5, and writes one in 7.0 against the crate's 4.5.

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
| `Wire.Codec stack check/enter/read` | interpreted codec (OCaml 5, OxCaml) | `View.check` of the same bounded stack codec, which builds nothing, then `View.enter` of each layer present (a `View.sub` or `View.sub_option` per sub-codec field) and a `View.read` of a `View.field` per field |
| `Wire.Codec check/read, linked headers` | interpreted codecs of the fixed headers (OCaml 5, OxCaml) | a `Codec.View.codec` per header codec; `View.check`/`View.ok`/`View.get` per header, then a `View.read` of a `View.field` per field; the harness links the layers |
| `wire.ocaml fused stack`, `selected outputs` | generated OCaml, default target on OCaml 5, ``~target:`OxCaml`` on OxCaml | the scanner of the stack: `fold` and `fold_frames` (batch), on OxCaml also their unboxed forms; `selected outputs` hands out only the checksum's fields |
| `wire.ocaml View.check/view, linked headers` | generated OCaml record modules of the fixed headers | `check b len off`/`ok`/`get` per fixed header, then an accessor `<field> b v` per field; the harness links the layers, as the `wire.rust, linked headers` row does |
| `wire.rust fused stack`, `selected outputs` | generated Rust, `Wire_rust.scanner` of the stack | `scan` per frame; `selected outputs` hands out only the checksum's fields |
| `wire.rust, linked headers` | generated Rust of the fixed headers | `Valid::at` per header, then a read per field; the harness links the layers |
| `EverParse C, default visitor` (`BUILD_EVERPARSE=1`) | the verified validator EverParse generates from `Packet_stack.codec` | `EthernetStackValidateWireEthernetStack` per slot, then the members of the default `EthernetStack_Fields` visitor, zeroed per slot |
| `EverParse C, inline visitor` (`BUILD_EVERPARSE=1`) | the same validator, compiled with a custom visitor | the validator per slot, then the same members of a visitor copied from `EthernetStack_Fields.c` and trimmed to them, its setters `static inline` in the validator's translation unit |
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
bounds. The folds of `Wire_ocaml` and `Wire_rust.scanner` write those rules out
from one plan as a fused scanner; the harness only folds returned scalar
fields into the checksum. Its
`off`/`len` arguments bound each frame even inside a larger corpus buffer.
`Codec.decode` uses the same stack inside a framing region of one corpus
slot and constructs its ordinary nested values. Byte fields are zero-copy
slices.

The frame loop of the batch rows (`fold_frames` and its unboxed and step
forms) walks four frames per iteration, so that it polls for signals once
per four frames. Each iteration first computes where its four frames start
and loads the field their transport header is chosen on, the IPv4
protocol, then walks the frames: a mispredicted branch of one frame does
not hold back the next frame's dispatch by a load. The generated Rust has no frame
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
Their `ocaml-ox` lanes also compare the boxed and unboxed folds and check
`fold_frames_unboxed` with `[@zero_alloc]` in release builds. With
`BUILD_EVERPARSE=1`, the same differential driver compiles the projected
3D schemas through EverParse and compares C acceptance, consumed length and
the scalar members of the default visitor, nested fields included.
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

With `BUILD_EVERPARSE=1` (`make bench-rust BUILD_EVERPARSE=1`), the two
EverParse C rows run too. `EverParse C, default visitor` links the validator and
default visitor that `gen_stubs.exe` archives with the other benchmark schemas
(compiled at `-O2`, as dune compiles C stubs), and its C loop calls the
validator once per slot, bounded by the slot, reads the IPv4, TCP and UDP
fields as named visitor members (`ipv4_src`, `ipv4_tcp_present`, ...; see
[the FFI visitor](../docs/projection.md#the-ffi-visitor)) and folds them into the
same checksum. Every field reaches the default visitor through an out-of-line
setter call in another translation unit, 30 for a TCP packet.
`EverParse C, inline visitor` (`bench/rust/everparse/stack_inline.c`) compiles
the same generated validator in one translation unit with a custom visitor,
copied from the shipped `EthernetStack_Fields.c` and trimmed to the members
the checksum reads: its setters are `static inline` and write into a local
struct, so the compiler inlines each into the validator and folds its switch
on the constant field index, and the row measures the validator itself. Both
loops read the same members and fold them the same way, and time their
passes in C as the Rust harness does.

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
the OCaml, OxCaml, Rust and C tables (`flow_table.std.ml`,
`flow_table.ox.ml`, `cargo/src/flow.rs`, `everparse/stack_c.c`) fill the
same slots. A slot belongs to the pass whose epoch it carries, so no
pass clears the table. Each row prints the number of flows and a checksum of
every live slot (its place, key, packets and bytes), read after the timed
passes and not timed; the report fails unless every row agrees. The default packets hold 342,506 flows.

| Row | Implementation | Parse |
|---|---|---|
| `Wire.Codec capture decode` | interpreted codec (OCaml 5, OxCaml) | `Codec.decode` of the pcap record around the frame, its `incl_len` bounding `Packet_stack.capture`, then the nested values |
| `wire.ocaml View.check/view, linked headers` | generated OCaml record modules of `headers.ml`, IPv6 included | `check b len off`/`ok`/`get` per fixed header, an accessor `<field> b v` per field; the harness links the layers and checks each header ends inside the capture |
| `wire.ocaml scanner, selected outputs, fold_step` | generated OCaml scanner of `Packet_stack.capture` | `classify_index` over the frames, handing the classifier's fields to `Flow_step` (`Flow_parse` without the table) by name |
| `wire.ocaml check/view, accessors` | the views the generated OCaml scanner of `Packet_stack.capture` writes | `check` per frame, then the accessor of each field, each sub-record (`ipv4`, `ipv6`, `tcp`, `udp`) entered through the accessor of its field, which tests its presence and places it again |
| `wire.rust scanner, selected outputs` | generated Rust scanner of `Packet_stack.capture` | `scan` per frame |
| `wire.rust, linked headers` | generated Rust of `headers.ml` | `Valid::at` per header, then a read per field; the harness links the layers |
| EverParse C (`BUILD_EVERPARSE=1`) | the verified validator of `Packet_stack.capture` | `CapturedStackValidateWireCapturedStack` per frame, then the members of the default `CapturedStack_Fields` visitor, zeroed per frame |
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

The methodology follows ixy, the user-space driver for Intel's ixgbe NICs
(P. Emmerich et al., "The Case for Writing Network Drivers in High-Level
Programming Languages", ANCS 2019): the paper writes it in Rust, Go, C#,
Java, OCaml, Haskell, Swift, JavaScript and Python, each "from scratch in
idiomatic style" and following "the same basic architecture" as the
reference C driver it compares them with
([ixy-languages](https://github.com/ixy-languages) adds C++). Every test is
"restricted to a single CPU core", forwards packets while modifying one byte
of each, and counts packets per second, since "the limiting factor for
network drivers is the number of packets per second, not the bandwidth".
Here too: one core, packets per second, and the same design in every
language, so the rows differ in the parser and not in the work around it.
ixy swept the batch size, because each batch received or sent needs a
synchronisation with the NIC, at 3.3 GHz and at 1.6 GHz, and read CPU
counters per forwarded packet (Rust spends 65% more instructions than C at a
batch of 32, and 6% more cycles); here the packets are already in memory and
every row walks them one frame at a time, which is the batch the whole
buffer gives, so there is no batch size to sweep.

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
| EverParse C, default visitor | 95.62 | 99.07 | 10.5 |
| wire.ocaml scanner, selected outputs, fold_step, OCaml | 98.61 | 101.16 | 10.1 |
| Wire.Codec capture decode, OxCaml | 632.38 | 654.09 | 1.6 |
| Wire.Codec capture decode, OCaml | 681.18 | 711.91 | 1.5 |
| Wire.Codec capture decode, OCaml flambda | 712.21 | 739.20 | 1.4 |

Every row counted 342,506 flows and gave checksum `3901415651386128532`.

### Instructions per packet

The host was loaded, so the counts below are instructions, which another
process changes little, with cycles for scale. `bench/count.sh` runs a row
with 1 and with 21 timed passes and divides the difference by 20 passes of
the 1,000,000 packets, which cancels the start of the process, the read of
the capture, the recording of the keys and the warm-up pass:

```
sh bench/count.sh 1000000 ROW HARNESS flow _build/bench-flow/mawi.pcap RUNS ROW
sh bench/count.sh 1000000 ROW wire-bench-flow-parse _build/bench-flow/mawi.pcap RUNS ROW
```

Measured on the branch that rewrites the OCaml table, on the machine above:
the OCaml rows with a load average of 98 before and 122 after, the Rust rows
earlier at 62 and 65, no idle CPU either time. Parse is the row on `mawi
parse`, which folds its keys instead of counting them; table is the row on
`mawi` less its parse.

| Row | Total | Parse | Table | Cycles, total | Cycles, parse |
| --- | ---: | ---: | ---: | ---: | ---: |
| wire.rust scanner, selected outputs | 113.5 | 68.8 | 44.7 | 98.7 | 26.9 |
| wire.rust, linked headers | 111.0 | 65.6 | 45.4 | 99.6 | 26.9 |
| pnet_packet 0.35 | 108.6 | 66.7 | 41.9 | 92.3 | 26.6 |
| smoltcp 0.14 | 139.1 | 63.5 | 75.6 | 107.5 | 25.7 |
| etherparse 0.21, lax | 480.2 | 428.3 | 51.9 | 290.4 | 96.5 |
| wire.ocaml scanner, OxCaml | 235.4 | 117.7 | 117.7 | 288.2 | 41.7 |
| wire.ocaml View, OxCaml | 306.0 | 200.0 | 106.0 | 327.3 | 58.3 |
| Wire.Codec, OxCaml | 8342.9 | 8236.7 | 106.2 | 2280.5 | 1776.7 |
| wire.ocaml scanner, OCaml flambda | 257.0 | 152.9 | 104.1 | 315.4 | 48.6 |
| wire.ocaml View, OCaml flambda | 350.3 | 246.7 | 103.6 | 351.4 | 64.6 |
| Wire.Codec, OCaml flambda | 10149.2 | 10040.1 | 109.1 | 2455.8 | 1921.5 |
| wire.ocaml scanner, OCaml | 404.6 | 310.7 | 93.9 | 459.7 | 93.2 |
| wire.ocaml View, OCaml | 311.4 | 222.0 | 89.4 | 287.8 | 65.5 |
| Wire.Codec, OCaml | 9025.4 | 8927.2 | 98.2 | 2366.1 | 1782.2 |

The table alone, `flow table, keys from an array` less the fold of `keys
from an array, parse only`, costs 44.9 instructions per packet in Rust
(64.4 less 19.5), 109.0 on OxCaml (178.0 less 69.0), 105.1 on OCaml flambda
(180.3 less 75.2) and 93.5 on OCaml (161.0 less 67.5). Two more counts gave
107.5 and 112.2 on OxCaml, 103.4 and 105.5 on flambda, 94.0 and 93.8 on
OCaml. Counts of one row repeat within 5 instructions on this host, but one
of four counts of the Rust table row came out 6.4 higher (70.8). The table
held its slots in one `Bytes` buffer read through checked accessors before,
for 200.8, 244.9 and 206.0. Against `pnet_packet`, the OxCaml scanner spends
127 instructions and 196 cycles more per packet; the table is 76 of the
instructions and 181 of the cycles.

The OCaml table is what a user writes for the Rust one's `Vec<Slot>`: an
array of slot records with mutable fields, one checked index per probe, the
fields then read through the record. Both tables hash, probe and compare
alike. One `int array` per field instead costs 139.2 instructions on OCaml,
154.5 on flambda and 172.9 on OxCaml, and about 400 cycles, as each probe
touches 13 arrays. The Rust `Slot` is `#[repr(C)]` and padded to 64 bytes, to
match the C table's layout; that is all it does beyond plain Rust.

What the OCaml tables spend beyond the Rust one, read in the disassembly:

1. calls: no compiler inlines `add4` or `add6` into the loop over the keys,
   and the probe is a function of its own, entered by a tail call with 9
   arguments on OxCaml (one through the domain state) and a closure on OCaml
   and flambda, where Rust inlines `add4` and the probe;
2. the hash constant, on OxCaml: the tagged multiplication by
   `0x1E37_79B9_7F4A_7C15` builds the constant and its tag correction, 8
   instructions per mix, 32 for an IPv4 key, where Rust keeps the constant in
   a register;
3. the stores: each write of a mutable field is a `dmb ishld` and a `str` on
   OxCaml and an `add` and a `stlr` on OCaml 5.4.1, and a claimed slot writes
   13 fields one at a time, where Rust writes its 64 bytes with paired and
   128-bit stores in 8 instructions; counting a packet writes 2 fields;
4. the layout: a slot is a pointer in the array to a record of 14 words
   (112 bytes) where Rust's slots lie in the vector itself, so each probe
   loads the record's address first, and the cycles count the extra cache
   line;
5. tagging: the slot index and the counters stay tagged, a word more per
   arithmetic step.

On OxCaml the table is `flow_table.ox.ml`: each word of a slot an `int64#`
stored flat in its record, an address two words in the order of Rust's
`[u32; 4]`, the probe a `while` loop over a `let mutable` index in each of
`add4` and `add6`, indexing plain and checked. The constant is then built
once per mix with no tag correction, nothing in a slot is tagged and an IPv4
slot writes 9 words. Counted at a load average of 251 before and 271 after,
the table alone falls from 108.6 to 81.9 instructions per packet (149.1 less
67.2), and on `mawi` the scanner row from 219.2 to 193.1 and the `View` row
from 309.1 to 283.1. An array of unboxed records `#{ ... }`, the layout of
Rust's `Vec<Slot>`, read and written through the stdlib's checked array
primitives declared at any layout, costs 192 instead: reading an element loads its 9 words
before any is compared, each with its own address, and each bound check
divides the array's size by 9.

What the OxCaml table spends beyond Rust's 44 is then the compiler's and
the memory model's: `add4` is still called, not inlined, and reloads the
table on every key; the constant is built four times per key
([oxcaml#8183](https://github.com/oxcaml/oxcaml/issues/8183)); the bound
check decodes the array's header on every probe
([oxcaml#8175](https://github.com/oxcaml/oxcaml/issues/8175)); the index is
tagged and each load computes its address apart
([oxcaml#8171](https://github.com/oxcaml/oxcaml/issues/8171),
[oxcaml#8178](https://github.com/oxcaml/oxcaml/issues/8178)); the loop's
exit tests its flag again and polls; and every store of a mutable field
follows a `dmb ishld`, 2 per counted packet and 9 per claimed slot, which no
compiler pass removes under OCaml 5's memory model.

The OxCaml scanner parsed in 117.7 instructions where the Rust scanner takes
68.8: about 25 in the loop over the frames, which reloaded the lengths of
the buffer (8) and of the array of lengths (3) on every frame and checked
both array reads (8), where Rust bounds the frame in 7; about 12 in the
fold, whose state was a global record written behind two barriers; the rest
in tagging the fields the scanner hands out. The row now walks the frames
with `fold_index_step`, which reads both lengths once per pass and the two
arrays with no bound check, and `Flow_parse` keeps its keys and sum in an
unboxed pair the loop carries in registers. Counted on the tree that adds
this, at a load average of 93 before and 79 after, the parse falls from
116.0 to 91.5 instructions per packet and the row on `mawi` from 233.9 to
218.4. What the loop still spends beyond Rust's, about 14 of its 21: the
poll at its back edge (3), the tagged index (2), an address computed for
each of the two loads where Rust loads the pair at once (3), the test of a
negative offset (2) and the untagging of the offset and the length (2). On
OCaml an accumulator of two values would be allocated per key, so
`Flow_parse` keeps `Flow_sum`'s global state there, and the loop saves less
(151.9 to 143.2 instructions on OCaml flambda, 308.2 to 298.2 on OCaml;
256.0 to 245.2 and 405.6 to 392.6 on `mawi`): the flambda register
allocator keeps the loop's buffer, arrays and lengths on the stack around the
inlined walk, and on OCaml the walk called its consumer out of line, as
below.

On OxCaml `fold_index_step` now walks four frames an iteration, as
`fold_frames` does, so the poll, the test of the index against the bound
and the back edge are paid once per four frames. Counted three times at a
load average between 160 and 285, the parse falls from a median of 92.0 to
85.2 instructions per packet and the row on `mawi` from 219.1 to 213.1;
the header benchmark's OxCaml rows, which walk fixed-stride slots with
`fold_frames`, are unchanged.

The code `Wire_ocaml` writes for OxCaml then spends fewer instructions on
each header. An indexed frame's bounds are tested on its untagged offset and
length, as the sign of one `logor`, where the tagged test took two branches
and their tag fixups. The IPv4 version and header length are one unsigned
comparison of their byte, which one shift takes from the word already
loaded, where a 16-bit load, a shift and two comparisons tested them. A
field inside a loaded word is taken out with two shifts, where OxCaml built
the mask of a shift and a mask in a register for each use. The TCP data
offset is read with the 16-bit load that ends on its byte, where a 32-bit
load, a swap and a mask took four instructions. The first frame of the
unrolled loop then walks an IPv4 TCP packet in 82 instructions, 6 fewer.
Counted three times at a load average between 300 and 450, the parse falls
from a median of 84.7 to 81.4 instructions per packet, against 69.2 for the
Rust scanner, 66.9 for `pnet_packet` and 64.2 for `smoltcp`, counted alike.
The header benchmark's OxCaml scanner rows fall by 1.2 to 4.2 instructions
per packet on every corpus (`selected outputs, batch` from 65.6 to 63.7 on
`fixed64-random`, where `pnet_packet` takes 113.8).

On OxCaml the key is now computed on `int64`, as `Sum` in Rust computes it
on `u64`: `Flow_sum` folds it so, and the scanner's consumer `Flow_parse`
hands it the `int64` values the scanner gives, which stay unboxed. On OCaml
and OCaml flambda it stays an `int`, the type a user of those compilers
folds it in, because an `int64` handed to a call is boxed there (`Flow_int`,
written by `bench/rust/dune`, names the type for each compiler). An IPv4 TCP
key then takes 17 instructions on OxCaml instead of 24: no tag on each
value, no constant 1 built for each tag; the code of the other compilers is
unchanged. Counted three times at a load average between 300 and 450, before
and after:

| Row on `mawi`, parse only | OCaml | OCaml flambda | OxCaml |
| --- | ---: | ---: | ---: |
| scanner | 299.9 to 299.4 | 144.8 to 144.5 | 82.0 to 73.3 |
| `View` | 223.5 to 222.9 | 249.6 to 250.5 | 200.2 to 198.3 |

The rows with the table do not fold the key. `pnet_packet` takes 66.9 on the
parse and 108.3 with the table, the Rust scanner 69.2 and 113.5.

On OCaml the scanner then spent more on the parse than `View`, because the
walk did not inline its consumer. `Flow_step` and `Flow_parse` bound `i` and
`ip6_bytes` inside `valid`, and OCaml without flambda inlines no function
whose body defines one, so each of the walk's 8 leaves called `valid`: it
boxed every `int64` output it passed, a block of 3 words on the minor heap,
and handed 21 of its 29 arguments through the domain state, where `valid`
loaded them back and unboxed them. That call was about 150 of the 300
instructions per packet. The consumers now define both beside `valid`, as
they define `ports`, and the walk inlines `valid` and the `add4` and `add6`
of `Flow_sum` behind it; what the loop keeps on the stack is its index and
bound. The code of the other compilers is unchanged. Counted five times,
medians, at a load average of 16 before and 23 after on OCaml, 22 and 16
on the other two:

| Row on `mawi` | Instructions | Cycles |
| --- | ---: | ---: |
| scanner, parse only, OCaml | 299.7 to 149.6 | 75.7 to 43.1 |
| scanner, OCaml | 393.8 to 232.8 | 456.0 to 305.1 |
| scanner, parse only, OCaml flambda | 145.1 to 145.5 | 47.0 to 45.0 |
| scanner, OCaml flambda | 248.3 to 248.3 | 245.4 to 258.0 |
| scanner, parse only, OxCaml | 73.3 to 73.3 | 32.6 to 31.6 |
| scanner, OxCaml | 185.2 to 185.2 | 201.8 to 191.5 |

In the same counts the `View` row takes 223.0 instructions and 60.2 cycles
on the OCaml parse; `pnet_packet` takes 67.2 and 26.9 and the Rust scanner
68.9 and 27.0, counted alike at a load average of 13 and 14.

Half of what every row spends on the parse is branches it mispredicts. The
same packets sorted by kind (Ethernet type, then IPv4 protocol or IPv6 next
header), which leaves the parse-only result unchanged, take `pnet_packet`
11.2 cycles instead of 22.5, the Rust scanner 12.3 instead of 23.0, the
OxCaml scanner 15.9 instead of 28.2 and the OCaml one 31.8 instead of 49.3,
with the same instructions (one batch, load average 20). After a
mispredicted branch the next frame's walk starts again from its index: a
load of its offset, then of its Ethernet type at that offset, before its
first choice. `fold_index_step` now loads the next frame's offset, length
and dispatch fields before it walks the current one, two frames an iteration
([How a codec runs](../docs/execution.md#generated-code)). Counted nine
times, interleaved, medians, at a load average of 52 before and 55 after:

| Row on `mawi`, parse only | Instructions | Cycles |
| --- | ---: | ---: |
| scanner, OxCaml | 73.1 to 77.1 | 32.7 to 24.2 |
| scanner, OCaml flambda | 144.7 to 150.1 | 46.5 to 38.5 |
| scanner, OCaml | 150.3 to 164.3 | 50.3 to 42.4 |

`pnet_packet` took 66.9 instructions and 23.7 cycles and the Rust scanner
68.8 and 23.5 in the same rounds. Another such batch, at a load average of 38
to 51, gave 26.1 cycles on OxCaml against 22.7 for `pnet_packet`, and two
batches of the OxCaml rows alone, at 28 to 50, 20.4 and 20.9 against 22.4
and 22.5: the cycles of one row move by several between batches at this
load, so the OxCaml scanner parses level with `pnet_packet`, where it took 9
cycles more. Seven rounds at a load average of 7 gave 21.5 cycles on OxCaml
against 30.0 before, 23.7 for `pnet_packet` and 24.0 for the Rust scanner,
with flambda at 30.8 and OCaml at 35.7. On the sorted packets it takes 13.0
cycles instead of 15.6.
With the table the parse is not what bounds the row: the OxCaml scanner
took 190 cycles against 200, within the noise, and 204 instructions against
182, as `Flow_step` calls the table's `add4` and `add6` out of line and the
frame the loop carries is spilled around each call.

What the OxCaml parse still spends beyond `pnet_packet` is mostly the
compiler's: the key's constant part is built again for each key, the loop
over the index computes the address of each tagged index (2 per frame) and
untags the offset and the length (2), and the IPv6 key folds 8 words one by
one where LLVM folds them as vectors.

`Wire.Codec` spends 8236.7 instructions and 1776.7 cycles per packet on the
parse on OxCaml. macOS `sample`, 10 s of the parse row (7514 samples), puts
21.1% of its time in closure application (`caml_apply4` 16.9%), 28.0% in
`Codec_seal` (`apply_fwd`, the record constructors, 12.2%), 14.0% in
`Codec_compile` (the optional nested sections and the variable-length
fields), 12.2% in `Codec_repr` (`read_slot`, the field values read again for
the constraints, 9.0%), 9.9% in the compiled constraint expressions, 7.4% in
`Codec_layout` (offsets computed per decode) and 7.4% elsewhere. Allocation
costs little: 51.1 minor words (409 bytes) per packet on OCaml 5.4.1, none
promoted, and no GC function of the runtime reaches 5 samples.

### The generated views

The row `wire.ocaml check/view, accessors` reads the frames through the
views the scanner of `Packet_stack.capture` writes (docs/execution.md,
Generated code): one `check` per frame, which makes every check of the
frame once, then an accessor per field, `ipv4 b len v` answering a check
of `Captured_ipv4` and `Captured_ipv4.tcp` one of `Stack_tcp`, each
testing again the presence of its record and placing it. The loop is
`flow_rows.ml`'s, as the other rows': no unchecked array read, the IPv4
and IPv6 keys in a function each. It is compared below with the scanner's
`fold_step` row, with the linked headers row and with the hand-written
prototype of the same module the design was measured on (its `check` and
`enter` written by hand, with the IPv4 header's own word tests, one
function per frame), not committed.

Counted with `bench/count.sh` on `mawi parse`, on the tree of the commit
that adds this section with the prototype's module and row added for the
count, each row five times, the median of its instructions and of its
cycles per packet, at a load average of 2.9 before and 4.2 after:

| Row, parse only | OCaml | OCaml flambda | OxCaml |
| --- | ---: | ---: | ---: |
| check/view, accessors | 237.1 / 51.6 | 244.0 / 53.0 | 174.3 / 43.1 |
| scanner, `fold_step` | 150.0 / 43.1 | 144.6 / 42.0 | 73.2 / 30.0 |
| `View.check`, linked headers | 222.2 / 48.8 | 250.1 / 52.7 | 197.9 / 48.8 |
| prototype, check + enter, re-read | 230.0 / 50.7 | 233.7 / 52.8 | 213.0 / 50.2 |

`pnet_packet 0.35` takes 66.7 / 22.5 and the Rust scanner 68.7 / 23.0,
counted alike. On OxCaml the views spend 39 instructions and 7 cycles
less than the prototype, and 24 and 6 less than the linked headers: the
check is the scanner's walk on `int64#`, and each accessor's tests read
`int64#` too, a bitfield through its word with one masked test, fields
sharing a 64-bit word from one load of it, a span and the record after it
with one bound test. They still spend 101 instructions and 13 cycles more
than `fold_step`: what remains per IPv4 TCP packet, read in the
disassembly, is the re-read by design (the EtherType again for `ipv4`,
the IPv4 word and protocol again for `tcp`, about 20 instructions), the
answer of each check joined from its failures then tested by `ok` and
`get` (about 4 per record), the IHL tested again not to make the options
negative (2), positions and returned fields tagged at the accessor's
boundary, and the call of the IPv4 or IPv6 function with its spills
around the walk. On OCaml and flambda the views cost 7 and 10 more than
the prototype: the same IHL test, a bitfield word masked to 16 bits and
then by its test, and `ok` and `get` testing the answer twice, which only
flambda2 merges.

Since the one module per codec, an accessor returns its field's carrier,
a `UInt16.t` for a port, which the row reads as `(x :> int)`, and a
`UInt32.t` for an address, read with `UInt32.to_int`. The 16-bit and 8-bit
carriers type the accessor's `int` with their `unsafe_of_int`, the
`%identity` primitive: through `cast` and `UInt16.to_int`, two inlined
functions, OCaml without flambda kept a binding that tags each port and
then untags it to shift it, 1.7 more instructions a packet.

The one module per codec changes no row that reads a generated module:
`bench/count.sh`, five runs of each row, the tree before it (`fc6e0ec4`)
and after it interleaved within each run, median of five, on the ARM64
macOS host at a load average of 15.1 before and 16.1 after. Instructions
/ cycles per item (apps, 100,000 items) or per packet (the MAWI parse);
every row gave the same result on both trees:

| Row | OCaml before | OCaml after | flambda before | flambda after | OxCaml before | OxCaml after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| routing | 56.2 / 16.1 | 56.2 / 14.9 | 56.1 / 15.1 | 56.1 / 15.0 | 44.2 / 13.8 | 44.1 / 13.3 |
| gateway | 762 / 196 | 762 / 193 | 964 / 193 | 964 / 193 | 626 / 194 | 625 / 191 |
| clcw | 46.9 / 7.3 | 46.9 / 6.8 | 46.9 / 7.4 | 47.0 / 7.5 | 42.0 / 6.8 | 42.0 / 6.7 |
| routing_write | 97.1 / 19.6 | 97.1 / 19.4 | 96.2 / 19.5 | 96.1 / 18.8 | 68.1 / 18.3 | 68.1 / 17.8 |
| gateway_write | 782 / 87.4 | 783 / 86.9 | 783 / 79.8 | 783 / 84.5 | 511 / 69.3 | 511 / 61.2 |
| clcw_write | 66.2 / 8.1 | 66.1 / 8.2 | 66.1 / 7.9 | 66.1 / 7.9 | 51.1 / 6.2 | 51.2 / 6.8 |
| MAWI scanner, `fold_step` | 149.8 / 43.4 | 149.9 / 44.0 | 144.8 / 41.4 | 144.6 / 41.2 | 72.9 / 28.8 | 73.3 / 29.5 |
| MAWI check/view, accessors | 237.2 / 52.1 | 237.3 / 51.9 | 244.1 / 53.4 | 244.2 / 53.4 | 174.7 / 45.8 | 174.3 / 43.8 |
| MAWI `View.check`, linked headers | 222.3 / 49.2 | 222.4 / 48.3 | 249.3 / 51.0 | 249.3 / 51.0 | 198.6 / 54.1 | 197.7 / 49.1 |
| MAWI swap, generated `Global` | 188.8 / 43.4 | 188.8 / 44.0 | 209.3 / 46.1 | 209.2 / 45.3 | 188.3 / 44.3 | 188.4 / 44.3 |

The NEON lanes rows, on OxCaml, count 10.4 / 1.0 (clcw) and 7.1 / 1.2
(clcw_write) before and 10.4 / 1.0 and 7.1 / 1.1 after.

### One loop, interpreted or generated (`make bench-flow-swap`)

Code written against a generated module's views runs on its interpreted
twin, the functor Wire_ocaml writes beside it over the codec value.
`bench/rust/flow_swap.ml` is one flow counter written so: its header modules
are those of `swap.ml`, and its one line `module H = Swap.Generated` picks
them; the dune rules build the interpreted rows from the same file with that
line replaced by `module H = Swap.Interpreted`, and the parse-only rows by
making `Flow_sum` the sink. Each side is one line per header,
`module Ipv4 = G_ipv4` or
`module Ipv4 = G_ipv4_codec.Make (struct type r = Headers.ipv4 let codec = Headers.ipv4 end)`,
with the same accessors (`ihl`, `src_addr`, ...) reading the types the codec
declares.

Per frame the loop checks Ethernet, IPv4 (its options skipped by IHL) and
TCP or UDP with the checks of the classifier above, and hands the IPv4
5-tuple (no ports for a fragment) and total length to the table; IPv6, VLAN
tags and every other frame are counted, not parsed, as is an IPv4 packet
failing a check. `smoltcp 0.14, IPv4 flows` makes the same checks in Rust.
The rows run on corpora of their own, `mawi swap` and `mawi swap parse`, and
`make bench-flow` runs them with the others; `make bench-flow-swap` times
and counts them alone, into `_build/bench-flow/swap/`. On the default
packets every row counts 314,998 flows, checksum `1215103043457353650`, and
IPv6 178,454, VLAN 0, other 1,286, IPv4 rejected 1 (parse only: 820,259
keys, `4332859610784037926`).

Measured at commit `4497904b` on the machine above, best CPU ns/packet of
21 passes and `bench/count.sh` per packet: the OxCaml and Rust rows at a
load average of 481 before and 415 after, the OCaml 5.4.1 rows at 331 and
403, when the row `wire.ocaml View.check/view, linked headers, parse only`
counted 220.9 instructions (222.0 above):

| Row | ns | ns, parse | Instructions | Instructions, parse | Cycles | Cycles, parse |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| swap=interpreted, OCaml 5.4.1 | 186.6 | 59.8 | 880.7 | 809.2 | 535.1 | 159.0 |
| swap=generated, OCaml 5.4.1 | 105.0 | 20.9 | 258.4 | 190.2 | 286.9 | 58.6 |
| swap=interpreted, OxCaml | 185.7 | 54.3 | 921.4 | 852.5 | 521.2 | 148.3 |
| swap=generated, OxCaml | 96.2 | 19.7 | 254.2 | 186.0 | 268.3 | 49.3 |
| smoltcp 0.14, IPv4 flows | 34.5 | 9.0 | 121.0 | 56.0 | 93.1 | 24.2 |

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
