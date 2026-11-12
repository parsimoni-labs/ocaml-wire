# Wire Benchmarks

All benchmarks derive from the same Wire DSL definition. The field-level
demo benchmark (`bench/demo/`) compares three tiers:

1. **Pure OCaml** -- `Wire.Codec.get/set` (zero-copy field access)
2. **EverParse C** -- EverParse-generated verified C validator in a tight C loop
3. **OCaml->C FFI** -- calling the EverParse C validator from OCaml

The application benchmarks (`bench/clcw/`, `bench/routing/`, `bench/gateway/`)
compare two tiers: Pure OCaml vs EverParse C.

Metrics: ns/op, allocation (words), ratio vs C, throughput.

## Rules

- **No hand-written C parsers.** All field extraction in C must go through
  EverParse-generated validators using the default `<Name>_Fields` plug:
  stack-allocate `<Name>Fields`, pass `(WIRECTX *)&ctx` to the validator,
  read named struct members. No manual bitfield manipulation, no index
  arrays, no hand-rolled `WIRECTX`.
- Both tiers must operate on the same data and produce the same results.

## Field-level codec (`bench/demo/`)

Per-field read/write performance across all Wire DSL constructs:

- Integer types: uint8, uint16be, uint32be, uint64be
- Bitfields: bf_uint8, bf_uint16be, bf_uint32be
- Type combinators: `bool`, `map`, `cases`, `enum`, `where`
- Nested protocols: 3-layer Ethernet->IPv4->TCP traversal
- Real protocols: CLCW (CCSDS), TCP/IP

```
make bench                # requires EverParse (3d.exe in PATH)
```

## Application benchmarks

Throughput on realistic workloads. The C loop calls the EverParse validator for
field extraction, then runs application logic. The OCaml loop does the same
work with `Codec.get`/`Codec.set`.

| Target | Scenario | Protocols |
|---|---|---|
| `make bench-routing` | APID demux (10M variable-size packets) | Space Packet |
| `make bench-gateway` | TM frame reassembly (1M frames) | TMFrame + Space Packet |
| `make bench-clcw` | COP-1 CLCW polling (10M words) | CLCW |

## Cross-language headers (`bench/rust/`)

The code generators against established parsers: Ethernet, IPv4 and TCP or
UDP headers, read from one corpus of 64-byte packets by

- `Wire.Codec` (`valid`, then a proof-carrying `read` per field), on OCaml 5
  and on OxCaml;
- the OCaml `Wire_ocaml` generates (`validate`, then a getter per field): for
  the default target on OCaml 5, and with ``~target:`OxCaml`` on OxCaml;
- the Rust `Wire_rust` generates (`Valid::at`, then a read per field);
- `etherparse` (`SlicedPacket::from_ethernet`) and `smoltcp::wire`
  (`new_checked` per layer), two widely used Rust crates.

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

The codecs (`headers.ml`) make the checks on a header's own fields; the
bounds against the packet and the fragment test are a few lines in each
harness, the same in OCaml and Rust (`smoltcp` checks the bounds itself; its
row adds the version and fragment tests). A packet counts only when every
header it has passes, as with `etherparse`. No row copies a byte: the crates
return sub-slices, the other rows the same bounds as offsets. Every row folds
the same fields into a checksum, and the report fails if they disagree.

Three packets in four of the corpus are well formed: an IPv4 header of 20
bytes, then TCP without options or UDP, filling the slot. The others are
spread evenly over these cases, so each check meets packets that pass it and
packets that fail it, and the rows agree only if each makes every check:

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

```
make bench-rust           # needs cargo; fetches the two crates the first time
```

`OXCAML_SWITCH` names the opam switch the OxCaml run uses (default
`5.4.0-ox9`; skipped when absent), and `BENCH_RUST_PACKETS` and
`BENCH_RUST_RUNS` size the corpus and the number of timed passes. The report
gives the best and the median pass: on a loaded machine the best is the
steadier measure of the code's own cost.

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
