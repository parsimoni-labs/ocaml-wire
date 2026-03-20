# Wire Benchmarks

All benchmarks derive from the same Wire DSL definitions. Every benchmark
compares three tiers:

1. **EverParse C** -- generated verified C validator in a tight C loop
2. **OCaml->C FFI** -- calling the EverParse C validator from OCaml
3. **Pure OCaml** -- `Wire.Codec.get/set` (zero-copy field access)

Metrics: ns/op, allocation (words), ratio vs C, GB/s.

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

## Scenario benchmarks

Throughput on realistic workloads (Wire OCaml vs hand-written C):

| Target | Scenario | Protocols |
|---|---|---|
| `make bench-routing` | APID demux (10M variable-size packets) | Space Packet |
| `make bench-gateway` | TM frame reassembly (1M frames) | TMFrame + Space Packet |
| `make bench-clcw` | COP-1 CLCW polling (10M words) | CLCW |

## Profiling

```
make prof                 # CPU profile with Instruments (macOS)
make memtrace             # allocation hotspots via memtrace
```

## EverParse pipeline

`make bench` triggers the EverParse code generation pipeline:

1. `gen_stubs.exe` generates `.3d` files from Wire DSL definitions
2. EverParse (`3d.exe`) compiles `.3d` to verified C validators
3. `Wire_stubs` generates C/OCaml FFI stubs to wrap the validators for benchmarking

All schemas (demo, space, net) flow through `Wire.C` -> `Wire_3d` -> `Wire_stubs`.
