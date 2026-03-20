# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make bench              # EverParse C vs OCaml (needs 3d.exe)
make bench-routing      # APID demux throughput (wire vs hand-written)
make bench-gateway      # TM frame reassembly (wire vs hand-written)
make bench-clcw         # CLCW polling loop (wire vs hand-written)
make prof               # CPU profile with Instruments (open prof.trace)
make memtrace           # allocation hotspots via memtrace
make clean              # dune clean
```

`make bench` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).
All bench/prof/memtrace targets use `--profile=release`.

## Project structure

- `lib/` -- core `wire` library: DSL types, 3D codegen, Codec (parse/encode), FFI stub generators
- `lib/3d/` -- `wire.3d` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `examples/` -- protocol definitions as Wire DSL examples
  - `examples/space/` -- space protocols (CLCW, SpacePacket, TMFrame, etc.)
  - `examples/net/` -- TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy `byte_slice` demo
- `bench/` -- benchmarks using schemas from `examples/`
  - `bench/bench.ml` -- field-level read/write: EverParse C vs OCaml FFI vs pure OCaml get/set
  - `bench/gen_stubs.ml` -- generates C/OCaml stubs for EverParse comparison
  - `bench/demo/` -- field-level codec benchmark (EverParse C vs FFI vs OCaml)
  - `bench/bench_lib.ml` -- shared benchmark framework (timing, tables, 3-tier comparison)
  - `bench/memtrace.ml` -- allocation profiling
- `fuzz/` -- Crowbar fuzz tests (`fuzz_wire.ml`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)

## Code generation pipeline

All C code generation flows through `Wire_3d` (no duplication):
1. `Wire.to_3d` / `Wire.to_3d_file` -- generate .3d files from Wire DSL
2. `Wire_3d.run_everparse` -- invoke EverParse to produce C parsers
3. `Wire.to_c_stubs` / `Wire.to_ml_stubs` -- generate OCaml FFI bindings calling `Validate` directly (not through EverParse Wrapper)

## Benchmarking principles

Benchmarks compare field-level access, all derived from the same Wire DSL definitions:

1. **EverParse C** -- generated verified C validator from `.3d` files, timed in a pure C loop.
2. **OCaml→C FFI** -- OCaml calling the EverParse-generated C validator via generated stubs.
3. **Pure OCaml** -- `Wire.Codec.get` / `Wire.Codec.set` (zero-copy field access, no record allocation).

Every Wire type is covered: uint8, uint16be, uint32be, uint64be, bf_uint8, bf_uint16be, bf_uint32be, bool(bf1), map, cases, enum, where, and nested protocol traversals via byte_slice.

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Cu.` qualified names for Crowbar_util -- use `open Crowbar_util`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
