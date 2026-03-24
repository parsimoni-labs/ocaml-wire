# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make bench              # EverParse C vs OCaml (needs 3d.exe)
make bench-routing      # APID demux throughput (Wire OCaml vs hand-written C)
make bench-gateway      # TM frame reassembly (Wire OCaml vs hand-written C)
make bench-clcw         # CLCW polling loop (Wire OCaml vs hand-written C)
make prof               # CPU profile with Instruments (open prof.trace)
make memtrace           # allocation hotspots via memtrace
make clean              # dune clean
```

`make bench` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).
All bench/prof/memtrace targets use `--profile=release`.

## Project structure

- `lib/` -- core `wire` library: DSL types, 3D codegen, Codec (parse/encode)
- `lib/3d/` -- `wire.3d` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/stubs/` -- `wire.stubs` sublibrary: generate OCaml/C FFI stubs for generated validators
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `lib/test/stubs/` -- Wire_stubs test suite (compile + EverParse e2e tests)
- `examples/` -- protocol definitions as Wire DSL examples
  - `examples/space/` -- space protocols (CLCW, SpacePacket, TMFrame, etc.)
  - `examples/net/` -- TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy `byte_slice` demo
- `bench/` -- benchmarks using schemas from `examples/`
  - `bench/demo/` -- field-level codec benchmark (EverParse C validation vs FFI vs OCaml `Codec.get`/`Codec.set`)
  - `bench/clcw/` -- CLCW polling loop (Wire OCaml vs hand-written C), uses `Codec.bitfield`/`load_word`/`extract`
  - `bench/routing/` -- APID demux throughput (Wire OCaml vs hand-written C)
  - `bench/gateway/` -- TM frame reassembly (Wire OCaml vs hand-written C)
  - `bench/gen_stubs.ml` -- generates C/OCaml stubs for EverParse comparison
  - `bench/bench_lib.ml` -- shared benchmark framework (timing, tables, comparison)
  - `bench/memtrace/` -- allocation profiling
- `fuzz/` -- Crowbar fuzz tests (`fuzz_wire.ml`, `fuzz_c.ml`, `fuzz_param.ml`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)
- `.github/workflows/ci.yml` -- CI workflow

## Code generation pipeline

All C code generation flows through `Wire_3d` (no duplication):
1. `C.schema codec` -- project a codec to a 3D schema (with extern callbacks for field extraction)
2. `C.generate ~outdir [ schema ]` -- write `.3d` files; `C.Raw.to_3d` / `C.Raw.to_3d_file` for low-level rendering
3. `Wire_3d.generate` -- invoke EverParse to produce C parsers
4. `Wire_stubs.to_c_stubs` / `Wire_stubs.to_ml_stubs` -- generate OCaml FFI bindings calling `Validate` directly (not through EverParse Wrapper)

## Benchmarking principles

**Demo bench** (`bench/demo/`) compares field-level access, all derived from the same Wire DSL definitions:

1. **EverParse C** -- generated verified C validator from `.3d` files, timed in a pure C loop (validation only, different operation from OCaml `Codec.get`).
2. **OCaml→C FFI** -- OCaml calling the EverParse-generated C validator via generated stubs.
3. **Pure OCaml** -- `Wire.Codec.get` / `Wire.Codec.set` (zero-copy field access, no record allocation).

Every Wire type is covered: uint8, uint16be, uint32be, uint64be, bf_uint8, bf_uint16be, bf_uint32be, bool(bf1), map, cases, enum, where, and nested protocol traversals via byte_slice.

**Application benchmarks** compare Wire OCaml against hand-written C doing the same work:

- `bench/clcw/` -- CLCW polling loop: batch bitfield reads via `Codec.bitfield`/`load_word`/`extract` vs hand-written C bitfield extraction (`clcw_c.c`)
- `bench/routing/` -- APID demux: Wire `Codec.get` for packet routing vs hand-written C (`routing_c.c`)
- `bench/gateway/` -- TM frame reassembly: Wire codec vs hand-written C (`gateway_c.c`)

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Cu.` qualified names for Crowbar_util -- use `open Crowbar_util`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
