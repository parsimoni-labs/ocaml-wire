# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make bench              # EverParse C vs OCaml (needs 3d.exe)
make bench-routing      # APID demux throughput
make bench-gateway      # TM frame reassembly
make bench-clcw         # CLCW polling loop
make prof               # CPU profile with Instruments (open prof.trace)
make memtrace           # allocation hotspots via memtrace
make clean              # dune clean
```

`make bench` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).
All bench/prof/memtrace targets use `--profile=release`.

## Project structure

- `lib/` -- core `wire` library: DSL types, Everparse codegen, Codec (parse/encode)
- `lib/3d/` -- `wire.3d` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/stubs/` -- `wire.stubs` sublibrary: generate OCaml/C FFI stubs for generated validators
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `lib/test/stubs/` -- Wire_stubs test suite (compile + EverParse e2e tests)
- `examples/` -- protocol definitions as Wire DSL examples
  - `examples/space/` -- space protocols (CLCW, SpacePacket, TMFrame, etc.)
  - `examples/net/` -- TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy `byte_slice` demo
- `bench/` -- benchmarks using schemas from `examples/`
  - `bench/demo/` -- field-level codec benchmark (EverParse C validation vs FFI vs OCaml `Codec.get`/`Codec.set`)
  - `bench/clcw/` -- CLCW polling loop, uses `Codec.bitfield`/`load_word`/`extract`
  - `bench/routing/` -- APID demux throughput
  - `bench/gateway/` -- TM frame reassembly
  - `bench/gen_stubs.ml` -- generates C/OCaml stubs for EverParse comparison
  - `bench/bench_lib.ml` -- shared benchmark framework (timing, tables, comparison)
  - `bench/memtrace/` -- allocation profiling
- `fuzz/` -- Crowbar fuzz tests (`fuzz_wire.ml`, `fuzz_c.ml`, `fuzz_param.ml`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)
- `.github/workflows/ci.yml` -- CI workflow

## Code generation pipeline

All C code generation flows through `Wire.Everparse` (no duplication):
1. `Everparse.schema codec` -- project a codec to a 3D schema (with extern callbacks for field extraction)
2. `Everparse.write_3d ~outdir [ schema ]` -- write `.3d` files
3. `Wire_3d.run ~outdir [ schema ]` -- invoke EverParse to produce C parsers
4. `Wire_stubs.generate ~schema_dir ~outdir [ C codec ]` -- generate OCaml FFI bindings

## Benchmarking principles

The **only** goal of benchmarks is to compare these three tiers — all derived
from the same Wire DSL definition:

1. **Pure OCaml** -- `Wire.Codec.get` / `Wire.Codec.set` (zero-copy field access)
2. **EverParse C** -- EverParse-generated verified C validator in a C loop
3. **OCaml→C FFI** -- OCaml calling the EverParse-generated C validator

**Never use hand-written C parsers.** Field extraction in C benchmarks must go
through EverParse-generated validators (output types + WireSet callbacks), not
manual bitfield manipulation. C benchmark loops may contain application logic
(routing, anomaly counting, reassembly) but all field access must use the
generated EverParse API.

**Demo bench** (`bench/demo/`) covers every Wire type at the field level.

**Application benchmarks** (`bench/clcw/`, `bench/routing/`, `bench/gateway/`)
add real workloads on top of field access. The C loop calls the EverParse
validator for field extraction, then runs application logic. The OCaml loop
does the same work with `Codec.get`/`Codec.set`.

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Cu.` qualified names for Crowbar_util -- use `open Crowbar_util`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
