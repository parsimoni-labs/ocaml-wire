# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make test-wasm          # test/wasm under node via wasm_of_ocaml (needs wasm_of_ocaml, node)
make 3d                 # validate all schemas with 3d.exe (needs 3d.exe)
make bench              # EverParse C vs OCaml (needs 3d.exe)
make bench-routing      # APID demux throughput
make bench-gateway      # TM frame reassembly
make bench-clcw         # CLCW polling loop
make prof               # CPU profile with Instruments (open prof.trace)
make memtrace           # allocation hotspots via memtrace
make cppcheck           # static analysis of the hand-written C (skips if cppcheck absent)
make clean              # dune clean
```

`make bench` requires EverParse (`3d.exe` in PATH or `~/.local/everparse/bin/`).
All bench/prof/memtrace targets use `--profile=release`.
The rules that keep tests portable to js_of_ocaml and wasm_of_ocaml
(`make test-wasm`) are in [docs/projection.md](docs/projection.md#narrow-int-targets).

## Project structure

- `lib/` -- core `wire` library: DSL types, Everparse codegen, Codec (parse/encode)
- `lib/3d/` -- `wire.3d` sublibrary: EverParse pipeline (generate .3d, run 3d.exe, generate C tests)
- `lib/stubs/` -- `wire.stubs` sublibrary: generate OCaml/C FFI stubs for generated validators
- `lib/diff/` and `lib/diff-gen/` -- differential test infrastructure
- `test/stubs/` -- Wire_stubs test suite (compile + EverParse e2e tests)
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

## Feature shippability

Every feature in `ocaml-wire` must project cleanly to EverParse 3D. A
feature that decodes/encodes on the OCaml side but has no clean 3D
projection is **not shipped**. The OCaml side and the 3D side are part
of the same release-shaped unit; partial work goes in a branch but
does not merge until both halves land.

A shape that cannot project is rejected when it is built, never
half-supported in OCaml: the combinator raises `Invalid_argument` with a
message naming the shape and the reason, and every rejection has a unit
test asserting it. The reject predicate mirrors the handler's supported
set (`Field.is_repeat_case_body` mirrors `Codec.read_elem`). Before
extending an element handler to a new shape, check that the shape
projects in that position (as a repeat element, not only standalone).

The EverParse limits that decide what projects, and so what must be
rejected, are listed in [docs/projection.md](docs/projection.md#the-3d-ceiling).

### Tagged unions

`variants` and `casetype` stay separate. `variants` maps a discriminator
to a named OCaml value, numbered by list position; `casetype` selects a
different body layout per tag, with explicit indices because protocol
tags are assigned and sparse (DHCP option 53, TCP option kind 8). A
single combinator would make one of the two verbose or wrong.

## Security posture

`wire` parses adversarial input. No code path assumes input is
well-behaved: a data-dependent failure returns `Error`, never
`invalid_arg`, and no value that feeds constraint evaluation is silently
truncated. A confirmed buffer corruption, out-of-bounds access or silent
wrong answer is fixed when found; an API design fork may need a choice,
but every option on the table fixes the defect.

## Scalar type names

A numbered scalar type name states the field's width on the wire, never
the OCaml carrier: `uint8`, `uint16`, `uint32`, `uint64` are 1, 2, 4 and
8 bytes. `uint ~size:n` is n bytes (1 to 7) and `bits ~width:n` is n
bits. Widths without a power-of-two size have no alias and go through
`uint ~size`; a new name is added only for a width a real protocol
needs, and never by renaming an existing type. A carrier narrower than
its field is a carrier defect, fixed by changing the carrier.

## Tests and commits

- Cram tests never pin compiler diagnostic text, whose quoting differs
  between OCaml patch releases (opam-repo-ci uses an older image than
  this repo's CI). Grep the stable part and normalise:
  `... 2>&1 | grep 'Unbound value' | tr -d '"'`.
- The pre-commit hook checks the working-tree copy of each staged
  `.ml`/`.mli`, not the staged content, so a file with unstaged edits is
  judged on those edits: split two changes to one file by sequencing the
  working tree, not with `git apply --cached`. It merlints every touched
  file in full, so pre-existing issues in a touched file are fixed in
  the same branch.

## Code generation pipeline

All C code generation flows through `Wire.Everparse` (no duplication):
1. `Everparse.schema codec` -- project a codec to a 3D schema (with extern callbacks for field extraction)
2. `Everparse.write_3d ~outdir [ schema ]` -- write `.3d` files
3. `Wire_3d.run ~outdir [ schema ]` -- invoke EverParse to produce C parsers
4. `Wire_stubs.generate ~schema_dir ~outdir [ C codec ]` -- generate OCaml FFI bindings

How the standalone C archive builds in every dune context, and how many
codecs share one library, is in [docs/projection.md](docs/projection.md#generated-c).

## Benchmarking principles

The **only** goal of benchmarks is to compare these three tiers -- all derived
from the same Wire DSL definition:

1. **Pure OCaml** -- `Wire.Codec.get` / `Wire.Codec.set` (zero-copy field access)
2. **EverParse C** -- EverParse-generated verified C validator in a C loop
3. **OCaml->C FFI** -- OCaml calling the EverParse-generated C validator

**Never use hand-written C parsers.** Field extraction in C benchmarks must go
through EverParse-generated validators: stack-allocate the schema's default
plug struct (`<Name>Fields` from `<Name>_Fields.h`), pass `(WIRECTX *)&ctx` to
the validator, read named struct members. No manual bitfield manipulation, no
index arrays, no hand-rolled `WIRECTX` layouts. C benchmark loops may contain
application logic (routing, anomaly counting, reassembly) but all field
access must use the default plug (or a custom plug made by copy-and-trim of
the shipped `<Name>_Fields.c`).

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
