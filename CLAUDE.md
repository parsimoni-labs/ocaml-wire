# CLAUDE.md

## Build & Test

```
make build              # dune build
make test               # dune runtest
make test-wasm          # test/wasm under node via wasm_of_ocaml (needs wasm_of_ocaml, node)
make 3d                 # validate all schemas with EverParse (needs everparse-3d.exe)
make prove              # F* proves the scanners proofs/fstar-gen lists (needs EverParse)
make verify             # Z3 checks the generated code against 3d.exe (needs EverParse, z3)
make bench              # EverParse C vs OCaml (needs everparse-3d.exe)
make bench-routing      # APID demux throughput
make bench-gateway      # TM frame reassembly
make bench-clcw         # CLCW polling loop
make bench-rust         # Ethernet/IPv4/TCP/UDP: OCaml, OxCaml, Rust (needs cargo)
make bench-flow         # a MAWI trace through a flow classifier (downloads it once)
make bench-apps-rust    # routing, gateway, CLCW workloads: OCaml, OxCaml, Rust (needs cargo)
make fuzz-rust          # sampled codecs vs the Rust Wire_rust generates (needs rustc)
make prof               # CPU profile with Instruments (open prof.trace)
make memtrace           # allocation hotspots via memtrace
make cppcheck           # static analysis of the hand-written C (skips if cppcheck absent)
make clean              # dune clean
```

`make bench` requires EverParse (`everparse-3d.exe`, or `3d.exe` before
v2026.10, in PATH or `~/.local/everparse/bin/`).
All bench/prof/memtrace targets use `--profile=bench`: the release profile
with `-O3` on every unit, the libraries a benchmark links included
(`dune-workspace`).
The rules that keep tests portable to js_of_ocaml and wasm_of_ocaml
(`make test-wasm`) are in [docs/projection.md](docs/projection.md#narrow-int-targets).

## Project structure

- `lib/` -- core `wire` library: DSL types, Everparse codegen, Codec (parse/encode)
- `lib/3d/` -- `wire.3d` sublibrary: EverParse pipeline (generate .3d, run everparse-3d.exe, generate C tests)
- `lib/asn1/` -- `wire.asn1` sublibrary: everparse-asn1 pipeline (write .asn, run everparse-asn1.exe, generate C tests); the `Wire.Asn1` DSL itself is in `lib/`
- `lib/stubs/` -- `wire.stubs` sublibrary: generate OCaml/C FFI stubs for generated validators
- `lib/ocaml/` -- `wire.ocaml` sublibrary: a fixed-size codec written out as OCaml (or OxCaml)
- `lib/rust/` -- `wire.rust` sublibrary: the same codec written out as dependency-free Rust
- `lib/diff/` -- `wire.diff` sublibrary: differential test infrastructure
- `test/stubs/` -- Wire_stubs test suite (compile + EverParse e2e tests)
- `examples/` -- protocol definitions as Wire DSL examples
  - `examples/space/` -- space protocols (CLCW, SpacePacket, TMFrame, etc.)
  - `examples/net/` -- TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy `byte_slice` demo
- `bench/` -- benchmarks using schemas from `examples/`
  - `bench/demo/` -- field-level codec benchmark (EverParse C validation vs FFI vs OCaml `Codec.get`/`Codec.set`)
  - `bench/clcw/` -- CLCW polling loop, uses `Codec.Word`
  - `bench/routing/` -- APID demux throughput
  - `bench/gateway/` -- TM frame reassembly
  - `bench/apps/` -- the routing, gateway and CLCW workloads in Wire.Codec, generated OCaml/OxCaml, generated Rust and crates
  - `bench/gen_stubs.ml` -- generates C/OCaml stubs for EverParse comparison
  - `bench/bench_lib.ml` -- shared benchmark framework (timing, tables, comparison)
  - `bench/memtrace/` -- allocation profiling
- `fuzz/` -- Alcobar fuzz tests (`fuzz_wire.ml`, `fuzz_param.ml`, `3d/`, `diff/`) covering all DSL combinators
- `test/` -- Alcotest unit tests and differential tests (`test/diff/`)
- `.github/workflows/ci.yml` -- CI workflow

## Feature shippability

A feature ships when a verified parser backend expresses it and its
projection is checked there. Today the backends are EverParse 3D and
everparse-asn1. One wire description projects each codec to the
backend that expresses it:

- a codec 3D can express projects to 3D and is checked there;
- a codec that needs what only everparse-asn1 has (DER tags and
  lengths, OPTIONAL decided by peeking at a tag, SET OF ordering)
  projects to everparse-asn1 and is checked there;
- a codec that nests codecs of both kinds projects each nested codec
  to its own backend, handed over as a byte region that backend's
  validator checks, and each is checked.

A feature that decodes/encodes on the OCaml side but has no checked
verified projection is **not shipped**. The OCaml side and its
projection are part of the same release-shaped unit; partial work goes
in a branch but does not merge until both halves land.

A shape no backend can express is rejected when it is built, never
half-supported in OCaml: the combinator raises `Invalid_argument` with a
message naming the shape and the reason, and every rejection has a unit
test asserting it. The reject predicate mirrors the handler's supported
set (`Field.is_repeat_case_body` mirrors `Codec.read_elem`). Before
extending an element handler to a new shape, check that the shape
projects in that position (as a repeat element, not only standalone).

The EverParse limits that decide what projects, and so what must be
rejected, are listed in [docs/projection.md](docs/projection.md#what-3d-cannot-express)
for 3D and in [docs/asn1.md](docs/asn1.md#refused-when-built) for
everparse-asn1. The `Wire.Asn1` combinators mirror everparse-asn1's
checks (recursion, an implicit tag on a CHOICE, tags a decoder cannot
tell apart, DEFAULT on a constructed type) as the binary combinators
mirror 3D's. The DER rules and the handover between backends are in
[docs/asn1.md](docs/asn1.md).

### Tagged unions

`variants` and `casetype` stay separate. `variants` maps a discriminator
to a named OCaml value, numbered by list position; `casetype` selects a
different body layout per tag, with explicit indices because protocol
tags are assigned and sparse (DHCP option 53, TCP option kind 8). A
single combinator would make one of the two verbose or wrong.

## Untrusted input

`wire` parses input from attackers, so no code assumes the input is
well formed. Bad input makes a decoder return `Error`, never raise
`Invalid_argument`, and a value used in a constraint is never silently
truncated. A buffer overrun, an out-of-bounds read or a wrong answer is
fixed as soon as it is found, even when the fix needs an API change.

## Scalar type names

A numbered scalar type name states the field's width on the wire, never
the OCaml carrier: `uint8`, `uint16`, `uint32`, `uint64` are 1, 2, 4 and
8 bytes. `uint n` is n bytes (1 to 7) and `bits ~width:n` is n
bits. Widths without a power-of-two size have no alias and go through
`uint`; a new name is added only for a width a real protocol
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
1. `Everparse.project codec` -- project a codec to a 3D schema (with extern callbacks for field extraction)
2. `Everparse.write ~outdir [ schema ]` -- write `.3d` files
3. `Wire_3d.run ~outdir [ schema ]` -- invoke EverParse to produce C parsers
4. `Wire_stubs.generate ~schema_dir ~outdir [ C codec ]` -- generate OCaml FFI bindings

How the standalone C archive builds in every dune context, and how many
codecs share one library, is in [docs/projection.md](docs/projection.md#generated-c).

## Benchmarking principles

The **only** goal of benchmarks is to compare these three tiers -- all derived
from the same Wire DSL definition:

1. **Pure OCaml** -- `Wire.Codec`'s reads and writes (zero-copy field access)
2. **EverParse C** -- EverParse-generated verified C validator in a C loop
3. **OCaml->C FFI** -- OCaml calling the EverParse-generated C validator

**Never use hand-written C parsers.** Field extraction in C benchmarks must go
through EverParse-generated validators: stack-allocate the schema's default
visitor struct (`<Name>Fields` from `<Name>_Fields.h`), pass `(WIRECTX *)&ctx` to
the validator, read named struct members. No manual bitfield manipulation, no
index arrays, no hand-rolled `WIRECTX` layouts. C benchmark loops may contain
application logic (routing, anomaly counting, reassembly) but all field
access must use the default visitor (or a custom visitor made by copy-and-trim of
the shipped `<Name>_Fields.c`).

**Benchmark loops are what a user of each library writes.** Every
optimisation lives in the code generator or the runtime, never in the
harness: plain checked indexing, no `unsafe_get`, masks, hand unboxing,
inlining attributes or intrinsics in a loop, and SIMD only through an API
the generator emits. The rows of one workload in one language share one
loop shape and differ only in the API they call; a crate row calls the
crate the way its documentation shows. A harness trick is a defect: it
measures the trick, not the library, and its win belongs in the generator.

**Demo bench** (`bench/demo/`) covers every Wire type at the field level.

**Application benchmarks** (`bench/clcw/`, `bench/routing/`, `bench/gateway/`)
add real workloads on top of field access. The C loop calls the EverParse
validator for field extraction, then runs application logic. The OCaml loop
does the same work as a user would, in the terms of docs/api.md: the
interpreted view loop for the `Wire.Codec` rows (`Codec.View.check`, `is_ok`
and `get` once per header, `Codec.View.read` per field and `Codec.View.set`
per field written in place), the same loop over the generated module for the
generated rows (`check`, `is_ok`, `get`, an accessor per field, `set_<field>`
and `write`), and a fold with its visitor `~f` where the workload is one pass
over many records.

**Cross-language benchmark** (`bench/rust/`, `make bench-rust`) is the one
exception to the three tiers: it measures the code generators against
established parsers of another language. Ethernet, IPv4 and TCP/UDP headers
are read from four packet corpora by `Wire.Codec`, by the OCaml `Wire_ocaml`
generates (for the OCaml target on OCaml, for the OxCaml target on OxCaml), by
the Rust `Wire_rust` generates, and by released Rust crates, `etherparse`,
`smoltcp::wire` and `pnet_packet`, as external reference implementations.
Every implementation does the checks `etherparse` does (bench/README.md lists
them and each row's API), reads the same fields and folds them into one
checksum, which must agree; the corpus holds packets failing each check. The
stack rows take dispatch, bounds and fragment rules from one codec,
`Packet_stack.codec` in `examples/net`, so the generated scanner inherits
them from the DSL, in OCaml and in Rust; the `linked headers` rows read the
fixed headers of `bench/rust/headers.ml` and link them in the harness. Keep
the harnesses symmetric: a change to how one side times or folds goes into
the other too, and never weaken a crate's row. The Rust side is a cargo
project outside the dune build, so `dune build` never needs the network.

**Cross-language application benchmarks** (`bench/apps/`, `make
bench-apps-rust`) run the three application workloads the same way: the
OCaml workload of each (`<app>_workload.ml` in its directory, the one its
EverParse comparison times) on OCaml and on OxCaml, and the same application
logic over the modules `Wire_ocaml` (OCaml target on OCaml, OxCaml target on
OxCaml) and `Wire_rust` generate from the same codecs, on the inputs the
OCaml generators write; dune rules regenerate the OCaml modules. Each
workload has a reading side and a writing side (`bench.exe write`,
`wire-bench-apps write`), whose result is the hash of the buffer it writes.
Each row is the loop a user of its library writes
(`bench/apps/loops.ml` for OCaml and OxCaml, `cargo/src/main.rs` for Rust),
and every row must give the same result. A
released crate gets a row only where one reads and writes the format
(`spacepackets`, `ccsds_primary_header` and `gaia-ccsds-c2a` for the space
packet header, `chronus-gateway` for the TM frame header, `gaia-ccsds-c2a`
for the CLCW); a hand-written Rust parser never stands in for one. Timing, the report and
the cargo layout are those of `bench/rust` (`Bench_lib.measure` and
`bench/rust/cargo/src/timing.rs`, shared by both).

## Style

- Use `Fmt.pf` / `Fmt.pr` instead of `Printf.fprintf` / `Printf.printf`
- Use `Alcotest.failf` instead of `Alcotest.fail (Fmt.str ...)`
- Avoid `Fuzz_gen.` qualified names in fuzz tests -- use `open Fuzz_gen`
- No `make_` prefix on data constructor functions (e.g., `minimal_data` not `make_minimal_data`)
