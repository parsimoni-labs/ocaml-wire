# ocaml-wire

Binary wire format DSL with EverParse 3D output.

## Overview

Wire is a GADT-based OCaml DSL for describing binary wire formats.
Define your format once, then:

- **Read and write fields in-place** via `Codec.get` / `Codec.set` — zero-copy,
  zero-allocation for immediate types (int, bool)
- **Traverse nested protocols** via `Codec.sub` — navigate Ethernet → IPv4 → TCP
  without intermediate allocations or buffer copies
- **Emit EverParse `.3d` files** for verified C parser generation
- **Generate C/OCaml FFI stubs** for differential testing between OCaml and C

Wire covers both parsing *and* serialization from a single schema definition.
EverParse only generates verified parsers/validators — Wire's `Codec.set`
complements this with in-place field writes (bounds-checked, with automatic
read-modify-write for bitfields).

## Features

- **Integer types** — `uint8`, `uint16`, `uint16be`, `uint32`, `uint32be`,
  `uint64`, `uint64be` (32-bit integers are unboxed on 64-bit platforms)
- **Bitfields** — `bits ~width:n bf_uint8/bf_uint16be/bf_uint32be` to extract
  bit ranges from integer bases
- **Bool** — `bool (bits ~width:1 bf)` maps single-bit fields to `true`/`false`
- **Byte slices** — `byte_slice ~size:(int n)` for zero-copy sub-protocol access
- **Enumerations** — named integer constants with validation
- **Structs** — records with dependent fields and constraints
- **Tagged unions** — `casetype` with tag-based dispatch
- **Arrays** — fixed-count, byte-sized, and variable-length
- **Constraints** — `where` clauses with arithmetic and logical expressions
- **Parameterised types** — reusable type templates
- **3D code generation** — emit `.3d` files compatible with EverParse

## Installation

```
opam install wire
```

Requires OCaml >= 5.1.

## Usage

### Defining a codec

Define a typed record codec with zero-copy field access:

```ocaml
open Wire

type packet = { version : int; flags : int; length : int }

let f_version = Codec.field "Version" (bits ~width:4 bf_uint8) (fun p -> p.version)
let f_flags   = Codec.field "Flags"   (bits ~width:4 bf_uint8) (fun p -> p.flags)
let f_length  = Codec.field "Length"   uint16be                 (fun p -> p.length)

let codec =
  let open Codec in
  record "Packet" (fun version flags length -> { version; flags; length })
  |+ f_version |+ f_flags |+ f_length |> seal
```

### Zero-copy field access

Read and write individual fields directly in a buffer — no record allocation,
no copies:

```ocaml
(* Read a field *)
let v = Codec.get codec f_version buf 0

(* Write a field (read-modify-write for bitfields) *)
let () = Codec.set codec f_length buf 0 1024

(* Full record decode/encode when needed *)
let pkt = Codec.decode codec buf 0
let () = Codec.encode codec pkt buf 0
```

### Nested protocol traversal

Use `byte_slice` fields and `Codec.sub` to navigate layered protocols
in a single buffer without allocation:

```ocaml
(* Ethernet → IPv4 → TCP, all in one buffer *)
let ip_off  = Codec.sub ethernet_codec f_eth_payload buf 0 in
let tcp_off = Codec.sub ipv4_codec f_ip_payload buf ip_off in
let dst_port = Codec.get tcp_codec f_tcp_dst_port buf tcp_off

(* In-place mutation through layers *)
let () = Codec.set tcp_codec f_tcp_dst_port buf tcp_off 8080
```

### Generating EverParse 3D

The same codec definition produces `.3d` files for verified C parser generation:

```ocaml
let struct_ = Codec.to_struct codec
let module_ = Wire.module_ "Protocol" [ Wire.typedef ~entrypoint:true struct_ ]
let () = print_string (Wire.to_3d module_)
```

Output:

```
module Protocol

typedef struct Packet {
  UINT8 Version:4;
  UINT8 Flags:4;
  UINT16BE Length;
} Packet;
```

### Generating FFI stubs

Generate C and OCaml stubs for calling EverParse-generated validators from OCaml:

```ocaml
let () = print_string (Wire.to_c_stubs [struct_])
let () = print_string (Wire.to_ml_stubs [struct_])
```

## Real-world examples

### CCSDS Space Packet (6 bytes, bitfields across uint16be)

```ocaml
let packet_codec =
  let open Codec in
  record "SpacePacket"
    (fun version type_ sec_hdr apid seq_flags seq_count data_len -> ...)
  |+ field "Version"    (bits ~width:3  bf_uint16be) (fun p -> p.sp_version)
  |+ field "Type"       (bits ~width:1  bf_uint16be) (fun p -> p.sp_type)
  |+ field "SecHdrFlag" (bits ~width:1  bf_uint16be) (fun p -> p.sp_sec_hdr)
  |+ field "APID"       (bits ~width:11 bf_uint16be) (fun p -> p.sp_apid)
  |+ field "SeqFlags"   (bits ~width:2  bf_uint16be) (fun p -> p.sp_seq_flags)
  |+ field "SeqCount"   (bits ~width:14 bf_uint16be) (fun p -> p.sp_seq_count)
  |+ field "DataLength"  uint16be                    (fun p -> p.sp_data_len)
  |> seal
```

### TCP header (20 bytes, bool bitfields)

```ocaml
let f_tcp_src_port = Codec.field "SrcPort" uint16be (fun t -> t.tcp_src_port)
let f_tcp_dst_port = Codec.field "DstPort" uint16be (fun t -> t.tcp_dst_port)
let f_tcp_syn = Codec.field "SYN" (bool (bits ~width:1 bf_uint16be)) (fun t -> t.tcp_syn)
let f_tcp_ack = Codec.field "ACK" (bool (bits ~width:1 bf_uint16be)) (fun t -> t.tcp_ack)

let tcp_codec =
  let open Codec in
  record "TCP" (fun src dst seq ack_num ... -> ...)
  |+ f_tcp_src_port |+ f_tcp_dst_port
  |+ field "SeqNum" uint32be (fun t -> t.tcp_seq)
  (* ... flags as individual bool bitfields ... *)
  |+ f_tcp_syn |+ f_tcp_ack
  (* ... *)
  |> seal
```

## Architecture

```
                        ┌──────────────────┐
                        │  Wire OCaml DSL  │
                        └────────┬─────────┘
                                 │
              ┌──────────────────┼──────────────────┐
              │                  │                   │
              ▼                  ▼                   ▼
     ┌─────────────────┐ ┌────────────┐ ┌───────────────────┐
     │ Codec.get / set │ │  to_3d     │ │  to_c/ml_stubs    │
     │ Codec.sub       │ │            │ │                   │
     │ Codec.decode    │ │            │ │                   │
     │ Codec.encode    │ │            │ │                   │
     └─────────────────┘ └──────┬─────┘ └───────────────────┘
      Zero-copy R/W in           │        FFI bindings for
      OCaml (read+write)         │        differential testing
                                 ▼
                        ┌─────────────────┐
                        │   EverParse 3D  │
                        │   (external)    │
                        └─────────────────┘
                         Verified C parsers
                         (read/validate only)
```

## Benchmarks

Benchmarks compare field-level access performance across three approaches,
all derived from the same Wire DSL definitions:

1. **EverParse C** — generated verified C validator in a tight C loop (baseline)
2. **OCaml→C FFI** — OCaml calling the EverParse C validator via generated stubs
3. **Pure OCaml** — `Codec.get` / `Codec.set` (zero-copy, no record allocation)

EverParse only generates parsers/validators, so write benchmarks measure
`Codec.set` alone — there is no C-side equivalent to compare against. Parsing
untrusted input is where the security-critical complexity lives; writing a
known-good value at a compile-time-known offset is inherently simpler.

```
make bench    # requires EverParse (3d.exe in PATH or ~/.local/everparse/bin/)
```

## Development

```
make build      # dune build
make test       # dune runtest
make bench      # BUILD_EVERPARSE=1 dune exec bench/bench.exe
make memtrace   # MEMTRACE=trace.ctf dune exec bench/memtrace.exe && memtrace_hotspots trace.ctf
make clean      # dune clean
```

## Project structure

| Directory | Description |
|-----------|-------------|
| `lib/` | Core `wire` library: DSL types, 3D codegen, Codec, FFI stub generators |
| `lib/c/` | `wire.c` sublibrary: EverParse pipeline (generate .3d, run 3d.exe) |
| `examples/space/` | CCSDS space protocols (SpacePacket, CLCW, TMFrame) |
| `examples/net/` | TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy demo |
| `bench/` | Field-level read/write benchmarks: EverParse C vs FFI vs pure OCaml |
| `fuzz/` | Crowbar fuzz tests covering all DSL combinators |
| `test/` | Alcotest unit tests and differential tests |

## Dependencies

| Library | Purpose |
|---------|---------|
| [bytesrw](https://erratique.ch/software/bytesrw) | Byte stream reading and writing |
| [fmt](https://erratique.ch/software/fmt) | Pretty printing |

## References

- [EverParse](https://project-everest.github.io/everparse/) — verified parser
  generator from Project Everest
- [3D Language Reference](https://project-everest.github.io/everparse/3d-lang.html)
  — EverParse DSL specification

## Licence

ISC
