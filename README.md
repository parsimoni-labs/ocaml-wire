# ocaml-wire

Binary wire format DSL with EverParse 3D output.

## Overview

Wire is a GADT-based OCaml DSL for describing binary wire formats.
Define your format once, then:

- **Read and write fields in-place** via `Codec.get` / `Codec.set` — zero-copy,
  zero-allocation for immediate types (int, bool)
- **Decode and encode records** via `Codec.decode` / `Codec.encode`
- **Emit EverParse `.3d` files** for verified C parser generation
- **Render RFC-style ASCII diagrams** via `Ascii.of_codec`
- **Generate C/OCaml FFI stubs** for differential testing between OCaml and C

## Quick start

```ocaml
open Wire

type packet = { version : int; flags : int; length : int }

let f_version = Codec.field "Version" (bits ~width:4 U8) (fun p -> p.version)
let f_flags   = Codec.field "Flags"   (bits ~width:4 U8) (fun p -> p.flags)
let f_length  = Codec.field "Length"   uint16be           (fun p -> p.length)

let codec =
  Codec.view "Packet"
    (fun version flags length -> { version; flags; length })
    Codec.[f_version; f_flags; f_length]
```

```
  0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |Version| Flags |            Length             |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### Zero-copy field access

Read and write individual fields directly in a buffer:

```ocaml
(* Staged for performance — force once, reuse *)
let get_version = Staged.unstage (Codec.get codec f_version)
let set_length  = Staged.unstage (Codec.set codec f_length)

let v = get_version buf 0
let () = set_length buf 0 1024

(* Full record decode/encode *)
let pkt = Codec.decode codec buf 0
let () = Codec.encode codec pkt buf 0
```

### EverParse 3D output

The same codec produces `.3d` files for verified C parser generation:

```ocaml
let s = C.struct_of_codec codec
let m = C.module_ [ C.typedef ~entrypoint:true s ]
let () = print_string (C.to_3d m)
```

### ASCII diagrams

```ocaml
let () = print_string (Ascii.of_codec codec)
```

## Features

- **Integer types** — `uint8`, `uint16`, `uint16be`, `uint32`, `uint32be`,
  `uint64`, `uint64be`
- **Bitfields** — `bits ~width:n U8/U16be/U32be`
- **Bool** — `bool (bits ~width:1 U8)` maps single-bit fields to `true`/`false`
- **Byte slices** — `byte_slice ~size:(int n)` for zero-copy sub-protocol access
- **Enumerations** — `enum` for named integer constants, `variants` for OCaml values
- **Constraints** — `where` clauses and field-level `~constraint_`
- **Actions** — `Action.assign`, `Action.return_bool`, `Action.abort`, `Action.if_`
- **Parameters** — `Param.input` / `Param.output` with typed handles
- **Tagged unions** — `casetype` with tag-based dispatch
- **Arrays** — `array ~len`, `byte_array ~size`, `nested ~size`
- **Dependent sizes** — `byte_slice ~size:(Codec.field_ref f_len)`
- **3D code generation** — emit `.3d` files compatible with EverParse
- **ASCII diagrams** — RFC 791-style 32-bit-wide bit layout diagrams
- **Labeled map** — `map ~decode ~encode` for custom value conversions

## Real-world examples

### IPv4 header

```ocaml
let f_version  = Codec.field "Version"  (bits ~width:4 U32)  (fun p -> p.ip_version)
let f_ihl      = Codec.field "IHL"      (bits ~width:4 U32)  (fun p -> p.ip_ihl)
let f_dscp     = Codec.field "DSCP"     (bits ~width:6 U32)  (fun p -> p.ip_dscp)
let f_ecn      = Codec.field "ECN"      (bits ~width:2 U32)  (fun p -> p.ip_ecn)
let f_tot_len  = Codec.field "TotalLen" (bits ~width:16 U32) (fun p -> p.ip_total_length)
(* ... *)
```

```
  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |Version|  IHL  |   DSCP    |ECN|          TotalLength          |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |        Identification         |Flags|       FragOffset        |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |      TTL      |   Protocol    |           Checksum            |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                            SrcAddr                            |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |                            DstAddr                            |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### TCP flags (bool bitfields)

```ocaml
let f_syn = Codec.field "SYN" (bool (bits ~width:1 U16be)) (fun t -> t.tcp_syn)
let f_ack = Codec.field "ACK" (bool (bits ~width:1 U16be)) (fun t -> t.tcp_ack)
```

### Parameters and actions

```ocaml
let max_len = Param.input "max_len" uint16be
let out_len = Param.output "out_len" uint16be

let bounded ~max_len:v =
  let ml = Param.init max_len v in
  Codec.view "Bounded"
    ~where:Expr.(Codec.field_ref f_len <= ml)
    (fun len data -> { len; data })
    Codec.[
      Codec.field "Length"
        ~action:(Action.on_success [ Action.assign out_len (Codec.field_ref f_len) ])
        uint16be (fun r -> r.len);
      Codec.field "Data" (byte_array ~size:(Codec.field_ref f_len)) (fun r -> r.data);
    ]

let c = bounded ~max_len:1024
let v = Codec.decode c buf 0
let len = Param.get out_len
```

## Architecture

```
                        +------------------+
                        |  Wire OCaml DSL  |
                        +--------+---------+
                                 |
              +------------------+------------------+
              |                  |                  |
              v                  v                  v
     +----------------+ +-------------+ +------------------+
     | Codec.get/set  | |  C.to_3d    | |  Ascii.of_codec  |
     | Codec.decode   | |  C.to_3d_   | |  Ascii.of_struct |
     | Codec.encode   | |  file       | |                  |
     +----------------+ +------+------+ +------------------+
      Zero-copy R/W            |         RFC-style diagrams
                               v
                      +-----------------+
                      |   EverParse 3D  |
                      |   (external)    |
                      +-----------------+
                       Verified C parsers
```

## Development

```
make build      # dune build
make test       # dune runtest
make bench      # requires EverParse (3d.exe in PATH)
make clean      # dune clean
```

## Project structure

| Directory | Description |
|-----------|-------------|
| `lib/` | Core `wire` library: DSL types, Codec, Eval, Param, Action, Ascii, C |
| `lib/c/` | `wire.c` sublibrary: EverParse pipeline (generate .3d, run 3d.exe) |
| `examples/space/` | CCSDS space protocols (SpacePacket, CLCW, TMFrame) |
| `examples/net/` | TCP/IP headers (Ethernet, IPv4, TCP, UDP) with zero-copy demo |
| `bench/` | Field-level read/write benchmarks: EverParse C vs FFI vs pure OCaml |
| `fuzz/` | Fuzz tests (wire, c, param) covering all DSL combinators |
| `test/` | Alcotest unit tests and differential tests |

## References

- [EverParse](https://project-everest.github.io/everparse/) — verified parser
  generator from Project Everest
- [3D Language Reference](https://project-everest.github.io/everparse/3d-lang.html)
  — EverParse DSL specification

## Licence

ISC
