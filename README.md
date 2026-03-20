# ocaml-wire

Binary wire format DSL with EverParse 3D output.

## Overview

Wire is a GADT-based OCaml DSL for describing binary wire formats.
Define your format once, then:

- **Name reusable fields** with `Field.v` and assemble records with `Codec`
- **Read and write fields in-place** via `Codec.get` / `Codec.set` — zero-copy,
  zero-allocation for immediate types (int, bool)
- **Decode and encode records** via `Codec.decode` / `Codec.encode`
- **Export EverParse `.3d` schemas** via `C.schema` / `C.generate`
- **Generate verified C artifacts** via `Wire_3d`
- **Generate OCaml FFI stubs** via `Wire_stubs` when OCaml should call the C
- **Render RFC-style ASCII diagrams** via `Ascii.of_codec`
- **Differential-test OCaml against C** via `Wire_diff`

## Quick start

```ocaml
open Wire

type packet = { version : int; flags : int; length : int }

let version = Field.v "Version" (bits ~width:4 U8)
let flags   = Field.v "Flags"   (bits ~width:4 U8)
let length  = Field.v "Length"  uint16be

let codec =
  Codec.v "Packet" (fun version flags length -> { version; flags; length })
  |+ version (fun p -> p.version)
  |+ flags   (fun p -> p.flags)
  |+ length  (fun p -> p.length)
  |> Codec.seal
```

```
  0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |Version| Flags |            Length             |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### Zero-copy field access

The same field handles work for `get`/`set`, dependent sizes, and `Field.ref`:

```ocaml
(* Bind into codec fields for get/set *)
let cf_version = Codec.bind version (fun p -> p.version)

(* Staged for performance — force once, reuse *)
let get_version = Staged.unstage (Codec.get codec cf_version)
let set_version = Staged.unstage (Codec.set codec cf_version)

let v = get_version buf 0
let () = set_version buf 0 3

(* Dependent sizes use Field.ref *)
let f_len  = Field.v "Length" uint16be
let f_data = Field.v "Data" (byte_array ~size:(Field.ref f_len))

(* Full record decode/encode *)
let () = Codec.encode codec { version = 1; flags = 2; length = 1024 } buf 0
let (Ok pkt) = Codec.decode codec buf 0
```

### EverParse 3D output

The same codec produces `.3d` files:

```ocaml
let schema = C.schema codec
let () = C.generate ~outdir:"schemas" [ schema ]
```

To turn those schemas into EverParse-generated C:

```ocaml
let () = Wire_3d.generate ~outdir:"schemas" [ schema ]
```

If OCaml needs to call the generated C validators, generate stubs separately
from the exported structs:

```ocaml
let struct_ = C.struct_of_codec codec
let c_stubs = Wire_stubs.to_c_stubs [ struct_ ]
let ml_stubs = Wire_stubs.to_ml_stubs [ struct_ ]
```

For unusual EverParse constructs that have no codec equivalent yet, use the
explicit escape hatch `C.Raw`.

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
- **Dependent sizes** — `byte_slice ~size:(Field.ref f_len)`
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
let f_len = Field.v "Length" uint16be
let f_data =
  Field.v "Data"
    ~action:(Action.on_success [ Action.assign out_len (Field.ref f_len) ])
    (byte_array ~size:(Field.ref f_len))

let bounded ~max_len:v =
  let ml = Param.init max_len v in
  Codec.view "Bounded"
    ~where:Expr.(Field.ref f_len <= ml)
    (fun len data -> { len; data })
    Codec.[
      Codec.bind f_len (fun r -> r.len);
      Codec.bind f_data (fun r -> r.data);
    ]

let c = bounded ~max_len:1024
let _ = Codec.decode c buf 0
let len = Param.get out_len
```

## Architecture

```
      +-----------------------------+
      | Field.v + Codec.view/bind   |
      | describe record formats     |
      +--------------+--------------+
                     |
         +-----------+-----------+
         |                       |
         v                       v
  +---------------+       +---------------+
  | Codec         |       | Ascii         |
  | decode/encode |       | of_codec      |
  | get/set       |       |               |
  +-------+-------+       +---------------+
          |
          v
  +---------------+
  | C.schema      |
  | C.generate    |
  +-------+-------+
          |
          v
  +---------------+       +---------------+
  | Wire_3d       | ----> | Wire_stubs    |
  | EverParse/C   |       | OCaml FFI     |
  | tooling       |       | glue          |
  +-------+-------+       +-------+-------+
          |                       |
          +-----------+-----------+
                      |
                      v
                +---------------+
                | Wire_diff     |
                | compare       |
                | OCaml vs C    |
                +---------------+
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
| `lib/3d/` | `wire.3d` sublibrary: EverParse tooling (write `.3d`, run `3d.exe`, generate C artifacts) |
| `lib/stubs/` | `wire.stubs` sublibrary: generate OCaml/C FFI stubs for generated validators |
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
