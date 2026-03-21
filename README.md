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

let f_version = Field.v "Version" (bits ~width:4 U8)
let f_flags   = Field.v "Flags"   (bits ~width:4 U8)
let f_length  = Field.v "Length"  uint16be

(* Bind fields before the codec — same objects used for get/set *)
let bf_version = Codec.(f_version $ (fun p -> p.version))
let bf_flags   = Codec.(f_flags   $ (fun p -> p.flags))
let bf_length  = Codec.(f_length  $ (fun p -> p.length))

let codec =
  let open Codec in
  v "Packet" (fun version flags length -> { version; flags; length })
    [ bf_version; bf_flags; bf_length ]
```

```
  0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 3 3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |Version| Flags |            Length             |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### Zero-copy field access

```ocaml
(* Staged for performance — force once, reuse the closure *)
let get_version = Staged.unstage (Codec.get codec bf_version)
let set_version = Staged.unstage (Codec.set codec bf_version)

let buf = Bytes.create (Codec.wire_size codec)
let () = Codec.encode codec { version = 1; flags = 2; length = 1024 } buf 0
let v = get_version buf 0        (* read version without allocating a record *)
let () = set_version buf 0 3     (* mutate version in place *)
```

### Dependent sizes

```ocaml
let f_len  = Field.v "Length" uint16be
let f_data = Field.v "Data" (byte_array ~size:(Field.ref f_len))
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

| Feature | OCaml | [EverParse 3D][3d-ref] |
|---------|-------|------------------------|
| Integer types | `uint8`, `uint16be`, `uint32be`, `uint64be` | `UINT8`, `UINT16BE`, … |
| Bitfields | `bits ~width:n U8/U16be/U32be` | `UINT32BE { x : 4 }` |
| Bool | `bool (bits ~width:1 U8)` | — |
| Byte slices | `byte_slice ~size:e` (zero-copy) | `UINT8 [: e]` |
| Byte arrays | `byte_array ~size:e` (copied) | `UINT8 [: e]` |
| Enumerations | `enum`, `variants` | [`enum`][3d-enum] |
| Constraints | `where`, `~constraint_` | [`where`][3d-where] |
| Actions | `Action.assign`, `abort`, `if_` | [`:on-success`][3d-act] |
| Parameters | `Param.input` / `Param.output` | [`entrypoint … (params)`][3d-param] |
| Tagged unions | `casetype` | [`casetype`][3d-case] |
| Arrays | `array ~len:e`, `nested ~size:e` | `t [: e]` |
| Dependent sizes | `Field.ref f_len` | field references |
| Custom mappings | `map ~decode ~encode` | — |
| ASCII diagrams | `Ascii.of_codec` | — |

[3d-ref]: https://project-everest.github.io/everparse/3d-lang.html
[3d-enum]: https://project-everest.github.io/everparse/3d-lang.html#enums
[3d-where]: https://project-everest.github.io/everparse/3d-lang.html#constraints
[3d-act]: https://project-everest.github.io/everparse/3d-lang.html#actions
[3d-param]: https://project-everest.github.io/everparse/3d-lang.html#parameterized-types
[3d-case]: https://project-everest.github.io/everparse/3d-lang.html#tagged-unions

## Real-world examples

### IPv4 header

```ocaml
let f_version  = Field.v "Version"  (bits ~width:4 U32)
let f_ihl      = Field.v "IHL"      (bits ~width:4 U32)
let f_dscp     = Field.v "DSCP"     (bits ~width:6 U32)
let f_ecn      = Field.v "ECN"      (bits ~width:2 U32)
let f_tot_len  = Field.v "TotalLen" (bits ~width:16 U32)
(* ... bound with $ inside Codec.v *)
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
let f_syn = Field.v "SYN" (bool (bits ~width:1 U16be))
let f_ack = Field.v "ACK" (bool (bits ~width:1 U16be))
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
  let open Codec in
  v "Bounded"
    ~where:Expr.(Field.ref f_len <= ml)
    (fun len data -> { len; data })
    [ f_len  $ (fun r -> r.len);
      f_data $ (fun r -> r.data) ]

let c = bounded ~max_len:1024
let _ = Codec.decode c buf 0
let len = Param.get out_len
```

## Architecture

```
      +-----------------------------+
      | Field.v + Codec.v / ($)     |
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
