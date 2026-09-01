# ocaml-wire

Binary wire format DSL with EverParse 3D output.

## Overview

Hand-written binary parsers in C are a recurring source of memory-safety
bugs, which is why [EverParse](https://project-everest.github.io/everparse/)
is attractive for security-critical systems: it generates C parsers with
machine-checked proofs of memory safety and correctness. Its `.3d` schemas
are written by hand, though, and a `.3d` file gives you a validator, not a
serialiser or a codec for the language the application is written in.

Wire describes a binary format once, as an OCaml value, and derives from that
single description both a zero-copy OCaml codec, for parsing and serialising,
and the EverParse `.3d` schema that compiles to a verified C parser. Define
the format, then:

- **Name reusable fields** with `Field.v` and assemble records with `Codec`
- **Read and write fields in-place** via staged `Codec.get` / `Codec.set` --
  zero-copy, with zero per-call allocation for parameter-free immediate types
  (int, bool)
- **Decode records** via `Codec.decode`, allocate validated encodings with
  `Codec.to_bytes` / `Codec.to_string`, or encode into an existing buffer with
  `Codec.encode`
- **Export EverParse `.3d` schemas** via `Everparse.project` / `Everparse.write`
- **Generate verified C artifacts** via `Wire_3d.run`
- **Generate OCaml FFI stubs** via `Wire_stubs` when OCaml should call the C
- **Render RFC-style ASCII diagrams** via `Ascii.of_codec`
- **Differential-test OCaml against C** via `Wire_diff`

## Install

```
opam install wire
```

API reference: [wire on ocaml.org](https://ocaml.org/p/wire/latest).

## Quick start

```ocaml
open Wire

type packet = { version : int; flags : int; length : UInt16.t; tag : UInt8.t }

let f_version = Field.v "Version" (bits ~width:4 U8)
let f_flags   = Field.v "Flags"   (bits ~width:4 U8)
let f_length  = Field.v "Length"  uint16be
let f_tag     = Field.v "Tag"     uint8

(* Bind fields before the codec -- same objects used for get/set *)
let bf_version = Codec.(f_version $ (fun p -> p.version))
let bf_flags   = Codec.(f_flags   $ (fun p -> p.flags))
let bf_length  = Codec.(f_length  $ (fun p -> p.length))
let bf_tag     = Codec.(f_tag     $ (fun p -> p.tag))

let codec =
  let open Codec in
  v "Packet" (fun version flags length tag ->
      { version; flags; length; tag })
    [ bf_version; bf_flags; bf_length; bf_tag ]
```

```
  0               1               2               3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
 |Version| Flags |            Length             |      Tag      |
 +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

### Zero-copy field access

```ocaml
(* Staged for performance -- force once, reuse the closure *)
let get_version = Staged.unstage (Codec.get codec bf_version)
let set_version = Staged.unstage (Codec.set codec bf_version)

let buf =
  Codec.to_bytes codec
    { version = 1; flags = 2; length = UInt16.v 1024; tag = UInt8.zero }
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
let schema = Everparse.project ~mode:`Ffi codec

let write () = Everparse.write ~mode:`Ffi ~outdir:"schemas" [ schema ]
```

The 3D output uses the EverParse output-types pattern: the generated C
validates and, in the same pass, extracts every field via schema-prefixed
extern callbacks (`<Name>SetU8`, `<Name>SetU16BE`, ...). See
[Consuming from C](#consuming-from-c) for what that means at the C level.

To turn those schemas into EverParse-generated C:

```ocaml
let run_3d () = Wire_3d.run ~outdir:"schemas" [ schema ]
```

If OCaml needs to call the generated C validators, generate FFI stubs:

```ocaml
let stubs () =
  Wire_stubs.generate ~schema_dir:"schemas" ~outdir:"."
    [ Wire_stubs.C codec ]
```

For unusual EverParse constructs that have no codec equivalent yet, use the
`Everparse.Raw` API.

## Consuming from C

`Wire_3d.run` emits a verified validator (`<Name>.h`/`.c`) alongside a
default "plug" (`<Name>_Fields.h`/`.c`) that extracts every named field
into a typed `<Name>Fields` struct. Link the plug, stack-allocate the
struct, pass it as the context, read the members you care about.

```c
#include "SpacePacket.h"
#include "SpacePacket_Fields.h"

static void err(const char *t, const char *f, const char *r,
                uint64_t c, uint8_t *ctx, uint8_t *i, uint64_t p) { (void)0; }

SpacePacketFields p = {0};
if (EverParseIsSuccess(SpacePacketValidateSpacePacket(
        (WIRECTX *)&p, NULL, err, buf, len, 0))) {
  printf("APID=%u SeqCount=%u\n", p.APID, p.SeqCount);
}
```

### Custom plug (hot-path optimisation)

If profiling says the field stores are hot, copy the shipped `<Name>_Fields.c`
to your own `my_plug.c`, delete the `case`s for fields you don't need, and
link your copy instead of the default. Override `<Name>_ExternalTypedefs.h`
and `<Name>_Fields.h` in your include path if you also want a smaller
`WIRECTX` struct; skip the override and the default struct just carries a
few unused bytes.

```c
/* my_plug.c -- started from SpacePacket_Fields.c, trimmed to one field */
#include <stdint.h>
#include "SpacePacket_Fields.h"
#include "SpacePacket_ExternalTypedefs.h"
#include "SpacePacket_ExternalAPI.h"

void SpacePacketSetU16BE(WIRECTX *ctx, uint32_t idx, uint16_t v) {
  SpacePacketFields *f = (SpacePacketFields *)ctx;
  switch (idx) {
    case SPACEPACKET_IDX_APID: f->APID = v; break;
    default: (void)f; (void)v; break;
  }
}
```

If your schema uses multiple setter type families (e.g. `u8` fields *and*
`u16be` fields), the shipped `_Fields.c` defines one function per family.
Your copy keeps all of those functions -- delete `case`s, not whole
functions. A family you don't care about reduces to a function whose
`switch` has no real cases, just the `default`. Usually one or two
short one-liners.

No weak symbols, no linker magic: whichever plug `.c` you link gets used.

## Features

Wire covers the binary-format constructs that project cleanly to 3D. Each row
is one construct, the OCaml that describes it, and the 3D it generates:

| Feature | OCaml | [EverParse 3D][3d-ref] |
|---------|-------|------------------------|
| Integer types | `uint8`, `uint16be`, `uint32be`, `uint64be` | `UINT8`, `UINT16BE`, ... |
| Bitfields | `bits ~width:n U8/U16be/U32be` | `UINT32BE { x : 4 }` |
| Bool | `bit (bits ~width:1 U8)` | -- |
| Byte slices | `byte_slice ~size:e` (zero-copy from `bytes`) | `UINT8 [: e]` |
| Byte arrays | `byte_array ~size:e` (copied) | `UINT8 [: e]` |
| Enumerations | `enum`, `variants` | [`enum`][3d-enum] |
| Constraints | `where`, `~constraint_` | [`where`][3d-where] |
| Actions | `Action.assign`, `abort`, `if_` | [`:on-success`][3d-act] |
| Parameters | `Param.input` / `Param.output` | [`entrypoint ... (params)`][3d-param] |
| Tagged unions | `casetype` | [`casetype`][3d-case] |
| Arrays | `array ~len:e`, `nested ~size:e` | `t [: e]` |
| Dependent sizes | `Field.ref f_len` | field references |
| Custom mappings | `map ~decode ~encode` | -- |

[3d-ref]: https://project-everest.github.io/everparse/3d-lang.html
[3d-enum]: https://project-everest.github.io/everparse/3d-lang.html#constants-and-enumerations
[3d-where]: https://project-everest.github.io/everparse/3d-lang.html#constraints
[3d-act]: https://project-everest.github.io/everparse/3d-lang.html#actions
[3d-param]: https://project-everest.github.io/everparse/3d-lang.html#parameterized-data-types
[3d-case]: https://project-everest.github.io/everparse/3d-lang.html#tagged-unions-or-casetype

## Real-world examples

The [`examples/`](https://github.com/parsimoni-labs/ocaml-wire/tree/main/examples)
directory has complete definitions for CCSDS space packets and TCP/IP headers.
The fragments below give the flavour.

### Diagrams from the codec

The diagrams below are not hand-drawn. `Ascii.of_codec` renders any codec as a
32-bit-wide bit layout in the conventions of RFC 791: a two-row bit ruler, one
row per 32 bits, and each field sized by the bits it actually occupies, so a
diagram cannot drift from the definition the parser is built from.

```ocaml
let diagram = Ascii.of_codec codec
let () = print_string diagram
```

`Ascii.pp_codec` is the `Format` version, and `of_struct` / `pp_struct` take a
`Types.struct_` for a description that has no codec. A field whose width is not
known until decode renders as a full-width row carrying its size expression:

```
 +-------------------------------+
 | Data (Len * 8 bits)           |
 +-------------------------------+
```

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
let f_syn = Field.v "SYN" (bit (bits ~width:1 U16be))
let f_ack = Field.v "ACK" (bit (bits ~width:1 U16be))
```

### Parameters and actions

```ocaml
type bounded = { len : UInt16.t; data : string }

let max_len = Param.input "max_len" uint16be
let out_len = Param.output "out_len" uint16be
let f_len = Field.v "Length" uint16be
let f_data =
  Field.v "Data"
    ~action:(Action.on_success [ Action.assign out_len (Field.ref f_len) ])
    (byte_array ~size:(Field.ref f_len))

let codec =
  let open Codec in
  v "Bounded"
    ~where:Expr.(Field.ref f_len <= Param.expr max_len)
    (fun len data -> { len; data })
    [ f_len  $ (fun r -> r.len);
      f_data $ (fun r -> r.data) ]

let env = Codec.env codec |> Param.bind max_len (UInt16.v 1024)
let _ = Codec.decode ~env codec buf 0
let len = Param.get env out_len
```

## Development

```
dune build
dune runtest
```

The benchmarks compare the OCaml codec against the EverParse-generated C and
the FFI bridge, so they need `3d.exe` on the `PATH`. The `Makefile` has the
individual `make bench-*` targets.

## References

- [Describing Binary Formats in OCaml](https://gazagnaire.org/blog/2026-03-31-ocaml-wire.html)
  -- the design rationale behind wire, with benchmarks
- [EverParse](https://project-everest.github.io/everparse/) -- verified parser
  generator from Project Everest
- [3D Language Reference](https://project-everest.github.io/everparse/3d-lang.html)
  -- EverParse DSL specification

## Licence

ISC
