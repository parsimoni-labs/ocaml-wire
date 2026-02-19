# d3t

Dependent Data Descriptions for binary wire formats.

## Overview

d3t is a GADT-based DSL for describing binary wire formats compatible with
EverParse's 3D language. Define your format once, then:

- Emit EverParse 3D files for verified C parser generation
- Parse and encode directly in OCaml via bytesrw
- Generate C stubs for differential testing between OCaml and C implementations

## Features

- **Integer types** -- UINT8, UINT16, UINT32, UINT64 (little and big endian)
- **Bitfields** -- extract bit ranges from integer bases with constraints
- **Enumerations** -- named integer constants with validation
- **Structs** -- records with dependent fields and constraints
- **Tagged unions** -- casetype with tag-based dispatch
- **Arrays** -- fixed-count, byte-sized, and variable-length
- **Constraints** -- where clauses with arithmetic and logical expressions
- **Parameterised types** -- reusable type templates

## Installation

```
opam install d3t
```

## Usage

### Defining a Format

```ocaml
open D3t

(* Simple packet header *)
let header =
  struct_ "Header" [
    field "version" uint8;
    field "length" uint16;
    field "flags" uint8;
  ]

(* With constraints *)
let constrained_header =
  struct_ "ConstrainedHeader" [
    field "version" ~constraint_:Expr.(ref "version" >= int 1) uint8;
    field "length" ~constraint_:Expr.(ref "length" <= int 1500) uint16;
  ]

(* Bitfields *)
let flags =
  struct_ "Flags" [
    field "priority" (bits ~width:3 bf_uint8);
    field "encrypted" (bits ~width:1 bf_uint8);
    field "compressed" (bits ~width:1 bf_uint8);
    field "reserved" (bits ~width:3 bf_uint8);
  ]

(* Tagged union *)
let message =
  casetype "Message" uint8 [
    case 1 (struct_ "Text" [field "text" all_bytes]);
    case 2 (struct_ "Binary" [field "data" (byte_array ~size:(ref "len"))]);
  ]
```

### Generating EverParse 3D

```ocaml
let m = module_ "Protocol" [
  typedef header;
  typedef constrained_header;
]

let () = print_string (to_3d m)
```

Output:
```
module Protocol

typedef struct Header {
  UINT8 version;
  UINT16 length;
  UINT8 flags;
} Header;

typedef struct ConstrainedHeader {
  UINT8 version { version >= 1 };
  UINT16 length { length <= 1500 };
} ConstrainedHeader;
```

### Direct OCaml Parsing

```ocaml
open D3t

(* Define a record codec *)
let header_codec =
  Record.(
    field "version" uint8 ~get:(fun h -> h.version) ~set:(fun h v -> { h with version = v })
    @@ field "length" uint16 ~get:(fun h -> h.length) ~set:(fun h v -> { h with length = v })
    @@ field "flags" uint8 ~get:(fun h -> h.flags) ~set:(fun h v -> { h with flags = v })
    @@ finish { version = 0; length = 0; flags = 0 }
  )

(* Parse from bytes *)
let parse_header buf =
  decode_record_from_string header_codec buf

(* Encode to bytes *)
let encode_header header =
  encode_record_to_string header_codec header
```

### C Code Generation

Generate C headers and OCaml FFI stubs for differential testing:

```ocaml
let structs = [header; constrained_header]

(* Generate C runtime header *)
let () = Out_channel.write_all "d3t.h" ~data:(to_c_runtime ())

(* Generate per-struct C headers *)
let () =
  List.iter (fun s ->
    Out_channel.write_all (s.name ^ ".h") ~data:(to_c_header s)
  ) structs

(* Generate OCaml external stubs *)
let () = print_string (to_ml_stubs structs)
```

## Expressions

The `Expr` module provides operators for constraints and computations:

```ocaml
open D3t.Expr

(* Arithmetic *)
let size_check = ref "length" - int 4

(* Comparison *)
let valid_version = ref "version" >= int 1 && ref "version" <= int 255

(* Bitwise *)
let masked = ref "flags" land int 0x0F

(* Field references *)
let dynamic_size = ref "header_length" + ref "payload_length"
```

## Architecture

```
┌─────────────────┐
│  OCaml DSL      │  Define formats with GADTs
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
┌───────┐ ┌───────────┐
│ to_3d │ │ Record    │  EverParse output or direct parsing
└───┬───┘ │ codecs    │
    │     └─────┬─────┘
    ▼           ▼
┌───────┐ ┌───────────┐
│ .3d   │ │ OCaml     │
│ files │ │ parse/    │
└───┬───┘ │ encode    │
    │     └───────────┘
    ▼
┌───────────────┐
│ EverParse     │  Verified C parser generation
│ (external)    │
└───────────────┘
```

## Dependencies

| Library | Purpose |
|---------|---------|
| bytesrw | Byte stream reading and writing |
| fmt | Pretty printing |

## References

- [EverParse](https://project-everest.github.io/everparse/) -- verified parser generator
- [3D Language Reference](https://project-everest.github.io/everparse/3d-lang.html) -- EverParse DSL specification

## Licence

ISC
