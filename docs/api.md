# The end-state API

`wire` describes a binary format once, as an OCaml value, and derives from
that value everything that reads and writes the bytes: a decoder and an
encoder, in-place views of untrusted buffers, folds over many records,
generated OCaml, OxCaml and Rust, and a verified C validator. This document
states the API `wire` reaches: the model first, on one running example, then
the signatures that follow from it, what goes, what is still open and the
decisions it rests on. Paths and line numbers are those of `ed6be0fc`.

## The model

The running example is the CCSDS space packet primary header, six bytes
that a ground station routes by its `apid`. It is `Space.packet_codec`
(examples/space/space.ml:8-45), called `packet` here, whose bound fields
`bf_sp_apid` and `bf_sp_data_len` are called `apid` and `data_length`.
`View` is `Codec.View`.

### A codec describes the bytes once

```ocaml
(* space.ml *)
open Wire

type packet = {
  version : int; type_ : int; sec_hdr : int; apid : int;
  seq_flags : int; seq_count : int; data_len : UInt16.t;
}

let apid = Codec.(Field.v "APID" (bits ~width:11 U16be) $ fun p -> p.apid)
let data_length = Codec.(Field.v "DataLength" uint16be $ fun p -> p.data_len)

let packet =
  Codec.v "SpacePacket"
    (fun version type_ sec_hdr apid seq_flags seq_count data_len ->
      { version; type_; sec_hdr; apid; seq_flags; seq_count; data_len })
    Codec.[
      (Field.v "Version" (bits ~width:3 U16be) $ fun p -> p.version);
      (Field.v "Type" (bits ~width:1 U16be) $ fun p -> p.type_);
      (Field.v "SecHdrFlag" (bits ~width:1 U16be) $ fun p -> p.sec_hdr);
      apid;
      (Field.v "SeqFlags" (bits ~width:2 U16be) $ fun p -> p.seq_flags);
      (Field.v "SeqCount" (bits ~width:14 U16be) $ fun p -> p.seq_count);
      data_length;
    ]
```

A field states its width on the wire (`uint16be` is two bytes, `bits
~width:11 U16be` eleven bits of a big-endian 16-bit word) and its OCaml type.
`$` binds it to the record, and the constructor takes the fields in their
order. Everything below is derived from `packet`. A shape that no verified
backend can express is refused here, by `Codec.v`, with `Invalid_argument`.

### Values: decode and encode copy the whole record

```ocaml
match Codec.decode packet buf ~off:0 with
| Ok p -> Fmt.pr "apid %d@." p.apid
| Error e -> Fmt.pr "refused: %a@." Error.pp e
```

`buf` comes from an attacker, so no byte makes a decoder raise: bad bytes are
an `Error`, never an exception. `Invalid_argument` means misuse by the
program: a negative offset, or a record `Codec.encode packet p buf ~off`
would write as bytes `decode` refuses.

### Views: check once, then read and write in place

A router that reads one field per packet should not copy the other six. A
view reads the field where it lies.

```ocaml
let apid_f = View.field packet apid

let apid_of buf ~off =
  let (Buf.Any b) = Buf.v buf in
  let c = View.check packet b ~off in
  if View.is_ok c then Some (View.read b (View.get c) apid_f) else None
```

The definition `apid` says what the field is and exists before the codec,
since `Codec.v` and `Field.ref` take it; the copy `apid_f` says where the
field lies in `packet`, which only `Codec.v` can compute. One value for both
would need the codec to create its fields, or `Codec.v` to mutate the
definitions. `Codec.v` resolves a copy of each definition it receives (a
constant offset or how to compute one, and the field's reader and writer),
and `View.field` hands that copy out: plain data, read with no lookup. A
definition in two codecs has a copy in each, and a field that is only
decoded or encoded needs just the definition. `View.field` raises
`Invalid_argument` on a definition that is not one of the codec's.

`check` runs every check of the codec once. It never raises and allocates
nothing: its answer is an immediate integer, and so is the view. `is_ok` says
whether the record passed and `get` makes the answer a view, as
`Result.is_ok` and `Option.get` do; `get` of a refused check raises
`Invalid_argument`, since `is_ok` said so. A view reads a field with no check
of its own, in one load in the generated form, and `View.set b v apid_f x`
writes it in place. `Buf.v` names the buffer with a type `'b`, which the
check and the view carry, so a view is read and written only in the buffer
it was checked in. Why a record was refused is out of line: `Codec.validate
packet buf ~off` answers its `Error`.

### Fold and visitor: views over many records

```ocaml
let routes buf =
  View.fold packet Codec.[ apid ] buf ~init:(Array.make 4 0)
    ~f:(fun counts apid ->
      let r = route_of_apid apid in
      counts.(r) <- counts.(r) + 1;
      counts)
```

A fold checks each record of `buf`, as `check` does, and hands the fields it
names to its visitor `f`, positionally and in the order of the list, as
`Codec.v` hands them to the constructor (`route_of_apid` is the
application's table, as in bench/routing/routing_workload.ml:76). It takes
the definitions and resolves them once, when it is applied. The fields are
read during the check, not read again through a view. By default the records
lie back to back; `~stride` places them in fixed slots and `~offs ~lens`
takes an index of frames, as a capture file lists them. Back to back, a
refused record stops the fold with `Error (e, acc)`, since its length cannot
be trusted to find the next record. In slots or an index, `~invalid` is called on a refused record
and the fold goes on: a flow classifier over a capture needs that (4,945 of
the 1M MAWI frames are refused). Without `~invalid` it stops there too.

### Two forms, same concepts

Everything so far is the *interpreted* form: `wire` alone, nothing
generated, fields addressed by the codec's copies of the definitions it was
built from. It
is for scripts, utop, codecs built at run time and the reference oracle. The
*generated* form is a module `Wire_ocaml` (OCaml or OxCaml) or `Wire_rust`
writes from the same codec, in which a field is a function named after it. It
is for hot loops and other languages, and the OCaml module links
`wire.runtime` only.

```ocaml
(* gen.ml, run by (rule (targets space_packet.ml space_packet.mli)
                        (action (run ./gen.exe %{targets}))) *)
let () = Wire_ocaml.(main [ v ~fold:[ "apid" ] Space.packet_codec ])

(* the program *)
let apid_of buf ~off =
  let (Buf.Any b) = Buf.v buf in
  let c = Space_packet.check b ~off in
  if Space_packet.is_ok c then Some (Space_packet.apid b (Space_packet.get c))
  else None

let routes buf =
  Space_packet.fold buf ~init:(Array.make 4 0) ~f:(fun counts apid -> ...)
```

Switching is a one-to-one rename:

| Interpreted (`wire`) | Generated (`Space_packet`) |
|---|---|
| `View.field packet apid` | none: the module has it built in |
| `View.check packet b ~off` | `Space_packet.check b ~off` |
| `View.is_ok c`, `View.get c` | `Space_packet.is_ok c`, `Space_packet.get c` |
| `View.read b v apid_f` | `Space_packet.apid b v` |
| `View.set b v apid_f x` | `Space_packet.set_apid b v x` |
| `View.fold packet Codec.[ apid ] buf ~init ~f` | `Space_packet.fold buf ~init ~f`, its fields declared in `gen.ml` |
| `Codec.decode packet buf ~off`, `Codec.validate packet buf ~off` | `Space_packet.decode buf ~off`, `Space_packet.validate buf ~off` |

The two forms share no module type: a generated module has its own record
type and its fields are functions, where the interpreted form's are values.
The differential tests (`test/diff`) prove that both answer the same on the
same bytes. A generated module has at most one fold; two field sets over one
codec are two modules (`v ~name:"space_packet_route" ~fold:[ ... ]`).

#### Same shape as the generated module

A program can give the interpreted form the generated module's functions,
with the same arguments and labels, so that one line switches between the
two:

```ocaml
(* p.ml; p.mli leaves out apid_f *)
module View = Wire.Codec.View
let apid_f = View.field Space.packet Space.apid    (* staged once: the codec's copy *)
let check b ~off = View.check Space.packet b ~off
let is_ok = View.is_ok
let get = View.get
let apid b v = View.read b v apid_f
let set_apid b v x = View.set b v apid_f x
let fold ?stride ?offs ?lens ?invalid buf ~init ~f =
  View.fold ?stride ?offs ?lens ?invalid Space.packet Wire.Codec.[ Space.apid ] buf ~init ~f

(* the program; [module P = Space_packet] runs it on the generated form *)
let apid_of buf ~off =
  let (Wire.Buf.Any b) = Wire.Buf.v buf in
  let c = P.check b ~off in
  if P.is_ok c then Some (P.apid b (P.get c)) else None
```

User code sees two values with one name in two modules: `Space.apid`, the
field, and `P.apid`, its accessor, the same pair as `Space.apid` and
`Space_packet.apid` in the generated form. Staging returns data
(`View.field`), not a function: a reader made by partial application, `let
apid = View.read Space.packet Space.apid`, falls under the value restriction,
its buffer brand `'b` becomes weak, and each `Buf.Any` unpacking has a fresh
`'b`. The eta-expanded functions keep `'b` polymorphic and only interpret
the copy resolved once. The module is optional (plain `View.read b v apid_f`
works without it), and `wire` does not generate it, since the interpreted
form needs no generation.

### A nested codec

The same concepts hold when a field holds a codec. `Packet_stack.codec`
(examples/net/packet_stack.ml:220-229, called `eth` here) is an Ethernet
header, then an IPv4 header when `ether_type` is `0x0800`, then a TCP header
when the packet is unfragmented TCP (packet_stack.ml:127-131, 161-165). Its
IPv4 and TCP codecs, `Packet_stack.ipv4` and `Packet_stack.tcp`, are called
`stack_ipv4` and `stack_tcp`, and its bound fields `eth_ipv4`,
`ipv4_protocol`, `ipv4_tcp` and `tcp_dst_port` are called `ipv4`,
`protocol`, `tcp` and `dst_port` here.

One `check` validates the frame and every layer present in it. Reaching a
field that holds a codec gives that codec's check, whose `is_ok` says whether
the layer is present, and makes no check again. `View.layer` resolves such a
field in its codec, as `View.field` resolves any other.

```ocaml
(* interpreted *)
let ipv4_f = View.layer eth ipv4
let protocol_f = View.field stack_ipv4 protocol

let c = View.check eth b ~off in
if View.is_ok c then begin
  let c4 = View.sub b (View.get c) ipv4_f in
  if View.is_ok c4 then begin
    let v4 = View.get c4 in
    if View.read b v4 protocol_f = UInt8.v 6 then
      View.set b v4 protocol_f (UInt8.v 17)
  end
end

(* generated by Wire_ocaml.v ~name:"eth" eth *)
let c = Eth.check b ~off in
if Eth.is_ok c then begin
  let c4 = Eth.ipv4 b (Eth.get c) in
  if Eth.Stack_ipv4.is_ok c4 then begin
    let v4 = Eth.Stack_ipv4.get c4 in
    if Eth.Stack_ipv4.protocol b v4 = UInt8.v 6 then
      Eth.Stack_ipv4.set_protocol b v4 (UInt8.v 17)
  end
end
```

The generated module has one submodule per codec a field holds, named after
the codec and written once however many fields hold it: `StackIPv4` is
`Eth.Stack_ipv4` and `StackTCP` is `Eth.Stack_tcp`
(lib/ocaml/view_module.ml:98-121 derives the name, 244-270 writes each codec
once). Each has `check`, `is_ok`, `get`, its accessors and `set_<field>`:
`Eth.ipv4 b v : Eth.Stack_ipv4.check`, `Eth.Stack_ipv4.tcp b v4 :
Eth.Stack_tcp.check`. `View.sub b v ipv4_f` is `Eth.ipv4 b v`, and
`View.read b v4 protocol_f` is `Eth.Stack_ipv4.protocol b v4`.

A fold names a nested field by the path of its definitions. A field under an
optional layer reaches `f` as an `option`, `None` where the layer is absent:

```ocaml
View.fold eth Codec.[ Field.(ipv4 / protocol); Field.(ipv4 / tcp / dst_port) ]
  buf ~offs ~lens ~init ~f:(fun acc proto dport -> ...)

(* generated by Wire_ocaml.v ~name:"eth" ~fold:[ "ipv4.protocol"; "ipv4.tcp.dst_port" ] eth *)
Eth.fold buf ~offs ~lens ~init ~f:(fun acc proto dport -> ...)
```

The paths and the options are proposals, listed under § Open questions.

### Verified backends

Each codec projects to the verified parser technology that can express it,
and is checked there: EverParse 3D for binary layouts, everparse-asn1 for
ASN.1 DER (docs/asn1.md on `lane/asn1-design`, `6c26ab0d`).

```ocaml
(* gen_3d.ml, run by the dune rules it writes *)
let () = Wire_3d.main ~mode:`Ffi ~package:"space" [ Wire_3d.pack Space.packet_codec ]
```

This writes `SpacePacket.3d`, runs EverParse on it for a verified C
validator, and `Wire_stubs` binds that validator to OCaml. On a refusal the C
tier answers the `Error` `Codec.validate` answers, so every tier gives one
error for the same bytes.

## The surface

Each block is an interface, abbreviated where it says so, under the concept
it serves.

### `wire`: codecs (a) and values (b)

The DSL is unchanged (wire.mli:50-1019): `typ`, `expr`, `Param`, `Action`,
`Expr`, `Field`, the scalar types, `bits`, `map`, `enum`, `variants`,
`casetype`, the arrays and the byte types, `nested`, `codec`. `Field.packed`
and `Field.decl_of_packed` go, and `size : 'a typ -> int option` becomes
`fixed_size`.

```ocaml
module Codec : sig
  type 'r t                                          (** A codec of records ['r]. *)
  type ('a, 'r) field                                (** A field bound to ['r]. *)
  type ('f, 'k, 'r) fields =
    | [] : ('k, 'k, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'k, 'r) fields -> ('a -> 'f, 'k, 'r) fields
                         (** Fields given in order to a function ending in ['k]:
                             the constructor of {!v}, or the visitor of a fold. *)
  val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field   (** [f $ proj] binds [f]. *)
  val v : string -> ?where:bool expr -> ?doc:string -> 'f -> ('f, 'r, 'r) fields -> 'r t
                                                     (** Raises on a shape no backend checks. *)
  val name : 'r t -> string
  val rename : string -> 'r t -> 'r t
  val doc : 'r t -> string option
  val env : 'r t -> Param.env                        (** A fresh environment for [c]. *)
  val min_size : 'r t -> int                         (** The fewest bytes a record takes. *)
  val fixed_size : 'r t -> int option                (** The bytes of every record, if constant. *)

  val decode :
    ?consume:consumption -> ?env:Param.env -> 'r t -> bytes -> off:int -> ('r, Error.t) result
  val validate : ?env:Param.env -> 'r t -> bytes -> off:int -> (unit, Error.t) result
  val encode : ?env:Param.env -> 'r t -> 'r -> bytes -> off:int -> unit
                                                     (** Raises on a value [decode] refuses. *)
  val to_string : ?env:Param.env -> 'r t -> 'r -> string
  val size : ?env:Param.env -> 'r t -> 'r -> int     (** The bytes [encode] writes. *)
  val size_at : ?env:Param.env -> 'r t -> bytes -> off:int -> (int, Error.t) result
  val pp_value : 'r t -> 'r Fmt.t
  module View : (* below *)
end
```

`Error` (lib/runtime/error.mli) is unchanged: `t = { at; field; kind }`,
`pp`, `equal`, `compare`.

### `wire`: views (c) and folds (d)

```ocaml
module View : sig
  type ('b, 'r) check [@@immediate]                  (** A check of an ['r] in the buffer ['b]. *)
  type ('b, 'r) t [@@immediate]                      (** An ['r] that passed in ['b]. *)

  val check : ?env:Param.env -> ?len:int -> 'r Codec.t -> 'b Buf.t -> off:int -> ('b, 'r) check
                                       (** Never raises, allocates nothing; [len] defaults
                                           to the bytes from [off] to the end. *)
  val is_ok : ('b, 'r) check -> bool
  val get : ('b, 'r) check -> ('b, 'r) t             (** Raises [Invalid_argument] unless [is_ok]. *)

  type ('a, 'r) field                  (** A field resolved in one codec of ['r]: plain data. *)
  val field : 'r Codec.t -> ('a, 'r) Codec.field -> ('a, 'r) field
                       (** The codec's copy of a definition. Raises
                           [Invalid_argument] unless it is one of the codec's. *)
  type ('s, 'r) layer                  (** A field of ['r] holding an ['s], resolved. *)
  val layer : 'r Codec.t -> ('s, 'r) Codec.layer -> ('s, 'r) layer
                       (** The same, for a field that holds a record. *)

  val read : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) field -> 'a
                       (** Never reads outside ['b]. A field that bytes written
                           after the check move out of ['b] reads as absent. *)
  val set : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) field -> 'a -> unit
                       (** No constraint checked. Raises on a value the field
                           cannot hold, or a field with no constant place. *)
  val sub : 'b Buf.t -> ('b, 'r) t -> ('s, 'r) layer -> ('b, 's) check
                       (** The record a field holds, checked by the outer check;
                           refused where an optional layer is absent. *)

  val fold :
    ?env:Param.env -> ?stride:int -> ?offs:int array -> ?lens:int array ->
    ?invalid:('acc -> 'acc) ->
    'r Codec.t -> ('f, 'acc, 'r) Codec.fields -> bytes -> init:'acc -> f:('acc -> 'f) ->
    ('acc, Error.t * 'acc) result
       (** Back to back by default; [~stride] or [~offs ~lens] place the records.
           [~invalid] continues past a refused record, in slots or an index only. *)
end
```

The interpreted calls take the buffer as the generated ones do, so a view is
an immediate in both forms and `check`, `is_ok` and `get` allocate nothing.

`Codec.v` resolves a copy of every definition it receives into a static
table: the field's constant offset or how to compute it, its reader and its
writer. Definitions stay immutable: nothing is stored in them and nothing is
sealed, so one definition in two codecs has an independent copy in each, as
`test_same_field_two_codecs` (test/test_codec_action.ml:740) requires.
`View.field` and `View.layer` hand a copy out, checking once that the
definition is the codec's, as the tip checks it today (`field_access`,
lib/codec.ml:598-603); `read`, `set` and `sub` interpret it, with no lookup
per call. `fold` resolves its definitions once, when it is applied.

A view is indexed by its record type, and so is a copy, so the types refuse
a copy from a codec of another record type. The one mismatch they miss, a
copy from another codec of the same record type, is § Open questions, 7.
`Codec.layer`, a definition that holds a record, optional or not, and the
paths `Field.( / )` a fold takes are proposals (§ Open questions, 6).

### `wire.runtime`

```ocaml
module Buf : sig
  type 'b t = private bytes                          (** A buffer named ['b]. *)
  type any = Any : 'b t -> any [@@unboxed]
  val v : bytes -> any                               (** [v buf] names [buf]. *)
  val pp : Format.formatter -> 'b t -> unit
end
module Error : sig (* lib/runtime/error.mli, unchanged *) end
module UInt8, UInt16, UInt32, SInt8, SInt16, SInt32 (* v, v_opt, to_int, zero,
  min_int, max_int, equal, compare, pp; lib/runtime/uInt16.mli:13-41 *)
module Vec (* the SIMD lanes the [~simd] modules use *)
```

### The generated module: views (c) and fold (d)

What `Wire_ocaml.v ~fold:[ "apid" ] Space.packet_codec` writes, as
`space_packet.mli`. Accessors are snake_case, as prost spells protobuf fields.

```ocaml
type 'b check = private int
type 'b view = private int
val min_size : int
val check : 'b Buf.t -> off:int -> 'b check                (** Constant size; never raises. *)
val is_ok : 'b check -> bool
val get : 'b check -> 'b view                              (** Raises unless [is_ok]. *)
val validate : bytes -> off:int -> (unit, Error.t) result

val version : 'b Buf.t -> 'b view -> int                   (** One load each. *)
val apid : 'b Buf.t -> 'b view -> int
val seq_count : 'b Buf.t -> 'b view -> int
val data_length : 'b Buf.t -> 'b view -> UInt16.t          (* ... one per field *)
val set_apid : 'b Buf.t -> 'b view -> int -> unit          (** Raises past 11 bits. *)

type t = { version : int; type_ : int; sec_hdr_flag : int; apid : int;
           seq_flags : int; seq_count : int; data_length : UInt16.t }
val decode : bytes -> off:int -> (t, Error.t) result
val encode : t -> bytes -> off:int -> unit
val size : t -> int
val pp : Format.formatter -> t -> unit
val write : bytes -> off:int -> version:int -> type_:int -> sec_hdr_flag:int -> apid:int ->
  seq_flags:int -> seq_count:int -> data_length:UInt16.t -> unit   (** Constant size only. *)

val fold :
  ?stride:int -> ?offs:int array -> ?lens:int array -> ?invalid:('acc -> 'acc) ->
  bytes -> init:'acc -> f:('acc -> int -> 'acc) -> ('acc, Error.t * 'acc) result
```

A codec of variable size takes its window, `check b ~off ~len`. A field that
holds a record answers the check of the submodule named after that record's
codec (`Eth.ipv4 b v : Eth.Stack_ipv4.check`), which passes iff the layer is
present. The Rust module has
the same names on a view that borrows its buffer:

```rust
let c = space_packet::check(buf, off);
if c.is_ok() { let v = c.get(); route(v.apid()); }
let r = space_packet::fold(buf, [0u64; 4], |mut n, apid| { n[route(apid)] += 1; n });
```

### The generators

```ocaml
(* Wire_ocaml *)
type target = [ `OCaml | `OxCaml ]
type map
val map : string -> typ:string -> 'a Wire.typ -> map     (** A map called by its OCaml path. *)
type t
val v : ?name:string -> ?target:target -> ?simd:bool -> ?fold:string list -> ?maps:map list ->
  'r Wire.Codec.t -> t   (** [fold]: the paths of the fields the module's fold hands [f]. *)
val name : t -> string
val to_ml : t -> string
val to_mli : t -> string
val main : t list -> unit                                 (** [gen.exe FILE...]. *)

(* Wire_rust *)
type t
val v : ?name:string -> ?fold:string list -> 'r Wire.Codec.t -> t
val name : t -> string
val to_rs : t -> string
val main : t list -> unit
```

### Verified backends (f)

```ocaml
module Wire.Everparse : sig
  type t                                             (** A 3D schema, abstract. *)
  type mode = [ `Ffi | `Standalone ]
  val project : ?mode:mode -> 'r Codec.t -> t
  val name : t -> string
  val filename : t -> string
  val write : ?mode:mode -> outdir:string -> ?name:string -> t list -> unit
end
module Wire.Ascii : sig
  val of_codec : 'r Codec.t -> string                (** The codec's bit diagram. *)
  val pp_codec : Format.formatter -> 'r Codec.t -> unit
end

(* Wire_3d *)
type packed
val pack : 'a Wire.Codec.t -> packed
val run : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
val main : ?name:string -> mode:[ `Ffi | `Standalone ] -> package:string -> packed list -> unit
val parse_3d : ?batch:bool -> outdir:string -> string -> (unit, string) result
val locate_3d_exe : unit -> string option

(* Wire_stubs *)
val generate : schema_dir:string -> outdir:string -> Wire_3d.packed list -> unit
val build_codec_archive : ?cflags:string list -> schema_dir:string -> archive:string -> unit -> unit
```

## What goes

`lib/wire.mli` at `ed6be0fc` has 360 declarations (`val`, `type`, `module`,
`module type`, read outside comments, the four local `:=` substitutions not
counted). 171 go or become internal and 189 stay, 7 of them renamed.

| Group | Declarations (wire.mli line) | Count |
|---|---|---:|
| In-place access besides `View` | `Codec.get` 1310, `set` 1545, `writer` 1557, `args` 1141, `field_readers` 1604; `Codec.Word` 1652-1705 (the module and its 11 members) | 17 |
| `View`'s codec names and second field kinds | `codec` (type) 1391, `CODEC` and its 3 members 1395-1402, `codec` 1406, `checked_field` 1478, `sub_option` 1523: a view is indexed by its record type, and `View.field` and `View.layer` cover every field | 8 |
| Typ-level I/O | `of_reader` 1049, `of_string` 1072, `of_bytes` 1080, `to_writer` 1097, `to_bytes` 1104, `to_string` 1107: a codec is the unit | 6 |
| Struct validators | `validator` 1716, `validator_of_struct` 1719, `validate_struct` 1722, `struct_size_of` 1726, `struct_min_size` 1729, `wire_size_info_of_validator` 1732 | 6 |
| EverParse plumbing | `Everparse`'s `struct_`, `decl`, `decl_case`, `module_`, `validate_output_name`, `uses_wire_ctx`, `visitor_field`, `visitor_member`, `visitor_fields`, `visitor_setters`, `entrypoint_struct`, `extern_fn_names`, `field_action_form`, `equal_field_action_form`, `field_action_forms` (1769-1856); `Everparse.Raw` and its 47 members (1877-2053) | 63 |
| `Ascii` of a struct | `of_struct` 2064, `pp_struct` 2070 | 2 |
| `Private` | the module and its 66 members (2083-2362) | 67 |
| Packed fields | `Field.packed` 431, `Field.decl_of_packed` 612 | 2 |

Renamed: `View.ok` (1443) to `is_ok`; `Codec.size` (1203) and `size` (1019)
to `fixed_size`; `Codec.encoded_size` (1218) to `size`; `View.sub` (type
1505 and value 1509) to `layer`, and `View.enter` (1530) to `sub`.
`View.check` (1431) takes `~off` and no `'b Buf.length`; `View.field`
(1464) and `View.layer` take the codec itself; `View.read` (1491) and
`View.sub` take the buffer, the view and the copy. The fold's `Codec.fields`
gains the parameter of its result.

Beyond `wire.mli`:

- `Wire_ocaml` (lib/ocaml/wire_ocaml.mli): 59 of 68 go. `fold` (type and
  value, 99-102) and `?folds` give way to `?fold:string list`; `outputs` 213,
  `view`/`view_member` 257-262, `views` 275, `fstar` 279 and `Plan` with its
  51 members (321-687) become internal. `to_ml`/`to_mli` lose `?twin` and
  `?visitors`, `main` its `--visitor`: the interpreted twin (`<name>_codec`),
  the visitor module and the per-fold functions `n`, `n_frames`, `n_index` go.
- `Wire_rust` (lib/rust/wire_rust.mli): `fold` (type and value, 87-90) go,
  `?fold` replaces `?folds`, `to_rs` loses `?visitors`.
- `Wire_3d` (lib/3d/wire_3d.mli): 30 of 36 become internal, `pool` and
  `max_jobs` among them; `packed`, `pack`, `run`, `main`, `parse_3d` and
  `locate_3d_exe` stay.
- `Wire_stubs` (lib/stubs/wire_stubs.mli): 10 of 12 become internal;
  `packed_codec` gives way to `Wire_3d.packed`.
- `wire.runtime`: `Buf.length` (type and value) and `Buf.local`
  (lib/runtime/buf.std.mli:12-24) go; `Unchecked` (16), `Errors` (19),
  `Arith` (10), `Target` and each carrier's `le`, `be`, `set_*`, `unsafe_*`
  and `int_eq` are hidden, since only generated code calls them.

## Open questions

Each with the measurement that settles it. The counts run as
`bench/count.sh` does: `make bench-flow`, then
`sh bench/count.sh 1000000 view _build/default/bench/rust/bench.exe flow _build/bench-flow/mawi.pcap RUNS "<row>"`,
and `make bench-apps-count` for the routing, gateway and CLCW rows, on OCaml
5.4.1 and OxCaml.

1. **Whether a view could carry its buffer**, in both forms, so that
   `Space_packet.apid v` and `View.read v apid_f` drop the `b`. Such a view is
   a block, allocated per record unless the compiler unboxes it, where
   `check`, `is_ok` and `get` now allocate nothing. Settled by the row
   `wire.ocaml check/view, accessors, parse only` and the `wire.ocaml` rows
   of `bench-apps-count`, on a branch whose views carry their buffer: the
   buffer stays an argument unless that branch counts within noise on OCaml
   5.4.1 and OxCaml.
2. **Whether binding a field before `Codec.v`** (`f $ (fun p -> p.apid)`)
   can be lighter. Not a cost: the binding runs once. Settled by writing
   examples/space/space.ml and examples/net in the lighter form, with `make
   test` and the 3D projections unchanged.
3. **Whether `check` takes `~len` for a codec of variable size.** The ruling
   on the window of a check stands (below). Once `Buf.length` goes, `check`
   reads the buffer's length itself, where a loop now reads it once; recount
   the rows of 1 with `check` reading it, and a cost beyond noise brings the
   length back as an argument.
4. **Positional visitor fields swap silently** when two of them share a type:
   `~f:(fun acc seq_count apid -> ...)` over `[ apid; seq_count ]` type-checks.
   Labelled arguments would catch it but cannot be shared with the
   interpreted fold, whose field list is a value. Settled by counting the
   folds of examples/ and bench/ whose fields share a type, and the rows of 1
   for a generated fold taking labelled arguments.
5. **`View.get` against `View.read`.** `View.get` turns a check into a view
   and only that, as `Option.get`; a field is read by `View.read b v field`
   (the tip's name, wire.mli:1491). Using `get` for both, as the decision of
   2026-11-27 wrote `View.get v apid`, would clash. A proposal, to confirm.
6. **Nested fields** (proposals, from the shape shown with the nested
   example):
   - A field under an absent optional layer reaches `f` as `None`. On stock
     OCaml each present value allocates a `Some` per record; the
     alternative, today's generated form, hands `f` a presence `bool` beside
     a zero value (`<path>_present`, wire_ocaml.mli:213-218). Settled by the
     MAWI rows of `bench/count.sh` for a fold over `ipv4 / protocol` and
     `ipv4 / tcp / dst_port` both ways.
   - The typed path combinator `Field.( / )`, and the type `Codec.layer` of
     a field that holds a record, so that a path through a field that holds
     none is a type error, and an optional layer under another gives one
     `option`, not two. Settled by its typing in `wire.mli` and a cram test
     of the rejected path.
7. **A copy from another codec with the same record type.** A copy and a
   view are both indexed by the record type, so when two codecs share one, a
   copy from one (`View.field c1 apid`) given with a view of the other passes
   the types. Three options:
   - (a) Interpreted reads are loads bounds-checked against the buffer. The
     mismatch is then memory-safe, but reads the copy's place in the other
     record and answers a wrong value on misuse. Cost: one bound test per
     interpreted read; the generated form is unaffected.
   - (b) The codec's id packed into the view beside its offset, and one
     compare per read raising `Invalid_argument`. Cost: offset bits, which on
     31-bit ints (js_of_ocaml, wasm_of_ocaml) narrow the buffers a view can
     address, against principle 5.
   - (c) A type-level brand per codec, `('c, 'r) Codec.t`, which a
     generative functor or an extra type parameter on `Codec.t` would give
     (the tip names a codec with a first-class module for this,
     wire.mli:1391-1423). Cost: every codec is named before its views are
     used, the step the model removes.

   How often it arises, at `ed6be0fc`, read from the 279 `.cmt` files of
   `dune build` (821 applications of `Codec.v`): 488 of them fall in 65
   groups of two or more codecs with one record type, record types with a
   type variable left out. All are in test/ and fuzz/, or share a tuple type
   with a test from another program (scripts/fstar-gen/proved.ml,
   bench/rust/headers.ml:8); no file of examples/, bench/, lib/ or scripts/
   holds two codecs of one record type, and the 34 codecs of examples/ have
   34 record types. `Codec.rename` has 7 callers (fuzz/gen/api_checks.ml:391,
   fuzz/gen/curated.ml:509, test/diff/gen_diff.ml:313 and 358,
   test/test_codec.ml:1104, 1120 and 1132); a renamed codec keeps the id and
   the layout of its source (lib/dsl.ml:341), so its copies and views agree.
   Settled by a ruling; the counts say how rarely the case arises outside
   the tests.

## Decisions it rests on

**No backward compatibility** (Thomas, 2026-11-26): "don't worry about
backward compat. Just design a safe and efficient and clean and beautiful
API."

**One generated module per codec** (Thomas, 2026-11-25): "No record/scanner
split: one generated module per codec." "No generic `enter` verb: a field's
accessor is named after the field; a scalar reads its value, a sub-record its
view, an optional sub-record a check. A sub-record's module is declared once
per codec."

**Names** (Thomas): "`View` (checked record), `Visitor` (the module receiving
fields, serde's term [...]). Don't invent new names." Since 2026-11-27 the
visitor is the fold's function `~f`, and every new name has a precedent
(below).

**Shapes** (orchestrator, 2026-11-25): "Every shape gets decode, encode, size
and set_<field> on a view; labelled write only where every field has a
constant size."

**The window of a check** (orchestrator, measured): "A codec of constant size:
`check b ~off` [...]; variable size: `check b ~off ~len`. Reason: the uniform
`~len` cost 5-6 instr per check with the natural window [...] (OxCaml apps
clcw 41.7 -> 47.9, gateway 627 -> 735 per frame)."

**Deleted beyond v1** (agreed): "`Buf.length`, `Buf.local` hidden"; "DSL
unchanged".

**The project's rules**, CLAUDE.md: Feature shippability, Untrusted input
("Bad input makes a decoder return `Error`, never raise
`Invalid_argument`"), Scalar type names, Tagged unions and the Benchmarking
principles ("Every optimisation lives in the code generator or the runtime,
never in the harness"). docs/execution.md, Invariants and Generated code
("OCaml without flambda never inlines a function passed as an argument",
execution.md:159-161).

**The model** (Thomas, 2026-11-27): the running example, the concepts in the
order above, and the two forms. "Interpreted" and "generated" name an
interpreter against a code generator, as in Carette, Kiselyov and Shan,
"Finally Tagless, Partially Evaluated", MetaOCaml and the Futamura
projections; never "twin", "dynamic" or "compiled", since ocamlopt compiles
both. What goes is decision 5 of that day: `Codec.get`/`set`/`writer`,
`Codec.Word` and `field_readers`; the EverParse plumbing, `Everparse.Raw`,
`Private` and the struct validators; `Wire_ocaml`'s `outputs`, `views`,
`Plan` and `fstar`; `Buf.length`/`Buf.local` and the runtime's helpers; the
typ-level I/O. `Codec.View` stays as the one interpreted in-place API.

**Names and their precedents** (Thomas, 2026-11-27; cited from memory, not
checked against the sources): `is_ok` as `Result.is_ok`; `get` as
`Option.get`, raising on the empty case; view as C++ `std::string_view`;
visitor as serde's `Visitor`; fold as `List.fold_left`, labelled `~init ~f`
as `ListLabels.fold_left` and Base's `List.fold`; snake_case accessors as
prost spells protobuf fields; `set_<field>` as protobuf's C++ `set_foo()`;
`check` and `validate` as EverParse's `<Base>Check<T>` wrapper (a yes or no,
lib/3d/check_wrapper.mli:1-6) and `<Mod>Validate<T>` validator; `sub` as
`Bytes.sub`; `size` the encoded size of a value and `fixed_size` the constant
size.

**The fold takes `~f` everywhere** (Thomas, 2026-11-27: "The alternative is
~f everywhere. -> yes"), and is called `fold` in both forms ("Space_packet.route
-> why `route` and not `fold` ?"). A generated module has at most one fold,
its fields declared in the generator. Measured on `bench/count.sh`, MAWI, 1M
packets, instructions per packet (branches `lane/fold-f`, `f01541ec`, and
`lane/fused-f`, `fb57955e`):

| Row | OCaml 5.4.1 classify | OCaml 5.4.1 parse | OxCaml classify | OxCaml parse |
|---|---:|---:|---:|---:|
| fused fold, visitor module named in the dune rule | 240 | 164 | 203 | 77 |
| fused fold taking `~f` (closure, fields positional): **chosen** | 503 | 409 | 205 | 88 |
| `fold ~f` where `f` gets a view and re-reads its fields | 359 | 261 | 258 | 172 |
| hand-written check/view loop | 326 | 238 | 259 | 174 |

On OxCaml the closure is inlined, and the fold costs what the fused fold with
a named visitor costs (classify 205 against 203, parse 88 against 77). On
stock OCaml 5.4.1 `f` is never inlined: each record is an indirect call
(`caml_apply29`, 22 arguments through the domain state), and the fold takes
about twice the instructions (classify 503 against 240, parse 409 against
164). This trades stock OCaml's fold speed for one API across both forms and
both compilers, and relaxes principle 3 for stock-OCaml folds. A visitor
given a view loses fusion on both compilers, so `f` gets the fields.

**The principles** the API is checked against:

1. **Bad input is a value**: a check, a decoder or an accessor answers a
   refused check or an `Error.t` for any bytes and never raises;
   `Invalid_argument` is misuse. Check: `make test`, `fuzz/` and the
   differential tests against the EverParse validators.
2. **An unchecked read is proved in bounds**: only a view, which only a check
   makes, reads without a bounds check, and only inside its record. Check:
   the `paranoid` profile and `verify/`.
3. **The obvious loop is the fast loop**: `check`, `is_ok`, `get` and the
   accessors, or a fold, are what a user writes and what the benchmarks time,
   on OCaml, flambda and OxCaml. On stock OCaml, a fold calls its `~f`
   indirectly per record and costs about twice a fused fold with a named
   visitor (the table above); that cost is accepted for one fold API. Check:
   `make bench-apps-count` and `bench/count.sh` on the MAWI rows.
4. **One way to do each thing**: one check, one accessor and one setter per
   field, one record write, one fold, one name per concept. Check: § What
   goes leaves no second path.
5. **Names state wire widths, and nothing assumes a 63-bit `int`**: `uint16`
   is two bytes, a carrier holds its field, and a module that needs more than
   31 bits refuses to load where ints are narrower. Check: `make test-wasm`.
