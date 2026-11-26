# The end-state API

This is the public API `wire` reaches once the phases of the one generated
module per codec land: the libraries `wire`, `wire.runtime`, `wire.ocaml`,
`wire.rust`, `wire.3d` and `wire.stubs`, the OCaml module `Wire_ocaml`
generates, its interpreted twin and the Rust module `Wire_rust` generates.
Nothing is kept for existing callers (Thomas, 2026-11-26: "don't worry about
backward compat. Just design a safe and efficient and clean and beautiful
API.").

It applies a design already agreed and records it here, where the agreed
text otherwise lives only in a session's scratch files. A section below
quotes each decision it rests on; a point no decision settles is an open
question, with the measurement that would settle it. The surface it starts
from is the tip of `wire-no-raise` (`307d233d`, #568).

## Decisions it rests on

**One generated module per codec** (Thomas, 2026-11-25): "No record/scanner
split: one generated module per codec." Its common part (the agreed sketch):

> ```ocaml
> module type S = sig                     (* Wire.S *)
>   type 'b view = private int
>   type 'b check = private int
>   val min_size : int
>   val check : 'b Buf.t -> off:int -> len:int -> 'b check   (* never raises *)
>   val ok : 'b check -> bool
>   val get : 'b check -> 'b view
>   val validate : bytes -> off:int -> len:int -> (unit, Error.t) result
> end
> ```
>
> No generic `enter` verb: a field's accessor is named after the field; a
> scalar reads its value, a sub-record its view, an optional sub-record a
> check. A sub-record's module is declared once per codec.

**Names** (Thomas): "`View` (checked record), `Visitor` (the module receiving
fields, serde's term; was consumer/step/plug). 'plug' renamed to 'visitor'
everywhere. Don't invent new names."

**The interpreted twin** (Thomas): "a functor over the codec value, no
`~schema` string [...] It lives in a sibling unit (links `wire`), so the
generated module links only `wire.runtime`. Its field handles come from the
codec value's field list, checked by position and kind plus the layout digest
when applied."

**Visitors and shapes** (orchestrator, 2026-11-25, on Thomas's "don't ask me,
experiment"): "Visitor named by its file in the dune rule [...] Not
`__FUNCTION__`." "Every shape gets decode, encode, size and set_<field> on a
view; labelled write only where every field has a constant size."

**The window of a check** (orchestrator, measured): "A codec of constant size:
`check b ~off` (record in the buffer from off, the old View.check contract);
variable size: `check b ~off ~len`. Reason: the uniform `~len` cost 5-6 instr
per check with the natural window [...] (OxCaml apps clcw 41.7 -> 47.9,
gateway 627 -> 735 per frame)."

**Deleted beyond v1** (agreed): the scanner's output record, its `decode`,
`box`, `decode_unboxed`; every fold variant; the record modules' `Get`,
`Value`, `Valid`, `View.at`, `View.with_`, `is_valid`, `view_*`, `read_*`,
`*_int`, `*_word`, `*_of_word`, `Field` handles, `Global`, `Bigstring`;
`Record.S` and `Scan.S` become one `S`; "`Buf.length`, `Buf.local` hidden";
`Codec.Make`, `Gen.Outputs`, `Gen.Scan`, `Gen.field`, `Gen.kind`. From the
first design review, carried over: `Staged`; the sizes collapse to `size`,
`min_size`, `size_at`, `encoded_size`; `field_ref`, `field_readers`,
`bitfield`/`load_word`/`extract`, the validator family and the `'r codec`
alias go; `Everparse.Raw` and `Private` move to an internal library; "DSL
unchanged".

**The project's rules**, CLAUDE.md: Feature shippability ("A shape that
cannot project is rejected when it is built [...] the combinator raises
`Invalid_argument`"), Untrusted input ("Bad input makes a decoder return
`Error`, never raise `Invalid_argument`"), Scalar type names ("A numbered
scalar type name states the field's width on the wire, never the OCaml
carrier"), Tagged unions (`variants` and `casetype` stay separate) and the
Benchmarking principles ("Benchmark loops are what a user of each library
writes [...] Every optimisation lives in the code generator or the runtime,
never in the harness"). docs/execution.md, Invariants ("A read without a
bounds check is created only for a field whose offset and width are constants
of the schema [...] and a view is made only of a record whose frame lies in
the buffer"; "Input never raises `Invalid_argument`") and Generated code
("OCaml without flambda never inlines a function passed as an argument").
docs/projection.md, [Narrow-int targets](projection.md#narrow-int-targets).

## Principles

1. **Bad input is a value**: a check, a decoder or an accessor answers a refused check or an `Error.t` for any bytes and never raises; `Invalid_argument` is misuse (a negative offset, a codec that cannot project, a value its field cannot hold). Check: `make test`, `fuzz/` and the differential lanes against the EverParse validators.
2. **An unchecked read is proved in bounds**: only a view, which only a check makes, reads without a bounds check, and only the leading prefix of its record. Check: the `paranoid` profile and `verify/`'s Z3 proof of the packet-stack scanner.
3. **The obvious loop is the fast loop**: a declared fold calling its visitor by name, or `check`, `ok`, `get` and the accessors, is what a user writes and what the benchmarks time, on OCaml, flambda and OxCaml. Check: `make bench-apps-count` and `bench/count.sh` on the MAWI rows, against the table below.
4. **One way to do each thing**: one check, one accessor and one setter per field, one record write, one fold form, one name per concept (`View`, `Visitor`, `check`/`ok`/`get`, `Wire.Error`). Check: § What goes leaves no second path.
5. **Names state wire widths, and nothing assumes a 63-bit `int`**: `uint16` is two bytes, a carrier holds its field, and a module that needs more than 31 bits refuses to load where ints are narrower. Check: `make test-wasm`.

The measured table behind 3 (MAWI, 1M packets, `bench/count.sh`, median of
5, instructions / cycles per packet, parse only; the design measurement of
2026-11-25, then bench/README.md "The generated views" for the current tree):

| Row | OCaml 5.4.1 | flambda | OxCaml |
|---|---:|---:|---:|
| scanner `fold_step`, at the design | 299.6 / 73.1 | 144.7 / 41.3 | 73.2 / 29.5 |
| scanner `fold_step`, current tree | 150.0 / 43.1 | 144.6 / 42.0 | 73.2 / 30.0 |
| check/view, accessors, current tree | 237.1 / 51.6 | 244.0 / 53.0 | 174.3 / 43.1 |
| the header modules' `View.check`, linked headers | 222.2 / 48.8 | 250.1 / 52.7 | 197.9 / 48.8 |
| prototype, check + enter, re-read | 230.0 / 50.7 | 233.7 / 52.8 | 213.0 / 50.2 |

Rust, counted alike: the `wire.rust` scanner 68.7 / 23.0, `pnet_packet 0.35`
66.7 / 22.5. A fold is the fast path of a whole-frame scan on all three
compilers (check and accessors spend 1.6 to 2.4 times its instructions);
check and accessors are the path of a loop that decides per record, and cost
about what the linked headers cost (finding 3 of the design: moving header linking
into the generated accessors is free).

## Signatures

Each block is the interface, abbreviated only where it says so. One doc line
per value; the `.mli` carries the full text.

### `wire`: the DSL

Unchanged from the tip ("DSL unchanged"), but for the types that named the
internal `Types` (`Field.action`, `seq_map`, `case_def`, now stated here) and
`Field.packed`, which goes.

```ocaml
type 'a expr                  (** An expression of the wire description. *)
type 'a typ                   (** A wire type with OCaml values ['a]. *)
type bitfield = U8 | U16 | U16be | U32 | U32be
                              (** The base word of a {!bits} slice. *)
type bit_order = Msb_first | Lsb_first   (** Which end the first slice takes. *)
type endian = Little | Big    (** Byte order of {!uint}. *)
type consumption = [ `Prefix | `All ]    (** What a decoder does after one value. *)
type param                    (** A parameter, untyped. *)
type ('elt, 'seq) seq_map =
  | Seq_map : {
      empty : 'b; add : 'b -> 'elt -> 'b; finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    } -> ('elt, 'seq) seq_map  (** How elements make a sequence. *)
type ('a, 'k) case_def        (** A case of a tagged union. *)

module Param : sig
  type input                  (** The kind of an input parameter. *)
  type output                 (** The kind of an output parameter. *)
  type ('a, 'k) t             (** A parameter of type ['a] and kind ['k]. *)
  type env                    (** The values of a codec's parameters. *)
  val input : string -> 'a typ -> ('a, input) t      (** [input n t] is an input parameter. *)
  val output : string -> 'a typ -> ('a, output) t    (** [output n t] is an output parameter. *)
  val decl : ('a, 'k) t -> param                     (** [decl p] is [p], untyped. *)
  val name : ('a, 'k) t -> string                    (** [name p] is [p]'s name. *)
  val bind : ('a, input) t -> 'a -> env -> env       (** [bind p v env] sets [p] to [v]. *)
  val bind_by_name : string -> int -> env -> env     (** [bind_by_name n v env] sets [n] to [v]. *)
  val get : env -> ('a, 'k) t -> 'a                  (** [get env p] is [p]'s value. *)
  val expr : ('a, 'k) t -> int expr                  (** [expr p] is [p] in an expression. *)
  val pp : Format.formatter -> ('a, 'k) t -> unit    (** [pp] prints a parameter's name. *)
end

module Action : sig
  type t                                             (** A field action. *)
  type stmt                                          (** A statement of an action. *)
  val on_success : stmt list -> t                    (** Runs once the field is read. *)
  val on_act : stmt list -> t                        (** Runs as the field is read. *)
  val assign : ('a, Param.output) Param.t -> int expr -> stmt  (** Sets an output. *)
  val return_bool : bool expr -> stmt                (** Accepts or refuses the field. *)
  val abort : stmt                                   (** Refuses the field. *)
  val if_ : bool expr -> stmt list -> stmt list option -> stmt  (** A conditional. *)
  val var : string -> int expr -> stmt               (** Binds a local. *)
  val pp : Format.formatter -> t -> unit             (** [pp] prints an action. *)
end

val int : int -> int expr                            (** [int n] is the literal [n]. *)
val sizeof : 'a typ -> int expr                      (** [sizeof t] is [t]'s constant size. *)
val sizeof_this : int expr                           (** The bytes read so far in the record. *)
val field_pos : int expr                             (** The offset of the field being read. *)

module Expr : sig
  (* Unchanged: int64, + - * / mod land lor lxor lnot lsl lsr, land64, lsr64,
     = <> < <= > >= && || not, if_then_else, true_, false_, bool, to_uint8,
     to_uint16, to_uint32, to_uint64, each with its one-line doc. *)
end

module UInt8 = Wire_runtime.UInt8     (** § wire.runtime. *)
module UInt16 = Wire_runtime.UInt16
module UInt32 = Wire_runtime.UInt32
module UInt64 = Wire_runtime.UInt64   (** Moves to the runtime: see § Gaps. *)
module SInt8 = Wire_runtime.SInt8
module SInt16 = Wire_runtime.SInt16
module SInt32 = Wire_runtime.SInt32
module Buf = Wire_runtime.Buf
module Error = Wire_runtime.Error
module type S = Wire_runtime.S

module Field : sig
  type 'a t                                          (** A named field of values ['a]. *)
  type 'a anon                                       (** An anonymous (padding) field. *)
  val v :
    string -> ?constraint_:bool expr -> ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) -> ?action:Action.t -> ?doc:string ->
    'a typ -> 'a t                                   (** [v name t] is a field. *)
  val optional :
    string -> ?constraint_:bool expr -> ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) -> ?action:Action.t -> present:bool expr ->
    'a typ -> 'a option t                            (** Present iff [present]. *)
  val optional_or :
    string -> ?constraint_:bool expr -> ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) -> ?action:Action.t -> present:bool expr ->
    default:'a -> 'a typ -> 'a t                     (** [default] where absent. *)
  val repeat :
    string -> ?constraint_:bool expr -> ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) -> ?action:Action.t -> size:int expr ->
    'a typ -> 'a list t                              (** Elements filling [size] bytes. *)
  val repeat_seq :
    string -> ?constraint_:bool expr -> ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) -> ?action:Action.t ->
    seq:('a, 'seq) seq_map -> size:int expr -> 'a typ -> 'seq t   (** {!repeat} into ['seq]. *)
  val union :
    string -> ?action:Action.t -> ?doc:string -> on:'k t -> casetype:string ->
    ('a, 'k) case_def list -> 'a t                   (** One case, chosen by [on]. *)
  val anon : 'a typ -> 'a anon                       (** [anon t] is padding of type [t]. *)
  val ref : 'a t -> int expr                         (** [ref f] is [f] in an expression. *)
  val int : 'a t -> int expr                         (** [int f] is [f] as an [int]. *)
  val int64 : 'a t -> int64 expr                     (** [int64 f] is [f] on 64 bits. *)
  val name : 'a t -> string                          (** [name f] is [f]'s name. *)
  val typ : 'a t -> 'a typ                           (** [typ f] is [f]'s type. *)
  val constraint_ : 'a t -> bool expr option         (** [f]'s constraint. *)
  val action : 'a t -> Action.t option               (** [f]'s action (was [Types.action]). *)
  val doc : 'a t -> string option                    (** [f]'s note. *)
  val pp : Format.formatter -> 'a t -> unit          (** [pp] prints [f]'s name. *)
end

(* Types: each one-line doc states its width and range, as the tip does. *)
val uint8 : UInt8.t typ            val uint16 : UInt16.t typ      val uint16be : UInt16.t typ
val uint32 : UInt32.t typ          val uint32be : UInt32.t typ    val uint64 : UInt64.t typ
val uint64be : UInt64.t typ        val int8 : SInt8.t typ         val int16 : SInt16.t typ
val int16be : SInt16.t typ         val int32 : SInt32.t typ       val int32be : SInt32.t typ
val int64 : int64 typ              val int64be : int64 typ        val float32 : float typ
val float32be : float typ          val float64 : float typ        val float64be : float typ
val uint : ?endian:endian -> int -> Optint.Int63.t typ         (** [uint n]: [n] bytes, 1 to 7. *)
val uint_var : ?endian:endian -> int expr -> Optint.Int63.t typ (** A width read from the data. *)
val bits : ?bit_order:bit_order -> width:int -> bitfield -> int typ  (** [width] bits of a word. *)
val is_finite : float Field.t -> bool expr                     (** The field is finite. *)
val is_nan : float Field.t -> bool expr                        (** The field is a NaN. *)
val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w typ -> 'a typ   (** A view of ['w] as ['a]. *)
val bool : bool -> bool expr                                   (** A boolean literal. *)
val bit : 'a typ -> bool typ                                   (** A one-bit slice as a [bool]. *)
val lookup : 'a list -> 'b typ -> 'a typ                       (** An index into a table. *)
val empty : unit typ                                           (** No bytes. *)
val all_bytes : string typ                                     (** The rest of the input. *)
val all_zeros : string typ                                     (** The rest, all zero. *)
val zeroterm : string typ                                      (** Bytes up to a zero. *)
val zeroterm_at_most : size:int expr -> string typ             (** {!zeroterm} within [size]. *)
val where : bool expr -> 'a typ -> 'a typ                      (** [t] refined by a predicate. *)
val seq_list : ('a, 'a list) seq_map                           (** Elements as a list. *)
val array : len:int expr -> 'a typ -> 'a list typ              (** [len] elements. *)
val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ  (** {!array} into ['seq]. *)
val byte_array : size:int expr -> string typ                   (** [size] bytes, copied. *)
val byte_array_where : size:int expr -> per_byte:(int expr -> bool expr) -> string typ
                                                               (** Bytes each passing a test. *)
val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ    (** [size] bytes, in place. *)
val rest_bytes : (_, _) Param.t -> string typ                  (** Bytes up to a parameter's end. *)
val nested : size:int expr -> 'a typ -> 'a typ                 (** [t] in exactly [size] bytes. *)
val nested_at_most : size:int expr -> 'a typ -> 'a typ         (** [t] in at most [size] bytes. *)
val enum : string -> (string * int) list -> 'a typ -> 'a typ   (** A closed enumeration. *)
val enum_open : string -> (string * int) list -> 'a typ -> 'a typ  (** An open enumeration. *)
val variants : string -> (string * 'a) list -> 'b typ -> 'a typ    (** Values by position. *)
val case : ?index:'k -> 'w typ -> inject:('w -> 'a) -> project:('a -> 'w option) -> ('a, 'k) case_def
                                                               (** The body of tag [index]. *)
val default : 'w typ -> inject:('k -> 'w -> 'a) -> project:('a -> ('k * 'w) option) -> ('a, 'k) case_def
                                                               (** The body of any other tag. *)
val casetype : string -> 'k typ -> ('a, 'k) case_def list -> 'a typ  (** A tag, then its body. *)
val size : 'a typ -> int option                                (** [t]'s constant size. *)

val of_reader : 'a typ -> Bytesrw.Bytes.Reader.t -> ('a, Error.t) result   (** One value. *)
val of_string : ?consume:consumption -> 'a typ -> string -> ('a, Error.t) result
val of_bytes : ?consume:consumption -> 'a typ -> bytes -> ('a, Error.t) result
val to_writer : 'a typ -> 'a -> Bytesrw.Bytes.Writer.t -> unit             (** One value. *)
val to_bytes : 'a typ -> 'a -> bytes
val to_string : 'a typ -> 'a -> string
```

The types refer to `Codec.t` through `codec`, which follows the `Codec`
module below. The `'r codec` type alias goes; `codec` is the value only.

### `wire`: `Codec` and `Codec.View`

```ocaml
module Codec : sig
  type 'r t                                          (** A codec of records ['r]. *)
  type ('a, 'r) field                                (** A field bound to ['r]. *)
  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields
  type ('f, 'r) args =
    | [] : (unit, 'r) args
    | ( :: ) : ('a, 'r) field * ('f, 'r) args -> ('a -> 'f, 'r) args

  val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field    (** [f $ proj] binds [f]. *)
  val v : string -> ?where:bool expr -> ?doc:string -> 'f -> ('f, 'r) fields -> 'r t
                                                     (** [v name make fields] is a codec. *)
  val rename : string -> 'r t -> 'r t                (** [rename n c] is [c] named [n]. *)
  val name : 'r t -> string                          (** [name c] is [c]'s name. *)
  val doc : 'r t -> string option                    (** [doc c] is [c]'s note. *)
  val min_size : 'r t -> int                         (** The fewest bytes a record takes. *)
  val size : 'r t -> int option                      (** The bytes of every record, if constant. *)
  val size_at : ?env:Param.env -> 'r t -> bytes -> int -> (int, Error.t) result
                                                     (** The size of the record at [off]. *)
  val encoded_size : ?env:Param.env -> 'r t -> 'r -> int   (** The bytes {!encode} writes. *)
  val env : 'r t -> Param.env                        (** A fresh environment for [c]. *)

  val decode :
    ?consume:consumption -> ?env:Param.env -> 'r t -> bytes -> int -> ('r, Error.t) result
                                                     (** The record at [off], or its error. *)
  val validate : ?env:Param.env -> 'r t -> bytes -> int -> (unit, Error.t) result
                                                     (** The record's error, if any. *)
  val encode : ?env:Param.env -> 'r t -> 'r -> bytes -> int -> unit
                                                     (** Writes [r] at [off]. *)
  val to_bytes : ?env:Param.env -> 'r t -> 'r -> bytes      (** [r], encoded and checked. *)
  val to_string : ?env:Param.env -> 'r t -> 'r -> string    (** [r], encoded and checked. *)
  val pp_value : 'r t -> 'r Fmt.t                    (** Prints a record field by field. *)

  val get : ?env:Param.env -> 'r t -> ('a, 'r) field -> bytes -> int -> ('a, Error.t) result
                                                     (** One field, for bytes the program wrote. *)
  val set : ?env:Param.env -> 'r t -> ('a, 'r) field -> bytes -> int -> 'a -> unit
                                                     (** Writes one field in place. *)
  val writer : 'r t -> 'r -> ('f, 'r) args -> bytes -> int -> 'f
                                                     (** Writes a template with [args] set. *)

  (** {1 Word reads (until Q11)} *)

  type bitfield                                      (** A slice of a loaded word. *)
  val bitfield : 'r t -> (int, 'r) field -> bitfield (** [bitfield c f] is [f]'s slice. *)
  val load_word : bitfield -> bytes -> int -> (Optint.t, Error.t) result
                                                     (** The word [f] is a slice of. *)
  val extract : bitfield -> Optint.t -> int          (** [f]'s bits of a word. *)
  type 'r word = private int                         (** A record of [c] in an [int]. *)
  type 'r word_reader                                (** A reader of records as words. *)
  val word_reader : 'r t -> 'r word_reader           (** Raises on a record over 7 bytes. *)
  val read_word : 'r word_reader -> bytes -> int -> ('r word, Error.t) result
                                                     (** The record at [off]: the read-word fix. *)
  type ('a, 'r) part                                 (** A field of a word. *)
  val word_part : 'r t -> ('a, 'r) field -> ('a, 'r) part   (** [f] in the words. *)
  val of_word : ('a, 'r) part -> 'r word -> 'a       (** [p]'s value in [w]. *)

  module View : sig
    type 'r record_codec := 'r t
    type ('a, 'r) record_field := ('a, 'r) field

    type ('c, 'r) codec                              (** A codec of ['r] named ['c]. *)
    module type CODEC = sig
      type c                                         (** The name of {!val-codec}. *)
      type r                                         (** Its records. *)
      val codec : (c, r) codec                       (** The named codec. *)
    end
    val codec : ?env:Param.env -> 'r record_codec -> (module CODEC with type r = 'r)
                                                     (** [c] under a name of its own. *)

    type ('b, 'c) check [@@immediate]                (** A check in the buffer ['b]. *)
    val check : ('c, 'r) codec -> 'b Buf.t -> int -> ('b, 'c) check
                                                     (** Checks the record at [off]; never raises. *)
    val ok : ('b, 'c) check -> bool                  (** [ok k] iff [k] passed. *)
    type ('b, 'c) t [@@immediate]                    (** A record that passed in ['b]. *)
    val get : ('b, 'c) check -> ('b, 'c) t           (** Raises [Invalid_argument] unless [ok]. *)

    type ('a, 'c) field                              (** A field of the views of ['c]. *)
    val field : ('c, 'r) codec -> ('a, 'r) record_field -> ('a, 'c) field
                                                     (** A field read with no check. *)
    val checked_field :
      ('c, 'r) codec -> ('a, 'r) record_field -> (('a, Error.t) result, 'c) field
                                                     (** Any other field, read as {!get}. *)
    val read : ('a, 'c) field -> 'b Buf.t -> ('b, 'c) t -> 'a   (** Never raises. *)

    type ('cs, 'c) sub                               (** A field holding a ['cs] record. *)
    val sub : ('c, 'r) codec -> ('cs, 's) codec -> ('s, 'r) record_field -> ('cs, 'c) sub
    val sub_option :
      ('c, 'r) codec -> ('cs, 's) codec -> ('s option, 'r) record_field -> ('cs, 'c) sub
    val enter : ('cs, 'c) sub -> 'b Buf.t -> ('b, 'c) t -> ('b, 'cs) check
                                                     (** The sub-record, with no check again. *)
  end
end

val codec : 'r Codec.t -> 'r typ                     (** A codec as a field type. *)
```

`get`, `set`, `writer`, `decode` and `encode` compile their codec when
applied to it, so `let get = Codec.get c f in` is the staged reader and no
`Staged.t` remains. `Codec.get` and `Codec.set` stay for "a lone field"
(CLAUDE.md, Benchmarking principles); on untrusted input the reader is
`View`. With [Q9](#q9) answered as agreed, `View.check` and `View.enter` read
the buffer's length from `'b Buf.t`.

### `wire`: `Everparse` and `Ascii`

```ocaml
module Everparse : sig
  type t                                             (** A 3D schema. *)
  type mode = [ `Ffi | `Standalone ]                 (** With or without the FFI visitor. *)
  val project : ?mode:mode -> 'r Codec.t -> t        (** [c]'s 3D schema. *)
  val name : t -> string                             (** The schema's name. *)
  val filename : t -> string                         (** Its [.3d] file name. *)
  val write : ?mode:mode -> outdir:string -> ?name:string -> t list -> unit
                                                     (** Writes the [.3d] files. *)
end

module Ascii : sig
  val of_codec : 'r Codec.t -> string                (** [c]'s bit diagram. *)
  val pp_codec : Format.formatter -> 'r Codec.t -> unit   (** Prints {!of_codec}. *)
end
```

### `wire.runtime`

What generated code links, and what `wire` re-exports. Its documented
surface is `Buf`, `Error`, the carriers, `Vec` and `S`; the modules
generated code calls (`Unchecked`, `Errors`, `Arith`, `Target`) and each
carrier's unchecked loads are compiled in but hidden, `(**/**)` or a unit the
main module does not re-export (house style: "the escape hatch is named
`unsafe_` and hidden").

```ocaml
module Buf : sig
  type 'b t = private bytes                          (** A buffer named ['b]. *)
  type any = Any : 'b t -> any [@@unboxed]           (** A buffer under a fresh name. *)
  val v : bytes -> any                               (** [v buf] names [buf]. *)
  val pp : Format.formatter -> 'b t -> unit          (** Prints the length, not the bytes. *)
end

module Error : sig
  type predicate = Where | Field | Action | Per_byte (** Where a failed constraint comes from. *)
  type kind =
    | Unexpected_eof of { expected : int; got : int }
    | Trailing_bytes of int
    | Invalid_enum of { value : int; valid : int list }
    | Invalid_tag of int
    | Missing_terminator
    | Non_zero_padding
    | Value_out_of_range of { value : int64 }
    | Zero_divisor
    | Constraint_failed of { which : predicate; value : int64 option }
                                                     (** Why input is refused. *)
  type t = { at : int; field : string list; kind : kind }   (** Where and why. *)
  val pp : Format.formatter -> t -> unit             (** Prints [len@4: ...]. *)
  val pp_kind : Format.formatter -> kind -> unit     (** Prints a kind alone. *)
  val equal : t -> t -> bool                         (** Equality. *)
  val compare : t -> t -> int                        (** A total order. *)
end

module UInt16 : sig
  type t = private int                               (** 0 to 65535. *)
  val v : int -> t                                   (** Raises [Invalid_argument] out of range. *)
  val v_opt : int -> t option                        (** [None] out of range. *)
  val to_int : t -> int                              (** The value. *)
  val zero : t                                       (** 0. *)
  val min_int : t                                    (** 0. *)
  val max_int : t                                    (** 65535. *)
  val equal : t -> t -> bool                         (** Equality. *)
  val compare : t -> t -> int                        (** Unsigned order. *)
  val pp : Format.formatter -> t -> unit             (** Prints the value. *)
end
(* UInt8, SInt8 and SInt16: the same signature over their range.
   UInt32, SInt32 and UInt64: [type t] abstract (a 32-bit value is no [int]
   where ints have 31 or 32 bits), the same [v], [v_opt], [zero], [min_int],
   [max_int], [equal], [compare], [pp], plus [to_int32]/[of_int32] or
   [to_int64]/[of_int64], and [to_int_opt]; [to_int] is Q12. *)

module Vec : sig
  (* The SIMD lanes of [X4] (OxCaml on arm64 and amd64, empty elsewhere): [t],
     [load], [store], [add], [sub], [logand], [logor], [equal], [is_zero],
     [shift_right_logical], [shift_left], [v], [v4], [get], [before], [sum],
     [is_all_zero], each with its one-line doc as at the tip. *)
end

module type S = sig
  type 'b view = private int                         (** A record that passed in ['b]. *)
  type 'b check = private int                        (** The answer of a check in ['b]. *)
  val min_size : int                                 (** The fewest bytes a record takes. *)
  val ok : 'b check -> bool                          (** [ok c] iff [c] passed. *)
  val get : 'b check -> 'b view                      (** Raises [Invalid_argument] unless [ok]. *)
  val validate : bytes -> int -> (unit, Error.t) result   (** The record's error, if any. *)
end
```

`check` is not in `S`: its window depends on the shape (the orchestrator's
measured decision above), so each module states it. `validate` takes `buf
off` as `Codec.validate` does: the record lies between `off` and the end of
`buf`, where the sketch had `~off ~len`.

### The generated module

`Wire_ocaml` writes one module per codec, `<name>.ml` and `<name>.mli`,
linking `wire.runtime` only (and `bytesrw` for a byte slice, `wire` for a
map). It satisfies `Wire.S` (a test states `module _ : Wire.S = Clcw`) and
adds members by shape. Shown for `Packet_stack.capture` with a fold
`classify` whose visitor is `flow_step.ml`, and for the CLCW record; `Buf` is
`Wire_runtime.Buf`.

```ocaml
(* Every module: Wire.S, the record values and a member per named field. *)
include Wire.S

type t = {
  dst : Bytesrw.Bytes.Slice.t; src : Bytesrw.Bytes.Slice.t;
  ether_type : Wire_runtime.UInt16.t; ipv4 : Captured_ipv4.t option;
}                                                    (** The codec's records. *)
val pp : Format.formatter -> t -> unit               (** Prints a record. *)
val size : t -> int                                  (** The bytes {!encode} writes. *)
val decode : bytes -> int -> (t, Wire.Error.t) result    (** The record at [off]. *)
val encode : t -> bytes -> int -> unit               (** Writes [r]; raises on values it refuses. *)

val ether_type : 'b Buf.t -> 'b view -> Wire_runtime.UInt16.t   (** One load. *)
val dst : 'b Buf.t -> 'b view -> Bytesrw.Bytes.Slice.t          (** A slice of [b]. *)
val ipv4 : 'b Buf.t -> 'b view -> 'b Captured_ipv4.check        (** Present and in [b]. *)
val set_ether_type : 'b Buf.t -> 'b view -> Wire_runtime.UInt16.t -> unit
                                                     (** One store; no constraint checked. *)

(* A codec of variable size: the window is the frame's. *)
val check : 'b Buf.t -> off:int -> len:int -> 'b check    (** Never raises. *)

(* One module per codec a field holds, named after it, written once. *)
module Captured_ipv4 : sig
  type 'b view = private int                         (** An IPv4 header that passed. *)
  type 'b check = private int                        (** Its presence and place in [b]. *)
  val ok : 'b check -> bool                          (** [ok c] iff present and in [b]. *)
  val get : 'b check -> 'b view                      (** Raises [Invalid_argument] unless [ok]. *)
  val protocol : 'b Buf.t -> 'b view -> Wire_runtime.UInt8.t     (** One load. *)
  val src : 'b Buf.t -> 'b view -> Wire_runtime.UInt32.t         (** One load. *)
  val tcp : 'b Buf.t -> 'b view -> 'b Stack_tcp.check            (** No check made again. *)
  val opaque_present : 'b Buf.t -> 'b view -> bool               (** An optional's presence. *)
  val set_protocol : 'b Buf.t -> 'b view -> Wire_runtime.UInt8.t -> unit
  type t = { (* ... *) }                             (** The header's values. *)
  val pp : Format.formatter -> t -> unit
  val size : t -> int
end

(* Each declared fold [n], calling its visitor by name. *)
val classify : bytes -> int -> int -> init:Flow_step.t -> Flow_step.t
                                                     (** One frame. *)
val classify_frames : bytes -> int -> int -> stride:int -> init:Flow_step.t -> Flow_step.t
                                                     (** [len / stride] frames. *)
val classify_index :
  bytes -> offs:int array -> lens:int array -> init:Flow_step.t -> Flow_step.t
                                                     (** The frames an index lists. *)
```

```ocaml
(* A codec of constant size (CLCW) instead has: *)
val check : 'b Buf.t -> int -> 'b check              (** The record at [off]; never raises. *)
val write :
  bytes -> int -> controlwordtype:int -> clcwversion:int -> (* ... *) reportvalue:int -> unit
                                                     (** Writes a record; raises on a bitfield
                                                         out of its range. *)

(* With [~simd:true], OxCaml on arm64 or amd64, a 4-byte record of unsigned
   one-byte fields also has [module X4] (Q13). *)
```

```ocaml
(* The visitor of a fold: a source file the dune rule names,
   [--visitor classify=%{dep:flow_step.ml}]. *)
type t                                               (** The accumulator. *)
val valid : t -> ipv4_present:bool -> ipv4_src:int64 -> (* ... *) -> t
                                                     (** A record that passed: its outputs. *)
val invalid : t -> t                                 (** A record that failed. *)
```

A field the walk places again (after a span, under a gate) is read after a
test against the buffer's length, and reads as absent (0, `false`, an empty
slice) when bytes written after the check move it out of the buffer
(execution.md, Invariants). An integer is returned as its carrier, a bitfield
as an `int`. An accessor whose name the module already defines takes a
trailing `_`.

### The interpreted twin

`Wire_ocaml.main` writes `<name>_codec.ml` and `.mli` beside the module. It
links `wire`.

```ocaml
module Make (C : sig
  type r                                             (** The codec's records. *)
  val codec : r Wire.Codec.t                         (** The codec the module came from. *)
end) : sig
  include Wire.S
  (* The module's [check], accessors, setters, sub-record modules and folds,
     with the types the module has for the [`OCaml] target, read through
     [Wire.Codec.View]. Not the record values: [t], [decode], [encode],
     [size] and [write] build the module's own record type, which the twin
     cannot make from [C.r]. *)
end
(** Raises [Invalid_argument] when applied to a codec whose shape is not the
    one the module was generated from: each field by place and kind, and the
    digest of the layout. *)
```

Code written against the views runs on either by one line:
`module P = Captured_stack` or
`module P = Captured_stack_codec.Make (struct type r = Packet_stack.captured let codec = Packet_stack.capture end)`.
The decision's example omits `type r`; the functor needs it to type `codec`.

### `Wire_ocaml`

```ocaml
type target = [ `OCaml | `OxCaml ]                   (** The compiler the code is for. *)
type map                                             (** A map called by its OCaml path. *)
val map : string -> typ:string -> 'a Wire.typ -> map (** [map path ~typ m]. *)
type fold                                            (** A fold a module declares. *)
val fold : ?outputs:string list -> string -> fold    (** [fold name], every output by default. *)
type t                                               (** A module to generate. *)
val v :
  ?name:string -> ?target:target -> ?simd:bool -> ?folds:fold list -> ?maps:map list ->
  'r Wire.Codec.t -> t                               (** The module of [c]; [folds] default [[]]. *)
val name : t -> string                               (** Its unit name. *)
val to_ml : ?twin:bool -> ?visitors:(string * string) list -> t -> string
                                                     (** Its source, or its twin's. *)
val to_mli : ?twin:bool -> ?visitors:(string * string) list -> t -> string
                                                     (** Its interface, or its twin's. *)
val main : t list -> unit                            (** [gen.exe [--visitor F=FILE]... FILE...]. *)
val outputs : 'r Wire.Codec.t -> (string * bool) list
                                                     (** The outputs a fold hands out. *)
val fstar : ?fields:string list -> codec:string -> 'r Wire.Codec.t -> (string * string) list
                                                     (** The F* proof of its folds. *)
```

`v` raises `Invalid_argument`, naming the field, for a shape the walk does not
cover (Feature shippability); `to_ml` raises it for a declared fold that no
visitor names, so every fold calls its visitor by name. `fstar` states its
theorem over the visitor form (`StepProof`, `OxStepProof`), the closure form
having gone.

### `Wire_rust` and the Rust module

```ocaml
type fold                                            (** A fold a module declares. *)
val fold : ?outputs:string list -> string -> fold    (** As {!Wire_ocaml.fold}. *)
type t                                               (** A module to generate. *)
val v : ?name:string -> ?folds:fold list -> 'r Wire.Codec.t -> t
                                                     (** The module of [c], as {!Wire_ocaml.v}. *)
val name : t -> string                               (** Its file is [name t ^ ".rs"]. *)
val to_rs : ?visitors:(string * string) list -> t -> string
                                                     (** Its source; a visitor is a Rust type. *)
val main : t list -> unit                            (** [gen.exe [--visitor F=TYPE]... FILE...]. *)
```

```rust
// The module of Packet_stack.capture; no_std, no unsafe code, no panic.
pub const MIN_SIZE: usize;
pub struct Error { pub at: usize, pub field: Path, pub kind: Kind }  // Wire.Error.t
pub fn check(buf: &[u8], off: usize, len: usize) -> Check<'_>;      // constant size: (buf, off)
pub struct Check<'a>;  impl Check<'a> { pub fn ok(&self) -> bool; pub fn get(self) -> Option<View<'a>>; }
pub struct View<'a>;   impl View<'a> {
    pub fn ether_type(&self) -> u16;                                 // one load
    pub fn ipv4(&self) -> captured_ipv4::Check<'a>;                  // no check made again
}
pub fn offset(v: &View<'_>) -> usize;
pub fn set_ether_type(buf: &mut [u8], at: usize, x: u16) -> Result<(), Error>;
pub struct Record<'a> { /* each field, a slice borrowing buf */ }
pub fn validate(buf: &[u8], off: usize) -> Result<(), Error>;
pub fn decode(buf: &[u8], off: usize) -> Result<Record<'_>, Error>;
pub fn encode(r: &Record<'_>, buf: &mut [u8], off: usize) -> Result<(), Error>;
pub fn size(r: &Record<'_>) -> Result<usize, Error>;
pub mod classify { pub trait Visitor { fn valid(&mut self /* , outputs */); fn invalid(&mut self); } }
pub fn classify<V: classify::Visitor>(v: &mut V, buf: &[u8], off: usize, len: usize);
pub fn classify_frames<V: classify::Visitor>(v: &mut V, buf: &[u8], off: usize, len: usize, stride: usize) -> bool;
pub fn classify_index<V: classify::Visitor>(v: &mut V, buf: &[u8], offs: &[usize], lens: &[usize]) -> bool;
pub mod captured_ipv4 { /* Check, View, set_<field>, Record, size */ }
```

A `Record` literal names each field, as the labels of the OCaml `write` do, so
`encode` takes the place of `write`. A view borrows its buffer, so a setter
takes the buffer and the view's `offset`. The module has no `CODEC` and no
`DIGEST`, as the OCaml module has no `digest`.

### `Wire_3d` and `Wire_stubs`

```ocaml
(* Wire_3d *)
type packed                                          (** A codec, its type erased. *)
val pack : 'a Wire.Codec.t -> packed                 (** [pack c]. *)
val run : ?quiet:bool -> outdir:string -> Wire.Everparse.t list -> unit
                                                     (** [.3d] files, EverParse, C validators. *)
val main : ?name:string -> mode:[ `Ffi | `Standalone ] -> package:string -> packed list -> unit
                                                     (** The generator a dune rule runs. *)
val parse_3d : ?batch:bool -> outdir:string -> string -> (unit, string) result
                                                     (** Whether 3D accepts a [.3d] file. *)
val has_3d_exe : unit -> bool                        (** Whether EverParse is installed. *)

(* Wire_stubs *)
val generate : schema_dir:string -> outdir:string -> Wire_3d.packed list -> unit
                                                     (** OCaml bindings to the C validators. *)
val build_codec_archive :
  ?cflags:string list -> schema_dir:string -> archive:string -> unit -> unit
                                                     (** Archives the validators for dune. *)
```

A generated stub answers `(record, Wire.Error.t) result` where the tip raises
`Failure` on a refused record (§ Gaps).

### Internal

One library of the package, outside the documented API, holds what the
libraries share and what tests, fuzzers and benchmarks reach into:
`Wire.Private` (its `Gen` is what the twin calls), `Everparse.Raw` and the
`Everparse` values the FFI pipeline reads (`visitor_fields`, `visitor_member`,
`visitor_setters`, `uses_wire_ctx`, `entrypoint_struct`, ...),
`Codec.field_readers` and the struct validators, `Wire_ocaml.Plan` (shared
with `Wire_rust` and `Wire_hardcaml`), `Wire_ocaml.views`, the steps of
`Wire_3d` (`generate_*`, `run_everparse`, `write_fields`, `fork_pool`, ...)
and of `Wire_stubs` (`to_c_stubs`, `to_ml_stubs`, ...). Its name is an
implementation choice.

## What goes

### The libraries

Every `val`, `type`, `module`, `module type` and `exception` of the public
interfaces at the tip, read mechanically from `lib/wire.mli`, `lib/uInt64.mli`,
`lib/uInt63.mli`, `lib/ocaml/wire_ocaml.mli`, `lib/rust/wire_rust.mli`,
`lib/3d/wire_3d.mli`, `lib/stubs/wire_stubs.mli`, `lib/diff/wire_diff.mli` and
the fourteen modules `wire.runtime` exports with an interface (`buf.std.mli`,
`vec.neon.mli`): 727 declarations. `Wire_runtime.Paranoid`, which a rule
writes with no interface (`enabled`), is hidden with the runtime's support
modules. 322 stay (some renamed or retyped, as above), 392 go and 13 go
if [Q9](#q9) and [Q11](#q11) are answered as the agreed design expects.
`lib/codec.mli` is `Wire.Codec`'s implementation: the 60 declarations it has beyond
`Wire.Codec` are reachable only as `Wire__Codec` and are not counted.
`wire.diff` is test infrastructure and keeps its surface.

The count after each name is the files outside its own library that name it
as `Parent.name`, in the repository's OCaml, Rust, Markdown, cram, dune
and make files: a module bound to a
local alias (`module V = Wire_runtime.Vec`) or reached by generated code is
not counted, so a 0 is a lower bound, not a proof of no use. A "none" below
rests on its own grep, given with it.

| Module | Declarations, each with its count of user files | Replacement |
| --- | --- | --- |
| `Wire` | `Staged` (module) 6 | partial application: `Codec.get c f` is the reader |
| `Wire.Staged` | `t` (type) 0, `stage` 1, `unstage` 24 | partial application: `Codec.get c f` is the reader |
| `Wire.Field` | `packed` (type) 0, `decl_of_packed` 2 | the internal library (§ Internal) |
| `Wire.Codec` | `wire_size_opt` 10 | `Codec.size` |
| `Wire.Codec` | `wire_size` 26 | `Codec.size` (an option); `Codec.min_size` for a codec of constant size |
| `Wire.Codec` | `min_wire_size` 12 | `Codec.min_size` |
| `Wire.Codec` | `wire_size_at` 16 | `Codec.size_at` |
| `Wire.Codec` | `size_of_value` 25 | `Codec.encoded_size` |
| `Wire.Codec` | `decoder` 2 | `Codec.decode c`, applied once |
| `Wire.Codec` | `encoder` 2 | `Codec.encode c`, applied once |
| `Wire.Codec` | `field_ref` 2 | `Field.ref` of the source field |
| `Wire.Codec` | `field_readers` 3, `validator` (type) 0, `validator_of_struct` 6, `validate_struct` 4, `struct_size_of` 1, `struct_min_size` 2, `wire_size_info_of_validator` 1 | the internal library (§ Internal) |
| `Wire.Codec` | `bitfield` (type) 5, `bitfield` 5, `load_word` 8, `extract` 7, `word` (type) 1, `word_reader` (type) 2, `word_reader` 2, `read_word` 8, `part` (type) 0, `word_part` 1, `of_word` 8 | Q11: `Codec.View` and the generated accessors (open) |
| `Wire` | `codec` (type) 17 | `Codec.t` |
| `Wire` | `pp_value` 1 | `Codec.pp_value` |
| `Wire.Everparse` | `struct_` (type) 2, `decl` (type) 0, `decl_case` (type) 0, `module_` (type) 2, `validate_output_name` 2, `uses_wire_ctx` 3, `entrypoint_struct` 4, `extern_fn_names` 2, `field_action_form` (type) 0, `equal_field_action_form` 2, `field_action_forms` 3, `Raw` (module) 47 | the internal library (§ Internal) |
| `Wire.Everparse` | `plug_field` (type) 5 | the internal library (§ Internal), as `visitor_field` |
| `Wire.Everparse` | `plug_member` 4 | the internal library (§ Internal), as `visitor_member` |
| `Wire.Everparse` | `plug_fields` 7 | the internal library (§ Internal), as `visitor_fields` |
| `Wire.Everparse` | `plug_setters` 4 | the internal library (§ Internal), as `visitor_setters` |
| `Wire.Everparse.Raw` | `struct_` (type) 13, `field` (type) 4, `decl` (type) 0, `decl_case` (type) 1, `module_` (type) 7, `t` (type) 0, `typedef` 5, `define` 1, `extern_fn` 1, `extern_probe` 1, `enum_decl` 1, `decl_case` 1, `decl_default` 1, `casetype_decl` 1, `module_` 7, `to_3d` 10, `to_3d_file` 4, `struct_of_codec` 16, `project_struct` 8, `field` 4, `anon_field` 1, `field_ref` 1, `struct_` 13, `struct_name` 12, `field_names` 3, `ocaml_kind` (type) 0, `field_kinds` 4, `int_slot` (type) 2, `field_seed` (type) 1, `int_slots` 2, `field_seeds` 4, `struct_project` 2, `struct_params` 2, `input_param_names` 4, `input_param_c_types` 2, `param_c_types` 1, `struct_typ` 2, `param` 1, `mutable_param` 2, `param_struct` 2, `apply` 1, `type_ref` 1, `qualified_ref` 1, `pp_typ` 1, `pp_module` 1, `struct_size` 2, `of_module` 4 | the internal library (§ Internal) |
| `Wire.Ascii` | `of_struct` 1, `pp_struct` 2 | the internal library (§ Internal) |
| `Wire` | `Private` (module) 73 | the internal library (§ Internal) |
| `Wire.Private` | `UInt32` (module) 9, `Unchecked` (module) 1, `UInt63` (module) 4, `Input_end` (module) 0, `Types` (module) 29, `Eval` (module) 0, `Expr_compiler` (module) 0, `Bitfield` (module) 1, `Uint_var` (module) 1, `Shape` (module) 0, `Names` (module) 7, `Layout` (module) 6, `Gen` (module) 5, `Testing` (module) 9, `param_name` 2, `param_is_mutable` 2, `param_c_type` 2, `ml_type_of` 2, `c_type_of` 1 | the internal library (§ Internal) |
| `Wire.Private.Eval` | `ctx` (type) 0, `empty` 1, `expr` 1, `int_of` 1, `int_of_exn` 2 | the internal library (§ Internal) |
| `Wire.Private.Bitfield` | `byte_size` 6, `total_bits` 1, `equal` 1, `read_word` 2, `u32_field_le` 1, `u32_field_be` 1, `write_word` 1, `native_bit_order` 1, `shift` 0, `extract` 1 | the internal library (§ Internal) |
| `Wire.Private.Gen` | `to_struct` 10, `digest` 3, `check` 1, `field_k` (type) 0, `with_field_reader` 1, `map` (type) 0, `map_int` 0, `map_int32` 0, `map_id` 1, `check_map` 1, `kind` (type) 0, `Outputs` (module) 0, `record` (type) 0, `root` 0, `sub_at` 0, `enter_at` 0, `enter` 0, `check_at` 0, `check_window` 0, `reader` (type) 0, `read` 0, `field_at` 0, `present_at` 0, `writer` (type) 0, `set_at` 0, `scan` 0, `frames` 0, `index` 0 | the internal library (§ Internal) |
| `Wire.Private.Gen.Outputs` | `t` (type) 0, `int` 1, `bool` 1 | the internal library (§ Internal) |
| `Wire.Private.Testing` | `general` 9, `fresh` 2, `paths` 3, `staged_intervals` 3, `all_paths` 1 | the internal library (§ Internal) |
| `Wire.UInt64` | `le` 1, `be` 1, `set_le` 1, `set_be` 1 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire.UInt64` | `of_int` 4 | `UInt64.v`, as the other carriers name it |
| `Wire.Private.UInt63` | `t` (type) 3, `pp` 2, `check_encode` 1, `to_int` 1, `of_int` 4 | the internal library (§ Internal) |
| `Wire_ocaml` | `buffer` (type) 0 | none: no bigarray user outside the tests (Q10) |
| `Wire_ocaml` | `ident` 1 | `Wire_ocaml.outputs`, which spells each name |
| `Wire_ocaml` | `view` (type) 1, `view_member` (type) 0, `views` 1, `Plan` (module) 21 | the internal library (§ Internal) |
| `Wire_ocaml.Plan` | `kind` (type) 0, `check` (type) 0, `output` (type) 2, `field` (type) 0, `node` (type) 0, `t` (type) 2, `Facts` (module) 0, `v` 10, `nodes` 0, `pos` (type) 0, `values` (type) 0, `bound` (type) 0, `choice` (type) 0, `join` (type) 0, `short_frames` (type) 0, `decision` (type) 0, `pp_decision` 0, `decisions` 2, `outputs` 3, `select` 0, `slot_depth` 4, `slots` 1, `live` 2, `conditions` 0, `word_range` 1, `range` 0, `Scope` (module) 0, `tree` (type) 0, `tnode` (type) 0, `gates` 0, `decide` 0, `shared` 0 | the internal library (§ Internal) |
| `Wire_ocaml.Plan.Facts` | `t` (type) 1, `empty` 1, `assume` 1, `simplify` 1 | the internal library (§ Internal) |
| `Wire_ocaml.Plan.Scope` | `plan` (type) 0, `t` (type) 1, `v` 1, `facts` 1, `refine` 1, `range` 1, `wide` 1, `nonnegative` 0 | the internal library (§ Internal) |
| `Wire_rust` | `ident` 2 | none: a Rust visitor's parameters are positional |
| `Wire_rust` | `digest` 1 | none: no reader of `DIGEST` (as `Wire_ocaml`'s `digest`, 8e20a5bf) |
| `Wire_rust` | `scanner` 9 | `Wire_rust.v ~folds`, `to_rs`, `main` |
| `Wire_rust` | `rs` 7 | `Wire_rust.v`, `to_rs`, `main` |
| `Wire_3d` | `everparse_name` 6, `pascal_case` 3, `generate_3d` 7, `generate_dune` 1, `run_everparse` 4, `check_provenance` 1, `fork_pool` 3, `rm_rf` 5, `batch_check` 1, `write_external_typedefs` 3, `write_fields` 5, `generate_c` 4, `harden_wrapper_source` 1, `generate_standalone` 3, `generate_dune_standalone` 2, `wrapper_symbols` 1, `archive_link_steps` 1, `generate_corpus` 5, `generate_agree` 1, `ensure_dir` 1, `strict_cc_flags` 5, `everparse_type_defines` 3, `Private` (module) 2 | the internal library (§ Internal) |
| `Wire_3d` | `check_plug_names` 4 | the internal library (§ Internal), as `check_visitor_names` |
| `Wire_3d.Private` | `run_3d_spawned` 1, `pool_in_process` 1, `locate_3d_exe` 2, `has_custard` 1 | the internal library (§ Internal) |
| `Wire_stubs` | `packed_codec` (type) 0 | `Wire_3d.packed` |
| `Wire_stubs` | `of_structs` 1, `check_names` 3, `to_c_stubs` 3, `to_ml_stubs` 3, `to_ml_stub` 1, `to_ml_stub_name` 1, `everparse_name` 0, `validator_name` 3, `ml_type_of` 0 | the internal library (§ Internal) |
| `Wire_runtime.Arith` | `add_overflows` 2, `sub_overflows` 1, `mul_overflows` 1, `Overflow` 2, `add` 2, `sub` 1, `mul` 1, `checked_add` 2, `checked_sub` 2, `checked_mul` 2 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.Buf` | `length` (type) 32, `length` 32 | Q9: the length is read from `'b Buf.t` (open) |
| `Wire_runtime.Buf` | `local` 1 | `Buf.v` on a local buffer (gap: OxCaml mode polymorphism) |
| `Wire_runtime.Buf` | `Bigstring` (module) 5 | none: no bigarray user outside the tests (Q10) |
| `Wire_runtime.Buf.Bigstring` | `bigstring` (type) 0, `t` (type) 5, `length` (type) 3, `any` (type) 0, `v` 1, `local` 0, `length` 3, `pp` 0 | none: no bigarray user outside the tests (Q10) |
| `Wire_runtime.Errors` | `fail` 8, `is_error` 7, `error_of` 7, `negative_offset` 4, `eof` 0, `frame` 1, `field_constraint` 1, `invalid_enum` 0, `invalid_tag` 0, `where_clause` 1, `relocate` 0, `encode_too_short` 3, `encode_rejected` 1, `unsigned_too_wide` 2, `byte_length` 2, `enum_encode` 0, `check_unsigned` 3, `check_length` 0, `check_enum` 1 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.SInt8` | `get` 4, `set` 4 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.SInt8` | `unsafe_of_int` 0, `unsafe_get` 6, `int_eq` 2 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.SInt16` | `le` 4, `be` 4, `set_le` 4, `set_be` 4 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.SInt16` | `unsafe_of_int` 0, `unsafe_le` 6, `unsafe_be` 6, `int_eq` 2 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.SInt32` | `le` 4, `be` 4, `set_le` 2, `set_be` 2 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.SInt32` | `check_encode` 2, `unsafe_le` 5, `unsafe_be` 5, `int_eq` 2 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.UInt8` | `get` 5, `set` 5 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.UInt8` | `unsafe_of_int` 1, `unsafe_get` 5, `int_eq` 3 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.UInt16` | `le` 4, `be` 4, `set_le` 4, `set_be` 4 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.UInt16` | `unsafe_of_int` 1, `unsafe_le` 5, `unsafe_be` 5, `int_eq` 3 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.UInt32` | `le` 4, `be` 6, `set_le` 4, `set_be` 4 | hidden: the codec's and generated code's loads (§ Runtime) |
| `Wire_runtime.UInt32` | `check_encode` 1, `mask32` 11, `unsafe_le` 5, `unsafe_be` 5, `int_eq` 4 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.UInt32` | `of_int` 17 | `UInt32.v`, as the other carriers name it |
| `Wire_runtime.Staged` | `t` (type) 3, `stage` 2, `unstage` 26 | partial application (with `Wire.Staged`) |
| `Wire_runtime.Target` | its `include module type of struct ... end` (3 declarations) 0 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.Unchecked` | `checked` 2, `u8` 13, `u16le` 8, `u16be` 12, `i32le` 2, `i32be` 6, `i64le` 0, `i64be` 2, `set_u8` 7, `set_u16le` 5, `set_u16be` 7, `set_i32le` 3, `set_i32be` 5, `set_i64le` 0, `set_i64be` 0, `blit_string` 2 | hidden: generated code only (§ Runtime) |
| `Wire_runtime.Unchecked` | `Bigstring` (module) 6 | none: no bigarray user outside the tests (Q10) |
| `Wire_runtime.Unchecked.Bigstring` | `t` (type) 5, `length` 3, `u8` 0, `u16le` 0, `u16be` 0, `i32le` 0, `i32be` 0, `i64be` 0, `set_u8` 0, `set_u16le` 0, `set_u16be` 0, `set_i32le` 0, `set_i32be` 0, `set_i64le` 0, `set_i64be` 0, `get_uint8` 0, `get_uint16_le` 0, `get_uint16_be` 0, `get_int32_le` 0, `get_int32_be` 0, `sub_string` 0, `unsafe_blit_string` 0, `to_bytes` 0 | none: no bigarray user outside the tests (Q10) |
| `Wire_runtime.Vec` | `load_bigstring` 0, `store_bigstring` 0 | none: no bigarray user outside the tests (Q10) |

405 declarations in 72 rows.

The greps behind each "none":

- bigarray buffers: `grep -rln Bigstring --include='*.ml' bench examples`
  prints nothing; `~buffer:`Bigstring` is used by `test/ocaml/gen.ml`,
  `test/ocaml/standalone/gen.ml` and `test/diff/gen_ocaml.ml` only.
- `DIGEST`: `grep -rn DIGEST --include='*.rs' bench test | grep -v /gen/`
  prints nothing.
- `Wire_rust.ident`, `Wire_rust.digest`, `Wire_ocaml.ident`,
  `Wire_ocaml.views`: `grep -rln 'Wire_rust.ident\|Wire_rust.digest\|Wire_ocaml.ident\|Wire_ocaml.views' --include='*.ml' .`
  prints `test/rust/test_wire_rust.ml`, `test/diff/gen_ocaml.ml` and
  `test/diff/gen_rust.ml`, and the libraries themselves.

### The generated module

Every kind of member of the committed modules (`examples/generated/clcw.mli`
and `stack.mli`, read mechanically, each field name made `<field>` and each
sub-record module `<Sub>`), and the kinds only other options print
(`lib/ocaml/interface.ml`, line given). "api-prune" marks a member that lane
already deletes.

| Member | Where | Status | Replacement |
| --- | --- | --- | --- |
| `digest` | clcw.mli:27 | gone, api-prune (8e20a5bf) | none: the twin carries its own copy |
| `Value`, `Value.<field>` (type, val), `Value.<field>_low` | clcw.mli:38 | gone, api-prune (b5e3a96e) | the accessor's type (`int`, the carrier) |
| `v` (positional constructor) | clcw.mli:202 | goes | the record literal |
| `is_valid` | clcw.mli:230 | gone, api-prune (896d18c7) | `ok (check b off)`, `validate` |
| `write_int`, `write_at`, `write_int_at` | clcw.mli:272-321 | gone, api-prune (b5e3a96e) | `write` |
| `Get`, `Get.<field>`, `Get.<field>_word`, `<field>_of_word` | clcw.mli:351-533 | gone from modules with views, api-prune (0abb5783); kept on modules without | the accessor after `check`; no module without views |
| `view_<field>_word` | clcw.mli:747 | gone, api-prune (0abb5783) | the accessor |
| `fold`, `fold_frames` without a visitor | clcw.mli:539, stack.mli:53 | goes | a fold with a visitor: `n`, `n_frames`, `n_index` |
| `decode_with` (OxCaml) | interface.ml:334 | gone, api-prune (896d18c7) | `check` and accessors |
| `<n>_unboxed`, `<n>_frames_unboxed` (OxCaml) | interface.ml:832, 841 | goes | a fold with a visitor |
| `Valid`, `Valid.v`, `read_<field>` | interface.ml:456, 469 | goes with `~buffer` | `check` and accessors |
| `View`, `View.check`, `View.ok`, `View.get`, `View.with_` | interface.ml:489-506 | goes with `~buffer` | the module's own `check`, `ok`, `get` |
| `view_<field>`, `view_<field>_int` | interface.ml:536, 542 | gone, api-prune (0abb5783) | the accessor |
| `X4.with_`, `X4.write_int_at` | interface.ml:576, 597 | open: Q13 | `X4.check`, `X4.ok`, `X4.get`, `X4.write` |
| Rust `CODEC`, `DIGEST` | `Wire_rust` module | goes | none: no reader |

Stay: `min_size`, `t`, `pp`, `size`, `validate`, `decode`, `encode`,
`write`, `view`, `check` (type and value), `ok`, `get`, `<field>`,
`<field>_present`, `<sub>`, `set_<field>`, `<Sub>` with its members, the
visitor folds `<n>`, `<n>_frames`, `<n>_index`, and `X4` but for Q13.

That is 30 kinds of generated member gone (12 already on api-prune) and 2
more if Q13 aligns `X4`, beside the 392 declarations of the libraries.

## Open questions

The questions of the design (2026-11-25), each answered from a document or a
measurement, or left open with the measurement that would answer it.

**Q1. Where the twin lives.** Answered by Thomas: a sibling unit
`<name>_codec`, so the module links `wire.runtime` only. In the code:
`Wire_ocaml.main` writes it.

**Q2. Field handles.** Answered by Thomas: by place and kind in the codec's
field list, and the layout digest, when the functor is applied; no naming
convention in the schema module.

**Q3. Scope of the one walk.** Answered for what the walk refuses: "generation
refuses, naming the field, only what the walk cannot cover" (the design's
Capabilities), which Feature shippability requires. Still open: a fixed record
of integers wider than 32 bits or of maps has no views at the tip and reads
through `Get`; the end state extends the walk to them so that every module
has views and `Get` goes. Still open, needing a ruling rather than a
measurement: which of `casetype`, arrays, repeats, parameters and actions the
walk covers next, and what names a `casetype` branch's accessors take (its
cases carry tags, not names).

**Q4. Sub-record module names.** Answered by docs/execution.md, Generated
code: "one module per codec, named after it (`CapturedIPv4` is
`Captured_ipv4`, written once for the IPv4 and IPv6 fields that hold
`StackTCP`)".

**Q5. Several folds per module.** Answered: `?folds` with names
(`Wire_ocaml.fold`; docs/execution.md, "each fold the module declares"). The
form without a visitor goes: docs/execution.md, "OCaml without flambda never
inlines a function passed as an argument", so it is never the fast path on
stock OCaml, and a second form is a second way.

<a id="q6"></a>**Q6. Fold framing.** One frame, frames of a stride and frames
of an index exist. A fold over consecutive self-delimited records (the routing
workload: each packet's length gives the next one's offset) stays open. It
needs the fold written, then the routing rows counted against the loop
`bench/apps/loops.ml` writes today:

```
make bench-apps-count
sh bench/count.sh 100000 "routing run wire.ocaml" \
  _build/default/bench/apps/bench.exe run _build/bench-apps-count/routing.bin \
  100000 RUNS wire.ocaml
```

**Q7. Record values for variable shapes.** Answered by the orchestrator:
"Every shape gets decode, encode, size and set_<field> on a view; labelled
write only where every field has a constant size."

**Q8. Stock OCaml's fold slower than the views.** Answered by measurement: the
generator fixed the consumer's cost. bench/README.md, "The generated views":
`fold_step` counts 150.0 / 43.1 on OCaml 5.4.1 against 237.1 / 51.6 for check
and accessors (299.6 / 73.1 at the design).

<a id="q9"></a>**Q9. The length in `'b Buf.t`.** Agreed ("`Buf.length`,
`Buf.local` hidden"; the sketch's `check` takes no length) but unmeasured: a
check would read `Bytes.length` itself where the loop now reads it once. Open
until counted on a branch where `check`, `enter` and the sub-record accessors
read it, every row that checks, on OCaml, flambda and OxCaml:

```
make bench-apps-count     # rows "wire.ocaml" of routing, gateway, clcw
make bench-flow           # builds _build/bench-flow/mawi.pcap
sh bench/count.sh 1000000 view \
  _build/default/bench/rust/bench.exe flow _build/bench-flow/mawi.pcap RUNS \
  "wire.ocaml check/view, accessors, parse only"
```

(`, OxCaml target` before `, parse only` on OxCaml.) A cost beyond noise keeps
`'b Buf.length` as an argument, and the end state's `check` takes it.

**Q10. Carried over.** Bounds-only `Get`: gone from modules with views
(api-prune, 0abb5783), the rest with Q3. `_exn`: none is left but `Private.Eval.int_of_exn`, which moves to the
internal library; `Parse_error` left the public surface (10510762). One visitor source for both targets: answered by
docs/execution.md, "The consumer's `valid` takes its integer outputs as
`int64` on both targets, so one consumer compiles for either." In-place writes
on a view: `set_<field>` (execution.md, Generated code). Bigarray users: none
outside the tests (grep above), so `?buffer`, `Buf.Bigstring`,
`Unchecked.Bigstring` and `Vec.load_bigstring`/`store_bigstring` go. DSL
renames: none ("DSL unchanged"); the clash of the two `bitfield` types ends
only if Q11 deletes `Codec.bitfield`.

**Thomas's open point, how the generator learns the visitor.** Answered by the
orchestrator and in the code: `main --visitor n=%{dep:flow_step.ml}`.

<a id="q11"></a>**Q11. Word reads.** The first review deletes
`bitfield`/`load_word`/`extract`; CLAUDE.md's Benchmarking principles name
`Codec.read_word`, `Codec.of_word`, `Codec.load_word` and `Codec.extract` as
the OCaml tier's loop, because `Codec.View` reads a field of a frame shorter
than 8 bytes out of line (wire.mli, `word_reader`). Open until the `Wire.Codec`
rows of the three workloads are counted with `<app>_workload.ml` reading
through `Codec.View`:

```
make bench-apps-count
sh bench/count.sh 100000 "routing run Wire.Codec" \
  _build/default/bench/apps/bench.exe run _build/bench-apps-count/routing.bin \
  100000 RUNS Wire.Codec
```

(and `gateway`, `clcw`, `run` and `write`, as the Makefile's `bench_count_rows`
lists them). Within noise: the word reads go and CLAUDE.md changes with them.
Beyond it: they stay, with `read_word` answering a result (the read-word
lane).

<a id="q12"></a>**Q12. `UInt32.to_int` on narrow ints.** uInt32.mli: "on a
narrower one a value with bit 31 set does not fit and is truncated", which the
narrow-int rule forbids an API to assume away. Open, a ruling rather than a
measurement: `to_int_opt` only (an option per read in a loop), or `to_int`
kept for code that, like a generated module reading a 32-bit field into an
`int`, refuses to load where ints have 32 bits or fewer.

<a id="q13"></a>**Q13. `X4`'s names.** `X4.with_` takes continuations and
`X4.write_int_at` a buffer and its length, names the end state deletes
elsewhere. Aligning them on `check`/`ok`/`get` and `write` is open until the
NEON rows are counted on OxCaml:

```
make bench-apps-count     # rows "NEON lanes, OxCaml" of clcw run and clcw write
```

## Gaps

Where the code, at the tip or on a running lane, contradicts the end state.

### api-prune (Phase 5, deleting legacy generated members)

The lane deletes `digest`, `Value`, `write_int`, `write_at`, `write_int_at`,
`is_valid`, `decode_with`, `view_<field>_word`, `view_<field>_int`, `Get` on
modules with views and `Gen.Outputs`. Also:

1. Delete the positional constructor `v` of a fixed record
   (examples/generated/clcw.mli:50 on the lane): the record literal builds the
   same value and `encode`/`write` check it.
2. Delete the folds without a visitor (`n`, `n_frames`, `n_unboxed`,
   `n_frames_unboxed` taking `~invalid ~valid`) and `v`'s default fold: make
   `?folds` default `[]` and make `to_ml`/`main` refuse a declared fold no
   `--visitor` names. Port `examples/generated` (its `fold`, `fold_frames`),
   `test/diff`, `verify/` (which classifies `fold` and `fold_frames`) and
   `Wire_ocaml.fstar` (its `Scan`/`Proof` theorem is over `fold ~invalid
   ~valid`; `Step`/`StepProof` already state it over a visitor).
3. Delete `?buffer` and everything it reaches: `Wire_ocaml.buffer`,
   `Buf.Bigstring`, `Unchecked.Bigstring`, `Vec.load_bigstring`,
   `Vec.store_bigstring`, the bigarray `Get`, `Valid`, `View` (with
   `View.with_`) and `read_<field>`, and their tests in `test/ocaml`,
   `test/ocaml/standalone` and `test/diff`.
4. Delete `Wire_ocaml.ident` (`outputs` spells each name) and move
   `Wire_ocaml.view`, `view_member`, `views` and `Plan` to the internal
   library.
5. Rewrite the docs of `Wire_ocaml.target` and `Wire_ocaml.v` (they name
   `is_valid`, `write_int`, `decode_with`, `Get`, `Valid`, `View.with_`,
   `write_int_at`) and docs/execution.md, Generated code (`is_valid`, `Get`,
   `Valid`, `View`, `read_<field>`, `view_<field>`, `n_unboxed`,
   `` ~buffer:`Bigstring ``).

### rust-api (Phase 4, Rust parity)

The lane's `wire_rust.mli` (uncommitted in its worktree) has `v`, `fold`,
`name`, `to_rs` and `main`, and drops `scanner` and `rs`. Also:

1. Delete `Wire_rust.ident` and `Wire_rust.digest`, and the module's `CODEC`
   and `DIGEST`: nothing reads them, and the OCaml module dropped its digest
   for that reason (8e20a5bf).
2. Write the checksum visitors `stack_step.rs` and `selected_step.rs` by hand
   in `bench/rust/cargo/src`, not from `bench/rust/gen.ml` through
   `Wire_ocaml.Plan`: a benchmark loop is what a user writes, and `Plan` is
   internal in the end state. A Rust visitor's parameters are positional, so
   it needs no generated names.

### wire-read-word (`read_word` answers an error)

1. Land it: a buffer that cuts the record is input (Untrusted input), whatever
   Q11 decides. Make the result's cost part of the commit: `read_word`
   answering `('r word, Error.t) result` must not allocate per header in the
   routing and gateway loops; count `routing run Wire.Codec` and
   `gateway run Wire.Codec` before and after with the Q11 command.
2. Fix `read_word`'s doc, which still says it raises "as get does".
3. If Q11 then deletes the word reads, delete `read_word` with them and update
   CLAUDE.md's Benchmarking principles.

### No lane yet

1. `Staged`: delete `Wire.Staged` and `Wire_runtime.Staged`; `Codec.get`,
   `set`, `writer` and `load_word` return their function, `decoder` and
   `encoder` go (partial application of `decode` and `encode`).
2. `Codec`: rename the sizes (`wire_size_opt`, `wire_size`, `min_wire_size`,
   `wire_size_at`, `size_of_value` to `size`, `min_size`, `size_at`,
   `encoded_size`); make `name` public; move `Wire.pp_value` to
   `Codec.pp_value`; delete `field_ref`, the `'r codec` alias and the validator
   family; `Field.action` answers `Action.t option`.
3. The internal library: move `Wire.Private`, `Everparse.Raw`, the FFI values
   of `Everparse`, `Codec.field_readers`, `Ascii.of_struct`/`pp_struct`, the
   steps of `Wire_3d` and of `Wire_stubs` into it; make `Everparse.t` abstract
   with `name`.
4. "plug" to "visitor" everywhere (Thomas): `Everparse.plug_field`,
   `plug_member`, `plug_fields`, `plug_setters` and
   `Wire_3d.check_plug_names` still carry it at the tip.
5. The runtime: add `Wire_runtime.S` (and `Wire.S`) and a test that each
   committed module satisfies it; hide `Unchecked`, `Errors`, `Arith`,
   `Target` and the carriers' `unsafe_*`, `int_eq`, `check_encode`, `mask32`,
   `le`, `be`, `get`, `set`, `set_le`, `set_be`; rename `UInt32.of_int` and
   `UInt64.of_int` to `v`; move `UInt64` into the runtime, which a module
   reading a 64-bit field needs (Q3).
6. `Buf.local`: hidden in the agreed design, but OxCaml names a buffer held
   `local` with it (`test/ocaml-ox`); `Buf.v` must take a local buffer first.
7. `Wire_stubs`: a stub raises `Failure` on a refused record (wire_stubs.mli
   preamble), which Untrusted input forbids: answer a result. Take
   `Wire_3d.packed` in place of `packed_codec`.
8. Documents: CLAUDE.md (Benchmarking principles name the word reads and
   `Codec.writer`; the pipeline names `Wire_stubs.generate ... [ C codec ]`),
   docs/projection.md (`Wire_3d.generate_dune_standalone`), and the README,
   which has no section on the generated module.

## Decided here

Points no agreed text settles, which this document chose and Thomas may
overrule:

- `check` takes `off` positionally for a codec of constant size, as every
  other offset of the API does (`decode buf off`); the decision's text wrote
  `check b ~off`.
- `validate` takes `buf off`, as `Codec.validate` does, not the sketch's `~off
  ~len`.
- `Codec.get`, `Codec.set` and `Codec.writer` stay, as CLAUDE.md names them;
  only `Staged` leaves them.
- `Wire.pp_value` becomes `Codec.pp_value`; `Codec.name` becomes public.
- `Wire_3d` keeps `pack`, `run`, `main`, `parse_3d` and `has_3d_exe`;
  `Wire_stubs` keeps `generate` and `build_codec_archive`, over
  `Wire_3d.packed`.
- `Vec` stays public with `X4`: it is the API CLAUDE.md requires SIMD to go
  through.
- The carriers' byte reads (`le`, `be`, `get`, `set`, ...) are hidden: no
  user outside the libraries and their tests calls them.
