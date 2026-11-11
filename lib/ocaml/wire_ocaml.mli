(** OCaml code generated from a codec.

    A codec decodes and encodes through closures built when it is compiled.
    [Wire_ocaml] writes the same work out as OCaml source, with every offset a
    constant, so the compiler sees straight-line loads and stores and a direct
    call to the record's constructor. Its code holds no integer literal wider
    than 31 bits, so js_of_ocaml and wasm_of_ocaml compile it as written; a
    module that reads a 32-bit field into an [int] raises [Invalid_argument]
    when it loads on a target whose ints have 32 bits or fewer.

    {b Runtime.} The generated module links [wire.runtime] (and so [optint]),
    not [wire], and needs no codec at run time: it can be embedded in a program
    that does not link [wire]. Its integer types are [Wire_runtime.UInt8.t] and
    the like, which are {!Wire.UInt8.t} and the like where [wire] is linked, and
    it raises [Wire_runtime.Errors.Parse_error], which is {!Wire.Parse_error},
    with the value the codec raises, and the codec's [Invalid_argument]
    messages. The one exception is a module generated with [~maps]: a map's
    functions are a [Wire] value, so that module links [wire]. The module
    carries the digest of its codec's shape; {!bridge} writes a module that
    checks it against the codec when it loads, for a program that links [wire]
    anyway.

    A [+], [-] or [*] in a constraint or a [where] clause is checked as the
    codec checks it: an overflow rejects the record with the codec's error.

    A generator executable, run by a dune rule, prints the module for a codec
    with [print_string (Wire_ocaml.ml ~codec:"Ipv4.codec" Ipv4.codec)].

    The generated module has [get_<field> buf off] for each field, which reads
    it as {!Wire.Codec.get} does with one bounds-checked load at a constant
    offset, and [get_<field>_int] for a 32-bit field's [int] view. [Valid.v buf]
    validates the record at offset 0, as {!Wire.Codec.valid} does, and
    [read_<field>] on what it returns is one load with no check of its own. As
    for {!Wire.Codec.valid}, the promise is memory safety only: a buffer's
    length never changes, so a read never leaves the buffer, but if its bytes
    change after [Valid.v], a read may return a value that breaks a constraint.
    [(v :> bytes)] is the buffer itself, not a copy. For a bitfield,
    [get_<field>_word] loads the word that holds it and [<field>_of_word] takes
    the field out, so fields packed in one word share one load.

    [is_valid buf off] checks the record at [off] without raising, loading only
    the fields its constraints read; [validate] raises the codec's error where
    it is false. [View.at buf off] validates the record at any offset and stands
    for it, with the same promise as [Valid]: each [view_<field>] (and
    [view_<field>_int], [view_<field>_word]) is one load with no bounds check.
    [View.check buf off] does the same without raising, and [View.ok] tells
    whether the record passed. A view that failed holds no bytes: [View.buf] is
    then an empty buffer and each read gives zero without loading, so failed
    views share nothing a caller can write. On OxCaml a view is an unboxed
    record, held in registers; on OCaml it is a small record. A [Bigstring]
    submodule has the same functions, [Valid] and [View] included, on a bigarray
    of bytes. It has [size], [decode], [validate] and [write buf off v1 ... vn],
    which stores field values in order. Without [~make] it also defines a record
    [t] of the field values, [make] and [encode]. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [validate], [write_int], [write] and the reads; [decode_with k],
    which takes a continuation for flambda2 to inline, so a [k] that uses a few
    fields reads only those, under one frame check; and, for the generated
    record, [decode_local], which builds it on the stack. *)

val ident : string -> string
(** [ident name] is the OCaml name a generated module derives from the field
    [name], for either target: its getter is [get_<ident name>], its read
    [read_<ident name>] and its record field [ident name]. It is [name]
    lower-cased, any character other than a letter, a digit or ['_'] turned into
    ['_'], prefixed with ["f_"] when that is empty or starts with a digit, and
    suffixed with ['_'] when it is a keyword of OCaml or of OxCaml, so the code
    for the default target compiles on an OxCaml compiler too. *)

(** A {!Wire.typ} of any value type. *)
type typ = Typ : 'a Wire.typ -> typ

val ml :
  ?target:target ->
  ?make:string ->
  ?maps:(string * typ) list ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [ml ?target ?make ?maps ~codec c] is the source of a module specialised to
    [c], for [target] (default [`OCaml]). [codec], the OCaml path of [c], names
    it in the module's header; the module itself never refers to [c]. [make],
    the path of a function taking the field values in order, builds the decoded
    value; it defaults to a generated record, which a codec with a map field
    cannot use.

    A field under a {!Wire.map} is specialised when [maps] names the map: a pair
    of its OCaml path and the map itself, as in
    [("Page.u16be_int", Typ Page.u16be_int)]. The generated module calls the
    map's functions by that path; {!bridge} checks that the field still holds
    that very map.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, or a map [maps]
    does not name; and naming both fields when two of them would define the same
    OCaml name ({!ident}), or one of them a name the module defines for itself,
    such as [get_failed]. *)

val bridge :
  ?maps:(string * typ) list ->
  codec:string ->
  core:string ->
  'r Wire.Codec.t ->
  string
(** [bridge ?maps ~codec ~core c] is the source of a module that checks, when it
    loads, that the codec [c] at the path [codec] still has the shape the module
    at the path [core] was generated from by {!ml}, and that each map [maps]
    names is still the very map of its field. It links [wire]; link it next to
    [core] wherever [wire] is linked anyway. *)

(** {1 Scanning}

    [scanner] writes the code that walks a codec of any shape in its scope and
    hands out its scalar fields, with no value built and no byte copied. *)

val scanner_fields : 'r Wire.Codec.t -> (string * bool) list
(** [scanner_fields c] is the outputs of {!scanner} for [c], in declaration
    order: each named scalar field, its name the path of field labels ({!ident})
    joined with ['_'], and for each optional field [<path>_present], flagged
    [true]. A field in an anonymous one is checked but not an output. Raises
    [Invalid_argument] as {!scanner} does. *)

val scanner :
  ?target:target ->
  ?fields:string list ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [scanner ?target ?fields ~codec c] is the source of a module that checks a
    record of [c] in a frame of bytes and hands out its scalar fields. It covers
    integers up to 32 bits, bitfields, byte arrays and slices of any size,
    nested codecs, optional fields and fields in a sized region, exact or at
    most; it raises [Invalid_argument] for any other field, parameters and
    actions. The generated module links [wire.runtime], and refuses to load
    where ints have 32 bits or fewer if it holds a value of more than 31 bits.

    [fold buf off len ~invalid ~valid] checks the record at [off] in the [len]
    bytes there, as {!Wire.Codec.decode} does on those bytes alone, and calls
    [valid] with each output as a labelled [int] (a [bool] for a presence), or
    [invalid ()] when the record or the bounds are bad. An absent field is 0.
    [fields] selects the outputs: the others are not read unless a check needs
    them, and every check still runs. [decode buf off len] returns the outputs
    in a record with an [ok] flag.

    [n] optional fields give code linear in [n] when they follow each other, or
    when they nest and [fields] selects no output. Nested inside each other with
    every output handed out, they give code quadratic in [n].
    [fold_frames buf off len ~stride ~init ~invalid ~valid] folds over the
    [len / stride] frames of [stride] bytes, each checked on its own; it raises
    [Invalid_argument] for bad bounds or a [stride] that is not positive.

    On OxCaml the outputs are [int64_u] inside, and [fold_unboxed],
    [fold_frames_unboxed] (whose accumulator has layout [bits64]) and
    [decode_unboxed] hand them out unboxed; [box] makes the record of an unboxed
    one. *)

(** {1 The plan}

    The checked plan both generators print, for backends of other languages:
    nodes in the order a codec checks them, over variables unique in the plan.
*)
module Plan : sig
  (** What a failed check raises: a violated constraint, a value no case of a
      closed enum has, an index past the cases of a lookup. *)
  type kind = Constraint | Enum of int list | Tag

  type check = { cond : bool Wire.Private.Types.expr; kind : kind }
  (** A check: what must hold, and what its failure raises. *)

  type output = {
    id : string;  (** The variable that holds it. *)
    name : string;  (** As {!scanner_fields} names it. *)
    is_bool : bool;  (** A presence. *)
    wire_width : int;  (** Its bits on the wire; 1 for a presence. *)
    signed : bool;
  }

  type field = {
    id : string;  (** The variable that holds its plain value. *)
    name : string;  (** Its name in the codec; empty when anonymous. *)
    label : string;
    offset : int;  (** In its prefix; for a bitfield, its word's. *)
    store : Wire.Private.Layout.store;
    checks : check list;  (** In the order the codec runs them. *)
    enum : (string * int list) option;
    map : string option;
    output : output option;
  }

  type node =
    | Prefix of field list * int
        (** Fields at constant offsets, and the bytes they span. *)
    | Span of int Wire.Private.Types.expr  (** Bytes skipped. *)
    | Gate of bool Wire.Private.Types.expr * output * output list * node list
        (** An optional field: its condition, its presence, the outputs under it
            and its nodes. *)
    | Region of int Wire.Private.Types.expr * bool * node list
        (** A sized window, [true] when its nodes may use less of it. *)
    | Test of bool Wire.Private.Types.expr  (** A where clause. *)

  type t

  (** What a branch establishes about the plan's variables. *)
  module Facts : sig
    type t

    val empty : t
    (** [empty] establishes nothing. *)

    val assume : t -> bool Wire.Private.Types.expr -> bool -> t
    (** [assume f e b] adds to [f] that [e] is [b]. *)

    val simplify :
      t -> bool Wire.Private.Types.expr -> bool Wire.Private.Types.expr
    (** [simplify f e] is [e] with what [f] decides folded away. *)
  end

  val v : 'r Wire.Codec.t -> t
  (** [v c] is the plan of [c]. Raises [Invalid_argument] as {!scanner} does. *)

  val nodes : t -> node list
  (** [nodes t] is the nodes of [t], in the order the codec checks them. *)

  val outputs : t -> output list
  (** [outputs t] is the outputs of [t], in declaration order. *)

  val conditions : field -> bool Wire.Private.Types.expr list
  (** [conditions f] is the conditions of [f]'s checks. *)

  val range : t -> int Wire.Private.Types.expr -> int * int
  (** [range t e] bounds the values [e] takes, from the ranges of its variables;
      [(min_int, max_int)] when it cannot tell. *)
end
