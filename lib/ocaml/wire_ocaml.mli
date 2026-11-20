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
    with [print_string (Wire_ocaml.ml ~codec:"Ipv4.codec" Ipv4.codec)], and its
    interface with {!mli} and the same arguments. Each generator has such an
    interface printer: the interface states the module's public API, each value
    documented from the codec, and hides the rest; on OxCaml it states the modes
    and the [[@zero_alloc]] checks of the implementation. The text is not laid
    out as ocamlformat would; a project that commits the module pipes it through
    [ocamlformat --impl --name=FILE -] (and [--intf] for the interface) in that
    rule, so the committed copy is formatted under the project's own
    [.ocamlformat] and [dune fmt] leaves it alone, and keeps it current with a
    runtest [diff] and [dune promote] or with [(mode promote)].
    [examples/generated] in the sources does this.

    The generated module has [Get.<field> buf off] for each field, which reads
    it as {!Wire.Codec.get} does with one bounds-checked load at a constant
    offset, and [Get.<field>_int] for a 32-bit field's [int] view. [Valid.v buf]
    validates the record at offset 0, as {!Wire.Codec.check} does, and
    [read_<field>] on what it returns is one load with no check of its own. As
    for {!Wire.Codec.check}, the promise is memory safety only: a buffer's
    length never changes, so a read never leaves the buffer, but if its bytes
    change after [Valid.v], a read may return a value that breaks a constraint.
    [(v :> bytes)] is the buffer itself, not a copy. For a bitfield,
    [Get.<field>_word] loads the word that holds it and [<field>_of_word] takes
    the field out, so fields packed in one word share one load.

    [is_valid buf off] checks the record at [off] without raising, loading only
    the fields its constraints read; [validate] answers the codec's error where
    it is false, and [validate_exn] raises it. A view reads a buffer named once
    with [Wire_runtime.Buf.v], as ['b Buf.t], and whose length is read once:
    [View.check b len off] checks the record at [off] without raising,
    [View.ok c] tells whether it passed and [View.get c] turns an answer that
    passed into a ['b View.t], with the same promise as [Valid]: each
    [view_<field> b v] (and [view_<field>_int], [view_<field>_word]) is one load
    with no bounds check, and only the buffer named ['b] is accepted. A view is
    an [int], so it allocates nothing. [View.at] raises the codec's error where
    the check fails, and [View.with_ b len off ~ok ~fail] hands the view to [ok]
    or calls [fail]. A [Bigstring] submodule has the same functions, [Valid] and
    [View] included, on a bigarray of bytes. It has [size], [decode], [validate]
    and [write buf off v1 ... vn], which stores field values in order. Without
    [~make] it also defines a record [t] of the field values, its constructor
    [v], its printer [pp] and [encode]. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [validate], [write_int], [write] and the reads; [decode_with k],
    which takes a continuation for flambda2 to inline, so a [k] that uses a few
    fields reads only those, under one frame check; and, for the generated
    record, [decode_local], which builds it on the stack. *)

val ident : string -> string
(** [ident name] is the OCaml name a generated module derives from the field
    [name], for either target: its getter is [Get.<ident name>], its read
    [read_<ident name>] and its record field [ident name]. It is [name]
    lower-cased, any character other than a letter, a digit or ['_'] turned into
    ['_'], prefixed with ["f_"] when that is empty or starts with a digit, and
    suffixed with ['_'] when it is a keyword of OCaml or of OxCaml, so the code
    for the default target compiles on an OxCaml compiler too. *)

type map
(** The type for a {!Wire.map} the generated code calls by its OCaml path. *)

val map : string -> typ:string -> 'a Wire.typ -> map
(** [map path ~typ m] is the map [m], at the OCaml path [path], whose values
    have the OCaml type [typ], as in
    [map "Page.u16be_int" ~typ:"int" Page.u16be_int]. *)

val ml :
  ?target:target ->
  ?simd:bool ->
  ?make:string * string ->
  ?maps:map list ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [ml ?target ?simd ?make ?maps ~codec c] is the source of a module
    specialised to [c], for [target] (default [`OCaml]). [codec], the OCaml path
    of [c], names it in the module's header; the module itself never refers to
    [c]. [make] is [(f, t)]: [f] the path of a function taking the field values
    in order, which builds the decoded value, and [t] the path of the type it
    returns. It defaults to a generated record, which a codec with a map field
    cannot use.

    A field under a {!Wire.map} is specialised when [maps] names the map. The
    generated module calls the map's functions by its path; {!bridge} checks
    that the field still holds that very map.

    With [~simd:true] (default [false]), for the OxCaml target on arm64 or
    amd64, a 4-byte record whose fields each sit in one byte, unsigned and with
    no check, enum or map, also gets [X4], four records in the lanes of a SIMD
    register. [X4.with_] checks and loads four records, and [X4.<field>] takes a
    field out of each lane. A write builds an [X4.part] from each field's bits,
    [X4.<field>_bits] (its low bits) or [X4.<field>_of] (checked), joined with
    [X4.( ++ )]; since any such bits are a valid record, [X4.store] and
    [X4.fill] check only that the records fit, and raise what [write_int_at]
    would for the first that does not. The generated module then needs the
    runtime's [Vec], which exists on OxCaml for arm64 (NEON) and amd64 (SSE)
    only.

    Raises [Invalid_argument], naming the field and the reason, for a codec it
    cannot specialise: a variable size, parameters, actions, or a map [maps]
    does not name; for [~simd:true] with the [`OCaml] target; and naming both
    fields when two of them would define the same OCaml name ({!ident}), or one
    of them a getter that would shadow a name the getters call, such as
    [Get.cast]. *)

val mli :
  ?target:target ->
  ?simd:bool ->
  ?make:string * string ->
  ?maps:map list ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [mli ?target ?simd ?make ?maps ~codec c] is the interface of
    [ml ?target ?simd ?make ?maps ~codec c]: its types and values, each
    documented, the fields of [c] with their place, width and constraints in the
    preamble. The interface for [`OCaml] is OCaml, so js_of_ocaml and
    wasm_of_ocaml read it too; the one for [`OxCaml] takes buffers [local] and
    states the [[@zero_alloc]] checks and the portability of the implementation.
    Raises [Invalid_argument] as {!ml} does. *)

val bridge :
  ?maps:map list -> codec:string -> core:string -> 'r Wire.Codec.t -> string
(** [bridge ?maps ~codec ~core c] is the source of a module that checks, when it
    loads, that the codec [c] at the path [codec] still has the shape the module
    at the path [core] was generated from by {!ml}, and that each map [maps]
    names is still the very map of its field. It links [wire]; link it next to
    [core] wherever [wire] is linked anyway. *)

val bridge_mli :
  ?maps:map list -> codec:string -> core:string -> 'r Wire.Codec.t -> string
(** [bridge_mli ?maps ~codec ~core c] is the interface of
    [bridge ?maps ~codec ~core c], which defines nothing: its doc comment. *)

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
  ?step:string ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [scanner ?target ?fields ?step ~codec c] is the source of a module that
    checks a record of [c] in a frame of bytes and hands out its scalar fields.
    It covers integers up to 32 bits, bitfields, byte arrays and slices of any
    size, nested codecs, optional fields and fields in a sized region, exact or
    at most; it raises [Invalid_argument] for any other field, parameters and
    actions. The generated module links [wire.runtime], and refuses to load
    where ints have 32 bits or fewer if it holds a value of more than 31 bits.

    [fold buf off len ~invalid ~valid] checks the record at [off] in the [len]
    bytes there, as {!Wire.Codec.decode} does on those bytes alone, and calls
    [valid] with each output as a labelled [int] (a [bool] for a presence), or
    [invalid ()] when the record or the bounds are bad. An absent field is 0.
    [fields] selects the outputs: the others are not read unless a check needs
    them, and every check still runs. [decode buf off len] returns the outputs
    in a record with an [ok] flag, which [pp] prints.

    [n] optional fields give code linear in [n], whether they follow each other
    or nest, and whatever outputs are handed out.
    [fold_frames buf off len ~stride ~init ~invalid ~valid] folds over the
    [len / stride] frames of [stride] bytes, each checked on its own; it raises
    [Invalid_argument] for bad bounds or a [stride] that is not positive.

    [step], the path of a module with a type [t], the accumulator, and
    [valid : t -> f1:int64 -> ... -> fn:int64 -> t] (a presence is a [bool]) and
    [invalid : t -> t], adds [fold_step buf off len ~init] and
    [fold_frames_step buf off len ~stride ~init], which are [fold] and
    [fold_frames] with [init] as the accumulator, calling that module's
    functions by name. OCaml without flambda never inlines a function passed as
    an argument, so only these call the consumer with no closure call. They pass
    each integer to [valid] as an [int64]. Nothing is boxed and no frame
    allocates where the compiler inlines [valid] and the runtime's loads. Dune's
    dev profile compiles [wire.runtime] with [-opaque], which prevents inlining
    the loads. Where ints have 32 bits or fewer, an [int64] is a heap value and
    they run slower than [fold]. The module must be in scope where the generated
    code is compiled. On OxCaml [valid] takes each integer as an [int].

    On OxCaml the outputs are [int64_u] inside, and [fold_unboxed],
    [fold_frames_unboxed] (whose accumulator has layout [bits64]) and
    [decode_unboxed] hand them out unboxed; [box] makes the record of an unboxed
    one. *)

val scanner_mli :
  ?target:target ->
  ?fields:string list ->
  ?step:string ->
  codec:string ->
  'r Wire.Codec.t ->
  string
(** [scanner_mli ?target ?fields ?step ~codec c] is the interface of
    [scanner ?target ?fields ?step ~codec c], as {!mli} is that of {!ml}. Raises
    [Invalid_argument] as {!scanner} does. *)

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
  (** [v c] is the plan of [c]. Raises [Invalid_argument] with the reason for a
      codec whose shape {!scanner} refuses; the names a backend spells its
      outputs with are its own to check. *)

  val nodes : t -> node list
  (** [nodes t] is the nodes of [t], in the order the codec checks them. *)

  val outputs : t -> output list
  (** [outputs t] is the outputs of [t], in declaration order. *)

  val select : t -> string list option -> output list
  (** [select t names] is the outputs of [t] that [names] names, as {!scanner}'s
      [fields] does, in declaration order; all of them for [None]. Raises
      [Invalid_argument] naming a name that no output has. *)

  val slot_depth : int
  (** [slot_depth] is 4: realistic codecs nest optional fields at most that
      deep, and up to it immutable bindings are faster on OCaml without flambda.
  *)

  val slots : t -> output list -> output list
  (** [slots t outputs] is the outputs among [outputs] under more than
      {!slot_depth} optional fields nested inside each other, the presence of
      the innermost included. A walk binds each once, where it starts, to its
      absent value ([false], zero) in a mutable variable that it assigns where
      its field is present, so an absent field and the code after one write
      nothing for them. Any other output is immutable: bound where its field is
      read, to its absent value where the field is absent, and handed to the
      code after the field. *)

  val live : t -> output list -> string -> bool
  (** [live t outputs v] is [true] when a walk handing out [outputs] binds the
      variable [v]: one of [outputs], or one a check, a size or a condition
      reads. *)

  val conditions : field -> bool Wire.Private.Types.expr list
  (** [conditions f] is the conditions of [f]'s checks. *)

  val word_range :
    field ->
    field ->
    ((int * Wire.Private.Types.bitfield_base) * int * int) option
  (** [word_range f g] is [Some (word, lo, hi)] when [f] is the top bits of the
      word at [word] (its offset in the prefix and its base), [g] the bits right
      below, and their checks [f = c] and [g >= d] hold exactly when the word
      lies in [lo, hi]. A backend that names no failed field can test the word
      once. *)

  val range : t -> int Wire.Private.Types.expr -> int * int
  (** [range t e] bounds the values [e] takes, from the ranges of its variables;
      [(min_int, max_int)] when it cannot tell. *)

  (** What the code at a point of a walk knows: the ranges of the variables,
      narrowed by the checks passed on the way, and the facts those establish.
      The arithmetic decisions below are the ones {!scanner} makes. *)
  module Scope : sig
    type plan := t
    type t

    val v : plan -> t
    (** [v p] knows the ranges of [p]'s variables and no fact. *)

    val facts : t -> Facts.t
    (** [facts s] is the facts [s] knows. *)

    val refine : t -> bool Wire.Private.Types.expr list -> t
    (** [refine s conditions] is [s] after checks of [conditions] passed. *)

    val range : t -> int Wire.Private.Types.expr -> int * int
    (** [range s e] bounds the values [e] takes, as {!val-range} does with the
        ranges [s] knows. *)

    val checked : t -> int Wire.Private.Types.expr -> bool
    (** [checked s e] is [true] when [e] is an addition, a subtraction or a
        product that the ranges of its operands do not keep inside the native
        int: the code checks it, and rejects the record where it overflows, as
        the codec does. Any other is plain arithmetic. *)

    val nonnegative : t -> int Wire.Private.Types.expr -> bool
    (** [nonnegative s e] is [true] when [e] cannot be negative, by its range or
        by a passed check comparing the operands of a subtraction. *)
  end

  (** {2 Optional fields}

      A run of optional fields whose conditions exclude each other is one
      decision tree, which tests each atom of their conditions at most once on a
      path. *)

  type tree = { tid : int; node : tnode }
  (** A node of a decision tree; [tid] identifies it. *)

  (** A leaf names the case that runs, or none. *)
  and tnode =
    | Leaf of int option
    | Branch of bool Wire.Private.Types.expr * tree * tree

  val gates :
    Facts.t ->
    node list ->
    (bool Wire.Private.Types.expr * output * output list * node list) list
    * node list
  (** [gates f nodes] is the run of optional fields at the head of [nodes] whose
      conditions, simplified by [f], exclude each other, each with its condition
      so simplified, and the nodes after the run. *)

  val decide : Facts.t -> (int * bool Wire.Private.Types.expr) list -> tree
  (** [decide f cases] is the decision tree that picks the first of [cases]
      whose condition holds, or none. A subtree reached from two paths is one
      value. *)

  val shared : tree -> (tree -> bool) * (int -> bool)
  (** [shared t] tells which branches of [t] are reached more than once, and
      which cases are, so a writer gives each of those one definition. *)
end
