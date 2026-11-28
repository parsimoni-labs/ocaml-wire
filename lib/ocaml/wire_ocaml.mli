(** OCaml code generated from a codec.

    [Wire_ocaml] writes the {e generated} form of a codec: an OCaml module with
    the concepts of the interpreted form, {!Wire.Codec.View}, under the same
    names. A check, its [is_ok] and [get], a view, and [decode], [validate] and
    [encode] are values of the module; each field is a function named after it,
    its accessor, with [set_<field>] beside it; and the module has at most one
    fold, its fields named when the module is generated. Switching from one form
    to the other is a rename: [View.check packet b ~off] is
    [Space_packet.check b ~off], [View.read b v apid_f] is
    [Space_packet.apid b v]. {{!modules}The generated module} gives the
    signatures every module meets.

    The module states each offset as a constant, so the compiler sees
    straight-line loads and stores, and a field at a constant place is one load.
    Its code holds no integer literal wider than 31 bits, so js_of_ocaml and
    wasm_of_ocaml compile it as written; a module that reads a 32-bit field into
    an [int] raises [Invalid_argument] when it loads on a target whose ints have
    32 bits or fewer. An expression in a constraint or a [where] clause that can
    leave the native int is computed on 64 bits, as unsigned integers, as the
    codec and 3D compute it.

    {b Runtime.} The module links [wire.runtime] only, not [wire], and needs no
    codec at run time. Its integer types are [Wire_runtime.UInt8.t] and the
    like, which are {!Wire.UInt8.t} and the like where [wire] is linked, and it
    answers [Wire_runtime.Error.t], which is {!Wire.Error.t}, with the value the
    codec answers. The exceptions are a module generated with [~maps], whose
    maps are [Wire] values, so it links [wire]; and a module whose codec holds a
    byte slice, which it hands out as a [Bytesrw.Bytes.Slice.t] and so links
    [bytesrw].

    A generator executable declares its modules with {!v} and hands them to
    {!main}, which writes the files a dune rule's command line names:
    {v
    (* gen.ml *)
    let () = Wire_ocaml.(main [ v ~fold:[ "apid" ] Space.packet_codec ])
    v}
    {v
    (rule
     (targets space_packet.ml space_packet.mli)
     (action (run ./gen.exe %{targets})))
    v}
    The text is not laid out as ocamlformat would: a project that commits the
    module pipes it through [ocamlformat --impl --name=FILE -] (and [--intf] for
    the interface) in that rule, and keeps it current with a runtest [diff] and
    [dune promote]. [examples/generated] in the sources does this. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [write] and on the check [validate] runs, holds the values its
    walks read as unboxed [int64_u], and takes the buffers it does not keep
    [local]. *)

type map
(** The type for a {!Wire.map} the generated code calls by its OCaml path. *)

val map : string -> typ:string -> 'a Wire.typ -> map
(** [map path ~typ m] is the map [m], at the OCaml path [path], whose values
    have the OCaml type [typ], as in
    [map "Page.u16be_int" ~typ:"int" Page.u16be_int]. *)

(** {1:generating Generating} *)

type t
(** The type for the module generated from a codec. *)

val v :
  ?name:string ->
  ?target:target ->
  ?simd:bool ->
  ?fold:string list ->
  ?maps:map list ->
  'r Wire.Codec.t ->
  t
(** [v ~name ~target ~simd ~fold ~maps c] is the module of [c], named [name]
    (defaults to the codec's name, split at its words and lower-cased:
    [CapturedIPv4] is [captured_ipv4]), for [target] (defaults to the compiler
    the generator runs on). The module never refers to [c].

    [fold] names the fields the module's [fold] hands its visitor, each by its
    path: the accessor names from the codec down, joined with ['.'], as
    ["ipv4.tcp.dst_port"]. The visitor takes each field labelled by its path,
    the names joined with ['_'] ([~ipv4_tcp_dst_port]), so two fields of one
    type cannot be swapped. Each is an integer field, of the type its accessor
    answers, an option where the field is optional; a field under a layer is one
    option more, as {!Wire.Codec.( / )} types it, [None] where an optional layer
    of its path is absent. Without [fold] the module has no fold; two field sets
    over one codec are two modules, each with its own [name].

    A field under a {!Wire.map} is specialised when [maps] names the map: the
    record holds the map's OCaml type, and the module calls the map's functions
    by its path.

    With [~simd:true] (defaults to [false]), for the OxCaml target on arm64 or
    amd64, a 4-byte record whose fields each sit in one byte, unsigned and with
    no check, enum or map, also gets [X4], four records in the lanes of a SIMD
    register. The module then needs the runtime's [Vec], which exists on OxCaml
    for arm64 (NEON) and amd64 (SSE) only.

    Raises [Invalid_argument], naming the field and the reason, for a codec no
    module covers: parameters, actions, a field of a type it does not read (a
    repetition, a tagged union), or a map [maps] does not name; for a path of
    [fold] that names no field or a field no fold reads, and for two paths of
    [fold] that give one label; for [~maps] or [~simd:true] on a codec the
    module does not cover; for [~simd:true] with the [`OCaml] target; and naming
    both fields when two of them would define the same OCaml name. *)

val name : t -> string
(** [name t] is the name of [t]'s module: its file is [name t ^ ".ml"]. *)

val to_ml : t -> string
(** [to_ml t] is the source of [t]'s module. *)

val to_mli : t -> string
(** [to_mli t] is the interface of [to_ml t]: the module's public API, each
    value documented from the codec. On OxCaml it states the modes and the
    [[@zero_alloc]] checks of the implementation. *)

val main : t list -> unit
(** [main ts] is a generator's command line, [gen.exe FILE...]. It writes each
    [FILE], [<name>.ml] or [<name>.mli], the module of [ts] named [<name>] or
    its interface; anything after the extension is ignored, so [stack.ml.raw] is
    the module [stack] too. It exits with status 2 and a message, before it
    writes any file, on a file that names no module of [ts]. *)

(** {1:modules The generated module}

    The signatures every module {!v} writes meets, so a test ascribes it to
    them: [module _ : Wire_ocaml.FIXED = Space_packet]. What no signature can
    name comes from the codec's fields:
    - [<field> b v], the accessor of each named field, returning its value as
      the codec decodes it: one load for a field at a constant place inside the
      bytes the check guarantees. A field that holds a record answers the check
      of the record's submodule, which passes iff the layer is present.
    - [set_<field> b v x] for each field at a constant place, which writes [x]
      in place, checks none of the codec's constraints, and raises
      [Invalid_argument] on a value the field cannot hold.
    - One submodule per codec a field holds, named after that codec ([StackIPv4]
      is [Stack_ipv4]) and written once however many fields hold it. It meets
      {!RECORD} and has the accessors and the writers of its own fields.
    - [write buf ~off ~f1 ... ~fn] for a codec of constant size, which writes
      the record of these field values.

    An accessor named as a value the module defines ([size], [check], [decode],
    ...) takes a trailing ['_'], as a keyword does.

    The signatures are those of the [`OCaml] target. A module for [`OxCaml]
    takes every buffer it does not keep [local]: [check], [validate], [decode],
    [encode], the accessors, the setters, [write] and [fold] take
    [bytes @ local] or ['b Wire.Buf.t @ local], so a caller passes a buffer
    allocated on its stack. A function of a [local] buffer is no function of a
    global one, so such a module meets these signatures with each such buffer
    argument [local], and a test ascribes it to that copy of them. *)

(* Open question 1, measured: a view does not carry its buffer. *)

(** The records of a codec and their views, which every module and submodule
    has. *)
module type RECORD = sig
  type t
  (** The type for the records, decoded: an optional field is an option, a field
      that holds a record that record's submodule's [t]. *)

  type 'b check = private int
  (** The type for checks of a record in the buffer named ['b]. *)

  type 'b view = private int
  (** The type for records that passed their check in the buffer named ['b]. *)

  val is_ok : 'b check -> bool
  (** [is_ok k] is [true] iff the check [k] passed. *)

  val get : 'b check -> 'b view
  (** [get k] is the view of the record the check [k] passed. Raises
      [Invalid_argument] if [is_ok k] is [false]: a refused record is input,
      test {!is_ok} first. *)

  val size : t -> int
  (** [size r] is the number of bytes [r] takes encoded. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] prints an unspecified representation of [r] on [ppf]. *)
end

(** A module, whatever the size of its codec. *)
module type S = sig
  include RECORD

  val min_size : int
  (** [min_size] is the fewest bytes a record takes. *)

  val validate : bytes -> off:int -> (unit, Wire.Error.t) result
  (** [validate buf ~off] is [Ok ()] if the record at [off] in [buf] passes the
      codec's checks, and the codec's error otherwise. Raises [Invalid_argument]
      if [off] is negative. *)

  val decode : bytes -> off:int -> (t, Wire.Error.t) result
  (** [decode buf ~off] is the record at [off] in [buf], or the codec's error.
      Raises [Invalid_argument] if [off] is negative. *)

  val encode : t -> bytes -> off:int -> unit
  (** [encode r buf ~off] writes [r] at [off] in [buf]. Raises
      [Invalid_argument] where the codec's encoder does. *)
end

(** A module of a codec of constant size. *)
module type FIXED = sig
  include S

  val check : 'b Wire.Buf.t -> off:int -> 'b check
  (** [check b ~off] is the check of the record at [off] in [b]. It never raises
      and allocates nothing: a negative [off] or a record [b] does not hold is
      refused. *)
end

(** A module of a codec of variable size, which checks a record in its window.
*)
module type VARIABLE = sig
  include S

  val check : 'b Wire.Buf.t -> off:int -> len:int -> 'b check
  (** [check b ~off ~len] is the check of the record at [off] in the [len] bytes
      there. It never raises and allocates nothing: a window [b] does not hold
      is refused. *)
end

(** The fold of a module generated with [~fold]. *)
module type FOLD = sig
  type 'acc visitor
  (** The type for the visitor: ['acc -> l1:x1 -> ... -> ln:xn -> 'acc], over
      the fields the [~fold] of {!val-v} names, each labelled by its path. A
      test substitutes it to ascribe a module:
      [module _ : FOLD with type 'acc visitor := 'acc -> apid:int -> 'acc =
       Space_packet]. *)

  val fold :
    ?stride:int ->
    ?offs:int array ->
    ?lens:int array ->
    ?invalid:('acc -> 'acc) ->
    bytes ->
    init:'acc ->
    f:'acc visitor ->
    ('acc, Wire.Error.t * 'acc) result
  (** [fold buf ~init ~f] is {!Wire.Codec.View.fold} of the module's codec and
      fields: the records lie back to back from offset [0], in slots of [stride]
      bytes, or in the frames of [lens.(i)] bytes at [offs.(i)]; a refused
      record stops the fold with [Error (e, acc)], or in slots or frames is
      [invalid acc]. Raises [Invalid_argument] as that fold does. *)
end

(**/**)

(** {1 Private}

    The plan, the outputs and the views a module is written from, and the F*
    proof of its fold, for [wire.rust] and the Hardcaml backend. Not part of the
    API: it changes with them. *)

val outputs : 'r Wire.Codec.t -> (string * bool) list
(** [outputs c] is the outputs a fold of [c] hands out, in declaration order:
    each named scalar field, its name the path of field labels joined with
    ['_'], and for each optional field [<path>_present], flagged [true]. A field
    in an anonymous one is checked but not an output. A field's label is the
    OCaml name the module gives it, its record field and its accessor unless the
    module defines that name ({{!views}Views}), for either target: the field's
    name lower-cased, any character other than a letter, a digit or ['_'] turned
    into ['_'], prefixed with ["f_"] when that is empty or starts with a digit,
    and suffixed with ['_'] when it is a keyword of OCaml or of OxCaml, so the
    code for the default target compiles on an OxCaml compiler too. Raises
    [Invalid_argument] for a codec no fold covers. *)

type view = { module_name : string option; members : view_member list }
(** The views of a record: its module, [None] for the codec's own, and its
    accessors in declaration order. *)

(** An accessor and the output of {!outputs} it reads. *)
and view_member =
  | Int of { name : string; output : string; carrier : string option }
      (** An integer field, and the module of its accessor's type when that is
          not [int]: ["Wire_runtime.UInt16"] for a 16-bit integer. *)
  | Bool of { name : string; output : string }
      (** The presence of an optional field that holds no record. *)
  | Record of { name : string; presence : string option; view : view }
      (** A field that holds a record, with the output of its presence when it
          is optional. *)
  | Bytes of { name : string; slice : bool }
      (** A named field of bytes, which no fold hands out: a
          [Bytesrw.Bytes.Slice.t] when [slice], a [string] otherwise. *)

val views : 'r Wire.Codec.t -> view
(** [views c] is the views the module of [c] has. Raises [Invalid_argument] for
    a codec no fold covers. *)

val fstar :
  ?fields:string list ->
  codec:string ->
  'r Wire.Codec.t ->
  (string * string) list
(** [fstar ?fields ~codec c] is the F* proof that the fold [fold_step] over the
    outputs [fields] of [c] answers what the parser EverParse writes for the 3D
    projection of [c] answers on the same bytes: each F* file of the proof, by
    name. [<codec>.Step.fst] is [fold_step] and [fold_step_within], from the
    walk that writes the OCaml, values held as [int64], its operations and loads
    written as calls into the primitives [WireGen.OCaml], calling the module
    [<codec>.Visitor], which [<codec>.Visitor.fsti] declares and leaves
    abstract; [<codec>.Flat.fst] reads the record field by field and proves that
    reading equal to the parser; [<codec>.StepProof.fst] proves
    [fold_step_is_3d]: on a window inside the buffer,
    [fold_step buf off len init] is [S.valid init] applied to the outputs of the
    value the parser reads from the window's bytes, each as its [int64], or
    [S.invalid init] where it reads none. [<codec>.OxStep] and
    [<codec>.OxStepProof] are the same of that module for [~target:`OxCaml],
    each [int64_u] written as its 64-bit pattern. [<codec>.Frames] and
    [<codec>.Index] (and [<codec>.OxFrames], [<codec>.OxIndex]) are the walks of
    a frame of the loops of [fold_step_frames] and [fold_step_index], each a
    function whose parameters are typed with what the loop ensures of them, and
    their [Proof] modules prove [frame_is_3d]: such a walk answers what the
    parser answers on the frame's bytes.

    The proof is checked with the F* and Z3 that ship with EverParse, against
    the F* module [codec] that 3d.exe writes from the [.3d] of
    [Wire.Everparse.write ~mode:`Standalone ~name:codec], and the library
    [proofs/fstar-gen/lib] of this repository. It covers records whose fields
    are unsigned big-endian integers of up to 32 bits, bitfields of such words,
    byte arrays of any size, nested codecs, optional codecs of a variable size
    and codecs in a sized region, with constraints; it raises [Invalid_argument]
    for any other codec, and for one whose fold holds an output in a mutable
    local. *)

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
    name : string;  (** As {!outputs} names it. *)
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
      codec whose shape {!v} refuses; the names a backend spells its outputs
      with are its own to check. *)

  val nodes : t -> node list
  (** [nodes t] is the nodes of [t], in the order the codec checks them. *)

  (** {2:decisions Decisions}

      What {!v} decides, beyond the plan, that changes which checks the
      generated code runs or which bytes it reads. The module's walks are
      written from these decisions, so they are what that code does. *)

  type pos = { var : string option; at : int }
  (** A position in a window: a variable when the data decides it, plus a
      constant. *)

  type values = [ `Int | `Int64 | `Int64_u ]
  (** How a walk holds integer values: on OCaml as [int] in the walk of a
      generated module's check and as [int64] in its folds, or as [int64_u] on
      OxCaml. Positions and sizes are [int]. *)

  (** How a span's bound is written. *)
  type bound =
    | Next  (** Left to the check of the bytes right after the span. *)
    | Hoisted
        (** Checked once before the dispatch whose cases start with it. *)
    | Checked of {
        negative : bool;  (** With a test that its size is negative. *)
        cancelled :
          (int Wire.Private.Types.expr * int Wire.Private.Types.expr) option;
            (** As [lhs > rhs], the terms its position and size share cancelled.
            *)
      }  (** Checked where it is. *)

  (** Which case of a dispatch runs. *)
  type choice =
    | Run of int option  (** The case of that index, or none. *)
    | If of bool Wire.Private.Types.expr * choice * choice
        (** A test, and where it leads when it holds and when it does not. *)

  (** Where the code after a dispatch is written. *)
  type join =
    | Joined  (** Once, in a local function every case jumps to. *)
    | Copied  (** Into each case, which keeps what the case established. *)

  (** How a frame loop walks frames shorter than the bytes it leaves unchecked.
  *)
  type short_frames =
    | Apart  (** Through [fold], in a function of their own. *)
    | Inline  (** Through [fold], in the frame loop's function. *)

  type decision =
    | Walk of { name : string; values : values; known : int }
        (** A walk of a record starts in the function [name], with [known] bytes
            of its window known to be there. *)
    | Prefix_bound of { at : pos; window : string; bytes : int; tested : bool }
        (** The run of fields from [at] needs [bytes] bytes of the window
            [window]; [tested] when a test of its length is written, otherwise
            the bytes are already known to be there. *)
    | Span_bound of {
        size : int Wire.Private.Types.expr;
        at : pos;
        window : string;
        size_var : string;  (** The variable its size is bound to. *)
        bound : bound;
      }  (** A span or a region of [size] bytes at [at]. *)
    | Shared_words of {
        at : int;
        constant : bool;  (** The position is a constant of the window. *)
        lo : int;
        hi : int;
        window : string;
        fields : (int * int) list;
            (** The offset and the width in bytes of each field a word may hold.
            *)
        held : int list;  (** The words loaded before that it reads. *)
        loads : int list;  (** The words it loads. *)
      }
        (** In the [int64] form, the 64-bit words a run of fields at [at] loads
            once and reads several fields from. Offsets are from the run's
            address; every word lies in [[lo, hi]]. *)
    | Word_range of {
        at : pos;
        window : string;
        first : string;
        second : string;
        lo : int;
        hi : int;
      }
        (** The checks of the bitfields [first] and [second] of the run at [at]
            are one test that their word is in [[lo, hi]]. *)
    | Dispatch of {
        at : pos;
        window : string;
        cases : int;
        conditions : bool Wire.Private.Types.expr list;
            (** The condition of each case, in order. *)
        choice : choice;
        join : join;
        hoisted : int Wire.Private.Types.expr option;
      }
        (** A run of [cases] optional fields at [at], each tested condition
            evaluated at most once on a path; with [hoisted], the size of the
            span every case starts with, whose bound is checked once before. *)
    | Cut of { name : string; window : string; known : int }
        (** The rest of a walk is written as the function [name], in the window
            [window] with [known] bytes known to be there. *)
    | Frame_loop of {
        name : string;
        need : int;
        frames : int;
        short : short_frames option;
        ahead : (string * int * int) list;
      }
        (** The loop [name] over frames walks [frames] frames an iteration, each
            with the [need] bytes of its leading fields known once its stride
            is; [short] walks the frames shorter than that. An iteration of
            several frames loads the fields [ahead] of each of them before the
            first frame's checks: the fields its optional fields are chosen on,
            so that a mispredicted branch of one frame does not hold back the
            next frame's choice by a load. Each is the field's variable, its
            offset from the frame's start and the bytes loaded there. *)

  val pp_decision : Format.formatter -> decision -> unit
  (** [pp_decision ppf d] prints [d] on one line, its tests as
      {!Wire.Private.Types.pp_expr} does. *)

  val decisions :
    ?target:target -> ?fold:string list -> 'r Wire.Codec.t -> decision list
  (** [decisions ~target ~fold c] is the decisions [to_ml (v ~target ~fold c)]
      takes, in the order it writes them, each case of an optional field
      included, with [target] [`OCaml] and no fold by default. A [window] is the
      variable holding the window's length: the frame's, or a region's size.
      Raises [Invalid_argument] as {!v} and {!to_ml} do. *)

  val outputs : t -> output list
  (** [outputs t] is the outputs of [t], in declaration order. *)

  val select : t -> string list option -> output list
  (** [select t names] is the outputs of [t] that [names] names, as {!v}'s
      [fold] does, in declaration order; all of them for [None]. Raises
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
      The arithmetic decisions below are the ones {!v} makes. *)
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

    val wide : t -> int Wire.Private.Types.expr -> bool
    (** [wide s e] is [true] when [e] or one of its subexpressions can take a
        value past the native int, or shifts by its width or more: the code
        computes it on 64 bits, as unsigned integers, as the codec and 3D do.
        Any other is plain arithmetic on the native int. *)

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

  (** {2:records Records}

      The records the views of a module reach, each planned on its own: in the
      plan of a record, each record a field of it holds is a {!Span} of a
      variable standing for its size, so the plan holds the record's own fields
      only. *)

  type sub = {
    var : string;  (** The variable that stands for its size. *)
    path : string list option;
        (** The field's path, [None] under an anonymous field. *)
    codec_id : int;
    codec_name : string;
    fixed_size : int option;  (** Its size, when constant. *)
    codec : Wire.Private.Types.struct_;
  }
  (** A record a field holds. *)

  (** A named member of a record's view, named as its accessor. *)
  type member =
    | Scalar of { name : string; field : field }  (** An integer field. *)
    | Present of { name : string; presence : output }
        (** The presence of an optional field that holds no record. *)
    | Sub of { name : string; sub : sub; presence : output option }
        (** A record a field holds, with its presence when optional. *)
    | Bytes of { name : string; label : string; at : bytes_at; slice : bool }
        (** A named field of bytes, its label, where its bytes are, and whether
            it is a slice rather than a string. *)

  (** Where the bytes of a field are: at a constant offset in a prefix, or in a
      span of the plan. *)
  and bytes_at = Fixed of field | Sized of node

  type record = {
    id : int;  (** The id of its codec; [-1] for the root. *)
    codec_name : string;
    module_name : string option;  (** [None] for the root. *)
    struct_ : Wire.Private.Types.struct_;
    plan : t;  (** Its plan, the records it holds kept apart. *)
    subs : sub list;
    fixed : int;  (** The bytes at constant offsets a view of it holds. *)
    fixed_size : int option;
    members : member list;
  }
  (** A record a view reaches. *)

  type records = { root : record; records : record list; size : int option }
  (** The records of a codec: its root, every record it holds, each once, those
      a record holds before it, and the codec's size when constant. *)

  val records : ?reserved:string list -> 'r Wire.Codec.t -> records
  (** [records ?reserved c] is the records the views of [c]'s module reach, with
      its accessors named as {!views} names them, a name of [reserved] escaped
      as a keyword is. Raises [Invalid_argument] with the reason for a codec no
      fold covers. *)

  val settable : record -> (string * field) list
  (** [settable r] is the fields of [r] a setter writes in place, each with its
      accessor's name: an integer or a bitfield at a constant offset inside the
      bytes a view of [r] holds. *)
end
