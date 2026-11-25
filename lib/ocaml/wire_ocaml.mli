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
    it answers [Wire_runtime.Error.t], which is {!Wire.Error.t}, with the value
    the codec answers, and raises the codec's [Invalid_argument] messages. The
    exceptions are a module generated with [~maps]: a map's functions are a
    [Wire] value, so that module links [wire]; and a module whose codec holds a
    byte slice, which it hands out as a [Bytesrw.Bytes.Slice.t], as the codec
    does, and so links [bytesrw]. The module carries the digest of its codec's
    shape, which its interpreted twin ({!to_ml}) checks against the codec.

    An expression in a constraint or a [where] clause that can leave the native
    int is computed on 64 bits, as unsigned integers, as the codec and 3D
    compute it. The generator runs with the 63-bit ints of a 64-bit host and
    decides which expressions can leave them there. That decision holds wherever
    the module loads: a module that holds a value of more than 31 bits, a 32-bit
    field or arithmetic past [2^30], raises [Invalid_argument] when it loads on
    a target whose ints have 32 bits or fewer, and any other computes the same
    values in every [int].

    A generator executable declares its modules with {!v} and hands them to
    {!main}, which writes the files a dune rule's command line names; see
    {{!generating}Generating}. Each module has an interface: it states the
    module's public API, each value documented from the codec, and hides the
    rest; on OxCaml it states the modes and the [[@zero_alloc]] checks of the
    implementation. The text is not laid out as ocamlformat would; a project
    that commits the module pipes it through [ocamlformat --impl --name=FILE -]
    (and [--intf] for the interface) in that rule, so the committed copy is
    formatted under the project's own [.ocamlformat] and [dune fmt] leaves it
    alone, and keeps it current with a runtest [diff] and [dune promote] or with
    [(mode promote)]. [examples/generated] in the sources does this.

    {b The module.} Every module has [check], [ok] and [get], its views
    ({{!views}Views}), with an accessor per named field and [set_<field>] for a
    field at a constant offset; [min_size]; [validate buf off], which answers
    the codec's error where the record is bad; [decode buf off], the record as
    the module's record type [t]; [encode r buf off]; [size r], the bytes
    [encode] writes; [pp]; and the folds it declares ({!fold}). A codec of
    constant size also has [write buf off ~f1 ... ~fn], which writes the record
    of these field values, [Get.<field> buf off], which reads one field with one
    bounds-checked load at a constant offset, and, for a bitfield,
    [Get.<field>_word] and [<field>_of_word], so fields packed in one word share
    one load. Bad input never makes a decoder raise.

    With [~buffer:`Bigstring] the module reads and writes a bigarray of bytes
    instead, with the same functions and the same errors, and no walk:
    [Valid.v buf] validates the record at offset 0 and [read_<field>] on what it
    returns is one load with no check of its own; [View.check b len off],
    [View.ok], [View.get] and [view_<field> b v] are its views, and
    [View.with_ b len off ~ok ~fail] hands the view to [ok] or calls [fail]. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. [`OxCaml] adds [[@zero_alloc]]
    checks on [is_valid], [write_int], [write] and the reads, and
    [decode_with k ~fail], which takes continuations for flambda2 to inline, so
    a [k] that uses a few fields reads only those, under one frame check. *)

type buffer = [ `Bytes | `Bigstring ]
(** The buffer a generated module reads and writes: [bytes], or a bigarray of
    bytes ([Wire_runtime.Unchecked.Bigstring.t]). The buffer is chosen when the
    module is generated; a program that reads both generates two modules. *)

val ident : string -> string
(** [ident name] is the OCaml name a generated module derives from the field
    [name], for either target: its getter is [Get.<ident name>] and its record
    field [ident name]. It is [name] lower-cased, any character other than a
    letter, a digit or ['_'] turned into ['_'], prefixed with ["f_"] when that
    is empty or starts with a digit, and suffixed with ['_'] when it is a
    keyword of OCaml or of OxCaml, so the code for the default target compiles
    on an OxCaml compiler too. *)

type map
(** The type for a {!Wire.map} the generated code calls by its OCaml path. *)

val map : string -> typ:string -> 'a Wire.typ -> map
(** [map path ~typ m] is the map [m], at the OCaml path [path], whose values
    have the OCaml type [typ], as in
    [map "Page.u16be_int" ~typ:"int" Page.u16be_int]. *)

(** {1:generating Generating}

    A generator executable declares the modules of its codecs and hands them to
    {!main}, which writes the files its command line names:
    {v
    let () =
      Wire_ocaml.(
        main
          [
            v Space.clcw_codec;
            v Packet_stack.capture
              ~folds:[ fold "classify" ~outputs:[ "ipv4_src"; "ipv4_dst" ] ];
          ])
    v}
    One dune rule writes each module, its interface and the visitors its folds
    call:
    {v
    (rule
     (targets captured_stack.ml captured_stack.mli)
     (action
      (run ./gen.exe %{targets} --visitor classify=%{dep:flow_step.ml})))
    v} *)

type fold
(** The type for the folds a module declares. *)

val fold : ?outputs:string list -> string -> fold
(** [fold ?outputs name] is the fold [name], which checks the record of a frame
    and hands [outputs] (default: every one, {!outputs}) to a consumer; the
    others are not read unless a check needs them, and every check still runs.
    Its consumer is the visitor the module is written with for [name] (see
    {!to_ml}): the module [V] of the visitor's source file, with the type [V.t]
    of its accumulators, [V.valid : V.t -> o1:int64 -> ... -> on:int64 -> V.t]
    (a presence is a [bool]) and [V.invalid : V.t -> V.t], which the fold calls
    by name. Its functions are [name buf off len ~init], one frame;
    [name_frames buf off len ~stride ~init], the [len / stride] frames of
    [stride] bytes at [off]; and [name_index buf ~offs ~lens ~init], the frames
    of [lens.(i)] bytes at [offs.(i)], reading the length of [buf] once. OCaml
    without flambda never inlines a function passed as an argument, so the
    visitor is called with no closure call, and nothing is boxed where the
    compiler inlines [valid] (docs/execution.md, Generated code). Without a
    visitor, the fold takes its consumer as arguments:
    [name buf off len ~invalid ~valid] and
    [name_frames buf off len ~stride ~init ~invalid ~valid], and on OxCaml
    [name_unboxed] and [name_frames_unboxed], which hand the outputs out
    unboxed. *)

type t
(** The type for the module generated from a codec. *)

val v :
  ?name:string ->
  ?target:target ->
  ?buffer:buffer ->
  ?simd:bool ->
  ?folds:fold list ->
  ?maps:map list ->
  'r Wire.Codec.t ->
  t
(** [v ?name ?target ?buffer ?simd ?folds ?maps c] is the module of [c], named
    [name] (default: the codec's name, split at its words and lower-cased:
    [CapturedIPv4] is [captured_ipv4]), for [target] (default: the compiler the
    generator runs on), on [buffer] (default [`Bytes]), with [folds] (default:
    one fold [fold] of every output, without a visitor, when the module has
    folds). The module never refers to [c].

    A field under a {!Wire.map} is specialised when [maps] names the map: the
    record holds the map's OCaml type, and the module calls the map's functions
    by its path.

    With [~simd:true] (default [false]), for the OxCaml target on arm64 or
    amd64, a 4-byte record whose fields each sit in one byte, unsigned and with
    no check, enum or map, also gets [X4], four records in the lanes of a SIMD
    register. [X4.with_] checks and loads four records, and [X4.<field>] takes a
    field out of each lane. A write builds an [X4.part] from each field's bits,
    [X4.<field>_bits] (its low bits) or [X4.<field>_of] (checked), joined with
    [X4.( ++ )]; since any such bits are a valid record, [X4.store] and
    [X4.fill] check only that the records fit, and raise what [write_int_at]
    would for the first that does not. The module then needs the runtime's
    [Vec], which exists on OxCaml for arm64 (NEON) and amd64 (SSE) only.

    The walk covers integers up to 32 bits, bitfields, byte arrays and slices of
    any size, nested codecs, optional fields and fields in a sized region, exact
    or at most; a fixed record of named scalars at constant offsets also takes
    wider integers and maps. Raises [Invalid_argument], naming the field and the
    reason, for a codec no part of the module covers: parameters, actions, a
    field of a type it does not read (a repetition, a tagged union), or a map
    [maps] does not name; for [folds], [~maps] or [~simd:true] on a codec the
    walk, or the fixed record, does not cover; for [~simd:true] with the
    [`OCaml] target; naming both fields when two of them would define the same
    OCaml name ({!ident}), or one of them a getter that would shadow a name the
    getters call, such as [Get.cast]; and naming the names, for a fold whose
    functions take a name the module defines or another fold's. *)

val name : t -> string
(** [name t] is the name of [t]'s module: its file is [name t ^ ".ml"]. *)

val to_ml : ?twin:bool -> ?visitors:(string * string) list -> t -> string
(** [to_ml ?twin ?visitors t] is the source of [t]'s module. Each of [visitors]
    is a fold's name and the source file of its visitor, whose module
    ([flow_step.ml] is [Flow_step]) the fold calls by name; a fold that none
    names takes its consumer as arguments.

    With [~twin:true], it is the source of the module's interpreted twin
    instead, the unit [<name>_codec] beside the module: a functor [Make] over a
    module [C] with a type [r] and [codec : r Wire.Codec.t], which reads
    [C.codec] as [t]'s module reads its bytes, through [Wire.Codec.View] and
    [Wire.Codec.validate], with the views, accessors and setters, [min_size],
    [validate] and the folds of [t]'s module, under the interface [t]'s module
    has for the [`OCaml] target. Each field is found by its place in the codec's
    field list and checked against the type the module reads, and [Make] raises
    [Invalid_argument] unless the codec has the shape [t] was generated from.
    The twin links [wire]; code written against those members runs on either by
    changing one line:
    {v
    module P = Captured_stack
    module P = Captured_stack_codec.Make (struct
      type r = Packet_stack.captured
      let codec = Packet_stack.capture
    end)
    v}
    Raises [Invalid_argument] for a fold [t] does not declare, and with
    [~twin:true] for a record the twin cannot reach through one field. *)

val to_mli : ?twin:bool -> ?visitors:(string * string) list -> t -> string
(** [to_mli ?twin ?visitors t] is the interface of [to_ml ?twin ?visitors t]. *)

val main : t list -> unit
(** [main ts] is a generator's command line,
    [gen.exe [--visitor FOLD=FILE]... FILE...]. It writes each [FILE]:
    [<name>.ml] and [<name>.mli], the module of [ts] named [<name>] and its
    interface, and [<name>_codec.ml] and [<name>_codec.mli], its interpreted
    twin ({!to_ml}); anything after the extension is ignored, so [stack.ml.raw]
    is the module [stack] too. [--visitor FOLD=FILE] gives each written module's
    fold [FOLD] the visitor of the source file [FILE]: naming the visitor in the
    rule makes dune rebuild the module when it changes. It exits with status 2
    and a message on a file that names no module of [ts] and on a visitor of a
    fold no written module declares. *)

val outputs : 'r Wire.Codec.t -> (string * bool) list
(** [outputs c] is the outputs a fold of [c] hands out, in declaration order:
    each named scalar field, its name the path of field labels ({!ident}) joined
    with ['_'], and for each optional field [<path>_present], flagged [true]. A
    field in an anonymous one is checked but not an output. Raises
    [Invalid_argument] for a codec no fold covers. *)

(** {2 Views}

    A module also has the views of its codec: [check b length ~off ~len] runs
    the checks of [fold] on the record at [off] in the [len] bytes there, in the
    buffer [b] of length [length] named by [Wire_runtime.Buf.v], and answers a
    ['b check]. A codec of constant size takes no window: [check b length off]
    checks its record at [off] in [b], as {!Wire.Codec.View.check} does; [ok]
    tests it and [get] makes it a ['b view], an [int] that only the accessors of
    [b] take. Each named integer field of the record is an accessor named after
    it, [name b v], returning its value as the codec decodes it, a
    [Wire_runtime.UInt16.t] for a 16-bit integer and an [int] for a bitfield (0
    when it is absent), each optional field that holds no record a presence
    [name_present b v], and each field that holds a record an accessor
    [name b len v] returning a check of that record's module, which passes iff
    the record is present and its fixed bytes lie in [b]. Each codec a field
    holds has one module, named after it ([CapturedIPv4] is [Captured_ipv4]),
    with the same accessors. A field at a constant offset inside the bytes the
    check guarantees is one load; any other is placed again from the bytes that
    place it, with a bound check, and a presence or a tag is tested again, so an
    accessor reads no byte outside [b] even when bytes are written after the
    check. A named field of bytes is an accessor too, answering a
    [Bytesrw.Bytes.Slice.t] of [b] for a byte slice and a copy for a byte array,
    as the codec decodes them. An integer or a bitfield at a constant offset
    inside the bytes the check guarantees also has [set_<name> b v x], which
    writes [x] there in place, one store or a load and a store of a bitfield's
    word; it refuses a bitfield too wide for its field with [Invalid_argument]
    and checks none of the record's constraints, as {!Wire.Codec.set} does. An
    accessor named as a value the module defines ([size], [check], [decode],
    ...) takes a trailing ['_'], as a keyword does: [size] is [size_] and
    [size_] [size__]. *)

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
(** [fstar ?fields ~codec c] is the F* proof that the [fold] of
    [to_ml (v ~folds:[ fold ?outputs:fields "fold" ] c)] answers what the parser
    EverParse writes for the 3D projection of [c] answers on the same bytes:
    each F* file of the proof, by name. [<codec>.Scan.fst] is that [fold], from
    the walk that writes the OCaml, its operations and loads written as calls
    into the primitives [WireGen.OCaml]; [<codec>.Flat.fst] reads the record
    field by field and proves that reading equal to the parser;
    [<codec>.Proof.fst] proves [fold_is_3d]: on a window inside the buffer,
    [fold buf off len ~invalid ~valid] is [valid] applied to the outputs of the
    value the parser reads from the window's bytes, or [invalid ()] where it
    reads none. [<codec>.Step.fst] is [fold_step] and [fold_step_within] of the
    same module with a fold [fold "fold_step"] whose visitor is [s.ml]
    ({!to_ml}), values held as [int64], calling the module [<codec>.Visitor],
    which [<codec>.Visitor.fsti] declares and leaves abstract;
    [<codec>.StepProof.fst] proves [fold_step_is_3d], the same statement for
    [fold_step buf off len init], with [S.invalid init] and [S.valid init] given
    each output as its [int64]. [<codec>.OxScan], [<codec>.OxProof],
    [<codec>.OxStep] and [<codec>.OxStepProof] are the same of that module for
    [~target:`OxCaml], each [int64_u] written as its 64-bit pattern.
    [<codec>.Frames] and [<codec>.Index] (and [<codec>.OxFrames],
    [<codec>.OxIndex]) are the walks of a frame of the loops of
    [fold_step_frames] and [fold_step_index], each a function whose parameters
    are typed with what the loop ensures of them, and their [Proof] modules
    prove [frame_is_3d]: such a walk answers what the parser answers on the
    frame's bytes.

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
  (** How a walk holds integer values: as [int], as [int64] in the OCaml step
      functions, or as [int64_u] on OxCaml. Positions and sizes are [int]. *)

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
    | Walked  (** Through a walk of their own that knows no byte. *)

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
    ?target:target ->
    ?folds:fold list ->
    ?visitors:(string * string) list ->
    'r Wire.Codec.t ->
    decision list
  (** [decisions ?target ?folds ?visitors c] is the decisions
      [to_ml ?visitors (v ~target ~folds c)] takes, in the order it writes them,
      each case of an optional field included, with [target] [`OCaml] and
      [folds] one fold [fold] of every output by default. A [window] is the
      variable holding the window's length: the frame's, or a region's size.
      Raises [Invalid_argument] as {!v} does. *)

  val outputs : t -> output list
  (** [outputs t] is the outputs of [t], in declaration order. *)

  val select : t -> string list option -> output list
  (** [select t names] is the outputs of [t] that [names] names, as {!fold}'s
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
end
