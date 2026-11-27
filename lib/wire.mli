(** Binary formats described once.

    A {!Codec} describes the bytes of a record once, as an OCaml value, and
    everything that reads and writes those bytes derives from it. {e Values}:
    {!Codec.decode} and {!Codec.encode} copy a whole record between bytes and
    OCaml. {e Views}: {!Codec.View.check} runs every check of the codec once,
    and a view then reads and writes single fields in place. {e Folds}:
    {!Codec.View.fold} checks the records of a buffer one after the other and
    hands the fields it names to a function. All of this is the {e interpreted}
    form, [wire] alone. The {e generated} form is the module [Wire_ocaml] or
    [Wire_rust] writes from the same codec, with the same concepts under the
    same names, each field an accessor named after it. {!Everparse} projects the
    same codec to a verified C validator.

    Bad bytes are a value: a check, a decoder or a read answers a refused check
    or an {!Error.t} for any input and never raises. [Invalid_argument] is a
    misuse by the program.

    The model is shown on one running example in {{!page-api}the API guide}. *)

(** {1 Expressions}

    Expressions describe sizes, constraints, and dependencies between fields.
    They are part of the wire description itself: no evaluation happens at
    interface construction time.

    They are used whenever the layout depends on previously decoded data: array
    lengths, byte-slice sizes, field constraints, and similar dependent
    structure. *)

type 'a expr = 'a Types.expr
type bitfield = U8 | U16 | U16be | U32 | U32be

(** Which end of a packed base word the first declared bitfield occupies.

    - {!constructor-Msb_first} (default): the first declared field lands at the
      most significant bit of the base word, matching how RFC, CCSDS, and IETF
      specs draw their bit diagrams. Copy-pasting a spec into field declarations
      just works.

    - {!constructor-Lsb_first}: the first declared field lands at bit 0 of the
      base word, matching MSVC's C bit-field packing. Useful when mirroring a C
      struct.

    Bit order is independent of byte order: any combination of base word and bit
    order is a valid wire description, and the EverParse 3D projection reverses
    declaration order within a bit group when necessary so that every pairing
    emits a valid 3D schema with identical byte layout. *)
type bit_order = Types.bit_order = Msb_first | Lsb_first

(** Byte order for multi-byte integers. *)
type endian = Types.endian = Little | Big

type 'a typ = 'a Types.typ

type consumption = Types.consumption
(** How a bytes-backed decoder treats input after one value. [`Prefix] accepts a
    leading value and leaves any remaining bytes uninterpreted; [`All] requires
    that value to consume the rest of the supplied input. *)

type param
(** Untyped formal parameter declaration. Create via {!val:Param.input} or
    {!val:Param.output}. *)

module Param : sig
  (** Formal parameters for codecs.

      A parameter lets a codec depend on a value that is not in the buffer. An
      {e input} parameter is supplied by the caller before decoding -- use it in
      constraints or size expressions. An {e output} parameter is written by an
      action during decoding -- read it back afterwards to extract a computed
      result.

      Both kinds carry a wire type (uint8 ... uint64, bool, enum) so the same
      definition can be projected to EverParse 3D.

      {[
      open Wire

      type bounded = { len : UInt16.t; data : Bytesrw.Bytes.Slice.t }

      let max_len = Param.input "max_len" uint16be
      let out_len = Param.output "out_len" uint16be
      let f_length = Field.v "Length" uint16be

      let f_data =
        Field.v "Data"
          ~action:
            (Action.on_success [ Action.assign out_len (Field.ref f_length) ])
          (byte_slice ~size:(Field.ref f_length))

      let codec =
        let open Codec in
        v "Bounded"
          ~where:Expr.(Field.ref f_length <= Param.expr max_len)
          (fun len data -> { len; data })
          [ (f_length $ fun r -> r.len); (f_data $ fun r -> r.data) ]
      ]}

      Do not share an {!env} across concurrent decodes. *)

  type input = Param.input
  (** Phantom kind for input parameters. *)

  type output = Param.output
  (** Phantom kind for output parameters. *)

  type ('a, 'k) t = ('a, 'k) Param.t
  (** Typed handle for one formal parameter. *)

  val input : string -> 'a typ -> ('a, input) t
  (** [input name typ] declares an input parameter. *)

  val output : string -> 'a typ -> ('a, output) t
  (** [output name typ] declares an output parameter. *)

  val decl : ('a, 'k) t -> param
  (** Project to an untyped formal declaration (for 3D rendering). *)

  val name : ('a, 'k) t -> string
  (** Parameter name. *)

  type env = Param.env
  (** Parameter environment. Create with {!Codec.env}, bind inputs with {!bind},
      read outputs with {!get}. An environment belongs to that codec and cannot
      be used with another one. *)

  val bind : ('a, input) t -> 'a -> env -> env
  (** [bind p v env] returns an environment with input [p] set to [v].

      Raises [Invalid_argument] naming [p] if [v] cannot fit the platform's
      native integer representation used by parameter environments. *)

  val bind_by_name : string -> int -> env -> env
  (** [bind_by_name name v env] binds the input parameter called [name] to the
      integer value [v] without its typed handle (for tooling that has the codec
      but not the {!t} handles). A no-op if [name] is not referenced. *)

  val get : env -> ('a, 'k) t -> 'a
  (** [get env p] reads param [p]. For outputs, call after decode. *)

  val expr : ('a, 'k) t -> int expr
  (** [expr p] returns the expression referencing this param. *)

  val pp : Format.formatter -> ('a, 'k) t -> unit
  (** Pretty-print the parameter's name. *)
end

module Action : sig
  (** Small imperative language for side-effects during validation.

      Actions are attached to struct fields and run after each field is
      successfully parsed. They can assign to mutable output parameters,
      conditionally abort validation, or introduce local variables. *)

  type t
  (** An action block. *)

  type stmt
  (** A statement inside an action block. *)

  val on_success : stmt list -> t
  (** Action run after successful validation of the annotated field. *)

  val on_act : stmt list -> t
  (** Action block for the 3D [:act] form. *)

  val assign : ('a, Param.output) Param.t -> int expr -> stmt
  (** [assign out e] assigns expression [e] to the mutable output parameter
      [out]. Only output parameters (created with {!val:Param.output}) can be
      assigned to -- the type system prevents assigning to input parameters. *)

  val return_bool : bool expr -> stmt
  (** Boolean return: [true] continues, [false] fails validation. *)

  val abort : stmt
  (** Unconditional validation failure. *)

  val if_ : bool expr -> stmt list -> stmt list option -> stmt
  (** Conditional execution. *)

  val var : string -> int expr -> stmt
  (** Local variable binding inside an action block. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print an action block. *)
end

val int : int -> int expr
(** Constant integer expression. *)

val sizeof : 'a typ -> int expr
(** Size of a fixed-size wire description. *)

val sizeof_this : int expr
(** Size in bytes of the enclosing description's fixed prefix: the bytes before
    its first field whose extent is not statically known, or its whole size when
    every field is fixed.

    One constant for the description, the same wherever it is read, and not the
    number of bytes consumed up to the reading field. A field after a
    variable-size one adds nothing to it. This is 3D's [sizeof(this)], which
    EverParse resolves from the type's own size and requires to fold to a
    compile-time constant.

    It is used in dependent sizes and constraints, most often as
    [Param.expr total - sizeof_this] for a trailing payload; see
    {!val-rest_bytes}. *)

val field_pos : int expr
(** Zero-based index of the current field in the enclosing sequential
    description.

    EverParse 3D has no form for it, so {!Codec.v} refuses it in a constraint,
    size or presence condition. *)

module Expr : sig
  (** Arithmetic, bitwise, and comparison operators on expressions.

      Open this module locally to build constraint and size expressions:
      [Expr.(Field.ref f + int 1)]. *)

  val int64 : int64 -> int64 expr
  (** Constant 64-bit integer expression. *)

  val ( + ) : int expr -> int expr -> int expr
  (** Addition. *)

  val ( - ) : int expr -> int expr -> int expr
  (** Subtraction. *)

  val ( * ) : int expr -> int expr -> int expr
  (** Multiplication. *)

  val ( / ) : int expr -> int expr -> int expr
  (** Division. *)

  val ( mod ) : int expr -> int expr -> int expr
  (** Modulo. *)

  val ( land ) : int expr -> int expr -> int expr
  (** Bitwise AND. *)

  val land64 : int64 expr -> int64 expr -> int64 expr
  (** Bitwise AND on full-width 64-bit operands, for masking a 64-bit field such
      as a sign-magnitude offset before a comparison. The plain machine-word AND
      truncates the field, so it cannot be used here. Projects to 3D as [&]. *)

  val lsr64 : int64 expr -> int expr -> int64 expr
  (** Logical shift right on a full-width 64-bit operand, for isolating high
      bits (a float64 exponent) that the plain machine-word shift would lose on
      a narrow-int platform. The shift amount must be a constant. Projects to 3D
      as [>>]. *)

  val ( lor ) : int expr -> int expr -> int expr
  (** Bitwise OR. *)

  val ( lxor ) : int expr -> int expr -> int expr
  (** Bitwise XOR. *)

  val lnot : int expr -> int expr
  (** Bitwise NOT. *)

  val ( lsl ) : int expr -> int expr -> int expr
  (** Logical shift left. *)

  val ( lsr ) : int expr -> int expr -> int expr
  (** Logical shift right. *)

  val ( = ) : 'a expr -> 'a expr -> bool expr
  (** Equality. *)

  val ( <> ) : 'a expr -> 'a expr -> bool expr
  (** Inequality. *)

  val ( < ) : 'a expr -> 'a expr -> bool expr
  (** Less than, on native-integer or 64-bit expressions. *)

  val ( <= ) : 'a expr -> 'a expr -> bool expr
  (** Less than or equal, on native-integer or 64-bit expressions. *)

  val ( > ) : 'a expr -> 'a expr -> bool expr
  (** Greater than, on native-integer or 64-bit expressions. *)

  val ( >= ) : 'a expr -> 'a expr -> bool expr
  (** Greater than or equal, on native-integer or 64-bit expressions. *)

  val ( && ) : bool expr -> bool expr -> bool expr
  (** Boolean conjunction. *)

  val ( || ) : bool expr -> bool expr -> bool expr
  (** Boolean disjunction. *)

  val not : bool expr -> bool expr
  (** Boolean negation. *)

  val if_then_else : bool expr -> int expr -> int expr -> int expr
  (** [if_then_else c t e] is [t] when [c] holds, else [e]. Use it for a value
      that depends on another field, e.g. a size where [0] means a maximum:
      [if_then_else Expr.(field = int 0) (int 65536) field]. *)

  val true_ : bool expr
  (** Constant [true]. *)

  val false_ : bool expr
  (** Constant [false]. *)

  val bool : bool -> bool expr
  (** Constant boolean expression. *)

  val to_uint8 : int expr -> int expr
  (** Truncate to unsigned 8-bit range (mask [0xFF]). *)

  val to_uint16 : int expr -> int expr
  (** Truncate to unsigned 16-bit range (mask [0xFFFF]). *)

  val to_uint32 : int expr -> int expr
  (** Truncate to unsigned 32-bit range (mask [0xFFFFFFFF]). *)

  val to_uint64 : int expr -> int expr
  (** 3D codegen cast. At runtime this is the identity -- OCaml integers cannot
      represent the full unsigned 64-bit range. Use only for 3D output where
      EverParse needs an explicit [(UINT64)] cast annotation. *)
end

(** {1 Fields}

    A field is a named, typed piece of a wire layout -- the building block for
    everything else.

    Define each field once with {!Field.v}, then reuse it everywhere: bind it
    into a {!Codec} with [Codec.( $ )], reference it from dependent expressions
    with {!Field.ref}, or project it to EverParse 3D with the codec.

    {[
    open Wire

    let f_version = Field.v "Version" (bits ~width:4 U8)
    let f_length = Field.v "Length" uint16be
    let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_length))
    ]}

    Fields carry no buffer position -- that comes from the {!Codec} they are
    bound into. The same field can appear in multiple codecs. *)

module UInt64 = UInt64
(** Unsigned 64-bit integers, the carrier of {!uint64} and {!uint64be}.

    Ordered as an unsigned number, which [int64] is not: [Int64.compare] ranks
    0xFFFF_FFFF_FFFF_FFFF, the largest value here, below 1, on every target. The
    carrier holds the eight bytes exactly, so there is no range to enforce. *)

module UInt32 = UInt32
(** Unsigned 32-bit integers, the carrier of {!uint32} and {!uint32be}.

    Ordered as an unsigned number on every target: the representation is shared
    with {!SInt32}, and where the native [int] is narrower than the field it is
    [Int32], whose comparison ranks 0xFFFFFFFF, the largest value here, below 1.
*)

module SInt32 = SInt32
(** Signed 32-bit integers, the carrier of {!int32} and {!int32be}.

    Distinct from {!UInt32} on purpose: the two share a representation, and a
    value read with the wrong signedness is a silent misparse rather than a type
    error. *)

module SInt8 = SInt8
(** Signed 8-bit integers, the carrier of {!int8}.

    A [private int], so a value reads as the [int] it is; only building one
    names the range, and [SInt8.v 200] is refused there rather than at the
    encoder. *)

module SInt16 = SInt16
(** Signed 16-bit integers, the carrier of {!int16} and {!int16be}.

    A [private int] for the same reason {!SInt8} is: a value reads as the [int]
    it is, only building one names the range, and [SInt16.v 40000] is refused
    there rather than at the encoder. *)

module UInt8 = UInt8
(** Unsigned 8-bit integers, the carrier of {!uint8}.

    A [private int] for the same reason {!SInt8} is, over the byte's unsigned
    range: a value reads as the [int] it is, only building one names the range,
    and [UInt8.v 511] is refused there rather than at the encoder. *)

module UInt16 = UInt16
(** Unsigned 16-bit integers, the carrier of {!uint16} and {!uint16be}.

    A [private int] for the same reason {!SInt16} is, over the other two bytes:
    a value reads as the [int] it is, only building one names the range, and
    [UInt16.v 70000] is refused there rather than at the encoder. *)

module Buf = Buf
(** Buffers named by a type.

    [let (Buf.Any b) = Buf.v bytes in ...] gives [bytes] a type ['b Buf.t] that
    no other buffer has. {!Codec.View} checks records in such a buffer, and a
    view checked in one buffer reads and writes no other. *)

module Field : sig
  type 'a t
  (** A named field carrying values of type ['a]. *)

  type 'a anon
  (** An anonymous (padding) field. Cannot be referenced. *)

  val v :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) ->
    ?action:Action.t ->
    ?doc:string ->
    'a typ ->
    'a t
  (** [v name typ] creates a named field. [?doc] attaches a free-text note (e.g.
      an RFC section) that the documentation projection renders as a [/* ... */]
      comment on the field in the generated 3D. [?self_constraint] receives the
      field's own ref and returns a constraint over it (e.g.
      [self <= int 1500]). [?self_int64] is the same convenience for full-width
      64-bit constraints using {!Expr.int64} literals and {!int64} field
      references.

      EverParse refines scalar fields only. {!Codec.v} therefore rejects
      [?constraint_] and [?self_constraint] on a byte span, array, sub-codec or
      casetype field; put the predicate on a scalar field it reads instead.

      Use {!optional} / {!optional_or} / {!repeat} for optional and repeating
      payloads -- they only project to 3D as the top of a struct field, never as
      a nested type. *)

  val optional :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) ->
    ?action:Action.t ->
    present:bool expr ->
    'a typ ->
    'a option t
  (** [optional name ~present t] is a field present iff [present] evaluates to
      [true]. Absent fields decode as [None] and consume zero bytes.

      To make a whole field group conditional, define the group as a sub-codec
      and pass [codec group] as [t]. Expressions inside the group can refer to
      its earlier fields, so the group may start with a byte-length prefix
      followed by {!repeat}; fields after the optional group remain in the
      parent codec. This shape projects to 3D as a gate-selected sub-codec.

      {[
      type ext = { len : UInt8.t; items : UInt8.t list }

      let f_ext_len = Field.v "ExtLen" uint8

      let ext_codec =
        Codec.v "Ext"
          (fun len items -> { len; items })
          Codec.
            [
              (f_ext_len $ fun e -> e.len);
              ( Field.repeat "ExtItems" ~size:(Field.ref f_ext_len) uint8
              $ fun e -> e.items );
            ]

      let f_gate = Field.v "Gate" uint8

      let f_ext =
        Field.optional "Ext"
          ~present:Expr.(Field.ref f_gate <> int 0)
          (codec ext_codec)
      ]} *)

  val optional_or :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) ->
    ?action:Action.t ->
    present:bool expr ->
    default:'a ->
    'a typ ->
    'a t
  (** [optional_or name ~present ~default t] is a field that decodes to the
      inner value when [present], or [default] when absent. *)

  val repeat :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) ->
    ?action:Action.t ->
    size:int expr ->
    'a typ ->
    'a list t
  (** [repeat name ~size t] parses elements of type [t] until [size] bytes are
      consumed exactly. The bound is a byte budget, not an element count, which
      is what lets the elements be variable-size (a tagged union, a
      length-prefixed sub-codec). An element cannot cross the region boundary;
      encoding raises [Invalid_argument] when the supplied values span fewer or
      more bytes than [size]. It projects to 3D as [t name[:byte-size size]].
      Raises [Invalid_argument] on a literal [size] that is negative or more
      than a buffer can hold. There is no count-driven form: "a count field,
      then that many variable-size elements" is expressible neither here nor in
      3D, whose only list construct is the byte budget ([elem[n]] is rejected
      for any element wider than a byte), so that shape needs caller-side
      iteration over the element parser.

      There is no sizeless form either, for the same reason: 3D has no
      rest-of-region list, so a repeat that consumed whatever remained would
      have no projection. When the region size is not known where the codec is
      built, take it as a {!module-Param.val-input} and bind it per call. Baking
      in a literal makes the budget a constant the encoder will also hold the
      values to, which is a promise the values cannot keep. *)

  val repeat_seq :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?self_int64:(int64 expr -> bool expr) ->
    ?action:Action.t ->
    seq:('a, 'seq) Types.seq_map ->
    size:int expr ->
    'a typ ->
    'seq t
  (** Like {!repeat} but accumulates into a custom sequence via [seq], with the
      same exact byte-budget requirement. *)

  val union :
    string ->
    ?action:Action.t ->
    ?doc:string ->
    on:'k t ->
    casetype:string ->
    ('a, 'k) Types.case_def list ->
    'a t
  (** [union name ~on ~casetype cases] is a field holding one of [cases],
      selected by the value of the earlier field [on]. Unlike {!Wire.casetype},
      whose tag is the first bytes of the value itself, the tag here is a field
      of its own, so other fields may sit between it and the union, and several
      unions may switch on the same tag. The union occupies the bytes of the
      selected case only.

      [on] must be integer-valued (an integer, a bitfield, an enum or a
      {!val-variants}) and declared before the union; every {!Wire.case} gives
      its [~index] in [on]'s type. Decoding a tag no case claims fails with
      {!Error.constructor-Invalid_tag} unless a {!Wire.default} branch catches
      it. Encoding raises [Invalid_argument] when the value's case is not the
      one the tag field holds.

      Projects to a 3D [casetype] named [casetype], taking the tag as its
      parameter, and to a field [casetype(on) name]. *)

  val anon : 'a typ -> 'a anon
  (** [anon typ] creates an anonymous (padding) field. *)

  val ref : 'a t -> int expr
  (** [ref f] returns the expression referencing this field's underlying integer
      value. Works on any field whose wire type is or wraps an integer,
      including boolean fields created with {!bit}. *)

  val int : 'a t -> int expr
  (** [int f] returns this field as a native-integer expression. *)

  val int64 : 'a t -> int64 expr
  (** [int64 f] returns the expression referencing this field's full 64-bit
      value, without truncating to OCaml's native integer range. *)

  val name : 'a t -> string
  (** Field name. *)

  val typ : 'a t -> 'a typ
  (** Wire type. *)

  val pp : Format.formatter -> 'a t -> unit
  (** Pretty-print the field's name. *)

  val constraint_ : 'a t -> bool expr option
  (** Field constraint, if any. *)

  val action : 'a t -> Action.t option
  (** Field action attached via {!v}'s [?action], if any. *)

  val doc : 'a t -> string option
  (** Field note attached via {!v}'s [?doc], if any. *)
end

(** {1 Type Descriptions}

    The primitive constructors describe immediate wire values. The combinators
    build larger descriptions out of them.

    In ordinary use, one starts from primitive integer or byte descriptions and
    combines them with {!bits}, {!array}, {!byte_slice}, {!where}, {!enum}, and
    related combinators.

    Every fixed-width integer accepts exactly the range its decoder produces:
    [0] to [2^n - 1] for an unsigned width, [-2^(n-1)] to [2^(n-1) - 1] for a
    signed one, and carries that range in its own type, so a number the field
    cannot hold is refused where the value is built. A {!bits} slice is the one
    shape left in a plain [int], which holds far more than the slice does, so a
    four-bit slice given [0x1F] raises [Invalid_argument] on every entry point
    ({!Codec.encode} and {!Codec.View.set} alike) rather than putting an [0xF]
    on the wire that reads back as a perfectly legal [15], with nothing left to
    say the caller meant something else. *)

val uint8 : UInt8.t typ
(** Unsigned 8-bit integer. Holds [0] to [255]: [511] is not a [uint8], even
    though its low byte is a legal one, and would come back from the decoder as
    [255]. The range is the carrier's, so {!UInt8.v} refuses it where the value
    is built and every encode path takes only values the field holds. *)

val uint16 : UInt16.t typ
(** Unsigned 16-bit little-endian integer. Holds [0] to [65535]: [70000] is not
    a [uint16], even though its low two bytes are legal ones, and would come
    back from the decoder as [4464]. The range is the carrier's, so {!UInt16.v}
    refuses it where the value is built and every encode path takes only values
    the field holds. *)

val uint16be : UInt16.t typ
(** Unsigned 16-bit big-endian integer. Same range and same carrier as
    {!uint16}. *)

val uint32 : UInt32.t typ
(** Unsigned 32-bit little-endian integer. Encodes [0] to [2{^32} - 1], and
    carries the value as a {!UInt32.t} so a word with bit 31 set survives on a
    target whose native [int] is narrower than the field. Build one with
    {!UInt32.of_int32} from a bit pattern, or {!UInt32.of_int}. *)

val uint32be : UInt32.t typ
(** Unsigned 32-bit big-endian integer. Encodes [0] to [2{^32} - 1]. *)

val uint64 : UInt64.t typ
(** [uint64] is an unsigned 64-bit little-endian integer represented as an OCaml
    64-bit integer. The representation is exactly the eight bytes written, so
    every [int64] encodes and none is out of range. *)

val uint64be : UInt64.t typ
(** [uint64be] is an unsigned 64-bit big-endian integer represented as an OCaml
    64-bit integer. *)

val int8 : SInt8.t typ
(** Signed 8-bit two's-complement integer. Holds [-128] to [127]: [200] is not
    an [int8], even though its low byte is a legal one, and would come back from
    the decoder as [-56]. The range is the carrier's, so {!SInt8.v} refuses it
    where the value is built and every encode path takes only values the byte
    holds. *)

val int16 : SInt16.t typ
(** Signed 16-bit little-endian integer. Holds [-32768] to [32767]: [40000] is
    not an [int16], even though its low two bytes are legal ones, and would come
    back from the decoder as [-25536]. The range is the carrier's, so
    {!SInt16.v} refuses it where the value is built and every encode path takes
    only values the field holds. *)

val int16be : SInt16.t typ
(** Signed 16-bit big-endian integer. Same range and same carrier as {!int16}.
*)

val int32 : SInt32.t typ
(** Signed 32-bit little-endian integer. Encodes [-2{^31}] to [2{^31} - 1], and
    carries the value as a {!SInt32.t} so the whole range survives on a target
    whose native [int] is narrower than the field. *)

val int32be : SInt32.t typ
(** [int32be] is a signed 32-bit big-endian integer, returned as an OCaml
    integer. *)

val int64 : int64 typ
(** Signed 64-bit little-endian integer. Like {!uint64}, the representation is
    exactly the eight bytes written, so no value is out of range. *)

val int64be : int64 typ
(** Signed 64-bit big-endian integer. *)

val float32 : float typ
(** [float32] is an IEEE 754 binary32 little-endian, widened to OCaml [float].
    The 3D projection emits [UINT32] (no native float in 3D); float predicates
    compile to bit-pattern refinements over the unsigned width. *)

val float32be : float typ
(** [float32be] is an IEEE 754 binary32 big-endian. *)

val float64 : float typ
(** [float64] is an IEEE 754 binary64 little-endian. *)

val float64be : float typ
(** [float64be] is an IEEE 754 binary64 big-endian. *)

val is_finite : float Field.t -> bool expr
(** [is_finite f] is a constraint that holds iff [f]'s decoded IEEE 754 value is
    neither [NaN] nor [+/- infinity]. Compiles to a bit-mask check on the
    field's exponent (against [0x7F80_0000] for binary32 and
    [0x7FF0_0000_0000_0000] for binary64); the same expression is emitted into
    the 3D output, so wire's OCaml decoder and EverParse's verified C decoder
    reject the same set of inputs. *)

val is_nan : float Field.t -> bool expr
(** [is_nan f] holds iff [f] decodes to a NaN bit pattern (exponent all-ones AND
    mantissa non-zero). *)

val uint : ?endian:endian -> int -> Optint.Int63.t typ
(** [uint size] is an unsigned integer occupying [size] bytes (1-7) with the
    given byte order (default {!Big}). Decodes to an [Optint.Int63.t]: a 7-byte
    value needs 56 bits, which does not fit an int on a narrow-int target
    (js/wasm).

    Encoding a value that needs more than [size] bytes raises [Invalid_argument]
    rather than dropping its high bytes: the truncated result is itself a legal
    [size]-byte number, so nothing downstream could tell the two apart. *)

val uint_var : ?endian:endian -> int expr -> Optint.Int63.t typ
(** [uint_var size] is like {!uint}, with a dynamic size expression for
    parameter-driven widths. *)

val bits : ?bit_order:bit_order -> width:int -> bitfield -> int typ
(** [bits ~width base] declares a bitfield of [width] bits inside [base].

    [~bit_order] selects which end of the base word the first declared bitfield
    occupies. It defaults to {!Msb_first}, which makes the DSL match how
    protocol specifications draw their bit diagrams. Pass [~bit_order:Lsb_first]
    when mirroring MSVC-style C bit-fields.

    Encoding a value with bits above [width] raises [Invalid_argument] rather
    than masking it, for the same reason as {!uint}.

    Raises [Invalid_argument] when [width] does not fit [base], and when the
    value does not fit the native [int] with its sign bit free: on js_of_ocaml
    (32-bit [int]) a field holds at most 31 bits, on wasm_of_ocaml (31-bit) at
    most 30. Read a whole 32-bit word there with {!uint32}. *)

val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w typ -> 'a typ
(** [map ~decode ~encode t] views a wire value through conversion functions.

    [decode] must be total over the wire values [t] and its constraints admit.
    {!Codec.validate} and {!Codec.View.check} do not call it, so a [decode] that
    raises on such a value makes {!Codec.decode} raise on a buffer they accept.
    For a table indexed by the wire value use {!lookup}, whose bound they check.
*)

val bool : bool -> bool expr
(** Constant boolean expression. *)

val bit : 'a typ -> bool typ
(** [bit t] views an integer wire value as a boolean. Zero is [false], non-zero
    is [true].

    [t] may be any integer-valued description, whatever OCaml type carries the
    integer: a {!val-uint8}, a bitfield, a {!val-uint32}. A description with no
    integer reading raises [Invalid_argument] where it is written down. *)

val lookup : 'a list -> 'b typ -> 'a typ
(** [lookup table t] decodes an integer as a zero-based index into a finite
    table.

    [t] may be any integer-valued description, as for {!bit}.

    The decoded integer selects the corresponding element from the list. An
    out-of-range index is an {!Error.constructor-Invalid_tag} error. Encoding
    raises [Invalid_argument] if the value is not in the table. *)

val empty : unit typ
(** [empty] is a description carrying no bytes and producing [()]. *)

val all_bytes : string typ
(** All remaining bytes of the enclosing sequential description as a string.

    This is mainly useful as the final field of a struct or record-shaped
    layout. *)

val all_zeros : string typ
(** All remaining bytes of the enclosing sequential description, requiring each
    of them to be zero.

    This is mainly useful as the final field of a struct or record-shaped
    layout. Encoding a value with a non-zero byte raises [Invalid_argument]:
    decode reports it as {!Error.constructor-Non_zero_padding}, so writing it
    would produce bytes that fail to read back. *)

val zeroterm : string typ
(** [zeroterm] is a NUL-terminated string: the bytes up to (but excluding) the
    first [0x00], which is consumed. Encoding a string that itself contains a
    [0x00] raises [Invalid_argument].

    Projects to the 3D [field[:zeroterm]] form, which desugars to the EverParse
    prelude [cstring]/[parse_string] combinator. EverParse cannot extract a
    struct where that field has a sibling (project-everest/everparse#321), so
    {!Codec.v} accepts it only as the codec's sole field; use
    {!zeroterm_at_most} in a larger record. The manual's Arrays section does not
    cover either form; the grammar is in 3D's lexer and the semantics in
    [EverParse3d.Prelude.fsti]. *)

val zeroterm_at_most : size:int expr -> string typ
(** [zeroterm_at_most ~size] is a NUL-terminated string occupying a fixed
    [size]-byte region: the terminator must appear within [size] bytes and the
    field always consumes exactly [size] bytes (bytes after the terminator are
    zero padding).

    Projects to the 3D [field[:zeroterm-byte-size-at-most size]] form
    ([t_at_most] of [cstring]). Like {!zeroterm}, it is absent from the manual;
    see [EverParse3d.Prelude.fsti]. Raises [Invalid_argument] on a literal
    [size] that is negative or more than a buffer can hold. *)

val where : bool expr -> 'a typ -> 'a typ
(** Refine a description with a boolean constraint.

    This projects to 3D's unnamed field refinement, [T f { e }], evaluated as
    soon as the field is read. Despite the name it is not 3D's [where] clause,
    which is what {!Codec.v}'s [?where] projects to. {!Field.v}'s [?constraint_]
    is the same refinement written at the field instead of the type.

    Encoding a value the constraint rejects raises [Invalid_argument] rather
    than emitting bytes decode would refuse. *)

(** Builder for sequence accumulation (Jsont-style). *)
type ('elt, 'seq) seq_map = ('elt, 'seq) Types.seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

val seq_list : ('a, 'a list) seq_map
(** Default builder: accumulate into a list. *)

val array : len:int expr -> 'a typ -> 'a list typ
(** [array ~len t] parses exactly [len] elements of type [t], where [len] is an
    element count, not a byte size. It projects to 3D only when [t] has a fixed
    wire size: 3D arrays are byte-budgeted, so the count is lowered to
    [len * sizeof t] bytes. For variable-size elements use {!Field.repeat},
    which is bounded by a byte budget instead. Encoding a list whose cardinality
    differs from [len] raises [Invalid_argument] before writing. Raises
    [Invalid_argument] on a literal [len] that is negative, or whose elements
    take more bytes than a buffer can hold. *)

val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ
(** Like {!array} but accumulates into a custom sequence via a {!type-seq_map}.
    Encoding enforces the same exact cardinality, and a literal [len] raises as
    for {!array}. *)

val byte_array : size:int expr -> string typ
(** Fixed-size byte sequence copied as a string.

    Encoding raises [Invalid_argument] unless the string is exactly [size]
    bytes: the field is an exact byte span, so a shorter or longer value is a
    caller error rather than something to pad or clip. For a short value in a
    fixed-size region use {!zeroterm_at_most}, which is NUL-terminated and
    round-trips.

    When [size] contains the simple product of a field and a literal, and that
    field has a simple [field <= bound] constraint, {!Codec.v} rejects the codec
    if [bound * literal] can reach [2^32]. EverParse represents byte sizes as
    [u32] and cannot verify such a schema; tighten the field bound. More complex
    bounds are left to EverParse.

    A [size] of [f - k], with [f] an unsigned field of at most 32 bits and [k] a
    nonnegative constant, needs no constraint on [f]: decoding rejects [f < k],
    and the 3D projection states [f >= k] on [f], so EverParse proves the
    subtraction. The same holds for {!byte_slice}.

    Raises [Invalid_argument] on a literal [size] that is negative or more than
    a buffer can hold. *)

val byte_array_where :
  size:int expr -> per_byte:(int expr -> bool expr) -> string typ
(** [byte_array_where ~size ~per_byte] is a byte sequence of [size] bytes where
    each byte must satisfy [per_byte]. The argument to [per_byte] is an
    expression bound to the current byte's integer value.

    Decode answers {!Error.constructor-Constraint_failed} for the first byte
    that violates the constraint; encode raises [Invalid_argument]. Encoding
    otherwise follows {!byte_array}: the string must be exactly [size] bytes.
    The motivating shape is SSH name-list payloads (RFC 4251 sec 5), where every
    byte must be printable US-ASCII. Raises [Invalid_argument] on a literal
    [size] that is negative or more than a buffer can hold, and on a [per_byte]
    that reads a parameter or another field, or whose arithmetic 3d.exe cannot
    verify: it projects to a one-byte element struct of its own, whose
    {!val-sizeof_this} is [1]. *)

val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
(** Fixed-size byte sequence exposed as a zero-copy slice of the decoded bytes.
    Encoding raises [Invalid_argument] unless the slice is exactly [size] bytes,
    as for {!byte_array}. Raises [Invalid_argument] on a literal [size] that is
    negative or more than a buffer can hold. *)

val rest_bytes : (_, _) Param.t -> string typ
(** [rest_bytes total] is the trailing payload of a record whose total decoded
    length is bound to [total]. Equivalent to
    [byte_array ~size:Expr.(Param.expr total - sizeof_this)]. Use this as the
    final field of a struct when the caller passes the buffer length as a
    parameter. Projects to 3D as [UINT8[:byte-size (total - sizeof(this))]]. *)

val nested : size:int expr -> 'a typ -> 'a typ
(** [nested ~size t] parses one value of type [t] inside a length-prefixed
    region of [size] bytes.

    This is for layouts where a length expression denotes the size of a region,
    but that region is known to contain exactly one value, such as a single
    nested message. Decoding rejects an inner value that consumes fewer bytes;
    encoding raises [Invalid_argument] unless the value's encoded size is
    exactly [size]. It projects to 3D as
    [t f[:byte-size-single-element-array size]], a different construct from the
    byte-budgeted list {!array} and {!Field.repeat} project to. Over a single
    scalar it projects to a struct of that scalar which requires [size] to be
    the scalar's width, under {!Field.optional} too. Raises [Invalid_argument]
    on a literal [size] that is negative or more than a buffer can hold. *)

val nested_at_most : size:int expr -> 'a typ -> 'a typ
(** [nested_at_most ~size t] is like {!nested}, but treats [size] as an upper
    bound rather than an exact size.

    This is for length-prefixed regions where the one logical element may
    consume fewer bytes than the available space. Encoding zero-pads the unused
    region; a value larger than [size] raises [Invalid_argument]. It projects to
    3D as [t f[:byte-size-single-element-array-at-most size]]; over a single
    scalar, to a struct of that scalar and the rest of the region, which
    requires [size] to be at least the scalar's width. Raises [Invalid_argument]
    on a literal [size] that is negative or more than a buffer can hold. *)

val enum : string -> (string * int) list -> 'a typ -> 'a typ
(** [enum name cases base] validates that the decoded integer is one of the
    named values, on encode as well as on decode: encoding an unlisted value
    raises [Invalid_argument]. The result still decodes to whatever [base]
    decodes to -- use {!variants} instead if you want proper OCaml values.
    [enum] is mainly useful for 3D projection where the name and cases appear in
    the generated [.3d] file.

    [base] may be any integer-valued description, whatever OCaml type carries
    the integer the case values name. A description with no integer reading
    raises [Invalid_argument] where it is written down. *)

val enum_open : string -> (string * int) list -> 'a typ -> 'a typ
(** [enum_open name cases base] is like {!enum} but for an open value set: the
    named cases document the known values, but any value is accepted. Decode
    does not reject unlisted values, and the field projects as its base scalar
    with no membership refinement, so a protocol field that may carry unknown or
    future codes is not wrongly rejected. The known codes are still emitted as a
    3D enum declaration, so they remain documented in the generated [.3d]. *)

val variants : string -> (string * 'a) list -> 'b typ -> 'a typ
(** [variants name cases base] maps integer values to OCaml values via a named
    enumeration. Unlike {!enum}, this converts to proper OCaml values.

    [base] may be any integer-valued description, as for {!enum}. *)

type ('a, 'k) case_def = ('a, 'k) Types.case_def

val case :
  ?index:'k ->
  'w typ ->
  inject:('w -> 'a) ->
  project:('a -> 'w option) ->
  ('a, 'k) case_def
(** Tagged branch of a casetype. *)

val default :
  'w typ ->
  inject:('k -> 'w -> 'a) ->
  project:('a -> ('k * 'w) option) ->
  ('a, 'k) case_def
(** [default inner ~inject ~project] is the default branch of a casetype.

    The decoder uses it when no [~index]-tagged case matches, passing the
    matched tag to [inject] (so the decoded value can recover which unclaimed
    tag it caught) together with the decoded body. The encoder writes the tag
    [project] returns, so an arbitrary unclaimed tag round-trips, e.g. for DHCP
    options: [inject = (fun code body -> Other (code, body))] and
    [project = (function Other (c, d) -> Some (c, d) | _ -> None)]. *)

val casetype : string -> 'k typ -> ('a, 'k) case_def list -> 'a typ
(** [casetype name tag defs] is a tag-dispatched union. The discriminator typ
    ['k] can be an integer, a string, or any other typ with decidable equality;
    every case must supply an explicit [~index]. Projecting an integer-tagged
    casetype raises [Invalid_argument] naming the case index when it cannot fit
    the platform's native integer representation.

    An integer tag projects to a 3D [casetype], whose validator switches on the
    tag and validates the selected case body. 3D dispatches on integers only, so
    a tag of any other type projects to framing instead: the tag bytes, then a
    rest-of-buffer field holding the body. The generated C therefore accepts any
    tag with any body and leaves dispatch to its caller. The OCaml decoder does
    dispatch, and rejects a tag no case claims unless a {!default} branch
    catches it, so on an unknown tag the two sides disagree by design. Because
    the body takes whatever bytes remain, such a casetype has to be the trailing
    field of its struct. *)

val fixed_size : 'a typ -> int option
(** [fixed_size t] is [Some n] if every value of [t] takes [n] bytes, and [None]
    if its size varies. *)

(** {1 Parsing Errors} *)

module Error = Wire_runtime.Error
(** Why a decoder refuses its input. Every decoder of this library answers an
    {!Error.t} as a value: the cases distinguish structural failure on input,
    such as unexpected end of input or a constraint violation, from semantic
    failure such as an invalid enum or tag. *)

(** {1:codecs Codecs}

    A codec describes the bytes of a record once: {!Codec.v} binds
    {!module-Field} definitions to the fields of an OCaml record and resolves
    where each lies. Everything that reads and writes the bytes derives from it:
    the values {!Codec.decode} and {!Codec.encode} copy, the views of
    {!Codec.View} that read and write fields in place, the folds over many
    records, the modules [Wire_ocaml] and [Wire_rust] generate and the validator
    {!Everparse} projects. *)

module Codec : sig
  (** {1:codecs Codecs} *)

  type 'r t
  (** The type for codecs of records ['r]. *)

  type ('a, 'r) field
  (** The type for definitions of fields of type ['a] bound to records ['r]. A
      definition exists before any codec, since {!v} takes it, and one
      definition can be bound into several codecs. *)

  (** The type for definitions given in order to a function ending in ['k]: the
      constructor of {!v}, where ['k] is the record ['r], or the visitor of
      {!View.fold}, where ['k] is its accumulator. *)
  type ('f, 'k, 'r) fields =
    | [] : ('k, 'k, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'k, 'r) fields -> ('a -> 'f, 'k, 'r) fields

  val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
  (** [f $ proj] is the definition [f] bound to the records ['r], from which
      [proj] reads its value. *)

  val v :
    string ->
    ?where:bool expr ->
    ?doc:string ->
    'f ->
    ('f, 'r, 'r) fields ->
    'r t
  (** [v name ~where ~doc make fields] is the codec [name] of the records [make]
      builds from the values of [fields], given in their order. [doc] is a
      free-text note, such as an RFC citation, that {!Everparse.project} renders
      as a [/*++ ... --*/] comment on the codec's 3D typedef.

      [where] is a whole-record constraint. Mentioning only
      {!module-Param.val-input} parameters, it projects to 3D's [where] clause,
      which EverParse checks before reading any field. 3D's [where] sees
      parameters only, so an expression that also refers to fields projects as a
      refinement on the last field it mentions, the earliest point at which 3D
      can evaluate it.

      [v] resolves where each definition of [fields] lies in the record: its
      constant offset or how to compute it, its reader and its writer. The
      definitions are not changed: one definition in two codecs is resolved in
      each, and {!View.field} hands out the codec's resolution.

      Raises [Invalid_argument], naming the shape and the reason, on a shape no
      verified backend can express (the EverParse limits of
      [docs/projection.md]); on a repeated field or parameter name; on a
      constraint, action or [where] that refers to a field it cannot see; on a
      size expression that refers to a field it cannot see, a bitfield after a
      variable-size field, or fixed fields that add up to more than a buffer can
      hold; and on a path ([( / )]) in [fields]. *)

  (** {2:layers Layers}

      A layer is a field that holds a record of another codec: the IPv4 header
      of an Ethernet frame, present when its [ether_type] says so. A fold names
      a field under a layer by its path, [ipv4 / protocol]. *)

  (* Open question 6: [layer], [opt_layer], [( / )] and [( // )] are the
     proposal, settled by this typing and a cram test of a rejected path. *)

  type ('s, 'r) layer
  (** The type for fields of records ['r] that hold a record ['s], present or
      optional. *)

  val layer : ('s, 'r) field -> ('s, 'r) layer
  (** [layer f] is [f] as a layer, the field of a record: a {!Wire.codec} field,
      or a {!Wire.nested} or {!Wire.nested_at_most} of one. Raises
      [Invalid_argument] for any other field. *)

  val opt_layer : ('s option, 'r) field -> ('s, 'r) layer
  (** [opt_layer f] is {!layer} for a {!Field.optional} [f]: the layer is absent
      where [f] is. *)

  val ( / ) : ('s, 'r) layer -> ('a, 's) field -> ('a option, 'r) field
  (** [l / f] is the field [f] of the record [l] holds, as a field of ['r]: it
      is [None] where [l] is absent. A path names a nested field to
      {!View.fold}; {!v} refuses it. *)

  val ( // ) : ('s, 'r) layer -> ('t, 's) layer -> ('t, 'r) layer
  (** [l // l'] is the layer [l'] of the record [l] holds, absent where [l] or
      [l'] is, so [l // l' / f] is [None] where either is: one option, however
      deep the path. *)

  (** {1:props Properties} *)

  val name : 'r t -> string
  (** [name c] is the name of [c], the name of its 3D struct. *)

  val rename : string -> 'r t -> 'r t
  (** [rename n c] is [c] named [n]. The name is the 3D struct's only, so [c]'s
      bytes, its checks, its definitions and its views are unchanged. *)

  val doc : 'r t -> string option
  (** [doc c] is the note {!v}'s [doc] attaches to [c], if any. *)

  val env : 'r t -> Param.env
  (** [env c] is a fresh parameter environment for [c]. *)

  val min_size : 'r t -> int
  (** [min_size c] is the fewest bytes a record of [c] takes: its size when [c]
      has a constant size. *)

  val fixed_size : 'r t -> int option
  (** [fixed_size c] is [Some n] if every record of [c] takes [n] bytes, and
      [None] if [c] has a variable size. *)

  (** {1:values Values}

      A value is a whole record, copied between bytes and OCaml. Decoders and
      validators answer the error of bad bytes as a value. The encoders raise
      [Invalid_argument] on a record the decoder would refuse: they never write
      bytes their own decoder refuses. *)

  val decode :
    ?consume:consumption ->
    ?env:Param.env ->
    'r t ->
    bytes ->
    off:int ->
    ('r, Error.t) result
  (** [decode c buf ~off] is the record of [c] at [off] in [buf], or the error
      of its bytes. [consume] defaults to [`Prefix], accepting one record
      followed by more bytes; [`All] requires the record to end at
      [Bytes.length buf] and answers {!Error.constructor-Trailing_bytes}
      otherwise. [env] supplies the input parameters of [c], and receives its
      output parameters on success.

      Raises [Invalid_argument] if [off] is negative, if [env] was made for
      another codec, or if [c] has input parameters and [env] is omitted or
      leaves one unbound: an unbound parameter would size a field as empty. *)

  val validate :
    ?env:Param.env -> 'r t -> bytes -> off:int -> (unit, Error.t) result
  (** [validate c buf ~off] is [Ok ()] if [decode c buf ~off] is a record, and
      its error otherwise, with no record built. Field actions run, but their
      assignments are not written back to [env]. Raises [Invalid_argument] as
      {!decode} does. *)

  val encode : ?env:Param.env -> 'r t -> 'r -> bytes -> off:int -> unit
  (** [encode c r buf ~off] writes the record [r] at [off] in [buf]. A codec
      with input parameters takes the width of each parametric field from [env]
      (built with [Param.bind p n (env c)]), and the values of [r] must have
      those widths. Field actions do not run.

      Raises [Invalid_argument] if [buf] is too short, if [env] was made for
      another codec, is omitted or leaves an input parameter unbound, or if
      {!decode} would refuse [r]: a closed enum carrying an unlisted value, an
      {!val-all_zeros} field carrying a non-zero byte, a constraint that does
      not hold. The bytes from [off] then hold a partial record: use {!to_bytes}
      when none must escape. *)

  val to_bytes : ?env:Param.env -> 'r t -> 'r -> bytes
  (** [to_bytes c r] is [r] encoded in fresh bytes, then validated as
      {!validate} does, so field actions run and one that refuses the record
      raises [Invalid_argument]. Raises as {!encode} does; no partial record
      escapes. *)

  val to_string : ?env:Param.env -> 'r t -> 'r -> string
  (** [to_string c r] is [Bytes.to_string (to_bytes c r)], with no copy. *)

  val size : ?env:Param.env -> 'r t -> 'r -> int
  (** [size c r] is the number of bytes [encode c r] writes: [n] when
      [fixed_size c] is [Some n]. Raises [Invalid_argument] on a record
      {!encode} refuses. *)

  val size_at :
    ?env:Param.env -> 'r t -> bytes -> off:int -> (int, Error.t) result
  (** [size_at c buf ~off] is the size of the record of [c] at [off] in [buf],
      read from the sizes its bytes hold, or the error of bytes that hold none,
      such as a length field [buf] cuts. It does not check that the record fits
      in [buf]. Raises [Invalid_argument] as {!decode} does. *)

  val pp_value : 'r t -> 'r Fmt.t
  (** [pp_value c] formats a record of [c] field by field: an integer-valued
      field as [name = value], the others not at all. Raises [Invalid_argument]
      on a codec with input parameters, which it has no environment to bind. *)

  (** {1:views Views and folds} *)

  (** Views of checked records, and folds over many.

      A {e check} runs every check of a codec on one record of a buffer, once. A
      {e view} is a record that passed: its fields are then read and written in
      place, with no check of their own, through the codec's resolution of each
      definition ({!field}). A {e fold} checks each record of a buffer and hands
      the fields it names to a function.

      The buffer is named by a type, ['b] ({!Buf}), which a check and a view
      carry: a view is read and written only in the buffer it was checked in. A
      check and a view are immediate integers, so {!check}, {!is_ok} and {!get}
      allocate nothing. Why a record was refused is out of line:
      {!Codec.validate} answers its error.

      The module [Wire_ocaml] generates from a codec has the same values under
      the same names, each field an accessor named after it:
      [Space_packet.apid b v] is [read b v (field packet apid)]. *)
  module View : sig
    type 'r codec := 'r t
    type ('a, 'r) definition := ('a, 'r) field
    type ('s, 'r) codec_layer := ('s, 'r) layer
    type ('f, 'k, 'r) definitions := ('f, 'k, 'r) fields

    (** {1:checks Checks} *)

    type ('b, 'r) check [@@immediate]
    (** The type for checks of a record ['r] in the buffer named ['b]. *)

    (* Open question 3: [?len] stays unless the buffer's length read in
       [check] counts beyond noise. *)

    val check :
      ?env:Param.env ->
      ?len:int ->
      'r codec ->
      'b Buf.t ->
      off:int ->
      ('b, 'r) check
    (** [check c b ~off] is the check of the record of [c] at [off] in [b], in
        the [len] bytes there (defaults to the bytes from [off] to the end of
        [b]). It passes iff [Codec.validate c (b :> bytes) ~off] accepts that
        record: the same frame test, field and record constraints, enums and
        actions run. [env] supplies the input parameters of [c], as for
        {!Codec.decode}.

        [check] never raises on [b], [off] or [len] and allocates nothing: a
        negative [off] or a window [b] does not hold is refused. Raises
        [Invalid_argument] only on [env], as {!Codec.decode} does. *)

    val is_ok : ('b, 'r) check -> bool
    (** [is_ok k] is [true] iff the check [k] passed. *)

    (** {1:views Views} *)

    (* Open question 1: a view does not carry its buffer unless a view that
       does counts within noise on OCaml 5.4.1 and OxCaml. *)

    type ('b, 'r) t [@@immediate]
    (** The type for records ['r] that passed their check in the buffer named
        ['b]. *)

    val get : ('b, 'r) check -> ('b, 'r) t
    (** [get k] is the view of the record the check [k] passed.

        Raises [Invalid_argument] if [is_ok k] is [false]. A refused record is
        input, not misuse: test {!is_ok} first. *)

    (** {1:fields Fields} *)

    type ('a, 'r) field
    (** The type for definitions of fields of type ['a] resolved in a codec of
        records ['r]: plain data, read with no lookup. *)

    val field : 'r codec -> ('a, 'r) definition -> ('a, 'r) field
    (** [field c f] is the definition [f] as {!Codec.v} resolved it in [c]:
        where it lies in the records of [c], its reader and its writer. Make it
        once per field, not once per record.

        Raises [Invalid_argument] if [f] is not one of the definitions [c] was
        built from.

        {b Warning.} A field and a view are indexed by their record type, not by
        their codec. A field of another codec of the same record type is
        accepted with a view of [c]: the read stays in the buffer, since an
        interpreted read is a load bounds-checked against it, but it reads the
        place the field has in the other codec and answers a wrong value. *)

    val read : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) field -> 'a
    (** [read b v f] is the value of the field [f] in the record [v] of [b].

        [read] never raises and never reads outside [b]. A field that bytes
        written to [b] after the check move out of [b] reads as absent: [0] for
        an integer, an empty slice or string for bytes. *)

    val set : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) field -> 'a -> unit
    (** [set b v f x] writes [x] in place as the field [f] of the record [v] of
        [b]. It checks none of the codec's constraints and runs no action: check
        the record again after a batch of writes.

        Raises [Invalid_argument], leaving [b] unchanged, if [x] is a value [f]
        cannot hold, such as a bitfield value wider than its field or a string
        of another length than its byte array, and if [f] has no constant place
        in the record, as a field after a field of variable size. *)

    (** {1:layers Layers} *)

    type ('s, 'r) layer
    (** The type for layers holding records ['s] resolved in a codec of records
        ['r]. *)

    val layer : 'r codec -> ('s, 'r) codec_layer -> ('s, 'r) layer
    (** [layer c l] is {!field} for a layer: the layer [l] as {!Codec.v}
        resolved it in [c]. Raises [Invalid_argument] if [l] is not one of the
        definitions [c] was built from. *)

    val sub : 'b Buf.t -> ('b, 'r) t -> ('s, 'r) layer -> ('b, 's) check
    (** [sub b v l] is the check of the record the layer [l] holds in the record
        [v] of [b]. The check of [v] already made every check of that record, so
        [sub] makes none again: it passes iff the layer is present and its frame
        lies in [b].

        [sub] never raises: a layer that bytes written to [b] after the check
        move out of [b] is refused. *)

    (** {1:folds Folds} *)

    (* Open question 4: the visitor takes the fields positionally, in the
       order of the list, as the constructor of [Codec.v] takes them. *)

    val fold :
      ?env:Param.env ->
      ?stride:int ->
      ?offs:int array ->
      ?lens:int array ->
      ?invalid:('acc -> 'acc) ->
      'r codec ->
      ('f, 'acc, 'r) definitions ->
      bytes ->
      init:'acc ->
      f:('acc -> 'f) ->
      ('acc, Error.t * 'acc) result
    (** [fold c fields buf ~init ~f] checks each record of [c] in [buf], as
        {!check} does, and folds [f] over those that pass, from [init]:
        [f acc x1 ... xn], where [xi] is the value of the [i]-th field of
        [fields] in the record, read during its check. A path ([Codec.( / )]) in
        [fields] reads [None] where its layer is absent. The records lie:
        - back to back from offset [0] (default), each where the one before
          ends, up to the end of [buf];
        - in slots of [stride] bytes, the [i]-th at [i * stride], over the
          [Bytes.length buf / stride] whole slots of [buf];
        - in frames, the [i]-th in the [lens.(i)] bytes at [offs.(i)], as a
          capture file lists them.

        A record in a slot or a frame is checked in its window alone.

        A refused record stops the fold with [Error (e, acc)], where [e] is the
        error {!Codec.validate} answers and [acc] the fold of the records before
        it: back to back, a refused record's length cannot be trusted to find
        the next one. In slots or frames, [invalid acc] is the accumulator after
        a refused record and the fold goes on.

        [fold c fields] resolves [fields] once, when applied to them. Raises
        [Invalid_argument] if an element of [fields] is not one of the
        definitions [c] was built from, if [stride] is not positive, if [offs]
        and [lens] are not given together or differ in length, if [stride] is
        given with [offs], if [invalid] is given with records back to back, and
        on [env] as {!check} does. *)
  end
end

(** {1 Nested Codec Combinators}

    These combinators extend the type language with structured sub-codecs,
    optional fields, and repeated elements -- for protocols like CCSDS TM frames
    where the layout depends on mission configuration. *)

val codec : 'r Codec.t -> 'r typ
(** [codec c] embeds a sub-codec as a field type. The sub-codec's decode and
    encode functions are called at the appropriate offset. *)

(** Optional and repeated payloads are field decorations only -- see
    {!Field.optional} / {!Field.optional_or} / {!Field.repeat}. They are not
    exposed at the typ level because there is no 3D projection for them outside
    the top of a struct field (e.g. nested in [array] or [casetype] case
    bodies). *)

(** {1:backends Verified backends}

    {!Everparse} projects a codec to an EverParse 3D schema, from which
    EverParse writes a verified C validator: [Wire_3d] runs EverParse and
    [Wire_stubs] binds the validator to OCaml. On a refusal the C validator
    answers the {!Error.t} {!Codec.validate} answers, so every tier gives one
    error for the same bytes. *)

module Everparse : sig
  type t
  (** The type for 3D schemas. *)

  type mode = [ `Ffi | `Standalone ]
  (** The type for how a codec projects to 3D. [`Standalone] emits a [.3d] that
      EverParse compiles to a standalone verified C parser, which also reads as
      the protocol's specification. [`Ffi] adds the [WireCtx] extern and a
      setter callback per field, so the C validator is called from OCaml and
      hands every field to its visitor. *)

  val project : ?mode:mode -> 'r Codec.t -> t
  (** [project ~mode c] is the 3D schema of [c] ([mode] defaults to
      [`Standalone]). *)

  val name : t -> string
  (** [name s] is the name of the schema [s], the name of its codec. *)

  val filename : t -> string
  (** [filename s] is the [.3d] file [s] is written to. Raises
      [Invalid_argument] unless [name s] is a portable identifier: ASCII
      letters, digits and underscores, starting with a letter or an underscore,
      and no device name such as [NUL] or [COM1]. *)

  val write : ?mode:mode -> outdir:string -> ?name:string -> t list -> unit
  (** [write ~mode ~outdir ~name ss] writes the schemas [ss] to [outdir]. With
      [`Ffi], each schema is a [.3d] of its own and [name] is ignored. With
      [`Standalone] (the default), the schemas are merged into one [<name>.3d],
      a type shared by several codecs written once, so a protocol family reads
      as one specification; [name] is then required. A codec packed on its own
      and also reached through another codec's field is one such shared type:
      its one declaration keeps the entrypoint marker and the doc comment either
      copy carried.

      Raises [Invalid_argument], before writing any file, if an output name is
      not a portable identifier ({!filename}) or if two schemas declare
      different types under one name. *)
end

(** {1 ASN.1 DER}

    ASN.1 types describe DER encodings (X.690) in the same description as binary
    codecs: a SEQUENCE binds {!Field.t} components as {!Codec.v} does,
    {!Asn1.codec} makes any ASN.1 type a {!Codec.t}, a DER value nests in a
    binary codec through {!val-codec} and a binary codec nests in DER through
    {!Asn1.containing}. A codec projects to EverParse 3D where 3D expresses it
    and to everparse-asn1 where it does not; see docs/asn1.md.

    [Asn1] is experimental: its API will certainly break in 2.1.0. *)

module Asn1 : sig
  type 'a t
  (** The type for ASN.1 types with OCaml values ['a]. *)

  type cls = [ `Universal | `Application | `Private ]
  (** The type for tag classes other than context-specific. *)

  (** {1:oid Object identifiers} *)

  (** Object identifiers, with arcs below [2{^32}]. *)
  module Oid : sig
    type t
    (** The type for object identifiers. *)

    val base : int -> int -> t
    (** [base a b] is the object identifier [a.b]. Raises [Invalid_argument]
        unless [a] is [0], [1] or [2], and [b] is below [40] when [a] is not
        [2]. *)

    val ( <| ) : t -> int -> t
    (** [o <| n] is [o] with the arc [n] appended. Raises [Invalid_argument] if
        [n] is negative or not below [2{^32}]. *)

    val ( <|| ) : t -> int list -> t
    (** [o <|| ns] is [o] with the arcs [ns] appended, as {!( <| )}. *)

    val equal : t -> t -> bool
    (** [equal o o'] is [true] iff [o] and [o'] have the same arcs. *)

    val compare : t -> t -> int
    (** [compare] orders object identifiers arc by arc. *)

    val of_string : string -> t option
    (** [of_string s] is the object identifier of the dotted string [s]. *)

    val to_string : t -> string
    (** [to_string o] is the dotted string of [o]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] prints an object identifier as {!to_string} does. *)
  end

  (** Relative object identifiers, with arcs below [2{^32}]. *)
  module Relative_oid : sig
    type t
    (** The type for relative object identifiers. *)

    val v : int list -> t
    (** [v ns] is the relative object identifier of the arcs [ns]. Raises
        [Invalid_argument] if [ns] is empty, or an arc is negative or not below
        [2{^32}]. *)

    val equal : t -> t -> bool
    (** [equal r r'] is [true] iff [r] and [r'] have the same arcs. *)

    val compare : t -> t -> int
    (** [compare] orders relative object identifiers arc by arc. *)

    val of_string : string -> t option
    (** [of_string s] is the relative object identifier of the dotted string
        [s]. *)

    val to_string : t -> string
    (** [to_string r] is the dotted string of [r]. *)

    val pp : Format.formatter -> t -> unit
    (** [pp] prints a relative object identifier as {!to_string} does. *)
  end

  (** {1:prim Primitives} *)

  val bool : bool t
  (** [bool] is BOOLEAN: [0x00] or [0xFF]. *)

  val integer : string t
  (** [integer] is INTEGER as its minimal two's complement content octets. *)

  val bounded_integer : min:int64 -> max:int64 -> int64 t
  (** [bounded_integer ~min ~max] is [INTEGER (min..max)]. Raises
      [Invalid_argument] if [min > max]. *)

  val enumerated : (string * int) list -> int t
  (** [enumerated ns] is [ENUMERATED] with the named numbers [ns]. Raises
      [Invalid_argument] on an empty list, a repeated name or a repeated number.
  *)

  val null : unit t
  (** [null] is NULL. *)

  val oid : Oid.t t
  (** [oid] is OBJECT IDENTIFIER. *)

  val relative_oid : Relative_oid.t t
  (** [relative_oid] is RELATIVE-OID. *)

  val bit_string : (int * string) t
  (** [bit_string] is BIT STRING as its count of unused bits, [0] to [7], and
      its octets, whose unused bits are zero. *)

  val octet_string : string t
  (** [octet_string] is OCTET STRING. *)

  val utf8_string : string t
  (** [utf8_string] is UTF8String: valid UTF-8. *)

  val printable_string : string t
  (** [printable_string] is PrintableString: its X.680 alphabet. *)

  val ia5_string : string t
  (** [ia5_string] is IA5String: 7-bit characters. *)

  val numeric_string : string t
  (** [numeric_string] is NumericString: digits and space. *)

  val visible_string : string t
  (** [visible_string] is VisibleString: printable 7-bit characters. *)

  val teletex_string : string t
  (** [teletex_string] is TeletexString, content unchecked. *)

  val videotex_string : string t
  (** [videotex_string] is VideotexString, content unchecked. *)

  val graphic_string : string t
  (** [graphic_string] is GraphicString, content unchecked. *)

  val general_string : string t
  (** [general_string] is GeneralString, content unchecked. *)

  val object_descriptor : string t
  (** [object_descriptor] is ObjectDescriptor, content unchecked. *)

  val bmp_string : string t
  (** [bmp_string] is BMPString as its UTF-16BE content octets. *)

  val universal_string : string t
  (** [universal_string] is UniversalString as its UTF-32BE content octets. *)

  val utc_time : string t
  (** [utc_time] is UTCTime as its characters, [YYMMDDhhmmssZ]. *)

  val generalized_time : string t
  (** [generalized_time] is GeneralizedTime as its characters,
      [YYYYMMDDhhmmss[.f]Z]. *)

  val date : string t
  (** [date] is DATE as its characters, [YYYYMMDD], the year from 1582. *)

  val time_of_day : string t
  (** [time_of_day] is TIME-OF-DAY as its characters, [hhmmss]. *)

  val date_time : string t
  (** [date_time] is DATE-TIME as its characters, [YYYYMMDDhhmmss], the year
      from 1582. *)

  val oid_iri : string t
  (** [oid_iri] is OID-IRI as its UTF-8 characters, [/] then labels separated by
      [/]. *)

  val relative_oid_iri : string t
  (** [relative_oid_iri] is RELATIVE-OID-IRI as its UTF-8 characters, labels
      separated by [/]. *)

  val any : string t
  (** [any] is ANY: one whole DER element, tag and length included. *)

  (** {1:constraints Constraints} *)

  val size : min:int -> max:int -> string t -> string t
  (** [size ~min ~max t] is [t (SIZE (min..max))], counted in characters. Raises
      [Invalid_argument] unless [t] is an OCTET STRING or a character string,
      and [0 <= min <= max < 65536]. *)

  (** {1:tags Tags} *)

  val implicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [implicit ?cls n t] is [[cls n] IMPLICIT t]. [cls] defaults to
      context-specific. Raises [Invalid_argument] if [t] is a CHOICE or ANY,
      which have no tag of their own to replace. *)

  val explicit : ?cls:cls -> int -> 'a t -> 'a t
  (** [explicit ?cls n t] is [[cls n] EXPLICIT t]. [cls] defaults to
      context-specific. *)

  (** {1:constructed Constructed types} *)

  val required : string -> 'a t -> 'a Field.t
  (** [required name t] is a component [name] of type [t]. *)

  val optional : string -> 'a t -> 'a option Field.t
  (** [optional name t] is a component [name] of type [t] marked OPTIONAL. *)

  val default : string -> 'a -> 'a t -> 'a Field.t
  (** [default name v t] is a component [name] of type [t] marked [DEFAULT v]:
      absent when it is [v]. Raises [Invalid_argument] unless [t] is a primitive
      other than OBJECT IDENTIFIER and RELATIVE-OID, possibly under one EXPLICIT
      tag, and on a time or IRI [v] that is not a DER value of [t]. *)

  val sequence : string -> 'f -> ('f, 'r) Codec.fields -> 'r t
  (** [sequence name make fields] is the SEQUENCE [name] of [fields]. Raises
      [Invalid_argument] on a field not made by this module, a repeated name,
      and a run of OPTIONAL or DEFAULT components whose tags overlap each other
      or the next component's. *)

  val set : string -> 'f -> ('f, 'r) Codec.fields -> 'r t
  (** [set name make fields] is the SET [name] of [fields]. Raises
      [Invalid_argument] as {!sequence} does, on two components with one tag,
      and on more than 64 components. *)

  val sequence_of : ?non_empty:bool -> 'a t -> 'a list t
  (** [sequence_of ?non_empty t] is [SEQUENCE OF t],
      [SEQUENCE SIZE (1..MAX) OF t] if [non_empty] (defaults to [false]). *)

  val set_of : ?non_empty:bool -> 'a t -> 'a list t
  (** [set_of ?non_empty t] is [SET OF t], its elements in DER order. *)

  type 'a alternative
  (** The type for alternatives of a CHOICE of values ['a]. *)

  val alternative :
    string ->
    'w t ->
    inject:('w -> 'a) ->
    project:('a -> 'w option) ->
    'a alternative
  (** [alternative name t ~inject ~project] is the alternative [name] of type
      [t]. *)

  val choice : 'a alternative list -> 'a t
  (** [choice alts] is [CHOICE] over [alts]. Raises [Invalid_argument] on an
      empty list, two alternatives with one tag, and an ANY alternative. *)

  (** {1:defined_by Components defined by a key} *)

  type ('a, 'k) case
  (** The type for the cases of a component of values ['a] defined by a key of
      values ['k]. *)

  val case :
    'k ->
    'w t ->
    inject:('w -> 'a) ->
    project:('a -> 'w option) ->
    ('a, 'k) case
  (** [case k t ~inject ~project] is the component of type [t] under the key
      [k]. *)

  val absent : 'k -> inject:'a -> project:('a -> bool) -> ('a, 'k) case
  (** [absent k ~inject ~project] is no component under the key [k]: it decodes
      as [inject], and encodes a value [project] accepts. *)

  val fallback :
    ?optional:bool ->
    'w t ->
    inject:('k -> 'w option -> 'a) ->
    project:('a -> ('k * 'w option) option) ->
    ('a, 'k) case
  (** [fallback ?optional t ~inject ~project] is the component of type [t] under
      every other key, OPTIONAL if [optional] (defaults to [false]). *)

  val defined_by : string -> on:'k Field.t -> ('a, 'k) case list -> 'a Field.t
  (** [defined_by name ~on cases] is the component [name] whose type the key
      [on] selects among [cases]; with no {!fallback}, another key is refused.
      Raises [Invalid_argument] unless [on] is an earlier mandatory component of
      the same SEQUENCE, of a primitive type with no constraint, and on a
      repeated key or a second {!fallback}. *)

  (** {1:bytes Bytes} *)

  val keep_bytes : 'a t -> ('a * string) t
  (** [keep_bytes t] is [t] with the DER bytes of each decoded value; {!codec}'s
      encoder writes the value and ignores the bytes. *)

  val containing : 'a typ -> 'a t
  (** [containing t] is an OCTET STRING whose content is exactly one value of
      [t]. *)

  (** {1:values Values and codecs} *)

  val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w t -> 'a t
  (** [map ~decode ~encode t] is [t] read as ['a]. *)

  val codec : 'a t -> 'a Codec.t
  (** [codec t] is the codec of the DER encoding of [t]. A view of it reads each
      component through {!Codec.View.checked_field}, and sets none in place. *)

  val fold :
    ('c, 'r) Codec.View.codec ->
    ('a list, 'r) Codec.field ->
    'b Buf.t ->
    ('b, 'c) Codec.View.t ->
    init:'acc ->
    f:('acc -> 'a -> 'acc) ->
    'acc
  (** [fold c l b v ~init ~f] folds [f] over the elements of the SEQUENCE OF or
      SET OF component [l] of the view [v], in encoding order. An element of
      bytes written after the check that no longer holds ends the fold. Raises
      [Invalid_argument] unless [l] is a mandatory component of the SEQUENCE or
      SET [c] names whose type, under its tags, is a SEQUENCE OF or SET OF. *)

  (** {1:projection Projection to everparse-asn1} *)

  type schema
  (** The type for ASN.1 modules, the input of everparse-asn1. *)

  val project : string -> 'r Codec.t -> schema
  (** [project m c] is the ASN.1 module [m] of [c]: [c]'s own types if [c] is an
      ASN.1 codec, else the ASN.1 types [c]'s fields hold. Raises
      [Invalid_argument] if [c] holds none. *)

  val contained : 'r Codec.t -> Everparse.t list
  (** [contained c] is the 3D schema of each binary codec under a {!containing}
      of [c]. *)

  val name : schema -> string
  (** [name s] is the module name of [s]. *)

  val filename : schema -> string
  (** [filename s] is the [.asn] file name of [s]. *)

  val roots : schema -> string list
  (** [roots s] is the types of [s] given to everparse-asn1's [--root]. *)

  val pp : Format.formatter -> schema -> unit
  (** [pp] prints a schema as its [.asn] text. *)

  val write : outdir:string -> schema list -> unit
  (** [write ~outdir ss] writes the [.asn] file of each schema in [outdir].
      Raises [Invalid_argument] if two different schemas have one name. *)
end

(** {1:ascii Bit diagrams}

    RFC-style bit layout diagrams, 32 bits wide, following the conventions of
    {{:https://www.rfc-editor.org/rfc/rfc791}RFC 791}. *)

module Ascii : sig
  val of_codec : 'r Codec.t -> string
  (** [of_codec c] is the bit diagram of [c]. *)

  val pp_codec : Format.formatter -> 'r Codec.t -> unit
  (** [pp_codec ppf c] formats the bit diagram of [c] on [ppf]. *)
end

(**/**)

(** {1 Private}

    The internals [wire.ocaml], [wire.rust], [wire.3d], [wire.stubs] and
    [wire.diff] are built on. Not part of the API: it changes with them. *)

module Private : sig
  module UInt32 = UInt32
  module Unchecked = Unchecked
  module UInt63 = UInt63
  module Input_end = Input_end
  module Types = Types

  (** Top-level expression evaluation, in a context with no field bindings. *)
  module Eval : sig
    type ctx
    (** Evaluation context. *)

    val empty : ctx
    (** Context with no bindings. *)

    val expr : ctx -> 'a Types.expr -> 'a
    (** Evaluate a top-level expression. *)

    val int_of : 'a Types.typ -> 'a -> int option
    (** Convert a typed value to [int], [None] when it does not fit. *)

    val int_of_exn : 'a Types.typ -> 'a -> int
    (** {!val-int_of} without the option; stops at the parse error
        {!Types.is_parse_error} finds. *)
  end

  module Expr_compiler = Expr_compiler

  (** Packed bitfield base words. *)
  module Bitfield : sig
    val byte_size : Types.bitfield_base -> int
    (** Bytes in one packed base word. *)

    val total_bits : Types.bitfield_base -> int
    (** Bits in one packed base word. *)

    val equal : Types.bitfield_base -> Types.bitfield_base -> bool
    (** Same base type and endianness. *)

    val read_word : Types.bitfield_base -> bytes -> int -> int
    (** Read a base word at a byte offset. *)

    val u32_field_le : bytes -> int -> int -> int -> int
    (** [u32_field_le buf off shift mask] is a field of the little-endian 32-bit
        word at [off]. *)

    val u32_field_be : bytes -> int -> int -> int -> int
    (** Big-endian {!u32_field_le}. *)

    val write_word : Types.bitfield_base -> bytes -> int -> int -> unit
    (** Write a base word at a byte offset. *)

    val native_bit_order : Types.bitfield_base -> Types.bit_order
    (** The bit order matching EverParse 3D's native packing for a base. *)

    val shift :
      bit_order:Types.bit_order ->
      total:int ->
      bits_used:int ->
      width:int ->
      int
    (** Where a field of [width] bits sits in a base word of [total] bits, after
        [bits_used] bits of earlier fields. *)

    val extract :
      bit_order:Types.bit_order ->
      total:int ->
      bits_used:int ->
      width:int ->
      int ->
      int
    (** Extract a field's bits from a base word. *)
  end

  module Uint_var = Uint_var
  module Shape = Shape
  module Names = Names
  module Layout = Record_layout

  (** A named or an anonymous field, whatever its type. *)
  type packed = Named : 'a Field.t -> packed | Anon : 'a Field.anon -> packed

  val decl_of_packed : packed -> Types.field
  (** The underlying field declaration of a packed field. *)

  (** The 3D plumbing of [wire.3d] and [wire.stubs]. *)
  module Everparse : sig
    type t = Everparse.t
    type mode = Everparse.mode
    type struct_ = Types.struct_

    type decl
    (** 3D top-level declaration. *)

    type decl_case
    (** Case of a top-level 3D casetype declaration. *)

    type module_
    (** A 3D module. *)

    val module_ : t -> module_
    (** [module_ s] is the 3D module of [s]. *)

    val wire_size : t -> int option
    (** [wire_size s] is the size of the records of [s], [None] if it varies. *)

    val source : t -> struct_ option
    (** [source s] is the struct [s] was projected from, [None] for a schema
        wrapping a raw module. *)

    val validate_output_name : string -> unit
    (** [validate_output_name name] accepts portable ASCII identifiers beginning
        with a letter or underscore and containing only letters, digits, and
        underscores, except filesystem device names such as [NUL] and [COM1].
        Raises [Invalid_argument] for any other name. *)

    val uses_wire_ctx : t -> bool
    (** [uses_wire_ctx s] is [true] when the schema declares the [WireCtx]
        extern typedef. The generated C header then [#include]s
        [<Name>_ExternalTypedefs.h], so that file must be present at compile
        time. Schemas built via {!project} always satisfy this. *)

    type visitor_field = {
      name : string;
      idx : int;
      c_type : string;
      setter : string;
      val_c_type : string;
    }
    (** Visitor info: the data needed to materialise a typed struct and
        [<Name>Set*] dispatchers for a schema. See {!Everparse.visitor_field}.
    *)

    val visitor_member : string -> string
    (** [visitor_member name] is the visitor struct member a field named [name]
        takes: [name] escaped against C's keywords and the macros of the headers
        the visitor includes ([int_], [NULL_], and [int__] for [int_]), so
        distinct names stay distinct members. *)

    val visitor_fields : t -> visitor_field list
    (** [visitor_fields s] enumerates the named fields of the source struct in
        declaration order. Returns [[]] for schemas without a source struct. *)

    val visitor_setters : t -> (string * string) list
    (** [visitor_setters s] lists the unique [WireSet*] setters referenced by
        [s] as [(setter, val_c_type)] pairs. *)

    val entrypoint_struct : t -> struct_ option
    (** Entrypoint typedef struct in the schema's module, if any. *)

    val extern_fn_names : t -> string list
    (** Names of every extern function declared in the schema's module. *)

    type field_action_form = No_action | On_act | On_success

    val equal_field_action_form : field_action_form -> field_action_form -> bool
    (** Structural equality on field action forms. *)

    val field_action_forms :
      struct_ -> (string option * bool * field_action_form) list
    (** [field_action_forms st] enumerates fields as [(name, is_bitfield, form)]
        tuples. Used by tests to assert the codegen invariant that bitfields
        carry {!constructor:On_act}, scalars {!constructor:On_success},
        anonymous fields {!constructor:No_action}. *)

    module Raw : sig
      (** Escape hatch for manual 3D authoring.

          These constructors exist for EverParse features that currently have no
          codec-level equivalent, plus the struct-level projection knobs. Most
          users should stay on the {!module-Field}/{!module-Codec} path and use
          {!project}. *)

      type nonrec struct_ = struct_
      type field = packed
      type nonrec decl = decl
      type nonrec decl_case = decl_case
      type nonrec module_ = module_
      type nonrec t = t

      val typedef :
        ?entrypoint:bool ->
        ?export:bool ->
        ?output:bool ->
        ?extern_:bool ->
        ?doc:string ->
        struct_ ->
        decl
      (** Top-level typedef declaration. *)

      val define : string -> int -> decl
      (** Top-level integer definition. *)

      val extern_fn : string -> param list -> 'a typ -> decl
      (** External function declaration used by 3D actions. *)

      val extern_probe : ?init:bool -> string -> decl
      (** External probe declaration. *)

      val enum_decl : string -> (string * int) list -> 'a typ -> decl
      (** Top-level enum declaration. *)

      val decl_case : int -> 'a typ -> decl_case
      (** One tagged case in a top-level casetype declaration. *)

      val decl_default : 'a typ -> decl_case
      (** Default case in a top-level casetype declaration. *)

      val casetype_decl :
        string -> param list -> 'a typ -> decl_case list -> decl
      (** Top-level casetype declaration. *)

      val module_ : ?doc:string -> decl list -> module_
      (** Builds a 3D module from declarations. *)

      val to_3d : ?enum_as_type:bool -> module_ -> string
      (** Renders a 3D module to text. With [~enum_as_type:true] an enum field
          renders as its named 3D enum type rather than the base type plus a
          membership refinement. *)

      val to_3d_file : ?enum_as_type:bool -> string -> module_ -> unit
      (** Writes a rendered 3D module to a file. See {!to_3d} for
          [enum_as_type]. *)

      val struct_of_codec : 'r Codec.t -> struct_
      (** Lower a codec to its 3D struct, the intermediate the stubs and
          benchmark codegen operate on. *)

      val project_struct : ?mode:mode -> struct_ -> t
      (** [project_struct ?mode s] is {!project} on an already-lowered struct.
      *)

      val field :
        string -> ?constraint_:bool expr -> ?action:Action.t -> 'a typ -> field
      (** Named field. Returns a packed {!Field.t}. *)

      val anon_field : 'a typ -> field
      (** Anonymous field in a 3D struct. *)

      val field_ref : field -> int expr
      (** [field_ref f] returns the expression referencing field [f] by name.
          The field must have been created with {!val:field} (not
          {!anon_field}). *)

      val struct_ : string -> field list -> struct_
      (** Non-parameterised 3D struct. *)

      val struct_name : struct_ -> string
      (** Name of a struct declaration. *)

      val field_names : struct_ -> string list
      (** Named field names in declaration order. *)

      (** The OCaml representation kind of a field (for FFI stub generation). *)
      type ocaml_kind = Int | Int64 | Float32 | Float64 | Bool | String | Unit

      val field_kinds : struct_ -> (string * ocaml_kind) list
      (** Named field names with their OCaml type kind. *)

      type int_slot = { width : int; endian : endian }
      (** Byte width and order of a whole-byte integer field on the wire. *)

      type field_seed = { field : string; slot : int_slot; values : int64 list }
      (** A field whose own declaration singles out particular wire values. *)

      val int_slots : struct_ -> (string * int_slot) list
      (** [int_slots s] is the byte slot of every named whole-byte integer field
          of [s], in declaration order. Bitfields (their base word is shared),
          floats, byte spans and composites have no slot of their own and are
          left out. {!field_seeds} is this same set narrowed to the fields whose
          declaration singles out particular values. *)

      val field_seeds : struct_ -> field_seed list
      (** [field_seeds s] is, for each named whole-byte integer field of [s]
          whose any refinement in the record compares against a value, the byte
          slot it occupies and those values: the constant an equality or
          inequality names, the values either side of an ordering boundary, and
          the members of a closed enumeration. A refinement is credited to the
          field it compares rather than to the field carrying it, and a
          struct-level {!val-where} is read where the projection puts it, so a
          field's values are the ones the generated validator tests it against.
          A casetype field seeds too: its tag is parsed at the start of the
          field's own bytes, so the slot is the tag's and the values are the
          case indices. Bitfields are omitted because their base word is shared,
          so a byte offset alone cannot seed one.

          The list over-approximates: it names candidates worth trying, not
          values the description is guaranteed to admit, so a consumer must run
          each through the codec to learn the verdict. *)

      val struct_project : struct_ -> name:string -> keep:packed list -> struct_
      (** [struct_project s ~name ~keep] returns a copy of [s] renamed to [name]
          keeping only the fields in [keep]. All others become anonymous. *)

      val struct_params : struct_ -> param list
      (** Formal parameters of a struct (empty for non-parameterised structs).
      *)

      val input_param_names : struct_ -> string list
      (** Names of the input (non-mutable) parameters, in declaration order,
          which is the order the generated validator and its wrapper take them.
      *)

      val input_param_c_types : struct_ -> string list
      (** C type of each input (non-mutable) parameter, in declaration order,
          e.g. [["uint8_t"; "uint16_t"]]. Matches the widths the generated
          validator wrapper takes, so a caller can build the wrapper call
          without reading the generated header. *)

      val param_c_types :
        struct_ -> [ `Input of string | `Output of string ] list
      (** [param_c_types s] is, for each parameter of [s] in declaration order,
          its C type: an input's is the type the generated validator wrapper
          takes ({!input_param_c_types}), an output's the type of the cell the
          wrapper takes a pointer to. *)

      val struct_typ : struct_ -> unit typ
      (** View a 3D struct as a wire description. *)

      val param : string -> 'a typ -> param
      (** Immutable parameter declaration. *)

      val mutable_param : string -> 'a typ -> param
      (** Mutable parameter declaration. *)

      val param_struct :
        string -> param list -> ?where:bool expr -> field list -> struct_
      (** Parameterised 3D struct. *)

      val apply : 'a typ -> int expr list -> 'a typ
      (** Apply a parameterised description to integer arguments. *)

      val type_ref : string -> 'a typ
      (** Unqualified type reference. *)

      val qualified_ref : string -> string -> 'a typ
      (** Qualified type reference. *)

      val pp_typ : Format.formatter -> 'a typ -> unit
      (** Pretty-printer for 3D-facing type syntax. *)

      val pp_module : Format.formatter -> module_ -> unit
      (** Pretty-printer for 3D modules. *)

      val struct_size : struct_ -> int option
      (** Fixed wire size of a struct, if known statically. *)

      val of_module : name:string -> module_:module_ -> wire_size:int -> t
      (** Wraps an existing 3D module as a schema. *)
    end
  end

  (** Struct validators: a [Types.struct_] validated without a constructor. *)

  type validator
  (** A struct validator without a constructor. *)

  val validator_of_struct : Types.struct_ -> validator
  (** [validator_of_struct s] compiles [s] into a validator. *)

  val validate_struct : validator -> bytes -> int -> (unit, Error.t) result
  (** [validate_struct v buf off] runs the validator [v] on the struct at [off],
      answering its error as a value. *)

  val struct_size_of : validator -> bytes -> int -> int
  (** Byte size of the struct starting at [off]. *)

  val struct_min_size : validator -> int
  (** Minimum byte size accepted. *)

  val wire_size_info_of_validator :
    validator -> [ `Fixed of int | `Variable of bytes -> int -> int ]
  (** Wire-size info for the validator. *)

  (** Bit diagrams of a struct. *)
  module Ascii : sig
    val of_struct : Types.struct_ -> string
    (** Render a struct as an RFC-style bit diagram. *)

    val pp_struct : Format.formatter -> Types.struct_ -> unit
    (** Pretty-print a struct as an RFC-style bit diagram. *)
  end

  (** Support for code generated from a codec by [Wire_ocaml]. *)
  module Gen : sig
    val to_struct : 'r Codec.t -> Types.struct_
    (** [to_struct t] is the description [t] is built from. *)

    val digest : 'r Codec.t -> string
    (** [digest t] names the shape of [t]: its fields, their types and
        constraints, every expression in full, and the version of what generated
        code assumes of it. *)

    val check : 'r Codec.t -> string -> unit
    (** [check t d] raises [Invalid_argument] unless [digest t = d]. *)

    type 'b field_k = {
      k : 'a. 'a Types.typ -> (bytes -> int -> ('a, Error.t) result) -> 'b;
    }
    (** A use of a field's reader, whatever the field's type. *)

    val with_field_reader : 'r Codec.t -> string -> 'b field_k -> 'b option
    (** [with_field_reader t name k] is [k.k ty r], where [r] is the reader the
        decoder reads the field [name] of [t] with and [ty] its type, or [None]
        when [t] has no field [name]. *)

    (** {2 Maps} *)

    type ('w, 'a) map = {
      dec : at:int -> 'w -> 'a;
      get : at:int -> field:string -> 'w -> ('a, Error.t) result;
      enc : 'a -> 'w;
    }
    (** A map's decoder and encoder over its raw wire value. [dec] is for a
        value the record's check accepted; [get] answers the error of any other,
        at [at] and under the name it is given. *)

    val map_int : 'a Types.typ -> (int, 'a) map
    (** [map_int t] is the map [t] over an integer up to 16 bits wide or a
        bitfield, read as an [int]. Raises [Invalid_argument] for any other [t].
    *)

    val map_int32 : 'a Types.typ -> (int32, 'a) map
    (** [map_int32 t] is the map [t] over a 32-bit integer. *)

    val map_id : 'a Types.typ -> int option
    (** [map_id t] identifies the map [t] among every map built, [None] when [t]
        is not a map. *)

    val check_map : 'r Codec.t -> string -> 'a Types.typ -> unit
    (** [check_map c name t] raises [Invalid_argument] unless the field [name]
        of [c] is the very value [t]. *)

    val scan : 'r Codec.t -> bytes -> int -> int -> int64 array option
    (** [scan c buf off len] is the outputs of the record in the [len] bytes at
        [off], which the codec accepts on their own, or [None]: each named
        scalar field, and for each optional field its presence as [1L] or [0L],
        in declaration order, an absent field [0L]. *)

    val frames :
      name:string ->
      bytes ->
      int ->
      int ->
      stride:int ->
      init:'a ->
      ('a -> int -> int -> 'a) ->
      'a
    (** [frames ~name buf off len ~stride ~init f] folds [f] over the offset and
        the length of each of the [len / stride] frames of [stride] bytes at
        [off]. Raises [Invalid_argument], naming [name], on bad bounds. *)

    val index :
      name:string ->
      offs:int array ->
      lens:int array ->
      init:'a ->
      ('a -> int -> int -> 'a) ->
      'a
    (** [index ~name ~offs ~lens ~init f] folds [f] over the frames of an index.
        Raises [Invalid_argument], naming [name], if [offs] and [lens] differ in
        length. *)
  end

  (** How a codec is compiled, for the fuzzer: the same codec with every fast
      path off, a codec not yet compiled, and the fast paths compiling selected.
  *)
  module Testing : sig
    val general : 'r Codec.t -> 'r Codec.t
    (** [general t] is [t] with every fast path off. *)

    val fresh : 'r Codec.t -> 'r Codec.t
    (** [fresh t] is [t] not yet compiled. *)

    val paths : 'r Codec.t -> string list
    (** [paths t] names the fast paths compiling [t] selected. *)

    val staged_intervals : 'r Codec.t -> bytes -> int -> (bool * bool) option
    (** [staged_intervals t buf off] is the answer of the staged word tests of
        the check pass of [t] on the record at [off] in [buf], paired with the
        answer of the interval scan they stand for. [None] when the check pass
        of [t] has no staged word tests or [buf] does not hold the record's
        frame at [off]. *)

    val all_paths : string list
    (** Every name {!paths} can return. *)
  end

  val param_name : param -> string
  (** Name of a formal parameter. *)

  val param_is_mutable : param -> bool
  (** Returns true for output (mutable) parameters. *)

  val param_c_type : param -> string
  (** C type name of a parameter (e.g., ["uint16_t"]). *)

  val ml_type_of : 'a typ -> string
  (** OCaml type name for FFI stub generation. *)

  val c_type_of : 'a typ -> string
  (** C type name for FFI stub generation. *)
end
