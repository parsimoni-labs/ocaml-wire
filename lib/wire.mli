(** Binary wire format descriptions.

    A wire format is a sequence of typed {!Field}s -- integers, bitfields,
    enumerations, byte arrays -- laid out at fixed bit offsets in a buffer. A
    {!Codec} binds those fields to an OCaml record, giving you:

    - Zero-copy field access via staged getters and setters.
    - Full-record [decode] / [encode] between bytes and OCaml values.
    - Constraint checking and dependent-size fields.

    {[
    open Wire

    type header = { version : int; length : int }

    let f_version = Field.v "Version" (bits ~width:4 U8)
    let f_length = Field.v "Length" uint16be
    let bf_version = Codec.(f_version $ fun (h : header) -> h.version)
    let bf_length = Codec.(f_length $ fun (h : header) -> h.length)

    let codec : header Codec.t =
      Codec.v "Header"
        (fun version length -> { version; length })
        Codec.[ bf_version; bf_length ]

    let buf = Bytes.create (Codec.wire_size codec)
    let () = Codec.encode codec { version = 1; length = 42 } buf 0
    let get_version = Staged.unstage (Codec.get codec bf_version)
    let () = assert (get_version buf 0 = 1)

    let () =
      match Codec.decode codec buf 0 with
      | Ok h -> assert (h.version = 1 && h.length = 42)
      | Error _ -> assert false
    ]}

    The same description can be projected to an EverParse 3D schema via
    {!Everparse}, for verified C parser generation. *)

module Staged : sig
  type +'a t

  val stage : 'a -> 'a t
  (** [stage v] wraps [v] as a staged value. *)

  val unstage : 'a t -> 'a
  (** [unstage s] forces the staged value [s]. *)
end

(** {1 Expressions}

    Expressions describe sizes, constraints, and dependencies between fields.
    They are part of the wire description itself: no evaluation happens at
    interface construction time.

    They are used whenever the layout depends on previously decoded data: array
    lengths, byte-slice sizes, field constraints, and similar dependent
    structure. *)

type 'a expr
type bitfield = U8 | U16 | U16be | U32 | U32be

type bit_order = Types.bit_order =
  | Msb_first
  | Lsb_first
      (** Which end of a packed base word the first declared bitfield occupies.

          - [Msb_first] (default): the first declared field lands at the most
            significant bit of the base word, matching how RFC, CCSDS, and IETF
            specs draw their bit diagrams. Copy-pasting a spec into field
            declarations just works.

          - [Lsb_first]: the first declared field lands at bit 0 of the base
            word, matching MSVC's C bit-field packing. Useful when mirroring a C
            struct.

          Bit order is independent of byte order: any combination of base word
          and bit order is a valid wire description, and the EverParse 3D
          projection reverses declaration order within a bit group when
          necessary so that every pairing emits a valid 3D schema with identical
          byte layout. *)

type endian = Types.endian =
  | Little
  | Big  (** Byte order for multi-byte integers. *)

type 'a typ

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

      type bounded = { len : int; data : Bytesrw.Bytes.Slice.t }

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

  type env = Param.env
  (** Parameter environment. Create with {!Codec.env}, bind inputs with {!bind},
      read outputs with {!get}. *)

  val bind : ('a, input) t -> 'a -> env -> env
  (** [bind p v env] returns an environment with input [p] set to [v]. *)

  val get : env -> ('a, 'k) t -> 'a
  (** [get env p] reads param [p]. For outputs, call after decode. *)

  val expr : ('a, 'k) t -> int expr
  (** [expr p] returns the expression referencing this param. *)
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
end

val int : int -> int expr
(** Constant integer expression. *)

val int64 : int64 -> int64 expr
(** Constant 64-bit integer expression. *)

val sizeof : 'a typ -> int expr
(** Size of a fixed-size wire description. *)

val sizeof_this : int expr
(** Number of bytes already consumed in the enclosing sequential description.

    This is meaningful only while interpreting a larger description, typically a
    struct or record-shaped layout. It is used in dependent sizes and
    constraints for later fields. *)

val field_pos : int expr
(** Zero-based index of the current field in the enclosing sequential
    description.

    Like {!sizeof_this}, this is context-dependent and is mainly used inside
    dependent field constraints and projections. *)

module Expr : sig
  (** Arithmetic, bitwise, and comparison operators on expressions.

      Open this module locally to build constraint and size expressions:
      [Expr.(Field.ref f + int 1)]. *)

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

  val ( < ) : int expr -> int expr -> bool expr
  (** Less than. *)

  val ( <= ) : int expr -> int expr -> bool expr
  (** Less than or equal. *)

  val ( > ) : int expr -> int expr -> bool expr
  (** Greater than. *)

  val ( >= ) : int expr -> int expr -> bool expr
  (** Greater than or equal. *)

  val ( && ) : bool expr -> bool expr -> bool expr
  (** Boolean conjunction. *)

  val ( || ) : bool expr -> bool expr -> bool expr
  (** Boolean disjunction. *)

  val not : bool expr -> bool expr
  (** Boolean negation. *)

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
  (** 3D codegen cast. At runtime this is the identity -- OCaml [int] cannot
      represent the full unsigned 64-bit range. Use only for 3D output where
      EverParse needs an explicit [(UINT64)] cast annotation. *)
end

(** {1 Fields}

    A field is a named, typed piece of a wire layout -- the building block for
    everything else.

    Define each field once with {!Field.v}, then reuse it everywhere: bind it
    into a {!Codec} for zero-copy access and full-record round-trips, reference
    it from dependent expressions with {!Field.ref}, or project it to EverParse
    3D via {!Everparse.schema}.

    {[
    open Wire

    let f_version = Field.v "Version" (bits ~width:4 U8)
    let f_length = Field.v "Length" uint16be
    let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_length))
    ]}

    Fields carry no buffer position -- that comes from the {!Codec} they are
    bound into. The same field can appear in multiple codecs. *)

module Field : sig
  type 'a t
  (** A named field carrying values of type ['a]. *)

  type 'a anon
  (** An anonymous (padding) field. Cannot be referenced. *)

  type packed = Named : 'a t -> packed | Anon : 'a anon -> packed

  val v :
    string ->
    ?constraint_:bool expr ->
    ?self_constraint:(int expr -> bool expr) ->
    ?action:Action.t ->
    'a typ ->
    'a t
  (** [v name typ] creates a named field. [?self_constraint] receives the
      field's own ref and returns a constraint over it; useful for proving a
      later size-expression safe (e.g. [self >= int 7] when a later field uses
      [byte_slice ~size:(ref len - int 7)]). *)

  val anon : 'a typ -> 'a anon
  (** [anon typ] creates an anonymous (padding) field. *)

  val ref : 'a t -> int expr
  (** [ref f] returns the expression referencing this field's underlying integer
      value. Works on any field whose wire type is or wraps an integer,
      including [bool] fields created with {!bit}. *)
end

(** {1 Type Descriptions}

    The primitive constructors describe immediate wire values. The combinators
    build larger descriptions out of them.

    In ordinary use, one starts from primitive integer or byte descriptions and
    combines them with {!bits}, {!array}, {!byte_slice}, {!where}, {!enum}, and
    related combinators. *)

val uint8 : int typ
(** Unsigned 8-bit integer. *)

val uint16 : int typ
(** Unsigned 16-bit little-endian integer. *)

val uint16be : int typ
(** Unsigned 16-bit big-endian integer. *)

val uint32 : int typ
(** Unsigned 32-bit little-endian integer. *)

val uint32be : int typ
(** Unsigned 32-bit big-endian integer. *)

val uint63 : int typ
(** Unsigned 63-bit little-endian integer carried on 8 bytes. *)

val uint63be : int typ
(** Unsigned 63-bit big-endian integer carried on 8 bytes. *)

val uint64 : int64 typ
(** [uint64] is an unsigned 64-bit little-endian integer represented as [int64].
*)

val uint64be : int64 typ
(** [uint64be] is an unsigned 64-bit big-endian integer represented as [int64].
*)

val uint : ?endian:endian -> int expr -> int typ
(** [uint size] is an unsigned integer of [size] bytes (1-7) with the given byte
    order (default {!Big}). The size may be a dynamic expression for
    parameter-driven widths. *)

val bits : ?bit_order:bit_order -> width:int -> bitfield -> int typ
(** [bits ~width base] declares a bitfield of [width] bits inside [base].

    [~bit_order] selects which end of the base word the first declared bitfield
    occupies. It defaults to {!Msb_first}, which makes the DSL match how
    protocol specifications draw their bit diagrams. Pass [~bit_order:Lsb_first]
    when mirroring MSVC-style C bit-fields. *)

val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w typ -> 'a typ
(** [map ~decode ~encode t] views a wire value through conversion functions. *)

val bool : bool -> bool expr
(** Constant boolean expression. *)

val bit : int typ -> bool typ
(** [bit t] views an integer wire value as a boolean. Zero is [false], non-zero
    is [true]. *)

val lookup : 'a list -> int typ -> 'a typ
(** [lookup table t] decodes an integer as a zero-based index into a finite
    table.

    The decoded integer selects the corresponding element from the list. An
    out-of-range index produces an {!Invalid_tag} parse error (reported via
    [result] in {!of_reader} / {!of_string} / {!of_bytes}). Encoding raises
    [Invalid_argument] if the value is not in the table. *)

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
    layout. *)

val where : bool expr -> 'a typ -> 'a typ
(** Refine a description with a boolean constraint. *)

type ('elt, 'seq) seq_map = ('elt, 'seq) Types.seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map
      (** Builder for sequence accumulation (Jsont-style). *)

val seq_list : ('a, 'a list) seq_map
(** Default builder: accumulate into a list. *)

val array : len:int expr -> 'a typ -> 'a list typ
(** Repetition of a description, with length computed from an expression. *)

val array_seq : ('a, 'seq) seq_map -> len:int expr -> 'a typ -> 'seq typ
(** Repetition with custom builder. *)

val byte_array : size:int expr -> string typ
(** Fixed-size byte sequence copied as a string. *)

val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
(** Fixed-size byte sequence exposed as a zero-copy slice. *)

val nested : size:int expr -> 'a typ -> 'a typ
(** [nested ~size t] parses one value of type [t] inside a length-prefixed
    region of [size] bytes.

    This is for layouts where a length expression denotes the size of a region,
    but that region is known to contain exactly one value, such as a single
    nested message. *)

val nested_at_most : size:int expr -> 'a typ -> 'a typ
(** [nested_at_most ~size t] is like {!nested}, but treats [size] as an upper
    bound rather than an exact size.

    This is for length-prefixed regions where the one logical element may
    consume fewer bytes than the available space. *)

val enum : string -> (string * int) list -> int typ -> int typ
(** [enum name cases base] validates that the decoded integer is one of the
    named values. The result is still [int] -- use {!variants} instead if you
    want to decode to proper OCaml values. [enum] is mainly useful for 3D
    projection where the name and cases appear in the generated [.3d] file. *)

val variants : string -> (string * 'a) list -> int typ -> 'a typ
(** [variants name cases base] maps integer values to OCaml values via a named
    enumeration. Unlike {!enum}, this converts to proper OCaml values. *)

type 'a case_def

val case :
  ?index:int ->
  'w typ ->
  inject:('w -> 'a) ->
  project:('a -> 'w option) ->
  'a case_def
(** Tagged branch of a casetype. *)

val default :
  'w typ -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case_def
(** Default branch of a casetype. *)

val casetype :
  ?first:int -> ?step:int -> string -> int typ -> 'a case_def list -> 'a typ
(** Tag-dispatched choice between several descriptions. *)

val size : 'a typ -> int option
(** [size t] is the fixed wire size of a description, if known statically. *)

(** {1 Parsing Errors}

    Direct decoding reports failures as values of type {!parse_error}. The cases
    distinguish structural failure on input, such as unexpected end of input or
    a constraint violation, from semantic failure such as an invalid enum or
    tag. *)

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error
(** Raised by the [_exn] direct decoders ({!of_string_exn}, {!of_bytes_exn},
    {!of_reader_exn}) and by {!Codec.decode_exn} on parse failure. *)

exception Validation_error of parse_error
(** Raised by {!Codec.validate} on constraint or where-clause failure. *)

val pp_parse_error : Format.formatter -> parse_error -> unit
(** Pretty-printer for decode errors. *)

(** {1 Direct Decoding and Encoding}

    These functions interpret a ['a typ] directly and exchange ordinary OCaml
    values with bytes.

    Use them when you want a value now: one-shot decoding, streaming code built
    around {!Bytesrw}, tests, small tools, and formats that are naturally
    consumed as values.

    Use {!Codec} instead when the format is record-shaped and the main goal is
    repeated access to individual fields in an existing buffer, without
    allocating an OCaml record for each read. *)

val of_reader : 'a typ -> Bytesrw.Bytes.Reader.t -> ('a, parse_error) result
(** Decodes one value from the current reader position.

    If the description references parameters, bind them with {!Param.bind}
    before calling. Output parameters are updated during decoding; read them
    back with {!Param.get}.

    For the zero-copy codec path, prefer {!Codec.decode} which takes an explicit
    {!Param.env}.

    Decoding is prefix-based: success does not imply that the reader is
    exhausted afterwards. *)

val of_reader_exn : 'a typ -> Bytesrw.Bytes.Reader.t -> 'a
(** Like {!of_reader} but raises {!exception:Parse_error} on failure. *)

val of_string : 'a typ -> string -> ('a, parse_error) result
(** Decodes one value from the start of the string. Trailing bytes, if any, are
    left uninterpreted. *)

val of_string_exn : 'a typ -> string -> 'a
(** Like {!of_string} but raises {!exception:Parse_error} on failure. *)

val of_bytes : 'a typ -> bytes -> ('a, parse_error) result
(** Decodes one value from the start of the byte sequence. Trailing bytes, if
    any, are left uninterpreted. *)

val of_bytes_exn : 'a typ -> bytes -> 'a
(** Like {!of_bytes} but raises {!exception:Parse_error} on failure. *)

(** {1 Direct Encoding}

    Encoding follows the same description language as decoding. The functions in
    this section are the direct counterparts of {!of_reader}, {!of_string}, and
    {!of_bytes}: they work with whole OCaml values rather than field-level
    accessors.

    Unlike decoding, encoding is exception-based rather than result-based.
    Decoding fails on untrusted input (truncated data, constraint violations),
    so callers need structured errors. Encoding fails only on programmer errors
    (wrong description for the value, unsupported form), which are not
    data-dependent and should not be silently ignored. *)

val to_writer : 'a typ -> 'a -> Bytesrw.Bytes.Writer.t -> unit
(** Encodes one value to a {!Bytesrw.Bytes.Writer.t}.

    This function is exception-based. Unsupported description forms, such as
    unresolved type references, raise an exception rather than returning an
    error value. *)

val to_bytes : 'a typ -> 'a -> bytes
(** Encodes one value to freshly allocated bytes. *)

val to_string : 'a typ -> 'a -> string
(** Encodes one value to a freshly allocated string. *)

(** {1 Codecs}

    A codec is the primary way to work with a wire format. It binds {!Field}s to
    an OCaml record type and provides:

    - {b Zero-copy field access}: {!Codec.get} and {!Codec.set} read and write
      individual fields directly in a buffer -- no intermediate record
      allocated.
    - {b Full-record round-trip}: {!Codec.decode} and {!Codec.encode} convert
      between bytes and OCaml values.
    - {b Bitfield batch access}: {!Codec.load_word} reads a packed word once,
      then {!Codec.extract} retrieves each sub-field with pure shift+mask.

    All three modes derive from the same definition, so the layout is always
    consistent. {!Everparse.schema} projects the same codec to a verified C
    parser -- no separate description to maintain. *)

module Codec : sig
  type 'r t
  (** Sealed codec for record values of type ['r]. *)

  type ('a, 'r) field
  (** A field bound to a record projection. *)

  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
  (** [f $ proj] binds a {!Field.t} to a record projection. *)

  val v : string -> ?where:bool expr -> 'f -> ('f, 'r) fields -> 'r t
  (** [v name constructor fields] seals a codec.

      {[
      let codec =
        Codec.v "Packet"
          (fun version length -> { version; length })
          Codec.
            [ (f_version $ fun p -> p.version); (f_length $ fun p -> p.length) ]
      ]} *)

  val wire_size : 'r t -> int
  (** Fixed wire size of the codec.

      Raises [Invalid_argument] if the codec is variable-size. *)

  val min_wire_size : 'r t -> int
  (** Minimum wire size of the codec. *)

  val wire_size_at : 'r t -> bytes -> int -> int
  (** Computes the actual wire size from a buffer at the given base offset. *)

  val is_fixed : 'r t -> bool
  (** [true] iff the codec has a statically known size. *)

  val env : 'r t -> Param.env
  (** [env c] creates a fresh parameter environment for codec [c]. *)

  val decode :
    ?env:Param.env -> 'r t -> bytes -> int -> ('r, parse_error) result
  (** [decode ?env c buf off] decodes one record value at the given base offset.
      If [?env] is supplied, input params are read from it and output params are
      written back to it on success. *)

  val decode_exn : ?env:Param.env -> 'r t -> bytes -> int -> 'r
  (** Like {!decode} but raises {!exception:Parse_error} on failure. *)

  val encode : 'r t -> 'r -> bytes -> int -> unit
  (** Encodes one record value into a buffer at the given base offset.

      Raises [Invalid_argument] if the destination buffer is too short. *)

  val validate : 'r t -> bytes -> int -> unit
  (** [validate c buf off] checks field [~constraint_] and [~where] clauses
      without constructing a record and without firing actions.

      Raises {!Validation_error} on failure. *)

  val get :
    ?env:Param.env -> 'r t -> ('a, 'r) field -> (bytes -> int -> 'a) Staged.t
  (** Staged field reader. If the field has an [~action], the action fires on
      every read. Pass [~env] to sync output parameters after each action.
      Fields without actions have zero overhead regardless of [~env].

      Does not check [~where] clauses or other fields' constraints -- call
      {!validate} first on untrusted input. *)

  val set : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a -> unit) Staged.t
  (** Staged field writer. Does not check constraints or fire actions -- call
      {!validate} after a batch of writes to verify constraints still hold. *)

  val field_ref : ('a, 'r) field -> int expr
  (** Field reference expression from a bound field handle. *)

  (** {2 Slice navigation}

      Zero-copy access to the offset/length of a [byte_slice] field. The naive
      nesting [Slice.first (Codec.get c f buf base)] forces [Codec.get] to
      allocate a fresh {!Bytesrw.Bytes.Slice.t} -- 4 words -- only for
      [Slice.first] to extract one int and discard the rest. {!slice_offset}
      skips the make and returns the int directly. *)

  val slice_offset :
    'r t -> (Bytesrw.Bytes.Slice.t, 'r) field -> (bytes -> int -> int) Staged.t
  (** [slice_offset c f] is a staged reader returning the absolute byte offset
      of slice field [f] within the buffer. Stage once with
      [Codec.slice_offset c f |> Staged.unstage], then call the resulting
      [buf -> base -> int] reader on the hot path -- 0 allocations versus
      [Slice.first (Codec.get c f buf base)]'s 4 words.

      Type-restricted to [Slice.t] fields, so passing a non-slice field is a
      compile-time error. *)

  val slice_length :
    'r t -> (Bytesrw.Bytes.Slice.t, 'r) field -> (bytes -> int -> int) Staged.t
  (** [slice_length c f] is a staged reader returning the byte length of slice
      field [f]. *)

  (** {2 Bitfield batch access}

      For multiple bitfield fields sharing the same base word, {!load_word}
      reads the word once and {!extract} retrieves individual fields with pure
      shift+mask -- no redundant memory loads. *)

  type bitfield
  (** A bitfield accessor -- shift and mask for one field in a packed word. *)

  val bitfield : 'r t -> (int, 'r) field -> bitfield
  (** [bitfield codec field] returns a bitfield accessor. *)

  val load_word : bitfield -> (bytes -> int -> int) Staged.t
  (** Staged word reader. Force once, reuse for every read. Fields in the same
      base word share the same underlying reader -- call once and use {!extract}
      on the result for each field. *)

  val extract : bitfield -> int -> int
  (** [extract bf word] extracts the field from a pre-loaded word. Pure
      shift+mask, no memory access. *)

  (** {2 Struct validator}

      For a [Types.struct_] (e.g. from EverParse 3D), build a validator directly
      without going through {!v}'s record-constructor machinery. The same
      int-array kernel that backs {!validate} on a [Codec.t]. *)

  type validator
  (** A struct validator without a constructor. *)

  val validator_of_struct : Types.struct_ -> validator
  (** [validator_of_struct s] compiles [s] into a validator. *)

  val validate_struct : validator -> bytes -> int -> unit
  (** Run the validator. Raises {!Validation_error} on failure. *)

  val struct_size_of : validator -> bytes -> int -> int
  (** Byte size of the struct starting at [off]. *)

  val struct_min_size : validator -> int
  (** Minimum byte size accepted. *)

  val wire_size_info_of_validator :
    validator -> [ `Fixed of int | `Variable of bytes -> int -> int ]
  (** Wire-size info for the validator. *)
end

type 'r codec = 'r Codec.t
(** Convenience alias so callers can write [Wire.codec] instead of
    [Wire.Codec.t]. *)

val pp_value : 'r codec -> 'r Fmt.t
(** [pp_value c] is a formatter that prints a record field-by-field through
    codec [c]. Integer-valued fields show as [name = value]; non-integer fields
    are skipped. Use with [%a]: [Fmt.pr "%a@." (Wire.pp_value c) v]. *)

(** {1 Nested Codec Combinators}

    These combinators extend the type language with structured sub-codecs,
    optional fields, and repeated elements -- for protocols like CCSDS TM frames
    where the layout depends on mission configuration. *)

val codec : 'r Codec.t -> 'r typ
(** [codec c] embeds a sub-codec as a field type. The sub-codec's decode and
    encode functions are called at the appropriate offset. *)

val optional : bool expr -> 'a typ -> 'a option typ
(** [optional present t] is a field that is present when [present] evaluates to
    [true], absent otherwise. Absent fields decode as [None] and consume zero
    bytes. *)

val optional_or : bool expr -> default:'a -> 'a typ -> 'a typ
(** [optional_or present ~default t] is a field that decodes to the inner value
    when [present] is true, or returns [default] when absent. No option wrapper
    -- zero allocation for the absent case. *)

val repeat : size:int expr -> 'a typ -> 'a list typ
(** [repeat ~size t] parses elements of type [t] repeatedly until [size] bytes
    have been consumed. *)

val repeat_seq : ('a, 'seq) seq_map -> size:int expr -> 'a typ -> 'seq typ
(** Repeat with custom builder. *)

(** {1 Export}

    {!Everparse} is the pure export layer. The normal workflow is:

    - build a record-shaped description with {!Field} and {!Codec};
    - project it with {!Everparse.schema};
    - emit one [.3d] file per schema with {!Everparse.write_3d};
    - run EverParse/C tooling with [Wire_3d];
    - optionally generate OCaml FFI stubs with [Wire_stubs].

    For unusual EverParse constructs that have no codec equivalent yet, see the
    explicit escape hatch {!Everparse.Raw}. *)

module Everparse : sig
  type struct_
  (** 3D struct declaration. *)

  type decl
  (** Top-level 3D declaration. *)

  type decl_case
  (** Case of a top-level 3D casetype declaration. *)

  type module_
  (** A 3D module. *)

  type t = {
    name : string;
    module_ : module_;
    wire_size : int option;
    source : struct_ option;
        (** Pre-[with_output] source struct, [Some] for codec-derived schemas
            and [None] for raw-module schemas. *)
  }
  (** A named 3D schema with its module and wire size ([None] for variable-size
      schemas). *)

  val struct_of_codec : 'r Codec.t -> struct_
  (** Projects a record codec to a 3D struct. *)

  val schema_of_struct : struct_ -> t
  (** [schema_of_struct s] builds a schema from a raw 3D struct while using the
      same EverParse output-types pattern as {!schema}. Named fields extract via
      [WireSet*] callbacks; anonymous fields remain validation-only. *)

  val schema : 'r Codec.t -> t
  (** [schema codec] builds a schema from a codec. The resulting module uses the
      EverParse output-types pattern: the generated C validates AND extracts all
      field values via schema-prefixed extern callbacks ([<Name>Set*]). *)

  val filename : t -> string
  (** [filename s] is the [.3d] output filename for schema [s]. *)

  val uses_wire_ctx : t -> bool
  (** [uses_wire_ctx s] is [true] when the schema declares the [WireCtx] extern
      typedef. The generated C header then [#include]s
      [<Name>_ExternalTypedefs.h], so that file must be present at compile time.
      Schemas built via {!schema} always satisfy this. *)

  type plug_field = {
    pf_name : string;
    pf_idx : int;
    pf_c_type : string;
    pf_setter : string;
    pf_val_c_type : string;
  }
  (** Plug info: the data needed to materialise a typed struct and [<Name>Set*]
      dispatchers for a schema. See {!Everparse.plug_field}. *)

  val plug_fields : t -> plug_field list
  (** [plug_fields s] enumerates the named fields of the source struct in
      declaration order. Returns [[]] for schemas without a source struct. *)

  val plug_setters : t -> (string * string) list
  (** [plug_setters s] lists the unique [WireSet*] setters referenced by [s] as
      [(setter_name, val_c_type)] pairs. *)

  val entrypoint_struct : t -> struct_ option
  (** Entrypoint typedef struct in the schema's module, if any. *)

  val extern_fn_names : t -> string list
  (** Names of every extern function declared in the schema's module. *)

  type field_action_form = No_action | On_act | On_success

  val field_action_forms :
    struct_ -> (string option * bool * field_action_form) list
  (** [field_action_forms st] enumerates fields as [(name, is_bitfield, form)]
      tuples. Used by tests to assert the codegen invariant that bitfields carry
      [On_act], scalars [On_success], anonymous fields [No_action]. *)

  val write_3d : outdir:string -> t list -> unit
  (** Writes one [.3d] file per schema into [outdir]. *)

  module Raw : sig
    (** Escape hatch for manual 3D authoring.

        These constructors exist for EverParse features that currently have no
        codec-level equivalent. Most users should stay on the {!Field}/{!Codec}
        path and use {!struct_of_codec} or {!schema}. *)

    type nonrec struct_ = struct_
    type field = Field.packed
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

    val casetype_decl : string -> param list -> 'a typ -> decl_case list -> decl
    (** Top-level casetype declaration. *)

    val module_ : ?doc:string -> decl list -> module_
    (** Builds a 3D module from declarations. *)

    val to_3d : module_ -> string
    (** Renders a 3D module to text. *)

    val to_3d_file : string -> module_ -> unit
    (** Writes a rendered 3D module to a file. *)

    val field :
      string -> ?constraint_:bool expr -> ?action:Action.t -> 'a typ -> field
    (** Named field. Returns a packed {!Field.t}. *)

    val anon_field : 'a typ -> field
    (** Anonymous field in a 3D struct. *)

    val field_ref : field -> int expr
    (** [field_ref f] returns the expression referencing field [f] by name. The
        field must have been created with {!val:field} (not {!anon_field}). *)

    val struct_ : string -> field list -> struct_
    (** Non-parameterised 3D struct. *)

    val struct_name : struct_ -> string
    (** Name of a struct declaration. *)

    val field_names : struct_ -> string list
    (** Named field names in declaration order. *)

    type ocaml_kind =
      | K_int
      | K_int64
      | K_bool
      | K_string
      | K_unit
          (** The OCaml representation kind of a field (for FFI stub
              generation). *)

    val field_kinds : struct_ -> (string * ocaml_kind) list
    (** Named field names with their OCaml type kind. *)

    val struct_project :
      struct_ -> name:string -> keep:Field.packed list -> struct_
    (** [struct_project s ~name ~keep] returns a copy of [s] renamed to [name]
        keeping only the fields in [keep]. All others become anonymous. *)

    val struct_params : struct_ -> param list
    (** Formal parameters of a struct (empty for non-parameterised structs). *)

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

(** {1 ASCII Bit Diagrams}

    RFC-style 32-bit-wide ASCII bit layout diagrams, following the conventions
    of RFC 791 and similar documents. *)

module Ascii : sig
  val of_struct : Everparse.struct_ -> string
  (** Render a struct as an RFC-style bit diagram. *)

  val of_codec : 'r Codec.t -> string
  (** Render a codec as an RFC-style bit diagram. *)

  val pp_struct : Format.formatter -> Everparse.struct_ -> unit
  (** Pretty-print a struct as an RFC-style bit diagram. *)

  val pp_codec : Format.formatter -> 'r Codec.t -> unit
  (** Pretty-print a codec as an RFC-style bit diagram. *)
end

(**/**)

(** {1 Private}

    Unstable internals exposed for testing. Do not depend on this module. *)

module Private : sig
  module UInt32 = UInt32
  module UInt63 = UInt63
  module Types = Types
  module Eval = Eval

  val param_name : param -> string
  (** Name of a formal parameter. *)

  val param_is_mutable : param -> bool
  (** [true] for output (mutable) parameters. *)

  val param_c_type : param -> string
  (** C type name of a parameter (e.g., ["uint16_t"]). *)

  val ml_type_of : 'a typ -> string
  (** OCaml type name for FFI stub generation. *)

  val c_type_of : 'a typ -> string
  (** C type name for FFI stub generation. *)
end
