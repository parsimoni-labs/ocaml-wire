(** Typed descriptions of binary wire formats.

    A value of type ['a typ] says how values of type ['a] are represented on the
    wire. That description can then be used in three complementary ways:

    - {!decode}, {!decode_string}, {!decode_bytes}, {!encode},
      {!encode_to_bytes}, and {!encode_to_string} interpret the description
      directly and exchange OCaml values with byte streams or buffers;
    - {!Codec} specialises record-shaped descriptions into typed zero-copy field
      accessors over existing buffers;
    - {!C} projects descriptions to EverParse 3D declarations.

    [Wire] is thus about binary values and their layout. The 3D declaration
    language has its own vocabulary in {!C}. *)

module Staged = Staged
module UInt32 = UInt32
module UInt63 = UInt63

(** {1 Expressions}

    Expressions describe sizes, constraints, and dependencies between fields.
    They are part of the wire description itself: no evaluation happens at
    interface construction time.

    They are used whenever the layout depends on previously decoded data: array
    lengths, byte-slice sizes, field constraints, and similar dependent
    structure. *)

type 'a expr
type bitfield = U8 | U16 | U16be | U32 | U32be
type 'a typ

module Action : sig
  type t = Action.t
  type stmt = Action.stmt

  val on_success : stmt list -> t
  val on_act : stmt list -> t
  val assign : string -> int expr -> stmt
  val return_bool : bool expr -> stmt
  val abort : stmt
  val if_ : bool expr -> stmt list -> stmt list option -> stmt
  val var : string -> int expr -> stmt
end

val int : int -> int expr
(** Constant integer expression. *)

val int64 : int64 -> int64 expr
(** Constant 64-bit integer expression. *)

val field_ref : string -> int expr
(** Reference to a previously named field. *)

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
  val ( + ) : int expr -> int expr -> int expr
  val ( - ) : int expr -> int expr -> int expr
  val ( * ) : int expr -> int expr -> int expr
  val ( / ) : int expr -> int expr -> int expr
  val ( mod ) : int expr -> int expr -> int expr
  val ( land ) : int expr -> int expr -> int expr
  val ( lor ) : int expr -> int expr -> int expr
  val ( lxor ) : int expr -> int expr -> int expr
  val lnot : int expr -> int expr
  val ( lsl ) : int expr -> int expr -> int expr
  val ( lsr ) : int expr -> int expr -> int expr
  val ( = ) : 'a expr -> 'a expr -> bool expr
  val ( <> ) : 'a expr -> 'a expr -> bool expr
  val ( < ) : int expr -> int expr -> bool expr
  val ( <= ) : int expr -> int expr -> bool expr
  val ( > ) : int expr -> int expr -> bool expr
  val ( >= ) : int expr -> int expr -> bool expr
  val ( && ) : bool expr -> bool expr -> bool expr
  val ( || ) : bool expr -> bool expr -> bool expr
  val not : bool expr -> bool expr
  val true_ : bool expr
  val false_ : bool expr
  val to_uint8 : int expr -> int expr
  val to_uint16 : int expr -> int expr
  val to_uint32 : int expr -> int expr
  val to_uint64 : int expr -> int expr
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

val uint32 : UInt32.t typ
(** Unsigned 32-bit little-endian integer. *)

val uint32be : UInt32.t typ
(** Unsigned 32-bit big-endian integer. *)

val uint63 : UInt63.t typ
(** Unsigned 63-bit little-endian integer carried on 8 bytes. *)

val uint63be : UInt63.t typ
(** Unsigned 63-bit big-endian integer carried on 8 bytes. *)

val uint64 : int64 typ
(** Unsigned 64-bit little-endian integer represented as [int64]. *)

val uint64be : int64 typ
(** Unsigned 64-bit big-endian integer represented as [int64]. *)

val bits : width:int -> bitfield -> int typ
(** Bitfield of the given width within the given base word. *)

val map : ('w -> 'a) -> ('a -> 'w) -> 'w typ -> 'a typ
(** View a wire value through decode and encode functions. *)

val to_bool : int typ -> bool typ
(** Boolean view over an integer wire value. Zero is [false], non-zero is
    [true]. *)

val cases : 'a list -> int typ -> 'a typ
(** Finite positional mapping over an integer representation.

    The decoded integer is used as a zero-based index into the list. Decoding or
    encoding raises [Invalid_argument] if the value falls outside the list or
    the encoded value is not present in it. *)

val empty : unit typ
(** Empty description carrying no bytes and producing [()]. *)

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

val array : len:int expr -> 'a typ -> 'a list typ
(** Repetition of a description, with length computed from an expression. *)

val byte_array : size:int expr -> string typ
(** Fixed-size byte sequence copied as a string. *)

val byte_slice : size:int expr -> Bytesrw.Bytes.Slice.t typ
(** Fixed-size byte sequence exposed as a zero-copy slice. *)

val single_elem_array : size:int expr -> 'a typ -> 'a typ
(** One logical value carried inside a region described by an array-sized byte
    count.

    This is for layouts where a length expression denotes the size of a region,
    but that region is known to contain exactly one value, such as a single
    nested message. *)

val single_elem_array_at_most : size:int expr -> 'a typ -> 'a typ
(** Like {!single_elem_array}, but treating [size] as an upper bound rather than
    an exact size.

    This is for length-prefixed regions where the one logical element may
    consume fewer bytes than the available space. *)

val enum : string -> (string * int) list -> int typ -> int typ
(** Named integer enumeration with validation. *)

val variants : string -> (string * 'a) list -> int typ -> 'a typ
(** Enumeration viewed as an OCaml variant-like value. *)

type ('tag, 'a) case

val case : 'tag -> 'a typ -> ('tag, 'a) case
(** Tagged branch of a casetype. *)

val default : 'a typ -> ('tag, 'a) case
(** Default branch of a casetype. *)

val casetype : string -> 'tag typ -> ('tag, 'a) case list -> 'a typ
(** Tag-dispatched choice between several descriptions. *)

val pp_3d_typ : Format.formatter -> 'a typ -> unit
(** Pretty-printer for wire descriptions in 3D syntax. *)

val wire_size : 'a typ -> int option
(** Fixed wire size of a description, if known statically. *)

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

val pp_parse_error : Format.formatter -> parse_error -> unit
(** Pretty-printer for decode errors. *)

exception Parse_error of parse_error
(** Exception form of {!parse_error}.

    The direct decoding functions in {!Wire} return results; lower-level and
    codec-oriented operations use the exception form on hot paths. *)

(** {1 Direct Decoding and Encoding}

    These functions interpret a ['a typ] directly and exchange ordinary OCaml
    values with bytes.

    Use them when you want a value now: one-shot decoding, streaming code built
    around {!Bytesrw}, tests, small tools, and formats that are naturally
    consumed as values.

    Use {!Codec} instead when the format is record-shaped and the main goal is
    repeated access to individual fields in an existing buffer, without
    allocating an OCaml record for each read. *)

val decode : 'a typ -> Bytesrw.Bytes.Reader.t -> ('a, parse_error) result
(** Decodes one value from the current reader position.

    Decoding is prefix-based: success does not imply that the reader is
    exhausted afterwards. *)

val decode_string : 'a typ -> string -> ('a, parse_error) result
(** Decodes one value from the start of the string.

    Trailing bytes, if any, are left uninterpreted. *)

val decode_bytes : 'a typ -> bytes -> ('a, parse_error) result
(** Decodes one value from the start of the byte sequence.

    Trailing bytes, if any, are left uninterpreted. *)

(** {1 Direct Encoding}

    Encoding follows the same description language as decoding. The functions in
    this section are the direct counterparts of {!decode}, {!decode_string}, and
    {!decode_bytes}: they work with whole OCaml values rather than field-level
    accessors. *)

val encode : 'a typ -> 'a -> Bytesrw.Bytes.Writer.t -> unit
(** Encodes one value to a {!Bytesrw.Bytes.Writer.t}.

    This function is exception-based. Unsupported description forms, such as
    unresolved type references, raise an exception rather than returning an
    error value. *)

val encode_to_bytes : 'a typ -> 'a -> bytes
(** Encodes one value to freshly allocated bytes. *)

val encode_to_string : 'a typ -> 'a -> string
(** Encodes one value to a freshly allocated string. *)

(** {1 Zero-Copy Codecs}

    A codec is the record-oriented companion to a wire description.

    Where {!decode} and {!encode} turn whole values into bytes and back, a codec
    seals a record description together with its typed fields and can then:

    - decode or encode a whole record value;
    - compute its wire size;
    - derive staged readers and writers for individual fields.

    Codecs are the right tool for hot paths that inspect or update a few fields
    repeatedly in existing buffers.

    The underlying wire description remains the same; what changes is the mode
    of use. Direct decoding gives you values. A codec gives you a sealed,
    record-specific operational interface. *)

module Codec : sig
  type ('a, 'r) field = ('a, 'r) Codec.field
  (** Description of one typed field in a record codec. *)

  type 'r t = 'r Codec.t
  (** Sealed codec for record values of type ['r]. *)

  type ('f, 'r) fields = ('f, 'r) Codec.fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields
        (** Heterogeneous field list in record order. *)

  val field : string -> 'a typ -> ('r -> 'a) -> ('a, 'r) field
  (** Declares one field of a record codec. *)

  val view : string -> 'f -> ('f, 'r) fields -> 'r t
  (** Builds a record codec from a constructor and its fields.

      The constructor is applied in field order at decode time; the field
      projections are used at encode time.

      Example:
      {[
        type header = { version : int; length : int }

        let header_codec =
          Codec.view "Header"
            (fun version length -> { version; length })
            Codec.
              [
                Codec.field "version" uint8 (fun h -> h.version);
                Codec.field "length" uint16 (fun h -> h.length);
              ]
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

  val decode : 'r t -> bytes -> int -> 'r
  (** Decodes one record value from a buffer at the given base offset.

      This function is exception-based and raises {!Parse_error} on malformed
      input. *)

  val encode : 'r t -> 'r -> bytes -> int -> unit
  (** Encodes one record value into a buffer at the given base offset.

      Raises [Invalid_argument] if the destination buffer is too short. *)

  val get : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a) Staged.t
  (** Staged field reader specialised for one field of one codec.

      Force the staged value once, then reuse the resulting function for many
      reads. *)

  val set : 'r t -> ('a, 'r) field -> (bytes -> int -> 'a -> unit) Staged.t
  (** Staged field writer specialised for one field of one codec.

      Force the staged value once, then reuse the resulting function for many
      writes. *)

  val field_ref : ('a, 'r) field -> int expr
  (** Field reference expression, used in dependent sizes and constraints. *)
end

(** {1 3D Projection}

    {!C} is the 3D-facing companion to {!Wire}. It reuses the same description
    language, but introduces names for declarations, modules, and schema
    generation.

    This is the part of the library to use when the goal is not to decode bytes
    in OCaml, but to describe or emit EverParse 3D artefacts from the same wire
    definitions. *)

module C : sig
  type schema = C.schema
  (** A named 3D schema together with its output module and wire size. *)

  type struct_ = C.struct_
  (** 3D struct declaration. *)

  type field = C.field
  (** Field of a 3D struct. *)

  type param = C.param
  (** Parameter of a parameterised 3D declaration. *)

  type action = Action.t
  (** 3D action attached to a field. *)

  type action_stmt = Action.stmt
  (** Statement inside a 3D action. *)

  type decl = C.decl
  (** Top-level 3D declaration. *)

  type decl_case = C.decl_case
  (** Case of a top-level 3D casetype declaration. *)

  type module_ = C.module_
  (** A 3D module. *)

  val struct_of_codec : 'r Codec.t -> struct_
  (** Projects a record codec to a 3D struct. *)

  val schema : 'r Codec.t -> schema
  (** Builds a one-struct schema from a codec. *)

  val generate : outdir:string -> schema list -> unit
  (** Writes one [.3d] file per schema into [outdir]. *)

  val typedef :
    ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
  (** Top-level typedef declaration. *)

  val define : string -> int -> decl
  (** Top-level integer definition. *)

  val extern_fn : string -> param list -> 'a typ -> decl
  (** External function declaration. *)

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
    string -> ?constraint_:bool expr -> ?action:action -> 'a typ -> field
  (** Named field in a 3D struct. *)

  val anon_field : 'a typ -> field
  (** Anonymous field in a 3D struct. *)

  val struct_ : string -> field list -> struct_
  (** Non-parameterised 3D struct. *)

  val struct_name : struct_ -> string
  (** Name of a struct declaration. *)

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

  val wire_size : struct_ -> int option
  (** Fixed wire size of a struct, if known statically. *)

  val ml_type_of : 'a typ -> string
  (** OCaml type name used by stub generators. *)

  val of_module : name:string -> module_:module_ -> wire_size:int -> schema
  (** Wraps an existing 3D module as a schema. *)
end
