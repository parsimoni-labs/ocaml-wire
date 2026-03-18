(** Typed descriptions of binary wire formats.

    [Wire] is an authoring language for binary formats.

    The point is not merely to emit EverParse 3D files. The point is to write
    the format once, at the level of typed binary descriptions, and then reuse
    that same description in several ways.

    A value of type ['a typ] says how values of type ['a] are laid out on the
    wire. From that one description, this library derives three complementary
    uses:

    - {!decode}, {!decode_string}, {!decode_bytes}, {!encode},
      {!encode_to_bytes}, and {!encode_to_string} interpret the description
      directly and exchange OCaml values with byte streams or buffers;
    - {!Codec} specialises record-shaped descriptions into typed zero-copy field
      accessors over existing buffers;
    - {!C} projects descriptions to EverParse 3D declarations.

    Writing the source description in OCaml rather than in a directory of
    handwritten [.3d] files has a practical benefit: the format lives in a real
    programming language, with abstraction, reuse, tests, fuzzing, and ordinary
    composition tools close at hand. The generated 3D is then a projection of
    that description, not a second source of truth to keep in sync.

    [Wire] is thus about binary values and their layout. {!C} gives names to the
    3D declaration vocabulary when a 3D artefact is wanted.

    See also the official EverParse manual:
    {{:https://project-everest.github.io/everparse/3d.html}3d: Dependent Data
     Descriptions for Verified Validation} and the
    {{:https://project-everest.github.io/everparse/3d-lang.html}3d language
     reference}. *)

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
type 'a typ

type param
(** Untyped formal parameter declaration. Create via {!Param.v}. *)

module Param : sig
  (** Typed parameter handles.

      Input parameters carry their value. Output parameters carry a mutable cell
      that actions write to during decoding. Both are passed as regular OCaml
      function arguments to codec constructors — no separate environment.

      {[
        let bounded ~max_len ~out_len =
          Codec.view "Bounded"
            ~params:[ Param.v max_len; Param.v out_len ]
            ~where:Expr.(field_ref "Length" <= field_ref "max_len")
            (fun len data -> { len; data })
            Codec.
              [
                Codec.field "Length"
                  ~action:(Action.on_success [ Action.assign out_len ... ])
                  uint16be (fun r -> r.len);
                Codec.field "Data" (byte_array ~size:(field_ref "Length"))
                  (fun r -> r.data);
              ]

        let out = Param.output "out_len" uint16be
        let c = bounded
          ~max_len:(Param.input "max_len" uint16be 1024)
          ~out_len:out
        let v = Codec.decode c buf 0
        let len = Param.get out
      ]}

      Output parameters are mutable — do not share an output handle across
      concurrent decodes. *)

  type input = Param.input
  (** Phantom kind for immutable input parameters. *)

  type output = Param.output
  (** Phantom kind for mutable output parameters. *)

  type ('a, 'k) t = ('a, 'k) Param.t
  (** Typed handle for one formal parameter. *)

  val input : string -> 'a typ -> 'a -> ('a, input) t
  (** [input name typ value] creates an input parameter bound to [value]. *)

  val output : string -> 'a typ -> ('a, output) t
  (** [output name typ] creates a mutable output parameter. *)

  val v : ('a, 'k) t -> param
  (** Formal declaration for codecs and 3D structs. *)

  val name : ('a, 'k) t -> string
  (** Parameter name. *)

  val get : ('a, 'k) t -> 'a
  (** Read the current value. For output params, call after decoding. *)

  val set : ('a, 'k) t -> 'a -> unit
  (** Set the value of a parameter. *)

  type packed = Param.packed =
    | Pack : ('a, 'k) t -> packed  (** Existentially packed parameter handle. *)
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
      [out]. Only output parameters (created with {!Param.output}) can be
      assigned to — the type system prevents assigning to input parameters. *)

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
  (** Arithmetic, bitwise, and comparison operators on expressions.

      Open this module locally to build constraint and size expressions:
      [Expr.(field_ref "x" + int 1)]. *)

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

  val to_uint8 : int expr -> int expr
  (** Cast to unsigned 8-bit range. *)

  val to_uint16 : int expr -> int expr
  (** Cast to unsigned 16-bit range. *)

  val to_uint32 : int expr -> int expr
  (** Cast to unsigned 32-bit range. *)

  val to_uint64 : int expr -> int expr
  (** Cast to unsigned 64-bit range. *)
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

val bits : width:int -> bitfield -> int typ
(** Bitfield of the given width within the given base word. *)

val map : decode:('w -> 'a) -> encode:('a -> 'w) -> 'w typ -> 'a typ
(** [map ~decode ~encode t] views a wire value through conversion functions. *)

val bool : int typ -> bool typ
(** [bool t] views an integer wire value as a boolean. Zero is [false], non-zero
    is [true]. *)

val lookup : 'a list -> int typ -> 'a typ
(** [lookup table t] decodes an integer as a zero-based index into a finite
    table.

    The decoded integer selects the corresponding element from the list. An
    out-of-range index produces an {!Invalid_tag} parse error (reported via
    [result] in {!decode} / {!decode_string} / {!decode_bytes}). Encoding raises
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

val array : len:int expr -> 'a typ -> 'a list typ
(** Repetition of a description, with length computed from an expression. *)

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
    named values. The result is still [int] — use {!variants} instead if you
    want to decode to proper OCaml values. [enum] is mainly useful for 3D
    projection where the name and cases appear in the generated [.3d] file. *)

val variants : string -> (string * 'a) list -> int typ -> 'a typ
(** [variants name cases base] maps integer values to OCaml values via a named
    enumeration. Unlike {!enum}, this converts to proper OCaml values. *)

type ('tag, 'a) case

val case : 'tag -> 'a typ -> ('tag, 'a) case
(** Tagged branch of a casetype. *)

val default : 'a typ -> ('tag, 'a) case
(** Default branch of a casetype. *)

val casetype : string -> 'tag typ -> ('tag, 'a) case list -> 'a typ
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

val decode : 'a typ -> Bytesrw.Bytes.Reader.t -> ('a, parse_error) result
(** Decodes one value from the current reader position.

    Parameters referenced by the description must be bound in the closed-over
    {!Param} handles before decoding. Output parameters are updated by actions
    during decoding; read them back with {!Param.get}.

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
    accessors.

    Unlike decoding, encoding is exception-based rather than result-based.
    Decoding fails on untrusted input (truncated data, constraint violations),
    so callers need structured errors. Encoding fails only on programmer errors
    (wrong description for the value, unsupported form), which are not
    data-dependent and should not be silently ignored. *)

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

  val field :
    string ->
    ?constraint_:bool expr ->
    ?action:Action.t ->
    'a typ ->
    ('r -> 'a) ->
    ('a, 'r) field
  (** Declares one field of a record codec.

      Field constraints and actions share the same semantics as in direct
      decoding. *)

  val view :
    string ->
    ?params:Param.packed list ->
    ?where:bool expr ->
    'f ->
    ('f, 'r) fields ->
    'r t
  (** Builds a sealed record codec from a constructor and its fields.

      The constructor is applied in field order at decode time; the field
      projections are used at encode time.

      [params] passes the parameter handles (packed with {!Param.Pack}). Input
      params carry their values; output params carry mutable cells that actions
      update during decoding. [where] is a record-level constraint checked after
      all fields and actions have run.

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

  val decode : 'r t -> bytes -> int -> ('r, parse_error) result
  (** Decodes one record value from a buffer at the given base offset. *)

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
    definitions.

    For the target language itself, see the
    {{:https://project-everest.github.io/everparse/3d.html}EverParse manual} and
    the
    {{:https://project-everest.github.io/everparse/3d-lang.html}3d language
     reference}. *)

module C : sig
  type schema
  (** A named 3D schema together with its output module and wire size. *)

  type struct_
  (** 3D struct declaration. *)

  type field
  (** Field of a 3D struct. *)

  type decl
  (** Top-level 3D declaration. *)

  type decl_case
  (** Case of a top-level 3D casetype declaration. *)

  type module_
  (** A 3D module. *)

  val struct_of_codec : 'r Codec.t -> struct_
  (** Projects a record codec to a 3D struct. *)

  val schema : 'r Codec.t -> schema
  (** Builds a one-struct schema from a codec. *)

  val generate : outdir:string -> schema list -> unit
  (** Writes one [.3d] file per schema into [outdir]. *)

  val typedef :
    ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
  (** Top-level typedef declaration.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#structs}struct
       definitions}. *)

  val define : string -> int -> decl
  (** Top-level integer definition.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#constants-and-enumerations}constants}.
  *)

  val extern_fn : string -> param list -> 'a typ -> decl
  (** External function declaration used by 3D actions.

      See the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#actions}actions}
      section. *)

  val extern_probe : ?init:bool -> string -> decl
  (** External probe declaration. *)

  val enum_decl : string -> (string * int) list -> 'a typ -> decl
  (** Top-level enum declaration.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#constants-and-enumerations}enumerations}.
  *)

  val decl_case : int -> 'a typ -> decl_case
  (** One tagged case in a top-level casetype declaration. *)

  val decl_default : 'a typ -> decl_case
  (** Default case in a top-level casetype declaration. *)

  val casetype_decl : string -> param list -> 'a typ -> decl_case list -> decl
  (** Top-level casetype declaration.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#tagged-unions-or-casetype}casetype}.
  *)

  val module_ : ?doc:string -> decl list -> module_
  (** Builds a 3D module from declarations.

      See the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#modular-structure-and-files}modular
       structure} section. *)

  val to_3d : module_ -> string
  (** Renders a 3D module to text. *)

  val to_3d_file : string -> module_ -> unit
  (** Writes a rendered 3D module to a file. *)

  val field :
    string -> ?constraint_:bool expr -> ?action:Action.t -> 'a typ -> field
  (** Named field in a 3D struct.

      Field constraints correspond to the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#constraints}constraints}
      language; field actions correspond to the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#actions}actions}
      language. *)

  val anon_field : 'a typ -> field
  (** Anonymous field in a 3D struct. *)

  val struct_ : string -> field list -> struct_
  (** Non-parameterised 3D struct.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#structs}structs}.
  *)

  val struct_name : struct_ -> string
  (** Name of a struct declaration. *)

  val struct_typ : struct_ -> unit typ
  (** View a 3D struct as a wire description. *)

  val param : string -> 'a typ -> param
  (** Immutable parameter declaration.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#parameterized-data-types}parameterized
       data types}. *)

  val mutable_param : string -> 'a typ -> param
  (** Mutable parameter declaration.

      Corresponds to mutable pointer parameters in 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#actions}actions}.
  *)

  val param_struct :
    string -> param list -> ?where:bool expr -> field list -> struct_
  (** Parameterised 3D struct.

      Corresponds to 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#parameterized-data-types}parameterized
       data types}. The optional [where] clause corresponds to the same
      section's precondition form. *)

  val apply : 'a typ -> int expr list -> 'a typ
  (** Apply a parameterised description to integer arguments.

      Corresponds to instantiation of 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#parameterized-data-types}parameterized
       data types}. *)

  val type_ref : string -> 'a typ
  (** Unqualified type reference.

      See the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#modular-structure-and-files}modular
       structure} section. *)

  val qualified_ref : string -> string -> 'a typ
  (** Qualified type reference.

      See the 3D
      {{:https://project-everest.github.io/everparse/3d-lang.html#modular-structure-and-files}modular
       structure} section. *)

  val pp_typ : Format.formatter -> 'a typ -> unit
  (** Pretty-printer for 3D-facing type syntax. *)

  val pp_module : Format.formatter -> module_ -> unit
  (** Pretty-printer for 3D modules. *)

  val size : struct_ -> int option
  (** Fixed wire size of a struct, if known statically. *)

  val of_module : name:string -> module_:module_ -> wire_size:int -> schema
  (** Wraps an existing 3D module as a schema. *)
end

(** {1 ASCII Bit Diagrams}

    RFC-style 32-bit-wide ASCII bit layout diagrams, following the conventions
    of RFC 791 and similar documents. *)

module Ascii : sig
  val of_struct : C.struct_ -> string
  (** Render a struct as an RFC-style bit diagram. *)

  val of_codec : 'r Codec.t -> string
  (** Render a codec as an RFC-style bit diagram. *)

  val pp_struct : Format.formatter -> C.struct_ -> unit
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

  val ml_type_of : 'a typ -> string
  (** OCaml type name for FFI stub generation. *)
end
