(** Dependent Data Descriptions for binary wire formats.

    Wire is a GADT-based DSL for describing binary wire formats compatible with
    EverParse's 3D language. Define your format once, then:

    - Use {!to_3d} to emit EverParse 3D format for verified C parser generation

    {b EverParse 3D Language Support}

    This module supports the full EverParse 3D language including:
    - Base types: UINT8, UINT16, UINT32, UINT64 (LE and BE)
    - Bitfields with constraints
    - Enumerations
    - Structs with dependent fields
    - Tagged unions (casetype)
    - Arrays (fixed-size, byte-size, variable)
    - Parameterized types
    - Constraints (where clauses)
    - Actions (:on-success, :act)
    - Extern declarations *)

(** {1 Staged Computations}

    Following the pattern from
    {{:https://mirage.github.io/repr/repr/Repr/index.html}Irmin's repr}, staged
    functions make specialization explicit. The cost of building a specialized
    encoder/decoder is paid once when [unstage] is called, then subsequent calls
    to the resulting function are fast.

    Example:
    {[
      let codec = Record.record "Packet" ~default ...
      let encode = Staged.unstage (Record.encode codec)  (* pay cost here *)
      for _ = 1 to n do
        let _ = encode packet in  (* fast - no interpretation overhead *)
        ...
      done
    ]} *)
module Staged : sig
  type +'a t
  (** A staged computation of type ['a]. *)

  val stage : 'a -> 'a t
  (** [stage x] wraps [x] in a staged computation. *)

  val unstage : 'a t -> 'a
  (** [unstage t] extracts the value from a staged computation. This is where
      the cost of specialization is paid. *)
end

(** {1 Unboxed Integer Types}

    On 64-bit platforms, these types are unboxed (immediate) for zero-allocation
    parsing. On 32-bit platforms, the module will fail at initialization. *)

module UInt32 : sig
  type t = int
  (** Unsigned 32-bit integer. Unboxed on 64-bit platforms (fits in 63-bit int).
  *)

  val get_le : bytes -> int -> t
  (** [get_le buf off] reads a little-endian value from [buf] at offset [off].
  *)

  val get_be : bytes -> int -> t
  (** [get_be buf off] reads a big-endian value from [buf] at offset [off]. *)

  val set_le : bytes -> int -> t -> unit
  (** [set_le buf off v] writes [v] in little-endian at offset [off]. *)

  val set_be : bytes -> int -> t -> unit
  (** [set_be buf off v] writes [v] in big-endian at offset [off]. *)

  val to_int : t -> int
  (** [to_int v] converts [v] to an OCaml int. *)

  val of_int : int -> t
  (** [of_int n] converts an OCaml int to [t]. *)
end

module UInt63 : sig
  type t = int
  (** Unsigned 63-bit integer. Reads 8 bytes but masks to 63 bits. *)

  val get_le : bytes -> int -> t
  (** [get_le buf off] reads a little-endian value from [buf] at offset [off].
  *)

  val get_be : bytes -> int -> t
  (** [get_be buf off] reads a big-endian value from [buf] at offset [off]. *)

  val set_le : bytes -> int -> t -> unit
  (** [set_le buf off v] writes [v] in little-endian at offset [off]. *)

  val set_be : bytes -> int -> t -> unit
  (** [set_be buf off v] writes [v] in big-endian at offset [off]. *)

  val to_int : t -> int
  (** [to_int v] converts [v] to an OCaml int. *)

  val of_int : int -> t
  (** [of_int n] converts an OCaml int to [t]. *)
end

(** {1 Endianness} *)

type endian =
  | Little  (** Little-endian (native for most systems) *)
  | Big  (** Big-endian (network byte order) *)

(** {1 Types} *)

type _ typ
(** ['a typ] describes how to parse/serialize values of type ['a]. *)

type _ expr
(** ['a expr] represents a computation yielding type ['a]. *)

type action
(** An imperative action. *)

type action_stmt
(** An action statement. *)

type _ record_codec
(** A record codec for encoding/decoding fixed-size binary records of type ['a].
    Use the {!Record} module to create and use codecs. *)

type ('a, 'r) field_codec
(** A field codec for a field of type ['a] in record type ['r]. *)

type _ field_codec_packed
(** A type-erased field codec for a record of type ['r]. *)

(** {1 Expressions}

    Expressions for constraints, array sizes, and computations. *)

(** {2 Literals} *)

val int : int -> int expr
(** [int n] is the constant integer [n]. *)

val int64 : int64 -> int64 expr
(** [int64 n] is the constant 64-bit integer [n]. *)

val true_ : bool expr
(** [true_] is the constant [true] expression. *)

val false_ : bool expr
(** [false_] is the constant [false] expression. *)

(** {2 Field References} *)

val ref : string -> int expr
(** [ref name] references the integer value of field [name] from the current
    struct. Field values are converted to [int] via {!val_to_int}. *)

(** {2 Sizeof} *)

val sizeof : 'a typ -> int expr
(** [sizeof t] is the size of type [t] in bytes. *)

val sizeof_this : int expr
(** [sizeof_this] is the size of the non-variable prefix of the current struct.
*)

val field_pos : int expr
(** [field_pos] is the current field position. *)

(** {2 Arithmetic Operators} *)

module Expr : sig
  (** {2 Arithmetic operators} *)

  val ( + ) : int expr -> int expr -> int expr
  (** [x + y] is the sum of [x] and [y]. *)

  val ( - ) : int expr -> int expr -> int expr
  (** [x - y] is [x] minus [y]. *)

  val ( * ) : int expr -> int expr -> int expr
  (** [x * y] is the product of [x] and [y]. *)

  val ( / ) : int expr -> int expr -> int expr
  (** [x / y] is [x] divided by [y]. *)

  val ( mod ) : int expr -> int expr -> int expr
  (** [x mod y] is the remainder of [x] divided by [y]. *)

  (** {2 Bitwise operators} *)

  val ( land ) : int expr -> int expr -> int expr
  (** [x land y] is the bitwise AND of [x] and [y]. *)

  val ( lor ) : int expr -> int expr -> int expr
  (** [x lor y] is the bitwise OR of [x] and [y]. *)

  val ( lxor ) : int expr -> int expr -> int expr
  (** [x lxor y] is the bitwise XOR of [x] and [y]. *)

  val lnot : int expr -> int expr
  (** [lnot x] is the bitwise complement of [x]. *)

  val ( lsl ) : int expr -> int expr -> int expr
  (** [x lsl n] is [x] shifted left by [n] bits. *)

  val ( lsr ) : int expr -> int expr -> int expr
  (** [x lsr n] is [x] shifted right by [n] bits (logical). *)

  (** {2 Comparison operators} *)

  val ( = ) : 'a expr -> 'a expr -> bool expr
  (** [x = y] is [true] if [x] equals [y]. *)

  val ( <> ) : 'a expr -> 'a expr -> bool expr
  (** [x <> y] is [true] if [x] does not equal [y]. *)

  val ( < ) : int expr -> int expr -> bool expr
  (** [x < y] is [true] if [x] is less than [y]. *)

  val ( <= ) : int expr -> int expr -> bool expr
  (** [x <= y] is [true] if [x] is less than or equal to [y]. *)

  val ( > ) : int expr -> int expr -> bool expr
  (** [x > y] is [true] if [x] is greater than [y]. *)

  val ( >= ) : int expr -> int expr -> bool expr
  (** [x >= y] is [true] if [x] is greater than or equal to [y]. *)

  (** {2 Logical operators} *)

  val ( && ) : bool expr -> bool expr -> bool expr
  (** [x && y] is the logical AND of [x] and [y]. *)

  val ( || ) : bool expr -> bool expr -> bool expr
  (** [x || y] is the logical OR of [x] and [y]. *)

  val not : bool expr -> bool expr
  (** [not x] is the logical negation of [x]. *)

  (** {2 Casts} *)

  val to_uint8 : int expr -> int expr
  (** [to_uint8 e] casts [e] to an 8-bit unsigned integer. *)

  val to_uint16 : int expr -> int expr
  (** [to_uint16 e] casts [e] to a 16-bit unsigned integer. *)

  val to_uint32 : int expr -> int expr
  (** [to_uint32 e] casts [e] to a 32-bit unsigned integer. *)

  val to_uint64 : int expr -> int expr
  (** [to_uint64 e] casts [e] to a 64-bit unsigned integer. *)
end

(** {1 Type Constructors} *)

(** {2 Integer Types} *)

val uint8 : int typ
(** Unsigned 8-bit integer. *)

val uint16 : int typ
(** Unsigned 16-bit integer, little-endian. *)

val uint16be : int typ
(** Unsigned 16-bit integer, big-endian. *)

val uint32 : UInt32.t typ
(** Unsigned 32-bit integer, little-endian. Unboxed on 64-bit. *)

val uint32be : UInt32.t typ
(** Unsigned 32-bit integer, big-endian. Unboxed on 64-bit. *)

val uint63 : UInt63.t typ
(** Unsigned 63-bit integer, little-endian. Unboxed on 64-bit. Reads 8 bytes. *)

val uint63be : UInt63.t typ
(** Unsigned 63-bit integer, big-endian. Unboxed on 64-bit. Reads 8 bytes. *)

val uint64 : int64 typ
(** Unsigned 64-bit integer, little-endian. Boxed (full 64-bit precision). *)

val uint64be : int64 typ
(** Unsigned 64-bit integer, big-endian. Boxed (full 64-bit precision). *)

(** {2 Bitfields} *)

type bitfield_base
(** Base type for bitfields (UINT8, UINT16, UINT32). *)

val bf_uint8 : bitfield_base
(** 8-bit bitfield base. *)

val bf_uint16 : bitfield_base
(** 16-bit bitfield base, little-endian. *)

val bf_uint16be : bitfield_base
(** 16-bit bitfield base, big-endian. *)

val bf_uint32 : bitfield_base
(** 32-bit bitfield base, little-endian. *)

val bf_uint32be : bitfield_base
(** 32-bit bitfield base, big-endian. *)

val bits : width:int -> bitfield_base -> int typ
(** [bits ~width base] extracts [width] bits from a bitfield base type. *)

(** {2 Type Combinators} *)

val map : ('w -> 'a) -> ('a -> 'w) -> 'w typ -> 'a typ
(** [map dec enc t] maps wire type [t] through [dec] and [enc].
    {[
      let status = map status_of_int int_of_status (bits ~width:3 bf_uint32be)
    ]} *)

val bool : int typ -> bool typ
(** [bool t] maps integer type [t] to boolean (0 = false).
    {[
      let flag = bool (bits ~width:1 bf_uint16be)
    ]} *)

val cases : 'a list -> int typ -> 'a typ
(** [cases variants t] maps integer type [t] through a list of variants.
    Position in the list determines the integer value (0-indexed). Raises
    [Invalid_argument] on unknown values.
    {[
      let ptype = cases [ Telemetry; Telecommand ] (bits ~width:1 bf_uint16be)
    ]} *)

(** {2 Special Types} *)

val unit : unit typ
(** Empty type (zero bytes). *)

val all_bytes : string typ
(** Consume all remaining bytes. *)

val all_zeros : string typ
(** Consume all remaining bytes, validating they are zeros. *)

(** {2 Constraints} *)

val where : bool expr -> 'a typ -> 'a typ
(** [where cond t] adds constraint [cond] to type [t]. *)

(** {2 Arrays} *)

val array : len:int expr -> 'a typ -> 'a list typ
(** [array ~len t] is a fixed-count array of [len] elements. *)

val byte_array : size:int expr -> string typ
(** [byte_array ~size] is [UINT8[:byte-size size]] in 3D. *)

val single_elem_array : size:int expr -> 'a typ -> 'a typ
(** [single_elem_array ~size t] is a single element consuming exactly [size]
    bytes. Emits as [[:byte-size-single-element-array size]] in 3D. *)

val single_elem_array_at_most : size:int expr -> 'a typ -> 'a typ
(** [single_elem_array_at_most ~size t] is a single element consuming at most
    [size] bytes with padding. *)

(** {2 Enumerations} *)

val enum : string -> (string * int) list -> int typ -> int typ
(** [enum name cases base] defines an enumeration. *)

(** {2 Tagged Unions (casetype)} *)

type ('tag, 'a) case
(** A case in a tagged union. *)

val case : 'tag -> 'a typ -> ('tag, 'a) case
(** [case tag t] matches when the discriminant equals [tag]. *)

val default : 'a typ -> ('tag, 'a) case
(** [default t] is the default case. *)

val casetype : string -> 'tag typ -> ('tag, 'a) case list -> 'a typ
(** [casetype name tag cases] defines a tagged union. *)

(** {2 Structs} *)

type field
(** A struct field (type-erased). *)

val field :
  string -> ?constraint_:bool expr -> ?action:action -> 'a typ -> field
(** [field name ?constraint_ ?action t] defines a named field. *)

val anon_field : 'a typ -> field
(** [anon_field t] defines a padding field. *)

type struct_
(** A struct definition. *)

val struct_ : string -> field list -> struct_
(** [struct_ name fields] defines a struct type. *)

val struct_name : struct_ -> string
(** [struct_name s] returns the name of struct [s]. *)

val struct_typ : struct_ -> unit typ
(** [struct_typ s] converts a struct to a type. *)

(** {2 Parameterized Types} *)

type param
(** A type parameter. *)

val param : string -> 'a typ -> param
(** [param name t] declares a parameter with type [t]. *)

val mutable_param : string -> 'a typ -> param
(** [mutable_param name t] declares a mutable output parameter. *)

val param_struct :
  string -> param list -> ?where:bool expr -> field list -> struct_
(** [param_struct name params ?where fields] defines a parameterized struct. *)

val apply : 'a typ -> int expr list -> 'a typ
(** [apply t args] applies arguments to a parameterized type. *)

(** {2 Type References} *)

val type_ref : string -> 'a typ
(** [type_ref name] references a type by name. *)

val qualified_ref : string -> string -> 'a typ
(** [qualified_ref module_ name] references [Module::Type]. *)

(** {1 Actions}

    Actions execute during validation. *)

val on_success : action_stmt list -> action
(** [on_success stmts] creates an [:on-success] action from [stmts]. *)

val on_act : action_stmt list -> action
(** [on_act stmts] creates an [:act] action that always succeeds. *)

val assign : string -> int expr -> action_stmt
(** [assign ptr expr] is [*ptr = expr]. *)

val return_bool : bool expr -> action_stmt
(** [return_bool e] is [return e]. *)

val abort : action_stmt
(** [abort] aborts validation. *)

val action_if :
  bool expr -> action_stmt list -> action_stmt list option -> action_stmt
(** [action_if cond then_ else_] is a conditional. *)

val var : string -> int expr -> action_stmt
(** [var name e] binds a local variable. *)

(** {1 Declarations} *)

type decl
(** A top-level declaration. *)

val typedef : ?entrypoint:bool -> ?export:bool -> ?doc:string -> struct_ -> decl
(** [typedef ?entrypoint ?export ?doc s] declares a type. *)

val define : string -> int -> decl
(** [define name value] is [#define name value]. *)

val extern_fn : string -> param list -> 'a typ -> decl
(** [extern_fn name params ret] declares an external function. *)

val extern_probe : ?init:bool -> string -> decl
(** [extern_probe ?init name] declares an external probe. *)

val enum_decl : string -> (string * int) list -> 'a typ -> decl
(** [enum_decl name cases base] declares a top-level enum. *)

type decl_case
(** A case in a top-level casetype declaration. *)

val decl_case : int -> 'a typ -> decl_case
(** [decl_case tag t] matches when the discriminant equals [tag]. *)

val decl_default : 'a typ -> decl_case
(** [decl_default t] is the default case. *)

val casetype_decl : string -> param list -> 'a typ -> decl_case list -> decl
(** [casetype_decl name params tag cases] declares a top-level casetype. *)

(** {1 Modules} *)

type module_
(** A 3D module (file). *)

val module_ : ?doc:string -> string -> decl list -> module_
(** [module_ ?doc name decls] creates a module. *)

(** {1 3D Output} *)

val to_3d : module_ -> string
(** [to_3d m] emits the module as EverParse 3D format text. *)

val to_3d_file : string -> module_ -> unit
(** [to_3d_file path m] writes the module to a .3d file. *)

(** {1 Pretty Printing} *)

val pp_typ : Format.formatter -> 'a typ -> unit
(** [pp_typ ppf t] pretty-prints type [t] to formatter [ppf]. *)

val pp_module : Format.formatter -> module_ -> unit
(** [pp_module ppf m] pretty-prints module [m] to formatter [ppf]. *)

(** {1 Binary Parsing (bytesrw)}

    Parse binary data according to type schemas. This parser is designed to be
    semantically equivalent to EverParse-generated C parsers, enabling
    differential fuzzing. *)

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

val pp_parse_error : Format.formatter -> parse_error -> unit
(** [pp_parse_error ppf e] pretty-prints parse error [e] to formatter [ppf]. *)

exception Parse_error of parse_error
(** Exception raised by [_exn] decode functions on parse errors. *)

(** {2 Parsing Context}

    The parsing context tracks field values for dependent type evaluation. *)

type ctx
(** Parsing context with field bindings. *)

val empty_ctx : ctx
(** Empty parsing context. *)

(** {2 Parsing Functions} *)

val parse : 'a typ -> Bytesrw.Bytes.Reader.t -> ('a, parse_error) result
(** [parse typ reader] parses a value of type [typ] from [reader]. *)

val parse_string : 'a typ -> string -> ('a, parse_error) result
(** [parse_string typ s] parses a value from a string. *)

val parse_bytes : 'a typ -> bytes -> ('a, parse_error) result
(** [parse_bytes typ b] parses a value from bytes. *)

(** {1 Binary Encoding (bytesrw)}

    Encode OCaml values to binary according to type schemas. *)

val encode : 'a typ -> 'a -> Bytesrw.Bytes.Writer.t -> unit
(** [encode typ v writer] encodes [v] to [writer]. *)

val encode_to_bytes : 'a typ -> 'a -> bytes
(** [encode_to_bytes typ v] encodes [v] to bytes. *)

val encode_to_string : 'a typ -> 'a -> string
(** [encode_to_string typ v] encodes [v] to a string. *)

(** {1 Record Codec (Legacy API)}

    The legacy record codec API using explicit field_codec, pack_field, and
    record_codec. For new code, prefer the {!Codec} module. *)

val field_codec :
  string ->
  ?constraint_:bool expr ->
  'a typ ->
  get:('r -> 'a) ->
  set:('a -> 'r -> 'r) ->
  ('a, 'r) field_codec
(** [field_codec name ?constraint_ typ ~get ~set] creates a field codec. *)

val pack_field : ('a, 'r) field_codec -> 'r field_codec_packed
(** [pack_field fc] type-erases a field codec for use in a list. *)

val record_codec :
  string -> default:'r -> 'r field_codec_packed list -> 'r record_codec
(** [record_codec name ~default fields] creates a record codec from packed
    fields. *)

val record_to_struct : 'r record_codec -> struct_
(** [record_to_struct codec] converts a record codec to a struct for 3D
    generation. *)

val encode_record_to_slice :
  'r record_codec -> ('r -> Bytesrw.Bytes.Slice.t) Staged.t
(** [encode_record_to_slice codec] returns a staged encoder that writes a record
    to a new slice. *)

val decode_record_from_slice :
  'r record_codec -> (Bytesrw.Bytes.Slice.t -> 'r) Staged.t
(** [decode_record_from_slice codec] returns a staged decoder that reads a
    record from a slice. *)

type 'r encode_context = {
  buffer : bytes;
  wire_size : int;
  encode : 'r -> unit;
}
(** Context for zero-copy encoding to a pre-allocated buffer. *)

val encode_record_to_bytes : 'r record_codec -> 'r encode_context option
(** [encode_record_to_bytes codec] returns a context for encoding records to a
    shared buffer, or [None] if the codec has variable size. *)

val decode_record_from_bytes : 'r record_codec -> (bytes -> int -> 'r) Staged.t
(** [decode_record_from_bytes codec] returns a staged decoder that reads a
    record from bytes at a given offset. *)

(** {2 Record Module (Convenience API)}

    A convenience module that wraps the legacy record codec API for cleaner
    syntax. *)

module Record : sig
  type ('a, 'r) field = ('a, 'r) field_codec
  (** A field in a record of type ['r]. *)

  type 'r t = 'r record_codec
  (** A record codec. *)

  val field :
    string ->
    ?constraint_:bool expr ->
    'a typ ->
    get:('r -> 'a) ->
    set:('a -> 'r -> 'r) ->
    ('a, 'r) field
  (** [field name ?constraint_ typ ~get ~set] creates a field specification. *)

  val record : string -> default:'r -> ('a, 'r) field list -> 'r t
  (** [record name ~default fields] creates a record codec. *)

  val encode : 'r t -> ('r -> Bytesrw.Bytes.Slice.t) Staged.t
  (** [encode codec] returns a staged encoder for records. *)

  val decode : 'r t -> (Bytesrw.Bytes.Slice.t -> 'r) Staged.t
  (** [decode codec] returns a staged decoder for records. *)

  val encode_bytes : 'r t -> 'r encode_context option
  (** [encode_bytes codec] returns a context for encoding to bytes. *)

  val decode_bytes : 'r t -> (bytes -> int -> 'r) Staged.t
  (** [decode_bytes codec] returns a staged decoder from bytes. *)

  val to_struct : 'r t -> struct_
  (** [to_struct codec] converts to a struct for 3D generation. *)
end

(** {1 Direct Decoders}

    Zero-allocation decoders built from type specifications. These compile field
    readers at construction time for maximum decode performance. *)

val decode_make1 : 'a1 typ -> make:('a1 -> 'r) -> (bytes -> int -> 'r) Staged.t
(** Build decoder for 1-field record. *)

val decode_make2 :
  'a1 typ -> 'a2 typ -> make:('a1 -> 'a2 -> 'r) -> (bytes -> int -> 'r) Staged.t
(** Build decoder for 2-field record. *)

val decode_make3 :
  'a1 typ ->
  'a2 typ ->
  'a3 typ ->
  make:('a1 -> 'a2 -> 'a3 -> 'r) ->
  (bytes -> int -> 'r) Staged.t
(** Build decoder for 3-field record. *)

val decode_make4 :
  'a1 typ ->
  'a2 typ ->
  'a3 typ ->
  'a4 typ ->
  make:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'r) ->
  (bytes -> int -> 'r) Staged.t
(** Build decoder for 4-field record. *)

(** {2 Bounds-checked Decoders}

    Same as [decode_make*] but with bounds checking that raises {!Parse_error}.
*)

val decode_make1_exn :
  'a1 typ -> make:('a1 -> 'r) -> (bytes -> int -> 'r) Staged.t
(** Build bounds-checked decoder for 1-field record. *)

val decode_make2_exn :
  'a1 typ -> 'a2 typ -> make:('a1 -> 'a2 -> 'r) -> (bytes -> int -> 'r) Staged.t
(** Build bounds-checked decoder for 2-field record. *)

val decode_make3_exn :
  'a1 typ ->
  'a2 typ ->
  'a3 typ ->
  make:('a1 -> 'a2 -> 'a3 -> 'r) ->
  (bytes -> int -> 'r) Staged.t
(** Build bounds-checked decoder for 3-field record. *)

val decode_make4_exn :
  'a1 typ ->
  'a2 typ ->
  'a3 typ ->
  'a4 typ ->
  make:('a1 -> 'a2 -> 'a3 -> 'a4 -> 'r) ->
  (bytes -> int -> 'r) Staged.t
(** Build bounds-checked decoder for 4-field record. *)

(** {1 Typed Record Codec}

    Define typed record schemas for encoding and decoding fixed-size binary
    structs. Uses a Bunzli-style compositional API with closure chaining for
    zero intermediate allocations.

    {2 Example}

    {[
      type packet = { version : int; length : int }

      let codec =
        let open Wire.Codec in
        record "Packet" (fun version length -> { version; length })
        |+ field "version" uint8 (fun p -> p.version)
        |+ field "length" uint16be (fun p -> p.length)
        |> seal

      let decode = Wire.Codec.decode codec
      let encode = Wire.Codec.encode codec
      let struct_ = Wire.Codec.to_struct codec
    ]} *)

module Codec : sig
  type ('a, 'r) field
  (** A field specification for a value of type ['a] in a record of type ['r].
  *)

  type ('f, 'r) record
  (** A record codec under construction. ['f] is the remaining constructor
      arguments, ['r] is the final record type. *)

  type 'r t
  (** A sealed record codec for type ['r]. *)

  type view
  (** A validated buffer region for zero-copy field access. *)

  val record : string -> 'f -> ('f, _) record
  (** [record name make] starts building a codec named [name] with constructor
      [make]. *)

  val field : string -> 'a typ -> ('r -> 'a) -> ('a, 'r) field
  (** [field name typ get] defines a field with type [typ] and getter [get]. Use
      {!val:Wire.map} or {!val:Wire.bool} on the type for conversions.

      The returned field can be used both in the record pipeline ({!(|+)}) and
      as a zero-copy accessor ({!get}/{!set}) on a {!view}. *)

  val ( |+ ) : ('a -> 'b, 'r) record -> ('a, 'r) field -> ('b, 'r) record
  (** [r |+ f] adds field [f] to record codec [r]. *)

  val seal : ('r, 'r) record -> 'r t
  (** [seal r] finalizes the record codec, adding bounds checking. *)

  val wire_size : 'r t -> int
  (** [wire_size codec] returns the fixed wire size of the codec in bytes. *)

  val decode : 'r t -> bytes -> int -> 'r
  (** [decode codec buf off] decodes a record from [buf] at offset [off]. Raises
      {!Parse_error} if the buffer is too short. *)

  val encode : 'r t -> 'r -> bytes -> int -> unit
  (** [encode codec v buf off] encodes [v] into [buf] at offset [off]. *)

  val to_struct : 'r t -> struct_
  (** [to_struct codec] converts the codec to a struct for 3D generation. *)

  val view : 'r t -> bytes -> int -> view
  (** [view codec buf off] validates that [buf] has enough bytes at [off] for
      the codec's wire size and returns a view. Raises {!Parse_error} if the
      buffer is too short. *)

  val get : ('a, _) field -> view -> 'a
  (** [get f v] reads the field value from the view's buffer. Zero allocation
      for immediate types (int, bool). *)

  val set : ('a, _) field -> view -> 'a -> unit
  (** [set f v x] writes [x] into the view's buffer. For bitfields, uses
      read-modify-write to preserve adjacent bits. *)
end

(** {1 FFI Code Generation}

    Generate OCaml/C FFI stubs for roundtrip testing and interop with C parsers
    generated by EverParse. *)

val size_of_struct : struct_ -> int option
(** [size_of_struct s] computes the fixed wire size of struct [s] in bytes.
    Returns [None] if the struct contains variable-length fields. *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier for a
    struct name. EverParse 3D normalizes names that start with two or more
    consecutive uppercase letters by lowercasing the whole name and capitalizing
    only the first letter (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes
    [Tmframe]). Names with standard camelCase are preserved. *)

val ml_type_of : 'a typ -> string
(** [ml_type_of typ] returns the OCaml type name for a wire type (e.g., ["int"],
    ["int32"], ["int64"]). *)

val to_c_stubs : struct_ list -> string
(** [to_c_stubs structs] generates a C file with OCaml FFI stubs that call
    EverParse-generated validators. For each struct [Foo], generates:
    - Validation stub [caml_wire_foo_check] calling [Validate] directly

    The generated C code includes the EverParse headers and sources. Compile
    with [-I <schema_dir>] to find them. EverParse identifier normalization is
    handled automatically. *)

val to_ml_stubs : struct_ list -> string
(** [to_ml_stubs structs] generates OCaml [external] declarations matching the C
    stubs from {!to_c_stubs}. For each struct [Foo], generates:
    {[
      external foo_check : bytes -> bool = "caml_wire_foo_check"
    ]} *)

val to_ml_stub_name : struct_ -> string
(** [to_ml_stub_name s] returns the OCaml module name for the generated stub
    file (e.g., ["foo"] for struct [Foo]). *)

val to_ml_stub : struct_ -> string
(** [to_ml_stub s] generates a flat OCaml stub module for a single struct:
    {[
      type t = int * int * int32

      external read : string -> t option = "caml_wire_Foo_read"
      external write : t -> string option = "caml_wire_Foo_write"
    ]} *)

(** {1 Struct-level Read/Write}

    Parse and re-encode struct field values without requiring a typed record
    codec. Field values are retained in an opaque existential representation. *)

type parsed_struct
(** Opaque representation of parsed struct field values. *)

val read_struct : struct_ -> string -> (parsed_struct, parse_error) result
(** [read_struct s buf] parses struct [s] from [buf], retaining field values for
    re-encoding via {!write_struct}. *)

val write_struct : struct_ -> parsed_struct -> (string, parse_error) result
(** [write_struct s ps] encodes previously-parsed field values [ps] back to
    bytes using the schema of struct [s]. *)
