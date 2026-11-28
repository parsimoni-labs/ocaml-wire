(** Zero-copy record codecs for binary wire formats.

    {2 Access model}

    - {!decode} constructs a full record. It checks field constraints,
      [where]-clauses, and fires field actions, and allocates the record value.

    - {!validate} checks a record without constructing it and answers its error.
      Field [~action]s fire, so {!validate} and {!decode} agree on what is
      valid; nothing is written back, so the output parameters an action assigns
      are not observable.

    - {!View} checks a record once and reads and writes its fields in place. Use
      it for untrusted input: a field is read only from a record that passed, in
      the buffer it passed in. *)

type ('a, 'r) field
(** A field bound to a record projection. The binding retains the identity of
    its source {!Field.t}: an accessor may use another binding of that same
    source field, but not a newly declared field that merely has the same name.
    One source field may still be shared by multiple codecs. *)

type 'r t
(** A sealed record codec for type ['r]. *)

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
(** [f $ proj] binds a field to a record projection. *)

val field_name : ('a, 'r) field -> string
(** [field_name f] is the name of the source field of [f]. *)

val field_id : ('a, 'r) field -> int
(** [field_id f] is the {!Field.id} of the source field of [f]. *)

val field_typ : ('a, 'r) field -> 'a Types.typ
(** [field_typ f] is the type of the source field of [f]. *)

val field_proj : ('a, 'r) field -> 'r -> 'a
(** [field_proj f r] is the value of [f] in the record [r], by the projection
    [f] was bound with. *)

val v :
  string ->
  ?where:bool Types.expr ->
  ?doc:string ->
  'f ->
  ('f, 'r) fields ->
  'r t
(** [v name constructor fields] seals a codec from a field list. [?doc] attaches
    a free-text note (e.g. an RFC citation) that the documentation projection
    renders as a [/*++ ... --*/] comment on the codec's 3D typedef; see
    {!Everparse.project}.

    [v] checks the description and compiles nothing. The layout, readers,
    writers and validators are compiled by the first operation that needs them,
    on whichever domain calls it first: every function of this module that takes
    a codec except {!name}, {!rename}, {!doc}, {!pp}, {!to_struct} and {!env}.
    Embedding a codec with {!Wire.codec} compiles it. Domains that use a new
    codec at the same time may each compile it: the compile is pure, and one
    result is kept.

    Raises [Invalid_argument] if two fields have the same name, if distinct
    parameter handles referenced by the codec have the same name, if the field
    list has a shape EverParse 3D cannot validate (such as a field that consumes
    the rest of the buffer followed by more fields), or if a field constraint, a
    field action or [?where] refers to a field it cannot see: a constraint or an
    action sees the fields before it and its own, and [?where] sees them all.

    The first operation on the codec raises [Invalid_argument] for what only the
    compile finds, and raises it again on every later call: a size expression
    that refers to a field it cannot see, a bitfield after a variable-size
    field, a field type the codec has no reader or writer for, or fixed fields
    that add up to more than a buffer can hold. *)

val size : 'r t -> int option
(** [size c] is the number of bytes of every record of [c], or [None] if [c] is
    of variable size. *)

val min_size : 'r t -> int
(** [min_size c] is the fewest bytes a record of [c] takes: its size when [c] is
    of constant size. *)

val size_at : ?env:Param.env -> 'r t -> bytes -> int -> (int, Error.t) result
(** [size_at c buf off] is the size of the record at [off] in [buf], read from
    the sizes the buffer holds, or the error of bytes that hold none: the end of
    input where the buffer cuts a length field. It does not check that the
    record fits in [buf].

    Raises [Invalid_argument] on a negative [off], and when [c]'s field sizes
    are driven by an input param and [?env] is omitted, the same as {!decode}:
    an unbound param reads 0, which would silently measure a param-sized field
    as empty and report an extent shorter than the record. *)

val encoded_size : ?env:Param.env -> 'r t -> 'r -> int
(** [encoded_size c v] is the number of bytes [encode c v] writes. Computed from
    [v], not from a buffer: works for variable-size codecs whose tail is
    [all_bytes] / [rest_bytes] / [all_zeros], where the buffer-driven {!size_at}
    cannot distinguish "value's tail" from "remaining buffer space". Fixed-size
    byte arrays and slices contribute their declared region size, which
    {!encode} requires the value to match exactly.

    Raises [Invalid_argument] for a value {!encode} would refuse, such as a
    casetype value no case projects: there is no size to report for bytes that
    cannot be written. *)

val encoded_size_ctx : 'r t -> Types.eval_ctx -> 'r -> int
(** Context-threaded {!encoded_size}, for an embedded sub-codec whose parent
    already holds the evaluation context. Internal use. *)

val env : 'r t -> Param.env
(** [env c] creates a fresh parameter environment for codec [c], with all params
    initialised to 0. *)

val decode :
  ?consume:Types.consumption ->
  ?env:Param.env ->
  'r t ->
  bytes ->
  int ->
  ('r, Types.parse_error) result
(** [decode ?env c buf off] decodes a record from [buf] at offset [off].

    [consume] defaults to [`Prefix], accepting one record followed by more
    bytes. [`All] requires the record to end at [Bytes.length buf] and returns a
    {!Types.Trailing_bytes} error otherwise.

    If [?env] is supplied, input params are read from it and output params are
    written back to it after decoding.

    Raises [Invalid_argument] when [off] is negative, when [?env] was created
    for a different codec, when the codec has input params and no env is
    supplied, or when the env left any input param unbound (the error names the
    offending param). An unbound input param would resolve a parametric field
    size to 0 and silently truncate the field, so it is rejected up front the
    same way {!encode} does. *)

val encode : ?env:Param.env -> 'r t -> 'r -> bytes -> int -> unit
(** [encode ?env c r buf off] encodes record [r] into [buf] at offset [off].

    For codecs with input parameters (e.g. [byte_array ~size:(Param.expr p)]),
    the caller decides the width of each parametric field and tells the encoder
    via [?env]:

    {[
    open Wire

    let p_iv_len = Param.input "iv_len" uint8

    let codec =
      Codec.v "Hdr"
        (fun iv -> iv)
        Codec.
          [
            (Field.v "iv" (byte_array ~size:(Param.expr p_iv_len)) $ fun s -> s);
          ]

    let buf = Bytes.create 12
    let env = Codec.env codec |> Param.bind p_iv_len (UInt8.v 12)

    let () =
      Codec.encode ~env codec (String.make 12 'a') buf ~off:0;
      assert (Bytes.to_string buf = String.make 12 'a')
    ]}

    Each bound width must match the length of the corresponding field value in
    [record] -- the encoder cross-checks them. The same env shape works for
    {!decode} when reading the bytes back, but [encode] does not need a prior
    decode to obtain it.

    Raises [Invalid_argument] when [?env] was created for a different codec,
    when the codec has parameters and no env is supplied, when the env left any
    input param unbound (the error names the offending param), when the
    destination buffer is too short, or when a parametric byte field's value
    length does not match its env-bound size.

    Also raises [Invalid_argument] on a record {!decode} would reject: a closed
    enum field carrying an unlisted value, an [all_zeros] field carrying a
    non-zero byte, a [byte_array_where] byte failing its refinement, or a
    [where] clause or field [~constraint_] that does not hold for the values
    given. Encode never emits bytes its own decoder refuses. Field [~action]s
    are not run by encode.

    Encode is not all-or-nothing: it writes the record field by field and checks
    the result, so after any of those raises the bytes from [off] on hold a
    partial record and must be treated as scrap. Only the single-field {!set}
    rolls its write back. *)

val to_bytes : ?env:Param.env -> 'r t -> 'r -> bytes
(** [to_bytes ?env c r] encodes [r] into freshly allocated bytes, then runs the
    same validation pass as {!validate}. In particular, field [~action]s fire,
    so this one-call whole-record operation cannot return bytes that {!decode}
    would reject. Action assignments are not written back to [?env].

    Raises [Invalid_argument] under the same conditions as {!encode}, and when a
    field action rejects the encoded record. Because the buffer is fresh, no
    partially encoded buffer is returned when either pass raises. *)

val to_string : ?env:Param.env -> 'r t -> 'r -> string
(** [to_string ?env c r] is the string counterpart of {!to_bytes}. The string is
    backed by the fresh encoding buffer, which is safe because no mutable alias
    to that buffer escapes. *)

val to_struct : 'r t -> Types.struct_
(** Project to a {!Types.struct_} declaration. *)

val validate :
  ?env:Param.env -> 'r t -> bytes -> int -> (unit, Types.parse_error) result
(** [validate ?env c buf off] checks field [~constraint_] and [~where] clauses
    without constructing a record, and answers the record's error as a value.
    Field [~action]s do fire, so a rejecting action fails validation, but
    assigned output parameters are not written back to [?env]. [?env] supplies
    bindings for any [Param.input] referenced in those clauses.

    Raises [Invalid_argument] if [off] is negative or if [?env] belongs to
    another codec or leaves an input parameter unbound. *)

val get :
  ?env:Param.env ->
  'r t ->
  ('a, 'r) field ->
  bytes ->
  int ->
  ('a, Error.t) result
(** [get ?env c f buf off] is the field [f] of the record at [off] in [buf],
    read in place with no check of the record, or the error of bytes that make
    no value of [f]. If [f] has an action, it fires on every read. The reader of
    {!Gen.with_field_reader}. *)

(** {2:defs Definitions, layers and paths} *)

(** The definitions of the public API: a field bound to a record
    ({!constructor-Def}), or the field of the record a layer holds
    ({!constructor-Path}). *)
type ('a, 'r) def =
  | Def : ('a, 'r) field -> ('a, 'r) def
  | Path : ('s, 'r) layer * ('a, 's) def -> ('a option, 'r) def

(** The fields that hold a record: present ({!constructor-Layer}), optional
    ({!constructor-Opt_layer}), or held by the record of another layer
    ({!constructor-Under}). *)
and ('s, 'r) layer =
  | Layer : ('s, 'r) field -> ('s, 'r) layer
  | Opt_layer : ('s option, 'r) field -> ('s, 'r) layer
  | Under : ('s, 'r) layer * ('t, 's) layer -> ('t, 'r) layer

val layer : ('s, 'r) def -> ('s, 'r) layer
(** [layer f] is [f] as a layer. Raises [Invalid_argument] unless [f] holds a
    record. *)

val opt_layer : ('s option, 'r) def -> ('s, 'r) layer
(** [opt_layer f] is {!layer} for a {!Field.optional} [f]. *)

val path : ('s, 'r) layer -> ('a, 's) def -> ('a option, 'r) def
(** [path l f] is the path to [f] under [l]. *)

val under : ('s, 'r) layer -> ('t, 's) layer -> ('t, 'r) layer
(** [under l l'] is the layer [l'] under [l]. *)

(** The definitions given in order to a function ending in ['k]. *)
module Defs : sig
  type ('f, 'k, 'r) t =
    | [] : ('k, 'k, 'r) t
    | ( :: ) : ('a, 'r) def * ('f, 'k, 'r) t -> ('a -> 'f, 'k, 'r) t
end

val fields_of_defs : ('f, 'r, 'r) Defs.t -> ('f, 'r) fields
(** [fields_of_defs ds] is the fields of {!v}. Raises [Invalid_argument] on a
    path. *)

(** {2:views Checked views} *)

(** Checked views: the implementation of [Wire.Codec.View]. A check and a view
    are the offset of the record, a refused check [-1]. *)
module View : sig
  type 'r codec := 'r t
  type ('b, 'r) check = int
  type ('b, 'r) t = int

  val check :
    ?env:Param.env ->
    ?len:int ->
    'r codec ->
    'b Buf.t ->
    off:int ->
    ('b, 'r) check
  (** [check c b ~off] is [off] if the record of [c] at [off] passes {!validate}
      in the [len] bytes there, and [-1] otherwise. Raises [Invalid_argument]
      only on [env]. *)

  val is_ok : ('b, 'r) check -> bool
  (** [is_ok k] is [k >= 0]. *)

  val get : ('b, 'r) check -> ('b, 'r) t
  (** [get k] is [k]. Raises [Invalid_argument] if [k] is refused. *)

  type ('a, 'r) rfield
  (** The type for fields resolved in a codec. *)

  val field : 'r codec -> ('a, 'r) def -> ('a, 'r) rfield
  (** [field c f] is [f] resolved in [c]. Raises [Invalid_argument] if [f] is
      not one of [c]'s, and for a field an input parameter of [c] places. *)

  val read : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) rfield -> 'a
  (** [read b v f] is the field [f] of the record [v] of [b], or its zero where
      it lies outside [b]. *)

  val set : 'b Buf.t -> ('b, 'r) t -> ('a, 'r) rfield -> 'a -> unit
  (** [set b v f x] writes [x] as the field [f] of the record [v] of [b]. *)

  val offset : ('b, 'r) t -> int
  (** [offset v] is where the record of the view [v] starts in its buffer. *)

  type ('s, 'r) sub_layer
  (** The type for layers resolved in a codec. *)

  val layer : 'r codec -> ('s, 'r) layer -> ('s, 'r) sub_layer
  (** [layer c l] is [l] resolved in [c]. *)

  val sub : 'b Buf.t -> ('b, 'r) t -> ('s, 'r) sub_layer -> ('b, 's) check
  (** [sub b v l] is the check of the record [l] holds in the record [v]. *)

  val fold :
    ?env:Param.env ->
    ?stride:int ->
    ?offs:int array ->
    ?lens:int array ->
    ?invalid:('acc -> 'acc) ->
    'r codec ->
    ('f, 'acc, 'r) Defs.t ->
    bytes ->
    init:'acc ->
    f:('acc -> 'f) ->
    ('acc, Error.t * 'acc) result
  (** [fold c fields buf ~init ~f] is [Wire.Codec.View.fold]. *)
end

val zeroterm_nul_pos : bytes -> first:int -> limit:int -> int
(** [zeroterm_nul_pos buf ~first ~limit] is the index of the first NUL byte in
    [\[first, limit)], stopping at a parse error on an unterminated run.
    Internal use (shared with the streaming decoders). *)

val raw_decode : 'r t -> bytes -> int -> 'r
(** [raw_decode c buf off] decodes without validation. Internal use. *)

val raw_encode : ?env:Param.env -> 'r t -> 'r -> bytes -> int -> int
(** [raw_encode ?env c r buf off] encodes a record without [where]-clause
    validation and returns the offset after the written bytes. Same env
    semantics as {!encode}. Internal use. *)

val embed_encode : 'r t -> 'r -> bytes -> int -> int
(** Encode a parameter-free sub-codec embedded as a field/element, skipping the
    env check {!encode} performs. Internal use. *)

val embed_encode_ctx : 'r t -> 'r -> Types.eval_ctx -> bytes -> int -> int
(** Context-threaded embedded encode. Internal use. *)

val embed_decode : 'r t -> bytes -> int -> 'r
(** Decode a sub-codec embedded as a field/element, enforcing its [where] and
    field constraints against param values seeded by the enclosing codec.
    Internal use. *)

val embed_decode_ctx :
  'r t -> Types.eval_ctx -> Input_end.t -> bytes -> int -> 'r
(** Context-threaded embedded decode. Under a context made by
    {!Types.checked_eval_ctx} it only builds the record, the enclosing check
    pass having checked it; otherwise it checks the record too, as
    {!embed_decode} does. Internal use. *)

val embed_validate_ctx :
  'r t -> Types.eval_ctx -> Input_end.t -> bytes -> int -> unit
(** [embed_validate_ctx c ctx input_end buf off] runs everything
    {!embed_decode_ctx} checks and builds nothing: the bounds check, the field
    constraints, the [where] clauses, the enum and span refinements and the
    field actions of the sub-codec at [off]. This is what an embedded codec's
    use site runs, mirroring the call to the nested struct's validator the
    generated C makes. Internal use. *)

val embed_validate_sized_ctx :
  'r t -> Types.eval_ctx -> Input_end.t -> bytes -> int -> int -> unit
(** [embed_validate_sized_ctx c ctx input_end buf off end_off] is
    {!embed_validate_ctx} for a sub-codec whose end [end_off] the caller has
    just measured over the same bytes: the bounds check takes that end instead
    of measuring the record again. Internal use. *)

val embed_check_end : 'r t -> Input_end.t -> bytes -> int -> int
(** [embed_check_end c input_end buf off] is where the record at [off] ends when
    its fast checks accept it, else [-1], or [-2] when it has none: an end it
    gives is the one the codec's extent measures, and
    {!embed_validate_sized_ctx} accepts the record with it. Internal use. *)

val embed_walk : 'r t -> 'r Types.walk option
(** [embed_walk c] is the walk of [c], the decode that checks and reads each
    field once and the check that makes the same checks, or [None] when [c] has
    none. Internal use. *)

val embed_ref : 'r t -> 'r Types.codec_ref
(** [embed_ref t] is [t] as the codec of a sub-codec field. *)

type 'r components = {
  find : 'a. ('a, 'r) field -> (bytes -> int -> ('a, Error.t) result) option;
}
(** The readers of the fields of an {!opaque} codec: [find f] reads the field
    [f] of the record at an offset, as {!get} does, or is [None] when [f] is not
    a field of the codec. Internal use. *)

val opaque :
  name:string ->
  Types.field ->
  min_size:int ->
  check:(Input_end.t -> bytes -> int -> unit) ->
  read:(Input_end.t -> bytes -> int -> 'r) ->
  extent:(Input_end.t -> bytes -> int -> int) ->
  write:('r -> bytes -> int -> int) ->
  size_of_value:('r -> int) ->
  components:'r components ->
  'r t
(** [opaque ~name f ~min_size ~check ~read ~extent ~write ~size_of_value
     ~components] is the codec [name] whose bytes another module reads, such as
    {!Asn1}'s DER: [f] stands for the whole record, [check] raises the parse
    error of a record that does not hold before its [Input_end.t], [read] reads
    a record [check] accepted, [extent] is where the record at an offset ends,
    [write] writes a value and returns where it ends, raising [Invalid_argument]
    on a value [check] would refuse, [size_of_value] is how many bytes [write]
    writes, and [components] reads its fields for {!get} and {!View.field}.
    Internal use. *)

val wire_size_info : 'r t -> [ `Fixed of int | `Variable of bytes -> int -> int ]
(** Wire size information for embedding. *)

val wire_size_info_ctx :
  'r t ->
  [ `Fixed of int
  | `Variable of Types.eval_ctx -> Input_end.t -> bytes -> int -> int ]
(** Context-threaded wire size information for embedding. Internal use. *)

val name : 'r t -> string
(** [name c] returns the codec's name. *)

val id : 'r t -> int
(** [id c] is the identity of [c], which {!rename} keeps. Internal use. *)

val rename : string -> 'r t -> 'r t
(** [rename n c] is [c] with its name set to [n]. The name is metadata: it only
    determines the generated 3D struct name (via
    {!Everparse.Raw.struct_of_codec}), not the wire encoding, so renaming leaves
    encode/decode and all field constraints unchanged. Use it to give a
    generically built codec a unique, meaningful name for projection or code
    generation. *)

val doc : 'r t -> string option
(** [doc c] is the note attached via {!v}'s [?doc], if any. *)

val field_readers :
  'r t -> (string * (bytes -> int -> (int, Types.parse_error) result)) list
(** [field_readers c] returns the int-valued field readers of [c], indexed by
    field name. Used for cross-codec name resolution when [c] is embedded as a
    sub-codec via {!Wire.codec}. *)

val field_readers_ctx :
  'r t -> (string * (Types.eval_ctx -> Input_end.t -> bytes -> int -> int)) list
(** Context-threaded field readers for embedding. Internal use. *)

val pp : Format.formatter -> 'r t -> unit
(** Pretty-print a codec (shows its name). *)

val pp_value : 'r t -> 'r Fmt.t
(** [pp_value c] prints a record of [c] field by field, each integer field with
    its value as {!field_readers} reads it from the record's encoding. *)

type validator
(** A struct validator without a constructor. The same int-array validation
    kernel that backs {!validate} on a [Codec.t], but built directly from a
    {!Types.struct_}. Used by [Wire.of_string] /[Wire.decode] for [Struct] types
    so all struct validation goes through the same code path. *)

val validator_of_struct : Types.struct_ -> validator
(** [validator_of_struct s] compiles [s] into a validator. Constraints, [where]
    clauses, and per-field actions are compiled to operate on a per-decode int
    array; the resulting validator is reusable. *)

val validate_struct :
  validator -> bytes -> int -> (unit, Types.parse_error) result
(** [validate_struct v buf off] runs the validator, answering its error as a
    value. *)

val struct_size_of : validator -> bytes -> int -> int
(** [struct_size_of v buf off] is the byte size of the struct starting at [off].
    Independent of [buf] for fixed-size structs; inspects the buffer otherwise.
*)

val struct_min_size : validator -> int
(** Minimum byte size accepted. *)

val wire_size_info_of_validator :
  validator -> [ `Fixed of int | `Variable of bytes -> int -> int ]
(** Wire-size info (parallels {!wire_size_info} for codecs). *)

val blit_string_exact : int -> bytes -> int -> string -> int
(** [blit_string_exact n buf off s] writes [s] into [buf] at [off] and returns
    [off + n]. Raises [Invalid_argument] unless [String.length s = n]: a
    fixed-size byte field is exact. Shared helper for both record and top-level
    encoders. *)

val blit_slice_exact : int -> bytes -> int -> Bytesrw.Bytes.Slice.t -> int
(** [blit_slice_exact n buf off src] is like {!blit_string_exact} but copies
    from a {!Bytesrw.Bytes.Slice.t}. Top-level [encode_direct] uses it for
    [byte_slice] fields. *)

(**/**)

(** Support for code generated from a codec. *)
module Gen : sig
  val to_struct : 'r t -> Types.struct_
  (** [to_struct t] is the description [t] is built from. *)

  val digest : 'r t -> string
  (** [digest t] names the shape of [t]: its fields, their types and
      constraints, every expression in full, and the version of what generated
      code assumes of it. *)

  val check : 'r t -> string -> unit
  (** [check t d] raises [Invalid_argument] unless [digest t = d]. *)

  type 'b field_k = {
    k : 'a. 'a Types.typ -> (bytes -> int -> ('a, Error.t) result) -> 'b;
  }
  (** A use of a field's reader, whatever the field's type. *)

  val with_field_reader : 'r t -> string -> 'b field_k -> 'b option
  (** [with_field_reader t name k] is [k.k typ r], where [r] is the reader
      {!get} makes for the field [name] of [t] and [typ] its type, or [None]
      when [t] has no field [name]. *)

  (** {2 Maps} *)

  type ('w, 'a) map = {
    dec : at:int -> 'w -> 'a;
    get : at:int -> field:string -> 'w -> ('a, Error.t) result;
    enc : 'a -> 'w;
  }
  (** A map's decoder and encoder over its raw wire value. [dec] is for a value
      the record's check accepted; [get] answers the error of any other, at [at]
      and under the name it is given. *)

  val map_int : 'a Types.typ -> (int, 'a) map
  (** [map_int t] is the map [t] over an integer up to 16 bits wide or a
      bitfield, read as an [int]. Raises [Invalid_argument] for any other [t].
  *)

  val map_int32 : 'a Types.typ -> (int32, 'a) map
  (** [map_int32 t] is the map [t] over a 32-bit integer. *)

  val map_id : 'a Types.typ -> int option
  (** [map_id t] identifies the map [t] among every map built, [None] when [t]
      is not a map. *)

  val check_map : 'r t -> string -> 'a Types.typ -> unit
  (** [check_map c name t] raises [Invalid_argument] unless the field [name] of
      [c] is the very value [t]. *)

  val scan : 'r t -> bytes -> int -> int -> int64 array option
  (** [scan c buf off len] is the outputs of the record in the [len] bytes at
      [off], which the codec accepts on their own, or [None]: each named scalar
      field, and for each optional field its presence as [1L] or [0L], in
      declaration order, an absent field [0L]. *)

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

(** Test-only views of how a codec is compiled. *)
module Testing : sig
  val general : 'r t -> 'r t
  (** [general t] is [t] compiled with every fast path off: {!val-decode},
      {!val-validate}, {!View.val-check} and {!val-encode} take their general
      path, which a fast path must agree with. *)

  val fresh : 'r t -> 'r t
  (** [fresh t] is [t] not yet compiled, so its next operation is its first. *)

  val paths : 'r t -> string list
  (** [paths t] names the fast paths compiling [t] selected, from {!all_paths}.
  *)

  val staged_intervals : 'r t -> bytes -> int -> (bool * bool) option
  (** [staged_intervals t buf off] is the answer of the staged word tests of the
      check pass of [t] on the record at [off] in [buf], paired with the answer
      of the interval scan they stand for. [None] when the check pass of [t] has
      no staged word tests or [buf] does not hold the record's frame at [off].
  *)

  val all_paths : string list
  (** Every name {!paths} can return. *)
end
