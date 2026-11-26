(** Zero-copy record codecs for binary wire formats.

    {2 Access model}

    - {!decode} constructs a full record. It checks field constraints,
      [where]-clauses, and fires field actions, and allocates the record value.

    - {!validate} checks a record without constructing it and answers its error.
      Field [~action]s fire, so {!validate} and {!decode} agree on what is
      valid; nothing is written back, so the output parameters an action assigns
      are not observable.

    - {!View} checks a record once and reads its fields from the result. Use it
      for untrusted input: a field is read only from a record that passed, in
      the buffer it passed in.

    - {!Word} checks that a short fixed header lies in the buffer and reads it
      as one word, with no other check. It is faster than {!View} on records
      shorter than 8 bytes.

    - {!get} and {!set} read and write one field in place, for bytes the program
      wrote itself. {!get} fires the field's [~action] (if any). Neither checks
      record-level [~where] clauses or other fields' constraints; {!set} refuses
      a value its own field cannot represent. *)

type ('a, 'r) field
(** A field bound to a record projection. The binding retains the identity of
    its source {!Field.t}: an accessor may use another binding of that same
    source field, but not a newly declared field that merely has the same name.
    One source field may still be shared by multiple codecs. *)

type 'r t
(** A sealed record codec for type ['r]. *)

(** The fields a {!writer} takes, in the order of its arguments. *)
type ('f, 'r) args =
  | [] : (unit, 'r) args
  | ( :: ) : ('a, 'r) field * ('f, 'r) args -> ('a -> 'f, 'r) args

type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

val ( $ ) : 'a Field.t -> ('r -> 'a) -> ('a, 'r) field
(** [f $ proj] binds a field to a record projection. *)

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

val wire_size_opt : 'r t -> int option
(** Fixed wire size in bytes, or [None] if the codec is variable-length. *)

val wire_size : 'r t -> int
(** Fixed wire size in bytes.

    Raises [Invalid_argument] if the codec is variable-length. *)

val min_wire_size : 'r t -> int
(** Minimum wire size in bytes (for variable-length codecs). *)

val wire_size_at :
  ?env:Param.env -> 'r t -> bytes -> int -> (int, Error.t) result
(** [wire_size_at c buf off] is the size of the record at [off] in [buf], read
    from the sizes the buffer holds, or the error of bytes that hold none: the
    end of input where the buffer cuts a length field. It does not check that
    the record fits in [buf].

    Raises [Invalid_argument] on a negative [off], and when [c]'s field sizes
    are driven by an input param and [?env] is omitted, the same as {!decode}:
    an unbound param reads 0, which would silently measure a param-sized field
    as empty and report an extent shorter than the record. *)

val size_of_value : ?env:Param.env -> 'r t -> 'r -> int
(** [size_of_value c v] returns the number of bytes that [encode c v] will
    write. Computed from [v], not from a buffer: works for variable-size codecs
    whose tail is [all_bytes] / [rest_bytes] / [all_zeros], where the
    buffer-driven {!wire_size_at} cannot distinguish "value's tail" from
    "remaining buffer space". Fixed-size byte arrays and slices contribute their
    declared region size, which {!encode} requires the value to match exactly.

    Raises [Invalid_argument] for a value {!encode} would refuse, such as a
    casetype value no case projects: there is no size to report for bytes that
    cannot be written. *)

val size_of_value_ctx : 'r t -> Types.eval_ctx -> 'r -> int
(** Context-threaded {!size_of_value}, for an embedded sub-codec whose parent
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

val decoder : 'r t -> (bytes -> int -> ('r, Types.parse_error) result) Staged.t
(** [decoder c] is [decode c], staged: [c] is compiled when the stage is built,
    and each call runs its decoder with no other dispatch. Unstage it once and
    call the result per record. It raises what {!decode} raises. *)

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
      Codec.encode ~env codec (String.make 12 'a') buf 0;
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

val encoder : 'r t -> ('r -> bytes -> int -> unit) Staged.t
(** [encoder c] is [encode c], staged as {!decoder} is. It raises what {!encode}
    raises. *)

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
  (bytes -> int -> ('a, Error.t) result) Staged.t
(** [get ?env c f] is a staged zero-copy getter for field [f] in codec [c]. If
    [f] has an action, it fires on every read. [env] syncs output parameters
    after each action and supplies parameters for dependent field layouts; omit
    it for parameter-free accessors. Raises [Invalid_argument] when the codec
    has input params and [env] is omitted, or when it was created for another
    codec, or when [f] was not bound from a source {!Field.t} used in [c]: an
    unbound param reads 0, which would stage the reader onto the bytes in front
    of the field, while a same-named lookalike could interpret those bytes with
    a different wire type. The reader answers the error of bytes that make no
    value of [f], under [f]'s name, and raises [Invalid_argument] on a negative
    offset. Does not check record-level where-clauses or other fields'
    constraints. On untrusted input use {!View}. *)

val writer : 'r t -> 'r -> ('f, 'r) args -> (bytes -> int -> 'f) Staged.t
(** [writer c template fields] is a staged writer of a record of [c]: called
    with a buffer, an offset and one value per field of [fields], it writes
    [template] at the offset with each of those fields set to its value, as
    {!encode} of [template] then {!set} of each field would. A header that
    differs from a template in a few fields is one call.

    A record of at most 7 bytes whose fields in [fields] are big-endian integers
    or bitfields of a byte or a big-endian 16- or 32-bit word is put together in
    an [int] and written in a few stores, after every check.

    Raises [Invalid_argument] when [c] is not of a fixed size or has input
    params, when a field is not one of [c]'s or comes twice, or as {!encode}
    raises on [template]. The writer raises [Invalid_argument] when the buffer
    does not hold the record at the offset, and as {!set} raises on a value its
    field cannot represent; the bytes from the offset on are then scrap, as
    after a failed {!encode}. *)

(** {2:views Checked views} *)

(** Checked views.

    A view is a record that passed every check of its codec in a buffer. Its
    fields are then read with no check of their own, as with the [View] of a
    module [Wire_ocaml] generates. The buffer is named by a type
    ({!Wire_runtime.Buf}) and so is the codec ({!View.val-codec}): a view is
    read only in the buffer it was checked in and only by the fields of its
    codec.

    A check result and a view are immediate, so {!View.val-check}, {!View.ok}
    and {!View.get} allocate nothing. {!View.read} allocates only what the
    field's value needs: nothing for an integer, a {!Bytesrw.Bytes.Slice.t} for
    a byte slice.

    {[
    open Wire

    type h = { version : int; ttl : UInt8.t }

    let version =
      Field.v "version"
        ~self_constraint:Expr.(fun v -> v = int 4)
        (bits ~width:4 U8)

    let ttl = Codec.(Field.v "ttl" uint8 $ fun h -> h.ttl)

    let codec =
      Codec.v "H"
        (fun version ttl -> { version; ttl })
        Codec.[ (version $ fun h -> h.version); ttl ]

    module H = (val Codec.View.codec codec)

    let h_ttl = Codec.View.field H.codec ttl

    let ttl_of bytes =
      let (Buf.Any b) = Buf.v bytes in
      let k = Codec.View.check H.codec b (Buf.length b) 0 in
      if Codec.View.ok k then Some (Codec.View.read h_ttl b (Codec.View.get k))
      else None

    let () =
      assert (ttl_of (Bytes.of_string "\x40\x07") = Some (UInt8.v 7));
      assert (ttl_of (Bytes.of_string "\x50\x07") = None);
      assert (ttl_of (Bytes.of_string "\x40") = None)
    ]} *)
module View : sig
  type 'r record_codec := 'r t
  type ('a, 'r) record_field := ('a, 'r) field

  (** {1:codecs Naming a codec} *)

  type ('c, 'r) codec
  (** The type for a codec of ['r] named ['c]. *)

  (** The type for modules that name a codec. *)
  module type CODEC = sig
    type c
    (** The name of {!val-codec}. No other codec has it. *)

    type r
    (** The type for the records of {!val-codec}. *)

    val codec : (c, r) codec
    (** [codec] is the named codec. *)
  end

  val codec :
    ?env:Param.env -> 'r record_codec -> (module CODEC with type r = 'r)
  (** [codec ~env c] is [c], compiled, under a name of its own. [env] supplies
      the input parameters of [c] as for {!Codec.get}; their values are read at
      each check and each read. Unpack it once, where the fields are made, as
      [module H = (val Codec.View.codec c)].

      Each unpacking is a new name, so two unpackings of one codec share neither
      fields nor views. In a function polymorphic in the record type, the type
      must be named,
      [let f (type r) (c : r Codec.t) = let module H = (val Codec.View.codec c)
       in ...], and a field or a view of [H] cannot be returned from [f]. A
      functor that unpacks in its body takes a final [()] argument: OCaml
      refuses a new type in the body of an applicative functor.

      Raises [Invalid_argument] if [env] was made for another codec or leaves an
      input parameter of [c] unbound, if [c] has input parameters and [env] is
      omitted, and as the first operation on [c] does (see {!Codec.v}). *)

  (** {1:checks Checking} *)

  type ('b, 'c) check [@@immediate]
  (** The type for the results of checking a record of the codec named ['c] in
      the buffer named ['b]. *)

  val check :
    ('c, 'r) codec ->
    'b Wire_runtime.Buf.t ->
    'b Wire_runtime.Buf.length ->
    int ->
    ('b, 'c) check
  (** [check v b len off] is the result of checking the record of [v] at [off]
      in [b]. The record is accepted iff {!Codec.validate} accepts it: the same
      frame test, field constraints, [~where] clauses, enums and field actions
      run. [len] must be [Buf.length b]; it is an argument so that a loop over
      the records of [b] reads it once.

      [check] never raises on [b] or [off]: a negative [off] or a record that
      does not fit is refused. The error of a refused record is
      [Codec.validate c (b :> bytes) off]. *)

  val ok : ('b, 'c) check -> bool
  (** [ok k] is [true] iff [k] accepted its record. *)

  (** {1:views Views} *)

  type ('b, 'c) t [@@immediate]
  (** The type for views: records of the codec named ['c] that passed their
      check in the buffer named ['b]. *)

  val get : ('b, 'c) check -> ('b, 'c) t
  (** [get k] is the view of the record [k] accepted.

      Raises [Invalid_argument] if [ok k] is [false]. Test {!ok} first: a
      refused record is input, not misuse. *)

  (** {1:fields Fields} *)

  type ('a, 'c) field
  (** The type for fields of type ['a] in the views of the codec named ['c]. *)

  val field : ('c, 'r) codec -> ('a, 'r) record_field -> ('a, 'c) field
  (** [field v f] is the field [f] in the views of [v], read with no check. Make
      it once per field, not once per record.

      An integer, a bitfield or a byte slice or string of constant size, at a
      constant offset inside the frame of [v], without an action and under no
      map, is read with no bounds check; a byte slice of size [0] is
      {!Bytesrw.Bytes.Slice.eod}. Under an {!Wire.val-enum} or a
      {!Wire.val-where} it reads the integer there, which the check accepted.

      Raises [Invalid_argument] if [f] was not bound from a {!Field.t} of the
      codec of [v], or if it is any other field (after a variable-size field,
      with an action, under a map): read that one with {!checked_field}. *)

  val checked_field :
    ('c, 'r) codec -> ('a, 'r) record_field -> (('a, Error.t) result, 'c) field
  (** [checked_field v f] is the field [f] in the views of [v], read as
      {!Codec.get} reads it: with its bounds check, its value's checks and its
      action, which has already run once in {!val-check}. Its read answers the
      error of bytes written after the check that no longer make a value of [f],
      such as a field they move out of the buffer.

      Raises [Invalid_argument] if [f] was not bound from a {!Field.t} of the
      codec of [v], or as {!Codec.get} does. *)

  val read : ('a, 'c) field -> 'b Wire_runtime.Buf.t -> ('b, 'c) t -> 'a
  (** [read f b r] is the value of [f] in the record [r] of [b]. It never raises
      and never reads outside [b].

      {b Warning.} If bytes of [b] are written after the check, the value may be
      one the codec's checks refuse. A field of {!field} lies in the frame the
      check tested, where no bytes move it; a field of {!checked_field} answers
      the error of bytes that move it out of [b]. The accessors of the views a
      [Wire_ocaml] module writes, which answer no error, read such a field as
      absent: [0] for an integer, [false] for a presence, an empty slice or
      string for bytes. *)

  (** {1:subs Sub-records} *)

  type ('cs, 'c) sub
  (** The type for the fields of the codec named ['c] that hold a record of the
      codec named ['cs]. *)

  val sub :
    ('c, 'r) codec -> ('cs, 's) codec -> ('s, 'r) record_field -> ('cs, 'c) sub
  (** [sub v w f] is the field [f] of [v] as a record of [w]: a field
      {!Wire.codec} of the codec of [w], a {!Wire.nested} or
      {!Wire.nested_at_most} of it, or a {!Field.optional_or} of those. Make it
      once per field, not once per record.

      Raises [Invalid_argument] if [f] was not bound from a {!Field.t} of the
      codec of [v], if it does not hold the codec of [w], or if the codec of [w]
      has input parameters. *)

  val sub_option :
    ('c, 'r) codec ->
    ('cs, 's) codec ->
    ('s option, 'r) record_field ->
    ('cs, 'c) sub
  (** [sub_option v w f] is {!sub} for a {!Field.optional} field [f]. *)

  val enter :
    ('cs, 'c) sub ->
    'b Wire_runtime.Buf.t ->
    'b Wire_runtime.Buf.length ->
    ('b, 'c) t ->
    ('b, 'cs) check
  (** [enter s b len r] is the result of checking the record [s] holds in the
      view [r]. The check that gave [r] already made every check of that record,
      so [enter] makes none of them again: it accepts the record iff it is
      present and its frame lies in [b]. [len] must be [Buf.length b].

      [enter] never raises on [b]: a record whose bytes were written after the
      check is refused when they move the sub-record out of [b]. *)
end

val set :
  ?env:Param.env ->
  'r t ->
  ('a, 'r) field ->
  (bytes -> int -> 'a -> unit) Staged.t
(** Staged zero-copy field setter. [env] supplies the input params a dependent
    field layout is measured from; omit it for parameter-free codecs. Raises
    [Invalid_argument] when the codec has input params and [env] is omitted, or
    when it was created for another codec, or when the field binding does not
    share its source {!Field.t} with the field in the codec: an unbound param
    reads 0, which would put the write on a field the caller never named and
    leave the one it did name unchanged.

    Raises [Invalid_argument], leaving the buffer untouched, on a value the
    field cannot represent: a byte string whose length differs from the declared
    size, an integer outside the range of its width (fixed or [bits ~width] or
    [uint size] alike), a byte a [byte_array_where] refinement rejects, or a
    sub-codec value its own [where] clause or field [~constraint_] rejects. Each
    of those would otherwise put different bytes on the wire than the caller
    asked for, and a masked integer is indistinguishable from a value meant that
    way. The buffer is restored even for a compound field the writer had to
    part-write before the failure showed up.

    Does not check what the surrounding record permits -- record-level [~where]
    clauses, other fields' [~constraint_] checks, [enum] membership -- and does
    not fire actions. Call {!validate} after a batch of writes to verify those
    still hold. *)

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

val field_ref : ('a, 'r) field -> int Types.expr
(** Expression referencing a field by name. *)

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

type bitfield
(** A bitfield accessor -- shift and mask for one field in a packed word.

    The word is read with a bounds check and no other check of the record; on
    untrusted input use {!View}. These stay for unchecked reads of several
    bitfields of one word of a record shorter than 8 bytes, such as the 4-byte
    words of [bench/clcw]: a view reads such a field out of line, where
    {!extract} is a shift and a mask. *)

val bitfield : 'r t -> (int, 'r) field -> bitfield
(** [bitfield codec field] returns a bitfield accessor. Raises if [field] is not
    a bitfield or was not bound from the source {!Field.t} used in [codec]. *)

val load_word :
  bitfield -> (bytes -> int -> (Optint.t, Error.t) result) Staged.t
(** [load_word bf] returns a staged word reader. Force once, then call the
    resulting function to read the packed base word. Fields sharing the same
    base word return readers that read the same bytes -- call once and pass the
    result to multiple {!extract} calls. The word is an [Optint.t] so a 32-bit
    base survives a platform whose int is narrower: an unboxed native int on a
    64-bit host.

    The reader answers the error {!get} answers for the field of [bf] where the
    buffer cuts the word, and raises [Invalid_argument] on a negative offset,
    which is misuse. *)

val extract : bitfield -> Optint.t -> int
(** [extract bf word] extracts the field from a pre-loaded word value. Pure
    shift+mask, no memory access. *)

(** Records of at most 7 bytes read as one word.

    A check of a record of a fixed size that fits an [int] with a bit to spare
    reads its bytes, big-endian, into one [int], with one bounds test and, where
    the buffer holds 8 bytes from the record on, one load; each field is then
    two shifts and a mask. A check tests no constraint of the record: on
    untrusted input use {!View}. A word check stays for short fixed headers,
    such as the 6-byte space packet and frame headers of [bench/routing] and
    [bench/gateway]: a view reads a field of a frame shorter than 8 bytes out of
    line, where a word is read once and each field taken from it inline.

    {[
    open Wire

    type hdr = { kind : int; len : UInt16.t }

    let kind = Codec.(Field.v "kind" (bits ~width:8 U8) $ fun h -> h.kind)
    let len = Codec.(Field.v "len" uint16be $ fun h -> h.len)

    let codec =
      Codec.v "Hdr" (fun kind len -> { kind; len }) Codec.[ kind; len ]

    let reader = Codec.Word.reader codec
    let w_len = Codec.Word.field codec len

    let len_of buf off =
      let k = Codec.Word.check reader buf off in
      if Codec.Word.ok k then Ok (Codec.Word.read w_len (Codec.Word.get k))
      else Error (Codec.Word.error reader buf off)

    let () =
      assert (len_of (Bytes.of_string "\x05\x00\x0c") 0 = Ok (UInt16.v 12));
      assert (Result.is_error (len_of (Bytes.of_string "\x05") 0))
    ]} *)
module Word : sig
  type 'r codec := 'r t
  type ('a, 'r) codec_field := ('a, 'r) field

  type 'r t = private int
  (** The type for the bytes of a record of a ['r] codec, big-endian. *)

  type 'r reader
  (** The type for readers of the records of a ['r] codec. *)

  val reader : 'r codec -> 'r reader
  (** [reader c] reads the records of [c]. Raises [Invalid_argument] when [c] is
      not of a fixed size or has more bytes than fit an [int] with a bit to
      spare (7 on a 64-bit host, 3 on a 31- or 32-bit one). *)

  type 'r check = private int
  (** The type for the results of checking a record of a ['r] codec: its word,
      or a negative [int] where it was refused. *)

  val check : 'r reader -> bytes -> int -> 'r check
  (** [check r buf off] is the record of [r] at [off] in [buf]. It is a function
      the caller's compiler knows, so the load is inlined into the caller's
      loop.

      [check] never raises: a negative [off] or a record [buf] does not hold is
      refused. Its error is {!error}. *)

  val ok : 'r check -> bool
  (** [ok k] is [true] iff [k] accepted its record. *)

  val get : 'r check -> 'r t
  (** [get k] is the word of the record [k] accepted.

      Raises [Invalid_argument] if [ok k] is [false]. Test {!ok} first: a
      refused record is input, not misuse. *)

  val error : 'r reader -> bytes -> int -> Error.t
  (** [error r buf off] is the error of a record {!check} refused: the error
      {!Codec.get} answers for the first field [buf] cuts. Out of line, for the
      slow path.

      Raises [Invalid_argument] on a negative [off], as {!Codec.get} does, and
      if {!check} accepts the record. *)

  type ('a, 'r) field
  (** The type for fields of type ['a] of the words of a ['r] codec. *)

  val field : 'r codec -> ('a, 'r) codec_field -> ('a, 'r) field
  (** [field c f] is the field [f] of the words of [c]. Raises
      [Invalid_argument] as {!reader} does, and when [f] is not one of [c]'s or
      is not a big-endian integer or a bitfield of a byte or a big-endian 16- or
      32-bit word. *)

  val read : ('a, 'r) field -> 'r t -> 'a
  (** [read f w] is the field [f] of [w]: two shifts and a mask, with no memory
      access but the field's own. *)
end

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
      {!get} stages for the field [name] of [t] and [typ] its type, or [None]
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

  (** {2 Kinds} *)

  (** The type of a field the twin reads, as the codec declares it. *)
  type _ kind =
    | Int : int kind
    | UInt8 : UInt8.t kind
    | UInt16 : UInt16.t kind
    | UInt32 : UInt32.t kind
    | SInt8 : SInt8.t kind
    | SInt16 : SInt16.t kind
    | SInt32 : SInt32.t kind
    | String : string kind
    | Slice : Bytesrw.Bytes.Slice.t kind

  (** The outputs of a record as a fold hands them out, by name: each named
      scalar field, its name the path of field labels joined with ['_'], and for
      each optional field [<path>_present]. An absent field is [0] or [false].
  *)
  module Outputs : sig
    type t
    (** The type for the outputs of one record. *)

    val int : t -> string -> int64
    (** [int o name] is the integer output [name] of [o]. Raises
        [Invalid_argument] if [o] has no integer output [name]. *)

    val bool : t -> string -> bool
    (** [bool o name] is the presence [name] of [o]. Raises [Invalid_argument]
        if [o] has no presence [name]. *)
  end

  (** {2 The interpreted twin}

      What the interpreted twin of a generated module ([Wire_ocaml]) reads its
      records through: each record of a codec, and each of its fields by its
      place in the codec's field list, checked against the type the generated
      module reads. A view and a check are the [int]s of {!View}. *)

  type record
  (** The type for the records of a codec. *)

  val root : 'r t -> record
  (** [root c] is the records of [c]. *)

  val sub_at : record -> int -> record
  (** [sub_at r i] is the records the field [i] of [r] holds. Raises
      [Invalid_argument] if it holds none. *)

  val enter_at : record -> int -> (unit, unit) View.sub
  (** [enter_at r i] is the field [i] of [r] as a sub-record. *)

  val enter :
    (unit, unit) View.sub ->
    'b Wire_runtime.Buf.t ->
    'b Wire_runtime.Buf.length ->
    int ->
    int
  (** [enter s b len v] is {!View.enter}. *)

  val check_at :
    record -> 'b Wire_runtime.Buf.t -> 'b Wire_runtime.Buf.length -> int -> int
  (** [check_at r b len off] is {!View.check} on the record at [off]. *)

  val check_window :
    record ->
    'b Wire_runtime.Buf.t ->
    'b Wire_runtime.Buf.length ->
    off:int ->
    len:int ->
    int
  (** [check_window r b length ~off ~len] is [off] when the [len] bytes at [off]
      lie in [b] and the codec accepts them on their own, and [-1] otherwise. *)

  type 'a reader = ('a, unit) View.field
  (** A field's read: a field of the views of its codec. *)

  val read : 'a reader -> 'b Wire_runtime.Buf.t -> int -> 'a
  (** [read h b v] reads the field [h] of the record [v] holds in [b]. *)

  val field_at : record -> int -> 'a kind -> 'a reader
  (** [field_at r i k] reads the field [i] of [r], of kind [k], or the zero of
      [k] where it is absent. Raises [Invalid_argument] if it has another kind.
  *)

  val present_at : record -> int -> bool reader
  (** [present_at r i] reads whether the optional field [i] of [r] is present.
  *)

  type 'a writer = { write : 'b. 'b Wire_runtime.Buf.t -> int -> 'a -> unit }
  (** The write of a field of the record a view holds. *)

  val set_at : record -> int -> 'a kind -> 'a writer
  (** [set_at r i k] writes the field [i] of [r], as {!Codec.set} does. *)

  val scan : 'r t -> bytes -> int -> int -> Outputs.t option
  (** [scan c buf off len] is the outputs of the record in the [len] bytes at
      [off], which the codec accepts on their own, or [None]. *)

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
