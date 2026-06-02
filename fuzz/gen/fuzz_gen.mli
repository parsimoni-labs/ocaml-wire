(** Fuzz generators that mirror Wire's surface.

    Each [t] bundles a codec with three Alcobar generators: [positive] produces
    a value together with bytes that decode to it, [random] produces arbitrary
    bytes the decoder must handle without raising, and [adversarial] produces
    bytes at the boundary of every constraint the codec checks (lengths, gate
    flips, bit-pattern extremes). {!Codec.v} composes leaves into record gens
    with the same shape as {!Wire.Codec.v} composes typs into record codecs -- a
    record gen is itself a [t], so composition is recursive. *)

type 'a t
(** A fuzz generator paired with the codec it tests. *)

val test_cases : string -> 'a t -> Alcobar.test_case list
(** [test_cases label g] is three Alcobar cases: one round-trip on [g.positive]
    and one crash-safety check each on [g.random] and [g.adversarial]. *)

val nested_cases : string -> int -> Alcobar.test_case list
(** [nested_cases label depth] generates an arbitrary nested codec per sample
    (combinators composed up to [depth] levels: optional / repeat / array /
    nested / record / casetype / map / where over each other and the leaves) and
    runs the same three checks as {!test_cases}. This exercises compositions no
    curated list enumerates, surfacing offset / [size_of_value] drift that only
    shows up when combinators nest. *)

val reject_cases : string -> 'a t -> Alcobar.test_case list
(** [reject_cases label g] is two Alcobar cases asserting that decode rejects
    every input on [g.random] and [g.adversarial]. Use for codecs that always
    fail (e.g. {!Wire.Action.abort}). *)

val direct_cases : string -> 'a t -> Alcobar.test_case list
(** [direct_cases label g] round-trips a gen through {!Wire.of_string} and
    {!Wire.to_string} (the typ-level API). Skipped when [g] needs a [Param.env],
    since those direct entry points don't thread one. *)

val streaming_cases : string -> 'a t -> Alcobar.test_case list
(** [streaming_cases label g] round-trips a gen through {!Wire.of_reader} and
    {!Wire.to_writer} (streaming Bytesrw entry points). Same env restriction as
    {!direct_cases}. *)

val validate_cases : string -> 'a t -> Alcobar.test_case list
(** [validate_cases label g] exercises {!Wire.Codec.validate}: positives must
    pass; random and adversarial inputs may pass or fail but must not crash.
    Threads the env via {!Wire.Codec.validate}'s [?env] when [g] needs one. *)

type packed =
  | Pack : 'a t -> packed
      (** A gen with its value type hidden, for uniform iteration over
          {!registry}. *)

val codec : 'a t -> 'a Wire.Codec.t
(** [codec g] is the codec [g] generates for. *)

val registry : (string * packed) list
(** The canonical curated gens, one per combinator family. Every suite drives
    off this list so a generated codec is exercised on both the OCaml round-trip
    path ({!test_cases}) and the 3D projection path ({!everparse_cases}) from
    one source. *)

val everparse_cases : string -> 'a t -> Alcobar.test_case list
(** [everparse_cases label g] projects [g]'s codec to a 3D schema and
    pretty-prints it, asserting neither raises. Covers the projection and
    code-generation path without invoking [3d.exe]. *)

val everparse_nested_cases : string -> int -> Alcobar.test_case list
(** [everparse_nested_cases label depth] is {!everparse_cases} over a freshly
    generated nested composition per sample (cf. {!nested_cases}). *)

val sized_cases : string -> Alcobar.test_case list
(** [sized_cases group] round-trips a two-field length/data codec whose data
    byte span is sized by a cross-field reference to the preceding length field,
    for each of several int-valued length-source field types (a plain scalar, a
    mapped scalar, a static optional-or field, and a refined field). Regression
    coverage for cross-field size expressions that read an unusual size-source
    field. *)

(** {1 Scalar leaves} *)

val uint8 : int t
(** [uint8] generates for {!Wire.uint8}. *)

val uint16 : int t
(** [uint16] generates for {!Wire.uint16}. *)

val uint16be : int t
(** [uint16be] generates for {!Wire.uint16be}. *)

val uint32 : int t
(** [uint32] generates for {!Wire.uint32}. *)

val uint32be : int t
(** [uint32be] generates for {!Wire.uint32be}. *)

val uint63 : int t
(** [uint63] generates for {!Wire.uint63}. *)

val uint63be : int t
(** [uint63be] generates for {!Wire.uint63be}. *)

val uint64 : int64 t
(** [uint64] generates for {!Wire.uint64}. *)

val uint64be : int64 t
(** [uint64be] generates for {!Wire.uint64be}. *)

val int8 : int t
(** [int8] generates for {!Wire.int8}. *)

val int16 : int t
(** [int16] generates for {!Wire.int16}. *)

val int16be : int t
(** [int16be] generates for {!Wire.int16be}. *)

val int32 : int t
(** [int32] generates for {!Wire.int32}. *)

val int32be : int t
(** [int32be] generates for {!Wire.int32be}. *)

val int64 : int64 t
(** [int64] generates for {!Wire.int64}. *)

val int64be : int64 t
(** [int64be] generates for {!Wire.int64be}. *)

val float32 : float t
(** [float32] generates for {!Wire.float32}. *)

val float32be : float t
(** [float32be] generates for {!Wire.float32be}. *)

val float64 : float t
(** [float64] generates for {!Wire.float64}. *)

val float64be : float t
(** [float64be] generates for {!Wire.float64be}. *)

val empty : unit t
(** [empty] generates for {!Wire.empty}. *)

(** {1 Bytes} *)

val byte_array : int -> string t
(** [byte_array n] generates for [Wire.byte_array ~size:(Wire.int n)]. *)

val byte_slice : int -> Bytesrw.Bytes.Slice.t t
(** [byte_slice n] generates for [Wire.byte_slice ~size:(Wire.int n)]. *)

val bits : ?bit_order:Wire.bit_order -> width:int -> Wire.bitfield -> int t
(** [bits ?bit_order ~width base] generates for
    [Wire.bits ?bit_order ~width base]. *)

val bit : int t -> bool t
(** [bit inner] generates for [Wire.bit inner.typ]. *)

val uint_var : endian:Wire.endian -> int -> int t
(** [uint_var ~endian size] generates for [Wire.uint ~endian (Wire.int size)].
*)

val optional : ?present:bool -> 'a t -> 'a option t
(** [optional ~present inner] generates for a [Field.optional]-wrapped record
    with a static [~present] gate. *)

val optional_or : ?present:bool -> default:'a -> 'a t -> 'a t
(** [optional_or ~present ~default inner] generates for a
    [Field.optional_or]-wrapped record with a static [~present] gate. *)

val repeat : bytes:int -> 'a t -> 'a list t
(** [repeat ~bytes inner] generates for a [Field.repeat] whose byte-budget is
    the value of a uint16 length-prefix field. *)

val codec_wrap :
  'a Wire.Codec.t ->
  value_gen:'a Alcobar.gen ->
  equal:('a -> 'a -> bool) ->
  'a t
(** [codec_wrap c ~value_gen ~equal] wraps a hand-built codec as a gen. Random
    and adversarial streams default to arbitrary bytes. *)

val nested_at_most : int -> 'a t -> 'a t
(** [nested_at_most n inner] generates for
    [Wire.nested_at_most ~size:(Wire.int n) inner.typ]. *)

val byte_array_where :
  int -> per_byte:(int Wire.expr -> bool Wire.expr) -> string t
(** [byte_array_where n ~per_byte] generates for
    [Wire.byte_array_where ~size:(Wire.int n) ~per_byte]. Positives are
    printable-ASCII bytes (space, 0x20) which satisfy most reasonable per-byte
    predicates; adversarials are random bytes that probably violate the
    predicate. *)

val all_bytes : string t
(** [all_bytes] generates for {!Wire.all_bytes}. *)

val all_zeros : string t
(** [all_zeros] generates for {!Wire.all_zeros}. *)

val zeroterm : string t
(** [zeroterm] generates for {!Wire.zeroterm}. Positives are NUL-free strings;
    the adversarial stream includes unterminated runs. *)

val zeroterm_at_most : int -> string t
(** [zeroterm_at_most n] generates for
    [Wire.zeroterm_at_most ~size:(Wire.int n)]. Positives are NUL-free strings
    capped at [n - 1] bytes so the terminator fits the region. *)

(** {1 Wrappers} *)

val nested : int -> 'a t -> 'a t
(** [nested n inner] generates for [Wire.nested ~size:(Wire.int n) inner.typ].
*)

val map : decode:('a -> 'b) -> encode:('b -> 'a) -> 'a t -> 'b t
(** [map ~decode ~encode inner] generates for
    [Wire.map ~decode ~encode inner.typ]. *)

val variants : string -> (string * 'a) list -> 'a t
(** [variants name cases] generates for [Wire.variants name cases Wire.uint8].
*)

val array : int -> 'a t -> 'a list t
(** [array n inner] generates for [Wire.array ~len:(Wire.int n) inner.typ].
    Positives are a fixed-length list of [inner.positive] samples. *)

val enum : string -> (string * int) list -> int t
(** [enum name cases] generates for [Wire.enum name cases Wire.uint8]. *)

val bounded_u8 : min:int -> max:int -> int t
(** [bounded_u8 ~min ~max] generates for a single-field record whose
    [Wire.uint8] field carries a [~self_constraint] [min <= v <= max].
    Adversarials are bytes at each boundary on both sides of the range. *)

val lookup : 'a list -> int t -> 'a t
(** [lookup table inner] generates for [Wire.lookup table inner.typ]. *)

val where : 'a t -> 'a t
(** [where inner] generates for [Wire.where Wire.Expr.true_ inner.typ]. *)

val array_seq : int -> 'a t -> 'a list t
(** [array_seq n inner] generates for
    [Wire.array_seq Wire.seq_list ~len:(Wire.int n) inner.typ]. *)

val repeat_seq : bytes:int -> 'a t -> 'a list t
(** [repeat_seq ~bytes inner] generates for
    [Field.repeat_seq ~seq:seq_list ~size:(...)]. *)

val field_anon : (int * int) t
(** [field_anon] exercises [Wire.Field.anon]. *)

val param_input : int t
(** [param_input] is a codec referencing [Wire.Param.input] in its [~where];
    tests bind the env. *)

val action : int t
(** [action] is a codec with [Wire.Field.v ~action:...] exercising the
    {!Wire.Action} surface ([on_success], [var], [assign], [if_],
    [return_bool]). *)

val action_abort : int t
(** [action_abort] is a codec whose field's [~action] is {!Wire.Action.abort}:
    every decode is rejected. Use {!reject_cases}. *)

val action_on_act : int t
(** [action_on_act] mirrors {!action} using [Action.on_act]. *)

val nan_float64 : float t
(** [nan_float64] is a single-float codec with [~where:(is_nan f)]; positives
    are NaN bit patterns, non-NaN bytes are rejected. *)

val optional_dynamic : (int * int option) t
(** [optional_dynamic] is a codec with
    [Field.optional ~present:(ref gate <> 0)]; the payload is present or absent
    depending on the gate byte. *)

val optional_or_dynamic : (int * int) t
(** [optional_or_dynamic] is the {!val-optional_or} equivalent of
    {!optional_dynamic}, using a fixed default value when absent. *)

val finite_float64 : float t
(** [finite_float64] is a single-float codec with [~self_constraint:is_finite];
    adversarials produce NaN and infinity bit patterns. *)

val expr_ops : (int * int) t
(** [expr_ops] is a codec whose [~where] uses every integer [Wire.Expr] operator
    (arithmetic, bitwise, comparison, logical, [to_uint*] casts). *)

val rest_bytes : (int * string) t
(** [rest_bytes] is a codec whose final field is [Wire.rest_bytes] sized from a
    bound [Wire.Param.input]. *)

val sizeof : (int * int) t
(** [sizeof] is a two-uint8 codec whose second field's [~self_constraint]
    references [Wire.sizeof_this] / [Wire.field_pos] / [Wire.sizeof]. *)

val codec_where : (int * int) t
(** [codec_where] generates for a two-{!val-uint8} record whose
    {!module-Codec.val-v} carries [~where:(a < b)]. Positives satisfy the
    predicate, adversarials sit at the equality boundary so the {!val-where}
    check fires. *)

(** {1 Casetype} *)

type 'a case
(** A single branch of a {!casetype_u8}. *)

val case :
  index:int -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case
(** [case ~index inner ~inject ~project] is a tagged case decoded when the
    discriminator equals [~index]. *)

val default_case :
  tag:int -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case
(** [default_case ~tag inner ~inject ~project] is the fallback case used when no
    tagged case matches; [~tag] is the byte written on encode. *)

val casetype_u8 : string -> 'a case list -> 'a t
(** [casetype_u8 name cases] generates for
    [Wire.casetype name Wire.uint8 cases]. Positives pick a case uniformly, emit
    its tag byte, and append the inner case's positive bytes. *)

(** {1 Record composition} *)

module Codec : sig
  type ('a, 'r) field

  val ( $ ) : 'a t -> ('r -> 'a) -> ('a, 'r) field
  (** [g $ getter] binds gen [g] to record projection [getter]. *)

  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  val v : string -> ?equal:('r -> 'r -> bool) -> 'f -> ('f, 'r) fields -> 'r t
  (** [v name builder fields] composes leaves into a record gen whose codec is
      the flat [Wire.Codec.v name builder typs] you'd write by hand. *)
end
