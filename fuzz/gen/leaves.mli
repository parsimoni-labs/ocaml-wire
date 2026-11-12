(** Leaf generators: scalars, byte spans, exact endian edges and the first
    wrappers (optional, repeat, nested, map, array, enum, variants, bounded
    ranges and refined records). *)

open Gen

val scalar_int :
  ?hostile:int Alcobar.gen ->
  int Wire.typ ->
  int ->
  int Alcobar.gen ->
  int list ->
  int t
(** [scalar_int ?hostile typ size value_gen boundaries] is the gen of an
    int-carried scalar of [size] bytes whose adversarial values are
    [boundaries]. *)

val u8_value_gen : Wire.UInt8.t Alcobar.gen
(** [u8_value_gen] generates arbitrary {!Wire.UInt8.t} values. *)

val uint8 : Wire.UInt8.t t
(** [uint8] generates for {!Wire.uint8}. *)

val uint16 : Wire.UInt16.t t
(** [uint16] generates for {!Wire.uint16}. *)

val uint16be : Wire.UInt16.t t
(** [uint16be] generates for {!Wire.uint16be}. *)

val uint32 : Wire.UInt32.t t
(** [uint32] generates for {!Wire.uint32}. *)

val uint32be : Wire.UInt32.t t
(** [uint32be] generates for {!Wire.uint32be}. *)

val uint64 : Wire.UInt64.t t
(** [uint64] generates for {!Wire.uint64}. *)

val uint64be : Wire.UInt64.t t
(** [uint64be] generates for {!Wire.uint64be}. *)

val int8 : Wire.SInt8.t t
(** [int8] generates for {!Wire.int8}. *)

val int16 : Wire.SInt16.t t
(** [int16] generates for {!Wire.int16}. *)

val int16be : Wire.SInt16.t t
(** [int16be] generates for {!Wire.int16be}. *)

val int32 : Wire.SInt32.t t
(** [int32] generates for {!Wire.int32}. *)

val int32be : Wire.SInt32.t t
(** [int32be] generates for {!Wire.int32be}. *)

val int64 : int64 t
(** [int64] generates for {!Wire.int64}. *)

val int64be : int64 t
(** [int64be] generates for {!Wire.int64be}. *)

val float64_bits_equal : float -> float -> bool
(** [float64_bits_equal a b] compares the bit patterns of [a] and [b], so NaN
    equals itself. *)

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

val byte_array : int -> string t
(** [byte_array n] generates for [Wire.byte_array ~size:(Wire.int n)]. *)

val byte_slice : int -> Bytesrw.Bytes.Slice.t t
(** [byte_slice n] generates for [Wire.byte_slice ~size:(Wire.int n)]. *)

val all_bytes : string t
(** [all_bytes] generates for {!Wire.all_bytes}. *)

val all_zeros : string t
(** [all_zeros] generates for {!Wire.all_zeros}. *)

val zeroterm : string t
(** [zeroterm] generates for {!Wire.zeroterm}. *)

val zeroterm_at_most : int -> string t
(** [zeroterm_at_most n] generates for
    [Wire.zeroterm_at_most ~size:(Wire.int n)]. *)

val byte_array_where :
  int -> per_byte:(int Wire.expr -> bool Wire.expr) -> string t
(** [byte_array_where n ~per_byte] generates for
    [Wire.byte_array_where ~size:(Wire.int n) ~per_byte]. *)

val nested : int -> 'a t -> 'a t
(** [nested n inner] generates for [Wire.nested ~size:(Wire.int n) inner.typ].
*)

val map : decode:('a -> 'b) -> encode:('b -> 'a) -> 'a t -> 'b t
(** [map ~decode ~encode inner] generates for
    [Wire.map ~decode ~encode inner.typ]. *)

val uint_var : endian:Wire.endian -> int -> Optint.Int63.t t
(** [uint_var ~endian size] generates for [Wire.uint ~endian size]. *)

val u16 : int -> Wire.UInt16.t
(** [u16 n] is {!Wire.UInt16.v}[ n]. *)

val uint16_endian_edges : Wire.UInt16.t t
(** [uint16_endian_edges] pins {!Wire.uint16} to exact byte patterns. *)

val uint16be_endian_edges : Wire.UInt16.t t
(** [uint16be_endian_edges] pins {!Wire.uint16be} to exact byte patterns. *)

val uint32_endian_edges : Wire.UInt32.t t
(** [uint32_endian_edges] pins {!Wire.uint32} to exact byte patterns. *)

val uint32be_endian_edges : Wire.UInt32.t t
(** [uint32be_endian_edges] pins {!Wire.uint32be} to exact byte patterns. *)

val u64 : int64 -> Wire.UInt64.t
(** [u64 n] is [n] as a {!Wire.UInt64.t}. *)

val uint64_endian_edges : Wire.UInt64.t t
(** [uint64_endian_edges] pins {!Wire.uint64} to exact byte patterns. *)

val uint64be_endian_edges : Wire.UInt64.t t
(** [uint64be_endian_edges] pins {!Wire.uint64be} to exact byte patterns. *)

val int16_endian_edges : Wire.SInt16.t t
(** [int16_endian_edges] pins {!Wire.int16} to exact byte patterns. *)

val int16be_endian_edges : Wire.SInt16.t t
(** [int16be_endian_edges] pins {!Wire.int16be} to exact byte patterns. *)

val int32_endian_edges : Wire.SInt32.t t
(** [int32_endian_edges] pins {!Wire.int32} to exact byte patterns. *)

val int32be_endian_edges : Wire.SInt32.t t
(** [int32be_endian_edges] pins {!Wire.int32be} to exact byte patterns. *)

val int64_endian_edges : int64 t
(** [int64_endian_edges] pins {!Wire.int64} to exact byte patterns. *)

val int64be_endian_edges : int64 t
(** [int64be_endian_edges] pins {!Wire.int64be} to exact byte patterns. *)

val uint_var3_little_edges : Optint.Int63.t t
(** [uint_var3_little_edges] pins a little-endian 3-byte {!Wire.uint} to exact
    byte patterns. *)

val uint_var3_big_edges : Optint.Int63.t t
(** [uint_var3_big_edges] pins a big-endian 3-byte {!Wire.uint} to exact byte
    patterns. *)

val uint_var7_little_edges : Optint.Int63.t t
(** [uint_var7_little_edges] pins a little-endian 7-byte {!Wire.uint} to exact
    byte patterns. *)

val uint_var7_big_edges : Optint.Int63.t t
(** [uint_var7_big_edges] pins a big-endian 7-byte {!Wire.uint} to exact byte
    patterns. *)

val optional : ?present:bool -> 'a t -> 'a option t
(** [optional ~present inner] generates for a [Field.optional]-wrapped record
    with a static [~present] gate. *)

val optional_or : ?present:bool -> default:'a -> 'a t -> 'a t
(** [optional_or ~present ~default inner] generates for a
    [Field.optional_or]-wrapped record with a static [~present] gate. *)

val shape_tag : 'a -> string
(** [shape_tag label] is a short stable tag for a shape, so two structurally
    different sub-codecs never take the same name. *)

val repeat_sized :
  string ->
  (size:int Wire.expr -> 'a Wire.typ -> 'a list Wire.Field.t) ->
  bytes:int ->
  'a t ->
  'a list t
(** [repeat_sized name make_items ~bytes inner] is the length-prefixed
    repetition {!repeat} and [repeat_seq] share, with [make_items] building the
    items field. *)

val repeat : ?name:string -> bytes:int -> 'a t -> 'a list t
(** [repeat ~bytes inner] generates for a [Field.repeat] whose byte-budget is
    the value of a uint16 length-prefix field. *)

val codec_wrap :
  'a Wire.Codec.t ->
  value_gen:'a Alcobar.gen ->
  equal:('a -> 'a -> bool) ->
  'a t
(** [codec_wrap c ~value_gen ~equal] wraps a hand-built codec as a gen. *)

val nested_at_most : int -> 'a t -> 'a t
(** [nested_at_most n inner] generates for
    [Wire.nested_at_most ~size:(Wire.int n) inner.typ]. *)

val variants : string -> (string * 'a) list -> 'a t
(** [variants name cases] generates for [Wire.variants name cases Wire.uint8].
*)

val array_sized :
  (len:int Wire.expr -> 'a Wire.typ -> 'a list Wire.typ) ->
  int ->
  'a t ->
  'a list t
(** [array_sized make_typ n inner] is the fixed-length array {!array} and
    [array_seq] share, with [make_typ] building the typ. *)

val array : int -> 'a t -> 'a list t
(** [array n inner] generates for [Wire.array ~len:(Wire.int n) inner.typ]. *)

val enum : string -> (string * int) list -> Wire.UInt8.t t
(** [enum name cases] generates for [Wire.enum name cases Wire.uint8]. *)

val enum_open : Wire.UInt8.t t
(** [enum_open] generates for [Wire.enum_open ... Wire.uint8]. *)

val enum_u16be : Wire.UInt16.t t
(** [enum_u16be] generates for [Wire.enum ... Wire.uint16be]. *)

val enum_open_u16be : Wire.UInt16.t t
(** [enum_open_u16be] generates for [Wire.enum_open ... Wire.uint16be]. *)

val variants_u16be : [ `High | `One | `Zero ] t
(** [variants_u16be] generates for [Wire.variants ... Wire.uint16be]. *)

val bounded_u8 : min:int -> max:int -> Wire.UInt8.t t
(** [bounded_u8 ~min ~max] generates for a single-field record whose
    [Wire.uint8] field carries a [~self_constraint] [min <= v <= max]. *)

val bounded_u16be : Wire.UInt16.t t
(** [bounded_u16be] is {!bounded_u8} over a [Wire.uint16be] in [[1000, 60000]].
*)

val bounded_u32be : Wire.UInt32.t t
(** [bounded_u32be] is {!bounded_u8} over a [Wire.uint32be] in
    [[1000, 1_000_000]]. *)

val codec_where : (Wire.UInt8.t * Wire.UInt8.t) t
(** [codec_where] generates for a two-{!uint8} record whose codec carries
    [~where:(a < b)]. *)

val typ_where : (Wire.UInt8.t * Wire.UInt8.t) t
(** [typ_where] generates for a two-{!uint8} record whose second field's typ is
    [Wire.where (len < 2) uint8]. *)

val field_constraint : (Wire.UInt8.t * Wire.UInt8.t) t
(** [field_constraint] exercises [Wire.Field.v ~constraint_] with a constraint
    referencing a previous field. *)

val field_int : (Wire.UInt8.t * Wire.UInt8.t) t
(** [field_int] exercises {!Wire.Field.int} in a field self constraint. *)

val self_int64 : Wire.UInt64.t t
(** [self_int64] exercises [Wire.Field.v ~self_int64] over a 64-bit field. *)
