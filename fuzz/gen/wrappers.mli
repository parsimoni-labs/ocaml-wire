(** More leaves and wrappers: lookups, refinements, bitfields and bit-packed
    records, sequences, params and actions, float predicates, expression
    operators, dynamic gates and casetypes. *)

open Gen

val lookup : 'a list -> 'b t -> 'a t
(** [lookup table inner] generates for [Wire.lookup table inner.typ]. *)

val where : 'a t -> 'a t
(** [where inner] generates for [Wire.where Wire.Expr.true_ inner.typ]. *)

val bits : ?bit_order:Wire.bit_order -> width:int -> Wire.bitfield -> int t
(** [bits ?bit_order ~width base] generates for
    [Wire.bits ?bit_order ~width base]. *)

val bitpack_split_orders : (int * int) t
(** [bitpack_split_orders] packs two 4-bit fields of opposite bit orders into
    one byte each. *)

val bitpack_u8_msb : (int * int * int) t
(** [bitpack_u8_msb] packs three bitfields into one [U8] word, MSB first. *)

val bitpack_u8_lsb : (int * int * int) t
(** [bitpack_u8_lsb] packs three bitfields into one [U8] word, LSB first. *)

val bitpack_u8_spill : (int * int) t
(** [bitpack_u8_spill] has two [U8] bitfields that do not fit one word, so the
    second spills into a new one. *)

val bitpack_u16_msb : (int * int * int) t
(** [bitpack_u16_msb] packs three bitfields into one [U16] word, MSB first. *)

val bitpack_u16_lsb : (int * int * int) t
(** [bitpack_u16_lsb] packs three bitfields into one [U16] word, LSB first. *)

val bitpack_u16be_msb : (int * int * int) t
(** [bitpack_u16be_msb] packs three bitfields into one [U16be] word, MSB first.
*)

val bitpack_u16be_lsb : (int * int * int) t
(** [bitpack_u16be_lsb] packs three bitfields into one [U16be] word, LSB first.
*)

val bitpack_u16be_spill : (int * int) t
(** [bitpack_u16be_spill] is {!bitpack_u8_spill} over [U16be] words. *)

val bitpack_u32_msb : (int * int * int) t
(** [bitpack_u32_msb] packs three bitfields into one [U32] word, MSB first. *)

val bitpack_u32_lsb : (int * int * int) t
(** [bitpack_u32_lsb] packs three bitfields into one [U32] word, LSB first. *)

val bitpack_u32be_msb : (int * int * int) t
(** [bitpack_u32be_msb] packs three bitfields into one [U32be] word, MSB first.
*)

val bitpack_u32be_lsb : (int * int * int) t
(** [bitpack_u32be_lsb] packs three bitfields into one [U32be] word, LSB first.
*)

val bit : 'a t -> bool t
(** [bit inner] generates for [Wire.bit inner.typ]. *)

val array_seq : int -> 'a t -> 'a list t
(** [array_seq n inner] generates for
    [Wire.array_seq Wire.seq_list ~len:(Wire.int n) inner.typ]. *)

val repeat_seq : ?name:string -> bytes:int -> 'a t -> 'a list t
(** [repeat_seq ~bytes inner] generates for
    [Field.repeat_seq ~seq:seq_list ~size:(...)]. *)

val field_anon : (Wire.UInt8.t * Wire.UInt8.t) t
(** [field_anon] exercises [Wire.Field.anon]. *)

val param_input : Wire.UInt8.t t
(** [param_input] is a codec referencing [Wire.Param.input] in its [~where]. *)

val action : Wire.UInt8.t t
(** [action] is a codec with [Wire.Field.v ~action:...]. *)

val action_abort : Wire.UInt8.t t
(** [action_abort] is a codec whose field's [~action] is {!Wire.Action.abort}.
*)

val action_on_act : Wire.UInt8.t t
(** [action_on_act] mirrors {!action} using [Action.on_act]. *)

val nan_float64 : float t
(** [nan_float64] is a single-float codec with [~where:(is_nan f)]. *)

val finite_float64 : float t
(** [finite_float64] is a single-float codec with [~self_constraint:is_finite].
*)

val expr_ops : (Wire.UInt8.t * Wire.UInt8.t) t
(** [expr_ops] is a codec whose [~where] uses every integer [Wire.Expr]
    operator. *)

val rest_bytes : (Wire.UInt8.t * string) t
(** [rest_bytes] is a codec whose final field is [Wire.rest_bytes] sized from a
    bound [Wire.Param.input]. *)

val param_size : string t
(** [param_size] is a [byte_array] whose [~size] is a [Param.input] bound by
    name. *)

val if_then_else : (Wire.UInt8.t * string) t
(** [if_then_else] is a payload whose width a [Wire.Expr.if_then_else] over a
    length byte selects. *)

val sizeof : (Wire.UInt8.t * Wire.UInt8.t) t
(** [sizeof] is a two-uint8 codec whose second field's [~self_constraint]
    references [Wire.sizeof_this] / [Wire.field_pos] / [Wire.sizeof]. *)

val optional_dynamic : (Wire.UInt8.t * Wire.UInt16.t option) t
(** [optional_dynamic] is a codec with
    [Field.optional ~present:(ref gate <> 0)]. *)

val optional_or_dynamic : (Wire.UInt8.t * Wire.UInt16.t) t
(** [optional_or_dynamic] is the [optional_or] equivalent of
    {!optional_dynamic}. *)

val optional_dyn_after_span :
  (Wire.UInt8.t * string * Wire.UInt16.t option * string) t
(** [optional_dyn_after_span] is a dynamic-gate optional behind a
    length-prefixed span, so its payload sits at a runtime offset. *)

type 'a case
(** A single branch of a casetype. *)

val case :
  index:int -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case
(** [case ~index inner ~inject ~project] is a tagged case decoded when the
    discriminator equals [~index]. *)

val default_case :
  tag:int -> 'w t -> inject:('w -> 'a) -> project:('a -> 'w option) -> 'a case
(** [default_case ~tag inner ~inject ~project] is the fallback case; [~tag] is
    the byte written on encode. *)

val combine_env_strategies : env_strategy option list -> env_strategy option
(** [combine_env_strategies parts] binds every part's params on the composite's
    own env, or is [None] when no part has any. *)

val casetype_tagged :
  string -> 'tag Wire.typ -> (int -> 'tag) -> 'a case list -> 'a t
(** [casetype_tagged name tag lift cases] generates for a casetype over the
    discriminator [tag], with [lift] carrying a case index into the tag's
    carrier. *)

val casetype_u8 : string -> 'a case list -> 'a t
(** [casetype_u8 name cases] generates for
    [Wire.casetype name Wire.uint8 cases]. *)

val casetype_u16be_default :
  [ `A of Wire.UInt8.t | `Other of Wire.UInt16.t * Wire.UInt16.t ] t
(** [casetype_u16be_default] generates for [Wire.casetype] over a uint16be
    discriminator and a [Wire.default] branch. *)
