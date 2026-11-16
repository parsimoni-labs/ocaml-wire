(** The properties one sample of a gen's streams is checked against: round-trip
    and size agreement on positives, crash-safety on hostile bytes and values,
    per-field accessors, offset invariance, framing, and read purity. Each check
    fails the current Alcobar case with a message naming [label]. *)

open Gen

val positive_env : 'a t -> Wire.Param.env option
(** [positive_env g] is the env the positive stream of [g] binds, if [g] needs
    one. *)

val check_direct_codec_agree : string -> string -> 'a t -> bytes -> unit
(** [check_direct_codec_agree label kind g bs] requires {!Wire.of_string} and
    {!Wire.Codec.decode} to reach the same verdict on [bs]. Skipped when [g]
    binds a [Param.env]. *)

val check_direct_codec_encode_agree : string -> string -> 'a t -> 'a -> unit
(** [check_direct_codec_encode_agree label kind g v] requires {!Wire.to_bytes}
    and {!Wire.Codec.encode} to refuse the same values and write the same bytes.
*)

val check_positive : string -> 'a t -> 'a * bytes -> unit
(** [check_positive label g (v, bs)] checks a positive: decode, size, the env
    requirement and encode all agree with [(v, bs)]. *)

val validate_one :
  ?env:Wire.Param.env -> 'a t -> bytes -> [> `Crash | `Ok | `Reject ]
(** [validate_one ?env g bs] is the verdict of {!Wire.Codec.validate} on [bs].
*)

val check_validate_positive : string -> 'a t -> bytes -> unit
(** [check_validate_positive label g bs] requires validate to accept [bs]. *)

val check_validate_safety :
  string -> string -> ?env:Wire.Param.env -> 'a t -> bytes -> unit
(** [check_validate_safety label kind ?env g bs] requires validate not to crash
    on [bs]. *)

val check_decode_safety :
  string -> string -> ?env:Wire.Param.env -> 'a t -> bytes -> unit
(** [check_decode_safety label kind ?env g bs] requires decode not to crash on
    [bs]. *)

val check_encode_safety : string -> ?env:Wire.Param.env -> 'a t -> 'a -> unit
(** [check_encode_safety label ?env g v] requires encode to refuse the hostile
    value [v] or to write bytes that decode back to it. *)

val check_decode_validate_agree :
  string -> string -> ?env:Wire.Param.env -> 'a t -> bytes -> unit
(** [check_decode_validate_agree label kind ?env g bs] requires validate to
    raise exactly when decode returns [Error], for the same reason. *)

val hostile_value_stream : 'a t -> 'a option Alcobar.gen
(** [hostile_value_stream g] is [g]'s hostile values, or [None] samples for a
    shape with nothing to violate. *)

type 'r addressed = {
  acodec : 'r Wire.Codec.t;
  achecks : 'r field_check list;
  anames : string list;
  ggeneral : 'r Wire.Codec.t;
  ageneral : 'r Wire.Codec.t;
}
(** A gen's codec with the fields {!Wire.Codec.get} and {!Wire.Codec.set} can
    address. *)

val addressed : 'a t -> 'a addressed
(** [addressed g] is [g]'s record fields, or the one-field wrapper of a leaf. *)

val stage_get :
  ?env:Wire.Param.env ->
  'a addressed ->
  ('b, 'a) Wire.Codec.field ->
  (bytes -> int -> 'b) option
(** [stage_get ?env a field] is the staged reader of [field], or [None] where
    the accessor API has none. *)

val stage_set :
  'a addressed ->
  ('b, 'a) Wire.Codec.field ->
  (bytes -> int -> 'b -> unit) option
(** [stage_set a field] is the staged writer of [field], or [None] where the
    accessor API has none. *)

val check_fields :
  string -> 'a addressed -> 'b t -> 'a * Bytes.t -> 'a -> 'a option -> unit
(** [check_fields label a g (v, bs) other hostile] checks every per-field
    accessor property on one positive, writing [other] and [hostile] over it. *)

val check_field_reads :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'a t ->
  bytes ->
  unit
(** [check_field_reads label kind ?env a g bs] requires every [get] to be safe
    on bytes validate accepts and to agree with decode where it accepts. *)

val noise_gen : bytes Alcobar.gen
(** [noise_gen] generates the short byte strings prepended and appended by the
    metamorphic checks. *)

val check_offset_invariance :
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'b t ->
  bytes ->
  bytes ->
  unit
(** [check_offset_invariance label ?env a g pad bs] requires every read of [bs]
    behind [pad] to give the answer it gives at offset 0. *)

val check_offset_encode :
  string -> ?env:Wire.Param.env -> 'a t -> 'a -> Bytes.t -> bytes -> unit
(** [check_offset_encode label ?env g v bs pad] requires encode at an offset to
    write the same frame and nothing outside it. *)

val check_frame_extent :
  string -> string -> ?env:Wire.Param.env -> 'a t -> bytes -> bytes -> unit
(** [check_frame_extent label kind ?env g tail bs] requires an accepted frame to
    read the same when cut to its declared size and when [tail] is appended. *)

val check_short_prefix :
  string -> string -> ?env:Wire.Param.env -> 'a t -> bytes -> unit
(** [check_short_prefix label kind ?env g bs] requires nothing shorter than
    [min_wire_size] to decode. *)

val check_read_purity :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'b t ->
  Bytes.t ->
  unit
(** [check_read_purity label kind ?env a g bs] requires every read path to leave
    [bs] byte-identical. *)

val check_frame_isolation :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'b t ->
  bytes ->
  bytes ->
  unit
(** [check_frame_isolation label kind ?env a g bs interloper] requires reading
    [interloper] through every path not to change any answer about [bs]. *)

val pure_value : 'a t -> 'a -> bool
(** [pure_value g v] is [true] when the getters of [v] answer the same on
    successive calls, so [v] has one encoding to compare byte for byte. *)

val check_fast_general :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'a t ->
  bytes ->
  unit
(** [check_fast_general label what ?env c g buf] checks that every entry point
    of the codec answers on [buf] what its general path answers. *)

val check_cold_warm :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a addressed ->
  'b t ->
  bytes ->
  unit
(** [check_cold_warm label what ?env c g buf] runs every entry point once on a
    codec not yet compiled and checks it against the same entry point on the
    warm one. *)

val check_encode_paths :
  string -> string -> ?env:Wire.Param.env -> 'a addressed -> 'a t -> 'a -> unit
(** [check_encode_paths label what ?env c g v] encodes [v] into a buffer of
    exactly its size, one short of it, and one behind a prefix, on the fast, the
    general and a cold codec, and checks they agree. *)

val check_set_paths : string -> 'a addressed -> 'a -> bytes -> unit
(** [check_set_paths label c v buf] sets each field on the fast and the general
    codec, from the same buffer, and checks they agree. *)

val check_fast_general_core :
  string ->
  string ->
  ?env:Wire.Param.env ->
  'a t ->
  'a Wire.Codec.t ->
  bytes ->
  unit
(** [check_fast_general_core label what ?env g general buf] checks [decode] and
    [validate] of the warm codec of [g] on [buf] against the codec [general],
    whose fast paths are off. *)
