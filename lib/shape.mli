(** Construction-time gate on the shape of a codec's field list.

    Wire only ships what projects to EverParse 3D, so a field layout that cannot
    be given a verified validator is refused when the codec is built, not
    silently accepted and then quietly weakened at decode. Each check here
    covers one such layout: a greedy field that is not last, a [where] buried
    inside a container element, a byte-size product that can exceed EverParse's
    u32 limit, and a repeated field name.

    A check reports by raising [Invalid_argument] with a message naming the
    codec and the offending field, so the failure lands on the line that wrote
    the schema. The checks read the field list and nothing else: no buffer, no
    slots, no sealed codec, which is why they can run before any of that exists.
*)

val reject_invalid_codec_shape : string -> Types.field list -> unit
(** [reject_invalid_codec_shape name fields] runs every check against the fields
    of the codec called [name]. Raises [Invalid_argument] on the first one that
    fails. *)
