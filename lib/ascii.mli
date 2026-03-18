(** RFC-style ASCII bit layout diagrams.

    Renders a wire struct or codec as a 32-bit-wide ASCII diagram following the
    conventions of RFC 791 and similar documents:

    {v
      0                   1                   2                   3
      0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
     |Version|  IHL  |    DSCP   |ECN|         Total Length          |
     +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
    v}

    Variable-length fields are rendered as full-width rows with a dependent-size
    annotation:

    {v
     +-------------------------------+
     | Data (Len * 8 bits)           |
     +-------------------------------+
    v} *)

val of_struct : Types.struct_ -> string
(** Render a struct as an RFC-style bit diagram. *)

val of_codec : 'r Codec.t -> string
(** Render a codec as an RFC-style bit diagram. *)

val pp_struct : Format.formatter -> Types.struct_ -> unit
(** Pretty-print a struct as an RFC-style bit diagram. *)

val pp_codec : Format.formatter -> 'r Codec.t -> unit
(** Pretty-print a codec as an RFC-style bit diagram. *)
