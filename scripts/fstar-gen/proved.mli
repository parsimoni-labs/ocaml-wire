(** The codecs [make prove] proves. *)

(** A codec and the name of its F* modules. *)
type codec = Codec : string * 'a Wire.Codec.t -> codec

val all : codec list
(** [all] is each codec [make prove] proves: Tiny (two fields and a constraint),
    the Ethernet and IPv4 headers of [bench/rust/headers.ml], the CLCW of
    [examples/space], a variable-size field between two fields (Span), an
    optional codec (Gate), two on one tag (Dispatch) and one in a region
    (Region). *)
