(** The F* modules that prove a scanner's [fold] equal to the parser 3d.exe
    writes for the codec. Internal to {!Wire_ocaml}. *)

val modules :
  ?fields:string list ->
  codec:string ->
  'a Wire.Codec.t ->
  (string * string) list
(** [modules ?fields ~codec c] is each F* file of [c], by name: see
    {!Wire_ocaml.fstar}. Raises {!Planner.Unsupported} on a codec it does not
    cover. *)
