(** The modules of a bridge that swap with the generated ones: a record module's
    [Global] as a [Wire.S] and its interpreted twin, and a scanner's interpreted
    twin. Internal to {!Wire_ocaml}. *)

val record_ml :
  record:string -> codec:string -> core:string -> Planner.field list -> string
(** [record_ml ~record ~codec ~core fields] is [Interpreted], [Wire.Codec.Make]
    over the codec at [codec] of records of type [record] with a handle of each
    of [fields], and [Global], the [Global] of the module at [core] with
    [field]. *)

val record_mli : record:string -> core:string -> Planner.field list -> string
(** [record_mli ~record ~core fields] is the interface of
    [record_ml ~record ~codec ~core fields]. *)

val scanner_ml :
  target:[ `OCaml | `OxCaml ] ->
  codec:string ->
  core:string ->
  Planner.output list ->
  string
(** [scanner_ml ~target ~codec ~core outputs] is [Global], the scanner at [core]
    as a [Wire.Scan]; [STEP], the consumer of a scanner handing out [outputs];
    and [Interpreted_scan], the scanner of the codec at [codec] read by
    [Wire.Codec], folding such a consumer. *)

val scanner_mli :
  target:[ `OCaml | `OxCaml ] -> core:string -> Planner.output list -> string
(** [scanner_mli ~target ~core outputs] is the interface of
    [scanner_ml ~target ~codec ~core outputs]. *)
