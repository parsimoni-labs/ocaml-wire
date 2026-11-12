(** EverParse projection cases: every codec that builds must project to a 3D
    schema and print, without invoking [3d.exe]. *)

open Gen

val everparse_cases : string -> 'a t -> Alcobar.test_case list
(** [everparse_cases label g] projects [g]'s codec to a 3D schema and
    pretty-prints it, asserting neither raises. *)

val everparse_nested_cases : string -> int -> Alcobar.test_case list
(** [everparse_nested_cases label depth] is {!everparse_cases} over a freshly
    generated nested composition per sample. *)

val afl_everparse_excluded : string list
(** [afl_everparse_excluded] names the registry codecs left out of the EverParse
    sweep because their projection is rejected at construction. *)

val afl_everparse_cases : ?max_len:int -> string -> Alcobar.test_case list
(** [afl_everparse_cases label] projects one registry codec picked per AFL
    input. *)
