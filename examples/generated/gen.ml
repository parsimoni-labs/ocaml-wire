(* Prints the OCaml module Wire_ocaml writes for the CLCW codec. *)

let () = print_string (Wire_ocaml.ml ~codec:"Space.clcw_codec" Space.clcw_codec)
