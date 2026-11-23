(* Prints a module Wire_ocaml writes, or its interface: [clcw.ml] and
   [clcw.mli], the record module of the CLCW codec, or [stack.ml] and
   [stack.mli], the scanner of the Ethernet, IPv4 and TCP or UDP stack. *)

let () =
  let clcw = "Space.clcw_codec" and stack = "Packet_stack.codec" in
  match Sys.argv with
  | [| _; "clcw.ml" |] ->
      print_string (Wire_ocaml.ml ~codec:clcw Space.clcw_codec)
  | [| _; "clcw.mli" |] ->
      print_string (Wire_ocaml.mli ~codec:clcw Space.clcw_codec)
  | [| _; "stack.ml" |] ->
      print_string (Wire_ocaml.scanner ~codec:stack Packet_stack.codec)
  | [| _; "stack.mli" |] ->
      print_string (Wire_ocaml.scanner_mli ~codec:stack Packet_stack.codec)
  | _ ->
      invalid_arg "usage: gen.exe (clcw.ml | clcw.mli | stack.ml | stack.mli)"
