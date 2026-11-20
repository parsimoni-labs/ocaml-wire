(* Prints a module Wire_ocaml writes: [clcw], the record module of the CLCW
   codec, or [stack], the scanner of the Ethernet, IPv4 and TCP or UDP
   stack. *)

let () =
  match Sys.argv with
  | [| _; "clcw" |] ->
      print_string (Wire_ocaml.ml ~codec:"Space.clcw_codec" Space.clcw_codec)
  | [| _; "stack" |] ->
      print_string
        (Wire_ocaml.scanner ~codec:"Packet_stack.codec" Packet_stack.codec)
  | _ -> invalid_arg "usage: gen.exe (clcw | stack)"
