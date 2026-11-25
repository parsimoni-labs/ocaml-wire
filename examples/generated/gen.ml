(* Writes the modules Wire_ocaml generates for the CLCW codec, [clcw], and
   for the Ethernet, IPv4 and TCP or UDP stack, [stack], or their
   interfaces, as its command line names them. *)

let () =
  Wire_ocaml.(main [ v Space.clcw_codec; v ~name:"stack" Packet_stack.codec ])
