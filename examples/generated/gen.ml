(* Writes the modules Wire_ocaml generates for the CLCW codec, [clcw], and
   for the Ethernet, IPv4 and TCP or UDP stack, [stack], or their
   interfaces, as its command line names them. The committed copies are for
   the OCaml target, whichever compiler builds them. *)

let () =
  let target = `OCaml in
  Wire_ocaml.(
    main
      [ v ~target Space.clcw_codec; v ~target ~name:"stack" Packet_stack.codec ])
