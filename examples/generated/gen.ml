(* Writes the modules Wire_ocaml generates for the CLCW codec, [clcw], and
   for the Ethernet, IPv4 and TCP or UDP stack, [stack], or their
   interfaces, as its command line names them. [stack] has a fold over the
   IPv4 protocol and the TCP destination port. The committed copies are for
   the OCaml target, whichever compiler builds them. *)

let () =
  let target = `OCaml in
  Wire_ocaml.(
    main
      [
        v ~target Space.clcw_codec;
        v ~target ~name:"stack"
          ~fold:[ "ipv4.protocol"; "ipv4.tcp.dst_port" ]
          Packet_stack.codec;
      ])
