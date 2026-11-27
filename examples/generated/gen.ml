(* Writes the modules Wire_ocaml generates for the CLCW codec, [clcw], and
   for the Ethernet, IPv4 and TCP or UDP stack, [stack], or their
   interfaces, as its command line names them. [stack] has a fold,
   [count_tcp], whose visitor is [Count_tcp]. The committed copies are for
   the OCaml target, whichever compiler builds them. *)

let () =
  let target = `OCaml in
  Wire_ocaml.(
    main
      [
        v ~target Space.clcw_codec;
        v ~target ~name:"stack"
          ~folds:[ fold ~outputs:[ "ipv4_tcp_present" ] "count_tcp" ]
          Packet_stack.codec;
      ])
