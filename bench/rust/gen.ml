(* Writes the code generated for the benchmark, for the compiler that builds
   this generator.

   With [--rust DIR], in [DIR]: the Wire_rust modules of the fixed header
   codecs, eth.rs, ipv4.rs, ipv6.rs, tcp.rs and udp.rs; that of
   [Packet_stack.codec], stack_selected.rs, whose fold hands out the fields
   the checksum reads; and that of [Packet_stack.capture], flow.rs, whose
   fold hands out the flow classifier's fields.

   Otherwise the modules Wire_ocaml writes, as {!Wire_ocaml.main} reads its
   command line: g_selected, the module of [Packet_stack.codec] whose fold
   hands out the fields the checksum reads; g_flow, that of
   [Packet_stack.capture] whose fold hands out the flow classifier's;
   g_capture, that of [Packet_stack.capture] with no fold, whose views the
   check/view row reads; and g_eth, g_ipv4, g_ipv6, g_tcp and g_udp, those
   of [Headers]. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

(* The fields of the checksum and of the flow classifier as the fold of a
   generated module names them, by their paths: a field under a layer is an
   option, [None] where the layer is absent, which stands for the presences
   of the outputs above. *)
let checksum_paths =
  [
    "ether_type";
    "ipv4.src";
    "ipv4.dst";
    "ipv4.ttl";
    "ipv4.protocol";
    "ipv4.total_length";
    "ipv4.tcp.src_port";
    "ipv4.tcp.dst_port";
    "ipv4.tcp.seq";
    "ipv4.tcp.ack";
    "ipv4.tcp.window";
    "ipv4.tcp.flags";
    "ipv4.tcp.data_offset";
    "ipv4.udp.src_port";
    "ipv4.udp.dst_port";
    "ipv4.udp.length";
  ]

let flow_paths =
  [
    "ipv4.total_length";
    "ipv4.protocol";
    "ipv4.src";
    "ipv4.dst";
    "ipv4.tcp.src_port";
    "ipv4.tcp.dst_port";
    "ipv4.udp.src_port";
    "ipv4.udp.dst_port";
    "ipv6.payload_length";
    "ipv6.next_header";
    "ipv6.src0";
    "ipv6.src1";
    "ipv6.src2";
    "ipv6.src3";
    "ipv6.dst0";
    "ipv6.dst1";
    "ipv6.dst2";
    "ipv6.dst3";
    "ipv6.tcp.src_port";
    "ipv6.tcp.dst_port";
    "ipv6.udp.src_port";
    "ipv6.udp.dst_port";
  ]

(* The fixed header codecs, each written by [g name codec]. *)
type g = { g : 'r. string -> 'r Wire.Codec.t -> unit }

let headers { g } =
  g "eth" Headers.eth;
  g "ipv4" Headers.ipv4;
  g "ipv6" Headers.ipv6;
  g "tcp" Headers.tcp;
  g "udp" Headers.udp

let () =
  match Array.to_list Sys.argv with
  | [ _; "--rust"; dir ] ->
      let rs name m =
        write (Filename.concat dir (name ^ ".rs")) (Wire_rust.to_rs m)
      in
      rs "stack_selected"
        (Wire_rust.v ~name:"stack_selected" ~fold:checksum_paths
           Packet_stack.codec);
      rs "flow" (Wire_rust.v ~name:"flow" ~fold:flow_paths Packet_stack.capture);
      headers { g = (fun name c -> rs name (Wire_rust.v ~name c)) }
  | _ ->
      Wire_ocaml.(
        main
          [
            v ~name:"g_selected" ~fold:checksum_paths Packet_stack.codec;
            v ~name:"g_flow" ~fold:flow_paths Packet_stack.capture;
            v ~name:"g_capture" Packet_stack.capture;
            v ~name:"g_eth" Headers.eth;
            v ~name:"g_ipv4" Headers.ipv4;
            v ~name:"g_ipv6" Headers.ipv6;
            v ~name:"g_tcp" Headers.tcp;
            v ~name:"g_udp" Headers.udp;
          ])
