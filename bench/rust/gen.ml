(* Writes the code generated for the benchmark, for the compiler that builds
   this generator.

   With [--rust DIR], in [DIR]: the Wire_rust modules of the fixed header
   codecs, eth.rs, ipv4.rs, ipv6.rs, tcp.rs and udp.rs; the scanner Wire_rust
   writes from [Packet_stack.codec], stack.rs; the same scanner handing out
   only the fields the checksum reads, stack_selected.rs; and the scanner of
   [Packet_stack.capture] handing out the flow classifier's fields, flow.rs.

   Otherwise the modules Wire_ocaml writes, as {!Wire_ocaml.main} reads its
   command line: g_stack and g_selected, the modules of [Packet_stack.codec]
   with every output and with those the checksum reads; g_flow, that of
   [Packet_stack.capture] with the flow classifier's; g_capture, that of
   [Packet_stack.capture] with every output, whose views the check/view row
   reads; and g_eth, g_ipv4, g_ipv6, g_tcp and g_udp, those of [Headers],
   and their interpreted twins, g_eth_codec and so on, which the swap rows
   read. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let checksum_fields =
  [
    "ether_type";
    "ipv4_present";
    "ipv4_src";
    "ipv4_dst";
    "ipv4_ttl";
    "ipv4_protocol";
    "ipv4_total_length";
    "ipv4_tcp_present";
    "ipv4_tcp_src_port";
    "ipv4_tcp_dst_port";
    "ipv4_tcp_seq";
    "ipv4_tcp_ack";
    "ipv4_tcp_window";
    "ipv4_tcp_flags";
    "ipv4_tcp_data_offset";
    "ipv4_udp_present";
    "ipv4_udp_src_port";
    "ipv4_udp_dst_port";
    "ipv4_udp_length";
  ]

(* {1 The flow classifier}

   The outputs of the scanner of [Packet_stack.capture] the classifier reads:
   the addresses, the protocol, the ports and the IP length of each packet. *)
let flow_fields =
  [
    "ipv4_present";
    "ipv4_total_length";
    "ipv4_protocol";
    "ipv4_src";
    "ipv4_dst";
    "ipv4_tcp_present";
    "ipv4_tcp_src_port";
    "ipv4_tcp_dst_port";
    "ipv4_udp_present";
    "ipv4_udp_src_port";
    "ipv4_udp_dst_port";
    "ipv6_present";
    "ipv6_payload_length";
    "ipv6_next_header";
    "ipv6_src0";
    "ipv6_src1";
    "ipv6_src2";
    "ipv6_src3";
    "ipv6_dst0";
    "ipv6_dst1";
    "ipv6_dst2";
    "ipv6_dst3";
    "ipv6_tcp_present";
    "ipv6_tcp_src_port";
    "ipv6_tcp_dst_port";
    "ipv6_udp_present";
    "ipv6_udp_src_port";
    "ipv6_udp_dst_port";
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
      let codec = "Packet_stack.codec" in
      write
        (Filename.concat dir "stack.rs")
        (Wire_rust.scanner ~codec Packet_stack.codec);
      write
        (Filename.concat dir "stack_selected.rs")
        (Wire_rust.scanner ~fields:checksum_fields ~codec Packet_stack.codec);
      write
        (Filename.concat dir "flow.rs")
        (Wire_rust.scanner ~fields:flow_fields ~codec:"Packet_stack.capture"
           Packet_stack.capture);
      headers
        {
          g =
            (fun name c ->
              write
                (Filename.concat dir (name ^ ".rs"))
                (Wire_rust.rs ~codec:("Headers." ^ name) c));
        }
  | _ ->
      let checksum o = Wire_ocaml.[ fold ?outputs:o "checksum" ]
      and flow =
        Wire_ocaml.
          [
            fold ~outputs:flow_fields "classify";
            fold ~outputs:flow_fields "parse";
          ]
      in
      Wire_ocaml.(
        main
          [
            v ~name:"g_stack" ~folds:(checksum None) Packet_stack.codec;
            v ~name:"g_selected"
              ~folds:(checksum (Some checksum_fields))
              Packet_stack.codec;
            v ~name:"g_flow" ~folds:flow Packet_stack.capture;
            v ~name:"g_capture" Packet_stack.capture;
            v ~name:"g_eth" Headers.eth;
            v ~name:"g_ipv4" Headers.ipv4;
            v ~name:"g_ipv6" Headers.ipv6;
            v ~name:"g_tcp" Headers.tcp;
            v ~name:"g_udp" Headers.udp;
          ])
