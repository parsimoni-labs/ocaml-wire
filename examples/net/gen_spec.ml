(* Render the net protocol family to its documentation [.3d] and print it.
   The committed [Net.3d] is this output; a runtest diff keeps the two in sync,
   so the checked-in spec never drifts from the codecs in [net.ml]. *)
let () =
  let dir = Filename.temp_dir "net_spec" "" in
  Wire.Everparse.write_doc ~outdir:dir ~name:"net"
    Wire.Everparse.
      [
        doc Net.ethernet_codec;
        doc Net.ipv4_codec;
        doc Net.tcp_codec;
        doc Net.udp_codec;
      ];
  let path = Filename.concat dir "Net.3d" in
  print_string (In_channel.with_open_text path In_channel.input_all)
