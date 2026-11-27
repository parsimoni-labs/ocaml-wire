(* Writes the code Wire generates from the codecs the application benchmarks
   read, Space.packet_codec, Space.tm_frame_codec and Space.clcw_codec.

   With [--rust DIR], in [DIR]: the modules Wire_rust writes, space_packet.rs,
   tm_frame.rs and clcw.rs. Otherwise the modules Wire_ocaml writes,
   g_space_packet, g_tm_frame and g_clcw, as {!Wire_ocaml.main} reads its
   command line, for the compiler that builds this generator. With
   [WIRE_SIMD=1] in the environment, which the rule sets on OxCaml for arm64
   or amd64, g_clcw has the SIMD lanes of its records. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let () =
  match Sys.argv with
  | [| _; "--rust"; dir |] ->
      let rs name c =
        write
          (Filename.concat dir (name ^ ".rs"))
          (Wire_rust.to_rs (Wire_rust.v ~name c))
      in
      rs "space_packet" Space.packet_codec;
      rs "tm_frame" Space.tm_frame_codec;
      rs "clcw" Space.clcw_codec
  | _ ->
      let simd = Sys.getenv_opt "WIRE_SIMD" = Some "1" in
      Wire_ocaml.(
        main
          [
            v ~name:"g_space_packet" Space.packet_codec;
            v ~name:"g_tm_frame" Space.tm_frame_codec;
            v ~name:"g_clcw" ~simd Space.clcw_codec;
          ])
