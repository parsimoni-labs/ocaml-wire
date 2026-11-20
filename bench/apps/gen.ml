(* Writes the code Wire generates from the codecs the application benchmarks
   read, Space.packet_codec, Space.tm_frame_codec and Space.clcw_codec.

   With [--ocaml] (or [--oxcaml], for the OxCaml target, and [--oxcaml
   --simd] for it on arm64 or amd64, with the SIMD lanes of the records that have
   them), in the current directory: the modules Wire_ocaml writes,
   g_space_packet.ml, g_tm_frame.ml and g_clcw.ml, each with its interface.
   With [--rust DIR], in [DIR]: the modules Wire_rust writes, space_packet.rs,
   tm_frame.rs and clcw.rs. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

(* Each codec, written by [g name path codec]. *)
type g = { g : 'r. string -> string -> 'r Wire.Codec.t -> unit }

let codecs { g } =
  g "space_packet" "Space.packet_codec" Space.packet_codec;
  g "tm_frame" "Space.tm_frame_codec" Space.tm_frame_codec;
  g "clcw" "Space.clcw_codec" Space.clcw_codec

let () =
  match Sys.argv with
  | [| _; ("--ocaml" | "--oxcaml") as t |] | [| _; "--oxcaml" as t; "--simd" |]
    ->
      let target = if t = "--oxcaml" then `OxCaml else `OCaml in
      let simd = Array.length Sys.argv = 3 in
      codecs
        {
          g =
            (fun name codec c ->
              write ("g_" ^ name ^ ".ml") (Wire_ocaml.ml ~target ~simd ~codec c);
              write
                ("g_" ^ name ^ ".mli")
                (Wire_ocaml.mli ~target ~simd ~codec c));
        }
  | [| _; "--rust"; dir |] ->
      codecs
        {
          g =
            (fun name codec c ->
              write (Filename.concat dir (name ^ ".rs")) (Wire_rust.rs ~codec c));
        }
  | _ ->
      prerr_endline "usage: gen.exe (--ocaml | --oxcaml [--simd] | --rust DIR)";
      exit 2
