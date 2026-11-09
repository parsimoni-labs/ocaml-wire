(* Writes the code generated from the codecs of [Headers]: with [--ocaml]
   (or [--oxcaml], for the OxCaml target), the Wire_ocaml modules g_eth.ml,
   g_ipv4.ml, g_tcp.ml and g_udp.ml in the current directory; with
   [--rust DIR], the Wire_rust modules eth.rs, ipv4.rs, tcp.rs and udp.rs in
   [DIR]. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let () =
  match Array.to_list Sys.argv with
  | [ _; (("--ocaml" | "--oxcaml") as t) ] ->
      let target = if t = "--oxcaml" then `OxCaml else `OCaml in
      let g name c =
        write
          ("g_" ^ name ^ ".ml")
          (Wire_ocaml.ml ~target ~codec:("Headers." ^ name) c)
      in
      g "eth" Headers.eth;
      g "ipv4" Headers.ipv4;
      g "tcp" Headers.tcp;
      g "udp" Headers.udp
  | [ _; "--rust"; dir ] ->
      let g name c =
        write
          (Filename.concat dir (name ^ ".rs"))
          (Wire_rust.rs ~codec:("Headers." ^ name) c)
      in
      g "eth" Headers.eth;
      g "ipv4" Headers.ipv4;
      g "tcp" Headers.tcp;
      g "udp" Headers.udp
  | _ ->
      prerr_endline "usage: gen.exe (--ocaml | --oxcaml | --rust DIR)";
      exit 2
