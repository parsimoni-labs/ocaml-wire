(* gen.exe DIR writes, for each codec make prove proves, DIR/<Name>.3d (the
   standalone 3D projection) and the F* modules Wire_ocaml.fstar writes for
   its scanner. *)

open Wire

let write dir name c =
  let s = Everparse.project ~mode:`Standalone c in
  Everparse.write ~mode:`Standalone ~outdir:dir ~name [ s ];
  List.iter
    (fun (file, text) ->
      Out_channel.with_open_text (Filename.concat dir file) (fun oc ->
          output_string oc text))
    (Wire_ocaml.fstar ~codec:name c)

let () =
  let dir = Sys.argv.(1) in
  List.iter (fun (Proved.Codec (name, c)) -> write dir name c) Proved.all;
  (* The scanner whose ihl make prove changes to three bits, which must not
     verify. *)
  write dir "IPv4Mutant" Headers.ipv4
