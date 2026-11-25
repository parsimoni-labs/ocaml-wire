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
  write dir "IPv4Mutant" Headers.ipv4;
  (* The CLCW, whose step walk make prove shifts vcid by 17, which must not
     verify. *)
  write dir "ClcwStepMutant" Space.clcw_codec;
  (* Tiny, whose OxCaml fold make prove reads [a] from the byte after it,
     which must not verify. *)
  List.iter
    (fun (Proved.Codec (name, c)) ->
      if name = "Tiny" then write dir "TinyOxMutant" c)
    Proved.all
