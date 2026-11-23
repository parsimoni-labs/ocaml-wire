(* gen.exe DIR writes, for each codec make prove proves, DIR/<Name>.3d (the
   standalone 3D projection) and the F* modules Wire_ocaml.fstar writes for
   its scanner. *)

open Wire

let tiny =
  Codec.v "Tiny"
    (fun a b -> (a, b))
    Codec.
      [
        (Field.v "a" uint8 $ fun (a, _) -> a);
        ( Field.v "b" uint16be ~self_constraint:(fun self ->
              Expr.(self >= int 4))
        $ fun (_, b) -> b );
      ]

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
  write dir "Tiny" tiny;
  write dir "Ethernet" Headers.eth;
  write dir "IPv4" Headers.ipv4;
  write dir "Clcw" Space.clcw_codec;
  (* The scanner whose ihl make prove changes to three bits, which must not
     verify. *)
  write dir "IPv4Mutant" Headers.ipv4
