(* gen.exe DIR writes DIR/<Name>.3d (standalone, no extern actions) and
   DIR/<name>_scan.ml, the scanner Wire_ocaml emits, for each codec. *)

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
  let ml = Wire_ocaml.scanner ~codec:name c in
  Out_channel.with_open_text
    (Filename.concat dir (String.lowercase_ascii name ^ "_scan.ml"))
    (fun oc -> output_string oc ml)

let () =
  let dir = Sys.argv.(1) in
  write dir "Tiny" tiny;
  write dir "Ethernet" Headers.eth;
  write dir "IPv4" Headers.ipv4;
  write dir "EthernetStack" Packet_stack.codec
