(* Writes the .asn module of each type of Asn1_modules in the directory given
   as argument, with [<M>.roots], the --root argument of everparse-asn1. *)

let () =
  let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Wire.Asn1.write ~outdir Asn1_modules.all;
  List.iter
    (fun s ->
      Out_channel.with_open_bin
        (Filename.concat outdir (Wire.Asn1.name s ^ ".roots"))
        (fun oc ->
          Out_channel.output_string oc (String.concat "," (Wire.Asn1.roots s))))
    Asn1_modules.all
