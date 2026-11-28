(* Writes the .cddl module of each schema of Cbor_modules in the directory
   given as argument, for evercddl. *)

let () =
  let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Wire.Cbor.write ~outdir Cbor_modules.all
