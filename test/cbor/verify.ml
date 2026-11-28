(* Writes the .cddl module of each schema of Cbor_modules in the directory
   given as argument and runs evercddl on each in full: F* verifies the module
   it writes, then its C is extracted and compiled. Prints each job's output,
   which ends with "<module>: verified in <n>s". *)

let () =
  let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Wire_3d.ensure_dir outdir;
  Wire_cbor.run ~quiet:false ~outdir Cbor_modules.all
