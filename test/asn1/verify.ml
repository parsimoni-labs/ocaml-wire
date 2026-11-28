(* Writes the .asn module of each schema of Asn1_modules in the directory given
   as argument and runs everparse-asn1 on each in full: F* verifies the
   modules it generates, then extracts their C. Prints each job's output, which
   ends with "<module>: verified in <n>s". *)

let () =
  let outdir = if Array.length Sys.argv > 1 then Sys.argv.(1) else "." in
  Wire_3d.Private.ensure_dir outdir;
  Wire_asn1.run ~quiet:false ~outdir Asn1_modules.all
