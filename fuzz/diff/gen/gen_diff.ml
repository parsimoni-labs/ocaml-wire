(** Generate the EverParse C parsers and FFI stubs the differential fuzzer
    compares against. Run by the [gen] dune rule when EverParse is available.

    Writes the EverParse C into [<argv1>/] (default [schemas/]) and [wire_ffi.c]
    + [stubs.ml] into the current directory. *)

let () =
  let schema_dir =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "schemas"
  in
  Wire_3d.run ~outdir:schema_dir Diff_codecs.schemas;
  Wire_stubs.generate ~schema_dir ~outdir:"."
    Wire_stubs.
      [
        C Diff_codecs.c_u8;
        C Diff_codecs.c_u16;
        C Diff_codecs.c_u32;
        C Diff_codecs.c_u64;
        C Diff_codecs.c_bits;
        C Diff_codecs.c_triple;
      ]
