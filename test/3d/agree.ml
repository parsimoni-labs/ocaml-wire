(* The [agree] differential: the EverParse validators of a package and the
   OCaml codecs they come from, run on one corpus. *)

open Wire

(* Build the package's validator and [agree] once, then pass [f] a function that
   runs [agree] on a corpus and returns its exit status and output. *)
let with_agree ~name ~package codecs f =
  let tmpdir = Filename.temp_dir ("wire_3d_diff_" ^ package) "" in
  Fun.protect
    ~finally:(fun () -> ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir))
    (fun () ->
      Wire_3d.generate_standalone ~outdir:tmpdir ~name ~package codecs;
      let sh cmd =
        let ic = Fmt.kstr Unix.open_process_in "cd %s && %s 2>&1" tmpdir cmd in
        let output = In_channel.input_all ic in
        (Unix.close_process_in ic, output)
      in
      let base = String.capitalize_ascii name in
      let run corpus =
        let oc = open_out (Filename.concat tmpdir "corpus") in
        (match corpus with
        | `Fuzz count ->
            let ppf = Format.formatter_of_out_channel oc in
            Wire_3d.generate_corpus ~count ppf codecs
        | `Lines lines -> List.iter (fun l -> output_string oc (l ^ "\n")) lines);
        close_out oc;
        sh "./agree corpus"
      in
      match
        Fmt.kstr sh
          "cc %s %s -c %s.c %sWrapper.c && ar rcs lib.a %s.o %sWrapper.o && cc \
           %s %s agree.c lib.a -o agree"
          Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines base base base
          base Wire_3d.strict_cc_flags Wire_3d.everparse_type_defines
      with
      | Unix.WEXITED 0, _ -> f run
      | _, output ->
          Alcotest.failf "agree for %s does not build:\n%s" name output)

let differential_ok ~name ~package ~corpus codecs =
  let status, output =
    with_agree ~name ~package codecs (fun run -> run corpus)
  in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      Alcotest.failf "differential %s failed with exit %d:\n%s" name n output
  | _ -> Alcotest.failf "differential %s terminated abnormally:\n%s" name output

(* A corpus line for [hex], after checking that the codec gives [accepted]:
   the record decodes and spans the whole input. *)
let corpus_line name c hex accepted =
  let buf =
    Bytes.init
      (String.length hex / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub hex (2 * i) 2)))
  in
  let codec_accepts =
    match Codec.decode c buf 0 with
    | Error _ -> false
    | Ok _ -> Codec.wire_size_at c buf 0 = Bytes.length buf
  in
  Alcotest.(check bool) (name ^ " " ^ hex) accepted codec_accepts;
  Fmt.str "%s - %s %d" name hex (Bool.to_int accepted)
