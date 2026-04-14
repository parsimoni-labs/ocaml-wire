(** Validate all example schemas through the EverParse 3d.exe pipeline.

    For each codec, this generates the full output-types schema (with WireSet
    callbacks), writes the .3d file, and runs 3d.exe to verify it produces valid
    C. Exits with code 1 if 3d.exe rejects any schema. *)

let schemas =
  let s = Wire.Everparse.schema in
  [
    s Demo.minimal_codec;
    s Demo.all_ints_codec;
    s Demo.bf8_codec;
    s Demo.bf16_codec;
    s Demo.bf32_codec;
    s Demo.bool_fields_codec;
    s Demo.large_mixed_codec;
    s Demo.mapped_codec;
    s Demo.cases_demo_codec;
    s Demo.enum_demo_codec;
    s Demo.constrained_codec;
    s Space.clcw_codec;
    s Space.packet_codec;
    s Space.full_packet_codec;
    s Space.tm_frame_codec;
    s Space.tm_with_ocf_codec;
    s Space.inner_cmd_codec;
    s Space.outer_hdr_codec;
    s Net.ethernet_codec;
    s Net.ipv4_codec;
    s Net.tcp_codec;
    s Net.udp_codec;
  ]

let validate_one ~tmpdir s =
  let name = s.Wire.Everparse.name in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  try
    Wire_3d.run_everparse ~quiet:true ~outdir:tmpdir [ s ];
    Fmt.pr "  OK  %s\n%!" name;
    true
  with Failure msg ->
    let path = Filename.concat tmpdir (name ^ ".3d") in
    let content =
      try In_channel.with_open_text path In_channel.input_all
      with Sys_error _ -> "(could not read .3d file)"
    in
    Fmt.epr "  FAIL %s: %s\n%s\n%!" name msg content;
    false

let () =
  if not (Wire_3d.has_3d_exe ()) then (
    Fmt.pr "3d.exe not found, skipping validation\n";
    exit 0);
  let tmpdir = Filename.temp_dir "wire_validate_3d" "" in
  Fmt.pr "Validating %d schemas with 3d.exe\n%!" (List.length schemas);
  let passed =
    List.fold_left
      (fun n s -> if validate_one ~tmpdir s then n + 1 else n)
      0 schemas
  in
  let total = List.length schemas in
  Fmt.pr "%d/%d schemas accepted by 3d.exe\n" passed total;
  Array.iter
    (fun f ->
      try Sys.remove (Filename.concat tmpdir f) with Sys_error _ -> ())
    (Sys.readdir tmpdir);
  (try Unix.rmdir tmpdir with Unix.Unix_error _ -> ());
  if passed < total then exit 1
