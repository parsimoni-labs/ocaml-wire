(** Validate every example schema by feeding it to [3d.exe] (parse-only, no
    [--batch]). Skipped when [3d.exe] is unavailable. *)

module Raw = Wire.Everparse.Raw

(* Two emission paths matter and both should produce parseable 3D:
   - [Raw]: a hand-built struct or module written via [to_3d_file].
   - [Codec]: a Wire codec projected via [Wire.Everparse.schema] and
     [Wire_3d.generate_3d]. This path adds entrypoint params, output
     types, and the [<Name>Set*] callbacks that the C validator uses. *)
type case =
  | Raw of string * Raw.module_
  | Codec : string * 'a Wire.Codec.t -> case

let case_name = function Raw (n, _) | Codec (n, _) -> n

let raw_struct ?(entrypoint = true) (s : Raw.struct_) =
  let module_ = Raw.module_ [ Raw.typedef ~entrypoint s ] in
  Raw (Raw.struct_name s, module_)

let raw_module name module_ = Raw (name, module_)

let cases =
  [
    raw_struct Demo.minimal_struct;
    raw_struct Demo.all_ints_struct;
    raw_struct Demo.bf8_struct;
    raw_struct Demo.bf16_struct;
    raw_struct Demo.bf32_struct;
    raw_struct Demo.bool_fields_struct;
    raw_struct Demo.large_mixed_struct;
    raw_struct Demo.mapped_struct;
    raw_struct Demo.cases_demo_struct;
    raw_struct Demo.enum_demo_struct;
    raw_struct Demo.constrained_struct;
    raw_struct Demo.array_struct;
    raw_struct Demo.byte_array_struct;
    raw_struct Demo.all_bytes_struct;
    raw_struct Demo.all_zeros_struct;
    raw_struct Demo.single_elem_struct;
    raw_struct Demo.single_elem_at_most_struct;
    raw_struct Demo.anon_field_struct;
    raw_struct Demo.action_struct;
    raw_struct Demo.action_full_struct;
    raw_struct Demo.param_demo_struct;
    raw_struct Demo.param_payload_struct;
    raw_struct Space.clcw_struct;
    raw_struct Space.packet_struct;
    raw_struct Space.tm_frame_struct;
    raw_struct Net.ethernet_struct;
    raw_struct Net.ipv4_struct;
    raw_struct Net.tcp_struct;
    raw_struct Net.udp_struct;
    raw_module "Message" Demo.casetype_module;
    raw_module "ExternDemo" Demo.extern_module;
    raw_module "Outer" Demo.type_ref_module;
    Codec ("FullPacket", Space.full_packet_codec);
    Codec ("TMWithOCF", Space.tm_with_ocf_codec);
    Codec ("InnerCmd", Space.inner_cmd_codec);
    Codec ("OuterHdr", Space.outer_hdr_codec);
  ]

let emit ~outdir = function
  | Raw (name, module_) ->
      let path = Filename.concat outdir (name ^ ".3d") in
      Raw.to_3d_file path module_;
      name ^ ".3d"
  | Codec (_, codec) ->
      let s = Wire.Everparse.schema codec in
      Wire_3d.generate_3d ~outdir [ s ];
      Wire.Everparse.filename s

let validate_one case =
  let outdir = Filename.temp_dir "wire_validate_3d" "" in
  let cleanup () =
    Array.iter
      (fun f ->
        try Sys.remove (Filename.concat outdir f) with Sys_error _ -> ())
      (try Sys.readdir outdir with Sys_error _ -> [||]);
    try Unix.rmdir outdir with Unix.Unix_error _ -> ()
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let file = emit ~outdir case in
  match Wire_3d.parse_3d ~outdir file with
  | Ok () -> ()
  | Error err ->
      let path = Filename.concat outdir file in
      let content =
        try In_channel.with_open_text path In_channel.input_all
        with Sys_error _ -> "(could not read .3d)"
      in
      Alcotest.failf "@[<v>3d.exe rejected %s:@,%s@,---@,%s@]" (case_name case)
        err content

let test_case case =
  Alcotest.test_case (case_name case) `Quick (fun () -> validate_one case)

let () =
  if not (Wire_3d.has_3d_exe ()) then begin
    Fmt.pr "3d.exe not found, skipping validation\n";
    exit 0
  end;
  Alcotest.run "validate_3d" [ ("schemas", List.map test_case cases) ]
