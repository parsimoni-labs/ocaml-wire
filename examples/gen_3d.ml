module Raw = Wire.Everparse.Raw

let write_struct (s : Raw.struct_) =
  let name = Raw.struct_name s in
  let module_ = Raw.module_ [ Raw.typedef ~entrypoint:true s ] in
  Raw.to_3d_file (name ^ ".3d") module_

let write_named_module file module_ = Raw.to_3d_file file module_

let () =
  List.iter write_struct
    [
      Demo.minimal_struct;
      Demo.all_ints_struct;
      Demo.bf8_struct;
      Demo.bf16_struct;
      Demo.bf32_struct;
      Demo.bool_fields_struct;
      Demo.large_mixed_struct;
      Demo.mapped_struct;
      Demo.cases_demo_struct;
      Demo.enum_demo_struct;
      Demo.constrained_struct;
      Demo.array_struct;
      Demo.byte_array_struct;
      Demo.all_bytes_struct;
      Demo.all_zeros_struct;
      Demo.single_elem_struct;
      Demo.single_elem_at_most_struct;
      Demo.anon_field_struct;
      Demo.action_struct;
      Demo.action_full_struct;
      Demo.param_demo_struct;
      Demo.param_payload_struct;
      Space.clcw_struct;
      Space.packet_struct;
      Wire.Everparse.struct_of_codec Space.full_packet_codec;
      Space.tm_frame_struct;
      Wire.Everparse.struct_of_codec Space.tm_with_ocf_codec;
      Wire.Everparse.struct_of_codec Space.inner_cmd_codec;
      Wire.Everparse.struct_of_codec Space.outer_hdr_codec;
      Net.ethernet_struct;
      Net.ipv4_struct;
      Net.tcp_struct;
      Net.udp_struct;
    ];
  List.iter
    (fun (file, module_) -> write_named_module file module_)
    [
      ("Message.3d", Demo.casetype_module);
      ("ExternDemo.3d", Demo.extern_module);
      ("Outer.3d", Demo.type_ref_module);
    ]
