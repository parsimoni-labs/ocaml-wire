(** Generate .3d files for differential testing schemas. *)

let () =
  (* Verify that simple_header_struct is the struct backing simple_header_module *)
  let struct_name = Wire.struct_name Schema.simple_header_struct in
  assert (struct_name = "SimpleHeader");
  (* Files are generated in the dune build directory *)
  Wire.to_3d_file "SimpleHeader.3d" Schema.simple_header_module;
  Wire.to_3d_file "ConstrainedPacket.3d" Schema.constrained_packet_module
