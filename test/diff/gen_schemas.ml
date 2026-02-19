(** Generate .3d files for differential testing schemas. *)

let () =
  (* Files are generated in the dune build directory *)
  D3t.to_3d_file "SimpleHeader.3d" Schema.simple_header_module;
  D3t.to_3d_file "ConstrainedPacket.3d" Schema.constrained_packet_module
