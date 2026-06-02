(** AFL fuzzer for the wire_3d library: the EverParse / 3D projection suite. *)

let () = Alcobar.run "everparse" [ Fuzz_everparse.suite ]
