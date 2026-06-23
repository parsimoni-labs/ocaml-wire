(** AFL fuzzer for the wire_3d library: the EverParse / 3D projection suite. *)

let () =
  List.iter
    (fun name -> Fmt.epr "everparse: excluding %s (projection rejected)@." name)
    Fuzz_gen.afl_everparse_excluded;
  Alcobar.run "everparse" [ Fuzz_everparse.suite ]
