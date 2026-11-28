(* Writes what the checker reads besides the committed generated modules:
   the 3D specifications of the codecs, projected as EverParse compiles them,
   the modules Wire_ocaml writes for codecs with no committed copy, and the
   module of the packet stack with a fold of every integer field, whose walks
   are [fold_step], [fold_step_frames] and [fold_step_index], on OCaml and on
   OxCaml. *)

let write dir file contents =
  Out_channel.with_open_bin (Filename.concat dir file) (fun oc ->
      output_string oc contents)

let spec dir name codec =
  Wire.Everparse.write ~mode:`Standalone ~outdir:dir ~name
    [ Wire.Everparse.project codec ]

let () =
  let dir = Sys.argv.(1) in
  spec dir "Clcw" Space.clcw_codec;
  spec dir "Stack" Packet_stack.codec;
  spec dir "BfConstrained" Demo.bf_constrained_codec;
  spec dir "ReservedFields" Demo.reserved_fields_codec;
  let rec paths prefix (view : Wire_ocaml.Views.t) =
    List.concat_map
      (function
        | Wire_ocaml.Views.Int { name; _ } -> [ prefix ^ name ]
        | Record { name; view; _ } -> paths (prefix ^ name ^ ".") view
        | Bool _ | Bytes _ -> [])
      view.members
  in
  let fold = paths "" (Wire_ocaml.Views.v Packet_stack.codec) in
  let stack target = Wire_ocaml.(to_ml (v ~target ~fold Packet_stack.codec)) in
  write dir "stack_step.gen" (stack `OCaml);
  write dir "stack_step_ox.gen" (stack `OxCaml);
  write dir "bf_constrained.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.bf_constrained_codec));
  write dir "reserved_fields.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.reserved_fields_codec))
