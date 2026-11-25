(* Writes what the checker reads besides the committed generated modules:
   the 3D specifications of the codecs, projected as EverParse compiles them,
   and the modules Wire_ocaml writes for codecs with no committed copy. *)

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
  let step = Wire_ocaml.[ fold "fold"; fold "fold_step" ]
  and visitors = [ ("fold_step", "step.ml") ] in
  let stack ?target ?folds () =
    Wire_ocaml.v ?target ?folds Packet_stack.codec
  in
  write dir "stack_ox.gen" (Wire_ocaml.to_ml (stack ~target:`OxCaml ()));
  write dir "stack_step.gen"
    (Wire_ocaml.to_ml ~visitors (stack ~target:`OCaml ~folds:step ()));
  write dir "stack_step_ox.gen"
    (Wire_ocaml.to_ml ~visitors (stack ~target:`OxCaml ~folds:step ()));
  write dir "bf_constrained.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.bf_constrained_codec));
  write dir "reserved_fields.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.reserved_fields_codec))
