(* Writes what the checker reads besides the committed generated modules:
   the 3D specifications of the codecs, projected as EverParse compiles them,
   the modules Wire_ocaml writes for codecs with no committed copy, and the
   module of the packet stack with the fold [fold_step], whose visitor is
   [Step], on OCaml and on OxCaml. *)

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
  let stack target =
    Wire_ocaml.(
      to_ml
        ~visitors:[ ("fold_step", "step.ml") ]
        (v ~target ~folds:[ fold "fold_step" ] Packet_stack.codec))
  in
  write dir "stack_step.gen" (stack `OCaml);
  write dir "stack_step_ox.gen" (stack `OxCaml);
  write dir "bf_constrained.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.bf_constrained_codec));
  write dir "reserved_fields.gen"
    (Wire_ocaml.to_ml (Wire_ocaml.v ~target:`OCaml Demo.reserved_fields_codec))
