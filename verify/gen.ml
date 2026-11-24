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
  write dir "stack_ox.gen"
    (Wire_ocaml.scanner ~target:`OxCaml ~codec:"Packet_stack.codec"
       Packet_stack.codec);
  write dir "stack_step.gen"
    (Wire_ocaml.scanner ~step:"Step" ~codec:"Packet_stack.codec"
       Packet_stack.codec);
  write dir "stack_step_ox.gen"
    (Wire_ocaml.scanner ~target:`OxCaml ~step:"Step" ~codec:"Packet_stack.codec"
       Packet_stack.codec);
  write dir "bf_constrained.gen"
    (Wire_ocaml.ml ~codec:"Demo.bf_constrained_codec" Demo.bf_constrained_codec);
  write dir "reserved_fields.gen"
    (Wire_ocaml.ml ~codec:"Demo.reserved_fields_codec"
       Demo.reserved_fields_codec)
