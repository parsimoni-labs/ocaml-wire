(* Writes s_capture_step.ml and its interface, the module of
   [Packet_stack.capture] whose fold [fold_step] hands out [Scan_step.fields]
   to the visitor [Scan_step], and s_capture_step_codec.ml and its interface,
   its interpreted twin, for the compiler that builds this generator. *)

let () =
  Wire_ocaml.(
    main
      [
        v ~name:"s_capture_step"
          ~folds:[ fold ~outputs:Scan_step.fields "fold_step" ]
          Packet_stack.capture;
      ])
