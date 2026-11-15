open Hardcaml
module S = Signal
module H = Wire_hardcaml

let ports plan direction xs =
  List.map
    (fun x ->
      let names = S.names x in
      match names with
      | [ name ] ->
          let wire_width, signed =
            if
              String.equal direction "output"
              && String.starts_with ~prefix:"field_" name
            then
              let field_name = String.sub name 6 (String.length name - 6) in
              let field =
                List.find
                  (fun (o : Wire_ocaml.Plan.output) ->
                    String.equal o.name field_name)
                  (Wire_ocaml.Plan.outputs plan)
              in
              (field.wire_width, field.signed)
            else if
              String.equal name "consumed" || String.equal name "frame_length"
            then (17, false)
            else (S.width x, false)
          in
          Fmt.str
            "{\"name\":%S,\"width\":%d,\"direction\":%S,\"wire_width\":%d,\"signed\":%b}"
            name (S.width x) direction wire_width signed
      | _ -> invalid_arg "noncanonical top-level circuit port")
    xs

let stack_circuit packing =
  let plan = Wire_ocaml.Plan.v Packet_stack.codec in
  let stream =
    H.Stream.v ~packing ~capacity:128 ~max_length:65549
      ~clock:(S.input "clock" 1) ~reset:(S.input "reset" 1)
      ~valid:(S.input "input_valid" 1) ~data:(S.input "input_data" 512)
      ~keep:(S.input "input_keep" 64) ~last:(S.input "input_last" 1)
      ~error:(S.input "input_error" 1) ~output_ready:(S.input "output_ready" 1)
      plan
  in
  let name = "wire_stack_stream_64" in
  let circuit = H.Stream.to_circuit ~name stream in
  (plan, stream, circuit)

let generate () =
  let packing, args =
    match Array.to_list Sys.argv with
    | _ :: "--contiguous" :: args -> (H.Stream.Contiguous, args)
    | _ :: args -> (H.Stream.Full_beats, args)
    | [] -> assert false
  in
  let file, manifest =
    match args with
    | [ file; manifest ] -> (file, manifest)
    | _ -> invalid_arg "usage: gen.exe [--contiguous] OUTPUT.v MANIFEST.json"
  in
  let plan, stream, circuit = stack_circuit packing in
  let name = Circuit.name circuit in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_file file) Rtl.Language.Verilog
    circuit;
  let ports =
    ports plan "input" (Circuit.inputs circuit)
    @ ports plan "output" (Circuit.outputs circuit)
  in
  Out_channel.with_open_text manifest (fun ch ->
      let ppf = Format.formatter_of_out_channel ch in
      Fmt.pf ppf
        "{\"module\":%S,\"capture_bytes\":128,\"max_length\":65549,\"read_prefix\":%d,\"latency_cycles\":3,\"packing\":%S,\"ports\":[%s]}@."
        name
        (H.read_prefix (H.Stream.decoder stream))
        (match packing with
        | H.Stream.Full_beats -> "full_nonfinal"
        | H.Stream.Contiguous -> "contiguous")
        (String.concat "," ports))

let () =
  match Array.to_list Sys.argv with
  | [ _; "--unnamed"; file ] ->
      let _, _, circuit = stack_circuit H.Stream.Full_beats in
      Signal_graph.iter (Circuit.signal_graph circuit) ~f:(fun signal ->
          if
            not
              (Circuit.is_input circuit signal
              || Circuit.is_output circuit signal)
          then S.Type.set_names signal []);
      Rtl.output ~output_mode:(Rtl.Output_mode.To_file file)
        Rtl.Language.Verilog circuit
  | [ _; ("--keep-reference" | "--keep-predicate"); file ] ->
      Rtl.output ~output_mode:(Rtl.Output_mode.To_file file)
        Rtl.Language.Verilog
        (Stream_properties.keep_predicate
           ~reference:(String.equal Sys.argv.(1) "--keep-reference"))
  | [ _; "--keep-proof"; file ] ->
      Rtl.output ~output_mode:(Rtl.Output_mode.To_file file)
        Rtl.Language.Verilog
        (Stream_properties.keep_equivalence ())
  | [ _; "--proof"; file ] | [ _; "--proof-contiguous"; file ] ->
      let packing =
        if String.equal Sys.argv.(1) "--proof" then H.Stream.Full_beats
        else H.Stream.Contiguous
      in
      Rtl.output ~output_mode:(Rtl.Output_mode.To_file file)
        Rtl.Language.Verilog
        (Stream_properties.circuit packing)
  | _ -> generate ()
