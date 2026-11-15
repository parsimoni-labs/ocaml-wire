let write circuit file =
  Hardcaml.Rtl.output ~output_mode:(Hardcaml.Rtl.Output_mode.To_file file)
    Hardcaml.Rtl.Language.Verilog circuit

let decoder_value () =
  let data = Hardcaml.Signal.input "data" 512 in
  let length = Hardcaml.Signal.input "length" 7 in
  Wire_hardcaml.v ~capacity:64 ~data ~length
    (Wire_ocaml.Plan.v Packet_stack.codec)

let decoder () =
  Wire_hardcaml.to_circuit ~name:"wire_decoder_64" (decoder_value ())

let checksum () =
  let value = Hw_bench.contribution (decoder_value ()) in
  Hardcaml.Circuit.create_exn ~name:"wire_checksum_64"
    [ Hardcaml.Signal.output "contribution" (Hardcaml.Signal.uresize value 64) ]

let () =
  match Array.to_list Sys.argv with
  | [ _; file ] -> write (Hw_bench.circuit ()) file
  | [ _; "--padding"; file ] -> write (Padding.circuit ()) file
  | [ _; "--decoder"; file ] -> write (decoder ()) file
  | [ _; "--pipeline-proof"; file ] -> write (Validation_miter.circuit ()) file
  | [ _; "--checksum"; file ] -> write (checksum ()) file
  | _ ->
      invalid_arg
        "usage: gen.exe [--padding|--decoder|--pipeline-proof|--checksum] \
         OUTPUT.v"
