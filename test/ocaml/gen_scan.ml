(* Writes s_capture_step.ml and its interface: the scanner of
   [Packet_stack.capture] handing out [Scan_step.fields] to the consumer
   [Scan_step]. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let () =
  let c = Packet_stack.capture and codec = "Packet_stack.capture" in
  let fields = Scan_step.fields and step = "Scan_step" in
  write "s_capture_step.ml" (Wire_ocaml.scanner ~fields ~step ~codec c);
  write "s_capture_step.mli" (Wire_ocaml.scanner_mli ~fields ~step ~codec c)
