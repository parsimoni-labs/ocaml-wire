(* Writes s_capture_step.ml and its interface, the scanner of
   [Packet_stack.capture] handing out [Scan_step.fields] to the consumer
   [Scan_step], and s_capture_step_wire.ml and its interface, its bridge. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

(* [--oxcaml] generates the scanner for the OxCaml target. *)
let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  let c = Packet_stack.capture and codec = "Packet_stack.capture" in
  let fields = Scan_step.fields and step = "Scan_step" in
  write "s_capture_step.ml" (Wire_ocaml.scanner ~target ~fields ~step ~codec c);
  write "s_capture_step.mli"
    (Wire_ocaml.scanner_mli ~target ~fields ~step ~codec c);
  let core = "S_capture_step" in
  write "s_capture_step_wire.ml"
    (Wire_ocaml.scanner_bridge ~target ~fields ~codec ~core c);
  write "s_capture_step_wire.mli"
    (Wire_ocaml.scanner_bridge_mli ~target ~fields ~codec ~core c)
