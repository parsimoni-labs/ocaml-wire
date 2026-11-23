open Hardcaml
module S = Signal
module H = Wire_hardcaml

let circuit () =
  let data = S.input "data" 512 and length = S.input "length" 7 in
  let clock = S.input "clock" 1 and reset = S.input "reset" 1 in
  let enable = S.input "enable" 1 in
  let spec = Reg_spec.create () ~clock in
  let plan = Wire_ocaml.Plan.v Packet_stack.codec in
  let reference =
    H.v ~capacity:64 ~data ~length plan |> H.register ~enable spec
  in
  let candidate =
    H.v ~pipeline:(spec, enable) ~capacity:64 ~data ~length plan
  in
  let seen = S.reg (Reg_spec.create ~clear:reset () ~clock) ~enable S.vdd in
  let same_result = Properties.same_result reference candidate in
  Circuit.create_exn ~name:"wire_pipeline_equivalence"
    [ S.output "proof_ok" S.(reset |: ~:seen |: same_result) ]
