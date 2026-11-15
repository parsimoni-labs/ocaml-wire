open Hardcaml
module S = Signal
module H = Wire_hardcaml

let mask count =
  S.concat_lsb (List.init 64 (fun i -> S.repeat S.(count >:. i) 8))

let circuit () =
  let left = S.input "left" 512 and right = S.input "right" 512 in
  let length = S.input "length" 7 in
  let plan = Wire_ocaml.Plan.v Packet_stack.codec in
  let a = H.v ~capacity:64 ~data:left ~length plan in
  let b = H.v ~capacity:64 ~data:right ~length plan in
  let same_prefix count =
    let mask = mask count in
    S.(left &: mask ==: (right &: mask))
  in
  let same_result = Properties.same_result a b in
  let physical = S.(~:(same_prefix length) |: same_result) in
  let prefix_assumption = S.(H.accepted a &: same_prefix (H.consumed a)) in
  let trailing = S.(~:prefix_assumption |: same_result) in
  Circuit.create_exn ~name:"wire_padding_noninterference"
    [
      S.output "physical_padding_ok" physical;
      S.output "trailing_bytes_ok" trailing;
      S.output "proof_ok" S.(physical &: trailing);
    ]
