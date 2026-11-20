open Hardcaml
module S = Signal
module H = Wire_hardcaml

let word n = S.of_int ~width:64 n
let contribution_width = 35
let part n = S.of_int ~width:contribution_width n

let rec sum = function
  | [] -> part 0
  | [ x ] -> x
  | values ->
      let middle = List.length values / 2 in
      let left = List.filteri (fun i _ -> i < middle) values in
      let right = List.filteri (fun i _ -> i >= middle) values in
      S.(sum left +: sum right)

let contribution decoder =
  (* Absent fields are zero, and the codec dispatch makes TCP/UDP exclusive.
     One balanced sum therefore implements the same consumer. Scalar storage
     widths bound even all terms together below 2^35. *)
  H.reduce decoder ~f:(fun fields ->
      let value name = S.uresize (List.assoc name fields) contribution_width in
      let ports proto =
        S.(sll (value (proto ^ "_src_port")) 16 |: value (proto ^ "_dst_port"))
      in
      sum
        [
          value "ether_type";
          S.(value "ipv4_src" ^: value "ipv4_dst");
          value "ipv4_ttl";
          value "ipv4_protocol";
          value "ipv4_total_length";
          ports "ipv4_tcp";
          S.(value "ipv4_tcp_seq" ^: value "ipv4_tcp_ack");
          value "ipv4_tcp_window";
          value "ipv4_tcp_flags";
          value "ipv4_tcp_data_offset";
          ports "ipv4_udp";
          value "ipv4_udp_length";
        ])

(* The checksum sink: the masked running sum and the frame count, advanced on
   each output transfer. *)
let sink spec ~transfer packet_sum =
  let total =
    S.reg_fb spec ~enable:transfer ~width:64 ~f:(fun total ->
        S.(total +: packet_sum))
  in
  let frames =
    S.reg_fb spec ~enable:transfer ~width:64 ~f:(fun frames ->
        S.(frames +: word 1))
  in
  (S.(total &: of_int64 ~width:64 0x3fffffffffffffffL), frames)

let circuit () =
  let clock = S.input "clock" 1 and reset = S.input "reset" 1 in
  let input_valid = S.input "input_valid" 1
  and output_ready = S.input "output_ready" 1 in
  let input_data = S.input "input_data" 512
  and input_length = S.input "input_length" 7 in
  let valid_spec = Reg_spec.create ~clear:reset () ~clock in
  let data_spec = Reg_spec.create () ~clock in
  let output_valid = S.wire 1 in
  let advance = S.(~:output_valid |: output_ready) in
  let ready = S.(advance &: ~:reset) in
  (* Input, load, decode and result registers: four enabled edges. *)
  S.assign output_valid
    (List.fold_left
       (fun valid _ -> S.reg valid_spec ~enable:advance valid)
       input_valid [ (); (); (); () ]);
  let data = S.reg data_spec ~enable:advance input_data in
  let length = S.reg data_spec ~enable:advance input_length in
  let decoder =
    H.v ~pipeline:(data_spec, advance) ~capacity:64 ~data ~length
      (Wire_ocaml.Plan.v Packet_stack.codec)
  in
  let decoder = H.register data_spec ~enable:advance decoder in
  let registered value = S.reg data_spec ~enable:advance value in
  let scope = registered (H.in_scope decoder) in
  let accepted = registered (H.accepted decoder) in
  let consumed = registered (H.consumed decoder) in
  let packet_sum =
    registered (contribution decoder) |> fun x -> S.uresize x 64
  in
  let visible_valid = S.(output_valid &: ~:reset) in
  let transfer = S.(visible_valid &: output_ready) in
  let total, frames = sink valid_spec ~transfer packet_sum in
  Circuit.create_exn ~name:"wire_stack_64"
    [
      S.output "input_ready" ready;
      S.output "output_valid" visible_valid;
      S.output "output_scope" scope;
      S.output "output_accepted" accepted;
      S.output "output_consumed" consumed;
      S.output "output_contribution" packet_sum;
      S.output "checksum" total;
      S.output "frames" frames;
    ]
