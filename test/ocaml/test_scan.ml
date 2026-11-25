(* The module generated from [Packet_stack.capture] and its interpreted twin
   fold [Scan_step] alike, on a corpus of frames. *)

(* The swap: one line picks the module, the code under it is the same. *)
module Generated = S_capture_step

module Interpreted = S_capture_step_codec.Make (struct
  type r = Packet_stack.capture

  let codec = Packet_stack.capture
end)

module Flows (S : sig
  val fold_step : bytes -> int -> int -> init:Scan_step.t -> Scan_step.t

  val fold_step_frames :
    bytes -> int -> int -> stride:int -> init:Scan_step.t -> Scan_step.t

  val fold_step_index :
    bytes -> offs:int array -> lens:int array -> init:Scan_step.t -> Scan_step.t
end) =
struct
  let index buf ~offs ~lens = S.fold_step_index buf ~offs ~lens ~init:[]

  let frames buf ~stride =
    S.fold_step_frames buf 0 (Bytes.length buf) ~stride ~init:[]

  let one buf off len = S.fold_step buf off len ~init:[]
end

(* {1 The corpus} *)

let hex s =
  let s = String.concat "" (String.split_on_char ' ' s) in
  Bytes.init
    (String.length s / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub s (2 * i) 2)))

let eth ty = "000102030405 060708090a0b " ^ ty

(* IPv4 header of 20 bytes with protocol [p], flags and offset [fo]. *)
let ipv4 ?(fo = "0000") p =
  "45000028 1234 " ^ fo ^ " 40" ^ p ^ "0000 c0a80001 0a000002"

let tcp = "1f90 0050 00000001 00000002 5012 ffff 0000 0000"
let udp = "0035 d431 001c 0000"

let ipv6 nh =
  "60000000 0014 " ^ nh
  ^ "40 20010db8000000000000000000000001 20010db80000000000000000deadbeef"

let frames =
  List.map hex
    [
      eth "0800" ^ " " ^ ipv4 "06" ^ " " ^ tcp;
      eth "0800" ^ " " ^ ipv4 "11" ^ " " ^ udp;
      eth "0800" ^ " " ^ ipv4 ~fo:"2001" "06" ^ " " ^ tcp;
      eth "0800" ^ " " ^ ipv4 "01" ^ " 0800f7ff00000000";
      eth "86dd" ^ " " ^ ipv6 "06" ^ " " ^ tcp;
      eth "86dd" ^ " " ^ ipv6 "11" ^ " " ^ udp;
      eth "86dd" ^ " " ^ ipv6 "3a" ^ " 8000000000000000";
      eth "0806" ^ " 0001080006040001";
      eth "0800" ^ " 46000028";
    ]
  |> List.map (fun f ->
      let b = Bytes.make 64 '\000' in
      Bytes.blit f 0 b 0 (min 64 (Bytes.length f));
      b)

(* Each frame, and copies with one byte flipped, cut short or both. *)
let corpus =
  let rand = Random.State.make [| 7 |] in
  List.concat_map
    (fun f ->
      f
      :: List.init 40 (fun _ ->
          let g = Bytes.copy f in
          let i = Random.State.int rand 64 in
          Bytes.set g i (Char.chr (Random.State.int rand 256));
          g))
    frames

let stride = 64
let buf = Bytes.concat Bytes.empty corpus
let n = List.length corpus

let lens =
  let rand = Random.State.make [| 9 |] in
  Array.init n (fun _ -> Random.State.int rand (stride + 1))

let offs = Array.init n (fun i -> i * stride)

let test_same () =
  let module G = Flows (Generated) in
  let module I = Flows (Interpreted) in
  let seen = Alcotest.(list string) in
  Alcotest.check seen "fold_step_index" (G.index buf ~offs ~lens)
    (I.index buf ~offs ~lens);
  Alcotest.check seen "fold_step_frames" (G.frames buf ~stride)
    (I.frames buf ~stride);
  List.iter
    (fun (off, len) ->
      Alcotest.check seen
        (Fmt.str "fold_step %d %d" off len)
        (G.one buf off len) (I.one buf off len))
    [ (0, 64); (-1, 64); (0, -1); (Bytes.length buf - 10, 20); (64, 0) ];
  let valid = List.filter (( <> ) "invalid") (G.frames buf ~stride) in
  let kinds = List.length (List.sort_uniq String.compare valid) in
  if kinds < 8 then Alcotest.failf "only %d kinds of valid frames" kinds

let suite =
  ( "scan",
    [
      Alcotest.test_case "generated and interpreted, one signature" `Quick
        test_same;
    ] )
