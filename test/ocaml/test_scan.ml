(* The fold of the module generated from [Packet_stack.capture] hands its
   [f] what the codec decodes, frame by frame, on a corpus of frames: in an
   index, in slots and one frame at a time. *)

open Wire

(* The fields the module's fold hands out, as the generator names them. *)
let show proto src dst tcp udp next dst3 =
  let o f = function Some x -> string_of_int (f x) | None -> "-" in
  String.concat " "
    [
      o UInt8.to_int proto;
      o UInt32.to_int src;
      o UInt32.to_int dst;
      o UInt16.to_int tcp;
      o UInt16.to_int udp;
      o UInt8.to_int next;
      o UInt32.to_int dst3;
    ]

(* The same fields of the record the codec decodes from the frame alone, or
   ["invalid"]. *)
let decoded buf off len =
  if off < 0 || len < 0 || off > Bytes.length buf - len then "invalid"
  else
    match Codec.decode Packet_stack.capture (Bytes.sub buf off len) ~off:0 with
    | Error _ -> "invalid"
    | Ok (r : Packet_stack.capture) ->
        let ipv4 f = Option.map f r.ipv4 and ipv6 f = Option.map f r.ipv6 in
        let ( >>= ) = Option.bind in
        show
          (ipv4 (fun (i : Packet_stack.captured_ipv4) -> i.protocol))
          (ipv4 (fun (i : Packet_stack.captured_ipv4) -> i.src))
          (ipv4 (fun (i : Packet_stack.captured_ipv4) -> i.dst))
          ( r.ipv4 >>= fun (i : Packet_stack.captured_ipv4) ->
            Option.map (fun (t : Packet_stack.tcp) -> t.dst_port) i.tcp )
          ( r.ipv4 >>= fun (i : Packet_stack.captured_ipv4) ->
            Option.map (fun (u : Packet_stack.udp_header) -> u.dst_port) i.udp
          )
          (ipv6 (fun (i : Packet_stack.ipv6) -> i.next_header))
          (ipv6 (fun (i : Packet_stack.ipv6) -> i.dst3))

let invalid acc = "invalid" :: acc
let f acc a b c d e g h = show a b c d e g h :: acc

let index buf ~offs ~lens =
  match S_capture_step.fold buf ~offs ~lens ~invalid ~init:[] ~f with
  | Ok l -> List.rev l
  | Error _ -> Alcotest.fail "index: an error with invalid"

let slots buf ~stride =
  match S_capture_step.fold buf ~stride ~invalid ~init:[] ~f with
  | Ok l -> List.rev l
  | Error _ -> Alcotest.fail "slots: an error with invalid"

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
  let seen = Alcotest.(list string) in
  Alcotest.check seen "index"
    (List.init n (fun i -> decoded buf offs.(i) lens.(i)))
    (index buf ~offs ~lens);
  Alcotest.check seen "slots"
    (List.init n (fun i -> decoded buf (i * stride) stride))
    (slots buf ~stride);
  List.iter
    (fun (off, len) ->
      Alcotest.check seen
        (Fmt.str "frame %d %d" off len)
        [ decoded buf off len ]
        (index buf ~offs:[| off |] ~lens:[| len |]))
    [ (0, 64); (-1, 64); (0, -1); (Bytes.length buf - 10, 20); (64, 0) ];
  let valid = List.filter (( <> ) "invalid") (slots buf ~stride) in
  let kinds = List.length (List.sort_uniq String.compare valid) in
  if kinds < 8 then Alcotest.failf "only %d kinds of valid frames" kinds

let suite =
  ("scan", [ Alcotest.test_case "the fold against the codec" `Quick test_same ])
