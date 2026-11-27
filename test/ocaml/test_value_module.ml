(* The values of the module of a structured codec, against the codec: its
   record, decoded through the views, encoded back and measured, its error
   on every prefix of a frame and on random bytes, and its printer. *)

open Wire

let rand = Random.State.make [| 13 |]

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* An Ethernet frame of 54 bytes holding IPv4 and TCP. *)
let capture_frame () =
  let b = Bytes.make 54 '\000' in
  Bytes.set_uint16_be b 12 0x0800;
  Bytes.set_uint8 b 14 0x45;
  Bytes.set_uint16_be b 16 40;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 6;
  Bytes.set_int32_be b 26 0x0a000001l;
  Bytes.set_int32_be b 30 0x0a000002l;
  Bytes.set_uint16_be b 34 1234;
  Bytes.set_uint16_be b 36 80;
  Bytes.set_uint8 b 46 0x50;
  b

let slice = Bytesrw.Bytes.Slice.to_string

(* The module of a structured codec decodes its value through its views, as
   the codec does, encodes it back to the same bytes and measures them, and
   answers the codec's error on every prefix of the frame. *)
let test_values () =
  let frame = capture_frame () in
  let p = Result.get_ok (Codec.decode Packet_stack.capture frame 0) in
  let r = Result.get_ok (S_capture.decode frame 0) in
  Alcotest.(check string) "dst" (slice p.dst) (slice r.dst);
  Alcotest.(check int)
    "ether_type"
    (UInt16.to_int p.ether_type)
    (UInt16.to_int r.ether_type);
  let ip = Option.get r.ipv4 and ip' = Option.get p.ipv4 in
  Alcotest.(check int) "src" (UInt32.to_int ip'.src) (UInt32.to_int ip.src);
  let tcp = Option.get ip.tcp in
  Alcotest.(check int) "data_offset" 5 tcp.data_offset;
  Alcotest.(check bool) "no ipv6" true (Option.is_none r.ipv6);
  Alcotest.(check int) "size" 54 (S_capture.size r);
  let out = Bytes.make 54 '\255' in
  S_capture.encode r out 0;
  Alcotest.(check bytes) "encode" frame out;
  for n = 0 to 53 do
    let short = Bytes.sub frame 0 n in
    Alcotest.(check bool)
      (Fmt.str "validate %d bytes" n)
      true
      (Result.equal
         ~ok:(fun () () -> true)
         ~error:Error.equal
         (Codec.validate Packet_stack.capture short 0)
         (S_capture.validate short 0))
  done;
  (* A record whose values break its sizes is refused, as the codec's
     encoder refuses it. *)
  let bad = { r with ipv4 = Some { ip with ihl = 4 } } in
  match S_capture.encode bad (Bytes.create 54) 0 with
  | () -> Alcotest.fail "encode: an IHL of 4"
  | exception Invalid_argument _ -> ()

(* Bad bytes never raise from decode or validate: they answer the codec's
   error. *)
let test_bad_input () =
  for _ = 1 to 5000 do
    let n = Random.State.int rand 80 in
    let b = Bytes.init n (fun _ -> Char.chr (Random.State.int rand 256)) in
    if n > 14 && Random.State.bool rand then Bytes.set_uint16_be b 12 0x0800;
    if n > 15 && Random.State.bool rand then Bytes.set_uint8 b 14 0x45;
    let off = Random.State.int rand (n + 2) in
    let want = Codec.decode Packet_stack.capture b off in
    match S_capture.decode b off with
    | Ok _ when Result.is_ok want -> ()
    | Error e
      when Result.equal ~ok:(fun _ _ -> false) ~error:Error.equal want (Error e)
      ->
        ()
    | Ok _ | Error _ ->
        Alcotest.failf "decode disagrees on %S at %d" (Bytes.to_string b) off
    | exception exn when Wire.Private.Types.is_parse_error exn ->
        let e = Wire.Private.Types.parse_error_of exn in
        Alcotest.failf "decode raised %a on %S" Error.pp e (Bytes.to_string b)
    | exception Invalid_argument m ->
        Alcotest.failf "decode raised Invalid_argument %s on %S" m
          (Bytes.to_string b)
  done

(* The printer shows the record as the codec decodes it, its slices by their
   bytes. *)
let test_pp () =
  let r = Result.get_ok (S_capture.decode (capture_frame ()) 0) in
  let s = Fmt.str "%a" S_capture.pp r in
  List.iter
    (fun sub -> Alcotest.(check bool) sub true (contains s sub))
    [ "ether_type = 2048"; "ipv6 = None"; "data_offset = 5"; "dst_port = 80" ]

let suite =
  ( "value_module",
    [
      Alcotest.test_case "values of a structured codec" `Quick test_values;
      Alcotest.test_case "bad input never raises" `Quick test_bad_input;
      Alcotest.test_case "printer" `Quick test_pp;
    ] )
