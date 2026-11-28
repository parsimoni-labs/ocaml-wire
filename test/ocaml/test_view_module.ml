(* The views a scanner writes: a check and an accessor per field, a module
   per record a field holds. Each field and each sub-record against the codec,
   then the accessors on bytes written after the check, which they read again
   and never past the buffer. *)

open Wire
module Buf = Wire_runtime.Buf

(* An Ethernet frame of 54 bytes holding IPv4 and TCP: 10.0.0.1:1234 to
   10.0.0.2:80, unfragmented. *)
let capture_frame () =
  let b = Bytes.make 54 '\000' in
  Bytes.set_uint16_be b 12 0x0800;
  Bytes.set_uint8 b 14 0x45;
  Bytes.set_uint16_be b 16 40;
  Bytes.set_uint16_be b 18 0x1234;
  Bytes.set_uint16_be b 20 0x4000;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 6;
  Bytes.set_int32_be b 26 0x0a000001l;
  Bytes.set_int32_be b 30 0x0a000002l;
  Bytes.set_uint16_be b 34 1234;
  Bytes.set_uint16_be b 36 80;
  Bytes.set_uint8 b 46 0x50;
  b

module C = S_capture

let ports b ip =
  let t = C.Captured_ipv4.tcp b ip in
  if C.Stack_tcp.is_ok t then
    let t = C.Stack_tcp.get t in
    Some
      ( UInt16.to_int (C.Stack_tcp.src_port b t),
        UInt16.to_int (C.Stack_tcp.dst_port b t) )
  else None

let test_views () =
  let frame = capture_frame () in
  let p =
    match Codec.decode Packet_stack.capture frame ~off:0 with
    | Ok p -> p
    | Error e -> Alcotest.failf "the frame: %a" Error.pp e
  in
  let ip = Option.get p.ipv4 and tcp = Option.get (Option.get p.ipv4).tcp in
  match Buf.v frame with
  | Buf.Any b -> (
      let c = C.check b (Buf.length b) ~off:0 ~len:54 in
      Alcotest.(check bool) "accepted" true (C.is_ok c);
      let v = C.get c in
      Alcotest.(check int)
        "ether_type"
        (UInt16.to_int p.ether_type)
        (UInt16.to_int (C.ether_type b v));
      Alcotest.(check bool) "no ipv6" false (C.Stack_ipv6.is_ok (C.ipv6 b v));
      let k = C.ipv4 b v in
      Alcotest.(check bool) "ipv4" true (C.Captured_ipv4.is_ok k);
      let i = C.Captured_ipv4.get k in
      Alcotest.(check int)
        "src" (UInt32.to_int ip.src)
        (UInt32.to_int (C.Captured_ipv4.src b i));
      Alcotest.(check int)
        "dst" (UInt32.to_int ip.dst)
        (UInt32.to_int (C.Captured_ipv4.dst b i));
      Alcotest.(check int)
        "total_length"
        (UInt16.to_int ip.total_length)
        (UInt16.to_int (C.Captured_ipv4.total_length b i));
      Alcotest.(check (option (pair int int)))
        "ports"
        (Some (UInt16.to_int tcp.src_port, UInt16.to_int tcp.dst_port))
        (ports b i);
      Alcotest.(check bool)
        "no udp" false
        (C.Stack_udp_header.is_ok (C.Captured_ipv4.udp b i));
      Alcotest.(check bool)
        "short frame" false
        (C.is_ok (C.check b (Buf.length b) ~off:0 ~len:53));
      Alcotest.(check bool)
        "past the buffer" false
        (C.is_ok (C.check b (Buf.length b) ~off:1 ~len:54));
      Alcotest.check_raises "get of a failed check"
        (Invalid_argument "CapturedStack.get: the check failed") (fun () ->
          ignore (C.get (C.check b (Buf.length b) ~off:(-1) ~len:54)));
      (* A setter of a nested record writes it in place, as the codec reads
         it back, and the bitfields beside it keep their bits. *)
      let t = C.Stack_tcp.get (C.Captured_ipv4.tcp b i) in
      C.Stack_tcp.set_src_port b t (UInt16.v 4321);
      C.Stack_tcp.set_reserved b t 5;
      C.Captured_ipv4.set_ttl b i (UInt8.v 9);
      match Codec.decode Packet_stack.capture frame ~off:0 with
      | Error e -> Alcotest.failf "after the writes: %a" Error.pp e
      | Ok p' ->
          let ip' = Option.get p'.ipv4 in
          let tcp' = Option.get ip'.tcp in
          Alcotest.(check int) "src_port" 4321 (UInt16.to_int tcp'.src_port);
          Alcotest.(check int) "reserved" 5 tcp'.reserved;
          Alcotest.(check int) "data_offset" tcp.data_offset tcp'.data_offset;
          Alcotest.(check int) "ttl" 9 (UInt8.to_int ip'.ttl);
          Alcotest.(check int)
            "dst_port"
            (UInt16.to_int tcp.dst_port)
            (UInt16.to_int tcp'.dst_port))

(* What a test expects of the views of a frame. *)
type expect = { e : 'b. string -> 'b Buf.t -> 'b C.view -> unit }

(* A sub-record whose bytes change after the check is placed again, and
   refused where it no longer lies in the buffer: no read leaves it. *)
let test_views_rewritten () =
  let after name write { e = expect } =
    let frame = capture_frame () in
    match Buf.v frame with
    | Buf.Any b ->
        let c = C.check b (Buf.length b) ~off:0 ~len:54 in
        Alcotest.(check bool) (name ^ ": accepted") true (C.is_ok c);
        let v = C.get c in
        write frame;
        expect name b v
  in
  let refused name b v =
    let k = C.ipv4 b v in
    Alcotest.(check bool) (name ^ ": ipv4") true (C.Captured_ipv4.is_ok k);
    Alcotest.(check (option (pair int int)))
      (name ^ ": tcp") None
      (ports b (C.Captured_ipv4.get k))
  in
  let refused = { e = refused } in
  after "ihl 15" (fun f -> Bytes.set_uint8 f 14 0x4f) refused;
  after "ihl 4" (fun f -> Bytes.set_uint8 f 14 0x44) refused;
  after "fragment" (fun f -> Bytes.set_uint16_be f 20 0x2000) refused;
  after "udp"
    (fun f -> Bytes.set_uint8 f 23 17)
    {
      e =
        (fun name b v ->
          refused.e name b v;
          let i = C.Captured_ipv4.get (C.ipv4 b v) in
          let u = C.Captured_ipv4.udp b i in
          Alcotest.(check bool)
            (name ^ ": udp") true
            (C.Stack_udp_header.is_ok u);
          Alcotest.(check int)
            (name ^ ": udp src_port") 1234
            (UInt16.to_int
               (C.Stack_udp_header.src_port b (C.Stack_udp_header.get u))));
    };
  after "ipv6"
    (fun f ->
      Bytes.set_uint16_be f 12 0x86dd;
      Bytes.set_uint8 f 20 6)
    {
      e =
        (fun name b v ->
          Alcotest.(check bool)
            (name ^ ": ipv4") false
            (C.Captured_ipv4.is_ok (C.ipv4 b v));
          let k = C.ipv6 b v in
          Alcotest.(check bool) (name ^ ": ipv6") true (C.Stack_ipv6.is_ok k);
          Alcotest.(check bool)
            (name ^ ": tcp past the buffer")
            false
            (C.Stack_tcp.is_ok (C.Stack_ipv6.tcp b (C.Stack_ipv6.get k))));
    }

(* A field after a span is placed again from the span's size, and reads as
   absent rather than past the buffer when that size has grown: an accessor
   never raises. *)
let test_views_moved_field () =
  let buf = Bytes.make 9 '\000' in
  Bytes.set_int32_be buf 0 0x0002_0000l;
  Bytes.set_uint8 buf 8 7;
  match Buf.v buf with
  | Buf.Any b ->
      let c = S_sized.check b (Buf.length b) ~off:0 ~len:9 in
      Alcotest.(check bool) "accepted" true (S_sized.is_ok c);
      let v = S_sized.get c in
      Alcotest.(check bool) "body" true (S_sized.body_present b v);
      Alcotest.(check int) "tail" 7 (UInt8.to_int (S_sized.tail b v));
      Bytes.set_int32_be buf 0 0x0001_0000l;
      Alcotest.(check bool) "no body" false (S_sized.body_present b v);
      Alcotest.(check int)
        "tail without the body" 0
        (UInt8.to_int (S_sized.tail b v));
      Bytes.set_int32_be buf 0 0x0010_0000l;
      Alcotest.(check bool)
        "a body past the buffer" true (S_sized.body_present b v);
      Alcotest.(check int)
        "a body past the buffer reads empty" 0
        (Bytesrw.Bytes.Slice.length (S_sized.body b v));
      Alcotest.(check int)
        "a tail past the buffer reads absent" 0
        (UInt8.to_int (S_sized.tail b v))

let suite =
  ( "view_module",
    [
      Alcotest.test_case "views of a capture" `Quick test_views;
      Alcotest.test_case "views of bytes written after the check" `Quick
        test_views_rewritten;
      Alcotest.test_case "a field placed again" `Quick test_views_moved_field;
    ] )
