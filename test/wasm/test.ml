(* Smoke test for the paths that differ between a 64-bit host and a
   narrow-int target (wasm_of_ocaml ints are 31-bit, js_of_ocaml 32-bit).
   The same binary runs natively and under wasm (see ./dune), so every check
   must hold on both: constants with bit 31 set are written as [Int32]
   literals, never as int literals that would themselves truncate on the
   target this test exists for. *)

open Wire
module UInt32 = Wire.Private.UInt32

let failures = ref 0

let check name ok =
  if not ok then (
    Fmt.epr "FAIL: %s@." name;
    incr failures)

let test_uint32 () =
  check "mask32 keeps the low 32 bits"
    (Int64.equal
       (Int64.logand (Int64.of_int UInt32.mask32) 0xFFFF_FFFFL)
       0xFFFF_FFFFL);
  check "of_int (-1) is the u32 all-ones"
    (Int32.equal (UInt32.to_int32 (UInt32.of_int (-1))) (-1l));
  let v = UInt32.of_int32 0xA695_853Bl in
  let buf = Bytes.create 4 in
  UInt32.set_be buf 0 v;
  check "set_be writes big-endian bytes"
    (String.equal (Bytes.to_string buf) "\xA6\x95\x85\x3B");
  check "be keeps bit 31"
    (Int32.equal (UInt32.to_int32 (UInt32.be buf 0)) 0xA695_853Bl);
  UInt32.set_le buf 0 v;
  check "le roundtrip keeps bit 31"
    (Int32.equal (UInt32.to_int32 (UInt32.le buf 0)) 0xA695_853Bl)

(* A hand-built Ethernet/IPv4/TCP frame (the layout of [Net.tcp_frame_data],
   which is not reused here because [Net.ipv4_addr] packs into a native int). *)
let frame =
  let b = Bytes.create Net.ethernet_size in
  Bytes.blit_string "\x00\x11\x22\x33\x44\x55\xAA\xBB\xCC\xDD\xEE\xFF" 0 b 0 12;
  Bytes.set_uint16_be b 12 0x0800;
  (* IPv4 at 14 *)
  Bytes.set_uint8 b 14 0x45;
  Bytes.set_uint8 b 15 0x00;
  Bytes.set_uint16_be b 16 40;
  Bytes.set_uint16_be b 18 0x1234;
  Bytes.set_uint16_be b 20 0x4000;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 6;
  Bytes.set_uint16_be b 24 0;
  Bytes.set_int32_be b 26 0xC0A8_0164l (* 192.168.1.100: bit 31 set *);
  Bytes.set_int32_be b 30 0x0A00_0001l (* 10.0.0.1 *);
  (* TCP at 34 *)
  Bytes.set_uint16_be b 34 12345;
  Bytes.set_uint16_be b 36 443;
  Bytes.set_int32_be b 38 0xA695_853Bl (* seq: bit 31 set *);
  Bytes.set_int32_be b 42 0l;
  Bytes.set_uint16_be b 46 0x5012 (* data offset 5, SYN + ACK *);
  Bytes.set_uint16_be b 48 65535;
  b

let get c f = Staged.unstage (Codec.get c f)

let test_zero_copy_get () =
  check "ethernet frame is 54 bytes" (Net.ethernet_size = 54);
  check "ethertype"
    (get Net.ethernet_codec Net.bf_eth_ethertype frame 0 = 0x0800);
  check "ip protocol" (get Net.ipv4_codec Net.bf_ip_protocol frame 14 = 6);
  check "ip src keeps bit 31"
    (Int32.equal
       (Optint.to_int32 (get Net.ipv4_codec Net.bf_ip_src frame 14))
       0xC0A8_0164l);
  check "ip dst"
    (Int32.equal
       (Optint.to_int32 (get Net.ipv4_codec Net.bf_ip_dst frame 14))
       0x0A00_0001l);
  check "tcp src port" (get Net.tcp_codec Net.bf_tcp_src_port frame 34 = 12345);
  check "tcp dst port" (get Net.tcp_codec Net.bf_tcp_dst_port frame 34 = 443);
  check "tcp syn" (get Net.tcp_codec Net.bf_tcp_syn frame 34);
  check "tcp ack" (get Net.tcp_codec Net.bf_tcp_ack frame 34)

let test_roundtrip () =
  let r = Codec.decode_exn Net.ethernet_codec frame 0 in
  let out = Bytes.create Net.ethernet_size in
  Codec.encode Net.ethernet_codec r out 0;
  check "ethernet decode/encode roundtrip" (Bytes.equal out frame)

let () =
  let backend =
    match Sys.backend_type with
    | Native -> "native"
    | Bytecode -> "bytecode"
    | Other s -> s
  in
  test_uint32 ();
  test_zero_copy_get ();
  test_roundtrip ();
  if !failures > 0 then (
    Fmt.epr "%d checks failed (backend=%s int_size=%d)@." !failures backend
      Sys.int_size;
    exit 1);
  Fmt.pr "wasm smoke ok (backend=%s int_size=%d word_size=%d)@." backend
    Sys.int_size Sys.word_size
