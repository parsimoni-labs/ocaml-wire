(* Tests for codec.ml: the packet stack through every decode entry point. *)

open Wire
module Slice = Bytesrw.Bytes.Slice

(* -- The packet stack through every entry point -- *)

(* An Ethernet / IPv4 / TCP-or-UDP stack, every layer bounded by its parent:
   three levels of nested sub-codecs, gated and sized by the fields of the
   layer above. *)
module Packet_stack = struct
  type tcp = {
    src_port : UInt16.t;
    dst_port : UInt16.t;
    seq : UInt32.t;
    ack : UInt32.t;
    data_offset : int;
    reserved : int;
    flags : int;
    window : UInt16.t;
    checksum : UInt16.t;
    urgent : UInt16.t;
    options : Bytesrw.Bytes.Slice.t;
  }

  let data_offset =
    Field.v "data_offset" (bits ~width:4 U16be) ~self_constraint:(fun x ->
        Expr.(x >= int 5))

  let tcp =
    Codec.v "StackTCP"
      (fun src_port dst_port seq ack data_offset reserved flags window checksum
           urgent options ->
        {
          src_port;
          dst_port;
          seq;
          ack;
          data_offset;
          reserved;
          flags;
          window;
          checksum;
          urgent;
          options;
        })
      Codec.
        [
          (Field.v "src_port" uint16be $ fun r -> r.src_port);
          (Field.v "dst_port" uint16be $ fun r -> r.dst_port);
          (Field.v "seq" uint32be $ fun r -> r.seq);
          (Field.v "ack" uint32be $ fun r -> r.ack);
          (data_offset $ fun r -> r.data_offset);
          (Field.v "reserved" (bits ~width:3 U16be) $ fun r -> r.reserved);
          (Field.v "flags" (bits ~width:9 U16be) $ fun r -> r.flags);
          (Field.v "window" uint16be $ fun r -> r.window);
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          (Field.v "urgent" uint16be $ fun r -> r.urgent);
          ( Field.v "options"
              (byte_slice
                 ~size:Expr.((Field.ref data_offset lsl int 2) - int 20))
          $ fun r -> r.options );
        ]

  type udp = {
    src_port : UInt16.t;
    dst_port : UInt16.t;
    length : UInt16.t;
    checksum : UInt16.t;
    payload : Bytesrw.Bytes.Slice.t;
  }

  let udp_length =
    Field.v "length" uint16be ~self_constraint:(fun x -> Expr.(x >= int 8))

  let udp =
    Codec.v "StackUDP"
      (fun src_port dst_port length checksum payload ->
        { src_port; dst_port; length; checksum; payload })
      Codec.
        [
          (Field.v "src_port" uint16be $ fun r -> r.src_port);
          (Field.v "dst_port" uint16be $ fun r -> r.dst_port);
          (udp_length $ fun r -> r.length);
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          ( Field.v "payload"
              (byte_slice ~size:Expr.(Field.ref udp_length - int 8))
          $ fun r -> r.payload );
        ]

  type ipv4 = {
    version : int;
    ihl : int;
    dscp : int;
    ecn : int;
    total_length : UInt16.t;
    identification : UInt16.t;
    flags : int;
    fragment_offset : int;
    ttl : UInt8.t;
    protocol : UInt8.t;
    checksum : UInt16.t;
    src : UInt32.t;
    dst : UInt32.t;
    options : Bytesrw.Bytes.Slice.t;
    tcp : tcp option;
    udp : udp option;
    opaque : Bytesrw.Bytes.Slice.t option;
  }

  let ihl =
    Field.v "ihl" (bits ~width:4 U8) ~self_constraint:(fun x ->
        Expr.(x >= int 5))

  let total_length =
    Field.v "total_length" uint16be ~self_constraint:(fun x ->
        Expr.(x >= Field.ref ihl lsl int 2))

  let flags = Field.v "flags" (bits ~width:3 U16be)
  let fragment_offset = Field.v "fragment_offset" (bits ~width:13 U16be)
  let protocol = Field.v "protocol" uint8
  let payload_size = Expr.(Field.ref total_length - (Field.ref ihl lsl int 2))

  let unfragmented =
    Expr.(
      Field.ref flags land int 1 = int 0 && Field.ref fragment_offset = int 0)

  let is_tcp = Expr.(unfragmented && Field.ref protocol = int 6)
  let is_udp = Expr.(unfragmented && Field.ref protocol = int 17)

  let ipv4_of_fields version ihl dscp ecn total_length identification flags
      fragment_offset ttl protocol checksum src dst options tcp udp opaque =
    {
      version;
      ihl;
      dscp;
      ecn;
      total_length;
      identification;
      flags;
      fragment_offset;
      ttl;
      protocol;
      checksum;
      src;
      dst;
      options;
      tcp;
      udp;
      opaque;
    }

  let ipv4 =
    Codec.v "StackIPv4" ipv4_of_fields
      Codec.
        [
          ( Field.v "version" (bits ~width:4 U8) ~self_constraint:(fun x ->
                Expr.(x = int 4))
          $ fun r -> r.version );
          (ihl $ fun r -> r.ihl);
          (Field.v "dscp" (bits ~width:6 U8) $ fun r -> r.dscp);
          (Field.v "ecn" (bits ~width:2 U8) $ fun r -> r.ecn);
          (total_length $ fun r -> r.total_length);
          (Field.v "identification" uint16be $ fun r -> r.identification);
          (flags $ fun r -> r.flags);
          (fragment_offset $ fun r -> r.fragment_offset);
          (Field.v "ttl" uint8 $ fun r -> r.ttl);
          (protocol $ fun r -> r.protocol);
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          (Field.v "src" uint32be $ fun r -> r.src);
          (Field.v "dst" uint32be $ fun r -> r.dst);
          ( Field.v "options"
              (byte_slice ~size:Expr.((Field.ref ihl lsl int 2) - int 20))
          $ fun r -> r.options );
          ( Field.optional "tcp" ~present:is_tcp
              (nested_at_most ~size:payload_size (codec tcp))
          $ fun r -> r.tcp );
          ( Field.optional "udp" ~present:is_udp
              (nested_at_most ~size:payload_size (codec udp))
          $ fun r -> r.udp );
          ( Field.optional "opaque"
              ~present:Expr.(not (is_tcp || is_udp))
              (byte_slice ~size:payload_size)
          $ fun r -> r.opaque );
        ]

  type t = {
    dst : Bytesrw.Bytes.Slice.t;
    src : Bytesrw.Bytes.Slice.t;
    ether_type : UInt16.t;
    ipv4 : ipv4 option;
  }

  let ether_type = Field.v "ether_type" uint16be

  let codec =
    Codec.v "EthernetStack"
      (fun dst src ether_type ipv4 -> { dst; src; ether_type; ipv4 })
      Codec.
        [
          (Field.v "dst" (byte_slice ~size:(int 6)) $ fun r -> r.dst);
          (Field.v "src" (byte_slice ~size:(int 6)) $ fun r -> r.src);
          (ether_type $ fun r -> r.ether_type);
          ( Field.optional "ipv4"
              ~present:Expr.(Field.ref ether_type = int 0x0800)
              (codec ipv4)
          $ fun r -> r.ipv4 );
        ]
end

(* Five 64-byte packets: TCP with IPv4 and TCP options, UDP, a fragment, a frame
   that is not IPv4, and TCP without options. *)
let stack_seeds =
  let packet ~ether_type ~ip =
    let b = Bytes.make 64 '\x00' in
    for i = 0 to 11 do
      Bytes.set_uint8 b i (0x10 + i)
    done;
    Bytes.set_uint16_be b 12 ether_type;
    ip b;
    b
  in
  let ipv4 ~ihl ~total ~frag ~proto b =
    Bytes.set_uint8 b 14 (0x40 lor ihl);
    Bytes.set_uint16_be b 16 total;
    Bytes.set_uint16_be b 18 0x1234;
    Bytes.set_uint16_be b 20 frag;
    Bytes.set_uint8 b 22 64;
    Bytes.set_uint8 b 23 proto;
    Bytes.set_int32_be b 26 0x0a000001l;
    Bytes.set_int32_be b 30 0x0a000002l;
    for i = 34 to 14 + (ihl * 4) - 1 do
      Bytes.set_uint8 b i (0xa0 + i)
    done;
    14 + (ihl * 4)
  in
  let tcp ~data_offset l4 b =
    Bytes.set_uint16_be b l4 443;
    Bytes.set_uint16_be b (l4 + 2) 51000;
    Bytes.set_int32_be b (l4 + 4) 0x01020304l;
    Bytes.set_int32_be b (l4 + 8) 0x05060708l;
    Bytes.set_uint16_be b (l4 + 12) ((data_offset lsl 12) lor 0x018);
    Bytes.set_uint16_be b (l4 + 14) 0xffff;
    for i = l4 + 20 to l4 + (data_offset * 4) - 1 do
      Bytes.set_uint8 b i (0xc0 + i)
    done
  in
  [
    packet ~ether_type:0x0800 ~ip:(fun b ->
        tcp ~data_offset:6 (ipv4 ~ihl:6 ~total:48 ~frag:0x4000 ~proto:6 b) b);
    packet ~ether_type:0x0800 ~ip:(fun b ->
        let l4 = ipv4 ~ihl:5 ~total:32 ~frag:0x4000 ~proto:17 b in
        Bytes.set_uint16_be b l4 53;
        Bytes.set_uint16_be b (l4 + 2) 5353;
        Bytes.set_uint16_be b (l4 + 4) 12);
    packet ~ether_type:0x0800 ~ip:(fun b ->
        ignore (ipv4 ~ihl:5 ~total:40 ~frag:0x2000 ~proto:6 b : int));
    packet ~ether_type:0x86dd ~ip:ignore;
    packet ~ether_type:0x0800 ~ip:(fun b ->
        tcp ~data_offset:5 (ipv4 ~ihl:5 ~total:50 ~frag:0 ~proto:6 b) b);
  ]

(* Each seed, each byte of it flipped five ways, and each truncation. *)
let stack_inputs =
  List.concat_map
    (fun seed ->
      let flips =
        List.concat_map
          (fun i ->
            List.map
              (fun x ->
                let b = Bytes.copy seed in
                Bytes.set_uint8 b i (Bytes.get_uint8 b i lxor x);
                b)
              [ 0x01; 0x0f; 0x40; 0x80; 0xff ])
          (List.init (Bytes.length seed) Fun.id)
      in
      let cuts = List.init (Bytes.length seed) (fun n -> Bytes.sub seed 0 n) in
      (seed :: flips) @ cuts)
    stack_seeds

let hex_slice s =
  String.concat ""
    (List.init (Slice.length s) (fun i ->
         Fmt.str "%02x" (Bytes.get_uint8 (Slice.bytes s) (Slice.first s + i))))

(* Unsigned on every int width: [UInt32.pp] prints a carrier, which is signed
   on a 32-bit target. *)
let u32 ppf v = Fmt.pf ppf "%lu" (UInt32.to_int32 v)

let show_tcp (t : Packet_stack.tcp) =
  Fmt.str "tcp %d %d %a %a %d %d %d %d %d %d %s" (UInt16.to_int t.src_port)
    (UInt16.to_int t.dst_port) u32 t.seq u32 t.ack t.data_offset t.reserved
    t.flags (UInt16.to_int t.window) (UInt16.to_int t.checksum)
    (UInt16.to_int t.urgent) (hex_slice t.options)

let show_udp (u : Packet_stack.udp) =
  Fmt.str "udp %d %d %d %d %s" (UInt16.to_int u.src_port)
    (UInt16.to_int u.dst_port) (UInt16.to_int u.length)
    (UInt16.to_int u.checksum) (hex_slice u.payload)

let show_ipv4 (ip : Packet_stack.ipv4) =
  Fmt.str "ipv4 %d %d %d %d %d %d %d %d %d %d %d %a %a %s [%s] [%s] [%s]"
    ip.version ip.ihl ip.dscp ip.ecn
    (UInt16.to_int ip.total_length)
    (UInt16.to_int ip.identification)
    ip.flags ip.fragment_offset (UInt8.to_int ip.ttl) (UInt8.to_int ip.protocol)
    (UInt16.to_int ip.checksum)
    u32 ip.src u32 ip.dst (hex_slice ip.options)
    (Option.fold ~none:"" ~some:show_tcp ip.tcp)
    (Option.fold ~none:"" ~some:show_udp ip.udp)
    (Option.fold ~none:"" ~some:hex_slice ip.opaque)

let show_stack (p : Packet_stack.t) =
  Fmt.str "eth %s %s %d [%s]" (hex_slice p.dst) (hex_slice p.src)
    (UInt16.to_int p.ether_type)
    (Option.fold ~none:"" ~some:show_ipv4 p.ipv4)

let stack_answer show f =
  match f () with
  | v -> "ok " ^ show v
  | exception Parse_error e -> Fmt.str "error %a" pp_parse_error e

let stack_slot = Param.input "stack_slot" uint8

let stack_frame size name =
  Codec.v name Fun.id
    Codec.
      [
        Field.v "packet" (nested_at_most ~size (codec Packet_stack.codec))
        $ Fun.id;
      ]

(* What every entry point answers on one input: the stack decoded and validated
   at offset 0 and at 2 in a longer buffer, embedded in a fixed region and in a
   param-sized one, and its IPv4 layer on its own. *)
let stack_answers =
  let frame = stack_frame (int 64) "StackFrame" in
  let param_frame = stack_frame (Param.expr stack_slot) "StackParamFrame" in
  let env () = Codec.env param_frame |> Param.bind stack_slot (UInt8.v 64) in
  let unit () = "" in
  fun input ->
    let shifted = Bytes.cat (Bytes.of_string "\xee\xee") input in
    let decode c b off () = Codec.decode_exn c b off in
    let validate c b off () = Codec.validate c b off in
    [
      stack_answer show_stack (decode Packet_stack.codec input 0);
      stack_answer show_stack (decode Packet_stack.codec shifted 2);
      stack_answer unit (validate Packet_stack.codec input 0);
      stack_answer unit (validate Packet_stack.codec shifted 2);
      stack_answer show_stack (decode frame input 0);
      stack_answer unit (validate frame input 0);
      stack_answer show_stack (fun () ->
          Codec.decode_exn ~env:(env ()) param_frame input 0);
      stack_answer unit (fun () ->
          Codec.validate ~env:(env ()) param_frame input 0);
      stack_answer show_ipv4 (decode Packet_stack.ipv4 input 14);
      stack_answer unit (validate Packet_stack.ipv4 input 14);
    ]

(* The answers on the corpus, as one digest, the same on every int width. Changing how the interpreter runs
   must not change what it answers: a value, an acceptance or a first error
   (offset, field path and kind) that moves changes the digest. *)
let test_stack_answers_pinned () =
  let lines = List.concat_map stack_answers stack_inputs in
  let oks =
    List.length (List.filter (fun l -> String.starts_with ~prefix:"ok" l) lines)
  in
  Alcotest.(check int) "answers" 19250 (List.length lines);
  Alcotest.(check int) "accepted" 15088 oks;
  Alcotest.(check string)
    "digest" "bbe5dae3ba1f2e29c38b4a137661ea0c"
    (Digest.to_hex (Digest.string (String.concat "\n" lines)))

let suite =
  ( "codec_stack",
    [
      Alcotest.test_case "stack: every entry point's answers are pinned" `Quick
        test_stack_answers_pinned;
    ] )
