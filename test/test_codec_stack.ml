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

  let tcp_src_port = Codec.(Field.v "src_port" uint16be $ fun r -> r.src_port)
  let tcp_dst_port = Codec.(Field.v "dst_port" uint16be $ fun r -> r.dst_port)
  let tcp_seq = Codec.(Field.v "seq" uint32be $ fun r -> r.seq)
  let tcp_ack = Codec.(Field.v "ack" uint32be $ fun r -> r.ack)
  let tcp_data_offset = Codec.(data_offset $ fun r -> r.data_offset)

  let tcp_flags =
    Codec.(Field.v "flags" (bits ~width:9 U16be) $ fun r -> r.flags)

  let tcp_options =
    Codec.(
      Field.v "options"
        (byte_slice ~size:Expr.((Field.ref data_offset lsl int 2) - int 20))
      $ fun r -> r.options)

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
          tcp_src_port;
          tcp_dst_port;
          tcp_seq;
          tcp_ack;
          tcp_data_offset;
          (Field.v "reserved" (bits ~width:3 U16be) $ fun r -> r.reserved);
          tcp_flags;
          (Field.v "window" uint16be $ fun r -> r.window);
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          (Field.v "urgent" uint16be $ fun r -> r.urgent);
          tcp_options;
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

  let udp_src_port = Codec.(Field.v "src_port" uint16be $ fun r -> r.src_port)
  let udp_length_f = Codec.(udp_length $ fun r -> r.length)

  let udp_payload =
    Codec.(
      Field.v "payload" (byte_slice ~size:Expr.(Field.ref udp_length - int 8))
      $ fun r -> r.payload)

  let udp =
    Codec.v "StackUDP"
      (fun src_port dst_port length checksum payload ->
        { src_port; dst_port; length; checksum; payload })
      Codec.
        [
          udp_src_port;
          (Field.v "dst_port" uint16be $ fun r -> r.dst_port);
          udp_length_f;
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          udp_payload;
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

  let ipv4_total_length = Codec.(total_length $ fun r -> r.total_length)
  let ipv4_ttl = Codec.(Field.v "ttl" uint8 $ fun r -> r.ttl)
  let ipv4_protocol = Codec.(protocol $ fun r -> r.protocol)
  let ipv4_src = Codec.(Field.v "src" uint32be $ fun r -> r.src)

  let ipv4_options =
    Codec.(
      Field.v "options"
        (byte_slice ~size:Expr.((Field.ref ihl lsl int 2) - int 20))
      $ fun r -> r.options)

  let ipv4_tcp =
    Codec.(
      Field.optional "tcp" ~present:is_tcp
        (nested_at_most ~size:payload_size (codec tcp))
      $ fun r -> r.tcp)

  let ipv4_udp =
    Codec.(
      Field.optional "udp" ~present:is_udp
        (nested_at_most ~size:payload_size (codec udp))
      $ fun r -> r.udp)

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
          ipv4_total_length;
          (Field.v "identification" uint16be $ fun r -> r.identification);
          (flags $ fun r -> r.flags);
          (fragment_offset $ fun r -> r.fragment_offset);
          ipv4_ttl;
          ipv4_protocol;
          (Field.v "checksum" uint16be $ fun r -> r.checksum);
          ipv4_src;
          (Field.v "dst" uint32be $ fun r -> r.dst);
          ipv4_options;
          ipv4_tcp;
          ipv4_udp;
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

  let eth_src =
    Codec.(Field.v "src" (byte_slice ~size:(int 6)) $ fun r -> r.src)

  let eth_ether_type = Codec.(ether_type $ fun r -> r.ether_type)

  let eth_ipv4 =
    Codec.(
      Field.optional "ipv4"
        ~present:Expr.(Field.ref ether_type = int 0x0800)
        (codec ipv4)
      $ fun r -> r.ipv4)

  let codec =
    Codec.v "EthernetStack"
      (fun dst src ether_type ipv4 -> { dst; src; ether_type; ipv4 })
      Codec.
        [
          (Field.v "dst" (byte_slice ~size:(int 6)) $ fun r -> r.dst);
          eth_src;
          eth_ether_type;
          eth_ipv4;
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
  | exception exn when Wire.Private.Types.is_parse_error exn ->
      let e = Wire.Private.Types.parse_error_of exn in
      Fmt.str "error %a" Error.pp e

let stack_slot = Param.input "stack_slot" uint8

let stack_frame span name =
  Codec.v name Fun.id
    Codec.
      [
        Field.v "packet" (nested_at_most ~size:span (codec Packet_stack.codec))
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
    let decode c b off () = Test_helpers.ok (Codec.decode c b ~off) in
    let validate c b off () = Test_helpers.ok (Codec.validate c b ~off) in
    [
      stack_answer show_stack (decode Packet_stack.codec input 0);
      stack_answer show_stack (decode Packet_stack.codec shifted 2);
      stack_answer unit (validate Packet_stack.codec input 0);
      stack_answer unit (validate Packet_stack.codec shifted 2);
      stack_answer show_stack (decode frame input 0);
      stack_answer unit (validate frame input 0);
      stack_answer show_stack (fun () ->
          Test_helpers.ok (Codec.decode ~env:(env ()) param_frame input ~off:0));
      stack_answer unit (fun () ->
          Test_helpers.ok
            (Codec.validate ~env:(env ()) param_frame input ~off:0));
      stack_answer show_ipv4 (decode Packet_stack.ipv4 input 14);
      stack_answer unit (validate Packet_stack.ipv4 input 14);
    ]

(* The answers on the corpus, as one digest, the same on every int width: a
   tripwire, not a contract. A value, an acceptance or a first error (offset,
   field path and kind) that moves changes it, and the commit that updates it
   says which answers moved and why. *)
let test_stack_answers_pinned () =
  let lines = List.concat_map stack_answers stack_inputs in
  let oks =
    List.length (List.filter (fun l -> String.starts_with ~prefix:"ok" l) lines)
  in
  Alcotest.(check int) "answers" 19250 (List.length lines);
  Alcotest.(check int) "accepted" 15088 oks;
  Alcotest.(check string)
    "digest" "2062c405835d450cadb1ec84c0003d07"
    (Digest.to_hex (Digest.string (String.concat "\n" lines)))

(* The stack, and a frame around it, decode through the walk, so the answers
   pinned above are the walk's. *)
let test_stack_walks () =
  List.iter
    (fun (name, paths) ->
      Alcotest.(check bool) name true (List.mem "decode.walk" paths))
    [
      ("stack", Private.Testing.paths Packet_stack.codec);
      ("ipv4", Private.Testing.paths Packet_stack.ipv4);
      ("frame", Private.Testing.paths (stack_frame (int 64) "StackFrame"));
    ]

(* The fields a view of each layer reads, from a decoded stack. *)
let show_read (p : Packet_stack.t) =
  let tcp (t : Packet_stack.tcp) =
    Fmt.str "tcp %d %d %a %a %d %d %s" (UInt16.to_int t.src_port)
      (UInt16.to_int t.dst_port) u32 t.seq u32 t.ack t.data_offset t.flags
      (hex_slice t.options)
  in
  let udp (u : Packet_stack.udp) =
    Fmt.str "udp %d %d %s" (UInt16.to_int u.src_port) (UInt16.to_int u.length)
      (hex_slice u.payload)
  in
  let ipv4 (ip : Packet_stack.ipv4) =
    Fmt.str "ipv4 %d %d %d %a %s [%s] [%s]"
      (UInt16.to_int ip.total_length)
      (UInt8.to_int ip.ttl) (UInt8.to_int ip.protocol) u32 ip.src
      (hex_slice ip.options)
      (Option.fold ~none:"" ~some:tcp ip.tcp)
      (Option.fold ~none:"" ~some:udp ip.udp)
  in
  Fmt.str "eth %s %d [%s]" (hex_slice p.src)
    (UInt16.to_int p.ether_type)
    (Option.fold ~none:"" ~some:ipv4 p.ipv4)

(* The stack read through checked views. [View.check] of the stack, and of a
   frame around it, accepts what [validate] accepts; on an accepted record each
   layer [View.sub] reaches reads the fields [decode] gives, and an absent layer
   is refused. *)
let test_stack_views () =
  let module V = Codec.View in
  let module P = Packet_stack in
  let packet =
    Codec.(
      Field.v "packet" (nested_at_most ~size:(int 64) (codec P.codec)) $ Fun.id)
  in
  let frame = Codec.v "StackViewFrame" Fun.id [ packet ] in
  let in_frame = V.layer frame (Codec.layer packet) in
  let ip = V.layer P.codec (Codec.opt_layer P.eth_ipv4) in
  let tcp = V.layer P.ipv4 (Codec.opt_layer P.ipv4_tcp) in
  let udp = V.layer P.ipv4 (Codec.opt_layer P.ipv4_udp) in
  let eth_src = V.field P.codec P.eth_src
  and ether_type = V.field P.codec P.eth_ether_type
  and ip_len = V.field P.ipv4 P.ipv4_total_length
  and ip_ttl = V.field P.ipv4 P.ipv4_ttl
  and ip_proto = V.field P.ipv4 P.ipv4_protocol
  and ip_src = V.field P.ipv4 P.ipv4_src
  and ip_options = V.field P.ipv4 P.ipv4_options
  and tcp_sp = V.field P.tcp P.tcp_src_port
  and tcp_dp = V.field P.tcp P.tcp_dst_port
  and tcp_seq = V.field P.tcp P.tcp_seq
  and tcp_ack = V.field P.tcp P.tcp_ack
  and tcp_off = V.field P.tcp P.tcp_data_offset
  and tcp_flags = V.field P.tcp P.tcp_flags
  and tcp_options = V.field P.tcp P.tcp_options
  and udp_sp = V.field P.udp P.udp_src_port
  and udp_len = V.field P.udp P.udp_length_f
  and udp_payload = V.field P.udp P.udp_payload in
  let view input off ~framed =
    let (Buf.Any b) = Buf.v input in
    let layer k show = if V.is_ok k then show (V.get k) else "" in
    let tcp_of t =
      Fmt.str "tcp %d %d %a %a %d %d %s"
        (UInt16.to_int (V.read b t tcp_sp))
        (UInt16.to_int (V.read b t tcp_dp))
        u32 (V.read b t tcp_seq) u32 (V.read b t tcp_ack) (V.read b t tcp_off)
        (V.read b t tcp_flags)
        (hex_slice (V.read b t tcp_options))
    in
    let udp_of u =
      Fmt.str "udp %d %d %s"
        (UInt16.to_int (V.read b u udp_sp))
        (UInt16.to_int (V.read b u udp_len))
        (hex_slice (V.read b u udp_payload))
    in
    let ipv4_of r =
      Fmt.str "ipv4 %d %d %d %a %s [%s] [%s]"
        (UInt16.to_int (V.read b r ip_len))
        (UInt8.to_int (V.read b r ip_ttl))
        (UInt8.to_int (V.read b r ip_proto))
        u32 (V.read b r ip_src)
        (hex_slice (V.read b r ip_options))
        (layer (V.sub b r tcp) tcp_of)
        (layer (V.sub b r udp) udp_of)
    in
    let eth_of e =
      Fmt.str "eth %s %d [%s]"
        (hex_slice (V.read b e eth_src))
        (UInt16.to_int (V.read b e ether_type))
        (layer (V.sub b e ip) ipv4_of)
    in
    if framed then
      let k = V.check frame b ~off in
      if V.is_ok k then Some (layer (V.sub b (V.get k) in_frame) eth_of)
      else None
    else
      let k = V.check P.codec b ~off in
      if V.is_ok k then Some (eth_of (V.get k)) else None
  in
  let decoded c input off =
    Result.to_option (Codec.decode c input ~off) |> Option.map show_read
  in
  let check name input off ~framed c =
    Alcotest.(check (option string))
      name (decoded c input off) (view input off ~framed)
  in
  List.iteri
    (fun i input ->
      let shifted = Bytes.cat (Bytes.of_string "\xee\xee") input in
      check (Fmt.str "stack %d" i) input 0 ~framed:false P.codec;
      check (Fmt.str "stack %d at 2" i) shifted 2 ~framed:false P.codec;
      check (Fmt.str "frame %d" i) input 0 ~framed:true frame)
    stack_inputs;
  let raises f =
    match f () with _ -> false | exception Invalid_argument _ -> true
  in
  let other =
    Codec.v "OtherStack"
      (fun ether_type : P.t ->
        { dst = Slice.eod; src = Slice.eod; ether_type; ipv4 = None })
      Codec.[ (Field.v "ether_type" uint16be $ fun (r : P.t) -> r.ether_type) ]
  in
  Alcotest.(check bool)
    "a field of another codec" true
    (raises (fun () -> V.layer other (Codec.opt_layer P.eth_ipv4)));
  Alcotest.(check bool)
    "a field holding no record" true
    (raises (fun () -> Codec.layer P.eth_ether_type))

(* A fixed span after one sized by an earlier field: the walk reads the latter
   where the fixed fields before it end, not at the record's minimum size, and
   answers as the general decode does on every input. *)
let test_walk_trailer () =
  let len = Field.v "len" uint8 in
  let c =
    Codec.v "Trailer"
      (fun l s t -> (l, s, t))
      Codec.
        [
          (len $ fun (l, _, _) -> l);
          (Field.v "s" (byte_array ~size:(Field.ref len)) $ fun (_, s, _) -> s);
          (Field.v "t" (byte_array ~size:(int 2)) $ fun (_, _, t) -> t);
        ]
  in
  Alcotest.(check bool)
    "walks" true
    (List.mem "decode.walk" (Private.Testing.paths c));
  let general = Private.Testing.general c in
  let show = function
    | Ok (l, s, t) -> Fmt.str "ok %d %S %S" (UInt8.to_int l) s t
    | Error e -> Fmt.str "error %a" Error.pp e
  in
  List.iter
    (fun s ->
      let buf = Bytes.of_string s in
      Alcotest.(check string)
        (Fmt.str "%S" s)
        (show (Codec.decode general buf ~off:0))
        (show (Codec.decode c buf ~off:0)))
    [ "\002abXY"; "\000XY"; "\002abX"; "\003abXY"; "\001aXYZZ" ];
  Alcotest.(check string)
    "value" "ok 2 \"ab\" \"XY\""
    (show (Codec.decode c (Bytes.of_string "\002abXY") ~off:0))

(* Gates that reuse an earlier gate's answer, in a record and in a sub-codec
   walked between the gate and its reuse: the walk answers as the general
   decode does on every input. *)
let test_walk_shared_gates () =
  let flag = Field.v "flag" uint8 in
  let on = Expr.(Field.ref flag = int 1) in
  let inner =
    Codec.v "Inner"
      (fun f a b -> (f, a, b))
      Codec.
        [
          (flag $ fun (f, _, _) -> f);
          ( Field.optional "a" ~present:on (byte_array ~size:(int 1))
          $ fun (_, a, _) -> a );
          ( Field.optional "b" ~present:Expr.(not on) (byte_array ~size:(int 2))
          $ fun (_, _, b) -> b );
        ]
  in
  let k = Field.v "k" uint8 in
  let one = Expr.(Field.ref k = int 1) in
  let c =
    Codec.v "Outer"
      (fun k x y -> (k, x, y))
      Codec.
        [
          (k $ fun (k, _, _) -> k);
          (Field.optional "x" ~present:one (codec inner) $ fun (_, x, _) -> x);
          ( Field.optional "y" ~present:Expr.(not one) (byte_array ~size:(int 1))
          $ fun (_, _, y) -> y );
        ]
  in
  Alcotest.(check bool)
    "walks" true
    (List.mem "decode.walk" (Private.Testing.paths c));
  let general = Private.Testing.general c in
  let opt = Option.value ~default:"-" in
  let show = function
    | Ok (k, x, y) ->
        Fmt.str "ok %d [%s] %s" (UInt8.to_int k)
          (match x with
          | None -> "-"
          | Some (f, a, b) ->
              Fmt.str "%d %s %s" (UInt8.to_int f) (opt a) (opt b))
          (opt y)
    | Error e -> Fmt.str "error %a" Error.pp e
  in
  List.iter
    (fun k ->
      List.iter
        (fun f ->
          for n = 0 to 4 do
            let buf = Bytes.make (2 + n) 'z' in
            Bytes.set_uint8 buf 0 k;
            Bytes.set_uint8 buf 1 f;
            Alcotest.(check string)
              (Fmt.str "k=%d flag=%d +%d" k f n)
              (show (Codec.decode general buf ~off:0))
              (show (Codec.decode c buf ~off:0))
          done)
        [ 0; 1; 2 ])
    [ 0; 1; 2 ]

(* A frame is checked against its own region, whatever the buffer holds after
   it: each input, as a frame of its own length, answers the same at the end of
   its buffer, where the region and the buffer end together, as in front of
   more packets. *)
let test_region_bounds_nested () =
  let trailer = Bytes.concat Bytes.empty stack_seeds in
  let answer frame buf =
    let decode () = Test_helpers.ok (Codec.decode frame buf ~off:0) in
    let validate () = Test_helpers.ok (Codec.validate frame buf ~off:0) in
    [ stack_answer show_stack decode; stack_answer (fun () -> "") validate ]
    |> String.concat " / "
  in
  List.iter
    (fun input ->
      let n = Bytes.length input in
      let frame = stack_frame (int n) "StackRegionFrame" in
      Alcotest.(check string)
        (Fmt.str "frame of %d bytes" n)
        (answer frame input)
        (answer frame (Bytes.cat input trailer)))
    stack_inputs;
  (* TCP without options in an IPv4 payload of 10 bytes: the TCP header counts
     the 10 bytes of its region, not the 30 left in the frame. *)
  let short = Bytes.copy (List.nth stack_seeds 4) in
  Bytes.set_uint16_be short 16 30;
  let expected =
    Fmt.str "error %a" Error.pp
      {
        Error.at = 34;
        field = [ "packet"; "ipv4"; "tcp" ];
        kind = Unexpected_eof { expected = 20; got = 10 };
      }
  in
  Alcotest.(check string)
    "short TCP region"
    (String.concat " / " [ expected; expected ])
    (answer (stack_frame (int 64) "StackRegionFrame") short)

(* Each sub-record [View.enter] places past regions, spans and sub-codecs
   that measure themselves, present or absent: it reads the fields [decode]
   gives, and an absent one is refused. *)
let test_enter_past_regions () =
  let module V = Codec.View in
  let k = Field.v "k" uint8 in
  let k_f = Codec.(k $ fun k -> UInt8.v k) in
  let inner =
    Codec.v "Inner"
      (fun k _ -> UInt8.to_int k)
      Codec.
        [
          (k $ fun k -> UInt8.v k);
          (Field.v "body" (byte_slice ~size:(Field.ref k)) $ fun _ -> Slice.eod);
        ]
  in
  let g = Field.v "g" uint8 and r = Field.v "r" uint8 in
  let bit n = Expr.(Field.ref g land int n = int n) in
  let a =
    Codec.(
      Field.optional "a" ~present:(bit 1)
        (nested_at_most ~size:(Field.ref r) (codec inner))
      $ fun (_, _, a, _, _, _, _) -> a)
  and b = Codec.(Field.v "b" (codec inner) $ fun (_, _, _, b, _, _, _) -> b)
  and c =
    Codec.(
      Field.optional "c" ~present:(bit 2) (codec inner)
      $ fun (_, _, _, _, _, c, _) -> c)
  and d = Codec.(Field.v "d" (codec inner) $ fun (_, _, _, _, _, _, d) -> d) in
  let outer =
    Codec.v "Outer"
      (fun g r a b _ c d -> (g, r, a, b, (), c, d))
      Codec.
        [
          (g $ fun (g, _, _, _, _, _, _) -> g);
          (r $ fun (_, r, _, _, _, _, _) -> r);
          a;
          b;
          (Field.v "s" (byte_slice ~size:(Field.ref r)) $ fun _ -> Slice.eod);
          c;
          d;
        ]
  in
  Alcotest.(check bool)
    "walks" true
    (List.mem "decode.walk" (Private.Testing.paths outer));
  let in_a = V.layer outer (Codec.opt_layer a)
  and in_b = V.layer outer (Codec.layer b)
  and in_c = V.layer outer (Codec.opt_layer c)
  and in_d = V.layer outer (Codec.layer d)
  and vk = V.field inner k_f in
  let show (_, _, a, b, _, c, d) =
    let o = Option.fold ~none:"-" ~some:string_of_int in
    Fmt.str "%s %d %s %d" (o a) b (o c) d
  in
  let viewed input =
    let (Buf.Any buf) = Buf.v input in
    let ck = V.check outer buf ~off:0 in
    if not (V.is_ok ck) then None
    else
      let o = V.get ck in
      let read e =
        if V.is_ok e then Some (UInt8.to_int (V.read buf (V.get e) vk))
        else None
      in
      let get e = Option.get (read e) in
      Some
        (show
           ( (),
             (),
             read (V.sub buf o in_a),
             get (V.sub buf o in_b),
             (),
             read (V.sub buf o in_c),
             get (V.sub buf o in_d) ))
  in
  let decoded input =
    match Codec.decode outer input ~off:0 with
    | Ok v -> Some (show v)
    | Error _ -> None
  in
  let rnd = Random.State.make [| 11 |] in
  for i = 1 to 5_000 do
    let input =
      Bytes.init
        (2 + Random.State.int rnd 14)
        (fun j ->
          Char.chr
            (if j = 0 then Random.State.int rnd 4 else Random.State.int rnd 4))
    in
    Alcotest.(check (option string))
      (Fmt.str "input %d" i) (decoded input) (viewed input)
  done

(* A field after a span has no constant place: a view locates it from the
   record's bytes and reads what [decode] reads. *)
let test_view_field_after_a_span () =
  let len = Field.v "len" uint8 in
  let tail = Codec.(Field.v "tail" uint8 $ fun (_, _, t) -> t) in
  let c =
    Codec.v "AfterSpan"
      (fun len body tail -> (len, body, tail))
      Codec.
        [
          (len $ fun (l, _, _) -> l);
          ( Field.v "body" (byte_array ~size:(Field.ref len)) $ fun (_, b, _) ->
            b );
          tail;
        ]
  in
  let buf = Bytes.of_string "\x02abX" in
  let _, _, t = Result.get_ok (Codec.decode c buf ~off:0) in
  Alcotest.(check int)
    "tail" (UInt8.to_int t)
    (UInt8.to_int (Test_helpers.reader c tail buf 0))

let suite =
  ( "codec_stack",
    [
      Alcotest.test_case "view: a field after a span reads what decode reads"
        `Quick test_view_field_after_a_span;
      Alcotest.test_case "walk: gates that reuse an earlier gate" `Quick
        test_walk_shared_gates;
      Alcotest.test_case "walk: a fixed field after a span" `Quick
        test_walk_trailer;
      Alcotest.test_case "stack: decode walks each layer once" `Quick
        test_stack_walks;
      Alcotest.test_case "view: enter past regions and sub-codecs" `Quick
        test_enter_past_regions;
      Alcotest.test_case "stack: views read what decode reads" `Quick
        test_stack_views;
      Alcotest.test_case "stack: every entry point's answers are pinned" `Quick
        test_stack_answers_pinned;
      Alcotest.test_case "stack: a nested header is bounded by its region"
        `Quick test_region_bounds_nested;
    ] )
