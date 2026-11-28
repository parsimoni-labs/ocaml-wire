(* Cross-language header parsing benchmark, OCaml side.

   bench.exe corpus DIR N     writes the four corpora below, N packets each,
                              to DIR/<corpus>.bin
   bench.exe run FILE STRIDE RUNS [ROW | --list]
                              times the OCaml implementations, or only the one
                              named ROW, failing when there is none, on the
                              corpus FILE of STRIDE-byte slots, RUNS passes
                              each, and prints the best, p10, median and p90
                              process CPU time and wall time per packet;
                              --list prints the row names
   bench.exe report FILE...   prints the RESULT lines of each FILE as tables

   Every implementation does the same work on each packet, the work
   etherparse's [SlicedPacket::from_ethernet] does, and folds the same fields
   as cargo/src/main.rs does, so all of them, Rust ones included, print the
   same checksum:
   - the Ethernet header: checked to fit, its EtherType read;
   - for IPv4, the fixed header: version 4, a header length [ihl] of at least
     5 words, a total length covering it and lying inside the packet, so the
     payload [ihl * 4, total length) is bounded; its addresses, TTL, protocol
     and total length read;
   - unless the packet is a fragment (more fragments set, or a fragment
     offset), the TCP or UDP header at [ihl * 4]: a TCP data offset of at
     least 5 words lying inside the payload, or a UDP length of at least 8
     lying inside it; its ports and the rest of the fields read.
   A packet counts, as etherparse counts it, only when every header it has
   passes; one that is not IPv4 counts its EtherType. No implementation
   copies a byte: the crates hand back sub-slices, the others the same bounds
   as offsets. *)

open Wire

let ipv4 = 0x0800

(* {1 The corpora}

   Two families of fixed-stride slots, each written in two orders. The
   generated batch consumers ([fold_frames]) take one stride per buffer and
   bound each frame by it, so a slot is the frame every implementation sees;
   bytes past the IPv4 total length are Ethernet padding.

   [fixed64]: slots of 64 bytes. Three packets in four are well formed: an
   Ethernet header, an IPv4 header of 20 bytes whose total length covers the
   rest of the slot, then a TCP header without options and 10 bytes of
   payload, or a UDP header and 22 bytes. The others are one of the 17
   variants below, each as likely as the next, so every check the rows make
   meets packets that pass it and packets that fail it.

   [mix128]: slots of 128 bytes: IPv4 headers of 20, 24, 32 or 40 bytes (with
   options), TCP headers of 20, 24, 32 or 40 bytes (with options), and an IPv4
   total length anywhere from the headers to the end of the slot, so packets
   of 54 to 128 bytes. The other packets carry an EtherType no parser here
   reads further, and one packet in 20 each is a fragment, has a total length
   past the slot, a TCP data offset past the payload, or a UDP length past it.

   The checksums agree only if every implementation makes the same checks.
   Addresses, ports, sequence numbers, flags, options and payload are random.

   [-random] draws the kind of each packet at random, as traffic does;
   [-sorted] holds the same packets grouped by kind, so every branch is
   predictable. The checksum is a sum, so both orders give the same one. *)

let fill rand b o n =
  for k = 0 to n - 1 do
    Bytes.set_uint8 b (o + k) (Random.State.int rand 256)
  done

type variant =
  | Well_formed
  | Not_ipv4
  | Version_6
  | Ihl_4
  | Ip_options
  | Total_under_20
  | Total_under_header
  | Total_past_packet
  | Total_inside_packet
  | More_fragments
  | Fragment_offset
  | Other_protocol
  | Tcp_offset_4
  | Tcp_offset_past_payload
  | Tcp_options
  | Udp_length_7
  | Udp_length_past_payload
  | Udp_length_inside_payload

let variants =
  [|
    Not_ipv4;
    Version_6;
    Ihl_4;
    Ip_options;
    Total_under_20;
    Total_under_header;
    Total_past_packet;
    Total_inside_packet;
    More_fragments;
    Fragment_offset;
    Other_protocol;
    Tcp_offset_4;
    Tcp_offset_past_payload;
    Tcp_options;
    Udp_length_7;
    Udp_length_past_payload;
    Udp_length_inside_payload;
  |]

let tcp_header b l4 rand variant =
  let words =
    match variant with
    | Tcp_offset_4 -> 4
    | Tcp_offset_past_payload -> 15
    | Tcp_options -> 6
    | _ -> 5
  in
  Bytes.set_uint16_be b (l4 + 12)
    ((words lsl 12) lor Random.State.int rand 0x200)

let udp_header b l4 rand variant ~payload =
  let length =
    match variant with
    | Udp_length_7 -> 7
    | Udp_length_past_payload -> payload + 1
    | Udp_length_inside_payload -> 8 + Random.State.int rand (payload - 8)
    | _ -> payload
  in
  Bytes.set_uint16_be b (l4 + 4) length

let ipv4_packet b ip rand variant ~slot =
  let rest = slot - 14 in
  let version = if variant = Version_6 then 6 else 4 in
  let ihl =
    match variant with
    | Ihl_4 -> 4
    | Ip_options | Total_under_header -> 6
    | _ -> 5
  in
  let total =
    match variant with
    | Total_under_20 -> 19
    | Total_under_header -> 22
    | Total_past_packet -> rest + 1
    | Total_inside_packet -> rest - 6
    | _ -> rest
  in
  let fragment =
    match variant with
    | More_fragments -> 0x2000
    | Fragment_offset -> 1 + Random.State.int rand 0x1fff
    | _ -> 0x4000
  in
  Bytes.set_uint8 b ip ((version lsl 4) lor ihl);
  Bytes.set_uint16_be b (ip + 2) total;
  Bytes.set_uint16_be b (ip + 6) fragment;
  Bytes.set_uint8 b (ip + 8) (1 + Random.State.int rand 254);
  let l4 = ip + (ihl * 4) and payload = total - (ihl * 4) in
  let tcp =
    match variant with
    | Tcp_offset_4 | Tcp_offset_past_payload | Tcp_options -> true
    | Udp_length_7 | Udp_length_past_payload | Udp_length_inside_payload ->
        false
    | _ -> Random.State.bool rand
  in
  if variant = Other_protocol then Bytes.set_uint8 b (ip + 9) 253
  else if tcp then begin
    Bytes.set_uint8 b (ip + 9) 6;
    tcp_header b l4 rand variant
  end
  else begin
    Bytes.set_uint8 b (ip + 9) 17;
    udp_header b l4 rand variant ~payload
  end

(* The draws are in the order the single-corpus harness made them, so
   [fixed64-random] is the corpus earlier results were measured on. The kind
   of a packet is its variant's rank. *)
let fixed64 rand b o =
  let slot = 64 in
  fill rand b o slot;
  let kind =
    if Random.State.int rand 4 > 0 then 0
    else 1 + Random.State.int rand (Array.length variants)
  in
  let variant = if kind = 0 then Well_formed else variants.(kind - 1) in
  if variant = Not_ipv4 then Bytes.set_uint16_be b (o + 12) 0x88B5
  else begin
    Bytes.set_uint16_be b (o + 12) ipv4;
    ipv4_packet b (o + 14) rand variant ~slot
  end;
  kind

let words = [| 5; 6; 8; 10 |]
let pick rand a = a.(Random.State.int rand (Array.length a))

(* The IPv4 header length in words, the TCP data offset in words and the IPv4
   total length of a [mix128] packet of [kind]. *)
let mix_sizes rand ~slot ~is_tcp kind =
  let ihl = pick rand words in
  (* A data offset of 15 words is past a payload of fewer than 40 bytes. *)
  let off = if kind = 3 then 15 else pick rand words in
  let l4_size = if not is_tcp then 8 else if kind = 3 then 20 else off * 4 in
  let room = slot - 14 - (ihl * 4) - l4_size in
  let data = Random.State.int rand (if kind = 3 then 20 else room + 1) in
  let total = if kind = 2 then slot - 14 + 1 else (ihl * 4) + l4_size + data in
  (ihl, off, total)

let mix_ipv4 rand b ip ~slot kind =
  let is_tcp = kind = 3 || (kind <> 4 && kind <= 12) in
  let ihl, off, total = mix_sizes rand ~slot ~is_tcp kind in
  Bytes.set_uint8 b ip (0x40 lor ihl);
  Bytes.set_uint16_be b (ip + 2) total;
  Bytes.set_uint16_be b (ip + 6) (if kind = 1 then 0x2000 else 0x4000);
  Bytes.set_uint8 b (ip + 8) (1 + Random.State.int rand 254);
  let l4 = ip + (ihl * 4) in
  if is_tcp then begin
    Bytes.set_uint8 b (ip + 9) 6;
    Bytes.set_uint16_be b (l4 + 12)
      ((off lsl 12) lor Random.State.int rand 0x200)
  end
  else begin
    Bytes.set_uint8 b (ip + 9) 17;
    Bytes.set_uint16_be b (l4 + 4)
      (total - (ihl * 4) + if kind = 4 then 1 else 0)
  end

let mix128 rand b o =
  let slot = 128 in
  fill rand b o slot;
  let kind = Random.State.int rand 20 in
  if kind = 0 then Bytes.set_uint16_be b (o + 12) 0x88B5
  else begin
    Bytes.set_uint16_be b (o + 12) ipv4;
    mix_ipv4 rand b (o + 14) ~slot kind
  end;
  kind

let write file b =
  Out_channel.with_open_bin file (fun oc -> Out_channel.output_bytes oc b)

let corpora dir n =
  List.iter
    (fun (family, stride, packet) ->
      let rand = Random.State.make [| 42 |] in
      let b = Bytes.create (n * stride) in
      let kinds = Array.init n (fun i -> packet rand b (i * stride)) in
      write (Filename.concat dir (family ^ "-random.bin")) b;
      let order = Array.init n Fun.id in
      Array.stable_sort (fun i j -> Int.compare kinds.(i) kinds.(j)) order;
      let sorted = Bytes.create (n * stride) in
      Array.iteri
        (fun k i -> Bytes.blit b (i * stride) sorted (k * stride) stride)
        order;
      write (Filename.concat dir (family ^ "-sorted.bin")) sorted)
    [ ("fixed64", 64, fixed64); ("mix128", 128, mix128) ]

(* {1 The fold} *)

let ip_fields acc ~src ~dst ~ttl ~proto ~len =
  acc + (src lxor dst) + (ttl + proto + len)

let tcp_fields acc ~sp ~dp ~seq ~ack ~win ~flags ~off =
  acc + ((sp lsl 16) lor dp) + (seq lxor ack) + (win + flags + off)

let udp_fields acc ~sp ~dp ~len = acc + ((sp lsl 16) lor dp) + len

let packet_fields acc (packet : Packet_stack.t) =
  let acc = acc + UInt16.to_int packet.ether_type in
  match packet.ipv4 with
  | None -> acc
  | Some ip -> (
      let acc =
        ip_fields acc ~src:(UInt32.to_int ip.src) ~dst:(UInt32.to_int ip.dst)
          ~ttl:(UInt8.to_int ip.ttl) ~proto:(UInt8.to_int ip.protocol)
          ~len:(UInt16.to_int ip.total_length)
      in
      match (ip.tcp, ip.udp) with
      | Some t, _ ->
          tcp_fields acc ~sp:(UInt16.to_int t.src_port)
            ~dp:(UInt16.to_int t.dst_port) ~seq:(UInt32.to_int t.seq)
            ~ack:(UInt32.to_int t.ack) ~win:(UInt16.to_int t.window)
            ~flags:t.flags ~off:t.data_offset
      | _, Some u ->
          udp_fields acc ~sp:(UInt16.to_int u.src_port)
            ~dp:(UInt16.to_int u.dst_port) ~len:(UInt16.to_int u.length)
      | _ -> acc)

(* {1 The fixed headers, linked by the harness}

   The rows below read each header with a codec of its own ([Headers]), as
   the wire.rust row does, and the harness makes the checks that link them:
   the bounds against the packet and the fragment test. [avail] is how many
   bytes of the frame follow the Ethernet header, [payload] how many the IPv4
   total length leaves the transport header. Each layer folds its fields into
   [acc] and hands the next one on; a header that fails its checks drops the
   packet, leaving [base], what [acc] was before it. *)

module H = Headers

(* {2 Wire.Codec: a checked view, then a read per field}

   The buffer is named once; [V.check c b ~off] checks a header of [c] at
   [off] and [V.get] turns the result into a view that only [b]'s reads of
   that codec's fields accept; each [V.read] of a field made once by
   [V.field] is then one bounded load, and a view is immediate, so nothing is
   allocated. *)

module V = Codec.View

let eth_type = V.field H.eth H.eth_type
let ip_ihl = V.field H.ipv4 H.ipv4_ihl
let ip_flags = V.field H.ipv4 H.ipv4_flags
let ip_frag = V.field H.ipv4 H.ipv4_fragment_offset
let ip_len = V.field H.ipv4 H.ipv4_total_length
let ip_ttl = V.field H.ipv4 H.ipv4_ttl
let ip_proto = V.field H.ipv4 H.ipv4_protocol
let ip_src = V.field H.ipv4 H.ipv4_src
let ip_dst = V.field H.ipv4 H.ipv4_dst
let tcp_sp = V.field H.tcp H.tcp_src_port
let tcp_dp = V.field H.tcp H.tcp_dst_port
let tcp_seq = V.field H.tcp H.tcp_seq
let tcp_ack = V.field H.tcp H.tcp_ack
let tcp_off = V.field H.tcp H.tcp_data_offset
let tcp_flags = V.field H.tcp H.tcp_flags
let tcp_win = V.field H.tcp H.tcp_window
let udp_sp = V.field H.udp H.udp_src_port
let udp_dp = V.field H.udp H.udp_dst_port
let udp_len = V.field H.udp H.udp_length

let codec_tcp base acc b o ~payload =
  let k = V.check H.tcp b ~off:o in
  if not (V.is_ok k) then base
  else
    let t = V.get k in
    let off = V.read b t tcp_off in
    if off * 4 > payload then base
    else
      tcp_fields acc
        ~sp:(UInt16.to_int (V.read b t tcp_sp))
        ~dp:(UInt16.to_int (V.read b t tcp_dp))
        ~seq:(UInt32.to_int (V.read b t tcp_seq))
        ~ack:(UInt32.to_int (V.read b t tcp_ack))
        ~win:(UInt16.to_int (V.read b t tcp_win))
        ~flags:(V.read b t tcp_flags) ~off

let codec_udp base acc b o ~payload =
  let k = V.check H.udp b ~off:o in
  if not (V.is_ok k) then base
  else
    let u = V.get k in
    let ulen = UInt16.to_int (V.read b u udp_len) in
    if ulen > payload then base
    else
      udp_fields acc
        ~sp:(UInt16.to_int (V.read b u udp_sp))
        ~dp:(UInt16.to_int (V.read b u udp_dp))
        ~len:ulen

let codec_ipv4 base acc b o ~avail =
  let k = V.check H.ipv4 b ~off:o in
  if not (V.is_ok k) then base
  else
    let ip = V.get k in
    let hlen = V.read b ip ip_ihl * 4
    and tlen = UInt16.to_int (V.read b ip ip_len) in
    if tlen < hlen || tlen > avail then base
    else
      let proto = UInt8.to_int (V.read b ip ip_proto) in
      let acc =
        ip_fields acc
          ~src:(UInt32.to_int (V.read b ip ip_src))
          ~dst:(UInt32.to_int (V.read b ip ip_dst))
          ~ttl:(UInt8.to_int (V.read b ip ip_ttl))
          ~proto ~len:tlen
      in
      if V.read b ip ip_flags land 1 <> 0 || V.read b ip ip_frag <> 0 then acc
      else
        let payload = tlen - hlen in
        match proto with
        | 6 -> codec_tcp base acc b (o + hlen) ~payload
        | 17 -> codec_udp base acc b (o + hlen) ~payload
        | _ -> acc

let codec_packet ~avail acc b o =
  let k = V.check H.eth b ~off:o in
  if not (V.is_ok k) then acc
  else
    let ty = UInt16.to_int (V.read b (V.get k) eth_type) in
    if ty = ipv4 then codec_ipv4 acc (acc + ty) b (o + 14) ~avail else acc + ty

(* {2 wire.ocaml: the generated view, then a read per field}

   The buffer is named once ([Buf.v]) and its length read once; then
   [check b len off] checks a header at [off] once and [get] turns the
   answer into a proof that only [b]'s accessors accept, as wire.rust's
   [Valid::at] does; each accessor [<field> b v] is then one load with no
   bounds check, and the proof is an [int], so nothing is allocated. *)

let view_tcp base acc b len o ~payload =
  let c = G_tcp.check b len o in
  if not (G_tcp.ok c) then base
  else
    let t = G_tcp.get c in
    let off = G_tcp.data_offset b t in
    if off * 4 > payload then base
    else
      tcp_fields acc
        ~sp:(UInt16.to_int (G_tcp.src_port b t))
        ~dp:(UInt16.to_int (G_tcp.dst_port b t))
        ~seq:(UInt32.to_int (G_tcp.seq b t))
        ~ack:(UInt32.to_int (G_tcp.ack b t))
        ~win:(UInt16.to_int (G_tcp.window b t))
        ~flags:(G_tcp.flags b t) ~off

let view_udp base acc b len o ~payload =
  let c = G_udp.check b len o in
  if not (G_udp.ok c) then base
  else
    let u = G_udp.get c in
    let ulen = UInt16.to_int (G_udp.length b u) in
    if ulen > payload then base
    else
      udp_fields acc
        ~sp:(UInt16.to_int (G_udp.src_port b u))
        ~dp:(UInt16.to_int (G_udp.dst_port b u))
        ~len:ulen

let view_ipv4 base acc b len o ~avail =
  let c = G_ipv4.check b len o in
  if not (G_ipv4.ok c) then base
  else
    let ip = G_ipv4.get c in
    let hlen = (G_ipv4.ihl b ip :> int) * 4
    and tlen = UInt16.to_int (G_ipv4.total_length b ip) in
    if tlen < hlen || tlen > avail then base
    else
      let proto = UInt8.to_int (G_ipv4.protocol b ip) in
      let acc =
        ip_fields acc
          ~src:(UInt32.to_int (G_ipv4.src_addr b ip))
          ~dst:(UInt32.to_int (G_ipv4.dst_addr b ip))
          ~ttl:(UInt8.to_int (G_ipv4.ttl b ip))
          ~proto ~len:tlen
      in
      if G_ipv4.flags b ip land 1 <> 0 || G_ipv4.fragment_offset b ip <> 0 then
        acc
      else
        let payload = tlen - hlen in
        match proto with
        | 6 -> view_tcp base acc b len (o + hlen) ~payload
        | 17 -> view_udp base acc b len (o + hlen) ~payload
        | _ -> acc

let view_packet ~avail acc b len o =
  let c = G_eth.check b len o in
  if not (G_eth.ok c) then acc
  else
    let e = G_eth.get c in
    let ty = UInt16.to_int (G_eth.ether_type b e) in
    if ty = ipv4 then view_ipv4 acc (acc + ty) b len (o + 14) ~avail
    else acc + ty

let oxcaml =
  match Sys.ocaml_release.extra with Some (_, "ox") -> true | _ -> false

(* The rows take the slot's stride at run time, as a reader of fixed-size
   frames that learns their size from its input does. *)

(* The frame size comes from the corpus's framing, while all protocol
   dispatch, header lengths and parent-region bounds come from Packet_stack. *)
let frame_field stride =
  Codec.(
    Field.v "packet"
      (nested_at_most ~size:(int stride) (codec Packet_stack.codec))
    $ Fun.id)

let frame_codec packet = Codec.v "BenchmarkFrame" (fun p -> p) [ packet ]

let codec ~stride frame_codec buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    match Codec.decode frame_codec buf ~off:(i * stride) with
    | Error _ -> ()
    | Ok packet -> acc := packet_fields !acc packet
  done;
  !acc

(* {2 Wire.Codec: the stack checked once, each layer reached}

   [V.check] of the frame makes every check [decode] makes, building nothing;
   [V.sub] then finds each layer the check accepted, testing only that its
   frame lies in the buffer, and refuses an absent one, and [V.read] reads its
   fields as in the linked rows. *)

module P = Packet_stack

let in_ip = V.layer P.codec (Codec.opt_layer P.eth_ipv4)
let in_tcp = V.layer P.ipv4 (Codec.opt_layer P.ipv4_tcp)
let in_udp = V.layer P.ipv4 (Codec.opt_layer P.ipv4_udp)
let p_type = V.field P.codec P.eth_ether_type
let p_len = V.field P.ipv4 P.ipv4_total_length
let p_ttl = V.field P.ipv4 P.ipv4_ttl
let p_proto = V.field P.ipv4 P.ipv4_protocol
let p_src = V.field P.ipv4 P.ipv4_src
let p_dst = V.field P.ipv4 P.ipv4_dst
let p_tcp_sp = V.field P.tcp P.tcp_src_port
let p_tcp_dp = V.field P.tcp P.tcp_dst_port
let p_tcp_seq = V.field P.tcp P.tcp_seq
let p_tcp_ack = V.field P.tcp P.tcp_ack
let p_tcp_off = V.field P.tcp P.tcp_data_offset
let p_tcp_flags = V.field P.tcp P.tcp_flags
let p_tcp_win = V.field P.tcp P.tcp_window
let p_udp_sp = V.field P.udp P.udp_src_port
let p_udp_dp = V.field P.udp P.udp_dst_port
let p_udp_len = V.field P.udp P.udp_length_field

let stack_transport acc b ip =
  let t = V.sub b ip in_tcp in
  if V.is_ok t then
    let t = V.get t in
    tcp_fields acc
      ~sp:(UInt16.to_int (V.read b t p_tcp_sp))
      ~dp:(UInt16.to_int (V.read b t p_tcp_dp))
      ~seq:(UInt32.to_int (V.read b t p_tcp_seq))
      ~ack:(UInt32.to_int (V.read b t p_tcp_ack))
      ~win:(UInt16.to_int (V.read b t p_tcp_win))
      ~flags:(V.read b t p_tcp_flags) ~off:(V.read b t p_tcp_off)
  else
    let u = V.sub b ip in_udp in
    if V.is_ok u then
      let u = V.get u in
      udp_fields acc
        ~sp:(UInt16.to_int (V.read b u p_udp_sp))
        ~dp:(UInt16.to_int (V.read b u p_udp_dp))
        ~len:(UInt16.to_int (V.read b u p_udp_len))
    else acc

let stack_packet acc b e =
  let acc = acc + UInt16.to_int (V.read b e p_type) in
  let ip = V.sub b e in_ip in
  if not (V.is_ok ip) then acc
  else
    let ip = V.get ip in
    let acc =
      ip_fields acc
        ~src:(UInt32.to_int (V.read b ip p_src))
        ~dst:(UInt32.to_int (V.read b ip p_dst))
        ~ttl:(UInt8.to_int (V.read b ip p_ttl))
        ~proto:(UInt8.to_int (V.read b ip p_proto))
        ~len:(UInt16.to_int (V.read b ip p_len))
    in
    stack_transport acc b ip

(* Each loop names the function it calls on every frame. *)

let codec_stack_view ~stride frame packet buf =
  let in_frame = V.layer frame (Codec.layer packet) in
  let (Buf.Any b) = Buf.v buf in
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    let k = V.check frame b ~off:(i * stride) in
    if V.is_ok k then
      let e = V.sub b (V.get k) in_frame in
      if V.is_ok e then acc := stack_packet !acc b (V.get e)
  done;
  !acc

let codec_linked ~stride buf =
  let (Buf.Any b) = Buf.v buf in
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := codec_packet ~avail:(stride - 14) !acc b (i * stride)
  done;
  !acc

let view_linked ~stride buf =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := view_packet ~avail:(stride - 14) !acc b len (i * stride)
  done;
  !acc

let generated_fold ~stride buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := Stack_read.fold !acc buf (i * stride) stride
  done;
  !acc

let generated_selected ~stride buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := Selected_read.fold !acc buf (i * stride) stride
  done;
  !acc

(* dune generates for the OxCaml target on an OxCaml compiler. *)
let target = if oxcaml then ", OxCaml target" else ""

let rows stride =
  let packet = frame_field stride in
  let frame_codec = frame_codec packet in
  [
    ("Wire.Codec stack decode", fun buf -> codec ~stride frame_codec buf);
    ( "Wire.Codec stack check/sub/read",
      codec_stack_view ~stride frame_codec packet );
    ("Wire.Codec check/read, linked headers", codec_linked ~stride);
    ("wire.ocaml View.check/view, linked headers" ^ target, view_linked ~stride);
    ("wire.ocaml fused stack, fold" ^ target, generated_fold ~stride);
    ( "wire.ocaml fused stack, batch" ^ target,
      fun buf -> Stack_read.batch ~stride 0 buf 0 (Bytes.length buf) );
    ("wire.ocaml selected outputs, fold" ^ target, generated_selected ~stride);
    ( "wire.ocaml selected outputs, batch" ^ target,
      fun buf -> Selected_read.batch ~stride 0 buf 0 (Bytes.length buf) );
  ]

(* {1 Timing}

   [Bench_lib.measure] times each row: one warm-up pass, which also gives the
   checksum, then [runs] timed passes, each reading the wall clock and the
   process CPU time around the call, as cargo/src/main.rs does. [filter]
   can select one row by its name, so a process can be measured whole for one
   row. *)

let mask62 = (1 lsl 62) - 1

let run file stride runs filter =
  let buf = Bench_lib.read_file file in
  if stride <= 0 || Bytes.length buf mod stride <> 0 then
    Fmt.failwith "bench.exe: %s is not a whole number of %d-byte slots" file
      stride;
  let corpus = Filename.remove_extension (Filename.basename file) in
  List.iter
    (fun (name, f) ->
      Bench_lib.measure ~corpus ~filter ~name
        ~items:(Bytes.length buf / stride)
        ~runs ~pp:Fmt.int
        (fun () -> f (Sys.opaque_identity buf) land mask62))
    (rows stride)

(* {1 The flow classifier}

   A capture, held in memory, through a flow classifier: each packet is parsed
   by the row, its 5-tuple (addresses, protocol, and the ports unless it is a
   fragment or has no TCP or UDP header) is hashed into [Flow_table], which
   counts the packets and the IP bytes of each flow. Every row parses with the
   same checks, those of [Packet_stack.capture]: each layer bounded by the
   bytes captured, each length field covering its header but bounding nothing,
   and a packet counted only when every header it has passes. The rows print
   the number of flows and a checksum of the table, read after the timed
   passes, which must agree.

   The capture is a pcap file of Ethernet frames, kept whole in one buffer:
   [index] gives the offset and the captured length of each frame in it. *)

let pcap_magic_us = 0xa1b2c3d4
let pcap_magic_ns = 0xa1b23c4d

let check_pcap_header h =
  let magic = Bytes.get_int32_le h 0 |> Int32.to_int |> ( land ) 0xffff_ffff in
  if magic <> pcap_magic_us && magic <> pcap_magic_ns then
    Fmt.failwith "not a little-endian pcap file (magic 0x%08x)" magic;
  let link = Bytes.get_int32_le h 20 |> Int32.to_int in
  if link <> 1 then Fmt.failwith "link type %d, not Ethernet (1)" link

(* [flow-extract N OUT]: the pcap header and the first [n] packets of the pcap
   stream on stdin, written to [out]. *)
let extract n out =
  set_binary_mode_in stdin true;
  let header = Bytes.create 24 in
  really_input stdin header 0 24;
  check_pcap_header header;
  Out_channel.with_open_bin out (fun oc ->
      Out_channel.output_bytes oc header;
      let record = Bytes.create 16 in
      let packet = Bytes.create 65536 in
      let rec go k =
        if k < n then (
          match really_input stdin record 0 16 with
          | exception End_of_file -> k
          | () ->
              let caplen = Int32.to_int (Bytes.get_int32_le record 8) in
              if caplen < 0 || caplen > 65536 then
                Fmt.failwith "packet %d: captured length %d" k caplen;
              really_input stdin packet 0 caplen;
              Out_channel.output_bytes oc record;
              Out_channel.output oc packet 0 caplen;
              go (k + 1))
        else k
      in
      let k = go 0 in
      if k < n then Fmt.failwith "the trace has %d packets, not %d" k n)

(* The offset and captured length of each frame of the pcap file [buf]. *)
let index buf =
  if Bytes.length buf < 24 then failwith "truncated pcap header";
  check_pcap_header buf;
  let offs = ref [] and lens = ref [] in
  let o = ref 24 in
  while !o < Bytes.length buf do
    if !o + 16 > Bytes.length buf then failwith "truncated pcap record";
    let caplen = Int32.to_int (Bytes.get_int32_le buf (!o + 8)) in
    if caplen < 0 || !o + 16 + caplen > Bytes.length buf then
      failwith "truncated pcap packet";
    offs := (!o + 16) :: !offs;
    lens := caplen :: !lens;
    o := !o + 16 + caplen
  done;
  (Array.of_list (List.rev !offs), Array.of_list (List.rev !lens))

(* {2 The trace's mix}

   [flow-mix FILE] counts what the rows meet in the capture, read byte by
   byte, independently of every parser here. *)

(* What [mix] counts, every one printed, zeros included. *)
let mix_keys =
  [
    "truncated";
    "short Ethernet";
    "VLAN";
    "neither IPv4 nor IPv6";
    "IPv4";
    "IPv4 header cut";
    "IPv4 version not 4";
    "IPv4 options";
    "IPv4 total length under header";
    "IPv4 total length past capture";
    "IPv4 AH";
    "IPv4 fragment";
    "IPv6";
    "IPv6 header cut";
    "IPv6 payload length past capture";
    "IPv6 extension header";
    "lax slicing differs";
  ]

let ipv6_extensions = [ 0; 43; 44; 50; 51; 60; 135; 139; 140; 253; 254 ]

(* The transport header of a packet whose IP length ends at [end_], before
   the capture ends at [cap]. Where the header runs past [end_] but not past
   [cap], etherparse's lax slicing, which cuts the payload at the IP length,
   rejects a header these rows accept. *)
let mix_transport bump buf ~l4 ~end_ ~cap proto =
  if proto = 6 && l4 + 13 <= cap then begin
    let hlen = (Bytes.get_uint8 buf (l4 + 12) lsr 4) * 4 in
    if l4 + hlen > end_ && l4 + hlen <= cap then bump "lax slicing differs"
  end
  else if proto = 17 && l4 + 8 <= cap && l4 + 8 > end_ then
    bump "lax slicing differs"

let mix_ipv4 bump buf ~cap ip =
  bump "IPv4";
  if ip + 20 > cap then bump "IPv4 header cut"
  else
    let b = Bytes.get_uint8 buf ip
    and total = Bytes.get_uint16_be buf (ip + 2) in
    let ihl = b land 15 and proto = Bytes.get_uint8 buf (ip + 9) in
    if b lsr 4 <> 4 then bump "IPv4 version not 4";
    if ihl > 5 then bump "IPv4 options";
    if total < ihl * 4 then bump "IPv4 total length under header";
    if ip + total > cap then bump "IPv4 total length past capture";
    if proto = 51 then bump "IPv4 AH";
    if Bytes.get_uint16_be buf (ip + 6) land 0x3fff <> 0 then
      bump "IPv4 fragment"
    else if ip + total < cap then
      mix_transport bump buf ~l4:(ip + (ihl * 4)) ~end_:(ip + total) ~cap proto

let mix_ipv6 bump buf ~cap ip =
  bump "IPv6";
  if ip + 40 > cap then bump "IPv6 header cut"
  else
    let len = Bytes.get_uint16_be buf (ip + 4)
    and nh = Bytes.get_uint8 buf (ip + 6) in
    if ip + 40 + len > cap then bump "IPv6 payload length past capture";
    if List.mem nh ipv6_extensions then bump "IPv6 extension header"
    else if ip + 40 + len < cap then
      mix_transport bump buf ~l4:(ip + 40) ~end_:(ip + 40 + len) ~cap nh

let mix_frame bump buf o caplen =
  let cap = o + caplen in
  if caplen < Int32.to_int (Bytes.get_int32_le buf (o - 4)) then
    bump "truncated";
  if caplen < 14 then bump "short Ethernet"
  else
    match Bytes.get_uint16_be buf (o + 12) with
    | 0x8100 | 0x88a8 | 0x9100 -> bump "VLAN"
    | 0x0800 -> mix_ipv4 bump buf ~cap (o + 14)
    | 0x86dd -> mix_ipv6 bump buf ~cap (o + 14)
    | _ -> bump "neither IPv4 nor IPv6"

let mix file =
  let buf = Bench_lib.read_file file in
  let offs, lens = index buf in
  let counts = Hashtbl.create 32 in
  List.iter (fun k -> Hashtbl.replace counts k 0) mix_keys;
  let bump k = Hashtbl.replace counts k (1 + Hashtbl.find counts k) in
  Array.iteri (fun i o -> mix_frame bump buf o lens.(i)) offs;
  let n = Array.length offs in
  Fmt.pr "%d packets, captured lengths %d to %d bytes@." n
    (Array.fold_left min max_int lens)
    (Array.fold_left max 0 lens);
  List.iter
    (fun k ->
      let v = Hashtbl.find counts k in
      Fmt.pr "%-36s %10d %7.3f%%@." k v (100. *. float v /. float n))
    mix_keys

(* {2 The rows} *)

(* wire.ocaml: the scanner of [Packet_stack.capture] over the frames of the
   index, handing out only the classifier's fields to [Flow_step], which it
   calls by name, or to [Flow_parse] for [flow_scanner_parse]. *)
let flow_scanner = Flow_read.classify
let flow_scanner_parse = Flow_read.parse

(* The rows of the classifier, each on its own the parse and the table: the
   parse rows hand their keys to [Flow_sum], and the table row takes the keys
   from an array, as does the row folding them without a parse. *)
let flow_rows =
  [
    ( "Wire.Codec capture decode",
      Flow_rows.flow_codec,
      Flow_rows_parse.flow_codec );
    ( "wire.ocaml View.check/view, linked headers" ^ target,
      Flow_rows.flow_view,
      Flow_rows_parse.flow_view );
    ( "wire.ocaml scanner, selected outputs, fold_step" ^ target,
      flow_scanner,
      flow_scanner_parse );
    ( "wire.ocaml check/view, accessors" ^ target,
      Flow_rows.flow_check,
      Flow_rows_parse.flow_check );
  ]

let pp_flows ppf (flows, sum) = Fmt.pf ppf "%d flows, %d" flows sum
let pp_keys ppf (keys, sum) = Fmt.pf ppf "%d keys, %d" keys sum

(* The swap rows, on corpora of their own: one flow counter, flow_swap.ml,
   over the modules wire.ocaml generates from the header codecs and over
   their interpreted twins, with the table and without it. Only IPv4
   makes a flow; the other packets are counted by kind, and every row prints
   both. *)
let swap_rows =
  [
    ( "swap=interpreted, twin",
      Flow_swap_interpreted.run,
      Flow_swap_interpreted_parse.run );
    ("swap=generated" ^ target, Flow_swap.run, Flow_swap_parse.run);
  ]

let pp_mix pp ppf (r, mix) = Fmt.pf ppf "%a; %a" pp r Flow_mix.pp mix

let flow file runs filter =
  let buf = Bench_lib.read_file file in
  let offs, lens = index buf in
  let n = Array.length offs in
  Flow_table.init ~capacity:(2 * n);
  let corpus = Filename.remove_extension (Filename.basename file) in
  let measure ~corpus ~name ~pp ~result reset f =
    Bench_lib.measure_state ~corpus ~filter ~name ~items:n ~runs ~pp ~result
      (fun () ->
        reset ();
        f ())
  in
  let table = measure ~pp:pp_flows ~result:Flow_table.summary Flow_table.reset
  and parse = measure ~pp:pp_keys ~result:Flow_sum.summary Flow_sum.reset in
  List.iter
    (fun (name, t, p) ->
      table ~corpus ~name (fun () -> t (Sys.opaque_identity buf) offs lens);
      parse ~corpus:(corpus ^ " parse") ~name:(name ^ ", parse only") (fun () ->
          p (Sys.opaque_identity buf) offs lens))
    flow_rows;
  let with_mix ~pp ~result reset =
    measure ~pp:(pp_mix pp)
      ~result:(fun () -> (result (), Flow_mix.state))
      (fun () ->
        reset ();
        Flow_mix.reset ())
  in
  let swap = with_mix ~pp:pp_flows ~result:Flow_table.summary Flow_table.reset
  and swap_parse =
    with_mix ~pp:pp_keys ~result:Flow_sum.summary Flow_sum.reset
  in
  List.iter
    (fun (name, t, p) ->
      swap ~corpus:(corpus ^ " swap") ~name (fun () ->
          t (Sys.opaque_identity buf) offs lens);
      swap_parse ~corpus:(corpus ^ " swap parse") ~name:(name ^ ", parse only")
        (fun () -> p (Sys.opaque_identity buf) offs lens))
    swap_rows;
  let keys =
    lazy
      (Flow_rows_keys.flow_view buf offs lens;
       Flow_keys.take ())
  in
  table ~corpus:(corpus ^ " table")
    ~name:("flow table, keys from an array" ^ target) (fun () ->
      Flow_rows.replay (Sys.opaque_identity (Lazy.force keys)));
  parse ~corpus:(corpus ^ " parse")
    ~name:("keys from an array, parse only" ^ target) (fun () ->
      Flow_rows_parse.replay (Sys.opaque_identity (Lazy.force keys)))

let () =
  match Array.to_list Sys.argv with
  | [ _; "corpus"; dir; n ] -> corpora dir (int_of_string n)
  | _ :: "run" :: file :: stride :: runs :: rest ->
      let filter =
        Bench_lib.filter (match rest with [ f ] -> Some f | _ -> None)
      in
      run file (int_of_string stride) (int_of_string runs) filter;
      Bench_lib.finish filter
  | [ _; "flow-extract"; n; out ] -> extract (int_of_string n) out
  | [ _; "flow-mix"; file ] -> mix file
  | _ :: "flow" :: file :: runs :: rest ->
      let filter =
        Bench_lib.filter (match rest with [ f ] -> Some f | _ -> None)
      in
      flow file (int_of_string runs) filter;
      Bench_lib.finish filter
  | _ :: "report" :: files -> Bench_lib.report ~unit:(fun _ -> "packet") files
  | _ ->
      prerr_endline
        "usage: bench.exe (corpus DIR N | run FILE STRIDE RUNS [ROW | --list] \
         | flow-extract N OUT | flow-mix FILE | flow FILE RUNS [ROW | --list] \
         | report FILE...)";
      exit 2
