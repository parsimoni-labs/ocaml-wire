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

(* {2 Wire.Codec: check, then a proof-carrying read per field}

   [Codec.check] answers a header that fails its checks with a proof [Codec.ok]
   refuses, so a bad header costs no exception. *)

let eth_type = Staged.unstage (Codec.read H.eth H.eth_type)
let ip_ihl = Staged.unstage (Codec.read H.ipv4 H.ipv4_ihl)
let ip_flags = Staged.unstage (Codec.read H.ipv4 H.ipv4_flags)
let ip_frag = Staged.unstage (Codec.read H.ipv4 H.ipv4_fragment_offset)
let ip_len = Staged.unstage (Codec.read H.ipv4 H.ipv4_total_length)
let ip_ttl = Staged.unstage (Codec.read H.ipv4 H.ipv4_ttl)
let ip_proto = Staged.unstage (Codec.read H.ipv4 H.ipv4_protocol)
let ip_src = Staged.unstage (Codec.read H.ipv4 H.ipv4_src)
let ip_dst = Staged.unstage (Codec.read H.ipv4 H.ipv4_dst)
let tcp_sp = Staged.unstage (Codec.read H.tcp H.tcp_src_port)
let tcp_dp = Staged.unstage (Codec.read H.tcp H.tcp_dst_port)
let tcp_seq = Staged.unstage (Codec.read H.tcp H.tcp_seq)
let tcp_ack = Staged.unstage (Codec.read H.tcp H.tcp_ack)
let tcp_off = Staged.unstage (Codec.read H.tcp H.tcp_data_offset)
let tcp_flags = Staged.unstage (Codec.read H.tcp H.tcp_flags)
let tcp_win = Staged.unstage (Codec.read H.tcp H.tcp_window)
let udp_sp = Staged.unstage (Codec.read H.udp H.udp_src_port)
let udp_dp = Staged.unstage (Codec.read H.udp H.udp_dst_port)
let udp_len = Staged.unstage (Codec.read H.udp H.udp_length)

let codec_tcp base acc buf o ~payload =
  let t = Codec.check H.tcp buf o in
  if not (Codec.ok t) then base
  else
    let off = tcp_off t in
    if off * 4 > payload then base
    else
      tcp_fields acc
        ~sp:(UInt16.to_int (tcp_sp t))
        ~dp:(UInt16.to_int (tcp_dp t))
        ~seq:(UInt32.to_int (tcp_seq t))
        ~ack:(UInt32.to_int (tcp_ack t))
        ~win:(UInt16.to_int (tcp_win t))
        ~flags:(tcp_flags t) ~off

let codec_udp base acc buf o ~payload =
  let u = Codec.check H.udp buf o in
  if not (Codec.ok u) then base
  else
    let len = UInt16.to_int (udp_len u) in
    if len > payload then base
    else
      udp_fields acc
        ~sp:(UInt16.to_int (udp_sp u))
        ~dp:(UInt16.to_int (udp_dp u))
        ~len

let codec_ipv4 base acc buf o ~avail =
  let ip = Codec.check H.ipv4 buf o in
  if not (Codec.ok ip) then base
  else
    let hlen = ip_ihl ip * 4 and len = UInt16.to_int (ip_len ip) in
    if len < hlen || len > avail then base
    else
      let proto = UInt8.to_int (ip_proto ip) in
      let acc =
        ip_fields acc
          ~src:(UInt32.to_int (ip_src ip))
          ~dst:(UInt32.to_int (ip_dst ip))
          ~ttl:(UInt8.to_int (ip_ttl ip))
          ~proto ~len
      in
      if ip_flags ip land 1 <> 0 || ip_frag ip <> 0 then acc
      else
        let payload = len - hlen in
        match proto with
        | 6 -> codec_tcp base acc buf (o + hlen) ~payload
        | 17 -> codec_udp base acc buf (o + hlen) ~payload
        | _ -> acc

let codec_packet ~avail acc buf o =
  let e = Codec.check H.eth buf o in
  if not (Codec.ok e) then acc
  else
    let ty = UInt16.to_int (eth_type e) in
    if ty = ipv4 then codec_ipv4 acc (acc + ty) buf (o + 14) ~avail
    else acc + ty

(* {2 wire.ocaml: the generated view, then a read per field}

   The buffer is named once ([Buf.v]) and its length read once; then
   [View.check b len off] checks a header at [off] once and [View.get] turns
   the answer into a proof that only [b]'s reads accept, as wire.rust's
   [Valid::at] does; each [view_<field> b v] is then one load with no bounds
   check, and the proof is an [int], so nothing is allocated. *)

let view_tcp base acc b len o ~payload =
  let c = G_tcp.View.check b len o in
  if not (G_tcp.View.ok c) then base
  else
    let t = G_tcp.View.get c in
    let w = G_tcp.view_data_offset_word b t in
    let off = G_tcp.data_offset_of_word w in
    if off * 4 > payload then base
    else
      tcp_fields acc
        ~sp:(UInt16.to_int (G_tcp.view_src_port b t))
        ~dp:(UInt16.to_int (G_tcp.view_dst_port b t))
        ~seq:(G_tcp.view_seq_int b t) ~ack:(G_tcp.view_ack_int b t)
        ~win:(UInt16.to_int (G_tcp.view_window b t))
        ~flags:(G_tcp.flags_of_word w) ~off

let view_udp base acc b len o ~payload =
  let c = G_udp.View.check b len o in
  if not (G_udp.View.ok c) then base
  else
    let u = G_udp.View.get c in
    let ulen = UInt16.to_int (G_udp.view_length b u) in
    if ulen > payload then base
    else
      udp_fields acc
        ~sp:(UInt16.to_int (G_udp.view_src_port b u))
        ~dp:(UInt16.to_int (G_udp.view_dst_port b u))
        ~len:ulen

let view_ipv4 base acc b len o ~avail =
  let c = G_ipv4.View.check b len o in
  if not (G_ipv4.View.ok c) then base
  else
    let ip = G_ipv4.View.get c in
    let hlen = (G_ipv4.view_ihl b ip :> int) * 4
    and tlen = UInt16.to_int (G_ipv4.view_total_length b ip) in
    if tlen < hlen || tlen > avail then base
    else
      let proto = UInt8.to_int (G_ipv4.view_protocol b ip) in
      let acc =
        ip_fields acc
          ~src:(G_ipv4.view_src_addr_int b ip)
          ~dst:(G_ipv4.view_dst_addr_int b ip)
          ~ttl:(UInt8.to_int (G_ipv4.view_ttl b ip))
          ~proto ~len:tlen
      in
      let w = G_ipv4.view_flags_word b ip in
      if
        G_ipv4.flags_of_word w land 1 <> 0
        || G_ipv4.fragment_offset_of_word w <> 0
      then acc
      else
        let payload = tlen - hlen in
        match proto with
        | 6 -> view_tcp base acc b len (o + hlen) ~payload
        | 17 -> view_udp base acc b len (o + hlen) ~payload
        | _ -> acc

let view_packet ~avail acc b len o =
  let c = G_eth.View.check b len o in
  if not (G_eth.View.ok c) then acc
  else
    let e = G_eth.View.get c in
    let ty = UInt16.to_int (G_eth.view_ether_type b e) in
    if ty = ipv4 then view_ipv4 acc (acc + ty) b len (o + 14) ~avail
    else acc + ty

let oxcaml = Stack_read.oxcaml

(* The rows take the slot's stride at run time, as a reader of fixed-size
   frames that learns their size from its input does. *)

(* The frame size comes from the corpus's framing, while all protocol
   dispatch, header lengths and parent-region bounds come from Packet_stack. *)
let frame_codec stride =
  Codec.v "BenchmarkFrame"
    (fun p -> p)
    Codec.
      [
        Field.v "packet"
          (nested_at_most ~size:(int stride) (codec Packet_stack.codec))
        $ Fun.id;
      ]

let codec ~stride frame_codec buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    match Codec.decode frame_codec buf (i * stride) with
    | Error _ -> ()
    | Ok packet -> acc := packet_fields !acc packet
  done;
  !acc

(* Each loop names the function it calls on every frame. *)

let codec_linked ~stride buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := codec_packet ~avail:(stride - 14) !acc buf (i * stride)
  done;
  !acc

let view_linked ~stride buf =
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let len = Wire_runtime.Buf.length b in
      let acc = ref 0 in
      for i = 0 to (Bytes.length buf / stride) - 1 do
        acc := view_packet ~avail:(stride - 14) !acc b len (i * stride)
      done;
      !acc

let generated ~stride buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := Stack_read.boxed !acc buf (i * stride) stride
  done;
  !acc

let generated_unboxed ~stride buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / stride) - 1 do
    acc := Stack_read.unboxed !acc buf (i * stride) stride
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
  let frame_codec = frame_codec stride in
  [
    ("Wire.Codec stack decode", fun buf -> codec ~stride frame_codec buf);
    ("Wire.Codec check/read, linked headers", codec_linked ~stride);
    ("wire.ocaml View.check/view, linked headers" ^ target, view_linked ~stride);
    ("wire.ocaml fused stack" ^ target, generated ~stride);
    ("wire.ocaml fused stack, fold" ^ target, generated_fold ~stride);
    ( "wire.ocaml fused stack, batch" ^ target,
      fun buf -> Stack_read.batch ~stride 0 buf 0 (Bytes.length buf) );
  ]
  @ (if oxcaml then
       [
         ( "wire.ocaml fused stack, unboxed batch",
           fun buf ->
             Stack_read.batch_unboxed ~stride 0 buf 0 (Bytes.length buf) );
       ]
     else [])
  @ [
      ("wire.ocaml selected outputs, fold" ^ target, generated_selected ~stride);
      ( "wire.ocaml selected outputs, batch" ^ target,
        fun buf -> Selected_read.batch ~stride 0 buf 0 (Bytes.length buf) );
    ]
  @
  if oxcaml then
    [
      ( "wire.ocaml selected outputs, unboxed batch",
        fun buf ->
          Selected_read.batch_unboxed ~stride 0 buf 0 (Bytes.length buf) );
      ("wire.ocaml fused stack, unboxed API", generated_unboxed ~stride);
    ]
  else []

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

(* [Wire.Codec]: the pcap record around each frame bounds it, and
   [Codec.decode] builds the nested value of [Packet_stack.capture]. *)
let pcap_record =
  let incl_len = Field.v "incl_len" uint32 in
  Codec.v "PcapRecord"
    (fun _ _ _ _ p -> p)
    Codec.
      [
        (Field.v "ts_sec" uint32 $ fun _ -> UInt32.zero);
        (Field.v "ts_frac" uint32 $ fun _ -> UInt32.zero);
        (incl_len $ fun _ -> UInt32.zero);
        (Field.v "orig_len" uint32 $ fun _ -> UInt32.zero);
        Field.v "packet"
          (nested_at_most ~size:(Field.ref incl_len)
             (Wire.codec Packet_stack.capture))
        $ Fun.id;
      ]

let classify_value (p : Packet_stack.capture) =
  let port = UInt16.to_int in
  match (p.ipv4, p.ipv6) with
  | Some ip, _ ->
      let ports =
        match (ip.tcp, ip.udp) with
        | Some t, _ -> (port t.src_port lsl 16) lor port t.dst_port
        | _, Some u -> (port u.src_port lsl 16) lor port u.dst_port
        | _ -> 0
      in
      Flow_table.add4 ~src:(UInt32.to_int ip.src) ~dst:(UInt32.to_int ip.dst)
        ~proto:(UInt8.to_int ip.protocol) ~ports
        ~bytes:(UInt16.to_int ip.total_length)
  | _, Some ip ->
      let ports =
        match (ip.tcp, ip.udp) with
        | Some t, _ -> (port t.src_port lsl 16) lor port t.dst_port
        | _, Some u -> (port u.src_port lsl 16) lor port u.dst_port
        | _ -> 0
      in
      let w = UInt32.to_int in
      Flow_table.add6 ~s0:(w ip.src0) ~s1:(w ip.src1) ~s2:(w ip.src2)
        ~s3:(w ip.src3) ~d0:(w ip.dst0) ~d1:(w ip.dst1) ~d2:(w ip.dst2)
        ~d3:(w ip.dst3)
        ~proto:(UInt8.to_int ip.next_header)
        ~ports
        ~bytes:(40 + UInt16.to_int ip.payload_length)
  | None, None -> ()

let flow_codec buf (offs : int array) _ =
  for i = 0 to Array.length offs - 1 do
    match Codec.decode pcap_record buf (offs.(i) - 16) with
    | Error _ -> ()
    | Ok p -> classify_value p
  done

(* wire.ocaml: the scanner of [Packet_stack.capture], handing out only the
   classifier's fields to [Flow_step], which it calls by name. *)
let flow_scanner buf (offs : int array) (lens : int array) =
  for i = 0 to Array.length offs - 1 do
    Flow_read.classify buf offs.(i) lens.(i)
  done

(* wire.ocaml, linked headers: the record modules of the fixed headers, and
   the harness making the checks that link them, the bounds against the
   capture ending at [cap] included. *)
let view_ports b len ~cap o proto =
  if proto = 6 then
    let c = G_tcp.View.check b len o in
    if not (G_tcp.View.ok c) then -1
    else
      let t = G_tcp.View.get c in
      let off = G_tcp.data_offset_of_word (G_tcp.view_data_offset_word b t) in
      if o + (off * 4) > cap then -1
      else
        (UInt16.to_int (G_tcp.view_src_port b t) lsl 16)
        lor UInt16.to_int (G_tcp.view_dst_port b t)
  else if proto = 17 then
    if o + 8 > cap then -1
    else
      let c = G_udp.View.check b len o in
      if not (G_udp.View.ok c) then -1
      else
        let u = G_udp.View.get c in
        (UInt16.to_int (G_udp.view_src_port b u) lsl 16)
        lor UInt16.to_int (G_udp.view_dst_port b u)
  else 0

let view_ipv4 b len ~cap o =
  if o + 20 <= cap then
    let c = G_ipv4.View.check b len o in
    if G_ipv4.View.ok c then
      let ip = G_ipv4.View.get c in
      let hlen = (G_ipv4.view_ihl b ip :> int) * 4
      and total = UInt16.to_int (G_ipv4.view_total_length b ip) in
      if total >= hlen && o + hlen <= cap then
        let proto = UInt8.to_int (G_ipv4.view_protocol b ip) in
        let w = G_ipv4.view_flags_word b ip in
        let ports =
          if
            G_ipv4.flags_of_word w land 1 <> 0
            || G_ipv4.fragment_offset_of_word w <> 0
          then 0
          else view_ports b len ~cap (o + hlen) proto
        in
        if ports >= 0 then
          Flow_table.add4
            ~src:(G_ipv4.view_src_addr_int b ip)
            ~dst:(G_ipv4.view_dst_addr_int b ip)
            ~proto ~ports ~bytes:total

let view_ipv6 b len ~cap o =
  if o + 40 <= cap then
    let c = G_ipv6.View.check b len o in
    if G_ipv6.View.ok c then
      let ip = G_ipv6.View.get c in
      let proto = UInt8.to_int (G_ipv6.view_next_header b ip) in
      let ports = view_ports b len ~cap (o + 40) proto in
      if ports >= 0 then
        Flow_table.add6
          ~s0:(G_ipv6.view_src0_int b ip)
          ~s1:(G_ipv6.view_src1_int b ip)
          ~s2:(G_ipv6.view_src2_int b ip)
          ~s3:(G_ipv6.view_src3_int b ip)
          ~d0:(G_ipv6.view_dst0_int b ip)
          ~d1:(G_ipv6.view_dst1_int b ip)
          ~d2:(G_ipv6.view_dst2_int b ip)
          ~d3:(G_ipv6.view_dst3_int b ip)
          ~proto ~ports
          ~bytes:(40 + UInt16.to_int (G_ipv6.view_payload_length b ip))

let view_frame b len o caplen =
  let cap = o + caplen in
  if caplen >= 14 then
    let c = G_eth.View.check b len o in
    if G_eth.View.ok c then
      let ty = UInt16.to_int (G_eth.view_ether_type b (G_eth.View.get c)) in
      if ty = 0x0800 then view_ipv4 b len ~cap (o + 14)
      else if ty = 0x86dd then view_ipv6 b len ~cap (o + 14)

let flow_view buf (offs : int array) (lens : int array) =
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let len = Wire_runtime.Buf.length b in
      for i = 0 to Array.length offs - 1 do
        view_frame b len offs.(i) lens.(i)
      done

let flow_rows =
  [
    ("Wire.Codec capture decode", flow_codec);
    ("wire.ocaml View.check/view, linked headers" ^ target, flow_view);
    ("wire.ocaml scanner, selected outputs, fold_step" ^ target, flow_scanner);
  ]

let pp_flows ppf (flows, sum) = Fmt.pf ppf "%d flows, %d" flows sum

let flow file runs filter =
  let buf = Bench_lib.read_file file in
  let offs, lens = index buf in
  let n = Array.length offs in
  Flow_table.init ~capacity:(2 * n);
  let corpus = Filename.remove_extension (Filename.basename file) in
  List.iter
    (fun (name, f) ->
      Bench_lib.measure_state ~corpus ~filter ~name ~items:n ~runs ~pp:pp_flows
        ~result:Flow_table.summary (fun () ->
          Flow_table.reset ();
          f (Sys.opaque_identity buf) offs lens))
    flow_rows

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
