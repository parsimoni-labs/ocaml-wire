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

(* {2 Wire.Codec: valid, then a proof-carrying read per field} *)

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
  match Codec.valid H.tcp buf o with
  | exception Parse_error _ -> base
  | t ->
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
  match Codec.valid H.udp buf o with
  | exception Parse_error _ -> base
  | u ->
      let len = UInt16.to_int (udp_len u) in
      if len > payload then base
      else
        udp_fields acc
          ~sp:(UInt16.to_int (udp_sp u))
          ~dp:(UInt16.to_int (udp_dp u))
          ~len

let codec_ipv4 base acc buf o ~avail =
  match Codec.valid H.ipv4 buf o with
  | exception Parse_error _ -> base
  | ip -> (
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
          | _ -> acc)

let codec_packet ~avail acc buf o =
  match Codec.valid H.eth buf o with
  | exception Parse_error _ -> acc
  | e ->
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

(* The rows for one stride. The stride is a literal at the application, as a
   caller with fixed-size frames would pass it; whether the loop sees it as a
   constant is the compiler's choice, as the loops carry no inlining
   attribute. *)
module type STRIDE = sig
  val stride : int
end

module Rows (S : STRIDE) = struct
  let stride = S.stride

  (* The frame size comes from the corpus's framing, while all protocol
     dispatch, header lengths and parent-region bounds come from
     Packet_stack. *)
  let frame_codec =
    Codec.v "BenchmarkFrame"
      (fun p -> p)
      Codec.
        [
          Field.v "packet"
            (nested_at_most ~size:(int stride) (codec Packet_stack.codec))
          $ Fun.id;
        ]

  let codec buf =
    let acc = ref 0 in
    for i = 0 to (Bytes.length buf / stride) - 1 do
      match Codec.decode frame_codec buf (i * stride) with
      | Error _ -> ()
      | Ok packet -> acc := packet_fields !acc packet
    done;
    !acc

  let frames f buf =
    let acc = ref 0 in
    for i = 0 to (Bytes.length buf / stride) - 1 do
      acc := f !acc buf (i * stride) stride
    done;
    !acc

  let linked f buf =
    let acc = ref 0 in
    for i = 0 to (Bytes.length buf / stride) - 1 do
      acc := f ~avail:(stride - 14) !acc buf (i * stride)
    done;
    !acc

  let codec_linked buf = linked codec_packet buf

  let view_linked buf =
    match Wire_runtime.Buf.v buf with
    | Wire_runtime.Buf.Any b ->
        let len = Wire_runtime.Buf.length b in
        let acc = ref 0 in
        for i = 0 to (Bytes.length buf / stride) - 1 do
          acc := view_packet ~avail:(stride - 14) !acc b len (i * stride)
        done;
        !acc

  let generated buf = frames Stack_read.boxed buf
  let generated_unboxed buf = frames Stack_read.unboxed buf
  let generated_fold buf = frames Stack_read.fold buf
  let generated_selected buf = frames Selected_read.fold buf
  let generated_batch buf = Stack_read.batch ~stride 0 buf 0 (Bytes.length buf)

  let generated_batch_unboxed buf =
    Stack_read.batch_unboxed ~stride 0 buf 0 (Bytes.length buf)

  let generated_selected_batch buf =
    Selected_read.batch ~stride 0 buf 0 (Bytes.length buf)

  let generated_selected_batch_unboxed buf =
    Selected_read.batch_unboxed ~stride 0 buf 0 (Bytes.length buf)

  (* dune generates for the OxCaml target on an OxCaml compiler. *)
  let target = if oxcaml then ", OxCaml target" else ""

  let all =
    [
      ("Wire.Codec stack decode", codec);
      ("Wire.Codec valid/read, linked headers", codec_linked);
      ("wire.ocaml View.check/view, linked headers" ^ target, view_linked);
      ("wire.ocaml fused stack" ^ target, generated);
      ("wire.ocaml fused stack, fold" ^ target, generated_fold);
      ("wire.ocaml fused stack, batch" ^ target, generated_batch);
    ]
    @ (if oxcaml then
         [ ("wire.ocaml fused stack, unboxed batch", generated_batch_unboxed) ]
       else [])
    @ [
        ("wire.ocaml selected outputs, fold" ^ target, generated_selected);
        ("wire.ocaml selected outputs, batch" ^ target, generated_selected_batch);
      ]
    @
    if oxcaml then
      [
        ( "wire.ocaml selected outputs, unboxed batch",
          generated_selected_batch_unboxed );
        ("wire.ocaml fused stack, unboxed API", generated_unboxed);
      ]
    else []
end

module Rows64 = Rows (struct
  let stride = 64
end)

module Rows128 = Rows (struct
  let stride = 128
end)

(* {1 Timing}

   [Bench_lib.measure] times each row: one warm-up pass, which also gives the
   checksum, then [runs] timed passes, each reading the wall clock and the
   process CPU time around the call, as cargo/src/main.rs does. [filter]
   can select one row by its name, so a process can be measured whole for one
   row. *)

let mask62 = (1 lsl 62) - 1

let run file stride runs filter =
  let rows =
    match stride with
    | 64 -> Rows64.all
    | 128 -> Rows128.all
    | _ -> Fmt.failwith "bench.exe: no rows for a stride of %d bytes" stride
  in
  let buf = Bench_lib.read_file file in
  if Bytes.length buf mod stride <> 0 then
    Fmt.failwith "bench.exe: %s is not a whole number of %d-byte slots" file
      stride;
  let corpus = Filename.remove_extension (Filename.basename file) in
  List.iter
    (fun (name, f) ->
      Bench_lib.measure ~corpus ~filter ~name
        ~items:(Bytes.length buf / stride)
        ~runs ~pp:Fmt.int
        (fun () -> f (Sys.opaque_identity buf) land mask62))
    rows

let () =
  match Array.to_list Sys.argv with
  | [ _; "corpus"; dir; n ] -> corpora dir (int_of_string n)
  | _ :: "run" :: file :: stride :: runs :: rest ->
      let filter =
        Bench_lib.filter (match rest with [ f ] -> Some f | _ -> None)
      in
      run file (int_of_string stride) (int_of_string runs) filter;
      Bench_lib.finish filter
  | _ :: "report" :: files -> Bench_lib.report ~unit:(fun _ -> "packet") files
  | _ ->
      prerr_endline
        "usage: bench.exe (corpus DIR N | run FILE STRIDE RUNS [ROW | --list] \
         | report FILE...)";
      exit 2
