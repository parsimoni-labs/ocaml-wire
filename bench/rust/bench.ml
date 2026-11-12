(* Cross-language header parsing benchmark, OCaml side.

   bench.exe corpus FILE N    writes N packets of [slot] bytes to FILE
   bench.exe run FILE RUNS    times the OCaml implementations on FILE, RUNS
                              passes each, and prints the best and the
                              median time per packet
   bench.exe report FILE...   prints the RESULT lines of each FILE as a table

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
module H = Headers

let slot = 64
let ipv4 = 0x0800

(* {1 The corpus}

   Fixed slots of 64 bytes. Three packets in four are well formed: an
   Ethernet header, an IPv4 header of 20 bytes whose total length covers the
   rest of the slot, then a TCP header without options and 10 bytes of
   payload, or a UDP header and 22 bytes. The others are one of the 17
   variants below, each as likely as the next, so every check the rows make
   meets packets that pass it and packets that fail it, and the checksums
   agree only if every row makes every check. Addresses, ports, sequence
   numbers, flags and payload are random. *)

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

let ipv4_packet b ip rand variant =
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

let corpus file n =
  let rand = Random.State.make [| 42 |] in
  let b = Bytes.create (n * slot) in
  for i = 0 to n - 1 do
    let o = i * slot in
    for k = 0 to slot - 1 do
      Bytes.set_uint8 b (o + k) (Random.State.int rand 256)
    done;
    let variant =
      if Random.State.int rand 4 > 0 then Well_formed
      else variants.(Random.State.int rand (Array.length variants))
    in
    if variant = Not_ipv4 then Bytes.set_uint16_be b (o + 12) 0x88B5
    else begin
      Bytes.set_uint16_be b (o + 12) ipv4;
      ipv4_packet b (o + 14) rand variant
    end
  done;
  let oc = open_out_bin file in
  output_bytes oc b;
  close_out oc

let read_file file =
  let ic = open_in_bin file in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  Bytes.unsafe_of_string s

(* {1 The fold} *)

let[@inline] ip_fields acc ~src ~dst ~ttl ~proto ~len =
  acc + (src lxor dst) + (ttl + proto + len)

let[@inline] tcp_fields acc ~sp ~dp ~seq ~ack ~win ~flags ~off =
  acc + ((sp lsl 16) lor dp) + (seq lxor ack) + (win + flags + off)

let[@inline] udp_fields acc ~sp ~dp ~len = acc + ((sp lsl 16) lor dp) + len

(* {1 Wire.Codec: valid, then a proof-carrying read per field} *)

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

(* Each layer folds its fields into [acc] and hands the next one on; a
   header that fails its checks drops the packet, leaving [base], what [acc]
   was before it. [payload] is how many bytes the IPv4 total length leaves
   the transport header. *)
let codec_tcp base acc buf o ~payload =
  match Codec.valid H.tcp buf o with
  | exception Parse_error _ -> base
  | t when tcp_off t * 4 > payload -> base
  | t ->
      tcp_fields acc
        ~sp:(UInt16.to_int (tcp_sp t))
        ~dp:(UInt16.to_int (tcp_dp t))
        ~seq:(UInt32.to_int (tcp_seq t))
        ~ack:(UInt32.to_int (tcp_ack t))
        ~win:(UInt16.to_int (tcp_win t))
        ~flags:(tcp_flags t) ~off:(tcp_off t)

let codec_udp base acc buf o ~payload =
  match Codec.valid H.udp buf o with
  | exception Parse_error _ -> base
  | u when UInt16.to_int (udp_len u) > payload -> base
  | u ->
      udp_fields acc
        ~sp:(UInt16.to_int (udp_sp u))
        ~dp:(UInt16.to_int (udp_dp u))
        ~len:(UInt16.to_int (udp_len u))

(* [avail] is how many bytes of the packet follow the Ethernet header. *)
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

let codec_packet acc buf o =
  match Codec.valid H.eth buf o with
  | exception Parse_error _ -> acc
  | e ->
      let ty = UInt16.to_int (eth_type e) in
      if ty = ipv4 then codec_ipv4 acc (acc + ty) buf (o + 14) ~avail:(slot - 14)
      else acc + ty

let codec buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / slot) - 1 do
    acc := codec_packet !acc buf (i * slot)
  done;
  !acc

(* {1 wire.ocaml: the generated validate, then a getter per field} *)

let generated_tcp base acc buf o ~payload =
  match G_tcp.validate buf o with
  | exception Parse_error _ -> base
  | () when G_tcp.get_data_offset buf o * 4 > payload -> base
  | () ->
      tcp_fields acc
        ~sp:(UInt16.to_int (G_tcp.get_src_port buf o))
        ~dp:(UInt16.to_int (G_tcp.get_dst_port buf o))
        ~seq:(G_tcp.get_seq_int buf o) ~ack:(G_tcp.get_ack_int buf o)
        ~win:(UInt16.to_int (G_tcp.get_window buf o))
        ~flags:(G_tcp.get_flags buf o)
        ~off:(G_tcp.get_data_offset buf o)

let generated_udp base acc buf o ~payload =
  match G_udp.validate buf o with
  | exception Parse_error _ -> base
  | () when UInt16.to_int (G_udp.get_length buf o) > payload -> base
  | () ->
      udp_fields acc
        ~sp:(UInt16.to_int (G_udp.get_src_port buf o))
        ~dp:(UInt16.to_int (G_udp.get_dst_port buf o))
        ~len:(UInt16.to_int (G_udp.get_length buf o))

let generated_ipv4 base acc buf o ~avail =
  match G_ipv4.validate buf o with
  | exception Parse_error _ -> base
  | () -> (
      let hlen = G_ipv4.get_ihl buf o * 4
      and len = UInt16.to_int (G_ipv4.get_total_length buf o) in
      if len < hlen || len > avail then base
      else
        let proto = UInt8.to_int (G_ipv4.get_protocol buf o) in
        let acc =
          ip_fields acc
            ~src:(G_ipv4.get_src_addr_int buf o)
            ~dst:(G_ipv4.get_dst_addr_int buf o)
            ~ttl:(UInt8.to_int (G_ipv4.get_ttl buf o))
            ~proto ~len
        in
        if
          G_ipv4.get_flags buf o land 1 <> 0
          || G_ipv4.get_fragment_offset buf o <> 0
        then acc
        else
          let payload = len - hlen in
          match proto with
          | 6 -> generated_tcp base acc buf (o + hlen) ~payload
          | 17 -> generated_udp base acc buf (o + hlen) ~payload
          | _ -> acc)

let generated_packet acc buf o =
  match G_eth.validate buf o with
  | exception Parse_error _ -> acc
  | () ->
      let ty = UInt16.to_int (G_eth.get_ether_type buf o) in
      if ty = ipv4 then
        generated_ipv4 acc (acc + ty) buf (o + 14) ~avail:(slot - 14)
      else acc + ty

let generated buf =
  let acc = ref 0 in
  for i = 0 to (Bytes.length buf / slot) - 1 do
    acc := generated_packet !acc buf (i * slot)
  done;
  !acc

(* {1 Timing and the table} *)

let mask62 = (1 lsl 62) - 1
let oxcaml = String.contains Sys.ocaml_version '+'

let measure name buf runs f =
  let packets = Bytes.length buf / slot in
  let sum = f buf land mask62 in
  let times =
    Array.init runs (fun _ ->
        let t = Unix.gettimeofday () in
        ignore (Sys.opaque_identity (f (Sys.opaque_identity buf)));
        (Unix.gettimeofday () -. t) *. 1e9 /. float packets)
  in
  Array.sort compare times;
  let compiler =
    if oxcaml then "OxCaml " ^ Sys.ocaml_version
    else "OCaml " ^ Sys.ocaml_version
  in
  Fmt.pr "RESULT\t%s (%s)\t%.2f\t%.2f\t%d@." name compiler times.(0)
    times.(runs / 2)
    sum

let results files =
  List.concat_map
    (fun file ->
      In_channel.with_open_text file In_channel.input_lines
      |> List.filter_map (fun l ->
          match String.split_on_char '\t' l with
          | [ "RESULT"; name; best; median; sum ] ->
              Some (name, float_of_string best, float_of_string median, sum)
          | _ -> None))
    files

let report files =
  let rs = results files in
  let fastest =
    List.fold_left (fun m (_, best, _, _) -> Float.min m best) infinity rs
  in
  let sums = List.sort_uniq compare (List.map (fun (_, _, _, s) -> s) rs) in
  Fmt.pr "%-58s %9s %9s %9s %9s@." "Implementation" "best" "median" "Mpkt/s"
    "vs best";
  Fmt.pr "%-58s %9s %9s@." "" "ns/pkt" "ns/pkt";
  Fmt.pr "%s@." (String.make 98 '-');
  List.iter
    (fun (name, best, median, _) ->
      Fmt.pr "%-58s %9.2f %9.2f %9.1f %8.2fx@." name best median (1e3 /. best)
        (best /. fastest))
    (List.sort (fun (_, a, _, _) (_, b, _, _) -> compare a b) rs);
  match sums with
  | [ s ] ->
      Fmt.pr "@.Every implementation read the same fields (checksum %s).@." s
  | _ ->
      Fmt.epr "The implementations disagree on the fields they read:@.";
      List.iter (fun (n, _, _, s) -> Fmt.epr "  %s: %s@." n s) rs;
      exit 1

let () =
  match Array.to_list Sys.argv with
  | [ _; "corpus"; file; n ] -> corpus file (int_of_string n)
  | [ _; "run"; file; runs ] ->
      let buf = read_file file and runs = int_of_string runs in
      measure "Wire.Codec valid/read" buf runs codec;
      (* dune generates for the OxCaml target on an OxCaml compiler. *)
      let target = if oxcaml then ", OxCaml target" else "" in
      measure ("wire.ocaml validate/get" ^ target) buf runs generated
  | _ :: "report" :: files -> report files
  | _ ->
      prerr_endline
        "usage: bench.exe (corpus FILE N | run FILE RUNS | report FILE...)";
      exit 2
