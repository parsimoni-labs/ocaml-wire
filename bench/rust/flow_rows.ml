(* The rows of the flow classifier that parse with Wire.Codec or with the
   generated record modules, and the row that takes the keys from an array:
   each hands the key of every packet to [Flow_table]. The dune rules of this
   directory copy this file as [Flow_rows_parse], with [Flow_table] standing
   for [Flow_sum], and as [Flow_rows_keys], with [Flow_table] standing for
   [Flow_keys], so that every compiler calls each sink directly: a functor
   over the sink would call it through a closure on OCaml without flambda.

   The rows parse with the checks of [Packet_stack.capture]; bench.ml
   describes them. *)

open Wire

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

(* The keys [keys] of {!Flow_keys}, handed to the sink without a parse. *)
let replay (keys : int array) =
  for i = 0 to (Array.length keys / Flow_keys.stride) - 1 do
    let k = i * Flow_keys.stride in
    if keys.(k) = 4 then
      Flow_table.add4
        ~src:keys.(k + 4)
        ~dst:keys.(k + 8)
        ~proto:keys.(k + 1)
        ~ports:keys.(k + 2)
        ~bytes:keys.(k + 3)
    else
      Flow_table.add6
        ~s0:keys.(k + 4)
        ~s1:keys.(k + 5)
        ~s2:keys.(k + 6)
        ~s3:keys.(k + 7)
        ~d0:keys.(k + 8)
        ~d1:keys.(k + 9)
        ~d2:keys.(k + 10)
        ~d3:keys.(k + 11)
        ~proto:keys.(k + 1)
        ~ports:keys.(k + 2)
        ~bytes:keys.(k + 3)
  done
