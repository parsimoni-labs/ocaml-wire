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
    match Codec.decode pcap_record buf ~off:(offs.(i) - 16) with
    | Error _ -> ()
    | Ok p -> classify_value p
  done

(* wire.ocaml, linked headers: the record modules of the fixed headers, and
   the harness making the checks that link them, the bounds against the
   capture ending at [cap] included. *)
let view_ports b len ~cap o proto =
  if proto = 6 then
    let c = G_tcp.check b len o in
    if not (G_tcp.ok c) then -1
    else
      let t = G_tcp.get c in
      if o + (G_tcp.data_offset b t * 4) > cap then -1
      else
        (UInt16.to_int (G_tcp.src_port b t) lsl 16)
        lor UInt16.to_int (G_tcp.dst_port b t)
  else if proto = 17 then
    if o + 8 > cap then -1
    else
      let c = G_udp.check b len o in
      if not (G_udp.ok c) then -1
      else
        let u = G_udp.get c in
        (UInt16.to_int (G_udp.src_port b u) lsl 16)
        lor UInt16.to_int (G_udp.dst_port b u)
  else 0

let view_ipv4 b len ~cap o =
  if o + 20 <= cap then
    let c = G_ipv4.check b len o in
    if G_ipv4.ok c then
      let ip = G_ipv4.get c in
      let hlen = (G_ipv4.ihl b ip :> int) * 4
      and total = UInt16.to_int (G_ipv4.total_length b ip) in
      if total >= hlen && o + hlen <= cap then
        let proto = UInt8.to_int (G_ipv4.protocol b ip) in
        let ports =
          if G_ipv4.flags b ip land 1 <> 0 || G_ipv4.fragment_offset b ip <> 0
          then 0
          else view_ports b len ~cap (o + hlen) proto
        in
        if ports >= 0 then
          Flow_table.add4
            ~src:(UInt32.to_int (G_ipv4.src_addr b ip))
            ~dst:(UInt32.to_int (G_ipv4.dst_addr b ip))
            ~proto ~ports ~bytes:total

let view_ipv6 b len ~cap o =
  if o + 40 <= cap then
    let c = G_ipv6.check b len o in
    if G_ipv6.ok c then
      let ip = G_ipv6.get c in
      let proto = UInt8.to_int (G_ipv6.next_header b ip) in
      let ports = view_ports b len ~cap (o + 40) proto in
      if ports >= 0 then
        Flow_table.add6
          ~s0:(UInt32.to_int (G_ipv6.src0 b ip))
          ~s1:(UInt32.to_int (G_ipv6.src1 b ip))
          ~s2:(UInt32.to_int (G_ipv6.src2 b ip))
          ~s3:(UInt32.to_int (G_ipv6.src3 b ip))
          ~d0:(UInt32.to_int (G_ipv6.dst0 b ip))
          ~d1:(UInt32.to_int (G_ipv6.dst1 b ip))
          ~d2:(UInt32.to_int (G_ipv6.dst2 b ip))
          ~d3:(UInt32.to_int (G_ipv6.dst3 b ip))
          ~proto ~ports
          ~bytes:(40 + UInt16.to_int (G_ipv6.payload_length b ip))

let view_frame b len o caplen =
  let cap = o + caplen in
  if caplen >= 14 then
    let c = G_eth.check b len o in
    if G_eth.ok c then
      let ty = UInt16.to_int (G_eth.ether_type b (G_eth.get c)) in
      if ty = 0x0800 then view_ipv4 b len ~cap (o + 14)
      else if ty = 0x86dd then view_ipv6 b len ~cap (o + 14)

let flow_view buf (offs : int array) (lens : int array) =
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let len = Wire_runtime.Buf.length b in
      for i = 0 to Array.length offs - 1 do
        view_frame b len offs.(i) lens.(i)
      done

(* wire.ocaml check/view: the views the scanner of [Packet_stack.capture]
   writes. [check] makes every check of the frame once; each sub-record is
   then entered through the accessor of its field, which tests its presence
   and places it again, and each field read by its accessor. *)
module S = G_capture

let check_ipv4 b len ip =
  let ports =
    let t = S.Captured_ipv4.tcp b len ip in
    if S.Stack_tcp.ok t then
      let t = S.Stack_tcp.get t in
      ((S.Stack_tcp.src_port b t :> int) lsl 16)
      lor (S.Stack_tcp.dst_port b t :> int)
    else
      let u = S.Captured_ipv4.udp b len ip in
      if S.Stack_udp_header.ok u then
        let u = S.Stack_udp_header.get u in
        ((S.Stack_udp_header.src_port b u :> int) lsl 16)
        lor (S.Stack_udp_header.dst_port b u :> int)
      else 0
  in
  Flow_table.add4
    ~src:(UInt32.to_int (S.Captured_ipv4.src b ip))
    ~dst:(UInt32.to_int (S.Captured_ipv4.dst b ip))
    ~proto:(S.Captured_ipv4.protocol b ip :> int)
    ~ports
    ~bytes:(S.Captured_ipv4.total_length b ip :> int)

let check_ipv6 b len ip =
  let ports =
    let t = S.Stack_ipv6.tcp b len ip in
    if S.Stack_tcp.ok t then
      let t = S.Stack_tcp.get t in
      ((S.Stack_tcp.src_port b t :> int) lsl 16)
      lor (S.Stack_tcp.dst_port b t :> int)
    else
      let u = S.Stack_ipv6.udp b len ip in
      if S.Stack_udp_header.ok u then
        let u = S.Stack_udp_header.get u in
        ((S.Stack_udp_header.src_port b u :> int) lsl 16)
        lor (S.Stack_udp_header.dst_port b u :> int)
      else 0
  in
  Flow_table.add6
    ~s0:(UInt32.to_int (S.Stack_ipv6.src0 b ip))
    ~s1:(UInt32.to_int (S.Stack_ipv6.src1 b ip))
    ~s2:(UInt32.to_int (S.Stack_ipv6.src2 b ip))
    ~s3:(UInt32.to_int (S.Stack_ipv6.src3 b ip))
    ~d0:(UInt32.to_int (S.Stack_ipv6.dst0 b ip))
    ~d1:(UInt32.to_int (S.Stack_ipv6.dst1 b ip))
    ~d2:(UInt32.to_int (S.Stack_ipv6.dst2 b ip))
    ~d3:(UInt32.to_int (S.Stack_ipv6.dst3 b ip))
    ~proto:(S.Stack_ipv6.next_header b ip :> int)
    ~ports
    ~bytes:(40 + (S.Stack_ipv6.payload_length b ip :> int))

let check_frame b len o caplen =
  let c = S.check b len ~off:o ~len:caplen in
  if S.ok c then
    let v = S.get c in
    let k4 = S.ipv4 b len v in
    if S.Captured_ipv4.ok k4 then check_ipv4 b len (S.Captured_ipv4.get k4)
    else
      let k6 = S.ipv6 b len v in
      if S.Stack_ipv6.ok k6 then check_ipv6 b len (S.Stack_ipv6.get k6)

let flow_check buf (offs : int array) (lens : int array) =
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let len = Wire_runtime.Buf.length b in
      for i = 0 to Array.length offs - 1 do
        check_frame b len offs.(i) lens.(i)
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
