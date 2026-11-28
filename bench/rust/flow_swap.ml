(* The flow counter of the swap rows, written once against the header modules
   [H], the generated modules or the interpreted form under their names, and
   the sink
   [Flow_table].
   The line below picks [H]. The dune rules of this directory make the
   interpreted rows from this file by replacing that line with
   [module H = Swap.Interpreted], and the rows without the table by making
   [Flow_sum] the sink: every copy calls its header modules and its sink
   directly, where a functor would call them through closures on OCaml
   without flambda.

   Per frame: Ethernet with 14 bytes captured; IPv4 with its header and
   options captured (IHL words) and a total length covering them, the ports
   of the TCP header (its options captured) or of the UDP header (8 bytes
   captured) unless the packet is a fragment; then the 5-tuple and the total
   length to the sink. IPv6, VLAN tags and every other frame are counted in
   [Flow_mix], as is an IPv4 packet failing a check. *)

module H = Swap.Generated
open Wire

let mix = Flow_mix.state

let ports b len ~cap o proto =
  if proto = 6 then
    let c = H.Tcp.check b len ~off:o in
    if not (H.Tcp.is_ok c) then -1
    else
      let t = H.Tcp.get c in
      let off = H.Tcp.data_offset b t in
      if o + (off * 4) > cap then -1
      else
        (UInt16.to_int (H.Tcp.src_port b t) lsl 16)
        lor UInt16.to_int (H.Tcp.dst_port b t)
  else if proto = 17 then
    if o + 8 > cap then -1
    else
      let c = H.Udp.check b len ~off:o in
      if not (H.Udp.is_ok c) then -1
      else
        let u = H.Udp.get c in
        (UInt16.to_int (H.Udp.src_port b u) lsl 16)
        lor UInt16.to_int (H.Udp.dst_port b u)
  else 0

let reject () = mix.rejected <- mix.rejected + 1

let ipv4 b len ~cap o =
  if o + 20 > cap then reject ()
  else
    let c = H.Ipv4.check b len ~off:o in
    if not (H.Ipv4.is_ok c) then reject ()
    else
      let ip = H.Ipv4.get c in
      let hlen = H.Ipv4.ihl b ip * 4
      and total = UInt16.to_int (H.Ipv4.total_length b ip) in
      if total < hlen || o + hlen > cap then reject ()
      else
        let proto = UInt8.to_int (H.Ipv4.protocol b ip) in
        let ports =
          if H.Ipv4.flags b ip land 1 <> 0 || H.Ipv4.fragment_offset b ip <> 0
          then 0
          else ports b len ~cap (o + hlen) proto
        in
        if ports < 0 then reject ()
        else
          Flow_table.add4
            ~src:(UInt32.to_int (H.Ipv4.src_addr b ip))
            ~dst:(UInt32.to_int (H.Ipv4.dst_addr b ip))
            ~proto ~ports ~bytes:total

let frame b len o caplen =
  if caplen < 14 then mix.other <- mix.other + 1
  else
    let c = H.Eth.check b len ~off:o in
    if not (H.Eth.is_ok c) then mix.other <- mix.other + 1
    else
      match UInt16.to_int (H.Eth.ether_type b (H.Eth.get c)) with
      | 0x0800 -> ipv4 b len ~cap:(o + caplen) (o + 14)
      | 0x86dd -> mix.ipv6 <- mix.ipv6 + 1
      | 0x8100 | 0x88a8 | 0x9100 -> mix.vlan <- mix.vlan + 1
      | _ -> mix.other <- mix.other + 1

let run buf (offs : int array) (lens : int array) =
  match Wire_runtime.Buf.v buf with
  | Wire_runtime.Buf.Any b ->
      let len = Wire_runtime.Buf.length b in
      for i = 0 to Array.length offs - 1 do
        frame b len offs.(i) lens.(i)
      done
