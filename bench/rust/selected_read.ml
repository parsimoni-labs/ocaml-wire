(* The checksum of the frames of a corpus through the fold of g_selected, in
   slots of [stride] bytes, which the rows pass as a literal so that it stays
   a constant once inlined: [f] adds each packet's contribution, the fields
   the checksum reads, as the Rust rows add theirs; a frame its checks refuse
   adds nothing. A field under a layer is [None] where the layer is absent:
   [ipv4.src] says whether the packet holds IPv4, [ipv4.tcp.src_port] whether
   it holds TCP. *)

let u8 = Wire.UInt8.to_int
let u16 = Wire.UInt16.to_int
let u32 = Wire.UInt32.to_int
let int f = function Some x -> f x | None -> 0

let[@inline always] contribution ether_type src dst ttl proto len tcp_sp tcp_dp
    seq ack win flags off udp_sp udp_dp udp_len =
  match src with
  | None -> u16 ether_type
  | Some src ->
      let ip0 = u32 src lxor int u32 dst in
      let ip1 = int u8 ttl + int u8 proto in
      let ip2 = u16 ether_type + int u16 len in
      let transport =
        match (tcp_sp, udp_sp) with
        | Some sp, _ ->
            let a = (u16 sp lsl 16) lor int u16 tcp_dp in
            let b = int u32 seq lxor int u32 ack in
            let c = int u16 win + int Fun.id flags in
            a + b + (c + int Fun.id off)
        | None, Some sp ->
            ((u16 sp lsl 16) lor int u16 udp_dp) + int u16 udp_len
        | None, None -> 0
      in
      ip0 + ip1 + (ip2 + transport)

let batch ~stride buf =
  G_selected.fold buf ~stride ~invalid:Fun.id ~init:0
    ~f:(fun
        acc
        ether_type
        src
        dst
        ttl
        proto
        len
        tcp_sp
        tcp_dp
        seq
        ack
        win
        flags
        off
        udp_sp
        udp_dp
        udp_len
      ->
      acc
      + contribution ether_type src dst ttl proto len tcp_sp tcp_dp seq ack win
          flags off udp_sp udp_dp udp_len)
  |> Result.fold ~ok:Fun.id ~error:snd
