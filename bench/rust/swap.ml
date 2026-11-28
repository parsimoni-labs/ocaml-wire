(* The header modules of the swap rows: the modules Wire_ocaml generates from
   the codecs of [Headers], or the interpreted form over the same codecs,
   given the generated modules' names as doc/api.mld shows: the same check,
   which leaves the length it takes unread, [is_ok], [get] and accessors,
   each field resolved once, when first read, so that the program starts
   where the interpreted views are not yet implemented. *)

module V = Wire.Codec.View

module Interpreted = struct
  module Eth = struct
    let type_ = lazy (V.field Headers.eth Headers.eth_type)
    let check b (_ : _ Wire.Buf.length) ~off = V.check Headers.eth b ~off
    let is_ok = V.is_ok
    let get = V.get
    let ether_type b v = V.read b v (Lazy.force type_)
  end

  module Ipv4 = struct
    let ihl_ = lazy (V.field Headers.ipv4 Headers.ipv4_ihl)
    let flags_ = lazy (V.field Headers.ipv4 Headers.ipv4_flags)

    let fragment_offset_ =
      lazy (V.field Headers.ipv4 Headers.ipv4_fragment_offset)

    let total_length_ = lazy (V.field Headers.ipv4 Headers.ipv4_total_length)
    let protocol_ = lazy (V.field Headers.ipv4 Headers.ipv4_protocol)
    let src_ = lazy (V.field Headers.ipv4 Headers.ipv4_src)
    let dst_ = lazy (V.field Headers.ipv4 Headers.ipv4_dst)
    let check b (_ : _ Wire.Buf.length) ~off = V.check Headers.ipv4 b ~off
    let is_ok = V.is_ok
    let get = V.get
    let ihl b v = V.read b v (Lazy.force ihl_)
    let flags b v = V.read b v (Lazy.force flags_)
    let fragment_offset b v = V.read b v (Lazy.force fragment_offset_)
    let total_length b v = V.read b v (Lazy.force total_length_)
    let protocol b v = V.read b v (Lazy.force protocol_)
    let src_addr b v = V.read b v (Lazy.force src_)
    let dst_addr b v = V.read b v (Lazy.force dst_)
  end

  module Tcp = struct
    let data_offset_ = lazy (V.field Headers.tcp Headers.tcp_data_offset)
    let src_port_ = lazy (V.field Headers.tcp Headers.tcp_src_port)
    let dst_port_ = lazy (V.field Headers.tcp Headers.tcp_dst_port)
    let check b (_ : _ Wire.Buf.length) ~off = V.check Headers.tcp b ~off
    let is_ok = V.is_ok
    let get = V.get
    let data_offset b v = V.read b v (Lazy.force data_offset_)
    let src_port b v = V.read b v (Lazy.force src_port_)
    let dst_port b v = V.read b v (Lazy.force dst_port_)
  end

  module Udp = struct
    let src_port_ = lazy (V.field Headers.udp Headers.udp_src_port)
    let dst_port_ = lazy (V.field Headers.udp Headers.udp_dst_port)
    let check b (_ : _ Wire.Buf.length) ~off = V.check Headers.udp b ~off
    let is_ok = V.is_ok
    let get = V.get
    let src_port b v = V.read b v (Lazy.force src_port_)
    let dst_port b v = V.read b v (Lazy.force dst_port_)
  end
end

module Generated = struct
  module Eth = G_eth
  module Ipv4 = G_ipv4
  module Tcp = G_tcp
  module Udp = G_udp
end
