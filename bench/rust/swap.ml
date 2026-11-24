(* The header modules of the swap rows, each a [Wire.S] with the same named
   handles: the interpreted codecs of [Headers] through [Wire.Codec.Make], or
   the [Global] modules Wire_ocaml generates from the same codecs, both from
   the bridges Wire_ocaml writes next to the generated modules. *)

module Interpreted = struct
  module Eth = G_eth_wire.Interpreted
  module Ipv4 = G_ipv4_wire.Interpreted
  module Tcp = G_tcp_wire.Interpreted
  module Udp = G_udp_wire.Interpreted
end

module Generated = struct
  module Eth = G_eth_wire.Global
  module Ipv4 = G_ipv4_wire.Global
  module Tcp = G_tcp_wire.Global
  module Udp = G_udp_wire.Global
end
