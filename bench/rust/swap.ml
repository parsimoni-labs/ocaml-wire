(* The header modules of the swap rows: the modules Wire_ocaml generates from
   the codecs of [Headers], or their interpreted twins over the same codecs,
   which have the same views. *)

module Interpreted = struct
  module Eth = G_eth_codec.Make (struct
    type r = Headers.eth

    let codec = Headers.eth
  end)

  module Ipv4 = G_ipv4_codec.Make (struct
    type r = Headers.ipv4

    let codec = Headers.ipv4
  end)

  module Tcp = G_tcp_codec.Make (struct
    type r = Headers.tcp

    let codec = Headers.tcp
  end)

  module Udp = G_udp_codec.Make (struct
    type r = Headers.udp

    let codec = Headers.udp
  end)
end

module Generated = struct
  module Eth = G_eth
  module Ipv4 = G_ipv4
  module Tcp = G_tcp
  module Udp = G_udp
end
