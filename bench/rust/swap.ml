(* The header modules of the swap rows, each a [Wire.S]: the interpreted
   codecs of [Headers] through [Wire.Codec.Make], or the [Global] modules
   Wire_ocaml generates from the same codecs. [Codec.Make] builds a field
   handle from a codec field with [field], where a generated module names its
   handles in [Field], so the interpreted side names the handles the loop
   reads as the generated side does. *)

open Wire

module Interpreted = struct
  module Eth = struct
    module M = Codec.Make (struct
      type r = Headers.eth

      let codec = Headers.eth
    end)

    include (
      M :
        Wire.S
          with type t = M.t
           and module View = M.View
           and module Field = M.Field)

    module Field = struct
      include M.Field

      let ether_type = M.field Headers.eth_type
    end
  end

  module Ipv4 = struct
    module M = Codec.Make (struct
      type r = Headers.ipv4

      let codec = Headers.ipv4
    end)

    include (
      M :
        Wire.S
          with type t = M.t
           and module View = M.View
           and module Field = M.Field)

    module Field = struct
      include M.Field

      let ihl = M.field Headers.ipv4_ihl
      let total_length = M.field Headers.ipv4_total_length
      let flags = M.field Headers.ipv4_flags
      let fragment_offset = M.field Headers.ipv4_fragment_offset
      let protocol = M.field Headers.ipv4_protocol
      let src_addr = M.field Headers.ipv4_src
      let dst_addr = M.field Headers.ipv4_dst
    end
  end

  module Tcp = struct
    module M = Codec.Make (struct
      type r = Headers.tcp

      let codec = Headers.tcp
    end)

    include (
      M :
        Wire.S
          with type t = M.t
           and module View = M.View
           and module Field = M.Field)

    module Field = struct
      include M.Field

      let src_port = M.field Headers.tcp_src_port
      let dst_port = M.field Headers.tcp_dst_port
      let data_offset = M.field Headers.tcp_data_offset
    end
  end

  module Udp = struct
    module M = Codec.Make (struct
      type r = Headers.udp

      let codec = Headers.udp
    end)

    include (
      M :
        Wire.S
          with type t = M.t
           and module View = M.View
           and module Field = M.Field)

    module Field = struct
      include M.Field

      let src_port = M.field Headers.udp_src_port
      let dst_port = M.field Headers.udp_dst_port
    end
  end
end

module Generated = struct
  module Eth = G_eth.Global
  module Ipv4 = G_ipv4.Global
  module Tcp = G_tcp.Global
  module Udp = G_udp.Global
end
