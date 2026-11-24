(** The consumer the scanner of [Packet_stack.capture] calls by name: it keeps
    each frame's outputs, shown, most recent first. *)

val fields : string list
(** [fields] is the outputs {!valid} takes, in declaration order. *)

type t = string list
(** The type for the frames seen, most recent first. *)

val invalid : t -> t
(** [invalid seen] is [seen] after a frame the scanner rejects. *)

val valid :
  t ->
  ipv4_present:bool ->
  ipv4_protocol:int64 ->
  ipv4_src:int64 ->
  ipv4_dst:int64 ->
  ipv4_tcp_present:bool ->
  ipv4_tcp_dst_port:int64 ->
  ipv4_udp_present:bool ->
  ipv4_udp_dst_port:int64 ->
  ipv6_present:bool ->
  ipv6_next_header:int64 ->
  ipv6_dst3:int64 ->
  t
(** [valid seen ~ipv4_present ~ipv4_protocol ~ipv4_src ~ipv4_dst
     ~ipv4_tcp_present ~ipv4_tcp_dst_port ~ipv4_udp_present ~ipv4_udp_dst_port
     ~ipv6_present ~ipv6_next_header ~ipv6_dst3] is [seen] after a frame with
    these outputs. *)

val pp : Format.formatter -> t -> unit
(** [pp ppf seen] prints [seen] on [ppf], one frame per line. *)
