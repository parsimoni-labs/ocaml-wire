(** The packets of the swap rows that make no IPv4 flow, counted by kind, as
    [Mix] of cargo/src/flow.rs counts them. *)

type t = {
  mutable ipv6 : int;  (** EtherType [0x86dd]. *)
  mutable vlan : int;  (** EtherType [0x8100], [0x88a8] or [0x9100]. *)
  mutable other : int;  (** Under 14 bytes captured, or any other EtherType. *)
  mutable rejected : int;  (** IPv4 failing a check of its headers. *)
}

val state : t
(** [state] holds the counts of the current pass. *)

val reset : unit -> unit
(** [reset ()] zeroes the counts, before a pass. *)

val pp : t Fmt.t
(** [pp] prints the counts as cargo/src/flow.rs prints its own. *)
