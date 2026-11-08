(** Codecs the generator tests run against, each with its constructor. *)

open Wire

type ipv4 =
  UInt8.t
  * UInt8.t
  * UInt16.t
  * UInt16.t
  * UInt16.t
  * UInt8.t
  * UInt8.t
  * UInt16.t
  * SInt32.t
  * UInt32.t

val ipv4_make :
  UInt8.t ->
  UInt8.t ->
  UInt16.t ->
  UInt16.t ->
  UInt16.t ->
  UInt8.t ->
  UInt8.t ->
  UInt16.t ->
  SInt32.t ->
  UInt32.t ->
  ipv4
(** [ipv4_make] builds an IPv4 header from its ten fields. *)

val ipv4 : ipv4 Codec.t
(** [ipv4] is a plain IPv4 header. *)

type checked = int * int * UInt16.t * UInt8.t * UInt8.t

val checked_make : int -> int -> UInt16.t -> UInt8.t -> UInt8.t -> checked
(** [checked_make] builds a {!checked} record. *)

val checked : checked Codec.t
(** [checked] has bitfields, field constraints, a closed enum and a [where]
    clause. *)

type mixed =
  UInt16.t * SInt16.t * SInt8.t * SInt16.t * UInt32.t * SInt32.t * int * int

val mixed_make :
  UInt16.t ->
  SInt16.t ->
  SInt8.t ->
  SInt16.t ->
  UInt32.t ->
  SInt32.t ->
  int ->
  int ->
  mixed
(** [mixed_make] builds a {!mixed} record. *)

val mixed : mixed Codec.t
(** [mixed] has little-endian and signed integers and 16-bit bitfields. *)

val small_make : UInt8.t -> UInt16.t -> UInt8.t * UInt16.t
(** [small_make] builds a {!small} record. *)

val small : (UInt8.t * UInt16.t) Codec.t
(** [small] is narrower than one 64-bit word. *)

val eth_make : string -> string -> UInt16.t -> string * string * UInt16.t
(** [eth_make] builds an Ethernet header. *)

val eth : (string * string * UInt16.t) Codec.t
(** [eth] has fixed byte arrays. *)

val variable_make : UInt8.t -> string -> UInt8.t * string
(** [variable_make] builds a {!variable} record. *)

val variable : (UInt8.t * string) Codec.t
(** [variable] has a byte array sized by a field, which the generator does not
    specialise. *)

val ipv4_frag : (UInt16.t, ipv4) Codec.field
(** [ipv4_frag] is a field of {!val-ipv4}. *)

val ipv4_proto : (UInt8.t, ipv4) Codec.field
(** [ipv4_proto] is a field of {!val-ipv4}. *)

val ipv4_src : (SInt32.t, ipv4) Codec.field
(** [ipv4_src] is a field of {!val-ipv4}. *)

val ipv4_dst : (UInt32.t, ipv4) Codec.field
(** [ipv4_dst] is a field of {!val-ipv4}. *)

val mixed_b : (SInt16.t, mixed) Codec.field
(** [mixed_b] is a field of {!val-mixed}. *)

val mixed_e : (UInt32.t, mixed) Codec.field
(** [mixed_e] is a field of {!val-mixed}. *)

val mixed_f : (SInt32.t, mixed) Codec.field
(** [mixed_f] is a field of {!val-mixed}. *)

val mixed_h : (int, mixed) Codec.field
(** [mixed_h] is a field of {!val-mixed}. *)

val eth_src : (string, string * string * UInt16.t) Codec.field
(** [eth_src] is a field of {!val-eth}. *)

val checked_proto : (UInt8.t, checked) Codec.field
(** [checked_proto] is the closed enum of {!val-checked}. *)

type wide = int * int * int * int * int * int

val wide_make : int -> int -> int -> int -> int -> int -> wide
(** [wide_make] builds a {!wide} record. *)

val wide : wide Codec.t
(** [wide] has 32-bit bitfield words in both byte orders. *)

val u16be_int : int typ
(** [u16be_int] is a big-endian 16-bit integer viewed as an [int]. *)

val i32be_int : int32 typ
(** [i32be_int] is a big-endian 32-bit integer viewed as an [int32]. *)

val proto_lookup : [ `Icmp | `Tcp | `Udp ] typ
(** [proto_lookup] is a byte read as an index into three cases. *)

val flag_bit : bool typ
(** [flag_bit] is a one-bit field viewed as a [bool]. *)

type mapped = int * int32 * [ `Icmp | `Tcp | `Udp ] * bool * int

val mapped_make :
  int -> int32 -> [ `Icmp | `Tcp | `Udp ] -> bool -> int -> mapped
(** [mapped_make] builds a {!mapped} record. *)

val mapped_len : (int, mapped) Codec.field
(** [mapped_len] is the mapped 16-bit field of {!val-mapped}. *)

val mapped : mapped Codec.t
(** [mapped] has every field under a map: over 16- and 32-bit integers, a lookup
    with an index bound, and a bitfield. *)
