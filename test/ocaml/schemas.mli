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
