(* Every module Wire_ocaml writes for the OxCaml target meets the signatures
   of [Wire_ocaml] with its buffers [local]: a function of a [local] buffer is
   no function of a global one, so the signatures, which hold for the OCaml
   target (test/ocaml/test_signature.ml), are copied here with each buffer
   argument as the OxCaml target takes it. A check and an accessor take their
   buffer [local], and so do the validator, decoder and encoder of a module
   that holds a record of constant size; those of a module that walks its
   record, the fold, the setters and a slice take it global. The ascriptions
   are checked when this file compiles, on an OxCaml compiler only. *)

module U8 = Wire.UInt8
module U16 = Wire.UInt16
module U32 = Wire.UInt32

module type RECORD = Wire_ocaml.RECORD
module type FOLD = Wire_ocaml.FOLD

(* The values of a module that holds a record of constant size. *)
module type S_LOCAL = sig
  include RECORD

  val min_size : int
  val validate : bytes @ local -> off:int -> (unit, Wire.Error.t) result
  val decode : bytes @ local -> off:int -> (t, Wire.Error.t) result
  val encode : t -> bytes @ local -> off:int -> unit
end

(* A module of a codec of constant size that holds its record. *)
module type FIXED = sig
  include S_LOCAL

  val check :
    'b Wire.Buf.t @ local -> 'b Wire.Buf.length -> off:int -> 'b check
end

(* A module of a codec of constant size that walks its record. *)
module type FIXED_WALK = sig
  include Wire_ocaml.S

  val check :
    'b Wire.Buf.t @ local -> 'b Wire.Buf.length -> off:int -> 'b check
end

(* A module of a codec of variable size. *)
module type VARIABLE = sig
  include Wire_ocaml.S

  val check :
    'b Wire.Buf.t @ local ->
    'b Wire.Buf.length ->
    off:int ->
    len:int ->
    'b check
end

(* Records of constant size. *)
module _ : FIXED = G_ipv4
module _ : FIXED = G_ipv4_record
module _ : FIXED = G_mixed_record
module _ : FIXED = G_checked
module _ : FIXED = G_mixed
module _ : FIXED = G_small
module _ : FIXED = G_eth
module _ : FIXED = G_wide
module _ : FIXED = G_mapped
module _ : FIXED = G_signed
module _ : FIXED = G_packed
module _ : FIXED = G_refined
module _ : FIXED = G_overflow
module _ : FIXED = G_lanes
module _ : FIXED = S_overflow
module _ : FIXED_WALK = S_huge

(* Records of variable size, and the records their fields hold. *)
module _ : VARIABLE = S_sized
module _ : VARIABLE = S_skipped
module _ : VARIABLE = S_nested
module _ : VARIABLE = S_capture
module _ : RECORD = S_capture.Captured_ipv4
module _ : RECORD = S_capture.Stack_ipv6
module _ : RECORD = S_capture.Stack_tcp
module _ : RECORD = S_capture.Stack_udp_header
module _ : VARIABLE = S_capture_step

(* Folds, whose buffer the fold takes global. *)
module _ :
  FOLD
    with type 'acc visitor :=
      'acc ->
      version:int ->
      ihl:int ->
      length:U16.t ->
      ttl:U8.t ->
      proto:U8.t ->
      'acc =
  G_checked

module _ : FOLD with type 'acc visitor := 'acc -> n:U32.t -> tail:U8.t -> 'acc =
  S_sized

module _ :
  FOLD
    with type 'acc visitor :=
      'acc -> a:U32.t -> hi:int -> lo:int -> b:U32.t -> c:U32.t -> 'acc =
  S_overflow

module _ :
  FOLD
    with type 'acc visitor :=
      'acc -> n:U32.t -> x:U8.t -> a:U8.t option -> b:U8.t option -> 'acc =
  S_skipped

module _ :
  FOLD with type 'acc visitor := 'acc -> flag:U8.t -> tail:U8.t -> 'acc =
  S_huge

module _ :
  FOLD
    with type 'acc visitor :=
      'acc ->
      ipv4_protocol:U8.t option ->
      ipv4_src:U32.t option ->
      ipv4_dst:U32.t option ->
      ipv4_tcp_dst_port:U16.t option ->
      ipv4_udp_dst_port:U16.t option ->
      ipv6_next_header:U8.t option ->
      ipv6_dst3:U32.t option ->
      'acc =
  S_capture_step
