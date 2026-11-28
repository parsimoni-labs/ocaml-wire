(* Every module Wire_ocaml writes for these tests and for examples/generated
   meets the signatures of [Wire_ocaml]: a module of a codec of constant size
   [FIXED], any other [VARIABLE], each submodule [RECORD], and a module with
   a fold [FOLD], its visitor the function of the fields it declares, each
   labelled by its path. The ascriptions are checked when this file
   compiles. *)

module U8 = Wire.UInt8
module U16 = Wire.UInt16
module U32 = Wire.UInt32

module type FOLD = Wire_ocaml.FOLD

(* Records of constant size. *)
module _ : Wire_ocaml.FIXED = G_ipv4
module _ : Wire_ocaml.FIXED = G_ipv4_record
module _ : Wire_ocaml.FIXED = G_mixed_record
module _ : Wire_ocaml.FIXED = G_checked
module _ : Wire_ocaml.FIXED = G_mixed
module _ : Wire_ocaml.FIXED = G_small
module _ : Wire_ocaml.FIXED = G_eth
module _ : Wire_ocaml.FIXED = G_wide
module _ : Wire_ocaml.FIXED = G_mapped
module _ : Wire_ocaml.FIXED = G_signed
module _ : Wire_ocaml.FIXED = G_packed
module _ : Wire_ocaml.FIXED = G_refined
module _ : Wire_ocaml.FIXED = G_overflow
module _ : Wire_ocaml.FIXED = G_lanes
module _ : Wire_ocaml.FIXED = S_overflow
module _ : Wire_ocaml.FIXED = S_huge
module _ : Wire_ocaml.FIXED = Clcw

(* Records of variable size, and the records their fields hold. *)
module _ : Wire_ocaml.VARIABLE = S_sized
module _ : Wire_ocaml.VARIABLE = S_skipped
module _ : Wire_ocaml.VARIABLE = S_nested
module _ : Wire_ocaml.VARIABLE = S_capture
module _ : Wire_ocaml.RECORD = S_capture.Captured_ipv4
module _ : Wire_ocaml.RECORD = S_capture.Stack_ipv6
module _ : Wire_ocaml.RECORD = S_capture.Stack_tcp
module _ : Wire_ocaml.RECORD = S_capture.Stack_udp_header
module _ : Wire_ocaml.VARIABLE = S_capture_step
module _ : Wire_ocaml.VARIABLE = Stack
module _ : Wire_ocaml.RECORD = Stack.Stack_ipv4
module _ : Wire_ocaml.RECORD = Stack.Stack_tcp
module _ : Wire_ocaml.RECORD = Stack.Stack_udp

(* Folds. *)
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

module _ :
  FOLD
    with type 'acc visitor :=
      'acc ->
      ipv4_protocol:U8.t option ->
      ipv4_tcp_dst_port:U16.t option ->
      'acc =
  Stack
