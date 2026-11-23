module IPv4.Scan

(* mask32, fold_src_addr, fold_record and fold of the scanner Wire_ocaml
   writes for Headers.ipv4 (ipv4_scan.ml), line for line. *)

module P = WireGen.OCaml

let mask32 = P.to_int 0xFFFFFFFF

let window (buf:P.buf) (base:P.int) (stop:P.int) : prop =
  0 <= base /\ stop <= P.length buf - base

type valid_t (r:Type) =
  P.int -> P.int -> P.int -> P.int -> P.int -> P.int -> P.int -> P.int -> P.int -> P.int ->
  P.int -> P.int -> P.int -> r

unfold
let fold_src_addr (#r:Type) (valid:valid_t r) (w8 w12:P.int64)
    (f1_version f2_ihl f3_dscp f4_ecn f5_total_length f6_identification f7_flags
     f8_fragment_offset f9_ttl f10_protocol f11_header_checksum:P.int) : r =
  let f12_src_addr = P.land (P.to_int (P.shift_right_logical w8 0)) mask32 in
  let f13_dst_addr = P.land (P.to_int (P.shift_right_logical w12 0)) mask32 in
  valid f1_version f2_ihl f3_dscp f4_ecn f5_total_length f6_identification f7_flags
    f8_fragment_offset f9_ttl f10_protocol f11_header_checksum f12_src_addr f13_dst_addr

unfold
let fold_record (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{window buf base stop})
    (invalid:unit -> r) (valid:valid_t r) : r =
  if P.lt stop 20 then invalid () else
  let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  let f1_version = P.lsr bw1 4 in
  let f2_ihl = P.land bw1 0xf in
  if P.lt bw1 69 || P.gt bw1 79 then invalid () else
  let f3_dscp = P.lsr bw2 2 in
  let f4_ecn = P.land bw2 0x3 in
  let f5_total_length = P.land (P.to_int (P.shift_right_logical w0 32)) 0xffff in
  if not (P.ge f5_total_length 20) then invalid () else
  let f6_identification = P.land (P.to_int (P.shift_right_logical w0 16)) 0xffff in
  let f7_flags = P.lsr bw3 13 in
  let f8_fragment_offset = P.land bw3 0x1fff in
  let f9_ttl = P.to_int (P.shift_right_logical w8 56) in
  let f10_protocol = P.land (P.to_int (P.shift_right_logical w8 48)) 0xff in
  let f11_header_checksum = P.land (P.to_int (P.shift_right_logical w8 32)) 0xffff in
  fold_src_addr valid w8 w12 f1_version f2_ihl f3_dscp f4_ecn f5_total_length f6_identification
    f7_flags f8_fragment_offset f9_ttl f10_protocol f11_header_checksum

let fold (#r:Type) (buf:P.buf) (off:P.int) (len:P.int)
    (invalid:unit -> r) (valid:valid_t r) : r =
  if P.lt off 0 || P.gt len (P.sub (P.length buf) off) then invalid () else
  let base = off in
  let stop = len in
  fold_record buf base stop invalid valid
