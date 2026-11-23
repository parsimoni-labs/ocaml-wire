module IPv4.Proof

open WireGen.Lib
module P = WireGen.OCaml

(* What each let of fold_record holds, in bytes of the window: one lemma
   per let, printed by the emitter from the offset, width and bits it chose
   for it (the bytes a shared word serves, a bitfield's shift and mask). Z3
   proves each on its own; record_is_flat then only compares tests. *)
#push-options "--z3rlimit 50 --fuel 1 --ifuel 1"
let let0 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  bw1 == be buf base 1)
  = WireGen.Tables.tables ()

let let1 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  bw2 == be buf (base + 1) 1)
  = WireGen.Tables.tables ()

let let2 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  bw3 == be buf (base + 6) 2)
  = WireGen.Tables.tables ()

let let3 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.lsr bw1 4 == P.bits bw1 4 4)
  = WireGen.Tables.tables ()

let let4 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land bw1 0xf == P.bits bw1 0 4)
  = WireGen.Tables.tables ()

let let5 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  ((P.lt bw1 69 || P.gt bw1 79) <==> not (P.bits bw1 0 4 >= 5 && P.bits bw1 4 4 = 4)))
  = WireGen.Tables.tables ()

let let6 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.lsr bw2 2 == P.bits bw2 2 6)
  = WireGen.Tables.tables ()

let let7 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land bw2 0x3 == P.bits bw2 0 2)
  = WireGen.Tables.tables ()

let let8 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w0 32)) 0xffff == be buf (base + 2) 2)
  = WireGen.Tables.tables ()

let let9 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w0 16)) 0xffff == be buf (base + 4) 2)
  = WireGen.Tables.tables ()

let let10 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.lsr bw3 13 == P.bits bw3 13 3)
  = WireGen.Tables.tables ()

let let11 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land bw3 0x1fff == P.bits bw3 0 13)
  = WireGen.Tables.tables ()

let let12 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.to_int (P.shift_right_logical w8 56) == be buf (base + 8) 1)
  = WireGen.Tables.tables ()

let let13 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w8 48)) 0xff == be buf (base + 9) 1)
  = WireGen.Tables.tables ()

let let14 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w8 32)) 0xffff == be buf (base + 10) 2)
  = WireGen.Tables.tables ()

let let15 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w8 0)) IPv4.Scan.mask32 == be buf (base + 12) 4)
  = WireGen.Tables.tables ()

let let16 (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  P.land (P.to_int (P.shift_right_logical w12 0)) IPv4.Scan.mask32 == be buf (base + 16) 4)
  = WireGen.Tables.tables ()

let lets (buf:P.buf) (base:P.int{0 <= base /\ base + 20 <= P.length buf})
  : Lemma (let w0 = P.i64be buf base in
  let w8 = P.i64be buf (P.add base 8) in
  let w12 = P.i64be buf (P.add base 12) in
  let bw1 = P.to_int (P.shift_right_logical w0 56) in
  let bw2 = P.land (P.to_int (P.shift_right_logical w0 48)) 0xff in
  let bw3 = P.land (P.to_int (P.shift_right_logical w0 0)) 0xffff in
  bw1 == be buf base 1 /\
  bw2 == be buf (base + 1) 1 /\
  bw3 == be buf (base + 6) 2 /\
  P.lsr bw1 4 == P.bits bw1 4 4 /\
  P.land bw1 0xf == P.bits bw1 0 4 /\
  ((P.lt bw1 69 || P.gt bw1 79) <==> not (P.bits bw1 0 4 >= 5 && P.bits bw1 4 4 = 4)) /\
  P.lsr bw2 2 == P.bits bw2 2 6 /\
  P.land bw2 0x3 == P.bits bw2 0 2 /\
  P.land (P.to_int (P.shift_right_logical w0 32)) 0xffff == be buf (base + 2) 2 /\
  P.land (P.to_int (P.shift_right_logical w0 16)) 0xffff == be buf (base + 4) 2 /\
  P.lsr bw3 13 == P.bits bw3 13 3 /\
  P.land bw3 0x1fff == P.bits bw3 0 13 /\
  P.to_int (P.shift_right_logical w8 56) == be buf (base + 8) 1 /\
  P.land (P.to_int (P.shift_right_logical w8 48)) 0xff == be buf (base + 9) 1 /\
  P.land (P.to_int (P.shift_right_logical w8 32)) 0xffff == be buf (base + 10) 2 /\
  P.land (P.to_int (P.shift_right_logical w8 0)) IPv4.Scan.mask32 == be buf (base + 12) 4 /\
  P.land (P.to_int (P.shift_right_logical w12 0)) IPv4.Scan.mask32 == be buf (base + 16) 4)
  = let0 buf base; let1 buf base; let2 buf base; let3 buf base; let4 buf base; let5 buf base; let6 buf base; let7 buf base; let8 buf base; let9 buf base; let10 buf base; let11 buf base; let12 buf base; let13 buf base; let14 buf base; let15 buf base; let16 buf base
#pop-options

#push-options "--z3rlimit 100 --fuel 1 --ifuel 1"
let record_is_flat (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{IPv4.Scan.window buf base stop /\ stop >= 0})
    (invalid:unit -> r) (valid:IPv4.Scan.valid_t r)
  : Lemma (IPv4.Scan.fold_record buf base stop invalid valid ==
           IPv4.Flat.flat invalid valid (Seq.slice buf base (base + stop)))
  = if stop < 20 then
      assert (IPv4.Scan.fold_record buf base stop invalid valid ==
              IPv4.Flat.flat invalid valid (Seq.slice buf base (base + stop)))
        by (WireGen.Tac.unfold_scan [`%IPv4.Scan.fold_record; `%IPv4.Scan.fold_src_addr;
              `%IPv4.Scan.mask32; `%IPv4.Flat.flat])
    else begin
      lets buf base;
      assert (IPv4.Scan.fold_record buf base stop invalid valid ==
              IPv4.Flat.flat invalid valid (Seq.slice buf base (base + stop)))
        by (WireGen.Tac.unfold_lets (`lets) [`%IPv4.Scan.fold_record; `%IPv4.Scan.fold_src_addr;
              `%IPv4.Flat.flat])
    end
#pop-options

let fold_is_3d #r buf off len invalid valid =
  if off < 0 || len < 0 || off + len > P.length buf then ()
  else begin
    record_is_flat buf off len invalid valid;
    IPv4.Flat.flat_is_3d invalid valid (Seq.slice buf off (off + len))
  end
