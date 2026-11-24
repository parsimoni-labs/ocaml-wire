(* The layout and the hash are cargo/src/flow.rs's and everparse/stack_c.c's,
   so every row fills the same slots and gives the same checksum. A slot is 64
   bytes, little-endian:

   0 epoch u32 | 4 meta u32 | 8 ports u32 | 12 src u32 x 4 | 28 dst u32 x 4 |
   44 packets u32 | 48 bytes u64 | 56 unused

   [meta] is [(version lsl 8) lor protocol], [ports] [(src lsl 16) lor dst]
   or 0. An IPv4 address is in [src0]/[dst0], the other words 0. A slot is
   live when its epoch is the table's: [reset] starts a pass by moving the
   epoch on, so no pass clears the table. *)

let slot = 64
let u32 b o = Int32.to_int (Bytes.get_int32_le b o) land 0xffff_ffff
let set_u32 b o v = Bytes.set_int32_le b o (Int32.of_int v)
let get64 = Bytes.get_int64_le
let set64 = Bytes.set_int64_le

(* The low 63 bits of the 64-bit golden-ratio multiplier. A product's low bits
   depend only on its operands' low bits, so this hash and the 64-bit one of
   the Rust and C rows agree on their low 63 bits. *)
let k = 0x1E37_79B9_7F4A_7C15
let mix h w = h lxor w * k

type t = { mutable table : bytes; mutable mask : int; mutable epoch : int }

let state = { table = Bytes.empty; mask = 0; epoch = 0 }

let init ~capacity =
  let n = ref 1 in
  while !n < capacity do
    n := 2 * !n
  done;
  state.table <- Bytes.make (!n * slot) '\000';
  state.mask <- !n - 1;
  state.epoch <- 0

let reset () = state.epoch <- (state.epoch + 1) land 0xffff_ffff

let count b o bytes =
  set_u32 b (o + 44) (u32 b (o + 44) + 1);
  set64 b (o + 48) (Int64.add (get64 b (o + 48)) (Int64.of_int bytes))

let claim b o ~epoch ~meta ~ports ~bytes =
  set_u32 b o epoch;
  set_u32 b (o + 4) meta;
  set_u32 b (o + 8) ports;
  set_u32 b (o + 44) 1;
  set64 b (o + 48) (Int64.of_int bytes)

let add4 ~src ~dst ~proto ~ports ~bytes =
  let meta = 0x400 lor proto in
  let h = mix (mix (mix (mix 0 src) dst) ports) meta in
  let b = state.table and mask = state.mask and epoch = state.epoch in
  let rec probe i =
    let o = i * slot in
    if u32 b o <> epoch then begin
      claim b o ~epoch ~meta ~ports ~bytes;
      set_u32 b (o + 12) src;
      set_u32 b (o + 16) 0;
      set_u32 b (o + 20) 0;
      set_u32 b (o + 24) 0;
      set_u32 b (o + 28) dst;
      set_u32 b (o + 32) 0;
      set_u32 b (o + 36) 0;
      set_u32 b (o + 40) 0
    end
    else if
      u32 b (o + 4) = meta
      && u32 b (o + 8) = ports
      && u32 b (o + 12) = src
      && u32 b (o + 28) = dst
    then count b o bytes
    else probe ((i + 1) land mask)
  in
  probe ((h lsr 32) land mask)

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  let meta = 0x600 lor proto in
  let h = mix (mix (mix (mix 0 s0) s1) s2) s3 in
  let h = mix (mix (mix (mix h d0) d1) d2) d3 in
  let h = mix (mix h ports) meta in
  let b = state.table and mask = state.mask and epoch = state.epoch in
  let rec probe i =
    let o = i * slot in
    if u32 b o <> epoch then begin
      claim b o ~epoch ~meta ~ports ~bytes;
      set_u32 b (o + 12) s0;
      set_u32 b (o + 16) s1;
      set_u32 b (o + 20) s2;
      set_u32 b (o + 24) s3;
      set_u32 b (o + 28) d0;
      set_u32 b (o + 32) d1;
      set_u32 b (o + 36) d2;
      set_u32 b (o + 40) d3
    end
    else if
      u32 b (o + 4) = meta
      && u32 b (o + 8) = ports
      && u32 b (o + 12) = s0
      && u32 b (o + 16) = s1
      && u32 b (o + 20) = s2
      && u32 b (o + 24) = s3
      && u32 b (o + 28) = d0
      && u32 b (o + 32) = d1
      && u32 b (o + 36) = d2
      && u32 b (o + 40) = d3
    then count b o bytes
    else probe ((i + 1) land mask)
  in
  probe ((h lsr 32) land mask)

let mask62 = (1 lsl 62) - 1

let summary () =
  let b = state.table in
  let flows = ref 0 and sum = ref 0 in
  for i = 0 to state.mask do
    let o = i * slot in
    if u32 b o = state.epoch then begin
      incr flows;
      let h = ref 0 in
      for w = 0 to 9 do
        h := mix !h (u32 b (o + 4 + (4 * w)))
      done;
      let packets = u32 b (o + 44)
      and bytes = Int64.to_int (get64 b (o + 48)) in
      sum := !sum + ((!h + (packets lsl 32) + bytes) * (i + 1))
    end
  done;
  (!flows, !sum land mask62)
