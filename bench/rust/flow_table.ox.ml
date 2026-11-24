(* The table of flow_table.std.ml on OxCaml: the same slots, hash, probe and
   checksum, with every word of a slot an [int64#] stored flat in the slot's
   record, and an address two words, as the Rust table's [[u32; 4]] lies in
   memory: [hi] holds its first 32-bit word in its low half and its second in
   its high half, [lo] the other two, so an IPv4 address is [hi] alone. *)

module I64 = Stdlib_upstream_compatible.Int64_u

type slot = {
  mutable epoch : I64.t;
  mutable meta : I64.t;
  mutable ports : I64.t;
  mutable src_hi : I64.t;
  mutable src_lo : I64.t;
  mutable dst_hi : I64.t;
  mutable dst_lo : I64.t;
  mutable packets : I64.t;
  mutable bytes : I64.t;
}

(* The 64-bit golden-ratio multiplier of the Rust and C rows. *)
let k = #0x9E37_79B9_7F4A_7C15L
let mix h w = I64.mul (I64.logxor h w) k
let zero = #0L
let one = #1L

type t = {
  mutable slots : slot array;
  mutable mask : int;
  mutable epoch : I64.t;
}

let state = { slots = [||]; mask = 0; epoch = zero }

let empty () =
  {
    epoch = zero;
    meta = zero;
    ports = zero;
    src_hi = zero;
    src_lo = zero;
    dst_hi = zero;
    dst_lo = zero;
    packets = zero;
    bytes = zero;
  }

let init ~capacity =
  let n = ref 1 in
  while !n < capacity do
    n := 2 * !n
  done;
  state.slots <- Array.init !n (fun _ -> empty ());
  state.mask <- !n - 1;
  state.epoch <- zero

let reset () =
  state.epoch <- I64.logand (I64.add state.epoch one) #0xffff_ffffL

let word w = I64.of_int w
let pair a b = I64.logor (word a) (I64.shift_left (word b) 32)
let first h mask = I64.to_int (I64.shift_right_logical h 32) land mask

let claim (s : slot) ~epoch ~meta ~ports ~bytes =
  s.epoch <- epoch;
  s.meta <- meta;
  s.ports <- ports;
  s.packets <- one;
  s.bytes <- bytes

let count (s : slot) ~bytes =
  s.packets <- I64.add s.packets one;
  s.bytes <- I64.add s.bytes bytes

let add4 ~src ~dst ~proto ~ports ~bytes =
  let meta = word (0x400 lor proto) and ports = word ports in
  let src = word src and dst = word dst and bytes = word bytes in
  let h = mix (mix (mix (mix zero src) dst) ports) meta in
  let slots = state.slots and mask = state.mask in
  let epoch = state.epoch in
  let mutable i = first h mask in
  let mutable probing = true in
  while probing do
    let s = slots.(i) in
    if not (I64.equal s.epoch epoch) then begin
      claim s ~epoch ~meta ~ports ~bytes;
      s.src_hi <- src;
      s.src_lo <- zero;
      s.dst_hi <- dst;
      s.dst_lo <- zero;
      probing <- false
    end
    else if
      I64.equal s.meta meta && I64.equal s.ports ports
      && I64.equal s.src_hi src && I64.equal s.dst_hi dst
    then begin
      count s ~bytes;
      probing <- false
    end
    else i <- (i + 1) land mask
  done

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  let meta = word (0x600 lor proto) and ports = word ports in
  let h = mix (mix (mix (mix zero (word s0)) (word s1)) (word s2)) (word s3) in
  let h = mix (mix (mix (mix h (word d0)) (word d1)) (word d2)) (word d3) in
  let h = mix (mix h ports) meta in
  let src_hi = pair s0 s1 and src_lo = pair s2 s3 in
  let dst_hi = pair d0 d1 and dst_lo = pair d2 d3 in
  let bytes = word bytes in
  let slots = state.slots and mask = state.mask in
  let epoch = state.epoch in
  let mutable i = first h mask in
  let mutable probing = true in
  while probing do
    let s = slots.(i) in
    if not (I64.equal s.epoch epoch) then begin
      claim s ~epoch ~meta ~ports ~bytes;
      s.src_hi <- src_hi;
      s.src_lo <- src_lo;
      s.dst_hi <- dst_hi;
      s.dst_lo <- dst_lo;
      probing <- false
    end
    else if
      I64.equal s.meta meta && I64.equal s.ports ports
      && I64.equal s.src_hi src_hi && I64.equal s.src_lo src_lo
      && I64.equal s.dst_hi dst_hi && I64.equal s.dst_lo dst_lo
    then begin
      count s ~bytes;
      probing <- false
    end
    else i <- (i + 1) land mask
  done

let mask62 = (1 lsl 62) - 1
let high w = I64.to_int (I64.shift_right_logical w 32)
let low w = I64.to_int w land 0xffff_ffff

(* [mix] on the low 63 bits, as flow_table.std.ml computes the checksum. *)
let int_mix h w = h lxor w * 0x1E37_79B9_7F4A_7C15

let summary () =
  let flows = ref 0 and sum = ref 0 in
  Array.iteri
    (fun i (s : slot) ->
      if I64.equal s.epoch state.epoch then begin
        incr flows;
        let h =
          List.fold_left int_mix 0
            [
              I64.to_int s.meta;
              I64.to_int s.ports;
              low s.src_hi;
              high s.src_hi;
              low s.src_lo;
              high s.src_lo;
              low s.dst_hi;
              high s.dst_hi;
              low s.dst_lo;
              high s.dst_lo;
            ]
        in
        let packets = I64.to_int s.packets and bytes = I64.to_int s.bytes in
        sum := !sum + ((h + (packets lsl 32) + bytes) * (i + 1))
      end)
    state.slots;
  (!flows, !sum land mask62)
