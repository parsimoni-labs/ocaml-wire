(* The slots and the hash are cargo/src/flow.rs's and everparse/stack_c.c's,
   so every row fills the same slots and gives the same checksum. A slot holds
   the key, the packets and the bytes of one flow: [meta] is
   [(version lsl 8) lor protocol], [ports] [(src lsl 16) lor dst] or 0, and an
   IPv4 address is in [s0]/[d0], the other words 0. A slot is live when its
   epoch is the table's: [reset] starts a pass by moving the epoch on, so no
   pass clears the table. *)

type slot = {
  mutable epoch : int;
  mutable meta : int;
  mutable ports : int;
  mutable s0 : int;
  mutable s1 : int;
  mutable s2 : int;
  mutable s3 : int;
  mutable d0 : int;
  mutable d1 : int;
  mutable d2 : int;
  mutable d3 : int;
  mutable packets : int;
  mutable bytes : int;
}

(* The low 63 bits of the 64-bit golden-ratio multiplier. A product's low bits
   depend only on its operands' low bits, so this hash and the 64-bit one of
   the Rust and C rows agree on their low 63 bits. *)
let k = 0x1E37_79B9_7F4A_7C15
let mix h w = h lxor w * k

type t = { mutable slots : slot array; mutable mask : int; mutable epoch : int }

let state = { slots = [||]; mask = 0; epoch = 0 }

let empty () =
  {
    epoch = 0;
    meta = 0;
    ports = 0;
    s0 = 0;
    s1 = 0;
    s2 = 0;
    s3 = 0;
    d0 = 0;
    d1 = 0;
    d2 = 0;
    d3 = 0;
    packets = 0;
    bytes = 0;
  }

let init ~capacity =
  let n = ref 1 in
  while !n < capacity do
    n := 2 * !n
  done;
  state.slots <- Array.init !n (fun _ -> empty ());
  state.mask <- !n - 1;
  state.epoch <- 0

let reset () = state.epoch <- (state.epoch + 1) land 0xffff_ffff

let claim (s : slot) ~epoch ~meta ~ports ~bytes =
  s.epoch <- epoch;
  s.meta <- meta;
  s.ports <- ports;
  s.packets <- 1;
  s.bytes <- bytes

let add4 ~src ~dst ~proto ~ports ~bytes =
  let meta = 0x400 lor proto in
  let h = mix (mix (mix (mix 0 src) dst) ports) meta in
  let slots = state.slots and mask = state.mask and epoch = state.epoch in
  let rec probe i =
    let s : slot = slots.(i) in
    if s.epoch <> epoch then begin
      claim s ~epoch ~meta ~ports ~bytes;
      s.s0 <- src;
      s.s1 <- 0;
      s.s2 <- 0;
      s.s3 <- 0;
      s.d0 <- dst;
      s.d1 <- 0;
      s.d2 <- 0;
      s.d3 <- 0
    end
    else if s.meta = meta && s.ports = ports && s.s0 = src && s.d0 = dst then begin
      s.packets <- s.packets + 1;
      s.bytes <- s.bytes + bytes
    end
    else probe ((i + 1) land mask)
  in
  probe ((h lsr 32) land mask)

let add6 ~s0 ~s1 ~s2 ~s3 ~d0 ~d1 ~d2 ~d3 ~proto ~ports ~bytes =
  let meta = 0x600 lor proto in
  let h = mix (mix (mix (mix 0 s0) s1) s2) s3 in
  let h = mix (mix (mix (mix h d0) d1) d2) d3 in
  let h = mix (mix h ports) meta in
  let slots = state.slots and mask = state.mask and epoch = state.epoch in
  let rec probe i =
    let s : slot = slots.(i) in
    if s.epoch <> epoch then begin
      claim s ~epoch ~meta ~ports ~bytes;
      s.s0 <- s0;
      s.s1 <- s1;
      s.s2 <- s2;
      s.s3 <- s3;
      s.d0 <- d0;
      s.d1 <- d1;
      s.d2 <- d2;
      s.d3 <- d3
    end
    else if
      s.meta = meta && s.ports = ports && s.s0 = s0 && s.s1 = s1 && s.s2 = s2
      && s.s3 = s3 && s.d0 = d0 && s.d1 = d1 && s.d2 = d2 && s.d3 = d3
    then begin
      s.packets <- s.packets + 1;
      s.bytes <- s.bytes + bytes
    end
    else probe ((i + 1) land mask)
  in
  probe ((h lsr 32) land mask)

let mask62 = (1 lsl 62) - 1

let summary () =
  let flows = ref 0 and sum = ref 0 in
  Array.iteri
    (fun i (s : slot) ->
      if s.epoch = state.epoch then begin
        incr flows;
        let h =
          List.fold_left mix 0
            [ s.meta; s.ports; s.s0; s.s1; s.s2; s.s3; s.d0; s.d1; s.d2; s.d3 ]
        in
        sum := !sum + ((h + (s.packets lsl 32) + s.bytes) * (i + 1))
      end)
    state.slots;
  (!flows, !sum land mask62)
