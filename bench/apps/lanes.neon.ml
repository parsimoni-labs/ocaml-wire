(* The CLCW logic of [Loops] over the NEON lanes of the generated CLCW: four
   words per register, the same answers. OxCaml syntax over arm64 builtins:
   only an OxCaml compiler for arm64 builds this file. *)

module C = G_clcw
module V = Wire_runtime.Vec
module Buf = Wire_runtime.Buf

(* Polling: an anomaly for each word with lockout, wait or retransmit set, or a
   report value other than one past the previous word's. Each lane compares
   its report with the previous lane's ([V.before]); the tail is one word at a
   time. *)
let clcw buf =
  let (Buf.Any b) = Buf.v buf in
  let len = Buf.length b in
  let words = Bytes.length buf / C.size in
  let one = V.v 1 and ff = V.v 0xFF in
  (* The report before word 0 is 255, so word 0 expects 0. *)
  let prev = ref (V.v 0xFF) and good = ref (V.v 0) in
  let i = ref 0 and stopped = ref false in
  while (not !stopped) && !i + 4 <= words do
    C.X4.with_ b len (!i * C.size)
      ~ok:(fun w ->
        let r = C.X4.reportvalue w in
        let flags =
          V.logor (C.X4.lockout w) (V.logor (C.X4.wait w) (C.X4.retransmit w))
        in
        let expected = V.logand (V.add (V.before !prev r) one) ff in
        good := V.sub !good (V.logand (V.is_zero flags) (V.equal r expected));
        prev := r;
        i := !i + 4)
      ~fail:(fun () -> stopped := true)
  done;
  let anomalies = ref (!i - V.sum !good) in
  let expected = ref ((V.get !prev 3 + 1) land 0xFF) in
  while (not !stopped) && !i < words do
    C.View.with_ b len (!i * C.size)
      ~ok:(fun v ->
        let w = C.view_lockout_word b v in
        let report = C.reportvalue_of_word w in
        if
          C.lockout_of_word w <> 0
          || C.wait_of_word w <> 0
          || C.retransmit_of_word w <> 0
          || report <> !expected
        then incr anomalies;
        expected := (report + 1) land 0xFF;
        incr i)
      ~fail:(fun () -> stopped := true)
  done;
  !anomalies

(* The CLCW a FARM reports with frame [i], four frames per register: COP-1 in
   effect on virtual channel 7, report value [i mod 256], lockout when the 12
   low bits of [i] are all set, wait when its 6 low bits are, retransmit from
   bit 13 of [i]; every other field 0. *)
let report n out =
  let (Buf.Any o) = Buf.v out in
  let olen = Buf.length o in
  (* The fields every frame shares, placed once; each word adds the four that
     change, each moved to its place from its low bits. [fill] checks the span
     of all the registers once. *)
  let fixed = C.X4.(copineffect_of (V.v 1) ++ vcid_of (V.v 7)) in
  let one = V.v 1 and fff = V.v 0xFFF and x3f = V.v 0x3F and four = V.v 4 in
  let idx = ref (V.v4 0 1 2 3) in
  C.X4.fill o olen 0 ~count:(n / 4) ~f:(fun _ ->
      let x = !idx in
      idx := V.add x four;
      C.X4.(
        fixed
        ++ lockout_bits (V.shift_right_logical 12 (V.add (V.logand x fff) one))
        ++ wait_bits (V.shift_right_logical 6 (V.add (V.logand x x3f) one))
        ++ retransmit_bits (V.shift_right_logical 13 x)
        ++ reportvalue_bits x));
  for i = n / 4 * 4 to n - 1 do
    C.write_int_at o olen (i * C.size) ~controlwordtype:0 ~clcwversion:0
      ~statusfield:0 ~copineffect:1 ~vcid:7 ~spare:0 ~norf:0 ~nobitlock:0
      ~lockout:(((i land 0xFFF) + 1) lsr 12)
      ~wait:(((i land 0x3F) + 1) lsr 6)
      ~retransmit:((i lsr 13) land 1)
      ~farmbcounter:0 ~spare2:0 ~reportvalue:(i land 0xFF)
  done;
  out

let rows = true
