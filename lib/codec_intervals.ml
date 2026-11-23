(* Interval tests over the fixed-offset fields of a record, flattened into
   an int array that one loop scans with no call. *)

open Codec_repr

(* Top level, so a test allocates no closure. *)
let rec in_set_from (set : (int * int) array) (v : int) i =
  i < Array.length set
  &&
  let lo, hi = Array.unsafe_get set i in
  (v >= lo && v <= hi) || in_set_from set v (i + 1)

let in_set (set : (int * int) array) v =
  if Array.length set = 1 then
    let lo, hi = Array.unsafe_get set 0 in
    v >= lo && v <= hi
  else in_set_from set v 0

(* A set test as ints: the read's kind, its offset, the bitfield shift and mask
   (0 and -1 for a whole scalar), the number of intervals, then their bounds. *)
let flat_interval : type a. a scalar -> (int * int) array -> int array =
 fun scalar set ->
  let kind, off, shift, mask =
    match scalar with
    | Byte off -> (0, off, 0, -1)
    | U16le off -> (1, off, 0, -1)
    | U16be off -> (2, off, 0, -1)
    | Sbyte off -> (3, off, 0, -1)
    | I16le off -> (4, off, 0, -1)
    | I16be off -> (5, off, 0, -1)
    | U32le off -> (6, off, 0, -1)
    | U32be off -> (7, off, 0, -1)
    | I32le off -> (8, off, 0, -1)
    | I32be off -> (9, off, 0, -1)
    | Bits8 { off; shift; mask } -> (0, off, shift, mask)
    | Bits16le { off; shift; mask } -> (1, off, shift, mask)
    | Bits16be { off; shift; mask } -> (2, off, shift, mask)
  in
  Array.concat
    [
      [| kind; off; shift; mask; Array.length set |];
      Array.concat
        (Array.to_list (Array.map (fun (lo, hi) -> [| lo; hi |]) set));
    ]

(* The index of the first failed test in [d], from [i], or -1. The read is
   dispatched in place and nothing is called, so the loop keeps its state in
   registers. The offsets lie inside the frame the caller has checked. *)
let rec scan_intervals d tables buf off n i =
  if i >= n then -1
  else
    let q = off + Array.unsafe_get d (i + 1) in
    let v =
      match Array.unsafe_get d i with
      | 0 -> Unchecked.u8 buf q
      | 1 -> Unchecked.u16le buf q
      | 2 -> Unchecked.u16be buf q
      | 3 -> SInt8.to_int (SInt8.unsafe_get buf q)
      | 4 -> SInt16.to_int (SInt16.unsafe_le buf q)
      | 5 -> SInt16.to_int (SInt16.unsafe_be buf q)
      | 6 -> UInt32.to_int (UInt32.unsafe_le buf q)
      | 7 -> UInt32.to_int (UInt32.unsafe_be buf q)
      | 8 -> SInt32.to_int (SInt32.unsafe_le buf q)
      | 9 -> SInt32.to_int (SInt32.unsafe_be buf q)
      | k ->
          (* A byte table: kind [10 + n] looks the byte up in table [n]. *)
          Char.code
            (String.unsafe_get
               (Array.unsafe_get tables (k - 10))
               (Unchecked.u8 buf q))
    in
    (* A bitfield shifts a value read unsigned, so [asr] is [lsr] there; a
       signed scalar shifts by 0, and wasm_of_ocaml takes [v lsr 0] for
       non-negative, so it would compare a negative [v] unsigned. *)
    let v =
      (v asr Array.unsafe_get d (i + 2)) land Array.unsafe_get d (i + 3)
    in
    let k = Array.unsafe_get d (i + 4) in
    if k = 1 then
      if v >= Array.unsafe_get d (i + 5) && v <= Array.unsafe_get d (i + 6) then
        scan_intervals d tables buf off n (i + 7)
      else i
    else scan_set d tables buf off n i v (i + 5) (i + 5 + (2 * k))

(* The rest of a test with several intervals. Every call in the scan is a tail
   call, so the scan keeps its state in registers. *)
and scan_set d tables buf off n i (v : int) j stop =
  if j >= stop then i
  else if v >= Array.unsafe_get d j && v <= Array.unsafe_get d (j + 1) then
    scan_intervals d tables buf off n stop
  else scan_set d tables buf off n i v (j + 2) stop

(* The byte an interval reads, and the field value a byte [b] gives, for the
   intervals that read a single byte. *)
let byte_view : type a. a scalar -> (int * (int -> int)) option = function
  | Byte off -> Some (off, Fun.id)
  | Sbyte off ->
      Some (off, fun b -> (b lsl (Sys.int_size - 8)) asr (Sys.int_size - 8))
  | Bits8 { off; shift; mask } -> Some (off, fun b -> (b lsr shift) land mask)
  | _ -> None

(* Several intervals on one byte become one lookup in a table of the 256 byte
   values, whatever the fields they test: [version = 4 && ihl >= 5] is one load
   and one lookup. *)
let flatten_intervals tests =
  let groups = Hashtbl.create 8 in
  Array.iter
    (fun (Ps scalar, set) ->
      match byte_view scalar with
      | Some (off, value) ->
          let prev = Option.value ~default:[] (Hashtbl.find_opt groups off) in
          Hashtbl.replace groups off ((value, set) :: prev)
      | None -> ())
    tests;
  let tables = Stdlib.ref [] and n_tables = Stdlib.ref 0 in
  let emitted = Hashtbl.create 8 in
  let table off tests =
    let accept b =
      List.for_all (fun (value, set) -> in_set set (value b)) tests
    in
    let t = String.init 256 (fun b -> if accept b then '\001' else '\000') in
    tables := t :: !tables;
    let i = !n_tables in
    incr n_tables;
    [| 10 + i; off; 0; -1; 1; 1; 1 |]
  in
  let flat =
    Array.to_list tests
    |> List.filter_map (fun (Ps scalar, set) ->
        match byte_view scalar with
        | Some (off, _) when List.length (Hashtbl.find groups off) > 1 ->
            if Hashtbl.mem emitted off then None
            else begin
              Hashtbl.replace emitted off ();
              Some (table off (Hashtbl.find groups off))
            end
        | _ -> Some (flat_interval scalar set))
  in
  (Array.concat flat, Array.of_list (List.rev !tables))

(* {1 Tests as words}

   A record of at least 8 bytes whose every test is one interval of a
   big-endian field or bitfield tests each field as bits of an 8-byte word of
   the record: one load per word, shared by the tests that read it, then a
   shift, a subtraction and one comparison per test, in one closure per number
   of tests: no dispatch and no call per test. *)

let word : type a. size:int -> a scalar -> Codec_words.word option =
 fun ~size s ->
  let w o ~bytes ~s ~w ~signed =
    Some (Codec_words.word ~size o ~bytes ~s ~w ~signed)
  in
  let bits = Codec_words.bit_width in
  match s with
  | Byte o -> w o ~bytes:1 ~s:0 ~w:8 ~signed:false
  | Sbyte o -> w o ~bytes:1 ~s:0 ~w:8 ~signed:true
  | U16be o -> w o ~bytes:2 ~s:0 ~w:16 ~signed:false
  | I16be o -> w o ~bytes:2 ~s:0 ~w:16 ~signed:true
  | U32be o -> w o ~bytes:4 ~s:0 ~w:32 ~signed:false
  | I32be o -> w o ~bytes:4 ~s:0 ~w:32 ~signed:true
  | Bits8 { off; shift; mask } ->
      w off ~bytes:1 ~s:shift ~w:(bits mask) ~signed:false
  | Bits16be { off; shift; mask } ->
      w off ~bytes:2 ~s:shift ~w:(bits mask) ~signed:false
  | U16le _ | U32le _ | I16le _ | I32le _ | Bits16le _ -> None

let[@inline] within buf off q l r m lo hi =
  let v = Codec_words.bits (Codec_words.load buf off q) l r m in
  v >= lo && v <= hi

let test ~size (Ps s, set) =
  match (word ~size s, set) with
  | Some { q; l; r; m; _ }, [| (lo, hi) |] -> Some (q, l, r, m, lo, hi)
  | _ -> None

let single ~size tests =
  if Sys.int_size < 63 || size < 8 || Array.length tests <> 1 then None
  else test ~size tests.(0)

(* The field at bit [s] of the word [w], [m + 1] values wide, passes when its
   distance above [lo] modulo [m + 1] is at most [span]. [word_test] clips the
   interval to the field's values, so that distance is the field's own and one
   comparison tests both bounds, signed or unsigned; an empty interval has
   [span = -1]. The test reads the word of the test before it when it reads
   the same one. *)
let[@inline] holds w s m lo span =
  (Int64.to_int (Int64.shift_right_logical w s) - lo) land m <= span

let word_test ~size ~prev (Ps s, set) =
  match (word ~size s, set) with
  | Some { q; l; r; m; _ }, [| (lo, hi) |] ->
      let w = 64 - r in
      let least, most =
        if m = -1 then (-(1 lsl (w - 1)), (1 lsl (w - 1)) - 1) else (0, m)
      in
      let lo = max lo least and hi = min hi most in
      Some
        (q = prev, q, r - l, (1 lsl w) - 1, lo, if lo > hi then -1 else hi - lo)
  | _ -> None

(* One closure per number of tests, in their order; a test on the word of the
   test before reuses its load. *)
let stage_tests tests =
  match tests with
  | [ t1 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1)
  | [ t1; t2 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2)
  | [ t1; t2; t3 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3)
  | [ t1; t2; t3; t4 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in let k4, q4, s4, m4, lo4, n4 = t4 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3 && let x4 = if k4 then x3 else Codec_words.load buf off q4 in holds x4 s4 m4 lo4 n4)
  | [ t1; t2; t3; t4; t5 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in let k4, q4, s4, m4, lo4, n4 = t4 in let k5, q5, s5, m5, lo5, n5 = t5 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3 && let x4 = if k4 then x3 else Codec_words.load buf off q4 in holds x4 s4 m4 lo4 n4 && let x5 = if k5 then x4 else Codec_words.load buf off q5 in holds x5 s5 m5 lo5 n5)
  | [ t1; t2; t3; t4; t5; t6 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in let k4, q4, s4, m4, lo4, n4 = t4 in let k5, q5, s5, m5, lo5, n5 = t5 in let k6, q6, s6, m6, lo6, n6 = t6 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3 && let x4 = if k4 then x3 else Codec_words.load buf off q4 in holds x4 s4 m4 lo4 n4 && let x5 = if k5 then x4 else Codec_words.load buf off q5 in holds x5 s5 m5 lo5 n5 && let x6 = if k6 then x5 else Codec_words.load buf off q6 in holds x6 s6 m6 lo6 n6)
  | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in let k4, q4, s4, m4, lo4, n4 = t4 in let k5, q5, s5, m5, lo5, n5 = t5 in let k6, q6, s6, m6, lo6, n6 = t6 in let k7, q7, s7, m7, lo7, n7 = t7 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3 && let x4 = if k4 then x3 else Codec_words.load buf off q4 in holds x4 s4 m4 lo4 n4 && let x5 = if k5 then x4 else Codec_words.load buf off q5 in holds x5 s5 m5 lo5 n5 && let x6 = if k6 then x5 else Codec_words.load buf off q6 in holds x6 s6 m6 lo6 n6 && let x7 = if k7 then x6 else Codec_words.load buf off q7 in holds x7 s7 m7 lo7 n7)
  | [ t1; t2; t3; t4; t5; t6; t7; t8 ] ->
      let _, q1, s1, m1, lo1, n1 = t1 in let k2, q2, s2, m2, lo2, n2 = t2 in let k3, q3, s3, m3, lo3, n3 = t3 in let k4, q4, s4, m4, lo4, n4 = t4 in let k5, q5, s5, m5, lo5, n5 = t5 in let k6, q6, s6, m6, lo6, n6 = t6 in let k7, q7, s7, m7, lo7, n7 = t7 in let k8, q8, s8, m8, lo8, n8 = t8 in Some (fun buf off -> let x1 = Codec_words.load buf off q1 in holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else Codec_words.load buf off q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else Codec_words.load buf off q3 in holds x3 s3 m3 lo3 n3 && let x4 = if k4 then x3 else Codec_words.load buf off q4 in holds x4 s4 m4 lo4 n4 && let x5 = if k5 then x4 else Codec_words.load buf off q5 in holds x5 s5 m5 lo5 n5 && let x6 = if k6 then x5 else Codec_words.load buf off q6 in holds x6 s6 m6 lo6 n6 && let x7 = if k7 then x6 else Codec_words.load buf off q7 in holds x7 s7 m7 lo7 n7 && let x8 = if k8 then x7 else Codec_words.load buf off q8 in holds x8 s8 m8 lo8 n8)
  | _ -> None
[@@ocamlformat "disable"]

(* Tests of one word merged into one: the bits [b] to [e] (exclusive) of the
   record they span, counted from its first bit, the mask and the value they
   test relative to the lowest of those bits, and the room of the test of the
   lowest bits, the only one that may not be exact. [open_] is false for a
   test kept on its own: one that fails always or whose values wrap. *)
type group = {
  b : int;
  e : int;
  gm : int;
  glo : int;
  gn : int;
  open_ : bool;
  test : int * int * int * int * int;
}

let group (_, q, s, m, lo, n) =
  let e = (q * 8) + 64 - s in
  {
    b = e - Codec_words.bit_width m;
    e;
    gm = m;
    glo = lo;
    gn = n;
    open_ = n >= 0 && lo >= 0 && lo + n <= m;
    test = (q, s, m, lo, n);
  }

(* [g] and [t] as one test, when one word in the frame of [size] bytes holds
   both, their bits do not overlap and a test that is not exact stays below
   every other bit: an exact test is a mask compared once, and a range below
   exact bits subtracts with no borrow from them unless it fails. *)
let merge ~size g t =
  let b = min g.b t.b and e = max g.e t.e in
  let q = min (b / 8) (size - 8) in
  let dg = e - g.e and dt = e - t.e in
  let lowest = t.b >= g.e in
  if
    g.open_ && t.open_
    && e - b <= 62
    && q * 8 <= b
    && e <= (q * 8) + 64
    && (g.gm lsl dg) land (t.gm lsl dt) = 0
    && if t.gn > 0 then g.gn = 0 && lowest else g.gn = 0 || t.e <= g.e
  then
    let gm = (g.gm lsl dg) lor (t.gm lsl dt)
    and glo = (g.glo lsl dg) lor (t.glo lsl dt)
    and gn = max g.gn t.gn in
    Some
      {
        b;
        e;
        gm;
        glo;
        gn;
        open_ = true;
        test = (q, (q * 8) + 64 - e, gm, glo, gn);
      }
  else None

(* The tests as words, merged where one word holds several: [version = 4 &&
   ihl >= 5] is one test of the first byte, and the tests of a fragment
   flag, the fragment offset and the protocol of IPv4 one test of a word. The
   tests make a conjunction, so their order does not change its answer. *)
let word_tests ~size tests =
  if Sys.int_size < 63 || size < 8 || Array.length tests = 0 then None
  else
    let rec words = function
      | [] -> Some []
      | t :: ts -> (
          match word_test ~size ~prev:(-1) t with
          | None -> None
          | Some w -> Option.map (fun ws -> group w :: ws) (words ts))
    in
    let rec add t = function
      | [] -> [ t ]
      | g :: gs -> (
          match merge ~size g t with Some g -> g :: gs | None -> g :: add t gs)
    in
    Option.map
      (fun ws ->
        let groups = List.fold_left (fun gs t -> add t gs) [] ws in
        let _, tests =
          List.fold_left
            (fun (prev, acc) g ->
              let q, s, m, lo, n = g.test in
              (q, (q = prev, q, s, m, lo, n) :: acc))
            (-1, []) groups
        in
        List.rev tests)
      (words (Array.to_list tests))

let staged ~size tests = Option.bind (word_tests ~size tests) stage_tests
