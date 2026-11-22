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
   the record, one load, shifts and two comparisons, in one closure per number
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

let stage_tests tests =
  match tests with
  | [ t1 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1)
  | [ t1; t2 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2)
  | [ t1; t2; t3 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3)
  | [ t1; t2; t3; t4 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in let q4, l4, r4, m4, lo4, hi4 = t4 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3 && within buf off q4 l4 r4 m4 lo4 hi4)
  | [ t1; t2; t3; t4; t5 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in let q4, l4, r4, m4, lo4, hi4 = t4 in let q5, l5, r5, m5, lo5, hi5 = t5 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3 && within buf off q4 l4 r4 m4 lo4 hi4 && within buf off q5 l5 r5 m5 lo5 hi5)
  | [ t1; t2; t3; t4; t5; t6 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in let q4, l4, r4, m4, lo4, hi4 = t4 in let q5, l5, r5, m5, lo5, hi5 = t5 in let q6, l6, r6, m6, lo6, hi6 = t6 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3 && within buf off q4 l4 r4 m4 lo4 hi4 && within buf off q5 l5 r5 m5 lo5 hi5 && within buf off q6 l6 r6 m6 lo6 hi6)
  | [ t1; t2; t3; t4; t5; t6; t7 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in let q4, l4, r4, m4, lo4, hi4 = t4 in let q5, l5, r5, m5, lo5, hi5 = t5 in let q6, l6, r6, m6, lo6, hi6 = t6 in let q7, l7, r7, m7, lo7, hi7 = t7 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3 && within buf off q4 l4 r4 m4 lo4 hi4 && within buf off q5 l5 r5 m5 lo5 hi5 && within buf off q6 l6 r6 m6 lo6 hi6 && within buf off q7 l7 r7 m7 lo7 hi7)
  | [ t1; t2; t3; t4; t5; t6; t7; t8 ] ->
      let q1, l1, r1, m1, lo1, hi1 = t1 in let q2, l2, r2, m2, lo2, hi2 = t2 in let q3, l3, r3, m3, lo3, hi3 = t3 in let q4, l4, r4, m4, lo4, hi4 = t4 in let q5, l5, r5, m5, lo5, hi5 = t5 in let q6, l6, r6, m6, lo6, hi6 = t6 in let q7, l7, r7, m7, lo7, hi7 = t7 in let q8, l8, r8, m8, lo8, hi8 = t8 in Some (fun buf off -> within buf off q1 l1 r1 m1 lo1 hi1 && within buf off q2 l2 r2 m2 lo2 hi2 && within buf off q3 l3 r3 m3 lo3 hi3 && within buf off q4 l4 r4 m4 lo4 hi4 && within buf off q5 l5 r5 m5 lo5 hi5 && within buf off q6 l6 r6 m6 lo6 hi6 && within buf off q7 l7 r7 m7 lo7 hi7 && within buf off q8 l8 r8 m8 lo8 hi8)
  | _ -> None
[@@ocamlformat "disable"]

let staged ~size tests =
  if Sys.int_size < 63 || size < 8 || Array.length tests = 0 then None
  else
    let tests = Array.to_list (Array.map (test ~size) tests) in
    if List.for_all Option.is_some tests then
      stage_tests (List.map Option.get tests)
    else None
