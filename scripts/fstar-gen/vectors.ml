(* vectors.exe prints WireGen.Vectors.fst: what each primitive the F*
   rendering calls returns in OCaml, on boundary and random operands, stated
   of its definition in WireGen.OCaml. F* checks each fact: by normalising
   for the arithmetic, by Z3 for the loads, which read a sequence of bytes,
   and for [land], which F* defines through bit vectors. Z3 reads the
   primitives WireGen.OCaml keeps opaque through the rules of
   WireGen.Defs. *)

open Operands

(* [a op b], where OCaml computes it without wrapping. *)
let exact op a b r =
  match op with
  | "add" -> a >= 0 <> (b >= 0) || r >= 0 = (a >= 0)
  | "sub" -> a >= 0 = (b >= 0) || r >= 0 = (a >= 0)
  | _ -> a = 0 || (r / a = b && not (a = -1 && b = min_int))

let arithmetic ppf =
  let pairs =
    List.concat_map (fun a -> List.map (fun b -> (a, b)) ints) ints
    |> List.filteri (fun i _ -> i mod 13 = 0)
  in
  List.iter
    (fun (op, f) ->
      List.iter
        (fun (a, b) ->
          let r = f a b in
          if exact op a b r then
            norm ppf (Fmt.str "P.%s %s %s == %s" op (lit a) (lit b) (lit r)))
        pairs)
    [ ("add", ( + )); ("sub", ( - )); ("mul", ( * )) ];
  List.iter
    (fun (op, f) ->
      List.iter
        (fun (a, b) ->
          norm ppf (Fmt.str "P.%s %s %s == %b" op (lit a) (lit b) (f a b)))
        pairs)
    [
      ("lt", ( < ));
      ("le", ( <= ));
      ("gt", ( > ));
      ("ge", ( >= ));
      ("eq", ( = ));
      ("ne", ( <> ));
    ]

let shifts ppf =
  List.iter
    (fun a ->
      List.iter
        (fun k ->
          norm ppf (Fmt.str "P.lsr %s %d == %s" (lit a) k (lit (a lsr k)));
          (* Its precondition, that the product fits, is stated of [pow2 k]. *)
          if (a lsl k) asr k = a then
            Fmt.pf ppf
              "let _ = assert_norm (pow2 %d == %s); assert_norm (P.lsl %s %d \
               == %s)@\n"
              k
              (pattern64 (Int64.shift_left 1L k))
              (lit a) k
              (lit (a lsl k)))
        [ 0; 1; 13; 32; 62 ])
    ints

let int64 ppf =
  List.iter
    (fun w ->
      norm ppf
        (Fmt.str "P.to_int %s == %s" (pattern64 w) (lit (Int64.to_int w)));
      List.iter
        (fun k ->
          norm ppf
            (Fmt.str "P.shift_right_logical %s %d == %s" (pattern64 w) k
               (pattern64 (Int64.shift_right_logical w k))))
        [ 0; 8; 16; 32; 48; 56; 63 ])
    int64s;
  List.iter
    (fun x ->
      norm ppf
        (Fmt.str "P.int32_to_int %s == %s" (pattern32 x) (lit (Int32.to_int x))))
    int32s

(* The [Int64] conversions of the step walk. *)
let conversions64 ppf =
  List.iter
    (fun a ->
      norm ppf
        (Fmt.str "P.int64_of_int %s == %s" (lit a) (pattern64 (Int64.of_int a))))
    ints;
  List.iter
    (fun x ->
      norm ppf
        (Fmt.str "P.int64_of_int32 %s == %s" (pattern32 x)
           (pattern64 (Int64.of_int32 x))))
    int32s

let pairs64 =
  List.concat_map (fun a -> List.map (fun b -> (a, b)) int64s) int64s
  |> List.filteri (fun i _ -> i mod 3 = 0)

(* The [Int64] arithmetic modulo 2^64 and the signed comparisons. *)
let arithmetic64 ppf =
  List.iter
    (fun (op, f) ->
      List.iter
        (fun (a, b) ->
          norm ppf
            (Fmt.str "P.int64_%s %s %s == %s" op (pattern64 a) (pattern64 b)
               (pattern64 (f a b))))
        pairs64)
    [ ("add", Int64.add); ("sub", Int64.sub); ("mul", Int64.mul) ];
  List.iter
    (fun (op, f) ->
      List.iter
        (fun (a, b) ->
          norm ppf
            (Fmt.str "P.int64_%s %s %s == %b" op (pattern64 a) (pattern64 b)
               (f (Int64.compare a b) 0)))
        pairs64)
    [
      ("lt", ( < ));
      ("le", ( <= ));
      ("gt", ( > ));
      ("ge", ( >= ));
      ("eq", ( = ));
      ("ne", ( <> ));
    ]

(* [Int64.shift_left], and [Int64.logand] by the masks the emitter writes. *)
let bits64 ppf =
  List.iter
    (fun w ->
      List.iter
        (fun k ->
          norm ppf
            (Fmt.str "P.shift_left %s %d == %s" (pattern64 w) k
               (pattern64 (Int64.shift_left w k))))
        [ 0; 1; 2; 8; 32; 63 ];
      List.iter
        (fun k ->
          let m = Int64.pred (Int64.shift_left 1L k) in
          Fmt.pf ppf
            "let _ = assert_norm (pow2 %d == %s); P.int64_mask %s %d; assert \
             (P.int64_logand %s %s == %s)@\n"
            k
            (pattern64 (Int64.shift_left 1L k))
            (pattern64 w) k (pattern64 w) (pattern64 m)
            (pattern64 (Int64.logand w m)))
        [ 1; 8; 16; 32; 62 ])
    int64s

(* [land] by the masks the emitter writes, each fact through the lemma that
   reads a mask as a remainder. *)
let masks ppf =
  List.iter
    (fun a ->
      List.iter
        (fun k ->
          let m = (1 lsl k) - 1 in
          Fmt.pf ppf
            "let _ = assert_norm (pow2 %d == %s); P.land_low %s %d; assert \
             (P.land %s %d == %s)@\n"
            k
            (pattern64 (Int64.shift_left 1L k))
            (lit a) k (lit a) m
            (lit (a land m)))
        [ 1; 3; 8; 16; 32; 62 ])
    ints;
  List.iter
    (fun x ->
      let m = (1 lsl 32) - 1 in
      Fmt.pf ppf
        "let _ = assert_norm (pow2 32 == %d); P.land_int32 %s %d; assert \
         (P.land (P.int32_to_int %s) %d == %s)@\n"
        (m + 1) (pattern32 x) m (pattern32 x) m
        (lit (Int32.to_int x land m)))
    int32s

let loads ppf =
  List.iteri
    (fun i b ->
      Fmt.pf ppf "let b%d : P.buf = %s@\n" i (seq b);
      List.iter
        (fun o ->
          Fmt.pf ppf "let _ = assert (P.u8 b%d %d == %d)@\n" i o
            (Wire_runtime.Unchecked.u8 b o);
          if o + 2 <= 16 then
            Fmt.pf ppf "let _ = assert (P.u16be b%d %d == %d)@\n" i o
              (Wire_runtime.Unchecked.u16be b o);
          if o + 4 <= 16 then
            Fmt.pf ppf "let _ = assert (P.i32be b%d %d == %s)@\n" i o
              (pattern32 (Wire_runtime.Unchecked.i32be b o));
          if o + 8 <= 16 then
            Fmt.pf ppf "let _ = assert (P.i64be b%d %d == %s)@\n" i o
              (pattern64 (Wire_runtime.Unchecked.i64be b o)))
        [ 0; 3; 8; 15 ])
    buffers

let () =
  let ppf = Format.std_formatter in
  Fmt.pf ppf
    "module WireGen.Vectors@\n\
     @\n\
     (* Generated by vectors.exe: the OCaml primitives on boundary and random \
     operands. Do not edit. *)@\n\
     @\n\
     module P = WireGen.OCaml@\n\
     open WireGen.Defs@\n\
     @\n\
     #push-options \"--fuel 0 --ifuel 0 --z3rlimit 20\"@\n";
  arithmetic ppf;
  shifts ppf;
  int64 ppf;
  conversions64 ppf;
  arithmetic64 ppf;
  bits64 ppf;
  masks ppf;
  Fmt.pf ppf
    "#pop-options@\n@\n#push-options \"--fuel 9 --ifuel 1 --z3rlimit 20\"@\n";
  loads ppf;
  Fmt.pf ppf "#pop-options@\n%!"
