(* vectors.exe prints WireGen.Vectors.fst: what each primitive the F*
   rendering calls returns in OCaml, on boundary and random operands, stated
   of its definition in WireGen.OCaml. F* checks each fact: by normalising
   for the arithmetic, by Z3 for the loads, which read a sequence of bytes,
   and for [land], which F* defines through bit vectors. *)

let random = Random.State.make [| 0x3d |]

let ints =
  [
    0;
    1;
    2;
    -1;
    -2;
    255;
    256;
    65535;
    65536;
    (1 lsl 31) - 1;
    1 lsl 31;
    (1 lsl 32) - 1;
    1 lsl 32;
    (1 lsl 62) - 1;
    -(1 lsl 62);
    max_int;
    min_int;
  ]
  @ List.init 4 (fun _ -> Random.State.bits random)
  @ List.init 4 (fun _ ->
      Random.State.int64 random Int64.max_int |> Int64.to_int)
  @ List.init 4 (fun _ -> -Random.State.bits random)

let int64s =
  [ 0L; 1L; -1L; Int64.max_int; Int64.min_int; 0xFFFF_FFFFL; 0x8000_0000L ]
  @ List.init 8 (fun _ -> Random.State.bits64 random)

let int32s =
  [ 0l; 1l; -1l; Int32.max_int; Int32.min_int ]
  @ List.init 8 (fun _ -> Random.State.bits32 random)

let lit n = if n < 0 then Fmt.str "(%d)" n else string_of_int n

(* The bit pattern of an [int64] or [int32], as F* holds it. *)
let pattern64 w = Fmt.str "%Lu" w
let pattern32 x = Fmt.str "%lu" x
let norm ppf fact = Fmt.pf ppf "let _ = assert_norm (%s)@\n" fact

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

let buffers =
  Bytes.init 16 (fun i -> Char.chr (0xf0 + i))
  :: Bytes.make 16 '\xff' :: Bytes.make 16 '\x00'
  :: List.init 3 (fun _ ->
      Bytes.init 16 (fun _ -> Char.chr (Random.State.int random 256)))

(* The bytes [b] as a sequence whose every byte Z3 reads in one step. *)
let seq b =
  let n = Bytes.length b in
  let byte i =
    if i = n - 1 then Fmt.str "%duy" (Bytes.get_uint8 b i)
    else Fmt.str "if i = %d then %duy else " i (Bytes.get_uint8 b i)
  in
  Fmt.str "Seq.init %d (fun (i:nat{i < %d}) -> %s)" n n
    (String.concat "" (List.init n byte))

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
     @\n\
     #push-options \"--fuel 0 --ifuel 0 --z3rlimit 20\"@\n";
  arithmetic ppf;
  shifts ppf;
  int64 ppf;
  masks ppf;
  Fmt.pf ppf
    "#pop-options@\n@\n#push-options \"--fuel 9 --ifuel 1 --z3rlimit 20\"@\n";
  loads ppf;
  Fmt.pf ppf "#pop-options@\n%!"
