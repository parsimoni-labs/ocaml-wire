(* The operands the vectors run the primitives on, OCaml's and OxCaml's
   alike: boundary values and random ones from a fixed seed, and the
   buffers the loads read. *)

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
