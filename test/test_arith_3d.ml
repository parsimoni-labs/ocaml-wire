(* Tests for arith_3d.ml: the arithmetic of constraints, sizes and presence
   conditions, as 3D types and verifies it. *)

open Wire
open Test_helpers

let projected c = Everparse.Raw.to_3d (Everparse.project ~mode:`Ffi c).module_

let projects_to what c sub =
  let s = projected c in
  Alcotest.(check bool)
    (Fmt.str "%s: %s" what sub)
    true
    (Re.execp (Re.compile (Re.str sub)) s)

let check_refused what want f =
  Alcotest.(check bool) what want (raises_invalid f)

(* A field [a] of [typ], then a [uint16be] [l] whose constraint is [f a l]. *)
let constrained typ f () =
  let a = Field.v "a" typ in
  Codec.v "Arith"
    (fun a l -> (a, l))
    Codec.
      [
        a $ fst;
        Field.v "l" uint16be ~self_constraint:(fun l -> f (Field.ref a) l) $ snd;
      ]

(* An IPv4-like first byte, then [Length] constrained by [f ihl]. *)
let header f =
  let ihl = Field.v "IHL" (bits ~width:4 U8) in
  Codec.v "Header"
    (fun v i l -> (v, i, l))
    Codec.
      [
        (Field.v "Version" (bits ~width:4 U8) $ fun (v, _, _) -> v);
        (ihl $ fun (_, i, _) -> i);
        ( Field.v "Length" uint16be ~self_constraint:(fun l ->
              f (Field.ref ihl) l)
        $ fun (_, _, l) -> l );
      ]

(* 3D computes [a << 2] over a [UINT8] in 8 bits and wraps, and refuses to
   verify [a * 4] there. The projection casts the operand to the narrowest type
   that holds the result, so the C computes what OCaml does, and leaves alone
   an operation whose range fits its type: [IHL * 4] over 4 bits. *)
let test_arith_widened () =
  projects_to "a lsl 2 over uint8"
    (constrained uint8 Expr.(fun a l -> l >= a lsl int 2) ())
    "(((UINT16) a) << 2)";
  projects_to "a * 4 over uint8"
    (constrained uint8 Expr.(fun a l -> l >= a * int 4) ())
    "(((UINT16) a) * 4)";
  projects_to "a + 1 over uint8"
    (constrained uint8 Expr.(fun a l -> l >= a + int 1) ())
    "(((UINT16) a) + 1)";
  projects_to "IHL * 4 over 4 bits"
    (header Expr.(fun ihl l -> l >= ihl * int 4))
    "(IHL * 4)";
  projects_to "IHL lsl 2 over 4 bits"
    (header Expr.(fun ihl l -> l >= ihl lsl int 2))
    "(IHL << 2)";
  projects_to "a guarded subtraction"
    (constrained uint8 Expr.(fun a l -> a >= int 1 && l >= a - int 1) ())
    "(a - 1)";
  projects_to "a lsl 2 in a size"
    (let n = Field.v "n" uint8 in
     Codec.v "SizeShift"
       (fun n d -> (n, d))
       Codec.
         [
           n $ fst;
           Field.v "d" (byte_array ~size:Expr.(Field.ref n lsl int 2)) $ snd;
         ])
    "[:byte-size (((UINT16) n) << 2)]"

(* Each shape 3d.exe cannot verify, or whose C would compute a value OCaml does
   not, is refused when the codec is built; its neighbour that 3D verifies and
   both compute alike is accepted. *)
let test_arith_refused () =
  let u8 = constrained uint8 and u16 = constrained uint16be in
  check_refused "a - 1 unguarded" true (u8 Expr.(fun a l -> l >= a - int 1));
  check_refused "a - 1 under a >= 1" false
    (u8 Expr.(fun a l -> a >= int 1 && l >= a - int 1));
  check_refused "a - 1 when a >= 1 holds or a = 0" false
    (u8 Expr.(fun a l -> a = int 0 || l >= a - int 1));
  check_refused "l - a under l >= a" false
    (u8 Expr.(fun a l -> l >= a && l - a <= int 5));
  check_refused "l / a, a possibly zero" true
    (u8 Expr.(fun a l -> l / a <= int 5));
  check_refused "l / a under a > 0" false
    (u8 Expr.(fun a l -> a > int 0 && l / a <= int 5));
  check_refused "l mod a, a possibly zero" true
    (u8 Expr.(fun a l -> l mod a <= int 5));
  check_refused "lnot a" true (u8 Expr.(fun a l -> l >= lnot a));
  check_refused "a land lnot 2 over uint8" false
    (u8 Expr.(fun a l -> l >= a land lnot (int 2)));
  check_refused "a land lnot 2 over uint16" true
    (u16 Expr.(fun a l -> l >= a land lnot (int 2)));
  check_refused "a negative literal" true (u8 Expr.(fun a _ -> a <> int (-1)));
  check_refused "field_pos" true (u8 Expr.(fun _ l -> l >= field_pos));
  check_refused "a shift by 64" true (u8 Expr.(fun a l -> l >= a lsl int 64));
  check_refused "a shift by an unbounded field" true
    (u8 Expr.(fun a l -> l >= l lsl a));
  check_refused "a shift by a bounded field" false
    (u8 Expr.(fun a l -> a < int 8 && l >= l lsr a));
  check_refused "a narrowing cast" true (u16 Expr.(fun a l -> l >= to_uint8 a));
  check_refused "a narrowing cast of a bounded value" false
    (u16 Expr.(fun a l -> a <= int 200 && l >= to_uint8 a));
  check_refused "a sum past OCaml's native int" true
    (u8 Expr.(fun a l -> l >= a + int Stdlib.(max_int - 5)));
  check_refused "a product past 64 bits" true
    (u8 Expr.(fun a l -> l >= a * a * a * a * a * a * a * a * a));
  check_refused "a uint64 sum past 64 bits" true
    (constrained uint64be Expr.(fun a l -> l >= a + int 1));
  check_refused "a masked uint64 sum, which F* bounds by its type" true
    (constrained uint64be Expr.(fun a l -> l >= (a land int 255) + int 1));
  let f = Field.v "f" float32be in
  check_refused "a float ordered from another field" true (fun () ->
      Codec.v "FloatCross"
        (fun f v -> (f, v))
        Codec.
          [
            f $ fst;
            Field.v "v" uint8 ~self_constraint:(fun v -> Expr.(Field.ref f < v))
            $ snd;
          ]);
  check_refused "a float compared for equality from another field" false
    (fun () ->
      Codec.v "FloatCrossEq"
        (fun f v -> (f, v))
        Codec.
          [
            f $ fst;
            Field.v "v" uint8 ~self_constraint:(fun _ ->
                Expr.(Field.ref f = int 0))
            $ snd;
          ])

(* 3D reads a signed field as its unsigned bit pattern in a presence condition
   and a size too. A comparison with a literal projects in two's-complement
   form; any other use is refused. A subtraction or multiplication in a
   presence condition is checked when the codec is built. *)
let test_arith_presence_and_size () =
  let gated typ present () =
    let n = Field.v "n" typ in
    Codec.v "Gated"
      (fun n d -> (n, d))
      Codec.
        [
          n $ fst;
          Field.optional "d" ~present:(present (Field.ref n)) uint8 $ snd;
        ]
  in
  let sized typ size () =
    let n = Field.v "n" typ in
    Codec.v "Sized"
      (fun n d -> (n, d))
      Codec.
        [ n $ fst; Field.v "d" (byte_array ~size:(size (Field.ref n))) $ snd ]
  in
  projects_to "an int8 presence"
    (gated int8 Expr.(fun n -> n >= int 5) ())
    "((n ^ 128) >= 133)";
  check_refused "an int8 presence over n + 1" true
    (gated int8 Expr.(fun n -> n + int 1 >= int 5));
  check_refused "an int8 size" true (sized int8 Fun.id);
  check_refused "an int16 size" true (sized int16be Fun.id);
  check_refused "a uint32 cube in a presence" true
    (gated uint32be Expr.(fun n -> n * n * n > int 0));
  check_refused "a uint8 square in a presence" false
    (gated uint8 Expr.(fun n -> n * n > int 3));
  check_refused "a subtraction in a presence" true
    (gated uint8 Expr.(fun n -> n - int 1 > int 3));
  check_refused "a size past 32 bits" true
    (sized uint8 Expr.(fun n -> n + (int 0x2000_0000 * int 8)));
  check_refused "a uint16 product size" false
    (sized uint16be Expr.(fun n -> n * int 2))

let suite =
  ( "arith_3d",
    [
      Alcotest.test_case "widened where 3D would overflow" `Quick
        test_arith_widened;
      Alcotest.test_case "refused where 3D cannot verify it" `Quick
        test_arith_refused;
      Alcotest.test_case "presence conditions and sizes" `Quick
        test_arith_presence_and_size;
    ] )
