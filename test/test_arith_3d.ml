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
  check_refused "a - a" false (u8 Expr.(fun a l -> l >= a - a));
  check_refused "l - a - 1 under l > a" false
    (u8 Expr.(fun a l -> l > a && l - a - int 1 <= int 5));
  check_refused "l - (a + 1) under l > a" false
    (u8 Expr.(fun a l -> l > a && l - (a + int 1) <= int 5));
  check_refused "l - a under l = a" false
    (u8 Expr.(fun a l -> l = a && l - a = int 0));
  check_refused "l - a - 1 under l >= a" true
    (u8 Expr.(fun a l -> l >= a && l - a - int 1 <= int 5));
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
  check_refused "a sum past OCaml's native int" false
    (u8 Expr.(fun a l -> l >= a + int Stdlib.(max_int - 5)));
  check_refused "a uint32 square" false
    (constrained uint32be Expr.(fun a l -> l <= a * a));
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
    (sized uint16be Expr.(fun n -> n * int 2));
  check_refused "a constant size of 2^32" true
    (sized uint8 Expr.(fun _ -> int 0x2000_0000 * int 8));
  check_refused "a constant size below 2^32" false
    (sized uint8 (fun _ -> int 0x2000_0000));
  let per_byte f () =
    Codec.v "PerByte" Fun.id
      Codec.
        [ Field.v "d" (byte_array_where ~size:(int 4) ~per_byte:f) $ Fun.id ]
  in
  check_refused "a per-byte subtraction that can go below zero" true
    (per_byte Expr.(fun b -> b - int 1 <= int 200));
  check_refused "a guarded per-byte subtraction" false
    (per_byte Expr.(fun b -> b >= int 1 && b - int 1 <= int 200));
  check_refused "a per-byte sum past the native int" true
    (per_byte Expr.(fun b -> b + int Stdlib.(max_int - 5) <> int max_int))

(* A value past the native int is computed on 64 bits, unsigned, at the type 3D
   gives it: [a + k] with [k] near [max_int] passes [max_int] once [a > 5], and
   the square of a [uint32] reaches [2^60] once [a >= 2^30]. *)
let test_arith_wide () =
  let k = Stdlib.(max_int - 5) in
  let near =
    constrained uint8
      Expr.(fun a l -> a + int k <> int max_int && l >= int 0)
      ()
  in
  let decodes c s = Result.is_ok (Codec.decode c (Bytes.of_string s) 0) in
  Alcotest.(check bool) "a = 4" true (decodes near "\004\000\000");
  Alcotest.(check bool) "a = 5" false (decodes near "\005\000\000");
  Alcotest.(check bool) "a = 255" true (decodes near "\255\000\000");
  let a = Field.v "a" uint32be in
  let square =
    Codec.v "Square"
      (fun a d -> (a, d))
      Codec.
        [
          a $ fst;
          Field.optional "d"
            ~present:
              Expr.(
                Field.ref a * Field.ref a
                >= int 0x2000_0000 * int 0x2000_0000 * int 4)
            uint8
          $ snd;
        ]
  in
  let present s =
    match Codec.decode square (Bytes.of_string s) 0 with
    | Ok (_, d) -> Some (Option.is_some d)
    | Error _ -> None
  in
  Alcotest.(check (option bool))
    "a = 2^30 - 1" (Some false)
    (present "\x3f\xff\xff\xff");
  Alcotest.(check (option bool))
    "a = 2^30" (Some true)
    (present "\x40\x00\x00\x00\x01");
  Alcotest.(check (option bool))
    "a = 2^32 - 1" (Some true)
    (present "\xff\xff\xff\xff\x01")

(* The body of an optional region is a 3D declaration of its own, which reads
   the fields it names as [UINT32] formals: a size over them must verify there
   as written. An action's arithmetic is checked and widened as a
   constraint's is. *)
let test_arith_regions_and_actions () =
  let inner = Codec.v "Inner" Fun.id Codec.[ Field.v "x" uint8 $ Fun.id ] in
  let a = Field.v "a" uint8 in
  let sized size = nested ~size:(size (Field.ref a)) (codec inner) in
  let region size () =
    Codec.v "Region"
      (fun a r -> (a, r))
      Codec.[ a $ fst; Field.v "r" (sized size) $ snd ]
  in
  let optional size () =
    Codec.v "OptRegion"
      (fun a r -> (a, r))
      Codec.
        [
          a $ fst;
          Field.optional "r" ~present:Expr.(Field.ref a > int 0) (sized size)
          $ snd;
        ]
  in
  check_refused "a product sizing an optional region" true
    (optional Expr.(fun a -> a * int 4));
  check_refused "a field sizing an optional region" false (optional Fun.id);
  projects_to "a product sizing a region"
    (region Expr.(fun a -> a * int 4) ())
    "(((UINT16) a) * 4)";
  let act f () =
    let out = Param.output "out" uint16be in
    Codec.v "Act" Fun.id
      Codec.
        [
          Field.v "a" uint8
            ~action:(Action.on_success [ Action.assign out (f (Field.ref a)) ])
          $ Fun.id;
        ]
  in
  projects_to "an assigned product"
    (act Expr.(fun a -> a * int 4) ())
    "*out = (((UINT16) a) * 4);";
  check_refused "an assigned subtraction that can go below zero" true
    (act Expr.(fun a -> a - int 1))

let suite =
  ( "arith_3d",
    [
      Alcotest.test_case "widened where 3D would overflow" `Quick
        test_arith_widened;
      Alcotest.test_case "refused where 3D cannot verify it" `Quick
        test_arith_refused;
      Alcotest.test_case "presence conditions and sizes" `Quick
        test_arith_presence_and_size;
      Alcotest.test_case "computed on 64 bits past the native int" `Quick
        test_arith_wide;
      Alcotest.test_case "optional regions and actions" `Quick
        test_arith_regions_and_actions;
    ] )
