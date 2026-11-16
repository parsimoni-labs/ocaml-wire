(* Tests for everparse.ml: the projection of bit groups and their order. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

(* -- Bit-order projection tests --

   Non-native (base, bit_order) combinations should still emit a valid 3D
   struct by reversing declaration order within the bit group and, if the
   widths don't fill the word, prepending anonymous padding. These tests
   lock in that the reorder actually happens in the emitted 3D text. *)

let test_3d_bitorder_u8msb_reorder () =
  (* Non-native: (U8, Msb_first). EverParse native for UINT8 is LSB-first,
     so the projection reverses [a; b] to [b; a] in the emitted 3D text.
     Widths sum to 8 -> no padding. *)
  let codec =
    let open Codec in
    v "ReorderU8"
      (fun a b -> (a, b))
      [
        Field.v "a" (bits ~width:3 U8) $ fst;
        Field.v "b" (bits ~width:5 U8) $ snd;
      ]
  in
  let schema = Everparse.project ~mode:`Ffi codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  let ia = index_of ~sub:"UINT8 a" s in
  let ib = index_of ~sub:"UINT8 b" s in
  Alcotest.(check bool) "a found" true (ia >= 0);
  Alcotest.(check bool) "b found" true (ib >= 0);
  Alcotest.(check bool) "reordered: b before a" true (ib < ia)

let test_3d_bitorder_u8msb_padding () =
  (* Non-native and incomplete: two 3-bit fields in (U8, Msb_first).
     Reversal alone would place the 6 bits at LSB-first positions 0..5;
     the projection prepends 2 bits of anonymous padding so the user's
     fields land at bits 2..7 (matching their Msb_first intent). *)
  let codec =
    let open Codec in
    v "ReorderU8Pad"
      (fun a b -> (a, b))
      [
        Field.v "a" (bits ~width:3 U8) $ fst;
        Field.v "b" (bits ~width:3 U8) $ snd;
      ]
  in
  let schema = Everparse.project ~mode:`Ffi codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "has anonymous padding field" true
    (contains ~sub:"UINT8 _anon_" s);
  Alcotest.(check bool) "padding width = 2" true (contains ~sub:": 2" s)

let test_3d_bitorder_constraint_collapse () =
  (* Non-native: (U8, Msb_first) with a backward-reference constraint.
     Original order [a {a<=5}; b {a+b<=10}]. After reversal to [b; a],
     a constraint [a+b<=10] attached to b would fire before [a] was
     parsed. The projection therefore collapses all group constraints
     onto the last reversed field (here [a]), where every referent is
     available. *)
  let f_a_placeholder = Field.v "a" (bits ~width:4 U8) in
  let f_b_placeholder = Field.v "b" (bits ~width:4 U8) in
  let f_a =
    Field.v "a"
      ~constraint_:Expr.(Field.ref f_a_placeholder <= int 5)
      (bits ~width:4 U8)
  in
  let f_b =
    Field.v "b"
      ~constraint_:
        Expr.(Field.ref f_a_placeholder + Field.ref f_b_placeholder <= int 10)
      (bits ~width:4 U8)
  in
  let codec =
    let open Codec in
    v "Ranged" (fun a b -> (a, b)) [ f_a $ fst; f_b $ snd ]
  in
  let schema = Wire.Everparse.project ~mode:`Ffi codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  (* b appears first in 3D (reversed), without a constraint block. *)
  let ia = index_of ~sub:"UINT8 a : 4" s in
  let ib = index_of ~sub:"UINT8 b : 4" s in
  Alcotest.(check bool) "b before a" true (ib < ia);
  (* The combined constraint lands on [a] (last in reversed order),
     including both the original [a <= 5] and [a + b <= 10]. *)
  Alcotest.(check bool)
    "a still referenced in combined constraint" true (contains ~sub:"a <= 5" s);
  Alcotest.(check bool)
    "b still referenced in combined constraint" true
    (contains ~sub:"(a + b) <= 10" s)

let test_3d_bitorder_native_noreorder () =
  (* Native: (U32be, Msb_first). Fields stay in declared order, no padding. *)
  let codec =
    let open Codec in
    v "NativeU32Be"
      (fun a b -> (a, b))
      [
        Field.v "x" (bits ~width:4 U32be) $ fst;
        Field.v "y" (bits ~width:28 U32be) $ snd;
      ]
  in
  let schema = Everparse.project ~mode:`Ffi codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  let ix = index_of ~sub:"UINT32BE x" s in
  let iy = index_of ~sub:"UINT32BE y" s in
  Alcotest.(check bool) "x found" true (ix >= 0);
  Alcotest.(check bool) "y found" true (iy >= 0);
  Alcotest.(check bool) "x before y (no reorder)" true (ix < iy);
  Alcotest.(check bool) "no padding" false (contains ~sub:"_anon_" s)

(* A constrained group of [Msb_first] bitfields in a base whose native order
   is [Lsb_first] must be reversed (and padded) wherever its struct is
   emitted: at top level, and as a sub-codec in a field, an array element or a
   casetype case. Emitted in declaration order, the C reads the other end of
   the word from the OCaml codec. *)
let msb_group (name, base, bits_word) =
  ( name,
    bits_word,
    Codec.v name
      (fun a b -> (a, b))
      Codec.
        [
          Field.v "a"
            ~self_constraint:(fun r -> Expr.(r <= int 5))
            (bits ~width:3 base)
          $ fst;
          Field.v "b"
            ~self_constraint:(fun r -> Expr.(r <= int 20))
            (bits ~width:5 base)
          $ snd;
        ] )

let msb_groups =
  List.map msb_group
    [
      ("MsbU8", U8, "UINT8");
      ("MsbU16", U16, "UINT16");
      ("MsbU32", U32, "UINT32");
    ]

let msb_group_positions (name, inner) =
  let top =
    ( Fmt.str "%s at top level" name,
      fun () -> Everparse.project ~mode:`Ffi inner )
  in
  let field =
    ( Fmt.str "%s in a field" name,
      fun () ->
        Everparse.project ~mode:`Ffi
          (Codec.v (name ^ "Field")
             (fun g x -> (g, x))
             Codec.[ Field.v "g" (codec inner) $ fst; Field.v "x" uint8 $ snd ])
    )
  in
  let array =
    ( Fmt.str "%s as an array element" name,
      fun () ->
        Everparse.project ~mode:`Ffi
          (Codec.v (name ^ "Array")
             (fun v -> v)
             Codec.
               [ (Field.v "v" (array ~len:(int 2) (codec inner)) $ fun v -> v) ])
    )
  in
  let case =
    ( Fmt.str "%s in a casetype case" name,
      fun () ->
        Everparse.project ~mode:`Ffi
          (Codec.v (name ^ "Case")
             (fun v -> v)
             Codec.
               [
                 ( Field.v "v"
                     (casetype (name ^ "Body") uint8
                        [
                          case ~index:(UInt8.v 1) (codec inner)
                            ~inject:(fun v -> `G v)
                            ~project:(function `G v -> Some v);
                        ])
                 $ fun v -> v );
               ]) )
  in
  [ top; field; array; case ]

(* The body of the typedef of struct [name] in [out]. *)
let typedef_body name out =
  let start = index_of ~sub:("typedef struct Wire" ^ name ^ "\n") out in
  let start =
    if start >= 0 then start
    else index_of ~sub:("typedef struct Wire" ^ name) out
  in
  if start < 0 then Alcotest.failf "no typedef for %s in:\n%s" name out;
  let rest = String.sub out start (String.length out - start) in
  let stop = index_of ~sub:("} " ^ name ^ ";") rest in
  if stop < 0 then Alcotest.failf "unterminated typedef for %s" name;
  String.sub rest 0 stop

let test_3d_bitorder_msb_constrained_everywhere () =
  List.iter
    (fun (name, word, inner) ->
      List.iter
        (fun (label, project) ->
          let out = to_3d (project () : Everparse.t).module_ in
          let body = typedef_body name out in
          let ia = index_of ~sub:(word ^ " a : 3") body in
          let ib = index_of ~sub:(word ^ " b : 5") body in
          if ia < 0 || ib < 0 then
            Alcotest.failf "%s: fields missing from:\n%s" label body;
          if ib > ia then
            Alcotest.failf "%s: the group is not reversed:\n%s" label body;
          if word <> "UINT8" && not (contains ~sub:"_anon_" body) then
            Alcotest.failf "%s: the group is not padded:\n%s" label body)
        (msb_group_positions (name, inner)))
    msb_groups

let suite =
  ( "everparse_bitorder",
    [
      Alcotest.test_case "3d: bit_order U8 Msb_first reorder" `Quick
        test_3d_bitorder_u8msb_reorder;
      Alcotest.test_case "3d: bit_order U8 Msb_first padding" `Quick
        test_3d_bitorder_u8msb_padding;
      Alcotest.test_case "3d: bit_order native no reorder" `Quick
        test_3d_bitorder_native_noreorder;
      Alcotest.test_case "3d: bit_order constraint collapse" `Quick
        test_3d_bitorder_constraint_collapse;
      Alcotest.test_case "3d: bit_order Msb_first constrained everywhere" `Quick
        test_3d_bitorder_msb_constrained_everywhere;
    ] )
