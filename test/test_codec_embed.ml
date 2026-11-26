(* Tests for codec.ml: embedded sub-codecs. *)

open Wire
open Test_helpers

(* -- Nested: Codec typ: embed a sub-codec as a field --
   [inner] / [outer] / [inner_codec] / [outer_codec] live in {!Test_helpers}. *)

let test_codec_embed_decode () =
  (* header(1) + tag(1) + value(2) + trailer(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int) "header" 0xAA (UInt8.to_int r.header);
  Alcotest.(check int) "inner.tag" 0x42 (UInt8.to_int r.inner.tag);
  Alcotest.(check int) "inner.value" 0x1234 (UInt16.to_int r.inner.value);
  Alcotest.(check int) "trailer" 0xBB (UInt8.to_int r.trailer)

let test_codec_embed_encode () =
  let v =
    {
      header = UInt8.v 0xAA;
      inner = { tag = UInt8.v 0x42; value = UInt16.v 0x1234 };
      trailer = UInt8.v 0xBB;
    }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec v buf 0;
  Alcotest.(check int) "header byte" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "value bytes" 0x1234 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "trailer byte" 0xBB (Bytes.get_uint8 buf 4)

let test_codec_embed_roundtrip () =
  let original =
    {
      header = UInt8.v 0x11;
      inner = { tag = UInt8.v 0x22; value = UInt16.v 0x3344 };
      trailer = UInt8.v 0x55;
    }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec original buf 0;
  let decoded = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int)
    "header"
    (UInt8.to_int original.header)
    (UInt8.to_int decoded.header);
  Alcotest.(check int)
    "inner.tag"
    (UInt8.to_int original.inner.tag)
    (UInt8.to_int decoded.inner.tag);
  Alcotest.(check int)
    "inner.value"
    (UInt16.to_int original.inner.value)
    (UInt16.to_int decoded.inner.value);
  Alcotest.(check int)
    "trailer"
    (UInt8.to_int original.trailer)
    (UInt8.to_int decoded.trailer)

let test_codec_embed_wire_size () =
  Alcotest.(check int) "wire_size" 5 (Codec.wire_size outer_codec)

(* Nested codec with bitfields *)

type bf_inner = { version : int; flags : int }

let bf_inner_codec =
  Codec.v "BfInner"
    (fun version flags -> { version; flags })
    Codec.
      [
        (Field.v "Version" (bits ~width:4 U8) $ fun r -> r.version);
        (Field.v "Flags" (bits ~width:4 U8) $ fun r -> r.flags);
      ]

type bf_outer = { id : UInt16.t; bf : bf_inner; checksum : UInt8.t }

let bf_outer_codec =
  Codec.v "BfOuter"
    (fun id bf checksum -> { id; bf; checksum })
    Codec.
      [
        (Field.v "Id" uint16be $ fun r -> r.id);
        (Field.v "Bf" (codec bf_inner_codec) $ fun r -> r.bf);
        (Field.v "Checksum" uint8 $ fun r -> r.checksum);
      ]

let test_codec_embed_bitfield () =
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  (* Default [bit_order = Msb_first]: version (first declared) at top nibble,
     flags at bottom nibble. version=0xA, flags=0x5 -> byte = 0xA5. *)
  Bytes.set_uint8 buf 2 0xA5;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode bf_outer_codec buf 0) in
  Alcotest.(check int) "id" 0x1234 (UInt16.to_int r.id);
  Alcotest.(check int) "version" 0xA r.bf.version;
  Alcotest.(check int) "flags" 0x5 r.bf.flags;
  Alcotest.(check int) "checksum" 0xFF (UInt8.to_int r.checksum)

(* Two levels of nesting --
   [l0] / [l1] / [l2] and their codecs live in {!Test_helpers}. *)

let test_codec_embed_nested () =
  (* l2(1) + l1_y(2) + z(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x42;
  Bytes.set_uint16_be buf 1 0xABCD;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int) "l2.x" 0x42 (UInt8.to_int r.inner.inner.x);
  Alcotest.(check int) "l1.y" 0xABCD (UInt16.to_int r.inner.y);
  Alcotest.(check int) "l0.z" 0xFF (UInt8.to_int r.z)

let test_codec_embed_nested_roundtrip () =
  let original : l0 =
    {
      inner = { inner = { x = UInt8.v 0x42 }; y = UInt16.v 0xABCD };
      z = UInt8.v 0xFF;
    }
  in
  let buf = Bytes.create 4 in
  Codec.encode l0_codec original buf 0;
  let decoded = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int)
    "l2.x"
    (UInt8.to_int original.inner.inner.x)
    (UInt8.to_int decoded.inner.inner.x);
  Alcotest.(check int)
    "l1.y"
    (UInt16.to_int original.inner.y)
    (UInt16.to_int decoded.inner.y);
  Alcotest.(check int) "l0.z" (UInt8.to_int original.z) (UInt8.to_int decoded.z)

(* Sub-codec field name shadowing: parent has its own field with same name as
   a sub-codec field. The parent's name should win for parent-scope expressions
   defined after the parent field. *)

type shadow_inner = { si_x : UInt8.t }

let f_si_x = Field.v "Shared" uint8

let shadow_inner_codec =
  Codec.v "ShadowInner"
    (fun x -> { si_x = x })
    Codec.[ (f_si_x $ fun r -> r.si_x) ]

type shadow_outer = { inner : shadow_inner; shared : UInt8.t; data : string }

let f_so_shared = Field.v "Shared" uint8

let shadow_outer_codec =
  Codec.v "ShadowOuter"
    (fun inner shared data -> { inner; shared; data })
    Codec.
      [
        (Field.v "Inner" (codec shadow_inner_codec) $ fun r -> r.inner);
        (f_so_shared $ fun r -> r.shared);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_so_shared)) $ fun r ->
          r.data );
      ]

let test_codec_field_shadow () =
  (* inner.shared=5, parent.shared=3 -> data should be 3 bytes (parent wins) *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 5;
  (* inner.shared *)
  Bytes.set_uint8 buf 1 3;
  (* parent.shared *)
  Bytes.blit_string "ABC" 0 buf 2 3;
  let r = decode_ok (Codec.decode shadow_outer_codec buf 0) in
  Alcotest.(check int) "inner.shared" 5 (UInt8.to_int r.inner.si_x);
  Alcotest.(check int) "parent.shared" 3 (UInt8.to_int r.shared);
  Alcotest.(check string) "data" "ABC" r.data

(* A sub-codec whose last field is the rest of its input, in a region shorter
   than the buffer and behind a gate: in the region it takes the rest of the
   region, as 3D's [consume-all] does, and behind the gate the rest of the
   buffer. *)
let tail =
  Codec.v "Tail"
    (fun a b -> (a, b))
    Codec.[ Field.v "a" uint8 $ fst; Field.v "rest" all_bytes $ snd ]

let framed_tail =
  Codec.v "FramedTail" Fun.id
    Codec.[ Field.v "t" (nested_at_most ~size:(int 4) (codec tail)) $ Fun.id ]

let tail_gate = Field.v "k" uint8

let gated_tail =
  Codec.v "GatedTail"
    (fun k t -> (k, t))
    Codec.
      [
        tail_gate $ fst;
        Field.optional "t"
          ~present:Expr.(Field.ref tail_gate = int 1)
          (codec tail)
        $ snd;
      ]

let test_codec_embed_tail () =
  let answer f =
    match f () with
    | v -> "ok " ^ v
    | exception exn when Wire.Private.Types.is_parse_error exn ->
        let e = Wire.Private.Types.parse_error_of exn in
        Fmt.str "error %a" Error.pp e
  in
  let show (a, r) = Fmt.str "%d %S" (UInt8.to_int a) r in
  let answers n =
    let b = Bytes.init n (fun i -> Char.chr (i + 1)) in
    [
      answer (fun () -> show (ok (Codec.decode framed_tail b 0)));
      answer (fun () -> ok (Codec.validate framed_tail b 0) |> fun () -> "");
      answer (fun () ->
          match ok (Codec.decode gated_tail b 0) with
          | _, Some t -> show t
          | _, None -> "none");
      answer (fun () -> ok (Codec.validate gated_tail b 0) |> fun () -> "");
    ]
  in
  let check n want =
    Alcotest.(check (list string)) (Fmt.str "%d bytes" n) want (answers n)
  in
  check 3
    [
      "error t@0: unexpected EOF: expected 4 bytes, got 3";
      "error t@0: unexpected EOF: expected 4 bytes, got 3";
      "ok 2 \"\\003\"";
      "ok ";
    ];
  check 4 [ "ok 1 \"\\002\\003\\004\""; "ok "; "ok 2 \"\\003\\004\""; "ok " ];
  check 8
    [
      "ok 1 \"\\002\\003\\004\"";
      "ok ";
      "ok 2 \"\\003\\004\\005\\006\\007\\b\"";
      "ok ";
    ]

(* 3D does not see a sub-codec's fields from the struct that embeds it: an
   expression of the enclosing codec naming one, in a constraint, an action,
   a size, a presence condition or the [where] clause, has no projection
   (3d.exe: "Variable x8 not found"), so the codec is refused when it is
   built. A field of the codec's own that shadows the name still builds. *)
let test_sub_codec_field_out_of_scope () =
  let x8 = Field.v "x8" uint8 in
  let sub = Codec.v "SubX8" Fun.id Codec.[ x8 $ Fun.id ] in
  let mid = Codec.v "MidX8" Fun.id Codec.[ Field.v "s" (codec sub) $ Fun.id ] in
  let with_sub ?where sub last =
    Codec.v "Outer" ?where
      (fun h y -> (h, y))
      Codec.[ Field.v "h" (codec sub) $ fst; last $ snd ]
  in
  let refused what f =
    match f () with
    | _ -> Alcotest.failf "%s: built" what
    | exception Invalid_argument msg ->
        if not (contains ~sub:"sub-codec" msg) then
          Alcotest.failf "%s: refused for another reason: %s" what msg
  in
  refused "constraint" (fun () ->
      with_sub sub
        (Field.v "y" uint8 ~self_constraint:(fun y -> Expr.(Field.ref x8 = y))));
  refused "size" (fun () ->
      with_sub sub (Field.v "y" (byte_array ~size:(Field.ref x8))));
  refused "present" (fun () ->
      with_sub sub
        (Field.optional "y" ~present:Expr.(Field.ref x8 = int 1) uint8));
  refused "where" (fun () ->
      with_sub sub ~where:Expr.(Field.ref x8 = int 5) (Field.v "y" uint8));
  refused "two levels" (fun () ->
      with_sub mid (Field.v "y" (byte_array ~size:(Field.ref x8))));
  let len = Field.v "len" (bits ~width:4 U8) in
  let bf =
    Codec.v "BfX8"
      (fun l f -> (l, f))
      Codec.[ len $ fst; Field.v "flags" (bits ~width:4 U8) $ snd ]
  in
  refused "bitfield" (fun () ->
      with_sub bf (Field.v "y" (byte_array ~size:(Field.ref len))));
  let own = Field.v "x8" uint8 in
  ignore
    (Codec.v "OwnX8"
       (fun h x d -> (h, x, d))
       Codec.
         [
           (Field.v "h" (codec sub) $ fun (h, _, _) -> h);
           (own $ fun (_, x, _) -> x);
           (Field.v "d" (byte_array ~size:(Field.ref own)) $ fun (_, _, d) -> d);
         ]
      : _ Codec.t)

let suite =
  ( "codec_embed",
    [
      Alcotest.test_case "embed: decode" `Quick test_codec_embed_decode;
      Alcotest.test_case "embed: encode" `Quick test_codec_embed_encode;
      Alcotest.test_case "embed: roundtrip" `Quick test_codec_embed_roundtrip;
      Alcotest.test_case "embed: wire_size" `Quick test_codec_embed_wire_size;
      Alcotest.test_case "embed: bitfield" `Quick test_codec_embed_bitfield;
      Alcotest.test_case "embed: nested" `Quick test_codec_embed_nested;
      Alcotest.test_case "embed: nested roundtrip" `Quick
        test_codec_embed_nested_roundtrip;
      Alcotest.test_case "embed: cross field ref shadow" `Quick
        test_codec_field_shadow;
      Alcotest.test_case "embed: rest of the buffer in a region" `Quick
        test_codec_embed_tail;
      Alcotest.test_case "embed: a sub-codec's field is out of scope" `Quick
        test_sub_codec_field_out_of_scope;
    ] )
