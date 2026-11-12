(* Tests for codec.ml: records, validation and decode errors. *)

open Wire
open Test_helpers

(* -- Record codec tests -- *)

let test_record_encode () =
  let v =
    { a = UInt8.v 0x42; b = UInt16.v 0x1234; c = UInt32.of_int32 0x56789ABCl }
  in
  match encode_record simple_record_codec v with
  | Error e -> Alcotest.failf "%a" pp_parse_error e
  | Ok encoded ->
      (* uint8 + uint16_le + uint32_le *)
      Alcotest.(check int) "length" 7 (String.length encoded);
      Alcotest.(check int) "byte 0 (a)" 0x42 (Char.code encoded.[0]);
      (* uint16 LE: 0x1234 -> 0x34, 0x12 *)
      Alcotest.(check int) "byte 1 (b low)" 0x34 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2 (b high)" 0x12 (Char.code encoded.[2])

let test_record_decode () =
  let input = "\x42\x34\x12\xBC\x9A\x78\x56" in
  match decode_record simple_record_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 0x42 (UInt8.to_int v.a);
      Alcotest.(check int) "b" 0x1234 (UInt16.to_int v.b);
      Alcotest.(check int32) "c" 0x56789ABCl (UInt32.to_int32 v.c)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_decode_consumption_modes () =
  let field = Field.v "value" uint8 in
  let codec = Codec.v "OneByte" Fun.id Codec.[ field $ Fun.id ] in
  let buf = Bytes.of_string "\x00\x2a\xff" in
  let check_value label = function
    | Ok v -> Alcotest.(check int) label 0x2a (UInt8.to_int v)
    | Error e -> Alcotest.failf "%s: %a" label pp_parse_error e
  in
  check_value "default prefix" (Codec.decode codec buf 1);
  check_value "explicit prefix" (Codec.decode ~consume:`Prefix codec buf 1);
  (match Codec.decode ~consume:`All codec buf 1 with
  | Error { at = 2; field = []; kind = Trailing_bytes 1 } -> ()
  | Error e -> Alcotest.failf "all: wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "all: accepted a trailing byte");
  let exact = Bytes.of_string "\x00\x2a" in
  check_value "all consumed" (Codec.decode ~consume:`All codec exact 1);
  let variable_field = Field.v "text" zeroterm in
  let variable_codec =
    Codec.v "Terminated" Fun.id Codec.[ variable_field $ Fun.id ]
  in
  (match
     Codec.decode ~consume:`All variable_codec (Bytes.of_string "ok\x00!") 0
   with
  | Error { at = 3; field = []; kind = Trailing_bytes 1 } -> ()
  | Error e -> Alcotest.failf "variable all: wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "variable all: accepted a trailing byte");
  Alcotest.check_raises "raising variant"
    (Parse_error { at = 2; field = []; kind = Trailing_bytes 1 })
    (fun () -> ignore (Codec.decode_exn ~consume:`All codec buf 1))

let test_record_roundtrip () =
  let original =
    { a = UInt8.v 0xAB; b = UInt16.v 0xCDEF; c = UInt32.of_int 0x12345678 }
  in
  match encode_record simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int)
            "a roundtrip" (UInt8.to_int original.a) (UInt8.to_int decoded.a);
          Alcotest.(check int)
            "b roundtrip" (UInt16.to_int original.b) (UInt16.to_int decoded.b);
          Alcotest.(check int)
            "c roundtrip" (UInt32.to_int original.c) (UInt32.to_int decoded.c)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_duplicate_names_rejected () =
  let check_invalid ~kind ~name f =
    match f () with
    | () -> Alcotest.failf "duplicate %s name was accepted" kind
    | exception Invalid_argument msg ->
        Alcotest.(check bool) "codec named" true (contains ~sub:"DupNames" msg);
        Alcotest.(check bool) "kind named" true (contains ~sub:kind msg);
        Alcotest.(check bool) "duplicate named" true (contains ~sub:name msg)
  in
  check_invalid ~kind:"field" ~name:"dup" (fun () ->
      ignore
        (Codec.v "DupNames"
           (fun a b -> (a, b))
           Codec.[ Field.v "dup" uint8 $ fst; Field.v "dup" uint8 $ snd ]));
  let first = Param.input "count" uint8 in
  let second = Param.input "count" uint8 in
  check_invalid ~kind:"parameter" ~name:"count" (fun () ->
      ignore
        (Codec.v "DupNames"
           (fun a b -> (a, b))
           Codec.
             [
               Field.v "a" (byte_array ~size:(Param.expr first)) $ fst;
               Field.v "b" (byte_array ~size:(Param.expr second)) $ snd;
             ]))

let test_struct_of_record () =
  let output = render_3d simple_record_codec in
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a;" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b;" output);
  Alcotest.(check bool) "contains field c" true (contains ~sub:"c;" output)

let meta_f_x = Field.v "x" uint8

let meta_codec =
  let open Codec in
  v "MetaRecord"
    ~where:Expr.(Field.ref meta_f_x = int 8)
    (fun x -> { x })
    [
      ( Field.v "x"
          ~constraint_:Expr.(Field.ref meta_f_x <= int 10)
          ~action:
            (Action.on_success
               [
                 Action.return_bool Expr.(Field.ref meta_f_x mod int 2 = int 0);
               ])
          uint8
      $ fun r -> r.x );
    ]

let test_codec_metadata_decode_ok () =
  let buf = Bytes.of_string "\x08" in
  let v = decode_ok (Codec.decode meta_codec buf 0) in
  Alcotest.(check int) "x" 8 (UInt8.to_int v.x)

let test_metadata_constraint_fail () =
  let buf = Bytes.of_string "\x0B" in
  match Codec.decode meta_codec buf 0 with
  | Error { kind = Constraint_failed { which = Field; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_metadata_action_fail () =
  let buf = Bytes.of_string "\x09" in
  match Codec.decode meta_codec buf 0 with
  | Error { kind = Constraint_failed { which = Action; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let _projection_limit_expr = Param.expr projection_limit

let test_metadata_with_params () =
  let env =
    Codec.env projection_codec |> Param.bind projection_limit (UInt8.v 10)
  in
  let buf = Bytes.of_string "\x08" in
  let v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int) "x" 8 (UInt8.to_int v.x);
  Alcotest.(check int) "outx" 8 (UInt8.to_int (Param.get env projection_outx))

let test_metadata_where_fail () =
  let env =
    Codec.env projection_codec |> Param.bind projection_limit (UInt8.v 7)
  in
  let buf = Bytes.of_string "\x08" in
  match Codec.decode ~env projection_codec buf 0 with
  | Error { kind = Constraint_failed { which = Where; _ }; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let validate_f_x = Field.v "x" uint8

let validate_codec, validate_cf_x =
  let open Codec in
  let cf_x =
    Field.v "x" ~constraint_:Expr.(Field.ref validate_f_x <= int 10) uint8
    $ fun r -> r.x
  in
  let codec =
    v "ValidateTest"
      ~where:Expr.(Field.ref validate_f_x = int 8)
      (fun x -> { x })
      [ cf_x ]
  in
  (codec, cf_x)

let test_validate_rejects_bad_where () =
  (* where requires x = 8, set x = 7 *)
  let buf = Bytes.of_string "\x07" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  (* get returns raw value without checking *)
  Alcotest.(check int) "get bypasses where" 7 (UInt8.to_int (get_x buf 0));
  (* validate catches the violation *)
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject where violation"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_rejects_bad_constraint () =
  (* constraint requires x <= 10, set x = 11 *)
  let buf = Bytes.of_string "\x0B" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "get bypasses constraint" 11 (UInt8.to_int (get_x buf 0));
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject constraint violation"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_then_get () =
  (* x = 8 satisfies both where (= 8) and constraint (<= 10) *)
  let buf = Bytes.of_string "\x08" in
  Codec.validate validate_codec buf 0;
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "validate then get" 8 (UInt8.to_int (get_x buf 0))

(* [Codec.validate] is the safety gate before a zero-copy [get] on untrusted
   input, so it must run decode's structural bounds check even for a codec with
   no constraints (whose constraint validator is a no-op). A short buffer must be
   rejected, not silently accepted. *)
let test_validate_bounds_constraint_free () =
  let c =
    Codec.v "NoConstr"
      (fun a b -> (a, b))
      Codec.[ Field.v "a" uint32be $ fst; Field.v "b" uint32be $ snd ]
  in
  (match Codec.validate c (Bytes.make 2 '\000') 0 with
  | exception Parse_error { kind = Unexpected_eof _; _ } -> ()
  | exception Parse_error _ ->
      Alcotest.fail "validate failed with the wrong error on a short buffer"
  | () -> Alcotest.fail "validate accepted a buffer too short for the codec");
  (* A full buffer still validates. *)
  Codec.validate c (Bytes.make 8 '\000') 0

(* [all_zeros] padding must read as all-zero. Decode enforces it; [Codec.validate]
   must too, or validate-then-get silently accepts tampered padding bytes. *)
let test_validate_all_zeros () =
  let c =
    Codec.v "Pad"
      (fun tag pad -> (tag, pad))
      Codec.[ Field.v "tag" uint8 $ fst; Field.v "pad" all_zeros $ snd ]
  in
  Codec.validate c (Bytes.of_string "\x01\x00\x00\x00") 0;
  let tampered = Bytes.of_string "\x01\x00\x05\x00" in
  (match Codec.decode c tampered 0 with
  | Error { kind = Non_zero_padding; _ } -> ()
  | Ok _ -> Alcotest.fail "decode accepted non-zero all_zeros padding"
  | Error _ -> Alcotest.fail "decode failed with the wrong error");
  match Codec.validate c tampered 0 with
  | exception Parse_error { kind = Non_zero_padding; _ } -> ()
  | () -> Alcotest.fail "validate accepted non-zero all_zeros padding"
  | exception Parse_error _ ->
      Alcotest.fail "validate failed with the wrong error"

(* A field [~self_constraint] violation reports which predicate failed and the
   offending field value, so a demux can route on it (e.g. a version field that
   failed [self = 2] hands back [value = Some 1L]). *)
let test_self_constraint_reports_value () =
  let c =
    Codec.v "Ver"
      (fun v rest -> (v, rest))
      Codec.
        [
          Field.v "version" uint8 ~self_constraint:(fun s -> Expr.(s = int 2))
          $ fst;
          Field.v "rest" uint8 $ snd;
        ]
  in
  match Codec.decode c (Bytes.of_string "\x01\x00") 0 with
  | Error
      { kind = Constraint_failed { which = Field; value = Some 1L }; field; _ }
    ->
      Alcotest.(check (list string)) "field path" [ "version" ] field
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "decode accepted a version violating its constraint"

(* Regression: the non-zero [all_zeros] byte's offset is carried on the struct
   field path, not only the direct [of_string] path. The struct path used to
   discard it into a stringly [Constraint_failed]. *)
let test_all_zeros_offset_on_struct_path () =
  let c =
    Codec.v "Pad"
      (fun tag pad -> (tag, pad))
      Codec.[ Field.v "tag" uint8 $ fst; Field.v "pad" all_zeros $ snd ]
  in
  match Codec.decode c (Bytes.of_string "\x01\x00\x05\x00") 0 with
  | Error { kind = Non_zero_padding; at; field } ->
      Alcotest.(check int) "non-zero byte offset" 2 at;
      Alcotest.(check (list string)) "field path" [ "pad" ] field
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "decode accepted non-zero padding"

(* A failure inside a nested sub-codec accumulates the root-to-leaf field path
   as the error unwinds. *)
let test_field_path_nested () =
  let inner =
    Codec.v "Inner" Fun.id
      Codec.
        [
          Field.v "leaf" uint8 ~self_constraint:(fun s -> Expr.(s = int 2))
          $ Fun.id;
        ]
  in
  let outer =
    Codec.v "Outer"
      (fun tag data -> (tag, data))
      Codec.[ Field.v "tag" uint8 $ fst; Field.v "inner" (codec inner) $ snd ]
  in
  (* tag = 1, then inner.leaf = 9 violates [leaf = 2] *)
  match Codec.decode outer (Bytes.of_string "\x01\x09") 0 with
  | Error { kind = Constraint_failed { which = Field; _ }; field; _ } ->
      Alcotest.(check (list string))
        "nested field path" [ "inner"; "leaf" ] field
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "decode accepted a violating nested field"

(* A codec's own field shadows an embedded sub-codec's field of the same name,
   as it does in the 3D struct EverParse checks: a [where] clause, a size and
   [Codec.field_readers] all name the codec's own [x], whether the sub-codec
   comes before it or after it. *)
let shadow_inner =
  Codec.v "ShadowInner"
    (fun x y -> (x, y))
    Codec.[ Field.v "x" uint8 $ fst; Field.v "y" uint8 $ snd ]

(* [Codec.validate], and the validator built from [s], the same layout as a
   struct, read the codec's own [x]: they accept [own], where it holds the
   value the codec needs, and refuse [sub], where only the sub-codec's [x]
   does. *)
let check_validates_own_x c s ~own ~sub =
  let v = Codec.validator_of_struct s in
  let accepts f bytes =
    match f (Bytes.of_string bytes) 0 with
    | () -> true
    | exception Parse_error _ -> false
  in
  List.iter
    (fun (what, f) ->
      Alcotest.(check bool) (what ^ " reads own x") true (accepts f own);
      Alcotest.(check bool)
        (what ^ " ignores the sub-codec's x")
        false (accepts f sub))
    [
      ("validate", Codec.validate c);
      ("validate_struct", Codec.validate_struct v);
    ]

let test_own_field_shadows_where () =
  let f_x = Field.v "x" uint8 in
  let outer =
    Codec.v "ShadowWhere"
      ~where:Expr.(Field.ref f_x = int 7)
      (fun s x -> (s, x))
      Codec.[ Field.v "s" (codec shadow_inner) $ fst; f_x $ snd ]
  in
  (* the sub-codec's x is 1, the codec's own x is 7 *)
  let s =
    let module T = Private.Types in
    T.param_struct "ShadowWhere" []
      ~where:Expr.(T.ref "x" = int 7)
      [ T.field "s" (codec shadow_inner); T.field "x" uint8 ]
  in
  check_validates_own_x outer s ~own:"\x01\x02\x07" ~sub:"\x07\x02\x01";
  match Codec.decode outer (Bytes.of_string "\x01\x02\x07") 0 with
  | Ok (_, x) -> Alcotest.(check int) "own x" 7 (UInt8.to_int x)
  | Error e ->
      Alcotest.failf "where read the sub-codec's x: %a" pp_parse_error e

let test_own_field_shadows_size () =
  let f_x = Field.v "x" uint8 in
  let outer =
    Codec.v "ShadowSize"
      (fun x _ d -> (x, d))
      Codec.
        [
          (f_x $ fun (x, _) -> x);
          (Field.v "s" (codec shadow_inner) $ fun _ -> (UInt8.v 5, UInt8.v 0));
          Field.v "data" (byte_array ~size:(Field.ref f_x)) $ snd;
        ]
  in
  (* the codec's own x is 2, the sub-codec's x is 5 *)
  let s =
    let module T = Private.Types in
    T.struct_ "ShadowSize"
      [
        T.field "x" uint8;
        T.field "s" (codec shadow_inner);
        T.field "data" (byte_array ~size:(T.ref "x"));
      ]
  in
  check_validates_own_x outer s ~own:"\x02\x05\x00ab" ~sub:"\x05\x02\x00ab";
  match Codec.decode outer (Bytes.of_string "\x02\x05\x00ab") 0 with
  | Ok (_, d) -> Alcotest.(check string) "sized by own x" "ab" d
  | Error e -> Alcotest.failf "size read the sub-codec's x: %a" pp_parse_error e

let test_own_field_shadows_readers () =
  let outer =
    Codec.v "ShadowReaders"
      (fun s x -> (s, x))
      Codec.[ Field.v "s" (codec shadow_inner) $ fst; Field.v "x" uint8 $ snd ]
  in
  match List.assoc_opt "x" (Codec.field_readers outer) with
  | None -> Alcotest.fail "no reader for x"
  | Some read ->
      Alcotest.(check int) "own x" 7 (read (Bytes.of_string "\x01\x02\x07") 0)

(* [Codec.field_readers] lists each visible name once, and none of the entries
   the scope keeps for the fields it shadows. *)
let test_field_readers_hide_shadowed () =
  let outer =
    Codec.v "ShadowNames"
      (fun s x -> (s, x))
      Codec.[ Field.v "s" (codec shadow_inner) $ fst; Field.v "x" uint8 $ snd ]
  in
  let names = List.sort compare (List.map fst (Codec.field_readers outer)) in
  Alcotest.(check (list string)) "names" [ "s"; "x"; "y" ] names

(* A shadowed field has no name of its own, so no field name is reserved: a
   field called [.x] is listed, and a size naming it reads it, beside a
   sub-codec's shadowed [x]. *)
let test_shadowed_field_takes_no_name () =
  let f_dot = Field.v ".x" uint8 in
  let outer =
    Codec.v "ShadowDot"
      (fun _ _ _ d -> d)
      Codec.
        [
          (Field.v "s" (codec shadow_inner) $ fun _ -> (UInt8.v 1, UInt8.v 0));
          (Field.v "x" uint8 $ fun _ -> UInt8.v 7);
          (f_dot $ fun d -> UInt8.v (String.length d));
          Field.v "data" (byte_array ~size:(Field.ref f_dot)) $ Fun.id;
        ]
  in
  let names = List.sort compare (List.map fst (Codec.field_readers outer)) in
  Alcotest.(check (list string)) "names" [ ".x"; "data"; "s"; "x"; "y" ] names;
  match Codec.decode outer (Bytes.of_string "\x01\x00\x07\x02ab") 0 with
  | Ok d -> Alcotest.(check string) "sized by .x" "ab" d
  | Error e -> Alcotest.failf "size read another field: %a" pp_parse_error e

(* The public [parse_error] / [eof] constructors build the record with the
   documented defaults, for a caller synthesizing an error outside a codec run. *)
let test_error_constructors () =
  let e = parse_error (Invalid_tag 7) in
  Alcotest.(check int) "parse_error default at" 0 e.at;
  Alcotest.(check (list string)) "parse_error default field" [] e.field;
  (match e.kind with
  | Invalid_tag 7 -> ()
  | _ -> Alcotest.fail "parse_error kind");
  let f = eof ~at:3 ~expected:4 ~got:2 () in
  Alcotest.(check int) "eof at" 3 f.at;
  match f.kind with
  | Unexpected_eof { expected = 4; got = 2 } -> ()
  | _ -> Alcotest.fail "eof kind"

(* A span sized by an expression can come out negative from the header. The
   record's frame still fits, since the next span takes the bytes back, so the
   span's own check is what refuses it, in validate as in decode. *)
let test_validate_negative_span () =
  let n = Field.v "n" uint8 in
  let c =
    Codec.v "NegSpan"
      (fun n b rest -> (n, b, rest))
      Codec.
        [
          (n $ fun (n, _, _) -> n);
          ( Field.v "b" (byte_slice ~size:Expr.(Field.ref n - int 4))
          $ fun (_, b, _) -> b );
          (Field.v "rest" all_bytes $ fun (_, _, r) -> r);
        ]
  in
  let buf = Bytes.make 16 '\x00' in
  Bytes.set_uint8 buf 0 1;
  let decoded =
    match Codec.decode c buf 0 with Ok _ -> "ok" | Error _ -> "refused"
  in
  let validated =
    match Codec.validate c buf 0 with
    | () -> "ok"
    | exception Wire.Parse_error _ -> "refused"
  in
  Alcotest.(check string) "decode" "refused" decoded;
  Alcotest.(check string) "validate agrees" decoded validated

(* A gated optional has no size of its own: with the gate on and one byte
   left before a field that takes the rest, both entry points refuse the
   record. *)
let test_validate_gated_optional () =
  let gate = Field.v "gate" uint8 in
  let c =
    Codec.v "Gated"
      (fun g p rest -> (g, p, rest))
      Codec.
        [
          (gate $ fun (g, _, _) -> g);
          ( Field.optional "payload"
              ~present:Expr.(Field.ref gate <> int 0)
              uint16be
          $ fun (_, p, _) -> p );
          (Field.v "rest" all_bytes $ fun (_, _, r) -> r);
        ]
  in
  let buf = Bytes.of_string "\x01\x00" in
  (match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "decode accepted a missing payload"
  | Error _ -> ());
  match Codec.validate c buf 0 with
  | () -> Alcotest.fail "validate accepted a missing payload"
  | exception Wire.Parse_error _ -> ()

(* An overlong span left in a record whose last span takes the rest: the
   record's extent still fits, the span does not, and both entry points refuse
   it with a parse error rather than an [Invalid_argument]. *)
let test_overlong_span () =
  let n = Field.v "n" uint8 in
  let c =
    Codec.v "LongSpan"
      (fun n b rest -> (n, b, rest))
      Codec.
        [
          (n $ fun (n, _, _) -> n);
          (Field.v "b" (byte_slice ~size:(Field.ref n)) $ fun (_, b, _) -> b);
          (Field.v "rest" all_bytes $ fun (_, _, r) -> r);
        ]
  in
  let buf = Bytes.make 8 '\x00' in
  Bytes.set_uint8 buf 0 12;
  (match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "decode accepted an overlong span"
  | Error _ -> ());
  match Codec.validate c buf 0 with
  | () -> Alcotest.fail "validate accepted an overlong span"
  | exception Wire.Parse_error _ -> ()

(* A field's own decode-side check (enum membership, lookup bound, a bounded
   embedded element) lives in its reader, not in a user constraint, so
   [Codec.validate] must run the validator pass for every codec or it would
   accept inputs [decode] rejects. Each case feeds bytes both reject. *)
let test_validate_matches_decode_rejections () =
  let rejects what c bytes =
    let buf = Bytes.of_string bytes in
    (match Codec.decode c buf 0 with
    | Error _ -> ()
    | Ok _ -> Alcotest.failf "%s: decode accepted a bad input" what);
    match Codec.validate c buf 0 with
    | exception Parse_error _ -> ()
    | () -> Alcotest.failf "%s: validate accepted what decode rejects" what
  in
  let enum_c =
    Codec.v "E" Fun.id
      Codec.[ Field.v "v" (enum "C" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]
  in
  rejects "unknown enum" enum_c "\xee";
  let lookup_c =
    Codec.v "L" Fun.id
      Codec.[ Field.v "v" (lookup [ "a"; "b"; "c" ] uint8) $ Fun.id ]
  in
  rejects "out-of-range lookup index" lookup_c "\x07";
  let elem =
    Codec.v "Bnd" Fun.id
      Codec.
        [
          Field.v "v" uint8 ~self_constraint:(fun r -> Expr.(r <= int 10))
          $ Fun.id;
        ]
  in
  let arr_c =
    Codec.v "Arr" Fun.id
      Codec.[ Field.v "vs" (array ~len:(int 2) (codec elem)) $ Fun.id ]
  in
  rejects "array element constraint" arr_c "\x01\x63"

(* [Codec.validate] is the gate [codec.mli] tells a caller to run on untrusted
   input before reading fields zero-copy, so a sub-codec used as a field has to
   be checked where it sits. EverParse compiles that field to a call to the
   nested struct's validator, which enforces everything the struct declares, so
   a use site that skipped those checks would accept bytes the verified C
   rejects. Each case puts the sub-codec behind a header byte, so a use site
   that validated at the wrong offset fails too. *)
type sub_codec_case =
  | Sub_codec_case : string * 'a Codec.t * string -> sub_codec_case

let sub_codec_check_cases =
  let outer name inner =
    Codec.v name
      (fun h s -> (h, s))
      Codec.[ Field.v "h" uint8 $ fst; Field.v "s" (codec inner) $ snd ]
  in
  let constraint_inner =
    Codec.v "SubConstraint" Fun.id
      Codec.
        [
          Field.v "v" uint8 ~self_constraint:(fun v -> Expr.(v >= int 10))
          $ Fun.id;
        ]
  in
  let where_inner =
    let f_a = Field.v "a" uint8 and f_b = Field.v "b" uint8 in
    Codec.v "SubWhere"
      ~where:Expr.(Field.ref f_a < Field.ref f_b)
      (fun a b -> (a, b))
      Codec.[ f_a $ fst; f_b $ snd ]
  in
  let enum_inner =
    Codec.v "SubEnum" Fun.id
      Codec.[ Field.v "e" (enum "Code" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]
  in
  let action_inner =
    let f_x = Field.v "x" uint8 in
    let f_y =
      Field.v "y" uint8
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_x < int 128) ])
    in
    Codec.v "SubAction" (fun x y -> (x, y)) Codec.[ f_x $ fst; f_y $ snd ]
  in
  let per_byte_inner =
    Codec.v "SubPerByte" Fun.id
      Codec.
        [
          Field.v "s"
            (byte_array_where ~size:(int 2) ~per_byte:(fun b ->
                 Expr.(b >= int 0x41)))
          $ Fun.id;
        ]
  in
  let variable_inner =
    let f_len =
      Field.v "len" uint8 ~self_constraint:(fun v -> Expr.(v <= int 2))
    in
    let f_data = Field.v "data" (byte_array ~size:(Field.ref f_len)) in
    Codec.v "SubVariable"
      (fun len data -> (len, data))
      Codec.[ f_len $ fst; f_data $ snd ]
  in
  let codec_in_codec =
    Codec.v "SubOfSub" Fun.id
      Codec.[ Field.v "i" (codec constraint_inner) $ Fun.id ]
  in
  [
    Sub_codec_case
      ("field constraint", outer "OuterConstraint" constraint_inner, "\000\001");
    Sub_codec_case
      ("codec where", outer "OuterWhere" where_inner, "\000\002\001");
    Sub_codec_case ("enum membership", outer "OuterEnum" enum_inner, "\000\009");
    Sub_codec_case
      ("field action", outer "OuterAction" action_inner, "\000\200\000");
    Sub_codec_case
      ("per-byte span", outer "OuterPerByte" per_byte_inner, "\000\000\000");
    Sub_codec_case
      ( "variable inner",
        outer "OuterVariable" variable_inner,
        "\000\009aaaaaaaaa" );
    Sub_codec_case
      ("codec in codec", outer "OuterOfSub" codec_in_codec, "\000\001");
  ]

let test_validate_runs_sub_codec_checks () =
  List.iter
    (fun (Sub_codec_case (what, c, bytes)) ->
      let buf = Bytes.of_string bytes in
      let decoded =
        match Codec.decode c buf 0 with
        | Ok _ -> Alcotest.failf "%s: decode accepted a bad sub-codec" what
        | Error e -> e
      in
      match Codec.validate c buf 0 with
      | () ->
          Alcotest.failf "%s: validate accepted what decode rejects (%a)" what
            pp_parse_error decoded
      | exception Parse_error e ->
          if not (equal_error_kind e.kind decoded.kind) then
            Alcotest.failf "%s: validate rejected for another reason (%a vs %a)"
              what pp_parse_error e pp_parse_error decoded)
    sub_codec_check_cases

(* A [Wire.where] cond carried in a field's typ ([where (len < 2) uint8]) must be
   enforced by both decode and validate, not only projected to 3D. The cond
   reaches the EverParse refinement, so leaving it unchecked on the OCaml side
   would accept inputs the verified C validator rejects. *)
let typ_where_codec =
  let open Codec in
  let f_len = Field.v "len" uint8 in
  let f_d = Field.v "d" (where Expr.(Field.ref f_len < int 2) uint8) in
  v "TypWhere" (fun len d -> (len, d)) [ f_len $ fst; f_d $ snd ]

let test_decode_enforces_typ_where () =
  (match Codec.decode typ_where_codec (Bytes.of_string "\006\000") 0 with
  | Error { kind = Constraint_failed _; _ } -> ()
  | Ok _ -> Alcotest.fail "decode accepted a Wire.where violation"
  | Error _ -> Alcotest.fail "decode failed with the wrong error");
  match Codec.decode typ_where_codec (Bytes.of_string "\001\000") 0 with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "decode rejected a valid input"

let test_validate_enforces_typ_where () =
  (match Codec.validate typ_where_codec (Bytes.of_string "\006\000") 0 with
  | () -> Alcotest.fail "validate accepted a Wire.where violation"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ());
  Codec.validate typ_where_codec (Bytes.of_string "\001\000") 0

(* decode runs field actions; validate must run the same ones, or the two paths
   disagree on a codec whose action can fail (here [return_bool (x < 128)]). *)
let action_validate_codec =
  let open Codec in
  let f_x = Field.v "x" uint8 in
  let f_y =
    Field.v "y" uint8
      ~action:
        (Action.on_success
           [ Action.return_bool Expr.(Field.ref f_x < int 128) ])
  in
  v "ActValidate" (fun x y -> (x, y)) [ f_x $ fst; f_y $ snd ]

let test_validate_runs_field_action () =
  (match Codec.decode action_validate_codec (Bytes.of_string "\200\000") 0 with
  | Error { kind = Constraint_failed _; _ } -> ()
  | _ -> Alcotest.fail "decode did not reject the action violation");
  (match
     Codec.validate action_validate_codec (Bytes.of_string "\200\000") 0
   with
  | () -> Alcotest.fail "validate skipped the field action decode enforces"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ());
  Codec.validate action_validate_codec (Bytes.of_string "\100\000") 0

(* A [Wire.where] inside a container element has no 3D projection (EverParse
   rejects the emitted refined element), so it is rejected at construction rather
   than shipping a codec whose [.3d] does not compile and whose OCaml decode
   silently ignores the constraint. A top-level field [where] stays valid. *)
let test_reject_nested_where () =
  let reject what make =
    match make () with
    | _ ->
        Alcotest.failf "%s: expected construction to reject a nested where" what
    | exception Invalid_argument _ -> ()
  in
  reject "array element" (fun () ->
      let limit = Field.v "limit" uint8 in
      Codec.v "ArrW"
        (fun l v -> (l, v))
        [
          Codec.(limit $ fst);
          Codec.(
            Field.v "v"
              (array ~len:(int 2) (where Expr.(Field.ref limit = int 1) uint8))
            $ snd);
        ]);
  reject "optional inner" (fun () ->
      let limit = Field.v "limit" uint8 in
      Codec.v "OptW"
        (fun l v -> (l, v))
        [
          Codec.(limit $ fst);
          Codec.(
            Field.optional "o"
              ~present:Expr.(Field.ref limit <> int 0)
              (where Expr.(Field.ref limit < int 9) uint8)
            $ snd);
        ]);
  reject "casetype case body" (fun () ->
      Codec.v "CtW"
        (fun t v -> (t, v))
        [
          Codec.(Field.v "tag" uint8 $ fst);
          Codec.(
            Field.v "body"
              (casetype "BodyW" uint8
                 [
                   case ~index:(UInt8.v 1)
                     (where Expr.(int 1 = int 1) uint8)
                     ~inject:(fun s -> s)
                     ~project:Option.some;
                 ])
            $ snd);
        ]);
  (* A top-level field where is still accepted (it projects and is enforced). *)
  ignore
    (Codec.v "TopW"
       (fun g -> g)
       [ Codec.(Field.v "g" (where Expr.(int 1 = int 1) uint8) $ fun g -> g) ])

let test_struct_of_codec_metadata () =
  let output = render_3d projection_codec in
  (* The struct-level [where] referencing the field [x] is lowered onto the
     field as a [{ ... }] constraint -- 3D's [where] only sees params. *)
  Alcotest.(check bool)
    "contains lowered where expression" true
    (contains ~sub:"x <= limit" output);
  Alcotest.(check bool)
    "contains on-success" true
    (contains ~sub:":on-success" output);
  (* Params should be recovered from Param_ref/Assign in the AST *)
  Alcotest.(check bool)
    "contains limit param" true
    (contains ~sub:"limit" output);
  Alcotest.(check bool)
    "contains mutable outx param" true
    (contains ~sub:"mutable" output)

(* Record with multiple uint16be fields --
   [multi_record] / [multi_record_codec] live in {!Test_helpers}. *)

let test_record_with_multi () =
  let original = { x = UInt16.v 0x1234; y = UInt16.v 0x5678 } in
  match encode_record multi_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int)
            "x" (UInt16.to_int original.x) (UInt16.to_int decoded.x);
          Alcotest.(check int)
            "y" (UInt16.to_int original.y) (UInt16.to_int decoded.y)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Record with byte_array field *)
type ba_record = { id : UInt32.t; uuid : string; tag : UInt16.t }

let ba_record_codec =
  let open Codec in
  v "BaRecord"
    (fun id uuid tag -> { id; uuid; tag })
    [
      (Field.v "id" uint32be $ fun r -> r.id);
      (Field.v "uuid" (byte_array ~size:(int 16)) $ fun r -> r.uuid);
      (Field.v "tag" uint16be $ fun r -> r.tag);
    ]

let test_record_byte_array_roundtrip () =
  let original =
    {
      id = UInt32.of_int 0x12345678;
      uuid = "0123456789abcdef";
      tag = UInt16.v 0xABCD;
    }
  in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int)
            "id"
            (UInt32.to_int original.id)
            (UInt32.to_int decoded.id);
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int)
            "tag"
            (UInt16.to_int original.tag)
            (UInt16.to_int decoded.tag)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_trailing_zeros () =
  (* A caller wanting "short" in a 16-byte span supplies the zeros itself. *)
  let original =
    {
      id = UInt32.of_int 1;
      uuid = "short" ^ String.make 11 '\x00';
      tag = UInt16.v 2;
    }
  in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      (* Verify the trailing zeros survive: bytes 9..19 should be zero *)
      for i = 9 to 19 do
        Alcotest.(check int)
          (Fmt.str "padding byte %d" i)
          0
          (Char.code encoded.[i])
      done;
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          (* Decoded uuid includes the trailing zeros *)
          Alcotest.(check int) "uuid length" 16 (String.length decoded.uuid);
          Alcotest.(check string)
            "uuid prefix" "short"
            (String.sub decoded.uuid 0 5)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let codec_seq_array : ('a, 'a array) seq_map =
  Seq_map
    {
      empty = [];
      add = (fun acc value -> value :: acc);
      finish = (fun values -> Array.of_list (List.rev values));
      iter = Array.iter;
    }

let expect_codec_array_cardinality label codec value expected actual =
  let buf = Bytes.make 8 '\x7f' in
  match Codec.encode codec value buf 0 with
  | () -> Alcotest.failf "%s: expected an array cardinality error" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": expected count")
        true
        (contains ~sub:(Fmt.str "expected %d" expected) msg);
      Alcotest.(check bool)
        (label ^ ": actual count") true
        (contains ~sub:(Fmt.str "got %d" actual) msg);
      Alcotest.(check string)
        (label ^ ": buffer unchanged")
        (String.make 8 '\x7f') (Bytes.to_string buf)

let test_codec_array_cardinality () =
  let list =
    Codec.v "ArrayList" Fun.id
      Codec.[ Field.v "values" (array ~len:(int 3) uint8) $ Fun.id ]
  in
  expect_codec_array_cardinality "short list" list [ UInt8.v 1; UInt8.v 2 ] 3 2;
  expect_codec_array_cardinality "long list" list
    [ UInt8.v 1; UInt8.v 2; UInt8.v 3; UInt8.v 4 ]
    3 4;
  let custom =
    Codec.v "ArrayCustom" Fun.id
      Codec.
        [
          Field.v "values" (array_seq codec_seq_array ~len:(int 3) uint8)
          $ Fun.id;
        ]
  in
  expect_codec_array_cardinality "short custom sequence" custom
    [| UInt8.v 1; UInt8.v 2 |]
    3 2;
  expect_codec_array_cardinality "long custom sequence" custom
    [| UInt8.v 1; UInt8.v 2; UInt8.v 3; UInt8.v 4 |]
    3 4

(* A bounded zero-terminated string occupies its whole region whatever the
   terminator's position, so a buffer sized from the codec's own minimum has to
   hold it. It reported zero, because the field had no reader on the fixed-size
   compile path and so compiled as variable-size; the terminator check has to
   survive the move. And a [zeroterm] field's terminator is found by the size
   walk, which named no field until now. *)
let test_zeroterm_extent_and_attribution () =
  let bounded =
    Codec.v "Ztam" Fun.id
      Codec.[ Field.v "s" (zeroterm_at_most ~size:(int 8)) $ Fun.id ]
  in
  Alcotest.(check int)
    "the region is the minimum size" 8
    (Codec.min_wire_size bounded);
  (match Codec.decode bounded (Bytes.of_string "abcdefgh") 0 with
  | Ok _ -> Alcotest.fail "expected a missing terminator"
  | Error { kind = Missing_terminator; _ } -> ()
  | Error _ -> Alcotest.fail "wrong error for an unterminated region");
  (match Codec.decode bounded (Bytes.of_string "abc\x00defg") 0 with
  | Ok v -> Alcotest.(check string) "reads up to the NUL" "abc" v
  | Error _ -> Alcotest.fail "expected the terminated region to decode");
  let greedy = Codec.v "Zt" Fun.id Codec.[ Field.v "s" zeroterm $ Fun.id ] in
  match Codec.decode greedy (Bytes.of_string "ab") 0 with
  | Ok _ -> Alcotest.fail "expected a missing terminator"
  | Error { kind = Missing_terminator; field; _ } ->
      Alcotest.(check (list string)) "names the field" [ "s" ] field
  | Error _ -> Alcotest.fail "wrong error for an unterminated string"

(* [expected] is documented as a count of bytes, and every consumer does
   arithmetic on it: the streaming reader's retry target and the corpus seeder's
   growth target both read it that way. The overrunning field is the one blamed,
   so the count is the whole span it asked for and [got] is what the buffer
   holds from where that span starts. *)
let test_eof_expected_is_a_byte_count () =
  let f_len = Field.v "len" uint8 in
  let c =
    Codec.v "Greedy"
      (fun l s t -> (l, s, t))
      Codec.
        [
          (f_len $ fun (l, _, _) -> l);
          ( Field.v "span" (byte_array ~size:(Field.ref f_len))
          $ fun (_, s, _) -> s );
          (Field.v "tail" all_zeros $ fun (_, _, t) -> t);
        ]
  in
  (* [len] is 20, so the span at offset 1 asks for 20 bytes with 1 left. *)
  let buf = Bytes.of_string "\x14\x00" in
  match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "expected the overrun to be refused"
  | Error { kind = Unexpected_eof { expected; got }; at; _ } ->
      Alcotest.(check bool)
        (Fmt.str "expected is a byte count, not %d" expected)
        true (expected >= 0);
      (* [got] is what remains from where the read starts, so a consumer sizing
         a new buffer takes [at + expected] and lands past the current end. *)
      Alcotest.(check int)
        "got is what remains from the read" got
        (Bytes.length buf - at);
      Alcotest.(check bool)
        "the growth target passes the current end" true
        (at + expected > Bytes.length buf)
  | Error _ -> Alcotest.fail "expected an end-of-input refusal"

(* A value too wide for the native int is refused by the conversion, which is
   handed the value and nothing else and so reports byte 0. The reader knows
   where it found it, and a caller that seeks to the reported offset has to land
   on the field rather than on the start of the buffer, whatever base the frame
   sits at. *)
let test_out_of_range_offset_follows_the_frame () =
  let c =
    Codec.v "Wide" Fun.id
      Codec.[ Field.v "k" (enum "K" [ ("A", 1) ] uint64) $ Fun.id ]
  in
  let at_of base b =
    match Codec.decode c b base with
    | Ok _ -> Alcotest.fail "expected the wide value to be refused"
    | Error e -> e.at
  in
  let wide = Bytes.make 8 '\xff' in
  Alcotest.(check int) "at base 0" 0 (at_of 0 wide);
  Alcotest.(check int)
    "at base 8" 8
    (at_of 8 (Bytes.cat (Bytes.make 8 '\x00') wide))

(* A field whose byte extent is an input parameter cannot be measured from the
   value alone. Reporting 0 for it is worse than refusing: a caller sizes a
   buffer from the answer, and encode then writes past the allocation, which
   surfaced as a raw out-of-bounds rather than as a wire error. *)
let test_size_of_value_resolves_param_sizes () =
  let p = Param.input "n" uint8 in
  let c =
    Codec.v "Nest" Fun.id
      Codec.[ Field.v "body" (nested ~size:(Param.expr p) uint16be) $ Fun.id ]
  in
  (* Reporting 0 here is what let a caller allocate a buffer encode then ran
     off the end of, so the answer has to be refused, not guessed. *)
  (match Codec.size_of_value c (UInt16.v 7) with
  | n -> Alcotest.failf "expected a refusal without ?env, got %d" n
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "the refusal names the remedy" true (contains ~sub:"?env" msg));
  (* And the record really is the width the parameter names. *)
  let env = Param.bind_by_name "n" 2 (Codec.env c) in
  let buf = Bytes.create 2 in
  Codec.encode ~env c (UInt16.v 7) buf 0

(* A field whose offset sits behind a param-sized one cannot be located without
   the parameter. An unbound one reads 0, which puts the accessor on the wrong
   bytes: [get] answers with the padding it should have skipped, and [set]
   writes there, corrupting a field the caller never named while leaving the one
   it did name untouched. [decode], [validate] and [wire_size_at] already refuse
   without an env; the accessors must not answer where those refuse. *)
let param_offset_codec () =
  let p = Param.input "n" uint8 in
  let f_pad = Codec.(Field.v "pad" (byte_array ~size:(Param.expr p)) $ fst) in
  let f_v = Codec.(Field.v "v" uint16be $ snd) in
  let c = Codec.v "ParamOffset" (fun pad v -> (pad, v)) [ f_pad; f_v ] in
  (c, f_v)

let test_get_needs_the_param_to_locate_the_field () =
  let c, f_v = param_offset_codec () in
  let buf = Bytes.of_string "AAAA\x12\x34" in
  let env = Param.bind_by_name "n" 4 (Codec.env c) in
  let read = Staged.unstage (Codec.get ~env c f_v) in
  Alcotest.(check int)
    "with ?env the field is where the param puts it" 0x1234
    (UInt16.to_int (read buf 0));
  (* Without it the reader must not answer with the padding at offset 0. *)
  match Codec.get c f_v with
  | _ -> Alcotest.fail "expected get to refuse a param layout without ?env"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "the refusal names the remedy" true (contains ~sub:"?env" msg)

let test_set_never_writes_over_an_unnamed_field () =
  let c, f_v = param_offset_codec () in
  let buf = Bytes.of_string "AAAA\x12\x34" in
  let env = Param.bind_by_name "n" 4 (Codec.env c) in
  let write = Staged.unstage (Codec.set ~env c f_v) in
  write buf 0 (UInt16.v 0xbeef);
  Alcotest.(check string)
    "with ?env only the named field moves" "AAAA\xbe\xef" (Bytes.to_string buf);
  Bytes.blit_string "AAAA\x12\x34" 0 buf 0 6;
  (* Without it the write would land on the padding and leave "v" as it was, so
     the record a caller reads back differs from the one it asked to change. *)
  match Codec.set c f_v with
  | _ -> Alcotest.fail "expected set to refuse a param layout without ?env"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "the refusal names the remedy" true (contains ~sub:"?env" msg);
      Alcotest.(check string)
        "the buffer is untouched" "AAAA\x12\x34" (Bytes.to_string buf)

(* A literal byte budget is the region size the description declares, and since
   1.2.0 encode holds the values to it too. A caller who does not know that size
   where the codec is built has nothing truthful to put there, and 3D has no
   rest-of-region list to offer as a sizeless alternative, so the refusal has to
   name the one thing that does work. *)
let test_repeat_budget_mismatch_names_the_remedy () =
  let c =
    Codec.v "Region" Fun.id
      Codec.[ Field.repeat "items" ~size:(int 0) uint16be $ Fun.id ]
  in
  match Codec.size_of_value c [ UInt16.v 1; UInt16.v 2 ] with
  | _ -> Alcotest.fail "expected the budget mismatch to be refused"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "names the budget and what the values came to" true
        (contains ~sub:"byte budget is 0" msg
        && contains ~sub:"elements span 4" msg);
      Alcotest.(check bool)
        "points at the parameter, not a literal" true
        (contains ~sub:"Param.input" msg)

(* -- Codec.rename -- *)

type rename_rec = { ra : UInt8.t; rb : UInt16.t }

let rename_codec =
  Codec.v "OrigName"
    (fun ra rb -> { ra; rb })
    Codec.
      [
        (Field.v "ra" uint8 $ fun r -> r.ra);
        (Field.v "rb" uint16be $ fun r -> r.rb);
      ]

let test_rename_projection () =
  let renamed = Codec.rename "NewName" rename_codec in
  let r2 = render_3d renamed in
  Alcotest.(check bool)
    "new name in projection" true
    (contains ~sub:"NewName" r2);
  Alcotest.(check bool) "old name absent" false (contains ~sub:"OrigName" r2);
  (* Renaming only substitutes the struct name: putting it back recovers the
     original projection byte for byte, so fields, layout, and any field
     constraints are untouched. *)
  let back =
    Re.replace_string (Re.compile (Re.str "NewName")) ~by:"OrigName" r2
  in
  Alcotest.(check string)
    "rename is a pure name substitution" (render_3d rename_codec) back

let test_rename_roundtrip () =
  let renamed = Codec.rename "NewName" rename_codec in
  let v = { ra = UInt8.v 7; rb = UInt16.v 1000 } in
  match (encode_record rename_codec v, encode_record renamed v) with
  | Ok b1, Ok b2 -> (
      Alcotest.(check string) "encode unchanged by rename" b1 b2;
      match decode_record renamed b2 with
      | Ok d -> Alcotest.(check bool) "decode unchanged by rename" true (d = v)
      | Error _ -> Alcotest.fail "decode failed after rename")
  | _ -> Alcotest.fail "encode failed"

let test_enum_codec_validates () =
  (* The Codec decode path rejects an enum value that is not one of the named
     cases, on a scalar field and on every array element, matching the EverParse
     validator. *)
  let e = enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ] uint8 in
  let cs =
    Codec.v "EColor" (fun v -> v) Codec.[ (Field.v "v" e $ fun v -> v) ]
  in
  let ca =
    Codec.v "EArr"
      (fun v -> v)
      Codec.[ (Field.v "v" (array ~len:(int 4) e) $ fun v -> v) ]
  in
  let ok c s =
    match Codec.decode c (Bytes.of_string s) 0 with
    | Ok _ -> true
    | Error _ -> false
  in
  Alcotest.(check bool) "scalar known value accepted" true (ok cs "\002");
  Alcotest.(check bool) "scalar unknown value rejected" false (ok cs "\238");
  Alcotest.(check bool)
    "array known values accepted" true (ok ca "\001\002\003\001");
  Alcotest.(check bool)
    "array unknown element rejected" false (ok ca "\238\220\220\187")

(* Decode diagnostics: end of input is reported only for a read that actually
   ran off the end of the buffer. A misuse in a size expression keeps its own
   error, and a byte span the data sizes below zero stays inside the result
   type. *)

let f_diag_len = Field.v "len" uint8

let field_pos_size_codec =
  Codec.v "FieldPosSize"
    (fun _len body -> body)
    Codec.
      [
        (f_diag_len $ fun x -> UInt8.v (String.length x));
        Field.v "body"
          (byte_array ~size:Expr.(Field.ref f_diag_len + field_pos))
        $ Fun.id;
      ]

let test_size_misuse_reports_itself () =
  let check name decode =
    match decode () with
    | Ok _ -> Alcotest.failf "%s: decoded a value" name
    | Error e ->
        Alcotest.failf "%s: misreported as a parse error: %a" name
          pp_parse_error e
    | exception Invalid_argument msg ->
        if not (contains ~sub:"[field_pos] only valid inside an action" msg)
        then Alcotest.failf "%s: unexpected message %S" name msg
  in
  check "Codec.decode" (fun () ->
      Codec.decode field_pos_size_codec (Bytes.of_string "\001ab") 0);
  check "of_string" (fun () -> of_string (codec field_pos_size_codec) "\001ab")

let dep_size_codec =
  Codec.v "DepSizeTrunc"
    (fun _len body -> body)
    Codec.
      [
        (f_diag_len $ fun x -> UInt8.v (String.length x));
        Field.v "body" (byte_array ~size:(Field.ref f_diag_len)) $ Fun.id;
      ]

(* The length field of the second span sits past the first span, so a short
   buffer cannot even reach it: computing the record's size is the read that
   runs off the end. *)
let two_span_codec =
  let f_n = Field.v "n" uint8 in
  Codec.v "TwoSpanTrunc"
    (fun _len a n b -> (a, n, b))
    Codec.
      [
        (f_diag_len $ fun (a, _, _) -> UInt8.v (String.length a));
        ( Field.v "a" (byte_array ~size:(Field.ref f_diag_len))
        $ fun (a, _, _) -> a );
        (f_n $ fun (_, n, _) -> n);
        (Field.v "b" (byte_array ~size:(Field.ref f_n)) $ fun (_, _, b) -> b);
      ]

let expect_eof name = function
  | Ok _ -> Alcotest.failf "%s: decoded a truncated buffer" name
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e ->
      Alcotest.failf "%s: expected an eof, got %a" name pp_parse_error e

let test_truncated_still_reports_eof () =
  expect_eof "Codec.decode"
    (Codec.decode dep_size_codec (Bytes.of_string "\010ab") 0);
  expect_eof "of_string" (of_string (codec dep_size_codec) "\010ab");
  expect_eof "Codec.decode two-span"
    (Codec.decode two_span_codec (Bytes.of_string "\010X") 0);
  expect_eof "of_string two-span" (of_string (codec two_span_codec) "\010X")

(* A sub-codec sized from a length field it carries. The field is two bytes
   wide, so a region of one byte cannot hold it. *)
let region_len_codec =
  let f_len = Field.v "len" uint16be in
  Codec.v "RegionLen"
    (fun _len body -> body)
    Codec.
      [
        (f_len $ fun body -> UInt16.v (String.length body));
        Field.v "body" (byte_array ~size:(Field.ref f_len)) $ Fun.id;
      ]

(* A [nested] region declares how many bytes of the buffer the value may
   occupy; the bytes past it belong to whatever follows, or to nothing at all.
   Sizing the sub-codec from those bytes makes the [expected] count of the
   resulting end-of-input a number no byte of the declared region carried, and
   a caller that sizes its next read from [expected] is then reading on the
   word of memory outside the region. Pin both halves of that: the reported
   shortfall is the length field's own extent, and it does not move when the
   bytes past the region do. *)
let test_nested_region_eof_stays_in_region () =
  let region = 1 in
  let error_for filler =
    let buf = Bytes.make 64 filler in
    Bytes.set_uint8 buf 0 0x00;
    match of_bytes (nested ~size:(int region) (codec region_len_codec)) buf with
    | Ok _ -> Alcotest.fail "decoded a region too short for its length field"
    | Error e -> e
  in
  let e = error_for '\x99' in
  (match e.kind with
  | Unexpected_eof { expected; got } ->
      Alcotest.(check int) "bytes the length field needed" 2 expected;
      Alcotest.(check int) "bytes the region had" region got
  | k -> Alcotest.failf "expected an eof, got %a" pp_error_kind k);
  Alcotest.(check string)
    "the shortfall ignores the bytes past the region"
    (Fmt.str "%a" pp_parse_error e)
    (Fmt.str "%a" pp_parse_error (error_for '\x01'))

(* Demanding the codec's mandatory extent from the region closes off a length
   field the codec always carries, but [two_span_codec]'s second one sits
   behind a span the first one sizes, so where it falls is data. A region as
   long as the mandatory extent still stops short of it, and the size walk
   reads it anyway. Cutting the buffer at the region has to give the answer a
   buffer that really ends there gives: any other answer was resolved from
   bytes the parse was never handed. *)
let test_dynamic_length_field_stays_in_region () =
  let region = 2 in
  let error_in buf =
    Bytes.set_uint8 buf 0 200;
    match of_bytes (nested ~size:(int region) (codec two_span_codec)) buf with
    | Ok _ ->
        Alcotest.fail "decoded a region that cannot reach its length field"
    | Error e -> Fmt.str "%a" pp_parse_error e
  in
  let past_the_region filler = error_in (Bytes.make 512 filler) in
  Alcotest.(check string)
    "a region ends the buffer as far as the walk is concerned"
    (error_in (Bytes.make region '\x00'))
    (past_the_region '\x99');
  Alcotest.(check string)
    "the shortfall ignores the bytes past the region" (past_the_region '\x01')
    (past_the_region '\xff')

(* [Codec.validate] scans a refined span where it lies rather than copying it,
   so the scan has to refuse everything the reader refuses. A literal width
   below zero is refused before any scan, and by the reader, so both entry
   points have to name the same fault: a validate that reported end-of-input
   here while decode reported the negative width would be a validator that
   rejects for a different reason than the decoder it stands in for. *)
let test_negative_span_width_agrees () =
  let c =
    Codec.v "NegWidth" Fun.id
      Codec.
        [
          Field.v "d"
            (byte_array_where ~size:(int (-1)) ~per_byte:printable_byte)
          $ Fun.id;
        ]
  in
  let buf = Bytes.make 8 'a' in
  let kind f =
    match f () with
    | () -> Alcotest.failf "accepted a span of width -1"
    | exception Wire.Parse_error e -> Fmt.str "%a" pp_error_kind e.kind
  in
  let validated = kind (fun () -> Codec.validate c buf 0) in
  let decoded =
    kind (fun () ->
        match Codec.decode c buf 0 with
        | Ok _ -> ()
        | Error e -> raise (Wire.Parse_error e))
  in
  Alcotest.(check string)
    "validate rejects a negative span width for decode's reason" decoded
    validated

let test_negative_span_is_parse_error () =
  let neg_codec =
    Codec.v "NegSpan" Fun.id
      Codec.[ Field.v "b" (byte_array ~size:(int (-1))) $ Fun.id ]
  in
  let expect name = function
    | Ok _ -> Alcotest.failf "%s: accepted a negative span" name
    | Error { kind = Value_out_of_range _; _ } -> ()
    | Error e ->
        Alcotest.failf "%s: expected Value_out_of_range, got %a" name
          pp_parse_error e
  in
  expect "Codec.decode" (Codec.decode neg_codec (Bytes.of_string "abc") 0);
  expect "of_string codec" (of_string (codec neg_codec) "abc");
  expect "of_string typ" (of_string (byte_array ~size:(int (-1))) "abc");
  expect "of_bytes typ"
    (of_bytes (byte_array ~size:(int (-1))) (Bytes.of_string "abc"))

let suite =
  ( "codec",
    [
      Alcotest.test_case "rename: projection name" `Quick test_rename_projection;
      Alcotest.test_case "rename: roundtrip unchanged" `Quick
        test_rename_roundtrip;
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
      Alcotest.test_case "decode: prefix and all consumption" `Quick
        test_codec_decode_consumption_modes;
      Alcotest.test_case "record: roundtrip" `Quick test_record_roundtrip;
      Alcotest.test_case "record: duplicate names rejected" `Quick
        test_duplicate_names_rejected;
      Alcotest.test_case "record: struct_of_codec" `Quick test_struct_of_record;
      Alcotest.test_case "record: metadata decode ok" `Quick
        test_codec_metadata_decode_ok;
      Alcotest.test_case "record: metadata constraint fail" `Quick
        test_metadata_constraint_fail;
      Alcotest.test_case "record: metadata action fail" `Quick
        test_metadata_action_fail;
      Alcotest.test_case "record: metadata decode with params" `Quick
        test_metadata_with_params;
      Alcotest.test_case "record: metadata where fail" `Quick
        test_metadata_where_fail;
      Alcotest.test_case "record: metadata struct_of_codec" `Quick
        test_struct_of_codec_metadata;
      Alcotest.test_case "validate: rejects bad where" `Quick
        test_validate_rejects_bad_where;
      Alcotest.test_case "validate: rejects bad constraint" `Quick
        test_validate_rejects_bad_constraint;
      Alcotest.test_case "validate: then get" `Quick test_validate_then_get;
      Alcotest.test_case "validate: bounds-checks a constraint-free codec"
        `Quick test_validate_bounds_constraint_free;
      Alcotest.test_case "validate: enforces all_zeros padding" `Quick
        test_validate_all_zeros;
      Alcotest.test_case "error: self_constraint reports the offending value"
        `Quick test_self_constraint_reports_value;
      Alcotest.test_case "error: all_zeros offset on the struct path" `Quick
        test_all_zeros_offset_on_struct_path;
      Alcotest.test_case "error: nested field path" `Quick
        test_field_path_nested;
      Alcotest.test_case "scope: own field shadows a sub-codec's in where"
        `Quick test_own_field_shadows_where;
      Alcotest.test_case "scope: own field shadows a sub-codec's in a size"
        `Quick test_own_field_shadows_size;
      Alcotest.test_case "scope: own field shadows a sub-codec's in readers"
        `Quick test_own_field_shadows_readers;
      Alcotest.test_case "scope: field_readers hides shadowed fields" `Quick
        test_field_readers_hide_shadowed;
      Alcotest.test_case "scope: a shadowed field takes no name" `Quick
        test_shadowed_field_takes_no_name;
      Alcotest.test_case "error: parse_error / eof constructors" `Quick
        test_error_constructors;
      Alcotest.test_case "validate: matches decode rejections" `Quick
        test_validate_matches_decode_rejections;
      Alcotest.test_case "validate: refuses a negative span size" `Quick
        test_validate_negative_span;
      Alcotest.test_case "decode: refuses an overlong span" `Quick
        test_overlong_span;
      Alcotest.test_case "validate: refuses a missing gated optional" `Quick
        test_validate_gated_optional;
      Alcotest.test_case "validate: runs a sub-codec's checks" `Quick
        test_validate_runs_sub_codec_checks;
      Alcotest.test_case "decode: enforces typ-level where" `Quick
        test_decode_enforces_typ_where;
      Alcotest.test_case "validate: enforces typ-level where" `Quick
        test_validate_enforces_typ_where;
      Alcotest.test_case "validate: runs field action" `Quick
        test_validate_runs_field_action;
      Alcotest.test_case "where: nested in container rejected" `Quick
        test_reject_nested_where;
      Alcotest.test_case "record: with_multi" `Quick test_record_with_multi;
      Alcotest.test_case "record: byte_array roundtrip" `Quick
        test_record_byte_array_roundtrip;
      Alcotest.test_case "record: byte_array trailing zeros" `Quick
        test_record_byte_array_trailing_zeros;
      Alcotest.test_case "array: encode cardinality" `Quick
        test_codec_array_cardinality;
      Alcotest.test_case "zeroterm: extent and field attribution" `Quick
        test_zeroterm_extent_and_attribution;
      Alcotest.test_case "decode: eof expected is a byte count" `Quick
        test_eof_expected_is_a_byte_count;
      Alcotest.test_case "decode: out-of-range offset follows the frame" `Quick
        test_out_of_range_offset_follows_the_frame;
      Alcotest.test_case "size_of_value: resolves param-driven sizes" `Quick
        test_size_of_value_resolves_param_sizes;
      Alcotest.test_case "repeat: budget mismatch names the remedy" `Quick
        test_repeat_budget_mismatch_names_the_remedy;
      Alcotest.test_case "get: param layout requires ?env" `Quick
        test_get_needs_the_param_to_locate_the_field;
      Alcotest.test_case "set: param layout requires ?env" `Quick
        test_set_never_writes_over_an_unnamed_field;
      Alcotest.test_case "enum: codec rejects unknown values" `Quick
        test_enum_codec_validates;
      Alcotest.test_case "diag: field_pos in a size expression reports itself"
        `Quick test_size_misuse_reports_itself;
      Alcotest.test_case "diag: truncated buffer still reports eof" `Quick
        test_truncated_still_reports_eof;
      Alcotest.test_case "diag: a nested region sizes only from its own bytes"
        `Quick test_nested_region_eof_stays_in_region;
      Alcotest.test_case "diag: a region bounds a length field it cannot reach"
        `Quick test_dynamic_length_field_stays_in_region;
      Alcotest.test_case "diag: negative span is a parse error" `Quick
        test_negative_span_is_parse_error;
      Alcotest.test_case "diag: negative span width agrees with decode" `Quick
        test_negative_span_width_agrees;
    ] )
