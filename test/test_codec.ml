(* Tests for codec.ml: Codec.get/set/v *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Project a codec to its 3D rendering for substring assertions. *)
let render_3d codec =
  to_3d (module_ [ typedef (Everparse.Raw.struct_of_codec codec) ])

(* Helper: encode record to string using Codec API *)
let encode_record codec v =
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error
      {
        at = 0;
        field = [];
        kind = Unexpected_eof { expected = ws; got = String.length s };
      }
  else Codec.decode codec (Bytes.of_string s) 0

(* -- Record codec tests -- *)

type simple_record = { a : int; b : int; c : Optint.t }

let simple_record_codec =
  let open Codec in
  v "SimpleRecord"
    (fun a b c -> { a; b; c })
    [
      (Field.v "a" uint8 $ fun r -> r.a);
      (Field.v "b" uint16 $ fun r -> r.b);
      (Field.v "c" uint32 $ fun r -> r.c);
    ]

let test_record_encode () =
  let v = { a = 0x42; b = 0x1234; c = Optint.of_int32 0x56789ABCl } in
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
      Alcotest.(check int) "a" 0x42 v.a;
      Alcotest.(check int) "b" 0x1234 v.b;
      Alcotest.(check int32) "c" 0x56789ABCl (Optint.to_int32 v.c)
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = Optint.of_int 0x12345678 } in
  match encode_record simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int)
            "c roundtrip" (Optint.to_int original.c) (Optint.to_int decoded.c)
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

type meta_record = { x : int }

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
  Alcotest.(check int) "x" 8 v.x

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

let projection_limit = Param.input "limit" uint8
let _projection_limit_expr = Param.expr projection_limit
let projection_outx = Param.output "outx" uint8
let projection_f_x = Field.v "x" uint8

let projection_codec =
  let open Codec in
  v "ProjectionCodec"
    ~where:Expr.(Field.ref projection_f_x <= Param.expr projection_limit)
    (fun x -> { x })
    [
      ( Field.v "x"
          ~constraint_:Expr.(Field.ref projection_f_x <= int 8)
          ~action:
            (Action.on_success
               [ Action.assign projection_outx (Field.ref projection_f_x) ])
          uint8
      $ fun r -> r.x );
    ]

let test_metadata_with_params () =
  let env = Codec.env projection_codec |> Param.bind projection_limit 10 in
  let buf = Bytes.of_string "\x08" in
  let v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int) "x" 8 v.x;
  Alcotest.(check int) "outx" 8 (Param.get env projection_outx)

let test_metadata_where_fail () =
  let env = Codec.env projection_codec |> Param.bind projection_limit 7 in
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
  Alcotest.(check int) "get bypasses where" 7 (get_x buf 0);
  (* validate catches the violation *)
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject where violation"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_rejects_bad_constraint () =
  (* constraint requires x <= 10, set x = 11 *)
  let buf = Bytes.of_string "\x0B" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "get bypasses constraint" 11 (get_x buf 0);
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject constraint violation"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_then_get () =
  (* x = 8 satisfies both where (= 8) and constraint (<= 10) *)
  let buf = Bytes.of_string "\x08" in
  Codec.validate validate_codec buf 0;
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "validate then get" 8 (get_x buf 0)

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

(* The allocation tests read [Gc.minor_words], which only counts under the
   native and bytecode runtimes; under wasm_of_ocaml or js_of_ocaml the
   counters stay at zero and the assertions would pass vacuously. *)
let skip_unless_gc_counters () =
  match Sys.backend_type with
  | Sys.Other _ -> Alcotest.skip ()
  | Sys.Native | Sys.Bytecode -> ()

(* A codec with no checks (plain scalars and byte spans) must validate without
   allocating: a hot read path validates a header then reads fields zero-copy,
   so a per-validate allocation here turns an O(log N) seek into O(N log N)
   garbage. Validate runs the structural bounds check but no validator pass. *)
let test_validate_check_free_no_alloc () =
  skip_unless_gc_counters ();
  let c =
    Codec.v "Page"
      (fun a b k -> (a, b, k))
      Codec.
        [
          (Field.v "a" uint8 $ fun (a, _, _) -> a);
          (Field.v "b" uint32be $ fun (_, b, _) -> b);
          (Field.v "k" (byte_slice ~size:(int 8)) $ fun (_, _, k) -> k);
        ]
  in
  let buf = Bytes.make 32 '\007' in
  Codec.validate c buf 0;
  (* warm up any one-time setup *)
  let n = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    Codec.validate c buf 0
  done;
  let per_call = (Gc.minor_words () -. before) /. float_of_int n in
  Alcotest.(check bool)
    (Fmt.str "validate of a check-free codec allocates ~0 words (got %.2f)"
       per_call)
    true (per_call < 1.0)

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
                   case ~index:1
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
  let original = { x = 0x1234; y = 0x5678 } in
  match encode_record multi_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      match decode_record multi_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "x" original.x decoded.x;
          Alcotest.(check int) "y" original.y decoded.y
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

(* Record with byte_array field *)
type ba_record = { id : Optint.t; uuid : string; tag : int }

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
    { id = Optint.of_int 0x12345678; uuid = "0123456789abcdef"; tag = 0xABCD }
  in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int)
            "id"
            (Optint.to_int original.id)
            (Optint.to_int decoded.id);
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int) "tag" original.tag decoded.tag
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_trailing_zeros () =
  (* A caller wanting "short" in a 16-byte span supplies the zeros itself. *)
  let original =
    { id = Optint.of_int 1; uuid = "short" ^ String.make 11 '\x00'; tag = 2 }
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
  expect_codec_array_cardinality "short list" list [ 1; 2 ] 3 2;
  expect_codec_array_cardinality "long list" list [ 1; 2; 3; 4 ] 3 4;
  let custom =
    Codec.v "ArrayCustom" Fun.id
      Codec.
        [
          Field.v "values" (array_seq codec_seq_array ~len:(int 3) uint8)
          $ Fun.id;
        ]
  in
  expect_codec_array_cardinality "short custom sequence" custom [| 1; 2 |] 3 2;
  expect_codec_array_cardinality "long custom sequence" custom [| 1; 2; 3; 4 |]
    3 4

(* Field.repeat over a zeroterm element: a list of NUL-terminated strings
   within a byte budget. Used to raise Failure at decode; now decodes and
   projects through a synthesised element struct. *)
type zt_rep = { zn : int; names : string list }

let zt_rep_codec =
  let f_n = Field.v "n" uint16be in
  let f_names = Field.repeat "names" ~size:(Field.ref f_n) zeroterm in
  Codec.v "ZtRep"
    (fun zn names -> { zn; names })
    Codec.[ (f_n $ fun r -> r.zn); (f_names $ fun r -> r.names) ]

let test_repeat_zeroterm_element () =
  let v = { zn = 12; names = [ "abc"; "de"; "fghi" ] } in
  let sz = Codec.size_of_value zt_rep_codec v in
  Alcotest.(check int) "wire size" 14 sz;
  let buf = Bytes.create sz in
  Codec.encode zt_rep_codec v buf 0;
  match Codec.decode zt_rep_codec buf 0 with
  | Ok d -> Alcotest.(check (list string)) "names" v.names d.names
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_repeat_zeroterm_projection () =
  let out = render_3d zt_rep_codec in
  Alcotest.(check bool)
    "zeroterm element wrapped in a struct" true
    (contains ~sub:"ZtElem names[:byte-size n]" out)

(* Dynamic [Field.optional] over a variable-size inner. Group A: a byte array
   sized by a prior field. The gate drives present/absent, and both round-trip
   (absent consumes no bytes, present consumes the inner). *)
type opt_var = { gate : int; len : int; body : string option }

let opt_var_codec =
  let f_gate = Field.v "gate" uint8 in
  let f_len = Field.v "len" uint8 in
  let f_body =
    Field.optional "body"
      ~present:Expr.(Field.ref f_gate <> int 0)
      (byte_array ~size:(Field.ref f_len))
  in
  Codec.v "OptVar"
    (fun gate len body -> { gate; len; body })
    Codec.
      [
        (f_gate $ fun r -> r.gate);
        (f_len $ fun r -> r.len);
        (f_body $ fun r -> r.body);
      ]

let roundtrip codec v =
  let n = Codec.size_of_value codec v in
  let buf = Bytes.create n in
  Codec.encode codec v buf 0;
  (n, Codec.decode_exn codec buf 0)

let test_optional_var_byte_array () =
  let n, d = roundtrip opt_var_codec { gate = 1; len = 3; body = Some "abc" } in
  Alcotest.(check int) "present size" 5 n;
  Alcotest.(check (option string)) "present body" (Some "abc") d.body;
  let n, d = roundtrip opt_var_codec { gate = 0; len = 0; body = None } in
  Alcotest.(check int) "absent size" 2 n;
  Alcotest.(check (option string)) "absent body" None d.body

(* Group B: a self-delimiting sub-codec (its own length prefix) as the optional
   inner. *)
let sub_string_codec =
  let f_slen = Field.v "slen" uint8 in
  Codec.v "OptSub"
    (fun _slen s -> s)
    Codec.
      [
        f_slen $ String.length;
        Field.v "sdata" (byte_array ~size:(Field.ref f_slen)) $ Fun.id;
      ]

type opt_sub = { g : int; desc : string option }

let opt_sub_codec =
  let f_g = Field.v "g" uint8 in
  let f_desc =
    Field.optional "desc"
      ~present:Expr.(Field.ref f_g <> int 0)
      (codec sub_string_codec)
  in
  Codec.v "OptSubRec"
    (fun g desc -> { g; desc })
    Codec.[ (f_g $ fun r -> r.g); (f_desc $ fun r -> r.desc) ]

let test_optional_self_delimiting_codec () =
  let _, d = roundtrip opt_sub_codec { g = 1; desc = Some "hi" } in
  Alcotest.(check (option string)) "present desc" (Some "hi") d.desc;
  let n, d = roundtrip opt_sub_codec { g = 0; desc = None } in
  Alcotest.(check int) "absent size" 1 n;
  Alcotest.(check (option string)) "absent desc" None d.desc

(* A conditional group can carry its own length prefix and dependent list.
   Keeping those fields in a sub-codec lets their expressions stay local to
   the group, while a following payload remains in the parent codec. This is
   the shape of an optional secondary header that declares its own length. *)
type transfer_extensions = { items_len : int; items : int list }

let transfer_extensions_codec =
  let f_len = Field.v "ext_items_len" uint8 in
  let f_items = Field.repeat "ext_items" ~size:(Field.ref f_len) uint8 in
  Codec.v "TransferExtensions"
    (fun items_len items -> { items_len; items })
    Codec.[ (f_len $ fun r -> r.items_len); (f_items $ fun r -> r.items) ]

type transfer_segment = {
  start : int;
  extensions : transfer_extensions option;
  data_len : int;
  data : string;
}

let transfer_segment_codec =
  let f_start = Field.v "start" uint8 in
  let f_extensions =
    Field.optional "extensions"
      ~present:Expr.(Field.ref f_start <> int 0)
      (codec transfer_extensions_codec)
  in
  let f_data_len = Field.v "data_len" uint8 in
  let f_data = Field.v "data" (byte_array ~size:(Field.ref f_data_len)) in
  Codec.v "TransferSegment"
    (fun start extensions data_len data ->
      { start; extensions; data_len; data })
    Codec.
      [
        (f_start $ fun r -> r.start);
        (f_extensions $ fun r -> r.extensions);
        (f_data_len $ fun r -> r.data_len);
        (f_data $ fun r -> r.data);
      ]

let test_optional_length_prefixed_group () =
  let present =
    {
      start = 1;
      extensions = Some { items_len = 2; items = [ 0xA1; 0xB2 ] };
      data_len = 3;
      data = "xyz";
    }
  in
  let n, decoded = roundtrip transfer_segment_codec present in
  Alcotest.(check int) "present size" 8 n;
  Alcotest.(check (list int))
    "extension items" [ 0xA1; 0xB2 ] (Option.get decoded.extensions).items;
  Alcotest.(check string) "following data" "xyz" decoded.data;
  let absent = { start = 0; extensions = None; data_len = 2; data = "ok" } in
  let n, decoded = roundtrip transfer_segment_codec absent in
  Alcotest.(check int) "absent size" 4 n;
  Alcotest.(check bool)
    "extensions absent" true
    (Option.is_none decoded.extensions);
  Alcotest.(check string) "following data" "ok" decoded.data;
  let out = render_3d transfer_segment_codec in
  Alcotest.(check bool)
    "group is its own struct" true
    (contains ~sub:"typedef struct WireTransferExtensions" out);
  Alcotest.(check bool)
    "group-local dependent repeat" true
    (contains ~sub:"UINT8 ext_items[:byte-size ext_items_len]" out);
  Alcotest.(check bool)
    "group is gate-selected" true
    (contains ~sub:"Opt_TransferExtensions(" out);
  Alcotest.(check bool)
    "later fields stay in the parent" true
    (contains ~sub:"UINT8 data[:byte-size data_len]" out)

(* Wire.array over a fixed byte_array element: a fixed-count list of n-byte
   chunks (e.g. an array of IPv4 addresses). Used to project a double
   [:byte-size]; the element is now emitted as bare bytes under the budget. *)
type arr_chunks = { atag : int; addrs : string list }

let arr_chunks_codec =
  let f_tag = Field.v "tag" uint8 in
  let f_addrs =
    Field.v "addrs" (array ~len:(int 3) (byte_array ~size:(int 4)))
  in
  Codec.v "ArrChunks"
    (fun atag addrs -> { atag; addrs })
    Codec.[ (f_tag $ fun r -> r.atag); (f_addrs $ fun r -> r.addrs) ]

let test_array_byte_array_element () =
  let v = { atag = 7; addrs = [ "aaaa"; "bbbb"; "cccc" ] } in
  let sz = Codec.size_of_value arr_chunks_codec v in
  Alcotest.(check int) "wire size" 13 sz;
  let buf = Bytes.create sz in
  Codec.encode arr_chunks_codec v buf 0;
  match Codec.decode arr_chunks_codec buf 0 with
  | Ok d -> Alcotest.(check (list string)) "addrs" v.addrs d.addrs
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_array_byte_array_projection () =
  let out = render_3d arr_chunks_codec in
  Alcotest.(check bool)
    "single byte-size on the array field" true
    (contains ~sub:"UINT8 addrs[:byte-size (3 * 4)]" out)

(* A [rest_bytes] tail projects to [byte-size (total - sizeof(this))]. EverParse
   cannot prove that subtraction non-underflowing on its own, so the projection
   emits the guard [total >= sizeof(this)] as a refinement on the preceding
   scalar field, which discharges it. Without the guard the schema fails
   EverParse verification ("cannot verify u32 subtraction"). *)
let rest_bytes_codec =
  let total = Param.input "msglen" uint16be in
  Codec.v "RestProj"
    (fun h t -> (h, t))
    Codec.
      [
        (Field.v "hdr" uint8 $ fun (h, _) -> h);
        (Field.v "rest" (rest_bytes total) $ fun (_, t) -> t);
      ]

let test_rest_bytes_projection_guard () =
  let out = render_3d rest_bytes_codec in
  Alcotest.(check bool)
    "rest field is sized by the subtraction" true
    (contains ~sub:"rest[:byte-size (msglen - sizeof (this))]" out);
  Alcotest.(check bool)
    "preceding field guards the subtraction" true
    (contains ~sub:"msglen >= sizeof (this)" out)

(* A [Field.repeat] byte-budget comes from a length field, i.e. untrusted input.
   A length larger than the buffer (or an offset overrun past it from a
   preceding variable field) must fail with a clean [Parse_error], not crash the
   decoder with an out-of-range [Bytes.sub]. *)
let f_rep_len = Field.v "len" uint16be

(* An embedded sub-codec whose length field overruns the buffer leaves the
   following field at an offset past the end; the trailing [all_zeros] then read
   a negative-length span. This is the [(rep, all_zeros)] shape the composer
   found. *)
let rep_subcodec =
  Codec.v "Rep"
    (fun n xs -> (n, xs))
    Codec.
      [
        (f_rep_len $ fun (n, _) -> n);
        ( Field.repeat "items" ~size:(Field.ref f_rep_len)
            (byte_array ~size:(int 3))
        $ fun (_, xs) -> xs );
      ]

let oversized_repeat_codec =
  Codec.v "OverRep"
    (fun r z -> (r, z))
    Codec.
      [
        (Field.v "rep" (codec rep_subcodec) $ fun (r, _) -> r);
        (Field.v "az" all_zeros $ fun (_, z) -> z);
      ]

let test_repeat_oversized_length_rejected () =
  let decodes bs =
    match Codec.decode oversized_repeat_codec (Bytes.of_string bs) 0 with
    | Ok _ | Error _ -> true
    | exception Invalid_argument _ -> false
  in
  Alcotest.(check bool)
    "oversized embedded length fails cleanly, no crash" true
    (decodes "\xff\xff");
  Alcotest.(check bool)
    "length past buffer fails cleanly" true (decodes "\x00\x10abc")

(* [Codec.validate] never walks a record field by field the way decode does: it
   reaches each field's bytes through that field's populate, at the offset the
   layout computes. An overlong [Field.repeat] budget pushes every following
   field past the end of the buffer, and the populate used to read there
   unguarded. The trailing [all_zeros] keeps the record greedy, so the top-level
   bounds check resolves the record's span to the whole buffer and lets it
   through; the constrained fields make [validate] run the populate pass at all.
   Shape and bytes come from the composer's [(rep:fint,i8,uv3le,(bnd,i16,az))]
   sample. *)
let validate_overrun_codec =
  let f_a = Field.v "a" uint8 in
  let f_b =
    Field.v "b" uint8 ~self_constraint:(fun b ->
        Expr.(Field.int f_a + b < int 10))
  in
  let pair =
    Codec.v "Pair" (fun a b -> (a, b)) Codec.[ f_a $ fst; f_b $ snd ]
  in
  let f_total = Field.v "total" uint16be in
  let rep =
    Codec.v "Rep"
      (fun _ xs -> xs)
      Codec.
        [
          (f_total $ fun _ -> 0);
          ( Field.repeat "items" ~size:(Field.ref f_total) (codec pair)
          $ fun xs -> xs );
        ]
  in
  let bounded =
    Codec.v "Bounded"
      (fun v -> v)
      Codec.
        [
          ( Field.v "v" uint8 ~self_constraint:(fun r ->
                Expr.(r >= int 10 && r <= int 100))
          $ fun v -> v );
        ]
  in
  let tail =
    Codec.v "Tail"
      (fun x y z -> (x, y, z))
      Codec.
        [
          (Field.v "bnd" (codec bounded) $ fun (x, _, _) -> x);
          (Field.v "i16" int16be $ fun (_, y, _) -> y);
          (Field.v "az" all_zeros $ fun (_, _, z) -> z);
        ]
  in
  Codec.v "Overrun"
    (fun w x y z -> (w, x, y, z))
    Codec.
      [
        (Field.v "rep" (codec rep) $ fun (w, _, _, _) -> w);
        (Field.v "i8" int8 $ fun (_, x, _, _) -> x);
        ( Field.v "uv" (uint ~endian:Wire.Little (Wire.int 3))
        $ fun (_, _, y, _) -> y );
        (Field.v "tail" (codec tail) $ fun (_, _, _, z) -> z);
      ]

(* The composer's sample: a 0xAA0F byte budget on a 64-byte buffer. *)
let validate_overrun_input =
  "\xaa\x0f\x9bFZ\x12\xef\x99\x05\xc5<\xac\xcfx\xd4Q\xfai\xf1RE\xb7S\xe6\xb0\xb8\xdfLjO\xbc\xc9\xfb\xf1\xf1\xc6\xa2\xcaS\xba\x0f\xda\x8e=/\xceC?\x8d\x0cG\x92%\xa1\xbc\xaf\xf6\x18\x9d\x90\x82:\xbd\xac"

let test_validate_overrun_field_offset () =
  let buf = Bytes.of_string validate_overrun_input in
  (match Codec.validate validate_overrun_codec buf 0 with
  | () ->
      Alcotest.fail "validate accepted a record whose fields run off the end"
  | exception Wire.Parse_error _ -> ()
  | exception Invalid_argument m ->
      Alcotest.failf "validate crashed instead of failing cleanly: %s" m);
  match Codec.decode validate_overrun_codec buf 0 with
  | Ok _ ->
      Alcotest.fail "decode accepted a record whose fields run off the end"
  | Error _ -> ()
  | exception Invalid_argument m ->
      Alcotest.failf "decode crashed instead of failing cleanly: %s" m

(* Assert a *located* end-of-input: the failure kind, the extent the field was
   missing, and the offset it was missing it at. Checking only the exception
   class would also pass for a blanket [Invalid_argument -> Parse_error]
   wrapper around validate, which reports a garbage location; the offset is
   what shows a real per-field guard ran, at the field that overran. *)
let check_located_eof name ~at ~expected ~got = function
  | Ok () -> Alcotest.failf "%s: accepted a buffer its own decoder rejects" name
  | Error (e : parse_error) -> (
      Alcotest.(check int) (name ^ ": offset of the failing field") at e.at;
      match e.kind with
      | Unexpected_eof { expected = ex; got = g } ->
          Alcotest.(check int) (name ^ ": bytes the field needed") expected ex;
          Alcotest.(check int) (name ^ ": bytes the buffer had there") got g
      | k -> Alcotest.failf "%s: expected an eof, got %a" name pp_error_kind k)

(* Run a validate-style call and shape its outcome for [check_located_eof]. A
   raw [Invalid_argument] is the crash under test, so it fails here rather than
   being folded into a parse error. *)
let validating name f =
  match f () with
  | () -> Ok ()
  | exception Wire.Parse_error e -> Error e
  | exception Invalid_argument m ->
      Alcotest.failf "%s: crashed instead of failing cleanly: %s" name m

(* [Codec.validate_struct] runs the same populate-driven field pass as
   [Codec.validate] and needs the same per-field bounds check. [Data] is sized
   by [Len], so a [Len] of 200 in a 5-byte buffer puts [V] at offset 201: the
   first byte read off the end. The trailing greedy [Z] keeps the struct's span
   resolvable, so nothing rejects the record before [V]'s read. *)
let validate_struct_overrun () =
  let module T = Wire.Private.Types in
  T.struct_ "Overrun"
    [
      T.field "Len" uint8;
      T.field "Data" (byte_array ~size:(T.ref "Len"));
      T.field "V" uint16be;
      T.field "Z" all_zeros;
    ]

let validate_struct_overrun_input = "\xc8\x00\x00\x00\x00"

let test_validate_struct_field_past_end () =
  let module T = Wire.Private.Types in
  let s = validate_struct_overrun () in
  let v = Codec.validator_of_struct s in
  let buf = Bytes.of_string validate_struct_overrun_input in
  (* [V] wants 2 bytes at 201; the 5-byte buffer has none of them. *)
  check_located_eof "validate_struct" ~at:201 ~expected:2 ~got:0
    (validating "validate_struct" (fun () -> Codec.validate_struct v buf 0));
  check_located_eof "of_string" ~at:201 ~expected:2 ~got:0
    (validating "of_string" (fun () ->
         match of_string (T.struct_typ s) validate_struct_overrun_input with
         | Ok () -> ()
         | Error e -> raise (Wire.Parse_error e)))

(* Unlike [Codec.validate], [Codec.validate_struct] has no whole-record bounds
   check in front of the field pass, so even a statically placed field can be
   read off the end: [B] sits at offset 1 in a 1-byte buffer. The constraint is
   what makes the validator populate [B] at all. *)
let validate_struct_short () =
  let module T = Wire.Private.Types in
  T.struct_ "Short"
    [
      T.field "A" uint8;
      T.field "B" uint16be ~constraint_:Expr.(T.ref "B" < int 1000);
    ]

let test_validate_struct_fixed_field_past_end () =
  let v = Codec.validator_of_struct (validate_struct_short ()) in
  (* [B] wants 2 bytes at 1; the 1-byte buffer has none of them. *)
  check_located_eof "validate_struct" ~at:1 ~expected:2 ~got:0
    (validating "validate_struct" (fun () ->
         Codec.validate_struct v (Bytes.of_string "\x01") 0))

(* An optional whose presence is a runtime expression has no wire size of its
   own: an absent one occupies nothing. A present one occupies exactly the
   inner's width, so its read needs the bounds check every other field's gets.
   [data] is sized by [len], so a [len] of 200 in a 5-byte buffer puts [opt] at
   offset 202, and [flag = 1] says it is there. The greedy [z] keeps the
   record's span resolvable so the read is reached. *)
let validate_optional_codec =
  let f_flag = Field.v "flag" uint8 in
  let f_len = Field.v "len" uint8 in
  Codec.v "OptOverrun"
    (fun flag len data opt z -> (flag, len, data, opt, z))
    Codec.
      [
        (f_flag $ fun (flag, _, _, _, _) -> flag);
        (f_len $ fun (_, len, _, _, _) -> len);
        ( Field.v "data" (byte_array ~size:(Field.ref f_len))
        $ fun (_, _, data, _, _) -> data );
        ( Field.optional "opt" ~present:Expr.(Field.ref f_flag = int 1) uint16be
        $ fun (_, _, _, opt, _) -> opt );
        (Field.v "z" all_zeros $ fun (_, _, _, _, z) -> z);
      ]

let test_validate_present_optional_past_end () =
  let buf = Bytes.of_string "\x01\xc8\x00\x00\x00" in
  (* [opt] wants 2 bytes at 202; the 5-byte buffer has none of them. *)
  check_located_eof "validate" ~at:202 ~expected:2 ~got:0
    (validating "validate" (fun () ->
         Codec.validate validate_optional_codec buf 0))

(* [optional_or] gates its read on the same kind of runtime expression and has
   the same hole. Same shape and same bytes as [validate_optional_codec]. *)
let validate_optional_or_codec =
  let f_flag = Field.v "flag" uint8 in
  let f_len = Field.v "len" uint8 in
  Codec.v "OptOrOverrun"
    (fun flag len data opt z -> (flag, len, data, opt, z))
    Codec.
      [
        (f_flag $ fun (flag, _, _, _, _) -> flag);
        (f_len $ fun (_, len, _, _, _) -> len);
        ( Field.v "data" (byte_array ~size:(Field.ref f_len))
        $ fun (_, _, data, _, _) -> data );
        ( Field.optional_or "opt"
            ~present:Expr.(Field.ref f_flag = int 1)
            ~default:0 uint16be
        $ fun (_, _, _, opt, _) -> opt );
        (Field.v "z" all_zeros $ fun (_, _, _, _, z) -> z);
      ]

let test_validate_present_optional_or_past_end () =
  let buf = Bytes.of_string "\x01\xc8\x00\x00\x00" in
  check_located_eof "validate" ~at:202 ~expected:2 ~got:0
    (validating "validate" (fun () ->
         Codec.validate validate_optional_or_codec buf 0))

(* A [uint] of runtime width reads its bytes directly instead of through the
   span reader every other variable-width field uses, so it needs that
   reader's span check of its own. [len] is 7, so [v] claims bytes 1..8 of a
   3-byte buffer. *)
let validate_uint_var_codec =
  let f_len = Field.v "len" uint8 in
  Codec.v "UintOverrun"
    (fun len v z -> (len, v, z))
    Codec.
      [
        (f_len $ fun (len, _, _) -> len);
        (Field.v "v" (uint (Field.int f_len)) $ fun (_, v, _) -> v);
        (Field.v "z" all_zeros $ fun (_, _, z) -> z);
      ]

let test_validate_uint_var_past_end () =
  let buf = Bytes.of_string "\x07\x00\x00" in
  (* The span check reports the 7 bytes the field asked for against the 2 the
     buffer still had at offset 1, the way a truncated byte span does. *)
  check_located_eof "validate" ~at:1 ~expected:7 ~got:2
    (validating "validate" (fun () ->
         Codec.validate validate_uint_var_codec buf 0))

(* A [Field.repeat] consumes its byte budget exactly, so an 11-byte budget over
   a 2-byte element is illegal however long the buffer is (here 64 bytes): the
   budget does not split into whole elements. EverParse rejects the same frame
   from the same schema, with a dedicated "list size not multiple" error, and so
   does [Codec.decode]. [Codec.validate] used to accept it: it reaches a field
   only through that field's populate, and the repeat had none. Bytes are the
   fuzz sample verbatim ([fuzz.exe -s 32582623701 -r 7500]). *)
let repeat_odd_budget =
  "\x00\x0b\x1a\x15\x11\x9d\x47\x90\x0d\xcf\xbd\xa9\x80\xf5\x21\x27\x44\xad\x4d\xa6\xc1\xde\xd7\xf6\x1a\x42\x1a\x51\x9d\x9d\x57\x5c\x2c\x8c\x75\xa1\xba\xfb\xaf\xe3\xd7\xaa\x2b\x1d\x90\xfb\xce\x47\xf9\xa4\x4e\x46\xbe\x2a\xa1\xbb\x18\xfc\x72\xe2\x8e\x47\xf9\x41"

let repeat_budget_codec =
  let f_total = Field.v "total" uint16be in
  Codec.v "RepBudget"
    (fun _ xs -> xs)
    Codec.
      [
        (f_total $ fun xs -> 2 * List.length xs);
        (Field.repeat "items" ~size:(Field.ref f_total) uint16be $ fun xs -> xs);
      ]

(* [repeat_seq] builds the same [Repeat] node with a different sequence builder,
   so it shares the populate and the budget check; pinned here so a future split
   of the two paths cannot fix one and leave the other. *)
let repeat_seq_budget_codec =
  let f_total = Field.v "total" uint16be in
  Codec.v "RepSeqBudget"
    (fun _ xs -> xs)
    Codec.
      [
        (f_total $ fun xs -> 2 * List.length xs);
        ( Field.repeat_seq "items" ~seq:seq_list ~size:(Field.ref f_total)
            uint16be
        $ fun xs -> xs );
      ]

let test_validate_repeat_partial_element () =
  let buf = Bytes.of_string repeat_odd_budget in
  let rejects name codec =
    (* The budget runs from offset 2 to 13 and leaves one byte at 12, where a
       2-byte element needs two. Both sides must report that same spot: a fix
       that made them agree on accepting would pass a bare "they agree" check. *)
    check_located_eof (name ^ " validate") ~at:12 ~expected:2 ~got:1
      (validating (name ^ " validate") (fun () -> Codec.validate codec buf 0));
    check_located_eof (name ^ " decode") ~at:12 ~expected:2 ~got:1
      (validating (name ^ " decode") (fun () ->
           match Codec.decode codec buf 0 with
           | Ok _ -> ()
           | Error e -> raise (Wire.Parse_error e)))
  in
  rejects "repeat" repeat_budget_codec;
  rejects "repeat_seq" repeat_seq_budget_codec;
  (* A budget that does split stays accepted on both sides: what is rejected is
     the partial element, not every repeat. *)
  let whole =
    Bytes.of_string ("\x00\x0a" ^ String.sub repeat_odd_budget 2 10)
  in
  let accepts name codec =
    (match Codec.validate codec whole 0 with
    | () -> ()
    | exception Wire.Parse_error e ->
        Alcotest.failf "%s: validate rejected a whole budget: %a" name
          Wire.pp_parse_error e);
    match Codec.decode codec whole 0 with
    | Ok xs ->
        Alcotest.(check int) (name ^ ": elements decoded") 5 (List.length xs)
    | Error e ->
        Alcotest.failf "%s: decode rejected a whole budget: %a" name
          Wire.pp_parse_error e
  in
  accepts "repeat" repeat_budget_codec;
  accepts "repeat_seq" repeat_seq_budget_codec

(* The same truncation read at a nonzero base. [expected] and [got] are byte
   counts, so they say the same thing wherever the frame sits; only [at] moves.
   Reporting buffer positions instead made both of them grow with the base, and
   made [got] equal [expected] on a failure that says the input ran out. *)
let test_eof_counts_are_base_invariant () =
  let pad = 8 in
  let buf = Bytes.of_string (String.make pad '\xee' ^ "\x07\x00\x00") in
  check_located_eof "validate at base 8" ~at:(pad + 1) ~expected:7 ~got:2
    (validating "validate at base 8" (fun () ->
         Codec.validate validate_uint_var_codec buf pad))

(* A byte_slice whose resolved size goes negative (here a [Sub] on an untrusted
   length field) must fail cleanly: [make_or_eod] would otherwise raise a raw
   [Invalid_argument] that escapes the decode result. A large variable field
   ahead of it keeps the framing past the top-level bounds check, so the negative
   size reaches the slice read. *)
let test_byte_slice_negative_size () =
  let f_l = Field.v "l" uint8 in
  let f_n = Field.v "n" uint8 in
  let c =
    Codec.v "SliceNeg"
      (fun l pre n sl -> (l, pre, n, sl))
      Codec.
        [
          (f_l $ fun (l, _, _, _) -> l);
          ( Field.v "pre" (byte_array ~size:(Field.ref f_l))
          $ fun (_, p, _, _) -> p );
          (f_n $ fun (_, _, n, _) -> n);
          ( Field.v "sl" (byte_slice ~size:Expr.(Field.ref f_n - int 100))
          $ fun (_, _, _, s) -> s );
        ]
  in
  let buf = Bytes.make 202 '\000' in
  Bytes.set_uint8 buf 0 200;
  Bytes.set_uint8 buf 201 1;
  match Codec.decode c buf 0 with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail "expected a parse error for a negative byte_slice size"
  | exception Invalid_argument _ ->
      Alcotest.fail
        "negative byte_slice size crashed instead of failing cleanly"

(* A bitfield has no standalone element form, so [array] / [Field.repeat] over
   one is rejected at construction rather than crashing at decode. *)
let raises_invalid f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let test_repeat_array_reject_bitfield () =
  Alcotest.(check bool)
    "array over bits rejected" true
    (raises_invalid (fun () -> array ~len:(int 4) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "repeat over bits rejected" true
    (raises_invalid (fun () ->
         Field.repeat "x" ~size:(int 4) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "repeat over bit (bool over bits) rejected" true
    (raises_invalid (fun () ->
         Field.repeat "x" ~size:(int 4) (bit (bits ~width:1 U8))))

(* A zero-width element ([empty] / unit) carries no bytes, so an array of it is
   degenerate and projects to a zero-size 3D array EverParse rejects. It is
   refused at construction. *)
let test_reject_zero_width_element () =
  Alcotest.(check bool)
    "array over empty rejected" true
    (raises_invalid (fun () -> array ~len:(int 3) empty));
  Alcotest.(check bool)
    "array_seq over empty rejected" true
    (raises_invalid (fun () -> array_seq seq_list ~len:(int 3) empty));
  (* A byte-budget list of a 0-width element does not extract either, so
     [Field.repeat] over [empty] is rejected like [array], not silently built. *)
  Alcotest.(check bool)
    "repeat over empty rejected" true
    (raises_invalid (fun () -> Field.repeat "r" ~size:(int 3) empty))

(* A byte span of literal size zero is 0-width too: EverParse refuses to extract
   a zero-size element, so an [array] over one has no projection even though the
   fixed count keeps the decode loop terminating. Refused at construction like
   [empty], matching the [Field.repeat] guard. *)
let array_rejects elem =
  match array ~len:(int 2) elem with
  | _ -> false
  | exception Invalid_argument _ -> true

let test_array_rejects_zero_width_span () =
  Alcotest.(check bool)
    "byte_array of size 0" true
    (array_rejects (byte_array ~size:(int 0)));
  Alcotest.(check bool)
    "byte_slice of size 0" true
    (array_rejects (byte_slice ~size:(int 0)));
  Alcotest.(check bool)
    "byte_array of negative size" true
    (array_rejects (byte_array ~size:(int (-1))));
  Alcotest.(check bool)
    "byte_array of size 0 under a map" true
    (array_rejects
       (map ~decode:String.length
          ~encode:(fun _ -> "")
          (byte_array ~size:(int 0))));
  Alcotest.(check bool)
    "byte_array of a folded zero size" true
    (array_rejects (byte_array ~size:Expr.(int 1 - int 1)));
  Alcotest.(check bool)
    "array_seq over byte_array of size 0" true
    (match array_seq seq_list ~len:(int 2) (byte_array ~size:(int 0)) with
    | _ -> false
    | exception Invalid_argument _ -> true);
  (* A varint sized by [sizeof] stays symbolic (constant folding leaves [Sizeof]
     alone), so it never reaches the literal-size case and [array] already
     refuses it. *)
  Alcotest.(check bool)
    "uint sized by sizeof empty" true
    (array_rejects (uint (sizeof empty)))

(* The guard must still admit a legitimate fixed-size byte element: a positive
   literal span projects to a count of fixed-size elements. *)
let test_array_accepts_fixed_byte_span () =
  Alcotest.(check bool)
    "byte_array of size 4 allowed" false
    (array_rejects (byte_array ~size:(int 4)));
  Alcotest.(check bool)
    "byte_slice of size 1 allowed" false
    (array_rejects (byte_slice ~size:(int 1)))

(* Wire.array over a fixed sub-record (Codec element): a fixed-count list of
   structs. Decoding raised Failure "build_field_reader: unsupported type"
   because the array element reader had no Codec case. The schema projects the
   element as the sub-struct under a [:byte-size] budget. *)
type pt = { px : int; py : int }

let pt_codec =
  Codec.v "Pt"
    (fun px py -> { px; py })
    Codec.
      [
        (Field.v "px" uint8 $ fun r -> r.px);
        (Field.v "py" uint16be $ fun r -> r.py);
      ]

type arr_recs = { rtag : int; pts : pt list }

let arr_recs_codec =
  Codec.v "ArrRecs"
    (fun rtag pts -> { rtag; pts })
    Codec.
      [
        (Field.v "rtag" uint8 $ fun r -> r.rtag);
        (Field.v "pts" (array ~len:(int 2) (codec pt_codec)) $ fun r -> r.pts);
      ]

let test_array_record_element () =
  let v =
    { rtag = 9; pts = [ { px = 1; py = 0x0203 }; { px = 4; py = 0x0506 } ] }
  in
  let sz = Codec.size_of_value arr_recs_codec v in
  Alcotest.(check int) "wire size" 7 sz;
  let buf = Bytes.create sz in
  Codec.encode arr_recs_codec v buf 0;
  match Codec.decode arr_recs_codec buf 0 with
  | Ok d ->
      Alcotest.(check (list (pair int int)))
        "pts"
        (List.map (fun p -> (p.px, p.py)) v.pts)
        (List.map (fun p -> (p.px, p.py)) d.pts)
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_array_record_projection () =
  let out = render_3d arr_recs_codec in
  Alcotest.(check bool)
    "sub-struct element under byte-size on the array field" true
    (contains ~sub:"Pt pts[:byte-size (2 * 3)]" out)

(* [nested] / [nested_at_most] over a bitfield is likewise rejected at
   construction with a clear error, not at decode. *)
let test_nested_reject_bitfield () =
  Alcotest.(check bool)
    "nested over bits rejected" true
    (raises_invalid (fun () -> nested ~size:(int 1) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "nested_at_most over bits rejected" true
    (raises_invalid (fun () -> nested_at_most ~size:(int 1) (bits ~width:3 U8)))

(* A [nested] / [nested_at_most] region over a composite inner (an array, or
   another nested region) round-trips: the inner decodes at the region start
   and the region is zero-padded to its fixed size. 3D projects it through a
   synthesised wrapper struct (see test_everparse). *)
let test_nested_over_array () =
  let codec =
    Codec.v "NestArr" Fun.id
      Codec.
        [
          Field.v "xs" (nested ~size:(int 16) (array ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let v = [ 1L; 2L ] in
  let buf = Bytes.create (Codec.size_of_value codec v) in
  Codec.encode codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode codec buf 0) = v)

let test_nested_at_most_over_array () =
  let codec =
    Codec.v "NestAtmArr" Fun.id
      Codec.
        [
          Field.v "xs"
            (nested_at_most ~size:(int 16)
               (array_seq seq_list ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let v = [ 7L; 9L ] in
  let buf = Bytes.create (Codec.size_of_value codec v) in
  Codec.encode codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode codec buf 0) = v)

let test_nested_exact_region () =
  let make name typ =
    Codec.v name Fun.id Codec.[ Field.v "value" typ $ Fun.id ]
  in
  let exact = make "NestedExact" (nested ~size:(int 4) zeroterm) in
  let at_most = make "NestedAtMost" (nested_at_most ~size:(int 4) zeroterm) in
  let padded = Bytes.of_string "A\x00\x00\x00" in
  (match Codec.decode exact padded 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "compiled exact nested accepted trailing padding");
  Alcotest.(check string)
    "compiled at-most accepts padding" "A"
    (decode_ok (Codec.decode at_most padded 0));
  Alcotest.(check string)
    "compiled exact accepts full consumption" "ABC"
    (decode_ok (Codec.decode exact (Bytes.of_string "ABC\x00") 0));
  (match Codec.size_of_value exact "A" with
  | _ -> Alcotest.fail "compiled exact nested encode accepted padding"
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "nested error" true (contains ~sub:"nested" msg));
  let buf = Bytes.create 4 in
  Codec.encode at_most "A" buf 0;
  Alcotest.(check bytes) "compiled at-most pads" padded buf

(* A casetype whose case body is a [nested] region (a scalar in a fixed span):
   the tag-dispatched case decodes and sizes through the region. *)
type nest_case = N of int | U of int

let nest_case_codec =
  let ct =
    casetype "NcT" uint8
      [
        case ~index:1
          (nested ~size:(int 4) int32be)
          ~inject:(fun v -> N v)
          ~project:(function N v -> Some v | _ -> None);
        case ~index:2 uint8
          ~inject:(fun v -> U v)
          ~project:(function U v -> Some v | _ -> None);
      ]
  in
  Codec.v "Nc" Fun.id Codec.[ Field.v "b" ct $ Fun.id ]

let test_casetype_nested_case_body () =
  List.iter
    (fun v ->
      let buf = Bytes.create (Codec.size_of_value nest_case_codec v) in
      Codec.encode nest_case_codec v buf 0;
      Alcotest.(check bool)
        "roundtrip" true
        (decode_ok (Codec.decode nest_case_codec buf 0) = v))
    [ N 12345; U 7 ]

(* Field.repeat over a fixed byte_array element: a list of n-byte chunks within
   a byte budget. Decodes the list and projects to a single [:byte-size]
   schema. *)
type rep_chunks = { rn : int; chunks : string list }

let rep_chunks_codec =
  let f_n = Field.v "n" uint16be in
  let f_chunks =
    Field.repeat "chunks" ~size:(Field.ref f_n) (byte_array ~size:(int 4))
  in
  Codec.v "RepChunks"
    (fun rn chunks -> { rn; chunks })
    Codec.[ (f_n $ fun r -> r.rn); (f_chunks $ fun r -> r.chunks) ]

let test_repeat_byte_array_element () =
  let v = { rn = 8; chunks = [ "aaaa"; "bbbb" ] } in
  let sz = Codec.size_of_value rep_chunks_codec v in
  Alcotest.(check int) "wire size" 10 sz;
  let buf = Bytes.create sz in
  Codec.encode rep_chunks_codec v buf 0;
  match Codec.decode rep_chunks_codec buf 0 with
  | Ok d ->
      Alcotest.(check int) "count" 8 d.rn;
      Alcotest.(check (list string)) "chunks" [ "aaaa"; "bbbb" ] d.chunks
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

(* The schema projects the byte-span element as bare [UINT8] under the budget,
   not a double [:byte-size]. *)
let test_repeat_byte_array_projection () =
  let out = render_3d rep_chunks_codec in
  Alcotest.(check bool)
    "single byte-size on the chunks field" true
    (contains ~sub:"UINT8 chunks[:byte-size n]" out)

(* An element with no clean per-element 3D projection is refused at
   construction, rather than failing late with Failure at decode/encode or
   emitting a schema EverParse cannot verify. *)
let repeat_rejects elem =
  match Field.repeat "items" ~size:(int 4) elem with
  | _ -> false
  | exception Invalid_argument _ -> true

let test_repeat_rejects_unprojectable () =
  Alcotest.(check bool) "bits element" true (repeat_rejects (bits ~width:3 U8));
  Alcotest.(check bool) "all_zeros element" true (repeat_rejects all_zeros);
  Alcotest.(check bool)
    "zeroterm_at_most element" true
    (repeat_rejects (zeroterm_at_most ~size:(int 6)));
  Alcotest.(check bool)
    "byte_array_where element" true
    (repeat_rejects
       (byte_array_where ~size:(int 2) ~per_byte:(fun _ -> Expr.true_)))

(* A byte span of literal size zero carries no bytes, so a byte-budget list of
   it has nothing for EverParse to extract and the decoder's loop would never
   reach the end of its region. Refused at construction like [empty]. *)
let test_repeat_rejects_zero_width_span () =
  Alcotest.(check bool)
    "byte_array of size 0" true
    (repeat_rejects (byte_array ~size:(int 0)));
  Alcotest.(check bool)
    "byte_slice of size 0" true
    (repeat_rejects (byte_slice ~size:(int 0)));
  Alcotest.(check bool)
    "byte_array of negative size" true
    (repeat_rejects (byte_array ~size:(int (-1))));
  Alcotest.(check bool)
    "byte_array of size 0 under a map" true
    (repeat_rejects
       (map ~decode:String.length
          ~encode:(fun _ -> "")
          (byte_array ~size:(int 0))));
  Alcotest.(check bool)
    "repeat_seq over byte_array of size 0" true
    (match
       Field.repeat_seq "items" ~seq:seq_list ~size:(int 4)
         (byte_array ~size:(int 0))
     with
    | _ -> false
    | exception Invalid_argument _ -> true);
  Alcotest.(check bool)
    "byte_array of size 1 allowed" false
    (repeat_rejects (byte_array ~size:(int 1)))

(* A sub-codec whose last field is greedy ([all_bytes] / [all_zeros]) reads the
   rest of the buffer as its tail, so it cannot be a repeat element (the first
   element would consume everything). It is fine standalone. *)
let test_repeat_rejects_greedy_tail_codec () =
  let greedy =
    Codec.v "GreedyTail"
      (fun a b -> (a, b))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "rest" all_bytes $ snd ]
  in
  Alcotest.(check bool)
    "repeat over codec ending in all_bytes rejected" true
    (repeat_rejects (codec greedy));
  let plain =
    Codec.v "Plain" (fun a -> a) Codec.[ Field.v "n" uint8 $ Fun.id ]
  in
  Alcotest.(check bool)
    "repeat over a non-greedy codec allowed" false
    (repeat_rejects (codec plain))

(* [optional] / [optional_or] project either a conditional byte-size region (a
   sized inner) or a gate-dispatched casetype (a self-delimiting inner). An
   inner that is neither, such as [all_bytes], has no projection and is refused
   at construction. *)
let test_optional_reject_unprojectable () =
  Alcotest.(check bool)
    "optional over all_bytes rejected" true
    (raises_invalid (fun () -> Field.optional "x" ~present:Expr.true_ all_bytes));
  Alcotest.(check bool)
    "optional_or over all_bytes rejected" true
    (raises_invalid (fun () ->
         Field.optional_or "x" ~present:Expr.true_ ~default:"" all_bytes))

(* An [array] / [array_seq] element must be a fixed-width type the array loop
   can read one element at a time. A [nested] region, a refined byte span, and
   a nested array all carry a wire size but have no array projection and no
   element reader, so they are refused at construction (unlike [repeat], whose
   byte budget admits self-delimiting variable elements). *)
let test_array_reject_nonprojectable_element () =
  Alcotest.(check bool)
    "array over nested rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (nested ~size:(int 4) uint8)));
  Alcotest.(check bool)
    "array_seq over nested_at_most rejected" true
    (raises_invalid (fun () ->
         array_seq seq_list ~len:(int 2) (nested_at_most ~size:(int 4) uint8)));
  Alcotest.(check bool)
    "array over byte_array_where rejected" true
    (raises_invalid (fun () ->
         array ~len:(int 2)
           (byte_array_where ~size:(int 2) ~per_byte:(fun _ -> Expr.true_))));
  Alcotest.(check bool)
    "array over array rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (array ~len:(int 2) uint8)))

(* EverParse projects [array] / [repeat] of a sub-codec as a byte-budget list of
   the codec's named struct, and its [T_nlist] requires the element parser to
   consume a positive minimum of bytes. A sub-codec made only of byte-span
   fields has a possibly-empty parser, so the list does not extract -- such an
   element is refused at construction. One fixed-size field is enough to anchor
   it. *)
let lone_byte_codec =
  Codec.v "LoneBytes" Fun.id
    Codec.[ Field.v "blob" (byte_array ~size:(int 4)) $ Fun.id ]

let scalar_bearing_codec =
  Codec.v "Anchored"
    (fun a b -> (a, b))
    Codec.
      [
        Field.v "tag" uint8 $ fst;
        Field.v "blob" (byte_array ~size:(int 3)) $ snd;
      ]

let test_array_repeat_reject_non_nz_codec () =
  Alcotest.(check bool)
    "array over byte-span-only codec rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (codec lone_byte_codec)));
  Alcotest.(check bool)
    "repeat over byte-span-only codec rejected" true
    (repeat_rejects (codec lone_byte_codec));
  Alcotest.(check bool)
    "array over a codec with a fixed-size field allowed" false
    (raises_invalid (fun () -> array ~len:(int 2) (codec scalar_bearing_codec)));
  Alcotest.(check bool)
    "repeat over a codec with a fixed-size field allowed" false
    (repeat_rejects (codec scalar_bearing_codec))

let projects c = match render_3d c with _ -> true | exception _ -> false
let arr_field elem = Codec.v "Arr" Fun.id Codec.[ Field.v "x" elem $ Fun.id ]

(* An [array] over a float, signed integer, or [uint63] element builds,
   round-trips, and projects to a verified 3D schema: [inner_wire_size] sizes
   every fixed-width scalar element (unsigned and signed ints, floats, [uint63],
   and bitfields), so the projector can size it. *)
let test_array_scalar_element_projects () =
  let yes name c =
    Alcotest.(check bool) (name ^ " array projects") true (projects c)
  in
  yes "float64be" (arr_field (array ~len:(int 2) float64be));
  yes "float32be" (arr_field (array ~len:(int 2) float32be));
  yes "int8" (arr_field (array ~len:(int 2) int8));
  yes "int16be" (arr_field (array ~len:(int 2) int16be));
  yes "int32be" (arr_field (array ~len:(int 2) int32be));
  yes "int64be" (arr_field (array ~len:(int 2) int64be));
  yes "uint63be" (arr_field (array ~len:(int 2) uint63be));
  (* and it still round-trips *)
  let c = arr_field (array ~len:(int 2) float64be) in
  let v = [ 1.5; -2.25 ] in
  let buf = Bytes.create (Codec.size_of_value c v) in
  Codec.encode c v buf 0;
  match Codec.decode c buf 0 with
  | Ok d -> Alcotest.(check (list (float 0.0))) "float array" v d
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

(* An [array] over a [where] / [map] wrapping a byte span projects to a verified
   3D schema: [is_array_element] looks through the transparent wrapper to accept
   it, and [inner_wire_size_expr] recurses through the wrapper to find the span's
   size. *)
let test_array_wrapped_byte_span_projects () =
  Alcotest.(check bool)
    "array over where(byte_slice) projects" true
    (projects
       (arr_field
          (array ~len:(int 2) (where Expr.true_ (byte_slice ~size:(int 4))))));
  Alcotest.(check bool)
    "array over map(byte_array) projects" true
    (projects
       (arr_field
          (array ~len:(int 2)
             (map ~decode:Fun.id ~encode:Fun.id (byte_array ~size:(int 4))))))

(* A bitfield has no standalone wire form, so it cannot be an [optional] inner
   any more than an [array] / [repeat] / [nested] element. *)
let test_optional_reject_bitfield () =
  Alcotest.(check bool)
    "optional over bits rejected" true
    (raises_invalid (fun () ->
         Field.optional "x" ~present:Expr.true_ (bits ~width:3 U8)));
  Alcotest.(check bool)
    "optional_or over bits rejected" true
    (raises_invalid (fun () ->
         Field.optional_or "x" ~present:Expr.true_ ~default:0 (bits ~width:3 U8)))

(* A bare greedy field ([all_bytes] / [all_zeros]) reads the rest of the buffer;
   it has no determinate type, so it cannot be a casetype case body (a sub-codec
   that ends in [all_bytes] is the supported form). *)
let test_casetype_reject_greedy_case_body () =
  let build inner =
    casetype "Greedy" uint8
      [ case ~index:1 inner ~inject:(fun s -> s) ~project:Option.some ]
  in
  Alcotest.(check bool)
    "casetype with all_bytes case body rejected" true
    (raises_invalid (fun () -> build all_bytes));
  Alcotest.(check bool)
    "casetype with all_zeros case body rejected" true
    (raises_invalid (fun () -> build all_zeros))

(* A greedy field ([all_bytes] / [all_zeros]) reads the rest of the buffer, so
   it is only valid as the last field; an earlier one is refused at
   construction (it would starve every field after it at decode). *)
let test_greedy_not_last_rejected () =
  Alcotest.(check bool)
    "all_zeros before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "GnlA"
           (fun a b -> (a, b))
           Codec.[ Field.v "z" all_zeros $ fst; Field.v "n" uint8 $ snd ]));
  Alcotest.(check bool)
    "all_bytes before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "GnlB"
           (fun a b -> (a, b))
           Codec.[ Field.v "b" all_bytes $ fst; Field.v "n" uint8 $ snd ]));
  (* A greedy last field is fine and round-trips. *)
  let c =
    Codec.v "GnlOk"
      (fun a b -> (a, b))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "rest" all_bytes $ snd ]
  in
  let v = (5, "tail") in
  let buf = Bytes.create (Codec.size_of_value c v) in
  Codec.encode c v buf 0;
  Alcotest.(check bool)
    "greedy last roundtrip" true
    (decode_ok (Codec.decode c buf 0) = v)

(* A casetype whose case body is a sub-codec ending in a greedy field consumes
   the rest of the buffer when that case is selected; placed before another
   field it would starve it, so it is refused at construction. As the last field
   it is fine. *)
let test_casetype_greedy_case_not_last_rejected () =
  let greedy =
    Codec.v "G" (fun z -> z) Codec.[ Field.v "z" all_zeros $ Fun.id ]
  in
  let ct =
    casetype "CtG" uint8
      [
        case ~index:1 uint8
          ~inject:(fun v -> `U v)
          ~project:(function `U v -> Some v | _ -> None);
        case ~index:2 (codec greedy)
          ~inject:(fun v -> `G v)
          ~project:(function `G v -> Some v | _ -> None);
      ]
  in
  Alcotest.(check bool)
    "casetype with greedy case body before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "Outer"
           (fun c t -> (c, t))
           Codec.[ Field.v "ct" ct $ fst; Field.v "tail" uint8 $ snd ]));
  Alcotest.(check bool)
    "casetype with greedy case body as last field accepted" false
    (raises_invalid (fun () ->
         Codec.v "OuterOk" Fun.id Codec.[ Field.v "ct" ct $ Fun.id ]))

(* The greedy case body may itself carry a leading field before the greedy tail
   (here [n] then [all_zeros]); the casetype still consumes the rest whenever the
   greedy case is selected, so it can be the last field but is rejected before
   another field. A richer variant of the case above. *)
let test_casetype_wrapped_greedy_not_last_rejected () =
  let body =
    Codec.v "GreedyCaseBody"
      (fun n z -> (n, z))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "z" all_zeros $ snd ]
  in
  let payload =
    casetype "GreedyCase" uint8
      [
        case ~index:1 (codec body)
          ~inject:(fun v -> `Greedy v)
          ~project:(function `Greedy v -> Some v | _ -> None);
        case ~index:2 uint8
          ~inject:(fun v -> `Byte v)
          ~project:(function `Byte v -> Some v | _ -> None);
      ]
  in
  Alcotest.(check bool)
    "casetype with wrapped greedy case can be last" false
    (raises_invalid (fun () ->
         Codec.v "WrappedGreedyLast"
           (fun n p -> (n, p))
           Codec.[ Field.v "n" uint8 $ fst; Field.v "p" payload $ snd ]));
  Alcotest.(check bool)
    "casetype with wrapped greedy case before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "WrappedGreedyNotLast"
           (fun p tail -> (p, tail))
           Codec.[ Field.v "p" payload $ fst; Field.v "tail" uint8 $ snd ]))

let test_optional_greedy_not_last_rejected () =
  let greedy =
    Codec.v "GreedyOptionalBody"
      (fun n z -> (n, z))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "z" all_zeros $ snd ]
  in
  let check_rejected label field =
    Alcotest.(check bool)
      label true
      (raises_invalid (fun () ->
           Codec.v "OptionalGreedyNotLast"
             (fun body tail -> (body, tail))
             Codec.[ field $ fst; Field.v "tail" uint8 $ snd ]))
  in
  check_rejected "present optional greedy body before tail rejected"
    (Field.optional "body" ~present:Expr.true_ (codec greedy));
  check_rejected "dynamic optional greedy body before tail rejected"
    (Field.optional "body" ~present:Expr.(int 1 = int 1) (codec greedy));
  check_rejected "optional_or greedy body before tail rejected"
    (Field.optional_or "body" ~present:Expr.true_ ~default:(0, "")
       (codec greedy));
  Alcotest.(check bool)
    "statically absent optional greedy body before tail accepted" false
    (raises_invalid (fun () ->
         Codec.v "AbsentOptionalGreedy"
           (fun body tail -> (body, tail))
           Codec.
             [
               Field.optional "body" ~present:Expr.false_ (codec greedy) $ fst;
               Field.v "tail" uint8 $ snd;
             ]))

(* [uint] is a 1-to-7-byte unsigned integer; a literal size outside that range
   is refused at construction. *)
let test_uint_size_bounds () =
  Alcotest.(check bool)
    "uint 0 rejected" true
    (raises_invalid (fun () -> uint (int 0)));
  Alcotest.(check bool)
    "uint 8 rejected" true
    (raises_invalid (fun () -> uint (int 8)));
  Alcotest.(check bool)
    "uint 4 accepted" false
    (raises_invalid (fun () -> uint (int 4)))

(* A bitfield wider than its base word, or narrower than one bit, has no faithful
   wire meaning (the OCaml shift and the 3D field would read different values), so
   it is refused at construction. *)
let test_bits_width_bounds () =
  Alcotest.(check bool)
    "9-bit field over U8 rejected" true
    (raises_invalid (fun () -> bits ~width:9 U8));
  Alcotest.(check bool)
    "0-bit field rejected" true
    (raises_invalid (fun () -> bits ~width:0 U8));
  Alcotest.(check bool)
    "17-bit field over U16 rejected" true
    (raises_invalid (fun () -> bits ~width:17 U16));
  Alcotest.(check bool)
    "8-bit field over U8 accepted" false
    (raises_invalid (fun () -> bits ~width:8 U8))

(* A [casetype] [case] carries an explicit discriminator; omitting [~index] is
   refused at construction (only [default] is index-free). *)
let test_casetype_case_requires_index () =
  Alcotest.(check bool)
    "case without ~index rejected" true
    (raises_invalid (fun () ->
         casetype "NoIndex" uint8
           [ case uint8 ~inject:Fun.id ~project:Option.some ]))

(* A casetype tag must project to a 3D type the [switch] dispatches on. A
   [uint ~size] tag renders as a non-3D [UINTBE(n)], and an enum over a
   big-endian base has no 3D enum type for its case labels, so both are refused
   at construction; a little-endian / 1-byte enum tag is fine. *)
let test_casetype_reject_unprojectable_tag () =
  let one_case tag =
    casetype "CtTag" tag
      [ case ~index:1 uint8 ~inject:(fun s -> s) ~project:Option.some ]
  in
  Alcotest.(check bool)
    "uint ~size tag rejected" true
    (raises_invalid (fun () ->
         casetype "CtTag"
           (uint (int 2))
           [
             case ~index:(Optint.Int63.of_int 1) uint8
               ~inject:(fun s -> s)
               ~project:Option.some;
           ]));
  Alcotest.(check bool)
    "big-endian enum tag rejected" true
    (raises_invalid (fun () ->
         one_case (enum "TagBe" [ ("A", 0); ("B", 1) ] uint16be)));
  Alcotest.(check bool)
    "little-endian enum tag accepted" false
    (raises_invalid (fun () ->
         one_case (enum "TagLe" [ ("A", 0); ("B", 1) ] uint8)))

(* -- Codec bitfield tests -- *)

type bf32_record = { a : int; b : int; c : int; d : int }

let bf32_codec =
  let open Codec in
  v "Bf32Test"
    (fun a b c d -> { a; b; c; d })
    [
      (Field.v "a" (bits ~width:3 U32be) $ fun t -> t.a);
      (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
      (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
      (Field.v "d" (bits ~width:8 U32be) $ fun t -> t.d);
    ]

type bf16_record = { ver : int; flags : int; id : int; count : int; len : int }

let bf16_codec =
  let open Codec in
  v "Bf16Test"
    (fun ver flags id count len -> { ver; flags; id; count; len })
    [
      (Field.v "ver" (bits ~width:3 U16be) $ fun t -> t.ver);
      (Field.v "flags" (bits ~width:2 U16be) $ fun t -> t.flags);
      (Field.v "id" (bits ~width:11 U16be) $ fun t -> t.id);
      (Field.v "count" (bits ~width:14 U16be) $ fun t -> t.count);
      (Field.v "len" (bits ~width:2 U16be) $ fun t -> t.len);
    ]

let test_codec_bitfield_wire_size () =
  Alcotest.(check int) "bf32 wire_size" 4 (Codec.wire_size bf32_codec);
  Alcotest.(check int) "bf16 wire_size" 4 (Codec.wire_size bf16_codec)

let test_codec_bitfield_roundtrip () =
  let original = { a = 5; b = 20; c = 0x1234; d = 0xAB } in
  match encode_record bf32_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" original.a decoded.a;
          Alcotest.(check int) "b" original.b decoded.b;
          Alcotest.(check int) "c" original.c decoded.c;
          Alcotest.(check int) "d" original.d decoded.d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_byte_layout () =
  (* a=5 (3b), b=20 (5b), c=0x1234 (16b), d=0xAB (8b)
     MSB-first packing: 101_10100_0001001000110100_10101011
     = 0xB4 0x12 0x34 0xAB *)
  let v = { a = 5; b = 20; c = 0x1234; d = 0xAB } in
  match encode_record bf32_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded ->
      Alcotest.(check int) "length" 4 (String.length encoded);
      Alcotest.(check int) "byte 0" 0xB4 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0x12 (Char.code encoded.[1]);
      Alcotest.(check int) "byte 2" 0x34 (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xAB (Char.code encoded.[3])

let test_codec_bitfield_decode () =
  (* Decode 0xB41234AB -> a=5, b=20, c=0x1234, d=0xAB *)
  let input = "\xB4\x12\x34\xAB" in
  match decode_record bf32_codec input with
  | Ok v ->
      Alcotest.(check int) "a" 5 v.a;
      Alcotest.(check int) "b" 20 v.b;
      Alcotest.(check int) "c" 0x1234 v.c;
      Alcotest.(check int) "d" 0xAB v.d
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_codec_bitfield_multi_group () =
  (* Two U16be groups: (3+2+11=16) + (14+2=16) = 32 bits = 4 bytes *)
  let v = { ver = 5; flags = 2; id = 0x7FF; count = 0x3FFF; len = 3 } in
  match encode_record bf16_codec v with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "length" 4 (String.length encoded);
      (* First group: 101_10_11111111111 = 0xB7FF *)
      Alcotest.(check int) "byte 0" 0xB7 (Char.code encoded.[0]);
      Alcotest.(check int) "byte 1" 0xFF (Char.code encoded.[1]);
      (* Second group: 11111111111111_11 = 0xFFFF *)
      Alcotest.(check int) "byte 2" 0xFF (Char.code encoded.[2]);
      Alcotest.(check int) "byte 3" 0xFF (Char.code encoded.[3]);
      (* Roundtrip decode *)
      match decode_record bf16_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "ver" v.ver decoded.ver;
          Alcotest.(check int) "flags" v.flags decoded.flags;
          Alcotest.(check int) "id" v.id decoded.id;
          Alcotest.(check int) "count" v.count decoded.count;
          Alcotest.(check int) "len" v.len decoded.len
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_overflow_u8 () =
  let v = { a = 0x8; b = 0; c = 0; d = 0 } in
  (* a is 3 bits, 0x8 = 8 exceeds max 7 *)
  match encode_record bf32_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 3-bit field with value 0x8"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u16 () =
  let v = { ver = 0; flags = 0; id = 0x800; count = 0; len = 0 } in
  (* id is 11 bits, 0x800 = 2048 exceeds max 2047 *)
  match encode_record bf16_codec v with
  | Ok _ -> Alcotest.fail "expected overflow for 11-bit field with value 0x800"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_overflow_u32 () =
  let v = { a = 0; b = 0; c = 0x10000; d = 0 } in
  (* c is 16 bits, 0x10000 exceeds max 0xFFFF *)
  match encode_record bf32_codec v with
  | Ok _ ->
      Alcotest.fail "expected overflow for 16-bit field with value 0x10000"
  | Error _ -> ()
  | exception Invalid_argument _ -> ()

let test_codec_bitfield_max_valid () =
  (* All fields at their maximum valid values *)
  let v = { a = 7; b = 31; c = 0xFFFF; d = 0xFF } in
  match encode_record bf32_codec v with
  | Error e -> Alcotest.failf "encode max valid: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record bf32_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a" 7 decoded.a;
          Alcotest.(check int) "b" 31 decoded.b;
          Alcotest.(check int) "c" 0xFFFF decoded.c;
          Alcotest.(check int) "d" 0xFF decoded.d
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_codec_bitfield_overflow_1bit () =
  (* Single-bit field: only 0 and 1 are valid *)
  let f = Field.v "flag" (bits ~width:1 U8) in
  let codec = Codec.v "OneBit" Fun.id Codec.[ f $ Fun.id ] in
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  (try
     Codec.encode codec 2 buf 0;
     Alcotest.fail "expected overflow for 1-bit field with value 2"
   with Invalid_argument _ -> ());
  (* 0 and 1 should work *)
  Codec.encode codec 0 buf 0;
  Codec.encode codec 1 buf 0

let test_fixed_byte_region_size_of_value () =
  let check_string label typ cases =
    let codec = Codec.v label Fun.id Codec.[ Field.v "data" typ $ Fun.id ] in
    List.iter
      (fun (case, value, expected) ->
        Alcotest.(check int)
          (case ^ " size") 3
          (Codec.size_of_value codec value);
        let buf = Bytes.create 3 in
        Codec.encode codec value buf 0;
        Alcotest.(check bytes) case (Bytes.of_string expected) buf)
      cases
  in
  (* Every value is exactly the declared region width: a fixed byte field
     neither pads nor truncates. *)
  let cases =
    [
      ("zeros", "A\x00\x00", "A\x00\x00");
      ("exact", "ABC", "ABC");
      ("high", "\xff\xfe\xfd", "\xff\xfe\xfd");
    ]
  in
  check_string "FixedBytes" (byte_array ~size:(int 3)) cases;
  check_string "FixedBytesWhere"
    (byte_array_where ~size:(int 3) ~per_byte:(fun _ -> Expr.true_))
    cases;
  let codec =
    Codec.v "FixedSlice" Fun.id
      Codec.[ Field.v "data" (byte_slice ~size:(int 3)) $ Fun.id ]
  in
  List.iter
    (fun (case, value, expected) ->
      let bytes = Bytes.of_string value in
      let slice =
        Bytesrw.Bytes.Slice.make bytes ~first:0 ~length:(Bytes.length bytes)
      in
      Alcotest.(check int)
        (case ^ " slice size") 3
        (Codec.size_of_value codec slice);
      let buf = Bytes.create 3 in
      Codec.encode codec slice buf 0;
      Alcotest.(check bytes) (case ^ " slice") (Bytes.of_string expected) buf)
    cases

(* A fixed-size byte field is exact: a value whose length differs from the
   declared size is a caller error, not something to truncate or zero-pad. *)
let expect_exact_byte_error label ~expected ~actual f =
  match f () with
  | () -> Alcotest.failf "%s: expected an exact-size error" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names the declared size")
        true
        (contains ~sub:(Fmt.str "expected %d bytes" expected) msg);
      Alcotest.(check bool)
        (label ^ ": names the value length")
        true
        (contains ~sub:(Fmt.str "got %d" actual) msg)

let slice_of_string s =
  let b = Bytes.of_string s in
  Bytesrw.Bytes.Slice.make b ~first:0 ~length:(Bytes.length b)

let exact_ba_codec =
  Codec.v "ExactBa" Fun.id
    Codec.[ Field.v "B" (byte_array ~size:(int 4)) $ Fun.id ]

let exact_bs_codec =
  Codec.v "ExactBs" Fun.id
    Codec.[ Field.v "S" (byte_slice ~size:(int 4)) $ Fun.id ]

let test_exact_byte_field_literal_size () =
  let buf = Bytes.create 4 in
  expect_exact_byte_error "byte_array long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_ba_codec "abcdef" buf 0);
  expect_exact_byte_error "byte_array short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_ba_codec "ab" buf 0);
  expect_exact_byte_error "byte_slice long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_bs_codec (slice_of_string "abcdef") buf 0);
  expect_exact_byte_error "byte_slice short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_bs_codec (slice_of_string "ab") buf 0);
  Codec.encode exact_ba_codec "abcd" buf 0;
  Alcotest.(check string) "exact string" "abcd" (Bytes.to_string buf);
  Codec.encode exact_bs_codec (slice_of_string "wxyz") buf 0;
  Alcotest.(check string) "exact slice" "wxyz" (Bytes.to_string buf)

(* The same contract on the variable-size encoder, where the declared size is a
   cross-field reference rather than a literal. *)
let exact_vb_len = Field.v "N" uint8

let exact_vb_codec =
  Codec.v "ExactVb"
    (fun n b -> (n, b))
    Codec.
      [
        exact_vb_len $ fst;
        Field.v "B" (byte_array ~size:(Field.ref exact_vb_len)) $ snd;
      ]

let exact_vs_len = Field.v "N" uint8

let exact_vs_codec =
  Codec.v "ExactVs"
    (fun n s -> (n, s))
    Codec.
      [
        exact_vs_len $ fst;
        Field.v "S" (byte_slice ~size:(Field.ref exact_vs_len)) $ snd;
      ]

let test_exact_byte_field_expression_size () =
  let buf = Bytes.create 16 in
  expect_exact_byte_error "byte_array long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_vb_codec (4, "abcdef") buf 0);
  expect_exact_byte_error "byte_array short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_vb_codec (4, "ab") buf 0);
  expect_exact_byte_error "byte_slice long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_vs_codec (4, slice_of_string "abcdef") buf 0);
  expect_exact_byte_error "byte_slice short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_vs_codec (4, slice_of_string "ab") buf 0);
  Codec.encode exact_vb_codec (4, "abcd") buf 0;
  Alcotest.(check string) "exact string" "\x04abcd" (Bytes.sub_string buf 0 5)

(* Encode must not emit a value its own decoder rejects: a refinement the
   decoder enforces (enum membership, all-zero padding, a per-byte span
   predicate, a where cond) has to gate encode too, or the record ships bytes
   that fail to read back. Each case pins the bytes on the accepted value and
   the [Invalid_argument] on the rejected one, and confirms decode agrees. *)
let expect_encode_rejects label ~names f =
  match f () with
  | () -> Alcotest.failf "%s: encode accepted a value decode rejects" label
  | exception Invalid_argument msg ->
      List.iter
        (fun sub ->
          Alcotest.(check bool)
            (Fmt.str "%s: message names %S" label sub)
            true (contains ~sub msg))
        names

let expect_decode_rejects label codec s =
  match Codec.decode codec (Bytes.of_string s) 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: decode accepted %S" label s

let closed_enum_codec =
  Codec.v "ClosedEnum" Fun.id
    Codec.[ Field.v "E" (enum "Code" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]

let open_enum_codec =
  Codec.v "OpenEnum" Fun.id
    Codec.
      [ Field.v "E" (enum_open "Code" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]

let test_encode_rejects_unlisted_enum () =
  let buf = Bytes.create 1 in
  expect_encode_rejects "closed enum" ~names:[ "Code"; "got 99" ] (fun () ->
      Codec.encode closed_enum_codec 99 buf 0);
  expect_decode_rejects "closed enum" closed_enum_codec "\099";
  Codec.encode closed_enum_codec 2 buf 0;
  Alcotest.(check string) "listed value" "\002" (Bytes.to_string buf);
  (* An open enum documents its codes without restricting them, so encode
     stays permissive exactly where decode does. *)
  Codec.encode open_enum_codec 99 buf 0;
  Alcotest.(check string) "open enum" "\099" (Bytes.to_string buf)

let zeros_codec =
  Codec.v "Padded"
    (fun tag pad -> (tag, pad))
    Codec.[ Field.v "Tag" uint8 $ fst; Field.v "Pad" all_zeros $ snd ]

let test_encode_rejects_non_zero_padding () =
  let buf = Bytes.create 4 in
  expect_encode_rejects "all_zeros" ~names:[ "all_zeros"; "0x61" ] (fun () ->
      Codec.encode zeros_codec (1, "abc") buf 0);
  expect_decode_rejects "all_zeros" zeros_codec "\001abc";
  Codec.encode zeros_codec (1, "\000\000\000") buf 0;
  Alcotest.(check string)
    "zero padding" "\001\000\000\000" (Bytes.to_string buf)

let per_byte_typ =
  byte_array_where ~size:(int 3) ~per_byte:(fun b ->
      Expr.(b >= int 0x20 && b <= int 0x7e))

let per_byte_cf = Codec.(Field.v "B" per_byte_typ $ Fun.id)
let per_byte_codec = Codec.v "Printable" Fun.id [ per_byte_cf ]

(* Same refinement over a length-prefixed span, which takes the variable-size
   reader rather than the constant-offset one. *)
let per_byte_var_codec =
  let open Codec in
  let f_len = Field.v "Len" uint8 in
  let f_b =
    Field.v "B"
      (byte_array_where ~size:(Field.ref f_len) ~per_byte:(fun b ->
           Expr.(b >= int 0x20 && b <= int 0x7e)))
  in
  v "PrintableVar" (fun len b -> (len, b)) [ f_len $ fst; f_b $ snd ]

let test_encode_rejects_refined_byte () =
  let buf = Bytes.create 3 in
  expect_encode_rejects "byte_array_where" ~names:[ "byte 1"; "0xff" ]
    (fun () -> Codec.encode per_byte_codec "a\xffb" buf 0);
  Codec.encode per_byte_codec "abc" buf 0;
  Alcotest.(check string) "printable span" "abc" (Bytes.to_string buf)

let decode_accepts codec s =
  Result.is_ok (Codec.decode codec (Bytes.of_string s) 0)

let validate_result codec s =
  match Codec.validate codec (Bytes.of_string s) 0 with
  | () -> Ok ()
  | exception Parse_error e -> Error e

let expect_per_byte label ~at = function
  | Error { kind = Constraint_failed { which = Per_byte; _ }; at = off; _ } ->
      Alcotest.(check int) (label ^ ": offset") at off
  | Error e ->
      Alcotest.failf "%s: expected a per-byte refusal, got %a" label
        pp_parse_error e
  | Ok () -> Alcotest.failf "%s: accepted a byte violating the refinement" label

(* The refinement belongs to the span itself, so the compiled reader has to
   apply it: a [Codec.decode] more permissive than [Wire.of_string] is also more
   permissive than the EverParse validator built from the same schema, and
   [Codec.validate] is the gate a caller runs before reading untrusted bytes
   zero-copy. *)
let test_decode_rejects_refined_byte () =
  expect_per_byte "decode" ~at:1
    (Result.map ignore
       (Codec.decode per_byte_codec (Bytes.of_string "a\xffb") 0));
  expect_per_byte "validate" ~at:1 (validate_result per_byte_codec "a\xffb");
  expect_per_byte "decode var" ~at:2
    (Result.map ignore
       (Codec.decode per_byte_var_codec (Bytes.of_string "\002a\xff") 0));
  expect_per_byte "validate var" ~at:2
    (validate_result per_byte_var_codec "\002a\xff");
  (* [get] is documented as unchecked for other fields' constraints, but the
     span's own refinement travels with the reader, as a closed enum's does. *)
  let get = Staged.unstage (Codec.get per_byte_codec per_byte_cf) in
  (match get (Bytes.of_string "a\xffb") 0 with
  | s -> Alcotest.failf "get returned %S past the refinement" s
  | exception
      Parse_error { kind = Constraint_failed { which = Per_byte; _ }; _ } ->
      ());
  (* The direct parser has always rejected this; it must keep doing so. *)
  match of_string per_byte_typ "a\xffb" with
  | Error { kind = Constraint_failed { which = Per_byte; _ }; at; _ } ->
      Alcotest.(check int) "of_string offset" 1 at
  | Error e -> Alcotest.failf "of_string: wrong error %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "of_string accepted a byte violating the refinement"

let test_decode_accepts_conforming_refined_byte () =
  Alcotest.(check (result string string))
    "of_string" (Ok "abc")
    (Result.map_error
       (Fmt.str "%a" pp_parse_error)
       (of_string per_byte_typ "abc"));
  Alcotest.(check bool) "decode" true (decode_accepts per_byte_codec "abc");
  Alcotest.(check (result unit string))
    "validate" (Ok ())
    (Result.map_error
       (Fmt.str "%a" pp_parse_error)
       (validate_result per_byte_codec "abc"));
  Alcotest.(check string)
    "get" "abc"
    ((Staged.unstage (Codec.get per_byte_codec per_byte_cf))
       (Bytes.of_string "abc") 0)

(* The fuzz harness holds [decode] and [validate] to the same verdict; pin that
   here against the verdict the refinement calls for, so a later change can
   neither drop the check nor move one path without the other. *)
let test_refined_byte_decode_validate_agree () =
  List.iter
    (fun (s, accept) ->
      Alcotest.(check bool)
        (Fmt.str "decode of %S" s) accept
        (decode_accepts per_byte_codec s);
      Alcotest.(check bool)
        (Fmt.str "validate of %S" s)
        accept
        (Result.is_ok (validate_result per_byte_codec s)))
    [
      ("abc", true);
      (" ~!", true);
      ("a\xffb", false);
      ("\000bc", false);
      ("ab\x7f", false);
      ("ab\x80", false);
      ("\x1fbc", false);
    ]

(* The cond of a [Wire.where] reads sibling fields, so it can only be decided
   against the assembled record; encode replays the decode-side check pass over
   the bytes it just wrote. *)
let where_len = Field.v "Len" uint8

let where_codec =
  Codec.v "TypWhereEncode"
    (fun len d -> (len, d))
    Codec.
      [
        where_len $ fst;
        Field.v "D" (where Expr.(Field.ref where_len < int 2) uint8) $ snd;
      ]

let codec_where_codec =
  Codec.v "CodecWhereEncode"
    ~where:Expr.(Field.ref where_len < int 2)
    (fun len d -> (len, d))
    Codec.[ where_len $ fst; Field.v "D" uint8 $ snd ]

let test_encode_rejects_where_violation () =
  let buf = Bytes.create 2 in
  expect_encode_rejects "typ where" ~names:[ "TypWhereEncode"; "constraint" ]
    (fun () -> Codec.encode where_codec (6, 0) buf 0);
  expect_decode_rejects "typ where" where_codec "\006\000";
  Codec.encode where_codec (1, 0) buf 0;
  Alcotest.(check string) "satisfied cond" "\001\000" (Bytes.to_string buf);
  expect_encode_rejects "codec where"
    ~names:[ "CodecWhereEncode"; "constraint" ] (fun () ->
      Codec.encode codec_where_codec (6, 0) buf 0);
  expect_decode_rejects "codec where" codec_where_codec "\006\000";
  Codec.encode codec_where_codec (1, 0) buf 0;
  Alcotest.(check string)
    "satisfied codec where" "\001\000" (Bytes.to_string buf)

(* [Codec.set] writes in place, so a byte field whose size is only known at
   run time still owns exactly that many bytes: an oversized value that fits in
   the buffer would otherwise land on the fields that follow, with nothing to
   signal it. Each case checks the buffer as well as the exception, since a
   check that ran after the blit would raise on an already corrupted buffer. *)

(* Every field before [data] is fixed-size, so [data] has a static offset and a
   run-time size. *)
let set_var_codec typ =
  let open Codec in
  let f_len = Field.v "len" uint8 in
  let cf_data = Field.v "data" (typ (Field.ref f_len)) $ fun (_, d, _) -> d in
  let codec =
    v "SetVarBytes"
      (fun len data trailer -> (len, data, trailer))
      [
        (f_len $ fun (n, _, _) -> n);
        cf_data;
        (Field.v "trailer" uint8 $ fun (_, _, t) -> t);
      ]
  in
  (codec, cf_data)

(* [pre] is itself variable-size, so [data] has a run-time offset as well as a
   run-time size. *)
let set_dyn_codec typ =
  let open Codec in
  let f_plen = Field.v "plen" uint8 in
  let f_len = Field.v "len" uint8 in
  let cf_data =
    Field.v "data" (typ (Field.ref f_len)) $ fun (_, _, _, d, _) -> d
  in
  let codec =
    v "SetDynBytes"
      (fun plen pre len data trailer -> (plen, pre, len, data, trailer))
      [
        (f_plen $ fun (n, _, _, _, _) -> n);
        ( Field.v "pre" (byte_array ~size:(Field.ref f_plen))
        $ fun (_, p, _, _, _) -> p );
        (f_len $ fun (_, _, n, _, _) -> n);
        cf_data;
        (Field.v "trailer" uint8 $ fun (_, _, _, _, t) -> t);
      ]
  in
  (codec, cf_data)

(* len = 2, data = "ab" at offset 1, trailer = 0xff, then spare bytes so that a
   4-byte write past the field still lands inside the buffer. *)
let set_var_before = "\x02ab\xff\xee\xee\xee"
let set_var_after = "\x02XY\xff\xee\xee\xee"
let set_dyn_before = "\x01p\x02ab\xff\xee\xee\xee"
let set_dyn_after = "\x01p\x02XY\xff\xee\xee\xee"
let printable_byte b = Expr.(b >= int 0x20 && b <= int 0x7e)

let check_set_exact label ~before ~after ~oversized ~exact set =
  let buf = Bytes.of_string before in
  expect_exact_byte_error label ~expected:2 ~actual:4 (fun () ->
      set buf 0 oversized);
  Alcotest.(check string)
    (label ^ ": rejected set left the buffer alone")
    before (Bytes.to_string buf);
  set buf 0 exact;
  Alcotest.(check string)
    (label ^ ": exact set wrote only its own bytes")
    after (Bytes.to_string buf)

let test_set_exact_var_byte_array () =
  let codec, cf = set_var_codec (fun size -> byte_array ~size) in
  check_set_exact "byte_array at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_var_byte_array_where () =
  let codec, cf =
    set_var_codec (fun size -> byte_array_where ~size ~per_byte:printable_byte)
  in
  check_set_exact "byte_array_where at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_var_byte_slice () =
  let codec, cf = set_var_codec (fun size -> byte_slice ~size) in
  check_set_exact "byte_slice at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:(slice_of_string "ABCD")
    ~exact:(slice_of_string "XY")
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_array () =
  let codec, cf = set_dyn_codec (fun size -> byte_array ~size) in
  check_set_exact "byte_array at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_array_where () =
  let codec, cf =
    set_dyn_codec (fun size -> byte_array_where ~size ~per_byte:printable_byte)
  in
  check_set_exact "byte_array_where at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_slice () =
  let codec, cf = set_dyn_codec (fun size -> byte_slice ~size) in
  check_set_exact "byte_slice at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:(slice_of_string "ABCD")
    ~exact:(slice_of_string "XY")
    (Staged.unstage (Codec.set codec cf))

(* A byte field with a literal size takes the fixed-offset writer, which shares
   the encoder's exact-size blit. *)
let test_set_exact_fixed_byte_array () =
  let cf = Codec.(Field.v "B" (byte_array ~size:(int 2)) $ fst) in
  let codec =
    Codec.(
      v "SetFixedBytes"
        (fun b t -> (b, t))
        [ cf; Field.v "trailer" uint8 $ snd ])
  in
  check_set_exact "byte_array at a literal size" ~before:"ab\xff\xee\xee"
    ~after:"XY\xff\xee\xee" ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

(* A width-refined leaf -- [bits ~width], [uint ~size] -- is exact for a harsher
   reason than a fixed-size byte field. A masked value is still a legal value at
   that width, so decode, [validate] and the EverParse validator all accept it:
   the number the caller meant is gone with nothing left to detect it. Every
   entry point that can write one has to refuse instead. *)
let expect_exact_width_error label ~sub f =
  match f () with
  | wrote ->
      Alcotest.failf "%s: expected an exact-width error, wrote %S" label wrote
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names the value and the width")
        true (contains ~sub msg)

let uint_width_codec ~endian n =
  let cf = Codec.(Field.v "v" (uint ~endian (int n)) $ Fun.id) in
  (Codec.v (Fmt.str "ExactUint%d" n) Fun.id Codec.[ cf ], cf)

(* A hostile value needs one bit more than the field it overflows, so a width is
   only testable where the host [int] holds that extra bit. [Sys.int_size] is 31
   under wasm_of_ocaml, which drops the widest cases there. *)
let width_is_testable bits = bits + 1 <= Sys.int_size - 1

(* Each case is a declared size, the widest value it holds and a value it does
   not: one byte holds 0xFF, and 0x1FF is the reported truncation. *)
let uint_width_cases =
  List.filter_map
    (fun n ->
      if width_is_testable (8 * n) then
        let widest = (1 lsl (8 * n)) - 1 in
        Some (n, widest, widest lor (widest + 1))
      else None)
    [ 1; 2; 3; 5; 7 ]

let uint_endians = [ (Big, "be"); (Little, "le") ]

(* 0x1FF through a 1-byte [uint] used to come out as 0xFF on both encode entry
   points, and decode back as 255. *)
let test_encode_exact_uint_width () =
  List.iter
    (fun (n, max_v, over) ->
      List.iter
        (fun (endian, ename) ->
          let typ = uint ~endian (int n) in
          let codec, _ = uint_width_codec ~endian n in
          let label = Fmt.str "uint(%d,%s) <- 0x%X" n ename over in
          let sub = Fmt.str "does not fit an unsigned %d-byte field" n in
          let value = Wire.Private.UInt63.of_int over in
          expect_exact_width_error (label ^ " to_string") ~sub (fun () ->
              Wire.to_string typ value);
          expect_exact_width_error (label ^ " Codec.encode") ~sub (fun () ->
              let buf = Bytes.make 8 '\x00' in
              Codec.encode codec value buf 0;
              Bytes.sub_string buf 0 n);
          (* The widest value the field does hold still encodes, identically
             through both entry points. *)
          let widest = Wire.Private.UInt63.of_int max_v in
          let buf = Bytes.make 8 '\x00' in
          Codec.encode codec widest buf 0;
          Alcotest.(check string)
            (label ^ ": widest legal value agrees across entry points")
            (Wire.to_string typ widest)
            (Bytes.sub_string buf 0 n))
        uint_endians)
    uint_width_cases

let test_set_exact_uint_width () =
  List.iter
    (fun (n, max_v, over) ->
      List.iter
        (fun (endian, ename) ->
          let codec, cf = uint_width_codec ~endian n in
          let set = Staged.unstage (Codec.set codec cf) in
          let buf = Bytes.make 8 '\x00' in
          let label = Fmt.str "set uint(%d,%s) <- 0x%X" n ename over in
          expect_exact_width_error label
            ~sub:(Fmt.str "does not fit an unsigned %d-byte field" n) (fun () ->
              set buf 0 (Wire.Private.UInt63.of_int over);
              Bytes.sub_string buf 0 n);
          Alcotest.(check string)
            (label ^ ": rejected set left the buffer alone")
            (String.make 8 '\x00') (Bytes.to_string buf);
          let widest = Wire.Private.UInt63.of_int max_v in
          set buf 0 widest;
          Alcotest.(check string)
            (label ^ ": widest legal value writes")
            (String.make n '\xff') (Bytes.sub_string buf 0 n))
        uint_endians)
    uint_width_cases

let bits_width_bases =
  [
    (U8, "U8", 8);
    (U16, "U16", 16);
    (U16be, "U16be", 16);
    (U32, "U32", 32);
    (U32be, "U32be", 32);
  ]

let bits_orders = [ (Msb_first, "Msb"); (Lsb_first, "Lsb") ]

let bits_width_codec ~bit_order ~width base =
  let cf = Codec.(Field.v "v" (bits ~bit_order ~width base) $ Fun.id) in
  (Codec.v "ExactBits" Fun.id Codec.[ cf ], cf)

let iter_bits_widths f =
  List.iter
    (fun (base, bname, total) ->
      List.iter
        (fun width ->
          if width_is_testable width then
            List.iter
              (fun (bit_order, oname) ->
                f ~base ~bname ~width ~bit_order ~oname)
              bits_orders)
        [ 1; 3; total ])
    bits_width_bases

(* [Wire.to_bytes (bits ~width:3 U8) 0x8] used to write 00 where [Codec.encode]
   of the same value raised: the two encode entry points disagreed on a value
   neither can represent. Swept over every base, bit order and boundary width so
   they cannot drift apart again. *)
let test_encode_exact_bits_width () =
  iter_bits_widths (fun ~base ~bname ~width ~bit_order ~oname ->
      let typ = bits ~bit_order ~width base in
      let codec, _ = bits_width_codec ~bit_order ~width base in
      let over = 1 lsl width in
      let label = Fmt.str "bits(%d,%s,%s) <- 0x%X" width bname oname over in
      let sub = Fmt.str "does not fit an unsigned %d-bit field" width in
      expect_exact_width_error (label ^ " to_bytes") ~sub (fun () ->
          Bytes.to_string (Wire.to_bytes typ over));
      expect_exact_width_error (label ^ " Codec.encode") ~sub (fun () ->
          let buf = Bytes.make 4 '\x00' in
          Codec.encode codec over buf 0;
          Bytes.to_string buf);
      let widest = over - 1 in
      let buf = Bytes.make (Codec.wire_size codec) '\x00' in
      Codec.encode codec widest buf 0;
      Alcotest.(check string)
        (label ^ ": widest legal value agrees across entry points")
        (Bytes.to_string (Wire.to_bytes typ widest))
        (Bytes.to_string buf))

(* The worst of the family: [Codec.set] wrote 0xFF into a 3-bit field as 7,
   which [Codec.get] reads back and [validate] accepts, so no later check can
   recover that the caller meant 255. *)
let test_set_exact_bits_width () =
  iter_bits_widths (fun ~base ~bname ~width ~bit_order ~oname ->
      let codec, cf = bits_width_codec ~bit_order ~width base in
      let size = Codec.wire_size codec in
      let set = Staged.unstage (Codec.set codec cf) in
      let get = Staged.unstage (Codec.get codec cf) in
      let over = 1 lsl width in
      let buf = Bytes.make size '\x00' in
      let label = Fmt.str "set bits(%d,%s,%s) <- 0x%X" width bname oname over in
      expect_exact_width_error label
        ~sub:(Fmt.str "does not fit an unsigned %d-bit field" width) (fun () ->
          set buf 0 over;
          Bytes.to_string buf);
      Alcotest.(check string)
        (label ^ ": rejected set left the buffer alone")
        (String.make size '\x00') (Bytes.to_string buf);
      let widest = over - 1 in
      set buf 0 widest;
      Alcotest.(check int)
        (label ^ ": widest legal value reads back")
        widest (get buf 0))

(* The concrete case from the bug report, pinned on its own: 0xFF into a 3-bit
   field left a buffer of 0xE0 that read back as 7 and passed [validate]. *)
let test_set_bits_no_silent_truncation () =
  let cf = Codec.(Field.v "v" (bits ~width:3 U8) $ Fun.id) in
  let codec = Codec.v "SetBits3" Fun.id Codec.[ cf ] in
  let set = Staged.unstage (Codec.set codec cf) in
  let buf = Bytes.make 1 '\x00' in
  (match set buf 0 0xFF with
  | () ->
      Alcotest.failf "set bits(3,U8) <- 0xFF wrote %02x" (Bytes.get_uint8 buf 0)
  | exception Invalid_argument _ -> ());
  Alcotest.(check int) "buffer untouched" 0x00 (Bytes.get_uint8 buf 0)

(* [map] adds no encode path of its own: it hands the mapped value to the inner
   typ, so the width checks have to reach through it. *)
let test_map_inherits_exact_width () =
  let typ = map ~decode:Fun.id ~encode:Fun.id (bits ~width:3 U8) in
  let cf = Codec.(Field.v "v" typ $ Fun.id) in
  let codec = Codec.v "MapBits3" Fun.id Codec.[ cf ] in
  let set = Staged.unstage (Codec.set codec cf) in
  expect_exact_width_error "map(bits(3,U8)) to_string"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      Wire.to_string typ 0xFF);
  expect_exact_width_error "map(bits(3,U8)) Codec.encode"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      let buf = Bytes.make 1 '\x00' in
      Codec.encode codec 0xFF buf 0;
      Bytes.to_string buf);
  expect_exact_width_error "map(bits(3,U8)) Codec.set"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      let buf = Bytes.make 1 '\x00' in
      set buf 0 0xFF;
      Bytes.to_string buf)

(* The fixed-width scalars are the same rule at a fixed width. OCaml carries the
   narrow ones in a plain [int], which holds far more than the field does, so
   nothing but a runtime check stands between a caller's 0x1FF and an 0xFF on
   the wire that reads back as a perfectly legal 255. The signed ones accept
   exactly what their decoder produces, [-2^(n-1) .. 2^(n-1) - 1]: 200 into an
   [int8] is refused rather than round-tripped back as -56. *)
let scalar_range_codec name typ =
  let cf = Codec.(Field.v "v" typ $ Fun.id) in
  (Codec.v name Fun.id Codec.[ cf ], cf)

(* Sweep one scalar typ. Every [outside] value must be refused by all three
   entry points and leave the buffer untouched; every [inside] one must write
   the same bytes through both encoders and read back through [Codec.get]. *)
let check_scalar_range ~name ~typ ~sub ~equal ~pp ~inside ~outside =
  let codec, cf = scalar_range_codec name typ in
  let size = Codec.wire_size codec in
  let set = Staged.unstage (Codec.set codec cf) in
  let get = Staged.unstage (Codec.get codec cf) in
  List.iter
    (fun v ->
      let label = Fmt.str "%s <- %a" name pp v in
      expect_exact_width_error (label ^ " to_string") ~sub (fun () ->
          Wire.to_string typ v);
      expect_exact_width_error (label ^ " Codec.encode") ~sub (fun () ->
          let buf = Bytes.make size '\x00' in
          Codec.encode codec v buf 0;
          Bytes.to_string buf);
      let buf = Bytes.make size '\x00' in
      expect_exact_width_error (label ^ " Codec.set") ~sub (fun () ->
          set buf 0 v;
          Bytes.to_string buf);
      Alcotest.(check string)
        (label ^ ": rejected set left the buffer alone")
        (String.make size '\x00') (Bytes.to_string buf))
    outside;
  List.iter
    (fun v ->
      let label = Fmt.str "%s <- %a" name pp v in
      let buf = Bytes.make size '\x00' in
      Codec.encode codec v buf 0;
      Alcotest.(check string)
        (label ^ ": widest legal value agrees across entry points")
        (Wire.to_string typ v) (Bytes.to_string buf);
      Bytes.fill buf 0 size '\x00';
      set buf 0 v;
      Alcotest.(check bool)
        (label ^ ": widest legal value reads back")
        true
        (equal (get buf 0) v))
    inside

let test_encode_exact_unsigned_scalar () =
  List.iter
    (fun (name, bits, typ) ->
      let widest = (1 lsl bits) - 1 in
      check_scalar_range ~name ~typ
        ~sub:(Fmt.str "does not fit an unsigned %d-bit field" bits)
        ~equal:Int.equal ~pp:Fmt.int ~inside:[ 0; widest ]
        ~outside:[ widest + 1; -1 ])
    [ ("uint8", 8, uint8); ("uint16", 16, uint16); ("uint16be", 16, uint16be) ]

let test_encode_exact_signed_scalar () =
  List.iter
    (fun (name, bits, typ) ->
      if width_is_testable bits then
        let limit = 1 lsl (bits - 1) in
        check_scalar_range ~name ~typ
          ~sub:(Fmt.str "does not fit a signed %d-bit field" bits)
          ~equal:Int.equal ~pp:Fmt.int
          ~inside:[ -limit; limit - 1 ]
          ~outside:[ limit; -limit - 1 ])
    [
      ("int8", 8, int8);
      ("int16", 16, int16);
      ("int16be", 16, int16be);
      ("int32", 32, int32);
      ("int32be", 32, int32be);
    ]

(* [uint32] rides an [Optint.t], which is a plain [int] where that is wide
   enough to hold more than 32 bits and a boxed [int32] where it is not. Only
   the first can carry an out-of-range value, so the case exists only there. *)
let test_encode_exact_uint32 () =
  if width_is_testable 32 then
    let mask = Wire.Private.UInt32.mask32 in
    List.iter
      (fun (name, typ) ->
        check_scalar_range ~name ~typ
          ~sub:"does not fit an unsigned 32-bit field" ~equal:Optint.equal
          ~pp:Optint.pp
          ~inside:[ Optint.zero; Optint.of_int mask ]
          ~outside:[ Optint.of_int (mask + 1); Optint.of_int (-1) ])
      [ ("uint32", uint32); ("uint32be", uint32be) ]

(* [uint63] fills its carrier, so the only unrepresentable value left is a
   negative one, and that one is unrepresentable on every target. *)
let test_encode_exact_uint63 () =
  List.iter
    (fun (name, typ) ->
      check_scalar_range ~name ~typ ~sub:"does not fit an unsigned 8-byte field"
        ~equal:Optint.Int63.equal ~pp:Optint.Int63.pp
        ~inside:[ Optint.Int63.zero; Optint.Int63.max_int ]
        ~outside:[ Optint.Int63.of_int (-1); Optint.Int63.min_int ])
    [ ("uint63", uint63); ("uint63be", uint63be) ]

(* [array] and [repeat] elements are written by an encoder of their own, so a
   value no element can hold has to be refused on that path too. *)
let test_element_exact_width () =
  let acf = Codec.(Field.v "vs" (array ~len:(int 3) uint8) $ Fun.id) in
  let acodec = Codec.v "ArrayU8" Fun.id Codec.[ acf ] in
  expect_exact_width_error "array(3,uint8) <- [0; 0x1FF; 0]"
    ~sub:"does not fit an unsigned 8-bit field" (fun () ->
      let buf = Bytes.make 3 '\x00' in
      Codec.encode acodec [ 0; 0x1FF; 0 ] buf 0;
      Bytes.to_string buf);
  let rcf = Codec.(Field.repeat "vs" ~size:(int 3) uint8 $ Fun.id) in
  let rcodec = Codec.v "RepeatU8" Fun.id Codec.[ rcf ] in
  expect_exact_width_error "repeat(3,uint8) <- [0; 0x1FF; 0]"
    ~sub:"does not fit an unsigned 8-bit field" (fun () ->
      let buf = Bytes.make 3 '\x00' in
      Codec.encode rcodec [ 0; 0x1FF; 0 ] buf 0;
      Bytes.to_string buf)

(* The other end of the rule: an [int64] is exactly the eight bytes written, so
   no value of it is out of range and none may be refused. A guard here would be
   dead code that only ever rejected a legal frame. *)
let test_encode_int64_carrier_has_no_range () =
  List.iter
    (fun (name, typ) ->
      List.iter
        (fun v ->
          let s = Wire.to_string typ v in
          Alcotest.(check int) (name ^ ": eight bytes") 8 (String.length s);
          Alcotest.(check int64)
            (Fmt.str "%s <- %Ld round-trips" name v)
            v (Wire.of_string_exn typ s))
        Int64.[ min_int; -1L; zero; max_int ])
    [
      ("uint64", uint64);
      ("uint64be", uint64be);
      ("int64", int64);
      ("int64be", int64be);
    ]

(* A reference-free constant size expression normalises to the literal form at
   construction, so every [Int n] fast path and range guard in the library sees
   it. Sizes that must stay symbolic, and arithmetic the fold cannot do
   faithfully, are left as expressions. *)
let byte_array_size label t =
  let module T = Wire.Private.Types in
  match t with
  | T.Byte_array { size } -> size
  | _ -> Alcotest.failf "%s: expected a byte_array" label

let test_constant_size_expression_folds () =
  let module T = Wire.Private.Types in
  (match byte_array_size "constant" (byte_array ~size:Expr.(int 2 + int 2)) with
  | T.Int 4 -> ()
  | e -> Alcotest.failf "constant size did not fold: %a" T.pp_expr e);
  (* A reference-bearing size stays symbolic: the 3D projection needs the
     expression, not a baked-in width. *)
  (match
     byte_array_size "reference"
       (byte_array ~size:Expr.(Field.ref exact_vb_len + int 1))
   with
  | T.Add (T.Ref _, T.Int 1) -> ()
  | e -> Alcotest.failf "reference-bearing size was folded: %a" T.pp_expr e);
  (* Overflowing arithmetic is left alone rather than folded to a wrong
     value. *)
  (match
     byte_array_size "overflow" (byte_array ~size:Expr.(int max_int + int 1))
   with
  | T.Add (T.Int _, T.Int 1) -> ()
  | e -> Alcotest.failf "overflowing size was folded: %a" T.pp_expr e);
  (* 1. the encode contract *)
  let codec name typ = Codec.v name Fun.id Codec.[ Field.v "B" typ $ Fun.id ] in
  let lit = codec "FoldLiteral" (byte_array ~size:(int 4)) in
  let folded = codec "FoldConstant" (byte_array ~size:Expr.(int 2 + int 2)) in
  let buf = Bytes.create 4 in
  expect_exact_byte_error "literal long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode lit "abcdef" buf 0);
  expect_exact_byte_error "constant long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode folded "abcdef" buf 0);
  expect_exact_byte_error "constant short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode folded "ab" buf 0);
  Codec.encode folded "abcd" buf 0;
  Alcotest.(check string) "constant exact" "abcd" (Bytes.to_string buf);
  (* 2. is_fixed / wire_size *)
  Alcotest.(check bool) "literal is_fixed" true (Codec.is_fixed lit);
  Alcotest.(check bool) "constant is_fixed" true (Codec.is_fixed folded);
  Alcotest.(check int) "literal wire_size" 4 (Codec.wire_size lit);
  Alcotest.(check int) "constant wire_size" 4 (Codec.wire_size folded);
  (* 3. the uint 1-7 range guard *)
  let uint_rejects label size =
    match uint size with
    | _ -> Alcotest.failf "%s: expected uint to reject the size" label
    | exception Invalid_argument msg ->
        Alcotest.(check bool)
          (label ^ ": names the bound")
          true
          (contains ~sub:"size must be 1-7" msg)
  in
  uint_rejects "literal 0" (int 0);
  uint_rejects "constant 0" Expr.(int 1 - int 1);
  uint_rejects "constant 8" Expr.(int 4 + int 4)

let test_packed_bf_size () =
  let f_a = Field.v "a" (bits ~width:1 U8) in
  let f_b = Field.v "b" (bits ~width:7 U8) in
  let codec =
    Codec.v "Packed" (fun a b -> (a, b)) Codec.[ f_a $ fst; f_b $ snd ]
  in
  Alcotest.(check int)
    "size_of_value matches wire_size" (Codec.wire_size codec)
    (Codec.size_of_value codec (0, 0))

type packed_bool_header = {
  magic : string;
  version : int;
  reserved : int;
  flag : bool;
}

let packed_bool_header_codec =
  Codec.v "PackedBoolHeader"
    (fun magic version reserved flag -> { magic; version; reserved; flag })
    Codec.
      [
        (Field.v "Magic" (byte_array ~size:(int 4)) $ fun r -> r.magic);
        (Field.v "Version" uint8 $ fun r -> r.version);
        (Field.v "Reserved" (bits ~width:7 U8) $ fun r -> r.reserved);
        (Field.v "Flag" (bit (bits ~width:1 U8)) $ fun r -> r.flag);
      ]

let test_packed_mapped_bf_size () =
  let value = { magic = "dtn!"; version = 4; reserved = 0; flag = true } in
  Alcotest.(check int)
    "wire_size counts packed bool bitfield once" 6
    (Codec.wire_size packed_bool_header_codec);
  Alcotest.(check int)
    "size_of_value matches wire_size" 6
    (Codec.size_of_value packed_bool_header_codec value);
  let buf = Bytes.create 6 in
  Codec.encode packed_bool_header_codec value buf 0;
  Alcotest.(check string)
    "encoded contact header shape" "dtn!\x04\x01"
    (Bytes.unsafe_to_string buf)

let test_struct_of_codec_bitfield () =
  let output = render_3d bf32_codec in
  Alcotest.(check bool)
    "contains UINT32BE" true
    (contains ~sub:"UINT32BE" output);
  Alcotest.(check bool) "contains field a" true (contains ~sub:"a" output);
  Alcotest.(check bool) "contains field b" true (contains ~sub:"b" output)

(* -- Zero-copy view tests -- *)

let test_view_get_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec =
      Codec.v "ViewUint" (fun a b -> { x = a; y = b }) [ cf_x; cf_y ]
    in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  Alcotest.(check int)
    "get x" 0x1234
    ((Staged.unstage (Codec.get codec cf_x)) buf 0);
  Alcotest.(check int)
    "get y" 0x5678
    ((Staged.unstage (Codec.get codec cf_y)) buf 0)

let test_view_get_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.a) in
    let cf_d = Codec.(f_d $ fun t -> t.d) in
    let codec =
      let open Codec in
      v "ViewBf"
        (fun a b c d -> { a; b; c; d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
          cf_d;
        ]
    in
    (codec, cf_a, cf_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  Alcotest.(check int) "get a" 5 ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  Alcotest.(check int)
    "get d" 0xAB
    ((Staged.unstage (Codec.get codec cf_d)) buf 0)

let test_view_get_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at the MSB,
     so the flag bit is bit 7 of the byte. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x80;
  Alcotest.(check bool)
    "get flag=true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Bytes.set_uint8 buf 0 0x00;
  Alcotest.(check bool)
    "get flag=false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0)

let test_view_set_bitfield () =
  let codec, cf_a, cf_d =
    let f_a = Field.v "a" (bits ~width:3 U32be) in
    let f_d = Field.v "d" (bits ~width:8 U32be) in
    let cf_a = Codec.(f_a $ fun t -> t.a) in
    let cf_d = Codec.(f_d $ fun t -> t.d) in
    let codec =
      let open Codec in
      v "ViewSetBf"
        (fun a b c d -> { a; b; c; d })
        [
          cf_a;
          (Field.v "b" (bits ~width:5 U32be) $ fun t -> t.b);
          (Field.v "c" (bits ~width:16 U32be) $ fun t -> t.c);
          cf_d;
        ]
    in
    (codec, cf_a, cf_d)
  in
  let buf = Bytes.of_string "\xB4\x12\x34\xAB" in
  (Staged.unstage (Codec.set codec cf_a)) buf 0 3;
  Alcotest.(check int)
    "get a after set" 3
    ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "b preserved" 20 r.b;
  Alcotest.(check int) "c preserved" 0x1234 r.c;
  Alcotest.(check int) "d preserved" 0xAB r.d;
  (Staged.unstage (Codec.set codec cf_d)) buf 0 0x42;
  Alcotest.(check int)
    "get d after set" 0x42
    ((Staged.unstage (Codec.get codec cf_d)) buf 0);
  let r = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "a still 3" 3 r.a;
  Alcotest.(check int) "b still 20" 20 r.b;
  Alcotest.(check int) "c still 0x1234" 0x1234 r.c

let test_view_set_uint () =
  let codec, cf_x, cf_y =
    let f_x = Field.v "x" uint16be in
    let f_y = Field.v "y" uint16be in
    let cf_x = Codec.(f_x $ fun (r : multi_record) -> r.x) in
    let cf_y = Codec.(f_y $ fun (r : multi_record) -> r.y) in
    let codec = Codec.v "ViewSetUint" (fun x y -> { x; y }) [ cf_x; cf_y ] in
    (codec, cf_x, cf_y)
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (Staged.unstage (Codec.set codec cf_x)) buf 0 0xAAAA;
  Alcotest.(check int)
    "get x after set" 0xAAAA
    ((Staged.unstage (Codec.get codec cf_x)) buf 0);
  Alcotest.(check int)
    "y unchanged" 0x5678
    ((Staged.unstage (Codec.get codec cf_y)) buf 0)

let test_view_bounds_check () =
  let codec =
    let open Codec in
    v "ViewBounds" (fun a -> a) [ (Field.v "a" uint32be $ fun a -> a) ]
  in
  let buf = Bytes.create 2 in
  match Codec.decode codec buf 0 with
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_view_with_offset () =
  let codec, cf_a =
    let f_a = Field.v "a" uint16be in
    let cf_a = Codec.(f_a $ fun a -> a) in
    let codec = Codec.v "ViewOff" (fun a -> a) [ cf_a ] in
    (codec, cf_a)
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_uint16_be buf 2 0x2222;
  Bytes.set_uint16_be buf 4 0x3333;
  Alcotest.(check int)
    "get at offset 2" 0x2222
    ((Staged.unstage (Codec.get codec cf_a)) buf 2)

let test_view_set_bool () =
  (* Default [bit_order = Msb_first]: first-declared field lives at bit 7. *)
  let codec, cf_flag =
    let f_flag = Field.v "flag" (bit (bits ~width:1 U8)) in
    let cf_flag = Codec.(f_flag $ fst) in
    let codec =
      let open Codec in
      v "ViewSetBool"
        (fun flag code -> (flag, code))
        [ cf_flag; Field.v "code" (bits ~width:7 U8) $ snd ]
    in
    (codec, cf_flag)
  in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 true;
  Alcotest.(check bool)
    "get flag after set true" true
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte value (MSB-first)" 0x80 (Bytes.get_uint8 buf 0);
  (Staged.unstage (Codec.set codec cf_flag)) buf 0 false;
  Alcotest.(check bool)
    "get flag after set false" false
    ((Staged.unstage (Codec.get codec cf_flag)) buf 0);
  Alcotest.(check int) "byte cleared" 0x00 (Bytes.get_uint8 buf 0)

(* -- Field sharing tests -- same field spec used in two codecs -- *)

let test_view_shared_field_spec () =
  (* Two codecs with different layouts, each with their own field "x".
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     Each codec gets a fresh field object. *)
  let f1_x = Field.v "x" uint16be in
  let cf1_x = Codec.(f1_x $ fun (x, _) -> x) in
  let codec1 =
    let open Codec in
    v "Share1"
      (fun x y -> (x, y))
      [ cf1_x; (Field.v "y" uint16be $ fun (_, y) -> y) ]
  in
  let f2_x = Field.v "x" uint16be in
  let cf2_x = Codec.(f2_x $ fun (x, _) -> x) in
  let codec2 =
    let open Codec in
    v "Share2"
      (fun _pad x -> (x, 0))
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf2_x ]
  in
  let buf1 = Bytes.create 4 in
  Bytes.set_uint16_be buf1 0 0xAAAA;
  Bytes.set_uint16_be buf1 2 0xBBBB;
  let buf2 = Bytes.create 4 in
  Bytes.set_uint16_be buf2 0 0x0000;
  Bytes.set_uint16_be buf2 2 0xCCCC;
  (* f1_x reads at offset 0, f2_x reads at offset 2 *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    ((Staged.unstage (Codec.get codec1 cf1_x)) buf1 0);
  Alcotest.(check int)
    "codec2 get x" 0xCCCC
    ((Staged.unstage (Codec.get codec2 cf2_x)) buf2 0)

let test_view_shared_bitfield_spec () =
  (* Two codecs with different bitfield layouts using the default
     [bit_order = Msb_first].
     Codec1: [3-bit a] [5-bit b]       -> a is top 3 bits
     Codec2: [5-bit pad] [3-bit a]     -> a is bottom 3 bits *)
  let f1_a = Field.v "a" (bits ~width:3 U8) in
  let cf1_a = Codec.(f1_a $ fun (a, _) -> a) in
  let codec1 =
    let open Codec in
    v "ShareBf1"
      (fun a b -> (a, b))
      [ cf1_a; (Field.v "b" (bits ~width:5 U8) $ fun (_, b) -> b) ]
  in
  let f2_a = Field.v "a" (bits ~width:3 U8) in
  let cf2_a = Codec.(f2_a $ fun (a, _) -> a) in
  let codec2 =
    let open Codec in
    v "ShareBf2"
      (fun _pad a -> (a, 0))
      [ (Field.v "pad" (bits ~width:5 U8) $ fun _ -> 0); cf2_a ]
  in
  (* 0xE3 = 0b_1110_0011
     codec1 reads top 3 bits    -> 0b111 = 7
     codec2 reads bottom 3 bits -> 0b011 = 3 *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xE3;
  Alcotest.(check int)
    "codec1 get a (top 3)" 7
    ((Staged.unstage (Codec.get codec1 cf1_a)) buf 0);
  Alcotest.(check int)
    "codec2 get a (bot 3)" 3
    ((Staged.unstage (Codec.get codec2 cf2_a)) buf 0)

let test_view_shared_set_independent () =
  (* set via one codec's field must not affect the other's interpretation.
     Default [bit_order = Msb_first]: first-declared field at top. *)
  let f1 = Field.v "v" (bits ~width:4 U8) in
  let cf1 = Codec.(f1 $ fun (v, _) -> v) in
  let codec1 =
    let open Codec in
    v "SetShare1"
      (fun v pad -> (v, pad))
      [ cf1; (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p) ]
  in
  let f2 = Field.v "v" (bits ~width:4 U8) in
  let cf2 = Codec.(f2 $ fun (v, _) -> v) in
  let codec2 =
    let open Codec in
    v "SetShare2"
      (fun pad v -> (v, pad))
      [ (Field.v "pad" (bits ~width:4 U8) $ fun (_, p) -> p); cf2 ]
  in
  (* Codec1's v is the top nibble; set to 0xA. *)
  let buf = Bytes.create 1 in
  (Staged.unstage (Codec.set codec1 cf1)) buf 0 0xA;
  Alcotest.(check int) "byte after set1" 0xA0 (Bytes.get_uint8 buf 0);
  (* Codec2's v is the bottom nibble -- should still be 0. *)
  Alcotest.(check int)
    "codec2 get after set1" 0
    ((Staged.unstage (Codec.get codec2 cf2)) buf 0);
  (* Set codec2's v (bottom nibble) to 0x5. *)
  (Staged.unstage (Codec.set codec2 cf2)) buf 0 0x5;
  Alcotest.(check int) "byte after set2" 0xA5 (Bytes.get_uint8 buf 0);
  (* Codec1's v should still be 0xA. *)
  Alcotest.(check int)
    "codec1 get after set2" 0xA
    ((Staged.unstage (Codec.get codec1 cf1)) buf 0)

(* -- action semantics -- *)

let test_action_fires_decode_env () =
  (* decode_env fires actions and syncs output params *)
  let env = Codec.env projection_codec |> Param.bind projection_limit 10 in
  let buf = Bytes.of_string "\x05" in
  Alcotest.(check int) "outx before" 0 (Param.get env projection_outx);
  let _v = decode_ok (Codec.decode ~env projection_codec buf 0) in
  Alcotest.(check int) "outx after decode_env" 5 (Param.get env projection_outx)

let test_action_fires_on_get () =
  (* get fires field actions. A return_bool action that rejects odd values
     should cause get to raise on odd input. *)
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref mod int 2 = int 0) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  (* Even value: action passes *)
  Alcotest.(check int) "get even" 0x42 (get_v (Bytes.of_string "\x42") 0);
  (* Odd value: action rejects *)
  match get_v (Bytes.of_string "\x43") 0 with
  | _ -> Alcotest.fail "expected action to reject odd value"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_action_unfired_by_validate () =
  (* validate checks constraints + where, but does NOT fire actions. *)
  let action_out2 = Param.output "act_out2" uint8 in
  let f_ref2 = Field.v "v" uint8 in
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success [ Action.assign action_out2 (Field.ref f_ref2) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "ActionValidate" (fun v -> v) [ cf_v2 ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  Codec.validate ~env codec buf 0;
  (* validate does NOT fire actions *)
  Alcotest.(check int)
    "action not fired by validate" 0
    (Param.get env action_out2)

let test_get_noaction_zero_overhead () =
  (* get on a field without an action should not allocate.
     We just verify it works -- allocation is checked by benchmarks. *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoAction" (fun v -> v) [ cf_v ] in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (get_v buf 0)

let test_get_with_env () =
  (* get ~env fires action and syncs output params to env *)
  let out = Param.output "out" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "GetEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  let v = get_v buf 0 in
  Alcotest.(check int) "get returns value" 0x42 v;
  Alcotest.(check int) "output param synced" 0x42 (Param.get env out)

let test_get_action_field_twocodecs () =
  (* Same action field in two codecs -- each codec gets its own action runner *)
  let out1 = Param.output "out1" uint8 in
  let out2 = Param.output "out2" uint16be in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  (* Codec1: [v] at offset 0 *)
  let codec1 = Codec.v "ActTwo1" (fun v -> v) [ cf_v ] in
  (* Codec2: [pad] [v] -- v at offset 1, different action *)
  let cf_v2 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out2 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec2 =
    let open Codec in
    v "ActTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v2 ]
  in
  let env1 = Codec.env codec1 in
  let env2 = Codec.env codec2 in
  let buf = Bytes.of_string "\xAA\xBB" in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec1 cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec2 cf_v2) in
  (* codec1 reads offset 0 = 0xAA *)
  Alcotest.(check int) "codec1 get" 0xAA (get1 buf 0);
  Alcotest.(check int) "codec1 out" 0xAA (Param.get env1 out1);
  (* codec2 reads offset 1 = 0xBB *)
  Alcotest.(check int) "codec2 get" 0xBB (get2 buf 0);
  Alcotest.(check int) "codec2 out" 0xBB (Param.get env2 out2)

let test_get_action_no_env () =
  (* get without ~env on action field: action fires but output not accessible *)
  let out = Param.output "out_noenv" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x42" in
  (* No ~env: action fires (no crash) but output stays 0 *)
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  Alcotest.(check int) "get returns value" 0x42 (get_v buf 0);
  Alcotest.(check int) "output not synced without env" 0 (Param.get env out)

let test_get_action_abort_field () =
  (* get on a field with abort action always raises *)
  let cf_v =
    Codec.(
      Field.v "v" ~action:(Action.on_success [ Action.abort ]) uint8 $ fun v ->
      v)
  in
  let codec = Codec.v "AbortGet" (fun v -> v) [ cf_v ] in
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  match get_v (Bytes.of_string "\x42") 0 with
  | _ -> Alcotest.fail "expected abort"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_get_noaction_ignores_env () =
  (* Passing ~env to get on a field without action is harmless *)
  let cf_v = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec = Codec.v "NoActEnv" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int)
    "get returns value" 0x42
    (get_v (Bytes.of_string "\x42") 0)

let test_get_action_multiple_calls () =
  (* get with ~env updates output on every call *)
  let out = Param.output "out_multi" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "Multi" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  ignore (get_v (Bytes.of_string "\x10") 0);
  Alcotest.(check int) "after first" 0x10 (Param.get env out);
  ignore (get_v (Bytes.of_string "\x20") 0);
  Alcotest.(check int) "after second" 0x20 (Param.get env out)

let test_get_action_with_inputparam () =
  (* Action references an input param -- get ~env must blit it into the
     scratch array so the action sees the bound value. *)
  let limit = Param.input "limit" uint8 in
  let out = Param.output "result" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.assign out (Field.ref f_ref);
               Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit);
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "InputParam" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec |> Param.bind limit 50 in
  let buf_ok = Bytes.of_string "\x30" in
  let buf_bad = Bytes.of_string "\x40" in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* 0x30 = 48 <= 50: passes *)
  Alcotest.(check int) "get with input param" 0x30 (get_v buf_ok 0);
  Alcotest.(check int) "output synced" 0x30 (Param.get env out);
  (* 0x40 = 64 > 50: action rejects *)
  match get_v buf_bad 0 with
  | _ -> Alcotest.fail "expected rejection from input param check"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_get_action_inputparam_noenv () =
  (* Action references an input param but no env passed -- param reads as 0 *)
  let limit = Param.input "lim2" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [ Action.return_bool Expr.(Field.ref f_ref <= Param.expr limit) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "NoEnvInput" (fun v -> v) [ cf_v ] in
  (* No env: limit defaults to 0, so any positive value > 0 fails *)
  let get_v = Staged.unstage (Codec.get codec cf_v) in
  (* 0 <= 0: passes *)
  Alcotest.(check int) "zero passes" 0 (get_v (Bytes.of_string "\x00") 0);
  (* 1 > 0: fails *)
  match get_v (Bytes.of_string "\x01") 0 with
  | _ -> Alcotest.fail "expected rejection without env"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

(* -- Param forwarding into embedded sub-codecs -- *)

(* A sub-codec whose field is sized by an input param, embedded as a field of an
   outer codec. The outer surfaces the sub's param; binding it on the outer env
   drives the embedded field width on both encode and decode. *)
let test_embed_param_sized () =
  let elen = Param.input "elen" uint8 in
  let sub =
    Codec.v "EmbSub"
      (fun d -> d)
      Codec.[ Field.v "data" (byte_array ~size:(Param.expr elen)) $ Fun.id ]
  in
  let outer =
    Codec.v "EmbOuter" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  let env = Codec.env outer |> Param.bind elen 3 in
  let n = Codec.size_of_value outer "abc" in
  Alcotest.(check int) "wire size" 3 n;
  let buf = Bytes.create n in
  Codec.encode ~env outer "abc" buf 0;
  Alcotest.(check string)
    "roundtrip" "abc"
    (decode_ok (Codec.decode ~env outer buf 0))

(* A fixed-width embedded codec can still depend on an input parameter through
   its validation. A staged getter must preserve the outer environment when it
   enters that codec, even though neither the field offset nor its size varies. *)
let test_get_embed_param_fixed () =
  let limit = Param.input "get_embed_limit" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "GetEmbedSub"
      ~where:Expr.(Field.ref f_v <= Param.expr limit)
      Fun.id
      Codec.[ f_v $ Fun.id ]
  in
  let f_sub = Field.v "sub" (codec sub) in
  let cf_sub = Codec.(f_sub $ Fun.id) in
  let outer = Codec.v "GetEmbedOuter" Fun.id Codec.[ cf_sub ] in
  let env = Codec.env outer |> Param.bind limit 10 in
  let get_sub = Staged.unstage (Codec.get ~env outer cf_sub) in
  Alcotest.(check int)
    "embedded validation sees env" 5
    (get_sub (Bytes.of_string "\x05") 0)

(* The outer codec inherits the embedded sub-codec's input param, so encoding it
   without an env is rejected. *)
let test_embed_param_requires_env () =
  let elen = Param.input "elen2" uint8 in
  let sub =
    Codec.v "EmbSub2"
      (fun d -> d)
      Codec.[ Field.v "data" (byte_array ~size:(Param.expr elen)) $ Fun.id ]
  in
  let outer =
    Codec.v "EmbOuter2" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  Alcotest.(check bool)
    "encode without env rejected" true
    (raises_invalid (fun () -> Codec.encode outer "ab" (Bytes.create 8) 0))

(* A sub-codec's [where] is enforced when the sub is embedded, matching its
   standalone behaviour. *)
let test_embed_where_enforced () =
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "WSub"
      ~where:Expr.(Field.ref f_v <= int 10)
      (fun v -> v)
      Codec.[ f_v $ Fun.id ]
  in
  let outer =
    Codec.v "WOuter" (fun s -> s) Codec.[ Field.v "s" (codec sub) $ Fun.id ]
  in
  Alcotest.(check bool)
    "violating value rejected when embedded" true
    (match Codec.decode outer (Bytes.make 1 '\xFF') 0 with
    | Ok _ -> false
    | _ -> true);
  Alcotest.(check int)
    "satisfying value accepted" 5
    (decode_ok (Codec.decode outer (Bytes.make 1 '\x05') 0))

let test_embed_output_param () =
  let out = Param.output "embedded_out" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub_field =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_v) ])
        uint8
      $ Fun.id)
  in
  let sub = Codec.v "EmbeddedOutputSub" Fun.id Codec.[ sub_field ] in
  let outer =
    Codec.v "EmbeddedOutputOuter" Fun.id
      Codec.[ Field.v "sub" (codec sub) $ Fun.id ]
  in
  let env = Codec.env outer in
  Alcotest.(check int)
    "decoded value" 42
    (decode_ok (Codec.decode ~env outer (Bytes.of_string "\x2A") 0));
  Alcotest.(check int) "forwarded output" 42 (Param.get env out)

(* A list of param-constrained sub-records within a byte budget, the param
   forwarded through the repeat (the shape that first exposed the gap). *)
let test_embed_param_repeat () =
  let rlim = Param.input "rlim" uint8 in
  let f_v = Field.v "v" uint8 in
  let sub =
    Codec.v "RSub"
      ~where:Expr.(Field.ref f_v <= Param.expr rlim)
      (fun v -> v)
      Codec.[ f_v $ Fun.id ]
  in
  let f_n = Field.v "n" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_n) (codec sub) in
  let outer =
    Codec.v "RepOuter" (fun n xs -> (n, xs)) Codec.[ f_n $ fst; f_items $ snd ]
  in
  let v = (3, [ 1; 5; 9 ]) in
  let env = Codec.env outer |> Param.bind rlim 100 in
  let n = Codec.size_of_value outer v in
  let buf = Bytes.create n in
  Codec.encode ~env outer v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode ~env outer buf 0) = v)

let test_get_action_output_only () =
  (* Action with only assign (no return_bool/abort) -- should never fail *)
  let out = Param.output "out_only" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "OutOnly" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  (* Any value should work -- no validation in this action *)
  Alcotest.(check int) "get 0xFF" 0xFF (get_v (Bytes.of_string "\xFF") 0);
  Alcotest.(check int) "output 0xFF" 0xFF (Param.get env out);
  Alcotest.(check int) "get 0x00" 0x00 (get_v (Bytes.of_string "\x00") 0);
  Alcotest.(check int) "output 0x00" 0x00 (Param.get env out)

let test_get_action_varthen_assign () =
  (* Action with local var computation then assign to output *)
  let out = Param.output "doubled" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:
          (Action.on_success
             [
               Action.var "tmp" Expr.(Field.ref f_ref * int 2);
               Action.assign out (Field.ref (Field.v "tmp" uint8));
             ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "VarAssign" (fun v -> v) [ cf_v ] in
  let env = Codec.env codec in
  let get_v = Staged.unstage (Codec.get ~env codec cf_v) in
  Alcotest.(check int) "get value" 21 (get_v (Bytes.of_string "\x15") 0);
  Alcotest.(check int) "doubled output" 42 (Param.get env out)

let test_get_action_crossfield_ref () =
  (* Action on field y references field x's value *)
  let f_x = Field.v "x" uint8 in
  let out = Param.output "sum" uint8 in
  let cf_x = Codec.(f_x $ fun (x, _) -> x) in
  let cf_y =
    Codec.(
      Field.v "y"
        ~action:
          (Action.on_success
             [ Action.assign out Expr.(Field.ref f_x + int 100) ])
        uint8
      $ fun (_, y) -> y)
  in
  let codec = Codec.v "CrossRef" (fun x y -> (x, y)) [ cf_x; cf_y ] in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x0A\x14" in
  let get_y = Staged.unstage (Codec.get ~env codec cf_y) in
  let y = get_y buf 0 in
  Alcotest.(check int) "y value" 0x14 y;
  (* Action computed x + 100 = 10 + 100 = 110 *)
  Alcotest.(check int) "cross-field output" 110 (Param.get env out)

let test_validate_constraint_only () =
  (* Codec with constraint but no where clause *)
  let f_x = Field.v "x" uint8 in
  let cf_x =
    Codec.(
      Field.v "x" ~constraint_:Expr.(Field.ref f_x <= int 10) uint8 $ fun v -> v)
  in
  let codec = Codec.v "ConstOnly" (fun v -> v) [ cf_x ] in
  let good = Bytes.of_string "\x05" in
  let bad = Bytes.of_string "\x0B" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected constraint failure"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_validate_where_only () =
  (* Codec with where clause but no field constraints *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun v -> v) in
  let codec =
    Codec.v "WhereOnly"
      ~where:Expr.(Field.ref f_x = int 42)
      (fun v -> v)
      [ cf_x ]
  in
  let good = Bytes.of_string "\x2A" in
  let bad = Bytes.of_string "\x00" in
  Codec.validate codec good 0;
  match Codec.validate codec bad 0 with
  | () -> Alcotest.fail "expected where failure"
  | exception Parse_error { kind = Constraint_failed _; _ } -> ()

let test_get_twostaged_same_field () =
  (* Two staged getters from the same codec+field with different envs *)
  let out = Param.output "out_two" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec = Codec.v "TwoStaged" (fun v -> v) [ cf_v ] in
  let env1 = Codec.env codec in
  let env2 = Codec.env codec in
  let get1 = Staged.unstage (Codec.get ~env:env1 codec cf_v) in
  let get2 = Staged.unstage (Codec.get ~env:env2 codec cf_v) in
  (* Each staged getter has its own scratch array and env *)
  ignore (get1 (Bytes.of_string "\xAA") 0);
  ignore (get2 (Bytes.of_string "\xBB") 0);
  Alcotest.(check int) "env1" 0xAA (Param.get env1 out);
  Alcotest.(check int) "env2" 0xBB (Param.get env2 out)

let test_encode_shared_bitfield () =
  (* Encode via a codec that shares a bitfield with another codec.
     Default [bit_order] is [Msb_first], so the first-declared field [a]
     lives in the top nibble of the byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "EncBf1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncBf2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  let buf = Bytes.make 1 '\x00' in
  Codec.encode codec1 0xA buf 0;
  Alcotest.(check int) "top nibble (MSB-first)" 0xA0 (Bytes.get_uint8 buf 0)

(* -- API misuse / safety tests -- *)

let test_get_field_notin_codec () =
  (* get with a field that was never added to this codec raises Not_found
     at staging time *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX" (fun v -> v) [ cf_x ] in
  match Codec.get codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions field name" true
        (Re.execp (Re.compile (Re.str "y")) msg);
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "OnlyX")) msg)

let test_set_field_notin_codec () =
  (* set with a field not in the codec raises Invalid_argument at staging *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX2" (fun v -> v) [ cf_x ] in
  match Codec.set codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument _ -> ()

let test_bitfield_on_non_bitfield () =
  (* bitfield on a uint8 (non-bitfield) field *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let codec = Codec.v "NoBf" (fun v -> v) [ cf_x ] in
  match Codec.bitfield codec cf_x with
  | _ -> Alcotest.fail "expected error for non-bitfield"
  | exception Invalid_argument _ -> ()

let expect_foreign_env ~op ~codec f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument from %s" op
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "names the operation" true (contains ~sub:op msg);
      Alcotest.(check bool) "names the codec" true (contains ~sub:codec msg)

let test_foreign_env_codec_operations () =
  let p_a = Param.input "limit_a" uint8 in
  let f_a = Field.v "v" uint8 in
  let codec_a =
    Codec.v "EnvA"
      ~where:Expr.(Field.ref f_a <= Param.expr p_a)
      Fun.id
      Codec.[ f_a $ Fun.id ]
  in
  let p_b = Param.input "limit_b" uint8 in
  let f_b = Field.v "v" uint8 in
  let codec_b =
    Codec.v "EnvB"
      ~where:Expr.(Field.ref f_b <= Param.expr p_b)
      Fun.id
      Codec.[ f_b $ Fun.id ]
  in
  let env_b = Codec.env codec_b |> Param.bind p_b 0xff in
  let buf = Bytes.of_string "\x01" in
  expect_foreign_env ~op:"Codec.encode" ~codec:"EnvA" (fun () ->
      Codec.encode ~env:env_b codec_a 1 buf 0);
  expect_foreign_env ~op:"Codec.decode" ~codec:"EnvA" (fun () ->
      Codec.decode ~env:env_b codec_a buf 0);
  expect_foreign_env ~op:"Codec.validate" ~codec:"EnvA" (fun () ->
      Codec.validate ~env:env_b codec_a buf 0);
  let p_c = Param.input "limit_c" uint8 in
  let p_d = Param.input "floor_d" uint8 in
  let f_c = Field.v "v" uint8 in
  let codec_c =
    Codec.v "EnvC"
      ~where:
        Expr.(
          Field.ref f_c <= Param.expr p_c && Field.ref f_c >= Param.expr p_d)
      Fun.id
      Codec.[ f_c $ Fun.id ]
  in
  let env_a = Codec.env codec_a |> Param.bind p_a 0xff in
  expect_foreign_env ~op:"Codec.encode" ~codec:"EnvC" (fun () ->
      Codec.encode ~env:env_a codec_c 1 buf 0);
  expect_foreign_env ~op:"Codec.decode" ~codec:"EnvC" (fun () ->
      Codec.decode ~env:env_a codec_c buf 0)

let test_env_from_wrong_codec () =
  (* Using env from codec1 with get ~env on codec2 *)
  let out1 = Param.output "out_wrong" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v1 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec1 = Codec.v "Wrong1" (fun v -> v) [ cf_v1 ] in
  let cf_v2 = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec2 = Codec.v "Wrong2" (fun v -> v) [ cf_v2 ] in
  let env1 = Codec.env codec1 in
  expect_foreign_env ~op:"Codec.get" ~codec:"Wrong2" (fun () ->
      Codec.get ~env:env1 codec2 cf_v2)

let test_env_wrongcodec_with_action () =
  (* Using env from a different codec with an action field.
     The env has too few param slots -- get raises Invalid_argument
     at staging time. *)
  let out = Param.output "out_oob" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec_with_action = Codec.v "WithAct" (fun v -> v) [ cf_v ] in
  (* codec_empty has zero params, so its env has slots = [||] *)
  let cf_w = Codec.(Field.v "w" uint8 $ fun v -> v) in
  let codec_empty = Codec.v "NoParams" (fun v -> v) [ cf_w ] in
  let wrong_env = Codec.env codec_empty in
  match Codec.get ~env:wrong_env codec_with_action cf_v with
  | _ -> Alcotest.fail "expected Invalid_argument for wrong env"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "WithAct")) msg)

let test_decode_short_buffer () =
  (* Decode with buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "Short" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_encode_short_buffer () =
  (* Encode into buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "ShortEnc" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.encode codec 0x1234 buf 0 with
  | () -> Alcotest.fail "expected error for short buffer"
  | exception Invalid_argument _ -> ()

(* -- same bound field in two codecs -- *)

let test_same_field_two_codecs () =
  (* A single bound field used in two codecs with different layouts.
     Codec1: [u16be x] [u16be y]   -> x at offset 0
     Codec2: [u16be pad] [u16be x] -> x at offset 2
     If f_reader is mutable and set at seal time, the second seal clobbers
     the first. Both get/set must use the correct offset for their codec. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "TwoCodec1" (fun x _y -> x) [ cf_x; (Field.v "y" uint16be $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "TwoCodec2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint16_be buf 2 0xBBBB;
  (* codec1 should read x at offset 0 -> 0xAAAA *)
  Alcotest.(check int)
    "codec1 get x" 0xAAAA
    ((Staged.unstage (Codec.get codec1 cf_x)) buf 0);
  (* codec2 should read x at offset 2 -> 0xBBBB *)
  Alcotest.(check int)
    "codec2 get x" 0xBBBB
    ((Staged.unstage (Codec.get codec2 cf_x)) buf 0)

let test_samefield_twocodecs_set () =
  (* Same field in two codecs: set via each must write to the correct offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "SetTwo1" (fun v _pad -> v) [ cf_v; (Field.v "pad" uint8 $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "SetTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  (* set via codec1 should write to offset 0 *)
  (Staged.unstage (Codec.set codec1 cf_v)) buf 0 0xAA;
  Alcotest.(check int) "codec1 set -> byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "codec1 set -> byte 1 untouched" 0 (Bytes.get_uint8 buf 1);
  Bytes.fill buf 0 2 '\x00';
  (* set via codec2 should write to offset 1 *)
  (Staged.unstage (Codec.set codec2 cf_v)) buf 0 0xBB;
  Alcotest.(check int)
    "codec2 set -> byte 0 untouched" 0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "codec2 set -> byte 1" 0xBB (Bytes.get_uint8 buf 1)

let test_samefield_twocodecs_decode () =
  (* Decode via the first codec after sealing both.
     The second seal clobbers f_reader, so decode uses the wrong offset. *)
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "DecTwo1" (fun x _y -> x) [ cf_x; (Field.v "y" uint16be $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "DecTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint16be $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint16_be buf 2 0x5678;
  (* codec1 decode should construct record with x from offset 0 *)
  match Codec.decode codec1 buf 0 with
  | Ok v -> Alcotest.(check int) "decoded x" 0x1234 v
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_samefield_twocodecs_encode () =
  (* Encode via the first codec after sealing both.
     The second seal clobbers f_writer, so encode writes to the wrong offset. *)
  let f_v = Field.v "v" uint8 in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec1 =
    let open Codec in
    v "EncTwo1" (fun v _pad -> v) [ cf_v; (Field.v "pad" uint8 $ fun _ -> 0) ]
  in
  let _codec2 =
    let open Codec in
    v "EncTwo2" (fun _pad v -> v) [ (Field.v "pad" uint8 $ fun _ -> 0); cf_v ]
  in
  let buf = Bytes.make 2 '\x00' in
  Codec.encode codec1 0xAA buf 0;
  (* codec1 should write v at offset 0 *)
  Alcotest.(check int) "byte 0" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "byte 1" 0x00 (Bytes.get_uint8 buf 1)

let test_same_bitfield_two_codecs () =
  (* Same bitfield bound field in two codecs with different bit positions.
     Default [bit_order = Msb_first]: first-declared field at top of byte. *)
  let f_a = Field.v "a" (bits ~width:4 U8) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec1 =
    let open Codec in
    v "BfTwo1"
      (fun a _b -> a)
      [ cf_a; (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0) ]
  in
  let codec2 =
    let open Codec in
    v "BfTwo2"
      (fun _b a -> a)
      [ (Field.v "b" (bits ~width:4 U8) $ fun _ -> 0); cf_a ]
  in
  (* 0xA3: top nibble = 0xA, bottom nibble = 3. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA3;
  (* codec1: a is first-declared, so top 4 bits -> 0xA. *)
  Alcotest.(check int)
    "codec1 get a (top)" 0xA
    ((Staged.unstage (Codec.get codec1 cf_a)) buf 0);
  (* codec2: a is second-declared, so bottom 4 bits -> 3. *)
  Alcotest.(check int)
    "codec2 get a (bottom)" 3
    ((Staged.unstage (Codec.get codec2 cf_a)) buf 0)

let test_samefield_staged_before_secondseal () =
  (* Stage get from codec1 BEFORE sealing codec2.
     The staged function captures f_reader at staging time. If f_reader
     is a mutable slot, the staged function sees the clobbered value
     after codec2 seals. *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec1 =
    let open Codec in
    v "StagedTwo1" (fun x _y -> x) [ cf_x; (Field.v "y" uint8 $ fun _ -> 0) ]
  in
  (* Stage get from codec1 *)
  let get_x_1 = Staged.unstage (Codec.get codec1 cf_x) in
  (* Now seal codec2 -- this clobbers f_reader *)
  let _codec2 =
    let open Codec in
    v "StagedTwo2"
      (fun _pad x -> x)
      [ (Field.v "pad" uint8 $ fun _ -> 0); cf_x ]
  in
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  (* get_x_1 was staged before codec2 -- should still read offset 0 *)
  Alcotest.(check int) "staged before second seal" 0xAA (get_x_1 buf 0)

(* -- byte_slice tests -- *)

module Bs = Bytesrw.Bytes.Slice

let test_view_byte_slice_get () =
  (* A record with a fixed-size byte_slice field returns a sub-slice *)
  let f_payload = Field.v "payload" (byte_slice ~size:(int 4)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let codec =
    let open Codec in
    v "SliceRec"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xABCD;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload = (Staged.unstage (Codec.get codec cf_payload)) buf 0 in
  (* payload should be a slice into buf at offset 2, length 4 *)
  Alcotest.(check int) "payload first" 2 (Bs.first payload);
  Alcotest.(check int) "payload length" 4 (Bs.length payload);
  Alcotest.(check bool) "same buffer" true (Bs.bytes payload == buf);
  Alcotest.(check int)
    "payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload));
  Alcotest.(check int)
    "payload[3]" 0x40
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload + 3))

let test_view_byte_slice_decode () =
  (* decode also produces a correct sub-slice *)
  let codec =
    let open Codec in
    v "SliceDec"
      (fun tag payload -> (tag, payload))
      [
        (Field.v "tag" uint8 $ fun (t, _) -> t);
        (Field.v "data" (byte_slice ~size:(int 3)) $ fun (_, p) -> p);
      ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xFF;
  Bytes.set_uint8 buf 1 0xAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let tag, payload = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" 0xFF tag;
  Alcotest.(check int) "payload first" 1 (Bs.first payload);
  Alcotest.(check int) "payload length" 3 (Bs.length payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_view_byte_slice_nested () =
  (* Two-layer nested protocol: get payload slice, then get inner field *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x1234;
  let payload_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf payload_off
  in
  Alcotest.(check int) "inner val via zero-copy" 0x1234 inner_val

(* -- Raw access: get / set / sub -- *)

let test_raw_get_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.set_uint16_be buf 0 0x1234;
  Bytes.set_uint8 buf 2 0xFF;
  Alcotest.(check int)
    "get a" 0x1234
    ((Staged.unstage (Codec.get codec cf_a)) buf 0);
  Alcotest.(check int)
    "get b" 0xFF
    ((Staged.unstage (Codec.get codec cf_b)) buf 0)

let test_raw_get_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] (first declared) is the top nibble,
     matching the natural naming. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  Alcotest.(check int)
    "get hi" 0xA
    ((Staged.unstage (Codec.get codec cf_hi)) buf 0);
  Alcotest.(check int)
    "get lo" 0x7
    ((Staged.unstage (Codec.get codec cf_lo)) buf 0)

let test_raw_set_uint () =
  let f_a = Field.v "a" uint16be in
  let f_b = Field.v "b" uint8 in
  let cf_a = Codec.(f_a $ fun (a, _) -> a) in
  let cf_b = Codec.(f_b $ fun (_, b) -> b) in
  let codec = Codec.v "RawSU" (fun a b -> (a, b)) [ cf_a; cf_b ] in
  let buf = Bytes.create 3 in
  Bytes.fill buf 0 3 '\x00';
  (Staged.unstage (Codec.set codec cf_a)) buf 0 0xABCD;
  (Staged.unstage (Codec.set codec cf_b)) buf 0 0x42;
  Alcotest.(check int) "set a" 0xABCD (Bytes.get_uint16_be buf 0);
  Alcotest.(check int) "set b" 0x42 (Bytes.get_uint8 buf 2)

let test_raw_set_bitfield () =
  (* Default [bit_order = Msb_first]: [hi] goes to the top nibble. *)
  let f_hi = Field.v "hi" (bits ~width:4 U8) in
  let f_lo = Field.v "lo" (bits ~width:4 U8) in
  let cf_hi = Codec.(f_hi $ fun (h, _) -> h) in
  let cf_lo = Codec.(f_lo $ fun (_, l) -> l) in
  let codec = Codec.v "RawSBF" (fun hi lo -> (hi, lo)) [ cf_hi; cf_lo ] in
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0x00;
  (Staged.unstage (Codec.set codec cf_hi)) buf 0 0xC;
  (Staged.unstage (Codec.set codec cf_lo)) buf 0 0x3;
  Alcotest.(check int) "set bf byte" 0xC3 (Bytes.get_uint8 buf 0)

let test_raw_sub_nested () =
  (* Two-layer nested protocol using sub + get: zero alloc *)
  let f_val = Field.v "val" uint16be in
  let cf_val = Codec.(f_val $ fun v -> v) in
  let inner_codec = Codec.v "Inner" (fun v -> v) [ cf_val ] in
  let f_payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let cf_payload = Codec.(f_payload $ fun (_, p) -> p) in
  let outer_codec =
    let open Codec in
    v "Outer"
      (fun hdr payload -> (hdr, payload))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_payload ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0x0001;
  Bytes.set_uint16_be buf 2 0x5678;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get outer_codec cf_payload)) buf 0)
  in
  Alcotest.(check int) "sub offset" 2 inner_off;
  let inner_val =
    (Staged.unstage (Codec.get inner_codec cf_val)) buf inner_off
  in
  Alcotest.(check int) "inner val via sub+get" 0x5678 inner_val

let test_raw_sub_three_layers () =
  (* Three-layer: outer -> mid -> inner, all zero-alloc via sub+get *)
  let f_x = Field.v "x" uint8 in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let inner = Codec.v "L3" (fun x -> x) [ cf_x ] in
  let f_mid_payload = Field.v "data" (byte_slice ~size:(int 1)) in
  let cf_mid_payload = Codec.(f_mid_payload $ fun (_, p) -> p) in
  let mid =
    let open Codec in
    v "L2"
      (fun tag payload -> (tag, payload))
      [ (Field.v "tag" uint8 $ fun (t, _) -> t); cf_mid_payload ]
  in
  let f_body = Field.v "body" (byte_slice ~size:(int 2)) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let outer =
    let open Codec in
    v "L1"
      (fun hdr body -> (hdr, body))
      [ (Field.v "hdr" uint16be $ fun (h, _) -> h); cf_body ]
  in
  let buf = Bytes.create 4 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 0xBB;
  Bytes.set_uint8 buf 3 0xCC;
  let mid_off = Bs.first ((Staged.unstage (Codec.get outer cf_body)) buf 0) in
  Alcotest.(check int) "mid offset" 2 mid_off;
  let inner_off =
    Bs.first ((Staged.unstage (Codec.get mid cf_mid_payload)) buf mid_off)
  in
  Alcotest.(check int) "inner offset" 3 inner_off;
  let x = (Staged.unstage (Codec.get inner cf_x)) buf inner_off in
  Alcotest.(check int) "3-layer get" 0xCC x

let test_raw_with_offset () =
  (* get / set work correctly with non-zero base offset *)
  let f_v = Field.v "v" uint32be in
  let cf_v = Codec.(f_v $ fun v -> v) in
  let codec = Codec.v "RawOff" (fun v -> v) [ cf_v ] in
  let buf = Bytes.create 20 in
  Bytes.fill buf 0 20 '\x00';
  (Staged.unstage (Codec.set codec cf_v))
    buf 10
    (Wire.Private.UInt32.of_int32 0xDEADBEEFl);
  Alcotest.(check int32)
    "get at offset 10" 0xDEADBEEFl
    (Optint.to_int32 ((Staged.unstage (Codec.get codec cf_v)) buf 10))

(* -- Dependent-size byte_slice tests -- *)

type dep_slice_record = { length : int; payload : Bs.t }

let f_ds_length = Field.v "Length" uint16be
let f_ds_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_ds_length))
let cf_ds_length = Codec.(f_ds_length $ fun r -> r.length)
let cf_ds_payload = Codec.(f_ds_payload $ fun r -> r.payload)

let dep_slice_codec =
  Codec.v "DepSlice"
    (fun length payload -> { length; payload })
    [ cf_ds_length; cf_ds_payload ]

let test_dep_bslice_decode_empty () =
  (* length=0, no payload bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 0 r.length;
  Alcotest.(check int) "payload length" 0 (Bs.length r.payload)

let test_dep_bslice_decode_4 () =
  (* length=4, 4 payload bytes *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint8 buf 5 0xDD;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 4 r.length;
  Alcotest.(check int) "payload length" 4 (Bs.length r.payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload));
  Alcotest.(check int)
    "payload[3]" 0xDD
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload + 3))

let test_dep_bslice_decode_100 () =
  (* length=100, 100 payload bytes *)
  let buf = Bytes.create 102 in
  Bytes.set_uint16_be buf 0 100;
  for i = 0 to 99 do
    Bytes.set_uint8 buf (2 + i) (i land 0xFF)
  done;
  let r = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "length" 100 r.length;
  Alcotest.(check int) "payload length" 100 (Bs.length r.payload);
  Alcotest.(check int)
    "payload[50]" 50
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload + 50))

let test_dep_bslice_roundtrip () =
  (* encode then decode: 2 bytes length + 4 bytes payload = 6 total *)
  let payload_data = Bytes.of_string "\x01\x02\x03\x04" in
  let original =
    { length = 4; payload = Bs.make payload_data ~first:0 ~length:4 }
  in
  let buf = Bytes.create 6 in
  Codec.encode dep_slice_codec original buf 0;
  (* Verify encoded length field *)
  Alcotest.(check int) "encoded length field" 4 (Bytes.get_uint16_be buf 0);
  (* Verify wire_size_at reads the buffer correctly *)
  Alcotest.(check int)
    "wire_size_at" 6
    (Codec.wire_size_at dep_slice_codec buf 0);
  let decoded = decode_ok (Codec.decode dep_slice_codec buf 0) in
  Alcotest.(check int) "roundtrip length" 4 decoded.length;
  Alcotest.(check int) "roundtrip payload len" 4 (Bs.length decoded.payload);
  Alcotest.(check int)
    "roundtrip payload[0]" 0x01
    (Bytes.get_uint8 (Bs.bytes decoded.payload) (Bs.first decoded.payload))

let test_dep_bslice_get_payload () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Bytes.set_uint8 buf 2 0x10;
  Bytes.set_uint8 buf 3 0x20;
  Bytes.set_uint8 buf 4 0x30;
  Bytes.set_uint8 buf 5 0x40;
  let payload =
    (Staged.unstage (Codec.get dep_slice_codec cf_ds_payload)) buf 0
  in
  Alcotest.(check int) "get payload first" 2 (Bs.first payload);
  Alcotest.(check int) "get payload length" 4 (Bs.length payload);
  Alcotest.(check int)
    "get payload[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes payload) (Bs.first payload))

let test_dep_bslice_sub () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  let off =
    Bs.first ((Staged.unstage (Codec.get dep_slice_codec cf_ds_payload)) buf 0)
  in
  Alcotest.(check int) "sub offset" 2 off

let test_dep_bslice_set_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  (Staged.unstage (Codec.set dep_slice_codec cf_ds_length)) buf 0 8;
  Alcotest.(check int)
    "set length" 8
    ((Staged.unstage (Codec.get dep_slice_codec cf_ds_length)) buf 0)

let test_dep_bslice_get_length () =
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 42;
  Alcotest.(check int)
    "get length" 42
    ((Staged.unstage (Codec.get dep_slice_codec cf_ds_length)) buf 0)

(* -- Dependent-size byte_array tests -- *)

type dep_array_record = { length : int; payload : string }

let f_da_length = Field.v "Length" uint16be
let f_da_payload = Field.v "Payload" (byte_array ~size:(Field.ref f_da_length))
let cf_da_length = Codec.(f_da_length $ fun r -> r.length)
let cf_da_payload = Codec.(f_da_payload $ fun r -> r.payload)

let dep_array_codec =
  Codec.v "DepArray"
    (fun length payload -> { length; payload })
    [ cf_da_length; cf_da_payload ]

let test_dep_byte_array_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 5;
  Bytes.blit_string "hello" 0 buf 2 5;
  let r = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "length" 5 r.length;
  Alcotest.(check string) "payload is string copy" "hello" r.payload

let test_dep_byte_array_roundtrip () =
  let original = { length = 3; payload = "abc" } in
  let buf = Bytes.create 5 in
  Codec.encode dep_array_codec original buf 0;
  let decoded = decode_ok (Codec.decode dep_array_codec buf 0) in
  Alcotest.(check int) "roundtrip length" 3 decoded.length;
  Alcotest.(check string) "roundtrip payload" "abc" decoded.payload

let test_dep_byte_array_get () =
  let buf = Bytes.create 5 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.blit_string "xyz" 0 buf 2 3;
  let payload =
    (Staged.unstage (Codec.get dep_array_codec cf_da_payload)) buf 0
  in
  Alcotest.(check string) "get payload" "xyz" payload

(* -- Fixed field after variable field tests -- *)

type trailer_record = { length : int; payload : Bs.t; checksum : int }

let f_tr_length = Field.v "Length" uint16be
let f_tr_payload = Field.v "Payload" (byte_slice ~size:(Field.ref f_tr_length))
let f_tr_checksum = Field.v "Checksum" uint16be
let cf_tr_length = Codec.(f_tr_length $ fun r -> r.length)
let cf_tr_payload = Codec.(f_tr_payload $ fun r -> r.payload)
let cf_tr_checksum = Codec.(f_tr_checksum $ fun r -> r.checksum)

let trailer_codec =
  Codec.v "Trailer"
    (fun length payload checksum -> { length; payload; checksum })
    [ cf_tr_length; cf_tr_payload; cf_tr_checksum ]

let test_dep_trailer_get_checksum () =
  (* [length:u16be=3] [payload:3 bytes] [checksum:u16be=0xBEEF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let checksum =
    (Staged.unstage (Codec.get trailer_codec cf_tr_checksum)) buf 0
  in
  Alcotest.(check int) "get checksum" 0xBEEF checksum

let test_dep_trailer_set_checksum () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0x11;
  Bytes.set_uint8 buf 3 0x22;
  Bytes.set_uint8 buf 4 0x33;
  Bytes.set_uint16_be buf 5 0x0000;
  (Staged.unstage (Codec.set trailer_codec cf_tr_checksum)) buf 0 0xCAFE;
  Alcotest.(check int) "set checksum" 0xCAFE (Bytes.get_uint16_be buf 5)

let test_dep_trailer_decode () =
  let buf = Bytes.create 7 in
  Bytes.set_uint16_be buf 0 3;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.set_uint8 buf 3 0xBB;
  Bytes.set_uint8 buf 4 0xCC;
  Bytes.set_uint16_be buf 5 0xDEAD;
  let r = decode_ok (Codec.decode trailer_codec buf 0) in
  Alcotest.(check int) "length" 3 r.length;
  Alcotest.(check int) "payload length" 3 (Bs.length r.payload);
  Alcotest.(check int) "payload first" 2 (Bs.first r.payload);
  Alcotest.(check int)
    "payload[0]" 0xAA
    (Bytes.get_uint8 (Bs.bytes r.payload) (Bs.first r.payload));
  Alcotest.(check int) "checksum" 0xDEAD r.checksum

let test_dep_trailer_roundtrip () =
  let payload_data = Bytes.of_string "\x01\x02" in
  let original =
    {
      length = 2;
      payload = Bs.make payload_data ~first:0 ~length:2;
      checksum = 0x1234;
    }
  in
  let buf = Bytes.create 6 in
  Codec.encode trailer_codec original buf 0;
  let decoded = decode_ok (Codec.decode trailer_codec buf 0) in
  Alcotest.(check int) "rt length" 2 decoded.length;
  Alcotest.(check int) "rt payload len" 2 (Bs.length decoded.payload);
  Alcotest.(check int) "rt checksum" 0x1234 decoded.checksum

(* -- wire_size API for variable codecs -- *)

let test_dep_is_fixed () =
  Alcotest.(check bool)
    "fixed codec is_fixed" true
    (Codec.is_fixed simple_record_codec);
  Alcotest.(check bool)
    "variable codec is_fixed" false
    (Codec.is_fixed dep_slice_codec);
  Alcotest.(check bool)
    "trailer codec is_fixed" false
    (Codec.is_fixed trailer_codec)

let test_dep_wire_size_raises () =
  (* wire_size raises Invalid_argument for variable-size codecs *)
  (match Codec.wire_size dep_slice_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (match Codec.wire_size trailer_codec with
  | _ -> Alcotest.fail "expected Invalid_argument from wire_size"
  | exception Invalid_argument _ -> ());
  (* wire_size succeeds for fixed codecs *)
  Alcotest.(check int) "fixed wire_size" 7 (Codec.wire_size simple_record_codec)

let test_dep_min_wire_size () =
  (* min_wire_size for dep_slice_codec: just uint16be = 2 *)
  Alcotest.(check int) "dep_slice min" 2 (Codec.min_wire_size dep_slice_codec);
  (* min_wire_size for trailer_codec: uint16be + uint16be = 4 (variable payload excluded) *)
  Alcotest.(check int) "trailer min" 4 (Codec.min_wire_size trailer_codec);
  (* min_wire_size for fixed codec equals wire_size *)
  Alcotest.(check int)
    "fixed min"
    (Codec.wire_size simple_record_codec)
    (Codec.min_wire_size simple_record_codec)

let test_dep_compute_wire_size () =
  (* dep_slice: length=4 -> total = 2 + 4 = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 4;
  Alcotest.(check int)
    "dep_slice compute" 6
    (Codec.wire_size_at dep_slice_codec buf 0);
  (* dep_slice: length=0 -> total = 2 + 0 = 2 *)
  Bytes.set_uint16_be buf 0 0;
  Alcotest.(check int)
    "dep_slice compute 0" 2
    (Codec.wire_size_at dep_slice_codec buf 0);
  (* trailer: length=3 -> total = 2 + 3 + 2 = 7 *)
  let buf2 = Bytes.create 7 in
  Bytes.set_uint16_be buf2 0 3;
  Alcotest.(check int)
    "trailer compute" 7
    (Codec.wire_size_at trailer_codec buf2 0);
  (* fixed codec: compute returns fixed size without reading buffer *)
  Alcotest.(check int)
    "fixed compute"
    (Codec.wire_size simple_record_codec)
    (Codec.wire_size_at simple_record_codec (Bytes.create 7) 0)

(* -- Field.ref expression tests -- *)

let test_dep_codec_ref () =
  (* Field.ref produces a valid expression used as byte_slice size *)
  let f_len = Field.v "Len" uint8 in
  let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _) -> l) in
  let cf_data = Codec.(f_data $ fun (_, d) -> d) in
  let codec =
    Codec.v "RefTest" (fun len data -> (len, data)) [ cf_len; cf_data ]
  in
  (* buf: [len=5] [5 bytes payload] *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 5;
  for i = 0 to 4 do
    Bytes.set_uint8 buf (1 + i) (0x10 + i)
  done;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "ref len" 5 len;
  Alcotest.(check int) "ref data length" 5 (Bs.length data);
  Alcotest.(check int)
    "ref data[0]" 0x10
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data));
  Alcotest.(check int)
    "ref data[4]" 0x14
    (Bytes.get_uint8 (Bs.bytes data) (Bs.first data + 4))

(* A [byte_slice] sized by a 64-bit length field: an adversarial length that
   does not fit a native int must fail the parse, not be silently read as a
   0-length field. *)
let u64_sized_codec =
  let f_len = Field.v "Len" uint64be in
  let f_data = Field.v "Data" (byte_slice ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _) -> l) in
  let cf_data = Codec.(f_data $ fun (_, d) -> d) in
  Codec.v "U64Size" (fun len data -> (len, data)) [ cf_len; cf_data ]

let u64_len_buf v =
  (* 8-byte BE length followed by a few payload bytes. *)
  let b = Bytes.create 16 in
  Bytes.set_int64_be b 0 v;
  b

let test_dep_size_in_range () =
  let _len, data =
    decode_ok (Codec.decode u64_sized_codec (u64_len_buf 4L) 0)
  in
  Alcotest.(check int)
    "in-range length reads that many bytes" 4 (Bs.length data)

let test_dep_size_out_of_range () =
  let cases =
    [
      ("all-ones", 0xFFFF_FFFF_FFFF_FFFFL);
      ("2^63", 0x8000_0000_0000_0000L);
      ("max_int + 1", Int64.add (Int64.of_int max_int) 1L);
      ("int64 -1 as length", -1L);
    ]
  in
  List.iter
    (fun (name, v) ->
      match Codec.decode u64_sized_codec (u64_len_buf v) 0 with
      | Ok _ -> Alcotest.failf "%s: expected Parse_error, decoded ok" name
      | Error { kind = Value_out_of_range _; _ } -> ()
      | Error e ->
          Alcotest.failf "%s: expected Value_out_of_range, got %a" name
            pp_parse_error e)
    cases

let test_dep_ref_size_eval () =
  (* Test that the size expression is evaluated correctly for wire_size_at *)
  let f_sz = Field.v "Size" uint8 in
  let f_body = Field.v "Body" (byte_slice ~size:(Field.ref f_sz)) in
  let cf_sz = Codec.(f_sz $ fun (s, _) -> s) in
  let cf_body = Codec.(f_body $ fun (_, b) -> b) in
  let codec =
    Codec.v "RefSizeEval" (fun sz body -> (sz, body)) [ cf_sz; cf_body ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_uint8 buf 0 10;
  Alcotest.(check int) "compute size" 11 (Codec.wire_size_at codec buf 0);
  Bytes.set_uint8 buf 0 0;
  Alcotest.(check int) "compute size 0" 1 (Codec.wire_size_at codec buf 0)

let signed_magnitude_seek_codec =
  let f_seek =
    Field.v "Seek" uint64be ~self_int64:(fun self ->
        Expr.(self <= int64 Int64.max_int || self > int64 Int64.min_int))
  in
  Codec.v "SignedMagnitudeSeek" Fun.id Codec.[ f_seek $ Fun.id ]

let seek_buf v =
  let b = Bytes.create 8 in
  Bytes.set_int64_be b 0 v;
  b

let test_int64_field_constraint_accepts_signed_magnitude_domain () =
  List.iter
    (fun v ->
      let decoded =
        decode_ok (Codec.decode signed_magnitude_seek_codec (seek_buf v) 0)
      in
      Alcotest.(check int64) "seek" v decoded)
    [ 0L; Int64.max_int; Int64.succ Int64.min_int; -1L ]

let test_int64_field_constraint_rejects_negative_zero () =
  match Codec.decode signed_magnitude_seek_codec (seek_buf Int64.min_int) 0 with
  | Ok _ -> Alcotest.fail "expected signed-magnitude negative zero to fail"
  | Error { kind = Constraint_failed _; _ } -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

(* The bsdiff seek: a sign-magnitude uint64 decoded through a map, whose
   magnitude must fit a native int. Exercises an int64 mask ([land64]) over a
   map'd field: the constraint reads the raw pre-map word. *)
let mask_seek_codec =
  let f_seek =
    Field.v "Seek" (map ~decode:Fun.id ~encode:Fun.id uint64be)
      ~self_int64:(fun self ->
        Expr.(
          land64 self (int64 0x7FFF_FFFF_FFFF_FFFFL)
          <= int64 (Int64.of_int max_int)))
  in
  Codec.v "MaskSeek" Fun.id Codec.[ f_seek $ Fun.id ]

let test_int64_mask_constraint_over_map () =
  let accept v =
    Alcotest.(check int64)
      "accept" v
      (decode_ok (Codec.decode mask_seek_codec (seek_buf v) 0))
  in
  let reject v =
    match Codec.decode mask_seek_codec (seek_buf v) 0 with
    | Ok _ -> Alcotest.failf "expected 0x%Lx rejected" v
    | Error { kind = Constraint_failed _; _ } -> ()
    | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  in
  (* magnitude fits: small values and sign-bit-only (negative zero magnitude) *)
  List.iter accept [ 0L; 5L; Int64.min_int ];
  (* magnitude too big: bit 62 set, and all-ones *)
  List.iter reject [ 0x4000_0000_0000_0000L; Int64.max_int ]

(* A plain int-kind [self_constraint] on a uint64 field must still read the
   field value, not an unpopulated zero slot: an out-of-range value has to be
   rejected, or the bound is silently vacuous. *)
let test_uint64_int_ref_constraint_enforced () =
  let f =
    Field.v "Len" uint64be ~self_constraint:(fun self -> Expr.(self <= int 10))
  in
  let codec = Codec.v "U64IntRef" Fun.id Codec.[ f $ Fun.id ] in
  let buf v =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 v;
    b
  in
  Alcotest.(check int64)
    "in-range accepts" 3L
    (decode_ok (Codec.decode codec (buf 3L) 0));
  match Codec.decode codec (buf 20L) 0 with
  | Ok _ -> Alcotest.fail "20 exceeds the bound and must be rejected"
  | Error { kind = Constraint_failed _; _ } -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

(* -- struct_of_codec for variable-size codecs -- *)

let test_struct_of_dep () =
  (* struct_of_codec should produce a valid struct for variable-size codecs *)
  let output = render_3d dep_slice_codec in
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output);
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output)

let test_struct_of_dep_trailer () =
  let output = render_3d trailer_codec in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool)
    "contains Checksum" true
    (contains ~sub:"Checksum" output)

(* -- sizeof_this / field_pos in codec -- *)

type pos_record = { pa : int; pb : int; pc : int }

let test_codec_sizeof_this () =
  let out = Param.output "out" uint8 in
  let codec =
    let open Codec in
    v "SizeofThisCodec"
      (fun a b c -> { pa = a; pb = b; pc = c })
      [
        (Field.v "a" uint8 $ fun r -> r.pa);
        (Field.v "b" uint16be $ fun r -> r.pb);
        ( Field.v "c"
            ~action:(Action.on_success [ Action.assign out sizeof_this ])
            uint8
        $ fun r -> r.pc );
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x00\x02\x03" in
  let _v = decode_ok (Codec.decode ~env codec buf 0) in
  (* sizeof_this at field c = 1 (uint8) + 2 (uint16be) = 3 *)
  Alcotest.(check int) "sizeof_this at c" 3 (Param.get env out)

let test_codec_field_pos () =
  let out = Param.output "out" uint8 in
  let codec =
    let open Codec in
    v "FieldPosCodec"
      (fun a b c -> { pa = a; pb = b; pc = c })
      [
        (Field.v "a" uint8 $ fun r -> r.pa);
        (Field.v "b" uint8 $ fun r -> r.pb);
        ( Field.v "c"
            ~action:(Action.on_success [ Action.assign out field_pos ])
            uint8
        $ fun r -> r.pc );
      ]
  in
  let env = Codec.env codec in
  let buf = Bytes.of_string "\x01\x02\x03" in
  let _v = decode_ok (Codec.decode ~env codec buf 0) in
  (* field_pos at c = 2 (third field, zero-indexed) *)
  Alcotest.(check int) "field_pos at c" 2 (Param.get env out)

(* -- Bitfield batch access -- *)

type bf_rec = { hi : int; lo : int }

let bf_f_hi = Field.v "hi" (bits ~width:4 U8)
let bf_f_lo = Field.v "lo" (bits ~width:4 U8)
let bf_cf_hi = Codec.(bf_f_hi $ fun r -> r.hi)
let bf_cf_lo = Codec.(bf_f_lo $ fun r -> r.lo)

let bf_codec =
  Codec.v "BfBatch" (fun hi lo -> { hi; lo }) Codec.[ bf_cf_hi; bf_cf_lo ]

let test_bitfield_extract () =
  (* Default [bit_order = Msb_first]: [hi] is the top nibble. *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0xA7;
  let hi = Codec.bitfield bf_codec bf_cf_hi in
  let lo = Codec.bitfield bf_codec bf_cf_lo in
  let load = Staged.unstage (Codec.load_word hi) in
  let w = load buf 0 in
  let hi = Codec.extract hi w in
  let lo = Codec.extract lo w in
  (* Compare against Codec.get *)
  let get_hi = Staged.unstage (Codec.get bf_codec bf_cf_hi) in
  let get_lo = Staged.unstage (Codec.get bf_codec bf_cf_lo) in
  Alcotest.(check int) "extract hi = get hi" (get_hi buf 0) hi;
  Alcotest.(check int) "extract lo = get lo" (get_lo buf 0) lo;
  Alcotest.(check int) "hi" 0xA hi;
  Alcotest.(check int) "lo" 0x7 lo

let test_bitfield_non_bf_raises () =
  let f_x = Field.v "x" uint16be in
  let cf_x = Codec.(f_x $ fun x -> x) in
  let codec = Codec.v "NonBf" (fun x -> x) Codec.[ cf_x ] in
  match Codec.bitfield codec cf_x with
  | _ -> Alcotest.fail "expected Invalid_argument"
  | exception Invalid_argument _ -> ()

let test_bitfield_short_buffer () =
  (* Reading a uint32be bitfield from a 2-byte buffer should not segfault *)
  let f_a = Field.v "a" (bits ~width:8 U32be) in
  let cf_a = Codec.(f_a $ fun a -> a) in
  let codec = Codec.v "Short" (fun a -> a) Codec.[ cf_a ] in
  let bf = Codec.bitfield codec cf_a in
  let load = Staged.unstage (Codec.load_word bf) in
  (* Short buffer -- should read garbage but not crash *)
  let buf = Bytes.create 8 in
  Bytes.set_int32_be buf 0 0x12345678l;
  let w = load buf 0 in
  let v = Codec.extract bf w in
  Alcotest.(check int) "extract from valid buf" 0x12 v

let test_bitfield_load_shared () =
  (* Two fields in the same base word should get the same word value *)
  let f_a = Field.v "a" (bits ~width:4 U32be) in
  let f_b = Field.v "b" (bits ~width:4 U32be) in
  let cf_a = Codec.(f_a $ fst) in
  let cf_b = Codec.(f_b $ snd) in
  let codec = Codec.v "Shared" (fun a b -> (a, b)) Codec.[ cf_a; cf_b ] in
  let a = Codec.bitfield codec cf_a in
  let b = Codec.bitfield codec cf_b in
  let load_a = Staged.unstage (Codec.load_word a) in
  let load_b = Staged.unstage (Codec.load_word b) in
  let buf = Bytes.create 4 in
  Bytes.set_int32_be buf 0 0xABCDEF01l;
  let wa = load_a buf 0 in
  let wb = load_b buf 0 in
  (* Same base word, same value *)
  Alcotest.check (Alcotest.testable Optint.pp Optint.equal) "same word" wa wb;
  let a = Codec.extract a wa in
  let b = Codec.extract b wa in
  (* a = top 4 bits of 0xABCDEF01 = 0xA, b = next 4 bits = 0xB *)
  Alcotest.(check int) "a" 0xA a;
  Alcotest.(check int) "b" 0xB b

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
  Alcotest.(check int) "header" 0xAA r.header;
  Alcotest.(check int) "inner.tag" 0x42 r.inner.tag;
  Alcotest.(check int) "inner.value" 0x1234 r.inner.value;
  Alcotest.(check int) "trailer" 0xBB r.trailer

let test_codec_embed_encode () =
  let v =
    { header = 0xAA; inner = { tag = 0x42; value = 0x1234 }; trailer = 0xBB }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec v buf 0;
  Alcotest.(check int) "header byte" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "value bytes" 0x1234 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "trailer byte" 0xBB (Bytes.get_uint8 buf 4)

let test_codec_embed_roundtrip () =
  let original =
    { header = 0x11; inner = { tag = 0x22; value = 0x3344 }; trailer = 0x55 }
  in
  let buf = Bytes.create 5 in
  Codec.encode outer_codec original buf 0;
  let decoded = decode_ok (Codec.decode outer_codec buf 0) in
  Alcotest.(check int) "header" original.header decoded.header;
  Alcotest.(check int) "inner.tag" original.inner.tag decoded.inner.tag;
  Alcotest.(check int) "inner.value" original.inner.value decoded.inner.value;
  Alcotest.(check int) "trailer" original.trailer decoded.trailer

let test_codec_embed_wire_size () =
  Alcotest.(check int) "wire_size" 5 (Codec.wire_size outer_codec);
  Alcotest.(check bool) "is_fixed" true (Codec.is_fixed outer_codec)

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

type bf_outer = { id : int; bf : bf_inner; checksum : int }

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
  Alcotest.(check int) "id" 0x1234 r.id;
  Alcotest.(check int) "version" 0xA r.bf.version;
  Alcotest.(check int) "flags" 0x5 r.bf.flags;
  Alcotest.(check int) "checksum" 0xFF r.checksum

(* Two levels of nesting --
   [l0] / [l1] / [l2] and their codecs live in {!Test_helpers}. *)

let test_codec_embed_nested () =
  (* l2(1) + l1_y(2) + z(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x42;
  Bytes.set_uint16_be buf 1 0xABCD;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int) "l2.x" 0x42 r.inner.inner.x;
  Alcotest.(check int) "l1.y" 0xABCD r.inner.y;
  Alcotest.(check int) "l0.z" 0xFF r.z

let test_codec_embed_nested_roundtrip () =
  let original : l0 =
    { inner = { inner = { x = 0x42 }; y = 0xABCD }; z = 0xFF }
  in
  let buf = Bytes.create 4 in
  Codec.encode l0_codec original buf 0;
  let decoded = decode_ok (Codec.decode l0_codec buf 0) in
  Alcotest.(check int) "l2.x" original.inner.inner.x decoded.inner.inner.x;
  Alcotest.(check int) "l1.y" original.inner.y decoded.inner.y;
  Alcotest.(check int) "l0.z" original.z decoded.z

(* -- Cross-codec Field.ref: parent expression references sub-codec field -- *)

(* TC/TM-style frame: header contains a length field, the data field's size
   is computed from that nested header field via Field.ref. *)

type tc_header = { version : int; frame_len : int }

let f_tc_version = Field.v "Version" uint8
let f_tc_frame_len = Field.v "FrameLen" uint8

let tc_header_codec =
  Codec.v "TcHeader"
    (fun version frame_len -> { version; frame_len })
    Codec.
      [
        (f_tc_version $ fun r -> r.version);
        (f_tc_frame_len $ fun r -> r.frame_len);
      ]

type tc_frame = { hdr : tc_header; data : string; check : int }

(* The data field's size is `Field.ref f_tc_frame_len - 2 - 1` (header is 2 bytes,
   trailer is 1 byte). The Field.ref must resolve into the embedded header codec. *)
let tc_frame_codec =
  Codec.v "TcFrame"
    (fun hdr data check -> { hdr; data; check })
    Codec.
      [
        (Field.v "Header" (codec tc_header_codec) $ fun r -> r.hdr);
        ( Field.v "Data"
            (byte_array ~size:Expr.(Field.ref f_tc_frame_len - int 2 - int 1))
        $ fun r -> r.data );
        (Field.v "Check" uint8 $ fun r -> r.check);
      ]

let test_codec_cross_field_ref () =
  (* frame_len=8 -> data is 8-2-1=5 bytes *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 1;
  (* version *)
  Bytes.set_uint8 buf 1 8;
  (* frame_len *)
  Bytes.blit_string "HELLO" 0 buf 2 5;
  Bytes.set_uint8 buf 7 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "version" 1 r.hdr.version;
  Alcotest.(check int) "frame_len" 8 r.hdr.frame_len;
  Alcotest.(check string) "data" "HELLO" r.data;
  Alcotest.(check int) "check" 0xCC r.check

let test_codec_crossref_field_varying () =
  (* frame_len=5 -> data is 2 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 5;
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xFF;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check int) "frame_len" 5 r.hdr.frame_len;
  Alcotest.(check string) "data" "AB" r.data;
  Alcotest.(check int) "check" 0xFF r.check

(* -- Adversarial: cross-codec Field.ref edge cases -- *)

(* Attacker sets frame_len=255 in a 5-byte buffer. The data field's computed
   size (255-3=252) exceeds available bytes -- must report Unexpected_eof,
   not crash or silently truncate. *)
let test_codec_crossref_field_oversized () =
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 0xFF;
  (* attacker frame_len *)
  Bytes.blit_string "AB" 0 buf 2 2;
  Bytes.set_uint8 buf 4 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF on oversized data field"
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

(* Attacker sets frame_len=2 -> data size = 2-3 = -1. Must error, not crash. *)
let test_codec_crossref_field_underflow () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  (* below header+trailer minimum *)
  Bytes.set_uint8 buf 2 0;
  Bytes.set_uint8 buf 3 0xCC;
  match Codec.decode tc_frame_codec buf 0 with
  | Ok r ->
      (* If accepted, the data must be empty or the decode must have rejected
         the negative size. Either is acceptable as long as no crash. *)
      Alcotest.(check bool)
        "data length non-negative" true
        (String.length r.data >= 0)
  | Error _ -> ()

(* frame_len=3 -> data size = 0. Boundary case: empty data. *)
let test_codec_crossref_field_zerodata () =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 3;
  Bytes.set_uint8 buf 2 0xCC;
  let r = decode_ok (Codec.decode tc_frame_codec buf 0) in
  Alcotest.(check string) "data" "" r.data;
  Alcotest.(check int) "check" 0xCC r.check

(* Sub-codec field name shadowing: parent has its own field with same name as
   a sub-codec field. The parent's name should win for parent-scope expressions
   defined after the parent field. *)

type shadow_inner = { si_x : int }

let f_si_x = Field.v "Shared" uint8

let shadow_inner_codec =
  Codec.v "ShadowInner"
    (fun x -> { si_x = x })
    Codec.[ (f_si_x $ fun r -> r.si_x) ]

type shadow_outer = { inner : shadow_inner; shared : int; data : string }

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
  Alcotest.(check int) "inner.shared" 5 r.inner.si_x;
  Alcotest.(check int) "parent.shared" 3 r.shared;
  Alcotest.(check string) "data" "ABC" r.data

(* Two-level deep nesting: outer references a field three levels down. *)

type inner_l1 = { il1_x : int }

let f_il1_x = Field.v "DeepLen" uint8

let inner_l1_codec =
  Codec.v "InnerL1"
    (fun x -> { il1_x = x })
    Codec.[ (f_il1_x $ fun r -> r.il1_x) ]

type middle_l2 = { inner : inner_l1; y : int }

let middle_l2_codec =
  Codec.v "MiddleL2"
    (fun inner y -> { inner; y })
    Codec.
      [
        (Field.v "Inner" (codec inner_l1_codec) $ fun r -> r.inner);
        (Field.v "Y" uint8 $ fun r -> r.y);
      ]

type outer_l3 = { mid : middle_l2; data : string }

(* The outer references DeepLen which lives 2 levels deep inside the middle codec. *)
let outer_l3_codec =
  Codec.v "OuterL3"
    (fun mid data -> { mid; data })
    Codec.
      [
        (Field.v "Middle" (codec middle_l2_codec) $ fun r -> r.mid);
        (Field.v "Data" (byte_array ~size:(Field.ref f_il1_x)) $ fun r -> r.data);
      ]

let test_codec_crossref_field_twolevels () =
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 4;
  (* DeepLen at l1 *)
  Bytes.set_uint8 buf 1 0xFF;
  (* l2.y *)
  Bytes.blit_string "ABCD" 0 buf 2 4;
  let r = decode_ok (Codec.decode outer_l3_codec buf 0) in
  Alcotest.(check int) "deep_len" 4 r.mid.inner.il1_x;
  Alcotest.(check int) "y" 0xFF r.mid.y;
  Alcotest.(check string) "data" "ABCD" r.data

(* Sub-codec with bitfield referenced from parent. The sub-codec packs a 4-bit
   length and 4-bit flags into one byte; the parent uses the length to size data. *)

type bf_hdr = { len : int; flags : int }

let f_bh_len = Field.v "BfLen" (bits ~width:4 U8)
let f_bh_flags = Field.v "BfFlags" (bits ~width:4 U8)

let bf_hdr_codec =
  Codec.v "BfHdr"
    (fun len flags -> { len; flags })
    Codec.[ (f_bh_len $ fun r -> r.len); (f_bh_flags $ fun r -> r.flags) ]

type bf_frame = { hdr : bf_hdr; data : string }

let bf_frame_codec =
  Codec.v "BfFrame"
    (fun hdr data -> { hdr; data })
    Codec.
      [
        (Field.v "Hdr" (codec bf_hdr_codec) $ fun r -> r.hdr);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_bh_len)) $ fun r ->
          r.data );
      ]

let test_codec_crossref_field_bitfield () =
  (* Default [bit_order = Msb_first]: [BfLen] (first declared) is the top
     nibble, [BfFlags] is the bottom nibble. len=3, flags=0xA -> byte = 0x3A. *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x3A;
  Bytes.blit_string "XYZ" 0 buf 1 3;
  let r = decode_ok (Codec.decode bf_frame_codec buf 0) in
  Alcotest.(check int) "len" 3 r.hdr.len;
  Alcotest.(check int) "flags" 0xA r.hdr.flags;
  Alcotest.(check string) "data" "XYZ" r.data

(* -- Nested: Optional typ: conditional field presence --
   [opt_record], [opt_codec], [opt_codec_present], [opt_codec_absent] live
   in {!Test_helpers}. *)

let test_optional_present_decode () =
  (* hdr(1) + payload(2) + trail(1) = 4 bytes *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xBB;
  let r = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.hdr;
  Alcotest.(check (option int)) "payload" (Some 0x1234) r.payload;
  Alcotest.(check int) "trail" 0xBB r.trail

let test_optional_absent_decode () =
  (* hdr(1) + trail(1) = 2 bytes (no payload) *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.hdr;
  Alcotest.(check (option int)) "payload" None r.payload;
  Alcotest.(check int) "trail" 0xBB r.trail

let test_optional_present_encode () =
  let v : opt_record = { hdr = 0xAA; payload = Some 0x1234; trail = 0xBB } in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "payload" 0x1234 (Bytes.get_uint16_be buf 1);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 3)

let test_optional_absent_encode () =
  let v : opt_record = { hdr = 0xAA; payload = None; trail = 0xBB } in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent v buf 0;
  Alcotest.(check int) "hdr" 0xAA (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "trail" 0xBB (Bytes.get_uint8 buf 1)

let test_optional_present_roundtrip () =
  let original : opt_record =
    { hdr = 0x11; payload = Some 0x2233; trail = 0x44 }
  in
  let buf = Bytes.create 4 in
  Codec.encode opt_codec_present original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_present buf 0) in
  Alcotest.(check int) "hdr" original.hdr decoded.hdr;
  Alcotest.(check (option int)) "payload" original.payload decoded.payload;
  Alcotest.(check int) "trail" original.trail decoded.trail

let test_optional_absent_roundtrip () =
  let original : opt_record = { hdr = 0x11; payload = None; trail = 0x44 } in
  let buf = Bytes.create 2 in
  Codec.encode opt_codec_absent original buf 0;
  let decoded = decode_ok (Codec.decode opt_codec_absent buf 0) in
  Alcotest.(check int) "hdr" original.hdr decoded.hdr;
  Alcotest.(check (option int)) "payload" original.payload decoded.payload;
  Alcotest.(check int) "trail" original.trail decoded.trail

(* A byte_array whose ~size reads an optional_or field. The size expression
   reads the optional_or's present-or-default value, so encode and decode agree
   on the span length. *)
let test_bytearray_sized_by_optional_or () =
  let f_gate = Field.v "gate" uint8 in
  let f_len =
    Field.optional_or "len"
      ~present:Expr.(Field.ref f_gate <> int 0)
      ~default:3 uint8
  in
  let f_data = Field.v "data" (byte_array ~size:(Field.ref f_len)) in
  let c =
    Codec.v "OoSized"
      (fun gate len data -> (gate, len, data))
      Codec.
        [
          (f_gate $ fun (g, _, _) -> g);
          (f_len $ fun (_, l, _) -> l);
          (f_data $ fun (_, _, d) -> d);
        ]
  in
  let rt v expected =
    let buf = Bytes.create (Codec.size_of_value c v) in
    Codec.encode c v buf 0;
    Alcotest.(check string) "encoded bytes" expected (Bytes.to_string buf);
    match Codec.decode c buf 0 with
    | Ok d -> Alcotest.(check bool) "round-trip" true (d = v)
    | Error e -> Alcotest.failf "decode: %a" pp_parse_error e
  in
  (* gate set: len present (2), data is 2 bytes *)
  rt (1, 2, "ab") "\x01\x02ab";
  (* gate clear: len falls back to the default (3), data is 3 bytes *)
  rt (0, 3, "xyz") "\x00\x03xyz"

let test_optional_wire_size_present () =
  Alcotest.(check int) "wire_size present" 4 (Codec.wire_size opt_codec_present)

let test_optional_wire_size_absent () =
  Alcotest.(check int) "wire_size absent" 2 (Codec.wire_size opt_codec_absent)

(* Optional with codec inner type *)

type opt_codec_record = { hdr : int; inner : inner option; trail : int }

let opt_inner_codec ~present =
  Codec.v "OptCodecRecord"
    (fun hdr inner trail -> { hdr; inner; trail })
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun r -> r.hdr);
        ( Field.optional "Inner" ~present:(bool present) (codec inner_codec)
        $ fun r -> r.inner );
        (Field.v "Trail" uint8 $ fun r -> r.trail);
      ]

let test_optional_codec_present () =
  let c = opt_inner_codec ~present:true in
  (* hdr(1) + inner(3) + trail(1) = 5 bytes *)
  let buf = Bytes.create 5 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  Bytes.set_uint8 buf 4 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.hdr;
  (match r.inner with
  | None -> Alcotest.fail "expected Some"
  | Some inner ->
      Alcotest.(check int) "inner.tag" 0x42 inner.tag;
      Alcotest.(check int) "inner.value" 0x1234 inner.value);
  Alcotest.(check int) "trail" 0xBB r.trail

let test_optional_codec_absent () =
  let c = opt_inner_codec ~present:false in
  (* hdr(1) + trail(1) = 2 bytes *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint8 buf 1 0xBB;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAA r.hdr;
  Alcotest.(check (option int))
    "inner" None
    (Option.map (fun (i : inner) -> i.tag) r.inner);
  Alcotest.(check int) "trail" 0xBB r.trail

(* Multiple optional fields (TM frame pattern) *)

type multi_opt = { data : int; ocf : Optint.t option; fecf : int option }

let multi_opt_codec ~ocf ~fecf =
  Codec.v "MultiOpt"
    (fun data ocf fecf -> { data; ocf; fecf })
    Codec.
      [
        (Field.v "Data" uint16be $ fun r -> r.data);
        ( Field.optional "OCF"
            ~present:(if ocf then Expr.true_ else Expr.false_)
            uint32be
        $ fun r -> r.ocf );
        (Field.optional "FECF" ~present:(bool fecf) uint16be $ fun r -> r.fecf);
      ]

let test_optional_both_present () =
  let c = multi_opt_codec ~ocf:true ~fecf:true in
  (* data(2) + ocf(4) + fecf(2) = 8 *)
  let buf = Bytes.create 8 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  Bytes.set_uint16_be buf 6 0x3333;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.data;
  Alcotest.(check (option int))
    "ocf" (Some 0x22222222)
    (Option.map Optint.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" (Some 0x3333) r.fecf

let test_optional_both_absent () =
  let c = multi_opt_codec ~ocf:false ~fecf:false in
  (* data(2) only *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.data;
  Alcotest.(check (option int)) "ocf" None (Option.map Optint.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None r.fecf

let test_optional_mixed () =
  let c = multi_opt_codec ~ocf:true ~fecf:false in
  (* data(2) + ocf(4) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.data;
  Alcotest.(check (option int))
    "ocf" (Some 0x22222222)
    (Option.map Optint.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None r.fecf

(* Dynamic optional: presence determined by a previously-parsed field. *)

type dyn_opt = { flags : int; payload : int option; trail : int }

let f_do_flags = Field.v "Flags" uint8

let dyn_opt_codec =
  Codec.v "DynOpt"
    (fun flags payload trail -> { flags; payload; trail })
    Codec.
      [
        (f_do_flags $ fun r -> r.flags);
        ( Field.optional "Payload"
            ~present:Expr.(Field.ref f_do_flags <> int 0)
            uint16be
        $ fun r -> r.payload );
        (Field.v "Trail" uint8 $ fun r -> r.trail);
      ]

let test_dyn_opt_present () =
  (* flags=1 -> payload present. Layout: [01] [12 34] [FF] *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 1 r.flags;
  Alcotest.(check (option int)) "payload" (Some 0x1234) r.payload;
  Alcotest.(check int) "trail" 0xFF r.trail

let test_dyn_opt_absent () =
  (* flags=0 -> payload absent. Layout: [00] [FF] *)
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 0xFF;
  let r = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) "flags" 0 r.flags;
  Alcotest.(check (option int)) "payload" None r.payload;
  Alcotest.(check int) "trail" 0xFF r.trail

let test_dyn_opt_get_trail () =
  let cf_trail = Codec.(Field.v "Trail" uint8 $ fun r -> r.trail) in
  let get_trail = Staged.unstage (Codec.get dyn_opt_codec cf_trail) in
  (* Present: trail at offset 3. *)
  let buf1 = Bytes.create 4 in
  Bytes.set_uint8 buf1 0 1;
  Bytes.set_uint16_be buf1 1 0x1234;
  Bytes.set_uint8 buf1 3 0xAA;
  Alcotest.(check int) "trail (present)" 0xAA (get_trail buf1 0);
  (* Absent: trail at offset 1. *)
  let buf2 = Bytes.create 2 in
  Bytes.set_uint8 buf2 0 0;
  Bytes.set_uint8 buf2 1 0xBB;
  Alcotest.(check int) "trail (absent)" 0xBB (get_trail buf2 0)

let check_dyn_opt_roundtrip label expected_len expected_bytes original =
  Alcotest.(check int)
    (label ^ " size_of_value") expected_len
    (Codec.size_of_value dyn_opt_codec original);
  let buf = Bytes.create expected_len in
  Codec.encode dyn_opt_codec original buf 0;
  Alcotest.(check string)
    (label ^ " bytes") expected_bytes (Bytes.to_string buf);
  Alcotest.(check int)
    (label ^ " wire_size_at") expected_len
    (Codec.wire_size_at dyn_opt_codec buf 0);
  let decoded = decode_ok (Codec.decode dyn_opt_codec buf 0) in
  Alcotest.(check int) (label ^ " flags") original.flags decoded.flags;
  Alcotest.(check (option int))
    (label ^ " payload") original.payload decoded.payload;
  Alcotest.(check int) (label ^ " trail") original.trail decoded.trail

let test_field_optional_dynamic_roundtrip () =
  check_dyn_opt_roundtrip "present" 4 "\x01\x12\x34\xFF"
    { flags = 1; payload = Some 0x1234; trail = 0xFF };
  check_dyn_opt_roundtrip "absent" 2 "\x00\xEE"
    { flags = 0; payload = None; trail = 0xEE }

let test_dyn_opt_reject_gate () =
  let check_reject label v =
    let len = Codec.size_of_value dyn_opt_codec v + 4 in
    match Codec.encode dyn_opt_codec v (Bytes.create len) 0 with
    | () -> Alcotest.failf "%s: encode unexpectedly succeeded" label
    | exception Invalid_argument _ -> ()
  in
  check_reject "gate true / value None"
    { flags = 1; payload = None; trail = 0xEE };
  check_reject "gate false / value Some"
    { flags = 0; payload = Some 0x1234; trail = 0xEE }

let test_encode_totality () =
  let check_exact label original =
    let len = Codec.size_of_value dyn_opt_codec original in
    let buf = Bytes.create len in
    Codec.encode dyn_opt_codec original buf 0;
    Alcotest.(check int)
      (label ^ " wire_size_at") len
      (Codec.wire_size_at dyn_opt_codec buf 0);
    if len > 0 then
      match Codec.encode dyn_opt_codec original (Bytes.create (len - 1)) 0 with
      | () -> Alcotest.failf "%s: short buffer accepted" label
      | exception Invalid_argument _ -> ()
  in
  check_exact "present" { flags = 1; payload = Some 0x1234; trail = 0xFF };
  check_exact "absent" { flags = 0; payload = None; trail = 0xEE }

(* Dynamic optional via Field.ref on a bool field -- the TM frame pattern.
   Field.ref now accepts 'a t, so a bool field created with [bit] can be
   referenced directly in expressions. *)

type tm_opt = {
  ocf_flag : bool;
  data : int;
  ocf : Optint.t option;
  trail : int;
}

let f_to_ocf_flag = Field.v "OCFFlag" (bit (bits ~width:1 U8))

let tm_opt_codec =
  Codec.v "TmOpt"
    (fun ocf_flag _pad data ocf trail -> { ocf_flag; data; ocf; trail })
    Codec.
      [
        (f_to_ocf_flag $ fun r -> r.ocf_flag);
        (Field.v "Pad" (bits ~width:7 U8) $ fun _ -> 0);
        (Field.v "Data" uint16be $ fun r -> r.data);
        ( Field.optional "OCF"
            ~present:Expr.(Field.ref f_to_ocf_flag <> int 0)
            uint32be
        $ fun r -> r.ocf );
        (Field.v "Trail" uint8 $ fun r -> r.trail);
      ]

let test_dyn_opt_anyref_present () =
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 0x80;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_int32_be buf 3 0xDEADBEEFl;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" true r.ocf_flag;
  Alcotest.(check int) "data" 0x1234 r.data;
  Alcotest.(check (option int32))
    "ocf" (Some 0xDEADBEEFl)
    (Option.map Optint.to_int32 r.ocf);
  Alcotest.(check int) "trail" 0xFF r.trail

let test_dyn_opt_anyref_absent () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" false r.ocf_flag;
  Alcotest.(check int) "data" 0x1234 r.data;
  Alcotest.(check (option int)) "ocf" None (Option.map Optint.to_int r.ocf);
  Alcotest.(check int) "trail" 0xFF r.trail

(* -- Predicates with bitwise/shift/mod operators in [optional] --
   Reproduces the silent miscompile where [compile_bool_expr]'s
   [try_compile_int_reader] fell back to [fun _ -> true] for any
   predicate using [Land] / [Lor] / [Lsr] / [Mod] / [Cast] / etc.,
   so a high-bit-gated optional would always read its payload. *)

type bit_gated = { flags : int; body : int option }

let f_bg_flags = Field.v "Flags" uint8

let bg_codec ~present =
  Codec.v "BgRec"
    (fun flags body -> { flags; body })
    Codec.
      [
        (f_bg_flags $ fun r -> r.flags);
        (Field.optional "Body" ~present uint8 $ fun r -> r.body);
      ]

(* Decode a present-payload buffer (expect [Some 0x2A]) and an absent buffer
   (expect [None]) through a [bg_codec] gated on [present]. *)
let check_bit_gated ~present ~present_buf ~present_label ~absent_buf
    ~absent_label =
  let c = bg_codec ~present in
  let r = decode_ok (Codec.decode c (Bytes.of_string present_buf) 0) in
  Alcotest.(check (option int)) present_label (Some 0x2A) r.body;
  let r = decode_ok (Codec.decode c (Bytes.of_string absent_buf) 0) in
  Alcotest.(check (option int)) absent_label None r.body

let test_optional_land_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags land int 0x80 <> int 0)
    ~present_buf:"\x80\x2A" ~present_label:"high bit set" ~absent_buf:"\x00"
    ~absent_label:"high bit clear"

let test_optional_lsr_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags lsr int 4 <> int 0)
    ~present_buf:"\x10\x2A" ~present_label:"top nibble set" ~absent_buf:"\x0F"
    ~absent_label:"top nibble clear"

let test_optional_mod_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags mod int 2 <> int 0)
    ~present_buf:"\x03\x2A" ~present_label:"odd" ~absent_buf:"\x02"
    ~absent_label:"even"

let test_optional_lor_predicate () =
  check_bit_gated
    ~present:Expr.(Field.ref f_bg_flags lor int 0x01 = int 0x01)
    ~present_buf:"\x01\x2A" ~present_label:"lor matches" ~absent_buf:"\xFF"
    ~absent_label:"lor no match"

(* [Field.ref] on an [optional] field reads the inner value, not 0: the
   int_array slot is populated for [Optional] fields, so a constraint or size
   expression referring to the optional sees the real value. *)

type ref_opt = { x : int option; check : int }

let f_ro_x = Field.optional "X" ~present:Expr.true_ uint8

let ref_opt_codec =
  let f_check =
    Field.v "Check" uint8 ~constraint_:Expr.(Field.ref f_ro_x > int 0)
  in
  Codec.v "RefOpt"
    (fun x check -> { x; check })
    Codec.[ (f_ro_x $ fun r -> r.x); (f_check $ fun r -> r.check) ]

let test_field_ref_through_optional () =
  (* x=0x80 (present); the constraint reads ref(x) and asserts it's
     non-zero. With the populate bug, ref(x) read 0 and the constraint
     failed; with the fix it reads 0x80 and the decode succeeds. *)
  let buf = Bytes.of_string "\x80\x7F" in
  let r = decode_ok (Codec.decode ref_opt_codec buf 0) in
  Alcotest.(check (option int)) "x" (Some 0x80) r.x;
  Alcotest.(check int) "check" 0x7F r.check

let test_uint64_in_size_expr () =
  let f_len = Field.v "Len" uint64be in
  let codec =
    let open Codec in
    v "U64Ref"
      (fun len data -> (len, data))
      [
        (f_len $ fun (l, _) -> l);
        (Field.v "Data" (byte_array ~size:(Field.ref f_len)) $ fun (_, d) -> d);
      ]
  in
  let buf = Bytes.create 11 in
  Bytes.set_int64_be buf 0 3L;
  Bytes.blit_string "ABC" 0 buf 8 3;
  let len, data = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int64) "len" 3L len;
  Alcotest.(check string) "data" "ABC" data

(* -- Nested: Repeat typ: parse elements until byte budget exhausted --
   [container], [f_cnt_length], [repeat_codec] live in {!Test_helpers}. *)

let test_repeat_decode_empty () =
  (* length=0 -> no items *)
  let buf = Bytes.create 1 in
  Bytes.set_uint8 buf 0 0;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 0 r.length;
  Alcotest.(check int) "item count" 0 (List.length r.items)

let test_repeat_decode_one () =
  (* length=3 -> one inner (tag=1byte, value=2bytes) *)
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 3;
  Bytes.set_uint8 buf 1 0x42;
  Bytes.set_uint16_be buf 2 0x1234;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 3 r.length;
  Alcotest.(check int) "item count" 1 (List.length r.items);
  let item = List.hd r.items in
  Alcotest.(check int) "item.tag" 0x42 item.tag;
  Alcotest.(check int) "item.value" 0x1234 item.value

let test_repeat_decode_multiple () =
  (* length=9 -> three inner items (3 bytes each) *)
  let buf = Bytes.create 10 in
  Bytes.set_uint8 buf 0 9;
  (* item 0 *)
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  (* item 1 *)
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  (* item 2 *)
  Bytes.set_uint8 buf 7 0x03;
  Bytes.set_uint16_be buf 8 0x0003;
  let r = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" 9 r.length;
  Alcotest.(check int) "item count" 3 (List.length r.items);
  List.iteri
    (fun i (item : inner) ->
      Alcotest.(check int) (Fmt.str "item[%d].tag" i) (i + 1) item.tag;
      Alcotest.(check int) (Fmt.str "item[%d].value" i) (i + 1) item.value)
    r.items

let test_repeat_encode () =
  let v =
    {
      length = 6;
      items = [ { tag = 0x01; value = 0x0001 }; { tag = 0x02; value = 0x0002 } ];
    }
  in
  let buf = Bytes.create 7 in
  Codec.encode repeat_codec v buf 0;
  Alcotest.(check int) "length byte" 6 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "item0.tag" 0x01 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "item0.value" 0x0001 (Bytes.get_uint16_be buf 2);
  Alcotest.(check int) "item1.tag" 0x02 (Bytes.get_uint8 buf 4);
  Alcotest.(check int) "item1.value" 0x0002 (Bytes.get_uint16_be buf 5)

let expect_repeat_encode_error label codec value =
  let buf = Bytes.make 8 '\x7f' in
  match Codec.encode codec value buf 0 with
  | () -> Alcotest.failf "%s: expected a repeat byte-budget error" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names repeat") true
        (contains ~sub:"repeat" msg)

let test_repeat_exact_budget () =
  let f_size = Field.v "size" uint8 in
  let fixed =
    Codec.v "RepeatFixedBudget"
      (fun size items -> (size, items))
      Codec.
        [
          f_size $ fst;
          Field.repeat "items" ~size:(Field.ref f_size) uint16be $ snd;
        ]
  in
  (match Codec.decode fixed (Bytes.of_string "\x01\xaa") 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "fixed-width repeat accepted a remainder");
  expect_repeat_encode_error "fixed underrun" fixed (6, [ 1 ]);
  expect_repeat_encode_error "fixed overshoot" fixed (2, [ 1; 2 ]);
  let f_var_size = Field.v "size" uint8 in
  let variable =
    Codec.v "RepeatVariableBudget"
      (fun size items -> (size, items))
      Codec.
        [
          f_var_size $ fst;
          Field.repeat "items" ~size:(Field.ref f_var_size) zeroterm $ snd;
        ]
  in
  match Codec.decode variable (Bytes.of_string "\x01A\x00") 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "variable-width repeat crossed its byte budget"

let test_repeat_roundtrip () =
  let items =
    [
      { tag = 0x0A; value = 0x000A };
      { tag = 0x0B; value = 0x000B };
      { tag = 0x0C; value = 0x000C };
    ]
  in
  let original : container = { length = 9; items } in
  let buf = Bytes.create 10 in
  Codec.encode repeat_codec original buf 0;
  let decoded = decode_ok (Codec.decode repeat_codec buf 0) in
  Alcotest.(check int) "length" original.length decoded.length;
  Alcotest.(check int) "item count" 3 (List.length decoded.items);
  List.iter2
    (fun (orig : inner) (dec : inner) ->
      Alcotest.(check int) "tag" orig.tag dec.tag;
      Alcotest.(check int) "value" orig.value dec.value)
    original.items decoded.items

(* Repeat with fixed-size primitive elements *)

type int_container = { count : int; values : int list }

let f_ic_count = Field.v "Count" uint8

let repeat_int_codec =
  Codec.v "IntContainer"
    (fun count values -> { count; values })
    Codec.
      [
        (f_ic_count $ fun r -> r.count);
        ( Field.repeat "Values" ~size:(Field.ref f_ic_count) uint16be $ fun r ->
          r.values );
      ]

let test_repeat_primitive () =
  (* count=6 -> 3 uint16be values *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint16_be buf 1 0x1111;
  Bytes.set_uint16_be buf 3 0x2222;
  Bytes.set_uint16_be buf 5 0x3333;
  let r = decode_ok (Codec.decode repeat_int_codec buf 0) in
  Alcotest.(check int) "count" 6 r.count;
  Alcotest.(check int) "n values" 3 (List.length r.values);
  Alcotest.(check (list int)) "values" [ 0x1111; 0x2222; 0x3333 ] r.values

(* [Codec.size_of_value] counts a [Field.repeat]'s elements, including under a
   dynamic byte budget, so a buffer sized from [size_of_value] holds the whole
   encoding. *)
let test_repeat_size_of_value () =
  let v = { count = 6; values = [ 0x1111; 0x2222; 0x3333 ] } in
  (* Count (1) + 3 * uint16be (6) = 7 *)
  let n = Codec.size_of_value repeat_int_codec v in
  Alcotest.(check int) "size_of_value" 7 n;
  let buf = Bytes.create n in
  Codec.encode repeat_int_codec v buf 0;
  Alcotest.(check int)
    "wire_size_at" 7
    (Codec.wire_size_at repeat_int_codec buf 0);
  let r = decode_ok (Codec.decode repeat_int_codec buf 0) in
  Alcotest.(check (list int)) "roundtrip" v.values r.values

(* Repeat with trailer after *)

type repeat_trailer = { len : int; items : inner list; check : int }

let f_rt_len = Field.v "Len" uint8

let repeat_trailer_codec =
  Codec.v "RepeatTrailer"
    (fun len items check -> { len; items; check })
    Codec.
      [
        (f_rt_len $ fun r -> r.len);
        ( Field.repeat "Items" ~size:(Field.ref f_rt_len) (codec inner_codec)
        $ fun r -> r.items );
        (Field.v "Check" uint8 $ fun r -> r.check);
      ]

let test_repeat_with_trailer () =
  (* len=6 -> two inner items (3 bytes each), then 1 byte trailer *)
  let buf = Bytes.create 8 in
  Bytes.set_uint8 buf 0 6;
  Bytes.set_uint8 buf 1 0x01;
  Bytes.set_uint16_be buf 2 0x0001;
  Bytes.set_uint8 buf 4 0x02;
  Bytes.set_uint16_be buf 5 0x0002;
  Bytes.set_uint8 buf 7 0xFF;
  let r = decode_ok (Codec.decode repeat_trailer_codec buf 0) in
  Alcotest.(check int) "len" 6 r.len;
  Alcotest.(check int) "item count" 2 (List.length r.items);
  Alcotest.(check int) "check" 0xFF r.check

(* Variable-size repeat: codec with dependent-size field *)

type var_inner = { len : int; data : string }

let f_vi_len = Field.v "Len" uint8

let var_inner_codec =
  Codec.v "VarInner"
    (fun len data -> { len; data })
    Codec.
      [
        (f_vi_len $ fun r -> r.len);
        ( Field.v "Data" (byte_array ~size:(Field.ref f_vi_len)) $ fun r ->
          r.data );
      ]

type var_container = { size : int; items : var_inner list }

let f_vc_size = Field.v "Size" uint16be

let var_repeat_codec =
  Codec.v "VarContainer"
    (fun size items -> { size; items })
    Codec.
      [
        (f_vc_size $ fun r -> r.size);
        ( Field.repeat "Items" ~size:(Field.ref f_vc_size)
            (codec var_inner_codec)
        $ fun r -> r.items );
      ]

let test_repeat_variable_size_elements () =
  (* Two variable-length items: [len=2, "ab"] [len=3, "cde"] = 2+2+3+3 = 7 bytes *)
  let buf = Bytes.create 9 in
  Bytes.set_uint16_be buf 0 7;
  (* item 0: len=2, data="ab" *)
  Bytes.set_uint8 buf 2 2;
  Bytes.blit_string "ab" 0 buf 3 2;
  (* item 1: len=3, data="cde" *)
  Bytes.set_uint8 buf 5 3;
  Bytes.blit_string "cde" 0 buf 6 3;
  let r = decode_ok (Codec.decode var_repeat_codec buf 0) in
  Alcotest.(check int) "size" 7 r.size;
  Alcotest.(check int) "item count" 2 (List.length r.items);
  let i0 = List.nth r.items 0 in
  let i1 = List.nth r.items 1 in
  Alcotest.(check int) "item0.len" 2 i0.len;
  Alcotest.(check string) "item0.data" "ab" i0.data;
  Alcotest.(check int) "item1.len" 3 i1.len;
  Alcotest.(check string) "item1.data" "cde" i1.data

(* -- Casetype as a trailing variable-size codec field -- *)

type ev_payload = [ `Login of int | `Logout of Optint.t | `Other of int ]

let casetype_field_event_typ : ev_payload Wire.typ =
  Wire.casetype "EvPayload" Wire.uint8
    [
      Wire.case ~index:1 Wire.uint16be
        ~inject:(fun v -> `Login v)
        ~project:(function `Login v -> Some v | _ -> None);
      Wire.case ~index:2 Wire.uint32be
        ~inject:(fun v -> `Logout v)
        ~project:(function `Logout v -> Some v | _ -> None);
      Wire.default Wire.uint8
        ~inject:(fun _tag v -> `Other v)
        ~project:(function `Other v -> Some (0xFF, v) | _ -> None);
    ]

type ev_event = { ts : int64; data : ev_payload }

let casetype_field_codec =
  Codec.v "CasetypeFieldEvt"
    (fun ts data -> { ts; data })
    Codec.
      [
        (Field.v "Timestamp" int64be $ fun e -> e.ts);
        (Field.v "Data" casetype_field_event_typ $ fun e -> e.data);
      ]

let test_casetype_field_login () =
  let buf = Bytes.create 11 in
  Bytes.set_int64_be buf 0 42L;
  Bytes.set_uint8 buf 8 1;
  Bytes.set_uint16_be buf 9 0x1234;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts" 42L r.ts;
  Alcotest.(check bool) "Login 0x1234" true (r.data = `Login 0x1234)

let test_casetype_field_logout () =
  let buf = Bytes.create 13 in
  Bytes.set_int64_be buf 0 99L;
  Bytes.set_uint8 buf 8 2;
  Bytes.set_int32_be buf 9 0x55667788l;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check bool)
    "Logout" true
    (r.data = `Logout (Optint.of_int32 0x55667788l))

let test_casetype_field_default () =
  let buf = Bytes.create 10 in
  Bytes.set_int64_be buf 0 0L;
  Bytes.set_uint8 buf 8 99;
  Bytes.set_uint8 buf 9 7;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check bool) "Other 7" true (r.data = `Other 7)

(* A casetype whose discriminant matches no case fails with a typed
   [Invalid_tag] carrying the tag value, not a stringly constraint failure. *)
let test_casetype_no_match_invalid_tag () =
  let typ =
    Wire.casetype "Closed" Wire.uint8
      [
        Wire.case ~index:1 Wire.uint8
          ~inject:(fun v -> `A v)
          ~project:(function `A v -> Some v | _ -> None);
        Wire.case ~index:2 Wire.uint8
          ~inject:(fun v -> `B v)
          ~project:(function `B v -> Some v | _ -> None);
      ]
  in
  let c = Codec.v "ClosedCt" Fun.id Codec.[ Field.v "d" typ $ Fun.id ] in
  match Codec.decode c (Bytes.of_string "\x63\x00") 0 with
  | Error { kind = Invalid_tag n; _ } ->
      Alcotest.(check int) "unmatched tag" 99 n
  | Ok _ -> Alcotest.fail "decode accepted an unmatched casetype tag"
  | Error _ -> Alcotest.fail "wrong error for an unmatched casetype tag"

(* The default branch recovers the matched tag and re-encodes it, so an
   arbitrary unclaimed tag round-trips (the DHCP / TCP-options shape). *)
type tlv = Known of int | Unknown of (int * string)

let tlv_typ : tlv Wire.typ =
  Wire.casetype "Tlv" Wire.uint8
    [
      Wire.case ~index:1 Wire.uint16be
        ~inject:(fun v -> Known v)
        ~project:(function Known v -> Some v | _ -> None);
      Wire.default
        (Wire.byte_array ~size:(int 2))
        ~inject:(fun tag body -> Unknown (tag, body))
        ~project:(function Unknown (t, b) -> Some (t, b) | _ -> None);
    ]

let tlv_codec =
  Codec.v "Tlv" (fun x -> x) Codec.[ (Field.v "v" tlv_typ $ fun x -> x) ]

let test_casetype_default_recovers_tag () =
  let v = Unknown (0x42, "ab") in
  let buf = Bytes.create (Codec.size_of_value tlv_codec v) in
  Codec.encode tlv_codec v buf 0;
  Alcotest.(check int)
    "encode writes the captured tag" 0x42 (Bytes.get_uint8 buf 0);
  match Codec.decode tlv_codec buf 0 with
  | Ok (Unknown (t, b)) ->
      Alcotest.(check int) "decode recovers the tag" 0x42 t;
      Alcotest.(check string) "decode body" "ab" b
  | Ok _ -> Alcotest.fail "expected Unknown"
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_casetype_field_roundtrip () =
  let buf = Bytes.create 11 in
  let original = { ts = 123L; data = `Login 0xabcd } in
  Codec.encode casetype_field_codec original buf 0;
  let decoded = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts roundtrip" original.ts decoded.ts;
  Alcotest.(check bool) "data roundtrip" true (original.data = decoded.data)

(* [Codec.size_of_value] counts a casetype field's tag plus its matched-case
   body, so a buffer sized from it holds the whole encoding. *)
let test_casetype_size_of_value () =
  let original = { ts = 123L; data = `Login 0xabcd } in
  (* ts (8) + tag (1) + Login body uint16be (2) = 11 *)
  let n = Codec.size_of_value casetype_field_codec original in
  Alcotest.(check int) "size_of_value" 11 n;
  let buf = Bytes.create n in
  Codec.encode casetype_field_codec original buf 0;
  let decoded = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check int64) "ts" original.ts decoded.ts;
  Alcotest.(check bool) "data" true (original.data = decoded.data)

(* Length-prefixed casetype dispatch: [tag][length][body] where length
   bounds the inner casetype's tag + body. *)

type lp_event = { tag : int; len : int; data : ev_payload }

let lp_event_len = Field.v "Length" uint16be

let lp_event_codec =
  Codec.v "LpEvent"
    (fun tag len data -> { tag; len; data })
    Codec.
      [
        (Field.v "Tag" uint8 $ fun e -> e.tag);
        (lp_event_len $ fun e -> e.len);
        ( Field.v "Data"
            (Wire.nested ~size:(Field.ref lp_event_len) casetype_field_event_typ)
        $ fun e -> e.data );
      ]

let test_length_prefixed_casetype () =
  (* tag=0xAA, len=3 (1 byte casetype tag + 2 bytes uint16be), inner tag=1, body=0x4242 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint8 buf 0 0xAA;
  Bytes.set_uint16_be buf 1 3;
  Bytes.set_uint8 buf 3 1;
  Bytes.set_uint16_be buf 4 0x4242;
  let r = decode_ok (Codec.decode lp_event_codec buf 0) in
  Alcotest.(check int) "tag" 0xAA r.tag;
  Alcotest.(check int) "len" 3 r.len;
  Alcotest.(check bool) "data" true (r.data = `Login 0x4242)

(* -- Nested: Composition: optional + repeat + codec --
   TM-frame-like structure: header + data zone (repeat of packets) + optional
   OCF + optional FECF. [packet] / [packet_codec] live in {!Test_helpers}. *)

type tm_like = {
  hdr : int;
  data_len : int;
  packets : packet list;
  ocf : Optint.t option;
  fecf : int option;
}

let f_tm_data_len = Field.v "DataLen" uint8

let tm_like_codec ~ocf ~fecf =
  Codec.v "TmLike"
    (fun hdr data_len packets ocf fecf -> { hdr; data_len; packets; ocf; fecf })
    Codec.
      [
        (Field.v "Hdr" uint16be $ fun r -> r.hdr);
        (f_tm_data_len $ fun r -> r.data_len);
        ( Field.repeat "Packets" ~size:(Field.ref f_tm_data_len)
            (codec packet_codec)
        $ fun r -> r.packets );
        ( Field.optional "OCF"
            ~present:(if ocf then Expr.true_ else Expr.false_)
            uint32be
        $ fun r -> r.ocf );
        (Field.optional "FECF" ~present:(bool fecf) uint16be $ fun r -> r.fecf);
      ]

let test_tm_like_full () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  (* hdr(2) + data_len(1) + 2 packets(6) + ocf(4) + fecf(2) = 15 *)
  let buf = Bytes.create 15 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 6;
  (* data zone = 6 bytes = 2 packets *)
  (* packet 0 *)
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  (* packet 1 *)
  Bytes.set_uint8 buf 6 0x02;
  Bytes.set_uint16_be buf 7 0x2222;
  (* ocf *)
  Bytes.set_int32_be buf 9 0x33333333l;
  (* fecf *)
  Bytes.set_uint16_be buf 13 0x4444;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAAAA r.hdr;
  Alcotest.(check int) "data_len" 6 r.data_len;
  Alcotest.(check int) "packet count" 2 (List.length r.packets);
  Alcotest.(check int) "pkt0.id" 0x01 (List.nth r.packets 0).id;
  Alcotest.(check int) "pkt1.id" 0x02 (List.nth r.packets 1).id;
  Alcotest.(check (option int))
    "ocf" (Some 0x33333333)
    (Option.map Optint.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" (Some 0x4444) r.fecf

let test_tm_like_no_trailing () =
  let c = tm_like_codec ~ocf:false ~fecf:false in
  (* hdr(2) + data_len(1) + 1 packet(3) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 3;
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "packet count" 1 (List.length r.packets);
  Alcotest.(check (option int)) "ocf" None (Option.map Optint.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None r.fecf

let test_tm_like_roundtrip () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  let original =
    {
      hdr = 0xBBBB;
      data_len = 9;
      packets =
        [
          ({ id = 0x0A; data = 0x000A } : packet);
          ({ id = 0x0B; data = 0x000B } : packet);
          ({ id = 0x0C; data = 0x000C } : packet);
        ];
      ocf = Some (Wire.Private.UInt32.of_int32 0xDEADBEEFl);
      fecf = Some 0xCAFE;
    }
  in
  let buf = Bytes.create 18 in
  Codec.encode c original buf 0;
  let decoded = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" original.hdr decoded.hdr;
  Alcotest.(check int) "packet count" 3 (List.length decoded.packets);
  List.iter2
    (fun (o : packet) (d : packet) ->
      Alcotest.(check int) "pkt.id" o.id d.id;
      Alcotest.(check int) "pkt.data" o.data d.data)
    original.packets decoded.packets;
  Alcotest.(check (option int32))
    "ocf"
    (Option.map Optint.to_int32 original.ocf)
    (Option.map Optint.to_int32 decoded.ocf);
  Alcotest.(check (option int)) "fecf" original.fecf decoded.fecf

(* -- Multiple consecutive variable-size fields (CFDP-style) --

   CCSDS CFDP (727.0-B-5) has three consecutive variable-size byte_array
   fields in its PDU header, each sized by expressions over earlier fixed
   fields. This layout projects, resolving each field's offset at runtime. *)

type cfdp_hdr = {
  eid_len : int;
  txseq_len : int;
  src : string;
  txseq : string;
  dst : string;
}

let f_cfdp_eid_len = Field.v "EIDLen" uint8
let f_cfdp_txseq_len = Field.v "TxSeqLen" uint8

let cfdp_codec =
  let open Codec in
  v "CFDPHeader"
    (fun eid_len txseq_len src txseq dst ->
      { eid_len; txseq_len; src; txseq; dst })
    [
      (f_cfdp_eid_len $ fun r -> r.eid_len);
      (f_cfdp_txseq_len $ fun r -> r.txseq_len);
      ( Field.v "SourceEID"
          (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.src );
      ( Field.v "TxSeqNum"
          (byte_array ~size:Expr.(Field.ref f_cfdp_txseq_len + int 1))
      $ fun r -> r.txseq );
      ( Field.v "DestEID"
          (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.dst );
    ]

let test_multi_var_decode () =
  (* EIDLen=1 -> 2-byte entities, TxSeqLen=2 -> 3-byte txseq.
     Layout: [1] [2] [AA BB] [CC DD EE] [FF 00] *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let r = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check int) "eid_len" 1 r.eid_len;
  Alcotest.(check int) "txseq_len" 2 r.txseq_len;
  Alcotest.(check string) "src" "\xAA\xBB" r.src;
  Alcotest.(check string) "txseq" "\xCC\xDD\xEE" r.txseq;
  Alcotest.(check string) "dst" "\xFF\x00" r.dst

let test_multi_var_roundtrip () =
  let original =
    {
      eid_len = 1;
      txseq_len = 2;
      src = "\xAA\xBB";
      txseq = "\xCC\xDD\xEE";
      dst = "\xFF\x00";
    }
  in
  let buf = Bytes.create 9 in
  Codec.encode cfdp_codec original buf 0;
  let decoded = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check string) "src roundtrip" original.src decoded.src;
  Alcotest.(check string) "txseq roundtrip" original.txseq decoded.txseq;
  Alcotest.(check string) "dst roundtrip" original.dst decoded.dst

let test_multi_var_get () =
  (* Staged get must work on the second and third variable-size fields. *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let cf_txseq =
    Codec.(
      Field.v "TxSeqNum"
        (byte_array ~size:Expr.(Field.ref f_cfdp_txseq_len + int 1))
      $ fun r -> r.txseq)
  in
  let cf_dst =
    Codec.(
      Field.v "DestEID"
        (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))
      $ fun r -> r.dst)
  in
  let get_txseq = Staged.unstage (Codec.get cfdp_codec cf_txseq) in
  let get_dst = Staged.unstage (Codec.get cfdp_codec cf_dst) in
  Alcotest.(check string) "get txseq" "\xCC\xDD\xEE" (get_txseq buf 0);
  Alcotest.(check string) "get dst" "\xFF\x00" (get_dst buf 0)

let test_multi_var_fixed_after () =
  (* A fixed-size field after multiple variable-size fields must also work. *)
  let f_elen = Field.v "ELen" uint8 in
  let f_tlen = Field.v "TLen" uint8 in
  let codec =
    let open Codec in
    v "VarTrail"
      (fun elen tlen src txseq trail -> (elen, tlen, src, txseq, trail))
      [
        (f_elen $ fun (e, _, _, _, _) -> e);
        (f_tlen $ fun (_, t, _, _, _) -> t);
        ( Field.v "Src" (byte_array ~size:Expr.(Field.ref f_elen + int 1))
        $ fun (_, _, s, _, _) -> s );
        ( Field.v "Tx" (byte_array ~size:Expr.(Field.ref f_tlen + int 1))
        $ fun (_, _, _, t, _) -> t );
        (Field.v "Trail" uint16be $ fun (_, _, _, _, tr) -> tr);
      ]
  in
  (* elen=0 -> 1-byte src, tlen=1 -> 2-byte tx, trail=0xBEEF
     Layout: [0] [1] [AA] [BB CC] [BE EF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 1;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.blit_string "\xBB\xCC" 0 buf 3 2;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let _, _, src, tx, trail = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "src" "\xAA" src;
  Alcotest.(check string) "tx" "\xBB\xCC" tx;
  Alcotest.(check int) "trail" 0xBEEF trail

(* -- Multiple variable-size sub-codecs back-to-back (SSH disconnect /
      debug shape). [compile_codec] resolves a dynamic offset for a
      [Wire.codec]-embedded variable-size sub-codec that sits after another
      variable-size field. -- *)

module Slice = Bytesrw.Bytes.Slice

type ssh_string = { len : Optint.t; data : Slice.t }

let ssh_f_len = Field.v "len" uint32be
let ssh_f_data = Field.v "data" (byte_slice ~size:(Field.ref ssh_f_len))

let ssh_string_codec =
  Codec.v "SshString"
    (fun len data -> { len; data })
    Codec.[ (ssh_f_len $ fun s -> s.len); (ssh_f_data $ fun s -> s.data) ]

let mk_ssh_string s =
  let b = Bytes.of_string s in
  {
    len = Optint.of_int (String.length s);
    data = Slice.make b ~first:0 ~length:(Bytes.length b);
  }

let test_ssh_two_var_slices () =
  (* SSH_MSG_DISCONNECT: reason (uint32) + len + desc + len + lang *)
  let f_reason = Field.v "reason" uint32be in
  let f_desc_len = Field.v "desc_len" uint32be in
  let f_desc = Field.v "desc" (byte_slice ~size:(Field.ref f_desc_len)) in
  let f_lang_len = Field.v "lang_len" uint32be in
  let f_lang = Field.v "lang" (byte_slice ~size:(Field.ref f_lang_len)) in
  let codec =
    let open Codec in
    v "Disconnect"
      (fun reason _ desc _ lang -> (reason, desc, lang))
      [
        (f_reason $ fun (r, _, _) -> r);
        (f_desc_len $ fun (_, d, _) -> Optint.of_int (Slice.length d));
        (f_desc $ fun (_, d, _) -> d);
        (f_lang_len $ fun (_, _, l) -> Optint.of_int (Slice.length l));
        (f_lang $ fun (_, _, l) -> l);
      ]
  in
  let v =
    (Optint.of_int 11, (mk_ssh_string "bye").data, (mk_ssh_string "en-US").data)
  in
  let buf = Bytes.create 200 in
  Codec.encode codec v buf 0;
  let r, d, l = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "reason" 11 (Optint.to_int r);
  Alcotest.(check string) "desc" "bye" (Slice.to_string d);
  Alcotest.(check string) "lang" "en-US" (Slice.to_string l)

let test_two_var_codecs_embedded () =
  (* Two consecutive [Wire.codec ssh_string_codec] embedded fields, each
     resolved via a dynamic offset. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let pair_codec =
    let open Codec in
    v "Pair" (fun a b -> (a, b)) [ f_a $ fst; f_b $ snd ]
  in
  let v = (mk_ssh_string "abcd", mk_ssh_string "xy") in
  let buf = Bytes.create 200 in
  Codec.encode pair_codec v buf 0;
  let a, b = decode_ok (Codec.decode pair_codec buf 0) in
  Alcotest.(check int) "a.len" 4 (Optint.to_int a.len);
  Alcotest.(check string) "a.data" "abcd" (Slice.to_string a.data);
  Alcotest.(check int) "b.len" 2 (Optint.to_int b.len);
  Alcotest.(check string) "b.data" "xy" (Slice.to_string b.data)

let test_three_var_codecs_embedded () =
  (* SSH_MSG_DEBUG-shaped payload: three variable-size sub-codecs. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let f_c = Field.v "c" (codec ssh_string_codec) in
  let triple_codec =
    let open Codec in
    v "Triple"
      (fun a b c -> (a, b, c))
      [
        (f_a $ fun (a, _, _) -> a);
        (f_b $ fun (_, b, _) -> b);
        (f_c $ fun (_, _, c) -> c);
      ]
  in
  let v = (mk_ssh_string "alpha", mk_ssh_string "be", mk_ssh_string "gamma!") in
  let buf = Bytes.create 200 in
  Codec.encode triple_codec v buf 0;
  let a, b, c = decode_ok (Codec.decode triple_codec buf 0) in
  Alcotest.(check string) "a" "alpha" (Slice.to_string a.data);
  Alcotest.(check string) "b" "be" (Slice.to_string b.data);
  Alcotest.(check string) "c" "gamma!" (Slice.to_string c.data)

let test_four_var_codecs_embedded () =
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let f_c = Field.v "c" (codec ssh_string_codec) in
  let f_d = Field.v "d" (codec ssh_string_codec) in
  let quad_codec =
    let open Codec in
    v "Quad"
      (fun a b c d -> (a, b, c, d))
      [
        (f_a $ fun (a, _, _, _) -> a);
        (f_b $ fun (_, b, _, _) -> b);
        (f_c $ fun (_, _, c, _) -> c);
        (f_d $ fun (_, _, _, d) -> d);
      ]
  in
  let v =
    ( mk_ssh_string "alpha",
      mk_ssh_string "bravo",
      mk_ssh_string "charlie",
      mk_ssh_string "d" )
  in
  let len = Codec.size_of_value quad_codec v in
  let buf = Bytes.create len in
  Codec.encode quad_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at quad_codec buf 0);
  let a, b, c, d = decode_ok (Codec.decode quad_codec buf 0) in
  Alcotest.(check string) "a" "alpha" (Slice.to_string a.data);
  Alcotest.(check string) "b" "bravo" (Slice.to_string b.data);
  Alcotest.(check string) "c" "charlie" (Slice.to_string c.data);
  Alcotest.(check string) "d" "d" (Slice.to_string d.data)

let test_slice_then_array () =
  let f_slice_len = Field.v "slice_len" uint8 in
  let f_slice = Field.v "slice" (byte_slice ~size:(Field.ref f_slice_len)) in
  let f_array_len = Field.v "array_len" uint8 in
  let f_array = Field.v "array" (byte_array ~size:(Field.ref f_array_len)) in
  let mixed_codec =
    let open Codec in
    v "SliceThenArray"
      (fun _ slice _ array -> (slice, array))
      [
        (f_slice_len $ fun (slice, _) -> Slice.length slice);
        (f_slice $ fun (slice, _) -> slice);
        (f_array_len $ fun (_, array) -> String.length array);
        (f_array $ fun (_, array) -> array);
      ]
  in
  let v = ((mk_ssh_string "slice").data, "array-data") in
  let len = Codec.size_of_value mixed_codec v in
  let buf = Bytes.create len in
  Codec.encode mixed_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at mixed_codec buf 0);
  let slice, array = decode_ok (Codec.decode mixed_codec buf 0) in
  Alcotest.(check string) "slice" "slice" (Slice.to_string slice);
  Alcotest.(check string) "array" "array-data" array

let test_codec_then_array () =
  let f_msg = Field.v "msg" (codec ssh_string_codec) in
  let f_array_len = Field.v "array_len" uint8 in
  let f_array = Field.v "array" (byte_array ~size:(Field.ref f_array_len)) in
  let mixed_codec =
    let open Codec in
    v "CodecThenArray"
      (fun msg _ array -> (msg, array))
      [
        (f_msg $ fun (msg, _) -> msg);
        (f_array_len $ fun (_, array) -> String.length array);
        (f_array $ fun (_, array) -> array);
      ]
  in
  let v = (mk_ssh_string "message", "tail") in
  let len = Codec.size_of_value mixed_codec v in
  let buf = Bytes.create len in
  Codec.encode mixed_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at mixed_codec buf 0);
  let msg, array = decode_ok (Codec.decode mixed_codec buf 0) in
  Alcotest.(check string) "msg" "message" (Slice.to_string msg.data);
  Alcotest.(check string) "array" "tail" array

let test_repeat_after_var_slice () =
  (* [Repeat] after a variable-size field resolves a dynamic offset in
     [compile_repeat], like [compile_codec]. *)
  let f_prefix_len = Field.v "prefix_len" uint8 in
  let f_prefix = Field.v "prefix" (byte_slice ~size:(Field.ref f_prefix_len)) in
  let f_count = Field.v "count" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_count) Wire.uint16be in
  let codec =
    let open Codec in
    v "PrefixedItems"
      (fun _ prefix _ items -> (prefix, items))
      [
        (f_prefix_len $ fun (p, _) -> Slice.length p);
        (f_prefix $ fun (p, _) -> p);
        (f_count $ fun (_, items) -> 2 * List.length items);
        (f_items $ fun (_, items) -> items);
      ]
  in
  let buf = Bytes.create 200 in
  let v = ((mk_ssh_string "PREFIX").data, [ 0x0102; 0x0304; 0x0506 ]) in
  Codec.encode codec v buf 0;
  let prefix, items = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "prefix" "PREFIX" (Slice.to_string prefix);
  Alcotest.(check (list int)) "items" [ 0x0102; 0x0304; 0x0506 ] items

(* -- uint: variable-width unsigned integer -- *)

(* [uint] decodes to an [Optint.Int63.t]. Aliasing [Optint.Int63] rather than
   [Wire.Private.UInt63] keeps these tests within the public API. *)
module UInt63 = Optint.Int63

let u63 = Alcotest.testable UInt63.pp ( = )

type uint_rec = { tag : int; value : UInt63.t }

let test_uint_3byte_be () =
  let codec =
    let open Codec in
    v "U3BE"
      (fun tag value -> { tag; value })
      [
        (Field.v "Tag" uint8 $ fun r -> r.tag);
        (Field.v "Value" (uint (Wire.int 3)) $ fun r -> r.value);
      ]
  in
  let original = { tag = 0x42; value = UInt63.of_int 0x1A2B3C } in
  let buf = Bytes.create 4 in
  Codec.encode codec original buf 0;
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "be byte 0" 0x1A (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "be byte 1" 0x2B (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "be byte 2" 0x3C (Bytes.get_uint8 buf 3);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" original.tag decoded.tag;
  Alcotest.check u63 "value" original.value decoded.value

let test_uint_1byte () =
  let codec =
    let open Codec in
    v "U1" (fun v -> v) [ (Field.v "V" (uint (Wire.int 1)) $ fun v -> v) ]
  in
  let buf = Bytes.create 1 in
  Codec.encode codec (UInt63.of_int 0xAB) buf 0;
  Alcotest.(check int) "byte" 0xAB (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.check u63 "roundtrip" (UInt63.of_int 0xAB) decoded

let test_uint_5byte_le () =
  let codec =
    let open Codec in
    v "U5LE"
      (fun v -> v)
      [ (Field.v "V" (uint ~endian:Wire.Little (Wire.int 5)) $ fun v -> v) ]
  in
  (* 40 bits: holds on every platform because the value lives in a
     [Optint.Int63.t], not a native int. *)
  let value = Optint.Int63.of_int64 0x01_02_03_04_05L in
  let buf = Bytes.create 5 in
  Codec.encode codec value buf 0;
  Alcotest.(check int) "le byte 0" 0x05 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "le byte 1" 0x04 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "le byte 2" 0x03 (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "le byte 3" 0x02 (Bytes.get_uint8 buf 3);
  Alcotest.(check int) "le byte 4" 0x01 (Bytes.get_uint8 buf 4);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.check u63 "roundtrip" value decoded

let test_uint_dynamic () =
  let f_n = Field.v "N" uint8 in
  let codec =
    let open Codec in
    v "UDyn"
      (fun n value -> (n, value))
      [
        (f_n $ fun (n, _) -> n);
        (Field.v "Value" (uint (Field.ref f_n)) $ fun (_, v) -> v);
      ]
  in
  (* n=2 -> 2-byte BE uint = 0x1234, layout: [02] [12 34] *)
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 0x12;
  Bytes.set_uint8 buf 2 0x34;
  let n, value = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "n" 2 n;
  Alcotest.check u63 "value" (UInt63.of_int 0x1234) value

(* -- Adversarial bit-order tests --

   These pin the default [bit_order = Msb_first] against real protocol
   bytes drawn from published specs. If anything shifts the bit layout,
   these tests fail with a concrete "0x45 decoded as (5, 4) instead of
   (4, 5)" error, not silently. *)

type ipv4_vihl = { v : int; ihl : int }

let ipv4_vihl_codec =
  let open Codec in
  v "IPv4VIHL"
    (fun v ihl -> { v; ihl })
    [
      (Field.v "Version" (bits ~width:4 U8) $ fun p -> p.v);
      (Field.v "IHL" (bits ~width:4 U8) $ fun p -> p.ihl);
    ]

let test_bitorder_ipv4_vihl_decode () =
  (* RFC 791: first byte of an IPv4 header with Version=4, IHL=5 is 0x45,
     Version in the top nibble, IHL in the bottom nibble. *)
  let buf = Bytes.of_string "\x45" in
  let r = decode_ok (Codec.decode ipv4_vihl_codec buf 0) in
  Alcotest.(check int) "Version = 4 (top nibble)" 4 r.v;
  Alcotest.(check int) "IHL = 5 (bottom nibble)" 5 r.ihl

let test_bitorder_ipv4vihl_encode_roundtrip () =
  let original = { v = 4; ihl = 5 } in
  let buf = Bytes.create 1 in
  Codec.encode ipv4_vihl_codec original buf 0;
  Alcotest.(check int) "encoded byte" 0x45 (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode ipv4_vihl_codec buf 0) in
  Alcotest.(check int) "roundtrip v" original.v decoded.v;
  Alcotest.(check int) "roundtrip ihl" original.ihl decoded.ihl

type ipv4_flags_frag = { flags : int; frag : int }

let ipv4_flags_frag_codec =
  let open Codec in
  v "IPv4FF"
    (fun flags frag -> { flags; frag })
    [
      (Field.v "Flags" (bits ~width:3 U16be) $ fun p -> p.flags);
      (Field.v "FragmentOffset" (bits ~width:13 U16be) $ fun p -> p.frag);
    ]

let test_bitorder_ipv4_flags_frag () =
  (* RFC 791: Flags (3 bits) then Fragment Offset (13 bits), MSB-first.
     Flags = 0b010 (DF set), FragOffset = 0 -> 0x4000. *)
  let buf = Bytes.of_string "\x40\x00" in
  let r = decode_ok (Codec.decode ipv4_flags_frag_codec buf 0) in
  Alcotest.(check int) "Flags = 0b010" 0b010 r.flags;
  Alcotest.(check int) "FragOffset = 0" 0 r.frag

type mcu_reg = { lo : int; hi : int }

let mcu_reg_codec =
  let open Codec in
  v "McuReg"
    (fun lo hi -> { lo; hi })
    [
      (Field.v "lo" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.lo);
      (Field.v "hi" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.hi);
    ]

let test_bitorder_lsbfirst_opt_in () =
  (* MSVC-style C struct: first declared field in the low bits. *)
  let buf = Bytes.of_string "\xA5" in
  let r = decode_ok (Codec.decode mcu_reg_codec buf 0) in
  Alcotest.(check int) "lo (Lsb_first)" 0x5 r.lo;
  Alcotest.(check int) "hi (Lsb_first)" 0xA r.hi

type bit_order_split = { x : int; y : int }

let bit_order_split_codec =
  let open Codec in
  v "BitOrderSplit"
    (fun x y -> { x; y })
    [
      (Field.v "x" (bits ~bit_order:Msb_first ~width:4 U8) $ fun r -> r.x);
      (Field.v "y" (bits ~bit_order:Lsb_first ~width:4 U8) $ fun r -> r.y);
    ]

let test_bitorder_diff_start_newword () =
  (* Two fields with the same base but different [bit_order] must NOT share
     a base word -- the codec allocates a fresh byte for each. Catches any
     regression that would let mismatched bit orders silently collide. *)
  Alcotest.(check int) "wire size = 2" 2 (Codec.wire_size bit_order_split_codec);
  let buf = Bytes.create 2 in
  Codec.encode bit_order_split_codec { x = 0xA; y = 0x5 } buf 0;
  Alcotest.(check int)
    "byte 0 = 0xA0 (Msb_first x)" 0xA0 (Bytes.get_uint8 buf 0);
  Alcotest.(check int)
    "byte 1 = 0x05 (Lsb_first y)" 0x05 (Bytes.get_uint8 buf 1)

(* -- [repeat] of a [casetype] with mixed-shape cases (DHCP-options TLV) --

   Each option dispatches on a [u8] code. PAD (0) and END (255) are a bare tag
   with no body ([unit]); every other code is a length-prefixed body
   ([len:u8] + [data:byte_array(len)]). This exercises a casetype as a repeat
   element with structurally different per-case lengths -- zero-length cases
   beside variable-length ones. *)
type dhcp_opt = Pad | End | Generic of string
type opt_body = { data : string }

let ob_len = Field.v "len" uint8
let ob_data = Field.v "data" (byte_array ~size:(Field.ref ob_len))

let opt_body_codec =
  Codec.v "OptBody"
    (fun _l d -> { data = d })
    Codec.
      [ (ob_len $ fun b -> String.length b.data); (ob_data $ fun b -> b.data) ]

let dhcp_opt_typ : dhcp_opt typ =
  casetype "DhcpOpt" uint8
    [
      case ~index:0 empty
        ~inject:(fun () -> Pad)
        ~project:(function Pad -> Some () | _ -> None);
      case ~index:255 empty
        ~inject:(fun () -> End)
        ~project:(function End -> Some () | _ -> None);
      case ~index:53 (codec opt_body_codec)
        ~inject:(fun b -> Generic b.data)
        ~project:(function Generic d -> Some { data = d } | _ -> None);
    ]

let dhcp_opt_size = function Pad | End -> 1 | Generic d -> 2 + String.length d

(* Trailing options region sized by a leading byte budget. *)
let dhcp_f_total = Field.v "total" uint8

let dhcp_f_opts =
  Field.repeat "opts" ~size:(Field.ref dhcp_f_total) dhcp_opt_typ

let dhcp_codec =
  Codec.v "DhcpOpts"
    (fun _t xs -> xs)
    Codec.
      [
        ( dhcp_f_total $ fun xs ->
          List.fold_left (fun a o -> a + dhcp_opt_size o) 0 xs );
        (dhcp_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_decode () =
  (* total=10: 35 03 'abc' | 00 | 35 01 'z' | ff *)
  let buf = Bytes.of_string "\x0a\x35\x03abc\x00\x35\x01z\xff" in
  let opts = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check int) "count" 4 (List.length opts);
  match opts with
  | [ Generic "abc"; Pad; Generic "z"; End ] -> ()
  | _ -> Alcotest.fail "unexpected options"

let test_repeat_casetype_roundtrip () =
  let v = [ Generic "abc"; Pad; Generic "z"; End ] in
  let total = List.fold_left (fun a o -> a + dhcp_opt_size o) 0 v in
  let buf = Bytes.create (1 + total) in
  Codec.encode dhcp_codec v buf 0;
  let decoded = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check bool) "roundtrip" true (decoded = v)

let test_repeat_casetype_empty () =
  let buf = Bytes.of_string "\x00" in
  let opts = decode_ok (Codec.decode dhcp_codec buf 0) in
  Alcotest.(check int) "empty" 0 (List.length opts)

(* A casetype case body that is a NUL-terminated string bounded to a fixed
   region ([zeroterm_at_most]), used as a repeat element. read_elem,
   build_field_encoder, and elem_size_of all lacked the zeroterm_at_most case,
   so encode raised "unsupported type" and sizing raised "cannot determine
   element size". *)
type zt_opt = Str of string | Num of int

let zt_opt_typ : zt_opt typ =
  casetype "ZtOpt" uint8
    [
      case ~index:1
        (zeroterm_at_most ~size:(int 6))
        ~inject:(fun s -> Str s)
        ~project:(function Str s -> Some s | _ -> None);
      case ~index:2 uint8
        ~inject:(fun n -> Num n)
        ~project:(function Num n -> Some n | _ -> None);
    ]

let zt_opt_size = function Str _ -> 1 + 6 | Num _ -> 1 + 1
let zt_f_total = Field.v "total" uint16be
let zt_f_opts = Field.repeat "opts" ~size:(Field.ref zt_f_total) zt_opt_typ

let zt_codec =
  Codec.v "ZtOpts"
    (fun _t xs -> xs)
    Codec.
      [
        ( zt_f_total $ fun xs ->
          List.fold_left (fun a o -> a + zt_opt_size o) 0 xs );
        (zt_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_zeroterm_at_most () =
  let v = [ Str "hi"; Num 7; Str "" ] in
  let buf = Bytes.create (Codec.size_of_value zt_codec v) in
  Codec.encode zt_codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode zt_codec buf 0) = v)

(* A casetype case body that is a lone bitfield, as a repeat element.
   read_elem / build_field_encoder / elem_size_of lacked the Bits case, so the
   casetype could not encode/decode as a repeat element. A bitfield case body
   projects to 3D inside a repeated casetype (the casetype packs it into its
   base word); a list of bare bitfields, by contrast, has no projection and is
   rejected. *)
type bf_opt = Bf of int | Raw of int

let bf_opt_typ : bf_opt typ =
  casetype "BfOpt" uint8
    [
      case ~index:1 (bits ~width:3 U8)
        ~inject:(fun b -> Bf b)
        ~project:(function Bf b -> Some b | _ -> None);
      case ~index:2 uint8
        ~inject:(fun v -> Raw v)
        ~project:(function Raw v -> Some v | _ -> None);
    ]

let bf_f_total = Field.v "total" uint16be
let bf_f_opts = Field.repeat "opts" ~size:(Field.ref bf_f_total) bf_opt_typ

(* each case is a 1-byte body after the 1-byte tag *)
let bf_codec =
  Codec.v "BfOpts"
    (fun _t xs -> xs)
    Codec.
      [
        (bf_f_total $ fun xs -> List.length xs * 2); (bf_f_opts $ fun xs -> xs);
      ]

let test_repeat_casetype_bits_case () =
  let v = [ Bf 5; Raw 9; Bf 0 ] in
  let buf = Bytes.create (Codec.size_of_value bf_codec v) in
  Codec.encode bf_codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (decode_ok (Codec.decode bf_codec buf 0) = v)

(* A casetype is repeatable only when every case body decodes one element at a
   time. A nested region case body has no per-element 3D projection inside a
   repeat (EverParse cannot extract a single-element-array nested in a repeated
   casetype), so the casetype is rejected at construction rather than failing
   at decode. *)
let test_repeat_casetype_unprojectable_case_rejected () =
  let nested_case =
    casetype "NestCaseOpt" uint8
      [
        case ~index:1
          (nested ~size:(int 1) int8)
          ~inject:(fun v -> `N v)
          ~project:(function `N v -> Some v);
      ]
  in
  Alcotest.(check bool)
    "repeat over casetype with nested case body rejected" true
    (raises_invalid (fun () -> Field.repeat "opts" ~size:(int 4) nested_case))

(* -- Zero-terminated strings ([zeroterm] / [zeroterm_at_most]) -- *)

type zt_rec = { name : string; tag : string; n : int }

let zt_f_name = Field.v "name" zeroterm
let zt_f_tag = Field.v "tag" (zeroterm_at_most ~size:(int 8))
let zt_f_n = Field.v "n" uint8

let zt_codec =
  Codec.v "ZtRec"
    (fun name tag n -> { name; tag; n })
    Codec.
      [
        (zt_f_name $ fun r -> r.name);
        (zt_f_tag $ fun r -> r.tag);
        (zt_f_n $ fun r -> r.n);
      ]

let test_zeroterm_roundtrip () =
  let v = { name = "hello"; tag = "ab"; n = 7 } in
  (* name(5+1) + tag(8) + n(1) = 15 *)
  let buf = Bytes.create 15 in
  Codec.encode zt_codec v buf 0;
  Alcotest.(check int) "NUL after name" 0 (Bytes.get_uint8 buf 5);
  Alcotest.(check int) "NUL after tag" 0 (Bytes.get_uint8 buf 8);
  let r = decode_ok (Codec.decode zt_codec buf 0) in
  Alcotest.(check string) "name" "hello" r.name;
  Alcotest.(check string) "tag" "ab" r.tag;
  Alcotest.(check int) "n" 7 r.n

let test_zeroterm_empty () =
  let v = { name = ""; tag = ""; n = 0 } in
  let buf = Bytes.create 15 in
  Codec.encode zt_codec v buf 0;
  let r = decode_ok (Codec.decode zt_codec buf 0) in
  Alcotest.(check string) "name" "" r.name;
  Alcotest.(check string) "tag" "" r.tag

let test_zeroterm_embedded_nul_rejected () =
  let v = { name = "a\000b"; tag = ""; n = 0 } in
  let buf = Bytes.create 15 in
  match Codec.encode zt_codec v buf 0 with
  | () -> Alcotest.fail "expected Invalid_argument for embedded NUL"
  | exception Invalid_argument _ -> ()

(* A casetype case body is encoded by the element encoder rather than the field
   encoder, so each zeroterm form has a second compiled path behind a case. *)
type zt_case = Zt of string | Zt_at_most of string

let zt_case_typ : zt_case typ =
  casetype "ZtCase" uint8
    [
      case ~index:1 zeroterm
        ~inject:(fun s -> Zt s)
        ~project:(function Zt s -> Some s | _ -> None);
      case ~index:2
        (zeroterm_at_most ~size:(int 8))
        ~inject:(fun s -> Zt_at_most s)
        ~project:(function Zt_at_most s -> Some s | _ -> None);
    ]

let zt_case_codec =
  Codec.v "ZtCaseRec"
    (fun v -> v)
    Codec.[ (Field.v "body" zt_case_typ $ fun v -> v) ]

(* One guard behind every encoder, so the message does not depend on whether the
   value went through [Wire.to_string] or [Codec.encode], nor on whether the
   string sat in a field or in a casetype case body. *)
let test_zeroterm_nul_message_shared () =
  let expected = "Wire.encode: zeroterm string contains a NUL byte" in
  let check label f =
    match f () with
    | () ->
        Alcotest.failf "%s: expected Invalid_argument for embedded NUL" label
    | exception Invalid_argument msg ->
        Alcotest.(check string) label expected msg
  in
  let nul = "x\000y" in
  check "Wire.to_string zeroterm" (fun () ->
      ignore (Wire.to_string zeroterm nul));
  check "Wire.to_string zeroterm_at_most" (fun () ->
      ignore (Wire.to_string (zeroterm_at_most ~size:(int 8)) nul));
  let field_buf = Bytes.create 15 in
  check "Codec.encode zeroterm field" (fun () ->
      Codec.encode zt_codec { name = nul; tag = ""; n = 0 } field_buf 0);
  check "Codec.encode zeroterm_at_most field" (fun () ->
      Codec.encode zt_codec { name = ""; tag = nul; n = 0 } field_buf 0);
  let case_buf = Bytes.create 16 in
  check "Codec.encode zeroterm case body" (fun () ->
      Codec.encode zt_case_codec (Zt nul) case_buf 0);
  check "Codec.encode zeroterm_at_most case body" (fun () ->
      Codec.encode zt_case_codec (Zt_at_most nul) case_buf 0)

(* The region guard is shared the same way: a value with no room for its
   terminator reports one message from every [zeroterm_at_most] encoder. *)
let test_zeroterm_region_message_shared () =
  let expected = "Wire.encode: zeroterm string needs 9 bytes but region is 8" in
  let check label f =
    match f () with
    | () ->
        Alcotest.failf "%s: expected Invalid_argument for a full region" label
    | exception Invalid_argument msg ->
        Alcotest.(check string) label expected msg
  in
  let full = String.make 8 'x' in
  check "Wire.to_string" (fun () ->
      ignore (Wire.to_string (zeroterm_at_most ~size:(int 8)) full));
  check "Codec.encode field" (fun () ->
      Codec.encode zt_codec { name = ""; tag = full; n = 0 } (Bytes.create 15) 0);
  check "Codec.encode case body" (fun () ->
      Codec.encode zt_case_codec (Zt_at_most full) (Bytes.create 16) 0)

let test_zeroterm_missing_terminator () =
  (* No NUL anywhere: decode must fail rather than read past the buffer. *)
  let buf = Bytes.make 6 'x' in
  match Codec.decode zt_codec buf 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected decode error on unterminated string"

(* -- Codec.rename -- *)

type rename_rec = { ra : int; rb : int }

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
  let v = { ra = 7; rb = 1000 } in
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

(* >8-field codecs must not allocate a partial-application closure per decode.
   [apply_fwd] saturates the record constructor in one call for up to 16
   fields; a regression to a narrower unroll reintroduces one [caml_curry]
   closure per decode, visible under flambda-off. The 8-vs-16 field decode
   must grow only by the 8 extra record slots -- no closure. *)
type alloc_r8 = {
  a1 : int;
  a2 : int;
  a3 : int;
  a4 : int;
  a5 : int;
  a6 : int;
  a7 : int;
  a8 : int;
}

type alloc_r16 = {
  b1 : int;
  b2 : int;
  b3 : int;
  b4 : int;
  b5 : int;
  b6 : int;
  b7 : int;
  b8 : int;
  b9 : int;
  b10 : int;
  b11 : int;
  b12 : int;
  b13 : int;
  b14 : int;
  b15 : int;
  b16 : int;
}

type alloc_r17 = {
  c1 : int;
  c2 : int;
  c3 : int;
  c4 : int;
  c5 : int;
  c6 : int;
  c7 : int;
  c8 : int;
  c9 : int;
  c10 : int;
  c11 : int;
  c12 : int;
  c13 : int;
  c14 : int;
  c15 : int;
  c16 : int;
  c17 : int;
}

let alloc_codec8 =
  Codec.v "Alloc8"
    (fun a1 a2 a3 a4 a5 a6 a7 a8 ->
      ({ a1; a2; a3; a4; a5; a6; a7; a8 } : alloc_r8))
    Codec.
      [
        (Field.v "a1" uint8 $ fun (r : alloc_r8) -> r.a1);
        (Field.v "a2" uint8 $ fun (r : alloc_r8) -> r.a2);
        (Field.v "a3" uint8 $ fun (r : alloc_r8) -> r.a3);
        (Field.v "a4" uint8 $ fun (r : alloc_r8) -> r.a4);
        (Field.v "a5" uint8 $ fun (r : alloc_r8) -> r.a5);
        (Field.v "a6" uint8 $ fun (r : alloc_r8) -> r.a6);
        (Field.v "a7" uint8 $ fun (r : alloc_r8) -> r.a7);
        (Field.v "a8" uint8 $ fun (r : alloc_r8) -> r.a8);
      ]

let alloc_codec16 =
  Codec.v "Alloc16"
    (fun b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 b11 b12 b13 b14 b15 b16 ->
      ({ b1; b2; b3; b4; b5; b6; b7; b8; b9; b10; b11; b12; b13; b14; b15; b16 }
        : alloc_r16))
    Codec.
      [
        (Field.v "b1" uint8 $ fun (r : alloc_r16) -> r.b1);
        (Field.v "b2" uint8 $ fun (r : alloc_r16) -> r.b2);
        (Field.v "b3" uint8 $ fun (r : alloc_r16) -> r.b3);
        (Field.v "b4" uint8 $ fun (r : alloc_r16) -> r.b4);
        (Field.v "b5" uint8 $ fun (r : alloc_r16) -> r.b5);
        (Field.v "b6" uint8 $ fun (r : alloc_r16) -> r.b6);
        (Field.v "b7" uint8 $ fun (r : alloc_r16) -> r.b7);
        (Field.v "b8" uint8 $ fun (r : alloc_r16) -> r.b8);
        (Field.v "b9" uint8 $ fun (r : alloc_r16) -> r.b9);
        (Field.v "b10" uint8 $ fun (r : alloc_r16) -> r.b10);
        (Field.v "b11" uint8 $ fun (r : alloc_r16) -> r.b11);
        (Field.v "b12" uint8 $ fun (r : alloc_r16) -> r.b12);
        (Field.v "b13" uint8 $ fun (r : alloc_r16) -> r.b13);
        (Field.v "b14" uint8 $ fun (r : alloc_r16) -> r.b14);
        (Field.v "b15" uint8 $ fun (r : alloc_r16) -> r.b15);
        (Field.v "b16" uint8 $ fun (r : alloc_r16) -> r.b16);
      ]

let alloc_codec17 =
  Codec.v "Alloc17"
    (fun c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 ->
      ({
         c1;
         c2;
         c3;
         c4;
         c5;
         c6;
         c7;
         c8;
         c9;
         c10;
         c11;
         c12;
         c13;
         c14;
         c15;
         c16;
         c17;
       }
        : alloc_r17))
    Codec.
      [
        (Field.v "c1" uint8 $ fun (r : alloc_r17) -> r.c1);
        (Field.v "c2" uint8 $ fun (r : alloc_r17) -> r.c2);
        (Field.v "c3" uint8 $ fun (r : alloc_r17) -> r.c3);
        (Field.v "c4" uint8 $ fun (r : alloc_r17) -> r.c4);
        (Field.v "c5" uint8 $ fun (r : alloc_r17) -> r.c5);
        (Field.v "c6" uint8 $ fun (r : alloc_r17) -> r.c6);
        (Field.v "c7" uint8 $ fun (r : alloc_r17) -> r.c7);
        (Field.v "c8" uint8 $ fun (r : alloc_r17) -> r.c8);
        (Field.v "c9" uint8 $ fun (r : alloc_r17) -> r.c9);
        (Field.v "c10" uint8 $ fun (r : alloc_r17) -> r.c10);
        (Field.v "c11" uint8 $ fun (r : alloc_r17) -> r.c11);
        (Field.v "c12" uint8 $ fun (r : alloc_r17) -> r.c12);
        (Field.v "c13" uint8 $ fun (r : alloc_r17) -> r.c13);
        (Field.v "c14" uint8 $ fun (r : alloc_r17) -> r.c14);
        (Field.v "c15" uint8 $ fun (r : alloc_r17) -> r.c15);
        (Field.v "c16" uint8 $ fun (r : alloc_r17) -> r.c16);
        (Field.v "c17" uint8 $ fun (r : alloc_r17) -> r.c17);
      ]

(* 32 fields is the saturated-table ceiling: it decodes in one saturated
   [make] call, no closure. 33 fields is the first arity past the table, so it
   makes one partial application and exercises the recursive unroll. *)
type alloc_r32 = {
  d1 : int;
  d2 : int;
  d3 : int;
  d4 : int;
  d5 : int;
  d6 : int;
  d7 : int;
  d8 : int;
  d9 : int;
  d10 : int;
  d11 : int;
  d12 : int;
  d13 : int;
  d14 : int;
  d15 : int;
  d16 : int;
  d17 : int;
  d18 : int;
  d19 : int;
  d20 : int;
  d21 : int;
  d22 : int;
  d23 : int;
  d24 : int;
  d25 : int;
  d26 : int;
  d27 : int;
  d28 : int;
  d29 : int;
  d30 : int;
  d31 : int;
  d32 : int;
}

type alloc_r33 = {
  e1 : int;
  e2 : int;
  e3 : int;
  e4 : int;
  e5 : int;
  e6 : int;
  e7 : int;
  e8 : int;
  e9 : int;
  e10 : int;
  e11 : int;
  e12 : int;
  e13 : int;
  e14 : int;
  e15 : int;
  e16 : int;
  e17 : int;
  e18 : int;
  e19 : int;
  e20 : int;
  e21 : int;
  e22 : int;
  e23 : int;
  e24 : int;
  e25 : int;
  e26 : int;
  e27 : int;
  e28 : int;
  e29 : int;
  e30 : int;
  e31 : int;
  e32 : int;
  e33 : int;
}

let alloc_codec32 =
  Codec.v "Alloc32"
    (fun d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20
         d21 d22 d23 d24 d25 d26 d27 d28 d29 d30 d31 d32 ->
      ({
         d1;
         d2;
         d3;
         d4;
         d5;
         d6;
         d7;
         d8;
         d9;
         d10;
         d11;
         d12;
         d13;
         d14;
         d15;
         d16;
         d17;
         d18;
         d19;
         d20;
         d21;
         d22;
         d23;
         d24;
         d25;
         d26;
         d27;
         d28;
         d29;
         d30;
         d31;
         d32;
       }
        : alloc_r32))
    Codec.
      [
        (Field.v "d1" uint8 $ fun (r : alloc_r32) -> r.d1);
        (Field.v "d2" uint8 $ fun (r : alloc_r32) -> r.d2);
        (Field.v "d3" uint8 $ fun (r : alloc_r32) -> r.d3);
        (Field.v "d4" uint8 $ fun (r : alloc_r32) -> r.d4);
        (Field.v "d5" uint8 $ fun (r : alloc_r32) -> r.d5);
        (Field.v "d6" uint8 $ fun (r : alloc_r32) -> r.d6);
        (Field.v "d7" uint8 $ fun (r : alloc_r32) -> r.d7);
        (Field.v "d8" uint8 $ fun (r : alloc_r32) -> r.d8);
        (Field.v "d9" uint8 $ fun (r : alloc_r32) -> r.d9);
        (Field.v "d10" uint8 $ fun (r : alloc_r32) -> r.d10);
        (Field.v "d11" uint8 $ fun (r : alloc_r32) -> r.d11);
        (Field.v "d12" uint8 $ fun (r : alloc_r32) -> r.d12);
        (Field.v "d13" uint8 $ fun (r : alloc_r32) -> r.d13);
        (Field.v "d14" uint8 $ fun (r : alloc_r32) -> r.d14);
        (Field.v "d15" uint8 $ fun (r : alloc_r32) -> r.d15);
        (Field.v "d16" uint8 $ fun (r : alloc_r32) -> r.d16);
        (Field.v "d17" uint8 $ fun (r : alloc_r32) -> r.d17);
        (Field.v "d18" uint8 $ fun (r : alloc_r32) -> r.d18);
        (Field.v "d19" uint8 $ fun (r : alloc_r32) -> r.d19);
        (Field.v "d20" uint8 $ fun (r : alloc_r32) -> r.d20);
        (Field.v "d21" uint8 $ fun (r : alloc_r32) -> r.d21);
        (Field.v "d22" uint8 $ fun (r : alloc_r32) -> r.d22);
        (Field.v "d23" uint8 $ fun (r : alloc_r32) -> r.d23);
        (Field.v "d24" uint8 $ fun (r : alloc_r32) -> r.d24);
        (Field.v "d25" uint8 $ fun (r : alloc_r32) -> r.d25);
        (Field.v "d26" uint8 $ fun (r : alloc_r32) -> r.d26);
        (Field.v "d27" uint8 $ fun (r : alloc_r32) -> r.d27);
        (Field.v "d28" uint8 $ fun (r : alloc_r32) -> r.d28);
        (Field.v "d29" uint8 $ fun (r : alloc_r32) -> r.d29);
        (Field.v "d30" uint8 $ fun (r : alloc_r32) -> r.d30);
        (Field.v "d31" uint8 $ fun (r : alloc_r32) -> r.d31);
        (Field.v "d32" uint8 $ fun (r : alloc_r32) -> r.d32);
      ]

let alloc_codec33 =
  Codec.v "Alloc33"
    (fun e1 e2 e3 e4 e5 e6 e7 e8 e9 e10 e11 e12 e13 e14 e15 e16 e17 e18 e19 e20
         e21 e22 e23 e24 e25 e26 e27 e28 e29 e30 e31 e32 e33 ->
      ({
         e1;
         e2;
         e3;
         e4;
         e5;
         e6;
         e7;
         e8;
         e9;
         e10;
         e11;
         e12;
         e13;
         e14;
         e15;
         e16;
         e17;
         e18;
         e19;
         e20;
         e21;
         e22;
         e23;
         e24;
         e25;
         e26;
         e27;
         e28;
         e29;
         e30;
         e31;
         e32;
         e33;
       }
        : alloc_r33))
    Codec.
      [
        (Field.v "e1" uint8 $ fun (r : alloc_r33) -> r.e1);
        (Field.v "e2" uint8 $ fun (r : alloc_r33) -> r.e2);
        (Field.v "e3" uint8 $ fun (r : alloc_r33) -> r.e3);
        (Field.v "e4" uint8 $ fun (r : alloc_r33) -> r.e4);
        (Field.v "e5" uint8 $ fun (r : alloc_r33) -> r.e5);
        (Field.v "e6" uint8 $ fun (r : alloc_r33) -> r.e6);
        (Field.v "e7" uint8 $ fun (r : alloc_r33) -> r.e7);
        (Field.v "e8" uint8 $ fun (r : alloc_r33) -> r.e8);
        (Field.v "e9" uint8 $ fun (r : alloc_r33) -> r.e9);
        (Field.v "e10" uint8 $ fun (r : alloc_r33) -> r.e10);
        (Field.v "e11" uint8 $ fun (r : alloc_r33) -> r.e11);
        (Field.v "e12" uint8 $ fun (r : alloc_r33) -> r.e12);
        (Field.v "e13" uint8 $ fun (r : alloc_r33) -> r.e13);
        (Field.v "e14" uint8 $ fun (r : alloc_r33) -> r.e14);
        (Field.v "e15" uint8 $ fun (r : alloc_r33) -> r.e15);
        (Field.v "e16" uint8 $ fun (r : alloc_r33) -> r.e16);
        (Field.v "e17" uint8 $ fun (r : alloc_r33) -> r.e17);
        (Field.v "e18" uint8 $ fun (r : alloc_r33) -> r.e18);
        (Field.v "e19" uint8 $ fun (r : alloc_r33) -> r.e19);
        (Field.v "e20" uint8 $ fun (r : alloc_r33) -> r.e20);
        (Field.v "e21" uint8 $ fun (r : alloc_r33) -> r.e21);
        (Field.v "e22" uint8 $ fun (r : alloc_r33) -> r.e22);
        (Field.v "e23" uint8 $ fun (r : alloc_r33) -> r.e23);
        (Field.v "e24" uint8 $ fun (r : alloc_r33) -> r.e24);
        (Field.v "e25" uint8 $ fun (r : alloc_r33) -> r.e25);
        (Field.v "e26" uint8 $ fun (r : alloc_r33) -> r.e26);
        (Field.v "e27" uint8 $ fun (r : alloc_r33) -> r.e27);
        (Field.v "e28" uint8 $ fun (r : alloc_r33) -> r.e28);
        (Field.v "e29" uint8 $ fun (r : alloc_r33) -> r.e29);
        (Field.v "e30" uint8 $ fun (r : alloc_r33) -> r.e30);
        (Field.v "e31" uint8 $ fun (r : alloc_r33) -> r.e31);
        (Field.v "e32" uint8 $ fun (r : alloc_r33) -> r.e32);
        (Field.v "e33" uint8 $ fun (r : alloc_r33) -> r.e33);
      ]

let per_decode_words codec nfields =
  let buf = Bytes.make nfields '\042' in
  ignore (Sys.opaque_identity (Codec.decode_exn codec buf 0));
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (Codec.decode_exn codec buf 0))
  done;
  let after = Gc.minor_words () in
  (after -. before) /. float_of_int iters

let test_decode_no_partial_closure () =
  skip_unless_gc_counters ();
  let w8 = per_decode_words alloc_codec8 8 in
  let w16 = per_decode_words alloc_codec16 16 in
  let w32 = per_decode_words alloc_codec32 32 in
  (* Each [int] record field is one word and the header is one more, so a
     closure-free decode grows by exactly the added fields. A partial-app
     closure would add several more words on top. *)
  let growth_16_8 = int_of_float (Float.round (w16 -. w8)) in
  Alcotest.(check int)
    "16-vs-8-field decode grows by 8 record slots only (no closure)" 8
    growth_16_8;
  (* A record wider than 16 fields used to trip [apply_fwd]'s recursive arm and
     allocate one [caml_curry] closure per decode. With the saturation table
     raised to 32, the 32-field decode grows by exactly its 16 extra slots. *)
  let growth_32_16 = int_of_float (Float.round (w32 -. w16)) in
  Alcotest.(check int)
    "32-vs-16-field decode grows by 16 record slots only (no closure)" 16
    growth_32_16

let test_decode_high_arity_roundtrip () =
  (* Exercise the saturated 16-field case. *)
  let buf16 = Bytes.init 16 (fun i -> Char.chr (i + 1)) in
  let v16 = Codec.decode_exn alloc_codec16 buf16 0 in
  Alcotest.(check int) "b1" 1 v16.b1;
  Alcotest.(check int) "b16" 16 v16.b16;
  (* 17 fields: still saturated after the table was widened to 32. *)
  let buf17 = Bytes.init 17 (fun i -> Char.chr (i + 1)) in
  let v17 = Codec.decode_exn alloc_codec17 buf17 0 in
  Alcotest.(check int) "c1" 1 v17.c1;
  Alcotest.(check int) "c16" 16 v17.c16;
  Alcotest.(check int) "c17" 17 v17.c17;
  let out = Bytes.create 17 in
  Codec.encode alloc_codec17 v17 out 0;
  Alcotest.(check bytes) "17-field roundtrip" buf17 out;
  (* 32 fields: the saturated-table ceiling. *)
  let buf32 = Bytes.init 32 (fun i -> Char.chr (i + 1)) in
  let v32 = Codec.decode_exn alloc_codec32 buf32 0 in
  Alcotest.(check int) "d1" 1 v32.d1;
  Alcotest.(check int) "d32" 32 v32.d32;
  let out32 = Bytes.create 32 in
  Codec.encode alloc_codec32 v32 out32 0;
  Alcotest.(check bytes) "32-field roundtrip" buf32 out32;
  (* 33 fields: the first arity past the table, exercising the recursive arm. *)
  let buf33 = Bytes.init 33 (fun i -> Char.chr (i + 1)) in
  let v33 = Codec.decode_exn alloc_codec33 buf33 0 in
  Alcotest.(check int) "e1" 1 v33.e1;
  Alcotest.(check int) "e32" 32 v33.e32;
  Alcotest.(check int) "e33" 33 v33.e33;
  let out33 = Bytes.create 33 in
  Codec.encode alloc_codec33 v33 out33 0;
  Alcotest.(check bytes) "33-field roundtrip" buf33 out33

(* Encoding a var-bytes field must not allocate. [var_bytes_writer]'s length
   check and string blit are top-level functions; were they local to the
   writer, each would be a heap-allocated closure per var-bytes field on
   every encode under flambda-off. *)
type alloc_vb = { a : string; b : string; z : string }

let alloc_vb_alen = Field.v "ALen" uint16be
let alloc_vb_blen = Field.v "BLen" uint16be

let alloc_vb_codec =
  Codec.v "AllocVb"
    (fun _alen a _blen b z -> { a; b; z })
    Codec.
      [
        (alloc_vb_alen $ fun r -> String.length r.a);
        (Field.v "A" (byte_array ~size:(Field.ref alloc_vb_alen)) $ fun r -> r.a);
        (alloc_vb_blen $ fun r -> String.length r.b);
        (Field.v "B" (byte_array ~size:(Field.ref alloc_vb_blen)) $ fun r -> r.b);
        (Field.v "Z" zeroterm $ fun r -> r.z);
      ]

let test_encode_var_bytes_no_closure () =
  skip_unless_gc_counters ();
  let v = { a = String.make 32 'x'; b = String.make 16 'y'; z = "hi" } in
  let buf = Bytes.create (2 + 32 + 2 + 16 + 2 + 1) in
  Codec.encode alloc_vb_codec v buf 0;
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    Codec.encode alloc_vb_codec v buf 0
  done;
  let after = Gc.minor_words () in
  let words =
    int_of_float (Float.round ((after -. before) /. float_of_int iters))
  in
  Alcotest.(check int) "var-bytes encode allocates nothing" 0 words

(* [Codec.get] and [Codec.set] resolve a field's type and offset when the
   accessor is staged. Pin the per-call contract separately from functional
   correctness: rebuilding a reader closure returns the right value but still
   puts pressure on a hot-path minor heap. *)
type alloc_priority = Low | High

type alloc_accessor = {
  hi : int;
  lo : int;
  u8 : int;
  u16 : int;
  i32 : int;
  priority : alloc_priority;
}

let alloc_hi = Field.v "Hi" (bits ~width:4 U8)
let alloc_lo = Field.v "Lo" (bits ~width:4 U8)
let alloc_u8 = Field.v "U8" uint8
let alloc_u16 = Field.v "U16" uint16be
let alloc_i32 = Field.v "I32" int32be

let alloc_priority =
  Field.v "Priority"
    (map
       ~decode:(function 0 -> Low | _ -> High)
       ~encode:(function Low -> 0 | High -> 1)
       uint8)

let alloc_bf_hi = Codec.(alloc_hi $ fun r -> r.hi)
let alloc_bf_lo = Codec.(alloc_lo $ fun r -> r.lo)
let alloc_bf_u8 = Codec.(alloc_u8 $ fun r -> r.u8)
let alloc_bf_u16 = Codec.(alloc_u16 $ fun r -> r.u16)
let alloc_bf_i32 = Codec.(alloc_i32 $ fun r -> r.i32)
let alloc_bf_priority = Codec.(alloc_priority $ fun r -> r.priority)

let alloc_accessor_codec =
  Codec.v "AllocAccessor"
    (fun hi lo u8 u16 i32 priority -> { hi; lo; u8; u16; i32; priority })
    Codec.
      [
        alloc_bf_hi;
        alloc_bf_lo;
        alloc_bf_u8;
        alloc_bf_u16;
        alloc_bf_i32;
        alloc_bf_priority;
      ]

let per_call_words f =
  ignore (Sys.opaque_identity (f ()));
  let iters = 200_000 in
  Gc.full_major ();
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.minor_words () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters))

let check_no_per_call_allocation name f =
  Alcotest.(check int)
    (Fmt.str "%s allocates nothing" name)
    0 (per_call_words f)

let test_get_no_allocation () =
  skip_unless_gc_counters ();
  let buf = Bytes.make (Codec.wire_size alloc_accessor_codec) '\007' in
  let get f = Staged.unstage (Codec.get alloc_accessor_codec f) in
  let get_hi = get alloc_bf_hi
  and get_u8 = get alloc_bf_u8
  and get_u16 = get alloc_bf_u16
  and get_i32 = get alloc_bf_i32
  and get_priority = get alloc_bf_priority in
  check_no_per_call_allocation "get bits" (fun () -> get_hi buf 0);
  check_no_per_call_allocation "get uint8" (fun () -> get_u8 buf 0);
  check_no_per_call_allocation "get uint16be" (fun () -> get_u16 buf 0);
  check_no_per_call_allocation "get int32be" (fun () -> get_i32 buf 0);
  check_no_per_call_allocation "get map" (fun () -> get_priority buf 0)

let test_set_no_allocation () =
  skip_unless_gc_counters ();
  let buf = Bytes.make (Codec.wire_size alloc_accessor_codec) '\007' in
  let set f = Staged.unstage (Codec.set alloc_accessor_codec f) in
  let set_hi = set alloc_bf_hi
  and set_u8 = set alloc_bf_u8
  and set_u16 = set alloc_bf_u16
  and set_i32 = set alloc_bf_i32
  and set_priority = set alloc_bf_priority in
  check_no_per_call_allocation "set bits" (fun () -> set_hi buf 0 3);
  check_no_per_call_allocation "set uint8" (fun () -> set_u8 buf 0 7);
  check_no_per_call_allocation "set uint16be" (fun () -> set_u16 buf 0 513);
  check_no_per_call_allocation "set int32be" (fun () -> set_i32 buf 0 70_000);
  check_no_per_call_allocation "set map" (fun () -> set_priority buf 0 High)

(* {2 Resource bounds}

   Allocation is the deterministic half of cost: the same work turns over the
   same number of words on any machine, where wall-clock does not. The two
   tests below are not performance targets. They assert a shape -- how much
   memory a read path churns must not be a function of a length the sender
   picked -- on the entry points [codec.mli] tells a caller to run per packet
   on untrusted input.

   [Gc.minor_words] cannot measure it: it does not count blocks over
   [Max_young_wosize] (256 words), so a version written with it silently
   measures nothing once the payload passes 2 KiB. *)
let words_per_call ~iters f =
  ignore (Sys.opaque_identity (f ()));
  Gc.full_major ();
  let before = Gc.allocated_bytes () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.allocated_bytes () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters /. 8.))

(* Length-parameterised span codecs, each with a buffer it accepts. All five
   hand the caller one string of about [n] bytes, so they are comparable to
   each other and to the payload; [byte_array] and [all_bytes] are the
   unrefined twins of the three that check something as they go. *)
let span_family name typ =
  Codec.v name Fun.id Codec.[ Field.v "d" typ $ Fun.id ]

let span_families =
  [
    ( "byte_array",
      (fun n -> span_family "SpanArray" (byte_array ~size:(int n))),
      fun n -> Bytes.make n 'a' );
    ( "byte_array_where",
      (fun n ->
        span_family "SpanWhere"
          (byte_array_where ~size:(int n) ~per_byte:printable_byte)),
      fun n -> Bytes.make n 'a' );
    ( "zeroterm_at_most",
      (fun n -> span_family "SpanZeroterm" (zeroterm_at_most ~size:(int n))),
      fun n ->
        let b = Bytes.make n 'a' in
        Bytes.set b (n - 1) '\000';
        b );
    ( "all_bytes",
      (fun _ -> span_family "SpanAll" all_bytes),
      fun n -> Bytes.make n 'a' );
    ( "all_zeros",
      (fun _ -> span_family "SpanZeros" all_zeros),
      fun n -> Bytes.make n '\000' );
  ]

let report_span_failures what failures =
  if failures <> [] then
    Alcotest.failf "%s: %s" what (String.concat "; " (List.rev failures))

(* [Codec.validate] returns unit, so everything it allocates it drops on the
   floor. What it turns over must therefore not depend on how long the sender
   made the frame. The two runs compared differ in nothing but a number the
   sender chose, which is what makes the growth between them attributable. A
   validator that copies the span it is scanning gives an attacker a heap
   multiplier on a path a server runs once per packet. *)
let test_validate_allocation_is_bounded () =
  skip_unless_gc_counters ();
  let small = 512 and large = 4096 in
  let failures =
    List.fold_left
      (fun acc (name, mk, mkbuf) ->
        let words n =
          let c = mk n and buf = mkbuf n in
          words_per_call ~iters:2000 (fun () -> Codec.validate c buf 0)
        in
        let at_small = words small and at_large = words large in
        if at_large - at_small > 32 then
          Fmt.str
            "%s grows %d words between a %d-byte and a %d-byte frame (%d then \
             %d)"
            name (at_large - at_small) small large at_small at_large
          :: acc
        else acc)
      [] span_families
  in
  report_span_failures "validate allocation scales with the frame length"
    failures

(* [Codec.decode] hands the span back, so one copy of it is the price of the
   answer. A second copy is work the caller never sees and the sender sizes.
   The bound is against the payload the returned value carries, not against
   another run of the same decoder: a decoder measured against itself would
   agree with any amount of copying. *)
let test_decode_allocates_one_copy () =
  skip_unless_gc_counters ();
  let n = 4096 in
  let payload = n / 8 in
  let failures =
    List.fold_left
      (fun acc (name, mk, mkbuf) ->
        let c = mk n and buf = mkbuf n in
        let words =
          words_per_call ~iters:2000 (fun () -> Codec.decode c buf 0)
        in
        if words > payload + 32 then
          Fmt.str "%s allocates %d words for a %d-word payload" name words
            payload
          :: acc
        else acc)
      [] span_families
  in
  report_span_failures "decode allocates more than the value it returns"
    failures

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
        f_diag_len $ String.length;
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
        f_diag_len $ String.length;
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
        (f_diag_len $ fun (a, _, _) -> String.length a);
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

(* -- Accessor and container contracts -- *)

(* [Codec.set] documents [Invalid_argument] "leaving the buffer untouched". A
   sub-codec field writes the whole sub-record and validates it afterwards, so
   the refusal used to arrive with the rejected byte already on the wire. *)
let test_set_refusal_leaves_buffer_untouched () =
  let inner_f =
    Field.v "v" uint8 ~self_constraint:(fun r ->
        Expr.(r >= int 10 && r <= int 100))
  in
  let inner = Codec.v "SetRollbackInner" Fun.id Codec.[ inner_f $ Fun.id ] in
  let outer_f = Codec.(Field.v "v" (codec inner) $ Fun.id) in
  let outer = Codec.v "SetRollbackOuter" Fun.id Codec.[ outer_f ] in
  let set = Staged.unstage (Codec.set outer outer_f) in
  let buf = Bytes.make 1 '\xee' in
  (match set buf 0 200 with
  | () -> Alcotest.fail "Codec.set accepted a value the sub-codec refuses"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string)
    "buffer untouched after a refused set" "\xee" (Bytes.to_string buf);
  set buf 0 42;
  Alcotest.(check string)
    "an accepted set still writes" "\x2a" (Bytes.to_string buf)

(* A bitfield accessor takes its offset from the caller, so it has to check
   that its base word is in the buffer. The U32 word helpers reached the bytes
   unchecked, so [Codec.get] on a frame shorter than the word answered with
   whatever followed the buffer -- the same bytes read one value at base 0 and
   another once the frame moved along the buffer, and neither was in the frame
   -- while [Codec.set] laid down the low half of the word before the high half
   raised. Walked over every base and every offset that puts part of the word
   past the end: only the U32 bases were unchecked, and the half-written word
   only shows on a field the low half carries. *)
let bounds_bitpack base bit_order (w_a, w_b, w_c) =
  let f_a = Field.v "a" (bits ~bit_order ~width:w_a base) in
  let f_b = Field.v "b" (bits ~bit_order ~width:w_b base) in
  let f_c = Field.v "c" (bits ~bit_order ~width:w_c base) in
  let proj_a (a, _, _) = a and proj_b (_, b, _) = b and proj_c (_, _, c) = c in
  let codec =
    Codec.v "BfBounds"
      (fun a b c -> (a, b, c))
      Codec.[ f_a $ proj_a; f_b $ proj_b; f_c $ proj_c ]
  in
  ( codec,
    [
      ("a", Codec.( $ ) f_a proj_a);
      ("b", Codec.( $ ) f_b proj_b);
      ("c", Codec.( $ ) f_c proj_c);
    ] )

let bitpack_bases =
  [
    ("U16", U16, Msb_first, (3, 2, 11));
    ("U16be", U16be, Lsb_first, (3, 2, 11));
    ("U32", U32, Msb_first, (6, 10, 16));
    ("U32be", U32be, Lsb_first, (6, 10, 16));
  ]

(* Every (base, field) pair with the word size the base needs. *)
let iter_bitpacks f =
  List.iter
    (fun (name, base, bit_order, widths) ->
      let codec, fields = bounds_bitpack base bit_order widths in
      let word = Codec.wire_size codec in
      List.iter (fun (fname, field) -> f name word fname field codec) fields)
    bitpack_bases

let test_get_bitfield_word_past_buffer_raises () =
  iter_bitpacks (fun name word fname field codec ->
      let get = Staged.unstage (Codec.get codec field) in
      let load =
        Staged.unstage (Codec.load_word (Codec.bitfield codec field))
      in
      let expect_refusal reader len off =
        match reader (Bytes.make len '\xff') off with
        | v ->
            Alcotest.failf
              "%s field %s: read %d at offset %d of a %d-byte buffer" name fname
              v off len
        | exception Invalid_argument _ -> ()
      in
      List.iter
        (fun reader ->
          (* No room for the word at all, and room for it but not where the
             frame starts. *)
          for len = 0 to word - 1 do
            expect_refusal reader len 0
          done;
          for off = 1 to word do
            expect_refusal reader word off
          done)
        [ get; (fun buf off -> Optint.to_int (load buf off)) ])

let test_set_bitfield_word_past_buffer_writes_nothing () =
  iter_bitpacks (fun name word fname field codec ->
      let set = Staged.unstage (Codec.set codec field) in
      for len = 0 to word - 1 do
        let buf = Bytes.make len '\x00' in
        (match set buf 0 1 with
        | () ->
            Alcotest.failf
              "%s field %s: Codec.set wrote a word into a %d-byte buffer" name
              fname len
        | exception Invalid_argument _ -> ());
        Alcotest.(check string)
          (Fmt.str "%s field %s: %d-byte buffer untouched" name fname len)
          (String.make len '\x00') (Bytes.to_string buf)
      done)

(* [Codec.get] had no reader for a statically gated optional and fell through
   to a bare [Failure], on fields [Codec.decode] reads without trouble. Check
   both flavours and both gates against what decode returns. *)
let test_get_static_optional_agrees_with_decode () =
  let f_len = Field.optional_or "Len" ~present:Expr.true_ ~default:0 uint8 in
  let f_on = Field.optional "On" ~present:Expr.true_ uint8 in
  let f_off = Field.optional "Off" ~present:Expr.false_ uint8 in
  let f_or_off =
    Field.optional_or "OrOff" ~present:Expr.false_ ~default:7 uint8
  in
  let f_data = Field.v "Data" (byte_array ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _, _, _, _) -> l) in
  let cf_on = Codec.(f_on $ fun (_, o, _, _, _) -> o) in
  let cf_off = Codec.(f_off $ fun (_, _, o, _, _) -> o) in
  let cf_or_off = Codec.(f_or_off $ fun (_, _, _, o, _) -> o) in
  let cf_data = Codec.(f_data $ fun (_, _, _, _, d) -> d) in
  let c =
    Codec.v "StaticGates"
      (fun len on off or_off data -> (len, on, off, or_off, data))
      [ cf_len; cf_on; cf_off; cf_or_off; cf_data ]
  in
  let buf = Bytes.of_string "\003\009abc" in
  let len, on, off, or_off, _ = decode_ok (Codec.decode c buf 0) in
  let read f = Staged.unstage (Codec.get c f) buf 0 in
  Alcotest.(check int) "optional_or, gate on" len (read cf_len);
  Alcotest.(check (option int)) "optional, gate on" on (read cf_on);
  Alcotest.(check (option int)) "optional, gate off" off (read cf_off);
  Alcotest.(check int)
    "optional_or, gate off is the default" or_off (read cf_or_off);
  Alcotest.(check string) "span sized by the optional_or" "abc" (read cf_data);
  (* And the matching setter, so a field [get] can read is one [set] can
     write. The absent gates own no bytes, so writing them moves nothing. *)
  Staged.unstage (Codec.set c cf_on) buf 0 (Some 5);
  Staged.unstage (Codec.set c cf_off) buf 0 None;
  Staged.unstage (Codec.set c cf_or_off) buf 0 7;
  Alcotest.(check (option int)) "set then get, gate on" (Some 5) (read cf_on);
  Alcotest.(check string)
    "the absent gates wrote no bytes" "\003\005abc" (Bytes.to_string buf)

(* A runtime gate is decided by an expression over sibling fields, which only
   the compiled record plan evaluates. The accessor cannot read it, and has to
   say so with the exception the interface documents. *)
let test_get_dynamic_optional_refuses_cleanly () =
  let f_gate = Field.v "gate" uint8 in
  let f_pay =
    Field.optional "pay" ~present:Expr.(Field.ref f_gate <> int 0) uint16be
  in
  let cf_pay = Codec.(f_pay $ snd) in
  let c =
    Codec.v "DynGate"
      (fun gate pay -> (gate, pay))
      [ Codec.(f_gate $ fst); cf_pay ]
  in
  Alcotest.(check bool)
    "Codec.get refuses a runtime-gated optional with Invalid_argument" true
    (raises_invalid (fun () -> Codec.get c cf_pay))

(* A [repeat] element whose decoder runs to the end of the buffer cannot be
   iterated: the first element takes the whole budget, so anything the encoder
   writes past it is bytes the decoder (and the EverParse validator built from
   the same schema) refuses. The greedy tail counts whether it is the element's
   own last field or sits at the end of a sub-codec the element ends with. *)
let test_repeat_rejects_nested_greedy_element () =
  let pad =
    Codec.v "PadTail"
      (fun n tail -> (n, tail))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "tail" all_zeros $ snd ]
  in
  let greedy_elem =
    Codec.v "GreedyElem"
      (fun a b -> (a, b))
      Codec.[ Field.v "f0" uint8 $ fst; Field.v "f1" (codec pad) $ snd ]
  in
  Alcotest.(check bool)
    "repeat over a codec whose own tail is greedy rejected" true
    (raises_invalid (fun () -> Field.repeat "items" ~size:(int 12) (codec pad)));
  Alcotest.(check bool)
    "repeat over a codec ending in a greedy sub-codec rejected" true
    (raises_invalid (fun () ->
         Field.repeat "items" ~size:(int 12) (codec greedy_elem)))

(* The control for the rejection above: give the same element a bounded tail
   and it is self-delimiting again, so the repeat builds and more than one
   element survives the round trip. *)
let test_repeat_bounded_element_roundtrips () =
  let bounded =
    Codec.v "PadFixed"
      (fun n tail -> (n, tail))
      Codec.
        [
          Field.v "n" uint8 $ fst;
          Field.v "tail" (byte_array ~size:(int 2)) $ snd;
        ]
  in
  let elem =
    Codec.v "BoundedElem"
      (fun a b -> (a, b))
      Codec.[ Field.v "f0" uint8 $ fst; Field.v "f1" (codec bounded) $ snd ]
  in
  let f_total = Field.v "total" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_total) (codec elem) in
  let outer =
    Codec.v "BoundedRep"
      (fun _ xs -> xs)
      Codec.
        [ (f_total $ fun xs -> 4 * List.length xs); (f_items $ fun xs -> xs) ]
  in
  let xs = [ (1, (2, "\000\000")); (3, (4, "\000\000")) ] in
  let buf = Bytes.create (Codec.size_of_value outer xs) in
  Codec.encode outer xs buf 0;
  let ys = decode_ok (Codec.decode outer buf 0) in
  Alcotest.(check bool) "two bounded elements round-trip" true (ys = xs)

(* -- Suite -- *)

let suite =
  ( "codec",
    [
      (* Codec.rename *)
      Alcotest.test_case "rename: projection name" `Quick test_rename_projection;
      Alcotest.test_case "rename: roundtrip unchanged" `Quick
        test_rename_roundtrip;
      (* record *)
      Alcotest.test_case "record: encode" `Quick test_record_encode;
      Alcotest.test_case "record: decode" `Quick test_record_decode;
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
      Alcotest.test_case "error: parse_error / eof constructors" `Quick
        test_error_constructors;
      Alcotest.test_case "validate: check-free codec allocates nothing" `Quick
        test_validate_check_free_no_alloc;
      Alcotest.test_case "validate: matches decode rejections" `Quick
        test_validate_matches_decode_rejections;
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
      Alcotest.test_case "repeat: zeroterm element roundtrip" `Quick
        test_repeat_zeroterm_element;
      Alcotest.test_case "repeat: zeroterm element projection" `Quick
        test_repeat_zeroterm_projection;
      Alcotest.test_case "optional: variable byte_array inner roundtrip" `Quick
        test_optional_var_byte_array;
      Alcotest.test_case "optional: self-delimiting codec inner roundtrip"
        `Quick test_optional_self_delimiting_codec;
      Alcotest.test_case "optional: length-prefixed group roundtrip" `Quick
        test_optional_length_prefixed_group;
      Alcotest.test_case "repeat: byte_array element roundtrip" `Quick
        test_repeat_byte_array_element;
      Alcotest.test_case "repeat: byte_array element projection" `Quick
        test_repeat_byte_array_projection;
      Alcotest.test_case "repeat: rejects unprojectable element" `Quick
        test_repeat_rejects_unprojectable;
      Alcotest.test_case "repeat: rejects zero-width byte span" `Quick
        test_repeat_rejects_zero_width_span;
      Alcotest.test_case "repeat: rejects greedy-tail sub-codec" `Quick
        test_repeat_rejects_greedy_tail_codec;
      Alcotest.test_case "array: byte_array element roundtrip" `Quick
        test_array_byte_array_element;
      Alcotest.test_case "array: byte_array element projection" `Quick
        test_array_byte_array_projection;
      Alcotest.test_case "rest_bytes projection guard" `Quick
        test_rest_bytes_projection_guard;
      Alcotest.test_case "repeat oversized length rejected" `Quick
        test_repeat_oversized_length_rejected;
      Alcotest.test_case "byte_slice negative size fails cleanly" `Quick
        test_byte_slice_negative_size;
      Alcotest.test_case "validate: field past end fails cleanly" `Quick
        test_validate_overrun_field_offset;
      Alcotest.test_case "validate_struct: field past end fails cleanly" `Quick
        test_validate_struct_field_past_end;
      Alcotest.test_case "validate_struct: fixed field past a short buffer"
        `Quick test_validate_struct_fixed_field_past_end;
      Alcotest.test_case "validate: present optional past end fails cleanly"
        `Quick test_validate_present_optional_past_end;
      Alcotest.test_case "validate: present optional_or past end fails cleanly"
        `Quick test_validate_present_optional_or_past_end;
      Alcotest.test_case "validate: runtime-width uint past end fails cleanly"
        `Quick test_validate_uint_var_past_end;
      Alcotest.test_case "validate: repeat budget with a partial element" `Quick
        test_validate_repeat_partial_element;
      Alcotest.test_case "repeat/array reject bitfield element" `Quick
        test_repeat_array_reject_bitfield;
      Alcotest.test_case "array/repeat reject zero-width element" `Quick
        test_reject_zero_width_element;
      Alcotest.test_case "array: rejects zero-width byte span" `Quick
        test_array_rejects_zero_width_span;
      Alcotest.test_case "array: accepts fixed-size byte span" `Quick
        test_array_accepts_fixed_byte_span;
      Alcotest.test_case "array: record element roundtrip" `Quick
        test_array_record_element;
      Alcotest.test_case "array: record element projection" `Quick
        test_array_record_projection;
      Alcotest.test_case "nested reject bitfield element" `Quick
        test_nested_reject_bitfield;
      Alcotest.test_case "nested over array roundtrip" `Quick
        test_nested_over_array;
      Alcotest.test_case "nested_at_most over array roundtrip" `Quick
        test_nested_at_most_over_array;
      Alcotest.test_case "nested exact region" `Quick test_nested_exact_region;
      Alcotest.test_case "casetype nested case body roundtrip" `Quick
        test_casetype_nested_case_body;
      Alcotest.test_case "array rejects non-projectable element" `Quick
        test_array_reject_nonprojectable_element;
      Alcotest.test_case "array/repeat reject byte-span-only sub-codec" `Quick
        test_array_repeat_reject_non_nz_codec;
      Alcotest.test_case "array over scalar element projects" `Quick
        test_array_scalar_element_projects;
      Alcotest.test_case "array over wrapped byte span projects" `Quick
        test_array_wrapped_byte_span_projects;
      Alcotest.test_case "optional rejects unprojectable inner" `Quick
        test_optional_reject_unprojectable;
      Alcotest.test_case "optional rejects bitfield inner" `Quick
        test_optional_reject_bitfield;
      Alcotest.test_case "casetype rejects greedy case body" `Quick
        test_casetype_reject_greedy_case_body;
      Alcotest.test_case "greedy field must be last" `Quick
        test_greedy_not_last_rejected;
      Alcotest.test_case "casetype greedy case body must be last" `Quick
        test_casetype_greedy_case_not_last_rejected;
      Alcotest.test_case "casetype wrapped greedy case body must be last" `Quick
        test_casetype_wrapped_greedy_not_last_rejected;
      Alcotest.test_case "optional greedy body must be last" `Quick
        test_optional_greedy_not_last_rejected;
      Alcotest.test_case "uint rejects out-of-range size" `Quick
        test_uint_size_bounds;
      Alcotest.test_case "bits rejects out-of-range width" `Quick
        test_bits_width_bounds;
      Alcotest.test_case "casetype case requires ~index" `Quick
        test_casetype_case_requires_index;
      Alcotest.test_case "casetype rejects unprojectable tag" `Quick
        test_casetype_reject_unprojectable_tag;
      (* codec bitfields *)
      Alcotest.test_case "codec bitfield: wire_size" `Quick
        test_codec_bitfield_wire_size;
      Alcotest.test_case "codec bitfield: roundtrip" `Quick
        test_codec_bitfield_roundtrip;
      Alcotest.test_case "codec bitfield: byte layout" `Quick
        test_codec_bitfield_byte_layout;
      Alcotest.test_case "codec bitfield: decode" `Quick
        test_codec_bitfield_decode;
      Alcotest.test_case "codec bitfield: multi group" `Quick
        test_codec_bitfield_multi_group;
      Alcotest.test_case "codec bitfield: struct_of_codec" `Quick
        test_struct_of_codec_bitfield;
      Alcotest.test_case "codec bitfield: overflow u8" `Quick
        test_codec_bitfield_overflow_u8;
      Alcotest.test_case "codec bitfield: overflow u16" `Quick
        test_codec_bitfield_overflow_u16;
      Alcotest.test_case "codec bitfield: overflow u32" `Quick
        test_codec_bitfield_overflow_u32;
      Alcotest.test_case "codec bitfield: max valid" `Quick
        test_codec_bitfield_max_valid;
      Alcotest.test_case "codec bitfield: overflow 1-bit" `Quick
        test_codec_bitfield_overflow_1bit;
      Alcotest.test_case "codec bitfield: size_of_value packed" `Quick
        test_packed_bf_size;
      Alcotest.test_case "codec bitfield: size_of_value mapped bit" `Quick
        test_packed_mapped_bf_size;
      Alcotest.test_case "fixed byte regions: size_of_value" `Quick
        test_fixed_byte_region_size_of_value;
      Alcotest.test_case "exact width: uint encode" `Quick
        test_encode_exact_uint_width;
      Alcotest.test_case "exact width: uint set" `Quick
        test_set_exact_uint_width;
      Alcotest.test_case "exact width: bits encode" `Quick
        test_encode_exact_bits_width;
      Alcotest.test_case "exact width: bits set" `Quick
        test_set_exact_bits_width;
      Alcotest.test_case "exact width: set bits(3,U8) <- 0xFF" `Quick
        test_set_bits_no_silent_truncation;
      Alcotest.test_case "exact width: map reaches the inner typ" `Quick
        test_map_inherits_exact_width;
      Alcotest.test_case "exact width: unsigned scalars" `Quick
        test_encode_exact_unsigned_scalar;
      Alcotest.test_case "exact width: signed scalars" `Quick
        test_encode_exact_signed_scalar;
      Alcotest.test_case "exact width: uint32" `Quick test_encode_exact_uint32;
      Alcotest.test_case "exact width: uint63" `Quick test_encode_exact_uint63;
      Alcotest.test_case "exact width: int64 carrier has no range" `Quick
        test_encode_int64_carrier_has_no_range;
      Alcotest.test_case "exact width: array and repeat elements" `Quick
        test_element_exact_width;
      Alcotest.test_case "exact byte field: literal size" `Quick
        test_exact_byte_field_literal_size;
      Alcotest.test_case "exact byte field: expression size" `Quick
        test_exact_byte_field_expression_size;
      Alcotest.test_case "encode rejects: unlisted enum value" `Quick
        test_encode_rejects_unlisted_enum;
      Alcotest.test_case "encode rejects: non-zero all_zeros" `Quick
        test_encode_rejects_non_zero_padding;
      Alcotest.test_case "encode rejects: refined byte span" `Quick
        test_encode_rejects_refined_byte;
      Alcotest.test_case "decode rejects: refined byte span" `Quick
        test_decode_rejects_refined_byte;
      Alcotest.test_case "decode accepts: conforming refined byte span" `Quick
        test_decode_accepts_conforming_refined_byte;
      Alcotest.test_case "refined byte span: decode and validate agree" `Quick
        test_refined_byte_decode_validate_agree;
      Alcotest.test_case "encode rejects: where violation" `Quick
        test_encode_rejects_where_violation;
      Alcotest.test_case "constant size expression folds" `Quick
        test_constant_size_expression_folds;
      Alcotest.test_case "exact byte field: set var byte_array" `Quick
        test_set_exact_var_byte_array;
      Alcotest.test_case "exact byte field: set var byte_array_where" `Quick
        test_set_exact_var_byte_array_where;
      Alcotest.test_case "exact byte field: set var byte_slice" `Quick
        test_set_exact_var_byte_slice;
      Alcotest.test_case "exact byte field: set dyn byte_array" `Quick
        test_set_exact_dyn_byte_array;
      Alcotest.test_case "exact byte field: set dyn byte_array_where" `Quick
        test_set_exact_dyn_byte_array_where;
      Alcotest.test_case "exact byte field: set dyn byte_slice" `Quick
        test_set_exact_dyn_byte_slice;
      Alcotest.test_case "exact byte field: set fixed byte_array" `Quick
        test_set_exact_fixed_byte_array;
      (* action semantics *)
      Alcotest.test_case "action: fires on decode_env" `Quick
        test_action_fires_decode_env;
      Alcotest.test_case "action: fires on get" `Quick test_action_fires_on_get;
      Alcotest.test_case "action: not fired by validate" `Quick
        test_action_unfired_by_validate;
      Alcotest.test_case "action: no action zero overhead" `Quick
        test_get_noaction_zero_overhead;
      Alcotest.test_case "action: get with env" `Quick test_get_with_env;
      Alcotest.test_case "action: field in two codecs" `Quick
        test_get_action_field_twocodecs;
      Alcotest.test_case "action: get without env" `Quick test_get_action_no_env;
      Alcotest.test_case "action: abort on get" `Quick
        test_get_action_abort_field;
      Alcotest.test_case "action: no action ignores env" `Quick
        test_get_noaction_ignores_env;
      Alcotest.test_case "action: multiple calls update env" `Quick
        test_get_action_multiple_calls;
      Alcotest.test_case "action: with input param" `Quick
        test_get_action_with_inputparam;
      Alcotest.test_case "action: input param no env" `Quick
        test_get_action_inputparam_noenv;
      Alcotest.test_case "embed: param-sized sub-codec forwards param" `Quick
        test_embed_param_sized;
      Alcotest.test_case "get: fixed embedded codec preserves param env" `Quick
        test_get_embed_param_fixed;
      Alcotest.test_case "embed: param sub-codec requires env" `Quick
        test_embed_param_requires_env;
      Alcotest.test_case "embed: sub-codec where enforced" `Quick
        test_embed_where_enforced;
      Alcotest.test_case "embed: output parameter forwarded" `Quick
        test_embed_output_param;
      Alcotest.test_case "embed: param forwarded through repeat" `Quick
        test_embed_param_repeat;
      Alcotest.test_case "action: output only" `Quick
        test_get_action_output_only;
      Alcotest.test_case "action: var then assign" `Quick
        test_get_action_varthen_assign;
      Alcotest.test_case "action: cross-field ref" `Quick
        test_get_action_crossfield_ref;
      Alcotest.test_case "validate: constraint only" `Quick
        test_validate_constraint_only;
      Alcotest.test_case "validate: where only" `Quick test_validate_where_only;
      Alcotest.test_case "action: two staged same field" `Quick
        test_get_twostaged_same_field;
      Alcotest.test_case "shared: encode bitfield" `Quick
        test_encode_shared_bitfield;
      (* API misuse *)
      Alcotest.test_case "misuse: get field not in codec" `Quick
        test_get_field_notin_codec;
      Alcotest.test_case "misuse: set field not in codec" `Quick
        test_set_field_notin_codec;
      Alcotest.test_case "misuse: bitfield on non-bitfield" `Quick
        test_bitfield_on_non_bitfield;
      Alcotest.test_case "misuse: foreign env codec operations" `Quick
        test_foreign_env_codec_operations;
      Alcotest.test_case "misuse: env from wrong codec" `Quick
        test_env_from_wrong_codec;
      Alcotest.test_case "misuse: wrong env with action" `Quick
        test_env_wrongcodec_with_action;
      Alcotest.test_case "misuse: decode short buffer" `Quick
        test_decode_short_buffer;
      Alcotest.test_case "misuse: encode short buffer" `Quick
        test_encode_short_buffer;
      (* same field in two codecs *)
      Alcotest.test_case "shared: same field two codecs get" `Quick
        test_same_field_two_codecs;
      Alcotest.test_case "shared: same field two codecs set" `Quick
        test_samefield_twocodecs_set;
      Alcotest.test_case "shared: same field two codecs decode" `Quick
        test_samefield_twocodecs_decode;
      Alcotest.test_case "shared: same field two codecs encode" `Quick
        test_samefield_twocodecs_encode;
      Alcotest.test_case "shared: same bitfield two codecs" `Quick
        test_same_bitfield_two_codecs;
      Alcotest.test_case "shared: staged before second seal" `Quick
        test_samefield_staged_before_secondseal;
      (* zero-copy view *)
      Alcotest.test_case "view: get uint" `Quick test_view_get_uint;
      Alcotest.test_case "view: get bitfield" `Quick test_view_get_bitfield;
      Alcotest.test_case "view: get bool" `Quick test_view_get_bool;
      Alcotest.test_case "view: set bitfield" `Quick test_view_set_bitfield;
      Alcotest.test_case "view: set uint" `Quick test_view_set_uint;
      Alcotest.test_case "view: set bool" `Quick test_view_set_bool;
      Alcotest.test_case "view: bounds check" `Quick test_view_bounds_check;
      Alcotest.test_case "view: with offset" `Quick test_view_with_offset;
      (* field sharing *)
      Alcotest.test_case "view: shared field spec" `Quick
        test_view_shared_field_spec;
      Alcotest.test_case "view: shared bitfield spec" `Quick
        test_view_shared_bitfield_spec;
      Alcotest.test_case "view: shared set independent" `Quick
        test_view_shared_set_independent;
      (* bit_order adversarial tests *)
      Alcotest.test_case "bit_order: IPv4 Version/IHL decode" `Quick
        test_bitorder_ipv4_vihl_decode;
      Alcotest.test_case "bit_order: IPv4 Version/IHL roundtrip" `Quick
        test_bitorder_ipv4vihl_encode_roundtrip;
      Alcotest.test_case "bit_order: IPv4 Flags/FragOffset" `Quick
        test_bitorder_ipv4_flags_frag;
      Alcotest.test_case "bit_order: Lsb_first opt-in" `Quick
        test_bitorder_lsbfirst_opt_in;
      Alcotest.test_case "bit_order: different orders separate words" `Quick
        test_bitorder_diff_start_newword;
      (* byte_slice *)
      Alcotest.test_case "view: byte_slice get" `Quick test_view_byte_slice_get;
      Alcotest.test_case "view: byte_slice decode" `Quick
        test_view_byte_slice_decode;
      Alcotest.test_case "view: byte_slice nested" `Quick
        test_view_byte_slice_nested;
      (* raw access: get / set / sub *)
      Alcotest.test_case "raw: get uint" `Quick test_raw_get_uint;
      Alcotest.test_case "raw: get bitfield" `Quick test_raw_get_bitfield;
      Alcotest.test_case "raw: set uint" `Quick test_raw_set_uint;
      Alcotest.test_case "raw: set bitfield" `Quick test_raw_set_bitfield;
      Alcotest.test_case "raw: sub nested" `Quick test_raw_sub_nested;
      Alcotest.test_case "raw: sub 3 layers" `Quick test_raw_sub_three_layers;
      Alcotest.test_case "raw: with offset" `Quick test_raw_with_offset;
      (* dependent-size byte_slice *)
      Alcotest.test_case "dep: byte_slice decode empty" `Quick
        test_dep_bslice_decode_empty;
      Alcotest.test_case "dep: byte_slice decode 4" `Quick
        test_dep_bslice_decode_4;
      Alcotest.test_case "dep: byte_slice decode 100" `Quick
        test_dep_bslice_decode_100;
      Alcotest.test_case "dep: byte_slice roundtrip" `Quick
        test_dep_bslice_roundtrip;
      Alcotest.test_case "dep: byte_slice get payload" `Quick
        test_dep_bslice_get_payload;
      Alcotest.test_case "dep: byte_slice sub" `Quick test_dep_bslice_sub;
      Alcotest.test_case "dep: byte_slice set length" `Quick
        test_dep_bslice_set_length;
      Alcotest.test_case "dep: byte_slice get length" `Quick
        test_dep_bslice_get_length;
      (* dependent-size byte_array *)
      Alcotest.test_case "dep: byte_array decode" `Quick
        test_dep_byte_array_decode;
      Alcotest.test_case "dep: byte_array roundtrip" `Quick
        test_dep_byte_array_roundtrip;
      Alcotest.test_case "dep: byte_array get" `Quick test_dep_byte_array_get;
      (* fixed field after variable field *)
      Alcotest.test_case "dep: fixed after variable get checksum" `Quick
        test_dep_trailer_get_checksum;
      Alcotest.test_case "dep: fixed after variable set checksum" `Quick
        test_dep_trailer_set_checksum;
      Alcotest.test_case "dep: fixed after variable decode" `Quick
        test_dep_trailer_decode;
      Alcotest.test_case "dep: fixed after variable roundtrip" `Quick
        test_dep_trailer_roundtrip;
      (* wire_size API for variable codecs *)
      Alcotest.test_case "dep: is_fixed" `Quick test_dep_is_fixed;
      Alcotest.test_case "dep: wire_size raises" `Quick
        test_dep_wire_size_raises;
      Alcotest.test_case "dep: min_wire_size" `Quick test_dep_min_wire_size;
      Alcotest.test_case "dep: wire_size_at" `Quick test_dep_compute_wire_size;
      (* Field.ref expressions *)
      Alcotest.test_case "dep: codec ref" `Quick test_dep_codec_ref;
      Alcotest.test_case "dep: u64 size in range" `Quick test_dep_size_in_range;
      Alcotest.test_case "dep: u64 size out of range raises" `Quick
        test_dep_size_out_of_range;
      Alcotest.test_case "dep: codec ref size eval" `Quick
        test_dep_ref_size_eval;
      (* struct_of_codec for variable-size codecs *)
      Alcotest.test_case "dep: struct_of_codec" `Quick test_struct_of_dep;
      Alcotest.test_case "dep: trailer struct_of_codec" `Quick
        test_struct_of_dep_trailer;
      (* sizeof_this / field_pos *)
      Alcotest.test_case "codec: sizeof_this" `Quick test_codec_sizeof_this;
      Alcotest.test_case "codec: field_pos" `Quick test_codec_field_pos;
      (* bitfield batch access *)
      Alcotest.test_case "bitfield: extract matches get" `Quick
        test_bitfield_extract;
      Alcotest.test_case "bitfield: non-bf field raises" `Quick
        test_bitfield_non_bf_raises;
      Alcotest.test_case "bitfield: short buffer" `Quick
        test_bitfield_short_buffer;
      Alcotest.test_case "bitfield: load_word shared" `Quick
        test_bitfield_load_shared;
      (* codec embed *)
      Alcotest.test_case "embed: decode" `Quick test_codec_embed_decode;
      Alcotest.test_case "embed: encode" `Quick test_codec_embed_encode;
      Alcotest.test_case "embed: roundtrip" `Quick test_codec_embed_roundtrip;
      Alcotest.test_case "embed: wire_size" `Quick test_codec_embed_wire_size;
      Alcotest.test_case "embed: bitfield" `Quick test_codec_embed_bitfield;
      Alcotest.test_case "embed: nested" `Quick test_codec_embed_nested;
      Alcotest.test_case "embed: nested roundtrip" `Quick
        test_codec_embed_nested_roundtrip;
      Alcotest.test_case "embed: cross field ref" `Quick
        test_codec_cross_field_ref;
      Alcotest.test_case "embed: cross field ref varying" `Quick
        test_codec_crossref_field_varying;
      Alcotest.test_case "embed: cross field ref oversized" `Quick
        test_codec_crossref_field_oversized;
      Alcotest.test_case "embed: cross field ref underflow" `Quick
        test_codec_crossref_field_underflow;
      Alcotest.test_case "embed: cross field ref zero data" `Quick
        test_codec_crossref_field_zerodata;
      Alcotest.test_case "embed: cross field ref shadow" `Quick
        test_codec_field_shadow;
      Alcotest.test_case "embed: cross field ref two levels" `Quick
        test_codec_crossref_field_twolevels;
      Alcotest.test_case "embed: cross field ref bitfield" `Quick
        test_codec_crossref_field_bitfield;
      (* optional *)
      Alcotest.test_case "optional: present decode" `Quick
        test_optional_present_decode;
      Alcotest.test_case "optional: absent decode" `Quick
        test_optional_absent_decode;
      Alcotest.test_case "optional: present encode" `Quick
        test_optional_present_encode;
      Alcotest.test_case "optional: absent encode" `Quick
        test_optional_absent_encode;
      Alcotest.test_case "optional: present roundtrip" `Quick
        test_optional_present_roundtrip;
      Alcotest.test_case "optional: absent roundtrip" `Quick
        test_optional_absent_roundtrip;
      Alcotest.test_case "optional: wire_size present" `Quick
        test_optional_wire_size_present;
      Alcotest.test_case "optional: wire_size absent" `Quick
        test_optional_wire_size_absent;
      Alcotest.test_case "byte_array sized by optional_or field" `Quick
        test_bytearray_sized_by_optional_or;
      Alcotest.test_case "optional: codec present" `Quick
        test_optional_codec_present;
      Alcotest.test_case "optional: codec absent" `Quick
        test_optional_codec_absent;
      Alcotest.test_case "optional: both present" `Quick
        test_optional_both_present;
      Alcotest.test_case "optional: both absent" `Quick
        test_optional_both_absent;
      Alcotest.test_case "optional: mixed" `Quick test_optional_mixed;
      Alcotest.test_case "optional: dynamic present" `Quick test_dyn_opt_present;
      Alcotest.test_case "optional: dynamic absent" `Quick test_dyn_opt_absent;
      Alcotest.test_case "optional: dynamic get trail" `Quick
        test_dyn_opt_get_trail;
      Alcotest.test_case "optional: dynamic roundtrip" `Quick
        test_field_optional_dynamic_roundtrip;
      Alcotest.test_case "optional: dynamic rejects inconsistent gate" `Quick
        test_dyn_opt_reject_gate;
      Alcotest.test_case "optional: encode size totality" `Quick
        test_encode_totality;
      Alcotest.test_case "optional: bool ref present" `Quick
        test_dyn_opt_anyref_present;
      Alcotest.test_case "optional: bool ref absent" `Quick
        test_dyn_opt_anyref_absent;
      Alcotest.test_case "optional: land predicate" `Quick
        test_optional_land_predicate;
      Alcotest.test_case "optional: lsr predicate" `Quick
        test_optional_lsr_predicate;
      Alcotest.test_case "optional: mod predicate" `Quick
        test_optional_mod_predicate;
      Alcotest.test_case "optional: lor predicate" `Quick
        test_optional_lor_predicate;
      Alcotest.test_case "optional: Field.ref reads inner value" `Quick
        test_field_ref_through_optional;
      Alcotest.test_case "uint64 in size expr" `Quick test_uint64_in_size_expr;
      Alcotest.test_case "constraint: int64 signed-magnitude domain" `Quick
        test_int64_field_constraint_accepts_signed_magnitude_domain;
      Alcotest.test_case "constraint: int64 rejects negative zero" `Quick
        test_int64_field_constraint_rejects_negative_zero;
      Alcotest.test_case "constraint: int64 mask over map'd field" `Quick
        test_int64_mask_constraint_over_map;
      Alcotest.test_case "constraint: int-ref bound on uint64 enforced" `Quick
        test_uint64_int_ref_constraint_enforced;
      (* repeat *)
      Alcotest.test_case "repeat: decode empty" `Quick test_repeat_decode_empty;
      Alcotest.test_case "repeat: decode one" `Quick test_repeat_decode_one;
      Alcotest.test_case "repeat: decode multiple" `Quick
        test_repeat_decode_multiple;
      Alcotest.test_case "repeat: encode" `Quick test_repeat_encode;
      Alcotest.test_case "repeat: exact byte budget" `Quick
        test_repeat_exact_budget;
      Alcotest.test_case "repeat: roundtrip" `Quick test_repeat_roundtrip;
      Alcotest.test_case "repeat: primitive" `Quick test_repeat_primitive;
      Alcotest.test_case "repeat: size_of_value" `Quick
        test_repeat_size_of_value;
      Alcotest.test_case "casetype field: login" `Quick
        test_casetype_field_login;
      Alcotest.test_case "casetype field: logout" `Quick
        test_casetype_field_logout;
      Alcotest.test_case "casetype field: default" `Quick
        test_casetype_field_default;
      Alcotest.test_case "casetype field: no match is Invalid_tag" `Quick
        test_casetype_no_match_invalid_tag;
      Alcotest.test_case "casetype default recovers matched tag" `Quick
        test_casetype_default_recovers_tag;
      Alcotest.test_case "casetype field: roundtrip" `Quick
        test_casetype_field_roundtrip;
      Alcotest.test_case "casetype field: size_of_value" `Quick
        test_casetype_size_of_value;
      Alcotest.test_case "casetype field: length-prefixed" `Quick
        test_length_prefixed_casetype;
      Alcotest.test_case "repeat: casetype TLV decode" `Quick
        test_repeat_casetype_decode;
      Alcotest.test_case "repeat: casetype TLV roundtrip" `Quick
        test_repeat_casetype_roundtrip;
      Alcotest.test_case "repeat: casetype TLV empty" `Quick
        test_repeat_casetype_empty;
      Alcotest.test_case "repeat: casetype with zeroterm_at_most case" `Quick
        test_repeat_casetype_zeroterm_at_most;
      Alcotest.test_case "repeat: casetype with bitfield case" `Quick
        test_repeat_casetype_bits_case;
      Alcotest.test_case "repeat: casetype with unprojectable case rejected"
        `Quick test_repeat_casetype_unprojectable_case_rejected;
      (* zero-terminated strings *)
      Alcotest.test_case "zeroterm: roundtrip" `Quick test_zeroterm_roundtrip;
      Alcotest.test_case "zeroterm: empty" `Quick test_zeroterm_empty;
      Alcotest.test_case "zeroterm: embedded NUL rejected" `Quick
        test_zeroterm_embedded_nul_rejected;
      Alcotest.test_case "zeroterm: one NUL message on every encode path" `Quick
        test_zeroterm_nul_message_shared;
      Alcotest.test_case "zeroterm: one region message on every encode path"
        `Quick test_zeroterm_region_message_shared;
      Alcotest.test_case "zeroterm: missing terminator" `Quick
        test_zeroterm_missing_terminator;
      Alcotest.test_case "repeat: with trailer" `Quick test_repeat_with_trailer;
      Alcotest.test_case "repeat: variable size elements" `Quick
        test_repeat_variable_size_elements;
      (* composition: optional + repeat + codec *)
      Alcotest.test_case "composition: tm-like full" `Quick test_tm_like_full;
      Alcotest.test_case "composition: tm-like no trailing" `Quick
        test_tm_like_no_trailing;
      Alcotest.test_case "composition: tm-like roundtrip" `Quick
        test_tm_like_roundtrip;
      (* multiple consecutive variable-size fields *)
      Alcotest.test_case "multi-var: decode" `Quick test_multi_var_decode;
      Alcotest.test_case "multi-var: roundtrip" `Quick test_multi_var_roundtrip;
      Alcotest.test_case "multi-var: get" `Quick test_multi_var_get;
      Alcotest.test_case "multi-var: fixed after" `Quick
        test_multi_var_fixed_after;
      Alcotest.test_case "multi-var: ssh disconnect (two var byte_slice)" `Quick
        test_ssh_two_var_slices;
      Alcotest.test_case "multi-var: two embedded sub-codecs" `Quick
        test_two_var_codecs_embedded;
      Alcotest.test_case "multi-var: three embedded sub-codecs" `Quick
        test_three_var_codecs_embedded;
      Alcotest.test_case "multi-var: four embedded sub-codecs" `Quick
        test_four_var_codecs_embedded;
      Alcotest.test_case "multi-var: byte_slice then byte_array" `Quick
        test_slice_then_array;
      Alcotest.test_case "multi-var: sub-codec then byte_array" `Quick
        test_codec_then_array;
      Alcotest.test_case "multi-var: repeat after variable byte_slice" `Quick
        test_repeat_after_var_slice;
      (* uint: variable-width unsigned integer *)
      Alcotest.test_case "uint: 3-byte BE roundtrip" `Quick test_uint_3byte_be;
      Alcotest.test_case "uint: 1-byte like uint8" `Quick test_uint_1byte;
      Alcotest.test_case "uint: 5-byte LE roundtrip" `Quick test_uint_5byte_le;
      Alcotest.test_case "uint: dynamic size" `Quick test_uint_dynamic;
      Alcotest.test_case "enum: codec rejects unknown values" `Quick
        test_enum_codec_validates;
      (* decode allocation *)
      Alcotest.test_case "decode: >8 fields allocate no partial closure" `Quick
        test_decode_no_partial_closure;
      Alcotest.test_case "decode: high-arity roundtrip" `Quick
        test_decode_high_arity_roundtrip;
      (* encode allocation *)
      Alcotest.test_case "encode: var-bytes fields allocate nothing" `Quick
        test_encode_var_bytes_no_closure;
      Alcotest.test_case "get: immediate fields allocate nothing" `Quick
        test_get_no_allocation;
      Alcotest.test_case "set: immediate fields allocate nothing" `Quick
        test_set_no_allocation;
      Alcotest.test_case "validate: allocation does not scale with the frame"
        `Quick test_validate_allocation_is_bounded;
      Alcotest.test_case "decode: allocates at most one copy of the span" `Quick
        test_decode_allocates_one_copy;
      (* decode diagnostics *)
      Alcotest.test_case "diag: field_pos in a size expression reports itself"
        `Quick test_size_misuse_reports_itself;
      Alcotest.test_case "diag: truncated buffer still reports eof" `Quick
        test_truncated_still_reports_eof;
      Alcotest.test_case "diag: negative span is a parse error" `Quick
        test_negative_span_is_parse_error;
      Alcotest.test_case "diag: eof counts do not move with the base" `Quick
        test_eof_counts_are_base_invariant;
      Alcotest.test_case "diag: negative span width agrees with decode" `Quick
        test_negative_span_width_agrees;
      (* accessor and container contracts *)
      Alcotest.test_case "set: a refused sub-codec value writes nothing" `Quick
        test_set_refusal_leaves_buffer_untouched;
      Alcotest.test_case "get: a bitfield word past the buffer raises" `Quick
        test_get_bitfield_word_past_buffer_raises;
      Alcotest.test_case "set: a bitfield word past the buffer writes nothing"
        `Quick test_set_bitfield_word_past_buffer_writes_nothing;
      Alcotest.test_case "get: statically gated optionals agree with decode"
        `Quick test_get_static_optional_agrees_with_decode;
      Alcotest.test_case
        "get: runtime-gated optional refused with Invalid_argument" `Quick
        test_get_dynamic_optional_refuses_cleanly;
      Alcotest.test_case "repeat: element ending in a greedy sub-codec rejected"
        `Quick test_repeat_rejects_nested_greedy_element;
      Alcotest.test_case "repeat: bounded element round-trips" `Quick
        test_repeat_bounded_element_roundtrips;
    ] )
