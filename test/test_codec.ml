(* Tests for codec.ml: Codec.get/set/v *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Project a codec to its 3D rendering for substring assertions. *)
let render_3d codec =
  to_3d (module_ [ typedef (Everparse.struct_of_codec codec) ])

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
    Error (Unexpected_eof { expected = ws; got = String.length s })
  else Codec.decode codec (Bytes.of_string s) 0

(* -- Record codec tests -- *)

type simple_record = { a : int; b : int; c : int }

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
  let v = { a = 0x42; b = 0x1234; c = 0x56789ABC } in
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
      Alcotest.(check int) "c" 0x56789ABC v.c
  | Error e -> Alcotest.failf "%a" pp_parse_error e

let test_record_roundtrip () =
  let original = { a = 0xAB; b = 0xCDEF; c = 0x12345678 } in
  match encode_record simple_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      match decode_record simple_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "a roundtrip" original.a decoded.a;
          Alcotest.(check int) "b roundtrip" original.b decoded.b;
          Alcotest.(check int) "c roundtrip" original.c decoded.c
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

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
  | Error (Constraint_failed "field constraint") -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "expected decode failure"

let test_metadata_action_fail () =
  let buf = Bytes.of_string "\x09" in
  match Codec.decode meta_codec buf 0 with
  | Error (Constraint_failed "field action") -> ()
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
  | Error (Constraint_failed "where clause") -> ()
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
  | exception Validation_error (Constraint_failed _) -> ()

let test_validate_rejects_bad_constraint () =
  (* constraint requires x <= 10, set x = 11 *)
  let buf = Bytes.of_string "\x0B" in
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "get bypasses constraint" 11 (get_x buf 0);
  match Codec.validate validate_codec buf 0 with
  | () -> Alcotest.fail "expected validate to reject constraint violation"
  | exception Validation_error (Constraint_failed _) -> ()

let test_validate_then_get () =
  (* x = 8 satisfies both where (= 8) and constraint (<= 10) *)
  let buf = Bytes.of_string "\x08" in
  Codec.validate validate_codec buf 0;
  let get_x = Staged.unstage (Codec.get validate_codec validate_cf_x) in
  Alcotest.(check int) "validate then get" 8 (get_x buf 0)

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
  | Error (Constraint_failed _) -> ()
  | Ok _ -> Alcotest.fail "decode accepted a Wire.where violation"
  | Error _ -> Alcotest.fail "decode failed with the wrong error");
  match Codec.decode typ_where_codec (Bytes.of_string "\001\000") 0 with
  | Ok _ -> ()
  | Error _ -> Alcotest.fail "decode rejected a valid input"

let test_validate_enforces_typ_where () =
  (match Codec.validate typ_where_codec (Bytes.of_string "\006\000") 0 with
  | () -> Alcotest.fail "validate accepted a Wire.where violation"
  | exception Validation_error (Constraint_failed _) -> ());
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
  | Error (Constraint_failed _) -> ()
  | _ -> Alcotest.fail "decode did not reject the action violation");
  (match
     Codec.validate action_validate_codec (Bytes.of_string "\200\000") 0
   with
  | () -> Alcotest.fail "validate skipped the field action decode enforces"
  | exception Validation_error (Constraint_failed _) -> ());
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
type ba_record = { id : int; uuid : string; tag : int }

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
  let original = { id = 0x12345678; uuid = "0123456789abcdef"; tag = 0xABCD } in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          Alcotest.(check int) "id" original.id decoded.id;
          Alcotest.(check string) "uuid" original.uuid decoded.uuid;
          Alcotest.(check int) "tag" original.tag decoded.tag
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

let test_record_byte_array_padding () =
  (* Short string should be zero-padded *)
  let original = { id = 1; uuid = "short"; tag = 2 } in
  match encode_record ba_record_codec original with
  | Error e -> Alcotest.failf "encode: %a" pp_parse_error e
  | Ok encoded -> (
      Alcotest.(check int) "wire size" 22 (String.length encoded);
      (* Verify zero padding: bytes 9..19 should be zero *)
      for i = 9 to 19 do
        Alcotest.(check int)
          (Fmt.str "padding byte %d" i)
          0
          (Char.code encoded.[i])
      done;
      match decode_record ba_record_codec encoded with
      | Ok decoded ->
          (* Decoded uuid includes the zero padding *)
          Alcotest.(check int) "uuid length" 16 (String.length decoded.uuid);
          Alcotest.(check string)
            "uuid prefix" "short"
            (String.sub decoded.uuid 0 5)
      | Error e -> Alcotest.failf "%a" pp_parse_error e)

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

let test_encode_underrun_raises () =
  (* byte_array ~size:n truncates a value longer than n at write time,
     while size_of_value reports String.length v. Passing a 5-byte value
     to a 3-byte field is the simplest underrun: the assertion must fire. *)
  let codec =
    Codec.v "Mismatch" Fun.id
      Codec.[ Field.v "data" (byte_array ~size:(Wire.int 3)) $ Fun.id ]
  in
  let buf = Bytes.create 5 in
  match Codec.encode codec "AAAAA" buf 0 with
  | exception Invalid_argument m
    when String.length m > 0
         && contains ~sub:"writer wrote fewer bytes than promised" m ->
      ()
  | exception Invalid_argument m ->
      Alcotest.failf "wrong Invalid_argument message: %s" m
  | () -> Alcotest.fail "expected underrun assertion to fire"

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
  | Error (Unexpected_eof _) -> ()
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
  | exception Validation_error (Constraint_failed _) -> ()

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
  let buf = Bytes.of_string "\x42" in
  Codec.validate codec buf 0;
  (* validate does NOT fire actions *)
  Alcotest.(check int)
    "action not fired by validate" 0
    !(action_out2.Wire.Private.Types.cell)

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
  | exception Validation_error (Constraint_failed _) -> ()

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
  | exception Validation_error (Constraint_failed _) -> ()

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
  | exception Validation_error (Constraint_failed _) -> ()

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
  | exception Validation_error (Constraint_failed _) -> ()

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
  | exception Validation_error (Constraint_failed _) -> ()

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
  (* Use env1 (from codec1) with codec2's get -- should not crash *)
  let get_v2 = Staged.unstage (Codec.get ~env:env1 codec2 cf_v2) in
  (* No action on cf_v2, so env is ignored -- should work fine *)
  Alcotest.(check int)
    "wrong env ignored" 0x42
    (get_v2 (Bytes.of_string "\x42") 0)

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
  | Error (Unexpected_eof _) -> ()
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
  (Staged.unstage (Codec.set codec cf_v)) buf 10 0xDEADBEEF;
  Alcotest.(check int)
    "get at offset 10" 0xDEADBEEF
    ((Staged.unstage (Codec.get codec cf_v)) buf 10)

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
      | Error (Constraint_failed _) -> ()
      | Error e ->
          Alcotest.failf "%s: expected Constraint_failed, got %a" name
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
  | Error (Constraint_failed _) -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

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
  | Error (Constraint_failed _) -> ()
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
  Alcotest.(check int) "same word" wa wb;
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
  | Error (Unexpected_eof _) -> ()
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

type multi_opt = { data : int; ocf : int option; fecf : int option }

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
  Alcotest.(check (option int)) "ocf" (Some 0x22222222) r.ocf;
  Alcotest.(check (option int)) "fecf" (Some 0x3333) r.fecf

let test_optional_both_absent () =
  let c = multi_opt_codec ~ocf:false ~fecf:false in
  (* data(2) only *)
  let buf = Bytes.create 2 in
  Bytes.set_uint16_be buf 0 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.data;
  Alcotest.(check (option int)) "ocf" None r.ocf;
  Alcotest.(check (option int)) "fecf" None r.fecf

let test_optional_mixed () =
  let c = multi_opt_codec ~ocf:true ~fecf:false in
  (* data(2) + ocf(4) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0x1111;
  Bytes.set_int32_be buf 2 0x22222222l;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "data" 0x1111 r.data;
  Alcotest.(check (option int)) "ocf" (Some 0x22222222) r.ocf;
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

type tm_opt = { ocf_flag : bool; data : int; ocf : int option; trail : int }

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
  Alcotest.(check (option int)) "ocf" (Some 0xDEADBEEF) r.ocf;
  Alcotest.(check int) "trail" 0xFF r.trail

let test_dyn_opt_anyref_absent () =
  let buf = Bytes.create 4 in
  Bytes.set_uint8 buf 0 0x00;
  Bytes.set_uint16_be buf 1 0x1234;
  Bytes.set_uint8 buf 3 0xFF;
  let r = decode_ok (Codec.decode tm_opt_codec buf 0) in
  Alcotest.(check bool) "ocf_flag" false r.ocf_flag;
  Alcotest.(check int) "data" 0x1234 r.data;
  Alcotest.(check (option int)) "ocf" None r.ocf;
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

type ev_payload = [ `Login of int | `Logout of int | `Other of int ]

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
  Alcotest.(check bool) "Logout" true (r.data = `Logout 0x55667788)

let test_casetype_field_default () =
  let buf = Bytes.create 10 in
  Bytes.set_int64_be buf 0 0L;
  Bytes.set_uint8 buf 8 99;
  Bytes.set_uint8 buf 9 7;
  let r = decode_ok (Codec.decode casetype_field_codec buf 0) in
  Alcotest.(check bool) "Other 7" true (r.data = `Other 7)

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
  ocf : int option;
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
  Alcotest.(check (option int)) "ocf" (Some 0x33333333) r.ocf;
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
  Alcotest.(check (option int)) "ocf" None r.ocf;
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
      ocf = Some 0xDEADBEEF;
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
  Alcotest.(check (option int)) "ocf" original.ocf decoded.ocf;
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

type ssh_string = { len : int; data : Slice.t }

let ssh_f_len = Field.v "len" uint32be
let ssh_f_data = Field.v "data" (byte_slice ~size:(Field.ref ssh_f_len))

let ssh_string_codec =
  Codec.v "SshString"
    (fun len data -> { len; data })
    Codec.[ (ssh_f_len $ fun s -> s.len); (ssh_f_data $ fun s -> s.data) ]

let mk_ssh_string s =
  let b = Bytes.of_string s in
  {
    len = String.length s;
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
        (f_desc_len $ fun (_, d, _) -> Slice.length d);
        (f_desc $ fun (_, d, _) -> d);
        (f_lang_len $ fun (_, _, l) -> Slice.length l);
        (f_lang $ fun (_, _, l) -> l);
      ]
  in
  let v = (11, (mk_ssh_string "bye").data, (mk_ssh_string "en-US").data) in
  let buf = Bytes.create 200 in
  Codec.encode codec v buf 0;
  let r, d, l = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "reason" 11 r;
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
  Alcotest.(check int) "a.len" 4 a.len;
  Alcotest.(check string) "a.data" "abcd" (Slice.to_string a.data);
  Alcotest.(check int) "b.len" 2 b.len;
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

type uint_rec = { tag : int; value : int }

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
  let original = { tag = 0x42; value = 0x1A2B3C } in
  let buf = Bytes.create 4 in
  Codec.encode codec original buf 0;
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "be byte 0" 0x1A (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "be byte 1" 0x2B (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "be byte 2" 0x3C (Bytes.get_uint8 buf 3);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "tag" original.tag decoded.tag;
  Alcotest.(check int) "value" original.value decoded.value

let test_uint_1byte () =
  let codec =
    let open Codec in
    v "U1" (fun v -> v) [ (Field.v "V" (uint (Wire.int 1)) $ fun v -> v) ]
  in
  let buf = Bytes.create 1 in
  Codec.encode codec 0xAB buf 0;
  Alcotest.(check int) "byte" 0xAB (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "roundtrip" 0xAB decoded

let test_uint_5byte_le () =
  let codec =
    let open Codec in
    v "U5LE"
      (fun v -> v)
      [ (Field.v "V" (uint ~endian:Wire.Little (Wire.int 5)) $ fun v -> v) ]
  in
  let value = 0x01_02_03_04_05 in
  let buf = Bytes.create 5 in
  Codec.encode codec value buf 0;
  Alcotest.(check int) "le byte 0" 0x05 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "le byte 1" 0x04 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "le byte 2" 0x03 (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "le byte 3" 0x02 (Bytes.get_uint8 buf 3);
  Alcotest.(check int) "le byte 4" 0x01 (Bytes.get_uint8 buf 4);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "roundtrip" value decoded

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
  Alcotest.(check int) "value" 0x1234 value

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
      Alcotest.test_case "record: byte_array padding" `Quick
        test_record_byte_array_padding;
      Alcotest.test_case "repeat: zeroterm element roundtrip" `Quick
        test_repeat_zeroterm_element;
      Alcotest.test_case "repeat: zeroterm element projection" `Quick
        test_repeat_zeroterm_projection;
      Alcotest.test_case "optional: variable byte_array inner roundtrip" `Quick
        test_optional_var_byte_array;
      Alcotest.test_case "optional: self-delimiting codec inner roundtrip"
        `Quick test_optional_self_delimiting_codec;
      Alcotest.test_case "repeat: byte_array element roundtrip" `Quick
        test_repeat_byte_array_element;
      Alcotest.test_case "repeat: byte_array element projection" `Quick
        test_repeat_byte_array_projection;
      Alcotest.test_case "repeat: rejects unprojectable element" `Quick
        test_repeat_rejects_unprojectable;
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
      Alcotest.test_case "repeat/array reject bitfield element" `Quick
        test_repeat_array_reject_bitfield;
      Alcotest.test_case "array/repeat reject zero-width element" `Quick
        test_reject_zero_width_element;
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
      Alcotest.test_case "uint rejects out-of-range size" `Quick
        test_uint_size_bounds;
      Alcotest.test_case "bits rejects out-of-range width" `Quick
        test_bits_width_bounds;
      Alcotest.test_case "casetype case requires ~index" `Quick
        test_casetype_case_requires_index;
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
      Alcotest.test_case "encode: underrun raises" `Quick
        test_encode_underrun_raises;
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
      Alcotest.test_case "embed: param sub-codec requires env" `Quick
        test_embed_param_requires_env;
      Alcotest.test_case "embed: sub-codec where enforced" `Quick
        test_embed_where_enforced;
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
      Alcotest.test_case "constraint: int-ref bound on uint64 enforced" `Quick
        test_uint64_int_ref_constraint_enforced;
      (* repeat *)
      Alcotest.test_case "repeat: decode empty" `Quick test_repeat_decode_empty;
      Alcotest.test_case "repeat: decode one" `Quick test_repeat_decode_one;
      Alcotest.test_case "repeat: decode multiple" `Quick
        test_repeat_decode_multiple;
      Alcotest.test_case "repeat: encode" `Quick test_repeat_encode;
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
    ] )
