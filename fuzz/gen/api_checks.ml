open Gen
open Properties
open Invariants

let expect_invalid label f =
  match f () with
  | _ -> Alcobar.failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

let construction_guard_cases label =
  [
    const_case (label ^ " bits width 0") (fun () ->
        expect_invalid "bits width 0" (fun () -> Wire.bits ~width:0 Wire.U8));
    const_case (label ^ " bits width > U8") (fun () ->
        expect_invalid "bits width > U8" (fun () -> Wire.bits ~width:9 Wire.U8));
    const_case (label ^ " bits width > U16") (fun () ->
        expect_invalid "bits width > U16" (fun () ->
            Wire.bits ~width:17 Wire.U16be));
    const_case (label ^ " bits width > U32") (fun () ->
        expect_invalid "bits width > U32" (fun () ->
            Wire.bits ~width:33 Wire.U32be));
    const_case (label ^ " casetype dynamic tag") (fun () ->
        expect_invalid "casetype dynamic tag" (fun () ->
            Wire.casetype "BadDynTag"
              (Wire.uint ~endian:Wire.Big 2)
              [
                Wire.case ~index:(Optint.Int63.of_int 0) Wire.uint8
                  ~inject:Fun.id ~project:(fun v -> Some v);
              ]));
    const_case (label ^ " casetype enum tag") (fun () ->
        expect_invalid "casetype enum tag" (fun () ->
            Wire.casetype "BadEnumTag"
              (Wire.enum "BadTag" [ ("A", 0) ] Wire.uint16be)
              [
                Wire.case ~index:(Wire.UInt16.v 0) Wire.uint8 ~inject:Fun.id
                  ~project:(fun v -> Some v);
              ]));
    const_case (label ^ " repeat empty") (fun () ->
        expect_invalid "repeat empty" (fun () ->
            Wire.Field.repeat "Empty" ~size:(Wire.int 1) Wire.empty));
    const_case (label ^ " repeat_seq empty") (fun () ->
        expect_invalid "repeat_seq empty" (fun () ->
            Wire.Field.repeat_seq "Empty" ~seq:Wire.seq_list ~size:(Wire.int 1)
              Wire.empty));
  ]

let check_all_zeros_semantic label () =
  let pad = Wire.Field.v "Pad" Wire.all_zeros in
  let g =
    {
      codec = Wire.Codec.v "FuzzZeros" Fun.id Wire.Codec.[ pad $ Fun.id ];
      typ =
        Wire.codec
          (Wire.Codec.v "FuzzZerosTyp" Fun.id Wire.Codec.[ pad $ Fun.id ]);
      positive = Alcobar.const ("", Bytes.empty);
      random = bytes_any;
      adversarial = bytes_any;
      equal = String.equal;
      env = None;
      fields = [];
      adversarial_value = None;
    }
  in
  check_decode_validate_agree label "all_zeros" g
    (Bytes.of_string "\000\001\000")

let signed_slice_semantic_codec () =
  let len = Wire.Field.v "Len" Wire.int8 in
  let data = Wire.Field.v "Data" (Wire.byte_slice ~size:(Wire.Field.ref len)) in
  let cf_data = Wire.Codec.(data $ snd) in
  let codec =
    Wire.Codec.v "FuzzSignedSlice"
      (fun l d -> (l, d))
      Wire.Codec.[ len $ fst; cf_data ]
  in
  let g =
    {
      codec;
      typ = Wire.codec codec;
      positive =
        Alcobar.const
          ((Wire.SInt8.v 0, slice_of_string ""), Bytes.of_string "\000");
      random = bytes_any;
      adversarial = bytes_any;
      equal =
        (fun (l1, s1) (l2, s2) ->
          Wire.SInt8.equal l1 l2
          && String.equal (string_of_slice s1) (string_of_slice s2));
      env = None;
      fields = [];
      adversarial_value = None;
    }
  in
  (g, codec, cf_data)

let check_negative_byte_slice_semantic label () =
  let g, codec, cf_data = signed_slice_semantic_codec () in
  let bad = Bytes.of_string "\255" in
  check_decode_validate_agree label "negative byte_slice" g bad;
  match validate_one g bad with
  | `Reject | `Crash -> ()
  | `Ok -> (
      match (Wire.Staged.unstage (Wire.Codec.get codec cf_data)) bad 0 with
      | s ->
          if Bytesrw.Bytes.Slice.length s < 0 then
            Alcobar.failf "%s validate-then-get produced negative slice" label
      | exception Invalid_argument m ->
          Alcobar.failf "%s validate-then-get crashed: %s" label m)

let semantic_invariant_cases label =
  [
    const_case
      (label ^ " decode/validate all_zeros")
      (check_all_zeros_semantic label);
    const_case
      (label ^ " decode/validate negative byte_slice")
      (check_negative_byte_slice_semantic label);
  ]

type api_access = {
  a : Wire.UInt8.t;
  hi : int;
  lo : int;
  payload : Bytesrw.Bytes.Slice.t;
}

let api_access_codec () =
  let f_a = Wire.Field.v "A" Wire.uint8 in
  let f_hi = Wire.Field.v "Hi" (Wire.bits ~width:3 Wire.U8) in
  let f_lo = Wire.Field.v "Lo" (Wire.bits ~width:5 Wire.U8) in
  let f_payload = Wire.Field.v "Payload" (Wire.byte_slice ~size:(Wire.int 3)) in
  let bf_a = Wire.Codec.(f_a $ fun r -> r.a) in
  let bf_hi = Wire.Codec.(f_hi $ fun r -> r.hi) in
  let bf_lo = Wire.Codec.(f_lo $ fun r -> r.lo) in
  let bf_payload = Wire.Codec.(f_payload $ fun r -> r.payload) in
  let codec =
    Wire.Codec.v "ApiAccess"
      (fun a hi lo payload -> { a; hi; lo; payload })
      Wire.Codec.[ bf_a; bf_hi; bf_lo; bf_payload ]
  in
  (codec, bf_a, bf_hi, bf_lo, bf_payload)

let check_int label expected actual =
  if actual <> expected then
    Alcobar.failf "%s: expected %d, got %d" label expected actual

let check_string label expected actual =
  if not (String.equal expected actual) then
    Alcobar.failf "%s: expected %S, got %S" label expected actual

let check_api_getters codec bf_a bf_hi bf_lo bf_payload buf base a hi lo
    payload_s =
  let get_a = Wire.Codec.get codec bf_a |> Wire.Staged.unstage in
  let get_hi = Wire.Codec.get codec bf_hi |> Wire.Staged.unstage in
  let get_lo = Wire.Codec.get codec bf_lo |> Wire.Staged.unstage in
  let get_payload = Wire.Codec.get codec bf_payload |> Wire.Staged.unstage in
  check_int "Codec.get scalar" a (Wire.UInt8.to_int (get_a buf base));
  check_int "Codec.get bitfield hi" hi (get_hi buf base);
  check_int "Codec.get bitfield lo" lo (get_lo buf base);
  check_string "Codec.get slice" payload_s
    (string_of_slice (get_payload buf base));
  let slice_offset =
    Wire.Codec.slice_offset codec bf_payload |> Wire.Staged.unstage
  in
  let slice_length =
    Wire.Codec.slice_length codec bf_payload |> Wire.Staged.unstage
  in
  check_int "Codec.slice_offset" (base + 2) (slice_offset buf base);
  check_int "Codec.slice_length" 3 (slice_length buf base)

let check_api_bitfields codec bf_hi bf_lo buf base hi lo =
  let bit_hi = Wire.Codec.bitfield codec bf_hi in
  let bit_lo = Wire.Codec.bitfield codec bf_lo in
  let load = Wire.Codec.load_word bit_hi |> Wire.Staged.unstage in
  let word = load buf base in
  check_int "Codec.extract hi" hi (Wire.Codec.extract bit_hi word);
  check_int "Codec.extract lo" lo (Wire.Codec.extract bit_lo word)

(* [byte_slice] is the mutable zero-copy span: both staged access and record
   decode must retain an alias to the caller's buffer, in both directions. *)
let check_api_slice_aliases codec bf_payload buf base =
  let module Slice = Bytesrw.Bytes.Slice in
  let get = Wire.Codec.get codec bf_payload |> Wire.Staged.unstage in
  let payload = get buf base in
  let first = Slice.first payload in
  let original = Bytes.get_uint8 buf first in
  let changed = original lxor 0xFF in
  Bytes.set_uint8 (Slice.bytes payload) first changed;
  check_int "Codec.get slice aliases buffer" changed (Bytes.get_uint8 buf first);
  Bytes.set_uint8 buf first original;
  check_int "buffer aliases Codec.get slice" original
    (Bytes.get_uint8 (Slice.bytes payload) first);
  match Wire.Codec.decode codec buf base with
  | Error e ->
      Alcobar.failf "Codec.decode for slice alias failed: %a"
        Wire.pp_parse_error e
  | Ok decoded ->
      Bytes.set_uint8 buf first changed;
      check_int "buffer aliases decoded slice" changed
        (Bytes.get_uint8
           (Slice.bytes decoded.payload)
           (Slice.first decoded.payload));
      Bytes.set_uint8 buf first original

let check_api_setters codec bf_a bf_hi bf_lo bf_payload buf base next_a next_hi
    next_lo next_payload_s =
  let get_a = Wire.Codec.get codec bf_a |> Wire.Staged.unstage in
  let get_hi = Wire.Codec.get codec bf_hi |> Wire.Staged.unstage in
  let get_lo = Wire.Codec.get codec bf_lo |> Wire.Staged.unstage in
  let get_payload = Wire.Codec.get codec bf_payload |> Wire.Staged.unstage in
  let set_a = Wire.Codec.set codec bf_a |> Wire.Staged.unstage in
  let set_hi = Wire.Codec.set codec bf_hi |> Wire.Staged.unstage in
  let set_lo = Wire.Codec.set codec bf_lo |> Wire.Staged.unstage in
  let set_payload = Wire.Codec.set codec bf_payload |> Wire.Staged.unstage in
  set_a buf base (Wire.UInt8.v next_a);
  set_hi buf base next_hi;
  set_lo buf base next_lo;
  set_payload buf base (slice_of_string next_payload_s);
  check_int "Codec.set scalar" next_a (Wire.UInt8.to_int (get_a buf base));
  check_int "Codec.set bitfield hi" next_hi (get_hi buf base);
  check_int "Codec.set bitfield lo" next_lo (get_lo buf base);
  check_string "Codec.set slice" next_payload_s
    (string_of_slice (get_payload buf base))

let check_api_decode_after_set codec buf base next_a next_hi next_lo
    next_payload_s =
  match Wire.Codec.decode codec buf base with
  | Ok decoded ->
      check_int "Codec.decode after set scalar" next_a
        (Wire.UInt8.to_int decoded.a);
      check_int "Codec.decode after set hi" next_hi decoded.hi;
      check_int "Codec.decode after set lo" next_lo decoded.lo;
      check_string "Codec.decode after set slice" next_payload_s
        (string_of_slice decoded.payload)
  | Error e ->
      Alcobar.failf "Codec.decode after set failed: %a" Wire.pp_parse_error e

let check_api_accessors a hi lo payload_s next_a next_hi next_lo next_payload_s
    =
  let hi = hi land 0x7 and lo = lo land 0x1F in
  let next_hi = next_hi land 0x7 and next_lo = next_lo land 0x1F in
  let codec, bf_a, bf_hi, bf_lo, bf_payload = api_access_codec () in
  let payload = slice_of_string payload_s in
  let value = { a = Wire.UInt8.v a; hi; lo; payload } in
  let base = 1 in
  let sz = Wire.Codec.wire_size codec in
  let buf = Bytes.make (sz + 2) '\xCC' in
  Wire.Codec.encode codec value buf base;
  check_api_getters codec bf_a bf_hi bf_lo bf_payload buf base a hi lo payload_s;
  check_api_bitfields codec bf_hi bf_lo buf base hi lo;
  check_api_slice_aliases codec bf_payload buf base;
  check_api_setters codec bf_a bf_hi bf_lo bf_payload buf base next_a next_hi
    next_lo next_payload_s;
  check_api_decode_after_set codec buf base next_a next_hi next_lo
    next_payload_s

let custom_array_seq =
  Wire.Seq_map
    {
      empty = [];
      add = (fun acc v -> v :: acc);
      finish = (fun acc -> Array.of_list (List.rev acc));
      iter = (fun f arr -> Array.iter f arr);
    }

let u8_array_equal a b =
  Array.length a = Array.length b && Array.for_all2 Wire.UInt8.equal a b

let check_custom_seq a b c d =
  let values = Array.map Wire.UInt8.v [| a; b; c; d |] in
  let typ = Wire.array_seq custom_array_seq ~len:(Wire.int 4) Wire.uint8 in
  let encoded = Wire.to_bytes typ values in
  (match Wire.of_bytes typ encoded with
  | Ok decoded ->
      if not (u8_array_equal decoded values) then
        Alcobar.failf "array_seq custom seq mismatch"
  | Error e ->
      Alcobar.failf "array_seq custom seq decode failed: %a" Wire.pp_parse_error
        e);
  let f_len = Wire.Field.v "Len" Wire.uint8 in
  let f_items =
    Wire.Field.repeat_seq "Items" ~seq:custom_array_seq
      ~size:(Wire.Field.ref f_len) Wire.uint8
  in
  let codec =
    Wire.Codec.v "ApiRepeatSeq"
      (fun _ items -> items)
      Wire.Codec.
        [
          (f_len $ fun xs -> Wire.UInt8.v (Array.length xs));
          (f_items $ fun xs -> xs);
        ]
  in
  let buf = Bytes.create 5 in
  Wire.Codec.encode codec values buf 0;
  match Wire.Codec.decode codec buf 0 with
  | Ok decoded ->
      if not (u8_array_equal decoded values) then
        Alcobar.failf "repeat_seq custom seq mismatch"
  | Error e ->
      Alcobar.failf "repeat_seq custom seq decode failed: %a"
        Wire.pp_parse_error e

let raw_len_field = Wire.Private.Types.field "Len" Wire.uint8
let raw_data_field = Wire.Private.Types.field "Data" Wire.all_bytes

let raw_struct =
  Wire.Private.Types.struct_ "ApiRaw"
    [ raw_len_field; Wire.Private.Types.anon_field Wire.uint8; raw_data_field ]

let fst5 (a, _, _, _, _) = a

let check_struct_validator len data_s =
  let len = len land 0xFF in
  let bytes = Bytes.create (2 + String.length data_s) in
  Bytes.set_uint8 bytes 0 len;
  Bytes.set_uint8 bytes 1 0;
  Bytes.blit_string data_s 0 bytes 2 (String.length data_s);
  let validator = Wire.Codec.validator_of_struct raw_struct in
  check_int "Codec.struct_min_size" 2 (Wire.Codec.struct_min_size validator);
  check_int "Codec.struct_size_of" (Bytes.length bytes)
    (Wire.Codec.struct_size_of validator bytes 0);
  (match Wire.Codec.wire_size_info_of_validator validator with
  | `Fixed _ -> Alcobar.failf "raw all_bytes struct unexpectedly fixed"
  | `Variable size_of ->
      check_int "Codec.wire_size_info_of_validator" (Bytes.length bytes)
        (size_of bytes 0));
  (try Wire.Codec.validate_struct validator bytes 0 with
  | Wire.Parse_error e ->
      Alcobar.failf "validate_struct rejected positive: %a" Wire.pp_parse_error
        e
  | Invalid_argument e -> Alcobar.failf "validate_struct crashed: %s" e);
  match Wire.of_bytes (Wire.Private.Types.struct_typ raw_struct) bytes with
  | Ok () -> ()
  | Error e ->
      Alcobar.failf "Raw.struct_typ direct decode failed: %a"
        Wire.pp_parse_error e

(* [Field.action] reports the action attached via [Field.v]'s [?action] (none on
   a plain field, [Some] on an annotated one), and [Action.pp] renders the
   block. *)
let check_field_action_pp f_plain f_acting action =
  (match Wire.Field.action f_plain with
  | None -> ()
  | Some _ -> Alcobar.failf "Field.action reported an action on a plain field");
  (match Wire.Field.action f_acting with
  | Some _ -> ()
  | None -> Alcobar.failf "Field.action missing on an annotated field");
  if Fmt.str "%a" Wire.Action.pp action = "" then
    Alcobar.failf "Action.pp produced empty output"

(* Build the input/output params and check their name / pp / [Private]
   projections; returns the pair for the codec [check_metadata_helpers] seals. *)
let check_param_metadata () =
  let open Wire in
  let p_in = Param.input "Limit" uint8 in
  let p_out = Param.output "Out" uint8 in
  check_string "Param.name" "Limit" (Param.name p_in);
  check_string "Param.pp" "Limit" (Fmt.str "%a" Param.pp p_in);
  check_string "Private.param_name" "Limit"
    (Private.param_name (Param.decl p_in));
  check_string "Private.param_c_type" "uint8_t"
    (Private.param_c_type (Param.decl p_in));
  check_string "Private.ml_type_of" "int" (Private.ml_type_of uint8);
  check_string "Private.c_type_of" "uint8_t" (Private.c_type_of uint8);
  if Private.param_is_mutable (Param.decl p_in) then
    Alcobar.failf "Param.input reported mutable";
  if not (Private.param_is_mutable (Param.decl p_out)) then
    Alcobar.failf "Param.output reported immutable";
  (p_in, p_out)

let check_metadata_helpers () =
  let open Wire in
  let f_meta =
    Field.v "Meta" ~doc:"metadata"
      ~constraint_:Expr.(bool true && not (bool false))
      uint8
  in
  check_string "Field.name" "Meta" (Field.name f_meta);
  check_int "Field.typ" 1 (Option.get (size (Field.typ f_meta)));
  (match Field.constraint_ f_meta with
  | Some _ -> ()
  | None -> Alcobar.failf "Field.constraint_ missing");
  (match Field.doc f_meta with
  | Some doc -> check_string "Field.doc" "metadata" doc
  | None -> Alcobar.failf "Field.doc missing");
  check_string "Field.pp" "Meta" (Fmt.str "%a" Field.pp f_meta);
  let _ = Field.decl_of_packed (Field.Named f_meta) in
  let _ = Field.decl_of_packed (Field.Anon (Field.anon uint8)) in
  let p_in, p_out = check_param_metadata () in
  let f_src = Field.v "Src" uint8 in
  let copy_action =
    Action.on_success [ Action.assign p_out (Field.ref f_src) ]
  in
  let f_copy = Field.v "Copy" ~action:copy_action uint8 in
  check_field_action_pp f_meta f_copy copy_action;
  let codec =
    Wire.Codec.v "ApiParam"
      ~where:Expr.(Field.ref f_src <= Param.expr p_in)
      (fun src copy -> (src, copy))
      Wire.Codec.[ f_src $ fst; f_copy $ snd ]
  in
  let env = Wire.Codec.env codec |> Param.bind p_in (UInt8.v 9) in
  check_int "Param.get input" 9 (UInt8.to_int (Param.get env p_in));
  let env_by_name = Wire.Codec.env codec |> Param.bind_by_name "Limit" 8 in
  check_int "Param.bind_by_name" 8 (UInt8.to_int (Param.get env_by_name p_in));
  let buf = bytes_of_octets [ 7; 1 ] in
  (match Wire.Codec.decode ~env codec buf 0 with
  | Ok _ -> check_int "Param.get output" 7 (UInt8.to_int (Param.get env p_out))
  | Error e -> Alcobar.failf "param/action decode failed: %a" pp_parse_error e);
  check_string "Codec schema name" "ApiParam"
    (Wire.Everparse.project ~mode:`Ffi codec).Wire.Everparse.name;
  let renamed = Wire.Codec.rename "ApiParamRenamed" codec in
  check_string "Codec.rename" "ApiParamRenamed"
    (Wire.Everparse.project ~mode:`Ffi renamed).Wire.Everparse.name;
  if Wire.Codec.doc codec <> None then
    Alcobar.failf "Codec.doc unexpectedly set";
  (* [pp_value] encodes the value to read its fields back, and encode refuses a
     record that fails its own where clause. It threads no [Param.env], so the
     param-gated [codec] above cannot satisfy its cond here; print through the
     same fields with no param in the clause. *)
  let pp_codec =
    Wire.Codec.v "ApiPp"
      (fun src copy -> (src, copy))
      Wire.Codec.[ f_src $ fst; f_copy $ snd ]
  in
  let pp_value = Fmt.str "%a" (pp_value pp_codec) (UInt8.v 7, UInt8.v 1) in
  if not (String.contains pp_value 'S') then
    Alcobar.failf "pp_value did not include field output: %S" pp_value;
  let _ = Wire.Codec.field_ref Wire.Codec.(f_src $ fst) in
  check_int "Wire.size fixed" 1 (Option.get (size uint8));
  match size all_bytes with
  | None -> ()
  | Some n -> Alcobar.failf "Wire.size all_bytes = Some %d" n

let public_raw_struct () =
  let len_field =
    Wire.Everparse.Raw.field "Len" ~constraint_:Wire.Expr.true_
      ~action:
        (Wire.Action.on_success [ Wire.Action.return_bool Wire.Expr.true_ ])
      Wire.uint8
  in
  let data_field = Wire.Everparse.Raw.field "Data" Wire.all_bytes in
  let raw_struct =
    Wire.Everparse.Raw.struct_ "ApiRaw"
      [ len_field; Wire.Everparse.Raw.anon_field Wire.uint8; data_field ]
  in
  (len_field, raw_struct)

let check_everparse_schema raw_struct =
  let schema = Wire.Everparse.Raw.project_struct ~mode:`Ffi raw_struct in
  check_string "Everparse.filename" "ApiRaw.3d" (Wire.Everparse.filename schema);
  if not (Wire.Everparse.uses_wire_ctx schema) then
    Alcobar.failf "schema_of_struct did not use WireCtx";
  (match Wire.Everparse.entrypoint_struct schema with
  | Some _ -> ()
  | None -> Alcobar.failf "schema_of_struct has no entrypoint");
  let plug_fields = Wire.Everparse.plug_fields schema in
  assert_contains "Everparse.plug_fields"
    (List.map (fun f -> f.Wire.Everparse.name) plug_fields)
    [ "Len"; "Data" ];
  if Wire.Everparse.plug_setters schema = [] then
    Alcobar.failf "Everparse.plug_setters empty";
  if Wire.Everparse.extern_fn_names schema = [] then
    Alcobar.failf "Everparse.extern_fn_names empty"

let check_raw_struct_metadata raw_struct len_field =
  let forms = Wire.Everparse.field_action_forms raw_struct in
  if List.length forms <> 3 then
    Alcobar.failf "field_action_forms returned %d fields" (List.length forms);
  check_string "Raw.struct_name" "ApiRaw"
    (Wire.Everparse.Raw.struct_name raw_struct);
  assert_contains "Raw.field_names"
    (Wire.Everparse.Raw.field_names raw_struct)
    [ "Len"; "Data" ];
  let projected =
    Wire.Everparse.Raw.struct_project raw_struct ~name:"ApiProjected"
      ~keep:[ len_field ]
  in
  let _ = Wire.Everparse.Raw.field_ref len_field in
  let _ = Wire.Everparse.Raw.struct_typ raw_struct in
  (match Wire.Everparse.Raw.struct_size raw_struct with
  | None -> ()
  | Some n -> Alcobar.failf "Raw.struct_size all_bytes = Some %d" n);
  assert_contains "Raw.struct_project"
    (Wire.Everparse.Raw.field_names projected)
    [ "Len" ]

let public_param_struct () =
  let p = Wire.Everparse.Raw.param "Limit" Wire.uint8 in
  let p_out = Wire.Everparse.Raw.mutable_param "Out" Wire.uint16be in
  let param_struct =
    Wire.Everparse.Raw.param_struct "ApiParamRaw" [ p; p_out ]
      [ Wire.Everparse.Raw.field "Value" Wire.uint8 ]
  in
  (p, param_struct)

let check_raw_param_metadata param_struct raw_struct =
  if List.length (Wire.Everparse.Raw.struct_params param_struct) <> 2 then
    Alcobar.failf "Raw.struct_params length mismatch";
  assert_contains "Raw.input_param_names"
    (Wire.Everparse.Raw.input_param_names param_struct)
    [ "Limit" ];
  if Wire.Everparse.Raw.input_param_c_types param_struct = [] then
    Alcobar.failf "Raw.input_param_c_types empty";
  if Wire.Everparse.Raw.field_kinds raw_struct = [] then
    Alcobar.failf "Raw.field_kinds empty";
  let app =
    Wire.Everparse.Raw.apply
      (Wire.Everparse.Raw.type_ref "ApiParamRaw")
      [ Wire.int 3 ]
  in
  let _ = Fmt.str "%a" Wire.Everparse.Raw.pp_typ app in
  let _ =
    Fmt.str "%a" Wire.Everparse.Raw.pp_typ
      (Wire.Everparse.Raw.qualified_ref "Api" "Other")
  in
  ()

let check_raw_module_helpers raw_struct p param_struct =
  let module_ =
    Wire.Everparse.Raw.module_ ~doc:"api raw module"
      [
        Wire.Everparse.Raw.define "ApiLimit" 255;
        Wire.Everparse.Raw.extern_probe ~init:true "ApiProbe";
        Wire.Everparse.Raw.extern_probe "ApiProbeNoInit";
        Wire.Everparse.Raw.extern_fn "ApiExtern" [ p ] Wire.uint8;
        Wire.Everparse.Raw.enum_decl "ApiEnum"
          [ ("Zero", 0); ("One", 1) ]
          Wire.uint8;
        Wire.Everparse.Raw.casetype_decl "ApiCase" [] Wire.uint8
          [
            Wire.Everparse.Raw.decl_case 1 Wire.uint8;
            Wire.Everparse.Raw.decl_default Wire.uint16be;
          ];
        Wire.Everparse.Raw.typedef ~entrypoint:true ~export:true ~output:true
          ~doc:"raw entry" raw_struct;
        Wire.Everparse.Raw.typedef param_struct;
      ]
  in
  let rendered = Wire.Everparse.Raw.to_3d module_ in
  if String.length rendered = 0 then Alcobar.failf "Raw.to_3d empty";
  let rendered_enum = Wire.Everparse.Raw.to_3d ~enum_as_type:true module_ in
  if String.length rendered_enum = 0 then
    Alcobar.failf "Raw.to_3d ~enum_as_type empty";
  let _ = Fmt.str "%a" Wire.Everparse.Raw.pp_module module_ in
  let module_schema =
    Wire.Everparse.Raw.of_module ~name:"ApiModule" ~module_ ~wire_size:1
  in
  check_string "Raw.of_module" "ApiModule" module_schema.Wire.Everparse.name;
  let codec = fst5 (api_access_codec ()) in
  let st = Wire.Everparse.Raw.struct_of_codec codec in
  let _ = Wire.Everparse.project ~mode:`Standalone codec in
  let _ = Fmt.str "%a" Wire.Ascii.pp_struct st in
  let _ = Fmt.str "%a" Wire.Ascii.pp_codec codec in
  ()

let check_everparse_helpers () =
  let len_field, raw_struct = public_raw_struct () in
  check_everparse_schema raw_struct;
  check_raw_struct_metadata raw_struct len_field;
  let p, param_struct = public_param_struct () in
  check_raw_param_metadata param_struct raw_struct;
  check_raw_module_helpers raw_struct p param_struct

let once_case name check =
  let ran = ref false in
  Alcobar.test_case name
    Alcobar.[ Alcobar.const () ]
    (fun () ->
      if not !ran then begin
        ran := true;
        check ()
      end)

(* [write ~mode:`Standalone] is only a merge when it is handed more than one
   schema, and a one-element list never sees a name twice, so the dedup went
   unexercised. A sub-codec packed as a codec of its own and also reached
   through another codec's field is the shape that drives it: the same struct
   projects twice, once carrying the entrypoint marker and the codec's doc and
   once carrying neither, and the merged spec has to collapse them into one
   declaration that keeps both. *)
let merged_write_once ~outdir =
  let shared =
    Wire.Codec.v "FuzzShared" ~doc:"The shared sub-codec." Fun.id
      Wire.Codec.[ Wire.Field.v "v" Wire.uint8 $ Fun.id ]
  in
  let parent =
    Wire.Codec.v "FuzzParent"
      (fun k s -> (k, s))
      Wire.Codec.
        [
          Wire.Field.v "k" Wire.uint8 $ fst;
          Wire.Field.v "s" (Wire.codec shared) $ snd;
        ]
  in
  let project c = Wire.Everparse.project ~mode:`Standalone c in
  let name = "ApiMerged" in
  (* Either order has to give the same spec: which schema was projected first
     decides nothing about what the type is. *)
  List.iter
    (fun schemas ->
      Wire.Everparse.write ~mode:`Standalone ~outdir ~name schemas;
      let path =
        Filename.concat outdir (String.capitalize_ascii name ^ ".3d")
      in
      let out = In_channel.with_open_text path In_channel.input_all in
      let occurrences sub =
        let n = String.length sub and len = String.length out in
        let rec go i acc =
          if i + n > len then acc
          else if String.equal (String.sub out i n) sub then go (i + n) (acc + 1)
          else go (i + 1) acc
        in
        go 0 0
      in
      if occurrences "} FuzzShared;" <> 1 then
        Alcobar.failf "merged spec declares FuzzShared %d times, expected once"
          (occurrences "} FuzzShared;");
      if occurrences "entrypoint\ntypedef struct WireFuzzShared" <> 1 then
        Alcobar.failf "merged spec dropped FuzzShared's entrypoint marker";
      if occurrences "The shared sub-codec." <> 1 then
        Alcobar.failf "merged spec dropped FuzzShared's doc comment")
    [ [ project shared; project parent ]; [ project parent; project shared ] ]

let check_write_helpers_once () =
  let codec = fst5 (api_access_codec ()) in
  let schema = Wire.Everparse.project ~mode:`Ffi codec in
  let doc_schema = Wire.Everparse.project ~mode:`Standalone codec in
  let outdir =
    Filename.concat (Filename.get_temp_dir_name ()) "wire_fuzz_api"
  in
  (try Sys.mkdir outdir 0o700 with Sys_error _ -> ());
  Wire.Everparse.write ~mode:`Ffi ~outdir [ schema ];
  Wire.Everparse.write ~mode:`Standalone ~outdir ~name:"ApiDoc" [ doc_schema ];
  merged_write_once ~outdir;
  Wire.Everparse.Raw.to_3d_file ~enum_as_type:true
    (Filename.concat outdir "ApiRawDirect.3d")
    schema.module_

(* Direct checks for a representative slice of the public [Wire] API -- field /
   bitfield / slice accessors and setters, custom sequences, struct validation,
   metadata accessors, the EverParse projection helpers, and the write helpers --
   complementing the round-trip and projection coverage the registry gives. This
   is not a complete mirror of every public symbol: it targets the surfaces with
   non-trivial behaviour worth asserting directly, while the simple scalar
   round-trips stay in the registry suite. The internal codec helpers
   ([raw_decode]/[raw_encode], [embed_encode]/[embed_decode], [field_readers],
   [wire_size_info], the padded blit helpers) are not part of the public facade
   and are not counted as API coverage; they are only exercised transitively by
   the round-trip suite. *)
let api_cases label =
  [
    Alcobar.test_case (label ^ " accessors")
      Alcobar.
        [
          uint8;
          range ~min:0 8;
          range ~min:0 32;
          bytes_fixed 3;
          uint8;
          range ~min:0 8;
          range ~min:0 32;
          bytes_fixed 3;
        ]
      check_api_accessors;
    Alcobar.test_case (label ^ " custom seq")
      Alcobar.[ uint8; uint8; uint8; uint8 ]
      check_custom_seq;
    Alcobar.test_case
      (label ^ " struct validator")
      Alcobar.[ uint8; bytes ]
      check_struct_validator;
    const_case (label ^ " metadata") check_metadata_helpers;
    const_case (label ^ " everparse metadata") check_everparse_helpers;
    once_case (label ^ " write once") check_write_helpers_once;
  ]
