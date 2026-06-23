(* Tests for c.ml: 3D generation and projection-facing syntax. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let test_bitfields () =
  let f_y = field "y" (bits ~width:10 U32) in
  let f_z = field "z" (bits ~width:16 U32) in
  let bf =
    struct_ "BF"
      [
        field "x" (bits ~width:6 U32);
        field "y"
          ~constraint_:Expr.(field_ref f_y <= int 900)
          (bits ~width:10 U32);
        field "z"
          ~constraint_:Expr.(field_ref f_y + field_ref f_z <= int 60000)
          (bits ~width:16 U32);
      ]
  in
  let m = module_ [ typedef bf ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT32" true (contains ~sub:"UINT32" output);
  Alcotest.(check bool) "contains BF" true (contains ~sub:"BF" output)

let test_enumerations () =
  let m =
    module_
      [
        enum_decl "Enum8"
          [ ("Enum8_1", 0); ("Enum8_2", 1); ("Enum8_3", 2) ]
          uint8;
      ]
  in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains enum" true (contains ~sub:"enum" output);
  Alcotest.(check bool) "contains Enum8_1" true (contains ~sub:"Enum8_1" output)

(* An enum over a big-endian base has no valid 3D enum type (EverParse types the
   constants as the native width and rejects the declaration), so it projects as
   the base scalar with a membership refinement instead of an enum decl. A native
   uint8 enum still projects as a named enum type. *)
let test_enum_big_endian_projection () =
  let be =
    enum "WideCode" [ ("Zero", 0); ("One", 1); ("High", 0xBEEF) ] uint16be
  in
  let out =
    to_3d ~enum_as_type:true
      (module_ [ typedef (struct_ "BeEnum" [ field "v" be ]) ])
  in
  Alcotest.(check bool)
    "no enum decl for BE base" false
    (contains ~sub:"enum WideCode" out);
  Alcotest.(check bool)
    "BE enum field carries membership refinement" true
    (contains ~sub:"48879" out);
  let le = enum "Color" [ ("Red", 1); ("Green", 2) ] uint8 in
  let out_le =
    to_3d ~enum_as_type:true
      (module_ [ typedef (struct_ "LeEnum" [ field "v" le ]) ])
  in
  Alcotest.(check bool)
    "uint8 enum keeps enum decl" true
    (contains ~sub:"enum Color" out_le)

let test_field_dependence () =
  let t_struct = param_struct "t" [ param "a" uint32 ] [ field "x" uint32 ] in
  let f_a = field "a" uint32 in
  let s_struct =
    struct_ "s"
      [ field "a" uint32; field "b" (apply (type_ref "t") [ field_ref f_a ]) ]
  in
  let m = module_ [ typedef t_struct; typedef s_struct ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool) "contains param" true (contains ~sub:"UINT32 a" output)

let test_casetype () =
  let d_casetype =
    casetype_decl "_D"
      [ param "key" uint32 ]
      uint32
      [ decl_case 1 uint16; decl_case 2 uint32 ]
  in
  let m = module_ [ d_casetype ] in
  let output = to_3d m in
  Alcotest.(check bool) "non-empty output" true (String.length output > 0);
  Alcotest.(check bool)
    "contains casetype" true
    (contains ~sub:"casetype" output);
  Alcotest.(check bool)
    "contains switch" true
    (contains ~sub:"switch (key)" output);
  Alcotest.(check bool) "public name is D" true (contains ~sub:"} D;" output)

let test_casetype_enum_tag_labels () =
  (* Switching on an enum tag, EverParse requires each case label to be the enum
     constant name, not the raw integer: [case InteriorIndex:], not [case 2:]. *)
  let kind = enum "PageKind" [ ("LeafIndex", 0); ("InteriorIndex", 2) ] uint8 in
  let d =
    casetype_decl "_PageBody"
      [ param "tag" kind ]
      kind
      [ decl_case 0 uint32; decl_case 2 uint16 ]
  in
  let output = to_3d (module_ [ d ]) in
  Alcotest.(check bool)
    "low case uses the enum constant name" true
    (contains ~sub:"case LeafIndex:" output);
  Alcotest.(check bool)
    "high case uses the enum constant name" true
    (contains ~sub:"case InteriorIndex:" output);
  Alcotest.(check bool)
    "no raw-integer case label" false
    (contains ~sub:"case 2:" output)

let test_pretty_print () =
  let simple =
    struct_ "Simple" [ field "a" uint8; field "b" uint16be; field "c" uint32 ]
  in
  let m = module_ [ typedef simple ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains typedef" true (String.length output > 0);
  Alcotest.(check bool) "contains UINT8" true (contains ~sub:"UINT8" output);
  Alcotest.(check bool)
    "contains UINT16BE" true
    (contains ~sub:"UINT16BE" output)

let test_3d_uint63_projects_to_uint64 () =
  (* 3D has no 63-bit type. uint63/uint63be must project to the 8-byte UINT64,
     not an invalid UINT63 that EverParse rejects (leaving the codec with no
     verified validator at all). *)
  let c =
    Codec.v "U63"
      (fun a b -> (a, b))
      Codec.
        [
          (Field.v "a" uint63 $ fun (a, _) -> a);
          (Field.v "b" uint63be $ fun (_, b) -> b);
        ]
  in
  let output = to_3d (module_ [ typedef (Everparse.struct_of_codec c) ]) in
  Alcotest.(check bool)
    "no invalid UINT63" false
    (contains ~sub:"UINT63" output);
  Alcotest.(check bool)
    "uint63 projects to UINT64" true
    (contains ~sub:"UINT64 a" output);
  Alcotest.(check bool)
    "uint63be projects to UINT64BE" true
    (contains ~sub:"UINT64BE b" output)

let test_3d_signed_float_setters () =
  (* Signed-int and float fields each route to a width-typed setter, not the
     generic SetBytes (whose single value type cannot hold two scalar fields of
     different widths), so a record mixing widths (e.g. float32 then float64)
     projects to a verified EverParse validator. *)
  let c =
    Codec.v "Mix"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (Field.v "a" float32be $ fun (a, _, _, _) -> a);
          (Field.v "b" float64be $ fun (_, b, _, _) -> b);
          (Field.v "c" int8 $ fun (_, _, c, _) -> c);
          (Field.v "d" int32be $ fun (_, _, _, d) -> d);
        ]
  in
  let output = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "float32be routes to SetU32BE" true
    (contains ~sub:"SetU32BE" output);
  Alcotest.(check bool)
    "float64be routes to SetU64BE" true
    (contains ~sub:"SetU64BE" output);
  Alcotest.(check bool)
    "int8 routes to SetU8" true
    (contains ~sub:"SetU8" output);
  Alcotest.(check bool)
    "no scalar falls through to SetBytes" false
    (contains ~sub:"SetBytes" output)

let test_3d_arith_constraint_widens () =
  (* A constraint adding two narrow unsigned fields overflows the field width in
     3D, which EverParse refuses to verify, while the OCaml decoder computes the
     sum in its wide native int. The projection widens each [Add] operand to
     UINT64 so the 3D sum cannot overflow and matches the decoder. *)
  let f_a = Field.v "a" uint8 in
  let f_b =
    Field.v "b" uint8 ~self_constraint:(fun b ->
        Expr.(Field.int f_a + b <= int 10))
  in
  let c = Codec.v "Arith" (fun a b -> (a, b)) Codec.[ f_a $ fst; f_b $ snd ] in
  let output = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "both Add operands widened to UINT64" true
    (contains ~sub:"((UINT64) a) + ((UINT64) b)" output);
  Alcotest.(check bool)
    "no un-widened narrow sum" false
    (contains ~sub:"(a + b)" output)

let test_3d_array_enum_element_decl () =
  (* An enum used as an array element still needs its declaration emitted. The
     decl collection only looked through Map/Where wrappers, so an enum inside an
     array left the schema referencing an undeclared type, which EverParse
     rejects. *)
  let e = enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ] uint8 in
  let c =
    Codec.v "ArrE"
      (fun v -> v)
      Codec.[ (Field.v "v" (array ~len:(int 4) e) $ fun v -> v) ]
  in
  let output = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "enum declaration emitted" true
    (contains ~sub:"enum Color" output);
  Alcotest.(check bool)
    "array element references the enum" true
    (contains ~sub:"Color v[" output)

let test_3d_array_open_enum_element () =
  (* An open enum admits any value, so as an array element it must project as its
     bare base, not the named enum type. Rendering it as the enum type makes the
     generated C validator reject codes outside the named set, while the OCaml
     decoder (an open enum checks no membership) accepts them: a soundness
     divergence the differential fuzzer flagged. *)
  let e = enum_open "Code" [ ("A", 1); ("B", 2); ("C", 3) ] uint8 in
  let c =
    Codec.v "ArrOpenE"
      (fun v -> v)
      Codec.[ (Field.v "v" (array ~len:(int 4) e) $ fun v -> v) ]
  in
  let output = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "open-enum array element renders as its base scalar" true
    (contains ~sub:"UINT8 v[" output);
  Alcotest.(check bool)
    "open-enum array element does not enforce membership via the enum type"
    false
    (contains ~sub:"Code v[" output)

let test_3d_enum_membership () =
  (* An enum field must enforce membership in the verified C, on a scalar field
     and inside a sub-codec, or the validator accepts values that Codec.decode
     rejects with Invalid_enum. *)
  let e = enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ] uint8 in
  let scalar =
    Codec.v "ES" (fun v -> v) Codec.[ (Field.v "v" e $ fun v -> v) ]
  in
  Alcotest.(check bool)
    "scalar enum bounds its value" true
    (contains ~sub:"(v == 1)" (to_3d (Everparse.schema scalar).module_));
  let rec_ =
    Codec.v "Rec"
      (fun a b -> (a, b))
      Codec.
        [
          (Field.v "e" e $ fun (a, _) -> a);
          (Field.v "u" uint64be $ fun (_, b) -> b);
        ]
  in
  let arr =
    Codec.v "ArrRec"
      (fun v -> v)
      Codec.[ (Field.v "v" (array ~len:(int 4) (codec rec_)) $ fun v -> v) ]
  in
  Alcotest.(check bool)
    "enum nested in a sub-codec bounds its value" true
    (contains ~sub:"(e == 1)" (to_3d (Everparse.schema arr).module_))

(* A closed enum used as a list element or optional inner must enforce membership
   in the verified C too. As a wide / big-endian array element, a repeat element
   of any width, or an optional inner it used to render as the bare base, so the
   generated C accepted codes Codec.decode rejects with Invalid_enum. Each now
   projects through a synthesised refined-element struct [_EnumElt_<Name>] that
   carries the membership refinement. *)
let test_3d_enum_membership_list_elements () =
  let e16 = enum "Wide" [ ("Lo", 1); ("Hi", 0xBEEF) ] uint16 in
  let arr =
    Codec.v "ArrWideEnum"
      (fun v -> v)
      Codec.[ (Field.v "v" (array ~len:(int 3) e16) $ fun v -> v) ]
  in
  let out_arr = to_3d (Everparse.schema arr).module_ in
  Alcotest.(check bool)
    "wide-enum array element goes through a refined struct" true
    (contains ~sub:"_EnumElt_Wide" out_arr);
  Alcotest.(check bool)
    "wide-enum array element carries membership" true
    (contains ~sub:"48879" out_arr);
  (* A repeat element loses the 1-byte enum-type rendering at every width, so even
     a uint8 enum repeat must carry membership through the refined struct. *)
  let e8 = enum "Small" [ ("A", 1); ("B", 2) ] uint8 in
  let rep =
    Codec.v "RepEnum"
      (fun v -> v)
      Codec.[ (Field.repeat "v" ~size:(int 3) e8 $ fun v -> v) ]
  in
  let out_rep = to_3d (Everparse.schema rep).module_ in
  Alcotest.(check bool)
    "repeat enum element goes through a refined struct" true
    (contains ~sub:"_EnumElt_Small" out_rep);
  Alcotest.(check bool)
    "repeat enum membership refinement present" true
    (contains ~sub:"(v == 1)" out_rep);
  (* An optional closed-enum inner must enforce the present value's membership. *)
  let f_g = Field.v "g" uint8 in
  let opt =
    Codec.v "OptEnum"
      (fun a b -> (a, b))
      Codec.
        [
          (f_g $ fun (a, _) -> a);
          ( Field.optional "v" ~present:Expr.(Field.ref f_g = int 1) e16
          $ fun (_, b) -> b );
        ]
  in
  let out_opt = to_3d (Everparse.schema opt).module_ in
  Alcotest.(check bool)
    "optional enum inner goes through a refined struct" true
    (contains ~sub:"_EnumElt_Wide" out_opt)

let test_enum_open_accepts_and_no_refinement () =
  (* An open enum names known values but accepts any: decode does not reject an
     unlisted value, and the projection emits no membership refinement (the
     field is its base scalar) yet still declares the named codes, so an open
     value set is documented without being wrongly rejected. *)
  let e = enum_open "Kind" [ ("A", 0); ("B", 2) ] uint8 in
  let c = Codec.v "OpenK" (fun v -> v) Codec.[ (Field.v "k" e $ fun v -> v) ] in
  let buf = Bytes.make 1 '\x05' in
  (match Codec.decode c buf 0 with
  | Ok v -> Alcotest.(check int) "open accepts unlisted value" 5 v
  | Error _ -> Alcotest.fail "open enum wrongly rejected an unlisted value");
  let out = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "no membership refinement" false (contains ~sub:"== 0" out);
  Alcotest.(check bool)
    "projects as base scalar" true
    (contains ~sub:"UINT8 k" out);
  Alcotest.(check bool)
    "names survive as a 3D enum declaration" true
    (contains ~sub:"enum Kind" out)

let test_doc_drops_ffi_scaffolding () =
  (* The doc projection keeps the structure but not the FFI callbacks the
     codegen schema carries. *)
  let c =
    Codec.v "Pkt" (fun v -> v) Codec.[ (Field.v "v" uint8 $ fun v -> v) ]
  in
  let schema = to_3d (Everparse.schema c).module_ in
  let doc = to_3d ~enum_as_type:true (Everparse.doc c).module_ in
  Alcotest.(check bool)
    "schema has WireCtx" true
    (contains ~sub:"WireCtx" schema);
  Alcotest.(check bool) "doc has no WireCtx" false (contains ~sub:"WireCtx" doc);
  Alcotest.(check bool) "doc has no WireSet" false (contains ~sub:"WireSet" doc);
  Alcotest.(check bool) "doc keeps the field" true (contains ~sub:"UINT8 v" doc)

let test_doc_enum_as_type () =
  (* An enum field renders as its named 3D enum type, so EverParse enforces
     membership through the type rather than an explicit refinement. *)
  let e = enum "Color" [ ("Red", 0); ("Green", 1); ("Blue", 2) ] uint8 in
  let c = Codec.v "Pix" (fun v -> v) Codec.[ (Field.v "Hue" e $ fun v -> v) ] in
  let doc = to_3d ~enum_as_type:true (Everparse.doc c).module_ in
  Alcotest.(check bool)
    "declares the enum" true
    (contains ~sub:"enum Color" doc);
  Alcotest.(check bool)
    "types the field as the enum" true
    (contains ~sub:"Color Hue" doc);
  Alcotest.(check bool)
    "drops the membership refinement" false (contains ~sub:"== 0" doc);
  Alcotest.(check bool)
    "codegen schema keeps membership" true
    (contains ~sub:"== 0" (to_3d (Everparse.schema c).module_))

let test_doc_codec_citation () =
  (* [Codec.v ~doc] renders as a [/*++ ... --*/] comment on the typedef, so a
     spec can cite the RFC (or other source) it comes from. *)
  let c =
    Codec.v "Pkt" ~doc:"RFC 9999 section 1"
      (fun v -> v)
      Codec.[ (Field.v "v" uint8 $ fun v -> v) ]
  in
  Alcotest.(check (option string))
    "codec exposes its doc" (Some "RFC 9999 section 1") (Codec.doc c);
  let out = to_3d ~enum_as_type:true (Everparse.doc c).module_ in
  Alcotest.(check bool)
    "typedef carries the citation comment" true
    (contains ~sub:"/*++ RFC 9999 section 1 --*/" out)

let test_doc_comment_wraps_at_80 () =
  (* A long [?doc] note must not render as one comment line past 80 columns: the
     generated .3d doubles as readable protocol documentation. The body is
     word-wrapped across several comment lines. *)
  let long =
    "RFC 9999 section 4.2.1 the version field carries the protocol revision \
     and must equal the value negotiated during the handshake or the peer \
     rejects the frame as malformed and resets the connection immediately"
  in
  let c =
    Codec.v "Wrapped"
      (fun v -> v)
      Codec.[ (Field.v "v" uint8 ~doc:long $ fun v -> v) ]
  in
  let out = to_3d (Everparse.schema c).module_ in
  let longest =
    String.split_on_char '\n' out
    |> List.fold_left (fun m l -> max m (String.length l)) 0
  in
  Alcotest.(check bool)
    (Fmt.str "no generated line exceeds 80 columns (longest=%d)" longest)
    true (longest <= 80);
  Alcotest.(check bool)
    "the long doc is still a comment" true
    (contains ~sub:"/* RFC 9999 section 4.2.1" out)

let test_doc_merge_dedup () =
  (* write_doc unions a family into one module, emitting a shared type once. *)
  let e = enum "Shared" [ ("A", 0); ("B", 1) ] uint8 in
  let c1 = Codec.v "One" (fun v -> v) Codec.[ (Field.v "x" e $ fun v -> v) ] in
  let c2 = Codec.v "Two" (fun v -> v) Codec.[ (Field.v "y" e $ fun v -> v) ] in
  let dir = Filename.get_temp_dir_name () in
  let name = "wire_doc_merge_test" in
  Everparse.write_doc ~outdir:dir ~name [ Everparse.doc c1; Everparse.doc c2 ];
  let path = Filename.concat dir (String.capitalize_ascii name ^ ".3d") in
  let content = In_channel.with_open_text path In_channel.input_all in
  Sys.remove path;
  let count sub = List.length (Re.all (Re.compile (Re.str sub)) content) in
  Alcotest.(check int) "shared enum declared once" 1 (count "enum Shared");
  Alcotest.(check bool)
    "first struct present" true
    (contains ~sub:"One" content);
  Alcotest.(check bool)
    "second struct present" true
    (contains ~sub:"Two" content)

let test_doc_field_citation () =
  (* [Field.v ~doc] renders as a plain [/* ... */] comment above the field --
     3d.exe rejects [/*++ --*/] at field position, so the per-field note uses
     the plain comment form. *)
  let f = Field.v "Length" ~doc:"RFC 9999 section 4.2" uint16be in
  Alcotest.(check (option string))
    "field exposes its doc" (Some "RFC 9999 section 4.2") (Field.doc f);
  let c = Codec.v "Pkt" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  let out = to_3d ~enum_as_type:true (Everparse.doc c).module_ in
  Alcotest.(check bool)
    "field carries the citation comment" true
    (contains ~sub:"/* RFC 9999 section 4.2 */" out)

let test_doc_bit_order_matches_schema () =
  (* Doc-mode emits a validator with no FFI, so its bitfield layout is never
     run through the differential C tests that check the schema (FFI)
     projection's bit order against [Codec.get]. Both projections must apply
     the same bit reordering; assert their bitfield sequence is identical so
     doc-mode's order stays pinned to the order those tests already cover.
     A,B,C share one [U8] word whose native order is [Lsb_first], so the
     default [Msb_first] declaration is reversed in the 3D -- a doc path that
     skipped the reorder would diverge here. *)
  let c =
    Codec.v "BitOrd"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (Field.v "A" (bits ~width:3 U8) $ fun (a, _, _, _) -> a);
          (Field.v "B" (bits ~width:1 U8) $ fun (_, b, _, _) -> b);
          (Field.v "C" (bits ~width:4 U8) $ fun (_, _, c, _) -> c);
          (Field.v "D" uint8 $ fun (_, _, _, d) -> d);
        ]
  in
  let bit_order_of schema =
    match Everparse.entrypoint_struct schema with
    | None -> Alcotest.fail "schema has no entrypoint struct"
    | Some s ->
        List.map
          (fun (name, is_bitfield, _) -> (name, is_bitfield))
          (Everparse.field_action_forms s)
  in
  Alcotest.(check (list (pair (option string) bool)))
    "doc reorders bitfields exactly as the schema projection does"
    (bit_order_of (Everparse.schema c))
    (bit_order_of (Everparse.doc c))

let test_3d_nested_byte_array_where () =
  (* A byte_array_where inside a nested region needs its synthesised refined-byte
     typedef emitted, before the wrapper that references it, or the schema names
     an undeclared type and EverParse rejects it. *)
  let inner =
    byte_array_where ~size:(int 4) ~per_byte:(fun b -> Expr.(b < int 10))
  in
  let c =
    Codec.v "NBW"
      (fun v -> v)
      Codec.[ (Field.v "n" (nested ~size:(int 4) inner) $ fun v -> v) ]
  in
  let output = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "refined-byte typedef is defined, not just referenced" true
    (contains ~sub:"} _RefByte_" output)

let test_3d_static_optional_transparent () =
  (* A statically-present optional projects as its inner: a byte-span or
     composite inner keeps its offset setter (passing the field value made
     EverParse reject the schema), and a byte_array_where inner still emits its
     refined typedef. *)
  let opt inner =
    Codec.v "Opt"
      (fun v -> v)
      Codec.[ (Field.optional "o" ~present:Expr.true_ inner $ fun v -> v) ]
  in
  let out_ba =
    to_3d (Everparse.schema (opt (byte_array ~size:(int 4)))).module_
  in
  Alcotest.(check bool)
    "byte-span optional uses an offset setter" true
    (contains ~sub:"(UINT32) 0, (UINT32) 0" out_ba);
  let baw =
    byte_array_where ~size:(int 4) ~per_byte:(fun b -> Expr.(b < int 10))
  in
  let out_baw = to_3d (Everparse.schema (opt baw)).module_ in
  Alcotest.(check bool)
    "byte_array_where optional emits its refined typedef" true
    (contains ~sub:"} _RefByte_" out_baw)

let test_3d_absent_optional_projects_to_unit () =
  (* A statically-absent optional contributes no bytes, so it projects as a
     [unit] field and the schema carries no zero-length byte-size suffix (which
     EverParse would refuse to name). *)
  let c inner =
    Codec.v "AbsentOpt"
      (fun v -> v)
      Codec.[ (Field.optional "o" ~present:Expr.false_ inner $ fun v -> v) ]
  in
  let out = to_3d (Everparse.schema (c uint8)).module_ in
  Alcotest.(check bool)
    "absent optional projects as unit" true
    (contains ~sub:"unit o" out);
  Alcotest.(check bool)
    "absent optional has no zero-length byte-size suffix" false
    (contains ~sub:"[:byte-size 0]" out)

let test_3d_on_act_drops_bool_return () =
  (* An [on_act] body that ends in [return_bool] projects to an [:act] block,
     which is unit in 3D: the trailing return is dropped (a no-op success in
     OCaml) and the auto setter runs last, so EverParse does not see a
     Bool-returning :act. *)
  let out = Param.output "out" uint8 in
  let f_v = Field.v "v" uint8 in
  let f =
    Field.v "v" uint8
      ~action:
        (Action.on_act
           [ Action.assign out (Field.ref f_v); Action.return_bool Expr.true_ ])
  in
  let codec = Codec.v "ActOnAct" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  let out3d = to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool) "emits an :act block" true (contains ~sub:":act" out3d);
  Alcotest.(check bool)
    "the :act block has no bool return" false
    (contains ~sub:"return" out3d)

let test_3d_on_success_conditional_return () =
  (* An [on_success] ending in a conditional [return] (a bare [if]) projects to
     a terminal [if]/[else] both returning Bool (3D rejects a bare [if] with a
     Bool return, and forbids statements after it). The setter is moved before
     the [if] and an [else { return true }] is synthesised. *)
  let out = Param.output "out" uint8 in
  let f_v = Field.v "v" uint8 in
  let f =
    Field.v "v" uint8
      ~action:
        (Action.on_success
           [
             Action.assign out (Field.ref f_v);
             Action.if_ Expr.true_ [ Action.return_bool Expr.true_ ] None;
           ])
  in
  let codec = Codec.v "ActIf" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  let out3d = to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool) "if is present" true (contains ~sub:"if (" out3d);
  Alcotest.(check bool)
    "an else branch is synthesised" true
    (contains ~sub:"else {" out3d);
  Alcotest.(check bool)
    "the field setter runs before the if" true
    (contains ~sub:"ActIfSetU8" out3d)

let projects_or_raises codec =
  match to_3d (Everparse.schema codec).module_ with
  | (_ : string) -> false
  | exception Invalid_argument _ -> true

let test_3d_negative_literal_rejected () =
  (* 3D has no negative integer literals, so a projected expression containing
     one is rejected at projection with a clear error, not emitted as C
     EverParse cannot parse. *)
  let f = Field.v "a" uint8 in
  let codec =
    Codec.v "NegLit"
      ~where:Expr.(Field.ref f <> int (-1))
      (fun a -> a)
      Codec.[ (f $ fun a -> a) ]
  in
  Alcotest.(check bool)
    "negative literal rejected by projection" true (projects_or_raises codec)

let test_3d_field_sub_mul_rejected () =
  (* A [Sub] / [Mul] over a field in a constraint has no sound narrow-width 3D
     projection (3d.exe refuses to verify the under/overflow), so it is rejected
     at projection. A constant [Sub] / [Mul] folds and an additive field
     constraint still projects. *)
  let f = Field.v "a" uint8 in
  let mul =
    Codec.v "MulC"
      (fun a b -> (a, b))
      Codec.
        [
          f $ fst;
          Field.v "b" uint8 ~self_constraint:(fun b ->
              Expr.(Field.int f * b = int 0))
          $ snd;
        ]
  in
  let sub =
    Codec.v "SubC"
      (fun a b -> (a, b))
      Codec.
        [
          f $ fst;
          Field.v "b" uint8 ~self_constraint:(fun b ->
              Expr.(Field.int f - b >= int 5))
          $ snd;
        ]
  in
  Alcotest.(check bool)
    "field multiplication rejected" true (projects_or_raises mul);
  Alcotest.(check bool)
    "field subtraction rejected" true (projects_or_raises sub);
  (* A constant arithmetic bound still projects. *)
  let constc =
    Codec.v "ConstC"
      (fun a -> a)
      Codec.
        [
          Field.v "a" uint8 ~self_constraint:(fun a ->
              Expr.(a < int 10 * int 5))
          $ Fun.id;
        ]
  in
  Alcotest.(check bool)
    "constant arithmetic still projects" false
    (projects_or_raises constc)

let test_signed_constraint_twos_complement () =
  (* A signed field projects to an unsigned UINT, so an ordering constraint must
     be rewritten to its two's-complement form ([x < 100] over [int8] becomes
     [(x ^ 128) < 228]) or the C validator compares the raw byte and diverges
     from the OCaml decoder (byte 200 = signed -56). *)
  let f =
    Field.v "x" int8 ~self_constraint:(fun self -> Expr.(self < int 100))
  in
  let codec = Codec.v "Signed" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  let out = to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool)
    "two's-complement refinement emitted" true
    (contains ~sub:"(x ^ 128) < 228" out);
  Alcotest.(check bool)
    "raw unsigned comparison not emitted" false
    (contains ~sub:"(x < 100)" out)

let test_signed_equality_twos_complement () =
  (* A signed field's equality refinement must compare against the
     two's-complement byte the unsigned projected field reads, and fold to a
     constant when the target is outside the signed range, or the C validator
     compares the raw byte and diverges from the OCaml decoder. *)
  let proj name k =
    to_3d
      (Everparse.schema
         (Codec.v name
            (fun v -> v)
            Codec.[ (Field.v "v" int8 ~self_constraint:k $ fun v -> v) ]))
        .module_
  in
  let eq_oor = proj "EqOOR" (fun s -> Expr.(s = int 200)) in
  Alcotest.(check bool)
    "out-of-range equality folds to false" true
    (contains ~sub:"v { false }" eq_oor);
  Alcotest.(check bool)
    "out-of-range equality drops the raw constant" false
    (contains ~sub:"200" eq_oor);
  let eq_inr = proj "EqInR" (fun s -> Expr.(s = int (-1))) in
  Alcotest.(check bool)
    "in-range equality uses the two's-complement byte" true
    (contains ~sub:"(v == 255)" eq_inr);
  let ne_oor = proj "NeOOR" (fun s -> Expr.(s <> int 200)) in
  Alcotest.(check bool)
    "out-of-range inequality folds to true" true
    (contains ~sub:"v { true }" ne_oor)

let test_float_ordering_rejected () =
  (* IEEE bit patterns do not order as unsigned, so a float field ordering
     constraint has no faithful 3D projection and is rejected at projection. *)
  let f =
    Field.v "f" float32 ~self_constraint:(fun self -> Expr.(self < int 100))
  in
  let codec = Codec.v "Flt" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  Alcotest.(check bool)
    "float ordering rejected by projection" true (projects_or_raises codec)

let test_if_then_else_projects () =
  (* Expr.if_then_else projects to a 3D conditional, e.g. a size where a 0 field
     means a maximum: [byte-size (... ? 65536 : N)]. *)
  let f_n = Field.v "N" uint16be in
  let size =
    Expr.if_then_else Expr.(Field.ref f_n = int 0) (int 65536) (Field.ref f_n)
  in
  let c =
    Codec.v "Pg"
      (fun n d -> (n, d))
      Codec.
        [
          (f_n $ fun (n, _) -> n);
          (Field.v "Data" (byte_array ~size) $ fun (_, d) -> d);
        ]
  in
  let out = to_3d (Everparse.schema c).module_ in
  Alcotest.(check bool)
    "renders the conditional" true
    (contains ~sub:"? 65536 : N" out)

let test_3d_int64_literal_uses_unsigned_decimal () =
  let f =
    Field.v "a" uint64be ~self_int64:(fun self ->
        Expr.(self > int64 Int64.min_int))
  in
  let codec = Codec.v "I64Lit" Fun.id Codec.[ f $ Fun.id ] in
  let out3d = to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool)
    "high-bit int64 literal prints unsigned" true
    (contains ~sub:"9223372036854775808uL" out3d)

let test_3d_field_pos_rejected () =
  (* EverParse has no [field_pos] keyword, so a projected expression using it is
     rejected at projection, not emitted as an undefined identifier. *)
  let f = Field.v "a" uint8 in
  let codec =
    Codec.v "FieldPos"
      ~where:Expr.(field_pos = int 0)
      (fun a -> a)
      Codec.[ (f $ fun a -> a) ]
  in
  Alcotest.(check bool)
    "field_pos rejected by projection" true (projects_or_raises codec)

(* -- Codec definitions for 3D extraction tests --
   The shared codecs ([inner], [outer], [l0]/[l1]/[l2], [opt_record],
   [container]/[repeat_codec], [packet]/[packet_codec]) live in
   {!Test_helpers}. *)

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

(* -- 3D extraction tests -- *)

let test_3d_codec_embed () =
  (* Codec embed: field type should reference the sub-codec's struct name *)
  let s_inner = Everparse.struct_of_codec inner_codec in
  let s_outer = Everparse.struct_of_codec outer_codec in
  let m = module_ [ typedef s_inner; typedef s_outer ] in
  let output = to_3d m in
  (* Inner struct must be defined *)
  Alcotest.(check bool)
    "inner struct defined" true
    (contains ~sub:"typedef struct _Inner" output);
  (* Outer struct references Inner by name *)
  Alcotest.(check bool)
    "outer references Inner" true
    (contains ~sub:"Inner Inner" output);
  Alcotest.(check bool)
    "contains Header field" true
    (contains ~sub:"Header" output);
  Alcotest.(check bool)
    "contains Trailer field" true
    (contains ~sub:"Trailer" output)

let test_3d_codec_nested () =
  (* Two-level nesting: L0 -> L1 -> L2, each should reference by name *)
  let s0 = Everparse.struct_of_codec l0_codec in
  let m = module_ [ typedef s0 ] in
  let output = to_3d m in
  (* L0's struct should reference L1 by name *)
  Alcotest.(check bool) "contains L1 ref" true (contains ~sub:"L1" output);
  Alcotest.(check bool) "contains Z field" true (contains ~sub:"Z" output)

let test_3d_optional_present () =
  (* Optional present: field should appear as normal UINT16BE *)
  let s = Everparse.struct_of_codec opt_codec_present in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains UINT16" true (contains ~sub:"UINT16" output);
  Alcotest.(check bool) "contains Payload" true (contains ~sub:"Payload" output);
  Alcotest.(check bool) "contains Trail" true (contains ~sub:"Trail" output);
  (* Must not contain invalid 3D syntax *)
  Alcotest.(check bool)
    "no optional() syntax" false
    (contains ~sub:"optional(" output)

let test_3d_optional_absent () =
  (* Optional absent: zero-length field *)
  let s = Everparse.struct_of_codec opt_codec_absent in
  let m = module_ [ typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains Trail" true (contains ~sub:"Trail" output);
  Alcotest.(check bool)
    "no optional() syntax" false
    (contains ~sub:"optional(" output)

let test_3d_repeat () =
  (* Repeat: should render as variable-length array with :byte-size *)
  let s_inner = Everparse.struct_of_codec inner_codec in
  let s = Everparse.struct_of_codec repeat_codec in
  let m = module_ [ typedef s_inner; typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Length" true (contains ~sub:"Length" output);
  Alcotest.(check bool) "contains Items" true (contains ~sub:"Items" output);
  (* Repeat should render as :byte-size array, referencing Inner type *)
  Alcotest.(check bool)
    "contains :byte-size" true
    (contains ~sub:":byte-size" output);
  Alcotest.(check bool)
    "references Inner elem type" true
    (contains ~sub:"Inner" output);
  (* Must not contain invalid 3D syntax *)
  Alcotest.(check bool)
    "no repeat() syntax" false
    (contains ~sub:"repeat(" output)

let test_3d_tm_like () =
  (* Full TM-like composition: all nested types should project to 3D *)
  let c = tm_like_codec ~ocf:true ~fecf:true in
  let s_pkt = Everparse.struct_of_codec packet_codec in
  let s = Everparse.struct_of_codec c in
  let m = module_ [ typedef s_pkt; typedef s ] in
  let output = to_3d m in
  Alcotest.(check bool) "contains Hdr" true (contains ~sub:"Hdr" output);
  Alcotest.(check bool) "contains DataLen" true (contains ~sub:"DataLen" output);
  Alcotest.(check bool) "contains Packets" true (contains ~sub:"Packets" output);
  Alcotest.(check bool) "contains OCF" true (contains ~sub:"OCF" output);
  Alcotest.(check bool) "contains FECF" true (contains ~sub:"FECF" output);
  (* Sub-codec should be referenced by name *)
  Alcotest.(check bool)
    "contains Packet typedef" true
    (contains ~sub:"typedef struct _Packet" output);
  (* Packet should be referenced in the TmLike struct *)
  Alcotest.(check bool)
    "Packet type referenced" true
    (contains ~sub:"Packet" output)

(* -- Bit-order projection tests --

   Non-native (base, bit_order) combinations should still emit a valid 3D
   struct by reversing declaration order within the bit group and, if the
   widths don't fill the word, prepending anonymous padding. These tests
   lock in that the reorder actually happens in the emitted 3D text. *)

(* Return the byte offset of the first occurrence of [sub] in [s],
   or -1 if not found. Used to assert relative field ordering. *)
let index_of ~sub s =
  let re = Re.compile (Re.str sub) in
  match Re.exec_opt re s with Some g -> Re.Group.start g 0 | None -> -1

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
  let schema = Everparse.schema codec in
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
  let schema = Everparse.schema codec in
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
  let schema = Wire.Everparse.schema codec in
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
  let schema = Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  let ix = index_of ~sub:"UINT32BE x" s in
  let iy = index_of ~sub:"UINT32BE y" s in
  Alcotest.(check bool) "x found" true (ix >= 0);
  Alcotest.(check bool) "y found" true (iy >= 0);
  Alcotest.(check bool) "x before y (no reorder)" true (ix < iy);
  Alcotest.(check bool) "no padding" false (contains ~sub:"_anon_" s)

(* -- Variable-size schema projection -- *)

type dep_frame = { frame_length : int; data : string }

let f_frame_length = Field.v "FrameLength" uint16be
let header_size = 2

let dep_frame_codec =
  Codec.v "DepFrame"
    (fun frame_length data -> { frame_length; data })
    Codec.
      [
        (f_frame_length $ fun r -> r.frame_length);
        ( Field.v "Data"
            (byte_array ~size:Expr.(Field.ref f_frame_length - int header_size))
        $ fun r -> r.data );
      ]

let test_3d_dep_size_schema () =
  let schema = Everparse.schema dep_frame_codec in
  Alcotest.(check bool) "variable wire_size" true (schema.wire_size = None);
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "contains FrameLength" true
    (contains ~sub:"FrameLength" s);
  Alcotest.(check bool) "contains Data" true (contains ~sub:"Data" s);
  Alcotest.(check bool)
    "contains byte-size expr" true
    (contains ~sub:":byte-size (FrameLength - 2)" s)

let test_3d_dep_size_roundtrip () =
  let original = { frame_length = 7; data = "HELLO" } in
  let buf = Bytes.create 7 in
  Codec.encode dep_frame_codec original buf 0;
  let decoded = Codec.decode dep_frame_codec buf 0 in
  match decoded with
  | Ok v ->
      Alcotest.(check int) "frame_length" 7 v.frame_length;
      Alcotest.(check string) "data" "HELLO" v.data
  | Error e -> Alcotest.failf "%a" pp_parse_error e

type param_frame = { pf_data : string }

let p_len = Param.input "len" uint16be

let param_frame_codec =
  Codec.v "ParamFrame"
    (fun data -> { pf_data = data })
    Codec.
      [
        ( Field.v "Data" (byte_array ~size:(Param.expr p_len)) $ fun r ->
          r.pf_data );
      ]

let test_3d_param_in_size () =
  (* A byte_array whose size is driven by a formal parameter must thread
     the parameter into the 3D typedef signature. *)
  let schema = Everparse.schema param_frame_codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "typedef carries len param" true
    (contains ~sub:"UINT16BE len" s);
  Alcotest.(check bool) "size uses len" true (contains ~sub:":byte-size len" s)

(* A codec embedding a parametric sub-codec must surface the sub's param as its
   own formal and apply it at the use site, else 3D rejects the reference. *)
let test_3d_param_embed () =
  let lim = Param.input "lim" uint8 in
  let sub =
    Codec.v "PSub"
      ~where:Expr.(Field.ref (Field.v "v" uint8) <= Param.expr lim)
      (fun v -> v)
      Codec.[ Field.v "v" uint8 $ Fun.id ]
  in
  let outer =
    Codec.v "PEmbed"
      (fun s -> s)
      Codec.[ Field.v "s" (Wire.codec sub) $ Fun.id ]
  in
  let s = Wire.Everparse.Raw.to_3d (Everparse.schema outer).module_ in
  Alcotest.(check bool)
    "sub typedef carries the formal" true
    (contains ~sub:"_PSub(UINT8 lim)" s);
  Alcotest.(check bool)
    "use site applies the formal" true
    (contains ~sub:"PSub(lim) s" s)

(* A [nested] region over a composite inner (an array) projects through a
   synthesised wrapper struct so the single-element-array element is a named
   type, not a malformed inline array. *)
let test_3d_nested_over_array () =
  let codec =
    Codec.v "NAEmbed"
      (fun xs -> xs)
      Codec.
        [
          Field.v "xs" (nested ~size:(int 16) (array ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let s = Wire.Everparse.Raw.to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool)
    "wrapper struct holds the array" true
    (contains ~sub:"UINT64BE v[:byte-size" s);
  Alcotest.(check bool)
    "use site is a single-element-array of the named wrapper" true
    (contains ~sub:"xs[:byte-size-single-element-array 16]" s)

(* -- Reserved word escaping -- *)

let test_reserved_word_escaping () =
  let f_type = field "type" uint8 in
  let f_case = field "case" uint16be in
  let s =
    struct_ "Reserved"
      [ field "type" uint8; field "case" uint16be; field "value" uint32 ]
  in
  let m =
    module_
      [
        typedef ~entrypoint:true
          (param_struct "Reserved"
             [ param "return" uint8 ]
             [
               field "type" uint8;
               field "case"
                 ~constraint_:
                   Expr.(field_ref f_type + field_ref f_case <= int 10)
                 uint16be;
             ]);
      ]
  in
  ignore s;
  let output = to_3d m in
  Alcotest.(check bool) "type escaped" true (contains ~sub:"UINT8 type_" output);
  Alcotest.(check bool)
    "case escaped" true
    (contains ~sub:"UINT16BE case_" output);
  Alcotest.(check bool)
    "return escaped in param" true
    (contains ~sub:"UINT8 return_" output);
  Alcotest.(check bool)
    "type_ in constraint" true
    (contains ~sub:"type_" output);
  Alcotest.(check bool)
    "no bare reserved word as field" false
    (contains ~sub:"UINT8 type;" output)

let test_3d_byte_array_where () =
  (* Synthesises a refinement struct so 3D can apply the per-byte constraint
     to each element of the byte-size array. *)
  let f_len = Field.v "Length" uint16be in
  let f_data =
    Field.v "Data"
      (byte_array_where ~size:(Field.ref f_len)
         ~per_byte:Expr.(fun b -> b >= int 0x20 && b <= int 0x7e))
  in
  let codec =
    Codec.v "Printable"
      (fun len data -> (len, data))
      Codec.[ f_len $ fst; f_data $ snd ]
  in
  let schema = Wire.Everparse.schema codec in
  let s = Wire.Everparse.Raw.to_3d schema.module_ in
  Alcotest.(check bool)
    "synth typedef present" true
    (contains ~sub:"struct __RefByte_" s);
  Alcotest.(check bool)
    "field references synth" true
    (contains ~sub:"_RefByte_" s);
  Alcotest.(check bool)
    "constraint inlined" true
    (contains ~sub:">= 32" s && contains ~sub:"<= 126" s);
  let synth_ref =
    Re.execp
      (Re.compile
         (Re.seq
            [ Re.str "_RefByte_"; Re.rep1 Re.digit; Re.str " Data[:byte-size" ]))
      s
  in
  Alcotest.(check bool) "Data references synth not raw UINT8" true synth_ref

(* A lookup / cases field decodes only in-range indices (OCaml raises
   [Invalid_tag] otherwise); the 3D projection must emit the matching [< len]
   refinement so the generated C validator rejects the same out-of-range inputs.
   Without it the verified C is more permissive than the OCaml decoder. *)
let test_lookup_index_bound () =
  let codec =
    Codec.v "Lk"
      (fun v -> v)
      Codec.[ (Field.v "v" (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8) $ fun v -> v) ]
  in
  let output = to_3d (module_ [ typedef (Everparse.struct_of_codec codec) ]) in
  Alcotest.(check bool)
    "lookup field bounds its index" true
    (contains ~sub:"(v < 4)" output)

let test_array_lookup_element_bound () =
  (* A lookup as an array element must bound every element, not just a scalar
     field, or the verified C validator accepts out-of-range indices the OCaml
     decoder rejects. The element projects through a synthesised refined-byte
     struct, as a byte_array_where does. *)
  let codec =
    Codec.v "ArrLk"
      (fun v -> v)
      Codec.
        [
          ( Field.v "a"
              (array ~len:(int 3) (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8))
          $ fun v -> v );
        ]
  in
  (* The synthesised element struct is emitted by the full schema projection,
     not [struct_of_codec] alone, so render the whole module. *)
  let output = to_3d (Everparse.schema codec).module_ in
  Alcotest.(check bool)
    "array element bounds its index" true
    (contains ~sub:"(__elt_lk4 < 4)" output);
  Alcotest.(check bool)
    "element refined via synth struct" true
    (contains ~sub:"_RefByte_lk4 a" output)

let suite =
  ( "everparse",
    [
      Alcotest.test_case "generation: lookup index bound" `Quick
        test_lookup_index_bound;
      Alcotest.test_case "generation: array lookup element bound" `Quick
        test_array_lookup_element_bound;
      Alcotest.test_case "generation: bitfields" `Quick test_bitfields;
      Alcotest.test_case "generation: enumerations" `Quick test_enumerations;
      Alcotest.test_case "generation: big-endian enum projection" `Quick
        test_enum_big_endian_projection;
      Alcotest.test_case "generation: field dependence" `Quick
        test_field_dependence;
      Alcotest.test_case "generation: casetype" `Quick test_casetype;
      Alcotest.test_case "generation: casetype enum-tag labels" `Quick
        test_casetype_enum_tag_labels;
      Alcotest.test_case "generation: if_then_else projects" `Quick
        test_if_then_else_projects;
      Alcotest.test_case "generation: pretty print" `Quick test_pretty_print;
      Alcotest.test_case "3d: codec embed" `Quick test_3d_codec_embed;
      Alcotest.test_case "3d: codec nested" `Quick test_3d_codec_nested;
      Alcotest.test_case "3d: optional present" `Quick test_3d_optional_present;
      Alcotest.test_case "3d: optional absent" `Quick test_3d_optional_absent;
      Alcotest.test_case "3d: repeat" `Quick test_3d_repeat;
      Alcotest.test_case "3d: tm-like" `Quick test_3d_tm_like;
      Alcotest.test_case "3d: bit_order U8 Msb_first reorder" `Quick
        test_3d_bitorder_u8msb_reorder;
      Alcotest.test_case "3d: bit_order U8 Msb_first padding" `Quick
        test_3d_bitorder_u8msb_padding;
      Alcotest.test_case "3d: bit_order native no reorder" `Quick
        test_3d_bitorder_native_noreorder;
      Alcotest.test_case "3d: bit_order constraint collapse" `Quick
        test_3d_bitorder_constraint_collapse;
      Alcotest.test_case "3d: dep-size schema" `Quick test_3d_dep_size_schema;
      Alcotest.test_case "3d: dep-size roundtrip" `Quick
        test_3d_dep_size_roundtrip;
      Alcotest.test_case "3d: param in size" `Quick test_3d_param_in_size;
      Alcotest.test_case "3d: param embed" `Quick test_3d_param_embed;
      Alcotest.test_case "3d: nested over array" `Quick
        test_3d_nested_over_array;
      Alcotest.test_case "3d: reserved word escaping" `Quick
        test_reserved_word_escaping;
      Alcotest.test_case "3d: byte_array_where synth typedef" `Quick
        test_3d_byte_array_where;
      Alcotest.test_case "3d: uint63 projects to UINT64" `Quick
        test_3d_uint63_projects_to_uint64;
      Alcotest.test_case "3d: signed and float setters" `Quick
        test_3d_signed_float_setters;
      Alcotest.test_case "3d: arithmetic constraint widens operands" `Quick
        test_3d_arith_constraint_widens;
      Alcotest.test_case "3d: field sub/mul constraint rejected" `Quick
        test_3d_field_sub_mul_rejected;
      Alcotest.test_case "3d: array enum element decl" `Quick
        test_3d_array_enum_element_decl;
      Alcotest.test_case "3d: array open enum element renders base" `Quick
        test_3d_array_open_enum_element;
      Alcotest.test_case "generation: open enum accepts and has no refinement"
        `Quick test_enum_open_accepts_and_no_refinement;
      Alcotest.test_case "3d: enum membership refinement" `Quick
        test_3d_enum_membership;
      Alcotest.test_case "3d: enum membership on list elements" `Quick
        test_3d_enum_membership_list_elements;
      Alcotest.test_case "doc: drops FFI scaffolding" `Quick
        test_doc_drops_ffi_scaffolding;
      Alcotest.test_case "doc: codec ~doc renders as citation comment" `Quick
        test_doc_codec_citation;
      Alcotest.test_case "doc: long doc comment wraps at 80 columns" `Quick
        test_doc_comment_wraps_at_80;
      Alcotest.test_case "doc: enum renders as named type" `Quick
        test_doc_enum_as_type;
      Alcotest.test_case "doc: merge dedups shared types" `Quick
        test_doc_merge_dedup;
      Alcotest.test_case "doc: field ~doc renders as citation comment" `Quick
        test_doc_field_citation;
      Alcotest.test_case "doc: bit order matches schema projection" `Quick
        test_doc_bit_order_matches_schema;
      Alcotest.test_case "3d: nested byte_array_where refined typedef" `Quick
        test_3d_nested_byte_array_where;
      Alcotest.test_case "3d: static optional transparent projection" `Quick
        test_3d_static_optional_transparent;
      Alcotest.test_case "3d: absent optional projects to unit" `Quick
        test_3d_absent_optional_projects_to_unit;
      Alcotest.test_case "3d: on_act drops the trailing bool return" `Quick
        test_3d_on_act_drops_bool_return;
      Alcotest.test_case "3d: on_success conditional return to if/else" `Quick
        test_3d_on_success_conditional_return;
      Alcotest.test_case "3d: negative literal rejected by projection" `Quick
        test_3d_negative_literal_rejected;
      Alcotest.test_case "3d: signed constraint two's-complement" `Quick
        test_signed_constraint_twos_complement;
      Alcotest.test_case "3d: signed equality two's-complement" `Quick
        test_signed_equality_twos_complement;
      Alcotest.test_case "3d: float ordering rejected" `Quick
        test_float_ordering_rejected;
      Alcotest.test_case "3d: int64 literal unsigned decimal" `Quick
        test_3d_int64_literal_uses_unsigned_decimal;
      Alcotest.test_case "3d: field_pos rejected by projection" `Quick
        test_3d_field_pos_rejected;
    ] )
