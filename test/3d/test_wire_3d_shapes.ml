(* Tests for wire_3d.ml: the 3D shape of adversarial projections. *)

open Wire
open Wire.Private.Everparse.Raw

(* Binary mode: on Windows a text channel drops each CR, so it yields fewer
   bytes than [in_channel_length] counts. *)
let read_file path = In_channel.with_open_bin path In_channel.input_all

(* -- Adversarial projection tests for 3D-shape transformations -- *)

let to_3d_string s =
  let m =
    Wire.Private.Everparse.Raw.module_ [ Wire.Private.Everparse.Raw.typedef s ]
  in
  Wire.Private.Everparse.Raw.to_3d m

let contains s sub =
  let lsub = String.length sub in
  let n = String.length s - lsub in
  let rec go i = i <= n && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0

(* [Action.var name e] must not project to [var name = e;] -- 3D's [var]
   binds an atomic action, not a pure expression. The DSL combinator [Var]
   has to be lowered by inlining the bound expression at every use site,
   widened as the arithmetic of its uses needs. *)
let test_var_inlines () =
  let f_a = field "Tag" uint8 in
  let f_b = field "Value" uint16be in
  let f_x = field "x" uint16be in
  let s =
    let out = Param.output "out" uint32be in
    param_struct "VarTest"
      [ Param.decl out ]
      [
        field "Tag" uint8;
        field "Value"
          ~action:
            (Action.on_act
               [
                 Action.var "x" Expr.(field_ref f_a + field_ref f_b);
                 Action.assign out Expr.(field_ref f_x + int 1);
               ])
          uint16be;
      ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "no [var x = ...] in 3D output" false (contains out "var x");
  Alcotest.(check bool)
    "x usage replaced by inlined expression" true
    (contains out "((((UINT32) Tag) + ((UINT32) Value)) + 1)")

(* Var bindings inside if-branches must not leak between sibling branches. *)
let test_var_scoping_in_if () =
  let f_a = field "Tag" uint8 in
  let f_x = field "x" uint16be in
  let s =
    let out = Param.output "out" uint32be in
    param_struct "VarIf"
      [ Param.decl out ]
      [
        field "Tag" uint8;
        field "Value"
          ~action:
            (Action.on_act
               [
                 Action.var "x" Expr.(field_ref f_a + int 7);
                 Action.if_
                   Expr.(field_ref f_x > int 100)
                   [ Action.assign out (field_ref f_x) ]
                   (Some [ Action.assign out (int 0) ]);
               ])
          uint8;
      ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "no var leak" false (contains out "var x");
  Alcotest.(check bool)
    "inlined inside then-branch" true
    (contains out "*out = (((UINT16) Tag) + 7);");
  Alcotest.(check bool)
    "inlined inside cond" true
    (contains out "(((UINT16) Tag) + 7) > 100")

(* Struct-level [where] referencing fields must lower to a constraint on
   the latest referenced field, not stay at struct level. 3D's [where]
   only sees parameters. *)
let test_where_lowers_to_field () =
  let f_len = field "Length" uint16be in
  let f_max = field "max" uint16be in
  let s =
    param_struct "WhereTest"
      [ param "max" uint16be ]
      ~where:Expr.(field_ref f_len <= field_ref f_max)
      [ field "Length" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "no top-level where" false (contains out "where (");
  Alcotest.(check bool)
    "constraint moved onto the field" true
    (contains out "Length <= max")

(* A param-only [where] must remain at the struct level. *)
let test_where_param_only_stays () =
  let s =
    let max_ = Param.input "max" uint16be in
    let lim = Param.input "lim" uint16be in
    param_struct "WhereParam"
      [ Param.decl max_; Param.decl lim ]
      ~where:Expr.(Param.expr max_ <= Param.expr lim)
      [ field "Length" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "where stays at struct level" true (contains out "where (")

(* The [where] must combine with any pre-existing field constraint via AND. *)
let test_where_combines_field_constraint () =
  let f_self = field "Length" uint16be in
  let f_len =
    field "Length" ~constraint_:Expr.(field_ref f_self > int 0) uint16be
  in
  let s =
    param_struct "WhereAnd" []
      ~where:Expr.(field_ref f_len < int 1024)
      [ f_len ]
  in
  let out = to_3d_string s in
  (* Both clauses must end up next to the field. *)
  Alcotest.(check bool) "left clause present" true (contains out "Length > 0");
  Alcotest.(check bool)
    "right clause present" true
    (contains out "Length < 1024")

(* Param/field names that collide with 3D or F* keywords must be escaped
   in the projected 3D output (not rejected at the API). The user keeps
   writing [Param.input "total" ...], the 3D output reads [total_]. *)
let test_keyword_param_escaped () =
  let p = Param.input "total" uint32be in
  let f = field "data" (byte_array ~size:(Param.expr p)) in
  let s = param_struct "Esc" [ Param.decl p ] [ f ] in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "param decl uses escaped name" true
    (contains out "UINT32BE total_");
  Alcotest.(check bool)
    "param ref uses escaped name" true
    (contains out "byte-size total_");
  Alcotest.(check bool)
    "raw [total] keyword does not appear" false
    (Re.execp (Re.compile (Re.seq [ Re.bow; Re.str "total"; Re.eow ])) out)

let expect_invalid_arg ~combinator f =
  match
    try
      let _ = f () in
      Ok ()
    with Invalid_argument msg -> Error msg
  with
  | Ok () ->
      Alcotest.failf "%s should have rejected the construction" combinator
  | Error _ -> ()

(* [array ~len:N (byte_array ~size:M)] is valid -- it projects to
   [UINT8 name[:byte-size (N * M)]]. The smart constructor only refuses
   element types that have no derivable wire size expression at all. *)
let test_array_accepts_sized_elem () =
  let s =
    struct_ "ArrSized"
      [ field "Items" (array ~len:(int 3) (byte_array ~size:(int 5))) ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses len * elem_size" true (contains out "byte-size")

let test_array_rejects_unsized_elem () =
  expect_invalid_arg ~combinator:"array (all_bytes)" (fun () ->
      array ~len:(int 4) all_bytes)

(* [Field.optional] accepts any element with a derivable size -- plain
   scalars, fixed bitfields, AND already-sized payloads like
   [byte_array {size}]. The projection emits a [byte-size if(cond, N, 0)]
   suffix that uses the inner's size expression. *)
let test_field_optional_byte_array () =
  let f_present = Field.v "present" uint8 in
  let cond = Expr.(Field.ref f_present <> int 0) in
  let codec =
    Codec.v "OptByteArr"
      (fun present body -> (present, body))
      Codec.
        [
          f_present $ fst;
          Field.optional "Body" ~present:cond (byte_array ~size:(int 8)) $ snd;
        ]
  in
  let s = Private.Everparse.Raw.struct_of_codec codec in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses inner size" true (contains out "8")

(* A gate-selected sub-codec may contain a group-local length followed by a
   repeat and still leave later fields in the parent. Run the full EverParse
   extraction here: rendering alone would not catch an invalid casetype or
   dependent repeat in the generated F*. *)
let test_optional_length_prefixed_group_projects () =
  Gate.needs_3d_exe ();
  begin
    let f_ext_len = Field.v "ext_items_len" uint8 in
    let extensions =
      Codec.v "ProjectedTransferExtensions"
        (fun len items -> (len, items))
        Codec.
          [
            f_ext_len $ fst;
            Field.repeat "ext_items" ~size:(Field.ref f_ext_len) uint8 $ snd;
          ]
    in
    let f_start = Field.v "start" uint8 in
    let f_data_len = Field.v "data_len" uint8 in
    let codec =
      Codec.v "ProjectedTransferSegment"
        (fun start ext data_len data -> (start, ext, data_len, data))
        Codec.
          [
            (f_start $ fun (start, _, _, _) -> start);
            ( Field.optional "extensions"
                ~present:Expr.(Field.ref f_start <> int 0)
                (codec extensions)
            $ fun (_, ext, _, _) -> ext );
            (f_data_len $ fun (_, _, data_len, _) -> data_len);
            ( Field.v "data" (byte_array ~size:(Field.ref f_data_len))
            $ fun (_, _, _, data) -> data );
          ]
    in
    let dir = Filename.temp_dir "wire_3d_optional_group" "" in
    let schema = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:dir [ schema ];
    Wire_3d.generate_c ~outdir:dir [ schema ];
    Alcotest.(check bool)
      "C generated for conditional group" true
      (Sys.file_exists (Filename.concat dir "ProjectedTransferSegment.c"))
  end

(* A bare UINT64 field was once reported as failing EverParse extraction.
   [generate_c] runs the whole pipeline (F* verification, then C extraction)
   and raises if EverParse rejects the module, so this pins the shape rather
   than only its rendering. *)
let test_bare_uint64_projects () =
  Gate.needs_3d_exe ();
  begin
    let codec =
      Codec.v "BareUint64"
        (fun value -> value)
        Codec.[ Field.v "value" uint64 $ Fun.id ]
    in
    let dir = Filename.temp_dir "wire_3d_bare_uint64" "" in
    let schema = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:dir [ schema ];
    Wire_3d.generate_c ~outdir:dir [ schema ];
    Alcotest.(check bool)
      "C generated for bare uint64" true
      (Sys.file_exists (Filename.concat dir "BareUint64.c"))
  end

(* A [nested ~size] / [nested_at_most] field projects through EverParse:
   byte-span and enum inners go through a synthesised wrapper struct, and scalar
   and sub-record inners render inline (rather than as an extern setter param
   typed [UINT8[:byte-size-single-element-array]], which is invalid 3D).
   [generate_c] runs [3d.exe] and raises if the .3d is rejected, so this is the
   regression guard. *)
let test_nested_field_projects () =
  Gate.needs_3d_exe ();
  begin
    let inner =
      Codec.v "NInner"
        (fun a b -> (a, b))
        Codec.[ Field.v "a" uint8 $ fst; Field.v "b" uint16be $ snd ]
    in
    let codec =
      Codec.v "NestedProj"
        (fun scalar bytes sub -> (scalar, bytes, sub))
        Codec.
          [
            (Field.v "scalar" (nested ~size:(int 4) uint8) $ fun (s, _, _) -> s);
            ( Field.v "bytes" (nested ~size:(int 4) (byte_array ~size:(int 3)))
            $ fun (_, b, _) -> b );
            ( Field.v "sub" (nested ~size:(int 8) (codec inner))
            $ fun (_, _, s) -> s );
          ]
    in
    let dir = Filename.temp_dir "wire_3d_nested" "" in
    let s = Everparse.project ~mode:`Ffi codec in
    Wire_3d.generate_3d ~outdir:dir [ s ];
    (* Runs 3d.exe; raises if EverParse rejects the generated .3d. *)
    Wire_3d.generate_c ~outdir:dir [ s ];
    Alcotest.(check bool)
      "C generated for nested fields" true
      (Sys.file_exists (Filename.concat dir "NestedProj.c"))
  end

(* Each half of a codec mixing binary and DER is checked by its own backend:
   EverParse verifies the 3D of the binary record holding DER, whose DER value
   is a byte region of its declared size, under an optional too, and of the
   binary codec a DER value holds under a containing. *)
let test_der_regions_project () =
  Gate.needs_3d_exe ();
  let framed = Asn1_modules.framed in
  (match Wire_3d.batch_check [ Everparse.project framed ] with
  | Ok () -> ()
  | Error e -> Alcotest.failf "standalone Framed: %s" e);
  (match Wire_3d.batch_check [ Everparse.project Asn1_modules.opt_framed ] with
  | Ok () -> ()
  | Error e -> Alcotest.failf "standalone OptFramed: %s" e);
  (let (Asn1_modules.Packed t) = Asn1_modules.wrapped in
   match Wire_3d.batch_check (Asn1.contained (Asn1.codec t)) with
   | Ok () -> ()
   | Error e -> Alcotest.failf "the binary Wrapped holds: %s" e);
  let ffi name codec =
    let dir = Filename.temp_dir "wire_3d_der" "" in
    Fun.protect
      ~finally:(fun () -> Wire_3d.rm_rf dir)
      (fun () ->
        Wire_3d.run ~outdir:dir [ Everparse.project ~mode:`Ffi codec ];
        let compile file =
          Fmt.kstr Sys.command "cc %s -I %s -c %s -o %s > %s 2>&1"
            Wire_3d.strict_cc_flags (Filename.quote dir)
            (Filename.quote (Filename.concat dir file))
            Filename.null Filename.null
        in
        Alcotest.(check int)
          (name ^ ": the validator compiles")
          0
          (compile (name ^ ".c"));
        Alcotest.(check int)
          (name ^ ": the visitor compiles")
          0
          (compile (name ^ "_Fields.c")))
  in
  ffi "Framed" framed;
  ffi "OptFramed" Asn1_modules.opt_framed

let test_keyword_field_escaped () =
  let s =
    struct_ "Esc2"
      [ field "let" uint8; field "match" uint8; field "type" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "let_ field" true (contains out "let_");
  Alcotest.(check bool) "match_ field" true (contains out "match_");
  Alcotest.(check bool) "type_ field" true (contains out "type_")

(* KaRaMeL pretty-prints an extern declaration to a column budget, so a schema
   name long enough to push the declaration past it lands [extern void] on one
   line and the symbol it introduces on the next. Both layouts have to read, and
   a header holding no declaration at all has to say so by name: pairing the
   symbols off against the schema's setters is what fails otherwise, from a call
   site that cannot tell which file came up short. Fabricate the header rather
   than shell out, so this runs in the lanes with no EverParse. *)
let test_extern_names_wrapped () =
  let base = "Smp27bits15u16belsbbits15u16belsbbits15u16belsbbits15u1" in
  let schema =
    Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi
      (struct_ base [ field "f0" uint16be; field "f1" uint8 ])
  in
  let setters = Wire.Private.Everparse.visitor_setters schema in
  (* EverParse changes the case of a setter's name, nothing else. *)
  let symbol i = String.lowercase_ascii (fst (List.nth setters i)) in
  let wrapped =
    String.concat ""
      (List.mapi
         (fun i (_, c_type) ->
           Fmt.str
             "extern void\n\
              %s(\n\
             \  WIRECTX *ctx,\n\
             \  uint32_t idx,\n\
             \  %s v\n\
              );\n\n"
             (symbol i) c_type)
         setters)
  in
  let fields_impl header =
    let dir = Filename.temp_dir "wire_3d_extern" "" in
    Fun.protect
      ~finally:(fun () -> Wire_3d.rm_rf dir)
      (fun () ->
        Out_channel.with_open_bin
          (Filename.concat dir (base ^ "_ExternalAPI.h"))
          (fun oc -> Out_channel.output_string oc header);
        Wire_3d.write_fields ~outdir:dir [ schema ];
        read_file (Filename.concat dir (base ^ "_Fields.c")))
  in
  Alcotest.(check bool)
    "the schema declares setters to pair" true (setters <> []);
  let impl = fields_impl wrapped in
  List.iteri
    (fun i _ ->
      Alcotest.(check bool)
        (Fmt.str "setter %d read across the wrap" i)
        true
        (contains impl (Fmt.str "void %s(WIRECTX *ctx," (symbol i))))
    setters;
  match fields_impl "/* nothing to read here */\n" with
  | impl -> Alcotest.failf "a header with no declaration produced %S" impl
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "the failure names the header it could not read" true
        (contains msg (base ^ "_ExternalAPI.h"))

(* Since v2026.10, EverParse does not declare the extern setters in the order
   the schema references them, and it CamelCases a snake-case schema name in
   them. Each setter of the visitor takes the declared name equal to its own up to
   case and underscores, and with it its own value type. *)
let test_extern_names_by_name () =
  let schema =
    Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi
      (struct_ "order_by_name" [ field "f0" uint16be; field "f1" uint8 ])
  in
  let setters = Wire.Private.Everparse.visitor_setters schema in
  Alcotest.(check int) "two setters to pair" 2 (List.length setters);
  let declared name =
    String.lowercase_ascii (String.concat "" (String.split_on_char '_' name))
  in
  let header =
    String.concat ""
      (List.rev_map
         (fun (name, c_type) ->
           Fmt.str "extern void %s(WIRECTX *x0, uint32_t x1, %s x2);\n"
             (declared name) c_type)
         setters)
  in
  let dir = Filename.temp_dir "wire_3d_order" "" in
  let impl =
    Fun.protect
      ~finally:(fun () -> Wire_3d.rm_rf dir)
      (fun () ->
        Out_channel.with_open_bin
          (Filename.concat dir "Order_by_name_ExternalAPI.h") (fun oc ->
            Out_channel.output_string oc header);
        Wire_3d.write_fields ~outdir:dir [ schema ];
        read_file (Filename.concat dir "Order_by_name_Fields.c"))
  in
  List.iter
    (fun (name, c_type) ->
      Alcotest.(check bool)
        (Fmt.str "%s takes %s" name c_type)
        true
        (contains impl
           (Fmt.str "void %s(WIRECTX *ctx, uint32_t idx, %s v)" (declared name)
              c_type)))
    setters

(* Since v2026.11, the [_ExternalAPI.h] EverParse writes only includes the
   module's header, which F* writes and which declares the extern setters. Each
   setter takes its name from the header the include names. *)
let test_extern_names_through_include () =
  let schema =
    Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi
      (struct_ "Included" [ field "f0" uint16be; field "f1" uint8 ])
  in
  let setters = Wire.Private.Everparse.visitor_setters schema in
  Alcotest.(check int) "two setters to pair" 2 (List.length setters);
  let module_header =
    String.concat ""
      (List.map
         (fun (name, c_type) ->
           Fmt.str "extern void %s(WIRECTX *, uint32_t, %s);\n" name c_type)
         setters)
  in
  let dir = Filename.temp_dir "wire_3d_include" "" in
  let impl =
    Fun.protect
      ~finally:(fun () -> Wire_3d.rm_rf dir)
      (fun () ->
        let write name text =
          Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
              Out_channel.output_string oc text)
        in
        write "Included_ExternalAPI.h" "#include \"Included.h\"\n";
        write "Included.h" module_header;
        Wire_3d.write_fields ~outdir:dir [ schema ];
        read_file (Filename.concat dir "Included_Fields.c"))
  in
  List.iter
    (fun (name, c_type) ->
      Alcotest.(check bool)
        (Fmt.str "%s takes %s" name c_type)
        true
        (contains impl
           (Fmt.str "void %s(WIRECTX *ctx, uint32_t idx, %s v)" name c_type)))
    setters

(* The default visitor of a schema, written next to a stand-in for the
   [_ExternalAPI.h] 3d.exe would emit, which declares each setter under its
   own name. *)
let visitor_of fields =
  let schema =
    Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi (struct_ "Kw" fields)
  in
  let dir = Filename.temp_dir "wire_3d_visitor" "" in
  Fun.protect
    ~finally:(fun () -> Wire_3d.rm_rf dir)
    (fun () ->
      let api =
        String.concat ""
          (List.map
             (fun (setter, c_type) ->
               Fmt.str "extern void %s(WIRECTX *ctx, uint32_t idx, %s v);\n"
                 setter c_type)
             (Wire.Private.Everparse.visitor_setters schema))
      in
      Out_channel.with_open_bin (Filename.concat dir "Kw_ExternalAPI.h")
        (fun oc -> Out_channel.output_string oc api);
      Wire_3d.write_fields ~outdir:dir [ schema ];
      let header = read_file (Filename.concat dir "Kw_Fields.h") in
      let source name text =
        let path = Filename.concat dir name in
        Out_channel.with_open_bin path (fun oc ->
            Out_channel.output_string oc text);
        path
      in
      let compile file =
        Fmt.kstr Sys.command "cc -std=c2x -c %s -o %s > %s 2>&1"
          (Filename.quote file)
          (Filename.quote (Filename.concat dir "probe.o"))
          Filename.null
        = 0
      in
      let compiles =
        (not (compile (source "empty.c" "int probe_empty;\n")))
        || compile
             (source "probe.c"
                "#include \"Kw_Fields.h\"\nKwFields probe_fields;\n")
      in
      (header, read_file (Filename.concat dir "Kw_Fields.c"), compiles))

(* The visitor names a field three ways: the [_IDX_] macro upper-cases it, so [X]
   and [x] would define one macro twice, which is refused naming the two
   fields. The struct member escapes C's keywords injectively, so [restrict]
   and [restrict_] are the members [restrict_] and [restrict__]. *)
let test_visitor_names () =
  let rejects what subs fields =
    match visitor_of fields with
    | _ -> Alcotest.failf "%s: accepted" what
    | exception Invalid_argument msg ->
        List.iter
          (fun sub ->
            if not (contains msg sub) then
              Alcotest.failf "%s: %S does not mention %S" what msg sub)
          subs
  in
  rejects "X and x"
    [ "\"X\""; "\"x\""; "KW_IDX_X" ]
    [ field "X" uint8; field "x" uint8 ];
  rejects "a-b" [ "\"a-b\""; "identifier" ] [ field "a-b" uint8 ];
  let header, impl, compiles =
    visitor_of
      [
        field "int" uint8;
        field "restrict" uint8;
        field "typeof" uint8;
        field "NULL" uint8;
        field "end" uint8;
        field "restrict_" uint8;
      ]
  in
  List.iter
    (fun m ->
      Alcotest.(check bool)
        ("member " ^ m) true
        (contains header (Fmt.str " %s;" m));
      Alcotest.(check bool) ("set " ^ m) true (contains impl ("f->" ^ m ^ " ")))
    [ "int_"; "restrict_"; "typeof_"; "NULL_"; "end"; "restrict__" ];
  Alcotest.(check bool)
    "macro keeps the field name" true
    (contains header "#define KW_IDX_RESTRICT 1");
  Alcotest.(check bool) "the visitor header compiles as C23" true compiles

(* A field named for an F* keyword reaches the F* EverParse writes once a
   constraint reads it, and [typeof] reaches the C, where GNU C and C23 read
   it as a keyword; both validate and compile once escaped. *)
let test_keyword_fields_e2e () =
  Gate.needs_3d_exe ();
  let f_end = field "end" uint8 in
  let s =
    struct_ "KwEnd"
      [
        f_end;
        field "y" ~constraint_:Expr.(field_ref f_end <= int 200) uint8;
        field "typeof" uint8;
        field "restrict" uint8;
      ]
  in
  let schema = Wire.Private.Everparse.Raw.project_struct ~mode:`Ffi s in
  let dir = Filename.temp_dir "wire_3d_kw" "" in
  Fun.protect
    ~finally:(fun () -> ignore (Fmt.kstr Sys.command "rm -rf %s" dir))
    (fun () ->
      Wire_3d.run ~outdir:dir [ schema ];
      let compile file =
        Fmt.kstr Sys.command "cc %s -I %s -c %s -o %s > %s 2>&1"
          Wire_3d.strict_cc_flags (Filename.quote dir)
          (Filename.quote (Filename.concat dir file))
          Filename.null Filename.null
      in
      Alcotest.(check int) "the validator compiles" 0 (compile "KwEnd.c");
      Alcotest.(check int) "the visitor compiles" 0 (compile "KwEnd_Fields.c"))

let suite =
  ( "wire_3d_shapes",
    [
      Alcotest.test_case "extern names across a wrapped declaration" `Quick
        test_extern_names_wrapped;
      Alcotest.test_case "extern names paired by name, not order" `Quick
        test_extern_names_by_name;
      Alcotest.test_case "extern names through the module header" `Quick
        test_extern_names_through_include;
      Alcotest.test_case "projection: var inlines into uses" `Quick
        test_var_inlines;
      Alcotest.test_case "projection: var scoping inside if" `Quick
        test_var_scoping_in_if;
      Alcotest.test_case "projection: where lowers to field constraint" `Quick
        test_where_lowers_to_field;
      Alcotest.test_case "projection: param-only where stays at struct" `Quick
        test_where_param_only_stays;
      Alcotest.test_case "projection: where ANDs with field constraint" `Quick
        test_where_combines_field_constraint;
      Alcotest.test_case "projection: F*-keyword param escaped" `Quick
        test_keyword_param_escaped;
      Alcotest.test_case "projection: F*-keyword field escaped" `Quick
        test_keyword_field_escaped;
      Alcotest.test_case "ctor: array accepts sized variable elem" `Quick
        test_array_accepts_sized_elem;
      Alcotest.test_case "ctor: array rejects unsized elem" `Quick
        test_array_rejects_unsized_elem;
      Alcotest.test_case "Field.optional: byte_array inner projects" `Quick
        test_field_optional_byte_array;
      Alcotest.test_case "optional length-prefixed group projects" `Slow
        test_optional_length_prefixed_group_projects;
      Alcotest.test_case "bare uint64 projects" `Slow test_bare_uint64_projects;
      Alcotest.test_case "nested field projects through EverParse" `Slow
        test_nested_field_projects;
      Alcotest.test_case "DER regions project through EverParse" `Slow
        test_der_regions_project;
      Alcotest.test_case "visitor: field names" `Quick test_visitor_names;
      Alcotest.test_case "keyword field names (needs 3d.exe)" `Quick
        test_keyword_fields_e2e;
    ] )
