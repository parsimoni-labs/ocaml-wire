(* Tests for wire_3d.ml: the 3D shape of adversarial projections. *)

open Wire
open Wire.Everparse.Raw

(* A case that shells out to EverParse skips when [3d.exe] is absent, as in
   [Test_wire_3d]. *)
let needs_3d_exe () = if not (Wire_3d.has_3d_exe ()) then Alcotest.skip ()

let read_file path =
  let ic = open_in path in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

(* -- Adversarial projection tests for 3D-shape transformations -- *)

let to_3d_string s =
  let m = Wire.Everparse.Raw.module_ [ Wire.Everparse.Raw.typedef s ] in
  Wire.Everparse.Raw.to_3d m

let contains s sub =
  let lsub = String.length sub in
  let n = String.length s - lsub in
  let rec go i = i <= n && (String.sub s i lsub = sub || go (i + 1)) in
  lsub = 0 || go 0

(* [Action.var name e] must not project to [var name = e;] -- 3D's [var]
   binds an atomic action, not a pure expression. The DSL combinator [Var]
   has to be lowered by inlining the bound expression at every use site. *)
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
    (contains out "(Tag + Value) + 1")

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
    (contains out "*out = (Tag + 7);");
  Alcotest.(check bool)
    "inlined inside cond" true
    (contains out "(Tag + 7) > 100")

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
  let s = Everparse.Raw.struct_of_codec codec in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses inner size" true (contains out "8")

(* A gate-selected sub-codec may contain a group-local length followed by a
   repeat and still leave later fields in the parent. Run the full EverParse
   extraction here: rendering alone would not catch an invalid casetype or
   dependent repeat in the generated F*. *)
let test_optional_length_prefixed_group_projects () =
  needs_3d_exe ();
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
  needs_3d_exe ();
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
  needs_3d_exe ();
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
    Wire.Everparse.Raw.project_struct ~mode:`Ffi
      (struct_ base [ field "f0" uint16be; field "f1" uint8 ])
  in
  let setters = Wire.Everparse.plug_setters schema in
  let symbol i = Fmt.str "%sSetter%d" base i in
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

let suite =
  ( "wire_3d_shapes",
    [
      Alcotest.test_case "extern names across a wrapped declaration" `Quick
        test_extern_names_wrapped;
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
    ] )
