(** Tests for the EverParse pipeline -- schema generation and 3D output. *)

open Wire
open Wire.Everparse.Raw

let simple_struct =
  struct_ "TestSimple" [ field "version" uint8; field "length" uint16be ]

let simple_module = module_ [ typedef ~entrypoint:true simple_struct ]

let test_everparse_name () =
  (* All-caps names get lowercased with first letter capitalized *)
  Alcotest.(check string) "CLCW -> Clcw" "Clcw" (Wire_3d.everparse_name "CLCW");
  Alcotest.(check string)
    "TMFrame -> Tmframe" "Tmframe"
    (Wire_3d.everparse_name "TMFrame");
  (* Standard camelCase is preserved *)
  Alcotest.(check string) "Foo -> Foo" "Foo" (Wire_3d.everparse_name "Foo");
  Alcotest.(check string)
    "myField -> myField" "myField"
    (Wire_3d.everparse_name "myField");
  (* Single uppercase letter at start is not >=2 consecutive uppercase *)
  Alcotest.(check string)
    "Hello -> Hello" "Hello"
    (Wire_3d.everparse_name "Hello");
  (* Underscore-separated: each segment normalized, underscores dropped *)
  Alcotest.(check string)
    "EP_Header -> EpHeader" "EpHeader"
    (Wire_3d.everparse_name "EP_Header");
  Alcotest.(check string)
    "MC_Status_Reply -> McStatusReply" "McStatusReply"
    (Wire_3d.everparse_name "MC_Status_Reply");
  Alcotest.(check string)
    "Foo_Bar -> FooBar" "FooBar"
    (Wire_3d.everparse_name "Foo_Bar");
  (* Empty string *)
  Alcotest.(check string) "empty" "" (Wire_3d.everparse_name "")

let test_generate_3d_files () =
  let tmpdir = Filename.temp_dir "wire_3d_test" "" in
  let s =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool) "3d file exists" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_schema_of_struct () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  (* generate_3d uses the schema -- check we can produce a .3d file *)
  let tmpdir = Filename.temp_dir "wire_3d_test2" "" in
  Wire_3d.generate_3d ~outdir:tmpdir [ s ];
  let path = Filename.concat tmpdir "TestSimple.3d" in
  Alcotest.(check bool)
    "3d file from schema_of_struct" true (Sys.file_exists path);
  Sys.remove path;
  Unix.rmdir tmpdir

let test_ensure_dir () =
  let tmpdir = Filename.temp_dir "wire_3d_ensure" "" in
  Unix.rmdir tmpdir;
  Wire_3d.ensure_dir tmpdir;
  Alcotest.(check bool) "dir created" true (Sys.file_exists tmpdir);
  Unix.rmdir tmpdir

let test_generate_c () =
  (* generate_c requires 3d.exe; skip when not available *)
  if Wire_3d.has_3d_exe () then begin
    let tmpdir = Filename.temp_dir "wire_3d_gen_c" "" in
    let s = Wire.Everparse.schema_of_struct simple_struct in
    Wire_3d.generate_3d ~outdir:tmpdir [ s ];
    Wire_3d.generate_c ~outdir:tmpdir [ s ];
    let c_path = Filename.concat tmpdir "TestSimple.c" in
    Alcotest.(check bool) "C file generated" true (Sys.file_exists c_path);
    let ext_path = Filename.concat tmpdir "TestSimple_ExternalTypedefs.h" in
    Alcotest.(check bool)
      "ExternalTypedefs.h generated" true (Sys.file_exists ext_path)
  end

(* End-to-end compile+run. Generates C for a schema, invokes the same
   cc command [generate_dune] emits, runs the resulting binary. This is
   the one test that catches every kind of name mismatch between what
   wire.3d emits and what EverParse actually produces: filenames in
   [dune.inc], validator function names in [test.c], setter names in
   [_Fields.c], struct types, [#include]s. If any one of them is off,
   cc/ld fails and the test fails with the actual compiler error.
   Parameterised over the schema so tricky names (underscores, all-
   caps prefixes, mixed case) each exercise the pipeline end-to-end. *)
let compile_and_run ~name codec =
  if not (Wire_3d.has_3d_exe ()) then ()
  else begin
    let tmpdir = Filename.temp_dir ("wire_3d_e2e_" ^ name) "" in
    let schema = Everparse.schema codec in
    Wire_3d.generate_3d ~outdir:tmpdir [ schema ];
    Wire_3d.generate_c ~outdir:tmpdir [ schema ];
    let base = String.capitalize_ascii schema.Everparse.name in
    let cmd =
      Fmt.str
        "cd %s && cc -std=c99 -Wall -Wextra -Werror -Wpedantic \
         -Wstrict-prototypes -Wmissing-prototypes -Wshadow -Wcast-qual -o \
         test_bin test.c %s.c %s_Fields.c 2>&1 && ./test_bin"
        tmpdir base base
    in
    let ic = Unix.open_process_in cmd in
    let output = In_channel.input_all ic in
    let status = Unix.close_process_in ic in
    ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
    match status with
    | Unix.WEXITED 0 -> ()
    | Unix.WEXITED n ->
        Alcotest.failf "%s: compile/run failed with exit %d:\n%s" name n output
    | _ ->
        Alcotest.failf "%s: compile/run terminated abnormally:\n%s" name output
  end

(* Schemas exercising every name-normalization edge case we know about. *)
let e2e_simple_codec =
  let open Wire in
  let f = Field.v "x" uint8 in
  Codec.v "Demo" (fun x -> x) [ Codec.( $ ) f (fun x -> x) ]

let e2e_allcaps_codec =
  let open Wire in
  let f = Field.v "x" uint8 in
  Codec.v "CLCW" (fun x -> x) [ Codec.( $ ) f (fun x -> x) ]

let e2e_tm_codec =
  let open Wire in
  let f = Field.v "version" (bits ~width:4 U8) in
  Codec.v "TMFrame" (fun v -> v) [ Codec.( $ ) f (fun v -> v) ]

let e2e_snake_codec =
  let open Wire in
  let fv = Field.v "v" uint8 in
  let fl = Field.v "len" uint16be in
  Codec.v "rpmsg_endpoint_info"
    (fun v l -> (v, l))
    [ Codec.( $ ) fv fst; Codec.( $ ) fl snd ]

let e2e_mixed_codec =
  let open Wire in
  let fh = Field.v "h" uint8 in
  Codec.v "EP_Header" (fun h -> h) [ Codec.( $ ) fh (fun h -> h) ]

(* Proximity1-style: variable-length data whose size is expressed as
   [length - header]. EverParse's SMT needs a constraint on [length]
   to prove the subtraction doesn't underflow, or verification of the
   generated .fst fails. [?self_constraint] on the length field emits
   that constraint inline. *)
let e2e_underflow_codec =
  let open Wire in
  let header_size = 7 in
  let f_len =
    Field.v "Length" uint16be ~self_constraint:(fun self ->
        Expr.(self >= int header_size))
  in
  let f_data =
    Field.v "Data" (byte_array ~size:Expr.(Field.ref f_len - int header_size))
  in
  let open Codec in
  v "UnderflowCheck" (fun len data -> (len, data)) [ f_len $ fst; f_data $ snd ]

(* [Wire.casetype] in a codec field. The Everparse projection auto-emits
   a [casetype_decl] dispatch + wrapper typedef per unique int-tagged
   [Casetype], then references the wrapper as the field type. End-to-end
   check that 3d.exe accepts the synthesised module and the generated C
   compiles. *)
type ep_case_v = [ `U16 of int | `Default of int ]

let e2e_casetype_codec =
  let open Wire in
  let body : ep_case_v typ =
    casetype "CtBody" uint8
      [
        case ~index:1 uint16
          ~inject:(fun v -> `U16 v)
          ~project:(function `U16 v -> Some v | _ -> None);
        default uint8
          ~inject:(fun v -> `Default v)
          ~project:(function `Default v -> Some v | _ -> None);
      ]
  in
  let f = Field.v "msg" body in
  Codec.v "Ctt" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* String-tagged casetype: the 3D projection rewrites the casetype field
   into two adjacent byte spans (tag and body) so dispatch happens in
   caller code, mirroring OpenSSH's two-step pattern. No 3D-side extern,
   scratch slot or runtime helper -- just two SetBytes setters. *)
type ssh_auth = [ `Publickey of int | `Other of int ]

let e2e_ssh_casetype_codec =
  let open Wire in
  let body : ssh_auth typ =
    casetype "AuthMethod"
      (byte_array ~size:(int 9))
      [
        case ~index:"publickey" uint8
          ~inject:(fun v -> `Publickey v)
          ~project:(function `Publickey v -> Some v | _ -> None);
        default uint8
          ~inject:(fun v -> `Other v)
          ~project:(function `Other v -> Some v | _ -> None);
      ]
  in
  let f = Field.v "method" body in
  Codec.v "Sshauth" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* Casetype with a sub-codec body that itself contains a trailing
   [all_bytes]. The 3D projection emits the sub-codec as its own typedef
   before the casetype's dispatch decl, and [all_bytes] inside the
   sub-codec consumes through end-of-buffer -- correct because the
   casetype is constrained to be the last field of its parent. *)
type session = { recipient : int; rest : string }
type channel_confirm = Session of session | Tcpip of int

let session_codec =
  let open Wire in
  let f_recip = Field.v "recipient" uint32be in
  let f_rest = Field.v "rest" all_bytes in
  Codec.v "Session"
    (fun r b -> { recipient = Wire.Private.UInt32.to_int r; rest = b })
    [
      Codec.( $ ) f_recip (fun s -> Wire.Private.UInt32.of_int s.recipient);
      Codec.( $ ) f_rest (fun s -> s.rest);
    ]

let e2e_codec_in_casetype =
  let open Wire in
  let body : channel_confirm typ =
    casetype "Confirm" uint8
      [
        case ~index:1 (codec session_codec)
          ~inject:(fun s -> Session s)
          ~project:(function Session s -> Some s | _ -> None);
        case ~index:2 uint16be
          ~inject:(fun n -> Tcpip n)
          ~project:(function Tcpip n -> Some n | _ -> None);
      ]
  in
  let f = Field.v "msg" body in
  Codec.v "ChannelOpen" (fun m -> m) [ Codec.( $ ) f (fun m -> m) ]

(* [Field.repeat] over a sub-codec whose own size is dynamic (here a
   length-prefixed name). Projects to a byte-budget repeat
   [Ext exts[:byte-size total]] in 3D. *)
type ext = { name : string }

let ext_codec =
  let open Wire in
  let f_len = Field.v "name_len" uint8 in
  let f_name = Field.v "name" (byte_array ~size:(Field.ref f_len)) in
  Codec.v "Ext"
    (fun _l n -> { name = n })
    [
      Codec.( $ ) f_len (fun e -> String.length e.name);
      Codec.( $ ) f_name (fun e -> e.name);
    ]

let e2e_repeat_var_elem =
  let open Wire in
  let f_total = Field.v "total" uint8 in
  let f_exts =
    Field.repeat "exts" ~size:(Field.ref f_total) (codec ext_codec)
  in
  Codec.v "Exts"
    (fun _t xs -> xs)
    [
      Codec.( $ ) f_total (fun xs ->
          List.fold_left (fun a e -> a + 1 + String.length e.name) 0 xs);
      Codec.( $ ) f_exts (fun xs -> xs);
    ]

let test_e2e_compile_run () =
  compile_and_run ~name:"Demo" e2e_simple_codec;
  compile_and_run ~name:"CLCW" e2e_allcaps_codec;
  compile_and_run ~name:"TMFrame" e2e_tm_codec;
  compile_and_run ~name:"Ctt" e2e_casetype_codec;
  compile_and_run ~name:"Sshauth" e2e_ssh_casetype_codec;
  compile_and_run ~name:"ChannelOpen" e2e_codec_in_casetype;
  compile_and_run ~name:"Exts" e2e_repeat_var_elem;
  compile_and_run ~name:"rpmsg_endpoint_info" e2e_snake_codec;
  compile_and_run ~name:"EP_Header" e2e_mixed_codec;
  compile_and_run ~name:"UnderflowCheck" e2e_underflow_codec

let test_uses_wire_ctx () =
  let s = Wire.Everparse.schema_of_struct simple_struct in
  Alcotest.(check bool)
    "schema_of_struct uses WireCtx" true
    (Wire.Everparse.uses_wire_ctx s);
  let raw =
    Wire.Everparse.Raw.of_module ~name:"TestSimple" ~module_:simple_module
      ~wire_size:3
  in
  Alcotest.(check bool)
    "raw module without extern WireCtx" false
    (Wire.Everparse.uses_wire_ctx raw)

let test_has_3d_exe () =
  (* Just check the function is callable and returns a bool *)
  let result = Wire_3d.has_3d_exe () in
  Alcotest.(check bool) "has_3d_exe returns bool" result result

let test_main_exists () =
  (* main dispatches on Sys.argv; calling it in tests would exit, so we only
     verify that the value exists and has the right type. *)
  let _ = (Wire_3d.main : package:string -> Wire.Everparse.t list -> unit) in
  ()

(* Adversarial: [schema.wire_size] must agree with [Codec.wire_size] AND with
   the byte count we expect by hand for every bitfield layout. Two layers:
   if the projection silently drifts from [Codec.wire_size] we validate the
   wrong amount of data; if the expected value is itself wrong, hand-computed
   constants catch that too. *)
let check_size ~name ~expected codec =
  let schema = Everparse.schema codec in
  let codec_size = Codec.wire_size codec in
  Alcotest.(check int)
    (Fmt.str "%s: Codec.wire_size = %d" name expected)
    expected codec_size;
  Alcotest.(check (option int))
    (Fmt.str "%s: schema.wire_size = %d" name expected)
    (Some expected) schema.Everparse.wire_size

let pack_four_u8 name ta tb tc td =
  let fa = Field.v "a" ta in
  let fb = Field.v "b" tb in
  let fc = Field.v "c" tc in
  let fd = Field.v "d" td in
  let open Codec in
  v name
    (fun a b c d -> (a, b, c, d))
    [
      (fa $ fun (a, _, _, _) -> a);
      (fb $ fun (_, b, _, _) -> b);
      (fc $ fun (_, _, c, _) -> c);
      (fd $ fun (_, _, _, d) -> d);
    ]

(* SSID-like layout: mix of [bit (bits 1 U8)] and bare [bits N U8] summing to
   exactly one U8. Historical bug: the Map-wrapped fields fell out of the bit
   group and produced spurious anon padding. *)
let ssid_style_codec =
  pack_four_u8 "SsidStyle"
    (bit (bits ~width:1 U8))
    (bits ~width:2 U8) (bits ~width:4 U8)
    (bit (bits ~width:1 U8))

(* All [bit] (Map-wrapped), packed into one U8. *)
let all_bool_codec =
  let b = bit (bits ~width:1 U8) in
  pack_four_u8 "AllBool" b b b b

(* Spilling across a base-word boundary: 5+5 bits on U8 must roll into two U8s. *)
let spill_u8_codec =
  let f1 = Field.v "a" (bits ~width:5 U8) in
  let f2 = Field.v "b" (bits ~width:5 U8) in
  let open Codec in
  v "SpillU8" (fun a b -> (a, b)) [ f1 $ fst; f2 $ snd ]

(* Bit group broken by a non-bitfield field. Two separate U8 groups plus
   the intervening scalar. *)
let broken_by_scalar_codec =
  let f1 = Field.v "a" (bits ~width:4 U8) in
  let f2 = Field.v "b" (bits ~width:4 U8) in
  let f3 = Field.v "sep" uint8 in
  let f4 = Field.v "c" (bits ~width:4 U8) in
  let f5 = Field.v "d" (bits ~width:4 U8) in
  let open Codec in
  v "BrokenByScalar"
    (fun a b sep c d -> (a, b, sep, c, d))
    [
      (f1 $ fun (a, _, _, _, _) -> a);
      (f2 $ fun (_, b, _, _, _) -> b);
      (f3 $ fun (_, _, s, _, _) -> s);
      (f4 $ fun (_, _, _, c, _) -> c);
      (f5 $ fun (_, _, _, _, d) -> d);
    ]

(* U32BE bitfields: total 32 bits -> one U32. *)
let u32_pack_codec =
  let f1 = Field.v "a" (bits ~width:8 U32be) in
  let f2 = Field.v "b" (bits ~width:16 U32be) in
  let f3 = Field.v "c" (bits ~width:8 U32be) in
  let open Codec in
  v "U32Pack"
    (fun a b c -> (a, b, c))
    [
      (f1 $ fun (a, _, _) -> a);
      (f2 $ fun (_, b, _) -> b);
      (f3 $ fun (_, _, c) -> c);
    ]

(* Bitfield wrapped in Where (constraint). Must still coalesce. *)
let where_wrapped_codec =
  let f1_ref = Field.v "a" (bits ~width:4 U8) in
  let f1 =
    Field.v "a" (bits ~width:4 U8) ~constraint_:Expr.(Field.ref f1_ref <= int 5)
  in
  let f2 = Field.v "b" (bits ~width:4 U8) in
  let open Codec in
  v "WhereWrapped" (fun a b -> (a, b)) [ f1 $ fst; f2 $ snd ]

let test_projection_sizes () =
  (* SSID: 1+2+4+1 = 8 bits, one U8. *)
  check_size ~name:"SsidStyle" ~expected:1 ssid_style_codec;
  (* 4 bool bits, one U8. *)
  check_size ~name:"AllBool" ~expected:1 all_bool_codec;
  (* 5+5 = 10 bits, must spill to two U8s. *)
  check_size ~name:"SpillU8" ~expected:2 spill_u8_codec;
  (* Two bit groups of one U8 each + an intervening uint8 = 3 bytes. *)
  check_size ~name:"BrokenByScalar" ~expected:3 broken_by_scalar_codec;
  (* 8+16+8 = 32 bits, one U32be. *)
  check_size ~name:"U32Pack" ~expected:4 u32_pack_codec;
  (* 4+4 = 8 bits, one U8, Where wrapping the first field. *)
  check_size ~name:"WhereWrapped" ~expected:1 where_wrapped_codec

(* Filenames must match what EverParse actually produces from the [.3d]
   (which wire writes as [String.capitalize_ascii name ^ ".3d"]). The
   C identifier inside is [everparse_name name]: strip underscores,
   CamelCase segments. Verify both together for a lowercase-underscore
   schema name like [rpmsg_endpoint_info] that exercises the split. *)
let test_projection_filenames () =
  let s =
    Everparse.Raw.struct_ "rpmsg_endpoint_info"
      [ Everparse.Raw.field "v" uint8; Everparse.Raw.field "id" uint16be ]
  in
  let schema = Everparse.schema_of_struct s in
  let tmpdir = Filename.temp_dir "wire_3d_filenames" "" in
  Wire_3d.generate_dune ~outdir:tmpdir ~package:"pkg" [ schema ];
  let dune_inc =
    let ic = open_in (Filename.concat tmpdir "dune.inc") in
    let n = in_channel_length ic in
    let buf = Bytes.create n in
    really_input ic buf 0 n;
    close_in ic;
    Bytes.unsafe_to_string buf
  in
  ignore (Fmt.kstr Sys.command "rm -rf %s" tmpdir);
  let contains_exact sub = Re.execp (Re.compile (Re.str sub)) dune_inc in
  Alcotest.(check bool)
    ".3d uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info.3d");
  Alcotest.(check bool)
    ".h uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info.h");
  Alcotest.(check bool)
    "Wrapper.c uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_infoWrapper.c");
  Alcotest.(check bool)
    "_Fields.c uses String.capitalize_ascii name" true
    (contains_exact "Rpmsg_endpoint_info_Fields.c");
  Alcotest.(check bool)
    "raw schema name not used as filename" false
    (contains_exact "rpmsg_endpoint_info.h")

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
  let s = Everparse.struct_of_codec codec in
  let out = to_3d_string s in
  Alcotest.(check bool)
    "byte-size suffix uses inner size" true (contains out "8")

let test_keyword_field_escaped () =
  let s =
    struct_ "Esc2"
      [ field "let" uint8; field "match" uint8; field "type" uint16be ]
  in
  let out = to_3d_string s in
  Alcotest.(check bool) "let_ field" true (contains out "let_");
  Alcotest.(check bool) "match_ field" true (contains out "match_");
  Alcotest.(check bool) "type_ field" true (contains out "type_")

let suite =
  ( "wire_3d",
    [
      Alcotest.test_case "everparse_name" `Quick test_everparse_name;
      Alcotest.test_case "generate 3d files" `Quick test_generate_3d_files;
      Alcotest.test_case "schema_of_struct" `Quick test_schema_of_struct;
      Alcotest.test_case "ensure_dir" `Quick test_ensure_dir;
      Alcotest.test_case "generate_c (needs 3d.exe)" `Quick test_generate_c;
      Alcotest.test_case "uses_wire_ctx" `Quick test_uses_wire_ctx;
      Alcotest.test_case "has_3d_exe" `Quick test_has_3d_exe;
      Alcotest.test_case "main exists" `Quick test_main_exists;
      Alcotest.test_case "projection: wire_size matches Codec" `Quick
        test_projection_sizes;
      Alcotest.test_case "projection: filenames match EverParse output" `Quick
        test_projection_filenames;
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
      Alcotest.test_case "e2e: compile + run across naming conventions" `Slow
        test_e2e_compile_run;
    ] )
