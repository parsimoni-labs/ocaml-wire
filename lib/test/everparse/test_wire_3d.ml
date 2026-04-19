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

(* SSID-like layout: mix of [bit (bits 1 U8)] and bare [bits N U8] summing to
   exactly one U8. Historical bug: the Map-wrapped fields fell out of the bit
   group and produced spurious anon padding. *)
let ssid_style_codec =
  let f1 = Field.v "a" (bit (bits ~width:1 U8)) in
  let f2 = Field.v "b" (bits ~width:2 U8) in
  let f3 = Field.v "c" (bits ~width:4 U8) in
  let f4 = Field.v "d" (bit (bits ~width:1 U8)) in
  let open Codec in
  v "SsidStyle"
    (fun a b c d -> (a, b, c, d))
    [
      (f1 $ fun (a, _, _, _) -> a);
      (f2 $ fun (_, b, _, _) -> b);
      (f3 $ fun (_, _, c, _) -> c);
      (f4 $ fun (_, _, _, d) -> d);
    ]

(* All [bit] (Map-wrapped), packed into one U8. *)
let all_bool_codec =
  let f1 = Field.v "a" (bit (bits ~width:1 U8)) in
  let f2 = Field.v "b" (bit (bits ~width:1 U8)) in
  let f3 = Field.v "c" (bit (bits ~width:1 U8)) in
  let f4 = Field.v "d" (bit (bits ~width:1 U8)) in
  let open Codec in
  v "AllBool"
    (fun a b c d -> (a, b, c, d))
    [
      (f1 $ fun (a, _, _, _) -> a);
      (f2 $ fun (_, b, _, _) -> b);
      (f3 $ fun (_, _, c, _) -> c);
      (f4 $ fun (_, _, _, d) -> d);
    ]

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
    ] )
