(** Fuzz tests for wire library.

    Since wire is primarily a code generator (OCaml -> 3D format), the main fuzz
    targets are: 1. Pretty-printer crash safety - ensuring pp functions don't
    crash 2. Future: differential testing against EverParse parser once we add a
    bytesrw parsing backend *)

module Cr = Crowbar
open Wire

(* Silence unused variable warnings for parse error handling *)
let _ = pp_parse_error

(* Helper: encode record to string using Codec API *)
let encode_record_to_string codec v =
  let ws = Codec.wire_size codec in
  let buf = Bytes.create ws in
  Codec.encode codec v buf 0;
  Ok (Bytes.unsafe_to_string buf)

(* Helper: decode record from string using Codec API *)
let decode_record_from_string codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error (Unexpected_eof { expected = ws; got = String.length s })
  else Ok (Codec.decode codec (Bytes.of_string s) 0)

(** Convert crowbar bytes to OCaml bytes. *)
let to_bytes buf =
  let len = String.length buf in
  let b = Bytes.create len in
  Bytes.blit_string buf 0 b 0 len;
  b

(** Truncate input to reasonable size for protocol messages. *)
let truncate buf =
  let max_len = 1024 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

(** Test pp_typ doesn't crash on well-formed types. *)
let test_pp_uint8 () =
  let _ = Format.asprintf "%a" pp_typ uint8 in
  ()

let test_pp_uint16 () =
  let _ = Format.asprintf "%a" pp_typ uint16 in
  ()

let test_pp_uint32 () =
  let _ = Format.asprintf "%a" pp_typ uint32 in
  ()

let test_pp_bitfield width =
  if width > 0 && width <= 32 then begin
    let t = bits ~width bf_uint32 in
    let _ = Format.asprintf "%a" pp_typ t in
    ()
  end

(** Test pp_module doesn't crash on valid modules. *)
let test_pp_module_simple () =
  let s = struct_ "Test" [ field "a" uint8; field "b" uint16 ] in
  let m = module_ "Test" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test struct with random field count. *)
let test_struct_random_fields n =
  let n = (n mod 10) + 1 in
  let fields = List.init n (fun i -> field (Printf.sprintf "f%d" i) uint8) in
  let s = struct_ "Random" fields in
  let m = module_ "Random" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test enum with random cases. *)
let test_enum_random_cases n =
  let n = (n mod 10) + 1 in
  let cases = List.init n (fun i -> (Printf.sprintf "C%d" i, i)) in
  let e = enum_decl "RandEnum" cases uint8 in
  let m = module_ "RandEnum" [ e ] in
  let _ = to_3d m in
  ()

(** Test casetype with random cases. *)
let test_casetype_random n =
  let n = (n mod 5) + 1 in
  let cases = List.init n (fun i -> decl_case i uint8) in
  let ct = casetype_decl "_RandCase" [ param "tag" uint8 ] uint8 cases in
  let m = module_ "RandCase" [ ct ] in
  let _ = to_3d m in
  ()

(** Test constraint expression generation. *)
let test_constraint_expr a =
  let v = abs a mod 1000 in
  let cond = Expr.(ref "x" <= int v) in
  let s = struct_ "Constrained" [ field "x" ~constraint_:cond uint16 ] in
  let m = module_ "Constrained" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test bitfield constraints. *)
let test_bitfield_constraint width =
  let width = (width mod 16) + 1 in
  let t = bits ~width bf_uint16 in
  let cond = Expr.(ref "x" <= int 100) in
  let s = struct_ "BFConstrained" [ field "x" ~constraint_:cond t ] in
  let m = module_ "BFConstrained" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test array types. *)
let test_array_type len =
  let len = (abs len mod 100) + 1 in
  let arr = array ~len:(int len) uint8 in
  let s = struct_ "WithArray" [ field "arr" arr ] in
  let m = module_ "WithArray" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test byte_array types. *)
let test_byte_array size =
  let size = (abs size mod 1000) + 1 in
  let ba = byte_array ~size:(int size) in
  let s = struct_ "WithByteArray" [ field "data" ba ] in
  let m = module_ "WithByteArray" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test parameterized struct. *)
let test_param_struct n =
  let n = (n mod 5) + 1 in
  let params = List.init n (fun i -> param (Printf.sprintf "p%d" i) uint32) in
  let ps = param_struct "Parametric" params [ field "x" uint8 ] in
  let m = module_ "Parametric" [ typedef ps ] in
  let _ = to_3d m in
  ()

(** Test action generation. *)
let test_action () =
  let act = on_success [ assign "ptr" (int 42); return_bool true_ ] in
  let s = struct_ "WithAction" [ field "x" ~action:act uint8 ] in
  let m = module_ "WithAction" [ typedef s ] in
  let _ = to_3d m in
  ()

(** Test complex nested structure. *)
let test_complex_nested () =
  let inner = struct_ "Inner" [ field "a" uint8 ] in
  let outer =
    struct_ "Outer" [ field "i" (struct_typ inner); field "b" uint16 ]
  in
  let m = module_ "Nested" [ typedef inner; typedef outer ] in
  let _ = to_3d m in
  ()

(** {1 Parsing Tests} *)

(** Parse uint8 - must not crash on arbitrary input. *)
let test_parse_uint8 buf =
  let buf = truncate buf in
  let _ = parse_string uint8 buf in
  ()

(** Parse uint16 - must not crash on arbitrary input. *)
let test_parse_uint16 buf =
  let buf = truncate buf in
  let _ = parse_string uint16 buf in
  ()

(** Parse uint32 - must not crash on arbitrary input. *)
let test_parse_uint32 buf =
  let buf = truncate buf in
  let _ = parse_string uint32 buf in
  ()

(** Parse uint64 - must not crash on arbitrary input. *)
let test_parse_uint64 buf =
  let buf = truncate buf in
  let _ = parse_string uint64 buf in
  ()

(** Parse bitfield - must not crash on arbitrary input. *)
let test_parse_bitfield buf =
  let buf = truncate buf in
  let t = bits ~width:6 bf_uint32 in
  let _ = parse_string t buf in
  ()

(** Parse array - must not crash on arbitrary input. *)
let test_parse_array buf =
  let buf = truncate buf in
  let len = min 10 (String.length buf) in
  let arr = array ~len:(int len) uint8 in
  let _ = parse_string arr buf in
  ()

(** Parse byte_array - must not crash on arbitrary input. *)
let test_parse_byte_array buf =
  let buf = truncate buf in
  let size = min 10 (String.length buf) in
  let ba = byte_array ~size:(int size) in
  let _ = parse_string ba buf in
  ()

(** Parse enum - must not crash on arbitrary input. *)
let test_parse_enum buf =
  let buf = truncate buf in
  let e = enum "TestEnum" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let _ = parse_string e buf in
  ()

(** Parse with constraint - must not crash. *)
let test_parse_where buf =
  let buf = truncate buf in
  (* Use a constraint that always succeeds - field refs need struct context *)
  let t = where true_ uint8 in
  let _ = parse_string t buf in
  ()

(** Parse all_bytes - must not crash. *)
let test_parse_all_bytes buf =
  let buf = truncate buf in
  let _ = parse_string all_bytes buf in
  ()

(** Parse all_zeros - must not crash. *)
let test_parse_all_zeros buf =
  let buf = truncate buf in
  let _ = parse_string all_zeros buf in
  ()

(** Parse struct - must not crash. *)
let test_parse_struct buf =
  let buf = truncate buf in
  let s =
    struct_ "Test" [ field "a" uint8; field "b" uint16; field "c" uint32 ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

(** Parse struct with constraint - must not crash. *)
let test_parse_struct_constrained buf =
  let buf = truncate buf in
  let s =
    struct_ "Constrained"
      [ field "x" ~constraint_:Expr.(ref "x" <= int 100) uint8 ]
  in
  let t = struct_typ s in
  let _ = parse_string t buf in
  ()

(** {1 Roundtrip Tests} *)

(** Roundtrip uint8 - encode then parse. *)
let test_roundtrip_uint8 n =
  let n = abs n mod 256 in
  let encoded = encode_to_string uint8 n in
  match parse_string uint8 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint8 roundtrip mismatch"
  | Error _ -> Cr.fail "uint8 roundtrip parse failed"

(** Roundtrip uint16 - encode then parse. *)
let test_roundtrip_uint16 n =
  let n = abs n mod 65536 in
  let encoded = encode_to_string uint16 n in
  match parse_string uint16 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint16 roundtrip mismatch"
  | Error _ -> Cr.fail "uint16 roundtrip parse failed"

(** Roundtrip uint32 - encode then parse. *)
let test_roundtrip_uint32 n =
  let n = n land ((1 lsl 32) - 1) in
  let encoded = encode_to_string uint32 n in
  match parse_string uint32 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint32 roundtrip mismatch"
  | Error _ -> Cr.fail "uint32 roundtrip parse failed"

(** Roundtrip uint64 - encode then parse. *)
let test_roundtrip_uint64 n =
  let encoded = encode_to_string uint64 n in
  match parse_string uint64 encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "uint64 roundtrip mismatch"
  | Error _ -> Cr.fail "uint64 roundtrip parse failed"

(** Roundtrip array - encode then parse. *)
let test_roundtrip_array a b c =
  let arr = [ abs a mod 256; abs b mod 256; abs c mod 256 ] in
  let t = array ~len:(int 3) uint8 in
  let encoded = encode_to_string t arr in
  match parse_string t encoded with
  | Ok decoded -> if arr <> decoded then Cr.fail "array roundtrip mismatch"
  | Error _ -> Cr.fail "array roundtrip parse failed"

(** Roundtrip byte_array - encode then parse. *)
let test_roundtrip_byte_array buf =
  let buf = truncate buf in
  let len = String.length buf in
  if len > 0 then begin
    let t = byte_array ~size:(int len) in
    let encoded = encode_to_string t buf in
    match parse_string t encoded with
    | Ok decoded ->
        if buf <> decoded then Cr.fail "byte_array roundtrip mismatch"
    | Error _ -> Cr.fail "byte_array roundtrip parse failed"
  end

(** Roundtrip enum - encode then parse. *)
let test_roundtrip_enum n =
  let n = abs n mod 3 in
  let t = enum "Test" [ ("A", 0); ("B", 1); ("C", 2) ] uint8 in
  let encoded = encode_to_string t n in
  match parse_string t encoded with
  | Ok decoded -> if n <> decoded then Cr.fail "enum roundtrip mismatch"
  | Error _ -> Cr.fail "enum roundtrip parse failed"

(** {1 Record Codec Tests} *)

type test_record = { x : int; y : int; z : int }

let test_record_codec =
  let open Codec in
  record "TestRecord" (fun x y z -> { x; y; z })
  |+ field "x" uint8 (fun r -> r.x)
  |+ field "y" uint16 (fun r -> r.y)
  |+ field "z" uint32 (fun r -> r.z)
  |> seal

(** Record roundtrip - encode then decode. *)
let test_record_roundtrip x y z =
  let x = abs x mod 256 in
  let y = abs y mod 65536 in
  let z = z land 0xFFFFFFFF in
  let original = { x; y; z } in
  match encode_record_to_string test_record_codec original with
  | Error _ -> Cr.fail "record encode failed"
  | Ok encoded -> (
      match decode_record_from_string test_record_codec encoded with
      | Ok decoded ->
          if original.x <> decoded.x then Cr.fail "record x mismatch";
          if original.y <> decoded.y then Cr.fail "record y mismatch";
          if original.z <> decoded.z then Cr.fail "record z mismatch"
      | Error _ -> Cr.fail "record roundtrip decode failed")

(** Record decode crash safety. *)
let test_record_decode_crash buf =
  let buf = truncate buf in
  let _ = decode_record_from_string test_record_codec buf in
  ()

let () =
  (* Pretty-printing tests *)
  Cr.add_test ~name:"wire: pp_typ uint8" [ Cr.const () ] test_pp_uint8;
  Cr.add_test ~name:"wire: pp_typ uint16" [ Cr.const () ] test_pp_uint16;
  Cr.add_test ~name:"wire: pp_typ uint32" [ Cr.const () ] test_pp_uint32;
  Cr.add_test ~name:"wire: pp_bitfield" [ Cr.range 33 ] test_pp_bitfield;
  Cr.add_test ~name:"wire: pp_module_simple"
    [ Cr.const () ]
    test_pp_module_simple;
  Cr.add_test ~name:"wire: struct_random_fields"
    [ Cr.range 100 ]
    test_struct_random_fields;
  Cr.add_test ~name:"wire: enum_random_cases"
    [ Cr.range 100 ]
    test_enum_random_cases;
  Cr.add_test ~name:"wire: casetype_random"
    [ Cr.range 100 ]
    test_casetype_random;
  Cr.add_test ~name:"wire: constraint_expr" [ Cr.int ] test_constraint_expr;
  Cr.add_test ~name:"wire: bitfield_constraint"
    [ Cr.range 32 ]
    test_bitfield_constraint;
  Cr.add_test ~name:"wire: array_type" [ Cr.int ] test_array_type;
  Cr.add_test ~name:"wire: byte_array" [ Cr.int ] test_byte_array;
  Cr.add_test ~name:"wire: param_struct" [ Cr.range 20 ] test_param_struct;
  Cr.add_test ~name:"wire: action" [ Cr.const () ] test_action;
  Cr.add_test ~name:"wire: complex_nested" [ Cr.const () ] test_complex_nested;

  (* Parsing tests *)
  Cr.add_test ~name:"wire: parse uint8" [ Cr.bytes ] test_parse_uint8;
  Cr.add_test ~name:"wire: parse uint16" [ Cr.bytes ] test_parse_uint16;
  Cr.add_test ~name:"wire: parse uint32" [ Cr.bytes ] test_parse_uint32;
  Cr.add_test ~name:"wire: parse uint64" [ Cr.bytes ] test_parse_uint64;
  Cr.add_test ~name:"wire: parse bitfield" [ Cr.bytes ] test_parse_bitfield;
  Cr.add_test ~name:"wire: parse array" [ Cr.bytes ] test_parse_array;
  Cr.add_test ~name:"wire: parse byte_array" [ Cr.bytes ] test_parse_byte_array;
  Cr.add_test ~name:"wire: parse enum" [ Cr.bytes ] test_parse_enum;
  Cr.add_test ~name:"wire: parse where" [ Cr.bytes ] test_parse_where;
  Cr.add_test ~name:"wire: parse all_bytes" [ Cr.bytes ] test_parse_all_bytes;
  Cr.add_test ~name:"wire: parse all_zeros" [ Cr.bytes ] test_parse_all_zeros;
  Cr.add_test ~name:"wire: parse struct" [ Cr.bytes ] test_parse_struct;
  Cr.add_test ~name:"wire: parse struct_constrained" [ Cr.bytes ]
    test_parse_struct_constrained;

  (* Roundtrip tests *)
  Cr.add_test ~name:"wire: roundtrip uint8" [ Cr.int ] test_roundtrip_uint8;
  Cr.add_test ~name:"wire: roundtrip uint16" [ Cr.int ] test_roundtrip_uint16;
  Cr.add_test ~name:"wire: roundtrip uint32" [ Cr.int ] test_roundtrip_uint32;
  Cr.add_test ~name:"wire: roundtrip uint64" [ Cr.int64 ] test_roundtrip_uint64;
  Cr.add_test ~name:"wire: roundtrip array" [ Cr.int; Cr.int; Cr.int ]
    test_roundtrip_array;
  Cr.add_test ~name:"wire: roundtrip byte_array" [ Cr.bytes ]
    test_roundtrip_byte_array;
  Cr.add_test ~name:"wire: roundtrip enum" [ Cr.int ] test_roundtrip_enum;

  (* Record codec tests *)
  Cr.add_test ~name:"wire: record roundtrip" [ Cr.int; Cr.int; Cr.int ]
    test_record_roundtrip;
  Cr.add_test ~name:"wire: record decode crash" [ Cr.bytes ]
    test_record_decode_crash
