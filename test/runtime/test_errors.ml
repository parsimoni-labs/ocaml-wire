(* Tests for Errors: each raiser raises the failure it documents, at the place
   it names, and the printers spell it as a codec's error does. *)

open Wire_runtime.Errors

(* The printers break lines where a long list needs it; what is said is the
   same either way. *)
let one_line s = String.map (function '\n' -> ' ' | c -> c) s

let parse_error f =
  match f () with
  | _ -> Alcotest.fail "no exception raised"
  | exception Parse_error e -> Fmt.kstr one_line "%a" pp_parse_error e

let invalid f =
  match f () with
  | _ -> Alcotest.fail "no exception raised"
  | exception Invalid_argument m -> one_line m

let check what expected got = Alcotest.(check string) what expected got

(* A record that does not fit names how many bytes were left, never fewer
   than none, and a negative offset is the caller's mistake. *)
let test_frame () =
  check "short" "@1: unexpected EOF: expected 5 bytes, got 3"
    (parse_error (fun () -> frame ~size:5 ~len:4 1));
  check "past the end" "@9: unexpected EOF: expected 5 bytes, got 0"
    (parse_error (fun () -> frame ~size:5 ~len:4 9));
  check "at the end" "@4: unexpected EOF: expected 1 bytes, got 0"
    (parse_error (fun () -> frame ~size:1 ~len:4 4));
  check "negative" "Codec: negative offset -1"
    (invalid (fun () -> frame ~size:5 ~len:40 (-1)))

let test_fields () =
  check "constraint" "TTL@3: field constraint violated (value 0)"
    (parse_error (fun () -> field_constraint ~at:3 ~field:"TTL" 0));
  check "negative value" "S@0: field constraint violated (value -6)"
    (parse_error (fun () -> field_constraint ~at:0 ~field:"S" (-6)));
  check "enum, no path" "@4: invalid enum value 7, valid: [1, 6, 17]"
    (parse_error (fun () ->
         invalid_enum ~at:4 ~field:[] ~value:7 ~valid:[ 1; 6; 17 ]));
  check "enum, path" "Proto@4: invalid enum value 7, valid: []"
    (parse_error (fun () ->
         invalid_enum ~at:4 ~field:[ "Proto" ] ~value:7 ~valid:[]));
  check "tag" "Proto@6: invalid tag: 3"
    (parse_error (fun () -> invalid_tag ~at:6 ~field:"Proto" 3));
  check "where" "@2: where clause violated"
    (parse_error (fun () -> where_clause ~at:2))

let test_encode () =
  check "too short" "Codec.encode E: buffer too short (need 4, got 1)"
    (invalid (fun () -> encode_too_short ~name:"E" ~size:4 ~len:3 2));
  check "past the end" "Codec.encode E: buffer too short (need 4, got -2)"
    (invalid (fun () -> encode_too_short ~name:"E" ~size:4 ~len:3 5));
  check "negative" "Codec.encode E: negative offset -3"
    (invalid (fun () -> encode_too_short ~name:"E" ~size:4 ~len:30 (-3)));
  check "rejected"
    "Codec.encode E: the encoded record is rejected by its own constraints \
     (@0: where clause violated)"
    (invalid (fun () ->
         encode_rejected ~name:"E"
           {
             at = 0;
             field = [];
             kind = Constraint_failed { which = Where; value = None };
           }));
  check "bits" "Wire.encode: value 16 does not fit an unsigned 4-bit field"
    (invalid (fun () -> unsigned_too_wide ~bits:4 16));
  check "enum" "Wire.encode: enum P expected one of [1, 6], got 2"
    (invalid (fun () -> enum_encode ~name:"P" ~valid:[ 1; 6 ] 2));
  Alcotest.(check bool)
    "byte length names both lengths" true
    (String.starts_with
       ~prefix:"Wire.encode: byte field expected 6 bytes, got 5"
       (invalid (fun () -> byte_length ~expected:6 ~actual:5)))

let suite =
  ( "errors",
    [
      Alcotest.test_case "frame" `Quick test_frame;
      Alcotest.test_case "fields" `Quick test_fields;
      Alcotest.test_case "encode" `Quick test_encode;
    ] )
