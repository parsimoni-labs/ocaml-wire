(** Tests for Wire_diff — differential testing logic. *)

open Wire

let simple_codec =
  Codec.view "Simple"
    (fun v l -> (v, l))
    Codec.[ Codec.field "Version" uint8 fst; Codec.field "Length" uint16be snd ]

(* Mock C functions that behave identically to OCaml *)
let mock_c_read buf =
  if String.length buf < 3 then None else Some (String.sub buf 0 3)

let mock_c_write buf = if String.length buf < 3 then None else Some buf

let mk_schema () =
  Wire_diff.schema ~name:"Simple" ~codec:simple_codec ~c_read:mock_c_read
    ~c_write:mock_c_write ~equal:(fun (a1, a2) (b1, b2) -> a1 = b1 && a2 = b2)

let test_read_match () =
  let s = mk_schema () in
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 256;
  let result = Wire_diff.read s (Bytes.unsafe_to_string buf) in
  Alcotest.(check bool) "match" true (result = Wire_diff.Match)

let test_read_both_failed () =
  let s = mk_schema () in
  let result = Wire_diff.read s "" in
  Alcotest.(check bool) "both_failed" true (result = Wire_diff.Both_failed)

let test_roundtrip_struct () =
  let s = struct_ "Simple" [ field "Version" uint8; field "Length" uint16be ] in
  let buf = "\x01\x01\x00" in
  match Wire_diff.roundtrip_struct s buf with
  | Ok rt -> Alcotest.(check string) "roundtrip" buf rt
  | Error _ -> Alcotest.fail "roundtrip failed"

let test_write () =
  let s = mk_schema () in
  let v = (7, 1000) in
  let result = Wire_diff.write s v in
  (* mock_c_write accepts any 3-byte buffer, so a valid 3-byte encode matches *)
  Alcotest.(check bool) "write result" true (result = Wire_diff.Match)

let test_full_roundtrip () =
  let s = mk_schema () in
  let v = (3, 512) in
  let result = Wire_diff.full_roundtrip s v in
  Alcotest.(check bool) "full_roundtrip result" true (result = Wire_diff.Match)

let test_pack () =
  let s = mk_schema () in
  let pt = Wire_diff.pack s ~wire_size:3 in
  Alcotest.(check string) "pack name" "Simple" pt.Wire_diff.name;
  Alcotest.(check int) "pack wire_size" 3 pt.Wire_diff.wire_size;
  (* test_read on a valid 3-byte buffer should match *)
  let buf = "\x01\x01\x00" in
  let result = pt.Wire_diff.test_read buf in
  Alcotest.(check bool) "pack test_read" true (result = Wire_diff.Match)

let suite =
  ( "wire_diff",
    [
      Alcotest.test_case "read match" `Quick test_read_match;
      Alcotest.test_case "read both failed" `Quick test_read_both_failed;
      Alcotest.test_case "roundtrip struct" `Quick test_roundtrip_struct;
      Alcotest.test_case "write" `Quick test_write;
      Alcotest.test_case "full_roundtrip" `Quick test_full_roundtrip;
      Alcotest.test_case "pack" `Quick test_pack;
    ] )
