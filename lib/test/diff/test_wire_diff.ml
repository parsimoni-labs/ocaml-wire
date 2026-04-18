(** Tests for Wire_diff -- differential testing logic. *)

open Wire

let f_diff_version = Field.v "Version" uint8
let f_diff_length = Field.v "Length" uint16be

let simple_codec =
  let open Codec in
  v "Simple" (fun v l -> (v, l)) [ f_diff_version $ fst; f_diff_length $ snd ]

let encode_simple v =
  let buf = Bytes.create (Codec.wire_size simple_codec) in
  Codec.encode simple_codec v buf 0;
  Bytes.unsafe_to_string buf

(* Mock C functions that behave identically to OCaml *)
let mock_c_read buf =
  if String.length buf < 3 then None else Some (String.sub buf 0 3)

let mock_c_write buf = if String.length buf < 3 then None else Some buf

let v ~name ?(codec = simple_codec) ~read ~write ~project ~equal ?ocaml_read ()
    =
  Wire_diff.harness ~name ~codec ~read ~write ~project ~equal ?ocaml_read ()

let mk_schema () =
  v ~name:"Simple" ~read:mock_c_read ~write:mock_c_write ~project:encode_simple
    ~equal:String.equal ()

let test_read_match () =
  let s = mk_schema () in
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 256;
  let result = s.Wire_diff.test_read (Bytes.unsafe_to_string buf) in
  Alcotest.(check bool) "match" true (result = Wire_diff.Match)

let test_read_both_failed () =
  let s = mk_schema () in
  let result = s.Wire_diff.test_read "" in
  Alcotest.(check bool) "both_failed" true (result = Wire_diff.Both_failed)

let test_read_ocaml_override () =
  let c_read buf = if String.length buf < 3 then None else Some "override" in
  let ocaml_read buf =
    if String.length buf < 3 then None else Some "override"
  in
  let s =
    v ~name:"Override" ~read:c_read ~write:mock_c_write ~project:encode_simple
      ~equal:String.equal ~ocaml_read ()
  in
  let result = s.Wire_diff.test_read (encode_simple (7, 1000)) in
  Alcotest.(check bool) "read uses override" true (result = Wire_diff.Match)

let test_write () =
  let s = mk_schema () in
  let buf = encode_simple (7, 1000) in
  let result = s.Wire_diff.test_write buf in
  Alcotest.(check bool) "write result" true (result = Wire_diff.Match)

let test_write_ocaml_override () =
  let c_write _ = Some "abc" in
  let ocaml_read buf = if buf = "abc" then Some "override" else None in
  let s =
    v ~name:"OverrideWrite" ~read:mock_c_read ~write:c_write
      ~project:(fun _ -> "override")
      ~equal:String.equal ~ocaml_read ()
  in
  let buf = encode_simple (7, 1000) in
  let result = s.Wire_diff.test_write buf in
  Alcotest.(check bool) "write uses override" true (result = Wire_diff.Match)

let test_full_roundtrip () =
  let s = mk_schema () in
  let buf = encode_simple (3, 512) in
  let result = s.Wire_diff.test_roundtrip buf in
  Alcotest.(check bool) "full_roundtrip result" true (result = Wire_diff.Match)

let test_full_roundtrip_c_rejects () =
  let c_write_reject _ = None in
  let s =
    v ~name:"CReject" ~read:mock_c_read ~write:c_write_reject
      ~project:encode_simple ~equal:String.equal ()
  in
  let buf = encode_simple (3, 512) in
  let result = s.Wire_diff.test_roundtrip buf in
  Alcotest.(check bool)
    "c rejects -> Only_ocaml_ok" true
    (result = Wire_diff.Only_ocaml_ok "External write failed")

let test_write_c_rejects () =
  let c_write_reject _ = None in
  let s =
    v ~name:"CRejectW" ~read:mock_c_read ~write:c_write_reject
      ~project:encode_simple ~equal:String.equal ()
  in
  let buf = encode_simple (3, 512) in
  let result = s.Wire_diff.test_write buf in
  Alcotest.(check bool)
    "write c rejects -> Only_ocaml_ok" true
    (result = Wire_diff.Only_ocaml_ok "External write failed")

let projection_codec =
  let open Codec in
  v "Projection"
    (fun version _length -> version)
    [ f_diff_version $ Fun.id; (f_diff_length $ fun _ -> 0) ]

let projection_read buf =
  if String.length buf < 3 then None else Some (String.get_uint8 buf 0)

let projection_write version =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 version;
  Bytes.set_uint16_be buf 1 0;
  Some (Bytes.unsafe_to_string buf)

let mk_projection () =
  Wire_diff.harness ~name:"Projection" ~codec:projection_codec
    ~read:projection_read ~write:projection_write ~project:Fun.id
    ~equal:Int.equal ()

let test_projection_read () =
  let s = mk_projection () in
  let buf = "\x09\x12\x34" in
  let result = s.Wire_diff.test_read buf in
  Alcotest.(check bool) "projection read" true (result = Wire_diff.Match)

let test_projection_roundtrip () =
  let s = mk_projection () in
  let buf = encode_simple (7, 0) in
  let result = s.Wire_diff.test_roundtrip buf in
  Alcotest.(check bool) "projection roundtrip" true (result = Wire_diff.Match)

let test_harness () =
  let s =
    Wire_diff.harness ~name:"Simple" ~codec:simple_codec ~read:mock_c_read
      ~write:mock_c_write ~project:encode_simple ~equal:String.equal ()
  in
  Alcotest.(check string) "name" "Simple" s.Wire_diff.name;
  Alcotest.(check int) "wire_size" 3 s.Wire_diff.wire_size;
  let buf = "\x01\x01\x00" in
  let result = s.Wire_diff.test_read buf in
  Alcotest.(check bool) "test_read" true (result = Wire_diff.Match)

let suite =
  ( "wire_diff",
    [
      Alcotest.test_case "read match" `Quick test_read_match;
      Alcotest.test_case "read both failed" `Quick test_read_both_failed;
      Alcotest.test_case "read uses ocaml_read override" `Quick
        test_read_ocaml_override;
      Alcotest.test_case "write" `Quick test_write;
      Alcotest.test_case "write uses ocaml_read override" `Quick
        test_write_ocaml_override;
      Alcotest.test_case "full_roundtrip" `Quick test_full_roundtrip;
      Alcotest.test_case "full_roundtrip: c rejects" `Quick
        test_full_roundtrip_c_rejects;
      Alcotest.test_case "write: c rejects" `Quick test_write_c_rejects;
      Alcotest.test_case "projection read" `Quick test_projection_read;
      Alcotest.test_case "projection roundtrip" `Quick test_projection_roundtrip;
      Alcotest.test_case "harness" `Quick test_harness;
    ] )
