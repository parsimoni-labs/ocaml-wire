(** Fuzz tests for schema roundtrips.

    These tests verify that our OCaml encoder/decoder round-trips correctly.
    When EverParse C integration is available, we can add differential tests. *)

module Cr = Crowbar
open D3t

let truncate buf =
  let max_len = 256 in
  if String.length buf > max_len then String.sub buf 0 max_len else buf

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

(** Test SimpleHeader roundtrip *)
let test_simple_header_roundtrip version length flags =
  let version = abs version mod 256 in
  let length = abs length mod 65536 in
  let flags = abs flags mod 256 in
  let original = Schema.{ version; length; flags } in
  match encode_record_to_string Schema.simple_header_codec original with
  | Error _ -> Cr.fail "encode failed"
  | Ok encoded -> (
      match decode_record_from_string Schema.simple_header_codec encoded with
      | Ok decoded ->
          if original.version <> decoded.version then Cr.fail "version mismatch";
          if original.length <> decoded.length then Cr.fail "length mismatch";
          if original.flags <> decoded.flags then Cr.fail "flags mismatch"
      | Error _ -> Cr.fail "decode failed")

(** Test SimpleHeader decode crash safety *)
let test_simple_header_crash buf =
  let buf = truncate buf in
  let _ = decode_record_from_string Schema.simple_header_codec buf in
  ()

(** Test ConstrainedPacket roundtrip with valid values *)
let test_constrained_packet_roundtrip pkt_type pkt_length =
  let pkt_type = abs pkt_type mod 4 in
  let pkt_length = abs pkt_length mod 1025 in
  let original = Schema.{ pkt_type; pkt_length } in
  match encode_record_to_string Schema.constrained_packet_codec original with
  | Error _ -> Cr.fail "encode failed"
  | Ok encoded -> (
      match
        decode_record_from_string Schema.constrained_packet_codec encoded
      with
      | Ok decoded ->
          if original.pkt_type <> decoded.pkt_type then
            Cr.fail "pkt_type mismatch";
          if original.pkt_length <> decoded.pkt_length then
            Cr.fail "pkt_length mismatch"
      | Error _ -> Cr.fail "decode failed")

(** Test ConstrainedPacket decode crash safety *)
let test_constrained_packet_crash buf =
  let buf = truncate buf in
  let _ = decode_record_from_string Schema.constrained_packet_codec buf in
  ()

let () =
  Cr.add_test ~name:"schema: simple_header roundtrip" [ Cr.int; Cr.int; Cr.int ]
    test_simple_header_roundtrip;
  Cr.add_test ~name:"schema: simple_header crash" [ Cr.bytes ]
    test_simple_header_crash;
  Cr.add_test ~name:"schema: constrained_packet roundtrip" [ Cr.int; Cr.int ]
    test_constrained_packet_roundtrip;
  Cr.add_test ~name:"schema: constrained_packet crash" [ Cr.bytes ]
    test_constrained_packet_crash
