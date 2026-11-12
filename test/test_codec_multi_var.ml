(* Tests for codec.ml: several variable-size fields in one codec. *)

open Wire
open Test_helpers

(* -- Nested: Composition: optional + repeat + codec --
   TM-frame-like structure: header + data zone (repeat of packets) + optional
   OCF + optional FECF. [packet] / [packet_codec] live in {!Test_helpers}. *)

type tm_like = {
  hdr : UInt16.t;
  data_len : UInt8.t;
  packets : packet list;
  ocf : UInt32.t option;
  fecf : UInt16.t option;
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

let test_tm_like_full () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  (* hdr(2) + data_len(1) + 2 packets(6) + ocf(4) + fecf(2) = 15 *)
  let buf = Bytes.create 15 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 6;
  (* data zone = 6 bytes = 2 packets *)
  (* packet 0 *)
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  (* packet 1 *)
  Bytes.set_uint8 buf 6 0x02;
  Bytes.set_uint16_be buf 7 0x2222;
  (* ocf *)
  Bytes.set_int32_be buf 9 0x33333333l;
  (* fecf *)
  Bytes.set_uint16_be buf 13 0x4444;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "hdr" 0xAAAA (UInt16.to_int r.hdr);
  Alcotest.(check int) "data_len" 6 (UInt8.to_int r.data_len);
  Alcotest.(check int) "packet count" 2 (List.length r.packets);
  Alcotest.(check int) "pkt0.id" 0x01 (UInt8.to_int (List.nth r.packets 0).id);
  Alcotest.(check int) "pkt1.id" 0x02 (UInt8.to_int (List.nth r.packets 1).id);
  Alcotest.(check (option int))
    "ocf" (Some 0x33333333)
    (Option.map UInt32.to_int r.ocf);
  Alcotest.(check (option int))
    "fecf" (Some 0x4444)
    (Option.map UInt16.to_int r.fecf)

let test_tm_like_no_trailing () =
  let c = tm_like_codec ~ocf:false ~fecf:false in
  (* hdr(2) + data_len(1) + 1 packet(3) = 6 *)
  let buf = Bytes.create 6 in
  Bytes.set_uint16_be buf 0 0xAAAA;
  Bytes.set_uint8 buf 2 3;
  Bytes.set_uint8 buf 3 0x01;
  Bytes.set_uint16_be buf 4 0x1111;
  let r = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int) "packet count" 1 (List.length r.packets);
  Alcotest.(check (option int)) "ocf" None (Option.map UInt32.to_int r.ocf);
  Alcotest.(check (option int)) "fecf" None (Option.map UInt16.to_int r.fecf)

let test_tm_like_roundtrip () =
  let c = tm_like_codec ~ocf:true ~fecf:true in
  let original =
    {
      hdr = UInt16.v 0xBBBB;
      data_len = UInt8.v 9;
      packets =
        [
          ({ id = UInt8.v 0x0A; data = UInt16.v 0x000A } : packet);
          ({ id = UInt8.v 0x0B; data = UInt16.v 0x000B } : packet);
          ({ id = UInt8.v 0x0C; data = UInt16.v 0x000C } : packet);
        ];
      ocf = Some (Wire.Private.UInt32.of_int32 0xDEADBEEFl);
      fecf = Some (UInt16.v 0xCAFE);
    }
  in
  let buf = Bytes.create 18 in
  Codec.encode c original buf 0;
  let decoded = decode_ok (Codec.decode c buf 0) in
  Alcotest.(check int)
    "hdr"
    (UInt16.to_int original.hdr)
    (UInt16.to_int decoded.hdr);
  Alcotest.(check int) "packet count" 3 (List.length decoded.packets);
  List.iter2
    (fun (o : packet) (d : packet) ->
      Alcotest.(check int) "pkt.id" (UInt8.to_int o.id) (UInt8.to_int d.id);
      Alcotest.(check int)
        "pkt.data" (UInt16.to_int o.data) (UInt16.to_int d.data))
    original.packets decoded.packets;
  Alcotest.(check (option int32))
    "ocf"
    (Option.map UInt32.to_int32 original.ocf)
    (Option.map UInt32.to_int32 decoded.ocf);
  Alcotest.(check (option int))
    "fecf"
    (Option.map UInt16.to_int original.fecf)
    (Option.map UInt16.to_int decoded.fecf)

(* -- Multiple consecutive variable-size fields (CFDP-style) --

   CCSDS CFDP (727.0-B-5) has three consecutive variable-size byte_array
   fields in its PDU header, each sized by expressions over earlier fixed
   fields. This layout projects, resolving each field's offset at runtime. *)

type cfdp_hdr = {
  eid_len : UInt8.t;
  txseq_len : UInt8.t;
  src : string;
  txseq : string;
  dst : string;
}

let f_cfdp_eid_len = Field.v "EIDLen" uint8
let f_cfdp_txseq_len = Field.v "TxSeqLen" uint8

let f_cfdp_src =
  Field.v "SourceEID" (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))

let f_cfdp_txseq =
  Field.v "TxSeqNum"
    (byte_array ~size:Expr.(Field.ref f_cfdp_txseq_len + int 1))

let f_cfdp_dst =
  Field.v "DestEID" (byte_array ~size:Expr.(Field.ref f_cfdp_eid_len + int 1))

let cfdp_codec =
  let open Codec in
  v "CFDPHeader"
    (fun eid_len txseq_len src txseq dst ->
      { eid_len; txseq_len; src; txseq; dst })
    [
      (f_cfdp_eid_len $ fun r -> r.eid_len);
      (f_cfdp_txseq_len $ fun r -> r.txseq_len);
      (f_cfdp_src $ fun r -> r.src);
      (f_cfdp_txseq $ fun r -> r.txseq);
      (f_cfdp_dst $ fun r -> r.dst);
    ]

let test_multi_var_decode () =
  (* EIDLen=1 -> 2-byte entities, TxSeqLen=2 -> 3-byte txseq.
     Layout: [1] [2] [AA BB] [CC DD EE] [FF 00] *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let r = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check int) "eid_len" 1 (UInt8.to_int r.eid_len);
  Alcotest.(check int) "txseq_len" 2 (UInt8.to_int r.txseq_len);
  Alcotest.(check string) "src" "\xAA\xBB" r.src;
  Alcotest.(check string) "txseq" "\xCC\xDD\xEE" r.txseq;
  Alcotest.(check string) "dst" "\xFF\x00" r.dst

let test_multi_var_roundtrip () =
  let original =
    {
      eid_len = UInt8.v 1;
      txseq_len = UInt8.v 2;
      src = "\xAA\xBB";
      txseq = "\xCC\xDD\xEE";
      dst = "\xFF\x00";
    }
  in
  let buf = Bytes.create 9 in
  Codec.encode cfdp_codec original buf 0;
  let decoded = decode_ok (Codec.decode cfdp_codec buf 0) in
  Alcotest.(check string) "src roundtrip" original.src decoded.src;
  Alcotest.(check string) "txseq roundtrip" original.txseq decoded.txseq;
  Alcotest.(check string) "dst roundtrip" original.dst decoded.dst

let test_multi_var_get () =
  (* Staged get must work on the second and third variable-size fields. *)
  let buf = Bytes.create 9 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint8 buf 1 2;
  Bytes.blit_string "\xAA\xBB" 0 buf 2 2;
  Bytes.blit_string "\xCC\xDD\xEE" 0 buf 4 3;
  Bytes.blit_string "\xFF\x00" 0 buf 7 2;
  let cf_txseq = Codec.(f_cfdp_txseq $ fun r -> r.txseq) in
  let cf_dst = Codec.(f_cfdp_dst $ fun r -> r.dst) in
  let get_txseq = Staged.unstage (Codec.get cfdp_codec cf_txseq) in
  let get_dst = Staged.unstage (Codec.get cfdp_codec cf_dst) in
  Alcotest.(check string) "get txseq" "\xCC\xDD\xEE" (get_txseq buf 0);
  Alcotest.(check string) "get dst" "\xFF\x00" (get_dst buf 0)

let test_multi_var_fixed_after () =
  (* A fixed-size field after multiple variable-size fields must also work. *)
  let f_elen = Field.v "ELen" uint8 in
  let f_tlen = Field.v "TLen" uint8 in
  let codec =
    let open Codec in
    v "VarTrail"
      (fun elen tlen src txseq trail -> (elen, tlen, src, txseq, trail))
      [
        (f_elen $ fun (e, _, _, _, _) -> e);
        (f_tlen $ fun (_, t, _, _, _) -> t);
        ( Field.v "Src" (byte_array ~size:Expr.(Field.ref f_elen + int 1))
        $ fun (_, _, s, _, _) -> s );
        ( Field.v "Tx" (byte_array ~size:Expr.(Field.ref f_tlen + int 1))
        $ fun (_, _, _, t, _) -> t );
        (Field.v "Trail" uint16be $ fun (_, _, _, _, tr) -> tr);
      ]
  in
  (* elen=0 -> 1-byte src, tlen=1 -> 2-byte tx, trail=0xBEEF
     Layout: [0] [1] [AA] [BB CC] [BE EF] *)
  let buf = Bytes.create 7 in
  Bytes.set_uint8 buf 0 0;
  Bytes.set_uint8 buf 1 1;
  Bytes.set_uint8 buf 2 0xAA;
  Bytes.blit_string "\xBB\xCC" 0 buf 3 2;
  Bytes.set_uint16_be buf 5 0xBEEF;
  let _, _, src, tx, trail = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "src" "\xAA" src;
  Alcotest.(check string) "tx" "\xBB\xCC" tx;
  Alcotest.(check int) "trail" 0xBEEF (UInt16.to_int trail)

(* -- Multiple variable-size sub-codecs back-to-back (SSH disconnect /
      debug shape). [compile_codec] resolves a dynamic offset for a
      [Wire.codec]-embedded variable-size sub-codec that sits after another
      variable-size field. -- *)

module Slice = Bytesrw.Bytes.Slice

type ssh_string = { len : UInt32.t; data : Slice.t }

let ssh_f_len = Field.v "len" uint32be
let ssh_f_data = Field.v "data" (byte_slice ~size:(Field.ref ssh_f_len))

let ssh_string_codec =
  Codec.v "SshString"
    (fun len data -> { len; data })
    Codec.[ (ssh_f_len $ fun s -> s.len); (ssh_f_data $ fun s -> s.data) ]

let mk_ssh_string s =
  let b = Bytes.of_string s in
  {
    len = UInt32.of_int (String.length s);
    data = Slice.make b ~first:0 ~length:(Bytes.length b);
  }

let test_ssh_two_var_slices () =
  (* SSH_MSG_DISCONNECT: reason (uint32) + len + desc + len + lang *)
  let f_reason = Field.v "reason" uint32be in
  let f_desc_len = Field.v "desc_len" uint32be in
  let f_desc = Field.v "desc" (byte_slice ~size:(Field.ref f_desc_len)) in
  let f_lang_len = Field.v "lang_len" uint32be in
  let f_lang = Field.v "lang" (byte_slice ~size:(Field.ref f_lang_len)) in
  let codec =
    let open Codec in
    v "Disconnect"
      (fun reason _ desc _ lang -> (reason, desc, lang))
      [
        (f_reason $ fun (r, _, _) -> r);
        (f_desc_len $ fun (_, d, _) -> UInt32.of_int (Slice.length d));
        (f_desc $ fun (_, d, _) -> d);
        (f_lang_len $ fun (_, _, l) -> UInt32.of_int (Slice.length l));
        (f_lang $ fun (_, _, l) -> l);
      ]
  in
  let v =
    (UInt32.of_int 11, (mk_ssh_string "bye").data, (mk_ssh_string "en-US").data)
  in
  let buf = Bytes.create 200 in
  Codec.encode codec v buf 0;
  let r, d, l = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "reason" 11 (UInt32.to_int r);
  Alcotest.(check string) "desc" "bye" (Slice.to_string d);
  Alcotest.(check string) "lang" "en-US" (Slice.to_string l)

let test_two_var_codecs_embedded () =
  (* Two consecutive [Wire.codec ssh_string_codec] embedded fields, each
     resolved via a dynamic offset. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let pair_codec =
    let open Codec in
    v "Pair" (fun a b -> (a, b)) [ f_a $ fst; f_b $ snd ]
  in
  let v = (mk_ssh_string "abcd", mk_ssh_string "xy") in
  let buf = Bytes.create 200 in
  Codec.encode pair_codec v buf 0;
  let a, b = decode_ok (Codec.decode pair_codec buf 0) in
  Alcotest.(check int) "a.len" 4 (UInt32.to_int a.len);
  Alcotest.(check string) "a.data" "abcd" (Slice.to_string a.data);
  Alcotest.(check int) "b.len" 2 (UInt32.to_int b.len);
  Alcotest.(check string) "b.data" "xy" (Slice.to_string b.data)

let test_three_var_codecs_embedded () =
  (* SSH_MSG_DEBUG-shaped payload: three variable-size sub-codecs. *)
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let f_c = Field.v "c" (codec ssh_string_codec) in
  let triple_codec =
    let open Codec in
    v "Triple"
      (fun a b c -> (a, b, c))
      [
        (f_a $ fun (a, _, _) -> a);
        (f_b $ fun (_, b, _) -> b);
        (f_c $ fun (_, _, c) -> c);
      ]
  in
  let v = (mk_ssh_string "alpha", mk_ssh_string "be", mk_ssh_string "gamma!") in
  let buf = Bytes.create 200 in
  Codec.encode triple_codec v buf 0;
  let a, b, c = decode_ok (Codec.decode triple_codec buf 0) in
  Alcotest.(check string) "a" "alpha" (Slice.to_string a.data);
  Alcotest.(check string) "b" "be" (Slice.to_string b.data);
  Alcotest.(check string) "c" "gamma!" (Slice.to_string c.data)

let test_four_var_codecs_embedded () =
  let f_a = Field.v "a" (codec ssh_string_codec) in
  let f_b = Field.v "b" (codec ssh_string_codec) in
  let f_c = Field.v "c" (codec ssh_string_codec) in
  let f_d = Field.v "d" (codec ssh_string_codec) in
  let quad_codec =
    let open Codec in
    v "Quad"
      (fun a b c d -> (a, b, c, d))
      [
        (f_a $ fun (a, _, _, _) -> a);
        (f_b $ fun (_, b, _, _) -> b);
        (f_c $ fun (_, _, c, _) -> c);
        (f_d $ fun (_, _, _, d) -> d);
      ]
  in
  let v =
    ( mk_ssh_string "alpha",
      mk_ssh_string "bravo",
      mk_ssh_string "charlie",
      mk_ssh_string "d" )
  in
  let len = Codec.size_of_value quad_codec v in
  let buf = Bytes.create len in
  Codec.encode quad_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at quad_codec buf 0);
  let a, b, c, d = decode_ok (Codec.decode quad_codec buf 0) in
  Alcotest.(check string) "a" "alpha" (Slice.to_string a.data);
  Alcotest.(check string) "b" "bravo" (Slice.to_string b.data);
  Alcotest.(check string) "c" "charlie" (Slice.to_string c.data);
  Alcotest.(check string) "d" "d" (Slice.to_string d.data)

let test_slice_then_array () =
  let f_slice_len = Field.v "slice_len" uint8 in
  let f_slice = Field.v "slice" (byte_slice ~size:(Field.ref f_slice_len)) in
  let f_array_len = Field.v "array_len" uint8 in
  let f_array = Field.v "array" (byte_array ~size:(Field.ref f_array_len)) in
  let mixed_codec =
    let open Codec in
    v "SliceThenArray"
      (fun _ slice _ array -> (slice, array))
      [
        (f_slice_len $ fun (slice, _) -> UInt8.v (Slice.length slice));
        (f_slice $ fun (slice, _) -> slice);
        (f_array_len $ fun (_, array) -> UInt8.v (String.length array));
        (f_array $ fun (_, array) -> array);
      ]
  in
  let v = ((mk_ssh_string "slice").data, "array-data") in
  let len = Codec.size_of_value mixed_codec v in
  let buf = Bytes.create len in
  Codec.encode mixed_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at mixed_codec buf 0);
  let slice, array = decode_ok (Codec.decode mixed_codec buf 0) in
  Alcotest.(check string) "slice" "slice" (Slice.to_string slice);
  Alcotest.(check string) "array" "array-data" array

let test_codec_then_array () =
  let f_msg = Field.v "msg" (codec ssh_string_codec) in
  let f_array_len = Field.v "array_len" uint8 in
  let f_array = Field.v "array" (byte_array ~size:(Field.ref f_array_len)) in
  let mixed_codec =
    let open Codec in
    v "CodecThenArray"
      (fun msg _ array -> (msg, array))
      [
        (f_msg $ fun (msg, _) -> msg);
        (f_array_len $ fun (_, array) -> UInt8.v (String.length array));
        (f_array $ fun (_, array) -> array);
      ]
  in
  let v = (mk_ssh_string "message", "tail") in
  let len = Codec.size_of_value mixed_codec v in
  let buf = Bytes.create len in
  Codec.encode mixed_codec v buf 0;
  Alcotest.(check int) "wire_size_at" len (Codec.wire_size_at mixed_codec buf 0);
  let msg, array = decode_ok (Codec.decode mixed_codec buf 0) in
  Alcotest.(check string) "msg" "message" (Slice.to_string msg.data);
  Alcotest.(check string) "array" "tail" array

let test_repeat_after_var_slice () =
  (* [Repeat] after a variable-size field resolves a dynamic offset in
     [compile_repeat], like [compile_codec]. *)
  let f_prefix_len = Field.v "prefix_len" uint8 in
  let f_prefix = Field.v "prefix" (byte_slice ~size:(Field.ref f_prefix_len)) in
  let f_count = Field.v "count" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_count) Wire.uint16be in
  let codec =
    let open Codec in
    v "PrefixedItems"
      (fun _ prefix _ items -> (prefix, items))
      [
        (f_prefix_len $ fun (p, _) -> UInt8.v (Slice.length p));
        (f_prefix $ fun (p, _) -> p);
        (f_count $ fun (_, items) -> UInt8.v (2 * List.length items));
        (f_items $ fun (_, items) -> items);
      ]
  in
  let buf = Bytes.create 200 in
  let v =
    ((mk_ssh_string "PREFIX").data, List.map UInt16.v [ 0x0102; 0x0304; 0x0506 ])
  in
  Codec.encode codec v buf 0;
  let prefix, items = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check string) "prefix" "PREFIX" (Slice.to_string prefix);
  Alcotest.(check (list int))
    "items" [ 0x0102; 0x0304; 0x0506 ]
    (List.map UInt16.to_int items)

let suite =
  ( "codec_multi_var",
    [
      Alcotest.test_case "composition: tm-like full" `Quick test_tm_like_full;
      Alcotest.test_case "composition: tm-like no trailing" `Quick
        test_tm_like_no_trailing;
      Alcotest.test_case "composition: tm-like roundtrip" `Quick
        test_tm_like_roundtrip;
      Alcotest.test_case "multi-var: decode" `Quick test_multi_var_decode;
      Alcotest.test_case "multi-var: roundtrip" `Quick test_multi_var_roundtrip;
      Alcotest.test_case "multi-var: get" `Quick test_multi_var_get;
      Alcotest.test_case "multi-var: fixed after" `Quick
        test_multi_var_fixed_after;
      Alcotest.test_case "multi-var: ssh disconnect (two var byte_slice)" `Quick
        test_ssh_two_var_slices;
      Alcotest.test_case "multi-var: two embedded sub-codecs" `Quick
        test_two_var_codecs_embedded;
      Alcotest.test_case "multi-var: three embedded sub-codecs" `Quick
        test_three_var_codecs_embedded;
      Alcotest.test_case "multi-var: four embedded sub-codecs" `Quick
        test_four_var_codecs_embedded;
      Alcotest.test_case "multi-var: byte_slice then byte_array" `Quick
        test_slice_then_array;
      Alcotest.test_case "multi-var: sub-codec then byte_array" `Quick
        test_codec_then_array;
      Alcotest.test_case "multi-var: repeat after variable byte_slice" `Quick
        test_repeat_after_var_slice;
    ] )
