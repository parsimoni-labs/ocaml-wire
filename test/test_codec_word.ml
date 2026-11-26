(* Tests for codec.ml: [Codec.read_word] and [Codec.of_word] read what [get]
   reads. *)

open Wire
open Test_helpers

let expect_invalid label f =
  match f () with
  | () -> Alcotest.failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

(* A 6-byte header: a 16-bit word of bitfields, a byte, a 16-bit integer and a
   byte of bitfields. *)
type hdr = {
  ver : int;
  id : int;
  seq : UInt8.t;
  len : UInt16.t;
  hi : int;
  lo : int;
}

let h_ver = Codec.(Field.v "Ver" (bits ~width:3 U16be) $ fun r -> r.ver)
let h_id = Codec.(Field.v "Id" (bits ~width:13 U16be) $ fun r -> r.id)
let h_seq = Codec.(Field.v "Seq" uint8 $ fun r -> r.seq)
let h_len = Codec.(Field.v "Len" uint16be $ fun r -> r.len)
let h_hi = Codec.(Field.v "Hi" (bits ~width:2 U8) $ fun r -> r.hi)
let h_lo = Codec.(Field.v "Lo" (bits ~width:6 U8) $ fun r -> r.lo)

let hdr_codec =
  Codec.v "WordHdr"
    (fun ver id seq len hi lo -> { ver; id; seq; len; hi; lo })
    Codec.[ h_ver; h_id; h_seq; h_len; h_hi; h_lo ]

(* Signed integers and a 32-bit word of bitfields, 7 bytes. *)
type signed = { s8 : SInt8.t; s16 : SInt16.t; top : int; low : int }

let g_s8 = Codec.(Field.v "S8" int8 $ fun r -> r.s8)
let g_s16 = Codec.(Field.v "S16" int16be $ fun r -> r.s16)
let g_top = Codec.(Field.v "Top" (bits ~width:9 U32be) $ fun r -> r.top)
let g_low = Codec.(Field.v "Low" (bits ~width:23 U32be) $ fun r -> r.low)

let signed_codec =
  Codec.v "WordSigned"
    (fun s8 s16 top low -> { s8; s16; top; low })
    Codec.[ g_s8; g_s16; g_top; g_low ]

(* Every field read through a word equals the field [get] reads, at every
   offset of a random buffer: from 8 bytes before its end, where one 8-byte
   load reads the record, to the last offset that holds it. *)
let check_parts codec parts =
  let size = Codec.wire_size codec in
  let rand = Random.State.make [| 3 |] in
  let buf = Bytes.init 64 (fun _ -> Char.chr (Random.State.int rand 256)) in
  let read = Codec.read_word (Codec.word_reader codec) in
  for off = 0 to Bytes.length buf - size do
    let w = read buf off in
    List.iter (fun check -> check buf off w) parts
  done

let same codec f pp =
  let get = reader codec f and p = Codec.word_part codec f in
  fun buf off w ->
    let expected = get buf off and got = Codec.of_word p w in
    if expected <> got then
      Alcotest.failf "offset %d: get %a, of_word %a" off pp expected pp got

let pp_int = Fmt.int
let pp_u8 ppf v = Fmt.int ppf (UInt8.to_int v)
let pp_u16 ppf v = Fmt.int ppf (UInt16.to_int v)
let pp_s8 ppf v = Fmt.int ppf (SInt8.to_int v)
let pp_s16 ppf v = Fmt.int ppf (SInt16.to_int v)

let test_hdr_equals_get () =
  if Sys.int_size < 63 then
    expect_invalid "6 bytes on a narrow int" (fun () ->
        ignore (Codec.word_reader hdr_codec : hdr Codec.word_reader))
  else
    check_parts hdr_codec
      [
        same hdr_codec h_ver pp_int;
        same hdr_codec h_id pp_int;
        same hdr_codec h_seq pp_u8;
        same hdr_codec h_len pp_u16;
        same hdr_codec h_hi pp_int;
        same hdr_codec h_lo pp_int;
      ]

let test_signed_equals_get () =
  if Sys.int_size >= 63 then
    check_parts signed_codec
      [
        same signed_codec g_s8 pp_s8;
        same signed_codec g_s16 pp_s16;
        same signed_codec g_top pp_int;
        same signed_codec g_low pp_int;
      ]

let test_out_of_bounds () =
  if Sys.int_size >= 63 then begin
    let read = Codec.read_word (Codec.word_reader hdr_codec) in
    let buf = Bytes.make 10 '\000' in
    expect_invalid "negative offset" (fun () -> ignore (read buf (-1)));
    expect_invalid "past the end" (fun () -> ignore (read buf 5));
    ignore (read buf 4 : hdr Codec.word)
  end

type wide = { a : UInt32.t; b : UInt32.t }

let w_a = Codec.(Field.v "A" uint32be $ fun r -> r.a)
let w_b = Codec.(Field.v "B" uint32be $ fun r -> r.b)
let wide_codec = Codec.v "WordWide" (fun a b -> { a; b }) Codec.[ w_a; w_b ]

type le = { x : UInt16.t; y : UInt8.t }

let l_x = Codec.(Field.v "X" uint16 $ fun r -> r.x)
let l_y = Codec.(Field.v "Y" uint8 $ fun r -> r.y)
let le_codec = Codec.v "WordLe" (fun x y -> { x; y }) Codec.[ l_x; l_y ]

let test_staging_refusals () =
  expect_invalid "8 bytes" (fun () ->
      ignore (Codec.word_reader wide_codec : wide Codec.word_reader));
  expect_invalid "a part of 8 bytes" (fun () ->
      ignore (Codec.word_part wide_codec w_a));
  expect_invalid "a little-endian integer" (fun () ->
      ignore (Codec.word_part le_codec l_x));
  let lookalike = Codec.(Field.v "Y" uint8 $ fun r -> r.y) in
  expect_invalid "a field the codec does not hold" (fun () ->
      ignore (Codec.word_part le_codec lookalike));
  let var_len = Field.v "N" uint8 in
  let var_codec =
    Codec.v "WordVar"
      (fun n s -> (n, s))
      Codec.
        [
          (var_len $ fun (n, _) -> n);
          (Field.v "S" (byte_array ~size:(Field.ref var_len)) $ fun (_, s) -> s);
        ]
  in
  expect_invalid "a variable-size codec" (fun () ->
      ignore
        (Codec.word_reader var_codec : (UInt8.t * string) Codec.word_reader))

let per_call_words f =
  ignore (Sys.opaque_identity (f ()));
  let iters = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    ignore (Sys.opaque_identity (f ()))
  done;
  let after = Gc.minor_words () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters))

let test_no_allocation () =
  skip_unless_gc_counters ();
  if Sys.int_size >= 63 then begin
    let read = Codec.read_word (Codec.word_reader hdr_codec) in
    let p = Codec.word_part hdr_codec h_id in
    let buf = Bytes.make 16 '\007' in
    Alcotest.(check int)
      "word and of_word allocate nothing" 0
      (per_call_words (fun () ->
           Codec.of_word p (read buf 0) + Codec.of_word p (read buf 10)))
  end

let suite =
  ( "codec_word",
    [
      Alcotest.test_case "header: of_word = get" `Quick test_hdr_equals_get;
      Alcotest.test_case "signed and 32-bit word: of_word = get" `Quick
        test_signed_equals_get;
      Alcotest.test_case "out of bounds" `Quick test_out_of_bounds;
      Alcotest.test_case "staging refusals" `Quick test_staging_refusals;
      Alcotest.test_case "word allocates nothing" `Quick test_no_allocation;
    ] )
