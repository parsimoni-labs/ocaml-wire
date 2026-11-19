(* Tests for codec.ml: [Codec.writer] writes what [encode] and [set] write. *)

open Wire
open Test_helpers

(* A 6-byte header: a 16-bit word of bitfields, a byte, a 16-bit integer and a
   byte of bitfields, every field on the path that puts the record together in
   an [int]. *)
type small = {
  ver : int;
  id : int;
  seq : UInt8.t;
  len : UInt16.t;
  hi : int;
  lo : int;
}

let s_ver = Codec.(Field.v "Ver" (bits ~width:3 U16be) $ fun r -> r.ver)
let s_id = Codec.(Field.v "Id" (bits ~width:13 U16be) $ fun r -> r.id)
let s_seq = Codec.(Field.v "Seq" uint8 $ fun r -> r.seq)
let s_len = Codec.(Field.v "Len" uint16be $ fun r -> r.len)
let s_hi = Codec.(Field.v "Hi" (bits ~width:2 U8) $ fun r -> r.hi)
let s_lo = Codec.(Field.v "Lo" (bits ~width:6 U8) $ fun r -> r.lo)

let small_codec =
  Codec.v "WriterSmall"
    (fun ver id seq len hi lo -> { ver; id; seq; len; hi; lo })
    Codec.[ s_ver; s_id; s_seq; s_len; s_hi; s_lo ]

let small_template =
  {
    ver = 5;
    id = 0x1abc;
    seq = UInt8.v 9;
    len = UInt16.v 0x1234;
    hi = 1;
    lo = 7;
  }

(* A 9-byte record with a little-endian integer: too long for one [int], so
   the writer copies the template and sets each field. *)
type big = { a : UInt16.t; b : SInt32.t; c : int; d : UInt16.t }

let b_a = Codec.(Field.v "A" uint16 $ fun r -> r.a)
let b_b = Codec.(Field.v "B" int32be $ fun r -> r.b)
let b_c = Codec.(Field.v "C" (bits ~width:5 U8) $ fun r -> r.c)
let b_d = Codec.(Field.v "D" uint16be $ fun r -> r.d)

let big_codec =
  Codec.v "WriterBig"
    (fun a b c d -> { a; b; c; d })
    Codec.[ b_a; b_b; b_c; b_d ]

let big_template =
  { a = UInt16.v 1; b = SInt32.of_int (-2); c = 3; d = UInt16.v 4 }

(* A 32-bit word of bitfields, as a CLCW is. *)
type word = { kind : int; flag : int; count : int; tag : int }

let w_a = Codec.(Field.v "WA" (bits ~width:7 U32be) $ fun r -> r.kind)
let w_b = Codec.(Field.v "WB" (bits ~width:1 U32be) $ fun r -> r.flag)
let w_c = Codec.(Field.v "WC" (bits ~width:16 U32be) $ fun r -> r.count)
let w_d = Codec.(Field.v "WD" (bits ~width:8 U32be) $ fun r -> r.tag)

let word_codec =
  Codec.v "WriterWord"
    (fun kind flag count tag -> { kind; flag; count; tag })
    Codec.[ w_a; w_b; w_c; w_d ]

let word_template = { kind = 0x55; flag = 1; count = 0xBEEF; tag = 0x7E }

(* [template] at [off], then each [set], as the writer must write it. *)
let expected codec template sets =
  let buf = Bytes.make 16 '\xAA' in
  Codec.encode codec template buf 3;
  List.iter (fun set -> set buf 3) sets;
  Bytes.to_string buf

let written write =
  let buf = Bytes.make 16 '\xAA' in
  write buf 3;
  Bytes.to_string buf

let set codec f v buf off = Staged.unstage (Codec.set codec f) buf off v

let test_small_equals_set () =
  let w3 =
    Staged.unstage
      (Codec.writer small_codec small_template [ s_id; s_len; s_lo ])
  in
  let rand = Random.State.make [| 7 |] in
  for _ = 1 to 1000 do
    let id = Random.State.int rand 0x2000
    and len = UInt16.v (Random.State.int rand 0x10000)
    and lo = Random.State.int rand 64 in
    Alcotest.(check string)
      "writer = encode + set"
      (expected small_codec small_template
         [
           set small_codec s_id id;
           set small_codec s_len len;
           set small_codec s_lo lo;
         ])
      (written (fun buf off -> w3 buf off id len lo))
  done

let test_small_arities () =
  let t = small_template in
  let w0 = Staged.unstage (Codec.writer small_codec t []) in
  Alcotest.(check string)
    "no field writes the template"
    (expected small_codec t [])
    (written (fun buf off -> w0 buf off));
  let w1 = Staged.unstage (Codec.writer small_codec t [ s_seq ]) in
  Alcotest.(check string)
    "one field"
    (expected small_codec t [ set small_codec s_seq (UInt8.v 200) ])
    (written (fun buf off -> w1 buf off (UInt8.v 200)));
  let w2 = Staged.unstage (Codec.writer small_codec t [ s_lo; s_ver ]) in
  Alcotest.(check string)
    "two fields, out of order"
    (expected small_codec t
       [ set small_codec s_lo 63; set small_codec s_ver 0 ])
    (written (fun buf off -> w2 buf off 63 0));
  let w4 =
    Staged.unstage (Codec.writer small_codec t [ s_ver; s_id; s_seq; s_hi ])
  in
  Alcotest.(check string)
    "four fields"
    (expected small_codec t
       [
         set small_codec s_ver 7;
         set small_codec s_id 1;
         set small_codec s_seq (UInt8.v 0);
         set small_codec s_hi 3;
       ])
    (written (fun buf off -> w4 buf off 7 1 (UInt8.v 0) 3));
  let w6 =
    Staged.unstage
      (Codec.writer small_codec t [ s_ver; s_id; s_seq; s_len; s_hi; s_lo ])
  in
  Alcotest.(check string)
    "every field"
    (expected small_codec
       { ver = 2; id = 3; seq = UInt8.v 4; len = UInt16.v 5; hi = 2; lo = 1 }
       [])
    (written (fun buf off -> w6 buf off 2 3 (UInt8.v 4) (UInt16.v 5) 2 1))

let test_big_equals_set () =
  let w =
    Staged.unstage (Codec.writer big_codec big_template [ b_a; b_b; b_c ])
  in
  let b = SInt32.of_int (-70_000) in
  Alcotest.(check string)
    "copy and set"
    (expected big_codec big_template
       [
         set big_codec b_a (UInt16.v 0xBEEF);
         set big_codec b_b b;
         set big_codec b_c 31;
       ])
    (written (fun buf off -> w buf off (UInt16.v 0xBEEF) b 31))

let test_word_equals_set () =
  let w =
    Staged.unstage (Codec.writer word_codec word_template [ w_d; w_b; w_c ])
  in
  Alcotest.(check string)
    "32-bit word"
    (expected word_codec word_template
       [ set word_codec w_d 0xFF; set word_codec w_b 0; set word_codec w_c 1 ])
    (written (fun buf off -> w buf off 0xFF 0 1))

let expect_invalid label f =
  match f () with
  | () -> Alcotest.failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

(* A value too wide for its bitfield is refused as [set] refuses it. A record
   put together in an [int], which needs an [int] of 63 bits, is written after
   every check, so nothing is written then. *)
let test_too_wide () =
  let w =
    Staged.unstage (Codec.writer small_codec small_template [ s_id; s_lo ])
  in
  let buf = Bytes.make 16 '\xAA' in
  expect_invalid "lo past its 6 bits" (fun () -> w buf 3 1 64);
  expect_invalid "negative id" (fun () -> w buf 3 (-1) 0);
  if Sys.int_size >= 63 then
    Alcotest.(check string)
      "nothing written" (String.make 16 '\xAA') (Bytes.to_string buf);
  let wb = Staged.unstage (Codec.writer big_codec big_template [ b_c ]) in
  expect_invalid "c past its 5 bits" (fun () -> wb buf 0 32)

let test_short_buffer () =
  let w = Staged.unstage (Codec.writer small_codec small_template [ s_seq ]) in
  let buf = Bytes.make 8 '\000' in
  expect_invalid "past the end" (fun () -> w buf 3 (UInt8.v 1));
  expect_invalid "negative offset" (fun () -> w buf (-1) (UInt8.v 1));
  w buf 2 (UInt8.v 1);
  let wb = Staged.unstage (Codec.writer big_codec big_template [ b_d ]) in
  expect_invalid "big, past the end" (fun () -> wb buf 0 (UInt16.v 1))

let test_staging_refusals () =
  expect_invalid "a field twice" (fun () ->
      ignore (Codec.writer small_codec small_template [ s_id; s_id ]));
  let lookalike = Codec.(Field.v "Seq" uint8 $ fun r -> r.seq) in
  expect_invalid "a field the codec does not hold" (fun () ->
      ignore (Codec.writer small_codec small_template [ lookalike ]));
  let var_len = Field.v "N" uint8 in
  let var_codec =
    Codec.v "WriterVar"
      (fun n s -> (n, s))
      Codec.
        [
          (var_len $ fun (n, _) -> n);
          (Field.v "S" (byte_array ~size:(Field.ref var_len)) $ fun (_, s) -> s);
        ]
  in
  expect_invalid "a variable-size codec" (fun () ->
      ignore (Codec.writer var_codec (UInt8.v 0, "") []))

let per_call_words f =
  f ();
  let iters = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to iters do
    f ()
  done;
  let after = Gc.minor_words () in
  int_of_float (Float.round ((after -. before) /. float_of_int iters))

let test_no_allocation () =
  skip_unless_gc_counters ();
  let w =
    Staged.unstage (Codec.writer small_codec small_template [ s_id; s_len ])
  in
  let buf = Bytes.create 6 and len = UInt16.v 7 in
  Alcotest.(check int)
    "writer allocates nothing" 0
    (per_call_words (fun () -> w buf 0 5 len))

let suite =
  ( "codec_writer",
    [
      Alcotest.test_case "small record: writer = encode + set" `Quick
        test_small_equals_set;
      Alcotest.test_case "small record: every arity" `Quick test_small_arities;
      Alcotest.test_case "long record: writer = encode + set" `Quick
        test_big_equals_set;
      Alcotest.test_case "32-bit word: writer = encode + set" `Quick
        test_word_equals_set;
      Alcotest.test_case "too wide a value writes nothing" `Quick test_too_wide;
      Alcotest.test_case "short buffer" `Quick test_short_buffer;
      Alcotest.test_case "staging refusals" `Quick test_staging_refusals;
      Alcotest.test_case "writer allocates nothing" `Quick test_no_allocation;
    ] )
