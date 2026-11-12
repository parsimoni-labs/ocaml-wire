(* Tests for codec.ml: exact widths and encode refusals. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

(* A fixed-size byte field is exact: a value whose length differs from the
   declared size is a caller error, not something to truncate or zero-pad. *)
let expect_exact_byte_error label ~expected ~actual f =
  match f () with
  | () -> Alcotest.failf "%s: expected an exact-size error" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names the declared size")
        true
        (contains ~sub:(Fmt.str "expected %d bytes" expected) msg);
      Alcotest.(check bool)
        (label ^ ": names the value length")
        true
        (contains ~sub:(Fmt.str "got %d" actual) msg)

let slice_of_string s =
  let b = Bytes.of_string s in
  Bytesrw.Bytes.Slice.make b ~first:0 ~length:(Bytes.length b)

let exact_ba_codec =
  Codec.v "ExactBa" Fun.id
    Codec.[ Field.v "B" (byte_array ~size:(int 4)) $ Fun.id ]

let exact_bs_codec =
  Codec.v "ExactBs" Fun.id
    Codec.[ Field.v "S" (byte_slice ~size:(int 4)) $ Fun.id ]

let test_exact_byte_field_literal_size () =
  let buf = Bytes.create 4 in
  expect_exact_byte_error "byte_array long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_ba_codec "abcdef" buf 0);
  expect_exact_byte_error "byte_array short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_ba_codec "ab" buf 0);
  expect_exact_byte_error "byte_slice long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_bs_codec (slice_of_string "abcdef") buf 0);
  expect_exact_byte_error "byte_slice short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_bs_codec (slice_of_string "ab") buf 0);
  Codec.encode exact_ba_codec "abcd" buf 0;
  Alcotest.(check string) "exact string" "abcd" (Bytes.to_string buf);
  Codec.encode exact_bs_codec (slice_of_string "wxyz") buf 0;
  Alcotest.(check string) "exact slice" "wxyz" (Bytes.to_string buf)

(* The same contract on the variable-size encoder, where the declared size is a
   cross-field reference rather than a literal. *)
let exact_vb_len = Field.v "N" uint8

let exact_vb_codec =
  Codec.v "ExactVb"
    (fun n b -> (n, b))
    Codec.
      [
        exact_vb_len $ fst;
        Field.v "B" (byte_array ~size:(Field.ref exact_vb_len)) $ snd;
      ]

let exact_vs_len = Field.v "N" uint8

let exact_vs_codec =
  Codec.v "ExactVs"
    (fun n s -> (n, s))
    Codec.
      [
        exact_vs_len $ fst;
        Field.v "S" (byte_slice ~size:(Field.ref exact_vs_len)) $ snd;
      ]

let test_exact_byte_field_expression_size () =
  let buf = Bytes.create 16 in
  expect_exact_byte_error "byte_array long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_vb_codec (UInt8.v 4, "abcdef") buf 0);
  expect_exact_byte_error "byte_array short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_vb_codec (UInt8.v 4, "ab") buf 0);
  expect_exact_byte_error "byte_slice long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode exact_vs_codec (UInt8.v 4, slice_of_string "abcdef") buf 0);
  expect_exact_byte_error "byte_slice short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_vs_codec (UInt8.v 4, slice_of_string "ab") buf 0);
  Codec.encode exact_vb_codec (UInt8.v 4, "abcd") buf 0;
  Alcotest.(check string) "exact string" "\x04abcd" (Bytes.sub_string buf 0 5)

(* Full-record encoding is deliberately non-transactional: a late refusal does
   not roll back fields already written. Callers that catch the exception must
   discard the destination instead of retrying from its contents. *)
let test_encode_failure_leaves_partial_record () =
  let buf = Bytes.make 6 '\xcc' in
  expect_exact_byte_error "late byte_array" ~expected:4 ~actual:2 (fun () ->
      Codec.encode exact_vb_codec (UInt8.v 4, "ab") buf 0);
  Alcotest.(check string)
    "earlier field remains written" "\x04\xcc\xcc\xcc\xcc\xcc"
    (Bytes.to_string buf)

(* Encode must not emit a value its own decoder rejects: a refinement the
   decoder enforces (enum membership, all-zero padding, a per-byte span
   predicate, a where cond) has to gate encode too, or the record ships bytes
   that fail to read back. Each case pins the bytes on the accepted value and
   the [Invalid_argument] on the rejected one, and confirms decode agrees. *)
let expect_encode_rejects label ~names f =
  match f () with
  | () -> Alcotest.failf "%s: encode accepted a value decode rejects" label
  | exception Invalid_argument msg ->
      List.iter
        (fun sub ->
          Alcotest.(check bool)
            (Fmt.str "%s: message names %S" label sub)
            true (contains ~sub msg))
        names

let expect_decode_rejects label codec s =
  match Codec.decode codec (Bytes.of_string s) 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.failf "%s: decode accepted %S" label s

let closed_enum_codec =
  Codec.v "ClosedEnum" Fun.id
    Codec.[ Field.v "E" (enum "Code" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]

let open_enum_codec =
  Codec.v "OpenEnum" Fun.id
    Codec.
      [ Field.v "E" (enum_open "Code" [ ("A", 1); ("B", 2) ] uint8) $ Fun.id ]

let test_encode_rejects_unlisted_enum () =
  let buf = Bytes.create 1 in
  expect_encode_rejects "closed enum" ~names:[ "Code"; "got 99" ] (fun () ->
      Codec.encode closed_enum_codec (UInt8.v 99) buf 0);
  expect_decode_rejects "closed enum" closed_enum_codec "\099";
  Codec.encode closed_enum_codec (UInt8.v 2) buf 0;
  Alcotest.(check string) "listed value" "\002" (Bytes.to_string buf);
  (* An open enum documents its codes without restricting them, so encode
     stays permissive exactly where decode does. *)
  Codec.encode open_enum_codec (UInt8.v 99) buf 0;
  Alcotest.(check string) "open enum" "\099" (Bytes.to_string buf)

(* The compiled codec reaches an enum / lookup / variants / bit field through
   its own readers, writers and staged accessors, none of which the typ-level
   [of_string] path shares. A base whose OCaml carrier is not [int] --
   [uint32], [int32] -- must travel all of them: the integer the refinement is
   compiled against is the only thing these combinators take from their base. *)
let carrier_enum_field =
  Codec.(Field.v "E" (enum "Code32" [ ("A", 1); ("B", 2) ] uint32be) $ Fun.id)

let carrier_enum_codec = Codec.v "CarrierEnum" Fun.id [ carrier_enum_field ]

let test_enum_over_carrier_base () =
  let buf = Bytes.of_string "\x00\x00\x00\x02" in
  (match Codec.decode carrier_enum_codec buf 0 with
  | Ok v -> Alcotest.(check int) "decode listed code" 2 (UInt32.to_int v)
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  Codec.validate carrier_enum_codec buf 0;
  let get = Staged.unstage (Codec.get carrier_enum_codec carrier_enum_field) in
  Alcotest.(check int) "get listed code" 2 (UInt32.to_int (get buf 0));
  expect_decode_rejects "unlisted code" carrier_enum_codec "\x00\x00\x00\x63";
  expect_encode_rejects "unlisted code" ~names:[ "Code32"; "got 99" ] (fun () ->
      Codec.encode carrier_enum_codec (UInt32.of_int 99) buf 0);
  Codec.encode carrier_enum_codec (UInt32.of_int 1) buf 0;
  Alcotest.(check string)
    "encode listed code" "\x00\x00\x00\x01" (Bytes.to_string buf);
  let set = Staged.unstage (Codec.set carrier_enum_codec carrier_enum_field) in
  set buf 0 (UInt32.of_int 2);
  Alcotest.(check string) "set" "\x00\x00\x00\x02" (Bytes.to_string buf)

let carrier_variants_field =
  Codec.(
    Field.v "V"
      (variants "Sign"
         [ ("Neg", `Neg); ("Zero", `Zero); ("Pos", `Pos) ]
         int32be)
    $ Fun.id)

let carrier_variants_codec =
  Codec.v "CarrierVariants" Fun.id [ carrier_variants_field ]

let test_variants_over_carrier_base () =
  let buf = Bytes.of_string "\x00\x00\x00\x01" in
  (match Codec.decode carrier_variants_codec buf 0 with
  | Ok v -> Alcotest.(check bool) "decode variant" true (v = `Zero)
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  Codec.validate carrier_variants_codec buf 0;
  let get =
    Staged.unstage (Codec.get carrier_variants_codec carrier_variants_field)
  in
  Alcotest.(check bool) "get variant" true (get buf 0 = `Zero);
  Codec.encode carrier_variants_codec `Pos buf 0;
  Alcotest.(check string) "encode" "\x00\x00\x00\x02" (Bytes.to_string buf);
  expect_decode_rejects "index past the variant list" carrier_variants_codec
    "\x00\x00\x00\x07"

let carrier_lookup_field =
  Codec.(Field.v "L" (lookup [ "a"; "b"; "c" ] uint32be) $ Fun.id)

let carrier_lookup_codec =
  Codec.v "CarrierLookup" Fun.id [ carrier_lookup_field ]

let carrier_bit_codec =
  Codec.v "CarrierBit" Fun.id Codec.[ Field.v "B" (bit uint32be) $ Fun.id ]

let test_lookup_and_bit_over_carrier_base () =
  let buf = Bytes.of_string "\x00\x00\x00\x02" in
  (match Codec.decode carrier_lookup_codec buf 0 with
  | Ok v -> Alcotest.(check string) "lookup decode" "c" v
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  Codec.validate carrier_lookup_codec buf 0;
  let get =
    Staged.unstage (Codec.get carrier_lookup_codec carrier_lookup_field)
  in
  Alcotest.(check string) "lookup get" "c" (get buf 0);
  Codec.encode carrier_lookup_codec "b" buf 0;
  Alcotest.(check string)
    "lookup encode" "\x00\x00\x00\x01" (Bytes.to_string buf);
  expect_decode_rejects "index past the table" carrier_lookup_codec
    "\x00\x00\x00\x07";
  (match Codec.decode carrier_bit_codec buf 0 with
  | Ok v -> Alcotest.(check bool) "bit decode" true v
  | Error e -> Alcotest.failf "%a" pp_parse_error e);
  Codec.encode carrier_bit_codec false buf 0;
  Alcotest.(check string) "bit encode" "\x00\x00\x00\x00" (Bytes.to_string buf)

(* An enum over a little-endian [uint32] keeps its named 3D type, exactly as one
   over [uint8] does; over the big-endian base it falls back to the membership
   refinement EverParse can type. Both must project, or the widened base would
   ship an OCaml-only feature. *)
let test_enum_over_carrier_base_projects () =
  let le = enum "Code32le" [ ("A", 1); ("B", 2) ] uint32 in
  let out =
    to_3d ~enum_as_type:true
      (module_ [ typedef (struct_ "LeEnum32" [ field "v" le ]) ])
  in
  Alcotest.(check bool)
    "uint32 enum keeps its enum decl" true
    (contains ~sub:"enum Code32le" out);
  let be = enum "Code32be" [ ("A", 1); ("B", 2) ] uint32be in
  let out_be =
    to_3d ~enum_as_type:true
      (module_ [ typedef (struct_ "BeEnum32" [ field "v" be ]) ])
  in
  Alcotest.(check bool)
    "uint32be enum carries a membership refinement" true
    (contains ~sub:"UINT32BE" out_be && contains ~sub:"== 2" out_be)

let zeros_codec =
  Codec.v "Padded"
    (fun tag pad -> (tag, pad))
    Codec.[ Field.v "Tag" uint8 $ fst; Field.v "Pad" all_zeros $ snd ]

let test_encode_rejects_non_zero_padding () =
  let buf = Bytes.create 4 in
  expect_encode_rejects "all_zeros" ~names:[ "all_zeros"; "0x61" ] (fun () ->
      Codec.encode zeros_codec (UInt8.v 1, "abc") buf 0);
  expect_decode_rejects "all_zeros" zeros_codec "\001abc";
  Codec.encode zeros_codec (UInt8.v 1, "\000\000\000") buf 0;
  Alcotest.(check string)
    "zero padding" "\001\000\000\000" (Bytes.to_string buf)

let per_byte_typ =
  byte_array_where ~size:(int 3) ~per_byte:(fun b ->
      Expr.(b >= int 0x20 && b <= int 0x7e))

let per_byte_cf = Codec.(Field.v "B" per_byte_typ $ Fun.id)
let per_byte_codec = Codec.v "Printable" Fun.id [ per_byte_cf ]

(* Same refinement over a length-prefixed span, which takes the variable-size
   reader rather than the constant-offset one. *)
let per_byte_var_codec =
  let open Codec in
  let f_len = Field.v "Len" uint8 in
  let f_b =
    Field.v "B"
      (byte_array_where ~size:(Field.ref f_len) ~per_byte:(fun b ->
           Expr.(b >= int 0x20 && b <= int 0x7e)))
  in
  v "PrintableVar" (fun len b -> (len, b)) [ f_len $ fst; f_b $ snd ]

let test_encode_rejects_refined_byte () =
  let buf = Bytes.create 3 in
  expect_encode_rejects "byte_array_where" ~names:[ "byte 1"; "0xff" ]
    (fun () -> Codec.encode per_byte_codec "a\xffb" buf 0);
  Codec.encode per_byte_codec "abc" buf 0;
  Alcotest.(check string) "printable span" "abc" (Bytes.to_string buf)

let decode_accepts codec s =
  Result.is_ok (Codec.decode codec (Bytes.of_string s) 0)

let validate_result codec s =
  match Codec.validate codec (Bytes.of_string s) 0 with
  | () -> Ok ()
  | exception Parse_error e -> Error e

let expect_per_byte label ~at = function
  | Error { kind = Constraint_failed { which = Per_byte; _ }; at = off; _ } ->
      Alcotest.(check int) (label ^ ": offset") at off
  | Error e ->
      Alcotest.failf "%s: expected a per-byte refusal, got %a" label
        pp_parse_error e
  | Ok () -> Alcotest.failf "%s: accepted a byte violating the refinement" label

(* The refinement belongs to the span itself, so the compiled reader has to
   apply it: a [Codec.decode] more permissive than [Wire.of_string] is also more
   permissive than the EverParse validator built from the same schema, and
   [Codec.validate] is the gate a caller runs before reading untrusted bytes
   zero-copy. *)
let test_decode_rejects_refined_byte () =
  expect_per_byte "decode" ~at:1
    (Result.map ignore
       (Codec.decode per_byte_codec (Bytes.of_string "a\xffb") 0));
  expect_per_byte "validate" ~at:1 (validate_result per_byte_codec "a\xffb");
  expect_per_byte "decode var" ~at:2
    (Result.map ignore
       (Codec.decode per_byte_var_codec (Bytes.of_string "\002a\xff") 0));
  expect_per_byte "validate var" ~at:2
    (validate_result per_byte_var_codec "\002a\xff");
  (* [get] is documented as unchecked for other fields' constraints, but the
     span's own refinement travels with the reader, as a closed enum's does. *)
  let get = Staged.unstage (Codec.get per_byte_codec per_byte_cf) in
  (match get (Bytes.of_string "a\xffb") 0 with
  | s -> Alcotest.failf "get returned %S past the refinement" s
  | exception
      Parse_error { kind = Constraint_failed { which = Per_byte; _ }; _ } ->
      ());
  (* The direct parser has always rejected this; it must keep doing so. *)
  match of_string per_byte_typ "a\xffb" with
  | Error { kind = Constraint_failed { which = Per_byte; _ }; at; _ } ->
      Alcotest.(check int) "of_string offset" 1 at
  | Error e -> Alcotest.failf "of_string: wrong error %a" pp_parse_error e
  | Ok _ -> Alcotest.fail "of_string accepted a byte violating the refinement"

let test_decode_accepts_conforming_refined_byte () =
  Alcotest.(check (result string string))
    "of_string" (Ok "abc")
    (Result.map_error
       (Fmt.str "%a" pp_parse_error)
       (of_string per_byte_typ "abc"));
  Alcotest.(check bool) "decode" true (decode_accepts per_byte_codec "abc");
  Alcotest.(check (result unit string))
    "validate" (Ok ())
    (Result.map_error
       (Fmt.str "%a" pp_parse_error)
       (validate_result per_byte_codec "abc"));
  Alcotest.(check string)
    "get" "abc"
    ((Staged.unstage (Codec.get per_byte_codec per_byte_cf))
       (Bytes.of_string "abc") 0)

(* The fuzz harness holds [decode] and [validate] to the same verdict; pin that
   here against the verdict the refinement calls for, so a later change can
   neither drop the check nor move one path without the other. *)
let test_refined_byte_decode_validate_agree () =
  List.iter
    (fun (s, accept) ->
      Alcotest.(check bool)
        (Fmt.str "decode of %S" s) accept
        (decode_accepts per_byte_codec s);
      Alcotest.(check bool)
        (Fmt.str "validate of %S" s)
        accept
        (Result.is_ok (validate_result per_byte_codec s)))
    [
      ("abc", true);
      (" ~!", true);
      ("a\xffb", false);
      ("\000bc", false);
      ("ab\x7f", false);
      ("ab\x80", false);
      ("\x1fbc", false);
    ]

(* The cond of a [Wire.where] reads sibling fields, so it can only be decided
   against the assembled record; encode replays the decode-side check pass over
   the bytes it just wrote. *)
let where_len = Field.v "Len" uint8

let where_codec =
  Codec.v "TypWhereEncode"
    (fun len d -> (len, d))
    Codec.
      [
        where_len $ fst;
        Field.v "D" (where Expr.(Field.ref where_len < int 2) uint8) $ snd;
      ]

let codec_where_codec =
  Codec.v "CodecWhereEncode"
    ~where:Expr.(Field.ref where_len < int 2)
    (fun len d -> (len, d))
    Codec.[ where_len $ fst; Field.v "D" uint8 $ snd ]

let test_encode_rejects_where_violation () =
  let buf = Bytes.create 2 in
  expect_encode_rejects "typ where" ~names:[ "TypWhereEncode"; "constraint" ]
    (fun () -> Codec.encode where_codec (UInt8.v 6, UInt8.v 0) buf 0);
  expect_decode_rejects "typ where" where_codec "\006\000";
  Codec.encode where_codec (UInt8.v 1, UInt8.v 0) buf 0;
  Alcotest.(check string) "satisfied cond" "\001\000" (Bytes.to_string buf);
  expect_encode_rejects "codec where"
    ~names:[ "CodecWhereEncode"; "constraint" ] (fun () ->
      Codec.encode codec_where_codec (UInt8.v 6, UInt8.v 0) buf 0);
  expect_decode_rejects "codec where" codec_where_codec "\006\000";
  Codec.encode codec_where_codec (UInt8.v 1, UInt8.v 0) buf 0;
  Alcotest.(check string)
    "satisfied codec where" "\001\000" (Bytes.to_string buf)

(* [Codec.set] writes in place, so a byte field whose size is only known at
   run time still owns exactly that many bytes: an oversized value that fits in
   the buffer would otherwise land on the fields that follow, with nothing to
   signal it. Each case checks the buffer as well as the exception, since a
   check that ran after the blit would raise on an already corrupted buffer. *)

(* Every field before [data] is fixed-size, so [data] has a static offset and a
   run-time size. *)
let set_var_codec typ =
  let open Codec in
  let f_len = Field.v "len" uint8 in
  let cf_data = Field.v "data" (typ (Field.ref f_len)) $ fun (_, d, _) -> d in
  let codec =
    v "SetVarBytes"
      (fun len data trailer -> (len, data, trailer))
      [
        (f_len $ fun (n, _, _) -> n);
        cf_data;
        (Field.v "trailer" uint8 $ fun (_, _, t) -> t);
      ]
  in
  (codec, cf_data)

(* [pre] is itself variable-size, so [data] has a run-time offset as well as a
   run-time size. *)
let set_dyn_codec typ =
  let open Codec in
  let f_plen = Field.v "plen" uint8 in
  let f_len = Field.v "len" uint8 in
  let cf_data =
    Field.v "data" (typ (Field.ref f_len)) $ fun (_, _, _, d, _) -> d
  in
  let codec =
    v "SetDynBytes"
      (fun plen pre len data trailer -> (plen, pre, len, data, trailer))
      [
        (f_plen $ fun (n, _, _, _, _) -> n);
        ( Field.v "pre" (byte_array ~size:(Field.ref f_plen))
        $ fun (_, p, _, _, _) -> p );
        (f_len $ fun (_, _, n, _, _) -> n);
        cf_data;
        (Field.v "trailer" uint8 $ fun (_, _, _, _, t) -> t);
      ]
  in
  (codec, cf_data)

(* len = 2, data = "ab" at offset 1, trailer = 0xff, then spare bytes so that a
   4-byte write past the field still lands inside the buffer. *)
let set_var_before = "\x02ab\xff\xee\xee\xee"
let set_var_after = "\x02XY\xff\xee\xee\xee"
let set_dyn_before = "\x01p\x02ab\xff\xee\xee\xee"
let set_dyn_after = "\x01p\x02XY\xff\xee\xee\xee"

let check_set_exact label ~before ~after ~oversized ~exact set =
  let buf = Bytes.of_string before in
  expect_exact_byte_error label ~expected:2 ~actual:4 (fun () ->
      set buf 0 oversized);
  Alcotest.(check string)
    (label ^ ": rejected set left the buffer alone")
    before (Bytes.to_string buf);
  set buf 0 exact;
  Alcotest.(check string)
    (label ^ ": exact set wrote only its own bytes")
    after (Bytes.to_string buf)

let test_set_exact_var_byte_array () =
  let codec, cf = set_var_codec (fun size -> byte_array ~size) in
  check_set_exact "byte_array at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_var_byte_array_where () =
  let codec, cf =
    set_var_codec (fun size -> byte_array_where ~size ~per_byte:printable_byte)
  in
  check_set_exact "byte_array_where at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_var_byte_slice () =
  let codec, cf = set_var_codec (fun size -> byte_slice ~size) in
  check_set_exact "byte_slice at a static offset" ~before:set_var_before
    ~after:set_var_after ~oversized:(slice_of_string "ABCD")
    ~exact:(slice_of_string "XY")
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_array () =
  let codec, cf = set_dyn_codec (fun size -> byte_array ~size) in
  check_set_exact "byte_array at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_array_where () =
  let codec, cf =
    set_dyn_codec (fun size -> byte_array_where ~size ~per_byte:printable_byte)
  in
  check_set_exact "byte_array_where at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

let test_set_exact_dyn_byte_slice () =
  let codec, cf = set_dyn_codec (fun size -> byte_slice ~size) in
  check_set_exact "byte_slice at a run-time offset" ~before:set_dyn_before
    ~after:set_dyn_after ~oversized:(slice_of_string "ABCD")
    ~exact:(slice_of_string "XY")
    (Staged.unstage (Codec.set codec cf))

(* A byte field with a literal size takes the fixed-offset writer, which shares
   the encoder's exact-size blit. *)
let test_set_exact_fixed_byte_array () =
  let cf = Codec.(Field.v "B" (byte_array ~size:(int 2)) $ fst) in
  let codec =
    Codec.(
      v "SetFixedBytes"
        (fun b t -> (b, t))
        [ cf; Field.v "trailer" uint8 $ snd ])
  in
  check_set_exact "byte_array at a literal size" ~before:"ab\xff\xee\xee"
    ~after:"XY\xff\xee\xee" ~oversized:"ABCD" ~exact:"XY"
    (Staged.unstage (Codec.set codec cf))

(* A width-refined leaf -- [bits ~width], [uint size] -- is exact for a
   harsher reason than a fixed-size byte field. A masked value is still a legal
   value at that width, so decode, [validate] and the EverParse validator all
   accept it: the number the caller meant is gone with nothing left to detect
   it. Every entry point that can write one has to refuse instead. *)
let expect_exact_width_error label ~sub f =
  match f () with
  | wrote ->
      Alcotest.failf "%s: expected an exact-width error, wrote %S" label wrote
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names the value and the width")
        true (contains ~sub msg)

let uint_width_codec ~endian n =
  let cf = Codec.(Field.v "v" (uint ~endian n) $ Fun.id) in
  (Codec.v (Fmt.str "ExactUint%d" n) Fun.id Codec.[ cf ], cf)

(* A hostile value needs one bit more than the field it overflows, so a width is
   only testable where the host [int] holds that extra bit. [Sys.int_size] is 31
   under wasm_of_ocaml, which drops the widest cases there. *)
let width_is_testable bits = bits + 1 <= Sys.int_size - 1

(* Each case is a declared size, the widest value it holds and a value it does
   not: one byte holds 0xFF, and 0x1FF is the reported truncation. *)
let uint_width_cases =
  List.filter_map
    (fun n ->
      if width_is_testable (8 * n) then
        let widest = (1 lsl (8 * n)) - 1 in
        Some (n, widest, widest lor (widest + 1))
      else None)
    [ 1; 2; 3; 5; 7 ]

let uint_endians = [ (Big, "be"); (Little, "le") ]

(* 0x1FF through a 1-byte [uint] used to come out as 0xFF on both encode
   entry points, and decode back as 255. *)
let test_encode_exact_uint_width () =
  List.iter
    (fun (n, max_v, over) ->
      List.iter
        (fun (endian, ename) ->
          let typ = uint ~endian n in
          let codec, _ = uint_width_codec ~endian n in
          let label = Fmt.str "uint(%d,%s) <- 0x%X" n ename over in
          let sub = Fmt.str "does not fit an unsigned %d-byte field" n in
          let value = Wire.Private.UInt63.of_int over in
          expect_exact_width_error (label ^ " to_string") ~sub (fun () ->
              Wire.to_string typ value);
          expect_exact_width_error (label ^ " Codec.encode") ~sub (fun () ->
              let buf = Bytes.make 8 '\x00' in
              Codec.encode codec value buf 0;
              Bytes.sub_string buf 0 n);
          (* The widest value the field does hold still encodes, identically
             through both entry points. *)
          let widest = Wire.Private.UInt63.of_int max_v in
          let buf = Bytes.make 8 '\x00' in
          Codec.encode codec widest buf 0;
          Alcotest.(check string)
            (label ^ ": widest legal value agrees across entry points")
            (Wire.to_string typ widest)
            (Bytes.sub_string buf 0 n))
        uint_endians)
    uint_width_cases

let test_set_exact_uint_width () =
  List.iter
    (fun (n, max_v, over) ->
      List.iter
        (fun (endian, ename) ->
          let codec, cf = uint_width_codec ~endian n in
          let set = Staged.unstage (Codec.set codec cf) in
          let buf = Bytes.make 8 '\x00' in
          let label = Fmt.str "set uint(%d,%s) <- 0x%X" n ename over in
          expect_exact_width_error label
            ~sub:(Fmt.str "does not fit an unsigned %d-byte field" n) (fun () ->
              set buf 0 (Wire.Private.UInt63.of_int over);
              Bytes.sub_string buf 0 n);
          Alcotest.(check string)
            (label ^ ": rejected set left the buffer alone")
            (String.make 8 '\x00') (Bytes.to_string buf);
          let widest = Wire.Private.UInt63.of_int max_v in
          set buf 0 widest;
          Alcotest.(check string)
            (label ^ ": widest legal value writes")
            (String.make n '\xff') (Bytes.sub_string buf 0 n))
        uint_endians)
    uint_width_cases

(* The decode half of the same rule, at every width [uint size] accepts.
   A full-width frame must come back as the number those bytes spell and go out
   again byte for byte: a carrier too narrow for the field would mask the top
   bits and hand back a legal smaller number instead. Widths are compared
   through [Int64] because a 7-byte maximum overflows the native [int] where it
   is 31 bits wide. *)
let test_uint_width_decodes_exactly () =
  List.iter
    (fun size ->
      List.iter
        (fun (endian, ename) ->
          let codec, _ = uint_width_codec ~endian size in
          let label = Fmt.str "uint(%d,%s)" size ename in
          let wire = String.make size '\xff' in
          let widest = Int64.sub (Int64.shift_left 1L (8 * size)) 1L in
          let v = Codec.decode_exn codec (Bytes.of_string wire) 0 in
          Alcotest.(check int64)
            (label ^ ": full-width frame decodes to the number it spells")
            widest (Optint.Int63.to_int64 v);
          let out = Bytes.make size '\x00' in
          Codec.encode codec v out 0;
          Alcotest.(check string)
            (label ^ ": full-width frame re-encodes unchanged")
            wire (Bytes.to_string out))
        uint_endians)
    [ 1; 2; 3; 4; 5; 6; 7 ]

let bits_width_bases =
  [
    (U8, "U8", 8);
    (U16, "U16", 16);
    (U16be, "U16be", 16);
    (U32, "U32", 32);
    (U32be, "U32be", 32);
  ]

let bits_orders = [ (Msb_first, "Msb"); (Lsb_first, "Lsb") ]

let bits_width_codec ~bit_order ~width base =
  let cf = Codec.(Field.v "v" (bits ~bit_order ~width base) $ Fun.id) in
  (Codec.v "ExactBits" Fun.id Codec.[ cf ], cf)

let iter_bits_widths f =
  List.iter
    (fun (base, bname, total) ->
      List.iter
        (fun width ->
          if width_is_testable width then
            List.iter
              (fun (bit_order, oname) ->
                f ~base ~bname ~width ~bit_order ~oname)
              bits_orders)
        [ 1; 3; total ])
    bits_width_bases

(* [Wire.to_bytes (bits ~width:3 U8) 0x8] used to write 00 where [Codec.encode]
   of the same value raised: the two encode entry points disagreed on a value
   neither can represent. Swept over every base, bit order and boundary width so
   they cannot drift apart again. *)
let test_encode_exact_bits_width () =
  iter_bits_widths (fun ~base ~bname ~width ~bit_order ~oname ->
      let typ = bits ~bit_order ~width base in
      let codec, _ = bits_width_codec ~bit_order ~width base in
      let over = 1 lsl width in
      let label = Fmt.str "bits(%d,%s,%s) <- 0x%X" width bname oname over in
      let sub = Fmt.str "does not fit an unsigned %d-bit field" width in
      expect_exact_width_error (label ^ " to_bytes") ~sub (fun () ->
          Bytes.to_string (Wire.to_bytes typ over));
      expect_exact_width_error (label ^ " Codec.encode") ~sub (fun () ->
          let buf = Bytes.make 4 '\x00' in
          Codec.encode codec over buf 0;
          Bytes.to_string buf);
      let widest = over - 1 in
      let buf = Bytes.make (Codec.wire_size codec) '\x00' in
      Codec.encode codec widest buf 0;
      Alcotest.(check string)
        (label ^ ": widest legal value agrees across entry points")
        (Bytes.to_string (Wire.to_bytes typ widest))
        (Bytes.to_string buf))

(* The worst of the family: [Codec.set] wrote 0xFF into a 3-bit field as 7,
   which [Codec.get] reads back and [validate] accepts, so no later check can
   recover that the caller meant 255. *)
let test_set_exact_bits_width () =
  iter_bits_widths (fun ~base ~bname ~width ~bit_order ~oname ->
      let codec, cf = bits_width_codec ~bit_order ~width base in
      let size = Codec.wire_size codec in
      let set = Staged.unstage (Codec.set codec cf) in
      let get = Staged.unstage (Codec.get codec cf) in
      let over = 1 lsl width in
      let buf = Bytes.make size '\x00' in
      let label = Fmt.str "set bits(%d,%s,%s) <- 0x%X" width bname oname over in
      expect_exact_width_error label
        ~sub:(Fmt.str "does not fit an unsigned %d-bit field" width) (fun () ->
          set buf 0 over;
          Bytes.to_string buf);
      Alcotest.(check string)
        (label ^ ": rejected set left the buffer alone")
        (String.make size '\x00') (Bytes.to_string buf);
      let widest = over - 1 in
      set buf 0 widest;
      Alcotest.(check int)
        (label ^ ": widest legal value reads back")
        widest (get buf 0))

(* The concrete case from the bug report, pinned on its own: 0xFF into a 3-bit
   field left a buffer of 0xE0 that read back as 7 and passed [validate]. *)
let test_set_bits_no_silent_truncation () =
  let cf = Codec.(Field.v "v" (bits ~width:3 U8) $ Fun.id) in
  let codec = Codec.v "SetBits3" Fun.id Codec.[ cf ] in
  let set = Staged.unstage (Codec.set codec cf) in
  let buf = Bytes.make 1 '\x00' in
  (match set buf 0 0xFF with
  | () ->
      Alcotest.failf "set bits(3,U8) <- 0xFF wrote %02x" (Bytes.get_uint8 buf 0)
  | exception Invalid_argument _ -> ());
  Alcotest.(check int) "buffer untouched" 0x00 (Bytes.get_uint8 buf 0)

(* [map] adds no encode path of its own: it hands the mapped value to the inner
   typ, so the width checks have to reach through it. *)
let test_map_inherits_exact_width () =
  let typ = map ~decode:Fun.id ~encode:Fun.id (bits ~width:3 U8) in
  let cf = Codec.(Field.v "v" typ $ Fun.id) in
  let codec = Codec.v "MapBits3" Fun.id Codec.[ cf ] in
  let set = Staged.unstage (Codec.set codec cf) in
  expect_exact_width_error "map(bits(3,U8)) to_string"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      Wire.to_string typ 0xFF);
  expect_exact_width_error "map(bits(3,U8)) Codec.encode"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      let buf = Bytes.make 1 '\x00' in
      Codec.encode codec 0xFF buf 0;
      Bytes.to_string buf);
  expect_exact_width_error "map(bits(3,U8)) Codec.set"
    ~sub:"does not fit an unsigned 3-bit field" (fun () ->
      let buf = Bytes.make 1 '\x00' in
      set buf 0 0xFF;
      Bytes.to_string buf)

(* Every fixed-width scalar puts that range in its carrier instead, so there is
   no out-of-range value left to hand an encoder: [test_uint32_of_int_range],
   [test_sint32_of_int_range] and the [sint8], [sint16], [uint8] and [uint16]
   suites pin those refusals, one step earlier than an encoder can reach. *)

(* The signed 32-bit range is enforced where the value is built rather than
   where it is written, so a number the field cannot hold never becomes an
   [int32] in the first place. [of_int32] needs no such guard: every [int32] is
   in range by construction. *)
let test_sint32_of_int_range () =
  (* Only a native [int] wider than the field can hold a number outside it, so
     the refusal is reachable only there; where the [int] is narrower every one
     of its values is a legal signed 32-bit field. *)
  if width_is_testable 32 then begin
    let limit = 1 lsl 31 in
    List.iter
      (fun n ->
        match SInt32.of_int n with
        | v -> Alcotest.failf "SInt32.of_int %d built %a" n SInt32.pp v
        | exception Invalid_argument _ -> ())
      [ limit; -limit - 1 ]
  end;
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int32) msg (Int32.of_int n))
        "SInt32.of_int %d round-trips" n
        (SInt32.to_int32 (SInt32.of_int n)))
    [ -1; 0; 1 ];
  (* [of_int32] is total: every [int32] is in range by construction, including
     the two ends no native [int] is guaranteed to hold. *)
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int32) msg n)
        "SInt32.of_int32 %ld round-trips" n
        (SInt32.to_int32 (SInt32.of_int32 n)))
    Int32.[ min_int; minus_one; zero; one; max_int ]

(* The unsigned 32-bit range is enforced where the value is built rather than
   where it is written, so a number the field cannot hold never becomes a
   [uint32] in the first place. Only a native [int] wider than the field can
   supply one, so the refusal is reachable only there. [of_int32] needs no
   guard: every [int32] pattern is a legal uint32. *)
let test_uint32_of_int_range () =
  let mask = Wire.Private.UInt32.mask32 in
  if width_is_testable 32 then
    List.iter
      (fun n ->
        match UInt32.of_int n with
        | v -> Alcotest.failf "UInt32.of_int %d built %a" n UInt32.pp v
        | exception Invalid_argument _ -> ())
      [ mask + 1; -1; min_int ];
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int) msg n)
        "UInt32.of_int %d round-trips" n
        (UInt32.to_int (UInt32.of_int n)))
    [ 0; 1; 0xFFFF ];
  List.iter
    (fun n ->
      Fmt.kstr
        (fun msg -> Alcotest.(check int32) msg n)
        "UInt32.of_int32 %ld round-trips" n
        (UInt32.to_int32 (UInt32.of_int32 n)))
    Int32.[ min_int; minus_one; zero; one; max_int ]

(* An 8-byte field spans 2^64 wire patterns and the generated C validator hands
   every one of them on unchanged, so the OCaml side must too. A carrier that
   held fewer would mask the top bits and hand back a legal smaller number, one
   nothing downstream can tell from the value the sender meant. Refusing the
   frame would be sound; altering it is not. *)
let check_eight_byte_unsigned name typ =
  let wire = String.make 8 '\xff' in
  let cf = Codec.(Field.v "v" typ $ Fun.id) in
  let codec = Codec.v "EightByteUnsigned" Fun.id Codec.[ cf ] in
  match Codec.decode codec (Bytes.of_string wire) 0 with
  | Error _ -> ()
  | Ok v ->
      let out = Bytes.make 8 '\x00' in
      Codec.encode codec v out 0;
      Alcotest.(check string)
        (name ^ ": all-ones frame re-encodes unchanged")
        wire (Bytes.to_string out)

let test_eight_byte_unsigned_preserves_wire () =
  check_eight_byte_unsigned "uint64" uint64;
  check_eight_byte_unsigned "uint64be" uint64be

(* The same rule one width down, on the signed side. Four bytes spell 2^32
   patterns and the generated C validator hands every one of them on unchanged,
   so the OCaml side has to as well. A frame that leaves as different bytes with
   no error on either path is the failure this pins: refusing one the target
   cannot hold would be sound, altering it silently is not. The carrier is what
   makes it hold, {!SInt32.t} widening to a boxed [int32] wherever the native
   [int] is narrower than the field, so no target drops the top bits. *)
let check_four_byte_signed name typ wire =
  let cf = Codec.(Field.v "v" typ $ Fun.id) in
  let codec = Codec.v "FourByteSigned" Fun.id Codec.[ cf ] in
  match Codec.decode codec (Bytes.of_string wire) 0 with
  | Error _ -> ()
  | Ok v ->
      let out = Bytes.make 4 '\x00' in
      Codec.encode codec v out 0;
      Fmt.kstr
        (fun msg -> Alcotest.(check string) msg wire (Bytes.to_string out))
        "%s: frame %a re-encodes unchanged" name
        Fmt.(list ~sep:nop (fmt "%02x"))
        (List.map Char.code (List.of_seq (String.to_seq wire)))

let test_four_byte_signed_preserves_wire () =
  List.iter
    (fun (name, typ) ->
      List.iter
        (check_four_byte_signed name typ)
        [ String.make 4 '\xff'; "\x80\x00\x00\x00"; "\x7f\xff\xff\xff" ])
    [ ("int32", int32); ("int32be", int32be) ]

(* The other end of the rule: an [int64] is exactly the eight bytes written, so
   no value of it is out of range and none may be refused. A guard here would be
   dead code that only ever rejected a legal frame. *)
let test_encode_int64_carrier_has_no_range () =
  let check name typ to_i64 of_i64 =
    List.iter
      (fun v ->
        let s = Wire.to_string typ (of_i64 v) in
        Alcotest.(check int) (name ^ ": eight bytes") 8 (String.length s);
        Alcotest.(check int64)
          (Fmt.str "%s <- %Ld round-trips" name v)
          v
          (to_i64 (Wire.of_string_exn typ s)))
      Int64.[ min_int; -1L; zero; max_int ]
  in
  (* The two signednesses carry their values in different types now, so the
     round-trip goes through each one's own conversion. *)
  check "uint64" uint64 UInt64.to_int64 UInt64.of_int64;
  check "uint64be" uint64be UInt64.to_int64 UInt64.of_int64;
  check "int64" int64 Fun.id Fun.id;
  check "int64be" int64be Fun.id Fun.id

(* A reference-free constant size expression normalises to the literal form at
   construction, so every [Int n] fast path and range guard in the library sees
   it. Sizes that must stay symbolic, and arithmetic the fold cannot do
   faithfully, are left as expressions. *)
let byte_array_size label t =
  let module T = Wire.Private.Types in
  match t with
  | T.Byte_array { size } -> size
  | _ -> Alcotest.failf "%s: expected a byte_array" label

let test_constant_size_expression_folds () =
  let module T = Wire.Private.Types in
  (match byte_array_size "constant" (byte_array ~size:Expr.(int 2 + int 2)) with
  | T.Int 4 -> ()
  | e -> Alcotest.failf "constant size did not fold: %a" T.pp_expr e);
  (* A reference-bearing size stays symbolic: the 3D projection needs the
     expression, not a baked-in width. *)
  (match
     byte_array_size "reference"
       (byte_array ~size:Expr.(Field.ref exact_vb_len + int 1))
   with
  | T.Add (T.Ref _, T.Int 1) -> ()
  | e -> Alcotest.failf "reference-bearing size was folded: %a" T.pp_expr e);
  (* Overflowing arithmetic is left alone rather than folded to a wrong
     value. *)
  (match
     byte_array_size "overflow" (byte_array ~size:Expr.(int max_int + int 1))
   with
  | T.Add (T.Int _, T.Int 1) -> ()
  | e -> Alcotest.failf "overflowing size was folded: %a" T.pp_expr e);
  (* 1. the encode contract *)
  let codec name typ = Codec.v name Fun.id Codec.[ Field.v "B" typ $ Fun.id ] in
  let lit = codec "FoldLiteral" (byte_array ~size:(int 4)) in
  let folded = codec "FoldConstant" (byte_array ~size:Expr.(int 2 + int 2)) in
  let buf = Bytes.create 4 in
  expect_exact_byte_error "literal long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode lit "abcdef" buf 0);
  expect_exact_byte_error "constant long" ~expected:4 ~actual:6 (fun () ->
      Codec.encode folded "abcdef" buf 0);
  expect_exact_byte_error "constant short" ~expected:4 ~actual:2 (fun () ->
      Codec.encode folded "ab" buf 0);
  Codec.encode folded "abcd" buf 0;
  Alcotest.(check string) "constant exact" "abcd" (Bytes.to_string buf);
  (* 2. wire_size *)
  Alcotest.(check int) "literal wire_size" 4 (Codec.wire_size lit);
  Alcotest.(check int) "constant wire_size" 4 (Codec.wire_size folded);
  (* 3. the uint_var 1-7 range guard after constant folding *)
  let uint_var_rejects label size =
    match uint_var size with
    | _ -> Alcotest.failf "%s: expected uint_var to reject the size" label
    | exception Invalid_argument msg ->
        Alcotest.(check bool)
          (label ^ ": names the bound")
          true
          (contains ~sub:"size must be 1-7" msg)
  in
  uint_var_rejects "literal 0" (int 0);
  uint_var_rejects "constant 0" Expr.(int 1 - int 1);
  uint_var_rejects "constant 8" Expr.(int 4 + int 4)

(* -- uint: arbitrary byte-width unsigned integer -- *)

(* [uint] decodes to an [Optint.Int63.t]. Aliasing [Optint.Int63] rather
   than [Wire.Private.UInt63] keeps these tests within the public API. *)
module UInt63 = Optint.Int63

let u63 = Alcotest.testable UInt63.pp ( = )

type uint_rec = { tag : UInt8.t; value : UInt63.t }

let test_uint_3byte_be () =
  let codec =
    let open Codec in
    v "U3BE"
      (fun tag value -> { tag; value })
      [
        (Field.v "Tag" uint8 $ fun r -> r.tag);
        (Field.v "Value" (uint 3) $ fun r -> r.value);
      ]
  in
  let original = { tag = UInt8.v 0x42; value = UInt63.of_int 0x1A2B3C } in
  let buf = Bytes.create 4 in
  Codec.encode codec original buf 0;
  Alcotest.(check int) "tag byte" 0x42 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "be byte 0" 0x1A (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "be byte 1" 0x2B (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "be byte 2" 0x3C (Bytes.get_uint8 buf 3);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int)
    "tag"
    (UInt8.to_int original.tag)
    (UInt8.to_int decoded.tag);
  Alcotest.check u63 "value" original.value decoded.value

let test_uint_1byte () =
  let codec =
    let open Codec in
    v "U1" (fun v -> v) [ (Field.v "V" (uint 1) $ fun v -> v) ]
  in
  let buf = Bytes.create 1 in
  Codec.encode codec (UInt63.of_int 0xAB) buf 0;
  Alcotest.(check int) "byte" 0xAB (Bytes.get_uint8 buf 0);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.check u63 "roundtrip" (UInt63.of_int 0xAB) decoded

let test_uint_5byte_le () =
  let codec =
    let open Codec in
    v "U5LE"
      (fun v -> v)
      [ (Field.v "V" (uint ~endian:Wire.Little 5) $ fun v -> v) ]
  in
  (* 40 bits: holds on every platform because the value lives in a
     [Optint.Int63.t], not a native int. *)
  let value = Optint.Int63.of_int64 0x01_02_03_04_05L in
  let buf = Bytes.create 5 in
  Codec.encode codec value buf 0;
  Alcotest.(check int) "le byte 0" 0x05 (Bytes.get_uint8 buf 0);
  Alcotest.(check int) "le byte 1" 0x04 (Bytes.get_uint8 buf 1);
  Alcotest.(check int) "le byte 2" 0x03 (Bytes.get_uint8 buf 2);
  Alcotest.(check int) "le byte 3" 0x02 (Bytes.get_uint8 buf 3);
  Alcotest.(check int) "le byte 4" 0x01 (Bytes.get_uint8 buf 4);
  let decoded = decode_ok (Codec.decode codec buf 0) in
  Alcotest.check u63 "roundtrip" value decoded

let test_uint_var_dynamic () =
  let f_n = Field.v "N" uint8 in
  let codec =
    let open Codec in
    v "UDyn"
      (fun n value -> (n, value))
      [
        (f_n $ fun (n, _) -> n);
        (Field.v "Value" (uint_var (Field.ref f_n)) $ fun (_, v) -> v);
      ]
  in
  (* n=2 -> 2-byte BE uint_var = 0x1234, layout: [02] [12 34] *)
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 2;
  Bytes.set_uint8 buf 1 0x12;
  Bytes.set_uint8 buf 2 0x34;
  let n, value = decode_ok (Codec.decode codec buf 0) in
  Alcotest.(check int) "n" 2 (UInt8.to_int n);
  Alcotest.check u63 "value" (UInt63.of_int 0x1234) value

let suite =
  ( "codec_encode",
    [
      Alcotest.test_case "exact width: uint encode" `Quick
        test_encode_exact_uint_width;
      Alcotest.test_case "exact width: uint set" `Quick
        test_set_exact_uint_width;
      Alcotest.test_case "exact width: uint decodes exactly" `Quick
        test_uint_width_decodes_exactly;
      Alcotest.test_case "exact width: bits encode" `Quick
        test_encode_exact_bits_width;
      Alcotest.test_case "exact width: bits set" `Quick
        test_set_exact_bits_width;
      Alcotest.test_case "exact width: set bits(3,U8) <- 0xFF" `Quick
        test_set_bits_no_silent_truncation;
      Alcotest.test_case "exact width: map reaches the inner typ" `Quick
        test_map_inherits_exact_width;
      Alcotest.test_case "exact width: UInt32.of_int range" `Quick
        test_uint32_of_int_range;
      Alcotest.test_case "exact width: int64 carrier has no range" `Quick
        test_encode_int64_carrier_has_no_range;
      Alcotest.test_case "8-byte unsigned fields preserve the wire" `Quick
        test_eight_byte_unsigned_preserves_wire;
      Alcotest.test_case "4-byte signed fields preserve the wire" `Quick
        test_four_byte_signed_preserves_wire;
      Alcotest.test_case "exact width: SInt32.of_int range" `Quick
        test_sint32_of_int_range;
      Alcotest.test_case "exact byte field: literal size" `Quick
        test_exact_byte_field_literal_size;
      Alcotest.test_case "exact byte field: expression size" `Quick
        test_exact_byte_field_expression_size;
      Alcotest.test_case "encode failure: destination is partial" `Quick
        test_encode_failure_leaves_partial_record;
      Alcotest.test_case "encode rejects: unlisted enum value" `Quick
        test_encode_rejects_unlisted_enum;
      Alcotest.test_case "enum: over a non-int carrier base" `Quick
        test_enum_over_carrier_base;
      Alcotest.test_case "variants: over a non-int carrier base" `Quick
        test_variants_over_carrier_base;
      Alcotest.test_case "lookup/bit: over a non-int carrier base" `Quick
        test_lookup_and_bit_over_carrier_base;
      Alcotest.test_case "enum: non-int carrier base projects to 3D" `Quick
        test_enum_over_carrier_base_projects;
      Alcotest.test_case "encode rejects: non-zero all_zeros" `Quick
        test_encode_rejects_non_zero_padding;
      Alcotest.test_case "encode rejects: refined byte span" `Quick
        test_encode_rejects_refined_byte;
      Alcotest.test_case "decode rejects: refined byte span" `Quick
        test_decode_rejects_refined_byte;
      Alcotest.test_case "decode accepts: conforming refined byte span" `Quick
        test_decode_accepts_conforming_refined_byte;
      Alcotest.test_case "refined byte span: decode and validate agree" `Quick
        test_refined_byte_decode_validate_agree;
      Alcotest.test_case "encode rejects: where violation" `Quick
        test_encode_rejects_where_violation;
      Alcotest.test_case "constant size expression folds" `Quick
        test_constant_size_expression_folds;
      Alcotest.test_case "exact byte field: set var byte_array" `Quick
        test_set_exact_var_byte_array;
      Alcotest.test_case "exact byte field: set var byte_array_where" `Quick
        test_set_exact_var_byte_array_where;
      Alcotest.test_case "exact byte field: set var byte_slice" `Quick
        test_set_exact_var_byte_slice;
      Alcotest.test_case "exact byte field: set dyn byte_array" `Quick
        test_set_exact_dyn_byte_array;
      Alcotest.test_case "exact byte field: set dyn byte_array_where" `Quick
        test_set_exact_dyn_byte_array_where;
      Alcotest.test_case "exact byte field: set dyn byte_slice" `Quick
        test_set_exact_dyn_byte_slice;
      Alcotest.test_case "exact byte field: set fixed byte_array" `Quick
        test_set_exact_fixed_byte_array;
      Alcotest.test_case "uint: 3-byte BE roundtrip" `Quick test_uint_3byte_be;
      Alcotest.test_case "uint: 1-byte like uint8" `Quick test_uint_1byte;
      Alcotest.test_case "uint: 5-byte LE roundtrip" `Quick test_uint_5byte_le;
      Alcotest.test_case "uint_var: dynamic size" `Quick test_uint_var_dynamic;
    ] )
