(* Tests for codec.ml: API misuse and accessor contracts. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

(* -- API misuse / safety tests -- *)

(* A description that cannot be decoded or encoded whatever the input is a
   programmer error, and [wire.mli] says so: it "raises [Invalid_argument], the
   same way encoding does". Eighteen sites raised [Failure] instead, thirteen of
   them reachable from a public entry point, which no documented caller has any
   reason to catch and which a [Codec.encode] caller cannot tell apart from a
   bug in the library. The evidence that the code was wrong rather than the doc
   is next door: an unbound field ref in a size expression already raised
   [Invalid_argument] while the same ref in a where clause raised [Failure].
   Every reachable site gets a case, so one that regresses names itself. *)

type misuse_v = A of UInt8.t | B of UInt8.t | Other of int

let misuse_casetype =
  casetype "MisuseCT" uint8
    [
      case ~index:(UInt8.v 1) uint8
        ~inject:(fun x -> A x)
        ~project:(function A x -> Some x | _ -> None);
      case ~index:(UInt8.v 2) uint8
        ~inject:(fun x -> B x)
        ~project:(function B x -> Some x | _ -> None);
    ]

let misuse_casetype_codec =
  Codec.v "MisuseCTRec" Fun.id Codec.[ Field.v "u" misuse_casetype $ Fun.id ]

let f_absent = Field.v "absent" uint8
let f_absent64 = Field.v "absent64" uint64be

let test_undecodable_description_is_invalid_argument () =
  let expect (name, f) =
    match f () with
    | () -> Alcotest.failf "%s: raised nothing" name
    | exception Invalid_argument _ -> ()
    | exception Failure m ->
        Alcotest.failf "%s: raised Failure %S, which the contract forbids" name
          m
  in
  (* [of_string] returning at all, [Ok] or [Error], is the failure [expect]
     reports. *)
  let returned = function Ok _ | Error _ -> () in
  List.iter expect
    [
      (* Direct decode and encode of a form with no reader or writer. *)
      ( "of_string type_ref",
        fun () -> returned (of_string (type_ref "T") "abcd") );
      ( "of_string qualified_ref",
        fun () -> returned (of_string (qualified_ref "M" "T") "abcd") );
      ( "of_string apply",
        fun () -> returned (of_string (apply uint8 [ int 1 ]) "abcd") );
      ("to_string type_ref", fun () -> ignore (to_string (type_ref "T") 0));
      ( "to_string qualified_ref",
        fun () -> ignore (to_string (qualified_ref "M" "T") 0) );
      ( "to_string apply",
        fun () -> ignore (to_string (apply uint8 [ int 1 ]) (UInt8.v 0)) );
      ( "to_string struct_typ",
        fun () ->
          ignore
            (to_string (struct_typ (struct_ "MisuseS" [ field "a" uint8 ])) ())
      );
      (* A cross-field reference evaluated where there is no record. *)
      ( "of_string size ref outside a struct",
        fun () ->
          returned (of_string (byte_array ~size:(Field.ref f_absent)) "\003abc")
      );
      ( "to_string size ref outside a struct",
        fun () ->
          ignore (to_string (byte_array ~size:(Field.ref f_absent)) "abc") );
      ( "of_string int64 ref outside a struct",
        fun () ->
          returned
            (of_string
               (where Expr.(Field.int64 f_absent64 = int64 0L) uint8)
               "\003") );
      (* A casetype value no case projects. *)
      ( "to_string casetype no case matches",
        fun () -> ignore (to_string misuse_casetype (Other 5)) );
      ( "Codec.encode casetype no case matches",
        fun () ->
          Codec.encode misuse_casetype_codec (Other 5) (Bytes.make 8 '\000') 0
      );
      (* A codec sealed against a field it never got. *)
      ( "Codec.v where ref to a field not in the codec",
        fun () ->
          ignore
            (Codec.v "MisuseWhere"
               ~where:Expr.(Field.ref f_absent = int 0)
               Fun.id
               Codec.[ Field.v "a" uint8 $ Fun.id ]) );
      ( "Codec.v constraint ref to a field not in the codec",
        fun () ->
          ignore
            (Codec.v "MisuseConstraint" Fun.id
               Codec.
                 [
                   Field.v "a" uint8
                     ~constraint_:Expr.(Field.ref f_absent = int 0)
                   $ Fun.id;
                 ]) );
    ]

(* [Codec.size_of_value] answers how many bytes [Codec.encode] will write, and a
   caller sizes its buffer from that answer. For a casetype value no case
   projects there are no such bytes: encode refuses it. Answering with the tag
   width instead sent the caller off to allocate for a write that cannot happen
   and meet the refusal one line later, so the two must refuse the same value in
   the same words. *)
let invalid_argument_message f =
  match f () with () -> None | exception Invalid_argument m -> Some m

let test_size_of_value_refuses_unmatched_case () =
  let buf = Bytes.make 8 '\000' in
  let sized =
    invalid_argument_message (fun () ->
        ignore (Codec.size_of_value misuse_casetype_codec (Other 5) : int))
  in
  (match sized with
  | None ->
      Alcotest.fail
        "Codec.size_of_value answered with a size for a value no case projects"
  | Some _ -> ());
  let encoded =
    invalid_argument_message (fun () ->
        Codec.encode misuse_casetype_codec (Other 5) buf 0)
  in
  Alcotest.(check (option string))
    "size_of_value refuses in encode's own words" encoded sized;
  (* The control: a value a case does project still sizes what encode writes. *)
  let n = Codec.size_of_value misuse_casetype_codec (B (UInt8.v 9)) in
  Codec.encode misuse_casetype_codec (B (UInt8.v 9)) buf 0;
  Alcotest.(check int) "a projected value sizes its tag and its body" 2 n;
  Alcotest.(check string)
    "and encode wrote exactly those bytes" "\002\009" (Bytes.sub_string buf 0 n)

let test_get_field_notin_codec () =
  (* get with a field that was never added to this codec raises Not_found
     at staging time *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX" (fun v -> v) [ cf_x ] in
  match Codec.get codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions field name" true
        (Re.execp (Re.compile (Re.str "y")) msg);
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "OnlyX")) msg)

let test_set_field_notin_codec () =
  (* set with a field not in the codec raises Invalid_argument at staging *)
  let cf_x = Codec.(Field.v "x" uint8 $ fun v -> v) in
  let cf_y = Codec.(Field.v "y" uint8 $ fun v -> v) in
  let codec = Codec.v "OnlyX2" (fun v -> v) [ cf_x ] in
  match Codec.set codec cf_y with
  | _ -> Alcotest.fail "expected Invalid_argument for unknown field"
  | exception Invalid_argument _ -> ()

let test_bitfield_on_non_bitfield () =
  (* [bitfield] takes an int-valued field; a whole byte mapped to [int] is one
     that is still not a bitfield. *)
  let cf_x =
    Codec.(
      Field.v "x" (map ~decode:UInt8.to_int ~encode:UInt8.v uint8) $ fun v -> v)
  in
  let codec = Codec.v "NoBf" (fun v -> v) [ cf_x ] in
  match Codec.bitfield codec cf_x with
  | _ -> Alcotest.fail "expected error for non-bitfield"
  | exception Invalid_argument _ -> ()

let expect_foreign_field ~op ~field ~codec f =
  match f () with
  | () -> Alcotest.failf "expected Invalid_argument from %s" op
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "names the operation" true (contains ~sub:op msg);
      Alcotest.(check bool) "names the field" true (contains ~sub:field msg);
      Alcotest.(check bool) "names the codec" true (contains ~sub:codec msg)

let test_lookalike_field_handles () =
  let word = Field.v "word" uint16be in
  let word_field = Codec.(word $ Fun.id) in
  let word_again = Codec.(word $ Fun.id) in
  let lookalike_word = Codec.(Field.v "word" uint16 $ Fun.id) in
  let word_codec = Codec.v "WordOwner" Fun.id [ word_field ] in
  let buf = Bytes.of_string "\x12\x34" in
  let read = Staged.unstage (Codec.get word_codec word_again) in
  Alcotest.(check int)
    "rebinding the same Field.t works" 0x1234
    (UInt16.to_int (read buf 0));
  expect_foreign_field ~op:"Codec.get" ~field:"word" ~codec:"WordOwner"
    (fun () -> ignore (Codec.get word_codec lookalike_word));
  expect_foreign_field ~op:"Codec.set" ~field:"word" ~codec:"WordOwner"
    (fun () -> ignore (Codec.set word_codec lookalike_word));

  let flags = Field.v "flags" (bits ~width:4 U16be) in
  let flags_field = Codec.(flags $ Fun.id) in
  let lookalike_flags = Codec.(Field.v "flags" (bits ~width:4 U16) $ Fun.id) in
  let flags_codec = Codec.v "FlagsOwner" Fun.id [ flags_field ] in
  expect_foreign_field ~op:"Codec.bitfield" ~field:"flags" ~codec:"FlagsOwner"
    (fun () -> ignore (Codec.bitfield flags_codec lookalike_flags));

  let payload = Field.v "payload" (byte_slice ~size:(int 2)) in
  let payload_field = Codec.(payload $ Fun.id) in
  let lookalike_payload =
    Codec.(Field.v "payload" (byte_slice ~size:(int 1)) $ Fun.id)
  in
  let payload_codec = Codec.v "PayloadOwner" Fun.id [ payload_field ] in
  expect_foreign_field ~op:"Codec.slice_offset" ~field:"payload"
    ~codec:"PayloadOwner" (fun () ->
      ignore (Codec.slice_offset payload_codec lookalike_payload));
  expect_foreign_field ~op:"Codec.slice_length" ~field:"payload"
    ~codec:"PayloadOwner" (fun () ->
      ignore (Codec.slice_length payload_codec lookalike_payload))

let expect_foreign_env ~op ~codec f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument from %s" op
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "names the operation" true (contains ~sub:op msg);
      Alcotest.(check bool) "names the codec" true (contains ~sub:codec msg)

let test_foreign_env_codec_operations () =
  let p_a = Param.input "limit_a" uint8 in
  let f_a = Field.v "v" uint8 in
  let codec_a =
    Codec.v "EnvA"
      ~where:Expr.(Field.ref f_a <= Param.expr p_a)
      Fun.id
      Codec.[ f_a $ Fun.id ]
  in
  let p_b = Param.input "limit_b" uint8 in
  let f_b = Field.v "v" uint8 in
  let codec_b =
    Codec.v "EnvB"
      ~where:Expr.(Field.ref f_b <= Param.expr p_b)
      Fun.id
      Codec.[ f_b $ Fun.id ]
  in
  let env_b = Codec.env codec_b |> Param.bind p_b (UInt8.v 0xff) in
  let buf = Bytes.of_string "\x01" in
  expect_foreign_env ~op:"Codec.encode" ~codec:"EnvA" (fun () ->
      Codec.encode ~env:env_b codec_a (UInt8.v 1) buf 0);
  expect_foreign_env ~op:"Codec.decode" ~codec:"EnvA" (fun () ->
      Codec.decode ~env:env_b codec_a buf 0);
  expect_foreign_env ~op:"Codec.validate" ~codec:"EnvA" (fun () ->
      Codec.validate ~env:env_b codec_a buf 0);
  let p_c = Param.input "limit_c" uint8 in
  let p_d = Param.input "floor_d" uint8 in
  let f_c = Field.v "v" uint8 in
  let codec_c =
    Codec.v "EnvC"
      ~where:
        Expr.(
          Field.ref f_c <= Param.expr p_c && Field.ref f_c >= Param.expr p_d)
      Fun.id
      Codec.[ f_c $ Fun.id ]
  in
  let env_a = Codec.env codec_a |> Param.bind p_a (UInt8.v 0xff) in
  expect_foreign_env ~op:"Codec.encode" ~codec:"EnvC" (fun () ->
      Codec.encode ~env:env_a codec_c (UInt8.v 1) buf 0);
  expect_foreign_env ~op:"Codec.decode" ~codec:"EnvC" (fun () ->
      Codec.decode ~env:env_a codec_c buf 0)

let test_env_from_wrong_codec () =
  (* Using env from codec1 with get ~env on codec2 *)
  let out1 = Param.output "out_wrong" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v1 =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out1 (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec1 = Codec.v "Wrong1" (fun v -> v) [ cf_v1 ] in
  let cf_v2 = Codec.(Field.v "v" uint8 $ fun v -> v) in
  let codec2 = Codec.v "Wrong2" (fun v -> v) [ cf_v2 ] in
  let env1 = Codec.env codec1 in
  expect_foreign_env ~op:"Codec.get" ~codec:"Wrong2" (fun () ->
      Codec.get ~env:env1 codec2 cf_v2)

let test_env_wrongcodec_with_action () =
  (* Using env from a different codec with an action field.
     The env has too few param slots -- get raises Invalid_argument
     at staging time. *)
  let out = Param.output "out_oob" uint8 in
  let f_ref = Field.v "v" uint8 in
  let cf_v =
    Codec.(
      Field.v "v"
        ~action:(Action.on_success [ Action.assign out (Field.ref f_ref) ])
        uint8
      $ fun v -> v)
  in
  let codec_with_action = Codec.v "WithAct" (fun v -> v) [ cf_v ] in
  (* codec_empty has zero params, so its env has slots = [||] *)
  let cf_w = Codec.(Field.v "w" uint8 $ fun v -> v) in
  let codec_empty = Codec.v "NoParams" (fun v -> v) [ cf_w ] in
  let wrong_env = Codec.env codec_empty in
  match Codec.get ~env:wrong_env codec_with_action cf_v with
  | _ -> Alcotest.fail "expected Invalid_argument for wrong env"
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        "mentions codec name" true
        (Re.execp (Re.compile (Re.str "WithAct")) msg)

let test_decode_short_buffer () =
  (* Decode with buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "Short" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.decode codec buf 0 with
  | Ok _ -> Alcotest.fail "expected EOF error"
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "wrong error: %a" pp_parse_error e

let test_encode_short_buffer () =
  (* Encode into buffer shorter than wire_size *)
  let cf_x = Codec.(Field.v "x" uint16be $ fun v -> v) in
  let codec = Codec.v "ShortEnc" (fun v -> v) [ cf_x ] in
  let buf = Bytes.of_string "\x42" in
  match Codec.encode codec (UInt16.v 0x1234) buf 0 with
  | () -> Alcotest.fail "expected error for short buffer"
  | exception Invalid_argument _ -> ()

(* -- Accessor and container contracts -- *)

(* [Codec.set] documents [Invalid_argument] "leaving the buffer untouched". A
   sub-codec field writes the whole sub-record and validates it afterwards, so
   the refusal used to arrive with the rejected byte already on the wire. *)
let test_set_refusal_leaves_buffer_untouched () =
  let inner_f =
    Field.v "v" uint8 ~self_constraint:(fun r ->
        Expr.(r >= int 10 && r <= int 100))
  in
  let inner = Codec.v "SetRollbackInner" Fun.id Codec.[ inner_f $ Fun.id ] in
  let outer_f = Codec.(Field.v "v" (codec inner) $ Fun.id) in
  let outer = Codec.v "SetRollbackOuter" Fun.id Codec.[ outer_f ] in
  let set = Staged.unstage (Codec.set outer outer_f) in
  let buf = Bytes.make 1 '\xee' in
  (match set buf 0 (UInt8.v 200) with
  | () -> Alcotest.fail "Codec.set accepted a value the sub-codec refuses"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string)
    "buffer untouched after a refused set" "\xee" (Bytes.to_string buf);
  set buf 0 (UInt8.v 42);
  Alcotest.(check string)
    "an accepted set still writes" "\x2a" (Bytes.to_string buf)

(* A bitfield accessor takes its offset from the caller, so it has to check
   that its base word is in the buffer. The U32 word helpers reached the bytes
   unchecked, so [Codec.get] on a frame shorter than the word answered with
   whatever followed the buffer -- the same bytes read one value at base 0 and
   another once the frame moved along the buffer, and neither was in the frame
   -- while [Codec.set] laid down the low half of the word before the high half
   raised. Walked over every base and every offset that puts part of the word
   past the end: only the U32 bases were unchecked, and the half-written word
   only shows on a field the low half carries. *)
let bounds_bitpack base bit_order (w_a, w_b, w_c) =
  let f_a = Field.v "a" (bits ~bit_order ~width:w_a base) in
  let f_b = Field.v "b" (bits ~bit_order ~width:w_b base) in
  let f_c = Field.v "c" (bits ~bit_order ~width:w_c base) in
  let proj_a (a, _, _) = a and proj_b (_, b, _) = b and proj_c (_, _, c) = c in
  let codec =
    Codec.v "BfBounds"
      (fun a b c -> (a, b, c))
      Codec.[ f_a $ proj_a; f_b $ proj_b; f_c $ proj_c ]
  in
  ( codec,
    [
      ("a", Codec.( $ ) f_a proj_a);
      ("b", Codec.( $ ) f_b proj_b);
      ("c", Codec.( $ ) f_c proj_c);
    ] )

let bitpack_bases =
  [
    ("U16", U16, Msb_first, (3, 2, 11));
    ("U16be", U16be, Lsb_first, (3, 2, 11));
    ("U32", U32, Msb_first, (6, 10, 16));
    ("U32be", U32be, Lsb_first, (6, 10, 16));
  ]

(* Every (base, field) pair with the word size the base needs. *)
let iter_bitpacks f =
  List.iter
    (fun (name, base, bit_order, widths) ->
      let codec, fields = bounds_bitpack base bit_order widths in
      let word = Codec.wire_size codec in
      List.iter (fun (fname, field) -> f name word fname field codec) fields)
    bitpack_bases

let test_get_bitfield_word_past_buffer_raises () =
  iter_bitpacks (fun name word fname field codec ->
      let get = Staged.unstage (Codec.get codec field) in
      let load =
        Staged.unstage (Codec.load_word (Codec.bitfield codec field))
      in
      let expect_refusal reader len off =
        match reader (Bytes.make len '\xff') off with
        | v ->
            Alcotest.failf
              "%s field %s: read %d at offset %d of a %d-byte buffer" name fname
              v off len
        | exception Invalid_argument _ -> ()
      in
      List.iter
        (fun reader ->
          (* No room for the word at all, and room for it but not where the
             frame starts. *)
          for len = 0 to word - 1 do
            expect_refusal reader len 0
          done;
          for off = 1 to word do
            expect_refusal reader word off
          done)
        [ get; (fun buf off -> Optint.to_int (load buf off)) ])

let test_set_bitfield_word_past_buffer_writes_nothing () =
  iter_bitpacks (fun name word fname field codec ->
      let set = Staged.unstage (Codec.set codec field) in
      for len = 0 to word - 1 do
        let buf = Bytes.make len '\x00' in
        (match set buf 0 1 with
        | () ->
            Alcotest.failf
              "%s field %s: Codec.set wrote a word into a %d-byte buffer" name
              fname len
        | exception Invalid_argument _ -> ());
        Alcotest.(check string)
          (Fmt.str "%s field %s: %d-byte buffer untouched" name fname len)
          (String.make len '\x00') (Bytes.to_string buf)
      done)

(* [Codec.get] had no reader for a statically gated optional and fell through
   to a bare [Failure], on fields [Codec.decode] reads without trouble. Check
   both flavours and both gates against what decode returns. *)
let test_get_static_optional_agrees_with_decode () =
  let f_len =
    Field.optional_or "Len" ~present:Expr.true_ ~default:UInt8.zero uint8
  in
  let f_on = Field.optional "On" ~present:Expr.true_ uint8 in
  let f_off = Field.optional "Off" ~present:Expr.false_ uint8 in
  let f_or_off =
    Field.optional_or "OrOff" ~present:Expr.false_ ~default:(UInt8.v 7) uint8
  in
  let f_data = Field.v "Data" (byte_array ~size:(Field.ref f_len)) in
  let cf_len = Codec.(f_len $ fun (l, _, _, _, _) -> l) in
  let cf_on = Codec.(f_on $ fun (_, o, _, _, _) -> o) in
  let cf_off = Codec.(f_off $ fun (_, _, o, _, _) -> o) in
  let cf_or_off = Codec.(f_or_off $ fun (_, _, _, o, _) -> o) in
  let cf_data = Codec.(f_data $ fun (_, _, _, _, d) -> d) in
  let c =
    Codec.v "StaticGates"
      (fun len on off or_off data -> (len, on, off, or_off, data))
      [ cf_len; cf_on; cf_off; cf_or_off; cf_data ]
  in
  let buf = Bytes.of_string "\003\009abc" in
  let len, on, off, or_off, _ = decode_ok (Codec.decode c buf 0) in
  let read f = Staged.unstage (Codec.get c f) buf 0 in
  Alcotest.(check int)
    "optional_or, gate on" (UInt8.to_int len)
    (UInt8.to_int (read cf_len));
  Alcotest.(check (option int))
    "optional, gate on"
    (Option.map UInt8.to_int on)
    (Option.map UInt8.to_int (read cf_on));
  Alcotest.(check (option int))
    "optional, gate off"
    (Option.map UInt8.to_int off)
    (Option.map UInt8.to_int (read cf_off));
  Alcotest.(check int)
    "optional_or, gate off is the default" (UInt8.to_int or_off)
    (UInt8.to_int (read cf_or_off));
  Alcotest.(check string) "span sized by the optional_or" "abc" (read cf_data);
  (* And the matching setter, so a field [get] can read is one [set] can
     write. The absent gates own no bytes, so writing them moves nothing. *)
  Staged.unstage (Codec.set c cf_on) buf 0 (Some (UInt8.v 5));
  Staged.unstage (Codec.set c cf_off) buf 0 None;
  Staged.unstage (Codec.set c cf_or_off) buf 0 (UInt8.v 7);
  Alcotest.(check (option int))
    "set then get, gate on" (Some 5)
    (Option.map UInt8.to_int (read cf_on));
  Alcotest.(check string)
    "the absent gates wrote no bytes" "\003\005abc" (Bytes.to_string buf)

(* A runtime gate is decided by an expression over sibling fields, which only
   the compiled record plan evaluates. The accessor cannot read it, and has to
   say so with the exception the interface documents. *)
let test_get_dynamic_optional_refuses_cleanly () =
  let f_gate = Field.v "gate" uint8 in
  let f_pay =
    Field.optional "pay" ~present:Expr.(Field.ref f_gate <> int 0) uint16be
  in
  let cf_pay = Codec.(f_pay $ snd) in
  let c =
    Codec.v "DynGate"
      (fun gate pay -> (gate, pay))
      [ Codec.(f_gate $ fst); cf_pay ]
  in
  Alcotest.(check bool)
    "Codec.get refuses a runtime-gated optional with Invalid_argument" true
    (raises_invalid (fun () -> Codec.get c cf_pay))

(* A [repeat] element whose decoder runs to the end of the buffer cannot be
   iterated: the first element takes the whole budget, so anything the encoder
   writes past it is bytes the decoder (and the EverParse validator built from
   the same schema) refuses. The greedy tail counts whether it is the element's
   own last field or sits at the end of a sub-codec the element ends with. *)
let test_repeat_rejects_nested_greedy_element () =
  let pad =
    Codec.v "PadTail"
      (fun n tail -> (n, tail))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "tail" all_zeros $ snd ]
  in
  let greedy_elem =
    Codec.v "GreedyElem"
      (fun a b -> (a, b))
      Codec.[ Field.v "f0" uint8 $ fst; Field.v "f1" (codec pad) $ snd ]
  in
  Alcotest.(check bool)
    "repeat over a codec whose own tail is greedy rejected" true
    (raises_invalid (fun () -> Field.repeat "items" ~size:(int 12) (codec pad)));
  Alcotest.(check bool)
    "repeat over a codec ending in a greedy sub-codec rejected" true
    (raises_invalid (fun () ->
         Field.repeat "items" ~size:(int 12) (codec greedy_elem)))

(* The control for the rejection above: give the same element a bounded tail
   and it is self-delimiting again, so the repeat builds and more than one
   element survives the round trip. *)
let test_repeat_bounded_element_roundtrips () =
  let bounded =
    Codec.v "PadFixed"
      (fun n tail -> (n, tail))
      Codec.
        [
          Field.v "n" uint8 $ fst;
          Field.v "tail" (byte_array ~size:(int 2)) $ snd;
        ]
  in
  let elem =
    Codec.v "BoundedElem"
      (fun a b -> (a, b))
      Codec.[ Field.v "f0" uint8 $ fst; Field.v "f1" (codec bounded) $ snd ]
  in
  let f_total = Field.v "total" uint8 in
  let f_items = Field.repeat "items" ~size:(Field.ref f_total) (codec elem) in
  let outer =
    Codec.v "BoundedRep"
      (fun _ xs -> xs)
      Codec.
        [
          (f_total $ fun xs -> UInt8.v (4 * List.length xs));
          (f_items $ fun xs -> xs);
        ]
  in
  let xs =
    [
      (UInt8.v 1, (UInt8.v 2, "\000\000")); (UInt8.v 3, (UInt8.v 4, "\000\000"));
    ]
  in
  let buf = Bytes.create (Codec.size_of_value outer xs) in
  Codec.encode outer xs buf 0;
  let ys = decode_ok (Codec.decode outer buf 0) in
  let elem_equal (a, (b, s)) (a', (b', s')) =
    UInt8.equal a a' && UInt8.equal b b' && String.equal s s'
  in
  Alcotest.(check bool)
    "two bounded elements round-trip" true
    (List.equal elem_equal ys xs)

let suite =
  ( "codec_misuse",
    [
      Alcotest.test_case "misuse: get field not in codec" `Quick
        test_get_field_notin_codec;
      Alcotest.test_case "misuse: set field not in codec" `Quick
        test_set_field_notin_codec;
      Alcotest.test_case "misuse: bitfield on non-bitfield" `Quick
        test_bitfield_on_non_bitfield;
      Alcotest.test_case "misuse: lookalike field handles" `Quick
        test_lookalike_field_handles;
      Alcotest.test_case "misuse: foreign env codec operations" `Quick
        test_foreign_env_codec_operations;
      Alcotest.test_case "misuse: env from wrong codec" `Quick
        test_env_from_wrong_codec;
      Alcotest.test_case "misuse: wrong env with action" `Quick
        test_env_wrongcodec_with_action;
      Alcotest.test_case "misuse: decode short buffer" `Quick
        test_decode_short_buffer;
      Alcotest.test_case "misuse: encode short buffer" `Quick
        test_encode_short_buffer;
      Alcotest.test_case "set: a refused sub-codec value writes nothing" `Quick
        test_set_refusal_leaves_buffer_untouched;
      Alcotest.test_case
        "misuse: an undecodable description raises Invalid_argument" `Quick
        test_undecodable_description_is_invalid_argument;
      Alcotest.test_case
        "size_of_value: a casetype value no case projects raises" `Quick
        test_size_of_value_refuses_unmatched_case;
      Alcotest.test_case "get: a bitfield word past the buffer raises" `Quick
        test_get_bitfield_word_past_buffer_raises;
      Alcotest.test_case "set: a bitfield word past the buffer writes nothing"
        `Quick test_set_bitfield_word_past_buffer_writes_nothing;
      Alcotest.test_case "get: statically gated optionals agree with decode"
        `Quick test_get_static_optional_agrees_with_decode;
      Alcotest.test_case
        "get: runtime-gated optional refused with Invalid_argument" `Quick
        test_get_dynamic_optional_refuses_cleanly;
      Alcotest.test_case "repeat: element ending in a greedy sub-codec rejected"
        `Quick test_repeat_rejects_nested_greedy_element;
      Alcotest.test_case "repeat: bounded element round-trips" `Quick
        test_repeat_bounded_element_roundtrips;
    ] )
