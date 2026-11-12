(* Tests for codec.ml: variable-size elements and reads past the end. *)

open Wire
open Test_helpers

(* Field.repeat over a zeroterm element: a list of NUL-terminated strings
   within a byte budget. Used to raise Failure at decode; now decodes and
   projects through a synthesised element struct. *)
type zt_rep = { zn : UInt16.t; names : string list }

let zt_rep_codec =
  let f_n = Field.v "n" uint16be in
  let f_names = Field.repeat "names" ~size:(Field.ref f_n) zeroterm in
  Codec.v "ZtRep"
    (fun zn names -> { zn; names })
    Codec.[ (f_n $ fun r -> r.zn); (f_names $ fun r -> r.names) ]

let test_repeat_zeroterm_element () =
  let v = { zn = UInt16.v 12; names = [ "abc"; "de"; "fghi" ] } in
  let sz = Codec.size_of_value zt_rep_codec v in
  Alcotest.(check int) "wire size" 14 sz;
  let buf = Bytes.create sz in
  Codec.encode zt_rep_codec v buf 0;
  match Codec.decode zt_rep_codec buf 0 with
  | Ok d -> Alcotest.(check (list string)) "names" v.names d.names
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_repeat_zeroterm_projection () =
  let out = render_3d zt_rep_codec in
  Alcotest.(check bool)
    "zeroterm element wrapped in a struct" true
    (contains ~sub:"ZtElem names[:byte-size n]" out)

(* Dynamic [Field.optional] over a variable-size inner. Group A: a byte array
   sized by a prior field. The gate drives present/absent, and both round-trip
   (absent consumes no bytes, present consumes the inner). *)
type opt_var = { gate : UInt8.t; len : UInt8.t; body : string option }

let opt_var_codec =
  let f_gate = Field.v "gate" uint8 in
  let f_len = Field.v "len" uint8 in
  let f_body =
    Field.optional "body"
      ~present:Expr.(Field.ref f_gate <> int 0)
      (byte_array ~size:(Field.ref f_len))
  in
  Codec.v "OptVar"
    (fun gate len body -> { gate; len; body })
    Codec.
      [
        (f_gate $ fun r -> r.gate);
        (f_len $ fun r -> r.len);
        (f_body $ fun r -> r.body);
      ]

let roundtrip codec v =
  let n = Codec.size_of_value codec v in
  let buf = Bytes.create n in
  Codec.encode codec v buf 0;
  (n, Codec.decode_exn codec buf 0)

let test_optional_var_byte_array () =
  let n, d =
    roundtrip opt_var_codec
      { gate = UInt8.v 1; len = UInt8.v 3; body = Some "abc" }
  in
  Alcotest.(check int) "present size" 5 n;
  Alcotest.(check (option string)) "present body" (Some "abc") d.body;
  let n, d =
    roundtrip opt_var_codec { gate = UInt8.v 0; len = UInt8.v 0; body = None }
  in
  Alcotest.(check int) "absent size" 2 n;
  Alcotest.(check (option string)) "absent body" None d.body

(* Group B: a self-delimiting sub-codec (its own length prefix) as the optional
   inner. *)
let sub_string_codec =
  let f_slen = Field.v "slen" uint8 in
  Codec.v "OptSub"
    (fun _slen s -> s)
    Codec.
      [
        (f_slen $ fun x -> UInt8.v (String.length x));
        Field.v "sdata" (byte_array ~size:(Field.ref f_slen)) $ Fun.id;
      ]

type opt_sub = { g : UInt8.t; desc : string option }

let opt_sub_codec =
  let f_g = Field.v "g" uint8 in
  let f_desc =
    Field.optional "desc"
      ~present:Expr.(Field.ref f_g <> int 0)
      (codec sub_string_codec)
  in
  Codec.v "OptSubRec"
    (fun g desc -> { g; desc })
    Codec.[ (f_g $ fun r -> r.g); (f_desc $ fun r -> r.desc) ]

let test_optional_self_delimiting_codec () =
  let _, d = roundtrip opt_sub_codec { g = UInt8.v 1; desc = Some "hi" } in
  Alcotest.(check (option string)) "present desc" (Some "hi") d.desc;
  let n, d = roundtrip opt_sub_codec { g = UInt8.v 0; desc = None } in
  Alcotest.(check int) "absent size" 1 n;
  Alcotest.(check (option string)) "absent desc" None d.desc

(* A conditional group can carry its own length prefix and dependent list.
   Keeping those fields in a sub-codec lets their expressions stay local to
   the group, while a following payload remains in the parent codec. This is
   the shape of an optional secondary header that declares its own length. *)
type transfer_extensions = { items_len : UInt8.t; items : UInt8.t list }

let transfer_extensions_codec =
  let f_len = Field.v "ext_items_len" uint8 in
  let f_items = Field.repeat "ext_items" ~size:(Field.ref f_len) uint8 in
  Codec.v "TransferExtensions"
    (fun items_len items -> { items_len; items })
    Codec.[ (f_len $ fun r -> r.items_len); (f_items $ fun r -> r.items) ]

type transfer_segment = {
  start : UInt8.t;
  extensions : transfer_extensions option;
  data_len : UInt8.t;
  data : string;
}

let transfer_segment_codec =
  let f_start = Field.v "start" uint8 in
  let f_extensions =
    Field.optional "extensions"
      ~present:Expr.(Field.ref f_start <> int 0)
      (codec transfer_extensions_codec)
  in
  let f_data_len = Field.v "data_len" uint8 in
  let f_data = Field.v "data" (byte_array ~size:(Field.ref f_data_len)) in
  Codec.v "TransferSegment"
    (fun start extensions data_len data ->
      { start; extensions; data_len; data })
    Codec.
      [
        (f_start $ fun r -> r.start);
        (f_extensions $ fun r -> r.extensions);
        (f_data_len $ fun r -> r.data_len);
        (f_data $ fun r -> r.data);
      ]

let test_optional_length_prefixed_group () =
  let present =
    {
      start = UInt8.v 1;
      extensions =
        Some { items_len = UInt8.v 2; items = [ UInt8.v 0xA1; UInt8.v 0xB2 ] };
      data_len = UInt8.v 3;
      data = "xyz";
    }
  in
  let n, decoded = roundtrip transfer_segment_codec present in
  Alcotest.(check int) "present size" 8 n;
  Alcotest.(check (list int))
    "extension items" [ 0xA1; 0xB2 ]
    (List.map UInt8.to_int (Option.get decoded.extensions).items);
  Alcotest.(check string) "following data" "xyz" decoded.data;
  let absent =
    { start = UInt8.zero; extensions = None; data_len = UInt8.v 2; data = "ok" }
  in
  let n, decoded = roundtrip transfer_segment_codec absent in
  Alcotest.(check int) "absent size" 4 n;
  Alcotest.(check bool)
    "extensions absent" true
    (Option.is_none decoded.extensions);
  Alcotest.(check string) "following data" "ok" decoded.data;
  let out = render_3d transfer_segment_codec in
  Alcotest.(check bool)
    "group is its own struct" true
    (contains ~sub:"typedef struct WireTransferExtensions" out);
  Alcotest.(check bool)
    "group-local dependent repeat" true
    (contains ~sub:"UINT8 ext_items[:byte-size ext_items_len]" out);
  Alcotest.(check bool)
    "group is gate-selected" true
    (contains ~sub:"Opt_TransferExtensions(" out);
  Alcotest.(check bool)
    "later fields stay in the parent" true
    (contains ~sub:"UINT8 data[:byte-size data_len]" out)

(* Wire.array over a fixed byte_array element: a fixed-count list of n-byte
   chunks (e.g. an array of IPv4 addresses). Used to project a double
   [:byte-size]; the element is now emitted as bare bytes under the budget. *)
type arr_chunks = { atag : UInt8.t; addrs : string list }

let arr_chunks_codec =
  let f_tag = Field.v "tag" uint8 in
  let f_addrs =
    Field.v "addrs" (array ~len:(int 3) (byte_array ~size:(int 4)))
  in
  Codec.v "ArrChunks"
    (fun atag addrs -> { atag; addrs })
    Codec.[ (f_tag $ fun r -> r.atag); (f_addrs $ fun r -> r.addrs) ]

let test_array_byte_array_element () =
  let v = { atag = UInt8.v 7; addrs = [ "aaaa"; "bbbb"; "cccc" ] } in
  let sz = Codec.size_of_value arr_chunks_codec v in
  Alcotest.(check int) "wire size" 13 sz;
  let buf = Bytes.create sz in
  Codec.encode arr_chunks_codec v buf 0;
  match Codec.decode arr_chunks_codec buf 0 with
  | Ok d -> Alcotest.(check (list string)) "addrs" v.addrs d.addrs
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_array_byte_array_projection () =
  let out = render_3d arr_chunks_codec in
  Alcotest.(check bool)
    "single byte-size on the array field" true
    (contains ~sub:"UINT8 addrs[:byte-size (3 * 4)]" out)

(* A [rest_bytes] tail projects to [byte-size (total - sizeof(this))]. EverParse
   cannot prove that subtraction non-underflowing on its own, so the projection
   emits the guard [total >= sizeof(this)] as a refinement on the preceding
   scalar field, which discharges it. Without the guard the schema fails
   EverParse verification ("cannot verify u32 subtraction"). *)
let rest_bytes_codec =
  let total = Param.input "msglen" uint16be in
  Codec.v "RestProj"
    (fun h t -> (h, t))
    Codec.
      [
        (Field.v "hdr" uint8 $ fun (h, _) -> h);
        (Field.v "rest" (rest_bytes total) $ fun (_, t) -> t);
      ]

let test_rest_bytes_projection_guard () =
  let out = render_3d rest_bytes_codec in
  Alcotest.(check bool)
    "rest field is sized by the subtraction" true
    (contains ~sub:"rest[:byte-size (msglen - sizeof (this))]" out);
  Alcotest.(check bool)
    "preceding field guards the subtraction" true
    (contains ~sub:"msglen >= sizeof (this)" out)

(* A [Field.repeat] byte-budget comes from a length field, i.e. untrusted input.
   A length larger than the buffer (or an offset overrun past it from a
   preceding variable field) must fail with a clean [Parse_error], not crash the
   decoder with an out-of-range [Bytes.sub]. *)
let f_rep_len = Field.v "len" uint16be

(* An embedded sub-codec whose length field overruns the buffer leaves the
   following field at an offset past the end; the trailing [all_zeros] then read
   a negative-length span. This is the [(rep, all_zeros)] shape the composer
   found. *)
let rep_subcodec =
  Codec.v "Rep"
    (fun n xs -> (n, xs))
    Codec.
      [
        (f_rep_len $ fun (n, _) -> n);
        ( Field.repeat "items" ~size:(Field.ref f_rep_len)
            (byte_array ~size:(int 3))
        $ fun (_, xs) -> xs );
      ]

let oversized_repeat_codec =
  Codec.v "OverRep"
    (fun r z -> (r, z))
    Codec.
      [
        (Field.v "rep" (codec rep_subcodec) $ fun (r, _) -> r);
        (Field.v "az" all_zeros $ fun (_, z) -> z);
      ]

let test_repeat_oversized_length_rejected () =
  let decodes bs =
    match Codec.decode oversized_repeat_codec (Bytes.of_string bs) 0 with
    | Ok _ | Error _ -> true
    | exception Invalid_argument _ -> false
  in
  Alcotest.(check bool)
    "oversized embedded length fails cleanly, no crash" true
    (decodes "\xff\xff");
  Alcotest.(check bool)
    "length past buffer fails cleanly" true (decodes "\x00\x10abc")

(* [Codec.validate] never walks a record field by field the way decode does: it
   reaches each field's bytes through that field's populate, at the offset the
   layout computes. An overlong [Field.repeat] budget pushes every following
   field past the end of the buffer, and the populate used to read there
   unguarded. The trailing [all_zeros] keeps the record greedy, so the top-level
   bounds check resolves the record's span to the whole buffer and lets it
   through; the constrained fields make [validate] run the populate pass at all.
   Shape and bytes come from the composer's [(rep:fint,i8,uv3le,(bnd,i16,az))]
   sample. *)
let validate_overrun_codec =
  let f_a = Field.v "a" uint8 in
  let f_b =
    Field.v "b" uint8 ~self_constraint:(fun b ->
        Expr.(Field.int f_a + b < int 10))
  in
  let pair =
    Codec.v "Pair" (fun a b -> (a, b)) Codec.[ f_a $ fst; f_b $ snd ]
  in
  let f_total = Field.v "total" uint16be in
  let rep =
    Codec.v "Rep"
      (fun _ xs -> xs)
      Codec.
        [
          (f_total $ fun _ -> UInt16.zero);
          ( Field.repeat "items" ~size:(Field.ref f_total) (codec pair)
          $ fun xs -> xs );
        ]
  in
  let bounded =
    Codec.v "Bounded"
      (fun v -> v)
      Codec.
        [
          ( Field.v "v" uint8 ~self_constraint:(fun r ->
                Expr.(r >= int 10 && r <= int 100))
          $ fun v -> v );
        ]
  in
  let tail =
    Codec.v "Tail"
      (fun x y z -> (x, y, z))
      Codec.
        [
          (Field.v "bnd" (codec bounded) $ fun (x, _, _) -> x);
          (Field.v "i16" int16be $ fun (_, y, _) -> y);
          (Field.v "az" all_zeros $ fun (_, _, z) -> z);
        ]
  in
  Codec.v "Overrun"
    (fun w x y z -> (w, x, y, z))
    Codec.
      [
        (Field.v "rep" (codec rep) $ fun (w, _, _, _) -> w);
        (Field.v "i8" int8 $ fun (_, x, _, _) -> x);
        (Field.v "uv" (uint ~endian:Wire.Little 3) $ fun (_, _, y, _) -> y);
        (Field.v "tail" (codec tail) $ fun (_, _, _, z) -> z);
      ]

(* The composer's sample: a 0xAA0F byte budget on a 64-byte buffer. *)
let validate_overrun_input =
  "\xaa\x0f\x9bFZ\x12\xef\x99\x05\xc5<\xac\xcfx\xd4Q\xfai\xf1RE\xb7S\xe6\xb0\xb8\xdfLjO\xbc\xc9\xfb\xf1\xf1\xc6\xa2\xcaS\xba\x0f\xda\x8e=/\xceC?\x8d\x0cG\x92%\xa1\xbc\xaf\xf6\x18\x9d\x90\x82:\xbd\xac"

let test_validate_overrun_field_offset () =
  let buf = Bytes.of_string validate_overrun_input in
  (match Codec.validate validate_overrun_codec buf 0 with
  | () ->
      Alcotest.fail "validate accepted a record whose fields run off the end"
  | exception Wire.Parse_error _ -> ()
  | exception Invalid_argument m ->
      Alcotest.failf "validate crashed instead of failing cleanly: %s" m);
  match Codec.decode validate_overrun_codec buf 0 with
  | Ok _ ->
      Alcotest.fail "decode accepted a record whose fields run off the end"
  | Error _ -> ()
  | exception Invalid_argument m ->
      Alcotest.failf "decode crashed instead of failing cleanly: %s" m

(* Assert a *located* end-of-input: the failure kind, the extent the field was
   missing, and the offset it was missing it at. Checking only the exception
   class would also pass for a blanket [Invalid_argument -> Parse_error]
   wrapper around validate, which reports a garbage location; the offset is
   what shows a real per-field guard ran, at the field that overran. *)
let check_located_eof name ~at ~expected ~got = function
  | Ok () -> Alcotest.failf "%s: accepted a buffer its own decoder rejects" name
  | Error (e : parse_error) -> (
      Alcotest.(check int) (name ^ ": offset of the failing field") at e.at;
      match e.kind with
      | Unexpected_eof { expected = ex; got = g } ->
          Alcotest.(check int) (name ^ ": bytes the field needed") expected ex;
          Alcotest.(check int) (name ^ ": bytes the buffer had there") got g
      | k -> Alcotest.failf "%s: expected an eof, got %a" name pp_error_kind k)

(* Run a validate-style call and shape its outcome for [check_located_eof]. A
   raw [Invalid_argument] is the crash under test, so it fails here rather than
   being folded into a parse error. *)
let validating name f =
  match f () with
  | () -> Ok ()
  | exception Wire.Parse_error e -> Error e
  | exception Invalid_argument m ->
      Alcotest.failf "%s: crashed instead of failing cleanly: %s" name m

(* A variable-size sub-codec can move the fixed field after it past the end of
   the buffer. A trailing greedy field then resolves the whole record's end
   back to the buffer length, so the structural bounds check alone cannot see
   the intermediate overrun. This is the composer's
   [(u64le,optdyn,i32,az)] failure reduced to its essential layout. *)
let dynamic_subcodec_before_fixed =
  let f_gate = Field.v "gate" uint8 in
  let inner =
    Codec.v "DynamicInner"
      (fun gate payload -> (gate, payload))
      Codec.
        [
          f_gate $ fst;
          Field.optional "payload"
            ~present:Expr.(Field.ref f_gate <> int 0)
            uint16be
          $ snd;
        ]
  in
  Codec.v "DynamicBeforeFixed"
    (fun inner value tail -> (inner, value, tail))
    Codec.
      [
        (Field.v "inner" (codec inner) $ fun (inner, _, _) -> inner);
        (Field.v "value" int32be $ fun (_, value, _) -> value);
        (Field.v "tail" all_zeros $ fun (_, _, tail) -> tail);
      ]

let test_decode_dynamic_field_past_end () =
  let buf = Bytes.of_string "\x01\x00\x00\x00" in
  let decoded =
    match Codec.decode dynamic_subcodec_before_fixed buf 0 with
    | Ok _ -> Ok ()
    | Error e -> Error e
    | exception Invalid_argument m ->
        Alcotest.failf "decode crashed instead of failing cleanly: %s" m
  in
  check_located_eof "decode" ~at:3 ~expected:4 ~got:1 decoded;
  check_located_eof "validate" ~at:3 ~expected:4 ~got:1
    (validating "validate" (fun () ->
         Codec.validate dynamic_subcodec_before_fixed buf 0))

(* [Codec.validate_struct] runs the same populate-driven field pass as
   [Codec.validate] and needs the same per-field bounds check. [Data] is sized
   by [Len], so a [Len] of 200 in a 5-byte buffer puts [V] at offset 201: the
   first byte read off the end. The trailing greedy [Z] keeps the struct's span
   resolvable, so nothing rejects the record before [V]'s read. *)
let validate_struct_overrun () =
  let module T = Wire.Private.Types in
  T.struct_ "Overrun"
    [
      T.field "Len" uint8;
      T.field "Data" (byte_array ~size:(T.ref "Len"));
      T.field "V" uint16be;
      T.field "Z" all_zeros;
    ]

let validate_struct_overrun_input = "\xc8\x00\x00\x00\x00"

let test_validate_struct_field_past_end () =
  let module T = Wire.Private.Types in
  let s = validate_struct_overrun () in
  let v = Codec.validator_of_struct s in
  let buf = Bytes.of_string validate_struct_overrun_input in
  (* [Data] is the field that overruns: it is declared 200 bytes at offset 1
     and the buffer holds 4 from there. The fields after it sit at offsets the
     overrun invented, so the failure belongs here rather than to [V] at 201. *)
  check_located_eof "validate_struct" ~at:1 ~expected:200 ~got:4
    (validating "validate_struct" (fun () -> Codec.validate_struct v buf 0));
  check_located_eof "of_string" ~at:1 ~expected:200 ~got:4
    (validating "of_string" (fun () ->
         match of_string (T.struct_typ s) validate_struct_overrun_input with
         | Ok () -> ()
         | Error e -> raise (Wire.Parse_error e)))

(* Unlike [Codec.validate], [Codec.validate_struct] has no whole-record bounds
   check in front of the field pass, so even a statically placed field can be
   read off the end: [B] sits at offset 1 in a 1-byte buffer. The constraint is
   what makes the validator populate [B] at all. *)
let validate_struct_short () =
  let module T = Wire.Private.Types in
  T.struct_ "Short"
    [
      T.field "A" uint8;
      T.field "B" uint16be ~constraint_:Expr.(T.ref "B" < int 1000);
    ]

let test_validate_struct_fixed_field_past_end () =
  let v = Codec.validator_of_struct (validate_struct_short ()) in
  (* [B] wants 2 bytes at 1; the 1-byte buffer has none of them. *)
  check_located_eof "validate_struct" ~at:1 ~expected:2 ~got:0
    (validating "validate_struct" (fun () ->
         Codec.validate_struct v (Bytes.of_string "\x01") 0))

(* An optional whose presence is a runtime expression has no wire size of its
   own: an absent one occupies nothing. A present one occupies exactly the
   inner's width, so its read needs the bounds check every other field's gets.
   [data] is sized by [len], so a [len] of 200 in a 5-byte buffer puts [opt] at
   offset 202, and [flag = 1] says it is there. The greedy [z] keeps the
   record's span resolvable so the read is reached. *)
let validate_optional_codec =
  let f_flag = Field.v "flag" uint8 in
  let f_len = Field.v "len" uint8 in
  Codec.v "OptOverrun"
    (fun flag len data opt z -> (flag, len, data, opt, z))
    Codec.
      [
        (f_flag $ fun (flag, _, _, _, _) -> flag);
        (f_len $ fun (_, len, _, _, _) -> len);
        ( Field.v "data" (byte_array ~size:(Field.ref f_len))
        $ fun (_, _, data, _, _) -> data );
        ( Field.optional "opt" ~present:Expr.(Field.ref f_flag = int 1) uint16be
        $ fun (_, _, _, opt, _) -> opt );
        (Field.v "z" all_zeros $ fun (_, _, _, _, z) -> z);
      ]

let test_validate_present_optional_past_end () =
  let buf = Bytes.of_string "\x01\xc8\x00\x00\x00" in
  (* The span before it is what overruns: 200 bytes declared at offset 2 with 3
     in the buffer from there. [opt] would sit at 202, an offset the overrun
     invented, so the failure belongs to the span. *)
  check_located_eof "validate" ~at:2 ~expected:200 ~got:3
    (validating "validate" (fun () ->
         Codec.validate validate_optional_codec buf 0))

(* [optional_or] gates its read on the same kind of runtime expression and has
   the same hole. Same shape and same bytes as [validate_optional_codec]. *)
let validate_optional_or_codec =
  let f_flag = Field.v "flag" uint8 in
  let f_len = Field.v "len" uint8 in
  Codec.v "OptOrOverrun"
    (fun flag len data opt z -> (flag, len, data, opt, z))
    Codec.
      [
        (f_flag $ fun (flag, _, _, _, _) -> flag);
        (f_len $ fun (_, len, _, _, _) -> len);
        ( Field.v "data" (byte_array ~size:(Field.ref f_len))
        $ fun (_, _, data, _, _) -> data );
        ( Field.optional_or "opt"
            ~present:Expr.(Field.ref f_flag = int 1)
            ~default:UInt16.zero uint16be
        $ fun (_, _, _, opt, _) -> opt );
        (Field.v "z" all_zeros $ fun (_, _, _, _, z) -> z);
      ]

let test_validate_present_optional_or_past_end () =
  let buf = Bytes.of_string "\x01\xc8\x00\x00\x00" in
  check_located_eof "validate" ~at:2 ~expected:200 ~got:3
    (validating "validate" (fun () ->
         Codec.validate validate_optional_or_codec buf 0))

(* A [uint_var] of runtime width reads its bytes directly instead of through
   the span reader every other variable-width field uses, so it needs that
   reader's span check of its own. [len] is 7, so [v] claims bytes 1..8 of a
   3-byte buffer. *)
let validate_uint_var_codec =
  let f_len = Field.v "len" uint8 in
  Codec.v "UintOverrun"
    (fun len v z -> (len, v, z))
    Codec.
      [
        (f_len $ fun (len, _, _) -> len);
        (Field.v "v" (uint_var (Field.int f_len)) $ fun (_, v, _) -> v);
        (Field.v "z" all_zeros $ fun (_, _, z) -> z);
      ]

let test_validate_uint_var_past_end () =
  let buf = Bytes.of_string "\x07\x00\x00" in
  (* The span check reports the 7 bytes the field asked for against the 2 the
     buffer still had at offset 1, the way a truncated byte span does. *)
  check_located_eof "validate" ~at:1 ~expected:7 ~got:2
    (validating "validate" (fun () ->
         Codec.validate validate_uint_var_codec buf 0))

(* A [Field.repeat] consumes its byte budget exactly, so an 11-byte budget over
   a 2-byte element is illegal however long the buffer is (here 64 bytes): the
   budget does not split into whole elements. EverParse rejects the same frame
   from the same schema, with a dedicated "list size not multiple" error, and so
   does [Codec.decode]. [Codec.validate] used to accept it: it reaches a field
   only through that field's populate, and the repeat had none. Bytes are the
   fuzz sample verbatim ([fuzz.exe -s 32582623701 -r 7500]). *)
let repeat_odd_budget =
  "\x00\x0b\x1a\x15\x11\x9d\x47\x90\x0d\xcf\xbd\xa9\x80\xf5\x21\x27\x44\xad\x4d\xa6\xc1\xde\xd7\xf6\x1a\x42\x1a\x51\x9d\x9d\x57\x5c\x2c\x8c\x75\xa1\xba\xfb\xaf\xe3\xd7\xaa\x2b\x1d\x90\xfb\xce\x47\xf9\xa4\x4e\x46\xbe\x2a\xa1\xbb\x18\xfc\x72\xe2\x8e\x47\xf9\x41"

let repeat_budget_codec =
  let f_total = Field.v "total" uint16be in
  Codec.v "RepBudget"
    (fun _ xs -> xs)
    Codec.
      [
        (f_total $ fun xs -> UInt16.v (2 * List.length xs));
        (Field.repeat "items" ~size:(Field.ref f_total) uint16be $ fun xs -> xs);
      ]

(* [repeat_seq] builds the same [Repeat] node with a different sequence builder,
   so it shares the populate and the budget check; pinned here so a future split
   of the two paths cannot fix one and leave the other. *)
let repeat_seq_budget_codec =
  let f_total = Field.v "total" uint16be in
  Codec.v "RepSeqBudget"
    (fun _ xs -> xs)
    Codec.
      [
        (f_total $ fun xs -> UInt16.v (2 * List.length xs));
        ( Field.repeat_seq "items" ~seq:seq_list ~size:(Field.ref f_total)
            uint16be
        $ fun xs -> xs );
      ]

let test_validate_repeat_partial_element () =
  let buf = Bytes.of_string repeat_odd_budget in
  let rejects name codec =
    (* The budget runs from offset 2 to 13 and leaves one byte at 12, where a
       2-byte element needs two. Both sides must report that same spot: a fix
       that made them agree on accepting would pass a bare "they agree" check. *)
    check_located_eof (name ^ " validate") ~at:12 ~expected:2 ~got:1
      (validating (name ^ " validate") (fun () -> Codec.validate codec buf 0));
    check_located_eof (name ^ " decode") ~at:12 ~expected:2 ~got:1
      (validating (name ^ " decode") (fun () ->
           match Codec.decode codec buf 0 with
           | Ok _ -> ()
           | Error e -> raise (Wire.Parse_error e)))
  in
  rejects "repeat" repeat_budget_codec;
  rejects "repeat_seq" repeat_seq_budget_codec;
  (* A budget that does split stays accepted on both sides: what is rejected is
     the partial element, not every repeat. *)
  let whole =
    Bytes.of_string ("\x00\x0a" ^ String.sub repeat_odd_budget 2 10)
  in
  let accepts name codec =
    (match Codec.validate codec whole 0 with
    | () -> ()
    | exception Wire.Parse_error e ->
        Alcotest.failf "%s: validate rejected a whole budget: %a" name
          Wire.pp_parse_error e);
    match Codec.decode codec whole 0 with
    | Ok xs ->
        Alcotest.(check int) (name ^ ": elements decoded") 5 (List.length xs)
    | Error e ->
        Alcotest.failf "%s: decode rejected a whole budget: %a" name
          Wire.pp_parse_error e
  in
  accepts "repeat" repeat_budget_codec;
  accepts "repeat_seq" repeat_seq_budget_codec

(* The same truncation read at a nonzero base. [expected] and [got] are byte
   counts, so they say the same thing wherever the frame sits; only [at] moves.
   Reporting buffer positions instead made both of them grow with the base, and
   made [got] equal [expected] on a failure that says the input ran out. *)
let test_eof_counts_are_base_invariant () =
  let pad = 8 in
  let buf = Bytes.of_string (String.make pad '\xee' ^ "\x07\x00\x00") in
  check_located_eof "validate at base 8" ~at:(pad + 1) ~expected:7 ~got:2
    (validating "validate at base 8" (fun () ->
         Codec.validate validate_uint_var_codec buf pad))

(* A byte_slice whose resolved size goes negative (here a [Sub] on an untrusted
   length field) must fail cleanly: [make_or_eod] would otherwise raise a raw
   [Invalid_argument] that escapes the decode result. A large variable field
   ahead of it keeps the framing past the top-level bounds check, so the negative
   size reaches the slice read. *)
let test_byte_slice_negative_size () =
  let f_l = Field.v "l" uint8 in
  let f_n = Field.v "n" uint8 in
  let c =
    Codec.v "SliceNeg"
      (fun l pre n sl -> (l, pre, n, sl))
      Codec.
        [
          (f_l $ fun (l, _, _, _) -> l);
          ( Field.v "pre" (byte_array ~size:(Field.ref f_l))
          $ fun (_, p, _, _) -> p );
          (f_n $ fun (_, _, n, _) -> n);
          ( Field.v "sl" (byte_slice ~size:Expr.(Field.ref f_n - int 100))
          $ fun (_, _, _, s) -> s );
        ]
  in
  let buf = Bytes.make 202 '\000' in
  Bytes.set_uint8 buf 0 200;
  Bytes.set_uint8 buf 201 1;
  match Codec.decode c buf 0 with
  | Error _ -> ()
  | Ok _ ->
      Alcotest.fail "expected a parse error for a negative byte_slice size"
  | exception Invalid_argument _ ->
      Alcotest.fail
        "negative byte_slice size crashed instead of failing cleanly"

let suite =
  ( "codec_variable",
    [
      Alcotest.test_case "repeat: zeroterm element roundtrip" `Quick
        test_repeat_zeroterm_element;
      Alcotest.test_case "repeat: zeroterm element projection" `Quick
        test_repeat_zeroterm_projection;
      Alcotest.test_case "optional: variable byte_array inner roundtrip" `Quick
        test_optional_var_byte_array;
      Alcotest.test_case "optional: self-delimiting codec inner roundtrip"
        `Quick test_optional_self_delimiting_codec;
      Alcotest.test_case "optional: length-prefixed group roundtrip" `Quick
        test_optional_length_prefixed_group;
      Alcotest.test_case "array: byte_array element roundtrip" `Quick
        test_array_byte_array_element;
      Alcotest.test_case "array: byte_array element projection" `Quick
        test_array_byte_array_projection;
      Alcotest.test_case "rest_bytes projection guard" `Quick
        test_rest_bytes_projection_guard;
      Alcotest.test_case "repeat oversized length rejected" `Quick
        test_repeat_oversized_length_rejected;
      Alcotest.test_case "byte_slice negative size fails cleanly" `Quick
        test_byte_slice_negative_size;
      Alcotest.test_case "validate: field past end fails cleanly" `Quick
        test_validate_overrun_field_offset;
      Alcotest.test_case "decode: dynamic field past end fails cleanly" `Quick
        test_decode_dynamic_field_past_end;
      Alcotest.test_case "validate_struct: field past end fails cleanly" `Quick
        test_validate_struct_field_past_end;
      Alcotest.test_case "validate_struct: fixed field past a short buffer"
        `Quick test_validate_struct_fixed_field_past_end;
      Alcotest.test_case "validate: present optional past end fails cleanly"
        `Quick test_validate_present_optional_past_end;
      Alcotest.test_case "validate: present optional_or past end fails cleanly"
        `Quick test_validate_present_optional_or_past_end;
      Alcotest.test_case
        "validate: runtime-width uint_var past end fails cleanly" `Quick
        test_validate_uint_var_past_end;
      Alcotest.test_case "validate: repeat budget with a partial element" `Quick
        test_validate_repeat_partial_element;
      Alcotest.test_case "diag: eof counts do not move with the base" `Quick
        test_eof_counts_are_base_invariant;
    ] )
