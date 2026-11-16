(* Tests for codec.ml: descriptions refused for lack of a 3D projection. *)

open Wire
open Test_helpers

(* A bitfield has no standalone element form, so [array] / [Field.repeat] over
   one is rejected at construction rather than crashing at decode. *)
let test_repeat_array_reject_bitfield () =
  Alcotest.(check bool)
    "array over bits rejected" true
    (raises_invalid (fun () -> array ~len:(int 4) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "repeat over bits rejected" true
    (raises_invalid (fun () ->
         Field.repeat "x" ~size:(int 4) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "repeat over bit (bool over bits) rejected" true
    (raises_invalid (fun () ->
         Field.repeat "x" ~size:(int 4) (bit (bits ~width:1 U8))))

(* A zero-width element ([empty] / unit) carries no bytes, so an array of it is
   degenerate and projects to a zero-size 3D array EverParse rejects. It is
   refused at construction. *)
let test_reject_zero_width_element () =
  Alcotest.(check bool)
    "array over empty rejected" true
    (raises_invalid (fun () -> array ~len:(int 3) empty));
  Alcotest.(check bool)
    "array_seq over empty rejected" true
    (raises_invalid (fun () -> array_seq seq_list ~len:(int 3) empty));
  (* A byte-budget list of a 0-width element does not extract either, so
     [Field.repeat] over [empty] is rejected like [array], not silently built. *)
  Alcotest.(check bool)
    "repeat over empty rejected" true
    (raises_invalid (fun () -> Field.repeat "r" ~size:(int 3) empty))

(* A byte span of literal size zero is 0-width too: EverParse refuses to extract
   a zero-size element, so an [array] over one has no projection even though the
   fixed count keeps the decode loop terminating. Refused at construction like
   [empty], matching the [Field.repeat] guard. *)
let array_rejects elem =
  match array ~len:(int 2) elem with
  | _ -> false
  | exception Invalid_argument _ -> true

let test_array_rejects_zero_width_span () =
  Alcotest.(check bool)
    "byte_array of size 0" true
    (array_rejects (byte_array ~size:(int 0)));
  Alcotest.(check bool)
    "byte_slice of size 0" true
    (array_rejects (byte_slice ~size:(int 0)));
  Alcotest.(check bool)
    "byte_array of size 0 under a map" true
    (array_rejects
       (map ~decode:String.length
          ~encode:(fun _ -> "")
          (byte_array ~size:(int 0))));
  Alcotest.(check bool)
    "byte_array of a folded zero size" true
    (array_rejects (byte_array ~size:Expr.(int 1 - int 1)));
  Alcotest.(check bool)
    "array_seq over byte_array of size 0" true
    (match array_seq seq_list ~len:(int 2) (byte_array ~size:(int 0)) with
    | _ -> false
    | exception Invalid_argument _ -> true);
  (* A varint sized by [sizeof] stays symbolic (constant folding leaves [Sizeof]
     alone), so it never reaches the literal-size case and [array] already
     refuses it. *)
  Alcotest.(check bool)
    "uint_var sized by sizeof empty" true
    (array_rejects (uint_var (sizeof empty)))

(* The guard must still admit a legitimate fixed-size byte element: a positive
   literal span projects to a count of fixed-size elements. *)
let test_array_accepts_fixed_byte_span () =
  Alcotest.(check bool)
    "byte_array of size 4 allowed" false
    (array_rejects (byte_array ~size:(int 4)));
  Alcotest.(check bool)
    "byte_slice of size 1 allowed" false
    (array_rejects (byte_slice ~size:(int 1)))

(* Wire.array over a fixed sub-record (Codec element): a fixed-count list of
   structs. Decoding raised Failure "build_field_reader: unsupported type"
   because the array element reader had no Codec case. The schema projects the
   element as the sub-struct under a [:byte-size] budget. *)
type pt = { px : UInt8.t; py : UInt16.t }

let pt_codec =
  Codec.v "Pt"
    (fun px py -> { px; py })
    Codec.
      [
        (Field.v "px" uint8 $ fun r -> r.px);
        (Field.v "py" uint16be $ fun r -> r.py);
      ]

type arr_recs = { rtag : UInt8.t; pts : pt list }

let arr_recs_codec =
  Codec.v "ArrRecs"
    (fun rtag pts -> { rtag; pts })
    Codec.
      [
        (Field.v "rtag" uint8 $ fun r -> r.rtag);
        (Field.v "pts" (array ~len:(int 2) (codec pt_codec)) $ fun r -> r.pts);
      ]

let test_array_record_element () =
  let v =
    {
      rtag = UInt8.v 9;
      pts =
        [
          { px = UInt8.v 1; py = UInt16.v 0x0203 };
          { px = UInt8.v 4; py = UInt16.v 0x0506 };
        ];
    }
  in
  let sz = Codec.size_of_value arr_recs_codec v in
  Alcotest.(check int) "wire size" 7 sz;
  let buf = Bytes.create sz in
  Codec.encode arr_recs_codec v buf 0;
  match Codec.decode arr_recs_codec buf 0 with
  | Ok d ->
      Alcotest.(check (list (pair int int)))
        "pts"
        (List.map (fun p -> (UInt8.to_int p.px, UInt16.to_int p.py)) v.pts)
        (List.map (fun p -> (UInt8.to_int p.px, UInt16.to_int p.py)) d.pts)
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

let test_array_record_projection () =
  let out = render_3d arr_recs_codec in
  Alcotest.(check bool)
    "sub-struct element under byte-size on the array field" true
    (contains ~sub:"Pt pts[:byte-size (2 * 3)]" out)

(* [nested] / [nested_at_most] over a bitfield is likewise rejected at
   construction with a clear error, not at decode. *)
let test_nested_reject_bitfield () =
  Alcotest.(check bool)
    "nested over bits rejected" true
    (raises_invalid (fun () -> nested ~size:(int 1) (bits ~width:3 U8)));
  Alcotest.(check bool)
    "nested_at_most over bits rejected" true
    (raises_invalid (fun () -> nested_at_most ~size:(int 1) (bits ~width:3 U8)))

(* A [nested] / [nested_at_most] region over a composite inner (an array, or
   another nested region) round-trips: the inner decodes at the region start
   and the region is zero-padded to its fixed size. 3D projects it through a
   synthesised wrapper struct (see test_everparse). *)
let test_nested_over_array () =
  let codec =
    Codec.v "NestArr" Fun.id
      Codec.
        [
          Field.v "xs" (nested ~size:(int 16) (array ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let v = List.map UInt64.of_int64 [ 1L; 2L ] in
  let buf = Bytes.create (Codec.size_of_value codec v) in
  Codec.encode codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (List.equal UInt64.equal (decode_ok (Codec.decode codec buf 0)) v)

let test_nested_at_most_over_array () =
  let codec =
    Codec.v "NestAtmArr" Fun.id
      Codec.
        [
          Field.v "xs"
            (nested_at_most ~size:(int 16)
               (array_seq seq_list ~len:(int 2) uint64be))
          $ Fun.id;
        ]
  in
  let v = List.map UInt64.of_int64 [ 7L; 9L ] in
  let buf = Bytes.create (Codec.size_of_value codec v) in
  Codec.encode codec v buf 0;
  Alcotest.(check bool)
    "roundtrip" true
    (List.equal UInt64.equal (decode_ok (Codec.decode codec buf 0)) v)

let test_nested_exact_region () =
  let make name typ =
    Codec.v name Fun.id Codec.[ Field.v "value" typ $ Fun.id ]
  in
  let exact = make "NestedExact" (nested ~size:(int 4) zeroterm) in
  let at_most = make "NestedAtMost" (nested_at_most ~size:(int 4) zeroterm) in
  let padded = Bytes.of_string "A\x00\x00\x00" in
  (match Codec.decode exact padded 0 with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "compiled exact nested accepted trailing padding");
  Alcotest.(check string)
    "compiled at-most accepts padding" "A"
    (decode_ok (Codec.decode at_most padded 0));
  Alcotest.(check string)
    "compiled exact accepts full consumption" "ABC"
    (decode_ok (Codec.decode exact (Bytes.of_string "ABC\x00") 0));
  (match Codec.size_of_value exact "A" with
  | _ -> Alcotest.fail "compiled exact nested encode accepted padding"
  | exception Invalid_argument msg ->
      Alcotest.(check bool) "nested error" true (contains ~sub:"nested" msg));
  let buf = Bytes.create 4 in
  Codec.encode at_most "A" buf 0;
  Alcotest.(check bytes) "compiled at-most pads" padded buf

(* A casetype whose case body is a [nested] region (a scalar in a fixed span):
   the tag-dispatched case decodes and sizes through the region. *)
type nest_case = N of SInt32.t | U of UInt8.t

let nest_case_codec =
  let ct =
    casetype "NcT" uint8
      [
        case ~index:(UInt8.v 1)
          (nested ~size:(int 4) int32be)
          ~inject:(fun v -> N v)
          ~project:(function N v -> Some v | _ -> None);
        case ~index:(UInt8.v 2) uint8
          ~inject:(fun v -> U v)
          ~project:(function U v -> Some v | _ -> None);
      ]
  in
  Codec.v "Nc" Fun.id Codec.[ Field.v "b" ct $ Fun.id ]

let test_casetype_nested_case_body () =
  List.iter
    (fun v ->
      let buf = Bytes.create (Codec.size_of_value nest_case_codec v) in
      Codec.encode nest_case_codec v buf 0;
      Alcotest.(check bool)
        "roundtrip" true
        (decode_ok (Codec.decode nest_case_codec buf 0) = v))
    [ N (SInt32.of_int 12345); U (UInt8.v 7) ]

(* Field.repeat over a fixed byte_array element: a list of n-byte chunks within
   a byte budget. Decodes the list and projects to a single [:byte-size]
   schema. *)
type rep_chunks = { rn : UInt16.t; chunks : string list }

let rep_chunks_codec =
  let f_n = Field.v "n" uint16be in
  let f_chunks =
    Field.repeat "chunks" ~size:(Field.ref f_n) (byte_array ~size:(int 4))
  in
  Codec.v "RepChunks"
    (fun rn chunks -> { rn; chunks })
    Codec.[ (f_n $ fun r -> r.rn); (f_chunks $ fun r -> r.chunks) ]

let test_repeat_byte_array_element () =
  let v = { rn = UInt16.v 8; chunks = [ "aaaa"; "bbbb" ] } in
  let sz = Codec.size_of_value rep_chunks_codec v in
  Alcotest.(check int) "wire size" 10 sz;
  let buf = Bytes.create sz in
  Codec.encode rep_chunks_codec v buf 0;
  match Codec.decode rep_chunks_codec buf 0 with
  | Ok d ->
      Alcotest.(check int) "count" 8 (UInt16.to_int d.rn);
      Alcotest.(check (list string)) "chunks" [ "aaaa"; "bbbb" ] d.chunks
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

(* The schema projects the byte-span element as bare [UINT8] under the budget,
   not a double [:byte-size]. *)
let test_repeat_byte_array_projection () =
  let out = render_3d rep_chunks_codec in
  Alcotest.(check bool)
    "single byte-size on the chunks field" true
    (contains ~sub:"UINT8 chunks[:byte-size n]" out)

(* An element with no clean per-element 3D projection is refused at
   construction, rather than failing late with Failure at decode/encode or
   emitting a schema EverParse cannot verify. *)
let repeat_rejects elem =
  match Field.repeat "items" ~size:(int 4) elem with
  | _ -> false
  | exception Invalid_argument _ -> true

let test_repeat_rejects_unprojectable () =
  Alcotest.(check bool) "bits element" true (repeat_rejects (bits ~width:3 U8));
  Alcotest.(check bool) "all_zeros element" true (repeat_rejects all_zeros);
  Alcotest.(check bool)
    "zeroterm_at_most element" true
    (repeat_rejects (zeroterm_at_most ~size:(int 6)));
  Alcotest.(check bool)
    "byte_array_where element" true
    (repeat_rejects
       (byte_array_where ~size:(int 2) ~per_byte:(fun _ -> Expr.true_)))

(* A byte span of literal size zero carries no bytes, so a byte-budget list of
   it has nothing for EverParse to extract and the decoder's loop would never
   reach the end of its region. Refused at construction like [empty]. *)
let test_repeat_rejects_zero_width_span () =
  Alcotest.(check bool)
    "byte_array of size 0" true
    (repeat_rejects (byte_array ~size:(int 0)));
  Alcotest.(check bool)
    "byte_slice of size 0" true
    (repeat_rejects (byte_slice ~size:(int 0)));
  Alcotest.(check bool)
    "byte_array of size 0 under a map" true
    (repeat_rejects
       (map ~decode:String.length
          ~encode:(fun _ -> "")
          (byte_array ~size:(int 0))));
  Alcotest.(check bool)
    "repeat_seq over byte_array of size 0" true
    (match
       Field.repeat_seq "items" ~seq:seq_list ~size:(int 4)
         (byte_array ~size:(int 0))
     with
    | _ -> false
    | exception Invalid_argument _ -> true);
  Alcotest.(check bool)
    "byte_array of size 1 allowed" false
    (repeat_rejects (byte_array ~size:(int 1)))

(* A sub-codec whose last field is greedy ([all_bytes] / [all_zeros]) reads the
   rest of the buffer as its tail, so it cannot be a repeat element (the first
   element would consume everything). It is fine standalone. *)
let test_repeat_rejects_greedy_tail_codec () =
  let greedy =
    Codec.v "GreedyTail"
      (fun a b -> (a, b))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "rest" all_bytes $ snd ]
  in
  Alcotest.(check bool)
    "repeat over codec ending in all_bytes rejected" true
    (repeat_rejects (codec greedy));
  let plain =
    Codec.v "Plain" (fun a -> a) Codec.[ Field.v "n" uint8 $ Fun.id ]
  in
  Alcotest.(check bool)
    "repeat over a non-greedy codec allowed" false
    (repeat_rejects (codec plain))

(* [optional] / [optional_or] project either a conditional byte-size region (a
   sized inner) or a gate-dispatched casetype (a self-delimiting inner). An
   inner that is neither, such as [all_bytes], has no projection and is refused
   at construction. *)
let test_optional_reject_unprojectable () =
  Alcotest.(check bool)
    "optional over all_bytes rejected" true
    (raises_invalid (fun () -> Field.optional "x" ~present:Expr.true_ all_bytes));
  Alcotest.(check bool)
    "optional_or over all_bytes rejected" true
    (raises_invalid (fun () ->
         Field.optional_or "x" ~present:Expr.true_ ~default:"" all_bytes))

(* An [array] / [array_seq] element must be a fixed-width type the array loop
   can read one element at a time. A [nested] region, a refined byte span, and
   a nested array all carry a wire size but have no array projection and no
   element reader, so they are refused at construction (unlike [repeat], whose
   byte budget admits self-delimiting variable elements). *)
let test_array_reject_nonprojectable_element () =
  Alcotest.(check bool)
    "array over nested rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (nested ~size:(int 4) uint8)));
  Alcotest.(check bool)
    "array_seq over nested_at_most rejected" true
    (raises_invalid (fun () ->
         array_seq seq_list ~len:(int 2) (nested_at_most ~size:(int 4) uint8)));
  Alcotest.(check bool)
    "array over byte_array_where rejected" true
    (raises_invalid (fun () ->
         array ~len:(int 2)
           (byte_array_where ~size:(int 2) ~per_byte:(fun _ -> Expr.true_))));
  Alcotest.(check bool)
    "array over array rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (array ~len:(int 2) uint8)))

(* EverParse projects [array] / [repeat] of a sub-codec as a byte-budget list of
   the codec's named struct, and its [T_nlist] requires the element parser to
   consume a positive minimum of bytes. A sub-codec made only of byte-span
   fields has a possibly-empty parser, so the list does not extract -- such an
   element is refused at construction. One fixed-size field is enough to anchor
   it. *)
let lone_byte_codec =
  Codec.v "LoneBytes" Fun.id
    Codec.[ Field.v "blob" (byte_array ~size:(int 4)) $ Fun.id ]

let scalar_bearing_codec =
  Codec.v "Anchored"
    (fun a b -> (a, b))
    Codec.
      [
        Field.v "tag" uint8 $ fst;
        Field.v "blob" (byte_array ~size:(int 3)) $ snd;
      ]

let test_array_repeat_reject_non_nz_codec () =
  Alcotest.(check bool)
    "array over byte-span-only codec rejected" true
    (raises_invalid (fun () -> array ~len:(int 2) (codec lone_byte_codec)));
  Alcotest.(check bool)
    "repeat over byte-span-only codec rejected" true
    (repeat_rejects (codec lone_byte_codec));
  Alcotest.(check bool)
    "array over a codec with a fixed-size field allowed" false
    (raises_invalid (fun () -> array ~len:(int 2) (codec scalar_bearing_codec)));
  Alcotest.(check bool)
    "repeat over a codec with a fixed-size field allowed" false
    (repeat_rejects (codec scalar_bearing_codec))

let projects c =
  match render_3d c with _ -> true | exception Invalid_argument _ -> false

let arr_field elem = Codec.v "Arr" Fun.id Codec.[ Field.v "x" elem $ Fun.id ]

(* An [array] over a float or signed integer element builds, round-trips, and
   projects to a verified 3D schema: [inner_wire_size] sizes every fixed-width
   scalar element (unsigned and signed ints, floats, and bitfields), so the
   projector can size it. *)
let test_array_scalar_element_projects () =
  let yes name c =
    Alcotest.(check bool) (name ^ " array projects") true (projects c)
  in
  yes "float64be" (arr_field (array ~len:(int 2) float64be));
  yes "float32be" (arr_field (array ~len:(int 2) float32be));
  yes "int8" (arr_field (array ~len:(int 2) int8));
  yes "int16be" (arr_field (array ~len:(int 2) int16be));
  yes "int32be" (arr_field (array ~len:(int 2) int32be));
  yes "int64be" (arr_field (array ~len:(int 2) int64be));
  yes "uint64be" (arr_field (array ~len:(int 2) uint64be));
  (* and it still round-trips *)
  let c = arr_field (array ~len:(int 2) float64be) in
  let v = [ 1.5; -2.25 ] in
  let buf = Bytes.create (Codec.size_of_value c v) in
  Codec.encode c v buf 0;
  match Codec.decode c buf 0 with
  | Ok d -> Alcotest.(check (list (float 0.0))) "float array" v d
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e

(* An [array] over a [where] / [map] wrapping a byte span projects to a verified
   3D schema: [is_array_element] looks through the transparent wrapper to accept
   it, and [inner_wire_size_expr] recurses through the wrapper to find the span's
   size. *)
let test_array_wrapped_byte_span_projects () =
  Alcotest.(check bool)
    "array over where(byte_slice) projects" true
    (projects
       (arr_field
          (array ~len:(int 2) (where Expr.true_ (byte_slice ~size:(int 4))))));
  Alcotest.(check bool)
    "array over map(byte_array) projects" true
    (projects
       (arr_field
          (array ~len:(int 2)
             (map ~decode:Fun.id ~encode:Fun.id (byte_array ~size:(int 4))))))

(* A bitfield has no standalone wire form, so it cannot be an [optional] inner
   any more than an [array] / [repeat] / [nested] element. *)
let test_optional_reject_bitfield () =
  Alcotest.(check bool)
    "optional over bits rejected" true
    (raises_invalid (fun () ->
         Field.optional "x" ~present:Expr.true_ (bits ~width:3 U8)));
  Alcotest.(check bool)
    "optional_or over bits rejected" true
    (raises_invalid (fun () ->
         Field.optional_or "x" ~present:Expr.true_ ~default:0 (bits ~width:3 U8)))

(* A bare greedy field ([all_bytes] / [all_zeros]) reads the rest of the buffer;
   it has no determinate type, so it cannot be a casetype case body (a sub-codec
   that ends in [all_bytes] is the supported form). *)
let test_casetype_reject_greedy_case_body () =
  let build inner =
    casetype "Greedy" uint8
      [
        case ~index:(UInt8.v 1) inner ~inject:(fun s -> s) ~project:Option.some;
      ]
  in
  Alcotest.(check bool)
    "casetype with all_bytes case body rejected" true
    (raises_invalid (fun () -> build all_bytes));
  Alcotest.(check bool)
    "casetype with all_zeros case body rejected" true
    (raises_invalid (fun () -> build all_zeros))

(* A greedy field ([all_bytes] / [all_zeros]) reads the rest of the buffer, so
   it is only valid as the last field; an earlier one is refused at
   construction (it would starve every field after it at decode). *)
let test_greedy_not_last_rejected () =
  Alcotest.(check bool)
    "all_zeros before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "GnlA"
           (fun a b -> (a, b))
           Codec.[ Field.v "z" all_zeros $ fst; Field.v "n" uint8 $ snd ]));
  Alcotest.(check bool)
    "all_bytes before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "GnlB"
           (fun a b -> (a, b))
           Codec.[ Field.v "b" all_bytes $ fst; Field.v "n" uint8 $ snd ]));
  (* A greedy last field is fine and round-trips. *)
  let c =
    Codec.v "GnlOk"
      (fun a b -> (a, b))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "rest" all_bytes $ snd ]
  in
  let v = (UInt8.v 5, "tail") in
  let buf = Bytes.create (Codec.size_of_value c v) in
  Codec.encode c v buf 0;
  Alcotest.(check bool)
    "greedy last roundtrip" true
    (let n, rest = decode_ok (Codec.decode c buf 0) in
     UInt8.equal n (fst v) && String.equal rest (snd v))

(* A casetype whose case body is a sub-codec ending in a greedy field consumes
   the rest of the buffer when that case is selected; placed before another
   field it would starve it, so it is refused at construction. As the last field
   it is fine. *)
let test_casetype_greedy_case_not_last_rejected () =
  let greedy =
    Codec.v "G" (fun z -> z) Codec.[ Field.v "z" all_zeros $ Fun.id ]
  in
  let ct =
    casetype "CtG" uint8
      [
        case ~index:(UInt8.v 1) uint8
          ~inject:(fun v -> `U v)
          ~project:(function `U v -> Some v | _ -> None);
        case ~index:(UInt8.v 2) (codec greedy)
          ~inject:(fun v -> `G v)
          ~project:(function `G v -> Some v | _ -> None);
      ]
  in
  Alcotest.(check bool)
    "casetype with greedy case body before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "Outer"
           (fun c t -> (c, t))
           Codec.[ Field.v "ct" ct $ fst; Field.v "tail" uint8 $ snd ]));
  Alcotest.(check bool)
    "casetype with greedy case body as last field accepted" false
    (raises_invalid (fun () ->
         Codec.v "OuterOk" Fun.id Codec.[ Field.v "ct" ct $ Fun.id ]))

(* The greedy case body may itself carry a leading field before the greedy tail
   (here [n] then [all_zeros]); the casetype still consumes the rest whenever the
   greedy case is selected, so it can be the last field but is rejected before
   another field. A richer variant of the case above. *)
let test_casetype_wrapped_greedy_not_last_rejected () =
  let body =
    Codec.v "GreedyCaseBody"
      (fun n z -> (n, z))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "z" all_zeros $ snd ]
  in
  let payload =
    casetype "GreedyCase" uint8
      [
        case ~index:(UInt8.v 1) (codec body)
          ~inject:(fun v -> `Greedy v)
          ~project:(function `Greedy v -> Some v | _ -> None);
        case ~index:(UInt8.v 2) uint8
          ~inject:(fun v -> `Byte v)
          ~project:(function `Byte v -> Some v | _ -> None);
      ]
  in
  Alcotest.(check bool)
    "casetype with wrapped greedy case can be last" false
    (raises_invalid (fun () ->
         Codec.v "WrappedGreedyLast"
           (fun n p -> (n, p))
           Codec.[ Field.v "n" uint8 $ fst; Field.v "p" payload $ snd ]));
  Alcotest.(check bool)
    "casetype with wrapped greedy case before another field rejected" true
    (raises_invalid (fun () ->
         Codec.v "WrappedGreedyNotLast"
           (fun p tail -> (p, tail))
           Codec.[ Field.v "p" payload $ fst; Field.v "tail" uint8 $ snd ]))

let test_optional_greedy_not_last_rejected () =
  let greedy =
    Codec.v "GreedyOptionalBody"
      (fun n z -> (n, z))
      Codec.[ Field.v "n" uint8 $ fst; Field.v "z" all_zeros $ snd ]
  in
  let check_rejected label field =
    Alcotest.(check bool)
      label true
      (raises_invalid (fun () ->
           Codec.v "OptionalGreedyNotLast"
             (fun body tail -> (body, tail))
             Codec.[ field $ fst; Field.v "tail" uint8 $ snd ]))
  in
  check_rejected "present optional greedy body before tail rejected"
    (Field.optional "body" ~present:Expr.true_ (codec greedy));
  check_rejected "dynamic optional greedy body before tail rejected"
    (Field.optional "body" ~present:Expr.(int 1 = int 1) (codec greedy));
  check_rejected "optional_or greedy body before tail rejected"
    (Field.optional_or "body" ~present:Expr.true_ ~default:(UInt8.zero, "")
       (codec greedy));
  Alcotest.(check bool)
    "statically absent optional greedy body before tail accepted" false
    (raises_invalid (fun () ->
         Codec.v "AbsentOptionalGreedy"
           (fun body tail -> (body, tail))
           Codec.
             [
               Field.optional "body" ~present:Expr.false_ (codec greedy) $ fst;
               Field.v "tail" uint8 $ snd;
             ]))

(* [uint] is a 1-to-7-byte unsigned integer; a literal size outside that
   range is refused at construction. *)
let test_uint_size_bounds () =
  Alcotest.(check bool)
    "uint 0 rejected" true
    (raises_invalid (fun () -> uint 0));
  Alcotest.(check bool)
    "uint 8 rejected" true
    (raises_invalid (fun () -> uint 8));
  Alcotest.(check bool)
    "uint 4 accepted" false
    (raises_invalid (fun () -> uint 4))

(* A bitfield wider than its base word, or narrower than one bit, has no faithful
   wire meaning (the OCaml shift and the 3D field would read different values), so
   it is refused at construction. *)
let test_bits_width_bounds () =
  Alcotest.(check bool)
    "9-bit field over U8 rejected" true
    (raises_invalid (fun () -> bits ~width:9 U8));
  Alcotest.(check bool)
    "0-bit field rejected" true
    (raises_invalid (fun () -> bits ~width:0 U8));
  Alcotest.(check bool)
    "17-bit field over U16 rejected" true
    (raises_invalid (fun () -> bits ~width:17 U16));
  Alcotest.(check bool)
    "8-bit field over U8 accepted" false
    (raises_invalid (fun () -> bits ~width:8 U8))

(* The value of a bitfield is a native [int], so a field must leave the sign
   bit free: every width of a U32 word on a 64-bit host, at most 31 bits under
   js_of_ocaml (32-bit [int]) and 30 under wasm_of_ocaml (31-bit). A wider one
   is refused when built, naming the limit, not read wrong later. *)
let test_bits_width_platform () =
  List.iter
    (fun width ->
      let fits = width <= Sys.int_size - 1 in
      match bits ~width U32be with
      | _ when fits -> ()
      | _ ->
          Alcotest.failf "%d-bit field built on a %d-bit int" width Sys.int_size
      | exception Invalid_argument msg ->
          if fits then Alcotest.failf "%d-bit field refused: %s" width msg;
          let sub = Fmt.str "this platform's %d-bit int" Sys.int_size in
          Alcotest.(check bool)
            (Fmt.str "%d-bit refusal names the limit" width)
            true (contains ~sub msg))
    [ 29; 30; 31; 32 ]

(* A [casetype] [case] carries an explicit discriminator; omitting [~index] is
   refused at construction (only [default] is index-free). *)
let test_casetype_case_requires_index () =
  Alcotest.(check bool)
    "case without ~index rejected" true
    (raises_invalid (fun () ->
         casetype "NoIndex" uint8
           [ case uint8 ~inject:Fun.id ~project:Option.some ]))

(* A casetype tag must project to a 3D type the [switch] dispatches on. A
   [uint size] tag renders as a non-3D [UINTBE(n)], and an enum over a
   big-endian base has no 3D enum type for its case labels, so both are refused
   at construction; a little-endian / 1-byte enum tag is fine. *)
let test_casetype_reject_unprojectable_tag () =
  (* The index is the tag's own carrier, so it comes from the caller rather
     than being fixed here. *)
  let one_case ~index tag =
    casetype "CtTag" tag
      [ case ~index uint8 ~inject:(fun s -> s) ~project:Option.some ]
  in
  Alcotest.(check bool)
    "uint tag rejected" true
    (raises_invalid (fun () ->
         casetype "CtTag" (uint 2)
           [
             case ~index:(Optint.Int63.of_int 1) uint8
               ~inject:(fun s -> s)
               ~project:Option.some;
           ]));
  Alcotest.(check bool)
    "big-endian enum tag rejected" true
    (raises_invalid (fun () ->
         one_case ~index:(UInt16.v 1)
           (enum "TagBe" [ ("A", 0); ("B", 1) ] uint16be)));
  Alcotest.(check bool)
    "little-endian enum tag accepted" false
    (raises_invalid (fun () ->
         one_case ~index:(UInt8.v 1) (enum "TagLe" [ ("A", 0); ("B", 1) ] uint8)))

let suite =
  ( "codec_projection",
    [
      Alcotest.test_case "repeat: byte_array element roundtrip" `Quick
        test_repeat_byte_array_element;
      Alcotest.test_case "repeat: byte_array element projection" `Quick
        test_repeat_byte_array_projection;
      Alcotest.test_case "repeat: rejects unprojectable element" `Quick
        test_repeat_rejects_unprojectable;
      Alcotest.test_case "repeat: rejects zero-width byte span" `Quick
        test_repeat_rejects_zero_width_span;
      Alcotest.test_case "repeat: rejects greedy-tail sub-codec" `Quick
        test_repeat_rejects_greedy_tail_codec;
      Alcotest.test_case "repeat/array reject bitfield element" `Quick
        test_repeat_array_reject_bitfield;
      Alcotest.test_case "array/repeat reject zero-width element" `Quick
        test_reject_zero_width_element;
      Alcotest.test_case "array: rejects zero-width byte span" `Quick
        test_array_rejects_zero_width_span;
      Alcotest.test_case "array: accepts fixed-size byte span" `Quick
        test_array_accepts_fixed_byte_span;
      Alcotest.test_case "array: record element roundtrip" `Quick
        test_array_record_element;
      Alcotest.test_case "array: record element projection" `Quick
        test_array_record_projection;
      Alcotest.test_case "nested reject bitfield element" `Quick
        test_nested_reject_bitfield;
      Alcotest.test_case "nested over array roundtrip" `Quick
        test_nested_over_array;
      Alcotest.test_case "nested_at_most over array roundtrip" `Quick
        test_nested_at_most_over_array;
      Alcotest.test_case "nested exact region" `Quick test_nested_exact_region;
      Alcotest.test_case "casetype nested case body roundtrip" `Quick
        test_casetype_nested_case_body;
      Alcotest.test_case "array rejects non-projectable element" `Quick
        test_array_reject_nonprojectable_element;
      Alcotest.test_case "array/repeat reject byte-span-only sub-codec" `Quick
        test_array_repeat_reject_non_nz_codec;
      Alcotest.test_case "array over scalar element projects" `Quick
        test_array_scalar_element_projects;
      Alcotest.test_case "array over wrapped byte span projects" `Quick
        test_array_wrapped_byte_span_projects;
      Alcotest.test_case "optional rejects unprojectable inner" `Quick
        test_optional_reject_unprojectable;
      Alcotest.test_case "optional rejects bitfield inner" `Quick
        test_optional_reject_bitfield;
      Alcotest.test_case "casetype rejects greedy case body" `Quick
        test_casetype_reject_greedy_case_body;
      Alcotest.test_case "greedy field must be last" `Quick
        test_greedy_not_last_rejected;
      Alcotest.test_case "casetype greedy case body must be last" `Quick
        test_casetype_greedy_case_not_last_rejected;
      Alcotest.test_case "casetype wrapped greedy case body must be last" `Quick
        test_casetype_wrapped_greedy_not_last_rejected;
      Alcotest.test_case "optional greedy body must be last" `Quick
        test_optional_greedy_not_last_rejected;
      Alcotest.test_case "uint rejects out-of-range size" `Quick
        test_uint_size_bounds;
      Alcotest.test_case "bits rejects out-of-range width" `Quick
        test_bits_width_bounds;
      Alcotest.test_case "bits width fits the native int" `Quick
        test_bits_width_platform;
      Alcotest.test_case "casetype case requires ~index" `Quick
        test_casetype_case_requires_index;
      Alcotest.test_case "casetype rejects unprojectable tag" `Quick
        test_casetype_reject_unprojectable_tag;
    ] )
