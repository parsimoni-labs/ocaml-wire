(* Tests for Shape, the construction-time gate on a codec's field list.

   Every check here stands for a layout that has no verified validator, so the
   contract is that it fails when the schema is written, with a message naming
   the codec and the offending field: a decode that silently dropped the
   constraint, or an EverParse run that failed on generated code, would both
   land far from the line at fault.

   The module reads the field list and nothing else, so each case is a literal
   list of fields with no codec sealed around it. The accepting cases matter as
   much as the rejections: a check that fires on a valid layout costs a user a
   shape 3D would have taken. *)

open Wire.Private

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

let accepts label fields =
  match Shape.reject_invalid_codec_shape label fields with
  | () -> ()
  | exception Invalid_argument msg ->
      Alcotest.failf "%s: rejected a shape 3D accepts: %s" label msg

let rejects label ~codec ~mentions fields =
  match Shape.reject_invalid_codec_shape codec fields with
  | () -> Alcotest.failf "%s: accepted a shape with no verified validator" label
  | exception Invalid_argument msg ->
      Alcotest.(check bool)
        (label ^ ": names the codec")
        true (contains ~sub:codec msg);
      List.iter
        (fun name ->
          Alcotest.(check bool)
            (Fmt.str "%s: names %s" label name)
            true (contains ~sub:name msg))
        mentions

let int n : int Types.expr = Types.Int n
let bool b : bool Types.expr = Types.Bool b
let guard = Types.Lt (Types.ref "n", int 4)

let int_tagged =
  Types.casetype "Tag" Types.uint8
    [
      Types.case ~index:(Wire.UInt8.v 1) Types.uint16be ~inject:Fun.id
        ~project:(fun v -> Some v);
      Types.case ~index:(Wire.UInt8.v 2) Types.uint16be ~inject:Fun.id
        ~project:(fun v -> Some v);
    ]

let sub =
  Wire.Codec.v "ShapeSub"
    (fun a b -> (a, b))
    Wire.Codec.
      [ Wire.Field.v "sa" Wire.uint8 $ fst; Wire.Field.v "sb" Wire.uint8 $ snd ]

(* A greedy field consumes the rest of the buffer, so anything after it is
   starved; 3D's [:consume-all] has the same rule. *)
let test_greedy_not_last () =
  rejects "all_bytes before another field" ~codec:"Head" ~mentions:[ "body" ]
    [ Types.field "body" Types.all_bytes; Types.field "crc" Types.uint16be ];
  rejects "all_zeros before another field" ~codec:"Padded" ~mentions:[ "pad" ]
    [ Types.field "pad" Types.all_zeros; Types.field "crc" Types.uint16be ];
  (* The greedy field need not be the first one to starve a successor. *)
  rejects "greedy in the middle" ~codec:"Middle" ~mentions:[ "body" ]
    [
      Types.field "n" Types.uint8;
      Types.field "body" Types.all_bytes;
      Types.field "crc" Types.uint16be;
    ];
  (* An anonymous field has no name to report, so the message says so rather
     than naming the wrong field. *)
  rejects "anonymous greedy" ~codec:"Anon" ~mentions:[ "<anon>" ]
    [ Types.anon_field Types.all_zeros; Types.field "crc" Types.uint16be ]

let test_greedy_last_accepted () =
  accepts "Trailer"
    [ Types.field "n" Types.uint8; Types.field "body" Types.all_bytes ];
  accepts "OnlyGreedy" [ Types.field "body" Types.all_zeros ];
  accepts "NoGreedy"
    [ Types.field "n" Types.uint8; Types.field "crc" Types.uint16be ]

(* A [where] projects to 3D only as a top-level field refinement. Inside a
   container the projection emits 3D that 3d.exe rejects, so the codec would
   ship a [.3d] that does not compile while OCaml decode dropped the
   constraint. *)
let test_nested_where_rejected () =
  rejects "array element" ~codec:"Arr" ~mentions:[ "vals" ]
    [
      Types.field "n" Types.uint8;
      Types.field "vals"
        (Types.array ~len:(Types.ref "n") (Types.where guard Types.uint8));
    ];
  rejects "repeat element" ~codec:"Rep" ~mentions:[ "vals" ]
    [
      Types.field "n" Types.uint8;
      Types.field "vals"
        (Types.repeat ~size:(Types.ref "n") (Types.where guard Types.uint8));
    ];
  rejects "sized region" ~codec:"Region" ~mentions:[ "inner" ]
    [
      Types.field "n" Types.uint8;
      Types.field "inner"
        (Types.nested ~size:(Types.ref "n") (Types.where guard Types.uint8));
    ];
  rejects "optional inner" ~codec:"Opt" ~mentions:[ "maybe" ]
    [
      Types.field "n" Types.uint8;
      Types.field "maybe" (Types.optional guard (Types.where guard Types.uint8));
    ];
  rejects "optional_or inner" ~codec:"OptOr" ~mentions:[ "maybe" ]
    [
      Types.field "n" Types.uint8;
      Types.field "maybe"
        (Types.optional_or guard ~default:Wire.UInt8.zero
           (Types.where guard Types.uint8));
    ];
  (* The container may be reached through a decoration, so the walk has to see
     past a [map] to the element underneath. *)
  rejects "array under a map" ~codec:"Mapped" ~mentions:[ "vals" ]
    [
      Types.field "n" Types.uint8;
      Types.field "vals"
        (Types.map List.length
           (fun _ -> [])
           (Types.array ~len:(Types.ref "n") (Types.where guard Types.uint8)));
    ]

(* A [where] as a casetype case body projects to [case k: T { cond } v;], which
   is not valid 3D either: a refinement is not allowed on a case body. *)
let test_where_in_case_body_rejected () =
  let tagged =
    Types.casetype "Body" Types.uint8
      [
        Types.case ~index:(Wire.UInt8.v 1) (Types.where guard Types.uint8)
          ~inject:Fun.id ~project:(fun v -> Some v);
      ]
  in
  rejects "casetype case body" ~codec:"Tagged" ~mentions:[ "payload" ]
    [ Types.field "n" Types.uint8; Types.field "payload" tagged ]

(* A [where true] emits no refinement, so it is a no-op wrapper that must stay
   allowed wherever a plain element is: rejecting it would refuse a shape 3D
   accepts. A top-level [where] is the form that does project, so it is allowed
   too. *)
let test_harmless_where_accepted () =
  accepts "TopLevel"
    [
      Types.field "n" Types.uint8;
      Types.field "v" (Types.where guard Types.uint8);
    ];
  accepts "TrivialInContainer"
    [
      Types.field "n" Types.uint8;
      Types.field "vals"
        (Types.array ~len:(Types.ref "n") (Types.where (bool true) Types.uint8));
    ];
  accepts "NoWhere"
    [
      Types.field "n" Types.uint8;
      Types.field "vals" (Types.array ~len:(Types.ref "n") Types.uint8);
    ]

(* The projection rewrites a casetype whose tag is not integer-dispatched into
   the tag plus an [all_bytes] body, so such a field runs to the end of the
   buffer whatever its cases hold. 3d.exe parses the result and then fails
   verification on the kind of that body, so the shape is caught here instead. *)
let string_tagged =
  Types.casetype "Body" Types.zeroterm
    [
      Types.case ~index:"a"
        (Types.byte_array ~size:(int 2))
        ~inject:Fun.id
        ~project:(fun v -> Some v);
      Types.case ~index:"b"
        (Types.byte_array ~size:(int 2))
        ~inject:Fun.id
        ~project:(fun v -> Some v);
    ]

let test_string_tagged_casetype_not_last () =
  rejects "string-tagged casetype before another field" ~codec:"Union"
    ~mentions:[ "kind" ]
    [ Types.field "kind" string_tagged; Types.field "crc" Types.uint16be ];
  rejects "string-tagged casetype in the middle" ~codec:"Framed"
    ~mentions:[ "kind" ]
    [
      Types.field "n" Types.uint8;
      Types.field "kind" string_tagged;
      Types.field "crc" Types.uint16be;
    ]

(* As the last field the same casetype is the shape EverParse verifies, and an
   integer tag bounds the body wherever the field sits. *)
let test_tagged_casetype_accepted () =
  accepts "Trailing"
    [ Types.field "n" Types.uint8; Types.field "kind" string_tagged ];
  accepts "IntTagged"
    [ Types.field "kind" int_tagged; Types.field "crc" Types.uint16be ]

(* EverParse #321: a direct [[:zeroterm]] field extracts by itself, but placing
   any other field before or after it creates a KaRaMeL bundling cycle. The
   bounded form is a different prelude construct and keeps extracting. *)
let test_zeroterm_with_sibling_rejected () =
  accepts "OnlyZeroterm" [ Types.field "z" Types.zeroterm ];
  rejects "field before zeroterm" ~codec:"TrailingZeroterm" ~mentions:[ "z" ]
    [ Types.field "n" Types.uint8; Types.field "z" Types.zeroterm ];
  rejects "field after zeroterm" ~codec:"LeadingZeroterm" ~mentions:[ "z" ]
    [ Types.field "z" Types.zeroterm; Types.field "n" Types.uint8 ];
  accepts "BoundedZeroterm"
    [
      Types.field "n" Types.uint8;
      Types.field "z" (Types.zeroterm_at_most ~size:(int 8));
    ]

(* 3D takes a refinement only after a field it reads as a scalar. On a byte
   span, an array, a sub-codec or a casetype, 3d.exe refuses the generated
   [.3d] ("Non-scalar field 'X' cannot be refined with constraints", or no
   reader for the field) while OCaml decode still enforces the predicate. *)
let constrained typ =
  [
    Types.field "n" Types.uint8;
    Types.field "x" ~constraint_:guard typ;
    Types.field "tail" Types.uint8;
  ]

let rejects_constrained label typ =
  rejects label ~codec:"Refined" ~mentions:[ "x" ] (constrained typ)

let test_refined_non_scalar_rejected () =
  rejects_constrained "byte_array" (Types.byte_array ~size:(int 4));
  rejects_constrained "byte_slice" (Types.byte_slice ~size:(int 4));
  rejects_constrained "byte_array_where"
    (Types.byte_array_where ~size:(int 4) ~per_byte:(fun b ->
         Types.Lt (b, int 9)));
  rejects_constrained "uint ~size" (Types.uint 3);
  rejects_constrained "zeroterm" Types.zeroterm;
  rejects_constrained "zeroterm_at_most" (Types.zeroterm_at_most ~size:(int 8));
  rejects_constrained "array" (Types.array ~len:(int 4) Types.uint8);
  rejects_constrained "repeat" (Types.repeat ~size:(int 4) Types.uint8);
  rejects_constrained "nested" (Types.nested ~size:(int 4) Types.uint32be);
  rejects_constrained "nested_at_most"
    (Types.nested_at_most ~size:(int 8) Types.uint32be);
  rejects_constrained "dynamic optional" (Types.optional guard Types.uint16be);
  rejects_constrained "sub-codec" (Wire.codec sub);
  rejects_constrained "casetype" int_tagged;
  (* A span under a [map] renders as the span does. *)
  rejects_constrained "span under a map"
    (Types.map String.length (fun _ -> "xxxx") (Types.byte_array ~size:(int 4)));
  (* A greedy field is refined the same way, and is legal only last. *)
  rejects "all_bytes" ~codec:"Greedy" ~mentions:[ "x" ]
    [
      Types.field "n" Types.uint8;
      Types.field "x" ~constraint_:guard Types.all_bytes;
    ];
  rejects "all_zeros" ~codec:"Padded" ~mentions:[ "x" ]
    [
      Types.field "n" Types.uint8;
      Types.field "x" ~constraint_:guard Types.all_zeros;
    ]

(* A [where] at the top of a field's type, or under the [enum] / [map] wrappers
   the projection sees through, is lifted to the field's refinement, so it meets
   the same rule as a [~constraint_] written there. *)
let test_lifted_where_on_non_scalar_rejected () =
  rejects "where on a span" ~codec:"Wrapped" ~mentions:[ "x" ]
    [
      Types.field "n" Types.uint8;
      Types.field "x" (Types.where guard (Types.byte_array ~size:(int 4)));
      Types.field "tail" Types.uint8;
    ];
  rejects "where under a map over a span" ~codec:"Lifted" ~mentions:[ "x" ]
    [
      Types.field "n" Types.uint8;
      Types.field "x"
        (Types.map String.length
           (fun _ -> "xxxx")
           (Types.where guard (Types.byte_array ~size:(int 4))));
      Types.field "tail" Types.uint8;
    ]

(* Refining the field whose value the predicate reads is the form that projects,
   so every scalar carrier has to stay allowed. *)
let test_refined_scalar_accepted () =
  accepts "Scalars"
    [
      Types.field "n" Types.uint8;
      Types.field "a" ~constraint_:guard Types.uint32be;
      Types.field "b" ~constraint_:guard Types.int16be;
      Types.field "c" ~constraint_:guard Types.float64;
      Types.field "d" ~constraint_:guard (Types.bits ~width:4 Types.U8);
      Types.field "e" ~constraint_:guard (Types.bits ~width:4 Types.U8);
    ];
  accepts "Wrappers"
    [
      Types.field "n" Types.uint8;
      Types.field "a" ~constraint_:guard
        (Types.enum "E" [ ("A", 1); ("B", 2) ] Types.uint8);
      Types.field "b" ~constraint_:guard
        (Types.map Wire.UInt8.to_int Wire.UInt8.v Types.uint8);
      Types.field "c" ~constraint_:guard (Types.where guard Types.uint8);
    ];
  (* A statically-present optional renders as its inner; a statically-absent one
     projects to a 0-byte [unit] field that carries no refinement at all; a
     dynamic [optional_or] occupies its inner's bytes either way. *)
  accepts "Optionals"
    [
      Types.field "n" Types.uint8;
      Types.field "a" ~constraint_:guard
        (Types.optional (bool true) Types.uint16be);
      Types.field "b" ~constraint_:guard
        (Types.optional (bool false) Types.uint16be);
      Types.field "c" ~constraint_:guard
        (Types.optional_or guard ~default:(Wire.UInt16.v 0) Types.uint16be);
    ]

(* EverParse holds a byte size in a [u32]. A count bounded by [n <= K] times a
   literal width is the one case where the overflow is certain from the schema,
   so it is reported here instead of as an F* diagnostic pointing into
   generated code. 2^29 elements of 8 bytes is exactly 2^32. *)
let count_times_width bound width =
  [
    Types.field "n"
      ~constraint_:(Types.Le (Types.ref "n", int bound))
      Types.uint32be;
    Types.field "body"
      (Types.byte_array ~size:(Types.Mul (Types.ref "n", int width)));
  ]

let test_byte_size_product_rejected () =
  rejects "bound reaches 2^32 bytes" ~codec:"Wide"
    ~mentions:[ "body"; "n"; "536870912"; "8" ]
    (count_times_width 0x2000_0000 8);
  rejects "bound exceeds 2^32 bytes" ~codec:"Wider" ~mentions:[ "body"; "n" ]
    (count_times_width 0x3000_0000 8);
  (* The literal may lead. *)
  rejects "literal coefficient first" ~codec:"Flipped" ~mentions:[ "body"; "n" ]
    [
      Types.field "n"
        ~constraint_:(Types.Le (Types.ref "n", int 0x2000_0000))
        Types.uint32be;
      Types.field "body"
        (Types.byte_array ~size:(Types.Mul (int 8, Types.ref "n")));
    ];
  (* The product may sit inside a larger size expression. *)
  rejects "product under an addition" ~codec:"Sum" ~mentions:[ "body"; "n" ]
    [
      Types.field "n"
        ~constraint_:(Types.Le (Types.ref "n", int 0x2000_0000))
        Types.uint32be;
      Types.field "body"
        (Types.byte_array
           ~size:(Types.Add (Types.Mul (Types.ref "n", int 8), int 4)));
    ]

let test_byte_size_product_accepted () =
  (* One element short of the first bound that overflows. *)
  accepts "Tight" (count_times_width 0x1FFF_FFFF 8);
  accepts "Narrow" (count_times_width 1024 8);
  (* A conjunction is read for the tightest bound it states, so the [<=] that
     saves the product counts even next to another clause. *)
  accepts "Conjunction"
    [
      Types.field "n"
        ~constraint_:
          (Types.And
             ( Types.Ge (Types.ref "n", int 1),
               Types.Le (Types.ref "n", int 1024) ))
        Types.uint32be;
      Types.field "body"
        (Types.byte_array ~size:(Types.Mul (Types.ref "n", int 8)));
    ];
  (* With no bound in the schema there is nothing certain to report, and the
     diagnostic stays with EverParse rather than guessing. *)
  accepts "Unbounded"
    [
      Types.field "n" Types.uint32be;
      Types.field "body"
        (Types.byte_array ~size:(Types.Mul (Types.ref "n", int 8)));
    ]

(* Field names index the decode slots and the generated struct members, so a
   repeat is ambiguous on both sides. *)
let test_duplicate_field_names () =
  rejects "same name twice" ~codec:"Dup" ~mentions:[ "x" ]
    [
      Types.field "x" Types.uint8;
      Types.field "y" Types.uint8;
      Types.field "x" Types.uint16be;
    ];
  rejects "adjacent duplicates" ~codec:"Adjacent" ~mentions:[ "len" ]
    [ Types.field "len" Types.uint8; Types.field "len" Types.uint8 ]

let test_distinct_and_anonymous_names_accepted () =
  accepts "Distinct"
    [ Types.field "x" Types.uint8; Types.field "y" Types.uint8 ];
  (* Anonymous padding fields have no name to collide on. *)
  accepts "Padding"
    [
      Types.anon_field Types.uint8;
      Types.anon_field Types.uint8;
      Types.field "x" Types.uint8;
    ]

let test_empty_field_list_accepted () = accepts "Empty" []

(* A layout that uses every checked construct within its limits passes all four
   checks together, which is what a real schema looks like. *)
let test_realistic_layout_accepted () =
  accepts "Frame"
    [
      Types.field "version"
        ~constraint_:(Types.Eq (Types.ref "version", int 1))
        Types.uint8;
      Types.field "n"
        ~constraint_:(Types.Le (Types.ref "n", int 1024))
        Types.uint32be;
      Types.field "vals" (Types.array ~len:(Types.ref "n") Types.uint8);
      Types.field "body"
        (Types.byte_array ~size:(Types.Mul (Types.ref "n", int 8)));
      Types.anon_field Types.uint16be;
      Types.field "trailer" Types.all_bytes;
    ]

let suite =
  ( "shape",
    [
      Alcotest.test_case "greedy not last rejected" `Quick test_greedy_not_last;
      Alcotest.test_case "greedy last accepted" `Quick test_greedy_last_accepted;
      Alcotest.test_case "nested where rejected" `Quick
        test_nested_where_rejected;
      Alcotest.test_case "where in case body rejected" `Quick
        test_where_in_case_body_rejected;
      Alcotest.test_case "string-tagged casetype not last rejected" `Quick
        test_string_tagged_casetype_not_last;
      Alcotest.test_case "tagged casetype accepted" `Quick
        test_tagged_casetype_accepted;
      Alcotest.test_case "zeroterm with sibling rejected" `Quick
        test_zeroterm_with_sibling_rejected;
      Alcotest.test_case "refined non-scalar rejected" `Quick
        test_refined_non_scalar_rejected;
      Alcotest.test_case "lifted where on non-scalar rejected" `Quick
        test_lifted_where_on_non_scalar_rejected;
      Alcotest.test_case "refined scalar accepted" `Quick
        test_refined_scalar_accepted;
      Alcotest.test_case "harmless where accepted" `Quick
        test_harmless_where_accepted;
      Alcotest.test_case "byte-size product rejected" `Quick
        test_byte_size_product_rejected;
      Alcotest.test_case "byte-size product accepted" `Quick
        test_byte_size_product_accepted;
      Alcotest.test_case "duplicate field names rejected" `Quick
        test_duplicate_field_names;
      Alcotest.test_case "distinct and anonymous names accepted" `Quick
        test_distinct_and_anonymous_names_accepted;
      Alcotest.test_case "empty field list accepted" `Quick
        test_empty_field_list_accepted;
      Alcotest.test_case "realistic layout accepted" `Quick
        test_realistic_layout_accepted;
    ] )
