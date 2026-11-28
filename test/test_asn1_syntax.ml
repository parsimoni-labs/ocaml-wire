(* Tests for asn1_syntax.ml: the ASN.1 combinators, object identifiers, and
   every shape docs/asn1.md ("Refused when built") says the combinators
   refuse. *)

open Wire

(* [f ()] raises [Invalid_argument] with a message that mentions [msg]: the
   message names the shape and the reason, so a refusal for another reason
   does not pass. *)
let rejects ~msg f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument mentioning %S" msg
  | exception Invalid_argument m ->
      if not (Re.execp (Re.compile (Re.str msg)) m) then
        Alcotest.failf "Invalid_argument %S does not mention %S" m msg

let builds f = ignore (f ())

(* -- The running example of docs/asn1.md: RFC 5280 SubjectPublicKeyInfo -- *)

type params = Null | Curve of Asn1.Oid.t | Absent
type alg = { oid : Asn1.Oid.t; params : params }
type spki = { alg : alg; key : int * string }

let rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 1 ])
let ec = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 2; 1 ])
let ed25519 = Asn1.Oid.(base 1 3 <|| [ 101; 112 ])
let f_oid = Asn1.required "algorithm" Asn1.oid

let params_cases =
  Asn1.
    [
      case rsa null
        ~inject:(fun () -> Null)
        ~project:(function Null -> Some () | _ -> None);
      case ec oid
        ~inject:(fun c -> Curve c)
        ~project:(function Curve c -> Some c | _ -> None);
      absent ed25519 ~inject:Absent ~project:(function
        | Absent -> true
        | _ -> false);
    ]

let f_params = Asn1.defined_by "parameters" ~on:f_oid params_cases

let alg_id () =
  Asn1.sequence "AlgorithmIdentifier"
    (fun oid params -> { oid; params })
    Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ]

let spki () =
  let f_alg = Asn1.required "algorithm" (alg_id ()) in
  let f_key = Asn1.required "subjectPublicKey" Asn1.bit_string in
  Asn1.sequence "SubjectPublicKeyInfo"
    (fun alg key -> { alg; key })
    Codec.[ (f_alg $ fun s -> s.alg); (f_key $ fun s -> s.key) ]

let test_spki_builds () = builds spki

(* -- Object identifiers -- *)

let oid = Alcotest.testable Asn1.Oid.pp Asn1.Oid.equal

let roundtrip s =
  match Asn1.Oid.of_string s with
  | None -> Alcotest.failf "of_string %S: None" s
  | Some o -> Alcotest.(check string) s s (Asn1.Oid.to_string o)

let test_oid_example_strings () =
  Alcotest.(check string)
    "rsaEncryption" "1.2.840.113549.1.1.1" (Asn1.Oid.to_string rsa);
  Alcotest.(check string)
    "id-ecPublicKey" "1.2.840.10045.2.1" (Asn1.Oid.to_string ec);
  Alcotest.(check string)
    "id-Ed25519" "1.3.101.112"
    (Asn1.Oid.to_string ed25519);
  Alcotest.(check string) "pp" "1.3.101.112" (Fmt.str "%a" Asn1.Oid.pp ed25519)

let test_oid_example_roundtrip () =
  List.iter
    (fun o ->
      Alcotest.(check (option oid))
        (Asn1.Oid.to_string o) (Some o)
        (Asn1.Oid.of_string (Asn1.Oid.to_string o)))
    [ rsa; ec; ed25519 ]

(* Arcs past 2^30 go through strings, which narrow-int targets read too. *)
let test_oid_edge_arcs () =
  List.iter roundtrip
    [
      "0.0";
      "0.39";
      "1.39";
      "2.0";
      "1.2.0";
      "1.2.2147483647" (* 2^31 - 1 *);
      "1.2.2147483648" (* 2^31, docs/asn1.md Q7 *);
      "1.2.4294967295" (* 2^32 - 1 *);
      "2.4294967215" (* 2^32 - 81: the first subidentifier is 2^32 - 1 *);
    ]

(* everparse-asn1 reads each subidentifier below 2^32
   (spec/ASN1.Spec.Content.OIDU32.fst), so 2^32 is not an arc, and under 2 the
   second arc is below 2^32 - 80 (spec/ASN1.Base.fst:193-194). *)
let test_oid_of_string_rejects () =
  List.iter
    (fun s -> Alcotest.(check (option oid)) s None (Asn1.Oid.of_string s))
    [
      "";
      "1";
      "1.";
      ".1";
      "1..2";
      "01.2";
      "1.02";
      "3.1";
      "0.40";
      "1.40";
      "-1.2";
      "1.-2";
      "+1.2";
      "1.2.a";
      "1.2.4294967296" (* 2^32 *);
      "1.2.99999999999";
      "2.4294967216" (* 2^32 - 80 *);
      " 1.2";
    ]

let test_oid_compare () =
  let o s = Option.get (Asn1.Oid.of_string s) in
  Alcotest.(check bool)
    "equal" true
    (Asn1.Oid.equal (o "1.2.840") (o "1.2.840"));
  Alcotest.(check bool)
    "builders" true
    Asn1.Oid.(equal (base 1 2 <| 840) (o "1.2.840"));
  Alcotest.(check bool)
    "prefix first" true
    (Asn1.Oid.compare (o "1.2") (o "1.2.0") < 0);
  Alcotest.(check bool)
    "arc by arc" true
    (Asn1.Oid.compare (o "1.2.9") (o "1.3") < 0);
  Alcotest.(check bool)
    "unsigned arcs" true
    (Asn1.Oid.compare (o "1.2.2147483647") (o "1.2.4294967295") < 0)

let test_oid_base_rejects () =
  rejects ~msg:"Oid.base 3 0" (fun () -> Asn1.Oid.base 3 0);
  rejects ~msg:"Oid.base 1 40" (fun () -> Asn1.Oid.base 1 40);
  rejects ~msg:"Oid.base 0 -1" (fun () -> Asn1.Oid.base 0 (-1));
  rejects ~msg:"Oid.base -1 0" (fun () -> Asn1.Oid.base (-1) 0)

let test_oid_arc_rejects () =
  rejects ~msg:"between 0 and 2^32 - 1" (fun () -> Asn1.Oid.(rsa <| -1));
  rejects ~msg:"between 0 and 2^32 - 1" (fun () -> Asn1.Oid.(rsa <|| [ 1; -1 ]));
  (* An [int] reaches 2^32 only where it is wider than 32 bits. *)
  if Sys.int_size > 32 then
    rejects ~msg:"between 0 and 2^32 - 1" (fun () -> Asn1.Oid.(rsa <| 1 lsl 32))

(* -- Relative object identifiers -- *)

let reloid = Alcotest.testable Asn1.Relative_oid.pp Asn1.Relative_oid.equal

(* A RELATIVE-OID is one or more arcs below 2^32, with no rule on the first
   two (X.690 8.20.2, spec/ASN1.Base.fst:204-207). *)
let test_relative_oid_strings () =
  List.iter
    (fun s ->
      match Asn1.Relative_oid.of_string s with
      | None -> Alcotest.failf "Relative_oid.of_string %S: None" s
      | Some r -> Alcotest.(check string) s s (Asn1.Relative_oid.to_string r))
    [ "0"; "5"; "99.1"; "1.2.3"; "2147483648" (* 2^31 *); "4294967295" ];
  Alcotest.(check string)
    "v" "8571.3.2"
    (Asn1.Relative_oid.to_string (Asn1.Relative_oid.v [ 8571; 3; 2 ]));
  Alcotest.(check string)
    "pp" "1.2"
    (Fmt.str "%a" Asn1.Relative_oid.pp (Asn1.Relative_oid.v [ 1; 2 ]));
  List.iter
    (fun s ->
      Alcotest.(check (option reloid)) s None (Asn1.Relative_oid.of_string s))
    [ ""; "."; "1."; ".1"; "1..2"; "01"; "-1"; "a"; "4294967296" (* 2^32 *) ];
  Alcotest.(check bool)
    "compare" true
    (Asn1.Relative_oid.(compare (v [ 1; 2 ]) (v [ 1; 2; 0 ])) < 0)

let test_relative_oid_v_rejects () =
  rejects ~msg:"a RELATIVE-OID has at least one arc" (fun () ->
      Asn1.Relative_oid.v []);
  rejects ~msg:"Relative_oid.v -1: an arc is between 0 and 2^32 - 1" (fun () ->
      Asn1.Relative_oid.v [ 1; -1 ]);
  if Sys.int_size > 32 then
    rejects ~msg:"an arc is between 0 and 2^32 - 1" (fun () ->
        Asn1.Relative_oid.v [ 1 lsl 32 ])

(* -- Positive shapes the rejections must not catch -- *)

(* A SEQUENCE tells an OPTIONAL [0] IMPLICIT INTEGER from a following [0]
   EXPLICIT one by the constructed bit, as everparse-asn1 does. *)
let test_sequence_constructed_bit () =
  let a = Asn1.optional "a" (Asn1.implicit 0 Asn1.integer) in
  let b = Asn1.required "b" (Asn1.explicit 0 Asn1.integer) in
  builds (fun () ->
      Asn1.sequence "S" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ])

let test_sequence_trailing_optional () =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.optional "b" Asn1.integer in
  let c = Asn1.optional "c" (Asn1.implicit 0 Asn1.integer) in
  builds (fun () ->
      Asn1.sequence "S"
        (fun a b c -> (a, b, c))
        Codec.
          [
            (a $ fun (a, _, _) -> a);
            (b $ fun (_, b, _) -> b);
            (c $ fun (_, _, c) -> c);
          ])

let test_default_shapes () =
  (* X.509's [[0] EXPLICIT Version DEFAULT v1]. *)
  builds (fun () ->
      Asn1.default "version" "\x00" (Asn1.explicit 0 Asn1.integer));
  builds (fun () -> Asn1.default "b" false Asn1.bool);
  builds (fun () -> Asn1.default "v" 1L (Asn1.bounded_integer ~min:0L ~max:2L));
  builds (fun () ->
      Asn1.default "s" "ab" (Asn1.size ~min:1 ~max:2 Asn1.utf8_string));
  (* DEFAULT on DATE, TIME-OF-DAY, DATE-TIME and the IRIs, also under one
     EXPLICIT tag (README:431-434,464-467). *)
  builds (fun () -> Asn1.default "d" "20250101" Asn1.date);
  builds (fun () ->
      Asn1.default "t" "120000" (Asn1.explicit 0 Asn1.time_of_day));
  builds (fun () -> Asn1.default "dt" "20250101120000" Asn1.date_time);
  builds (fun () -> Asn1.default "i" "/ISO/a" Asn1.oid_iri);
  builds (fun () -> Asn1.default "r" "a/1" Asn1.relative_oid_iri)

(* -- Refused when built: one test per shape of docs/asn1.md -- *)

let test_implicit_on_choice () =
  let c =
    Asn1.choice
      [ Asn1.alternative "i" Asn1.integer ~inject:Fun.id ~project:Option.some ]
  in
  rejects ~msg:"IMPLICIT on a CHOICE" (fun () -> Asn1.implicit 0 c)

let test_implicit_on_any () =
  rejects ~msg:"IMPLICIT on ANY" (fun () -> Asn1.implicit 0 Asn1.any)

let test_implicit_on_mapped_choice () =
  let c =
    Asn1.choice
      [ Asn1.alternative "i" Asn1.integer ~inject:Fun.id ~project:Option.some ]
  in
  rejects ~msg:"IMPLICIT on a CHOICE" (fun () ->
      Asn1.implicit 0 (Asn1.map ~decode:Fun.id ~encode:Fun.id c))

let test_implicit_on_containing () =
  rejects ~msg:"IMPLICIT on an OCTET STRING (CONTAINING)" (fun () ->
      Asn1.implicit 0 (Asn1.containing uint8))

let alt name t = Asn1.alternative name t ~inject:Fun.id ~project:Option.some

let test_choice_same_tag () =
  rejects ~msg:"alternatives \"a\" and \"b\" can both start with [UNIVERSAL 2]"
    (fun () -> Asn1.choice [ alt "a" Asn1.integer; alt "b" Asn1.integer ])

let test_choice_nested_same_tag () =
  let inner = Asn1.choice [ alt "x" Asn1.octet_string; alt "y" Asn1.integer ] in
  rejects ~msg:"can both start with [UNIVERSAL 2]" (fun () ->
      Asn1.choice [ alt "a" Asn1.integer; alt "b" inner ])

let test_choice_any () =
  rejects ~msg:"alternative \"a\" is ANY" (fun () ->
      Asn1.choice [ alt "a" Asn1.any; alt "b" Asn1.integer ])

let test_choice_empty () =
  rejects ~msg:"at least one alternative" (fun () -> Asn1.choice [])

let test_choice_repeated_name () =
  rejects ~msg:"alternative \"a\" is given twice" (fun () ->
      Asn1.choice [ alt "a" Asn1.integer; alt "a" Asn1.octet_string ])

let pair a b = Codec.[ (a $ fun (a, _) -> a); (b $ fun (_, b) -> b) ]
let mk a b = (a, b)

let test_set_same_tag () =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.required "b" Asn1.integer in
  rejects ~msg:"components \"a\" and \"b\" can both start with [UNIVERSAL 2]"
    (fun () -> Asn1.set "S" mk (pair a b))

(* A SET ignores the constructed bit (spec/ASN1.Base.fst:522-525). *)
let test_set_same_tag_other_flag () =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.integer) in
  let b = Asn1.required "b" (Asn1.explicit 0 Asn1.integer) in
  rejects ~msg:"can both start with [0]" (fun () -> Asn1.set "S" mk (pair a b))

(* [n] BOOLEAN components [[0]] to [[n - 1]], with the constructor that
   ignores them. *)
type fields = Fields : 'f * ('f, unit, unit) Codec.fields -> fields

let rec bools i n =
  if i = n then Fields ((), Codec.[])
  else
    match bools (i + 1) n with
    | Fields (make, rest) ->
        let c = Asn1.required (Fmt.str "c%d" i) (Asn1.implicit i Asn1.bool) in
        Fields ((fun (_ : bool) -> make), Codec.((c $ fun () -> false) :: rest))

let test_set_over_64 () =
  (match bools 0 64 with
  | Fields (make, fields) -> builds (fun () -> Asn1.set "S64" make fields));
  match bools 0 65 with
  | Fields (make, fields) ->
      rejects ~msg:"Wire.Asn1.set \"S65\": 65 components" (fun () ->
          Asn1.set "S65" make fields)

let test_set_defined_by () =
  rejects ~msg:"component \"parameters\" is defined by a key" (fun () ->
      Asn1.set "S"
        (fun oid params -> { oid; params })
        Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ])

let test_optional_then_same_tag () =
  let a = Asn1.optional "a" Asn1.integer in
  let b = Asn1.required "b" Asn1.integer in
  rejects
    ~msg:
      "component \"b\" and the OPTIONAL or DEFAULT component \"a\" before it \
       can both start with [UNIVERSAL 2]" (fun () ->
      Asn1.sequence "S" mk (pair a b))

let test_optionals_meet () =
  let a = Asn1.optional "a" (Asn1.implicit 1 Asn1.integer) in
  let b = Asn1.optional "b" (Asn1.implicit 1 Asn1.integer) in
  rejects ~msg:"component \"a\" before it can both start with [1]" (fun () ->
      Asn1.sequence "S" mk (pair a b))

let test_default_then_any () =
  let a = Asn1.default "a" false Asn1.bool in
  let b = Asn1.required "b" Asn1.any in
  rejects ~msg:"\"b\" and the OPTIONAL or DEFAULT component \"a\"" (fun () ->
      Asn1.sequence "S" mk (pair a b))

(* The run rule holds under each key: the case type takes the place of the
   [defined_by] component. *)
let test_defined_by_case_meets_optional () =
  let k = Asn1.required "k" Asn1.oid in
  let o = Asn1.optional "o" Asn1.integer in
  let d =
    Asn1.defined_by "d" ~on:k
      [ Asn1.case rsa Asn1.integer ~inject:Fun.id ~project:Option.some ]
  in
  rejects ~msg:"component \"d\" and the OPTIONAL or DEFAULT component \"o\""
    (fun () ->
      Asn1.sequence "S"
        (fun k o d -> (k, o, d))
        Codec.
          [
            (k $ fun (k, _, _) -> k);
            (o $ fun (_, o, _) -> o);
            (d $ fun (_, _, d) -> d);
          ])

let test_default_on_constructed () =
  rejects ~msg:"DEFAULT on a constructed type or ANY, here SEQUENCE OF INTEGER"
    (fun () -> Asn1.default "d" [] (Asn1.sequence_of Asn1.integer));
  let name = Asn1.implicit 0 (Asn1.size ~min:1 ~max:8 Asn1.utf8_string) in
  let c =
    Asn1.choice
      [
        Asn1.alternative "flag" Asn1.bool ~inject:Either.left
          ~project:Either.find_left;
        Asn1.alternative "name" name ~inject:Either.right
          ~project:Either.find_right;
      ]
  in
  rejects
    ~msg:
      "here CHOICE { flag BOOLEAN, name [0] IMPLICIT UTF8String (SIZE (1..8)) }"
    (fun () -> Asn1.default "d" (Either.Left true) c)

let test_default_on_any () =
  rejects ~msg:"DEFAULT on a constructed type or ANY, here ANY" (fun () ->
      Asn1.default "d" "\x05\x00" Asn1.any)

let test_default_on_oid () =
  rejects ~msg:"DEFAULT on an OBJECT IDENTIFIER" (fun () ->
      Asn1.default "d" rsa Asn1.oid)

let test_default_on_relative_oid () =
  rejects ~msg:"DEFAULT on a RELATIVE-OID" (fun () ->
      Asn1.default "d" (Asn1.Relative_oid.v [ 1 ]) Asn1.relative_oid)

(* everparse-asn1 checks that a time or IRI DEFAULT is a DER value of its type
   (README:431-434). *)
let test_default_not_a_value () =
  rejects ~msg:"the default \"2025-01-01\" is not a DER DATE" (fun () ->
      Asn1.default "d" "2025-01-01" Asn1.date);
  rejects ~msg:"the default \"240000\" is not a DER TIME-OF-DAY" (fun () ->
      Asn1.default "t" "240000" (Asn1.explicit 0 Asn1.time_of_day));
  rejects ~msg:"the default \"15000101120000\" is not a DER DATE-TIME"
    (fun () -> Asn1.default "dt" "15000101120000" Asn1.date_time);
  rejects ~msg:"the default \"ISO\" is not a DER OID-IRI" (fun () ->
      Asn1.default "i" "ISO" Asn1.oid_iri);
  rejects ~msg:"the default \"/a\" is not a DER RELATIVE-OID-IRI" (fun () ->
      Asn1.default "r" "/a" Asn1.relative_oid_iri);
  rejects ~msg:"the default \"250101120000\" is not a DER UTCTime" (fun () ->
      Asn1.default "u" "250101120000" Asn1.utc_time)

let test_default_explicit_constrained_integer () =
  rejects ~msg:"DEFAULT on a constrained INTEGER under an EXPLICIT tag"
    (fun () ->
      Asn1.default "v" 0L
        (Asn1.explicit 0 (Asn1.bounded_integer ~min:0L ~max:2L)))

let test_default_explicit_enumerated () =
  rejects ~msg:"DEFAULT on an ENUMERATED under an EXPLICIT tag" (fun () ->
      Asn1.default "v" 0 (Asn1.explicit 0 (Asn1.enumerated [ ("v1", 0) ])))

let test_default_two_explicit () =
  rejects ~msg:"DEFAULT under two EXPLICIT tags" (fun () ->
      Asn1.default "v" false (Asn1.explicit 0 (Asn1.explicit 1 Asn1.bool)))

let test_default_out_of_range () =
  rejects ~msg:"the default 3 is outside INTEGER (0..2)" (fun () ->
      Asn1.default "v" 3L (Asn1.bounded_integer ~min:0L ~max:2L))

let test_size_on_sequence_of () =
  rejects ~msg:"a SEQUENCE OF or SET OF takes ?non_empty" (fun () ->
      Asn1.size ~min:1 ~max:2
        (Asn1.map ~decode:(String.concat "")
           ~encode:(fun s -> [ s ])
           (Asn1.sequence_of Asn1.octet_string)))

let test_size_on_size () =
  rejects ~msg:"a type takes one SIZE" (fun () ->
      Asn1.size ~min:1 ~max:2 (Asn1.size ~min:0 ~max:4 Asn1.octet_string))

(* SIZE applies to OCTET STRING and the character strings only
   (README:452-458); RELATIVE-OID has no [string] carrier, so [size] does not
   take it. *)
let test_size_on_times_and_iris () =
  List.iter
    (fun t ->
      rejects ~msg:"applies to an OCTET STRING or a character string only"
        (fun () -> Asn1.size ~min:0 ~max:8 t))
    Asn1.[ date; time_of_day; date_time; oid_iri; relative_oid_iri ]

let test_size_not_a_range () =
  rejects ~msg:"SIZE (3..2) is not a range" (fun () ->
      Asn1.size ~min:3 ~max:2 Asn1.octet_string);
  rejects ~msg:"SIZE (-1..2) is not a range" (fun () ->
      Asn1.size ~min:(-1) ~max:2 Asn1.octet_string)

let test_size_reaches_65536 () =
  rejects ~msg:"SIZE (0..65536) is not a range" (fun () ->
      Asn1.size ~min:0 ~max:65536 Asn1.octet_string)

let case_int k = Asn1.case k Asn1.integer ~inject:Fun.id ~project:Option.some

let test_key_optional () =
  let k = Asn1.optional "k" Asn1.oid in
  rejects ~msg:"the key \"k\" is OPTIONAL" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int (Some rsa) ])

let test_key_default () =
  let k = Asn1.default "k" false Asn1.bool in
  rejects ~msg:"the key \"k\" is DEFAULT" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int true ])

let test_key_constrained () =
  let k = Asn1.required "k" (Asn1.bounded_integer ~min:0L ~max:9L) in
  rejects ~msg:"the key \"k\" is constrained" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int 1L ])

let test_key_relative_oid () =
  let k = Asn1.required "k" Asn1.relative_oid in
  rejects ~msg:"the key \"k\" is a RELATIVE-OID" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int (Asn1.Relative_oid.v [ 1 ]) ]);
  (* An IRI or a DATE is a key (README:308-311). *)
  let k = Asn1.required "k" Asn1.oid_iri in
  builds (fun () -> Asn1.defined_by "d" ~on:k [ case_int "/ISO" ]);
  let k = Asn1.required "k" Asn1.date in
  builds (fun () -> Asn1.defined_by "d" ~on:k [ case_int "20250101" ])

let test_key_not_primitive () =
  let k = Asn1.required "k" (Asn1.sequence_of Asn1.integer) in
  rejects ~msg:"the key \"k\" is not a primitive type" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int [] ])

let test_key_after_component () =
  rejects ~msg:"the key \"algorithm\" comes after component \"parameters\""
    (fun () ->
      Asn1.sequence "AlgorithmIdentifier"
        (fun params oid -> { oid; params })
        Codec.[ (f_params $ fun a -> a.params); (f_oid $ fun a -> a.oid) ])

let test_key_in_another_sequence () =
  let other = Asn1.required "other" Asn1.oid in
  rejects
    ~msg:"the key \"algorithm\" of component \"parameters\" is not a component"
    (fun () ->
      Asn1.sequence "Other"
        (fun oid params -> { oid; params })
        Codec.[ (other $ fun a -> a.oid); (f_params $ fun a -> a.params) ])

let test_second_table () =
  let d2 = Asn1.defined_by "more" ~on:f_oid params_cases in
  rejects
    ~msg:"components \"parameters\" and \"more\" are both defined by a key"
    (fun () ->
      Asn1.sequence "S"
        (fun oid p q -> (oid, p, q))
        Codec.
          [
            (f_oid $ fun (o, _, _) -> o);
            (f_params $ fun (_, p, _) -> p);
            (d2 $ fun (_, _, q) -> q);
          ])

let test_repeated_key () =
  let k = Asn1.required "k" Asn1.oid in
  rejects ~msg:"a key is given twice" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int rsa; case_int rsa ])

let test_second_fallback () =
  let k = Asn1.required "k" Asn1.oid in
  let fb =
    Asn1.fallback Asn1.integer
      ~inject:(fun _ v -> Option.value ~default:"" v)
      ~project:(fun v -> Some (rsa, Some v))
  in
  rejects ~msg:"a second fallback" (fun () ->
      Asn1.defined_by "d" ~on:k [ fb; fb ])

let test_oid_arc_2_32 () =
  (* An [int] reaches 2^32 only where it is wider than 32 bits; a narrow-int
     target meets 2^32 through [of_string] alone. *)
  if Sys.int_size > 32 then
    rejects ~msg:"an arc is between 0 and 2^32 - 1" (fun () ->
        Asn1.Oid.(rsa <| 1 lsl 32));
  Alcotest.(check (option oid))
    "of_string" None
    (Asn1.Oid.of_string "1.2.4294967296")

let test_tag_2_32 () =
  rejects ~msg:"tag number -1 is not between 0 and 2^32 - 1" (fun () ->
      Asn1.explicit (-1) Asn1.bool);
  if Sys.int_size > 32 then
    rejects ~msg:"is not between 0 and 2^32 - 1" (fun () ->
        Asn1.implicit (1 lsl 32) Asn1.bool)

let test_universal_0 () =
  rejects ~msg:"tag [UNIVERSAL 0] is reserved" (fun () ->
      Asn1.implicit ~cls:`Universal 0 Asn1.bool);
  rejects ~msg:"tag [UNIVERSAL 0] is reserved" (fun () ->
      Asn1.explicit ~cls:`Universal 0 Asn1.bool);
  rejects ~msg:"tag [UNIVERSAL 15] is reserved" (fun () ->
      Asn1.implicit ~cls:`Universal 15 Asn1.bool)

(* A UNIVERSAL tag everparse-asn1 verifies only where DER gives its type the
   form the tag has, and on the terminal type it numbers (the writers' [wf],
   spec/ASN1.Spec.Conformance.fst:351-394): a run of everparse-asn1 on
   [UNIVERSAL 9] EXPLICIT NULL and on [UNIVERSAL 7] IMPLICIT OCTET STRING
   fails their [ok_] obligation. *)
let test_universal_form () =
  let null = Asn1.null and seq = Asn1.sequence_of Asn1.bool in
  rejects ~msg:"[UNIVERSAL 9] on a constructed encoding" (fun () ->
      Asn1.explicit ~cls:`Universal 9 null);
  rejects ~msg:"[UNIVERSAL 7] on a type other than the one that tag numbers"
    (fun () -> Asn1.implicit ~cls:`Universal 7 Asn1.octet_string);
  rejects ~msg:"[UNIVERSAL 16] on a primitive encoding" (fun () ->
      Asn1.implicit ~cls:`Universal 16 Asn1.bool);
  rejects ~msg:"[UNIVERSAL 4] on a constructed encoding" (fun () ->
      Asn1.implicit ~cls:`Universal 4 seq);
  builds (fun () -> Asn1.explicit ~cls:`Universal 40 null);
  builds (fun () -> Asn1.explicit ~cls:`Universal 16 null);
  builds (fun () -> Asn1.implicit ~cls:`Universal 8 seq);
  builds (fun () -> Asn1.implicit ~cls:`Universal 10 Asn1.integer);
  builds (fun () -> Asn1.implicit ~cls:`Universal 9 Asn1.octet_string);
  builds (fun () -> Asn1.implicit ~cls:`Universal 4 Asn1.octet_string)

let test_keep_bytes_alternative () =
  rejects ~msg:"keep_bytes on a CHOICE alternative" (fun () ->
      Asn1.alternative "a" (Asn1.keep_bytes Asn1.integer) ~inject:fst
        ~project:(fun v -> Some (v, "")))

let test_keep_bytes_default () =
  rejects ~msg:"keep_bytes on a DEFAULT component" (fun () ->
      Asn1.default "d" (false, "") (Asn1.keep_bytes Asn1.bool))

let test_keep_bytes_key () =
  let k = Asn1.required "k" (Asn1.keep_bytes Asn1.oid) in
  rejects ~msg:"the key \"k\" is under keep_bytes" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int (rsa, "") ])

let test_keep_bytes_case () =
  let k = Asn1.required "k" Asn1.oid in
  rejects ~msg:"keep_bytes on the type of a case" (fun () ->
      Asn1.defined_by "d" ~on:k
        [
          Asn1.case rsa (Asn1.keep_bytes Asn1.integer) ~inject:fst
            ~project:(fun v -> Some (v, ""));
        ])

let test_der_in_binary_record () =
  let a = Asn1.required "a" Asn1.bool in
  rejects ~msg:"field a holds an ASN.1 component" (fun () ->
      Codec.v "Bin" Fun.id Codec.[ a $ Fun.id ])

let test_der_under_binary_wrapper () =
  let a = Asn1.required "a" Asn1.bool in
  let mapped = Field.v "a" (map ~decode:not ~encode:not (Field.typ a)) in
  rejects ~msg:"field a holds an ASN.1 component" (fun () ->
      Codec.v "Bin" Fun.id Codec.[ mapped $ Fun.id ])

(* -- Other refusals of asn1.mli -- *)

let test_bounded_integer_empty () =
  rejects ~msg:"INTEGER (2..1) is empty" (fun () ->
      Asn1.bounded_integer ~min:2L ~max:1L)

let test_enumerated_rejects () =
  rejects ~msg:"at least one value" (fun () -> Asn1.enumerated []);
  rejects ~msg:"the name \"a\" is given twice" (fun () ->
      Asn1.enumerated [ ("a", 0); ("a", 1) ]);
  rejects ~msg:"the number 0 is given twice" (fun () ->
      Asn1.enumerated [ ("a", 0); ("b", 0) ])

(* everparse-asn1 reads numbers of magnitude below 2^61
   (everparse-asn1/Lexer.mll:63), the bounds of an .asn INTEGER (lo..hi). *)
let test_bounded_integer_magnitude () =
  let limit = Int64.(sub (shift_left 1L 61) 1L) in
  builds (fun () -> Asn1.bounded_integer ~min:(Int64.neg limit) ~max:limit);
  rejects ~msg:"magnitude 2^61" (fun () ->
      Asn1.bounded_integer ~min:0L ~max:(Int64.succ limit));
  rejects ~msg:"magnitude 2^61" (fun () ->
      Asn1.bounded_integer ~min:Int64.min_int ~max:0L)

let test_enumerated_magnitude () =
  (* An [int] reaches 2^61 only where it has 63 bits. *)
  if Sys.int_size > 61 then
    rejects ~msg:"has magnitude 2^61" (fun () ->
        Asn1.enumerated [ ("a", 0); ("big", 1 lsl 61) ])

(* A DEFAULT is printed as an .asn literal; a value with none is refused. *)
let test_default_without_literal () =
  rejects ~msg:"control character" (fun () ->
      Asn1.default "s" "a\nb" Asn1.utf8_string);
  rejects ~msg:"not valid UTF-8" (fun () ->
      Asn1.default "s" "\xff" Asn1.utf8_string);
  rejects ~msg:"outside printable ASCII" (fun () ->
      Asn1.default "s" "caf\xc3\xa9" Asn1.printable_string);
  rejects ~msg:"multiple of 2" (fun () ->
      Asn1.default "s" "\x00" Asn1.bmp_string);
  rejects ~msg:"not a character" (fun () ->
      Asn1.default "s" "\xd8\x00" Asn1.bmp_string);
  rejects ~msg:"magnitude 2^61" (fun () ->
      Asn1.default "i" "\x40\x00\x00\x00\x00\x00\x00\x00" Asn1.integer);
  rejects ~msg:"no content octets" (fun () -> Asn1.default "i" "" Asn1.integer);
  rejects ~msg:"unused bits are not zero" (fun () ->
      Asn1.default "b" (4, "\xa1") Asn1.bit_string);
  rejects ~msg:"count of unused bits" (fun () ->
      Asn1.default "b" (8, "\x00") Asn1.bit_string);
  builds (fun () -> Asn1.default "s" "a \"b\"" Asn1.utf8_string);
  builds (fun () -> Asn1.default "s" "\x00\xe9" Asn1.bmp_string);
  builds (fun () -> Asn1.default "b" (4, "\xa0") Asn1.bit_string);
  builds (fun () -> Asn1.default "i" "\xff" Asn1.integer)

let test_defined_by_empty () =
  let k = Asn1.required "k" Asn1.oid in
  rejects ~msg:"no case and no fallback" (fun () ->
      Asn1.defined_by "d" ~on:k [])

let test_key_without_literal () =
  let k = Asn1.required "k" Asn1.integer in
  rejects ~msg:"a key is an INTEGER of magnitude 2^61" (fun () ->
      Asn1.defined_by "d" ~on:k
        [ case_int "\x7f\xff\xff\xff\xff\xff\xff\xff\xff" ]);
  let k = Asn1.required "k" Asn1.utf8_string in
  rejects ~msg:"control character" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int "a\tb" ])

(* A time or IRI key is written as everparse-asn1 reads its literal
   (README:431-434), which a value that is not DER does not have. *)
let test_key_time_not_der () =
  let k = Asn1.required "k" Asn1.date in
  builds (fun () -> Asn1.defined_by "d" ~on:k [ case_int "20240229" ]);
  rejects ~msg:"\"20230229\", which is not a DER DATE" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int "20230229" ]);
  let k = Asn1.required "k" Asn1.date_time in
  rejects ~msg:"which is not a DER DATE-TIME" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int "2024022912000" ]);
  let k = Asn1.required "k" Asn1.oid_iri in
  rejects ~msg:"\"a/b\", which is not a DER OID-IRI" (fun () ->
      Asn1.defined_by "d" ~on:k [ case_int "a/b" ])

let test_sequence_foreign_field () =
  let f = Field.v "x" uint8 in
  rejects ~msg:"field \"x\" is not an ASN.1 component" (fun () ->
      Asn1.sequence "S" Fun.id Codec.[ f $ Fun.id ])

let test_sequence_repeated_name () =
  let a = Asn1.required "a" Asn1.bool in
  let b = Asn1.required "a" Asn1.integer in
  rejects ~msg:"component \"a\" is given twice" (fun () ->
      Asn1.sequence "S" mk (pair a b))

let test_field_refined_component () =
  let a = Asn1.required "a" Asn1.bool in
  rejects ~msg:"is an ASN.1 component, which takes no constraint" (fun () ->
      Field.v "a" ~constraint_:(Expr.bool true) (Field.typ a))

let suite =
  ( "asn1_syntax",
    [
      Alcotest.test_case "SubjectPublicKeyInfo builds" `Quick test_spki_builds;
      Alcotest.test_case "oid: example strings" `Quick test_oid_example_strings;
      Alcotest.test_case "oid: example roundtrip" `Quick
        test_oid_example_roundtrip;
      Alcotest.test_case "oid: edge arcs roundtrip" `Quick test_oid_edge_arcs;
      Alcotest.test_case "oid: of_string rejects" `Quick
        test_oid_of_string_rejects;
      Alcotest.test_case "oid: compare" `Quick test_oid_compare;
      Alcotest.test_case "oid: base rejects" `Quick test_oid_base_rejects;
      Alcotest.test_case "oid: arc rejects" `Quick test_oid_arc_rejects;
      Alcotest.test_case "relative oid: strings" `Quick
        test_relative_oid_strings;
      Alcotest.test_case "relative oid: v rejects" `Quick
        test_relative_oid_v_rejects;
      Alcotest.test_case "sequence: constructed bit" `Quick
        test_sequence_constructed_bit;
      Alcotest.test_case "sequence: trailing optional" `Quick
        test_sequence_trailing_optional;
      Alcotest.test_case "default: accepted shapes" `Quick test_default_shapes;
      Alcotest.test_case "reject: implicit on CHOICE" `Quick
        test_implicit_on_choice;
      Alcotest.test_case "reject: implicit on ANY" `Quick test_implicit_on_any;
      Alcotest.test_case "reject: implicit on mapped CHOICE" `Quick
        test_implicit_on_mapped_choice;
      Alcotest.test_case "reject: implicit on CONTAINING" `Quick
        test_implicit_on_containing;
      Alcotest.test_case "reject: CHOICE same tag" `Quick test_choice_same_tag;
      Alcotest.test_case "reject: CHOICE nested same tag" `Quick
        test_choice_nested_same_tag;
      Alcotest.test_case "reject: CHOICE ANY alternative" `Quick test_choice_any;
      Alcotest.test_case "reject: CHOICE empty" `Quick test_choice_empty;
      Alcotest.test_case "reject: CHOICE repeated name" `Quick
        test_choice_repeated_name;
      Alcotest.test_case "reject: SET same tag" `Quick test_set_same_tag;
      Alcotest.test_case "reject: SET same tag, other flag" `Quick
        test_set_same_tag_other_flag;
      Alcotest.test_case "reject: SET over 64" `Quick test_set_over_64;
      Alcotest.test_case "reject: SET with defined_by" `Quick
        test_set_defined_by;
      Alcotest.test_case "reject: OPTIONAL then same tag" `Quick
        test_optional_then_same_tag;
      Alcotest.test_case "reject: OPTIONALs meet" `Quick test_optionals_meet;
      Alcotest.test_case "reject: DEFAULT then ANY" `Quick test_default_then_any;
      Alcotest.test_case "reject: defined_by case meets OPTIONAL" `Quick
        test_defined_by_case_meets_optional;
      Alcotest.test_case "reject: DEFAULT on constructed" `Quick
        test_default_on_constructed;
      Alcotest.test_case "reject: DEFAULT on ANY" `Quick test_default_on_any;
      Alcotest.test_case "reject: DEFAULT on OID" `Quick test_default_on_oid;
      Alcotest.test_case "reject: DEFAULT on RELATIVE-OID" `Quick
        test_default_on_relative_oid;
      Alcotest.test_case "reject: DEFAULT not a value" `Quick
        test_default_not_a_value;
      Alcotest.test_case "reject: DEFAULT on EXPLICIT constrained INTEGER"
        `Quick test_default_explicit_constrained_integer;
      Alcotest.test_case "reject: DEFAULT on EXPLICIT ENUMERATED" `Quick
        test_default_explicit_enumerated;
      Alcotest.test_case "reject: DEFAULT under two EXPLICIT" `Quick
        test_default_two_explicit;
      Alcotest.test_case "reject: DEFAULT out of range" `Quick
        test_default_out_of_range;
      Alcotest.test_case "reject: SIZE on SEQUENCE OF" `Quick
        test_size_on_sequence_of;
      Alcotest.test_case "reject: SIZE on SIZE" `Quick test_size_on_size;
      Alcotest.test_case "reject: SIZE on times and IRIs" `Quick
        test_size_on_times_and_iris;
      Alcotest.test_case "reject: SIZE not a range" `Quick test_size_not_a_range;
      Alcotest.test_case "reject: SIZE reaches 65536" `Quick
        test_size_reaches_65536;
      Alcotest.test_case "reject: key OPTIONAL" `Quick test_key_optional;
      Alcotest.test_case "reject: key DEFAULT" `Quick test_key_default;
      Alcotest.test_case "reject: key constrained" `Quick test_key_constrained;
      Alcotest.test_case "reject: key RELATIVE-OID" `Quick test_key_relative_oid;
      Alcotest.test_case "reject: key not primitive" `Quick
        test_key_not_primitive;
      Alcotest.test_case "reject: key after component" `Quick
        test_key_after_component;
      Alcotest.test_case "reject: key in another SEQUENCE" `Quick
        test_key_in_another_sequence;
      Alcotest.test_case "reject: second table" `Quick test_second_table;
      Alcotest.test_case "reject: repeated key" `Quick test_repeated_key;
      Alcotest.test_case "reject: second fallback" `Quick test_second_fallback;
      Alcotest.test_case "reject: OID arc 2^32" `Quick test_oid_arc_2_32;
      Alcotest.test_case "reject: tag number 2^32" `Quick test_tag_2_32;
      Alcotest.test_case "reject: UNIVERSAL 0" `Quick test_universal_0;
      Alcotest.test_case "reject: UNIVERSAL tag of another form or type" `Quick
        test_universal_form;
      Alcotest.test_case "reject: keep_bytes on alternative" `Quick
        test_keep_bytes_alternative;
      Alcotest.test_case "reject: keep_bytes on DEFAULT" `Quick
        test_keep_bytes_default;
      Alcotest.test_case "reject: keep_bytes on key" `Quick test_keep_bytes_key;
      Alcotest.test_case "reject: keep_bytes on case" `Quick
        test_keep_bytes_case;
      Alcotest.test_case "reject: DER in binary record" `Quick
        test_der_in_binary_record;
      Alcotest.test_case "reject: DER under binary wrapper" `Quick
        test_der_under_binary_wrapper;
      Alcotest.test_case "reject: bounded_integer empty" `Quick
        test_bounded_integer_empty;
      Alcotest.test_case "reject: enumerated" `Quick test_enumerated_rejects;
      Alcotest.test_case "reject: bounded_integer magnitude" `Quick
        test_bounded_integer_magnitude;
      Alcotest.test_case "reject: enumerated magnitude" `Quick
        test_enumerated_magnitude;
      Alcotest.test_case "reject: DEFAULT without literal" `Quick
        test_default_without_literal;
      Alcotest.test_case "reject: defined_by empty" `Quick test_defined_by_empty;
      Alcotest.test_case "reject: key without literal" `Quick
        test_key_without_literal;
      Alcotest.test_case "reject: time or IRI key not DER" `Quick
        test_key_time_not_der;
      Alcotest.test_case "reject: foreign field" `Quick
        test_sequence_foreign_field;
      Alcotest.test_case "reject: repeated component" `Quick
        test_sequence_repeated_name;
      Alcotest.test_case "reject: refined component" `Quick
        test_field_refined_component;
    ] )
