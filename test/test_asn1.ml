(* Tests for asn1.ml: the ASN.1 combinators, object identifiers, and every
   shape docs/asn1.md ("Refused when built") says the combinators refuse. *)

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
      Asn1.default "s" "ab" (Asn1.size ~min:1 ~max:2 Asn1.utf8_string))

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
type fields = Fields : 'f * ('f, unit) Codec.fields -> fields

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
  rejects ~msg:"DEFAULT on a constructed type" (fun () ->
      Asn1.default "d" [] (Asn1.sequence_of Asn1.integer))

let test_default_on_any () =
  rejects ~msg:"DEFAULT on a constructed type or ANY" (fun () ->
      Asn1.default "d" "\x05\x00" Asn1.any)

let test_default_on_oid () =
  rejects ~msg:"DEFAULT on an OBJECT IDENTIFIER" (fun () ->
      Asn1.default "d" rsa Asn1.oid)

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
      Asn1.explicit ~cls:`Universal 0 Asn1.bool)

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

let test_not_implemented () =
  rejects ~msg:"lane 3" (fun () -> Asn1.write ~outdir:"." [])

(* -- The DER codec -- *)

let tuple3 a b c =
  Codec.
    [
      (a $ fun (a, _, _) -> a);
      (b $ fun (_, b, _) -> b);
      (c $ fun (_, _, c) -> c);
    ]

let tuple4 a b c d =
  Codec.
    [
      (a $ fun (a, _, _, _) -> a);
      (b $ fun (_, b, _, _) -> b);
      (c $ fun (_, _, c, _) -> c);
      (d $ fun (_, _, _, d) -> d);
    ]

let mk3 a b c = (a, b, c)
let mk4 a b c d = (a, b, c, d)

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Fmt.str "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)))

(* The verdict of [c] on the whole of [s]: [decode] of exactly [s] answers
   it, and [validate] and [View.check] answer the same on bytes [decode]
   consumes in full. Nothing raises. *)
let verdict (type a) (c : a Codec.t) s =
  let b = Bytes.of_string s in
  let decoded = Codec.decode ~consume:`All c b 0 in
  let module V = (val Codec.View.codec c) in
  let (Buf.Any vb) = Buf.v b in
  let checked = Codec.View.ok (Codec.View.check V.codec vb (Buf.length vb) 0) in
  let validated = Result.is_ok (Codec.validate c b 0) in
  let consumed =
    match Codec.size_at c b 0 with
    | Ok n -> n = String.length s
    | Error _ -> false
  in
  if consumed && (Result.is_ok decoded <> validated || validated <> checked)
  then Alcotest.failf "%s: decode, validate and View.check disagree" (to_hex s);
  decoded

(* [c] accepts [s], and writes the value it reads back as [s]. *)
let accepts ?(msg = "") c s =
  match verdict c s with
  | Error e -> Alcotest.failf "%s %s: refused (%a)" msg (to_hex s) Error.pp e
  | Ok v ->
      Alcotest.(check string)
        (msg ^ " re-encodes") (to_hex s)
        (to_hex (Codec.to_string c v))

let refuses ?(msg = "") c s =
  match verdict c s with
  | Ok _ -> Alcotest.failf "%s %s: accepted" msg (to_hex s)
  | Error _ -> ()

let writes ?(msg = "") c v hex =
  Alcotest.(check string) (msg ^ " writes") hex (to_hex (Codec.to_string c v))

let rejects_write ~msg c v = rejects ~msg (fun () -> Codec.to_string c v)

(* One row per DER rule of docs/asn1.md, "DER rules and who enforces them":
   an accepted and a refused vector, each with its X.690 clause. *)

let octets = Asn1.codec Asn1.octet_string

(* X.690 10.1: definite length, in the fewest octets. *)
let test_rule_length () =
  accepts octets ("\x04\x81\x80" ^ String.make 128 'a');
  accepts octets "\x04\x02\xab\xcd";
  refuses octets "\x04\x81\x02\xab\xcd";
  refuses octets "\x04\x82\x00\x02\xab\xcd";
  refuses octets "\x24\x80\x04\x01\xaa\x00\x00"

(* X.690 8.1.2.4.2 c: a tag number in the fewest octets, the long form only
   from 31. *)
let test_rule_tag () =
  let high = Asn1.codec (Asn1.implicit 100 Asn1.integer) in
  accepts high (of_hex "9f640105");
  refuses high (of_hex "9f80640105");
  let low = Asn1.codec (Asn1.implicit 5 Asn1.null) in
  accepts low (of_hex "8500");
  refuses low (of_hex "9f0500")

(* X.690 10.2: a string is primitive. *)
let test_rule_primitive_strings () =
  accepts octets (of_hex "0402abcd");
  refuses octets (of_hex "24060401ab0401cd");
  let utf8 = Asn1.codec Asn1.utf8_string in
  accepts utf8 (of_hex "0c026869");
  refuses utf8 (of_hex "2c060c01680c0169")

(* X.690 8.3.2: an INTEGER in the fewest octets. *)
let test_rule_integer () =
  let int = Asn1.codec Asn1.integer in
  accepts int (of_hex "02020080");
  accepts int (of_hex "0202ff7f");
  refuses int (of_hex "02020001");
  refuses int (of_hex "0202ff80");
  refuses int (of_hex "0200");
  rejects_write ~msg:"not minimal" int "\x00\x01"

(* X.690 11.1: TRUE is 0xFF. *)
let test_rule_boolean () =
  let bool = Asn1.codec Asn1.bool in
  accepts bool (of_hex "0101ff");
  accepts bool (of_hex "010100");
  refuses bool (of_hex "010101");
  refuses bool (of_hex "0102ffff")

(* X.690 11.2.1: at most 7 unused bits, each zero. *)
let test_rule_bit_string () =
  let bits = Asn1.codec Asn1.bit_string in
  accepts bits (of_hex "03020780");
  accepts bits (of_hex "030100");
  refuses bits (of_hex "03020781");
  refuses bits (of_hex "03020880");
  refuses bits (of_hex "030107");
  rejects_write ~msg:"are not zero" bits (1, "\x01")

(* X.690 11.5: a DEFAULT component equal to its default is absent. *)
let seq_ab =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.default "b" false Asn1.bool in
  Asn1.codec (Asn1.sequence "Seq" mk (pair a b))

let test_rule_default () =
  accepts seq_ab (of_hex "30060201010101ff");
  accepts seq_ab (of_hex "3003020101");
  refuses seq_ab (of_hex "3006020101010100");
  writes seq_ab ("\x01", false) "3003020101";
  writes seq_ab ("\x01", true) "30060201010101ff"

(* X.690 10.3: the components of a SET in the canonical order of their tags. *)
let set_ab =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.integer) in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.codec (Asn1.set "Set" mk (pair a c))

let test_rule_set () =
  accepts set_ab (of_hex "31070402abcd800105");
  refuses set_ab (of_hex "31078001050402abcd");
  refuses set_ab (of_hex "31040402abcd");
  writes set_ab ("\x05", "\xab\xcd") "31070402abcd800105"

(* X.690 11.6: the elements of a SET OF sorted by encoding. *)
let set_of_octets = Asn1.codec (Asn1.set_of Asn1.octet_string)

let test_rule_set_of () =
  accepts set_of_octets (of_hex "31060401aa0401bb");
  accepts set_of_octets (of_hex "310504000401aa");
  refuses set_of_octets (of_hex "31060401bb0401aa");
  refuses set_of_octets (of_hex "31050401aa0400");
  writes set_of_octets [ "\xbb"; "\xaa" ] "31060401aa0401bb"

(* X.690 8.19.2: each subidentifier in the fewest octets; everparse-asn1 reads
   each below 2^32 (spec/ASN1.Spec.Content.OIDU32.fst). *)
let test_rule_oid () =
  let oid_c = Asn1.codec Asn1.oid in
  accepts oid_c (of_hex "06032a8648");
  refuses oid_c (of_hex "06042a808648");
  refuses oid_c (of_hex "0600");
  refuses oid_c (of_hex "06022a86");
  (* 1.2.4294967295, 2^32 - 1, read as a UInt32 on every target (Q7). *)
  let max = of_hex "06062a8fffffff7f" in
  accepts oid_c max;
  (match Codec.decode oid_c (Bytes.of_string max) 0 with
  | Ok o ->
      Alcotest.(check string) "2^32 - 1" "1.2.4294967295" (Asn1.Oid.to_string o)
  | Error e -> Alcotest.failf "%a" Error.pp e);
  refuses oid_c (of_hex "06072a9080808000")

(* -- Other constructs -- *)

let test_strings () =
  let c t = Asn1.codec t in
  accepts (c Asn1.printable_string) "\x13\x03a'?";
  refuses (c Asn1.printable_string) "\x13\x01*";
  accepts (c Asn1.ia5_string) "\x16\x01\x7f";
  refuses (c Asn1.ia5_string) "\x16\x01\x80";
  accepts (c Asn1.numeric_string) "\x12\x02 1";
  refuses (c Asn1.numeric_string) "\x12\x01a";
  accepts (c Asn1.visible_string) "\x1a\x01~";
  refuses (c Asn1.visible_string) "\x1a\x01\x7f";
  accepts (c Asn1.utf8_string) "\x0c\x03\xe2\x82\xac";
  refuses (c Asn1.utf8_string) "\x0c\x02\xc0\xaf";
  accepts (c Asn1.bmp_string) "\x1e\x02\x00a";
  refuses (c Asn1.bmp_string) "\x1e\x02\xd8\x00";
  accepts (c Asn1.universal_string) "\x1c\x04\x00\x10\xff\xff";
  refuses (c Asn1.universal_string) "\x1c\x04\x00\x11\x00\x00";
  accepts (c Asn1.teletex_string) "\x14\x01\xff";
  accepts (c Asn1.utc_time) "\x17\x0d991231235960Z";
  refuses (c Asn1.utc_time) "\x17\x0d990230000000Z";
  accepts (c Asn1.generalized_time) "\x18\x1120000229000000.5Z";
  refuses (c Asn1.generalized_time) "\x18\x1120000229000000.0Z";
  rejects_write ~msg:"outside the alphabet" (c Asn1.printable_string) "*"

let test_size () =
  let c = Asn1.codec (Asn1.size ~min:2 ~max:3 Asn1.utf8_string) in
  accepts c "\x0c\x04\xe2\x82\xaca";
  refuses c "\x0c\x03\xe2\x82\xac";
  rejects_write ~msg:"outside SIZE" c "a"

let test_bounded_and_enumerated () =
  let byte = Asn1.codec (Asn1.bounded_integer ~min:0L ~max:255L) in
  accepts byte (of_hex "020200ff");
  refuses byte (of_hex "02020100");
  refuses byte (of_hex "0209010000000000000000");
  rejects_write ~msg:"outside INTEGER" byte 256L;
  let enum = Asn1.codec (Asn1.enumerated [ ("a", 0); ("c", 5) ]) in
  accepts enum (of_hex "0a0105");
  refuses enum (of_hex "0a0101");
  rejects_write ~msg:"not a value" enum 1

type ch = I of string | B of bool | O of string

let choice_c =
  Asn1.choice
    [
      Asn1.alternative "i" Asn1.integer
        ~inject:(fun s -> I s)
        ~project:(function I s -> Some s | _ -> None);
      Asn1.alternative "b" Asn1.bool
        ~inject:(fun b -> B b)
        ~project:(function B b -> Some b | _ -> None);
      Asn1.alternative "o"
        (Asn1.implicit 0 Asn1.octet_string)
        ~inject:(fun s -> O s)
        ~project:(function O s -> Some s | _ -> None);
    ]

let test_choice_and_optional () =
  let c = Asn1.codec choice_c in
  accepts c (of_hex "020105");
  accepts c (of_hex "8002abcd");
  refuses c (of_hex "0400");
  refuses c (of_hex "a0020400");
  let opt =
    let a = Asn1.optional "a" Asn1.bool in
    let b = Asn1.optional "b" (Asn1.explicit 1 Asn1.integer) in
    Asn1.codec (Asn1.sequence "Opt" mk (pair a b))
  in
  accepts opt (of_hex "3000");
  accepts opt (of_hex "3005a103020105");
  accepts opt (of_hex "30080101ffa103020105");
  refuses opt (of_hex "30050201050101");
  refuses opt (of_hex "3008a1030201050101ff")

let test_keep_bytes () =
  let a = Asn1.required "a" (Asn1.keep_bytes Asn1.integer) in
  let b = Asn1.required "b" Asn1.bool in
  let c = Asn1.codec (Asn1.sequence "K" mk (pair a b)) in
  match Codec.decode c (Bytes.of_string (of_hex "30060201050101ff")) 0 with
  | Ok ((v, bytes), true) ->
      Alcotest.(check string) "value" "\x05" v;
      Alcotest.(check string) "bytes" "020105" (to_hex bytes);
      writes c (("\x05", "ignored"), true) "30060201050101ff"
  | Ok _ -> Alcotest.fail "wrong value"
  | Error e -> Alcotest.failf "%a" Error.pp e

(* The running example: AlgorithmIdentifier keyed by its OID. *)
let test_defined_by () =
  let c = Asn1.codec (alg_id ()) in
  let rsa_null = of_hex "300d06092a864886f70d0101010500" in
  accepts c rsa_null;
  let ed = of_hex "300506032b6570" in
  accepts c ed;
  (match Codec.decode c (Bytes.of_string ed) 0 with
  | Ok { params = Absent; _ } -> ()
  | _ -> Alcotest.fail "id-Ed25519 has no parameters");
  (* rsaEncryption with no NULL, Ed25519 with one, an unknown key. *)
  refuses c (of_hex "300b06092a864886f70d010101");
  refuses c (of_hex "300706032b65700500");
  refuses c (of_hex "300606022a030500");
  rejects_write ~msg:"not the case of its key" c { oid = rsa; params = Absent };
  rejects_write ~msg:"no case" c
    { oid = Asn1.Oid.(base 1 2 <| 3); params = Null }

(* A key with no case falls back to the declared type, OPTIONAL here, as
   everparse-asn1's AlgId vector schema does (DiffAny.asn). *)
type any_params = Null_123 | Other of Asn1.Oid.t * string option

let alg_123 = Asn1.Oid.(base 1 2 <| 3)

let any_alg_id =
  let algorithm = Asn1.required "algorithm" Asn1.oid in
  let parameters =
    Asn1.defined_by "parameters" ~on:algorithm
      Asn1.
        [
          case alg_123 null
            ~inject:(fun () -> Null_123)
            ~project:(function Null_123 -> Some () | Other _ -> None);
          fallback ~optional:true any
            ~inject:(fun k p -> Other (k, p))
            ~project:(function Other (k, p) -> Some (k, p) | Null_123 -> None);
        ]
  in
  Asn1.sequence "AlgId" mk (pair algorithm parameters)

let test_fallback () =
  let c = Asn1.codec any_alg_id in
  accepts c (of_hex "300606022a030500");
  accepts c (of_hex "300706022a04020105");
  accepts c (of_hex "300406022a04");
  refuses c (of_hex "300406022a03");
  refuses c (of_hex "300706022a03020105");
  rejects_write ~msg:"carries another key" c
    (Asn1.Oid.(base 1 2 <| 4), Other (alg_123, None))

(* -- The running example on real certificates -- *)

(* ocaml-x509's tests/testcertificates/cacert.pem (RSA) and fido.pem (EC on
   prime256v1), base64-decoded. *)
let cacert_der =
  of_hex
    (String.concat ""
       [
         "3082026b308201d4a003020102020900cedcd419548ce92c300d06092a864886";
         "f70d01010505003045310b30090603550406130241553113301106035504080c";
         "0a536f6d652d53746174653121301f060355040a0c18496e7465726e65742057";
         "69646769747320507479204c7464301e170d3134303630363132353632355a17";
         "0d3234303630333132353632355a3045310b3009060355040613024155311330";
         "1106035504080c0a536f6d652d53746174653121301f060355040a0c18496e74";
         "65726e6574205769646769747320507479204c746430819f300d06092a864886";
         "f70d010101050003818d0030818902818100e761b407ee916618eeaa4e3e2ca4";
         "0599ca376dfe5609ffcc72e2808f94a6a6c4e911fdc8cdef50a0b9ea83d394f2";
         "1ddde3d224f6aafc735382303c894fa2a2bdfa0cc5c9b9f4bbcae33f8528b91a";
         "9c192f70f993d83d6f10af05a07f8a4600b81695b4a6f159184ba67540042636";
         "b600bdeea9ba23645efdcd9b06b556223b010203010001a3633061301d060355";
         "1d0e041604149f73106142114d82caf0ff37e917c320c8db1e4a301f0603551d";
         "230418301680149f73106142114d82caf0ff37e917c320c8db1e4a3012060355";
         "1d130101ff040830060101ff020164300b0603551d0f040403020106300d0609";
         "2a864886f70d0101050500038181005f4a9c403af60f0eaa26431565952676b9";
         "c66749e99981bb98f1f820de39875212361a6c2c0c58d247cf17f81cc06991cf";
         "389f527019e21b83b9d52b39656f560ed68b9384d4917683c9c553357fcf40ac";
         "d6d5047121605a73fc6bc08d7d7397ab509277d21cb5fa0b27b0bf7bdbceab0f";
         "a7e4c4fce5309b1ebfc86f6798cda4";
       ])

let fido_der =
  of_hex
    (String.concat ""
       [
         "308201673082010da003020102020900d79549bd1a671751300a06082a8648ce";
         "3d04030230173115301306035504030c0c4654204649444f2030323030302017";
         "0d3139303232363030303030305a180f32303334303232353233353935395a30";
         "21311f301d06035504030c164654204649444f20503230343730303133343134";
         "31323059301306072a8648ce3d020106082a8648ce3d03010703420004d8465a";
         "43a2aca53fa6335eb58453e2bdde5e8ebbee53704dedeb9aa550dd56b793cc0a";
         "9dcf1f99ad7b138fa3c49d45f18b6fc60d274b93b41f9d0b20e7e9f800a33630";
         "34301d0603551d0e04160414525ea96c47b9a47933a09b4871c398df6407aaa4";
         "3013060b2b0601040182e51c020101040403020470300a06082a8648ce3d0403";
         "020348003045022100a7d0ff5e731f86a001509a09e345c233632897a45f4ff0";
         "a5a4f7f8e0abf51a7102204217fdbceeb8610a744d97ae4e80665058f1b17f8e";
         "c3ffb32623879ae178322b";
       ])

let decode_exn c s =
  match Codec.decode ~consume:`All c (Bytes.of_string s) 0 with
  | Ok v -> v
  | Error e -> Alcotest.failf "decode: %a" Error.pp e

(* A certificate reads, and writes back as the same bytes. Its
   SubjectPublicKeyInfo, kept as bytes, is what the docs/asn1.md codec writes
   for the value read. *)
let test_certificate name der () =
  let cert = decode_exn Rfc5280.certificate_codec der in
  Alcotest.(check string)
    (name ^ " re-encodes") (to_hex der)
    (to_hex (Codec.to_string Rfc5280.certificate_codec cert));
  let tbs, tbs_bytes = cert.Rfc5280.tbs_certificate in
  Alcotest.(check string)
    (name ^ " tbs bytes in place")
    (to_hex tbs_bytes)
    (to_hex (String.sub der 4 (String.length tbs_bytes)));
  let spki_value, spki_bytes = tbs.Rfc5280.subject_public_key_info in
  let doc_spki = Asn1.codec (spki ()) in
  let value = decode_exn doc_spki spki_bytes in
  Alcotest.(check string)
    (name ^ " SPKI") (to_hex spki_bytes)
    (to_hex (Codec.to_string doc_spki value));
  Alcotest.(check string)
    (name ^ " SPKI, both grammars")
    (to_hex spki_bytes)
    (to_hex (Codec.to_string Rfc5280.spki_codec spki_value));
  let again = decode_exn doc_spki (Codec.to_string doc_spki value) in
  Alcotest.(check bool) (name ^ " SPKI round trip") true (again = value)

let test_spki_params () =
  let params der =
    let cert = decode_exn Rfc5280.certificate_codec der in
    let tbs = fst cert.Rfc5280.tbs_certificate in
    (fst tbs.Rfc5280.subject_public_key_info).Rfc5280.alg.Rfc5280.params
  in
  (match params cacert_der with
  | Rfc5280.Null -> ()
  | _ -> Alcotest.fail "rsaEncryption parameters are NULL");
  match params fido_der with
  | Rfc5280.Curve c ->
      Alcotest.(check string)
        "prime256v1" "1.2.840.10045.3.1.7" (Asn1.Oid.to_string c)
  | _ -> Alcotest.fail "id-ecPublicKey parameters are a curve"

let test_ed25519_round_trip () =
  let c = Asn1.codec (spki ()) in
  let v =
    {
      alg = { oid = ed25519; params = Absent };
      key = (0, String.make 32 '\x42');
    }
  in
  let s = Codec.to_string c v in
  Alcotest.(check bool) "round trip" true (decode_exn c s = v)

(* Every cut of a certificate is an [Error]: nothing raises. *)
let test_truncations () =
  let b = Bytes.of_string cacert_der in
  for n = 0 to Bytes.length b - 1 do
    let cut = Bytes.sub b 0 n in
    match Codec.decode Rfc5280.certificate_codec cut 0 with
    | Ok _ -> Alcotest.failf "a certificate cut at %d decodes" n
    | Error _ -> ()
  done;
  for i = 0 to Bytes.length b - 1 do
    let flipped = Bytes.copy b in
    Bytes.set flipped i (Char.chr (Char.code (Bytes.get b i) lxor 0xFF));
    (match Codec.decode Rfc5280.certificate_codec flipped 0 with
    | Ok _ | Error _ -> ());
    match Codec.validate Rfc5280.certificate_codec flipped 0 with
    | Ok () | Error _ -> ()
  done

(* -- fold -- *)

let ints_record =
  let a = Asn1.required "a" Asn1.bool in
  let l = Asn1.required "l" (Asn1.sequence_of Asn1.integer) in
  let s = Asn1.required "s" (Asn1.implicit 0 (Asn1.set_of Asn1.integer)) in
  let m =
    Asn1.required "m"
      (Asn1.map ~decode:(fun s -> [ s ]) ~encode:List.hd Asn1.octet_string)
  in
  let a = Codec.(a $ fun (a, _, _, _) -> a) in
  let l = Codec.(l $ fun (_, l, _, _) -> l) in
  let s = Codec.(s $ fun (_, _, s, _) -> s) in
  let m = Codec.(m $ fun (_, _, _, m) -> m) in
  (Asn1.codec (Asn1.sequence "Ints" mk4 Codec.[ a; l; s; m ]), a, l, s, m)

let ints = (true, [ "\x01"; "\x02"; "\x03" ], [ "\x05"; "\x04" ], [ "m" ])

(* A component read in place, through Codec.get and View.checked_field. *)
let test_checked_field () =
  let c, a, l, s, m = ints_record in
  let bytes = Bytes.of_string (Codec.to_string c ints) in
  Alcotest.(check (result bool reject))
    "get a" (Ok true) (Codec.get c a bytes 0);
  (match Codec.get c s bytes 0 with
  | Ok v -> Alcotest.(check (list string)) "get s" [ "\x04"; "\x05" ] v
  | Error e -> Alcotest.failf "get s: %a" Error.pp e);
  (match Codec.get c m bytes 0 with
  | Ok v -> Alcotest.(check (list string)) "get m" [ "m" ] v
  | Error e -> Alcotest.failf "get m: %a" Error.pp e);
  let module V = (val Codec.View.codec c) in
  let read_l = Codec.View.checked_field V.codec l in
  let (Buf.Any b) = Buf.v bytes in
  let v = Codec.View.get (Codec.View.check V.codec b (Buf.length b) 0) in
  (match Codec.View.read read_l b v with
  | Ok l -> Alcotest.(check int) "checked_field l" 3 (List.length l)
  | Error e -> Alcotest.failf "checked_field l: %a" Error.pp e);
  (* Bytes written after the check: the read answers their error. *)
  Bytes.set bytes 4 '\x02';
  Alcotest.(check bool)
    "written after" true
    (Result.is_error (Codec.View.read read_l b v));
  rejects ~msg:"sets no field in place" (fun () -> Codec.set c a bytes 0 false);
  let other = Asn1.required "other" Asn1.bool in
  rejects ~msg:"does not belong to codec" (fun () ->
      Codec.get c Codec.(other $ fun _ -> true))

let test_fold () =
  let c, _, l, s, m = ints_record in
  let module V = (val Codec.View.codec c) in
  let (Buf.Any b) = Buf.v (Bytes.of_string (Codec.to_string c ints)) in
  let k = Codec.View.check V.codec b (Buf.length b) 0 in
  Alcotest.(check bool) "checked" true (Codec.View.ok k);
  let v = Codec.View.get k in
  let sum =
    Asn1.fold V.codec l b v ~init:0 ~f:(fun acc x -> acc + Char.code x.[0])
  in
  Alcotest.(check int) "SEQUENCE OF" 6 sum;
  let order =
    Asn1.fold V.codec s b v ~init:[] ~f:(fun acc x -> Char.code x.[0] :: acc)
  in
  Alcotest.(check (list int)) "SET OF in DER order" [ 5; 4 ] order;
  rejects ~msg:"is not a SEQUENCE OF or SET OF" (fun () -> Asn1.fold V.codec m)

(* -- Nesting -- *)

(* DER in binary: the DER validator runs on exactly the region and consumes
   all of it. *)
let framed_spki () =
  let len = Field.v "len" uint16be in
  let spki =
    Field.v "spki" (nested ~size:(Field.ref len) (codec (Asn1.codec (spki ()))))
  in
  Codec.v "Framed" mk Codec.[ len $ fst; spki $ snd ]

let test_der_in_binary () =
  let c = framed_spki () in
  let spki_bytes =
    Codec.to_string
      (Asn1.codec (spki ()))
      { alg = { oid = ed25519; params = Absent }; key = (0, "k") }
  in
  let n = String.length spki_bytes in
  let frame len body =
    let b = Bytes.create (2 + String.length body) in
    Bytes.set_uint16_be b 0 len;
    Bytes.blit_string body 0 b 2 (String.length body);
    b
  in
  (match Codec.decode c (frame n spki_bytes) 0 with
  | Ok (_, v) -> Alcotest.(check string) "key" "k" (snd v.key)
  | Error e -> Alcotest.failf "framed SPKI: %a" Error.pp e);
  Alcotest.(check bool)
    "validate" true
    (Result.is_ok (Codec.validate c (frame n spki_bytes) 0));
  (* A bad inner region: BIT STRING with 8 unused bits. *)
  let bad = Bytes.of_string spki_bytes in
  Bytes.set bad (n - 2) '\x08';
  Alcotest.(check bool)
    "bad inner refused" true
    (Result.is_error (Codec.validate c (frame n (Bytes.to_string bad)) 0));
  Alcotest.(check bool)
    "bad inner decode refused" true
    (Result.is_error (Codec.decode c (frame n (Bytes.to_string bad)) 0));
  (* A region one byte longer than the DER value. *)
  Alcotest.(check bool)
    "region not consumed" true
    (Result.is_error (Codec.validate c (frame (n + 1) (spki_bytes ^ "\x00")) 0));
  (* A region that cuts the DER value. *)
  Alcotest.(check bool)
    "region cut" true
    (Result.is_error (Codec.validate c (frame (n - 1) spki_bytes) 0))

let test_der_outside_nested () =
  let der = codec (Asn1.codec Asn1.bool) in
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "Bare" Fun.id Codec.[ Field.v "b" der $ Fun.id ]);
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "AtMost" Fun.id
        Codec.[ Field.v "b" (nested_at_most ~size:(int 3) der) $ Fun.id ]);
  rejects ~msg:"holds a DER codec outside nested ~size" (fun () ->
      Codec.v "Opt" Fun.id
        Codec.[ Field.optional "b" ~present:(bool true) der $ Fun.id ]);
  builds (fun () ->
      Codec.v "Nested" Fun.id
        Codec.[ Field.v "b" (nested ~size:(int 3) der) $ Fun.id ])

(* Binary in DER: an OCTET STRING whose contents the binary codec checks, in
   full. *)
let hdr =
  let version =
    Field.v "version" ~self_constraint:Expr.(fun v -> v = int 1) uint8
  in
  let length = Field.v "length" uint16be in
  Codec.v "Hdr" mk Codec.[ version $ fst; length $ snd ]

let tagged_hdr () =
  let label = Asn1.required "name" Asn1.utf8_string in
  let h = Asn1.required "hdr" (Asn1.containing (codec hdr)) in
  let n = Asn1.optional "n" (Asn1.containing uint16be) in
  Asn1.codec (Asn1.sequence "Tagged" mk3 (tuple3 label h n))

let test_binary_in_der () =
  let c = tagged_hdr () in
  let v = ("x", (UInt8.v 1, UInt16.v 7), Some (UInt16.v 9)) in
  let s = Codec.to_string c v in
  Alcotest.(check string) "bytes" "300c0c0178040301000704020009" (to_hex s);
  accepts c s;
  (* version 2 breaks the binary codec's constraint *)
  refuses c (of_hex "300c0c0178040302000704020009");
  (* a byte after the binary record inside the OCTET STRING *)
  refuses c (of_hex "300d0c017804040100070004020009");
  (* a cut binary record *)
  refuses c (of_hex "300b0c01780402010004020009");
  rejects_write ~msg:"rejected by its own constraints" c
    ("x", (UInt8.v 2, UInt16.v 7), None)

(* -- EverParse's vectors -- *)

type runner = Run : 'a Codec.t -> runner

let diff_seq =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.default "b" false Asn1.bool in
  let c = Asn1.optional "c" (Asn1.implicit 0 Asn1.octet_string) in
  let d = Asn1.optional "d" (Asn1.explicit 1 Asn1.integer) in
  Asn1.sequence "Seq" mk4 (tuple4 a b c d)

let int_seq = Asn1.sequence_of Asn1.integer

let nested_seq =
  let s = Asn1.required "s" diff_seq in
  let l = Asn1.required "l" int_seq in
  let ch = Asn1.optional "ch" choice_c in
  Asn1.sequence "Nested" mk3 (tuple3 s l ch)

let diff_set =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.integer) in
  let b = Asn1.optional "b" (Asn1.implicit 1 Asn1.bool) in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.set "Set" mk3 (tuple3 a b c)

let smoke =
  let a = Asn1.required "a" Asn1.integer in
  let b = Asn1.optional "b" Asn1.bool in
  let c = Asn1.required "c" Asn1.octet_string in
  Asn1.sequence "Smoke" mk3 (tuple3 a b c)

(* The universal types X.680 defines as a tag on another, with no content
   rule of their own (spec/ASN1.Spec.Interpreter.fst:120-125). *)
let universal_octets n = Asn1.implicit ~cls:`Universal n Asn1.octet_string

(* EXTERNAL, EMBEDDED PDV and CHARACTER STRING as spec/ASN1.UniversalTypes.fst
   writes them. *)
type encoding =
  | Single of string
  | Octet_aligned of string
  | Arbitrary of (int * string)

let external_ =
  let encoding =
    Asn1.choice
      [
        Asn1.alternative "single-ASN1-type" (Asn1.explicit 0 Asn1.any)
          ~inject:(fun s -> Single s)
          ~project:(function Single s -> Some s | _ -> None);
        Asn1.alternative "octet-aligned"
          (Asn1.implicit 1 Asn1.octet_string)
          ~inject:(fun s -> Octet_aligned s)
          ~project:(function Octet_aligned s -> Some s | _ -> None);
        Asn1.alternative "arbitrary"
          (Asn1.implicit 2 Asn1.bit_string)
          ~inject:(fun b -> Arbitrary b)
          ~project:(function Arbitrary b -> Some b | _ -> None);
      ]
  in
  let direct = Asn1.optional "direct-reference" Asn1.oid in
  let indirect = Asn1.optional "indirect-reference" Asn1.integer in
  let descriptor = Asn1.optional "data-value-descriptor" (universal_octets 7) in
  let encoding = Asn1.required "encoding" encoding in
  Asn1.implicit ~cls:`Universal 8
    (Asn1.sequence "EXTERNAL" mk4 (tuple4 direct indirect descriptor encoding))

type identification =
  | Syntaxes of (Asn1.Oid.t * Asn1.Oid.t)
  | Syntax of Asn1.Oid.t
  | Context_id of string
  | Negotiation of (string * Asn1.Oid.t)
  | Transfer of Asn1.Oid.t
  | Fixed

let identification =
  let two name na ta nb tb =
    let a = Asn1.required na ta and b = Asn1.required nb tb in
    Asn1.sequence name mk (pair a b)
  in
  let alt name t inject project = Asn1.alternative name t ~inject ~project in
  Asn1.choice
    [
      alt "syntaxes"
        (Asn1.implicit 0
           (two "syntaxes" "abstract" (Asn1.implicit 0 Asn1.oid) "transfer"
              (Asn1.implicit 1 Asn1.oid)))
        (fun x -> Syntaxes x)
        (function Syntaxes x -> Some x | _ -> None);
      alt "syntax" (Asn1.implicit 1 Asn1.oid)
        (fun x -> Syntax x)
        (function Syntax x -> Some x | _ -> None);
      alt "presentation-context-id"
        (Asn1.implicit 2 Asn1.integer)
        (fun x -> Context_id x)
        (function Context_id x -> Some x | _ -> None);
      alt "context-negotiation"
        (Asn1.implicit 3
           (two "context-negotiation" "presentation-context-id"
              (Asn1.implicit 0 Asn1.integer)
              "transfer-syntax" (Asn1.implicit 1 Asn1.oid)))
        (fun x -> Negotiation x)
        (function Negotiation x -> Some x | _ -> None);
      alt "transfer-syntax" (Asn1.implicit 4 Asn1.oid)
        (fun x -> Transfer x)
        (function Transfer x -> Some x | _ -> None);
      alt "fixed"
        (Asn1.implicit 5 Asn1.null)
        (fun () -> Fixed)
        (function Fixed -> Some () | _ -> None);
    ]

let presentation n name value =
  let id = Asn1.required "identification" (Asn1.explicit 0 identification) in
  let value = Asn1.required value (Asn1.implicit 2 Asn1.octet_string) in
  Asn1.implicit ~cls:`Universal n (Asn1.sequence name mk (pair id value))

let runners =
  let c t = Some (Run (Asn1.codec t)) in
  [
    ("DiffPrim.Bool", c Asn1.bool);
    ("DiffPrim.Int", c Asn1.integer);
    ("DiffPrim.Enum", c (Asn1.enumerated [ ("a", 0); ("b", 1); ("c", 5) ]));
    ("DiffPrim.Bits", c Asn1.bit_string);
    ("DiffPrim.Octets", c Asn1.octet_string);
    ("DiffPrim.Null", c Asn1.null);
    ("DiffPrim.Oid", c Asn1.oid);
    ("DiffDesc.Desc", c (universal_octets 7));
    ("DiffConstraints.Sized", c (Asn1.size ~min:2 ~max:3 Asn1.octet_string));
    ("DiffConstraints.Byte", c (Asn1.bounded_integer ~min:0L ~max:255L));
    ( "DiffConstraints.NonEmptySeqOf",
      c (Asn1.sequence_of ~non_empty:true Asn1.integer) );
    ( "DiffConstraints.NonEmptySetOf",
      c (Asn1.set_of ~non_empty:true Asn1.integer) );
    ("DiffConstructed.Seq", c diff_seq);
    ("DiffConstructed.IntSeq", c int_seq);
    ("DiffConstructed.OctetsSet", c (Asn1.set_of Asn1.octet_string));
    ("DiffConstructed.Choice", c choice_c);
    ("DiffConstructed.App", c (Asn1.implicit ~cls:`Application 5 Asn1.integer));
    ("DiffConstructed.PrivExp", c (Asn1.explicit ~cls:`Private 3 Asn1.bool));
    ("DiffConstructed.HighCtx", c (Asn1.implicit 100 Asn1.integer));
    ( "DiffConstructed.HighApp",
      c (Asn1.explicit ~cls:`Application 1000 Asn1.octet_string) );
    ("DiffConstructed.Nested", c nested_seq);
    ("DiffSet.Set", c diff_set);
    ("DiffSmoke.Smoke", c smoke);
    ("DiffAny.AnyT", c Asn1.any);
    ("DiffAny.AlgId", c any_alg_id);
    ("DiffExternal.Ext", c external_);
    ("DiffPdv.Pdv", c (presentation 11 "EMBEDDED PDV" "data-value"));
    ( "DiffCharStr.CharStr",
      c (presentation 29 "CHARACTER STRING" "string-value") );
    ("DiffStrings.Utf8", c Asn1.utf8_string);
    ("DiffStrings.Numeric", c Asn1.numeric_string);
    ("DiffStrings.Printable", c Asn1.printable_string);
    ("DiffStrings.Ia5", c Asn1.ia5_string);
    ("DiffStrings.Visible", c Asn1.visible_string);
    ("DiffStrings.Bmp", c Asn1.bmp_string);
    ("DiffStrings.Universal", c Asn1.universal_string);
    ("DiffStrings.Teletex", c Asn1.teletex_string);
    ("DiffVideotex.Videotex", c (universal_octets 21));
    ("DiffStrings2.Graphic", c (universal_octets 25));
    ("DiffStrings2.General", c (universal_octets 27));
    ("DiffTime.Utc", c Asn1.utc_time);
    ("DiffTime.Gen", c Asn1.generalized_time);
    (* No combinator reads these with their content rules: RELATIVE-OID,
       OID-IRI, RELATIVE-OID-IRI, DATE, TIME-OF-DAY, DATE-TIME. *)
    ("DiffRelOid.RelOid", None);
    ("DiffIRI.Iri", None);
    ("DiffIRI.RelIri", None);
    ("DiffTime2.Date", None);
    ("DiffTime2.Tod", None);
    ("DiffTime2.DateTime", None);
  ]

let test_vectors () =
  let run = ref 0 and skipped = ref 0 in
  List.iter
    (fun (v : Asn1_vectors.t) ->
      match List.assoc_opt v.typ runners with
      | None -> Alcotest.failf "%s: no runner for %s" v.id v.typ
      | Some None -> incr skipped
      | Some (Some (Run c)) ->
          incr run;
          let msg = Fmt.str "%a" Asn1_vectors.pp v in
          if v.accept then accepts ~msg c (of_hex v.hex)
          else refuses ~msg c (of_hex v.hex))
    Asn1_vectors.all;
  Alcotest.(check int) "vectors run" 289 !run;
  Alcotest.(check int) "vectors skipped" 36 !skipped

let suite =
  ( "asn1",
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
      Alcotest.test_case "reject: SIZE not a range" `Quick test_size_not_a_range;
      Alcotest.test_case "reject: SIZE reaches 65536" `Quick
        test_size_reaches_65536;
      Alcotest.test_case "reject: key OPTIONAL" `Quick test_key_optional;
      Alcotest.test_case "reject: key DEFAULT" `Quick test_key_default;
      Alcotest.test_case "reject: key constrained" `Quick test_key_constrained;
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
      Alcotest.test_case "reject: foreign field" `Quick
        test_sequence_foreign_field;
      Alcotest.test_case "reject: repeated component" `Quick
        test_sequence_repeated_name;
      Alcotest.test_case "reject: refined component" `Quick
        test_field_refined_component;
      Alcotest.test_case "not implemented names its lane" `Quick
        test_not_implemented;
      Alcotest.test_case "der: definite minimal length (X.690 10.1)" `Quick
        test_rule_length;
      Alcotest.test_case "der: minimal tag number (X.690 8.1.2)" `Quick
        test_rule_tag;
      Alcotest.test_case "der: primitive strings (X.690 10.2)" `Quick
        test_rule_primitive_strings;
      Alcotest.test_case "der: minimal INTEGER (X.690 8.3.2)" `Quick
        test_rule_integer;
      Alcotest.test_case "der: BOOLEAN 00/FF (X.690 11.1)" `Quick
        test_rule_boolean;
      Alcotest.test_case "der: BIT STRING padding (X.690 11.2.1)" `Quick
        test_rule_bit_string;
      Alcotest.test_case "der: DEFAULT omitted (X.690 11.5)" `Quick
        test_rule_default;
      Alcotest.test_case "der: SET in tag order (X.690 10.3)" `Quick
        test_rule_set;
      Alcotest.test_case "der: SET OF sorted (X.690 11.6)" `Quick
        test_rule_set_of;
      Alcotest.test_case "der: minimal OID subidentifiers (X.690 8.19)" `Quick
        test_rule_oid;
      Alcotest.test_case "der: character strings and times" `Quick test_strings;
      Alcotest.test_case "der: SIZE" `Quick test_size;
      Alcotest.test_case "der: bounded INTEGER and ENUMERATED" `Quick
        test_bounded_and_enumerated;
      Alcotest.test_case "der: CHOICE and OPTIONAL" `Quick
        test_choice_and_optional;
      Alcotest.test_case "der: keep_bytes" `Quick test_keep_bytes;
      Alcotest.test_case "der: defined_by" `Quick test_defined_by;
      Alcotest.test_case "der: defined_by fallback" `Quick test_fallback;
      Alcotest.test_case "spki: cacert.pem" `Quick
        (test_certificate "cacert" cacert_der);
      Alcotest.test_case "spki: fido.pem" `Quick
        (test_certificate "fido" fido_der);
      Alcotest.test_case "spki: parameters by key" `Quick test_spki_params;
      Alcotest.test_case "spki: Ed25519 round trip" `Quick
        test_ed25519_round_trip;
      Alcotest.test_case "der: no exception on cut or flipped bytes" `Quick
        test_truncations;
      Alcotest.test_case "fold" `Quick test_fold;
      Alcotest.test_case "checked_field" `Quick test_checked_field;
      Alcotest.test_case "nesting: DER in binary" `Quick test_der_in_binary;
      Alcotest.test_case "nesting: DER outside nested" `Quick
        test_der_outside_nested;
      Alcotest.test_case "nesting: binary in DER" `Quick test_binary_in_der;
      Alcotest.test_case "everparse vectors" `Quick test_vectors;
    ] )
