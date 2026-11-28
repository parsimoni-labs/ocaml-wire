(* ASN.1 types whose .asn modules the tests print and everparse-asn1 reads. *)

open Wire

let alt name t = Asn1.alternative name t ~inject:Fun.id ~project:Option.some

(* -- The running example of docs/asn1.md: RFC 5280 SubjectPublicKeyInfo -- *)

type params = Null | Curve of Asn1.Oid.t | Absent
type alg = { oid : Asn1.Oid.t; params : params }
type spki = { alg : alg; key : int * string }

let rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 1 ])
let ec = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 2; 1 ])
let ed25519 = Asn1.Oid.(base 1 3 <|| [ 101; 112 ])

let spki =
  let f_oid = Asn1.required "algorithm" Asn1.oid in
  let f_params =
    Asn1.(
      defined_by "parameters" ~on:f_oid
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
        ])
  in
  let alg_id =
    Asn1.sequence "AlgorithmIdentifier"
      (fun oid params -> { oid; params })
      Codec.[ (f_oid $ fun a -> a.oid); (f_params $ fun a -> a.params) ]
  in
  let f_alg = Asn1.required "algorithm" alg_id in
  let f_key = Asn1.required "subjectPublicKey" Asn1.bit_string in
  Asn1.sequence "SubjectPublicKeyInfo"
    (fun alg key -> { alg; key })
    Codec.[ (f_alg $ fun s -> s.alg); (f_key $ fun s -> s.key) ]

(* -- Every construct of docs/asn1.md, "Constructs and their projections" -- *)

type prims = {
  flag : bool;
  big : string;
  small : int64;
  colour : int;
  nothing : unit;
  id : Asn1.Oid.t;
  bits : int * string;
  bytes : string;
  mapped : int;
}

let prims =
  let flag = Asn1.required "flag" Asn1.bool in
  let big = Asn1.required "big" Asn1.integer in
  let small =
    Asn1.required "small" (Asn1.bounded_integer ~min:(-128L) ~max:127L)
  in
  let colour =
    Asn1.required "colour"
      (Asn1.enumerated [ ("red", 0); ("green", 1); ("blue", 2) ])
  in
  let nothing = Asn1.required "nothing" Asn1.null in
  let id = Asn1.required "id" Asn1.oid in
  let bits = Asn1.required "bits" Asn1.bit_string in
  let bytes = Asn1.required "bytes" Asn1.octet_string in
  let mapped =
    Asn1.required "mapped"
      (Asn1.map ~decode:String.length
         ~encode:(fun n -> String.make n 'x')
         Asn1.utf8_string)
  in
  Asn1.sequence "Prims"
    (fun flag big small colour nothing id bits bytes mapped ->
      { flag; big; small; colour; nothing; id; bits; bytes; mapped })
    Codec.
      [
        (flag $ fun r -> r.flag);
        (big $ fun r -> r.big);
        (small $ fun r -> r.small);
        (colour $ fun r -> r.colour);
        (nothing $ fun r -> r.nothing);
        (id $ fun r -> r.id);
        (bits $ fun r -> r.bits);
        (bytes $ fun r -> r.bytes);
        (mapped $ fun r -> r.mapped);
      ]

(* Every component is a string, so the record is the list of them. *)
let strings =
  let s = Asn1.required in
  let utf8 = s "utf8" Asn1.utf8_string in
  let printable = s "printable" Asn1.printable_string in
  let ia5 = s "ia5" Asn1.ia5_string in
  let numeric = s "numeric" Asn1.numeric_string in
  let visible = s "visible" Asn1.visible_string in
  let teletex = s "teletex" Asn1.teletex_string in
  let videotex = s "videotex" Asn1.videotex_string in
  let graphic = s "graphic" Asn1.graphic_string in
  let general = s "general" Asn1.general_string in
  let descriptor = s "descriptor" Asn1.object_descriptor in
  let bmp = s "bmp" Asn1.bmp_string in
  let universal = s "universal" Asn1.universal_string in
  let utc = s "utc" Asn1.utc_time in
  let generalized = s "generalized" Asn1.generalized_time in
  let sized = s "sized" (Asn1.size ~min:1 ~max:16 Asn1.utf8_string) in
  let octets = s "octets" (Asn1.size ~min:4 ~max:4 Asn1.octet_string) in
  let nth i l = List.nth l i in
  Asn1.sequence "Strings"
    (fun a b c d e f g h i j k l m n o p ->
      [ a; b; c; d; e; f; g; h; i; j; k; l; m; n; o; p ])
    Codec.
      [
        utf8 $ nth 0;
        printable $ nth 1;
        ia5 $ nth 2;
        numeric $ nth 3;
        visible $ nth 4;
        teletex $ nth 5;
        bmp $ nth 6;
        universal $ nth 7;
        utc $ nth 8;
        generalized $ nth 9;
        sized $ nth 10;
        octets $ nth 11;
        videotex $ nth 12;
        graphic $ nth 13;
        general $ nth 14;
        descriptor $ nth 15;
      ]

let tags =
  let imp =
    Asn1.required "application"
      (Asn1.implicit ~cls:`Application 3 Asn1.octet_string)
  in
  let exp =
    Asn1.required "private" (Asn1.explicit ~cls:`Private 4 Asn1.integer)
  in
  let ctx_imp =
    Asn1.required "contextImplicit" (Asn1.implicit 5 Asn1.integer)
  in
  let ctx_exp = Asn1.required "contextExplicit" (Asn1.explicit 6 Asn1.bool) in
  let univ =
    Asn1.required "universal" (Asn1.explicit ~cls:`Universal 40 Asn1.null)
  in
  Asn1.sequence "Tags"
    (fun a b c d e -> (a, b, c, d, e))
    Codec.
      [
        (imp $ fun (a, _, _, _, _) -> a);
        (exp $ fun (_, b, _, _, _) -> b);
        (ctx_imp $ fun (_, _, c, _, _) -> c);
        (ctx_exp $ fun (_, _, _, d, _) -> d);
        (univ $ fun (_, _, _, _, e) -> e);
      ]

let optionals =
  let opt = Asn1.optional "opt" (Asn1.explicit 0 Asn1.bool) in
  let flag = Asn1.default "flag" false (Asn1.implicit 1 Asn1.bool) in
  let level =
    Asn1.default "level" 3L
      (Asn1.implicit 2 (Asn1.bounded_integer ~min:0L ~max:7L))
  in
  let label =
    Asn1.default "label" "a \"b\"" (Asn1.implicit 3 Asn1.utf8_string)
  in
  let version = Asn1.default "version" "\x00" (Asn1.explicit 4 Asn1.integer) in
  let mask =
    Asn1.default "mask" (4, "\xA0") (Asn1.implicit 5 Asn1.bit_string)
  in
  let tail = Asn1.required "tail" Asn1.integer in
  Asn1.sequence "Optionals"
    (fun a b c d e f g -> (a, b, c, d, e, f, g))
    Codec.
      [
        (opt $ fun (a, _, _, _, _, _, _) -> a);
        (flag $ fun (_, b, _, _, _, _, _) -> b);
        (level $ fun (_, _, c, _, _, _, _) -> c);
        (label $ fun (_, _, _, d, _, _, _) -> d);
        (version $ fun (_, _, _, _, e, _, _) -> e);
        (mask $ fun (_, _, _, _, _, f, _) -> f);
        (tail $ fun (_, _, _, _, _, _, g) -> g);
      ]

let key_a = Asn1.Oid.(base 1 3 <|| [ 6; 1; 4; 1; 1 ])
let key_b = Asn1.Oid.(base 1 3 <|| [ 6; 1; 4; 1; 2 ])
let key_c = Asn1.Oid.(base 1 3 <|| [ 6; 1; 4; 1; 3 ])

type keyed = Int of string | Text of string | Nothing | Other of Asn1.Oid.t

let keyed =
  let kind = Asn1.required "kind" Asn1.oid in
  let value =
    Asn1.defined_by "value" ~on:kind
      [
        Asn1.case key_a Asn1.integer
          ~inject:(fun i -> Int i)
          ~project:(function Int i -> Some i | _ -> None);
        Asn1.case key_b
          (Asn1.explicit 0 Asn1.utf8_string)
          ~inject:(fun s -> Text s)
          ~project:(function Text s -> Some s | _ -> None);
        Asn1.absent key_c ~inject:Nothing ~project:(function
          | Nothing -> true
          | _ -> false);
        Asn1.fallback ~optional:true (Asn1.explicit 1 Asn1.any)
          ~inject:(fun k _ -> Other k)
          ~project:(function Other k -> Some (k, None) | _ -> None);
      ]
  in
  let trailer = Asn1.required "trailer" Asn1.integer in
  Asn1.sequence "Keyed"
    (fun a b c -> (a, b, c))
    Codec.
      [
        (kind $ fun (a, _, _) -> a);
        (value $ fun (_, b, _) -> b);
        (trailer $ fun (_, _, c) -> c);
      ]

let flagged =
  let on = Asn1.required "on" Asn1.bool in
  let value =
    Asn1.defined_by "value" ~on
      [
        Asn1.case true Asn1.integer ~inject:Option.some ~project:Fun.id;
        Asn1.absent false ~inject:None ~project:Option.is_none;
      ]
  in
  Asn1.sequence "Flagged" (fun a b -> (a, b)) Codec.[ on $ fst; value $ snd ]

let versioned =
  let ver = Asn1.required "v" Asn1.integer in
  let body =
    Asn1.defined_by "body" ~on:ver
      [
        Asn1.case "\x01" Asn1.utf8_string ~inject:Fun.id ~project:Option.some;
        Asn1.case "\xff" Asn1.octet_string ~inject:Fun.id ~project:Option.some;
      ]
  in
  Asn1.sequence "Versioned" (fun a b -> (a, b)) Codec.[ ver $ fst; body $ snd ]

let inner =
  let x = Asn1.required "x" Asn1.integer in
  Asn1.sequence "Inner" Fun.id Codec.[ x $ Fun.id ]

let pair =
  let a = Asn1.required "a" (Asn1.implicit 0 Asn1.bool) in
  let b = Asn1.required "b" (Asn1.implicit 1 Asn1.integer) in
  Asn1.set "Pair" (fun a b -> (a, b)) Codec.[ a $ fst; b $ snd ]

type pick = Num of string | Name of string

let lists =
  let names = Asn1.required "names" (Asn1.sequence_of Asn1.utf8_string) in
  let ids = Asn1.required "ids" (Asn1.set_of ~non_empty:true Asn1.oid) in
  let inners =
    Asn1.required "inners" (Asn1.sequence_of ~non_empty:true inner)
  in
  let pick =
    Asn1.required "pick"
      (Asn1.choice
         [
           Asn1.alternative "num" Asn1.integer
             ~inject:(fun i -> Num i)
             ~project:(function Num i -> Some i | _ -> None);
           Asn1.alternative "name"
             (Asn1.choice
                [ alt "utf8" Asn1.utf8_string; alt "ia5" Asn1.ia5_string ])
             ~inject:(fun s -> Name s)
             ~project:(function Name s -> Some s | _ -> None);
         ])
  in
  let pair = Asn1.required "pair" pair in
  Asn1.sequence "Lists"
    (fun a b c d e -> (a, b, c, d, e))
    Codec.
      [
        (names $ fun (a, _, _, _, _) -> a);
        (ids $ fun (_, b, _, _, _) -> b);
        (inners $ fun (_, _, c, _, _) -> c);
        (pick $ fun (_, _, _, d, _) -> d);
        (pair $ fun (_, _, _, _, e) -> e);
      ]

let payload_codec =
  let len = Field.v "len" uint16be in
  Codec.v "Payload" Fun.id Codec.[ len $ Fun.id ]

let kept =
  let whole = Asn1.required "whole" (Asn1.keep_bytes inner) in
  let elems =
    Asn1.required "elems" (Asn1.sequence_of (Asn1.keep_bytes inner))
  in
  let payload =
    Asn1.required "payload" (Asn1.containing (codec payload_codec))
  in
  let word = Asn1.required "word" (Asn1.containing uint32be) in
  let rest = Asn1.required "rest" Asn1.any in
  Asn1.sequence "Kept"
    (fun a b c d e -> (a, b, c, d, e))
    Codec.
      [
        (whole $ fun (a, _, _, _, _) -> a);
        (elems $ fun (_, b, _, _, _) -> b);
        (payload $ fun (_, _, c, _, _) -> c);
        (word $ fun (_, _, _, d, _) -> d);
        (rest $ fun (_, _, _, _, e) -> e);
      ]

let constructs =
  let f name t = Asn1.required name t in
  let prims = f "prims" prims in
  let strings = f "strings" strings in
  let tags = f "tags" tags in
  let optionals = f "optionals" optionals in
  let keyed = f "keyed" keyed in
  let flagged = f "flagged" flagged in
  let versioned = f "versioned" versioned in
  let lists = f "lists" lists in
  let kept = f "kept" kept in
  Asn1.sequence "Constructs"
    (fun a b c d e f g h i -> (a, b, c, d, e, f, g, h, i))
    Codec.
      [
        (prims $ fun (a, _, _, _, _, _, _, _, _) -> a);
        (strings $ fun (_, b, _, _, _, _, _, _, _) -> b);
        (tags $ fun (_, _, c, _, _, _, _, _, _) -> c);
        (optionals $ fun (_, _, _, d, _, _, _, _, _) -> d);
        (keyed $ fun (_, _, _, _, e, _, _, _, _) -> e);
        (flagged $ fun (_, _, _, _, _, f, _, _, _) -> f);
        (versioned $ fun (_, _, _, _, _, _, g, _, _) -> g);
        (lists $ fun (_, _, _, _, _, _, _, h, _) -> h);
        (kept $ fun (_, _, _, _, _, _, _, _, i) -> i);
      ]

(* -- Roots and names -- *)

(* A root that is not a SEQUENCE or SET is assigned to the module's name. *)
let names = Asn1.sequence_of ~non_empty:true Asn1.utf8_string

(* A root that keeps its bytes is a type of its own. *)
let kept_root = Asn1.keep_bytes inner

(* Two different SEQUENCEs of one name, one met twice, names to sanitise. *)
let clash =
  let item t =
    let a = Asn1.required "a" t in
    Asn1.sequence "item" Fun.id Codec.[ a $ Fun.id ]
  in
  let first = Asn1.required "first" (item Asn1.integer) in
  let second = Asn1.required "Second_Item" (item Asn1.bool) in
  let again = Asn1.required "again" (item Asn1.integer) in
  let weird = Asn1.required "--weird  name!" Asn1.null in
  let weird' = Asn1.required "weird-name" Asn1.null in
  let colour =
    Asn1.required "9colour"
      (Asn1.enumerated [ ("dark_red", 0); ("dark-red", 1); ("", 2) ])
  in
  let end_ =
    let e = Asn1.required "e" Asn1.integer in
    Asn1.required "end" (Asn1.sequence "END" Fun.id Codec.[ e $ Fun.id ])
  in
  Asn1.sequence "Clash"
    (fun a b c d e f g -> (a, b, c, d, e, f, g))
    Codec.
      [
        (first $ fun (a, _, _, _, _, _, _) -> a);
        (second $ fun (_, b, _, _, _, _, _) -> b);
        (again $ fun (_, _, c, _, _, _, _) -> c);
        (weird $ fun (_, _, _, d, _, _, _) -> d);
        (weird' $ fun (_, _, _, _, e, _, _) -> e);
        (colour $ fun (_, _, _, _, _, f, _) -> f);
        (end_ $ fun (_, _, _, _, _, _, g) -> g);
      ]

(* -- EverParse's X509Lite (share/everparse/tests/asn1/tool/X509Lite.asn) -- *)

let sha256_with_rsa = Asn1.Oid.(base 1 2 <|| [ 840; 113549; 1; 1; 11 ])
let ecdsa_with_sha256 = Asn1.Oid.(base 1 2 <|| [ 840; 10045; 4; 3; 2 ])

type alg_params = Rsa | Ecdsa | Unknown_alg of Asn1.Oid.t * string option

let x509lite =
  let alg_id =
    let algorithm = Asn1.required "algorithm" Asn1.oid in
    let parameters =
      Asn1.defined_by "parameters" ~on:algorithm
        [
          Asn1.case sha256_with_rsa Asn1.null
            ~inject:(fun () -> Rsa)
            ~project:(function Rsa -> Some () | _ -> None);
          Asn1.absent ecdsa_with_sha256 ~inject:Ecdsa ~project:(function
            | Ecdsa -> true
            | _ -> false);
          Asn1.fallback ~optional:true Asn1.any
            ~inject:(fun k v -> Unknown_alg (k, v))
            ~project:(function Unknown_alg (k, v) -> Some (k, v) | _ -> None);
        ]
    in
    Asn1.sequence "AlgId"
      (fun a p -> (a, p))
      Codec.[ algorithm $ fst; parameters $ snd ]
  in
  let extension =
    let extn_id = Asn1.required "extnID" Asn1.oid in
    let critical = Asn1.default "critical" false Asn1.bool in
    let extn_value =
      Asn1.defined_by "extnValue" ~on:extn_id
        [
          Asn1.fallback Asn1.octet_string
            ~inject:(fun k v -> (k, v))
            ~project:Option.some;
        ]
    in
    Asn1.sequence "Extension"
      (fun a b c -> (a, b, c))
      Codec.
        [
          (extn_id $ fun (a, _, _) -> a);
          (critical $ fun (_, b, _) -> b);
          (extn_value $ fun (_, _, c) -> c);
        ]
  in
  let tbs =
    let version =
      Asn1.default "version" "\x00" (Asn1.explicit 0 Asn1.integer)
    in
    let serial = Asn1.required "serial" Asn1.integer in
    let signature = Asn1.required "signature" alg_id in
    let subject =
      Asn1.required "subject"
        (Asn1.sequence_of ~non_empty:true
           (Asn1.size ~min:1 ~max:8 Asn1.utf8_string))
    in
    let extensions =
      Asn1.optional "extensions"
        (Asn1.explicit 3 (Asn1.sequence_of ~non_empty:true extension))
    in
    Asn1.sequence "TBS"
      (fun a b c d e -> (a, b, c, d, e))
      Codec.
        [
          (version $ fun (a, _, _, _, _) -> a);
          (serial $ fun (_, b, _, _, _) -> b);
          (signature $ fun (_, _, c, _, _) -> c);
          (subject $ fun (_, _, _, d, _) -> d);
          (extensions $ fun (_, _, _, _, e) -> e);
        ]
  in
  let f_tbs = Asn1.required "tbs" (Asn1.keep_bytes tbs) in
  let sig_alg = Asn1.required "sigAlg" alg_id in
  let sig_ = Asn1.required "sig" Asn1.bit_string in
  Asn1.sequence "Cert"
    (fun a b c -> (a, b, c))
    Codec.
      [
        (f_tbs $ fun (a, _, _) -> a);
        (sig_alg $ fun (_, b, _) -> b);
        (sig_ $ fun (_, _, c) -> c);
      ]

(* -- The six universal types everparse-asn1 adds to X.680's usual ones --

   Each time and IRI is a DEFAULT, so that its literal is printed:
   everparse-asn1 reads DATE, TIME-OF-DAY and DATE-TIME literals in the
   extended format and checks them in F* (README:431-434). A RELATIVE-OID has
   no literal and is required. *)

type times = {
  day : string;
  clock : string;
  stamp : string;
  iri : string;
  rel_iri : string;
  rel : Asn1.Relative_oid.t;
}

let times =
  let day = Asn1.default "day" "20240229" Asn1.date in
  let clock = Asn1.default "clock" "235960" Asn1.time_of_day in
  let stamp = Asn1.default "stamp" "15821015000000" Asn1.date_time in
  let iri =
    Asn1.default "iri" "/ISO/Registration_Authority/19785" Asn1.oid_iri
  in
  let rel_iri = Asn1.default "relIri" "a-b/1" Asn1.relative_oid_iri in
  let rel = Asn1.required "rel" Asn1.relative_oid in
  Asn1.sequence "Times"
    (fun day clock stamp iri rel_iri rel ->
      { day; clock; stamp; iri; rel_iri; rel })
    Codec.
      [
        (day $ fun r -> r.day);
        (clock $ fun r -> r.clock);
        (stamp $ fun r -> r.stamp);
        (iri $ fun r -> r.iri);
        (rel_iri $ fun r -> r.rel_iri);
        (rel $ fun r -> r.rel);
      ]

(* -- DER and binary codecs nested in each other -- *)

(* A binary record holding a DER SubjectPublicKeyInfo, under its length. *)
type framed = UInt16.t * spki

let framed : framed Codec.t =
  let len = Field.v "len" uint16be in
  let key =
    Field.v "key" (nested ~size:(Field.ref len) (codec (Asn1.codec spki)))
  in
  Codec.v "Framed" (fun len key -> (len, key)) Codec.[ len $ fst; key $ snd ]

(* An OCTET STRING holding a DER value, as X.509's extnValue holds its
   extension. *)
let wrapped =
  let inner =
    let n = Asn1.required "n" Asn1.integer in
    Asn1.sequence "Inner" Fun.id Codec.[ n $ Fun.id ]
  in
  let body =
    Asn1.required "body" (Asn1.containing (codec (Asn1.codec inner)))
  in
  let tail = Asn1.required "tail" (Asn1.containing (codec payload_codec)) in
  Asn1.sequence "Wrapped"
    (fun body tail -> (body, tail))
    Codec.[ body $ fst; tail $ snd ]

type packed = Packed : 'a Asn1.t -> packed

(* Each type with the name of its module. *)
let types =
  [
    ("SPKI", Packed spki);
    ("Constructs", Packed constructs);
    ("Names", Packed names);
    ("KeptRoot", Packed kept_root);
    ("Clash", Packed clash);
    ("X509Lite", Packed x509lite);
    ("Times", Packed times);
    ("Wrapped", Packed wrapped);
  ]

let all =
  List.map (fun (m, Packed t) -> Asn1.project m (Asn1.codec t)) types
  @ [ Asn1.project "Framed" framed ]

let spki = Packed spki
let constructs = Packed constructs
let names = Packed names
let kept_root = Packed kept_root
let clash = Packed clash
let x509lite = Packed x509lite
let times = Packed times
let wrapped = Packed wrapped
