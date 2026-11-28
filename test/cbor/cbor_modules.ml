(* CBOR types whose .cddl modules the tests print and evercddl reads. *)

open Wire

type packed = Packed : 'a Cbor.t -> packed

let case name t ~inject ~project = Cbor.case name t ~inject ~project

(* -- The running example of docs/cbor.md: COSE_Sign1 (RFC 9052) -- *)

type label = Int of Cbor.integer | Text of string
type iv = Iv of string | Partial_iv of string

type header = {
  alg : label option;
  crit : label list option;
  content_type : label option;
  kid : string option;
  iv : iv option;
  others : (label * string) list;
}

type sign1 = {
  protected : (header * string) option;
  unprotected : header;
  payload : string option;
  signature : string;
}

let label =
  Cbor.(
    choice
      [
        case "int" int
          ~inject:(fun i -> Int i)
          ~project:(function Int i -> Some i | _ -> None);
        case "tstr" text
          ~inject:(fun s -> Text s)
          ~project:(function Text s -> Some s | _ -> None);
      ])

let header_map =
  let f_alg = Cbor.mem_opt "alg" (`Int 1) label in
  let f_crit =
    Cbor.mem_opt "crit" (`Int 2) (Cbor.array ~non_empty:true label)
  in
  let f_cty = Cbor.mem_opt "content_type" (`Int 3) label in
  let f_kid = Cbor.mem_opt "kid" (`Int 4) Cbor.bytes in
  let f_iv =
    Cbor.(
      exclusive "iv"
        [
          member (`Int 5) bytes
            ~inject:(fun s -> Iv s)
            ~project:(function Iv s -> Some s | _ -> None);
          member (`Int 6) bytes
            ~inject:(fun s -> Partial_iv s)
            ~project:(function Partial_iv s -> Some s | _ -> None);
        ])
  in
  let f_others = Cbor.keep_unknown "others" label Cbor.any in
  Cbor.record "header_map"
    (fun alg crit content_type kid iv others ->
      { alg; crit; content_type; kid; iv; others })
    Codec.
      [
        (f_alg $ fun h -> h.alg);
        (f_crit $ fun h -> h.crit);
        (f_cty $ fun h -> h.content_type);
        (f_kid $ fun h -> h.kid);
        (f_iv $ fun h -> h.iv);
        (f_others $ fun h -> h.others);
      ]

let cose_sign1 =
  let protected =
    Cbor.(
      choice
        [
          case "serialized"
            (keep_bytes (encoded header_map))
            ~inject:Option.some ~project:Fun.id;
          case "empty" (size ~min:0 ~max:0 bytes)
            ~inject:(fun _ -> None)
            ~project:(function None -> Some "" | Some _ -> None);
        ])
  in
  let f_protected = Cbor.elem "protected" protected in
  let f_unprotected = Cbor.elem "unprotected" header_map in
  let f_payload = Cbor.elem "payload" (Cbor.nullable Cbor.bytes) in
  let f_signature = Cbor.elem "signature" Cbor.bytes in
  Cbor.tag 18
    (Cbor.tuple "COSE_Sign1"
       (fun protected unprotected payload signature ->
         { protected; unprotected; payload; signature })
       Codec.
         [
           (f_protected $ fun s -> s.protected);
           (f_unprotected $ fun s -> s.unprotected);
           (f_payload $ fun s -> s.payload);
           (f_signature $ fun s -> s.signature);
         ])

(* -- COSE_Key, as cose-sign.cddl's COSE_Key_OKP (RFC 9053 section 7.2) -- *)

type key = {
  crv : label;
  x : string option;
  d : string option;
  rest : (label * string) list;
}

let cose_key =
  let f_kty = Cbor.mem "kty" (`Int 1) (Cbor.literal (`Int 1)) in
  let f_crv = Cbor.mem "crv" (`Int (-1)) label in
  let f_x = Cbor.mem_opt "x" (`Int (-2)) Cbor.bytes in
  let f_d = Cbor.mem_opt "d" (`Int (-4)) Cbor.bytes in
  let f_rest = Cbor.keep_unknown "rest" label Cbor.any in
  Cbor.record "COSE_Key_OKP"
    (fun () crv x d rest -> { crv; x; d; rest })
    Codec.
      [
        (f_kty $ fun _ -> ());
        (f_crv $ fun k -> k.crv);
        (f_x $ fun k -> k.x);
        (f_d $ fun k -> k.d);
        (f_rest $ fun k -> k.rest);
      ]

(* -- One module per row of docs/cbor.md, "Constructs and their
   projections" -- *)

let pair name (n1, t1) (n2, t2) =
  Cbor.tuple name
    (fun a b -> (a, b))
    Codec.[ Cbor.elem n1 t1 $ fst; Cbor.elem n2 t2 $ snd ]

let triple name (n1, t1) (n2, t2) (n3, t3) =
  Cbor.tuple name
    (fun a b c -> (a, b, c))
    Codec.
      [
        (Cbor.elem n1 t1 $ fun (a, _, _) -> a);
        (Cbor.elem n2 t2 $ fun (_, b, _) -> b);
        (Cbor.elem n3 t3 $ fun (_, _, c) -> c);
      ]

let uints = pair "uints" ("u", Cbor.uint) ("n", Cbor.nint)

let ranges =
  triple "ranges"
    ("neg", Cbor.range ~min:(-10L) ~max:(-1L))
    ("pos", Cbor.range ~min:0L ~max:255L)
    ("both", Cbor.range ~min:(-128L) ~max:127L)

let strings = pair "strings" ("b", Cbor.bytes) ("t", Cbor.text)
let simple = pair "simple" ("flag", Cbor.bool) ("nothing", Cbor.null)

let literals =
  triple "literals"
    ("one", Cbor.literal (`Int 1))
    ("minus_one", Cbor.literal (`Int (-1)))
    ("context", Cbor.literal (`Text "Signature1"))

let sizes =
  triple "sizes"
    ("empty", Cbor.size ~min:0 ~max:0 Cbor.bytes)
    ("name", Cbor.size ~min:1 ~max:64 Cbor.text)
    ("hash", Cbor.size ~min:32 ~max:32 Cbor.bytes)

let arrays =
  pair "arrays"
    ("any", Cbor.array Cbor.uint)
    ("some", Cbor.array ~non_empty:true Cbor.text)

let entries =
  Cbor.tuple "entries"
    (fun a b c -> (a, b, c))
    Codec.
      [
        (Cbor.elem "a" Cbor.uint $ fun (a, _, _) -> a);
        (Cbor.elem_opt "b" Cbor.text $ fun (_, b, _) -> b);
        (Cbor.elem "c" Cbor.bytes $ fun (_, _, c) -> c);
      ]

type members = { a : UInt64.t; b : string option; c : string }

let members =
  let a = Cbor.mem "a" (`Int 1) Cbor.uint in
  let b = Cbor.mem_opt "b" (`Int (-1)) Cbor.text in
  let c = Cbor.mem "c" (`Text "c") Cbor.bytes in
  Cbor.record "members"
    (fun a b c -> { a; b; c })
    Codec.[ (a $ fun r -> r.a); (b $ fun r -> r.b); (c $ fun r -> r.c) ]

type one_of = Five of string | Six of string | Name of string

let exclusive =
  let a = Cbor.mem "a" (`Int 1) Cbor.uint in
  let iv =
    Cbor.(
      exclusive "iv"
        [
          member (`Int 5) bytes
            ~inject:(fun s -> Five s)
            ~project:(function Five s -> Some s | _ -> None);
          member (`Int 6) bytes
            ~inject:(fun s -> Six s)
            ~project:(function Six s -> Some s | _ -> None);
          member (`Text "name") text
            ~inject:(fun s -> Name s)
            ~project:(function Name s -> Some s | _ -> None);
        ])
  in
  Cbor.record "exclusive" (fun a iv -> (a, iv)) Codec.[ a $ fst; iv $ snd ]

let keep_unknown =
  let a = Cbor.mem_opt "a" (`Int 1) Cbor.uint in
  let rest = Cbor.keep_unknown "rest" Cbor.int Cbor.any in
  Cbor.record "open_map" (fun a rest -> (a, rest)) Codec.[ a $ fst; rest $ snd ]

let choice =
  Cbor.choice
    [
      case "n" Cbor.uint
        ~inject:(fun n -> `N n)
        ~project:(function `N n -> Some n | _ -> None);
      case "t" Cbor.text
        ~inject:(fun s -> `T s)
        ~project:(function `T s -> Some s | _ -> None);
      case "a" (Cbor.array Cbor.uint)
        ~inject:(fun l -> `A l)
        ~project:(function `A l -> Some l | _ -> None);
    ]

let encoded =
  let small =
    Cbor.choice
      [
        case "n" Cbor.uint
          ~inject:(fun n -> `N n)
          ~project:(function `N n -> Some n | _ -> None);
        case "t" Cbor.text
          ~inject:(fun s -> `T s)
          ~project:(function `T s -> Some s | _ -> None);
      ]
  in
  pair "encoded"
    ("number", Cbor.encoded Cbor.uint)
    ("either", Cbor.encoded small)

let payload_codec =
  let len = Field.v "len" uint16be in
  Codec.v "Payload" Fun.id Codec.[ len $ Fun.id ]

let containing =
  pair "containing"
    ("payload", Cbor.containing (codec payload_codec))
    ("word", Cbor.containing uint32be)

let keep_bytes = Cbor.keep_bytes members
let mapped = Cbor.map ~decode:UInt64.to_int ~encode:UInt64.of_int Cbor.uint

(* Names to sanitise and make unique: a record named after a prelude type,
   two different tuples of one name, names that are no CDDL name, and two
   labels that sanitise alike. *)
let clash =
  let one name t = Cbor.tuple name Fun.id Codec.[ Cbor.elem "v" t $ Fun.id ] in
  let any =
    Cbor.record "any" Fun.id Codec.[ Cbor.mem "k" (`Int 1) Cbor.uint $ Fun.id ]
  in
  Cbor.tuple "Clash"
    (fun a b c d e -> (a, b, c, d, e))
    Codec.
      [
        (Cbor.elem "a b" any $ fun (a, _, _, _, _) -> a);
        (Cbor.elem "a_b" (one "pair" Cbor.uint) $ fun (_, b, _, _, _) -> b);
        (Cbor.elem "c" (one "pair" Cbor.text) $ fun (_, _, c, _, _) -> c);
        (Cbor.elem "d" (one "x-y z" Cbor.bool) $ fun (_, _, _, d, _) -> d);
        (Cbor.elem "e" (one "1st" Cbor.null) $ fun (_, _, _, _, e) -> e);
      ]

(* A binary record holding a CBOR value under its length. *)
type framed = UInt16.t * sign1

let framed : framed Codec.t =
  let len = Field.v "len" uint16be in
  let msg =
    Field.v "msg" (nested ~size:(Field.ref len) (codec (Cbor.codec cose_sign1)))
  in
  Codec.v "Framed" (fun len msg -> (len, msg)) Codec.[ len $ fst; msg $ snd ]

(* A DER SEQUENCE holding a CBOR value in an OCTET STRING, itself holding a
   binary codec in a byte string. *)
type enveloped = string * (UInt16.t * UInt64.t)

let enveloped : enveloped Codec.t =
  let body =
    Cbor.tuple "Body"
      (fun p n -> (p, n))
      Codec.
        [
          Cbor.elem "payload" (Cbor.containing (codec payload_codec)) $ fst;
          Cbor.elem "count" Cbor.uint $ snd;
        ]
  in
  Asn1.codec
    (Asn1.sequence "Envelope"
       (fun v b -> (v, b))
       Codec.
         [
           Asn1.required "version" Asn1.integer $ fst;
           Asn1.required "body" (Asn1.containing (codec (Cbor.codec body)))
           $ snd;
         ])

let types =
  [
    ("COSE_Sign1_Tagged", Packed cose_sign1);
    ("COSE_Key", Packed cose_key);
    ("Cbor_uint", Packed uints);
    ("Cbor_int", Packed Cbor.int);
    ("Cbor_range", Packed ranges);
    ("Cbor_strings", Packed strings);
    ("Cbor_simple", Packed simple);
    ("Cbor_literal", Packed literals);
    ("Cbor_any", Packed Cbor.any);
    ("Cbor_size", Packed sizes);
    ("Cbor_tag", Packed (Cbor.tag 42 Cbor.bytes));
    ("Cbor_array", Packed arrays);
    ("Cbor_tuple", Packed entries);
    ("Cbor_assoc", Packed (Cbor.assoc Cbor.text Cbor.uint));
    ("Cbor_record", Packed members);
    ("Cbor_exclusive", Packed exclusive);
    ("Cbor_keep_unknown", Packed keep_unknown);
    ("Cbor_choice", Packed choice);
    ("Cbor_nullable", Packed (Cbor.nullable Cbor.text));
    ("Cbor_encoded", Packed encoded);
    ("Cbor_containing", Packed containing);
    ("Cbor_keep_bytes", Packed keep_bytes);
    ("Cbor_map", Packed mapped);
    ("Clash", Packed clash);
  ]

let project m (Packed t) = Cbor.project m (Cbor.codec t)

let all =
  List.map (fun (m, t) -> project m t) types
  @ [ Cbor.project "Framed" framed; Cbor.project "Enveloped" enveloped ]

let cose_sign1 = Packed cose_sign1
let cose_key = Packed cose_key
let containing = Packed containing
let clash = Packed clash
