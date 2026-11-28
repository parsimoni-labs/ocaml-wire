(* RFC 9052 COSE_Sign1 and COSE_Key as Wire.Cbor types: the running example of
   docs/cbor.md, and COSE_Key_OKP of EverParse's src/cose/cose-sign.cddl. *)

open Wire

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

type key = {
  crv : label;
  x : string option;
  d : string option;
  key_others : (label * string) list;
}

let label =
  Cbor.(
    choice
      [
        case "int" int
          ~inject:(fun i -> Int i)
          ~project:(function Int i -> Some i | Text _ -> None);
        case "tstr" text
          ~inject:(fun s -> Text s)
          ~project:(function Text s -> Some s | Int _ -> None);
      ])

let f_alg = Cbor.mem_opt "alg" (`Int 1) label
let f_crit = Cbor.mem_opt "crit" (`Int 2) (Cbor.array ~non_empty:true label)
let f_cty = Cbor.mem_opt "content_type" (`Int 3) label
let f_kid = Cbor.mem_opt "kid" (`Int 4) Cbor.bytes

let f_iv =
  Cbor.(
    exclusive "iv"
      [
        member (`Int 5) bytes
          ~inject:(fun s -> Iv s)
          ~project:(function Iv s -> Some s | Partial_iv _ -> None);
        member (`Int 6) bytes
          ~inject:(fun s -> Partial_iv s)
          ~project:(function Partial_iv s -> Some s | Iv _ -> None);
      ])

let f_others = Cbor.keep_unknown "others" label Cbor.any

let header_map =
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

(* The protected bucket: a byte string holding a header map, or empty. *)
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

let f_protected = Cbor.elem "protected" protected
let f_unprotected = Cbor.elem "unprotected" header_map
let f_payload = Cbor.elem "payload" (Cbor.nullable Cbor.bytes)
let f_signature = Cbor.elem "signature" Cbor.bytes

let sign1 =
  Cbor.tuple "COSE_Sign1"
    (fun protected unprotected payload signature ->
      { protected; unprotected; payload; signature })
    Codec.
      [
        (f_protected $ fun s -> s.protected);
        (f_unprotected $ fun s -> s.unprotected);
        (f_payload $ fun s -> s.payload);
        (f_signature $ fun s -> s.signature);
      ]

let sign1_codec = Cbor.codec (Cbor.tag 18 sign1)

(* COSE_Key_OKP: kty 1, crv -1, x -2, d -4 (RFC 9053 section 7.2). *)
let f_kty = Cbor.mem "kty" (`Int 1) (Cbor.literal (`Int 1))
let f_crv = Cbor.mem "crv" (`Int (-1)) label
let f_x = Cbor.mem_opt "x" (`Int (-2)) Cbor.bytes
let f_d = Cbor.mem_opt "d" (`Int (-4)) Cbor.bytes
let f_key_others = Cbor.keep_unknown "others" label Cbor.any

let key =
  Cbor.record "COSE_Key_OKP"
    (fun () crv x d key_others -> { crv; x; d; key_others })
    Codec.
      [
        (f_kty $ fun _ -> ());
        (f_crv $ fun k -> k.crv);
        (f_x $ fun k -> k.x);
        (f_d $ fun k -> k.d);
        (f_key_others $ fun k -> k.key_others);
      ]

let key_codec = Cbor.codec key
