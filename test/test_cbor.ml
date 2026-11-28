(* Tests for cbor.ml: the CBOR combinators, and every shape docs/cbor.md
   ("Refused when built") says the combinators refuse. *)

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

let label () =
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

let header_map () =
  let label = label () in
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

let cose_sign1 () =
  let header_map = header_map () in
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

let test_cose_sign1_builds () = builds cose_sign1

(* x5chain (RFC 9360 section 2): a DER certificate, or an array of them. *)
let test_x5chain_builds () =
  let cert = Cbor.containing (byte_array ~size:(int 4)) in
  builds (fun () ->
      Cbor.(
        choice
          [
            case "one" cert
              ~inject:(fun c -> [ c ])
              ~project:(function [ c ] -> Some c | _ -> None);
            case "chain"
              (array ~non_empty:true cert)
              ~inject:Fun.id ~project:Option.some;
          ]))

(* -- Constructors -- *)

let one name t = Cbor.tuple name Fun.id Codec.[ Cbor.elem "v" t $ Fun.id ]

let test_primitives () =
  builds (fun () -> one "uint" Cbor.uint);
  builds (fun () -> one "nint" Cbor.nint);
  builds (fun () -> one "int" Cbor.int);
  builds (fun () -> one "range" (Cbor.range ~min:(-10L) ~max:10L));
  builds (fun () -> Cbor.range ~min:Int64.min_int ~max:Int64.max_int);
  builds (fun () -> Cbor.range ~min:3L ~max:3L);
  builds (fun () -> one "bytes" Cbor.bytes);
  builds (fun () -> one "text" Cbor.text);
  builds (fun () -> one "bool" Cbor.bool);
  builds (fun () -> one "null" Cbor.null);
  builds (fun () -> one "any" Cbor.any);
  builds (fun () -> Cbor.literal (`Int (-1)));
  builds (fun () -> Cbor.literal (`Text "Signature1"));
  builds (fun () -> Cbor.literal (`Text (String.make 0xFFFF 'a')))

let test_size () =
  builds (fun () -> Cbor.size ~min:0 ~max:0 Cbor.bytes);
  builds (fun () -> Cbor.size ~min:1 ~max:64 Cbor.text)

let test_tag () =
  builds (fun () -> Cbor.tag 0 Cbor.text);
  builds (fun () -> Cbor.tag 18 (Cbor.tag 24 Cbor.bytes))

let test_array () =
  builds (fun () -> Cbor.array Cbor.uint);
  builds (fun () -> Cbor.array ~non_empty:true (Cbor.array Cbor.any))

let pair a b = Codec.[ a $ fst; b $ snd ]
let mk a b = (a, b)

let test_tuple () =
  (* An optional entry at the end, or before entries of other types, splits
     one way. *)
  builds (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem "a" Cbor.uint) (Cbor.elem_opt "b" Cbor.uint)));
  builds (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem_opt "a" Cbor.uint) (Cbor.elem "b" Cbor.text)));
  builds (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem_opt "a" Cbor.nint) (Cbor.elem_opt "b" Cbor.uint)));
  builds (fun () ->
      Cbor.tuple "T" mk
        (pair
           (Cbor.elem_opt "a" (Cbor.range ~min:0L ~max:9L))
           (Cbor.elem "b" (Cbor.range ~min:10L ~max:20L))))

let test_record () =
  builds (fun () ->
      Cbor.record "R" mk
        (pair
           (Cbor.mem "a" (`Int 1) Cbor.uint)
           (Cbor.mem_opt "b" (`Text "1") Cbor.uint)));
  builds (fun () ->
      Cbor.record "R" mk
        (pair
           (Cbor.mem "a" (`Int 1) Cbor.uint)
           (Cbor.mem "b" (`Int (-2)) Cbor.uint)))

let test_assoc () =
  builds (fun () -> Cbor.assoc Cbor.text Cbor.any);
  builds (fun () -> one "assoc" (Cbor.assoc Cbor.int (Cbor.array Cbor.uint)))

(* A case of any type: whether cases are disjoint depends on their types
   only. *)
let c name t = Cbor.case name t ~inject:(fun _ -> ()) ~project:(fun () -> None)

let test_choice () =
  builds (fun () -> Cbor.choice [ c "only" Cbor.uint ]);
  builds (fun () -> Cbor.choice [ c "u" Cbor.uint; c "n" Cbor.nint ]);
  builds (fun () ->
      Cbor.choice [ c "t" Cbor.text; c "e" (Cbor.encoded Cbor.text) ]);
  builds (fun () ->
      Cbor.choice [ c "t" Cbor.text; c "s" (Cbor.tag 1 Cbor.text) ]);
  builds (fun () ->
      Cbor.choice
        [
          c "e" (Cbor.encoded Cbor.text);
          c "s" (Cbor.size ~min:0 ~max:0 Cbor.bytes);
        ]);
  (* A range across zero is split at zero against a choice. *)
  builds (fun () ->
      Cbor.choice
        [
          c "r" (Cbor.range ~min:(-5L) ~max:5L);
          c "t" (Cbor.choice [ c "b" Cbor.bytes; c "x" Cbor.text ]);
        ]);
  builds (fun () ->
      Cbor.choice
        [
          c "a" (Cbor.map ~decode:Fun.id ~encode:Fun.id (Cbor.literal (`Int 1)));
          c "b" (Cbor.literal (`Int 2));
          c "x" (Cbor.literal (`Text "x"));
        ]);
  builds (fun () ->
      Cbor.choice
        [
          c "short" (Cbor.size ~min:0 ~max:4 Cbor.text);
          c "long" (Cbor.size ~min:5 ~max:9 Cbor.text);
        ]);
  builds (fun () ->
      Cbor.choice [ c "t" Cbor.bool; c "n" (Cbor.literal (`Int 0)) ])

let test_nullable () =
  builds (fun () -> Cbor.nullable Cbor.bytes);
  builds (fun () -> Cbor.nullable Cbor.bool)

let test_bytes () =
  builds (fun () -> Cbor.encoded (Cbor.array Cbor.uint));
  builds (fun () -> Cbor.containing uint8);
  builds (fun () -> Cbor.keep_bytes Cbor.any);
  builds (fun () ->
      Cbor.map ~decode:Int64.to_int ~encode:Int64.of_int
        (Cbor.range ~min:0L ~max:255L))

(* -- Refusals: docs/cbor.md, Refused when built -- *)

let test_range_empty () =
  rejects ~msg:"Wire.Cbor.range: 2..1 is empty" (fun () ->
      Cbor.range ~min:2L ~max:1L)

let test_size_not_string () =
  rejects ~msg:"Wire.Cbor.size: the type is not bytes or text" (fun () ->
      Cbor.size ~min:0 ~max:1 Cbor.any)

let test_size_negative () =
  rejects ~msg:"Wire.Cbor.size: -1..1 is no range of lengths" (fun () ->
      Cbor.size ~min:(-1) ~max:1 Cbor.bytes)

let test_size_empty () =
  rejects ~msg:"Wire.Cbor.size: 2..1 is no range of lengths" (fun () ->
      Cbor.size ~min:2 ~max:1 Cbor.text)

let test_literal_not_ascii () =
  rejects
    ~msg:"Wire.Cbor.literal: the text literal \"caf\\195\\169\" is not ASCII"
    (fun () -> Cbor.literal (`Text "caf\xc3\xa9"))

let test_literal_too_long () =
  rejects ~msg:"Wire.Cbor.literal: a text literal of 65536 bytes" (fun () ->
      Cbor.literal (`Text (String.make 0x10000 'a')))

let test_key_not_ascii () =
  rejects ~msg:"Wire.Cbor.mem: the text literal" (fun () ->
      Cbor.mem "k" (`Text "\xff") Cbor.uint);
  rejects ~msg:"Wire.Cbor.member: the text literal" (fun () ->
      Cbor.member (`Text "\xff") Cbor.uint ~inject:Fun.id ~project:Option.some)

let test_key_too_long () =
  rejects ~msg:"Wire.Cbor.mem_opt: a text literal of 65536 bytes" (fun () ->
      Cbor.mem_opt "k" (`Text (String.make 0x10000 'k')) Cbor.uint)

(* evercddl's lexer reads a text literal as printable ASCII with no escape. *)
let test_literal_unprintable () =
  List.iter
    (fun s ->
      rejects ~msg:"which evercddl's lexer does not read" (fun () ->
          Cbor.literal (`Text s));
      rejects ~msg:"which evercddl's lexer does not read" (fun () ->
          Cbor.mem "m" (`Text s) Cbor.uint))
    [ "a\"b"; "a\\b"; "a\nb"; "\x7f" ]

let test_tag_negative () =
  rejects ~msg:"Wire.Cbor.tag: tag -1 is negative" (fun () ->
      Cbor.tag (-1) Cbor.uint)

let test_tuple_foreign_field () =
  let f = Field.v "x" uint8 in
  rejects ~msg:"Wire.Cbor.tuple \"T\": field \"x\" is not made by Cbor.elem"
    (fun () -> Cbor.tuple "T" Fun.id Codec.[ f $ Fun.id ]);
  let m = Cbor.mem "m" (`Int 1) Cbor.uint in
  rejects ~msg:"Wire.Cbor.tuple \"T\": field \"m\" is not made by Cbor.elem"
    (fun () -> Cbor.tuple "T" Fun.id Codec.[ m $ Fun.id ])

let test_tuple_repeated_name () =
  rejects ~msg:"Wire.Cbor.tuple \"T\": member \"a\" is given twice" (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem "a" Cbor.uint) (Cbor.elem "a" Cbor.text)))

let test_tuple_empty () =
  rejects ~msg:"Wire.Cbor.tuple \"T\": no entry" (fun () ->
      Cbor.tuple "T" () Codec.[])

(* [? a: uint, b: uint] reads [1] as a and then misses b: the greedy [?] of
   cose-sign.cddl:54-80. *)
let test_tuple_ambiguous () =
  rejects ~msg:"optional entry \"a\" can also start the entries after it"
    (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem_opt "a" Cbor.uint) (Cbor.elem "b" Cbor.uint)));
  rejects ~msg:"optional entry \"a\" can also start the entries after it"
    (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem_opt "a" Cbor.int) (Cbor.elem_opt "b" Cbor.uint)));
  rejects ~msg:"optional entry \"a\" can also start the entries after it"
    (fun () ->
      Cbor.tuple "T" mk
        (pair (Cbor.elem_opt "a" Cbor.any) (Cbor.elem "b" Cbor.text)));
  rejects ~msg:"optional entry \"a\" can also start the entries after it"
    (fun () ->
      Cbor.tuple "T" mk
        (pair
           (Cbor.elem_opt "a" (Cbor.assoc Cbor.text Cbor.text))
           (Cbor.elem "b" (Cbor.assoc Cbor.uint Cbor.uint))))

type inner = { x : UInt8.t }
type outer = { l : inner }

let test_tuple_path () =
  let fx = Field.v "x" uint8 in
  let ic = Codec.v "I" (fun x -> { x }) Codec.[ (fx $ fun i -> i.x) ] in
  let fl = Field.v "l" (codec ic) in
  let l = Codec.layer Codec.(fl $ fun o -> o.l) in
  let path = Codec.(l / (fx $ fun i -> i.x)) in
  rejects ~msg:"Wire.Cbor.tuple \"T\": a path names a field of a layer"
    (fun () ->
      Cbor.tuple "T" (fun x -> { l = { x = Option.get x } }) Codec.[ path ])

let test_record_foreign_field () =
  let e = Cbor.elem "e" Cbor.uint in
  rejects ~msg:"Wire.Cbor.record \"R\": field \"e\" is not made by Cbor.mem"
    (fun () -> Cbor.record "R" Fun.id Codec.[ e $ Fun.id ])

let test_record_repeated_name () =
  rejects ~msg:"Wire.Cbor.record \"R\": member \"a\" is given twice" (fun () ->
      Cbor.record "R" mk
        (pair
           (Cbor.mem "a" (`Int 1) Cbor.uint)
           (Cbor.mem "a" (`Int 2) Cbor.uint)))

let test_record_shared_key () =
  rejects ~msg:"members \"a\" and \"b\" share the key 1" (fun () ->
      Cbor.record "R" mk
        (pair
           (Cbor.mem "a" (`Int 1) Cbor.uint)
           (Cbor.mem_opt "b" (`Int 1) Cbor.text)));
  let ex =
    Cbor.exclusive "x"
      [
        Cbor.member (`Text "k") Cbor.uint ~inject:Fun.id ~project:Option.some;
        Cbor.member (`Int 5) Cbor.uint ~inject:Fun.id ~project:Option.some;
      ]
  in
  rejects ~msg:"members \"a\" and \"x\" share the key \"k\"" (fun () ->
      Cbor.record "R" mk (pair (Cbor.mem "a" (`Text "k") Cbor.bytes) ex))

let test_record_two_keep_unknown () =
  rejects ~msg:"members \"a\" and \"b\" both keep unknown entries" (fun () ->
      Cbor.record "R" mk
        (pair
           (Cbor.keep_unknown "a" Cbor.uint Cbor.any)
           (Cbor.keep_unknown "b" Cbor.text Cbor.any)))

let test_exclusive_too_few () =
  rejects ~msg:"Wire.Cbor.exclusive \"x\": 1 member" (fun () ->
      Cbor.exclusive "x"
        [ Cbor.member (`Int 5) Cbor.uint ~inject:Fun.id ~project:Option.some ]);
  rejects ~msg:"Wire.Cbor.exclusive \"x\": 0 member" (fun () ->
      Cbor.exclusive "x" [])

let test_exclusive_repeated_key () =
  rejects ~msg:"Wire.Cbor.exclusive \"x\": the key 5 is given twice" (fun () ->
      Cbor.exclusive "x"
        [
          Cbor.member (`Int 5) Cbor.uint ~inject:Fun.id ~project:Option.some;
          Cbor.member (`Int 5) Cbor.nint ~inject:Fun.id ~project:Option.some;
        ])

let test_choice_empty () =
  rejects ~msg:"Wire.Cbor.choice: a choice has at least one case" (fun () ->
      Cbor.choice [])

let test_choice_repeated_name () =
  rejects ~msg:"Wire.Cbor.choice: case \"a\" is given twice" (fun () ->
      Cbor.choice [ c "a" Cbor.uint; c "a" Cbor.nint ])

let test_choice_meet () =
  let meet a b cases =
    rejects ~msg:(Fmt.str "cases %S and %S can match one data item" a b)
      (fun () -> Cbor.choice cases)
  in
  meet "u" "i" [ c "u" Cbor.uint; c "i" Cbor.int ];
  meet "t" "l"
    [
      c "t" Cbor.text;
      c "l"
        (Cbor.map
           ~decode:(fun () -> "")
           ~encode:ignore
           (Cbor.literal (`Text "x")));
    ];
  meet "a" "b" [ c "a" Cbor.any; c "b" Cbor.bool ];
  meet "n" "r" [ c "n" Cbor.nint; c "r" (Cbor.range ~min:(-3L) ~max:3L) ];
  meet "s" "t"
    [
      c "s" (Cbor.size ~min:0 ~max:4 Cbor.bytes);
      c "t" (Cbor.size ~min:4 ~max:8 Cbor.bytes);
    ];
  meet "x" "y" [ c "x" (Cbor.tag 1 Cbor.uint); c "y" (Cbor.tag 1 Cbor.int) ];
  meet "e" "b" [ c "e" (Cbor.encoded Cbor.uint); c "b" Cbor.bytes ];
  meet "u" "w"
    [ c "u" Cbor.uint; c "v" Cbor.text; c "w" (Cbor.range ~min:0L ~max:1L) ];
  meet "p" "q"
    [
      c "p" (Cbor.array Cbor.uint); c "q" (Cbor.array ~non_empty:true Cbor.int);
    ]

(* Two maps are never proved disjoint (Disjoint.Type.fst:205). *)
let test_choice_two_maps () =
  let r name k =
    Cbor.record name Fun.id Codec.[ Cbor.mem "a" (`Int k) Cbor.uint $ Fun.id ]
  in
  rejects ~msg:"two maps are never proved disjoint" (fun () ->
      Cbor.choice [ c "a" (r "A" 1); c "b" (r "B" 2) ]);
  rejects ~msg:"two maps are never proved disjoint" (fun () ->
      Cbor.choice
        [
          c "a"
            (Cbor.map
               ~decode:(fun _ -> 0)
               ~encode:(fun _ -> [])
               (Cbor.assoc Cbor.uint Cbor.uint));
          c "b" (r "B" 2);
        ])

let test_nullable_meets_nil () =
  rejects ~msg:"Wire.Cbor.nullable: the type matches nil" (fun () ->
      Cbor.nullable Cbor.null);
  rejects ~msg:"Wire.Cbor.nullable: the type matches nil" (fun () ->
      Cbor.nullable Cbor.any);
  rejects ~msg:"Wire.Cbor.nullable: the type matches nil" (fun () ->
      Cbor.nullable (Cbor.nullable Cbor.uint))

let test_member_in_binary_record () =
  let a = Cbor.elem "a" Cbor.uint in
  rejects ~msg:"field a holds a CBOR member" (fun () ->
      Codec.v "Bin" Fun.id Codec.[ a $ Fun.id ]);
  let m = Cbor.mem "m" (`Int 1) Cbor.uint in
  let mapped = Field.v "m" (map ~decode:Fun.id ~encode:Fun.id (Field.typ m)) in
  rejects ~msg:"field m holds a CBOR member" (fun () ->
      Codec.v "Bin" Fun.id Codec.[ mapped $ Fun.id ])

let test_member_refined () =
  let a = Cbor.elem "a" Cbor.uint in
  rejects ~msg:"is a CBOR member, which takes no constraint" (fun () ->
      Field.v "a" ~constraint_:(Expr.bool true) (Field.typ a))

(* -- The codec -- *)

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
  let decoded = Codec.decode ~consume:`All c b ~off:0 in
  let (Buf.Any vb) = Buf.v b in
  let checked = Codec.View.is_ok (Codec.View.check c vb ~off:0) in
  let validated = Result.is_ok (Codec.validate c b ~off:0) in
  let consumed =
    match Codec.size_at c b ~off:0 with
    | Ok n -> n = String.length s
    | Error _ -> false
  in
  if consumed && (Result.is_ok decoded <> validated || validated <> checked)
  then Alcotest.failf "%s: decode, validate and View.check disagree" (to_hex s);
  decoded

(* [c] accepts [hex], and writes the value it reads back as [hex]. *)
let accepts c hex =
  match verdict c (of_hex hex) with
  | Error e -> Alcotest.failf "%s: refused (%a)" hex Error.pp e
  | Ok v ->
      Alcotest.(check string)
        (hex ^ " re-encodes") hex
        (to_hex (Codec.to_string c v))

let refuses c hex =
  match verdict c (of_hex hex) with
  | Ok _ -> Alcotest.failf "%s: accepted" hex
  | Error _ -> ()

let writes c v hex =
  Alcotest.(check string) (hex ^ " written") hex (to_hex (Codec.to_string c v))

let rejects_write ~msg c v = rejects ~msg (fun () -> Codec.to_string c v)
let u64 = UInt64.of_int64

let test_codec_primitives () =
  let uint = Cbor.codec Cbor.uint in
  List.iter (accepts uint)
    [ "00"; "17"; "1818"; "1903e8"; "1bffffffffffffffff" ];
  writes uint (u64 (-1L)) "1bffffffffffffffff";
  refuses uint "20";
  let nint = Cbor.codec Cbor.nint in
  accepts nint "20";
  writes nint (u64 0L) "20";
  refuses nint "00";
  let int = Cbor.codec Cbor.int in
  accepts int "3bffffffffffffffff";
  writes int (Cbor.Nint (u64 9L)) "29";
  writes int (Cbor.Uint (u64 10L)) "0a";
  refuses int "40";
  let range = Cbor.codec (Cbor.range ~min:(-10L) ~max:10L) in
  accepts range "29";
  accepts range "0a";
  refuses range "0b";
  refuses range "2a";
  writes range (-10L) "29";
  rejects_write ~msg:"11 is outside -10..10" range 11L;
  let wide = Cbor.codec (Cbor.range ~min:Int64.min_int ~max:Int64.max_int) in
  accepts wide "3b7fffffffffffffff";
  refuses wide "3b8000000000000000";
  refuses wide "1b8000000000000000";
  let bytes = Cbor.codec Cbor.bytes in
  accepts bytes "40";
  accepts bytes "43010203";
  refuses bytes "6161";
  let text = Cbor.codec Cbor.text in
  accepts text "6161";
  refuses text "61ff";
  rejects_write ~msg:"is not UTF-8" text "\xff";
  let bool = Cbor.codec Cbor.bool in
  accepts bool "f4";
  accepts bool "f5";
  refuses bool "f6";
  let null = Cbor.codec Cbor.null in
  accepts null "f6";
  refuses null "f7";
  let minus_one = Cbor.codec (Cbor.literal (`Int (-1))) in
  accepts minus_one "20";
  refuses minus_one "21";
  let ab = Cbor.codec (Cbor.literal (`Text "ab")) in
  accepts ab "626162";
  refuses ab "626163";
  refuses ab "426162";
  let any = Cbor.codec Cbor.any in
  List.iter (accepts any) [ "a10102"; "c11a514b67b0"; "f0"; "f820"; "f7"; "80" ];
  rejects_write ~msg:"is not one deterministic data item" any "\x18\x01";
  rejects_write ~msg:"is not one deterministic data item" any "\x00\x00"

let test_codec_constructed () =
  let size = Cbor.codec (Cbor.size ~min:1 ~max:2 Cbor.bytes) in
  accepts size "4101";
  accepts size "420102";
  refuses size "40";
  refuses size "43010203";
  rejects_write ~msg:"3 bytes are outside .size (1..2)" size "abc";
  let tag = Cbor.codec (Cbor.tag 24 Cbor.uint) in
  accepts tag "d81801";
  refuses tag "d81901";
  refuses tag "01";
  let array = Cbor.codec (Cbor.array ~non_empty:true Cbor.uint) in
  accepts array "820102";
  refuses array "80";
  refuses array "8201";
  refuses array "82010203";
  rejects_write ~msg:"an empty list for a non-empty array" array [];
  let assoc = Cbor.codec (Cbor.assoc Cbor.uint Cbor.text) in
  writes assoc [ (u64 2L, "b"); (u64 1L, "a") ] "a2016161026162";
  accepts assoc "a2016161026162";
  refuses assoc "a2026162016161";
  refuses assoc "a2016161016162";
  rejects_write ~msg:"two map entries share a key" assoc
    [ (u64 1L, "a"); (u64 1L, "b") ];
  let pair =
    Cbor.codec
      (Cbor.tuple "Pair"
         (fun a b -> (a, b))
         Codec.
           [ Cbor.elem "a" Cbor.uint $ fst; Cbor.elem_opt "b" Cbor.text $ snd ])
  in
  accepts pair "82016161";
  accepts pair "8101";
  writes pair (u64 1L, None) "8101";
  refuses pair "80";
  refuses pair "820102";
  refuses pair "83016161f6";
  let choice =
    Cbor.codec
      Cbor.(
        choice
          [
            case "n" uint
              ~inject:(fun n -> `N n)
              ~project:(function `N n -> Some n | `S _ -> None);
            case "s" text
              ~inject:(fun s -> `S s)
              ~project:(function `S s -> Some s | `N _ -> None);
          ])
  in
  accepts choice "01";
  accepts choice "6161";
  refuses choice "40";
  let nullable = Cbor.codec (Cbor.nullable Cbor.uint) in
  accepts nullable "f6";
  accepts nullable "01";
  writes nullable None "f6";
  refuses nullable "f5";
  let kept = Cbor.codec (Cbor.keep_bytes Cbor.uint) in
  (match Codec.decode kept (Bytes.of_string (of_hex "1818")) ~off:0 with
  | Ok (n, b) ->
      Alcotest.(check int64) "value" 24L (UInt64.to_int64 n);
      Alcotest.(check string) "bytes" "1818" (to_hex b)
  | Error e -> Alcotest.failf "keep_bytes: %a" Error.pp e);
  writes kept (u64 1L, "ignored") "01";
  let mapped =
    Cbor.codec
      (Cbor.map ~decode:String.length
         ~encode:(fun n -> String.make n 'x')
         Cbor.text)
  in
  accepts mapped "627878";
  writes mapped 1 "6178"

(* [encoded]: the content of the byte string is exactly one deterministic
   item of the type. *)
let test_codec_encoded () =
  let enc = Cbor.codec (Cbor.encoded Cbor.uint) in
  accepts enc "4101";
  accepts enc "421818";
  writes enc (u64 24L) "421818";
  refuses enc "40";
  refuses enc "420101";
  refuses enc "421801";
  refuses enc "4118";
  refuses enc "416161";
  refuses enc "01";
  match Codec.decode enc (Bytes.of_string (of_hex "420101")) ~off:0 with
  | Error { kind = Trailing_bytes 1; at = 2; _ } -> ()
  | Error e -> Alcotest.failf "trailing in encoded: %a" Error.pp e
  | Ok _ -> Alcotest.fail "trailing in encoded: accepted"

(* Every rule of deterministic encoding (docs/cbor.md) is a decode error. *)
let test_codec_deterministic () =
  let any = Cbor.codec Cbor.any in
  let uint = Cbor.codec Cbor.uint in
  (* an argument that fits in a shorter head *)
  refuses uint "1817";
  refuses uint "190017";
  refuses uint "1a000000ff";
  refuses uint "1b00000000ffffffff";
  refuses any "5800";
  refuses any "980100";
  (* indefinite lengths, reserved additional information *)
  refuses any "9fff";
  refuses any "5f40ff";
  refuses any "1c";
  (* floats, and a two-byte simple value below 32 *)
  refuses any "f93c00";
  refuses any "fa3f800000";
  refuses any "f801";
  (* map keys out of order, and twice *)
  refuses any "a202000100";
  refuses any "a201000100";
  refuses any "a2616201616100";
  (* text that is not UTF-8 *)
  refuses any "62c0af"

(* [c] accepts [hex] and refuses every cut of it, never raising. *)
let cuts c hex =
  let s = of_hex hex in
  accepts c hex;
  for i = 0 to String.length s - 1 do
    match verdict c (String.sub s 0 i) with
    | Ok _ -> Alcotest.failf "%s cut at %d accepted" hex i
    | Error _ -> ()
  done

let test_codec_errors () =
  let c = Rfc9052.sign1_codec in
  let hex =
    "d28445a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
  in
  let s = of_hex hex in
  cuts c hex;
  cuts Rfc9052.key_codec "a3010120062100";
  cuts (Cbor.codec (Cbor.encoded Cbor.text)) "426161";
  cuts (Cbor.codec (Cbor.assoc Cbor.text Cbor.any)) "a2616101616280";
  cuts (Cbor.codec Cbor.uint) "1bffffffffffffffff";
  (match Codec.decode c (Bytes.of_string (String.sub s 0 10)) ~off:0 with
  | Error { kind = Unexpected_eof _; _ } -> ()
  | Error e -> Alcotest.failf "cut: %a" Error.pp e
  | Ok _ -> Alcotest.fail "cut: accepted");
  (* a byte after the item: refused when the item must fill the input *)
  let b = Bytes.of_string (s ^ "\x00") in
  (match Codec.decode ~consume:`All c b ~off:0 with
  | Error { kind = Trailing_bytes 1; _ } -> ()
  | Error e -> Alcotest.failf "trailing: %a" Error.pp e
  | Ok _ -> Alcotest.fail "trailing: accepted");
  Alcotest.(check bool) "prefix" true (Result.is_ok (Codec.decode c b ~off:0));
  (* every single-byte change is an answer, never an exception *)
  for i = 0 to String.length s - 1 do
    let b = Bytes.of_string s in
    Bytes.set b i (Char.chr (Char.code s.[i] lxor 0x80));
    match verdict c (Bytes.to_string b) with Ok _ | Error _ -> ()
  done;
  (* an error under a member is named by it *)
  match Codec.decode c (Bytes.of_string (of_hex "d28440a10507f640")) ~off:0 with
  | Error { field = [ "unprotected"; "iv" ]; _ } -> ()
  | Error e -> Alcotest.failf "path: %a" Error.pp e
  | Ok _ -> Alcotest.fail "iv of the wrong type accepted"

(* -- Views -- *)

let test_codec_views () =
  let f_kid = Cbor.mem_opt "kid" (`Int 4) Cbor.bytes in
  let f_n = Cbor.mem "n" (`Text "n") Cbor.uint in
  let c =
    Cbor.codec
      (Cbor.tag 6
         (Cbor.record "R"
            (fun kid n -> (kid, n))
            Codec.[ f_kid $ fst; f_n $ snd ]))
  in
  let bytes = Bytes.of_string (of_hex "c6a20441aa616e05") in
  let (Buf.Any b) = Buf.v bytes in
  let k = Codec.View.check c b ~off:0 in
  Alcotest.(check bool) "checked" true (Codec.View.is_ok k);
  let v = Codec.View.get k in
  let kid = Codec.View.field c Codec.(f_kid $ fst) in
  let n = Codec.View.field c Codec.(f_n $ snd) in
  Alcotest.(check (option string)) "kid" (Some "\xaa") (Codec.View.read b v kid);
  Alcotest.(check int64) "n" 5L (UInt64.to_int64 (Codec.View.read b v n));
  rejects ~msg:"has no constant place" (fun () ->
      Codec.View.set b v n (UInt64.of_int64 6L));
  let other = Cbor.mem "other" (`Int 9) Cbor.uint in
  rejects ~msg:"does not belong to codec" (fun () ->
      Codec.View.field c Codec.(other $ snd));
  (* bytes written after the check that no longer decode are misuse *)
  Bytes.set bytes 7 '\x18';
  rejects ~msg:"no longer decodes" (fun () -> Codec.View.read b v n);
  (* a refused check *)
  let bad = Bytes.of_string (of_hex "c6a2616e050441aa") in
  let (Buf.Any b) = Buf.v bad in
  let k = Codec.View.check c b ~off:0 in
  Alcotest.(check bool) "unsorted refused" false (Codec.View.is_ok k)

(* -- Nesting -- *)

let test_cbor_in_binary () =
  let len = Field.v "len" uint8 in
  let body =
    Field.v "body"
      (nested ~size:(Field.ref len) (codec (Cbor.codec (Cbor.array Cbor.uint))))
  in
  let c =
    Codec.v "Framed" (fun l b -> (l, b)) Codec.[ len $ fst; body $ snd ]
  in
  let frame hex = Bytes.of_string (of_hex hex) in
  (match Codec.decode c (frame "03820102") ~off:0 with
  | Ok (_, l) -> Alcotest.(check int) "elements" 2 (List.length l)
  | Error e -> Alcotest.failf "framed: %a" Error.pp e);
  Alcotest.(check bool)
    "region not consumed" true
    (Result.is_error (Codec.validate c (frame "0482010200") ~off:0));
  Alcotest.(check bool)
    "region cut" true
    (Result.is_error (Codec.validate c (frame "02820102") ~off:0));
  Alcotest.(check bool)
    "not deterministic" true
    (Result.is_error (Codec.validate c (frame "0482181702") ~off:0))

(* The binary half of the same codec projects to 3D, the CBOR value a byte
   region of its declared size: 3D checks the framing, evercddl the CBOR held
   in it. *)
let framed_3d =
  {|entrypoint
typedef struct WireFramed
{
   UINT8 len;
   UINT8 body[:byte-size len];
} Framed;

|}

let test_cbor_in_binary_3d () =
  let len = Field.v "len" uint8 in
  let body =
    Field.v "body"
      (nested ~size:(Field.ref len) (codec (Cbor.codec (Cbor.array Cbor.uint))))
  in
  let c =
    Codec.v "Framed" (fun l b -> (l, b)) Codec.[ len $ fst; body $ snd ]
  in
  Alcotest.(check string)
    "3D" framed_3d
    (Private.Everparse.Raw.to_3d
       (Private.Everparse.module_ (Everparse.project ~mode:`Standalone c)))

let test_cbor_outside_nested () =
  let cbor = codec (Cbor.codec Cbor.bool) in
  rejects ~msg:"holds a CBOR codec outside nested ~size" (fun () ->
      Codec.v "Bare" Fun.id Codec.[ Field.v "b" cbor $ Fun.id ]);
  rejects ~msg:"holds a CBOR codec outside nested ~size" (fun () ->
      Codec.v "AtMost" Fun.id
        Codec.[ Field.v "b" (nested_at_most ~size:(int 1) cbor) $ Fun.id ]);
  rejects ~msg:"holds a CBOR codec outside nested ~size" (fun () ->
      Codec.v "Opt" Fun.id
        Codec.[ Field.optional "b" ~present:(bool true) cbor $ Fun.id ]);
  builds (fun () ->
      Codec.v "Nested" Fun.id
        Codec.[ Field.v "b" (nested ~size:(int 1) cbor) $ Fun.id ])

(* A DER or CBOR codec in a byte string: no backend would check it there. *)
let test_containing_der () =
  let der = codec (Asn1.codec Asn1.bool) in
  rejects ~msg:"a DER codec in a byte string has no projection" (fun () ->
      Cbor.containing der);
  rejects ~msg:"a DER codec in a byte string has no projection" (fun () ->
      Cbor.containing (map ~decode:not ~encode:not der))

let test_containing_cbor () =
  rejects ~msg:"Cbor.encoded is the byte string evercddl checks" (fun () ->
      Cbor.containing (codec (Cbor.codec Cbor.uint)))

(* Binary in CBOR: a byte string whose content the binary codec checks, in
   full. *)
let test_binary_in_cbor () =
  let version =
    Field.v "version" ~self_constraint:Expr.(fun v -> v = int 1) uint8
  in
  let length = Field.v "length" uint16be in
  let hdr =
    Codec.v "Hdr" (fun v l -> (v, l)) Codec.[ version $ fst; length $ snd ]
  in
  let c = Cbor.codec (Cbor.containing (codec hdr)) in
  accepts c "43010007";
  writes c (UInt8.v 1, UInt16.v 7) "43010007";
  refuses c "43020007";
  refuses c "4401000700";
  refuses c "420100";
  rejects_write ~msg:"rejected by its own constraints" c (UInt8.v 2, UInt16.v 7)

(* CBOR in DER: an OCTET STRING whose content the CBOR codec checks, in
   full. *)
let test_cbor_in_der () =
  let d = Asn1.codec (Asn1.containing (codec (Cbor.codec Cbor.uint))) in
  (match
     Codec.decode ~consume:`All d (Bytes.of_string (of_hex "04021818")) ~off:0
   with
  | Ok n -> Alcotest.(check int64) "uint" 24L (UInt64.to_int64 n)
  | Error e -> Alcotest.failf "CBOR in DER: %a" Error.pp e);
  Alcotest.(check bool)
    "CBOR in DER not deterministic" true
    (Result.is_error
       (Codec.decode ~consume:`All d
          (Bytes.of_string (of_hex "04021801"))
          ~off:0));
  Alcotest.(check bool)
    "CBOR in DER trailing" true
    (Result.is_error
       (Codec.decode ~consume:`All d
          (Bytes.of_string (of_hex "04020101"))
          ~off:0))

(* -- COSE (examples/cose) -- *)

(* COSE_Sign1_Tagged messages with the verdict of the C evercddl generates
   from EverParse's src/cose/cose-sign.cddl (docs/cbor.md, Feasibility run),
   through the harness of that run, which calls
   COSE_Format_validate_and_parse_cose_sign1_tagged and accepts on [Some]
   with an empty remainder. *)

(* ocaml-cose's test/interop/pycose/traces/sign1.csv, column
   cose_sign1_hex, and ocaml-cose's test/fixtures/ecdsa-sig-01.json, the
   cose-wg example ECDSA-01. pycose writes the protected header in insertion
   order, so the three messages whose keys are not 1, 3, 4 in that order are
   not deterministic. *)
let cose_traces =
  [
    ( "es256_hello",
      true,
      "d28443a10126a04b68656c6c6f20776f726c645840ea0424d8ae8d9adedfe4eb7d1f765471c8beeafb0bbab2cfa0a4ff854b3abcbb816d8d3c11f9e2e05cac6a47ca90c2570e7cc689e67c4f75f1d3a0e30f874156"
    );
    ( "es256_with_ct",
      true,
      "d284581ea201260378186170706c69636174696f6e2f6f637465742d73747265616da054546869732069732074686520636f6e74656e742e5840d5f38e55c708df2cb1d5faba71888125857a55d0e954246967a19a2cac26a842198194df0eda0ed49e84ce62e3c930b44c24e84439f6f509e5f1d69ea05a4904"
    );
    ( "es256_with_kid",
      true,
      "d2844aa2012604456b65792d31a0507061796c6f61642077697468206b69645840637cfe40f6388b7aeea942efbfe19102589325faf273f1967c1a3b73c224f63b0b229ab983ab22c9605c8f28251686101d038f4552a25beada1188824c8800a4"
    );
    ( "es256_with_aad",
      true,
      "d28443a10126a04861616420746573745840a534844e897c96ea84fe8df932e3d4a327f2fb96183e471d201c72d1fb368c7f1f64c5b6fd927154aebfcf3877610dbcd78ab27e014537d3d1dc0391d5590cc7"
    );
    ( "es256_full",
      false,
      "d2845825a30126044e6d792d7369676e696e672d6b657903706170706c69636174696f6e2f63626f72a05066756c6c20686561646572207465737458408d43ed0092b1d94065cd54daba6f7703c4937f94c80e09674b88d8a99d6a74de272d1f7502c6896931ae85e2405f3d178e2260e4f252e338796ec55fa3eaebbe"
    );
    ( "es256_empty_payload",
      true,
      "d28443a10126a040584020d0e37ad656071b5213f76098cfa64293454e815e82f361710a2f1f06aca22759bc123c1862f43ddb7a1c579dd51855048caa8b4b14c39c04dacda9af545910"
    );
    ( "es256_binary",
      true,
      "d28443a10126a0590100000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f505152535455565758595a5b5c5d5e5f606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeafb0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff584029c853eab02b3e29a8a10f968179ba8bb751c8d6904feddd4252010078688297caaefc24b7f1ad945aaa29f73a835ef0c93e9541e6fb60e315b8254e70f87b33"
    );
    ( "es384_hello",
      true,
      "d28444a1013822a04b68656c6c6f2065733338345860b12a83235f7a060107f8b8b218db2f8deca6a25a6611ae7b39c71fbf7734a5bbec41a3bd4f305264651930b86b9ea47c31304c81091d92d3397912b532ae77ee978906dca387e37b6d277bee627d6dbf92e3d6c6be3d2fd48cb899227ff0f77a"
    );
    ( "es512_hello",
      true,
      "d28444a1013823a04b68656c6c6f20657335313258840040cc5af56cdc5329f63de7c815b351ad1154e1ea6607c1ce33c28c886bbba12301e1db755affa620c78eb06602c6874dcc293e0196b0099c95cf2b1cf42152901a015ae0234b8dcd60c07f647dbaf960d8d6f17f3b18773ad7cb750f7b73f06de1e56ee35e13d6d23bfe36d61a80b397bdbb1732f77fd274ec09e5bd3a2de5b9bea81f"
    );
    ( "eddsa_hello",
      true,
      "d28443a10127a04b68656c6c6f20656464736158402edce4560db0cec820aef0c4a927cda8b92d3dcd4fd1709a5c790ce1d5fa10746fc5fbccb80d25e6a598e33db9671858c30c5b1d17edd68d983f648b2bf43009"
    );
    ( "eddsa_full",
      false,
      "d2845822a30127044b65642d7369676e2d6b657903706170706c69636174696f6e2f6a736f6ea04f65646473612066756c6c20746573745840ba90a29d688dc36c9f9cb3c6448a673e600a312167de744961caf16c5de03918b912b571794f792fb4c444cd8c4dd9f17aa58d4f36fe0572b1d7263bd3af7b0b"
    );
    ( "eddsa_with_aad",
      true,
      "d28443a10127a04965646473612061616458401ebb8b49affe2af0e8ec8d687e1a32da6e52cd584b6dacf4e9541483b377b9cda0b164a6c75bc3f871d957292923fb15b5f1d43c27a5857d48bffcb34e17d808"
    );
    ( "es256_large",
      true,
      "d28443a10126a05903e84141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414141414158405fc3b9c4057139ff86502077e0d45e29f2d8d8fce12f6c225f7fb36462faf7b9f24d88086c19565b41e5c835ad37e29dd5d9af47f16df5a979e3637ae4a92b28"
    );
    ( "es256_sigstore",
      false,
      "d284583ca30126044c73696773746f72652d6b65790378286170706c69636174696f6e2f766e642e6465762e73696773746f72652e62756e646c652b6a736f6ea058387b226d6564696154797065223a226170706c69636174696f6e2f766e642e6465762e73696773746f72652e62756e646c652b6a736f6e227d584042cec3aac5529449fd2e8ee78170384128c86e212a1058170739d2f61a67288dfca84ab98dd91574852489bbb7e535f0ad84ec1d47cff4b38a1eaf46de224b38"
    );
    ( "ecdsa-01",
      true,
      "d28445a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
    );
  ]

(* Variants of ECDSA-01 and messages around each rule of the header map,
   well-formed CBOR all, each with the C's verdict. *)
let cose_probes =
  [
    ( "neg-long-length",
      false,
      "d28445a201260300a10442313154546869732069732074686520636f6e74656e742e5900406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
    );
    ( "neg-long-tag",
      false,
      "d8128445a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
    );
    ( "neg-indefinite-array",
      false,
      "d29f45a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380bff"
    );
    ( "neg-unsorted-protected",
      false,
      "d28445a203000126a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
    );
    ( "neg-trailing-byte",
      false,
      "d28445a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b00"
    );
    ("kid_int", true, "d28440a10407f640");
    ("kid_bstr", true, "d28440a1044100f640");
    ("alg_bstr", true, "d28440a10140f640");
    ("alg_large_uint", true, "d28440a1011bfffffffffffffffff640");
    ("alg_large_nint", true, "d28440a1013bfffffffffffffffff640");
    ("alg_and_unknown", true, "d28440a20126182a01f640");
    ("crit_empty", true, "d28440a10280f640");
    ("iv_only", true, "d28440a10540f640");
    ("iv_and_partial_iv", false, "d28440a2054006404040");
    ("iv_int", false, "d28440a10507f640");
    ("partial_iv_int", false, "d28440a10607f640");
    ("iv_bstr_partial_iv_int", false, "d28440a205400607f640");
    ("iv_int_partial_iv_bstr", false, "d28440a205070640f640");
    ("unknown_tstr_key", true, "d28440a1616101f640");
    ("unknown_bstr_key", false, "d28440a1410101f640");
    ("unknown_tag", true, "d28440a1182ac100f640");
    ("unknown_simple", true, "d28440a1182af0f640");
    ("unknown_simple_two_bytes", true, "d28440a1182af820f640");
    ("unknown_undefined", true, "d28440a1182af7f640");
    ("unknown_float", false, "d28440a101f93c00f640");
    ("key_not_utf8", false, "d28440a161ff01f640");
    ("protected_empty_map", true, "d28441a0a0f640");
    ("protected_trailing", false, "d28442a000a0f640");
    ("unprotected_unsorted", false, "d28440a2040103014040");
    ("protected_kid", true, "d2844aa2012604456b65792d31a0f640");
  ]

let test_cose_vectors () =
  List.iter
    (fun (name, ok, hex) ->
      match (verdict Rfc9052.sign1_codec (of_hex hex), ok) with
      | Ok v, true ->
          Alcotest.(check string)
            (name ^ " re-encodes") hex
            (to_hex (Codec.to_string Rfc9052.sign1_codec v))
      | Error _, false -> ()
      | Ok _, false -> Alcotest.failf "%s: accepted, the C refuses it" name
      | Error e, true ->
          Alcotest.failf "%s: refused (%a), the C accepts it" name Error.pp e)
    (cose_traces @ cose_probes)

let test_cose_ecdsa_01 () =
  let hex =
    "d28445a201260300a10442313154546869732069732074686520636f6e74656e742e58406520bbaf2081d7e0ed0f95f76eb0733d667005f7467cec4b87b9381a6ba1ede8e00df29f32a37230f39a842a54821fdd223092819d7728efb9d3a0080b75380b"
  in
  match
    Codec.decode Rfc9052.sign1_codec (Bytes.of_string (of_hex hex)) ~off:0
  with
  | Error e -> Alcotest.failf "ECDSA-01: %a" Error.pp e
  | Ok m -> (
      Alcotest.(check (option string))
        "payload" (Some "This is the content.") m.payload;
      Alcotest.(check int) "signature" 64 (String.length m.signature);
      Alcotest.(check (option string)) "kid" (Some "11") m.unprotected.kid;
      match m.protected with
      | Some (h, bytes) -> (
          Alcotest.(check string)
            "protected bytes" "45a201260300" (to_hex bytes);
          (match h.alg with
          | Some (Rfc9052.Int (Cbor.Nint n)) ->
              Alcotest.(check int64) "alg ES256" 6L (UInt64.to_int64 n)
          | _ -> Alcotest.fail "alg");
          match h.content_type with
          | Some (Rfc9052.Int (Cbor.Uint n)) ->
              Alcotest.(check int64) "content type" 0L (UInt64.to_int64 n)
          | _ -> Alcotest.fail "content type")
      | None -> Alcotest.fail "protected")

(* The Ed25519 key of RFC 8032 section 7.1, TEST 1, as a COSE_Key: kty 1
   (OKP), crv 6 (Ed25519), x and d (RFC 9053 section 7.2), and a kid, which
   only the other entries take. *)
let test_cose_key () =
  let x = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a" in
  let d = "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60" in
  let key =
    {
      Rfc9052.crv = Rfc9052.Int (Cbor.Uint (u64 6L));
      x = Some (of_hex x);
      d = Some (of_hex d);
      key_others = [ (Rfc9052.Int (Cbor.Uint (u64 2L)), of_hex "423131") ];
    }
  in
  let hex = "a50101024231312006215820" ^ x ^ "235820" ^ d in
  writes Rfc9052.key_codec key hex;
  accepts Rfc9052.key_codec hex;
  (* kty 2 (EC2) is not an OKP key *)
  refuses Rfc9052.key_codec "a201022006";
  refuses Rfc9052.key_codec "a10102";
  accepts Rfc9052.key_codec "a201012006";
  (* an x that is not a byte string is another entry *)
  accepts Rfc9052.key_codec "a3010120062100";
  rejects_write ~msg:"is one another member reads" Rfc9052.key_codec
    {
      key with
      x = None;
      key_others = [ (Rfc9052.Int (Cbor.Nint (u64 1L)), of_hex "4100") ];
    }

let suite =
  ( "cbor",
    [
      Alcotest.test_case "COSE_Sign1 builds" `Quick test_cose_sign1_builds;
      Alcotest.test_case "x5chain builds" `Quick test_x5chain_builds;
      Alcotest.test_case "primitives" `Quick test_primitives;
      Alcotest.test_case "size" `Quick test_size;
      Alcotest.test_case "tag" `Quick test_tag;
      Alcotest.test_case "array" `Quick test_array;
      Alcotest.test_case "tuple" `Quick test_tuple;
      Alcotest.test_case "record" `Quick test_record;
      Alcotest.test_case "assoc" `Quick test_assoc;
      Alcotest.test_case "choice" `Quick test_choice;
      Alcotest.test_case "nullable" `Quick test_nullable;
      Alcotest.test_case "encoded, containing, keep_bytes, map" `Quick
        test_bytes;
      Alcotest.test_case "reject: empty range" `Quick test_range_empty;
      Alcotest.test_case "reject: size on a non-string" `Quick
        test_size_not_string;
      Alcotest.test_case "reject: negative size" `Quick test_size_negative;
      Alcotest.test_case "reject: empty size" `Quick test_size_empty;
      Alcotest.test_case "reject: literal not ASCII" `Quick
        test_literal_not_ascii;
      Alcotest.test_case "reject: literal too long" `Quick test_literal_too_long;
      Alcotest.test_case "reject: key not ASCII" `Quick test_key_not_ascii;
      Alcotest.test_case "reject: key too long" `Quick test_key_too_long;
      Alcotest.test_case "reject: literal not printable" `Quick
        test_literal_unprintable;
      Alcotest.test_case "reject: negative tag" `Quick test_tag_negative;
      Alcotest.test_case "reject: foreign field in a tuple" `Quick
        test_tuple_foreign_field;
      Alcotest.test_case "reject: repeated entry" `Quick
        test_tuple_repeated_name;
      Alcotest.test_case "reject: empty tuple" `Quick test_tuple_empty;
      Alcotest.test_case "reject: ambiguous tuple" `Quick test_tuple_ambiguous;
      Alcotest.test_case "reject: path in a tuple" `Quick test_tuple_path;
      Alcotest.test_case "reject: foreign field in a record" `Quick
        test_record_foreign_field;
      Alcotest.test_case "reject: repeated member" `Quick
        test_record_repeated_name;
      Alcotest.test_case "reject: shared key" `Quick test_record_shared_key;
      Alcotest.test_case "reject: two keep_unknown" `Quick
        test_record_two_keep_unknown;
      Alcotest.test_case "reject: exclusive of one" `Quick
        test_exclusive_too_few;
      Alcotest.test_case "reject: exclusive repeated key" `Quick
        test_exclusive_repeated_key;
      Alcotest.test_case "reject: empty choice" `Quick test_choice_empty;
      Alcotest.test_case "reject: repeated case" `Quick
        test_choice_repeated_name;
      Alcotest.test_case "reject: cases that meet" `Quick test_choice_meet;
      Alcotest.test_case "reject: two map cases" `Quick test_choice_two_maps;
      Alcotest.test_case "reject: nullable of nil" `Quick
        test_nullable_meets_nil;
      Alcotest.test_case "reject: member in a binary record" `Quick
        test_member_in_binary_record;
      Alcotest.test_case "reject: refined member" `Quick test_member_refined;
      Alcotest.test_case "reject: CBOR codec outside nested" `Quick
        test_cbor_outside_nested;
      Alcotest.test_case "reject: DER codec under containing" `Quick
        test_containing_der;
      Alcotest.test_case "reject: CBOR codec under containing" `Quick
        test_containing_cbor;
      Alcotest.test_case "codec: primitives" `Quick test_codec_primitives;
      Alcotest.test_case "codec: constructed" `Quick test_codec_constructed;
      Alcotest.test_case "codec: encoded" `Quick test_codec_encoded;
      Alcotest.test_case "codec: deterministic" `Quick test_codec_deterministic;
      Alcotest.test_case "codec: errors" `Quick test_codec_errors;
      Alcotest.test_case "codec: views" `Quick test_codec_views;
      Alcotest.test_case "nesting: CBOR in binary" `Quick test_cbor_in_binary;
      Alcotest.test_case "nesting: CBOR in binary, 3D" `Quick
        test_cbor_in_binary_3d;
      Alcotest.test_case "nesting: binary in CBOR" `Quick test_binary_in_cbor;
      Alcotest.test_case "nesting: CBOR in DER" `Quick test_cbor_in_der;
      Alcotest.test_case "COSE: vectors" `Quick test_cose_vectors;
      Alcotest.test_case "COSE: ECDSA-01" `Quick test_cose_ecdsa_01;
      Alcotest.test_case "COSE: key" `Quick test_cose_key;
    ] )
