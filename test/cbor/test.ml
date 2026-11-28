(* Tests for the projection of CBOR types to evercddl modules: the printed
   .cddl of COSE_Sign1, COSE_Key and one module per construct of docs/cbor.md
   ("Constructs and their projections"), the binary codecs under a
   containing, and what the projection refuses. The modules are the ones
   evercddl reads (docs/cbor.md, Running everparse-cddl from the prefix). *)

open Wire

let rejects ~msg f =
  match f () with
  | _ -> Alcotest.failf "expected Invalid_argument mentioning %S" msg
  | exception Invalid_argument m ->
      if not (Re.execp (Re.compile (Re.str msg)) m) then
        Alcotest.failf "Invalid_argument %S does not mention %S" m msg

let printed s = Fmt.str "%a" Cbor.pp s
let contained (Cbor_modules.Packed t) = Cbor.contained (Cbor.codec t)

(* -- The printed modules -- *)

let cose_sign1_tagged_cddl =
  {|header_map = {
  ? 1 => int / tstr,
  ? 2 => [+ int / tstr],
  ? 3 => int / tstr,
  ? 4 => bstr,
  ((5 => bstr, ? 6 : everparse-no-match) //
   (6 => bstr, ? 5 : everparse-no-match) //
   (? 5 : everparse-no-match, ? 6 : everparse-no-match)),
  * (int / tstr) => any
}
COSE_Sign1 = [
  protected : bstr .everparse-det-cbor header_map / bstr .size 0,
  unprotected : header_map,
  payload : bstr / nil,
  signature : bstr
]
COSE_Sign1_Tagged = #6.18(COSE_Sign1)
|}

let cose_key_cddl =
  {|COSE_Key_OKP = {
  1 => 1,
  -1 => int / tstr,
  ? -2 => bstr,
  ? -4 => bstr,
  * (int / tstr) => any
}
|}

let cbor_uint_cddl = {|uints = [
  u : uint,
  n : nint
]
|}

let cbor_int_cddl = {|Cbor_int = int
|}

let cbor_range_cddl =
  {|ranges = [
  neg : -10..-1,
  pos : 0..255,
  both : -128..127
]
|}

let cbor_strings_cddl = {|strings = [
  b : bstr,
  t : tstr
]
|}

let cbor_simple_cddl = {|simple = [
  flag : bool,
  nothing : nil
]
|}

let cbor_literal_cddl =
  {|literals = [
  one : 1,
  minus_one : -1,
  context : "Signature1"
]
|}

let cbor_any_cddl = {|Cbor_any = any
|}

let cbor_size_cddl =
  {|sizes = [
  empty : bstr .size 0,
  name : tstr .size (1..64),
  hash : bstr .size 32
]
|}

let cbor_tag_cddl = {|Cbor_tag = #6.42(bstr)
|}

let cbor_array_cddl = {|arrays = [
  any : [* uint],
  some : [+ tstr]
]
|}

let cbor_tuple_cddl = {|entries = [
  a : uint,
  ? b : tstr,
  c : bstr
]
|}

let cbor_assoc_cddl = {|Cbor_assoc = { * tstr => uint }
|}

let cbor_record_cddl =
  {|members = {
  1 => uint,
  ? -1 => tstr,
  "c" => bstr
}
|}

let cbor_exclusive_cddl =
  {|exclusive = {
  1 => uint,
  ((5 => bstr, ? 6 : everparse-no-match, ? "name" : everparse-no-match) //
   (6 => bstr, ? 5 : everparse-no-match, ? "name" : everparse-no-match) //
   ("name" => tstr, ? 5 : everparse-no-match, ? 6 : everparse-no-match) //
   (? 5 : everparse-no-match, ? 6 : everparse-no-match, ? "name" : everparse-no-match))
}
|}

let cbor_keep_unknown_cddl = {|open_map = {
  ? 1 => uint,
  * int => any
}
|}

let cbor_choice_cddl = {|Cbor_choice = uint / tstr / [* uint]
|}

let cbor_nullable_cddl = {|Cbor_nullable = tstr / nil
|}

let cbor_encoded_cddl =
  {|encoded = [
  number : bstr .everparse-det-cbor uint,
  either : bstr .everparse-det-cbor (uint / tstr)
]
|}

let cbor_containing_cddl = {|containing = [
  payload : bstr,
  word : bstr
]
|}

let cbor_keep_bytes_cddl =
  {|members = {
  1 => uint,
  ? -1 => tstr,
  "c" => bstr
}
|}

let cbor_map_cddl = {|Cbor_map = uint
|}

let clash_cddl =
  {|any_2 = {
  1 => uint
}
pair = [
  v : uint
]
pair_2 = [
  v : tstr
]
x_y_z = [
  v : bool
]
t_1st = [
  v : nil
]
Clash = [
  a_b : any_2,
  a_b_2 : pair,
  c : pair_2,
  d : x_y_z,
  e : t_1st
]
|}

let enveloped_cddl = {|Body = [
  payload : bstr,
  count : uint
]
|}

let framed_cddl =
  {|header_map = {
  ? 1 => int / tstr,
  ? 2 => [+ int / tstr],
  ? 3 => int / tstr,
  ? 4 => bstr,
  ((5 => bstr, ? 6 : everparse-no-match) //
   (6 => bstr, ? 5 : everparse-no-match) //
   (? 5 : everparse-no-match, ? 6 : everparse-no-match)),
  * (int / tstr) => any
}
COSE_Sign1 = [
  protected : bstr .everparse-det-cbor header_map / bstr .size 0,
  unprotected : header_map,
  payload : bstr / nil,
  signature : bstr
]
Framed_msg = #6.18(COSE_Sign1)
|}

let modules =
  [
    ("COSE_Sign1_Tagged", cose_sign1_tagged_cddl);
    ("COSE_Key", cose_key_cddl);
    ("Cbor_uint", cbor_uint_cddl);
    ("Cbor_int", cbor_int_cddl);
    ("Cbor_range", cbor_range_cddl);
    ("Cbor_strings", cbor_strings_cddl);
    ("Cbor_simple", cbor_simple_cddl);
    ("Cbor_literal", cbor_literal_cddl);
    ("Cbor_any", cbor_any_cddl);
    ("Cbor_size", cbor_size_cddl);
    ("Cbor_tag", cbor_tag_cddl);
    ("Cbor_array", cbor_array_cddl);
    ("Cbor_tuple", cbor_tuple_cddl);
    ("Cbor_assoc", cbor_assoc_cddl);
    ("Cbor_record", cbor_record_cddl);
    ("Cbor_exclusive", cbor_exclusive_cddl);
    ("Cbor_keep_unknown", cbor_keep_unknown_cddl);
    ("Cbor_choice", cbor_choice_cddl);
    ("Cbor_nullable", cbor_nullable_cddl);
    ("Cbor_encoded", cbor_encoded_cddl);
    ("Cbor_containing", cbor_containing_cddl);
    ("Cbor_keep_bytes", cbor_keep_bytes_cddl);
    ("Cbor_map", cbor_map_cddl);
    ("Clash", clash_cddl);
  ]

let check_module name expected () =
  let t = List.assoc name Cbor_modules.types in
  let s = Cbor_modules.project name t in
  Alcotest.(check string) "text" expected (printed s);
  Alcotest.(check string) "name" name (Cbor.name s);
  Alcotest.(check string) "filename" (name ^ ".cddl") (Cbor.filename s)

(* Every module of Cbor_modules is pinned here. *)
let test_every_module_pinned () =
  Alcotest.(check (list string))
    "modules"
    (List.map fst Cbor_modules.types)
    (List.map fst modules)

(* A binary codec holding CBOR projects to the module of the CBOR types it
   holds, an anonymous one named by the path of its field. *)
let test_framed () =
  let s = Cbor.project "Framed" Cbor_modules.framed in
  Alcotest.(check string) "text" framed_cddl (printed s);
  Alcotest.(check (list string))
    "no binary under a containing" []
    (List.map Everparse.name (Cbor.contained Cbor_modules.framed))

(* A DER codec holding CBOR: Asn1.project prints an OCTET STRING, the CBOR
   is Cbor.project's, named by its path, and the binary codec under it is
   Cbor.contained's, not Asn1.contained's. *)
let test_enveloped () =
  let s = Cbor.project "Enveloped" Cbor_modules.enveloped in
  Alcotest.(check string) "text" enveloped_cddl (printed s);
  Alcotest.(check (list string))
    "CBOR contents" [ "Payload" ]
    (List.map Everparse.name (Cbor.contained Cbor_modules.enveloped));
  Alcotest.(check (list string))
    "DER contents" []
    (List.map Everparse.name (Asn1.contained Cbor_modules.enveloped))

(* The projection is a function of the type: a second projection prints the
   same text. *)
let test_deterministic () =
  List.iter
    (fun (name, t) ->
      Alcotest.(check string)
        name
        (printed (Cbor_modules.project name t))
        (printed (Cbor_modules.project name t)))
    Cbor_modules.types

(* -- The binary codecs under a containing -- *)

let test_contained () =
  Alcotest.(check (list string))
    "binary contents"
    [ "Payload"; "containing_word_contents" ]
    (List.map Everparse.name (contained Cbor_modules.containing));
  Alcotest.(check (list string))
    "none in COSE_Sign1" []
    (List.map Everparse.name (contained Cbor_modules.cose_sign1))

(* A codec held twice is one schema. *)
let test_contained_once () =
  let payload = Cbor.containing (codec Cbor_modules.payload_codec) in
  let t =
    Cbor.tuple "Twice"
      (fun a b -> (a, b))
      Codec.
        [
          Cbor.elem "a" payload $ fst; Cbor.elem "b" (Cbor.tag 1 payload) $ snd;
        ]
  in
  Alcotest.(check (list string))
    "one schema" [ "Payload" ]
    (List.map Everparse.name (Cbor.contained (Cbor.codec t)))

(* -- What the projection refuses -- *)

let test_module_names () =
  List.iter
    (fun m ->
      rejects ~msg:"is not an evercddl module name" (fun () ->
          Cbor.project m (Cbor.codec Cbor.uint)))
    [ "cose"; "A.b"; "A-b"; "1A"; ""; "_A" ]

let test_codec_without_cbor () =
  rejects ~msg:"codec \"Payload\" holds no CBOR type" (fun () ->
      Cbor.project "Payload" Cbor_modules.payload_codec);
  Alcotest.(check int)
    "contained" 0
    (List.length (Cbor.contained Cbor_modules.payload_codec))

(* -- Writing -- *)

let test_write () =
  let dir = Filename.temp_dir "cbor" "" in
  let s = Cbor_modules.project "COSE_Key" Cbor_modules.cose_key in
  Cbor.write ~outdir:dir [ s; s ];
  let file = Filename.concat dir "COSE_Key.cddl" in
  let text = In_channel.with_open_bin file In_channel.input_all in
  Sys.remove file;
  Sys.rmdir dir;
  Alcotest.(check string) "file" cose_key_cddl text

let test_write_clash () =
  rejects ~msg:"two different modules are named \"M\"" (fun () ->
      Cbor.write ~outdir:"."
        [
          Cbor.project "M" (Cbor.codec Cbor.uint);
          Cbor.project "M" (Cbor.codec Cbor.text);
        ])

let suite =
  ( "cbor-project",
    List.map
      (fun (name, expected) ->
        Alcotest.test_case name `Quick (check_module name expected))
      modules
    @ [
        Alcotest.test_case "every module pinned" `Quick test_every_module_pinned;
        Alcotest.test_case "CBOR in a binary codec" `Quick test_framed;
        Alcotest.test_case "CBOR in a DER codec" `Quick test_enveloped;
        Alcotest.test_case "deterministic" `Quick test_deterministic;
        Alcotest.test_case "contained" `Quick test_contained;
        Alcotest.test_case "contained once" `Quick test_contained_once;
        Alcotest.test_case "module names" `Quick test_module_names;
        Alcotest.test_case "codec without CBOR" `Quick test_codec_without_cbor;
        Alcotest.test_case "write" `Quick test_write;
        Alcotest.test_case "write clash" `Quick test_write_clash;
      ] )

let () = Alcotest.run "wire-cbor" [ suite ]
