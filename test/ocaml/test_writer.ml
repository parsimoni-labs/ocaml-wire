(* The one module Wire_ocaml writes for a codec, whatever its shape: the
   members of a fixed record beside the folds, the check and the views, each
   agreeing with the others and with the codec; the names they take; what
   [ml] and [scanner] write for one codec. *)

open Wire
module Buf = Wire_runtime.Buf

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let rand = Random.State.make [| 11 |]

(* A record of [Schemas.checked], 5 bytes, whose first byte is often the
   version 4 and an IHL in range, so that many pass, in a buffer of a few more
   bytes at a random offset. *)
let input () =
  let len = Random.State.int rand 12 in
  let buf = Bytes.init len (fun _ -> Char.chr (Random.State.int rand 256)) in
  let off = Random.State.int rand (len + 1) in
  if off < len && Random.State.bool rand then
    Bytes.set_uint8 buf off (0x40 lor Random.State.int rand 16);
  if off + 2 < len && Random.State.bool rand then
    Bytes.set_uint16_be buf (off + 1) (Random.State.int rand 80);
  (buf, off)

(* The check, the fold and the accessors of the module of a fixed record
   accept what its record members accept and read what they read. *)
let test_fixed () =
  for _ = 1 to 2000 do
    let buf, off = input () in
    let len = Bytes.length buf - off in
    let valid = G_checked.is_valid buf off in
    let folded =
      G_checked.fold buf off len
        ~invalid:(fun () -> None)
        ~valid:(fun ~version ~ihl ~length ~ttl ~proto ->
          Some [ version; ihl; length; ttl; proto ])
    in
    match Buf.v buf with
    | Buf.Any b ->
        let length = Buf.length b in
        let c = G_checked.check b length ~off ~len in
        Alcotest.(check bool) "check" valid (G_checked.ok c);
        Alcotest.(check bool) "fold" valid (Option.is_some folded);
        if valid then begin
          let v = G_checked.get c in
          let r = G_checked.View.at b length off in
          let read =
            [
              G_checked.version b v;
              G_checked.ihl b v;
              G_checked.length b v;
              G_checked.ttl b v;
              G_checked.proto b v;
            ]
          and view =
            [
              (G_checked.view_version b r :> int);
              (G_checked.view_ihl b r :> int);
              UInt16.to_int (G_checked.view_length b r);
              UInt8.to_int (G_checked.view_ttl b r);
              UInt8.to_int (G_checked.view_proto b r);
            ]
          in
          Alcotest.(check (list int)) "accessors" view read;
          Alcotest.(check (option (list int))) "fold" (Some view) folded
        end
  done

type codec = Codec : 'r Codec.t -> codec

(* [ml] and [scanner] write one module for one codec. *)
let test_one_module () =
  List.iter
    (fun (name, Codec c) ->
      Alcotest.(check string)
        name
        (Wire_ocaml.ml ~codec:"c" c)
        (Wire_ocaml.scanner ~codec:"c" c);
      Alcotest.(check string)
        (name ^ " interface")
        (Wire_ocaml.mli ~codec:"c" c)
        (Wire_ocaml.scanner_mli ~codec:"c" c))
    [
      ("ipv4", Codec Schemas.ipv4);
      ("checked", Codec Schemas.checked);
      ("packed", Codec Schemas.packed);
    ]

let pair n1 n2 =
  Codec.v "Pair"
    (fun a b -> (a, b))
    Codec.[ Field.v n1 uint8 $ fst; Field.v n2 uint8 $ snd ]

(* A field named as a value the module defines takes a trailing ['_'] as its
   accessor, as a keyword does; its fold label keeps its name. *)
let test_escaped () =
  let src = Wire_ocaml.scanner ~codec:"c" (pair "size" "size_") in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        name true
        (contains src ("let[@inline always] " ^ name ^ " ")))
    [ "size_"; "size__" ];
  let src = Wire_ocaml.scanner ~codec:"c" (pair "ok" "b") in
  Alcotest.(check bool) "ok_" true (contains src "let[@inline always] ok_ ");
  Alcotest.(check bool) "~ok" true (contains src "~ok");
  Alcotest.(check (list (pair string bool)))
    "outputs"
    [ ("ok", false); ("b", false) ]
    (Wire_ocaml.scanner_fields (pair "ok" "b"))

(* Names a record's members would define twice refuse [ml], which needs
   them, and leave them out of the module [scanner] writes. *)
let test_record_clash () =
  let c = pair "cast" "b" in
  (match Wire_ocaml.ml ~codec:"c" c with
  | _ -> Alcotest.fail "ml: a field Get.cast"
  | exception Invalid_argument _ -> ());
  let src = Wire_ocaml.scanner ~codec:"c" c in
  Alcotest.(check bool) "folds" true (contains src "fold ");
  Alcotest.(check bool) "no record" false (contains src "decode_exn")

let suite =
  ( "writer",
    [
      Alcotest.test_case "a fixed record's views and folds" `Quick test_fixed;
      Alcotest.test_case "one module for ml and scanner" `Quick test_one_module;
      Alcotest.test_case "accessors named apart" `Quick test_escaped;
      Alcotest.test_case "a record's clashing names" `Quick test_record_clash;
    ] )
