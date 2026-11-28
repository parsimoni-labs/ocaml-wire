(* The one module Wire_ocaml writes for a codec, whatever its shape: the
   members of a fixed record beside the folds, the check and the views, each
   agreeing with the others and with the codec; the names they take. *)

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
(* A result's value, its error raised as a getter raises it, so that the
   two compare the same. *)
let ok = function Ok v -> v | Error e -> Wire_runtime.Errors.fail e

let test_fixed () =
  for _ = 1 to 2000 do
    let buf, off = input () in
    let len = Bytes.length buf - off in
    let valid = Result.is_ok (Codec.validate Schemas.checked buf ~off) in
    let folded =
      match
        G_checked.fold buf ~offs:[| off |] ~lens:[| len |]
          ~invalid:(fun acc -> None :: acc)
          ~init:[]
          ~f:(fun acc ~version ~ihl ~length ~ttl ~proto ->
            Some
              [
                version;
                ihl;
                UInt16.to_int length;
                UInt8.to_int ttl;
                UInt8.to_int proto;
              ]
            :: acc)
      with
      | Ok [ outputs ] -> outputs
      | _ -> Alcotest.fail "fold: one answer for one record"
    in
    match Buf.v buf with
    | Buf.Any b ->
        let c = G_checked.check b ~off in
        Alcotest.(check bool) "check" valid (G_checked.is_ok c);
        Alcotest.(check bool) "fold" valid (Option.is_some folded);
        if valid then begin
          let v = G_checked.get c in
          let read =
            [
              G_checked.version b v;
              G_checked.ihl b v;
              UInt16.to_int (G_checked.length b v);
              UInt8.to_int (G_checked.ttl b v);
              UInt8.to_int (G_checked.proto b v);
            ]
          and get =
            let r = ok (G_checked.decode buf ~off) in
            [
              r.version;
              r.ihl;
              UInt16.to_int r.length;
              UInt8.to_int r.ttl;
              UInt8.to_int r.proto;
            ]
          in
          Alcotest.(check (list int)) "accessors" get read;
          Alcotest.(check (option (list int))) "fold" (Some get) folded
        end
  done

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception exn when Wire.Private.Types.is_parse_error exn ->
      let e = Wire.Private.Types.parse_error_of exn in
      Fmt.str "parse error %a" Error.pp e
  | exception Invalid_argument m -> "invalid " ^ m

let byte () = Char.chr (Random.State.int rand 256)

(* A record of [Schemas.checked] that passes its constraints. *)
let fix_checked buf off =
  Bytes.set_uint8 buf off 0x45;
  Bytes.set_uint8 buf (off + 3) (1 + Random.State.int rand 255);
  Bytes.set_uint8 buf (off + 4)
    (List.nth [ 1; 6; 17 ] (Random.State.int rand 3))

(* The accessors of a record checked at offset 0 read what [Codec.decode]
   reads, where [Codec.validate] accepts the record; [read buf] raises the
   codec's error otherwise. *)
let viewed ?(fix = fun _ _ -> ()) codec ~read ~get =
  let size = Codec.min_size codec in
  for _ = 1 to 2000 do
    let len = size + Random.State.int rand 4 - 1 in
    let buf = Bytes.init (max 0 len) (fun _ -> byte ()) in
    if Random.State.bool rand && len >= size then fix buf 0;
    let expected =
      outcome (fun () ->
          String.concat "," (get (ok (Codec.decode codec buf ~off:0))))
    in
    let got = outcome (fun () -> String.concat "," (read buf)) in
    Alcotest.(check string) "view" expected got
  done

let test_accessors () =
  let i32 v = string_of_int (SInt32.to_int v) in
  let u32 v = string_of_int (UInt32.to_int v) in
  let u16 v = string_of_int (UInt16.to_int v) in
  viewed Schemas.ipv4
    ~get:(fun (_, _, _, _, frag, _, _, _, src, dst) ->
      [ u16 frag; i32 src; u32 dst ])
    ~read:(fun buf ->
      ok (G_ipv4.validate buf ~off:0);
      let (Buf.Any b) = Buf.v buf in
      let v = G_ipv4.get (G_ipv4.check b ~off:0) in
      [ u16 (G_ipv4.frag b v); i32 (G_ipv4.src b v); u32 (G_ipv4.dst b v) ]);
  viewed ~fix:fix_checked Schemas.checked
    ~get:(fun (_, _, _, _, proto) -> [ string_of_int (UInt8.to_int proto) ])
    ~read:(fun buf ->
      ok (G_checked.validate buf ~off:0);
      let (Buf.Any b) = Buf.v buf in
      let v = G_checked.get (G_checked.check b ~off:0) in
      [ string_of_int (UInt8.to_int (G_checked.proto b v)) ]);
  viewed Schemas.mixed
    ~get:(fun (_, _, _, _, _, f, _, h) -> [ i32 f; string_of_int h ])
    ~read:(fun buf ->
      ok (G_mixed.validate buf ~off:0);
      let (Buf.Any b) = Buf.v buf in
      let v = G_mixed.get (G_mixed.check b ~off:0) in
      [ i32 (G_mixed.f b v); string_of_int (G_mixed.h b v) ]);
  viewed Schemas.eth
    ~get:(fun (_, src, _) -> [ src ])
    ~read:(fun buf ->
      ok (G_eth.validate buf ~off:0);
      let (Buf.Any b) = Buf.v buf in
      let v = G_eth.get (G_eth.check b ~off:0) in
      [ G_eth.src b v ])

(* The module of [c] for the OCaml target, with no fold ([ml]) or with a
   fold of the fields [fold] ([scanner]). *)
let ml c = Wire_ocaml.(to_ml (v ~target:`OCaml c))
let scanner ~fold c = Wire_ocaml.(to_ml (v ~target:`OCaml ~fold c))

let pair n1 n2 =
  Codec.v "Pair"
    (fun a b -> (a, b))
    Codec.[ Field.v n1 uint8 $ fst; Field.v n2 uint8 $ snd ]

(* A field named as a value the module defines takes a trailing ['_'] as its
   accessor, as a keyword does, and a fold names it by its accessor. *)
let test_escaped () =
  let src = scanner ~fold:[ "size_"; "size__" ] (pair "size" "size_") in
  List.iter
    (fun name ->
      Alcotest.(check bool)
        name true
        (contains src ("let[@inline always] " ^ name ^ " ")))
    [ "size_"; "size__" ];
  let src = scanner ~fold:[ "is_ok_"; "b" ] (pair "is_ok" "b") in
  Alcotest.(check bool)
    "is_ok_" true
    (contains src "let[@inline always] is_ok_ ");
  match scanner ~fold:[ "is_ok" ] (pair "is_ok" "b") with
  | _ -> Alcotest.fail "a fold of the field is_ok named as such"
  | exception Invalid_argument _ -> ()

(* A record with views has no getters, so a field named as a getter's helper
   defines nothing twice. *)
let test_record_clash () =
  let src = ml (pair "cast" "b") in
  Alcotest.(check bool) "views" true (contains src "let[@inline always] check ");
  Alcotest.(check bool) "no getters" false (contains src "module Get")

(* The fields of the record [v] in [b], read with no check of its
   constraints. *)
let fields b v =
  [
    G_checked.version b v;
    G_checked.ihl b v;
    UInt16.to_int (G_checked.length b v);
    UInt8.to_int (G_checked.ttl b v);
    UInt8.to_int (G_checked.proto b v);
  ]

(* A setter writes its field in place and leaves the bytes of every other
   field as they were; a bitfield too wide for its field is refused, the
   bytes untouched. *)
let test_setters () =
  for _ = 1 to 2000 do
    let buf, off = input () in
    match Buf.v buf with
    | Buf.Any b ->
        let c = G_checked.check b ~off in
        if G_checked.is_ok c then begin
          let v = G_checked.get c in
          let before = fields b v in
          let i = Random.State.int rand 5 in
          let x = Random.State.int rand (if i < 2 then 16 else 256) in
          (match i with
          | 0 -> G_checked.set_version b v x
          | 1 -> G_checked.set_ihl b v x
          | 2 -> G_checked.set_length b v (UInt16.v (x * 251))
          | 3 -> G_checked.set_ttl b v (UInt8.v x)
          | _ -> G_checked.set_proto b v (UInt8.v x));
          let want =
            List.mapi
              (fun j y -> if j = i then if i = 2 then x * 251 else x else y)
              before
          in
          Alcotest.(check (list int)) "set" want (fields b v);
          let copy = Bytes.copy buf in
          (match G_checked.set_ihl b v 16 with
          | () -> Alcotest.fail "set_ihl 16"
          | exception Invalid_argument _ -> ());
          Alcotest.(check bytes) "refused" copy buf
        end
  done

let suite =
  ( "writer",
    [
      Alcotest.test_case "setters" `Quick test_setters;
      Alcotest.test_case "a fixed record's views and folds" `Quick test_fixed;
      Alcotest.test_case "accessors" `Quick test_accessors;
      Alcotest.test_case "accessors named apart" `Quick test_escaped;
      Alcotest.test_case "a record's clashing names" `Quick test_record_clash;
    ] )
