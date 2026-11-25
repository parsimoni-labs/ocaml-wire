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
        let c = G_checked.check b length off in
        Alcotest.(check bool) "check" valid (G_checked.ok c);
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
            [
              (G_checked.Get.version buf off :> int);
              (G_checked.Get.ihl buf off :> int);
              UInt16.to_int (G_checked.Get.length buf off);
              UInt8.to_int (G_checked.Get.ttl buf off);
              UInt8.to_int (G_checked.Get.proto buf off);
            ]
          in
          Alcotest.(check (list int)) "accessors" get read;
          Alcotest.(check (option (list int))) "fold" (Some get) folded
        end
  done

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Parse_error e -> Fmt.str "parse error %a" pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let byte () = Char.chr (Random.State.int rand 256)

(* A record of [Schemas.checked] that passes its constraints. *)
let fix_checked buf off =
  Bytes.set_uint8 buf off 0x45;
  Bytes.set_uint8 buf (off + 3) (1 + Random.State.int rand 255);
  Bytes.set_uint8 buf (off + 4)
    (List.nth [ 1; 6; 17 ] (Random.State.int rand 3))

(* The accessors of a record checked at offset 0 read what [Codec.get]
   reads, where [Codec.validate_exn] accepts the record; [read buf] raises
   the codec's error otherwise. *)
let viewed ?(fix = fun _ _ -> ()) codec ~read ~gets =
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let len = size + Random.State.int rand 4 - 1 in
    let buf = Bytes.init (max 0 len) (fun _ -> byte ()) in
    if Random.State.bool rand && len >= size then fix buf 0;
    let expected =
      outcome (fun () ->
          Codec.validate_exn codec buf 0;
          String.concat "," (List.map (fun get -> get buf) gets))
    in
    let got = outcome (fun () -> String.concat "," (read buf)) in
    Alcotest.(check string) "view" expected got
  done

let test_accessors () =
  let get codec field show =
    let read = Staged.unstage (Codec.get codec field) in
    fun buf -> show (read buf 0)
  in
  let i32 v = string_of_int (SInt32.to_int v) in
  let u32 v = string_of_int (UInt32.to_int v) in
  viewed Schemas.ipv4
    ~gets:
      [
        get Schemas.ipv4 Schemas.ipv4_frag (fun v ->
            string_of_int (UInt16.to_int v));
        get Schemas.ipv4 Schemas.ipv4_src i32;
        get Schemas.ipv4 Schemas.ipv4_dst u32;
      ]
    ~read:(fun buf ->
      G_ipv4.validate_exn buf 0;
      let (Buf.Any b) = Buf.v buf in
      let v = G_ipv4.get (G_ipv4.check b (Buf.length b) 0) in
      [
        string_of_int (UInt16.to_int (G_ipv4.frag b v));
        string_of_int (G_ipv4.view_src_int b v);
        u32 (G_ipv4.dst b v);
      ]);
  viewed ~fix:fix_checked Schemas.checked
    ~gets:
      [
        get Schemas.checked Schemas.checked_proto (fun v ->
            string_of_int (UInt8.to_int v));
      ]
    ~read:(fun buf ->
      G_checked.validate_exn buf 0;
      let (Buf.Any b) = Buf.v buf in
      let v = G_checked.get (G_checked.check b (Buf.length b) 0) in
      [ string_of_int (UInt8.to_int (G_checked.proto b v)) ]);
  viewed Schemas.mixed
    ~gets:
      [
        get Schemas.mixed Schemas.mixed_f i32;
        get Schemas.mixed Schemas.mixed_h string_of_int;
      ]
    ~read:(fun buf ->
      G_mixed.validate_exn buf 0;
      let (Buf.Any b) = Buf.v buf in
      let v = G_mixed.get (G_mixed.check b (Buf.length b) 0) in
      [ i32 (G_mixed.f b v); string_of_int (G_mixed.h b v) ]);
  viewed Schemas.eth
    ~gets:[ get Schemas.eth Schemas.eth_src Fun.id ]
    ~read:(fun buf ->
      G_eth.validate_exn buf 0;
      let (Buf.Any b) = Buf.v buf in
      let v = G_eth.get (G_eth.check b (Buf.length b) 0) in
      [ G_eth.src b v ])

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
  Alcotest.(check bool) "no record" false (contains src "module Get")

(* The fields of the record at [off], read with no check of its
   constraints. *)
let fields buf off =
  [
    (G_checked.Get.version buf off :> int);
    (G_checked.Get.ihl buf off :> int);
    UInt16.to_int (G_checked.Get.length buf off);
    UInt8.to_int (G_checked.Get.ttl buf off);
    UInt8.to_int (G_checked.Get.proto buf off);
  ]

(* A setter writes its field in place and leaves the bytes of every other
   field as they were; a bitfield too wide for its field is refused, the
   bytes untouched. *)
let test_setters () =
  for _ = 1 to 2000 do
    let buf, off = input () in
    match Buf.v buf with
    | Buf.Any b ->
        let c = G_checked.check b (Buf.length b) off in
        if G_checked.ok c then begin
          let v = G_checked.get c in
          let before = fields buf off in
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
          Alcotest.(check (list int)) "set" want (fields buf off);
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
      Alcotest.test_case "one module for ml and scanner" `Quick test_one_module;
      Alcotest.test_case "accessors named apart" `Quick test_escaped;
      Alcotest.test_case "a record's clashing names" `Quick test_record_clash;
    ] )
