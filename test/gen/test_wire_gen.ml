(* The generated modules against the codecs they come from: on random buffers
   and offsets, decode and validate give the same value or raise the same
   error, and write stores the same bytes or raises the same error. *)

open Wire

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Parse_error e -> Fmt.str "parse error %a" pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let rand = Random.State.make [| 7 |]
let byte () = Char.chr (Random.State.int rand 256)

(* A buffer around a record, often shaped by [fix] so the record passes its
   constraints, and an offset that is mostly in range. *)
let input ~size ~fix =
  let len = Random.State.int rand (size + 6) in
  let buf = Bytes.init len (fun _ -> byte ()) in
  let off =
    match Random.State.int rand 8 with
    | 0 -> -1 - Random.State.int rand 3
    | 1 -> len - size + 1 + Random.State.int rand 2
    | _ -> if len >= size then Random.State.int rand (len - size + 1) else 0
  in
  if Random.State.bool rand && off >= 0 && off + size <= len then fix buf off;
  (buf, off)

let reads ?(fix = fun _ _ -> ()) codec ~decode ~validate =
  let size = Codec.min_wire_size codec in
  let enc v = Codec.to_string codec v in
  for _ = 1 to 3000 do
    let buf, off = input ~size ~fix in
    Alcotest.(check string)
      "decode"
      (outcome (fun () -> enc (Codec.decode_exn codec buf off)))
      (outcome (fun () -> enc (decode buf off)));
    Alcotest.(check string)
      "validate"
      (outcome (fun () ->
           Codec.validate codec buf off;
           ""))
      (outcome (fun () ->
           validate buf off;
           ""))
  done

let writes codec ~write values =
  let size = Codec.min_wire_size codec in
  List.iter
    (fun v ->
      let len = size + Random.State.int rand 3 in
      let off = Random.State.int rand (len + 2) - 1 in
      let run f =
        outcome (fun () ->
            let buf = Bytes.make len '\xaa' in
            f v buf off;
            Bytes.to_string buf)
      in
      Alcotest.(check string)
        "write"
        (run (fun v buf off -> Codec.encode codec v buf off))
        (run write))
    values

(* Values that decode, from random buffers. *)
let decoded ?(fix = fun _ _ -> ()) codec n =
  let size = Codec.min_wire_size codec in
  List.filter_map
    (fun _ ->
      let buf = Bytes.init size (fun _ -> byte ()) in
      if Random.State.bool rand then fix buf 0;
      match Codec.decode_exn codec buf 0 with
      | v -> Some v
      | exception Parse_error _ -> None)
    (List.init n Fun.id)

let fix_checked buf off =
  Bytes.set_uint8 buf off 0x45;
  Bytes.set_uint8 buf (off + 3) (1 + Random.State.int rand 255);
  Bytes.set_uint8 buf (off + 4)
    (List.nth [ 1; 6; 17 ] (Random.State.int rand 3))

let test_ipv4 () =
  reads Schemas.ipv4 ~decode:G_ipv4.decode ~validate:G_ipv4.validate;
  writes Schemas.ipv4
    ~write:(fun (a, b, c, d, e, f, g, h, i, j) buf off ->
      G_ipv4.write buf off a b c d e f g h i j)
    (decoded Schemas.ipv4 200)

let test_ipv4_record () =
  let size = Codec.min_wire_size Schemas.ipv4 in
  for _ = 1 to 1000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    let via_gen () =
      let r = G_ipv4_record.decode buf off in
      let out = Bytes.create size in
      G_ipv4_record.encode r out 0;
      Bytes.to_string out
    in
    Alcotest.(check string)
      "record"
      (outcome (fun () ->
           Codec.to_string Schemas.ipv4 (Codec.decode_exn Schemas.ipv4 buf off)))
      (outcome via_gen)
  done

let test_checked () =
  reads ~fix:fix_checked Schemas.checked ~decode:G_checked.decode
    ~validate:G_checked.validate;
  let wild () =
    ( Random.State.int rand 20,
      Random.State.int rand 20,
      UInt16.v (Random.State.int rand 80),
      UInt8.v (Random.State.int rand 3),
      UInt8.v (List.nth [ 1; 6; 17; 9 ] (Random.State.int rand 4)) )
  in
  writes Schemas.checked
    ~write:(fun (a, b, c, d, e) buf off -> G_checked.write buf off a b c d e)
    (decoded ~fix:fix_checked Schemas.checked 200
    @ List.init 300 (fun _ -> wild ()))

let test_mixed () =
  reads Schemas.mixed ~decode:G_mixed.decode ~validate:G_mixed.validate;
  let wild () =
    let a, b, c, d, e, f, _, _ =
      List.hd (decoded Schemas.mixed 1 @ decoded Schemas.mixed 5)
    in
    (a, b, c, d, e, f, Random.State.int rand 40, Random.State.int rand 3000)
  in
  writes Schemas.mixed
    ~write:(fun (a, b, c, d, e, f, g, h) buf off ->
      G_mixed.write buf off a b c d e f g h)
    (decoded Schemas.mixed 200 @ List.init 200 (fun _ -> wild ()))

let test_small () =
  reads Schemas.small ~decode:G_small.decode ~validate:G_small.validate;
  writes Schemas.small
    ~write:(fun (a, b) buf off -> G_small.write buf off a b)
    (decoded Schemas.small 100)

let test_eth () =
  reads Schemas.eth ~decode:G_eth.decode ~validate:G_eth.validate;
  let str n = String.init n (fun _ -> byte ()) in
  writes Schemas.eth
    ~write:(fun (a, b, c) buf off -> G_eth.write buf off a b c)
    (decoded Schemas.eth 100
    @ List.init 100 (fun _ ->
        ( str (4 + Random.State.int rand 4),
          str (5 + Random.State.int rand 3),
          UInt16.v (Random.State.int rand 0x10000) )))

(* [write_int] takes plain values: on any, it stores what converting each to
   its field's type, in field order, then encoding would store, or raises what
   that raises. *)
let test_write_int () =
  let wild k = Random.State.int rand (k + 40) - 20 in
  let size = Codec.min_wire_size Schemas.checked in
  for _ = 1 to 2000 do
    let a = wild 16 and b = wild 16 and c = wild 0x10000 and d = wild 256 in
    let e = List.nth [ 1; 6; 17; 9; 300 ] (Random.State.int rand 5) in
    let len = size + Random.State.int rand 3 in
    let off = Random.State.int rand (len + 2) - 1 in
    let run f =
      outcome (fun () ->
          let buf = Bytes.make len '\xaa' in
          f buf off;
          Bytes.to_string buf)
    in
    let typed buf off =
      let c = UInt16.v c in
      let d = UInt8.v d in
      let e = UInt8.v e in
      Codec.encode Schemas.checked (a, b, c, d, e) buf off
    in
    Alcotest.(check string)
      "checked" (run typed)
      (run (fun buf off -> G_checked.write_int buf off a b c d e))
  done;
  let size = Codec.min_wire_size Schemas.mixed in
  for _ = 1 to 2000 do
    let a = wild 0x10000 and b = wild 0x10000 - 0x8000 in
    let c = wild 256 - 128 and d = wild 0x10000 - 0x8000 in
    let e = Random.State.int32 rand Int32.max_int in
    let f = Int32.neg (Random.State.int32 rand Int32.max_int) in
    let g = wild 32 and h = wild 2048 in
    let len = size + Random.State.int rand 3 in
    let off = Random.State.int rand (len + 2) - 1 in
    let run fn =
      outcome (fun () ->
          let buf = Bytes.make len '\xaa' in
          fn buf off;
          Bytes.to_string buf)
    in
    let typed buf off =
      let a = UInt16.v a in
      let b = SInt16.v b in
      let c = SInt8.v c in
      let d = SInt16.v d in
      let e = UInt32.of_int32 e in
      let f = SInt32.of_int32 f in
      Codec.encode Schemas.mixed (a, b, c, d, e, f, g, h) buf off
    in
    Alcotest.(check string)
      "mixed" (run typed)
      (run (fun buf off -> G_mixed.write_int buf off a b c d e f g h))
  done

let test_wide () =
  reads Schemas.wide ~decode:G_wide.decode ~validate:G_wide.validate;
  let wild () =
    let w k = Random.State.full_int rand ((1 lsl k) + 4) in
    (w 1, w 8, w 23, w 3, w 29, w 31)
  in
  writes Schemas.wide
    ~write:(fun (a, b, c, d, e, f) buf off -> G_wide.write buf off a b c d e f)
    (decoded Schemas.wide 200 @ List.init 300 (fun _ -> wild ()))

(* A field taken out of its word reads what its own getter reads. *)
let test_words () =
  let size = Codec.min_wire_size Schemas.wide in
  for _ = 1 to 2000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    let fields =
      [
        (G_wide.get_a, G_wide.get_a_word, G_wide.a_of_word);
        (G_wide.get_c, G_wide.get_c_word, G_wide.c_of_word);
        (G_wide.get_e, G_wide.get_e_word, G_wide.e_of_word);
        (G_wide.get_f, G_wide.get_f_word, G_wide.f_of_word);
      ]
    in
    List.iter
      (fun (get, word, of_word) ->
        Alcotest.(check string)
          "word"
          (outcome (fun () -> string_of_int (get buf off)))
          (outcome (fun () -> string_of_int (of_word (word buf off)))))
      fields
  done

(* Generated code names the shape of the codec it comes from, and refuses to
   run against a codec whose shape has changed since. *)
let test_stale () =
  let src = Wire_gen.ml ~codec:"Schemas.small" Schemas.small in
  let digest = Wire.Private.Gen.digest Schemas.small in
  Alcotest.(check bool) "digest in source" true (contains src digest);
  Wire.Private.Gen.check Schemas.small digest;
  match Wire.Private.Gen.check Schemas.eth digest with
  | () -> Alcotest.fail "a changed codec was accepted"
  | exception Invalid_argument _ -> ()

(* The digest a generated module checks at load time changes with anything its
   code relies on: a field's sign, an enum's cases, a bitfield's layout. *)
let test_digest () =
  let differ msg a b = Alcotest.(check bool) msg false (String.equal a b) in
  let signed_vs_unsigned =
    let u = Codec.v "D" (fun x -> x) Codec.[ Field.v "x" uint16 $ Fun.id ] in
    let i = Codec.v "D" (fun x -> x) Codec.[ Field.v "x" int16 $ Fun.id ] in
    (Wire.Private.Gen.digest u, Wire.Private.Gen.digest i)
  in
  differ "signed and unsigned" (fst signed_vs_unsigned) (snd signed_vs_unsigned);
  let enum cases =
    Wire.Private.Gen.digest
      (Codec.v "D" Fun.id Codec.[ Field.v "x" (enum "E" cases uint8) $ Fun.id ])
  in
  differ "enum cases"
    (enum [ ("a", 1); ("b", 2) ])
    (enum [ ("a", 1); ("b", 3) ]);
  let bits order =
    Wire.Private.Gen.digest
      (Codec.v "D" Fun.id
         Codec.[ Field.v "x" (bits ~bit_order:order ~width:3 U8) $ Fun.id ])
  in
  differ "bit order" (bits Msb_first) (bits Lsb_first)

(* A codec the generator cannot specialise is refused when the code is
   generated, naming the field and why, rather than given a module with fewer
   functions. *)
let test_refused () =
  let refused ~maps name =
    match Wire_gen.ml ~maps ~make:"m" ~codec:"c" Schemas.mapped with
    | _ -> Alcotest.failf "%s: generated" name
    | exception Invalid_argument m ->
        Alcotest.(check bool)
          (name ^ ": names the field")
          true (contains m "Len")
  in
  refused ~maps:[] "an unnamed map";
  match Wire_gen.ml ~codec:"Schemas.variable" Schemas.variable with
  | _ -> Alcotest.fail "a variable-size codec was generated"
  | exception Invalid_argument m ->
      Alcotest.(check bool) "names the field" true (contains m "Rest")

(* A generated getter reads what [Codec.get] reads, or raises what it raises,
   on random buffers and offsets around the field. *)
let getter codec field ~get ~show =
  let read = Staged.unstage (Codec.get codec field) in
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    Alcotest.(check string)
      "get"
      (outcome (fun () -> show (read buf off)))
      (outcome (fun () -> show (get buf off)))
  done

(* Every field under a map: over 16- and 32-bit integers, a lookup whose index
   bound validate checks, and a bitfield. *)
let test_mapped () =
  reads Schemas.mapped ~decode:G_mapped.decode ~validate:G_mapped.validate;
  writes Schemas.mapped
    ~write:(fun (a, b, c, d, e) buf off -> G_mapped.write buf off a b c d e)
    (decoded Schemas.mapped 300);
  getter Schemas.mapped Schemas.mapped_len ~get:G_mapped.get_len
    ~show:string_of_int;
  let lookalike = map ~decode:UInt16.to_int ~encode:UInt16.v uint16be in
  Wire.Private.Gen.check_map Schemas.mapped "Len" Schemas.u16be_int;
  match Wire.Private.Gen.check_map Schemas.mapped "Len" lookalike with
  | () -> Alcotest.fail "a different map of the same shape was accepted"
  | exception Invalid_argument _ -> ()

(* A plain-[int] getter reads the [int] view of what [Codec.get] reads. *)
let int_getter codec field ~get ~to_int =
  let read = Staged.unstage (Codec.get codec field) in
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    Alcotest.(check string)
      "get int"
      (outcome (fun () -> string_of_int (to_int (read buf off))))
      (outcome (fun () -> string_of_int (get buf off)))
  done

let test_getters () =
  let u8 v = string_of_int (UInt8.to_int v)
  and u16 v = string_of_int (UInt16.to_int v) in
  getter Schemas.ipv4 Schemas.ipv4_frag ~get:G_ipv4.get_frag ~show:u16;
  getter Schemas.ipv4 Schemas.ipv4_proto ~get:G_ipv4.get_proto ~show:u8;
  getter Schemas.ipv4 Schemas.ipv4_src ~get:G_ipv4.get_src ~show:(fun v ->
      string_of_int (SInt32.to_int v));
  getter Schemas.ipv4 Schemas.ipv4_dst ~get:G_ipv4.get_dst ~show:(fun v ->
      string_of_int (UInt32.to_int v));
  int_getter Schemas.ipv4 Schemas.ipv4_src ~get:G_ipv4.get_src_int
    ~to_int:SInt32.to_int;
  int_getter Schemas.ipv4 Schemas.ipv4_dst ~get:G_ipv4.get_dst_int
    ~to_int:UInt32.to_int;
  int_getter Schemas.mixed Schemas.mixed_f ~get:G_mixed.get_f_int
    ~to_int:SInt32.to_int;
  getter Schemas.checked Schemas.checked_proto ~get:G_checked.get_proto ~show:u8;
  getter Schemas.mixed Schemas.mixed_b ~get:G_mixed.get_b ~show:(fun v ->
      string_of_int (SInt16.to_int v));
  getter Schemas.mixed Schemas.mixed_e ~get:G_mixed.get_e ~show:(fun v ->
      string_of_int (UInt32.to_int v));
  getter Schemas.mixed Schemas.mixed_f ~get:G_mixed.get_f ~show:(fun v ->
      string_of_int (SInt32.to_int v));
  getter Schemas.mixed Schemas.mixed_h ~get:G_mixed.get_h ~show:string_of_int;
  getter Schemas.eth Schemas.eth_src ~get:G_eth.get_src ~show:Fun.id

(* [Valid.v] accepts a buffer exactly when [Codec.validate] does at offset 0,
   raising its error otherwise, and each [read_<field>] then reads what
   [Codec.get] reads. *)
let viewed ?(fix = fun _ _ -> ()) codec ~valid ~reads =
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let len = size + Random.State.int rand 4 - 1 in
    let buf = Bytes.init (max 0 len) (fun _ -> byte ()) in
    if Random.State.bool rand && len >= size then fix buf 0;
    let expected =
      outcome (fun () ->
          Codec.validate codec buf 0;
          String.concat "," (List.map (fun (get, _) -> get buf) reads))
    in
    let got =
      outcome (fun () ->
          let v = valid buf in
          String.concat "," (List.map (fun (_, read) -> read v) reads))
    in
    Alcotest.(check string) "view" expected got
  done

let test_valid () =
  let get codec field show =
    let read = Staged.unstage (Codec.get codec field) in
    fun buf -> show (read buf 0)
  in
  let i32 v = string_of_int (SInt32.to_int v) in
  let u32 v = string_of_int (UInt32.to_int v) in
  viewed Schemas.ipv4 ~valid:G_ipv4.Valid.v
    ~reads:
      [
        ( get Schemas.ipv4 Schemas.ipv4_frag (fun v ->
              string_of_int (UInt16.to_int v)),
          fun v -> string_of_int (UInt16.to_int (G_ipv4.read_frag v)) );
        ( get Schemas.ipv4 Schemas.ipv4_src i32,
          fun v -> string_of_int (G_ipv4.read_src_int v) );
        (get Schemas.ipv4 Schemas.ipv4_dst u32, fun v -> u32 (G_ipv4.read_dst v));
      ];
  viewed ~fix:fix_checked Schemas.checked ~valid:G_checked.Valid.v
    ~reads:
      [
        ( get Schemas.checked Schemas.checked_proto (fun v ->
              string_of_int (UInt8.to_int v)),
          fun v -> string_of_int (UInt8.to_int (G_checked.read_proto v)) );
      ];
  viewed Schemas.mixed ~valid:G_mixed.Valid.v
    ~reads:
      [
        (get Schemas.mixed Schemas.mixed_f i32, fun v -> i32 (G_mixed.read_f v));
        ( get Schemas.mixed Schemas.mixed_h string_of_int,
          fun v -> string_of_int (G_mixed.read_h v) );
      ];
  viewed Schemas.eth ~valid:G_eth.Valid.v
    ~reads:[ (get Schemas.eth Schemas.eth_src Fun.id, G_eth.read_src) ]

(* The [Bigstring] functions on a bigarray copy of a buffer return what their
   [bytes] twins return on the buffer, or raise the same error. *)
let bigstring buf =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (Bytes.length buf)
    (Bytes.get buf)

let same_on_bigstring ?(fix = fun _ _ -> ()) codec ~on_bytes ~on_big =
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let buf, off = input ~size ~fix in
    Alcotest.(check string)
      "bigstring"
      (outcome (fun () -> on_bytes buf off))
      (outcome (fun () -> on_big (bigstring buf) off))
  done

let big_to_string b =
  String.init (Bigarray.Array1.dim b) (Bigarray.Array1.get b)

let test_bigstring () =
  let enc c v = Codec.to_string c v in
  same_on_bigstring Schemas.ipv4
    ~on_bytes:(fun b o -> enc Schemas.ipv4 (G_ipv4.decode b o))
    ~on_big:(fun b o -> enc Schemas.ipv4 (G_ipv4.Bigstring.decode b o));
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b o ->
      G_checked.validate b o;
      "")
    ~on_big:(fun b o ->
      G_checked.Bigstring.validate b o;
      "");
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b o -> enc Schemas.checked (G_checked.decode b o))
    ~on_big:(fun b o -> enc Schemas.checked (G_checked.Bigstring.decode b o));
  same_on_bigstring Schemas.mixed
    ~on_bytes:(fun b o -> enc Schemas.mixed (G_mixed.decode b o))
    ~on_big:(fun b o -> enc Schemas.mixed (G_mixed.Bigstring.decode b o));
  same_on_bigstring Schemas.eth
    ~on_bytes:(fun b o ->
      enc Schemas.eth (G_eth.decode b o) ^ G_eth.get_src b o)
    ~on_big:(fun b o ->
      enc Schemas.eth (G_eth.Bigstring.decode b o) ^ G_eth.Bigstring.get_src b o);
  same_on_bigstring Schemas.ipv4
    ~on_bytes:(fun b o ->
      string_of_int (G_ipv4.get_src_int b o + (G_ipv4.get_proto b o :> int)))
    ~on_big:(fun b o ->
      string_of_int
        (G_ipv4.Bigstring.get_src_int b o
        + (G_ipv4.Bigstring.get_proto b o :> int)));
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b _ ->
      let v = G_checked.Valid.v b in
      string_of_int ((G_checked.read_proto v :> int) + G_checked.read_ihl v))
    ~on_big:(fun b _ ->
      let v = G_checked.Bigstring.Valid.v b in
      string_of_int
        ((G_checked.Bigstring.read_proto v :> int)
        + G_checked.Bigstring.read_ihl v));
  same_on_bigstring Schemas.wide
    ~on_bytes:(fun b o ->
      string_of_int (G_wide.c_of_word (G_wide.get_c_word b o)))
    ~on_big:(fun b o ->
      string_of_int (G_wide.c_of_word (G_wide.Bigstring.get_c_word b o)));
  let size = Codec.min_wire_size Schemas.checked in
  for _ = 1 to 2000 do
    let w k = Random.State.int rand (k + 40) - 20 in
    let a = w 16 and b = w 16 and c = w 0x10000 and d = w 256 in
    let e = List.nth [ 1; 6; 17; 9 ] (Random.State.int rand 4) in
    let len = size + Random.State.int rand 3 in
    let off = Random.State.int rand (len + 2) - 1 in
    let on_bytes =
      outcome (fun () ->
          let buf = Bytes.make len '\xaa' in
          G_checked.write_int buf off a b c d e;
          Bytes.to_string buf)
    in
    let on_big =
      outcome (fun () ->
          let buf = bigstring (Bytes.make len '\xaa') in
          G_checked.Bigstring.write_int buf off a b c d e;
          big_to_string buf)
    in
    Alcotest.(check string) "write" on_bytes on_big
  done

(* The OxCaml target checks the hot paths for allocation, marks the cold ones
   as raising, and adds a stack-allocated decode for the generated record. The
   stock compiler cannot build it; the OxCaml tests do. *)
let test_oxcaml () =
  let src = Wire_gen.ml ~target:`OxCaml ~codec:"Schemas.ipv4" Schemas.ipv4 in
  List.iter
    (fun s -> Alcotest.(check bool) s true (contains src s))
    [
      "let[@zero_alloc] validate";
      "[@zero_alloc] write_int";
      "[@zero_alloc assume error] fail";
      "let[@zero_alloc] decode_local";
      "exclave_ stack_ {";
      "let[@inline] decode_with make";
    ];
  let stock = Wire_gen.ml ~codec:"Schemas.ipv4" Schemas.ipv4 in
  Alcotest.(check bool)
    "stock has no OxCaml syntax" false
    (contains stock "exclave_")

let suite =
  ( "wire_gen",
    [
      Alcotest.test_case "ipv4" `Quick test_ipv4;
      Alcotest.test_case "ipv4 record" `Quick test_ipv4_record;
      Alcotest.test_case "constraints, enum, where" `Quick test_checked;
      Alcotest.test_case "little-endian, signed, bitfields" `Quick test_mixed;
      Alcotest.test_case "under one word" `Quick test_small;
      Alcotest.test_case "byte arrays" `Quick test_eth;
      Alcotest.test_case "32-bit bitfields" `Quick test_wide;
      Alcotest.test_case "fields from their word" `Quick test_words;
      Alcotest.test_case "maps" `Quick test_mapped;
      Alcotest.test_case "write from plain values" `Quick test_write_int;
      Alcotest.test_case "stale code is refused" `Quick test_stale;
      Alcotest.test_case "the digest sees sign, cases and layout" `Quick
        test_digest;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "OxCaml target" `Quick test_oxcaml;
      Alcotest.test_case "getters" `Quick test_getters;
      Alcotest.test_case "validated view" `Quick test_valid;
      Alcotest.test_case "bigstring buffers" `Quick test_bigstring;
    ] )
