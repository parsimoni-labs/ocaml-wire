(* The generated modules against the codecs they come from: on random buffers
   and offsets, decode and validate give the same value or raise the same
   error, and write stores the same bytes or raises the same error. *)

open Wire
module Buf = Wire_runtime.Buf

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

(* A result's value, its error raised as a getter raises it, so that the
   two compare the same. *)
let ok = function Ok v -> v | Error e -> Wire_runtime.Errors.fail e

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception exn when Wire.Private.Types.is_parse_error exn ->
      let e = Wire.Private.Types.parse_error_of exn in
      Fmt.str "parse error %a" Error.pp e
  | exception Invalid_argument m -> "invalid " ^ m

(* The module of [c] and its interface, for the OCaml target unless [target]
   says otherwise; [scanner] declares its folds, [fold] of [fields] and with
   [step] [fold_step], whose visitor is [Step]. *)
let ml ?(target = `OCaml) ?simd ?buffer c =
  Wire_ocaml.(to_ml (v ~target ?simd ?buffer c))

let mli ?(target = `OCaml) c = Wire_ocaml.(to_mli (v ~target c))
let step_visitors = [ ("fold_step", "step.ml") ]

let step_folds ?fields () =
  Wire_ocaml.[ fold ?outputs:fields "fold"; fold ?outputs:fields "fold_step" ]

let scanner ?(target = `OCaml) ?fields ?(step = false) c =
  if step then
    Wire_ocaml.(
      to_ml ~visitors:step_visitors (v ~target ~folds:(step_folds ?fields ()) c))
  else Wire_ocaml.(to_ml (v ~target ~folds:[ fold ?outputs:fields "fold" ] c))

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

(* The bytes [encode] writes for the record [decode] reads: a generated
   module's record, whose type is its own, compared with the codec's through
   them. *)
let encoded size encode decode buf off =
  let r = ok (decode buf off) in
  let out = Bytes.make size '\000' in
  encode r out 0;
  Bytes.to_string out

let reads ?(fix = fun _ _ -> ()) codec ~decode ~validate =
  let size = Codec.min_wire_size codec in
  let enc v = Codec.to_string codec v in
  for _ = 1 to 3000 do
    let buf, off = input ~size ~fix in
    Alcotest.(check string)
      "decode"
      (outcome (fun () -> enc (ok (Codec.decode codec buf off))))
      (outcome (fun () -> decode buf off));
    Alcotest.(check string)
      "validate"
      (outcome (fun () ->
           ok (Codec.validate codec buf off);
           ""))
      (outcome (fun () ->
           ok (validate buf off);
           ""))
  done

(* [write v] builds the field carriers of [v], then writes them. A value a
   carrier refuses is one the codec refuses, with the same error, where the
   record fits; any other writes what the codec writes, or raises what it
   raises. *)
let writes codec ~write values =
  let size = Codec.min_wire_size codec in
  List.iter
    (fun v ->
      let len = size + Random.State.int rand 3 in
      let off = Random.State.int rand (len + 2) - 1 in
      let run f =
        outcome (fun () ->
            let buf = Bytes.make len '\xaa' in
            f buf off;
            Bytes.to_string buf)
      in
      match write v with
      | exception Invalid_argument m ->
          Alcotest.(check string)
            "refused"
            (outcome (fun () ->
                 Codec.encode codec v (Bytes.make size '\xaa') 0;
                 ""))
            ("invalid " ^ m)
      | w ->
          Alcotest.(check string)
            "write"
            (run (fun buf off -> Codec.encode codec v buf off))
            (run w))
    values

(* Values that decode, from random buffers. *)
let decoded ?(fix = fun _ _ -> ()) codec n =
  let size = Codec.min_wire_size codec in
  List.filter_map
    (fun _ ->
      let buf = Bytes.init size (fun _ -> byte ()) in
      if Random.State.bool rand then fix buf 0;
      Result.to_option (Codec.decode codec buf 0))
    (List.init n Fun.id)

let fix_checked buf off =
  Bytes.set_uint8 buf off 0x45;
  Bytes.set_uint8 buf (off + 3) (1 + Random.State.int rand 255);
  Bytes.set_uint8 buf (off + 4)
    (List.nth [ 1; 6; 17 ] (Random.State.int rand 3))

let test_ipv4 () =
  reads Schemas.ipv4
    ~decode:(encoded G_ipv4.min_size G_ipv4.encode G_ipv4.decode)
    ~validate:G_ipv4.validate;
  writes Schemas.ipv4
    ~write:(fun (a, b, c, d, e, f, g, h, i, j) buf off ->
      G_ipv4.write buf off ~versionihl:a ~tos:b ~length:c ~id:d ~frag:e ~ttl:f
        ~proto:g ~sum:h ~src:i ~dst:j)
    (decoded Schemas.ipv4 200)

let test_ipv4_record () =
  let size = Codec.min_wire_size Schemas.ipv4 in
  for _ = 1 to 1000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    let via_gen () =
      let r = ok (G_ipv4_record.decode buf off) in
      let out = Bytes.create size in
      G_ipv4_record.encode r out 0;
      Bytes.to_string out
    in
    Alcotest.(check string)
      "record"
      (outcome (fun () ->
           Codec.to_string Schemas.ipv4 (ok (Codec.decode Schemas.ipv4 buf off))))
      (outcome via_gen)
  done

let test_checked () =
  reads ~fix:fix_checked Schemas.checked
    ~decode:(encoded G_checked.min_size G_checked.encode G_checked.decode)
    ~validate:G_checked.validate;
  let wild () =
    ( Random.State.int rand 20,
      Random.State.int rand 20,
      UInt16.v (Random.State.int rand 80),
      UInt8.v (Random.State.int rand 3),
      UInt8.v (List.nth [ 1; 6; 17; 9 ] (Random.State.int rand 4)) )
  in
  writes Schemas.checked
    ~write:(fun (a, b, c, d, e) ->
      let a = G_checked.Value.version a and b = G_checked.Value.ihl b in
      fun buf off ->
        G_checked.write buf off ~version:a ~ihl:b ~length:c ~ttl:d ~proto:e)
    (decoded ~fix:fix_checked Schemas.checked 200
    @ List.init 300 (fun _ -> wild ()))

let test_mixed () =
  reads Schemas.mixed
    ~decode:(encoded G_mixed.min_size G_mixed.encode G_mixed.decode)
    ~validate:G_mixed.validate;
  let wild () =
    let a, b, c, d, e, f, _, _ =
      List.hd (decoded Schemas.mixed 1 @ decoded Schemas.mixed 5)
    in
    (a, b, c, d, e, f, Random.State.int rand 40, Random.State.int rand 3000)
  in
  writes Schemas.mixed
    ~write:(fun (a, b, c, d, e, f, g, h) ->
      let g = G_mixed.Value.g g and h = G_mixed.Value.h h in
      fun buf off -> G_mixed.write buf off ~a ~b ~c ~d ~e ~f ~g ~h)
    (decoded Schemas.mixed 200 @ List.init 200 (fun _ -> wild ()))

let test_small () =
  reads Schemas.small
    ~decode:(encoded G_small.min_size G_small.encode G_small.decode)
    ~validate:G_small.validate;
  writes Schemas.small
    ~write:(fun (a, b) buf off -> G_small.write buf off ~type_:a ~local__:b)
    (decoded Schemas.small 100)

let test_eth () =
  reads Schemas.eth
    ~decode:(encoded G_eth.min_size G_eth.encode G_eth.decode)
    ~validate:G_eth.validate;
  let str n = String.init n (fun _ -> byte ()) in
  writes Schemas.eth
    ~write:(fun (a, b, c) ->
      let a = G_eth.Value.dst a and b = G_eth.Value.src b in
      fun buf off -> G_eth.write buf off ~dst:a ~src:b ~type_:c)
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
      (run (fun buf off ->
           G_checked.write_int buf off ~version:a ~ihl:b ~length:c ~ttl:d
             ~proto:e))
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
      (run (fun buf off -> G_mixed.write_int buf off ~a ~b ~c ~d ~e ~f ~g ~h))
  done

let test_wide () =
  reads Schemas.wide
    ~decode:(encoded G_wide.min_size G_wide.encode G_wide.decode)
    ~validate:G_wide.validate;
  let wild () =
    let w k = Random.State.full_int rand ((1 lsl k) + 4) in
    (w 1, w 8, w 23, w 3, w 29, w 31)
  in
  writes Schemas.wide
    ~write:(fun (a, b, c, d, e, f) ->
      let module V = G_wide.Value in
      let a = V.a a and b = V.b b and c = V.c c in
      let d = V.d d and e = V.e e and f = V.f f in
      fun buf off -> G_wide.write buf off ~a ~b ~c ~d ~e ~f)
    (decoded Schemas.wide 200 @ List.init 300 (fun _ -> wild ()))

(* Generated code names the shape of the codec it comes from, and refuses to
   run against a codec whose shape has changed since. *)
let test_stale () =
  let src = ml Schemas.small in
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
  differ "bit order" (bits Msb_first) (bits Lsb_first);
  (* Operators and references the 3D printer spells alike. *)
  let two x y where =
    let x = Field.v "x" x and y = Field.v "y" y in
    Wire.Private.Gen.digest
      (Codec.v "D" ~where:(where x y)
         (fun a b -> (a, b))
         Codec.[ x $ fst; y $ snd ])
  in
  differ "lsr and lsr64"
    (two uint64 uint64 (fun x y ->
         Expr.(Field.int x lsr int 1 = Field.int y lsr int 1)))
    (two uint64 uint64 (fun x y ->
         Expr.(lsr64 (Field.int64 x) (int 1) = lsr64 (Field.int64 y) (int 1))));
  differ "a reference as an int and as an int64"
    (two uint64 uint64 (fun x y -> Expr.(Field.int x = Field.int y)))
    (two uint64 uint64 (fun x y -> Expr.(Field.int64 x = Field.int64 y)))

(* A signed field refined against a negative literal projects to 3D, so the
   generator specialises it. *)
let test_signed () =
  ignore (Wire.Everparse.project Schemas.signed : Wire.Everparse.t);
  reads Schemas.signed
    ~decode:(encoded G_signed.min_size G_signed.encode G_signed.decode)
    ~validate:G_signed.validate;
  writes Schemas.signed
    ~write:(fun (a, b) buf off -> G_signed.write buf off ~s:a ~t:b)
    (decoded Schemas.signed 200)

(* [packed]'s fields store as a few wide words: each value, a negative one
   among them, lands in its own bytes, and a plain value out of its field's
   range raises what converting it raises. *)
let test_packed () =
  let open Schemas in
  reads packed
    ~decode:(encoded G_packed.min_size G_packed.encode G_packed.decode)
    ~validate:G_packed.validate;
  writes packed
    ~write:(fun r ->
      let e = G_packed.Value.e r.e in
      fun buf off ->
        G_packed.write buf off ~a:r.a ~b:r.b ~c:r.c ~d:r.d ~e ~f:r.f ~g:r.g
          ~h:r.h ~i:r.i ~j:r.j ~k:r.k ~l:r.l)
    (decoded packed 300);
  let wild k = Random.State.int rand (k + 40) - 20 in
  let size = Codec.min_wire_size packed in
  for _ = 1 to 2000 do
    let a = wild 256 - 128 and b = wild 256 and c = wild 0x10000 - 0x8000 in
    let d = wild 0x10000 and e = wild 512 in
    let f = Int32.sub (Random.State.int32 rand Int32.max_int) 0x40000000l in
    let g = wild 256 - 128 and h = wild 256 and i = wild 0x10000 - 0x8000 in
    let j = wild 0x10000 and k = wild 256 - 128 and l = wild 256 in
    let len = size + Random.State.int rand 3 in
    let off = Random.State.int rand (len + 2) - 1 in
    let run fn =
      outcome (fun () ->
          let buf = Bytes.make len '\xaa' in
          fn buf off;
          Bytes.to_string buf)
    in
    let typed buf off =
      let a = SInt8.v a in
      let b = UInt8.v b in
      let c = SInt16.v c in
      let d = UInt16.v d in
      let f = SInt32.of_int32 f in
      let g = SInt8.v g in
      let h = UInt8.v h in
      let i = SInt16.v i in
      let j = UInt16.v j in
      let k = SInt8.v k in
      let l = UInt8.v l in
      Codec.encode packed (packed_make a b c d e f g h i j k l) buf off
    in
    Alcotest.(check string)
      "packed" (run typed)
      (run (fun buf off ->
           G_packed.write_int buf off ~a ~b ~c ~d ~e ~f ~g ~h ~i ~j ~k ~l))
  done

(* Values that make each check of [refined] pass or fail, often several at
   once, so the generated code must report the one the codec reports. *)
let fix_refined buf off =
  let pick l = List.nth l (Random.State.int rand (List.length l)) in
  Bytes.set_uint8 buf off ((Random.State.int rand 16 lsl 4) lor pick [ 0; 3 ]);
  Bytes.set_uint8 buf (off + 1) (pick [ 0; 1; 2; 5; 7 ]);
  Bytes.set_uint8 buf (off + 2) (pick [ 5; 10; 20; 30; 40 ])

let test_refined () =
  reads ~fix:fix_refined Schemas.refined
    ~decode:(encoded G_refined.min_size G_refined.encode G_refined.decode)
    ~validate:G_refined.validate;
  let wild () =
    ( Random.State.int rand 18,
      Random.State.int rand 18,
      UInt8.v (List.nth [ 0; 1; 2; 5; 7 ] (Random.State.int rand 5)),
      UInt8.v (List.nth [ 5; 10; 20; 40 ] (Random.State.int rand 4)) )
  in
  writes Schemas.refined
    ~write:(fun (a, b, c, d) ->
      let a = G_refined.Value.hi a and b = G_refined.Value.lo b in
      fun buf off -> G_refined.write buf off ~hi:a ~lo:b ~kind:c ~code:d)
    (decoded ~fix:fix_refined Schemas.refined 200
    @ List.init 300 (fun _ -> wild ()))

(* A codec the generator cannot specialise is refused when the code is
   generated, naming the field and why, rather than given a module with fewer
   functions. *)
(* Arithmetic near the top of the native int reads and writes as the codec
   does. *)
let test_overflow () =
  let fix buf off =
    Bytes.set_int32_be buf off (Int32.of_int (Random.State.int rand 8));
    Bytes.set_uint8 buf (off + 4) (Random.State.int rand 8);
    if Random.State.bool rand then Bytes.set_uint16_be buf (off + 5) 0
  in
  reads ~fix Schemas.overflow
    ~decode:(encoded G_overflow.min_size G_overflow.encode G_overflow.decode)
    ~validate:G_overflow.validate;
  let u32 = UInt32.of_int32 in
  writes Schemas.overflow
    ~write:(fun (a, hi, lo, b, c) ->
      let hi = G_overflow.Value.hi hi and lo = G_overflow.Value.lo lo in
      fun buf off -> G_overflow.write buf off ~a ~hi ~lo ~b ~c)
    (decoded ~fix Schemas.overflow 200
    @ [
        (u32 9l, 0, 0, u32 1l, u32 1l);
        (u32 1l, 0, 9, u32 1l, u32 1l);
        (u32 1l, 0, 0, u32 Int32.min_int, u32 Int32.min_int);
      ])

let test_refused () =
  let refused ~maps name =
    match Wire_ocaml.(to_ml (v ~target:`OCaml ~maps Schemas.mapped)) with
    | _ -> Alcotest.failf "%s: generated" name
    | exception Invalid_argument m ->
        Alcotest.(check bool)
          (name ^ ": names the field")
          true (contains m "Len")
  in
  refused ~maps:[] "an unnamed map";
  (* A codec of variable size is written through the walk, its field of
     bytes an accessor; a field no part of the module can read is refused,
     naming it. *)
  let src = ml Schemas.variable in
  Alcotest.(check bool)
    "a variable-size codec: its field of bytes" true
    (contains src "let[@inline always] rest ");
  let n = Field.v "n" uint8 in
  let decorated name field =
    let c =
      Codec.v "Decorated" (fun n x -> (n, x)) Codec.[ n $ fst; field $ snd ]
    in
    match scanner c with
    | _ -> Alcotest.failf "%s: generated" name
    | exception Invalid_argument m ->
        Alcotest.(check bool)
          (name ^ ": names the field")
          true (contains m "xs")
  in
  decorated "a repeated field" (Field.repeat "xs" ~size:(Field.ref n) uint8);
  decorated "an optional field with a default"
    (Field.optional_or "xs"
       ~present:Expr.(Field.ref n <> int 0)
       ~default:UInt8.zero uint8);
  let wide =
    Codec.v "Wide"
      (fun n big -> (n, big))
      Codec.[ Field.v "N" uint8 $ fst; Field.v "Big" uint64be $ snd ]
  in
  match ml wide with
  | _ -> Alcotest.fail "a 64-bit field was generated"
  | exception Invalid_argument m ->
      Alcotest.(check bool) "names the field" true (contains m "Big")

(* Field names that spell one OCaml name twice, once lower-cased, escaped or
   suffixed, are refused naming both fields, as is a name the generated module
   uses for itself: a later definition would silently shadow the first. *)
let test_clash () =
  let pair n1 t1 n2 t2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 t1 $ fst; Field.v n2 t2 $ snd ]
  in
  let refused ?(target = `OCaml) what c names =
    match ml ~target c with
    | _ -> Alcotest.failf "%s: generated" what
    | exception Invalid_argument m ->
        List.iter
          (fun n ->
            Alcotest.(check bool)
              (Fmt.str "%s: names %s" what n)
              true (contains m n))
          names
  in
  refused "a 32-bit x and x_int"
    (pair "x_int" uint8 "x" uint32be)
    [ "\"x_int\""; "\"x\"" ];
  refused "a bitfield x and x_word"
    (pair "x" (bits ~width:3 U8) "x_word" uint8)
    [ "\"x\""; "\"x_word\"" ];
  refused "X and x" (pair "X" uint8 "x" uint8) [ "\"X\""; "\"x\"" ];
  refused "a-b and a_b" (pair "a-b" uint8 "a_b" uint8) [ "\"a-b\""; "\"a_b\"" ];
  refused "1 and f_1" (pair "1" uint8 "f_1" uint8) [ "\"1\""; "\"f_1\"" ];
  (* The keyword escape is injective: a keyword next to its escape is two
     labels. *)
  let src = ml (pair "end" uint8 "end_" uint8) in
  Alcotest.(check bool)
    "end and end_ apart" true
    (contains src "  end_ :" && contains src "  end__ :");
  (* A getter in [Get] would shadow a helper the getters after it call. *)
  refused "cast"
    (Codec.v "C" Fun.id Codec.[ Field.v "cast" uint8 $ Fun.id ])
    [ "\"cast\""; "Get.cast" ];
  (* Outside [Get], a field may take a name the module uses for itself. *)
  let c = Codec.v "C" Fun.id Codec.[ Field.v "size" uint8 $ Fun.id ] in
  Alcotest.(check bool)
    "size getter" true
    (contains (ml c) "let[@inline] size buf off");
  (* An OxCaml keyword is escaped for the default target too, so its code
     compiles on an OxCaml compiler. *)
  let c = Codec.v "C" Fun.id Codec.[ Field.v "local_" uint8 $ Fun.id ] in
  Alcotest.(check bool)
    "default-target label" true
    (contains (ml c) "  local__ :");
  Alcotest.(check bool)
    "OxCaml label" true
    (contains (ml ~target:`OxCaml c) "  local__ :")

(* The integer literals of OCaml source, outside strings and comments: a
   token that starts with a digit and is not an [int32], [int64] or
   [nativeint] literal. *)
let int_literals src =
  let n = String.length src in
  let is_tok c =
    match c with
    | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
    | _ -> false
  in
  let rec go i depth acc =
    if i >= n then List.rev acc
    else
      match src.[i] with
      | '(' when i + 1 < n && src.[i + 1] = '*' -> go (i + 2) (depth + 1) acc
      | '*' when depth > 0 && i + 1 < n && src.[i + 1] = ')' ->
          go (i + 2) (depth - 1) acc
      | _ when depth > 0 -> go (i + 1) depth acc
      | '"' ->
          let rec close j =
            if j >= n then j
            else if src.[j] = '\\' then close (j + 2)
            else if src.[j] = '"' then j + 1
            else close (j + 1)
          in
          go (close (i + 1)) depth acc
      | '0' .. '9' when i = 0 || not (is_tok src.[i - 1]) ->
          let j = ref i in
          while !j < n && is_tok src.[!j] do
            incr j
          done;
          let tok = String.sub src i (!j - i) in
          let acc =
            match tok.[String.length tok - 1] with
            | 'l' | 'L' | 'n' -> acc
            | _ -> tok :: acc
          in
          go !j depth acc
      | _ -> go (i + 1) depth acc
  in
  go 0 0 []

(* js_of_ocaml has 32-bit ints and wasm_of_ocaml 31-bit ones, and the code is
   compiled for its target, not on the host that generates it: no literal
   may need more than 31 bits, and code that reads a 32-bit field into an
   [int] refuses to load on such a target. *)
let test_narrow_ints () =
  let max31 = (1 lsl 30) - 1 in
  List.iter
    (fun (name, src) ->
      List.iter
        (fun tok ->
          match int_of_string_opt tok with
          | Some v when abs v <= max31 -> ()
          | _ ->
              Alcotest.failf "%s: the literal %s needs more than 31 bits" name
                tok)
        (int_literals src);
      Alcotest.(check bool)
        (name ^ ": checks the int size")
        true
        (contains src "Sys.int_size"))
    [
      ("ipv4", ml Schemas.ipv4);
      ("mixed", ml Schemas.mixed);
      ("wide", ml Schemas.wide);
      ("wide OxCaml", ml ~target:`OxCaml Schemas.wide);
    ]

(* A generated getter reads what [Codec.get] reads, or raises what it raises,
   on random buffers and offsets around the field. *)
let getter codec field ~get ~show =
  let read = Staged.unstage (Codec.get codec field) in
  let size = Codec.min_wire_size codec in
  for _ = 1 to 2000 do
    let buf, off = input ~size ~fix:(fun _ _ -> ()) in
    Alcotest.(check string)
      "get"
      (outcome (fun () -> show (ok (read buf off))))
      (outcome (fun () -> show (ok (get buf off))))
  done

(* Every field under a map: over 16- and 32-bit integers, a lookup whose index
   bound validate checks, and a bitfield. *)
let test_mapped () =
  reads Schemas.mapped
    ~decode:(encoded G_mapped.min_size G_mapped.encode G_mapped.decode)
    ~validate:G_mapped.validate;
  writes Schemas.mapped
    ~write:(fun (a, b, c, d, e) ->
      let e = G_mapped.Value.rest e in
      fun buf off ->
        G_mapped.write buf off ~len:a ~addr:b ~proto:c ~flag:d ~rest:e)
    (decoded Schemas.mapped 300);
  getter Schemas.mapped Schemas.mapped_len ~get:G_mapped.Get.len
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
      (outcome (fun () -> string_of_int (to_int (ok (read buf off)))))
      (outcome (fun () -> string_of_int (ok (get buf off))))
  done

let test_getters () =
  let u8 v = string_of_int (UInt8.to_int v)
  and u16 v = string_of_int (UInt16.to_int v) in
  getter Schemas.ipv4 Schemas.ipv4_frag ~get:G_ipv4.Get.frag ~show:u16;
  getter Schemas.ipv4 Schemas.ipv4_proto ~get:G_ipv4.Get.proto ~show:u8;
  getter Schemas.ipv4 Schemas.ipv4_src ~get:G_ipv4.Get.src ~show:(fun v ->
      string_of_int (SInt32.to_int v));
  getter Schemas.ipv4 Schemas.ipv4_dst ~get:G_ipv4.Get.dst ~show:(fun v ->
      string_of_int (UInt32.to_int v));
  int_getter Schemas.ipv4 Schemas.ipv4_src ~get:G_ipv4.Get.src_int
    ~to_int:SInt32.to_int;
  int_getter Schemas.ipv4 Schemas.ipv4_dst ~get:G_ipv4.Get.dst_int
    ~to_int:UInt32.to_int;
  int_getter Schemas.mixed Schemas.mixed_f ~get:G_mixed.Get.f_int
    ~to_int:SInt32.to_int;
  getter Schemas.checked Schemas.checked_proto ~get:G_checked.Get.proto ~show:u8;
  getter Schemas.mixed Schemas.mixed_b ~get:G_mixed.Get.b ~show:(fun v ->
      string_of_int (SInt16.to_int v));
  getter Schemas.mixed Schemas.mixed_e ~get:G_mixed.Get.e ~show:(fun v ->
      string_of_int (UInt32.to_int v));
  getter Schemas.mixed Schemas.mixed_f ~get:G_mixed.Get.f ~show:(fun v ->
      string_of_int (SInt32.to_int v));
  getter Schemas.mixed Schemas.mixed_h
    ~get:(fun b o ->
      Result.map (fun v -> (v : G_mixed.Value.h :> int)) (G_mixed.Get.h b o))
    ~show:string_of_int;
  getter Schemas.eth Schemas.eth_src
    ~get:(fun b o ->
      Result.map (fun v -> (v : G_eth.Value.src :> string)) (G_eth.Get.src b o))
    ~show:Fun.id

(* [is_valid buf off] holds exactly when [Codec.validate] accepts the
   record at [off]. In the buffer named [b] of length [len],
   [check b len off] passes exactly then, and each accessor
   [<field> b v] of the view [get] makes then reads what [Codec.get] reads at
   [off]. *)
let test_view () =
  let get codec field show =
    let read = Staged.unstage (Codec.get codec field) in
    fun buf off -> show (ok (read buf off))
  in
  let frag =
    get Schemas.ipv4 Schemas.ipv4_frag (fun v ->
        string_of_int (UInt16.to_int v))
  and src =
    get Schemas.ipv4 Schemas.ipv4_src (fun v -> string_of_int (SInt32.to_int v))
  and proto =
    get Schemas.checked Schemas.checked_proto (fun v ->
        string_of_int (UInt8.to_int v))
  in
  for _ = 1 to 2000 do
    let size = Codec.min_wire_size Schemas.ipv4 in
    let len = size + Random.State.int rand 8 in
    let buf = Bytes.init len (fun _ -> byte ()) in
    let off = Random.State.int rand (len - size + 3) - 1 in
    let codec_ok =
      outcome (fun () ->
          ok (Codec.validate Schemas.ipv4 buf off);
          "ok")
    in
    Alcotest.(check bool)
      "is_valid"
      (String.starts_with ~prefix:"ok" codec_ok)
      (G_ipv4.is_valid buf off);
    let expected =
      outcome (fun () ->
          ok (Codec.validate Schemas.ipv4 buf off);
          frag buf off ^ "," ^ src buf off)
    in
    let (Buf.Any b) = Buf.v buf in
    let len = Buf.length b in
    let read v =
      string_of_int (UInt16.to_int (G_ipv4.frag b v))
      ^ ","
      ^ string_of_int (G_ipv4.view_src_int b v)
    in
    let c = G_ipv4.check b len off in
    Alcotest.(check bool) "check" (G_ipv4.is_valid buf off) (G_ipv4.ok c);
    let got =
      outcome (fun () ->
          if G_ipv4.ok c then read (G_ipv4.get c)
          else (
            ok (G_ipv4.validate buf off);
            ""))
    in
    Alcotest.(check string) "ipv4 view" expected got
  done;
  for _ = 1 to 2000 do
    let size = Codec.min_wire_size Schemas.checked in
    let len = size + Random.State.int rand 8 in
    let buf = Bytes.init len (fun _ -> byte ()) in
    let off = Random.State.int rand (len - size + 3) - 1 in
    if Random.State.bool rand && off >= 0 && off + size <= len then
      fix_checked buf off;
    let codec_ok =
      outcome (fun () ->
          ok (Codec.validate Schemas.checked buf off);
          "ok")
    in
    Alcotest.(check bool)
      "is_valid"
      (String.starts_with ~prefix:"ok" codec_ok)
      (G_checked.is_valid buf off);
    let expected =
      outcome (fun () ->
          ok (Codec.validate Schemas.checked buf off);
          proto buf off)
    in
    let (Buf.Any b) = Buf.v buf in
    let got =
      outcome (fun () ->
          let c = G_checked.check b (Buf.length b) off in
          if G_checked.ok c then
            string_of_int (UInt8.to_int (G_checked.proto b (G_checked.get c)))
          else (
            ok (G_checked.validate buf off);
            ""))
    in
    Alcotest.(check string) "checked view" expected got
  done

(* A check that failed gives no view: [get] refuses it, on [bytes] and on a
   bigarray alike, and [Valid.v] on a bigarray answers the codec's error. *)
let test_failed_views () =
  let short = Bytes.make 3 '\xff' in
  let (Buf.Any b) = Buf.v short in
  let c = G_ipv4.check b (Buf.length b) 0 in
  Alcotest.(check bool) "failed" false (G_ipv4.ok c);
  Alcotest.(check bool)
    "get refuses" true
    (match G_ipv4.get c with
    | _ -> false
    | exception Invalid_argument _ -> true);
  let big =
    Bigarray.Array1.init Bigarray.char Bigarray.c_layout 3 (fun _ -> '\xff')
  in
  let (Buf.Bigstring.Any b) = Buf.Bigstring.v big in
  let c = G_ipv4_big.View.check b (Buf.Bigstring.length b) 0 in
  Alcotest.(check bool) "bigstring: failed" false (G_ipv4_big.View.ok c);
  Alcotest.(check bool)
    "bigstring: get refuses" true
    (match G_ipv4_big.View.get c with
    | _ -> false
    | exception Invalid_argument _ -> true);
  Alcotest.(check string)
    "bigstring: Valid.v answers the codec's error"
    (outcome (fun () ->
         ok (Codec.validate Schemas.ipv4 short 0);
         ""))
    (outcome (fun () ->
         ignore (ok (G_ipv4_big.Valid.v big));
         ""));
  let off_end = Bytes.make 40 '\x45' in
  let (Buf.Any b) = Buf.v off_end in
  let len = Buf.length b in
  let at off = G_ipv4.check b len off in
  Alcotest.(check bool) "negative offset" false (G_ipv4.ok (at (-1)));
  Alcotest.(check bool) "past the end" false (G_ipv4.ok (at 21));
  (* A record with no check of its own (Eth, 14 bytes) tests no sign of its
     own: its answer to a negative offset is refused all the same. *)
  List.iter
    (fun (off, ok) ->
      Alcotest.(check bool)
        (Fmt.str "no check, at %d" off)
        ok
        (G_eth.ok (G_eth.check b len off)))
    [
      (min_int, false);
      (-14, false);
      (-1, false);
      (0, true);
      (26, true);
      (27, false);
      (max_int, false);
    ]

(* The module generated on a bigarray, on a bigarray copy of a buffer,
   returns what the module on [bytes] returns on the buffer, or raises the
   same error. *)
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

let test_bigstring () =
  let of_big bb =
    String.init (Bigarray.Array1.dim bb) (Bigarray.Array1.get bb)
  in
  let big size encode decode b o =
    let r = ok (decode b o) in
    let out = bigstring (Bytes.make size '\000') in
    encode r out 0;
    of_big out
  in
  same_on_bigstring Schemas.ipv4
    ~on_bytes:(fun b o ->
      encoded G_ipv4.min_size G_ipv4.encode G_ipv4.decode b o)
    ~on_big:(fun b o ->
      big G_ipv4_big.min_size G_ipv4_big.encode G_ipv4_big.decode b o);
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b o ->
      ok (G_checked.validate b o);
      "")
    ~on_big:(fun b o ->
      ok (G_checked_big.validate b o);
      "");
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b o ->
      encoded G_checked.min_size G_checked.encode G_checked.decode b o)
    ~on_big:(fun b o ->
      big G_checked_big.min_size G_checked_big.encode G_checked_big.decode b o);
  same_on_bigstring Schemas.mixed
    ~on_bytes:(fun b o ->
      encoded G_mixed.min_size G_mixed.encode G_mixed.decode b o)
    ~on_big:(fun b o ->
      big G_mixed_big.min_size G_mixed_big.encode G_mixed_big.decode b o);
  same_on_bigstring Schemas.eth
    ~on_bytes:(fun b o ->
      encoded G_eth.min_size G_eth.encode G_eth.decode b o
      ^ (ok (G_eth.Get.src b o) :> string))
    ~on_big:(fun b o ->
      big G_eth_big.min_size G_eth_big.encode G_eth_big.decode b o
      ^ (ok (G_eth_big.Get.src b o) :> string));
  same_on_bigstring Schemas.ipv4
    ~on_bytes:(fun b o ->
      string_of_int
        (ok (G_ipv4.Get.src_int b o) + (ok (G_ipv4.Get.proto b o) :> int)))
    ~on_big:(fun b o ->
      string_of_int
        (ok (G_ipv4_big.Get.src_int b o)
        + (ok (G_ipv4_big.Get.proto b o) :> int)));
  same_on_bigstring ~fix:fix_checked Schemas.checked
    ~on_bytes:(fun b _ ->
      ok (G_checked.validate b 0);
      string_of_int
        ((ok (G_checked.Get.proto b 0) :> int)
        + (ok (G_checked.Get.ihl b 0) :> int)))
    ~on_big:(fun b _ ->
      let v = ok (G_checked_big.Valid.v b) in
      string_of_int
        ((G_checked_big.read_proto v :> int) + (G_checked_big.read_ihl v :> int)));
  same_on_bigstring Schemas.wide
    ~on_bytes:(fun b o ->
      string_of_int (G_wide.c_of_word (ok (G_wide.Get.c_word b o))))
    ~on_big:(fun b o ->
      string_of_int (G_wide.c_of_word (ok (G_wide_big.Get.c_word b o))));
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
          G_checked.write_int buf off ~version:a ~ihl:b ~length:c ~ttl:d
            ~proto:e;
          Bytes.to_string buf)
    in
    let on_big =
      outcome (fun () ->
          let buf = bigstring (Bytes.make len '\xaa') in
          G_checked_big.write_int buf off ~version:a ~ihl:b ~length:c ~ttl:d
            ~proto:e;
          Diff_ocaml.string_of_big buf)
    in
    Alcotest.(check string) "write" on_bytes on_big
  done

(* A record that does not fit reports the first field in order that fails a
   check where it fits, before the end of input, as the validator 3d.exe
   generates does: [Checked]'s where clause is checked with [Length], the last
   field it reads. The generated module, on bytes and on a bigarray, answers
   what the codec does. *)
let test_short_faults () =
  let error = function
    | Ok _ -> "ok"
    | Error (e : Error.t) -> Fmt.str "%a" Error.pp e
  in
  List.iter
    (fun (s, expected) ->
      let buf = Bytes.of_string s in
      let check name got =
        Alcotest.(check string) (Fmt.str "%s %S" name s) expected got
      in
      check "codec decode" (error (Codec.decode Schemas.checked buf 0));
      check "codec validate" (error (Codec.validate Schemas.checked buf 0));
      check "generated decode" (error (G_checked.decode buf 0));
      check "generated validate" (error (G_checked.validate buf 0));
      check "generated bigstring decode"
        (error (G_checked_big.decode (bigstring buf) 0));
      check "generated bigstring validate"
        (error (G_checked_big.validate (bigstring buf) 0)))
    [
      ("\x55", "Version@0: field constraint violated (value 5)");
      ("\x45\x00\x00\x00", "@0: where clause violated");
      ("\x45\x00\x50\x00", "TTL@3: field constraint violated (value 0)");
      ("\x45\x00\x50\x01", "@0: unexpected EOF: expected 5 bytes, got 4");
    ]

(* The OxCaml target checks the hot paths for allocation, marks the cold ones
   as raising, takes each buffer it does not keep and the continuations of
   [decode_with] [local], and holds a 32-bit or 64-bit value unboxed. The stock compiler cannot build
   it; the OxCaml tests do. *)
let test_oxcaml () =
  let src = ml ~target:`OxCaml Schemas.ipv4 in
  List.iter
    (fun s -> Alcotest.(check bool) s true (contains src s))
    [
      "let[@inline][@zero_alloc] is_valid";
      "let[@inline always] check (_b : 'b Wire_runtime.Buf.t @ local)";
      "view_src_int ((b : 'b Wire_runtime.Buf.t) @ local) (v : 'b view)";
      "write_at ((b : 'b Wire_runtime.Buf.t) @ local)";
      "[@zero_alloc] write_int";
      "[@zero_alloc assume error] fail";
      "let[@inline] decode_with (make @ local) ~(fail @ local) (buf @ local) \
       off";
      "let[@inline] src (buf @ local) off";
      "let[@inline][@zero_alloc] write (buf @ local) off";
      "module Int32_u = Wire_runtime.Target.Int32_u";
    ];
  (* A write checks what it wrote only where the record has checks. *)
  Alcotest.(check bool)
    "no validate_written without checks" false
    (contains src "validate_written");
  Alcotest.(check bool)
    "validate_written" true
    (contains
       (ml ~target:`OxCaml Schemas.checked)
       "let[@inline][@zero_alloc] validate_written (buf @ local) off");
  (* Every allocation check runs: only a path that ends in a raise is assumed
     to allocate nothing, so none is taken on trust. *)
  Alcotest.(check bool)
    "no unchecked zero_alloc" false
    (contains src "[@zero_alloc assume]");
  let stock = ml Schemas.ipv4 in
  Alcotest.(check bool)
    "stock has no OxCaml syntax" false
    (contains stock "exclave_" || contains stock "#{")

(* [~simd:true] gives a 4-byte record whose fields each sit in one byte an [X4]
   of NEON lanes, each field at its byte's bits of a little-endian lane, and
   gives every other record none. *)
let test_simd () =
  let ox ?simd c = ml ~target:`OxCaml ?simd c in
  let lanes = ox ~simd:true Schemas.lanes in
  List.iter
    (fun s -> Alcotest.(check bool) s true (contains lanes s))
    [
      "module X4 : sig";
      "val ( ++ ) : part -> part -> part";
      "let[@inline] a w = V.logand (V.shift_right_logical 4 w)";
      "let[@inline] c w = V.logand (V.shift_right_logical 8 w)";
      "let[@inline] d w = V.logand (V.shift_right_logical 21 w)";
      "let[@inline] f w = (V.shift_right_logical 26 w)";
      "let[@inline] write_int_at";
    ];
  Alcotest.(check bool)
    "no X4 without ~simd" false
    (contains (ox Schemas.lanes) "module X4");
  List.iter
    (fun (name, src) ->
      Alcotest.(check bool) name false (contains src "module X4"))
    [
      ("ipv4", ox ~simd:true Schemas.ipv4);
      ("small", ox ~simd:true Schemas.small);
      ("checked", ox ~simd:true Schemas.checked);
      ("mixed", ox ~simd:true Schemas.mixed);
    ];
  match ml ~target:`OCaml ~simd:true Schemas.lanes with
  | _ -> Alcotest.fail "~simd on the OCaml target is refused"
  | exception Invalid_argument m ->
      Alcotest.(check bool) m true (contains m "OxCaml")

(* [Value.<f>_low x] is the carrier of [x]'s low bits, as [Value.<f>] of the
   masked value is, for any [x]. *)
let test_low () =
  for _ = 1 to 1000 do
    let x = Random.State.bits rand - (1 lsl 29) in
    let check name width low checked =
      Alcotest.(check int) name (checked (x land ((1 lsl width) - 1))) (low x)
    in
    let open G_lanes.Value in
    check "a" 4 (fun x -> (a_low x :> int)) (fun x -> (a x :> int));
    check "d" 3 (fun x -> (d_low x :> int)) (fun x -> (d x :> int));
    check "f" 6 (fun x -> (f_low x :> int)) (fun x -> (f x :> int))
  done

(* A frame of [len] bytes holding [n], then [rest]. *)
let frame n rest =
  let b = Bytes.create (4 + String.length rest) in
  Bytes.set_int32_be b 0 n;
  Bytes.blit_string rest 0 b 4 (String.length rest);
  b

let sized_reference =
  Diff_ocaml.Reference
    {
      label = "sized";
      codec = Schemas.sized;
      known = None;
      project =
        (fun _ (n, body, tail) ->
          [
            ("n", UInt32.to_int n);
            ("body_present", if Option.is_some body then 1 else 0);
            ("tail", UInt8.to_int tail);
          ]);
      inputs = (fun b -> [ b ]);
    }

let overflow_reference =
  Diff_ocaml.Reference
    {
      label = "overflow";
      codec = Schemas.overflow;
      known = None;
      project =
        (fun _ (a, hi, lo, b, c) ->
          [
            ("a", UInt32.to_int a);
            ("hi", hi);
            ("lo", lo);
            ("b", UInt32.to_int b);
            ("c", UInt32.to_int c);
          ]);
      inputs = (fun b -> [ b ]);
    }

let scan_ops name fields decode =
  {
    Diff_ocaml.scan_name = name;
    scan_fields = fields;
    scan = decode;
    scan_batch = None;
    scan_index = None;
  }

(* The scanner accepts what the codec accepts when a condition, size or check
   computes near the top of its type. *)
let test_scan_overflow () =
  let sized =
    scan_ops "S_sized" [ "n"; "body_present"; "tail" ] (fun b o n ->
        S_sized.fold b o n
          ~invalid:(fun () -> None)
          ~valid:(fun ~n ~body_present ~tail ->
            Some
              [
                ("n", n);
                ("body_present", if body_present then 1 else 0);
                ("tail", tail);
              ]))
  in
  let overflow =
    scan_ops "S_overflow" [ "a"; "hi"; "lo"; "b"; "c" ] (fun b o n ->
        S_overflow.fold b o n
          ~invalid:(fun () -> None)
          ~valid:(fun ~a ~hi ~lo ~b ~c ->
            Some [ ("a", a); ("hi", hi); ("lo", lo); ("b", b); ("c", c) ]))
  in
  List.iter
    (fun n ->
      List.iter
        (fun rest ->
          Diff_ocaml.check_scan sized_reference [ sized ] (frame n rest);
          Diff_ocaml.check_scan overflow_reference [ overflow ] (frame n rest))
        [ ""; "\001"; "\001\002"; "abcdefghij"; String.make 40 '\255' ])
    [ 0l; 1l; 2l; 3l; 6l; 46341l; 0x5a827999l; 0x7fffffffl; -1l ]

let skipped_reference =
  let int8 = UInt8.to_int in
  let opt name v =
    [
      (name ^ "_present", if Option.is_some v then 1 else 0);
      (name, Option.fold ~none:0 ~some:int8 v);
    ]
  in
  Diff_ocaml.Reference
    {
      label = "skipped";
      codec = Schemas.skipped;
      known = None;
      project =
        (fun _ (n, x, a, b) ->
          [ ("n", UInt32.to_int n); ("x", int8 x) ] @ opt "a" a @ opt "b" b);
      inputs = (fun b -> [ b ]);
    }

(* A condition that facts decide is still evaluated where the codec
   evaluates it: when [x = 4] holds, [n * n > max_int && x = 3] is false. *)
let test_scan_skipped () =
  let skipped =
    scan_ops "S_skipped" [ "n"; "x"; "a_present"; "a"; "b_present"; "b" ]
      (fun b o n ->
        S_skipped.fold b o n
          ~invalid:(fun () -> None)
          ~valid:(fun ~n ~x ~a_present ~a ~b_present ~b ->
            Some
              [
                ("n", n);
                ("x", x);
                ("a_present", if a_present then 1 else 0);
                ("a", a);
                ("b_present", if b_present then 1 else 0);
                ("b", b);
              ]))
  in
  List.iter
    (fun n ->
      List.iter
        (fun x ->
          List.iter
            (fun rest ->
              Diff_ocaml.check_scan skipped_reference [ skipped ]
                (frame n (String.make 1 (Char.chr x) ^ rest)))
            [ ""; "\007"; "\007\008" ])
        [ 3; 4; 5 ])
    [ 0l; 1l; 46341l; 0x5a827999l; 0x7fffffffl; -1l ]

let huge_reference =
  Diff_ocaml.Reference
    {
      label = "huge";
      codec = Schemas.huge;
      known = None;
      project =
        (fun _ (flag, _, tail) ->
          [ ("flag", UInt8.to_int flag); ("tail", UInt8.to_int tail) ]);
      inputs = (fun b -> [ b ]);
    }

(* Generating code for a prefix takes time in its number of fields, whatever
   its size in bytes. *)
let test_huge_prefix () =
  let array =
    Codec.v "HugeArray"
      (fun flag _ tail -> (flag, tail))
      Codec.
        [
          Field.v "Flag" uint8 $ fst;
          ( Field.v "Body" (byte_array ~size:(int (0xFFFF_FFFF - 8))) $ fun _ ->
            "" );
          Field.v "Tail" uint8 $ snd;
        ]
  in
  ignore (ml array : string);
  let huge =
    scan_ops "S_huge" [ "flag"; "tail" ] (fun b o n ->
        S_huge.fold b o n
          ~invalid:(fun () -> None)
          ~valid:(fun ~flag ~tail -> Some [ ("flag", flag); ("tail", tail) ]))
  and step =
    scan_ops "S_huge step" [] (fun b o n ->
        if Int.compare (S_huge.fold_step b o n ~init:0) 0 >= 0 then Some []
        else None)
  in
  List.iter
    (fun s ->
      Diff_ocaml.check_scan huge_reference [ huge; step ] (Bytes.of_string s))
    [ ""; "\001"; "\001\002"; String.make 64 '\001' ]

(* A codec whose constant offsets overflow the native int holds more than any
   buffer: it is refused when built, by the codec and so by the generator. *)
let test_scan_deep () =
  List.iter
    (fun (depth, big) ->
      match scanner (Schemas.deep depth big) with
      | _ -> Alcotest.failf "deep %d %d: scanner generated" depth big
      | exception Invalid_argument _ -> ())
    [ (32, Sys.max_string_length - 1); (3, Sys.max_string_length / 2) ]

(* Optional fields are written once each: the code after one is joined, or
   copied into its branches only while it is short, holds no optional field
   and the copies on a path stay few, and an output under more than
   [Plan.slot_depth] of them is bound once, so the code grows linearly with
   the number of optional fields, in a run or nested, whatever it hands out.
   The test codecs nest a record per field and their output names lengthen:
   lines and words, not bytes, measure the code, a word being a run of
   letters, digits and underscores, which a long line cannot hide. *)
let lines s = List.length (String.split_on_char '\n' s)

let words s =
  let word = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let n = ref 0 and inside = ref false in
  String.iter
    (fun c ->
      if word c && not !inside then incr n;
      inside := word c)
    s;
  !n

(* [n] optional fields nested inside each other that hand out every output.
   The OCaml target writes two walks ([fold] and [fold_frames]; [decode]
   calls [fold]) and the OxCaml target six, [fold_frames_unboxed] two of them:
   one for a stride that holds the fixed bytes of a frame, which tests none
   of their bounds, and one for a shorter stride. Past the first four levels,
   whose outputs are immutable, a level costs a walk about 21 lines: its
   three slots, the bound checks and reads of its two prefixes, its
   presence, its join and the jumps to it, or the short code after it copied
   into its branches; and about 150 words on OCaml, 215 on OxCaml, whose
   arithmetic is spelled as calls. [pp] prints the three outputs of a level
   on three lines. *)
let nested_budget target n =
  match target with
  | `OCaml -> ((47 * n) + 72, (340 * n) + 250)
  | `OxCaml -> ((132 * n) + 72, (1380 * n) + 250)

(* The scanner's text before its views, and its views, which start with the
   type of a view. A level of [gated] or [nested] costs the views a module
   of some 133 lines and 860 words, on either target: its types, [ok], [get],
   an accessor per field, each of which tests the presence again and places
   the field after the optional one, a setter per field at a constant offset,
   the function that finds where the record ends, and its values: the record
   type and its printer, [of_view], [encode_at], [size], and the checks and
   the walk that find the codec's error, some 70 lines and 420 words of
   these. *)
let scan_and_views src =
  let marker = "type 'b view = int" in
  let n = String.length marker in
  let rec find i =
    if i + n > String.length src then Alcotest.failf "no views in the scanner"
    else if String.sub src i n = marker then i
    else find (i + 1)
  in
  let i = find 0 in
  (String.sub src 0 i, String.sub src i (String.length src - i))

let views_budget n = ((140 * n) + 60, (900 * n) + 400)

let test_size () =
  List.iter
    (fun (name, codec, fields) ->
      List.iter
        (fun target ->
          let size n =
            let scan, views =
              scan_and_views (scanner ~target ?fields (codec n))
            in
            let max_lines, max_words = views_budget n in
            if lines views > max_lines || words views > max_words then
              Alcotest.failf
                "%s: the views of %d optional fields: %d lines and %d words, \
                 over %d and %d"
                name n (lines views) (words views) max_lines max_words;
            lines scan
          in
          let s8 = size 8 and s16 = size 16 and s32 = size 32 in
          if s32 - s16 > (2 * (s16 - s8)) + 2 then
            Alcotest.failf
              "%s: 8, 16 and 32 optional fields: %d, %d and %d lines" name s8
              s16 s32;
          if s32 > 160 * 32 then
            Alcotest.failf "%s: 32 optional fields: %d lines" name s32)
        [ `OCaml; `OxCaml ])
    [
      ("in a run", Schemas.gated, None);
      ("in a run, checks only", Schemas.gated, Some []);
      ("nested, checks only", Schemas.nested, Some []);
      ("nested", Schemas.nested, None);
    ];
  List.iter
    (fun (target, name) ->
      List.iter
        (fun n ->
          let src, _ = scan_and_views (scanner ~target (Schemas.nested n)) in
          let max_lines, max_words = nested_budget target n in
          if lines src > max_lines || words src > max_words then
            Alcotest.failf
              "%s: %d nested optional fields, every output: %d lines and %d \
               words, over %d and %d"
              name n (lines src) (words src) max_lines max_words)
        [ 8; 16; 32; 64 ])
    [ (`OCaml, "OCaml"); (`OxCaml, "OxCaml") ]

(* The scanner of nested optional fields that hands out every output
   allocates nothing where it calls its consumer by name, beyond the floats
   of the measure: a frame with no optional field gives zeros, one with all
   twelve levels a one for each of its 35 outputs. Dune's dev profile compiles
   wire.runtime [-opaque], so its 64-bit load returns a boxed [int64]: the
   batch then allocates and only the answers are checked. *)
let test_nested_no_alloc () =
  let stride = 32 and frames = 4096 in
  let len = stride * frames in
  let buf =
    Bytes.init len (fun i ->
        if i < stride then '\000'
        else if i < 2 * stride then '\001'
        else Char.chr (Random.State.int rand 2))
  in
  let one off = S_nested.fold_step buf off stride ~init:0 in
  Alcotest.(check int) "no optional field" 0 (one 0);
  Alcotest.(check int) "every optional field" 35 (one stride);
  let each () =
    let acc = ref 0 in
    for i = 0 to frames - 1 do
      acc := S_nested.fold_step buf (i * stride) stride ~init:!acc
    done;
    !acc
  in
  let batch () = S_nested.fold_step_frames buf 0 len ~stride ~init:0 in
  let measured what f =
    let before = Gc.minor_words () in
    let v = Sys.opaque_identity (f ()) in
    let words = Gc.minor_words () -. before in
    if words >= 64. && not Boxed_loads.v then
      Alcotest.failf "%s: %.0f words allocated over %d frames" what words frames;
    v
  in
  Alcotest.(check int)
    "fold_step and fold_step_frames"
    (measured "fold_step" each)
    (measured "fold_step_frames" batch);
  let offs = Array.init frames (fun i -> i * stride)
  and lens = Array.make frames stride in
  let index () = S_nested.fold_step_index buf ~offs ~lens ~init:0 in
  Alcotest.(check int)
    "fold_step and fold_step_index"
    (measured "fold_step" each)
    (measured "fold_step_index" index);
  match S_nested.fold_step_index buf ~offs ~lens:[| stride |] ~init:0 with
  | _ -> Alcotest.fail "fold_step_index: offs and lens of different lengths"
  | exception Invalid_argument _ -> ()

let refused what generate =
  match generate () with
  | _ -> Alcotest.failf "%s: generated" what
  | exception Invalid_argument _ -> ()

let test_scanner_refusals () =
  let pair n1 n2 =
    Codec.v "Pair"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 uint8 $ fst; Field.v n2 uint8 $ snd ]
  in
  refused "two outputs of one name" (fun () -> scanner (pair "a_b" "a_B"));
  refused "an unknown output" (fun () -> scanner ~fields:[ "c" ] (pair "a" "b"));
  Alcotest.(check string)
    "a structured codec through ml" (scanner Schemas.sized) (ml Schemas.sized);
  Alcotest.(check (list (pair string bool)))
    "outputs"
    [ ("n", false); ("body_present", true); ("tail", false) ]
    (Wire_ocaml.outputs Schemas.sized)

(* Every output is named by the labels on its path, never by a variable of
   the plan. A codec's fields are all named; the anonymous fields of a 3D
   struct reach a codec only inside a struct type, which the scanner
   refuses. *)
let test_scanner_outputs () =
  let inner =
    Codec.v "Inner"
      (fun a b -> (a, b))
      Codec.[ Field.v "a" uint8 $ fst; Field.v "b" uint8 $ snd ]
  in
  let tag = Field.v "tag" uint8 in
  let outer =
    Codec.v "Outer"
      (fun t x y -> (t, x, y))
      Codec.
        [
          (tag $ fun (t, _, _) -> t);
          ( Field.optional "x"
              ~present:Expr.(Field.ref tag = int 1)
              (codec inner)
          $ fun (_, x, _) -> x );
          ( Field.v "y" (nested_at_most ~size:(int 4) (codec inner))
          $ fun (_, _, y) -> y );
        ]
  in
  Alcotest.(check (list (pair string bool)))
    "outputs"
    [
      ("tag", false);
      ("x_present", true);
      ("x_a", false);
      ("x_b", false);
      ("y_a", false);
      ("y_b", false);
    ]
    (Wire_ocaml.outputs outer);
  let src = scanner outer in
  Alcotest.(check bool)
    "valid takes the outputs by name" true
    (contains src "valid ~tag:" && contains src "~x_present:"
   && contains src "~y_b:");
  let padded =
    Everparse.Raw.(struct_ "Padded" [ anon_field uint8; field "z" uint8 ])
  in
  let holder =
    Codec.v "Holder" Fun.id
      Codec.[ Field.v "h" (Everparse.Raw.struct_typ padded) $ Fun.id ]
  in
  refused "an anonymous field in a struct type" (fun () ->
      Wire_ocaml.outputs holder);
  refused "an anonymous field in a struct type, scanner" (fun () ->
      scanner holder)

(* Code whose values all fit 31 bits loads on every target; code holding a
   wider one checks the int size when it loads. *)
let test_portable () =
  let one t =
    Codec.v "One" Fun.id
      Codec.
        [
          Field.v "x" t ~self_constraint:(fun x ->
              Expr.(x + int 0x3FFF_FF00 >= int 0))
          $ Fun.id;
        ]
  in
  let guarded src = contains src "Sys.int_size <= 32" in
  Alcotest.(check bool) "u8 plus 2^30 - 256" false (guarded (ml (one uint8)));
  Alcotest.(check bool) "u16 plus 2^30 - 256" true (guarded (ml (one uint16be)));
  Alcotest.(check bool) "u8 scanner" false (guarded (scanner (one uint8)));
  Alcotest.(check bool) "u16 scanner" true (guarded (scanner (one uint16be)))

(* An Ethernet, IPv4 and TCP header of [Packet_stack.codec] in 64 bytes,
   which [edit] changes. *)
let stack_frame edit =
  let b = Bytes.make 64 '\000' in
  Bytes.set_uint16_be b 12 0x0800;
  Bytes.set_uint8 b 14 0x45;
  Bytes.set_uint16_be b 16 50;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 6;
  Bytes.set_int32_be b 26 0xc0a80001l;
  Bytes.set_int32_be b 30 0xc0a80002l;
  Bytes.set_uint16_be b 34 443;
  Bytes.set_uint16_be b 36 51000;
  Bytes.set_uint8 b 46 0x50;
  edit b;
  b

(* On OCaml the step functions read the Ethernet type from the word that also
   holds the start of the IPv4 header, loaded before the dispatch on the type.
   They give the codec's answers on records each check rejects and on frames
   cut around the bounds that word straddles. *)
let test_shared_words () =
  let src = scanner ~step:true Packet_stack.codec in
  Alcotest.(check bool)
    "the Ethernet type from the word at 12" true
    (contains src "(Int64.shift_right_logical w12 48)");
  let case =
    List.find
      (fun (Diff_codecs.Scan c) -> c.label = "scan_packet")
      Diff_codecs.scan_cases
  in
  let reference = Diff_ocaml.of_scan_case case
  and backends = (List.assoc "scan_packet" Ocaml_index.scanners) () in
  let records =
    [
      ("TCP", true, ignore);
      ("not IPv4", true, fun b -> Bytes.set_uint16_be b 12 0x86dd);
      ("version", false, fun b -> Bytes.set_uint8 b 14 0x65);
      ("header length", false, fun b -> Bytes.set_uint8 b 14 0x44);
      ("total length", false, fun b -> Bytes.set_uint16_be b 16 16);
      ("total past the frame", false, fun b -> Bytes.set_uint16_be b 16 51);
      ("fragment", true, fun b -> Bytes.set_uint16_be b 20 1);
      ( "UDP length",
        false,
        fun b ->
          Bytes.set_uint8 b 23 17;
          Bytes.set_uint16_be b 38 7 );
      ("data offset", false, fun b -> Bytes.set_uint8 b 46 0x40);
    ]
  in
  let frames =
    List.map
      (fun (what, ok, edit) ->
        let b = stack_frame edit in
        Alcotest.(check bool)
          (what ^ " accepted") ok
          (Result.is_ok (Codec.decode Packet_stack.codec b 0));
        b)
      records
  in
  Diff_ocaml.check_scan ~strides:[ 64 ] ~expand:false reference backends
    (Bytes.concat Bytes.empty frames);
  let ip = stack_frame ignore
  and other = stack_frame (fun b -> Bytes.set_uint16_be b 12 0x86dd) in
  List.iter
    (fun n ->
      let cut b = Bytes.sub b 0 n in
      Diff_ocaml.check_scan ~strides:[ n ] ~expand:false reference backends
        (Bytes.concat Bytes.empty [ cut ip; cut other; cut ip ]))
    [ 13; 14; 19; 20; 21; 33; 34; 35 ]

(* The decisions the scanner of [Packet_stack.codec] takes, on each target and
   with the step functions: the IPv4 header's two bitfields tested as one range
   of their byte, the IPv4 options' bound left to the check of the bytes after
   them, the payload's bound checked once before the dispatch on the protocol
   and hoisted in each case, the frame loop's shape, in the step functions the
   words the Ethernet and IPv4 headers share, and the TCP header's base.
   Collecting them writes the
   scanner it describes, which examples/generated/stack.ml pins, and leaves the
   next one written unchanged. *)
let test_decisions () =
  let open Wire_ocaml.Plan in
  let c = Packet_stack.codec in
  let src = scanner c in
  let ocaml = decisions c in
  Alcotest.(check string) "the scanner is unchanged" src (scanner c);
  let has ds what p = Alcotest.(check bool) what true (List.exists p ds) in
  let start = { var = None; at = 0 } in
  has ocaml "fold holds ints" (function
    | Walk { name = "fold"; values = `Int; known = 0 } -> true
    | _ -> false);
  has ocaml "Ethernet tested" (function
    | Prefix_bound { at; window = "stop"; bytes = 14; tested = true } ->
        at = start
    | _ -> false);
  has ocaml "Ethernet known in the frame loop" (function
    | Prefix_bound { at; window = "stop"; bytes = 14; tested = false } ->
        at = start
    | _ -> false);
  has ocaml "version and IHL as one range" (function
    | Word_range { lo = 0x45; hi = 0x4f; _ } -> true
    | _ -> false);
  has ocaml "IPv4 options left to the next check" (function
    | Span_bound { at = { var = None; at = 34 }; bound = Next; _ } -> true
    | _ -> false);
  has ocaml "the payload checked once, before the dispatch" (function
    | Dispatch { cases = 3; hoisted = Some _; join = Copied; _ } -> true
    | _ -> false);
  has ocaml "the payload's bound hoisted in a case" (function
    | Span_bound { bound = Hoisted; window = "stop"; _ } -> true
    | _ -> false);
  has ocaml "four frames an iteration, their dispatch fields read ahead"
    (function
    | Frame_loop
        {
          name = "fold_frames";
          need = 34;
          frames = 4;
          short = Some Apart;
          ahead = [ ("f14_ipv4_protocol", 23, 1) ];
        } ->
        true
    | _ -> false);
  has (decisions ~target:`OxCaml c)
    "four frames an iteration on OxCaml, their dispatch fields read ahead"
    (function
    | Frame_loop
        {
          name = "fold_frames";
          need = 34;
          frames = 4;
          short = Some Inline;
          ahead = [ ("f14_ipv4_protocol", 23, 1) ];
        } ->
        true
    | _ -> false);
  has
    (decisions ~folds:(step_folds ()) ~visitors:step_visitors c)
    "the IPv4 header reads the word the Ethernet type is in"
    (function
      | Shared_words { at = 14; window = "stop"; held = [ 12 ]; _ } -> true
      | _ -> false);
  has ocaml "the IPv4 header cut into a function of its own" (function
    | Cut { name = "fold_ipv4"; window = "stop"; known = 0 } -> true
    | _ -> false);
  let index ahead = function
    | Frame_loop
        {
          name = "fold_step_index";
          need = 34;
          frames = 2;
          short = None;
          ahead = a;
        } ->
        a = ahead
    | _ -> false
  in
  has
    (decisions ~folds:(step_folds ()) ~visitors:step_visitors c)
    "the index loop, pipelined, the next frame's dispatch bytes read ahead"
    (index [ ("f14_ipv4_protocol", 23, 1) ]);
  has
    (decisions ~target:`OxCaml ~folds:(step_folds ()) ~visitors:step_visitors c)
    "the index loop on OxCaml, the Ethernet type read ahead too"
    (index [ ("f3_ether_type", 12, 2); ("f14_ipv4_protocol", 23, 1) ]);
  (* The TCP header starts at [ihl * 4 - 20] bytes from 34: its base is the
     term and the constant is in each load. *)
  Alcotest.(check bool)
    "the TCP header's base" true
    (contains src "i64be buf (b14 + 14)");
  Alcotest.(check bool)
    "the TCP header's base on OxCaml" true
    (contains (scanner ~target:`OxCaml c) "(L.i64be_at buf (U.add b14 #14L))");
  (* A codec the scanner refuses has no decisions either. *)
  match decisions (Schemas.deep 32 (Sys.max_string_length - 1)) with
  | _ -> Alcotest.fail "decisions of a refused codec"
  | exception Invalid_argument _ -> ()

(* The entry of a scan tests [len < 0] only when the walk does not start with
   the bound check of a prefix, which rejects a negative [len]. Neither it
   nor a frame loop tests [off > Bytes.length buf]: with [len] not negative,
   [len > Bytes.length buf - off] rejects an [off] past the buffer. *)
let test_entry_bounds () =
  let full = "if off < 0 || len < 0 || len > Bytes.length buf - off then" in
  let past = "off > Bytes.length buf ||" in
  let short = "if off < 0 || len > Bytes.length buf - off then" in
  let stack = scanner Packet_stack.codec in
  let ox = scanner ~target:`OxCaml Packet_stack.codec in
  List.iter
    (fun (what, src) ->
      Alcotest.(check bool)
        (what ^ ": the short entry")
        true (contains src short);
      Alcotest.(check bool) (what ^ ": no full entry") false (contains src full);
      Alcotest.(check bool)
        (what ^ ": no test of off past the buffer")
        false (contains src past))
    [ ("OCaml", stack); ("OxCaml", ox) ];
  let region =
    Codec.v "Region" Fun.id
      Codec.[ Field.v "a" (nested ~size:(int 4) uint32be) $ Fun.id ]
  in
  Alcotest.(check bool)
    "a walk that starts with a region tests len < 0" true
    (contains (scanner region) full);
  Alcotest.(check bool)
    "a walk that starts with a region tests no off past the buffer" false
    (contains (scanner region) past)

(* A region's window holds the bytes the range of its size proves: in a region
   of constant size, the record it holds checks no bound and its exact end is
   not tested, while a region whose size a field gives checks both. *)
let test_constant_region () =
  let open Wire_ocaml.Plan in
  let leaf = Codec.v "Leaf" Fun.id Codec.[ Field.v "v" uint32be $ Fun.id ] in
  let n = Field.v "n" uint8 in
  let constant =
    Codec.v "Constant"
      (fun t a -> (t, a))
      Codec.
        [
          Field.v "t" uint8 $ fst;
          Field.v "a" (nested ~size:(int 4) (codec leaf)) $ snd;
        ]
  in
  let sized =
    Codec.v "Sized"
      (fun t a -> (t, a))
      Codec.
        [ n $ fst; Field.v "a" (nested ~size:(Field.ref n) (codec leaf)) $ snd ]
  in
  let leaf_bound tested = function
    | Prefix_bound { window = "n1"; bytes = 4; tested = t; _ } -> t = tested
    | _ -> false
  in
  Alcotest.(check bool)
    "the record in a constant region is known" true
    (List.exists (leaf_bound false) (decisions constant));
  Alcotest.(check bool)
    "the record in a sized region is tested" true
    (List.exists (leaf_bound true) (decisions sized));
  Alcotest.(check bool)
    "a constant region's end is not tested" false
    (contains (scanner constant) "<> n1)");
  Alcotest.(check bool)
    "a sized region's end is tested" true
    (contains (scanner sized) "<> n1)")

(* A region whose contents load no byte leaves its bound to the check after
   it, which covers its bytes; one whose contents are read checks its own. *)
let test_region_left_to_next () =
  let open Wire_ocaml.Plan in
  let size = Field.v "size" uint8 in
  let c =
    Codec.v "Exact"
      (fun size value tail -> (size, value, tail))
      Codec.
        [
          (size $ fun (x, _, _) -> x);
          ( Field.v "value" (nested ~size:(Field.ref size) uint16be)
          $ fun (_, x, _) -> x );
          (Field.v "tail" uint8 $ fun (_, _, x) -> x);
        ]
  in
  let region bound = function
    | Span_bound { size_var = "n1"; bound = b; _ } -> bound b
    | _ -> false
  in
  Alcotest.(check bool)
    "left to the next check when its contents are not read" true
    (List.exists
       (region (function Next -> true | _ -> false))
       (decisions ~folds:Wire_ocaml.[ fold ~outputs:[ "tail" ] "fold" ] c));
  Alcotest.(check bool)
    "checked when its contents are read" true
    (List.exists
       (region (function Checked _ -> true | _ -> false))
       (decisions c))

(* The structured cases' corpora, and the EverParse C validator when built. *)
let test_scanners () =
  List.iter
    (fun (Diff_codecs.Scan c as case) ->
      Diff_ocaml.scan_corpus
        ?oracle:(Diff_driver.oracle c.label)
        case
        ((List.assoc c.label Ocaml_index.scanners) ()))
    Diff_codecs.scan_cases

(* The codec and every scanner, alone and in a frame fold, accept the frames
   of [cases] marked accepted and reject the others. *)
let check_packet cases =
  let label = "scan_packet" in
  let codec =
    match
      List.find
        (fun (Diff_codecs.Scan c) -> c.label = label)
        Diff_codecs.scan_cases
    with
    | Diff_codecs.Scan c -> fun b -> Result.is_ok (Codec.decode c.codec b 0)
  in
  let backends = (List.assoc label Ocaml_index.scanners) () in
  List.iter
    (fun (name, b, accepted) ->
      let len = Bytes.length b in
      Alcotest.(check bool) (name ^ ": codec") accepted (codec b);
      List.iter
        (fun (s : Diff_ocaml.scan_ops) ->
          Alcotest.(check bool)
            (Fmt.str "%s: %s" name s.scan_name)
            accepted
            (Option.is_some (s.scan b 0 len));
          Option.iter
            (fun batch ->
              let frames =
                Option.value ~default:[] (batch (Bytes.cat b b) 0 (2 * len) len)
              in
              Alcotest.(check (list bool))
                (Fmt.str "%s: %s fold_frames" name s.scan_name)
                [ accepted; accepted ]
                (List.map Option.is_some frames))
            s.scan_batch)
        backends)
    cases

(* A frame that exactly fits a bound is accepted, and one a byte over is
   rejected. *)
let test_packet_bounds () = check_packet Diff_codecs.packet_bounds

(* The IPv4 version and header length byte is accepted exactly in
   [0x45, 0x4f]. *)
let test_packet_versions () = check_packet Diff_codecs.packet_versions

(* A backend sizes its ports from the plan's outputs: a signed field keeps its
   width on the wire and says it is signed, so a negative value survives. *)
let test_plan_signed_output () =
  let open Wire in
  let negative =
    Field.v "negative" int8 ~self_constraint:(fun x -> Expr.(x < int 0))
  in
  let codec = Codec.v "NegativeOutput" Fun.id Codec.[ negative $ Fun.id ] in
  let output =
    match Wire_ocaml.Plan.outputs (Wire_ocaml.Plan.v codec) with
    | [ output ] -> output
    | outputs -> Alcotest.failf "%d outputs, expected 1" (List.length outputs)
  in
  Alcotest.(check int) "wire width" 8 output.wire_width;
  Alcotest.(check bool) "signed" true output.signed;
  List.iter
    (fun value ->
      let bytes = Bytes.make 1 (Char.chr (value land 255)) in
      Alcotest.(check bool)
        (Fmt.str "%d accepted" value)
        true
        (Result.is_ok (Codec.decode codec bytes 0)))
    [ -128; -1 ]

(* The interface states the public API and hides the rest. The one for the
   default target is OCaml that js_of_ocaml and wasm_of_ocaml read; the one
   for OxCaml states the modes and the allocation checks. Compiling the
   generated modules checks each against its implementation; this checks what
   the text promises. *)
let test_interfaces () =
  let stock = mli Schemas.checked
  and ox = mli ~target:`OxCaml Schemas.checked in
  List.iter
    (fun name ->
      Alcotest.(check bool) ("hides " ^ name) false (contains stock name))
    [ "first_error"; "no_error"; "cast"; "codec_name"; "validate_written" ];
  Alcotest.(check bool) "stock: no mode" false (contains stock "@ local");
  Alcotest.(check bool) "stock: no check" false (contains stock "zero_alloc");
  List.iter
    (fun s -> Alcotest.(check bool) s true (contains ox s))
    [
      "val[@zero_alloc] is_valid : bytes @ local -> int -> bool @@ portable";
      "val pp : Format.formatter -> t -> unit @@ portable";
      "fail:(unit -> 'a) @ local ->";
    ];
  (* The constraints of the codec are documented with its fields. *)
  Alcotest.(check bool) "constraint" true (contains stock "where [");
  match mli Schemas.mapped with
  | _ -> Alcotest.fail "the interface of a refused codec was generated"
  | exception Invalid_argument _ -> ()

let suite =
  ( "wire_ocaml",
    [
      Alcotest.test_case "faults before the end of input" `Quick
        test_short_faults;
      Alcotest.test_case "scanned arithmetic past the native int" `Quick
        test_scan_overflow;
      Alcotest.test_case "wide arithmetic in a condition the facts decide"
        `Quick test_scan_skipped;
      Alcotest.test_case "offsets past the largest buffer" `Quick test_scan_deep;
      Alcotest.test_case "prefix of a huge fixed slice" `Quick test_huge_prefix;
      Alcotest.test_case "code size of optional fields" `Quick test_size;
      Alcotest.test_case "nested optional fields allocate nothing" `Quick
        test_nested_no_alloc;
      Alcotest.test_case "scanner refusals" `Quick test_scanner_refusals;
      Alcotest.test_case "plan signed output" `Quick test_plan_signed_output;
      Alcotest.test_case "scanner outputs are label paths" `Quick
        test_scanner_outputs;
      Alcotest.test_case "portable when values fit 31 bits" `Quick test_portable;
      Alcotest.test_case "structured scanners" `Quick test_scanners;
      Alcotest.test_case "packet bounds" `Quick test_packet_bounds;
      Alcotest.test_case "packet versions" `Quick test_packet_versions;
      Alcotest.test_case "a word shared across prefixes" `Quick
        test_shared_words;
      Alcotest.test_case "the scanner's decisions" `Quick test_decisions;
      Alcotest.test_case "the entry's bounds" `Quick test_entry_bounds;
      Alcotest.test_case "a region of constant size" `Quick test_constant_region;
      Alcotest.test_case "a region left to the next check" `Quick
        test_region_left_to_next;
      Alcotest.test_case "ipv4" `Quick test_ipv4;
      Alcotest.test_case "ipv4 record" `Quick test_ipv4_record;
      Alcotest.test_case "constraints, enum, where" `Quick test_checked;
      Alcotest.test_case "little-endian, signed, bitfields" `Quick test_mixed;
      Alcotest.test_case "under one word" `Quick test_small;
      Alcotest.test_case "byte arrays" `Quick test_eth;
      Alcotest.test_case "32-bit bitfields" `Quick test_wide;
      Alcotest.test_case "maps" `Quick test_mapped;
      Alcotest.test_case "write from plain values" `Quick test_write_int;
      Alcotest.test_case "stale code is refused" `Quick test_stale;
      Alcotest.test_case "the digest sees sign, cases and layout" `Quick
        test_digest;
      Alcotest.test_case "negative literals" `Quick test_signed;
      Alcotest.test_case "adjacent fields" `Quick test_packed;
      Alcotest.test_case "errors in the codec's order" `Quick test_refined;
      Alcotest.test_case "overflowing arithmetic" `Quick test_overflow;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
      Alcotest.test_case "no literal needs more than 31 bits" `Quick
        test_narrow_ints;
      Alcotest.test_case "OxCaml target" `Quick test_oxcaml;
      Alcotest.test_case "interfaces" `Quick test_interfaces;
      Alcotest.test_case "NEON lanes" `Quick test_simd;
      Alcotest.test_case "carriers from low bits" `Quick test_low;
      Alcotest.test_case "getters" `Quick test_getters;
      Alcotest.test_case "view at an offset" `Quick test_view;
      Alcotest.test_case "bigstring buffers" `Quick test_bigstring;
      Alcotest.test_case "failed views share nothing" `Quick test_failed_views;
    ] )
