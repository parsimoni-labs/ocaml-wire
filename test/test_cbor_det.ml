module D = Wire.Private.Cbor_det

let of_hex h =
  String.init
    (String.length h / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

let to_hex s =
  String.concat ""
    (List.map
       (fun c -> Fmt.str "%02x" (Char.code c))
       (List.of_seq (String.to_seq s)))

let error = Alcotest.testable D.pp_error ( = )

let pp_result ppf = function
  | Ok n -> Fmt.pf ppf "Ok %d" n
  | Error e -> Fmt.pf ppf "Error %a" D.pp_error e

let result = Alcotest.of_pp pp_result
let skip_all s = D.skip (Bytes.of_string s) ~off:0 ~limit:(String.length s)

(* [s] read whole: [of_string] and [skip] give one answer. *)
let read s =
  let v = D.of_string s in
  let k =
    match skip_all s with
    | Ok e when e < String.length s ->
        Error { D.at = e; reason = Trailing (String.length s - e) }
    | Ok _ -> Ok ()
    | Error e -> Error e
  in
  (match (v, k) with
  | Ok _, Ok () -> ()
  | Error e, Error e' -> Alcotest.check error "skip agrees" e e'
  | _ -> Alcotest.failf "skip and decode disagree on %s" (to_hex s));
  v

let accepts name s =
  match read s with
  | Ok v ->
      Alcotest.(check string)
        (name ^ " re-encodes") (to_hex s)
        (to_hex (D.to_string v))
  | Error e -> Alcotest.failf "%s refused: %a" name D.pp_error e

let refuses name s expected =
  match read s with
  | Ok _ -> Alcotest.failf "%s accepted" name
  | Error e -> Alcotest.check error name expected e

let refuses_hex (hex, at, reason) = refuses hex (of_hex hex) { at; reason }

let test_appendix_a () =
  let ok = ref 0 and refused = ref 0 in
  List.iter
    (fun (diag, verdict, hex) ->
      let s = of_hex hex in
      match verdict with
      | `Accept ->
          incr ok;
          accepts diag s
      | `Float at ->
          incr refused;
          refuses diag s { at; reason = Float }
      | `Indefinite at ->
          incr refused;
          refuses diag s { at; reason = Indefinite })
    Cbor_vectors.appendix_a;
  Alcotest.(check (pair int int)) "accepted, refused" (47, 34) (!ok, !refused)

let refuses_text name payload =
  let b = Buffer.create 16 in
  D.write_head b 3 (Int64.of_int (String.length payload));
  let at = Buffer.length b in
  Buffer.add_string b payload;
  refuses name (Buffer.contents b) { at; reason = Utf8 };
  match D.to_string (Text payload) with
  | _ -> Alcotest.failf "%s: text not UTF-8 encoded" name
  | exception Invalid_argument _ -> ()

let test_evercbor () =
  let ok = ref 0 and refused = ref 0 in
  List.iter
    (fun (name, verdict, hex) ->
      let s = of_hex hex in
      match verdict with
      | `Accept ->
          incr ok;
          accepts name s
      | `Prefix n -> (
          incr ok;
          Alcotest.check result name (Ok n) (skip_all s);
          match D.of_string s with
          | Error { reason = Trailing _; _ } -> ()
          | _ -> Alcotest.failf "%s: trailing bytes not refused" name)
      | `Refuse -> (
          incr refused;
          match read s with
          | Ok _ -> Alcotest.failf "%s accepted" name
          | Error _ -> ())
      | `Refuse_text ->
          incr refused;
          refuses_text name s)
    Cbor_vectors.evercbor;
  Alcotest.(check (pair int int)) "accepted, refused" (167, 196) (!ok, !refused)

(* A proper prefix of a data item is never one: every cut is an [Eof]. *)
let test_truncated () =
  List.iter
    (fun hex ->
      let s = of_hex hex in
      for n = 0 to String.length s - 1 do
        match read (String.sub s 0 n) with
        | Error { reason = Eof _; _ } -> ()
        | Ok _ -> Alcotest.failf "%s cut at %d accepted" hex n
        | Error e -> Alcotest.failf "%s cut at %d: %a" hex n D.pp_error e
      done)
    [
      "1b000000e8d4a51000";
      "c074323031332d30332d32315432303a30343a30305a";
      "a26161016162820203";
      "8301820203820405";
      "d818456449455446";
      "a3010203a10405a1a0f6f8ff";
    ]

(* Nesting has no bound, as in EverCBOR: an item far deeper than any stack
   decodes, re-encodes and prints, under js_of_ocaml and wasm_of_ocaml too. *)
let deep = 100_000

let deep_item name ~open_ ~leaf ~diag =
  let s = String.concat "" (List.init deep (fun _ -> open_)) ^ leaf in
  accepts name s;
  match D.of_string s with
  | Error e -> Alcotest.failf "%s: %a" name D.pp_error e
  | Ok v ->
      let printed = Fmt.str "%a" D.pp v in
      let squeezed =
        String.to_seq printed
        |> Seq.filter (fun c -> c <> ' ' && c <> '\n')
        |> String.of_seq
      in
      Alcotest.(check string) (name ^ " prints") diag squeezed

let test_deep () =
  let nest o c leaf =
    String.concat "" (List.init deep (fun _ -> o))
    ^ leaf
    ^ String.concat "" (List.init deep (fun _ -> c))
  in
  deep_item "arrays" ~open_:"\x81" ~leaf:"\x01" ~diag:(nest "[" "]" "1");
  deep_item "maps" ~open_:"\xa1\x00" ~leaf:"\x01" ~diag:(nest "{0:" "}" "1");
  deep_item "tags" ~open_:"\xc1" ~leaf:"\x01" ~diag:(nest "1(" ")" "1");
  Alcotest.check result "an empty array at the bottom"
    (Ok (deep + 1))
    (skip_all (String.make deep '\x81' ^ "\x80"));
  (* The encoder takes any nesting a value has. *)
  let v = ref (D.Uint 1L) in
  for _ = 1 to deep do
    v := Array [ !v ]
  done;
  Alcotest.(check int) "deep encode" (deep + 1) (String.length (D.to_string !v));
  (* A count the bytes cannot hold fails where they end. *)
  List.iter refuses_hex
    [
      ("9bffffffffffffffff00", 10, D.Eof 1);
      ("bb7fffffffffffffff0000", 11, Eof 1);
      ("5bffffffffffffffff00", 9, Eof max_int);
    ]

let test_maps () =
  accepts "keys of every major type"
    (of_hex "a8000020004100006100008000a000c00000f400");
  (* Bytewise order of the encodings, not length first. *)
  accepts "uint 32 before -1" (of_hex "a21820002000");
  accepts "\"b\" before \"aa\"" (of_hex "a261620062616100");
  List.iter refuses_hex
    [
      ("a201000100", 3, D.Duplicate);
      ("a202000100", 3, Unsorted);
      ("a22000182000", 3, Unsorted);
      ("a262616100616200", 5, Unsorted);
      ("a101a202000100", 5, Unsorted);
    ];
  Alcotest.(check string)
    "the encoder sorts" "a301616161620162616100"
    (to_hex
       (D.to_string
          (Map
             [ (Text "aa", Uint 0L); (Text "b", Uint 1L); (Uint 1L, Text "a") ])));
  Alcotest.(check string)
    "the encoder sorts nested maps" "a20081a26161006162006162a201000200"
    (to_hex
       (D.to_string
          (Map
             [
               (Text "b", Map [ (Uint 2L, Uint 0L); (Uint 1L, Uint 0L) ]);
               ( Uint 0L,
                 Array [ Map [ (Text "b", Uint 0L); (Text "a", Uint 0L) ] ] );
             ])));
  (match
     D.to_string
       (Map [ (Uint 1L, Uint 0L); (Uint 2L, Uint 0L); (Uint 1L, Uint 0L) ])
   with
  | _ -> Alcotest.fail "duplicate key encoded"
  | exception Invalid_argument _ -> ());
  Alcotest.(check string)
    "write_map sorts" "a201022003"
    (to_hex
       (let b = Buffer.create 8 in
        D.write_map b [ ("\x20", "\x03"); ("\x01", "\x02") ];
        Buffer.contents b));
  match D.to_string (Map [ (Uint 1L, Uint 0L); (Uint 1L, Uint 2L) ]) with
  | _ -> Alcotest.fail "duplicate key encoded"
  | exception Invalid_argument _ -> ()

let test_heads () =
  List.iter refuses_hex
    [
      ("1817", 0, D.Not_shortest);
      ("1900ff", 0, Not_shortest);
      ("1a0000ffff", 0, Not_shortest);
      ("1b00000000ffffffff", 0, Not_shortest);
      ("3800", 0, Not_shortest);
      ("5800", 0, Not_shortest);
      ("7800", 0, Not_shortest);
      ("9800", 0, Not_shortest);
      ("b800", 0, Not_shortest);
      ("d800", 0, Not_shortest);
      ("f800", 0, Simple);
      ("f81f", 0, Simple);
      ("f90000", 0, Float);
      ("fa00000000", 0, Float);
      ("fb0000000000000000", 0, Float);
      ("1c", 0, Reserved);
      ("fe", 0, Reserved);
      ("1f", 0, Indefinite);
      ("ff", 0, Indefinite);
      ("19", 1, Eof 2);
      ("", 0, Eof 1);
      ("62c328", 1, Utf8);
      ("63eda080", 1, Utf8);
      ("62c0af", 1, Utf8);
      ("64f4908080", 1, Utf8);
      ("0000", 1, Trailing 1);
    ];
  List.iter
    (fun (major, arg, hex) ->
      let b = Buffer.create 9 in
      D.write_head b major arg;
      Alcotest.(check string) hex hex (to_hex (Buffer.contents b)))
    [
      (0, 23L, "17");
      (0, 24L, "1818");
      (0, 255L, "18ff");
      (0, 256L, "190100");
      (0, 65535L, "19ffff");
      (0, 65536L, "1a00010000");
      (0, 0xFFFF_FFFFL, "1affffffff");
      (0, 0x1_0000_0000L, "1b0000000100000000");
      (1, -1L, "3bffffffffffffffff");
      (7, 22L, "f6");
      (7, 32L, "f820");
    ];
  List.iter
    (fun (major, arg) ->
      match D.write_head (Buffer.create 9) major arg with
      | () -> Alcotest.failf "head %d %Ld written" major arg
      | exception Invalid_argument _ -> ())
    [ (7, 24L); (7, 31L); (7, 256L); (8, 0L); (-1, 0L) ]

(* Bytes outside [off] to [limit] are never read. *)
let test_limit () =
  let buf = Bytes.of_string "\x82\x01\x02\x03" in
  Alcotest.check result "the limit cuts the array"
    (Error { at = 2; reason = Eof 1 })
    (D.skip buf ~off:0 ~limit:2);
  Alcotest.check result "from an offset" (Ok 4) (D.skip buf ~off:3 ~limit:4);
  match D.skip buf ~off:0 ~limit:5 with
  | _ -> Alcotest.fail "a limit past the buffer"
  | exception Invalid_argument _ -> ()

(* Diagnostic notation, as Appendix A writes these items. *)
let test_pp () =
  List.iter
    (fun (hex, diag) ->
      match D.of_string (of_hex hex) with
      | Ok v -> Alcotest.(check string) hex diag (Fmt.str "%a" D.pp v)
      | Error e -> Alcotest.failf "%s: %a" hex D.pp_error e)
    [
      ("3bffffffffffffffff", "-18446744073709551616");
      ("3903e7", "-1000");
      ("1bffffffffffffffff", "18446744073709551615");
      ("d74401020304", "23(h'01020304')");
      ("f8ff", "simple(255)");
      ("f7", "undefined");
      ("62225c", {|"\"\\"|});
      ("8301820203820405", "[1, [2, 3], [4, 5]]");
      ("a201020304", "{1: 2, 3: 4}");
    ]

let suite =
  ( "cbor_det",
    [
      Alcotest.test_case "RFC 8949 Appendix A" `Quick test_appendix_a;
      Alcotest.test_case "EverCBOR vectors" `Quick test_evercbor;
      Alcotest.test_case "truncated at every offset" `Quick test_truncated;
      Alcotest.test_case "deep nesting" `Quick test_deep;
      Alcotest.test_case "map keys" `Quick test_maps;
      Alcotest.test_case "heads, simple values, UTF-8" `Quick test_heads;
      Alcotest.test_case "limit" `Quick test_limit;
      Alcotest.test_case "diagnostic notation" `Quick test_pp;
    ] )
