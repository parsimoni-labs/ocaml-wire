(* The getters of a generated module with no views, on bytes that make no
   value of their field, against [Codec.get]: the module of a record with
   fields under maps. *)

open Wire

let rand = Random.State.make [| 7 |]

(* A buffer around a record of [size] bytes and an offset that is mostly in
   range. *)
let input ~size =
  let len = Random.State.int rand (size + 6) in
  let buf = Bytes.init len (fun _ -> Char.chr (Random.State.int rand 256)) in
  let off =
    match Random.State.int rand 8 with
    | 1 -> len - size + 1 + Random.State.int rand 2
    | _ -> if len >= size then Random.State.int rand (len - size + 1) else 0
  in
  (buf, off)

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Invalid_argument m -> "invalid " ^ m

let error = Alcotest.testable Error.pp Error.equal

(* A generated getter answers the error [Codec.get] answers where the bytes
   make no value of its field: a lookup index past its table, a field the
   buffer cuts. *)
let test_getter_errors () =
  let proto = Alcotest.testable (fun ppf _ -> Fmt.string ppf "a protocol") ( = )
  and len = Alcotest.int in
  let answers name value get field buf off ~expected =
    Alcotest.(check (result value error)) name (Error expected) (get buf off);
    Alcotest.(check (result value error))
      (name ^ ", as Codec.get") (Error expected)
      (Codec.get Schemas.mapped field buf off)
  in
  answers "a lookup index past its table" proto G_mapped.Get.proto
    Schemas.mapped_proto
    (Bytes.of_string "\x00\x01\x00\x00\x00\x00\x07\x00")
    0
    ~expected:{ at = 6; field = [ "Proto" ]; kind = Invalid_tag 7 };
  answers "a field the buffer cuts" len G_mapped.Get.len Schemas.mapped_len
    (Bytes.of_string "\x00") 0
    ~expected:
      {
        at = 0;
        field = [ "Len" ];
        kind = Unexpected_eof { expected = 2; got = 1 };
      }

(* A field taken out of its word reads what its own getter reads, and what
   [Codec.get] reads. *)
let test_words () =
  let size = Codec.min_size Schemas.mapped in
  let show = function
    | Ok v -> string_of_int v
    | Error e -> Fmt.str "%a" Error.pp e
  in
  let codec_rest = Codec.get Schemas.mapped Schemas.mapped_rest in
  for _ = 1 to 2000 do
    let buf, off = input ~size in
    let open G_mapped in
    let rest = outcome (fun () -> show (Get.rest buf off)) in
    Alcotest.(check string)
      "rest from its word" rest
      (outcome (fun () ->
           show (Result.map rest_of_word (Get.rest_word buf off))));
    Alcotest.(check string)
      "rest as Codec.get" rest
      (outcome (fun () -> show (codec_rest buf off)));
    Alcotest.(check string)
      "flag from its word"
      (outcome (fun () -> show (Result.map Bool.to_int (Get.flag buf off))))
      (outcome (fun () ->
           show (Result.map flag_of_word (Get.flag_word buf off))))
  done

(* A word load takes its record's offset from the caller: a buffer that cuts
   the word is input, answered as the field's getter answers it, and a negative
   offset is misuse, refused even where it puts the word inside the buffer. *)
let test_word_bad_input () =
  let short = Bytes.make 7 '\x00' in
  Alcotest.(check (result int error))
    "a word the buffer cuts"
    (G_mapped.Get.rest short 0)
    (G_mapped.Get.rest_word short 0);
  if Result.is_ok (G_mapped.Get.rest_word short 0) then
    Alcotest.fail "a word the buffer cuts was loaded";
  match G_mapped.Get.rest_word (Bytes.make 12 '\x00') (-4) with
  | _ ->
      Alcotest.fail "a negative offset loaded the word in front of the record"
  | exception Invalid_argument _ -> ()

let suite =
  ( "get",
    [
      Alcotest.test_case "getter errors" `Quick test_getter_errors;
      Alcotest.test_case "fields from their word" `Quick test_words;
      Alcotest.test_case "word loads on bad input" `Quick test_word_bad_input;
    ] )
