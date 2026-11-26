(* The getters of a generated module with no views, on bytes that make no
   value of their field, against [Codec.get]: the module of a record on a
   bigarray. *)

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

let big = Diff_ocaml.big

(* A generated getter answers the error [Codec.get] answers where the bytes
   make no value of its field: a closed enum outside its cases, a field the
   buffer cuts. *)
let test_getter_errors () =
  let error = Alcotest.testable Error.pp Error.equal in
  let answers name get codec field buf off ~expected =
    Alcotest.(check (result reject error)) name (Error expected) (get buf off);
    Alcotest.(check (result reject error))
      (name ^ ", as Codec.get") (Error expected)
      (Codec.get codec field buf off)
  in
  answers "a closed enum outside its cases"
    (fun b o -> G_checked_big.Get.proto (big b) o)
    Schemas.checked Schemas.checked_proto
    (Bytes.of_string "\x45\x00\x14\x40\x07")
    0
    ~expected:
      {
        at = 4;
        field = [ "Proto" ];
        kind = Invalid_enum { value = 7; valid = [ 1; 6; 17 ] };
      };
  answers "a field the buffer cuts"
    (fun b o -> G_ipv4_big.Get.frag (big b) o)
    Schemas.ipv4 Schemas.ipv4_frag
    (Bytes.of_string "\x00\x45\x00\x00\x14\x00\x00\x40")
    1
    ~expected:
      {
        at = 7;
        field = [ "Frag" ];
        kind = Unexpected_eof { expected = 2; got = 1 };
      }

(* A field taken out of its word reads what its own getter reads. *)
let test_words () =
  let size = Codec.min_size Schemas.wide in
  for _ = 1 to 2000 do
    let buf, off = input ~size in
    let buf = big buf in
    let fields =
      G_wide_big.
        [
          (Get.a, Get.a_word, a_of_word);
          (Get.c, Get.c_word, c_of_word);
          (Get.e, Get.e_word, e_of_word);
          (Get.f, Get.f_word, f_of_word);
        ]
    in
    let show = function
      | Ok v -> string_of_int v
      | Error e -> Fmt.str "%a" Error.pp e
    in
    List.iter
      (fun (get, word, of_word) ->
        Alcotest.(check string)
          "word"
          (outcome (fun () -> show (get buf off)))
          (outcome (fun () -> show (Result.map of_word (word buf off)))))
      fields
  done

(* A word load takes its record's offset from the caller: a buffer that cuts
   the word is input, answered as the field's getter answers it, and a negative
   offset is misuse, refused even where it puts the word inside the buffer. *)
let test_word_bad_input () =
  let error = Alcotest.testable Error.pp Error.equal in
  let answers name load buf off ~expected =
    Alcotest.(check (result int error)) name (Error expected) (load buf off)
  in
  let f_word b o = G_wide_big.Get.f_word (big b) o in
  answers "a word the buffer cuts" f_word (Bytes.make 10 '\x00') 0
    ~expected:
      {
        at = 8;
        field = [ "F" ];
        kind = Unexpected_eof { expected = 4; got = 2 };
      };
  match f_word (Bytes.make 12 '\x00') (-4) with
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
