(* The getters of a generated module on bytes that make no value of their
   field, against [Codec.get]. *)

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

(* A generated getter answers the error [Codec.get] answers where the bytes
   make no value of its field: a closed enum outside its cases, a field the
   buffer cuts. *)
let test_getter_errors () =
  let error = Alcotest.testable Error.pp Error.equal in
  let answers name get codec field buf off ~expected =
    Alcotest.(check (result reject error)) name (Error expected) (get buf off);
    Alcotest.(check (result reject error))
      (name ^ ", as Codec.get") (Error expected)
      (Staged.unstage (Codec.get codec field) buf off)
  in
  answers "a closed enum outside its cases" G_checked.Get.proto Schemas.checked
    Schemas.checked_proto
    (Bytes.of_string "\x45\x00\x14\x40\x07")
    0
    ~expected:
      {
        at = 4;
        field = [ "Proto" ];
        kind = Invalid_enum { value = 7; valid = [ 1; 6; 17 ] };
      };
  answers "a field the buffer cuts" G_ipv4.Get.frag Schemas.ipv4
    Schemas.ipv4_frag
    (Bytes.of_string "\x00\x45\x00\x00\x14\x00\x00\x40")
    1
    ~expected:
      {
        at = 7;
        field = [ "Frag" ];
        kind = Unexpected_eof { expected = 2; got = 1 };
      }

(* A field taken out of its word reads what its own getter reads, from a
   checked load or through a view. *)
let test_words () =
  let size = Codec.min_wire_size Schemas.wide in
  for _ = 1 to 2000 do
    (* A word load takes the word's position: a negative offset is none. *)
    let buf, off = input ~size in
    let off = max 0 off in
    let fields =
      G_wide.
        [
          ( (fun b o -> Result.map (fun v -> (v : Value.a :> int)) (Get.a b o)),
            Get.a_word,
            a_of_word );
          ( (fun b o -> Result.map (fun v -> (v : Value.c :> int)) (Get.c b o)),
            Get.c_word,
            c_of_word );
          ( (fun b o -> Result.map (fun v -> (v : Value.e :> int)) (Get.e b o)),
            Get.e_word,
            e_of_word );
          ( (fun b o -> Result.map (fun v -> (v : Value.f :> int)) (Get.f b o)),
            Get.f_word,
            f_of_word );
        ]
    in
    List.iter
      (fun (get, word, of_word) ->
        Alcotest.(check string)
          "word"
          (outcome (fun () ->
               match get buf off with
               | Ok v -> string_of_int v
               | Error { Error.kind = Unexpected_eof _; _ } ->
                   invalid_arg "index out of bounds"
               | Error e -> Fmt.str "%a" Error.pp e))
          (outcome (fun () -> string_of_int (of_word (word buf off)))))
      fields;
    (* The same through a view, whose word reads have no check of their own. *)
    match Wire_runtime.Buf.v buf with
    | Wire_runtime.Buf.Any b ->
        let c = G_wide.check b (Wire_runtime.Buf.length b) off in
        if G_wide.ok c then begin
          let v = G_wide.get c in
          let check name get of_view = Alcotest.(check int) name get of_view in
          check "view a"
            (Result.get_ok (G_wide.Get.a buf off) :> int)
            (G_wide.a_of_word (G_wide.view_a_word b v));
          check "view c"
            (Result.get_ok (G_wide.Get.c buf off) :> int)
            (G_wide.c_of_word (G_wide.view_c_word b v));
          check "view e"
            (Result.get_ok (G_wide.Get.e buf off) :> int)
            (G_wide.e_of_word (G_wide.view_e_word b v));
          check "view f"
            (Result.get_ok (G_wide.Get.f buf off) :> int)
            (G_wide.f_of_word (G_wide.view_f_word b v))
        end
  done

let suite =
  ( "get",
    [
      Alcotest.test_case "getter errors" `Quick test_getter_errors;
      Alcotest.test_case "fields from their word" `Quick test_words;
    ] )
