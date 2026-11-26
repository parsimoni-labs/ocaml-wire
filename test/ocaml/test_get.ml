(* The getters of a generated module on bytes that make no value of their
   field, against [Codec.get]. *)

open Wire

(* A generated getter answers the error [Codec.get] answers where the bytes
   make no value of its field: a closed enum outside its cases, a field the
   buffer cuts. *)
let test_getter_errors () =
  let answers name get buf off ~expected : unit =
    ignore (get buf off);
    Alcotest.failf "%s: read a value, expected %a" name Error.pp expected
  in
  answers "a closed enum outside its cases" G_checked.Get.proto
    (Bytes.of_string "\x45\x00\x14\x40\x07")
    0
    ~expected:
      {
        at = 4;
        field = [ "Proto" ];
        kind = Invalid_enum { value = 7; valid = [ 1; 6; 17 ] };
      };
  answers "a field the buffer cuts" G_ipv4.Get.frag
    (Bytes.of_string "\x00\x45\x00\x00\x14\x00\x00\x40")
    1
    ~expected:
      {
        at = 7;
        field = [ "Frag" ];
        kind = Unexpected_eof { expected = 2; got = 1 };
      }

let suite =
  ("get", [ Alcotest.test_case "getter errors" `Quick test_getter_errors ])
