(* Tests for Unchecked: each read gives what the checked [Bytes] read gives at
   every offset of a buffer whose bytes have the top bit set, and each write
   leaves the bytes the checked [Bytes] write leaves. *)

open Wire.Private

let buf = Bytes.of_string "\x01\x82\xf3\x04\xa5\x96"

let check name ~unchecked ~checked width =
  for off = 0 to Bytes.length buf - width do
    Alcotest.(check bool)
      (Fmt.str "%s at %d" name off)
      true
      (unchecked buf off = checked buf off)
  done

let test_u8 () = check "u8" ~unchecked:Unchecked.u8 ~checked:Bytes.get_uint8 1

let test_u16 () =
  check "u16le" ~unchecked:Unchecked.u16le ~checked:Bytes.get_uint16_le 2;
  check "u16be" ~unchecked:Unchecked.u16be ~checked:Bytes.get_uint16_be 2

let test_i32 () =
  check "i32le" ~unchecked:Unchecked.i32le ~checked:Bytes.get_int32_le 4;
  check "i32be" ~unchecked:Unchecked.i32be ~checked:Bytes.get_int32_be 4

let check_set name ~unchecked ~checked width v =
  for off = 0 to Bytes.length buf - width do
    let a = Bytes.copy buf and b = Bytes.copy buf in
    unchecked a off v;
    checked b off v;
    Alcotest.(check string)
      (Fmt.str "%s at %d" name off)
      (Bytes.to_string b) (Bytes.to_string a)
  done

let test_set () =
  check_set "set_u8" ~unchecked:Unchecked.set_u8 ~checked:Bytes.set_uint8 1 0xa7;
  check_set "set_u16le" ~unchecked:Unchecked.set_u16le
    ~checked:Bytes.set_uint16_le 2 0xbeef;
  check_set "set_u16be" ~unchecked:Unchecked.set_u16be
    ~checked:Bytes.set_uint16_be 2 0xbeef;
  check_set "set_i32le" ~unchecked:Unchecked.set_i32le
    ~checked:Bytes.set_int32_le 4 (-559038737l);
  check_set "set_i32be" ~unchecked:Unchecked.set_i32be
    ~checked:Bytes.set_int32_be 4 (-559038737l)

(* Under the [paranoid] profile every read here is checked, the bitfield word
   helpers included: one that ends past the buffer raises. Any other profile
   has nothing to run, since such a read is not defined there. Each read
   starts inside the buffer and stops in the padding of its last word. *)
let test_paranoid_traps () =
  if not Unchecked.checked then Alcotest.skip ();
  let n = Bytes.length buf in
  let traps name f =
    match f () with
    | () -> Alcotest.failf "%s: read past the end of the buffer" name
    | exception Invalid_argument _ -> ()
  in
  let read name f off = traps name (fun () -> ignore (f buf off : int)) in
  read "u8" Unchecked.u8 n;
  read "u16le" Unchecked.u16le (n - 1);
  read "u16be" Unchecked.u16be (n - 1);
  traps "i32le" (fun () -> ignore (Unchecked.i32le buf (n - 3) : int32));
  traps "i32be" (fun () -> ignore (Unchecked.i32be buf (n - 3) : int32));
  traps "blit_string" (fun () -> Unchecked.blit_string "" buf (n + 1));
  read "Bitfield.u32_field_le"
    (fun buf off -> Bitfield.u32_field_le buf off 0 0xff)
    (n - 3);
  read "Bitfield.u32_field_be"
    (fun buf off -> Bitfield.u32_field_be buf off 0 0xff)
    (n - 3);
  List.iter
    (fun (name, base, width) ->
      read
        ("Bitfield.read_word " ^ name)
        (Bitfield.read_word base)
        (n - width + 1))
    Wire.Private.Types.
      [
        ("u16", U16 Little, 2);
        ("u16be", U16 Big, 2);
        ("u32", U32 Little, 4);
        ("u32be", U32 Big, 4);
      ]

let suite =
  ( "unchecked",
    [
      Alcotest.test_case "u8" `Quick test_u8;
      Alcotest.test_case "u16" `Quick test_u16;
      Alcotest.test_case "i32" `Quick test_i32;
      Alcotest.test_case "writes" `Quick test_set;
      Alcotest.test_case "paranoid: a read past the end raises" `Quick
        test_paranoid_traps;
    ] )
