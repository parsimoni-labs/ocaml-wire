(* A generated record module, through its [Global] view, and the interpreted
   codec are both a [Wire.S], on random buffers, on OCaml as on OxCaml. *)

open Wire
module Buf = Wire_runtime.Buf

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Parse_error e -> Fmt.str "parse error %a" pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let rand = Random.State.make [| 11 |]
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

(* The generated record module and the interpreted codec are both a
   [Wire.S]: code written against it runs on either by changing one line. *)
module Interpreted = Codec.Make (struct
  type r = Schemas.ipv4

  let codec = Schemas.ipv4
end)

module _ : Wire.S = G_ipv4_record.Global
module _ : Wire.S = Interpreted

(* The same checks through two implementations of [Wire.S]: on random buffers
   and offsets, each answers what the other does, a record decoded by one
   encodes to the bytes the other's encodes to, and each field reads alike, by
   [Field.get] and through a view. *)
module Same (A : Wire.S) (B : Wire.S) = struct
  (* A field of [A], the same field of [B], and how to show its value. *)
  type pair = Pair : 'v A.Field.t * 'v B.Field.t * ('v -> string) -> pair

  let bytes_of encode decode buf off =
    let r = decode buf off in
    let out = Bytes.make A.min_size '\000' in
    encode r out 0;
    Bytes.to_string out

  let view_a fa show buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = A.View.check b (Buf.length b) off in
    if A.View.ok c then Some (show (fa.A.Field.view b (A.View.get c))) else None

  let view_b fb show buf off =
    let (Buf.Any b) = Buf.v buf in
    let c = B.View.check b (Buf.length b) off in
    if B.View.ok c then Some (show (fb.B.Field.view b (B.View.get c))) else None

  let run pairs =
    Alcotest.(check int) "min_size" A.min_size B.min_size;
    for _ = 1 to 3000 do
      let buf, off = input ~size:A.min_size ~fix:(fun _ _ -> ()) in
      let same name a b = Alcotest.(check string) name (a ()) (b ()) in
      same "is_valid"
        (fun () -> string_of_bool (A.is_valid buf off))
        (fun () -> string_of_bool (B.is_valid buf off));
      same "validate"
        (fun () ->
          outcome (fun () ->
              A.validate buf off;
              ""))
        (fun () ->
          outcome (fun () ->
              B.validate buf off;
              ""));
      let result = function
        | Ok _ -> "ok"
        | Error e -> Fmt.str "%a" pp_parse_error e
      in
      same "decode"
        (fun () -> outcome (fun () -> result (A.decode buf off)))
        (fun () -> outcome (fun () -> result (B.decode buf off)));
      same "decode then encode"
        (fun () -> outcome (fun () -> bytes_of A.encode A.decode_exn buf off))
        (fun () -> outcome (fun () -> bytes_of B.encode B.decode_exn buf off));
      List.iter
        (fun (Pair (fa, fb, show)) ->
          same "Field.get"
            (fun () -> outcome (fun () -> show (fa.A.Field.get buf off)))
            (fun () -> outcome (fun () -> show (fb.B.Field.get buf off)));
          Alcotest.(check (option string))
            "Field.view" (view_a fa show buf off) (view_b fb show buf off))
        pairs
    done
end

let test_same () =
  let module T = Same (G_ipv4_record.Global) (Interpreted) in
  let u16 v = string_of_int (UInt16.to_int v)
  and u8 v = string_of_int (UInt8.to_int v)
  and i32 v = string_of_int (SInt32.to_int v)
  and u32 v = string_of_int (UInt32.to_int v) in
  T.run
    [
      T.Pair
        ( G_ipv4_record.Global.Field.frag,
          Interpreted.field Schemas.ipv4_frag,
          u16 );
      T.Pair
        ( G_ipv4_record.Global.Field.proto,
          Interpreted.field Schemas.ipv4_proto,
          u8 );
      T.Pair
        (G_ipv4_record.Global.Field.src, Interpreted.field Schemas.ipv4_src, i32);
      T.Pair
        (G_ipv4_record.Global.Field.dst, Interpreted.field Schemas.ipv4_dst, u32);
    ]

let suite =
  ( "same",
    [
      Alcotest.test_case "generated and interpreted, one signature" `Quick
        test_same;
    ] )
