(* [X4] against one record at a time: on random buffers and offsets, a read of
   four records holds in each lane what [Get.<field>] reads of that record, or
   fails exactly when 16 bytes do not fit; a write of four records leaves the
   same bytes, and raises the same error, as four [write_int_at]. *)

module G = G_lanes
module V = Wire_runtime.Vec
module Buf = Wire_runtime.Buf

let rand = Random.State.make [| 42 |]
let widths = [| 4; 4; 8; 3; 5; 6; 2 |]

let per_record buf off =
  let open Wire_runtime in
  [
    (G.Get.a buf off :> int);
    (G.Get.b buf off :> int);
    UInt8.to_int (G.Get.c buf off);
    (G.Get.d buf off :> int);
    (G.Get.e buf off :> int);
    (G.Get.f buf off :> int);
    (G.Get.g buf off :> int);
  ]

let lanes w k =
  [
    V.get (G.X4.a w) k;
    V.get (G.X4.b w) k;
    V.get (G.X4.c w) k;
    V.get (G.X4.d w) k;
    V.get (G.X4.e w) k;
    V.get (G.X4.f w) k;
    V.get (G.X4.g w) k;
  ]

let test_read () =
  for _ = 1 to 2000 do
    let len = Random.State.int rand 40 in
    let buf = Bytes.init len (fun _ -> Char.chr (Random.State.int rand 256)) in
    let off = Random.State.int rand (len + 5) - 2 in
    let (Buf.Any b) = Buf.v buf in
    let got =
      G.X4.with_ b (Buf.length b) off
        ~ok:(fun w -> Some (List.init 4 (lanes w)))
        ~fail:(fun () -> None)
    in
    let want =
      if off >= 0 && off + 16 <= len then
        Some (List.init 4 (fun k -> per_record buf (off + (4 * k))))
      else None
    in
    Alcotest.(check (option (list (list int))))
      (Fmt.str "len %d off %d" len off)
      want got
  done

let outcome f =
  match f () with
  | () -> "ok"
  | exception Wire_runtime.Errors.Parse_error e ->
      Fmt.str "parse error %a" Wire_runtime.Errors.pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let test_write () =
  for _ = 1 to 2000 do
    let len = Random.State.int rand 40 in
    let off = Random.State.int rand (len + 5) - 2 in
    (* Mostly in range, sometimes one past a field's width. *)
    let value i =
      let w = widths.(i) in
      if Random.State.int rand 50 = 0 then 1 lsl w
      else Random.State.int rand (1 lsl w)
    in
    let fields = Array.init 7 (fun i -> Array.init 4 (fun _ -> value i)) in
    let vec i =
      let f = fields.(i) in
      V.v4 f.(0) f.(1) f.(2) f.(3)
    in
    let a = Bytes.make len '\000' and s = Bytes.make len '\000' in
    let (Buf.Any ba) = Buf.v a in
    let (Buf.Any bs) = Buf.v s in
    let got =
      outcome (fun () ->
          G.X4.write_int_at ba (Buf.length ba) off (vec 0) (vec 1) (vec 2)
            (vec 3) (vec 4) (vec 5) (vec 6))
    in
    let want =
      outcome (fun () ->
          for k = 0 to 3 do
            let f i = fields.(i).(k) in
            G.write_int_at bs (Buf.length bs) (off + (4 * k)) (f 0) (f 1) (f 2)
              (f 3) (f 4) (f 5) (f 6)
          done)
    in
    let name = Fmt.str "len %d off %d" len off in
    Alcotest.(check string) name want got;
    Alcotest.(check bytes) name s a
  done

let suite =
  ( "simd",
    [
      Alcotest.test_case "X4 reads what get reads" `Quick test_read;
      Alcotest.test_case "X4 writes what write_int_at writes" `Quick test_write;
    ] )
