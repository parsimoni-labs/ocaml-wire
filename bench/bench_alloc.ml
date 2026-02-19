(** Micro-benchmark to identify Codec decode allocation sources. *)

open D3t

type r3 = { a : int; b : int; c : int }

let codec3 =
  let open Codec in
  record "R3" (fun a b c -> { a; b; c })
  |+ field "a" uint16be (fun t -> t.a)
  |+ field "b" uint16be (fun t -> t.b)
  |+ field "c" uint16be (fun t -> t.c)
  |> seal

type r1 = { x : int }

let codec1 =
  let open Codec in
  record "R1" (fun x -> { x }) |+ field "x" uint16be (fun t -> t.x) |> seal

type r7 = {
  f1 : int;
  f2 : int;
  f3 : int;
  f4 : int;
  f5 : int;
  f6 : int;
  f7 : int;
}

let codec7 =
  let open Codec in
  record "R7" (fun f1 f2 f3 f4 f5 f6 f7 -> { f1; f2; f3; f4; f5; f6; f7 })
  |+ field "f1" uint16be (fun t -> t.f1)
  |+ field "f2" uint16be (fun t -> t.f2)
  |+ field "f3" uint16be (fun t -> t.f3)
  |+ field "f4" uint16be (fun t -> t.f4)
  |+ field "f5" uint16be (fun t -> t.f5)
  |+ field "f6" uint16be (fun t -> t.f6)
  |+ field "f7" uint16be (fun t -> t.f7)
  |> seal

type r8 = {
  g1 : int;
  g2 : int;
  g3 : int;
  g4 : int;
  g5 : int;
  g6 : int;
  g7 : int;
  g8 : int;
}

let codec8 =
  let open Codec in
  record "R8" (fun g1 g2 g3 g4 g5 g6 g7 g8 ->
      { g1; g2; g3; g4; g5; g6; g7; g8 })
  |+ field "g1" uint16be (fun t -> t.g1)
  |+ field "g2" uint16be (fun t -> t.g2)
  |+ field "g3" uint16be (fun t -> t.g3)
  |+ field "g4" uint16be (fun t -> t.g4)
  |+ field "g5" uint16be (fun t -> t.g5)
  |+ field "g6" uint16be (fun t -> t.g6)
  |+ field "g7" uint16be (fun t -> t.g7)
  |+ field "g8" uint16be (fun t -> t.g8)
  |> seal

let buf6 = Bytes.create 6
let buf2 = Bytes.create 2
let buf14 = Bytes.create 14
let buf16 = Bytes.create 16

let measure name n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  let per_call = (after -. before) /. float_of_int n in
  Printf.printf "  %-30s %6.1f words/call\n" name per_call

let () =
  let n = 1_000_000 in
  Printf.printf "Codec decode allocation breakdown (%d calls)\n\n" n;

  Printf.printf "Measurement baseline:\n";
  measure "noop" n (fun () -> ());
  measure "Bytes.length" n (fun () ->
      let _ = Bytes.length buf6 in
      ());

  Printf.printf "\n1-field record (no intermediate closures):\n";
  measure "Codec.decode codec1" n (fun () ->
      let _ = Codec.decode codec1 buf2 0 in
      ());

  Printf.printf "\n3-field record (2 intermediate closures):\n";
  measure "Codec.decode codec3" n (fun () ->
      let _ = Codec.decode codec3 buf6 0 in
      ());

  Printf.printf "\nBaseline (hand-written, same record):\n";
  measure "hand-written decode" n (fun () ->
      let a = Bytes.get_uint16_be buf6 0 in
      let b = Bytes.get_uint16_be buf6 2 in
      let c = Bytes.get_uint16_be buf6 4 in
      let _ = { a; b; c } in
      ());

  Printf.printf "\n7-field record (chunked fallback, 1 PA):\n";
  measure "Codec.decode codec7" n (fun () ->
      let _ = Codec.decode codec7 buf14 0 in
      ());
  measure "hand-written 7-field" n (fun () ->
      let f1 = Bytes.get_uint16_be buf14 0 in
      let f2 = Bytes.get_uint16_be buf14 2 in
      let f3 = Bytes.get_uint16_be buf14 4 in
      let f4 = Bytes.get_uint16_be buf14 6 in
      let f5 = Bytes.get_uint16_be buf14 8 in
      let f6 = Bytes.get_uint16_be buf14 10 in
      let f7 = Bytes.get_uint16_be buf14 12 in
      let _ = { f1; f2; f3; f4; f5; f6; f7 } in
      ());

  Printf.printf "\n8-field record (chunked fallback, 1 PA):\n";
  measure "Codec.decode codec8" n (fun () ->
      let _ = Codec.decode codec8 buf16 0 in
      ());
  measure "hand-written 8-field" n (fun () ->
      let g1 = Bytes.get_uint16_be buf16 0 in
      let g2 = Bytes.get_uint16_be buf16 2 in
      let g3 = Bytes.get_uint16_be buf16 4 in
      let g4 = Bytes.get_uint16_be buf16 6 in
      let g5 = Bytes.get_uint16_be buf16 8 in
      let g6 = Bytes.get_uint16_be buf16 10 in
      let g7 = Bytes.get_uint16_be buf16 12 in
      let g8 = Bytes.get_uint16_be buf16 14 in
      let _ = { g1; g2; g3; g4; g5; g6; g7; g8 } in
      ());

  Printf.printf "\nEncode:\n";
  let v = { a = 1; b = 2; c = 3 } in
  measure "Codec.encode codec3" n (fun () -> Codec.encode codec3 v buf6 0);
  measure "hand-written encode" n (fun () ->
      Bytes.set_uint16_be buf6 0 v.a;
      Bytes.set_uint16_be buf6 2 v.b;
      Bytes.set_uint16_be buf6 4 v.c);
  measure "Codec.encode codec3 + create" n (fun () ->
      let b = Bytes.create 6 in
      Codec.encode codec3 v b 0;
      let _ = b in
      ());

  Printf.printf "\nInt32 boxing (CLCW-like):\n";
  let buf4 = Bytes.create 4 in
  measure "Bytes.get_int32_be" n (fun () ->
      let _ = Bytes.get_int32_be buf4 0 in
      ());
  measure "byte-by-byte Int32" n (fun () ->
      let b0 = Bytes.get_uint8 buf4 0 in
      let b1 = Bytes.get_uint8 buf4 1 in
      let b2 = Bytes.get_uint8 buf4 2 in
      let b3 = Bytes.get_uint8 buf4 3 in
      let _ =
        Int32.of_int ((b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3)
      in
      ())
