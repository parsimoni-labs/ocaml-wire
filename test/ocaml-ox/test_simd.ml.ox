(* [X4] against one record at a time: on random buffers and offsets, a read of
   four records holds in each lane what the accessors read of that record, or
   fails exactly when 16 bytes do not fit; a write of four records leaves the
   same bytes, and raises the same error, as four [write]. *)

module G = G_lanes
module V = Wire_runtime.Vec
module Buf = Wire_runtime.Buf

let rand = Random.State.make [| 42 |]
let widths = [| 4; 4; 8; 3; 5; 6; 2 |]

(* The fields of the record at [off] in [b], through its view: a record of
   [G] has no check, so one that fits passes. *)
let per_record b off =
  let v = G.get (G.check b ~off) in
  [
    G.a b v;
    G.b b v;
    Wire_runtime.UInt8.to_int (G.c b v);
    G.d b v;
    G.e b v;
    G.f b v;
    G.g b v;
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
      G.X4.with_ b ~off
        ~ok:(fun w -> Some (List.init 4 (lanes w)))
        ~fail:(fun () -> None)
    in
    let want =
      if off >= 0 && off + 16 <= len then
        Some (List.init 4 (fun k -> per_record b (off + (4 * k))))
      else None
    in
    Alcotest.(check (option (list (list int))))
      (Fmt.str "len %d off %d" len off)
      want got
  done

let outcome f =
  match f () with
  | () -> "ok"
  | exception Invalid_argument m -> "invalid " ^ m
  | exception exn when Wire_runtime.Errors.is_error exn ->
      Fmt.str "parse error %a" Wire_runtime.Error.pp
        (Wire_runtime.Errors.error_of exn)

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
    let got =
      outcome (fun () ->
          G.X4.write_int_at ba ~off (vec 0) (vec 1) (vec 2)
            (vec 3) (vec 4) (vec 5) (vec 6))
    in
    let want =
      outcome (fun () ->
          for k = 0 to 3 do
            let f i = fields.(i).(k) in
            let c = Wire_runtime.UInt8.v (f 2) in
            G.write s
              ~off:(off + (4 * k))
              ~a:(f 0) ~b:(f 1) ~c ~d:(f 3) ~e:(f 4) ~f:(f 5) ~g:(f 6)
          done)
    in
    let name = Fmt.str "len %d off %d" len off in
    Alcotest.(check string) name want got;
    Alcotest.(check bytes) name s a
  done

(* A part from any lanes is four valid records: [store] of the fields' low
   bits writes what [write] writes of the masked values, and raises
   the same error where the four do not fit. [<field>_of] answers the same
   part when the lanes fit, and raises what [write] raises otherwise. *)
let test_parts () =
  let parts = G.X4.[| a_bits; b_bits; c_bits; d_bits; e_bits; f_bits; g_bits |]
  and checked = G.X4.[| a_of; b_of; c_of; d_of; e_of; f_of; g_of |] in
  for _ = 1 to 2000 do
    let len = Random.State.int rand 40 in
    let off = Random.State.int rand (len + 5) - 2 in
    let lanes =
      Array.init 7 (fun _ -> Array.init 4 (fun _ -> Random.State.int rand 1024))
    in
    let vec i = V.v4 lanes.(i).(0) lanes.(i).(1) lanes.(i).(2) lanes.(i).(3) in
    let part =
      Array.fold_left G.X4.( ++ ) G.X4.zero
        (Array.mapi (fun i f -> f (vec i)) parts)
    in
    let a = Bytes.make len '\000' and s = Bytes.make len '\000' in
    let (Buf.Any ba) = Buf.v a in
    let got = outcome (fun () -> G.X4.store ba ~off part) in
    let want =
      outcome (fun () ->
          for k = 0 to 3 do
            let f i = lanes.(i).(k) land ((1 lsl widths.(i)) - 1) in
            let c = Wire_runtime.UInt8.v (f 2) in
            G.write s
              ~off:(off + (4 * k))
              ~a:(f 0) ~b:(f 1) ~c ~d:(f 3) ~e:(f 4) ~f:(f 5) ~g:(f 6)
          done)
    in
    let name = Fmt.str "len %d off %d" len off in
    Alcotest.(check string) name want got;
    Alcotest.(check bytes) name s a;
    Array.iteri
      (fun i of_ ->
        let fits = Array.for_all (fun x -> x lsr widths.(i) = 0) lanes.(i) in
        let got = outcome (fun () -> ignore (of_ (vec i))) in
        let want =
          if fits then "ok"
          else
            outcome (fun () ->
                let bad =
                  Array.find_opt (fun x -> x lsr widths.(i) <> 0) lanes.(i)
                in
                Wire_runtime.Errors.unsigned_too_wide ~bits:widths.(i)
                  (Option.get bad))
        in
        Alcotest.(check string) (Fmt.str "%s field %d" name i) want got)
      checked
  done

(* [fill] checks the span of all its registers once, and writes and raises
   what one [store] per register does. *)
let test_fill () =
  for _ = 1 to 2000 do
    let len = Random.State.int rand 80 in
    let off = Random.State.int rand (len + 5) - 2 in
    let count = Random.State.int rand 6 in
    let parts =
      Array.init count (fun _ ->
          G.X4.c_bits
            (V.v4
               (Random.State.int rand 256)
               (Random.State.int rand 256)
               (Random.State.int rand 256)
               (Random.State.int rand 256)))
    in
    let a = Bytes.make len '\000' and s = Bytes.make len '\000' in
    let (Buf.Any ba) = Buf.v a in
    let (Buf.Any bs) = Buf.v s in
    let got =
      outcome (fun () ->
          G.X4.fill ba ~off ~count ~f:(fun k -> parts.(k)))
    in
    let want =
      outcome (fun () ->
          for k = 0 to count - 1 do
            G.X4.store bs ~off:(off + (16 * k)) parts.(k)
          done)
    in
    let name = Fmt.str "len %d off %d count %d" len off count in
    Alcotest.(check string) name want got;
    Alcotest.(check bytes) name s a
  done

let suite =
  ( "simd",
    [
      Alcotest.test_case "X4 reads what get reads" `Quick test_read;
      Alcotest.test_case "X4 writes what write writes" `Quick test_write;
      Alcotest.test_case "X4 parts are four valid records" `Quick test_parts;
      Alcotest.test_case "X4 fill checks the span once" `Quick test_fill;
    ] )
