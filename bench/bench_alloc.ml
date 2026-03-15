(** Wire Codec benchmark: timing and allocation for decode, encode, zero-copy.

    Usage: dune exec bench/bench_alloc.exe [-- ITERATIONS] Default: 10_000_000
    iterations. *)

open Wire
open Bench_schemas

(* ── Timing ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float_of_int n

(* ── Allocation ── *)

let alloc_words n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

(* ── Codecs for micro-benchmarks ── *)

type r3 = { a : int; b : int; c : int }

let f_a = Codec.field "a" uint16be (fun t -> t.a)
let f_b = Codec.field "b" uint16be (fun t -> t.b)
let f_c = Codec.field "c" uint16be (fun t -> t.c)

let codec3 =
  Codec.(record "R3" (fun a b c -> { a; b; c }) |+ f_a |+ f_b |+ f_c |> seal)

type r1 = { x : int }

let f_x = Codec.field "x" uint16be (fun t -> t.x)
let codec1 = Codec.(record "R1" (fun x -> { x }) |+ f_x |> seal)

(* ── Table formatting ── *)

let table_header title cols =
  Fmt.pr "\n%s\n%s\n\n" title (String.make (String.length title) '=');
  let widths =
    List.map (fun (name, w) -> (name, max w (String.length name))) cols
  in
  List.iter (fun (name, w) -> Fmt.pr "  %-*s" w name) widths;
  Fmt.pr "\n";
  List.iter (fun (_, w) -> Fmt.pr "  %s" (String.make w '-')) widths;
  Fmt.pr "\n";
  widths

let table_row widths cells =
  List.iter2 (fun (_, w) cell -> Fmt.pr "  %-*s" w cell) widths cells;
  Fmt.pr "\n"

(* ── Main ── *)

let () =
  let n =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 10_000_000
  in

  (* ── Part 1: Timing benchmark ── *)
  let widths =
    table_header "Wire Codec Timing"
      [ ("Operation", 30); ("ns/op", 8); ("alloc", 12); ("vs hand", 8) ]
  in

  (* CLCW: 13 bitfields in 4 bytes *)
  let clcw_buf = (clcw_data 1).(0) in

  let clcw_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode clcw_codec clcw_buf 0)
        done)
  in
  let clcw_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode clcw_codec clcw_buf 0))
  in
  table_row widths
    [
      "CLCW decode (13 bf)";
      Fmt.str "%.1f" clcw_decode_ns;
      Fmt.str "%.0fw" clcw_decode_alloc;
      "-";
    ];

  let clcw_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get clcw_codec cw_report clcw_buf 0)
        done)
  in
  let clcw_get_alloc =
    alloc_words n (fun () -> ignore (Codec.get clcw_codec cw_report clcw_buf 0))
  in
  table_row widths
    [
      "CLCW zero-copy get";
      Fmt.str "%.1f" clcw_get_ns;
      Fmt.str "%.0fw" clcw_get_alloc;
      Fmt.str "%.0fx" (clcw_decode_ns /. clcw_get_ns);
    ];

  let hand_clcw_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          let b0 = Bytes.get_uint8 clcw_buf 0 in
          let b1 = Bytes.get_uint8 clcw_buf 1 in
          let b2 = Bytes.get_uint8 clcw_buf 2 in
          let b3 = Bytes.get_uint8 clcw_buf 3 in
          let w = (b0 lsl 24) lor (b1 lsl 16) lor (b2 lsl 8) lor b3 in
          ignore (w land 0xFF)
        done)
  in
  table_row widths
    [ "hand-written get"; Fmt.str "%.1f" hand_clcw_ns; "0w"; "1x" ];

  Fmt.pr "\n";

  (* SpacePacket: 7 fields, 3 bf_uint16be + uint16be *)
  let sp_buf = (space_packet_data 1).(0) in
  let sp_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode space_packet_codec sp_buf 0)
        done)
  in
  let sp_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode space_packet_codec sp_buf 0))
  in
  table_row widths
    [
      "SpacePacket decode (7f)";
      Fmt.str "%.1f" sp_decode_ns;
      Fmt.str "%.0fw" sp_decode_alloc;
      "-";
    ];

  (* R3: 3 uint16be fields *)
  let buf6 = Bytes.create 6 in

  let r3_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode codec3 buf6 0)
        done)
  in
  let r3_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get codec3 f_c buf6 0)
        done)
  in
  let r3_decode_alloc =
    alloc_words n (fun () -> ignore (Codec.decode codec3 buf6 0))
  in
  let r3_get_alloc =
    alloc_words n (fun () -> ignore (Codec.get codec3 f_c buf6 0))
  in
  table_row widths
    [
      "R3 decode (3 uint16)";
      Fmt.str "%.1f" r3_decode_ns;
      Fmt.str "%.0fw" r3_decode_alloc;
      "-";
    ];
  table_row widths
    [
      "R3 zero-copy get";
      Fmt.str "%.1f" r3_get_ns;
      Fmt.str "%.0fw" r3_get_alloc;
      Fmt.str "%.0fx" (r3_decode_ns /. r3_get_ns);
    ];

  Fmt.pr "\n";

  (* R1: 1 field *)
  let buf2 = Bytes.create 2 in

  let r1_decode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.decode codec1 buf2 0)
        done)
  in
  let r1_get_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (Codec.get codec1 f_x buf2 0)
        done)
  in
  table_row widths
    [ "R1 decode (1 uint16)"; Fmt.str "%.1f" r1_decode_ns; Fmt.str "0w"; "-" ];
  table_row widths
    [
      "R1 zero-copy get";
      Fmt.str "%.1f" r1_get_ns;
      "0w";
      Fmt.str "%.0fx" (r1_decode_ns /. r1_get_ns);
    ];

  Fmt.pr "\n";

  (* Encode *)
  let clcw_val = clcw_default in
  let clcw_enc_buf = Bytes.create clcw_size in
  let clcw_encode_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          Codec.encode clcw_codec clcw_val clcw_enc_buf 0
        done)
  in
  table_row widths [ "CLCW encode"; Fmt.str "%.1f" clcw_encode_ns; "0w"; "-" ];

  let clcw_set_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          Codec.set clcw_codec cw_report clcw_buf 0 42
        done)
  in
  table_row widths
    [
      "CLCW zero-copy set";
      Fmt.str "%.1f" clcw_set_ns;
      "0w";
      Fmt.str "%.0fx" (clcw_encode_ns /. clcw_set_ns);
    ];

  Fmt.pr "\n";

  (* ── Part 2: Allocation breakdown ── *)
  let widths =
    table_header "Allocation Breakdown"
      [ ("Operation", 30); ("words/call", 12) ]
  in

  let alloc_row name f =
    let w = alloc_words n f in
    table_row widths [ name; Fmt.str "%.1f" w ]
  in

  alloc_row "noop" (fun () -> ());
  alloc_row "Codec.decode codec1" (fun () ->
      ignore (Codec.decode codec1 buf2 0));
  alloc_row "Codec.decode codec3" (fun () ->
      ignore (Codec.decode codec3 buf6 0));
  alloc_row "hand-written R3 decode" (fun () ->
      let a = Bytes.get_uint16_be buf6 0 in
      let b = Bytes.get_uint16_be buf6 2 in
      let c = Bytes.get_uint16_be buf6 4 in
      ignore { a; b; c });
  alloc_row "Codec.decode clcw" (fun () ->
      ignore (Codec.decode clcw_codec clcw_buf 0));
  alloc_row "Codec.get (zero-copy)" (fun () ->
      ignore (Codec.get clcw_codec cw_report clcw_buf 0));
  alloc_row "Codec.set (zero-copy)" (fun () ->
      Codec.set clcw_codec cw_report clcw_buf 0 42);

  Fmt.pr "\n";

  let widths =
    table_header "Int boxing" [ ("Operation", 30); ("words/call", 12) ]
  in
  let alloc_row name f =
    let w = alloc_words n f in
    table_row widths [ name; Fmt.str "%.1f" w ]
  in
  let buf4 = Bytes.create 4 in
  alloc_row "Bytes.get_int32_be (boxed)" (fun () ->
      ignore (Bytes.get_int32_be buf4 0));
  alloc_row "Wire.UInt32.get_be (unboxed)" (fun () ->
      ignore (Wire.UInt32.get_be buf4 0));
  let buf8 = Bytes.create 8 in
  alloc_row "Bytes.get_int64_be (boxed)" (fun () ->
      ignore (Bytes.get_int64_be buf8 0));
  alloc_row "Wire.UInt63.get_be (unboxed)" (fun () ->
      ignore (Wire.UInt63.get_be buf8 0));

  Fmt.pr "\n"
