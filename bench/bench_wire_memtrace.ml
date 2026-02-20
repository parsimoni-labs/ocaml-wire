(** Memtrace allocation profiling for all 3 wire codecs.

    Run with: MEMTRACE=wire_clcw.ctf dune exec ./bench_wire_memtrace.exe -- clcw
    MEMTRACE=wire_sp.ctf dune exec ./bench_wire_memtrace.exe -- space-packet
    MEMTRACE=wire_tm.ctf dune exec ./bench_wire_memtrace.exe -- tm
    MEMTRACE=wire_all.ctf dune exec ./bench_wire_memtrace.exe -- all *)

let iterations = 10_000

(** {1 CLCW test data} *)

let clcw_bytes =
  Array.init 1000 (fun i ->
      let b = Bytes.create 4 in
      let w =
        ((i mod 4) lsl 29)
        lor ((i mod 8) lsl 26)
        lor ((i mod 4) lsl 24)
        lor ((i mod 64 land 63) lsl 18)
        lor ((i mod 32) lsl 11)
        lor ((i mod 4) lsl 9)
        lor (i mod 256)
      in
      Bytes.set_int32_be b 0 (Int32.of_int w);
      b)

let clcw_wire_vals =
  Array.map (fun b -> Wire.Codec.decode Clcw.codec b 0) clcw_bytes

(** {1 Space Packet test data} *)

let sp_bytes =
  Array.init 1000 (fun i ->
      let b = Bytes.create 6 in
      Bytes.set_uint16_be b 0 (((i mod 2) lsl 12) lor (i mod 2048));
      Bytes.set_uint16_be b 2 (0xC000 lor (i mod 16384));
      Bytes.set_uint16_be b 4 (i mod 256);
      b)

let sp_wire_vals =
  Array.map (fun b -> Space_packet.decode_packed_header b 0) sp_bytes

(** {1 TM test data} *)

let tm_bytes =
  Array.init 1000 (fun i ->
      let b = Bytes.create 6 in
      let w0 =
        ((i mod 1024 land 0x3FF) lsl 4)
        lor ((i mod 8 land 0x7) lsl 1)
        lor (i mod 2)
      in
      Bytes.set_uint16_be b 0 w0;
      Bytes.set_uint16_be b 2 (((i mod 256) lsl 8) lor (i * 7 mod 256));
      Bytes.set_uint16_be b 4 ((1 lsl 14) lor (3 lsl 11) lor (i mod 2048));
      b)

let tm_wire_vals = Array.map (fun b -> Tm.decode_packed_header b 0) tm_bytes

(** {1 Roundtrip loops} *)

let clcw_roundtrip () =
  for i = 0 to Array.length clcw_bytes - 1 do
    let t = Wire.Codec.decode Clcw.codec clcw_bytes.(i) 0 in
    let buf = Bytes.create 4 in
    Wire.Codec.encode Clcw.codec t buf 0
  done

let sp_roundtrip () =
  for i = 0 to Array.length sp_bytes - 1 do
    let t = Space_packet.decode_packed_header sp_bytes.(i) 0 in
    let buf = Bytes.create 6 in
    Space_packet.encode_packed_header t buf 0
  done

let tm_roundtrip () =
  for i = 0 to Array.length tm_bytes - 1 do
    let t = Tm.decode_packed_header tm_bytes.(i) 0 in
    let buf = Bytes.create 6 in
    Tm.encode_packed_header t buf 0
  done

(* Decode-only loops *)
let clcw_decode () =
  for i = 0 to Array.length clcw_bytes - 1 do
    let _ = Wire.Codec.decode Clcw.codec clcw_bytes.(i) 0 in
    ()
  done

let sp_decode () =
  for i = 0 to Array.length sp_bytes - 1 do
    let _ = Space_packet.decode_packed_header sp_bytes.(i) 0 in
    ()
  done

let tm_decode () =
  for i = 0 to Array.length tm_bytes - 1 do
    let _ = Tm.decode_packed_header tm_bytes.(i) 0 in
    ()
  done

(* Encode-only loops *)
let clcw_encode () =
  for i = 0 to Array.length clcw_wire_vals - 1 do
    let buf = Bytes.create 4 in
    Wire.Codec.encode Clcw.codec clcw_wire_vals.(i) buf 0
  done

let sp_encode () =
  for i = 0 to Array.length sp_wire_vals - 1 do
    let buf = Bytes.create 6 in
    Space_packet.encode_packed_header sp_wire_vals.(i) buf 0
  done

let tm_encode () =
  for i = 0 to Array.length tm_wire_vals - 1 do
    let buf = Bytes.create 6 in
    Tm.encode_packed_header tm_wire_vals.(i) buf 0
  done

let run label decode encode roundtrip =
  Printf.printf "  %s decode...\n%!" label;
  for _ = 1 to iterations do
    decode ()
  done;
  Printf.printf "  %s encode...\n%!" label;
  for _ = 1 to iterations do
    encode ()
  done;
  Printf.printf "  %s roundtrip...\n%!" label;
  for _ = 1 to iterations do
    roundtrip ()
  done

let () =
  Memtrace.trace_if_requested ~context:"wire-codecs" ();

  let impl = if Array.length Sys.argv > 1 then Sys.argv.(1) else "all" in

  Printf.printf "Wire Codec memtrace profiling (%s)\n%!" impl;
  Printf.printf "(%d iterations x 1000 values)\n\n%!" iterations;

  (match impl with
  | "clcw" -> run "CLCW" clcw_decode clcw_encode clcw_roundtrip
  | "space-packet" -> run "Space Packet" sp_decode sp_encode sp_roundtrip
  | "tm" -> run "TM" tm_decode tm_encode tm_roundtrip
  | _ ->
      run "CLCW" clcw_decode clcw_encode clcw_roundtrip;
      run "Space Packet" sp_decode sp_encode sp_roundtrip;
      run "TM" tm_decode tm_encode tm_roundtrip);

  Printf.printf "\nDone.\n"
