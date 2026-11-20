(* The application benchmarks across languages, OCaml side.

   bench.exe inputs DIR PACKETS FRAMES WORDS
                              writes the inputs of bench/routing, bench/gateway
                              and bench/clcw, with their generators, to
                              DIR/routing.bin (PACKETS space packets),
                              DIR/gateway.bin (FRAMES TM frames) and
                              DIR/clcw.bin (WORDS CLCW words)
   bench.exe run FILE ITEMS RUNS [ROW | --list]
                              runs the workload FILE names on its ITEMS items,
                              RUNS timed passes, and prints a RESULT line per
                              row, or only for the row named ROW, failing when
                              there is none; --list prints the row names
   bench.exe write FILE ITEMS RUNS [ROW | --list]
                              the same for the workload's writing side, on the
                              corpus <workload>_write
   bench.exe report FILE...   prints the RESULT lines of each FILE as tables

   The [Wire.Codec] rows run the workload its benchmark times:
   [Routing_workload.run_packets], [Gateway_workload.checksum] and
   [Clcw_workload.run_all], and on the writing side
   [Routing_workload.restamp], [Gateway_workload.frame] and
   [Clcw_workload.report]. The [wire.ocaml] rows run the same application
   logic over the OCaml Wire_ocaml generates from the same codecs, and
   cargo/src/main.rs over the Rust Wire_rust generates and over released
   crates; every row must give the same result. A writing row's result is the
   FNV-1a hash of the buffer it writes. *)

let write file b =
  Out_channel.with_open_bin file (fun oc -> Out_channel.output_bytes oc b)

let inputs dir ~packets ~frames ~words =
  let routing, _, _ = Routing_workload.generate_stream packets in
  write (Filename.concat dir "routing.bin") routing;
  let gateway, _ = Gateway_workload.generate_frames frames in
  write (Filename.concat dir "gateway.bin") gateway;
  write (Filename.concat dir "clcw.bin") (Clcw_workload.generate_stream words)

(* {1 The application logic over the generated OCaml}

   The loops of the workloads, as cargo/src/main.rs writes them in Rust, over
   the modules Wire_ocaml generates, as a user of those modules writes them,
   the same source on OCaml and on OxCaml. *)

(* dune generates for the OxCaml target on an OxCaml compiler. *)
let generated =
  if Bench_lib.oxcaml then "wire.ocaml, OxCaml target" else "wire.ocaml"

let unit = function
  | "routing" | "routing_write" -> "packet"
  | "gateway" | "gateway_write" -> "frame"
  | "clcw" | "clcw_write" -> "word"
  | w -> Fmt.failwith "bench.exe: no workload %s" w

(* The result of a writing row: the FNV-1a hash of the buffer it wrote, taken
   when the result is printed, outside the timed passes. *)
let pp_written ppf b =
  let h = ref Gateway_workload.checksum_init in
  Bytes.iter (fun c -> h := Gateway_workload.hash_int !h (Char.code c)) b;
  Fmt.pf ppf "%016x" !h

(* The gateway hash, an [int] modulo 2^63, as the Rust rows print theirs. *)
let pp_checksum ppf h = Fmt.pf ppf "%u" h

let write file items runs filter =
  let buf = Bench_lib.read_file file in
  let w = Filename.remove_extension (Filename.basename file) in
  let corpus = w ^ "_write" in
  (* Each row writes into its own buffer, allocated by its first pass. *)
  let measure name len f =
    let out = lazy (Bytes.make len '\000') in
    Bench_lib.measure ~corpus ~filter ~name ~items ~runs ~pp:pp_written
      (fun () -> f (Lazy.force out))
  in
  match w with
  | "routing" ->
      let total_bytes = Bytes.length buf in
      measure "Wire.Codec" total_bytes (fun out ->
          Routing_workload.restamp ~buf ~total_bytes ~out items;
          out);
      measure generated total_bytes (fun out ->
          Loops.restamp (Sys.opaque_identity buf) items out)
  | "gateway" ->
      let len = items * Gateway_workload.cadu_size in
      measure "Wire.Codec" len (fun out ->
          Gateway_workload.frame ~out items;
          out);
      measure generated len (fun out -> Loops.frame items out)
  | "clcw" ->
      let len = items * Clcw_workload.word_size in
      measure "Wire.Codec" len (fun out ->
          Clcw_workload.report ~out items;
          out);
      measure generated len (fun out -> Loops.report items out);
      if Lanes.rows then
        measure "NEON lanes, OxCaml" len (fun out -> Lanes.report items out)
  | w -> Fmt.failwith "bench.exe: no workload %s" w

let run file items runs filter =
  let buf = Bench_lib.read_file file in
  let corpus = Filename.remove_extension (Filename.basename file) in
  let measure name = Bench_lib.measure ~corpus ~filter ~name ~items ~runs in
  match corpus with
  | "routing" ->
      let st = Routing_workload.state () in
      let total_bytes = Bytes.length buf in
      measure "Wire.Codec" ~pp:Routing_workload.pp_counts (fun () ->
          Routing_workload.reset st;
          Routing_workload.run_packets ~buf ~total_bytes st items);
      measure generated ~pp:Routing_workload.pp_counts (fun () ->
          Loops.routing (Sys.opaque_identity buf) items)
  | "gateway" ->
      let st = Gateway_workload.state buf in
      if items <> Bytes.length buf / Gateway_workload.cadu_size then
        Fmt.failwith "bench.exe: %s does not hold %d frames" file items;
      measure "Wire.Codec" ~pp:pp_checksum (fun () ->
          Gateway_workload.checksum st);
      measure generated ~pp:pp_checksum (fun () ->
          Loops.gateway (Sys.opaque_identity buf))
  | "clcw" ->
      let st = Clcw_workload.state buf in
      if items <> Bytes.length buf / Clcw_workload.word_size then
        Fmt.failwith "bench.exe: %s does not hold %d words" file items;
      measure "Wire.Codec" ~pp:Fmt.int (fun () ->
          Clcw_workload.reset st;
          Clcw_workload.run_all st);
      measure generated ~pp:Fmt.int (fun () ->
          Loops.clcw (Sys.opaque_identity buf));
      if Lanes.rows then
        measure "NEON lanes, OxCaml" ~pp:Fmt.int (fun () ->
            Lanes.clcw (Sys.opaque_identity buf))
  | w -> Fmt.failwith "bench.exe: no workload %s" w

let usage () =
  prerr_endline
    "usage: bench.exe (inputs DIR PACKETS FRAMES WORDS | (run | write) FILE \
     ITEMS RUNS [ROW | --list] | report FILE...)";
  exit 2

let rows mode file items runs row =
  let filter = Bench_lib.filter row in
  let items = int_of_string items and runs = int_of_string runs in
  (if mode = "run" then run else write) file items runs filter;
  Bench_lib.finish filter

let () =
  match Array.to_list Sys.argv with
  | [ _; "inputs"; dir; packets; frames; words ] ->
      inputs dir ~packets:(int_of_string packets) ~frames:(int_of_string frames)
        ~words:(int_of_string words)
  | [ _; (("run" | "write") as mode); file; items; runs ] ->
      rows mode file items runs None
  | [ _; (("run" | "write") as mode); file; items; runs; row ] ->
      rows mode file items runs (Some row)
  | _ :: "report" :: files -> Bench_lib.report ~unit files
  | _ -> usage ()
