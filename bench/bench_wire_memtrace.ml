(** Memtrace allocation profiling for Wire codecs.

    Profiles decode, encode, and roundtrip for all benchmark schemas to identify
    allocation hotspots.

    Usage: MEMTRACE=trace.ctf dune exec bench/bench_wire_memtrace.exe
    [-- SCHEMA]

    SCHEMA is one of the schema names (minimal, allints, ...) or "all". *)

open Bench_schemas

let n_values = 1000
let iterations = 10_000

let run_schema (Any s) =
  let data = s.make_data n_values in
  let buf = Bytes.create s.size in
  Fmt.pr "  %s decode...\n%!" s.name;
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      ignore (s.decode data.(i) 0)
    done
  done;
  Fmt.pr "  %s encode...\n%!" s.name;
  for _ = 1 to iterations do
    for _ = 0 to n_values - 1 do
      s.encode s.default buf 0
    done
  done;
  Fmt.pr "  %s roundtrip...\n%!" s.name;
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      let v = s.decode data.(i) 0 in
      s.encode v buf 0
    done
  done

let run_zero_copy () =
  let data = clcw_data n_values in
  let slices =
    Array.map
      (fun buf ->
        Bytesrw.Bytes.Slice.make buf ~first:0 ~length:(Bytes.length buf))
      data
  in
  Fmt.pr "  CLCW zero-copy get...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length slices - 1 do
      ignore (Wire.Codec.get cw_report slices.(i))
    done
  done;
  Fmt.pr "  CLCW zero-copy set...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length slices - 1 do
      Wire.Codec.set cw_report slices.(i) 42
    done
  done;
  Fmt.pr "  CLCW zero-copy roundtrip...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length slices - 1 do
      let x = Wire.Codec.get cw_report slices.(i) in
      Wire.Codec.set cw_report slices.(i) x
    done
  done

let () =
  Memtrace.trace_if_requested ~context:"wire-codecs" ();
  let filter = if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None in
  Fmt.pr "Wire Codec memtrace profiling\n%!";
  Fmt.pr "(%d iterations x %d values)\n\n%!" iterations n_values;
  List.iter
    (fun (Any s as any) ->
      match filter with
      | Some f
        when String.lowercase_ascii f <> "all"
             && String.lowercase_ascii f <> String.lowercase_ascii s.name ->
          ()
      | _ -> run_schema any)
    all_schemas;
  Fmt.pr "\n";
  (match filter with
  | Some f
    when String.lowercase_ascii f <> "all" && String.lowercase_ascii f <> "clcw"
    ->
      ()
  | _ -> run_zero_copy ());
  Fmt.pr "\nDone.\n"
