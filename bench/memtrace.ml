(** Memtrace allocation profiling for Wire codecs.

    Profiles decode, encode, and roundtrip for all benchmark schemas to identify
    allocation hotspots.

    Usage: MEMTRACE=trace.ctf dune exec bench/memtrace.exe [-- SCHEMA]

    SCHEMA is one of the schema names (minimal, allints, ...) or "all". *)

open Space

type 'a schema = {
  name : string;
  codec : 'a Wire.Codec.t;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}

type any_schema = Any : 'a schema -> any_schema

let schema name codec size default make_data =
  {
    name;
    codec;
    size;
    default;
    make_data;
    decode = Wire.Codec.decode codec;
    encode = Wire.Codec.encode codec;
  }

let all_schemas =
  [
    Any
      (schema "Minimal" Demo.minimal_codec Demo.minimal_size
         Demo.minimal_default Demo.minimal_data);
    Any
      (schema "AllInts" Demo.all_ints_codec Demo.all_ints_size
         Demo.all_ints_default Demo.all_ints_data);
    Any
      (schema "Bitfield8" Demo.bf8_codec Demo.bf8_size Demo.bf8_default
         Demo.bf8_data);
    Any
      (schema "Bitfield16" Demo.bf16_codec Demo.bf16_size Demo.bf16_default
         Demo.bf16_data);
    Any
      (schema "Bitfield32" Demo.bf32_codec Demo.bf32_size Demo.bf32_default
         Demo.bf32_data);
    Any
      (schema "BoolFields" Demo.bool_fields_codec Demo.bool_fields_size
         Demo.bool_fields_default Demo.bool_fields_data);
    Any
      (schema "SpacePacket" packet_codec packet_size packet_default packet_data);
    Any (schema "CLCW" clcw_codec clcw_size clcw_default clcw_data);
    Any
      (schema "TMFrame" tm_frame_codec tm_frame_size tm_frame_default
         tm_frame_data);
    Any
      (schema "LargeMixed" Demo.large_mixed_codec Demo.large_mixed_size
         Demo.large_mixed_default Demo.large_mixed_data);
  ]

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
  Fmt.pr "  CLCW zero-copy get...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      ignore (Wire.Codec.get clcw_codec cw_report data.(i) 0)
    done
  done;
  Fmt.pr "  CLCW zero-copy set...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      Wire.Codec.set clcw_codec cw_report data.(i) 0 42
    done
  done;
  Fmt.pr "  CLCW zero-copy roundtrip...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      let x = Wire.Codec.get clcw_codec cw_report data.(i) 0 in
      Wire.Codec.set clcw_codec cw_report data.(i) 0 x
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
