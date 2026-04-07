(** Memtrace allocation profiling for Wire codecs.

    Profiles decode, encode, and roundtrip for all benchmark schemas to identify
    allocation hotspots.

    Usage: MEMTRACE=trace.ctf dune exec bench/memtrace.exe [-- SCHEMA]

    SCHEMA is one of the schema names (minimal, allints, ...) or "all". *)

open Space

type 'a schema = {
  name : string;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> ('a, Wire.parse_error) result;
  encode : 'a -> bytes -> int -> unit;
}

type any_schema = Any : 'a schema -> any_schema

let schema name codec size default make_data =
  {
    name;
    size;
    default;
    make_data;
    decode = Wire.Codec.decode codec;
    encode = Wire.Codec.encode codec;
  }

(* ── Nested codec schemas for allocation tracking ── *)

(* Codec embed: outer record containing an inner sub-codec *)
type inner_rec = { i_tag : int; i_value : int }
type outer_rec = { o_hdr : int; o_inner : inner_rec; o_trail : int }

let inner_codec =
  Wire.Codec.v "Inner"
    (fun tag value -> { i_tag = tag; i_value = value })
    Wire.Codec.
      [
        (Wire.Field.v "Tag" Wire.uint8 $ fun r -> r.i_tag);
        (Wire.Field.v "Value" Wire.uint16be $ fun r -> r.i_value);
      ]

let outer_codec =
  Wire.Codec.v "Outer"
    (fun hdr inner trail -> { o_hdr = hdr; o_inner = inner; o_trail = trail })
    Wire.Codec.
      [
        (Wire.Field.v "Hdr" Wire.uint8 $ fun r -> r.o_hdr);
        (Wire.Field.v "Inner" (Wire.codec inner_codec) $ fun r -> r.o_inner);
        (Wire.Field.v "Trail" Wire.uint8 $ fun r -> r.o_trail);
      ]

let outer_size = 5

let outer_default =
  { o_hdr = 0; o_inner = { i_tag = 0; i_value = 0 }; o_trail = 0 }

let outer_data n =
  Array.init n (fun i ->
      let b = Bytes.create outer_size in
      Bytes.set_uint8 b 0 (i land 0xFF);
      Bytes.set_uint8 b 1 ((i + 1) land 0xFF);
      Bytes.set_uint16_be b 2 (i land 0xFFFF);
      Bytes.set_uint8 b 4 ((i + 2) land 0xFF);
      b)

(* Optional: record with optional trailing field *)
type opt_rec = { opt_hdr : int; opt_data : int; opt_fecf : int option }

let opt_codec_present =
  Wire.Codec.v "OptPresent"
    (fun hdr data fecf -> { opt_hdr = hdr; opt_data = data; opt_fecf = fecf })
    Wire.Codec.
      [
        (Wire.Field.v "Hdr" Wire.uint16be $ fun r -> r.opt_hdr);
        (Wire.Field.v "Data" Wire.uint16be $ fun r -> r.opt_data);
        ( Wire.Field.v "FECF" (Wire.optional (Wire.bool true) Wire.uint16be)
        $ fun r -> r.opt_fecf );
      ]

let opt_present_size = 6
let opt_present_default = { opt_hdr = 0; opt_data = 0; opt_fecf = Some 0 }

let opt_present_data n =
  Array.init n (fun i ->
      let b = Bytes.create opt_present_size in
      Bytes.set_uint16_be b 0 (i land 0xFFFF);
      Bytes.set_uint16_be b 2 ((i + 1) land 0xFFFF);
      Bytes.set_uint16_be b 4 ((i + 2) land 0xFFFF);
      b)

(* Repeat: container with repeated sub-codec elements *)
type repeat_rec = { r_len : int; r_items : inner_rec list }

let f_r_len = Wire.Field.v "Len" Wire.uint8

let repeat_codec =
  Wire.Codec.v "Repeat"
    (fun len items -> { r_len = len; r_items = items })
    Wire.Codec.
      [
        (f_r_len $ fun r -> r.r_len);
        ( Wire.Field.v "Items"
            (Wire.repeat ~size:(Wire.Field.ref f_r_len) (Wire.codec inner_codec))
        $ fun r -> r.r_items );
      ]

let repeat_size = 10 (* 1 + 3*3 = 10 bytes for 3 inner items *)

let repeat_default =
  {
    r_len = 9;
    r_items =
      [
        { i_tag = 0; i_value = 0 };
        { i_tag = 0; i_value = 0 };
        { i_tag = 0; i_value = 0 };
      ];
  }

let repeat_data n =
  Array.init n (fun i ->
      let b = Bytes.create repeat_size in
      Bytes.set_uint8 b 0 9;
      (* 3 items * 3 bytes each *)
      (* 3 inner items *)
      for j = 0 to 2 do
        let off = 1 + (j * 3) in
        Bytes.set_uint8 b off ((i + j) land 0xFF);
        Bytes.set_uint16_be b (off + 1) ((i + j) land 0xFFFF)
      done;
      b)

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
    Any (schema "CodecEmbed" outer_codec outer_size outer_default outer_data);
    Any
      (schema "OptPresent" opt_codec_present opt_present_size
         opt_present_default opt_present_data);
    Any (schema "Repeat" repeat_codec repeat_size repeat_default repeat_data);
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
      match v with Ok v -> s.encode v buf 0 | Error _ -> ()
    done
  done

let cf_report = Space.bf_cw_report
let cf_lockout = Space.bf_cw_lockout
let cf_wait = Space.bf_cw_wait
let cf_retransmit = Space.bf_cw_retransmit

let run_zero_copy () =
  let data = clcw_data n_values in
  let get_report = Wire.Staged.unstage (Wire.Codec.get clcw_codec cf_report) in
  let set_report = Wire.Staged.unstage (Wire.Codec.set clcw_codec cf_report) in
  Fmt.pr "  CLCW zero-copy get...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      ignore (get_report data.(i) 0)
    done
  done;
  Fmt.pr "  CLCW zero-copy set...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      set_report data.(i) 0 42
    done
  done;
  Fmt.pr "  CLCW zero-copy roundtrip...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      let x = get_report data.(i) 0 in
      set_report data.(i) 0 x
    done
  done

let run_clcw_polling () =
  let data = clcw_data n_values in
  let get_lockout =
    Wire.Staged.unstage (Wire.Codec.get clcw_codec cf_lockout)
  in
  let get_wait = Wire.Staged.unstage (Wire.Codec.get clcw_codec cf_wait) in
  let get_retransmit =
    Wire.Staged.unstage (Wire.Codec.get clcw_codec cf_retransmit)
  in
  let get_report = Wire.Staged.unstage (Wire.Codec.get clcw_codec cf_report) in
  Fmt.pr "  CLCW polling (4 bitfield reads per word)...\n%!";
  for _ = 1 to iterations do
    for i = 0 to Array.length data - 1 do
      let buf = data.(i) in
      let lockout = get_lockout buf 0 in
      let wait = get_wait buf 0 in
      let retransmit = get_retransmit buf 0 in
      let report = get_report buf 0 in
      ignore (Sys.opaque_identity (lockout + wait + retransmit + report))
    done
  done

let () =
  Memtrace.trace_if_requested ~context:"wire-codecs" ();
  let filter = if Array.length Sys.argv > 1 then Some Sys.argv.(1) else None in
  Fmt.pr "Wire Codec memtrace profiling\n%!";
  Fmt.pr "(%d iterations x %d values)\n\n%!" iterations n_values;
  let filter_matches s =
    match filter with
    | None | Some "all" -> true
    | Some f -> String.lowercase_ascii f = String.lowercase_ascii s
  in
  List.iter
    (fun (Any s as any) -> if filter_matches s.name then run_schema any)
    all_schemas;
  Fmt.pr "\n";
  if filter_matches "clcw" then run_zero_copy ();
  if filter_matches "polling" || filter_matches "clcw" then run_clcw_polling ();
  Fmt.pr "\nDone.\n"
