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
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}

type any_schema = Any : 'a schema -> any_schema

let schema name codec size default make_data =
  {
    name;
    size;
    default;
    make_data;
    decode = Wire.Codec.decode_exn codec;
    encode = Wire.Codec.encode codec;
  }

(* -- Nested codec schemas for allocation tracking -- *)

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
        ( Wire.Field.optional "FECF" ~present:(Wire.bool true) Wire.uint16be
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
        ( Wire.Field.repeat "Items" ~size:(Wire.Field.ref f_r_len)
            (Wire.codec inner_codec)
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

(* SSH_MSG_KEXINIT (RFC 4253 7.1): a 16-byte cookie followed by ten
   length-prefixed name-lists, then a flag and a reserved word. Every
   name-list is a [byte_slice] whose size is [ref length], so each one
   compiles through [compile_expr] (codec.ml:550) and reads its length
   field via [int_of_typ_value] (eval.ml). Because the offsets are
   dynamic, decoding field k re-chains every prior [size_fn], making the
   per-decode tuple/option allocation quadratic in the ten name-lists --
   the exact shape that surfaces both hotspots in a trace. *)
module Slice = Bytesrw.Bytes.Slice

type kexinit = {
  k_cookie : Slice.t;
  k_kex : Slice.t;
  k_host_key : Slice.t;
  k_enc_c2s : Slice.t;
  k_enc_s2c : Slice.t;
  k_mac_c2s : Slice.t;
  k_mac_s2c : Slice.t;
  k_comp_c2s : Slice.t;
  k_comp_s2c : Slice.t;
  k_lang_c2s : Slice.t;
  k_lang_s2c : Slice.t;
  k_first_follows : int;
  k_reserved : int;
}

let kex_len name = Wire.Field.v name Wire.uint32be
let f_kex_len = kex_len "KexLen"
let f_hk_len = kex_len "HostKeyLen"
let f_ec_len = kex_len "EncC2SLen"
let f_es_len = kex_len "EncS2CLen"
let f_mc_len = kex_len "MacC2SLen"
let f_ms_len = kex_len "MacS2CLen"
let f_cc_len = kex_len "CompC2SLen"
let f_cs_len = kex_len "CompS2CLen"
let f_lc_len = kex_len "LangC2SLen"
let f_ls_len = kex_len "LangS2CLen"

let name_list name len_field =
  Wire.Field.v name (Wire.byte_slice ~size:(Wire.Field.ref len_field))

let kexinit_codec =
  Wire.Codec.v "Kexinit"
    (fun cookie _kl kex _hl hk _ecl ec _esl es _mcl mc _msl ms _ccl cc _csl cs
         _lcl lc _lsl ls first reserved ->
      {
        k_cookie = cookie;
        k_kex = kex;
        k_host_key = hk;
        k_enc_c2s = ec;
        k_enc_s2c = es;
        k_mac_c2s = mc;
        k_mac_s2c = ms;
        k_comp_c2s = cc;
        k_comp_s2c = cs;
        k_lang_c2s = lc;
        k_lang_s2c = ls;
        k_first_follows = first;
        k_reserved = reserved;
      })
    Wire.Codec.
      [
        ( Wire.Field.v "Cookie" (Wire.byte_slice ~size:(Wire.int 16)) $ fun r ->
          r.k_cookie );
        (f_kex_len $ fun r -> Slice.length r.k_kex);
        (name_list "Kex" f_kex_len $ fun r -> r.k_kex);
        (f_hk_len $ fun r -> Slice.length r.k_host_key);
        (name_list "HostKey" f_hk_len $ fun r -> r.k_host_key);
        (f_ec_len $ fun r -> Slice.length r.k_enc_c2s);
        (name_list "EncC2S" f_ec_len $ fun r -> r.k_enc_c2s);
        (f_es_len $ fun r -> Slice.length r.k_enc_s2c);
        (name_list "EncS2C" f_es_len $ fun r -> r.k_enc_s2c);
        (f_mc_len $ fun r -> Slice.length r.k_mac_c2s);
        (name_list "MacC2S" f_mc_len $ fun r -> r.k_mac_c2s);
        (f_ms_len $ fun r -> Slice.length r.k_mac_s2c);
        (name_list "MacS2C" f_ms_len $ fun r -> r.k_mac_s2c);
        (f_cc_len $ fun r -> Slice.length r.k_comp_c2s);
        (name_list "CompC2S" f_cc_len $ fun r -> r.k_comp_c2s);
        (f_cs_len $ fun r -> Slice.length r.k_comp_s2c);
        (name_list "CompS2C" f_cs_len $ fun r -> r.k_comp_s2c);
        (f_lc_len $ fun r -> Slice.length r.k_lang_c2s);
        (name_list "LangC2S" f_lc_len $ fun r -> r.k_lang_c2s);
        (f_ls_len $ fun r -> Slice.length r.k_lang_s2c);
        (name_list "LangS2C" f_ls_len $ fun r -> r.k_lang_s2c);
        (Wire.Field.v "FirstFollows" Wire.uint8 $ fun r -> r.k_first_follows);
        (Wire.Field.v "Reserved" Wire.uint32be $ fun r -> r.k_reserved);
      ]

let kexinit_lists =
  [
    "curve25519-sha256,ecdh-sha2-nistp256,diffie-hellman-group14-sha256";
    "ssh-ed25519,rsa-sha2-512,rsa-sha2-256";
    "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-ctr";
    "chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-ctr";
    "hmac-sha2-256-etm@openssh.com,hmac-sha2-256";
    "hmac-sha2-256-etm@openssh.com,hmac-sha2-256";
    "none,zlib@openssh.com";
    "none,zlib@openssh.com";
    "";
    "";
  ]

let kexinit_wire () =
  let b = Buffer.create 512 in
  Buffer.add_string b (String.make 16 '\x42');
  List.iter
    (fun s ->
      let n = String.length s in
      Buffer.add_char b (Char.chr ((n lsr 24) land 0xFF));
      Buffer.add_char b (Char.chr ((n lsr 16) land 0xFF));
      Buffer.add_char b (Char.chr ((n lsr 8) land 0xFF));
      Buffer.add_char b (Char.chr (n land 0xFF));
      Buffer.add_string b s)
    kexinit_lists;
  Buffer.add_char b '\x00';
  Buffer.add_string b "\x00\x00\x00\x00";
  Bytes.unsafe_of_string (Buffer.contents b)

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
      try s.encode (s.decode data.(i) 0) buf 0 with Wire.Parse_error _ -> ()
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

let run_kexinit () =
  let buf = kexinit_wire () in
  let decode = Wire.Codec.decode_exn kexinit_codec in
  Fmt.pr "  KEXINIT decode (10 length-prefixed name-lists)...\n%!";
  for _ = 1 to iterations do
    for _ = 1 to n_values do
      ignore (Sys.opaque_identity (decode buf 0))
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
  if filter_matches "kexinit" then run_kexinit ();
  Fmt.pr "\nDone.\n"
