(* Whole-record validation: [Codec.validate] against the staged
   [Codec.validator], on headers a server or a pager validates once per
   buffer. *)

open Wire

(* IPv4 with the RFC 791 checks a receiver makes before trusting the header:
   version 4, a header of at least five words that the datagram holds. *)
type ipv4 = { version : int; ihl : int; total_length : int; rest : string }

let ip_ihl =
  Field.v "IHL" ~self_constraint:Expr.(fun v -> v >= int 5) (bits ~width:4 U8)

let ip_total_length =
  Field.v "TotalLength" (map ~decode:UInt16.to_int ~encode:UInt16.v uint16be)

let ipv4_checked =
  Codec.v "IPv4Checked"
    ~where:Expr.(Field.ref ip_total_length >= int 4 * Field.ref ip_ihl)
    (fun version ihl _tos total_length rest ->
      { version; ihl; total_length; rest })
    Codec.
      [
        ( Field.v "Version"
            ~self_constraint:Expr.(fun v -> v = int 4)
            (bits ~width:4 U8)
        $ fun p -> p.version );
        (ip_ihl $ fun p -> p.ihl);
        (Field.v "TOS" (byte_array ~size:(int 1)) $ fun _ -> "\x00");
        (ip_total_length $ fun p -> p.total_length);
        (Field.v "Rest" (byte_array ~size:(int 16)) $ fun p -> p.rest);
      ]

let ipv4_buf =
  Bytes.of_string
    "\x45\x00\x00\x54\x1c\x46\x40\x00\x40\x06\x00\x00\xc0\xa8\x00\x01\xc0\xa8\x00\xc7"

(* TCP with a data offset that covers the fixed header. *)
type tcp = { ports : string; data_offset : int; flags : int; tail : string }

let tcp_checked =
  Codec.v "TCPChecked"
    (fun ports data_offset flags tail -> { ports; data_offset; flags; tail })
    Codec.
      [
        (Field.v "Ports+Seq+Ack" (byte_array ~size:(int 12)) $ fun t -> t.ports);
        ( Field.v "DataOffset"
            ~self_constraint:Expr.(fun v -> v >= int 5)
            (bits ~width:4 U16be)
        $ fun t -> t.data_offset );
        (Field.v "Flags" (bits ~width:12 U16be) $ fun t -> t.flags);
        (Field.v "Tail" (byte_array ~size:(int 6)) $ fun t -> t.tail);
      ]

let tcp_buf =
  Bytes.of_string
    "\x30\x39\x00\x50\x00\x00\x00\x01\x00\x00\x00\x00\x50\x02\xff\xff\x00\x00\x00\x00"

(* CCSDS 133.0-B-2 primary header: version 0, a data field the length covers. *)
type ccsds = { cversion : int; apid : int; length : int; data : string }

let sp_length =
  Field.v "DataLength" (map ~decode:UInt16.to_int ~encode:UInt16.v uint16be)

let ccsds_checked =
  Codec.v "CCSDSChecked"
    (fun cversion apid _seq length data -> { cversion; apid; length; data })
    Codec.
      [
        ( Field.v "Version"
            ~self_constraint:Expr.(fun v -> v = int 0)
            (bits ~width:3 U16be)
        $ fun p -> p.cversion );
        (Field.v "Ident" (bits ~width:13 U16be) $ fun p -> p.apid);
        (Field.v "Seq" (byte_array ~size:(int 2)) $ fun _ -> "\x00\x00");
        (sp_length $ fun p -> p.length);
        ( Field.v "Data" (byte_array ~size:Expr.(Field.ref sp_length + int 1))
        $ fun p -> p.data );
      ]

let ccsds_buf =
  let b = Bytes.make (6 + 64) '\x00' in
  Bytes.set_uint16_be b 0 0x0801;
  Bytes.set_uint16_be b 2 0xc000;
  Bytes.set_uint16_be b 4 63;
  b

(* A SQLite leaf-table b-tree page, the codec a pager validates per page it
   fetches: a closed kind enum, counts bounded by the page, and spans sized by
   them. *)
type page = {
  kind : int;
  cell_count : int;
  content_start : int;
  pointers : string;
  gap : string;
  content : string;
}

let u16 = map ~decode:UInt16.to_int ~encode:UInt16.v uint16be

let kind =
  Field.v "kind"
    ~self_constraint:Expr.(fun v -> v = int 0x0d)
    (map ~decode:UInt8.to_int ~encode:UInt8.v
       (enum "PageKind"
          [
            ("InteriorIndex", 0x02);
            ("InteriorTable", 0x05);
            ("LeafIndex", 0x0a);
            ("LeafTable", 0x0d);
          ]
          uint8))

let cell_count =
  Field.v "cell_count" ~self_constraint:Expr.(fun v -> v <= int 32764) u16

let content_start =
  Field.v "cell_content_start"
    ~self_constraint:
      Expr.(
        fun v ->
          v = int 0
          || v >= int 8
             && Field.ref cell_count + Field.ref cell_count + int 8 <= v)
    u16

let page_codec =
  Codec.v "LeafTablePage"
    (fun kind _ cell_count content_start _ pointers gap content ->
      { kind; cell_count; content_start; pointers; gap; content })
    Codec.
      [
        (kind $ fun p -> p.kind);
        (Field.v "first_freeblock" u16 $ fun _ -> 0);
        (cell_count $ fun p -> p.cell_count);
        (content_start $ fun p -> p.content_start);
        (Field.v "fragmented_bytes" uint8 $ fun _ -> UInt8.v 0);
        ( Field.v "cell_pointers"
            (byte_array ~size:Expr.(int 2 * Field.ref cell_count))
        $ fun p -> p.pointers );
        ( Field.v "gap"
            (byte_array
               ~size:
                 Expr.(
                   Field.ref content_start - int 8
                   - (int 2 * Field.ref cell_count)))
        $ fun p -> p.gap );
        (Field.v "cell_content" all_bytes $ fun p -> p.content);
      ]

let page_buf =
  let b = Bytes.make 4096 '\x00' in
  Bytes.set_uint8 b 0 0x0d;
  Bytes.set_uint16_be b 3 40;
  Bytes.set_uint16_be b 5 1024;
  b

type case = Case : string * 'r Codec.t * bytes -> case

let cases =
  [
    Case ("IPv4 (bounds only)", Net.ipv4_codec, Bytes.make Net.ipv4_size '\x45');
    Case ("IPv4 (RFC 791 checks)", ipv4_checked, ipv4_buf);
    Case ("TCP (data offset)", tcp_checked, tcp_buf);
    Case ("CCSDS space packet", ccsds_checked, ccsds_buf);
    Case ("SQLite leaf page", page_codec, page_buf);
  ]

let validate_fn (Case (_, c, buf)) = fun () -> Codec.validate c buf 0

let staged_fn (Case (_, c, buf)) =
  let v = Staged.unstage (Codec.validator c) in
  fun () -> v buf 0

let name (Case (n, _, _)) = n

let loop n f () =
  for _ = 1 to n do
    f ()
  done

(* [bench.exe CASE MODE N] runs one loop and nothing else, for an instruction
   count under [/usr/bin/time -l]; with no arguments it prints the table. *)
let () =
  match Sys.argv with
  | [| _; i; mode; n |] ->
      let case = List.nth cases (int_of_string i) in
      let f = if mode = "staged" then staged_fn case else validate_fn case in
      loop (int_of_string n) f ()
  | _ ->
      let n = 10_000_000 in
      Fmt.pr "%-24s %12s %12s %10s %10s@." "codec" "validate ns" "staged ns"
        "alloc w" "staged w";
      List.iter
        (fun case ->
          let v = validate_fn case and s = staged_fn case in
          v ();
          s ();
          let tv = Bench_lib.time_ns n (loop n v)
          and ts = Bench_lib.time_ns n (loop n s) in
          let av = Bench_lib.alloc_words 100_000 v
          and as_ = Bench_lib.alloc_words 100_000 s in
          Fmt.pr "%-24s %12.1f %12.1f %10.1f %10.1f@." (name case) tv ts av as_)
        cases
