(* Tests for codec.ml: the check pass, and the order and cost of reads and writes. *)

open Wire
open Test_helpers

(* A codec with no checks (plain scalars and byte spans) must validate without
   allocating: a hot read path validates a header then reads fields zero-copy,
   so a per-validate allocation here turns an O(log N) seek into O(N log N)
   garbage. Validate runs the structural bounds check but no validator pass. *)
let test_validate_check_free_no_alloc () =
  skip_unless_gc_counters ();
  let c =
    Codec.v "Page"
      (fun a b k -> (a, b, k))
      Codec.
        [
          (Field.v "a" uint8 $ fun (a, _, _) -> a);
          (Field.v "b" uint32be $ fun (_, b, _) -> b);
          (Field.v "k" (byte_slice ~size:(int 8)) $ fun (_, _, k) -> k);
        ]
  in
  let buf = Bytes.make 32 '\007' in
  decode_ok (Codec.validate c buf 0);
  (* warm up any one-time setup *)
  let n = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    decode_ok (Codec.validate c buf 0)
  done;
  let per_call = (Gc.minor_words () -. before) /. float_of_int n in
  Alcotest.(check bool)
    (Fmt.str "validate of a check-free codec allocates ~0 words (got %.2f)"
       per_call)
    true (per_call < 1.0)

(* Encoding a fixed-size record writes its bytes and nothing else, whatever the
   field types: a signed or a float field must not cost more than an unsigned
   one. *)
let test_encode_fixed_no_alloc () =
  skip_unless_gc_counters ();
  let c =
    Codec.v "Fixed"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (Field.v "a" uint8 $ fun (a, _, _, _) -> a);
          (Field.v "b" int16be $ fun (_, b, _, _) -> b);
          (Field.v "c" int32be $ fun (_, _, c, _) -> c);
          (Field.v "d" float64be $ fun (_, _, _, d) -> d);
        ]
  in
  let v = (UInt8.v 1, SInt16.v 2, SInt32.of_int32 3l, 4.0) in
  let buf = Bytes.create 15 in
  Codec.encode c v buf 0;
  let n = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    Codec.encode c v buf 0
  done;
  let per_call = (Gc.minor_words () -. before) /. float_of_int n in
  Alcotest.(check bool)
    (Fmt.str "encode of a fixed codec allocates ~0 words (got %.2f)" per_call)
    true (per_call < 1.0)

(* [decode] reads a fixed scalar field with no bounds check of its own, after
   checking the record's extent. Each such read must give what the
   bounds-checked [Codec.get] gives, sign included, on bytes with the top bit
   set. *)
let test_decode_scalars_agree_with_get () =
  let buf = Bytes.of_string "\x00\xf1\x82\x93\xa4\xb5" in
  let check : type a. string -> a typ -> unit =
   fun name typ ->
    let b = Codec.(Field.v "x" typ $ Fun.id) in
    let c =
      Codec.v name
        (fun _pad x -> x)
        Codec.[ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); b ]
    in
    let get = Staged.unstage (Codec.get c b) in
    Alcotest.(check bool)
      name true
      (decode_ok (Codec.decode c buf 0) = get buf 0)
  in
  check "uint8" uint8;
  check "uint16" uint16;
  check "uint16be" uint16be;
  check "uint32" uint32;
  check "uint32be" uint32be;
  check "int8" int8;
  check "int16" int16;
  check "int16be" int16be;
  check "int32" int32;
  check "int32be" int32be;
  List.iter
    (fun (name, base) ->
      let hi = Codec.(Field.v "hi" (bits ~width:3 base) $ fst) in
      let lo = Codec.(Field.v "lo" (bits ~width:5 base) $ snd) in
      let c =
        Codec.v name
          (fun _pad hi lo -> (hi, lo))
          Codec.[ (Field.v "pad" uint8 $ fun _ -> UInt8.v 0); hi; lo ]
      in
      let get f = Staged.unstage (Codec.get c f) buf 0 in
      Alcotest.(check (pair int int))
        name
        (get hi, get lo)
        (ok (Codec.decode c buf 0)))
    [ ("bits u8", U8); ("bits u16", U16); ("bits u16be", U16be) ]

(* The fixed fields are read with no check of their own, so the offset is
   checked first, in a way that cannot overflow. *)
let test_decode_offset_out_of_range () =
  let c = Codec.v "Two" Fun.id Codec.[ Field.v "x" uint16be $ Fun.id ] in
  let buf = Bytes.make 4 '\x00' in
  (match Codec.decode c buf (-1) with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "decode at -1: expected Invalid_argument");
  match Codec.decode c buf (max_int - 1) with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "decode at max_int - 1: expected an error"

(* A literal size is a byte count. A negative one used to shrink the span
   [decode] checks before its unchecked reads, so the fields in front of it were
   read past the end of the buffer. *)
let test_literal_size_is_a_byte_count () =
  let check name f = Alcotest.(check bool) name true (raises_invalid f) in
  let big = Sys.max_string_length + 1 in
  check "byte_array -4" (fun () -> byte_array ~size:(int (-4)));
  check "byte_slice -1" (fun () -> byte_slice ~size:(int (-1)));
  check "zeroterm_at_most -1" (fun () -> zeroterm_at_most ~size:(int (-1)));
  check "byte_array_where -1" (fun () ->
      byte_array_where ~size:(int (-1)) ~per_byte:(fun _ -> Expr.bool true));
  check "array -1" (fun () -> array ~len:(int (-1)) uint8);
  check "byte_array folded negative" (fun () ->
      byte_array ~size:Expr.(int 1 - int 2));
  check "byte_array too big" (fun () -> byte_array ~size:(int big))

(* The fixed part of a record has to fit in a buffer, or the frame check
   [decode] makes before its unchecked reads would wrap round. A size of 2^32
   bytes or more has no 3D type, so the codec is refused when built there; a
   smaller one past the largest buffer is refused on first use. *)
let test_record_too_big_for_a_buffer () =
  Alcotest.(check bool)
    "refused" true
    (raises_invalid (fun () ->
         let c =
           Codec.v "Huge"
             (fun _ _ -> ())
             Codec.
               [
                 ( Field.v "a" (byte_array ~size:(int Sys.max_string_length))
                 $ fun _ -> "" );
                 (Field.v "b" uint32 $ fun _ -> UInt32.of_int 0);
               ]
         in
         Codec.decode c (Bytes.create 8) 0))

(* An array's size is a byte count too: one that would overflow is refused. *)
let test_array_size_overflow () =
  Alcotest.(check bool)
    "array" true
    (raises_invalid (fun () -> array ~len:(int Sys.max_string_length) uint32))

(* A field whose constraints only compare it with literals is checked as one
   interval test. Its slot is still filled when a later constraint or the
   [where] clause reads it, and a failure reports the field and its value. *)
let test_constraint_intervals () =
  let describe (e : Error.t) =
    let value =
      match e.kind with Constraint_failed { value; _ } -> value | _ -> None
    in
    Fmt.str "%s@%d=%a"
      (String.concat "." e.field)
      e.at
      Fmt.(option ~none:(any "-") int64)
      value
  in
  let a =
    Field.v "a"
      ~self_constraint:(fun s -> Expr.(s >= int 5 && s <= int 10))
      uint8
  in
  let b =
    Field.v "b" ~self_constraint:(fun s -> Expr.(s > Field.ref a)) uint8
  in
  let c =
    Codec.v "Interval"
      ~where:Expr.(Field.ref a <> int 7)
      (fun a b -> (UInt8.to_int a, UInt8.to_int b))
      Codec.[ (a $ fun (x, _) -> UInt8.v x); (b $ fun (_, y) -> UInt8.v y) ]
  in
  let outcome x y =
    let buf = Bytes.create 2 in
    Bytes.set_uint8 buf 0 x;
    Bytes.set_uint8 buf 1 y;
    let validated =
      match Codec.validate c buf 0 with Ok () -> "ok" | Error e -> describe e
    in
    let decoded =
      match Codec.decode c buf 0 with Ok _ -> "ok" | Error e -> describe e
    in
    Alcotest.(check string) "decode agrees with validate" validated decoded;
    validated
  in
  let check name x y expected =
    Alcotest.(check string) name expected (outcome x y)
  in
  check "below" 4 9 "a@0=4";
  check "above" 11 12 "a@0=11";
  check "low bound" 5 6 "ok";
  check "high bound" 10 11 "ok";
  check "later field reads it" 6 5 "b@1=5";
  check "where reads it" 7 9 "@0=-";
  let never =
    Codec.v "Never" Fun.id
      Codec.
        [
          Field.v "n" ~self_constraint:(fun s -> Expr.(s > int max_int)) uint8
          $ Fun.id;
        ]
  in
  Alcotest.(check bool)
    "an empty interval refuses everything" true
    (match Codec.validate never (Bytes.make 1 '\xff') 0 with
    | Ok () -> false
    | Error _ -> true)

(* A failure reports the frame's own value, never one a previous decode left in
   the scratch: a 64-bit value an int cannot hold leaves the int slot unset. *)
let test_failure_value_is_this_frames () =
  let codec () =
    Codec.v "Wide" Fun.id
      Codec.
        [
          Field.v "v" ~self_int64:(fun s -> Expr.(s < int64 1000L)) uint64be
          $ Fun.id;
        ]
  in
  let frame n =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 n;
    b
  in
  let failure c b =
    match Codec.decode c b 0 with
    | Ok _ -> Alcotest.fail "expected a constraint failure"
    | Error e -> Fmt.str "%a" Error.pp e
  in
  let fresh = failure (codec ()) (frame (-1L)) in
  let c = codec () in
  ignore (failure c (frame 1001L));
  Alcotest.(check string) "after another frame" fresh (failure c (frame (-1L)))

(* A constraint over a wide field sees the value EverParse sees: the field's
   whole bit pattern as an unsigned number, never a native int that dropped its
   top bits or read zero because the value did not fit. Each case is a value
   whose truncation lands on the other side of the constraint. *)
let test_wide_constraint () =
  let frame n =
    let b = Bytes.create 8 in
    Bytes.set_int64_be b 0 n;
    b
  in
  let outcomes (type r) (c : r Codec.t) b =
    let ok = function Ok _ -> true | Error _ -> false in
    let module V = (val Codec.View.codec c) in
    let (Buf.Any v) = Buf.v b in
    ( ok (Codec.decode c b 0),
      ok (Codec.validate c b 0),
      Codec.View.(ok (check V.codec v (Buf.length v) 0)) )
  in
  let check name c n expected =
    Alcotest.(check (triple bool bool bool))
      name
      (expected, expected, expected)
      (outcomes c (frame n))
  in
  let one typ f =
    Codec.v "Wide" Fun.id Codec.[ Field.v "v" ~self_constraint:f typ $ Fun.id ]
  in
  let le5 = one uint64be Expr.(fun s -> s <= int 5) in
  check "uint64 2^63+3 <= 5" le5 0x8000_0000_0000_0003L false;
  check "uint64 2^62+3 <= 5" le5 0x4000_0000_0000_0003L false;
  check "uint64 3 <= 5" le5 3L true;
  let ge5 = one uint64be Expr.(fun s -> s >= int 5) in
  check "uint64 2^63 >= 5" ge5 Int64.min_int true;
  let low = one uint64be Expr.(fun s -> s land int 0xff = int 3) in
  check "uint64 low byte of 2^63+3" low 0x8000_0000_0000_0003L true;
  let top = one uint64be Expr.(fun s -> s lsr int 60 = int 11) in
  check "uint64 top nibble" top 0xB000_0000_0000_0000L true;
  let eq5 = one int64be Expr.(fun s -> s = int 5) in
  check "int64 2^63+5 = 5" eq5 0x8000_0000_0000_0005L false;
  check "int64 5 = 5" eq5 5L true;
  let ne5 = one int64be Expr.(fun s -> s <> int 5) in
  check "int64 2^63+5 <> 5" ne5 0x8000_0000_0000_0005L true;
  let bits5 = one float64be Expr.(fun s -> s = int 5) in
  check "float64 bits 2^63+5 = 5" bits5 0x8000_0000_0000_0005L false;
  let wide = Field.v "v" uint64be in
  let cross =
    Codec.v "Cross"
      (fun v w -> (v, w))
      Codec.
        [
          wide $ fst;
          Field.v "w"
            ~self_constraint:(fun s -> Expr.(Field.ref wide >= s))
            uint8
          $ snd;
        ]
  in
  let b = Bytes.make 9 '\007' in
  Bytes.set_int64_be b 0 Int64.min_int;
  Alcotest.(check (triple bool bool bool))
    "cross: uint64 2^63 >= 7" (true, true, true) (outcomes cross b);
  let where =
    Codec.v "Where"
      ~where:Expr.(Field.ref wide >= int 5)
      Fun.id
      Codec.[ wide $ Fun.id ]
  in
  check "where: uint64 2^63 >= 5" where Int64.min_int true

(* A presence condition over a wide field reads it at full width too, as the
   generated C does: with [n >= 5] the byte after [n] is present for every
   large [n], so the record needs nine bytes and nine bytes are enough. *)
let test_wide_presence () =
  let n = Field.v "n" uint64be in
  let c =
    Codec.v "WidePresent"
      (fun n d -> (n, d))
      Codec.
        [
          n $ fst;
          Field.optional "d" ~present:Expr.(Field.ref n >= int 5) uint8 $ snd;
        ]
  in
  let frame len v =
    let b = Bytes.make len '\xaa' in
    Bytes.set_int64_be b 0 v;
    b
  in
  let module V = (val Codec.View.codec c) in
  let outcomes b =
    let ok = function Ok _ -> true | Error _ -> false in
    let (Buf.Any v) = Buf.v b in
    ( ok (Codec.decode c b 0),
      ok (Codec.validate c b 0),
      Codec.View.(ok (check V.codec v (Buf.length v) 0)) )
  in
  let check name v ~short ~full =
    Alcotest.(check (triple bool bool bool))
      (name ^ ", 8 bytes") (short, short, short)
      (outcomes (frame 8 v));
    Alcotest.(check (triple bool bool bool))
      (name ^ ", 9 bytes") (full, full, full)
      (outcomes (frame 9 v))
  in
  check "4" 4L ~short:true ~full:true;
  check "5" 5L ~short:false ~full:true;
  check "2^62 - 1" 0x3FFF_FFFF_FFFF_FFFFL ~short:false ~full:true;
  check "2^62" 0x4000_0000_0000_0000L ~short:false ~full:true;
  check "2^63" Int64.min_int ~short:false ~full:true;
  check "2^64 - 1" (-1L) ~short:false ~full:true;
  match Codec.decode c (frame 9 Int64.min_int) 0 with
  | Ok (_, Some d) -> Alcotest.(check int) "2^63: d" 0xaa (UInt8.to_int d)
  | Ok (_, None) -> Alcotest.fail "2^63: d absent"
  | Error e -> Alcotest.failf "2^63: %a" Error.pp e

(* A fixed codec of plain fields encodes through one frame check and one write
   per field. Bitfield words start from zero whatever the buffer held, a value
   too wide for its bits and a buffer too short are refused, and a codec with
   constraints refuses to write a value that breaks them. *)
let test_plain_encode () =
  let hi = Field.v "hi" (bits ~width:3 U8)
  and lo = Field.v "lo" (bits ~width:5 U8) in
  let f = Field.v "f" (bits ~width:4 U16be)
  and g = Field.v "g" (bits ~width:12 U16be) in
  let n = Field.v "n" ~self_constraint:(fun s -> Expr.(s <= int 1000)) uint16 in
  let x = Field.v "x" int32be in
  let c =
    Codec.v "Plain"
      (fun hi lo f g n x -> (hi, lo, f, g, UInt16.to_int n, SInt32.to_int32 x))
      Codec.
        [
          (hi $ fun (v, _, _, _, _, _) -> v);
          (lo $ fun (_, v, _, _, _, _) -> v);
          (f $ fun (_, _, v, _, _, _) -> v);
          (g $ fun (_, _, _, v, _, _) -> v);
          (n $ fun (_, _, _, _, v, _) -> UInt16.v v);
          (x $ fun (_, _, _, _, _, v) -> SInt32.of_int32 v);
        ]
  in
  let v = (5, 17, 9, 0xabc, 513, -2l) in
  let buf = Bytes.make 9 '\xff' in
  Codec.encode c v buf 0;
  Alcotest.(check string)
    "bytes" "\xb1\x9a\xbc\x01\x02\xff\xff\xff\xfe" (Bytes.to_string buf);
  Alcotest.(check bool) "roundtrip" true (decode_ok (Codec.decode c buf 0) = v);
  let refused name f =
    Alcotest.(check bool)
      name true
      (match f () with () -> false | exception Invalid_argument _ -> true)
  in
  refused "bits too wide" (fun () -> Codec.encode c (8, 0, 0, 0, 0, 0l) buf 0);
  refused "negative offset" (fun () -> Codec.encode c v buf (-1));
  refused "short buffer" (fun () -> Codec.encode c v buf 1);
  refused "constraint" (fun () -> Codec.encode c (0, 0, 0, 0, 1001, 0l) buf 0)

(* A record [View.check] accepts reads every field as [get] does.
   [View.check] refuses a short buffer and a negative offset without raising,
   [View.get] raises on what it refuses, and [View.field] refuses a field of
   the same name and type that the codec was not built from. *)
let test_view_read () =
  let a = Codec.(Field.v "a" uint8 $ fun (a, _, _) -> a) in
  let b = Codec.(Field.v "b" (bits ~width:4 U16be) $ fun (_, b, _) -> b) in
  let s = Codec.(Field.v "s" (byte_array ~size:(int 3)) $ fun (_, _, s) -> s) in
  let c = Codec.v "Valid" (fun a b s -> (a, b, s)) [ a; b; s ] in
  let module V = (val Codec.View.codec c) in
  let buf = Bytes.of_string "\x07\xa0\x00xyz" in
  let check buf off =
    let (Buf.Any b) = Buf.v buf in
    Codec.View.(ok (check V.codec b (Buf.length b) off))
  in
  let read f =
    let (Buf.Any b) = Buf.v buf in
    let k = Codec.View.check V.codec b (Buf.length b) 0 in
    Codec.View.read (Codec.View.field V.codec f) b (Codec.View.get k)
  in
  let get f = Staged.unstage (Codec.get c f) buf 0 in
  Alcotest.(check bool) "accepted" true (check buf 0);
  Alcotest.(check bool) "uint8" true (read a = get a);
  Alcotest.(check int) "bits" (get b) (read b);
  Alcotest.(check string) "byte_array" (get s) (read s);
  let raises f =
    match f () with _ -> false | exception Invalid_argument _ -> true
  in
  let short = Bytes.make 3 '\x00' in
  Alcotest.(check bool) "short buffer" false (check short 0);
  Alcotest.(check bool) "negative offset" false (check buf (-1));
  Alcotest.(check bool) "offset past the end" false (check buf 7);
  Alcotest.(check bool)
    "get of a failed check" true
    (raises (fun () ->
         let (Buf.Any b) = Buf.v short in
         ignore Codec.View.(get (check V.codec b (Buf.length b) 0))));
  let lookalike = Codec.(Field.v "a" uint8 $ fun (a, _, _) -> a) in
  Alcotest.(check bool)
    "a lookalike field" true
    (raises (fun () -> Codec.View.field V.codec lookalike))

(* In a frame of 4 to 7 bytes, the 4-byte word a view reads a field from
   ends at the frame's end at most: every field of a 4-byte word of bitfields
   and of a 6-byte header reads as [get] does, with the record at the very end
   of the buffer. *)
let test_view_short_frames () =
  let check_codec (type r) name (c : r Codec.t) fields =
    let module V = (val Codec.View.codec c) in
    let size = Codec.wire_size c in
    for i = 0 to 63 do
      let buf =
        Bytes.init (size + 3) (fun j ->
            Char.chr (((i * 37) + (j * 91)) land 0xff))
      in
      let off = 3 in
      let (Buf.Any b) = Buf.v buf in
      let k = Codec.View.check V.codec b (Buf.length b) off in
      if Codec.View.ok k then
        let r = Codec.View.get k in
        List.iter
          (fun f ->
            Alcotest.(check int)
              (Fmt.str "%s %d" name i)
              (Staged.unstage (Codec.get c f) buf off)
              (Codec.View.read (Codec.View.field V.codec f) b r))
          fields
    done
  in
  let bit w =
    Codec.(Field.v (Fmt.str "b%d" w) (bits ~width:w U32be) $ fun _ -> 0)
  in
  let b1 = bit 1
  and b3 = bit 3
  and b5 = bit 5
  and b7 = bit 7
  and b16 = bit 16 in
  check_codec "U32be bitfields"
    (Codec.v "Word" (fun _ _ _ _ _ -> ()) Codec.[ b1; b3; b5; b7; b16 ])
    [ b1; b3; b5; b7; b16 ];
  let hi = Codec.(Field.v "hi" (bits ~width:5 U16be) $ fun _ -> 0) in
  let lo = Codec.(Field.v "lo" (bits ~width:11 U16be) $ fun _ -> 0) in
  let mid = Codec.(Field.v "mid" uint16be $ fun _ -> UInt16.v 0) in
  let last = Codec.(Field.v "last" uint8 $ fun _ -> UInt8.v 0) in
  let pad = Codec.(Field.v "pad" uint8 $ fun _ -> UInt8.v 0) in
  let c6 =
    Codec.v "Six" (fun _ _ _ _ _ -> ()) Codec.[ hi; lo; mid; pad; last ]
  in
  check_codec "6-byte header bits" c6 [ hi; lo ];
  let module V = (val Codec.View.codec c6) in
  let buf = Bytes.of_string "\xff\xff\x12\x34\x00\xab" in
  let (Buf.Any b) = Buf.v buf in
  let r = Codec.View.(get (check V.codec b (Buf.length b) 0)) in
  Alcotest.(check int)
    "mid" 0x1234
    (UInt16.to_int Codec.View.(read (field V.codec mid) b r));
  Alcotest.(check int)
    "last" 0xab
    (UInt8.to_int Codec.View.(read (field V.codec last) b r))

(* Constraints on fields that share a byte are checked together, and a failure
   still names the first field, in declaration order, whose constraint fails. *)
let test_shared_byte_constraints () =
  let version =
    Field.v "version"
      ~self_constraint:(fun s -> Expr.(s = int 4))
      (bits ~width:4 U8)
  in
  let ihl =
    Field.v "ihl"
      ~self_constraint:(fun s -> Expr.(s >= int 5))
      (bits ~width:4 U8)
  in
  let c =
    Codec.v "VersionIhl" (fun v i -> (v, i)) Codec.[ version $ fst; ihl $ snd ]
  in
  let outcome byte =
    let buf = Bytes.make 1 (Char.chr byte) in
    let validated =
      match Codec.validate c buf 0 with
      | Ok () -> "ok"
      | Error e -> String.concat "." e.field
    in
    let decoded =
      match Codec.decode c buf 0 with
      | Ok _ -> "ok"
      | Error e -> String.concat "." e.field
    in
    Alcotest.(check string) "decode agrees" validated decoded;
    validated
  in
  Alcotest.(check string) "0x45" "ok" (outcome 0x45);
  Alcotest.(check string) "0x4f" "ok" (outcome 0x4f);
  Alcotest.(check string) "0x44" "ihl" (outcome 0x44);
  Alcotest.(check string) "0x55" "version" (outcome 0x55);
  Alcotest.(check string) "0x30" "version" (outcome 0x30);
  (* Two bytes, each with its own table. *)
  let two =
    Codec.v "TwoBytes"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (version $ fun (a, _, _, _) -> a);
          (ihl $ fun (_, b, _, _) -> b);
          ( Field.v "hi"
              ~self_constraint:(fun s -> Expr.(s <= int 3))
              (bits ~width:4 U8)
          $ fun (_, _, c, _) -> c );
          ( Field.v "lo"
              ~self_constraint:(fun s -> Expr.(s >= int 1))
              (bits ~width:4 U8)
          $ fun (_, _, _, d) -> d );
        ]
  in
  let two_outcome b0 b1 =
    match
      Codec.validate two (Bytes.init 2 (function 0 -> b0 | _ -> b1)) 0
    with
    | Ok () -> "ok"
    | Error e -> String.concat "." e.field
  in
  Alcotest.(check string) "two tables" "ok" (two_outcome '\x45' '\x31');
  Alcotest.(check string) "second table" "hi" (two_outcome '\x45' '\x41');
  Alcotest.(check string) "first table" "ihl" (two_outcome '\x44' '\x31')

(* Tests that read one 8-byte word are merged into one test of it: exact
   values anywhere in the word, and a range only below them. On every input,
   the merged tests answer as the scan of the tests they stand for, and the
   check of the codec, fixed or walked, as the general path. Kept apart: a
   range above an exact value, a signed field and a test past 62 bits. *)
let test_word_merged_tests () =
  let f name typ ok = Field.v name typ ~self_constraint:ok in
  let fields =
    Codec.
      [
        (f "version" (bits ~width:4 U8) (fun x -> Expr.(x = int 4)) $ fun _ -> 4);
        (f "ihl" (bits ~width:4 U8) (fun x -> Expr.(x >= int 5)) $ fun _ -> 5);
        (Field.v "free" uint8 $ fun _ -> UInt8.v 0);
        (f "fl" (bits ~width:3 U16be) (fun x -> Expr.(x = int 2)) $ fun _ -> 0);
        ( f "frag" (bits ~width:13 U16be) (fun x -> Expr.(x = int 0)) $ fun _ ->
          0 );
        (f "proto" uint8 (fun x -> Expr.(x = int 6)) $ fun _ -> UInt8.v 6);
        ( f "low" uint8 (fun x -> Expr.(x >= int 10 && x <= int 20)) $ fun _ ->
          UInt8.v 10 );
        (f "signed" int8 (fun x -> Expr.(x >= int (-3))) $ fun _ -> SInt8.v 0);
        (f "hi" (bits ~width:4 U8) (fun x -> Expr.(x >= int 2)) $ fun _ -> 2);
        (f "lo" (bits ~width:4 U8) (fun x -> Expr.(x = int 3)) $ fun _ -> 3);
        ( f "word" uint32be (fun x -> Expr.(x = int 0x3eadbeef)) $ fun _ ->
          UInt32.of_int 0x3eadbeef );
      ]
  in
  let c = Codec.v "MergedWords" (fun _ _ _ _ _ _ _ _ _ _ _ -> ()) fields in
  let n = Field.v "n" uint8 in
  let walked =
    Codec.v "MergedWalk"
      (fun _ () _ -> ())
      Codec.
        [
          (n $ fun _ -> UInt8.v 0);
          (Field.v "body" (codec c) $ fun _ -> ());
          ( Field.v "tail" (byte_slice ~size:(Field.ref n)) $ fun _ ->
            Bytesrw.Bytes.Slice.eod );
        ]
  in
  let general = Private.Testing.general c in
  let general_walked = Private.Testing.general walked in
  let module V = (val Codec.View.codec c) in
  let module W = (val Codec.View.codec walked) in
  let good = "\x45\x00\x40\x00\x06\x0a\x00\x23\x3e\xad\xbe\xef" in
  let rnd = Random.State.make [| 7 |] in
  let input () =
    Bytes.init 12 (fun i ->
        match Random.State.int rnd 4 with
        | 0 -> Char.chr (Random.State.int rnd 256)
        | 1 -> Char.chr (Char.code good.[i] lxor (1 lsl Random.State.int rnd 8))
        | _ -> good.[i])
  in
  let accepts c buf =
    match Codec.validate c buf 0 with Ok () -> true | Error _ -> false
  in
  Alcotest.(check bool) "good" true (accepts general (Bytes.of_string good));
  for i = 1 to 20_000 do
    let buf = input () in
    (match Private.Testing.staged_intervals c buf 0 with
    | Some (staged, scan) ->
        if staged <> scan then Alcotest.failf "input %d: staged %b" i staged
    | None ->
        (* Only a target whose ints hold 63 bits stages word tests. *)
        if Sys.int_size >= 63 then Alcotest.fail "the tests are not staged");
    let (Buf.Any b) = Buf.v buf in
    let viewed = Codec.View.(ok (check V.codec b (Buf.length b) 0)) in
    if viewed <> accepts general buf then
      Alcotest.failf "input %d: view %b" i viewed;
    let tail = Random.State.int rnd 3 in
    let buf =
      Bytes.cat (Bytes.make 1 (Char.chr (Random.State.int rnd 3))) buf
    in
    let buf = Bytes.cat buf (Bytes.make tail 'x') in
    let (Buf.Any b) = Buf.v buf in
    let viewed = Codec.View.(ok (check W.codec b (Buf.length b) 0)) in
    if viewed <> accepts general_walked buf then
      Alcotest.failf "input %d: walked view %b" i viewed
  done;
  (* A constraint on a 32-bit field keeps the record off the walk where ints
     have fewer than 63 bits. *)
  if Sys.int_size >= 63 then
    Alcotest.(check bool)
      "walks" true
      (List.mem "decode.walk" (Private.Testing.paths walked))

(* The checks on a field under [map] and [enum], and a disjunction, still give
   the errors they gave, whatever path the check pass takes. *)
let test_mapped_and_disjunctive_checks () =
  let u16 = map ~decode:UInt16.to_int ~encode:UInt16.v uint16be in
  let start =
    Field.v "start"
      ~self_constraint:(fun s -> Expr.(s = int 0 || s >= int 8))
      u16
  in
  let kind =
    Field.v "kind" (enum "Kind" [ ("A", 2); ("B", 5); ("C", 13) ] uint8)
  in
  let hi =
    Field.v "hi"
      ~self_constraint:(fun s -> Expr.(s <= int 3))
      (bits ~width:4 U8)
  in
  let lo =
    Field.v "lo"
      ~self_constraint:(fun s -> Expr.(s >= int 1))
      (bits ~width:4 U8)
  in
  let c =
    Codec.v "Mapped"
      (fun s k h l -> (s, k, h, l))
      Codec.
        [
          (start $ fun (s, _, _, _) -> s);
          (kind $ fun (_, k, _, _) -> k);
          (hi $ fun (_, _, h, _) -> h);
          (lo $ fun (_, _, _, l) -> l);
        ]
  in
  let describe (e : Error.t) =
    let kind =
      match e.kind with
      | Constraint_failed _ -> "constraint"
      | Invalid_enum _ -> "enum"
      | _ -> "other"
    in
    Fmt.str "%s:%s" (String.concat "." e.field) kind
  in
  let outcome s k b =
    let buf = Bytes.create 4 in
    Bytes.set_uint16_be buf 0 s;
    Bytes.set_uint8 buf 2 k;
    Bytes.set_uint8 buf 3 b;
    let validated =
      match Codec.validate c buf 0 with Ok () -> "ok" | Error e -> describe e
    in
    let decoded =
      match Codec.decode c buf 0 with Ok _ -> "ok" | Error e -> describe e
    in
    Alcotest.(check string) "decode agrees" validated decoded;
    validated
  in
  let check name expected s k b =
    Alcotest.(check string) name expected (outcome s k b)
  in
  check "start 0" "ok" 0 2 0x31;
  check "start 8" "ok" 8 5 0x31;
  check "start 100" "ok" 100 13 0x31;
  check "start 1" "start:constraint" 1 2 0x31;
  check "start 7" "start:constraint" 7 2 0x31;
  check "kind 3" "kind:enum" 8 3 0x31;
  check "hi 4" "hi:constraint" 8 2 0x41;
  check "lo 0" "lo:constraint" 8 2 0x30

(* A record of every fixed scalar kind, big enough for the staged reads and
   writes, decodes each field as [get] reads it and encodes back the bytes it
   came from, on pseudo-random buffers. *)
let staged_scalars ~d_typ ~f_typ =
  let a = Codec.(Field.v "a" uint8 $ fun (a, _, _, _, _, _, _, _, _, _) -> a) in
  let b = Codec.(Field.v "b" int8 $ fun (_, b, _, _, _, _, _, _, _, _) -> b) in
  let c =
    Codec.(Field.v "c" uint16be $ fun (_, _, c, _, _, _, _, _, _, _) -> c)
  in
  let d = Codec.(Field.v "d" d_typ $ fun (_, _, _, d, _, _, _, _, _, _) -> d) in
  let e =
    Codec.(Field.v "e" int32be $ fun (_, _, _, _, e, _, _, _, _, _) -> e)
  in
  let f = Codec.(Field.v "f" f_typ $ fun (_, _, _, _, _, f, _, _, _, _) -> f) in
  let g =
    Codec.(
      Field.v "g" (bits ~width:3 U8) $ fun (_, _, _, _, _, _, g, _, _, _) -> g)
  in
  let h =
    Codec.(
      Field.v "h" (bits ~width:5 U8) $ fun (_, _, _, _, _, _, _, h, _, _) -> h)
  in
  let i =
    Codec.(
      Field.v "i" (bits ~width:4 U16be) $ fun (_, _, _, _, _, _, _, _, i, _) ->
      i)
  in
  let j =
    Codec.(
      Field.v "j" (bits ~width:12 U16be) $ fun (_, _, _, _, _, _, _, _, _, j) ->
      j)
  in
  let codec =
    Codec.v "Staged"
      (fun a b c d e f g h i j -> (a, b, c, d, e, f, g, h, i, j))
      [ a; b; c; d; e; f; g; h; i; j ]
  in
  let rand = Random.State.make [| 42 |] in
  for _ = 1 to 2000 do
    let buf = Bytes.init 15 (fun _ -> Char.chr (Random.State.int rand 256)) in
    let v = decode_ok (Codec.decode codec buf 0) in
    let get x = Staged.unstage (Codec.get codec x) buf 0 in
    let a', b', c', d', e', f', g', h', i', j' = v in
    Alcotest.(check bool) "a" true (a' = get a);
    Alcotest.(check bool) "b" true (b' = get b);
    Alcotest.(check bool) "c" true (c' = get c);
    Alcotest.(check bool) "d" true (d' = get d);
    Alcotest.(check bool) "e" true (SInt32.equal e' (get e));
    Alcotest.(check bool) "f" true (f' = get f);
    Alcotest.(check int) "g" (get g) g';
    Alcotest.(check int) "h" (get h) h';
    Alcotest.(check int) "i" (get i) i';
    Alcotest.(check int) "j" (get j) j';
    let out = Bytes.make 15 '\xff' in
    Codec.encode codec v out 0;
    Alcotest.(check string)
      "encode"
      (Bytes.sub_string buf 0 15)
      (Bytes.sub_string out 0 15)
  done

(* Little-endian fields take the reader closures, big-endian ones the reads in
   place. *)
let test_staged_scalars () =
  staged_scalars ~d_typ:int16 ~f_typ:uint16;
  staged_scalars ~d_typ:int16be ~f_typ:uint16be

(* Fixed byte arrays read as spans of the frame: each decodes as [get] reads it
   and encodes back the bytes it came from. *)
let test_staged_spans () =
  let a = Codec.(Field.v "a" (byte_array ~size:(int 6)) $ fun (a, _, _) -> a) in
  let t = Codec.(Field.v "t" uint16be $ fun (_, t, _) -> t) in
  let b = Codec.(Field.v "b" (byte_array ~size:(int 3)) $ fun (_, _, b) -> b) in
  let codec = Codec.v "Spans" (fun a t b -> (a, t, b)) [ a; t; b ] in
  let rand = Random.State.make [| 7 |] in
  for _ = 1 to 500 do
    let buf = Bytes.init 11 (fun _ -> Char.chr (Random.State.int rand 256)) in
    let ((a', _, b') as v) = decode_ok (Codec.decode codec buf 0) in
    let get x = Staged.unstage (Codec.get codec x) buf 0 in
    Alcotest.(check string) "a" (get a) a';
    Alcotest.(check string) "b" (get b) b';
    let out = Bytes.make 11 '\x00' in
    Codec.encode codec v out 0;
    Alcotest.(check string) "encode" (Bytes.to_string buf) (Bytes.to_string out)
  done

(* A variable-size record of plain spans sized by its header gives the same
   answer, error included, as the same record forced onto the general path by
   an input parameter, on pseudo-random headers that fit, overrun or go
   negative. *)
let test_layout_pass () =
  let page ~params =
    let n = Field.v "n" uint8 in
    let m = Field.v "m" uint8 in
    let p = Param.input "p" uint8 in
    let where = if params then Some Expr.(Param.expr p >= int 0) else None in
    let c =
      Codec.v "Page" ?where
        (fun n m a b rest -> (n, m, a, b, rest))
        Codec.
          [
            (n $ fun (n, _, _, _, _) -> n);
            (m $ fun (_, m, _, _, _) -> m);
            ( Field.v "a" (byte_array ~size:Expr.(Field.ref n * int 2))
            $ fun (_, _, a, _, _) -> a );
            ( Field.v "b" (byte_slice ~size:Expr.(Field.ref m - int 4))
            $ fun (_, _, _, b, _) -> b );
            (Field.v "rest" all_bytes $ fun (_, _, _, _, r) -> r);
          ]
    in
    let env =
      if params then Some (Param.bind p (UInt8.v 0) (Codec.env c)) else None
    in
    (c, env)
  in
  let fast, _ = page ~params:false and slow, env = page ~params:true in
  let outcome c env buf =
    let v =
      match Codec.validate ?env c buf 0 with
      | Ok () -> "ok"
      | Error e -> Fmt.str "%a" Error.pp e
    in
    let d =
      match Codec.decode ?env c buf 0 with
      | Ok (_, _, a, _, rest) -> Fmt.str "ok %S %S" a rest
      | Error e -> Fmt.str "%a" Error.pp e
    in
    v ^ " / " ^ d
  in
  let rand = Random.State.make [| 3 |] in
  for _ = 1 to 2000 do
    let len = Random.State.int rand 40 in
    let buf = Bytes.init len (fun _ -> Char.chr (Random.State.int rand 256)) in
    if len >= 2 then begin
      Bytes.set_uint8 buf 0 (Random.State.int rand 12);
      Bytes.set_uint8 buf 1 (Random.State.int rand 16)
    end;
    Alcotest.(check string)
      "same answer" (outcome slow env buf) (outcome fast None buf)
  done

(* A span sized by a field times a literal past the buffer: [decode] refuses
   the record, and [validate] must agree. Where the product can pass the
   native int (31 and 32-bit ints), the codec is refused when it is built. *)
let test_layout_size_overflow () =
  let n = Field.v "n" uint16be in
  match
    Codec.v "Scaled"
      (fun n body -> (n, body))
      Codec.
        [
          (n $ fun (n, _) -> n);
          ( Field.v "body" (byte_array ~size:Expr.(Field.ref n * int 32769))
          $ fun (_, body) -> body );
        ]
  with
  | exception Invalid_argument _ when Sys.int_size < 63 -> ()
  | c ->
      let buf = Bytes.make (2 + 32767) '\x00' in
      Bytes.set_uint16_be buf 0 0xffff;
      let decoded = Result.is_ok (Codec.decode c buf 0) in
      let validated =
        match Codec.validate c buf 0 with Ok () -> true | Error _ -> false
      in
      Alcotest.(check bool) "decode refuses" false decoded;
      Alcotest.(check bool) "validate agrees with decode" decoded validated;
      Alcotest.(check bool)
        "no proof for a record decode refuses" false
        (let module V = (val Codec.View.codec c) in
         let (Buf.Any b) = Buf.v buf in
         Codec.View.(ok (check V.codec b (Buf.length b) 0)))

(* A negative offset is refused by name on every encode path, the general one
   included: a codec with a [where] clause does not take the plain one. *)
let test_encode_negative_offset () =
  let n = Field.v "n" uint8 in
  let c =
    Codec.v "Neg" ~where:Expr.(Field.ref n > int 0) Fun.id Codec.[ n $ Fun.id ]
  in
  match Codec.encode c (UInt8.v 1) (Bytes.create 4) (-1) with
  | () -> Alcotest.fail "encoded at a negative offset"
  | exception Invalid_argument m ->
      Alcotest.(check string) "message" "Codec.encode Neg: negative offset -1" m

(* A fixed record of byte arrays, a bitfield and an int writes the same bytes,
   or raises the same error, plain as on the general path an input parameter
   forces, whatever the lengths of the arrays and the width of the bitfield
   value. *)
let test_plain_encode_blits () =
  let enc ~params =
    let p = Param.input "p" uint8 in
    let where = if params then Some Expr.(Param.expr p >= int 0) else None in
    let c =
      Codec.v "Blits" ?where
        (fun a f b t -> (a, f, b, t))
        Codec.
          [
            (Field.v "a" (byte_array ~size:(int 3)) $ fun (a, _, _, _) -> a);
            (Field.v "f" (bits ~width:4 U8) $ fun (_, f, _, _) -> f);
            (Field.v "b" (byte_array ~size:(int 2)) $ fun (_, _, b, _) -> b);
            (Field.v "t" uint16be $ fun (_, _, _, t) -> t);
          ]
    in
    let env =
      if params then Some (Param.bind p (UInt8.v 0) (Codec.env c)) else None
    in
    fun v ->
      let buf = Bytes.make 8 '\xff' in
      match Codec.encode ?env c v buf 0 with
      | () -> Bytes.to_string buf
      | exception Invalid_argument m -> m
  in
  let plain = enc ~params:false and slow = enc ~params:true in
  let rand = Random.State.make [| 5 |] in
  let str n = String.init n (fun i -> Char.chr (65 + i)) in
  for _ = 1 to 500 do
    let la = 2 + Random.State.int rand 3 and lb = 1 + Random.State.int rand 3 in
    let f = Random.State.int rand 20 and t = Random.State.int rand 0x10000 in
    let v = (str la, f, str lb, UInt16.v t) in
    Alcotest.(check string) "same bytes or error" (slow v) (plain v)
  done

(* A gated optional sits at a fixed offset but has no fixed size, so the
   layout pass cannot count it as a constant: with the gate on and one byte
   left, validate refuses the record as decode does. *)
let test_layout_gated_optional () =
  let gate = Field.v "gate" uint8 in
  let c =
    Codec.v "Gated"
      (fun g p rest -> (g, p, rest))
      Codec.
        [
          (gate $ fun (g, _, _) -> g);
          ( Field.optional "payload"
              ~present:Expr.(Field.ref gate <> int 0)
              uint16be
          $ fun (_, p, _) -> p );
          (Field.v "rest" all_bytes $ fun (_, _, r) -> r);
        ]
  in
  let buf = Bytes.of_string "\x01\x00" in
  (match Codec.decode c buf 0 with
  | Ok _ -> Alcotest.fail "decode accepted a missing payload"
  | Error _ -> ());
  match Codec.validate c buf 0 with
  | Ok () -> Alcotest.fail "validate accepted a missing payload"
  | Error _ -> ()

(* Fields are read in declaration order, so the first field whose decoding
   fails is the one reported. Nine fields used to be read last to first. *)
let test_decode_reads_in_order () =
  let seen = ref [] in
  let f i =
    let typ =
      map
        ~decode:(fun v ->
          seen := i :: !seen;
          v)
        ~encode:Fun.id uint8
    in
    Codec.(Field.v (Fmt.str "f%d" i) typ $ fun _ -> UInt8.v 0)
  in
  let c =
    Codec.v "Nine"
      (fun _ _ _ _ _ _ _ _ _ -> ())
      Codec.[ f 1; f 2; f 3; f 4; f 5; f 6; f 7; f 8; f 9 ]
  in
  decode_ok (Codec.decode c (Bytes.make 9 '\x00') 0);
  Alcotest.(check (list int))
    "read order"
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]
    (List.rev !seen)

(* A compiled validator keeps a per-domain scratch, and the cost of reaching it
   on a domain that has not decoded yet must not track how many validators the
   program ever built. It used to: each validator minted its own
   [Domain.DLS.new_key], and a domain sizes its DLS array from the global key
   counter, so the first validate on a fresh domain paid about one word per key
   ever created. Measured here at 10,000 validators it was 32,792 words, and at
   50,000 it was 131,096, about 1 MB, charged to domains that never decode.

   [Gc.minor_words] cannot see this. The array is large enough to be allocated
   straight into the major heap, so read [Gc.allocated_bytes], which counts
   both. *)
let test_fresh_domain_validate_flat_in_validator_count () =
  skip_unless_gc_counters ();
  let f_x = Field.v "x" uint8 in
  let mk name =
    Codec.v name
      ~where:Expr.(Field.ref f_x <= int 200)
      (fun x -> x)
      Codec.[ (Field.v "x" uint8 $ fun x -> x) ]
  in
  (* [wire_size_opt] forces the compile, which builds the validator. *)
  for i = 1 to 10_000 do
    ignore (Sys.opaque_identity (Codec.wire_size_opt (Fmt.kstr mk "S%d" i)))
  done;
  let c = mk "Last" in
  let buf = Bytes.make 8 '\007' in
  let words =
    Domain.join
      (Domain.spawn (fun () ->
           let before = Gc.allocated_bytes () in
           decode_ok (Codec.validate c buf 0);
           (Gc.allocated_bytes () -. before) /. 8.))
  in
  Fmt.kstr
    (fun msg -> Alcotest.(check bool) msg true (words < 4096.))
    "a fresh domain's first validate does not track the validator count (got \
     %.0f words)"
    words

(* [Codec.v] only checks the description. The first operation on the codec
   compiles it, so a codec that is built but never used costs little. Here the
   first decode pays for the compile, so it allocates more than [v]. *)
module Deferred = struct
  type t = {
    version : int;
    kind : int;
    flags : int;
    len : UInt8.t;
    seq : UInt16.t;
    body : string;
  }

  let codec () =
    let f_len = Field.v "len" uint8 in
    Codec.v "Deferred"
      ~where:Expr.(Field.ref f_len <= int 64)
      (fun version kind flags len seq body ->
        { version; kind; flags; len; seq; body })
      Codec.
        [
          (Field.v "version" (bits ~width:3 U8) $ fun r -> r.version);
          (Field.v "kind" (bits ~width:1 U8) $ fun r -> r.kind);
          (Field.v "flags" (bits ~width:4 U8) $ fun r -> r.flags);
          (f_len $ fun r -> r.len);
          ( Field.v "seq" ~constraint_:Expr.(Field.ref f_len >= int 1) uint16be
          $ fun r -> r.seq );
          (Field.v "body" (byte_array ~size:(Field.ref f_len)) $ fun r -> r.body);
        ]
end

let test_v_defers_compilation () =
  skip_unless_gc_counters ();
  let words f =
    let before = Gc.minor_words () in
    let r = Sys.opaque_identity (f ()) in
    (r, Gc.minor_words () -. before)
  in
  let c, built = words Deferred.codec in
  let buf = Bytes.of_string "\x21\003\000\007abc" in
  let decoded, first = words (fun () -> Codec.decode c buf 0) in
  (match decoded with
  | Ok r -> Alcotest.(check string) "body" "abc" r.Deferred.body
  | Error e -> Alcotest.failf "decode: %a" Error.pp e);
  Fmt.kstr
    (fun msg -> Alcotest.(check bool) msg true (built < first))
    "Codec.v allocates less than the first decode, which compiles (v: %.0f \
     words, first decode: %.0f)"
    built first

(* Domains racing on a codec nobody has used yet may each compile it; all get
   a working codec. *)
let test_first_use_races_across_domains () =
  let c = Deferred.codec () in
  let buf = Bytes.of_string "\x21\003\000\007abc" in
  let decode () =
    match Codec.decode c buf 0 with
    | Ok r -> r.Deferred.body
    | Error e -> Fmt.str "%a" Error.pp e
  in
  let ds = List.init 4 (fun _ -> Domain.spawn decode) in
  List.iter (fun d -> Alcotest.(check string) "body" "abc" (Domain.join d)) ds

(* What [v] checks of the description stays where the codec is built: the
   shape, and every field ref a constraint, an action or the [where] clause
   makes. *)
let test_v_rejects_bad_description_eagerly () =
  let f_later = Field.v "later" uint8 in
  let f_missing = Field.v "missing" uint8 in
  let expect (what, build) =
    match build () with
    | (_ : (UInt8.t * UInt8.t) Codec.t) ->
        Alcotest.failf "Codec.v accepted %s" what
    | exception Invalid_argument _ -> ()
  in
  List.iter expect
    [
      ( "two fields named x",
        fun () ->
          Codec.v "Dup"
            (fun a b -> (a, b))
            Codec.[ Field.v "x" uint8 $ fst; Field.v "x" uint8 $ snd ] );
      ( "a constraint on a field declared after it",
        fun () ->
          Codec.v "Forward"
            (fun a b -> (a, b))
            Codec.
              [
                Field.v "a" ~constraint_:Expr.(Field.ref f_later > int 0) uint8
                $ fst;
                f_later $ snd;
              ] );
      ( "an action reading a field the codec does not have",
        fun () ->
          Codec.v "ActionRef"
            (fun a b -> (a, b))
            Codec.
              [
                Field.v "a"
                  ~action:
                    (Action.on_success
                       [ Action.return_bool Expr.(Field.ref f_missing = int 0) ])
                  uint8
                $ fst;
                Field.v "b" uint8 $ snd;
              ] );
    ]

(* A fixed-size record's frame check bounds its byte spans, so a bitfield
   compared with a literal between two spans still takes the interval scan,
   and agrees with the general path on every comparison, at and around the
   bound, and on a buffer too short for the frame. *)
let test_fixed_spans_keep_intervals () =
  let outcome c buf =
    match Codec.validate c buf 0 with
    | Ok () -> "ok"
    | Error e -> Fmt.str "%a" Error.pp e
  in
  let comparisons =
    Expr.
      [
        (">=", fun v -> v >= int 5);
        (">", fun v -> v > int 5);
        ("<=", fun v -> v <= int 5);
        ("<", fun v -> v < int 5);
        ("=", fun v -> v = int 5);
        ("<>", fun v -> v <> int 5);
      ]
  in
  (* Each nibble at either end of either byte runs through values at and
     around the bound, whichever end the bitfield sits at. *)
  let bytes =
    [ 0x00; 0x04; 0x05; 0x06; 0x0f; 0x40; 0x50; 0x60; 0xf0; 0x45; 0x54; 0xff ]
  in
  List.iter
    (fun (base_name, base) ->
      List.iter
        (fun (op, cmp) ->
          let c =
            Codec.v "Spans"
              (fun head v rest tail -> (head, v, rest, tail))
              Codec.
                [
                  ( Field.v "head" (byte_array ~size:(int 12))
                  $ fun (h, _, _, _) -> h );
                  ( Field.v "v" ~self_constraint:cmp (bits ~width:4 base)
                  $ fun (_, v, _, _) -> v );
                  ( Field.v "rest"
                      (bits ~width:(if base = U8 then 4 else 12) base)
                  $ fun (_, _, r, _) -> r );
                  ( Field.v "tail" (byte_array ~size:(int 6))
                  $ fun (_, _, _, t) -> t );
                ]
          in
          let name = Fmt.str "%s %s 5" base_name op in
          let paths = Private.Testing.paths c in
          Alcotest.(check bool)
            (name ^ ": interval tests")
            true
            (List.mem "pass.interval" paths || List.mem "pass.words" paths);
          let general = Private.Testing.general c in
          let size = if base = U8 then 19 else 20 in
          List.iter
            (fun hi ->
              List.iter
                (fun lo ->
                  let buf = Bytes.make size '\000' in
                  Bytes.set_uint8 buf 12 hi;
                  if size = 20 then Bytes.set_uint8 buf 13 lo;
                  let at = Fmt.str "%s: bytes %02x %02x" name hi lo in
                  Alcotest.(check string)
                    at (outcome general buf) (outcome c buf);
                  let short = Bytes.sub buf 0 (size - 1) in
                  Alcotest.(check string)
                    (at ^ ", short buffer") (outcome general short)
                    (outcome c short))
                bytes)
            bytes)
        comparisons)
    [ ("U8", U8); ("U16be", U16be); ("U16", U16) ]

let suite =
  ( "codec_pass",
    [
      Alcotest.test_case "validate: check-free codec allocates nothing" `Quick
        test_validate_check_free_no_alloc;
      Alcotest.test_case "decode: fields read in order" `Quick
        test_decode_reads_in_order;
      Alcotest.test_case "encode: fixed codec allocates nothing" `Quick
        test_encode_fixed_no_alloc;
      Alcotest.test_case "decode: fixed scalars agree with get" `Quick
        test_decode_scalars_agree_with_get;
      Alcotest.test_case "decode: offset out of range" `Quick
        test_decode_offset_out_of_range;
      Alcotest.test_case "check: fixed spans keep the interval scan" `Quick
        test_fixed_spans_keep_intervals;
      Alcotest.test_case "check: constraint intervals" `Quick
        test_constraint_intervals;
      Alcotest.test_case "check: a failure reports this frame's value" `Quick
        test_failure_value_is_this_frames;
      Alcotest.test_case "check: a wide field's constraint is not truncated"
        `Quick test_wide_constraint;
      Alcotest.test_case "check: a wide field's presence is not truncated"
        `Quick test_wide_presence;
      Alcotest.test_case "encode: plain fields" `Quick test_plain_encode;
      Alcotest.test_case "view: read agrees with get" `Quick test_view_read;
      Alcotest.test_case "view: short frames read as get" `Quick
        test_view_short_frames;
      Alcotest.test_case "check: tests merged in one word" `Quick
        test_word_merged_tests;
      Alcotest.test_case "check: constraints sharing a byte" `Quick
        test_shared_byte_constraints;
      Alcotest.test_case "check: mapped and disjunctive constraints" `Quick
        test_mapped_and_disjunctive_checks;
      Alcotest.test_case "decode: staged scalars agree with get" `Quick
        test_staged_scalars;
      Alcotest.test_case "decode: fixed byte arrays agree with get" `Quick
        test_staged_spans;
      Alcotest.test_case "layout: agrees with the general path" `Quick
        test_layout_pass;
      Alcotest.test_case "layout: a size that overflows is refused" `Quick
        test_layout_size_overflow;
      Alcotest.test_case "encode: fixed byte arrays agree with the general path"
        `Quick test_plain_encode_blits;
      Alcotest.test_case "encode: a negative offset is named" `Quick
        test_encode_negative_offset;
      Alcotest.test_case "layout: a gated optional has no fixed size" `Quick
        test_layout_gated_optional;
      Alcotest.test_case "size: a literal size is a byte count" `Quick
        test_literal_size_is_a_byte_count;
      Alcotest.test_case "size: a record must fit in a buffer" `Quick
        test_record_too_big_for_a_buffer;
      Alcotest.test_case "size: an overflowing array is refused" `Quick
        test_array_size_overflow;
      Alcotest.test_case
        "validate: fresh domain cost is flat in validator count" `Quick
        test_fresh_domain_validate_flat_in_validator_count;
      Alcotest.test_case "v: compiles on first use, not when built" `Quick
        test_v_defers_compilation;
      Alcotest.test_case "v: first use racing across domains" `Quick
        test_first_use_races_across_domains;
      Alcotest.test_case "v: a bad description still fails where it is built"
        `Quick test_v_rejects_bad_description_eagerly;
    ] )
