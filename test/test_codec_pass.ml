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
  Codec.validate c buf 0;
  (* warm up any one-time setup *)
  let n = 100_000 in
  let before = Gc.minor_words () in
  for _ = 1 to n do
    Codec.validate c buf 0
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
    Alcotest.(check bool) name true (Codec.decode_exn c buf 0 = get buf 0)
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
        (Codec.decode_exn c buf 0))
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
   [decode] makes before its unchecked reads would wrap round. *)
let test_record_too_big_for_a_buffer () =
  let c =
    Codec.v "Huge"
      (fun _ _ -> ())
      Codec.
        [
          ( Field.v "a" (byte_array ~size:(int Sys.max_string_length)) $ fun _ ->
            "" );
          (Field.v "b" uint32 $ fun _ -> UInt32.of_int 0);
        ]
  in
  Alcotest.(check bool)
    "first use" true
    (raises_invalid (fun () -> Codec.decode c (Bytes.create 8) 0))

(* An array's size is a byte count too: one that would overflow is refused. *)
let test_array_size_overflow () =
  Alcotest.(check bool)
    "array" true
    (raises_invalid (fun () -> array ~len:(int Sys.max_string_length) uint32))

(* A field whose constraints only compare it with literals is checked as one
   interval test. Its slot is still filled when a later constraint or the
   [where] clause reads it, and a failure reports the field and its value. *)
let test_constraint_intervals () =
  let describe (e : parse_error) =
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
      match Codec.validate c buf 0 with
      | () -> "ok"
      | exception Wire.Parse_error e -> describe e
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
    | () -> false
    | exception Wire.Parse_error _ -> true)

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
    | Error e -> Fmt.str "%a" pp_parse_error e
  in
  let fresh = failure (codec ()) (frame (-1L)) in
  let c = codec () in
  ignore (failure c (frame 1001L));
  Alcotest.(check string) "after another frame" fresh (failure c (frame (-1L)))

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
  Codec.decode_exn c (Bytes.make 9 '\x00') 0;
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
           Codec.validate c buf 0;
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
  | Error e -> Alcotest.failf "decode: %a" pp_parse_error e);
  Fmt.kstr
    (fun msg -> Alcotest.(check bool) msg true (built < first))
    "Codec.v allocates less than the first decode, which compiles (v: %.0f \
     words, first decode: %.0f)"
    built first

(* The compile runs once per codec value, whichever domain asks first. Two
   domains racing on a codec nobody has used yet both get a working codec. *)
let test_first_use_races_across_domains () =
  let c = Deferred.codec () in
  let buf = Bytes.of_string "\x21\003\000\007abc" in
  let decode () =
    match Codec.decode c buf 0 with
    | Ok r -> r.Deferred.body
    | Error e -> Fmt.str "%a" pp_parse_error e
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
      Alcotest.test_case "check: constraint intervals" `Quick
        test_constraint_intervals;
      Alcotest.test_case "check: a failure reports this frame's value" `Quick
        test_failure_value_is_this_frames;
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
      Alcotest.test_case "v: first use racing across domains compiles once"
        `Quick test_first_use_races_across_domains;
      Alcotest.test_case "v: a bad description still fails where it is built"
        `Quick test_v_rejects_bad_description_eagerly;
    ] )
