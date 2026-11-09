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
    (List.filteri (fun i _ -> i < 9) (List.rev !seen))

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
