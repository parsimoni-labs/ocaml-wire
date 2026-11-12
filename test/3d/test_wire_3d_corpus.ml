(* Tests for wire_3d.ml: the differential test corpus. *)

(* A uniform byte draw cannot reach this accepting side: one 32-bit value out
   of 2^32 satisfies the magic constraint. The generated corpus must construct
   accepted records and retain rejected neighbours. This test is pure OCaml and
   therefore runs without 3d.exe. *)
let corpus_magic_codec =
  let open Wire in
  let magic =
    Field.v "magic" uint32be ~self_constraint:(fun self ->
        Expr.(self = int 0x53504F53))
  in
  let ver = Field.v "ver" uint8 in
  Codec.v "Super"
    (fun m v -> (m, v))
    [ Codec.( $ ) magic fst; Codec.( $ ) ver snd ]

let corpus_verdicts ?(count = 256) codecs =
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  Wire_3d.generate_corpus ~count ppf codecs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.rindex_opt line ' ' with
      | Some i -> Some (String.sub line (i + 1) (String.length line - i - 1))
      | None -> None)
  |> List.fold_left
       (fun (accepted, rejected) -> function
         | "1" -> (accepted + 1, rejected)
         | "0" -> (accepted, rejected + 1)
         | _ -> (accepted, rejected))
       (0, 0)

let test_corpus_straddles_a_constraint () =
  let accepted, rejected =
    corpus_verdicts [ Wire_3d.pack corpus_magic_codec ]
  in
  Alcotest.(check bool)
    (Fmt.str "corpus reaches the accepting side (%d accepted)" accepted)
    true (accepted > 0);
  Alcotest.(check bool)
    (Fmt.str "corpus keeps rejected inputs (%d rejected)" rejected)
    true (rejected > 0);
  Alcotest.(check int) "every line carries a verdict" 256 (accepted + rejected)

(* Two four-byte magic tags out of 2^32: uniform bytes never select a case, and
   the tag is not a field of its own but the head of the casetype's bytes. The
   corpus must seed it from the case indices. *)
let corpus_casetype_codec =
  let open Wire in
  let body =
    casetype "Node" uint32be
      [
        case
          ~index:(UInt32.of_int32 0x4c454146l)
          uint8
          ~inject:(fun v -> `Leaf v)
          ~project:(function `Leaf v -> Some v | _ -> None);
        case
          ~index:(UInt32.of_int32 0x4e4f4445l)
          uint16
          ~inject:(fun v -> `Branch v)
          ~project:(function `Branch v -> Some v | _ -> None);
      ]
  in
  Codec.v "Page" Fun.id [ Codec.( $ ) (Field.v "body" body) Fun.id ]

let test_corpus_seeds_a_casetype_tag () =
  let accepted, rejected =
    corpus_verdicts [ Wire_3d.pack corpus_casetype_codec ]
  in
  Alcotest.(check bool)
    (Fmt.str "corpus selects a case (%d accepted)" accepted)
    true (accepted > 0);
  Alcotest.(check bool)
    (Fmt.str "corpus keeps unmatched tags (%d rejected)" rejected)
    true (rejected > 0)

(* An unconstrained length field filled from uniform bytes asks for gigabytes,
   and the demand surfaces on the record's whole extent, naming no field to
   repair. Growing to it is impossible, so the seeder must damp the length
   instead of giving up on the codec. *)
let corpus_length_codec =
  let open Wire in
  let len = Field.v "len" uint32be in
  Codec.v "Load"
    (fun l d -> (l, d))
    [
      Codec.( $ ) len (fun (l, _) -> l);
      Codec.( $ )
        (Field.v "data" (byte_array ~size:(Field.ref len)))
        (fun (_, d) -> d);
    ]

let test_corpus_damps_a_runaway_length () =
  let accepted, rejected =
    corpus_verdicts [ Wire_3d.pack corpus_length_codec ]
  in
  Alcotest.(check bool)
    (Fmt.str "corpus reaches a well-sized record (%d accepted)" accepted)
    true (accepted > 0);
  Alcotest.(check bool)
    (Fmt.str "corpus keeps mis-sized records (%d rejected)" rejected)
    true (rejected > 0)

(* A span sized from an input param. The record's extent is only computable with
   the env bound: without one every [Param.expr] resolves to 0, so the span
   measures as empty and the extent comes back shorter than the record. *)
let corpus_param_span_codec =
  let open Wire in
  let size = Param.input "size" uint8 in
  Codec.v "ParamSpan"
    (fun k d -> (k, d))
    [
      Codec.( $ ) (Field.v "kind" uint8) fst;
      Codec.( $ ) (Field.v "data" (byte_array ~size:(Param.expr size))) snd;
    ]

(* One [name params hex verdict] corpus line, split into the parts a reader
   needs to replay it. *)
let corpus_lines ?(count = 128) codecs =
  let buf = Buffer.create 4096 in
  let ppf = Fmt.with_buffer buf in
  Wire_3d.generate_corpus ~count ppf codecs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf |> String.split_on_char '\n'
  |> List.filter_map (fun line ->
      match String.split_on_char ' ' line with
      | [ _; params; hex; verdict ] ->
          Some (int_of_string params, hex, String.equal verdict "1")
      | _ -> None)

let bytes_of_hex h =
  if String.equal h "-" then Bytes.empty
  else
    Bytes.init
      (String.length h / 2)
      (fun i -> Char.chr (int_of_string ("0x" ^ String.sub h (2 * i) 2)))

(* The corpus verdict is the differential's oracle, so a wrong one does not
   refuse, it disagrees -- and the disagreement is reported against the C
   validator rather than against the harness. Nothing else checks the oracle,
   so replay every line through the codec it claims to speak for. Deliberately
   an independent reimplementation of the verdict, not a call into it. *)
let test_corpus_verdict_resolves_params () =
  let open Wire in
  let c = corpus_param_span_codec in
  let lines = corpus_lines [ Wire_3d.pack c ] in
  Alcotest.(check bool) "the corpus is non-empty" true (lines <> []);
  List.iter
    (fun (p, hex, verdict) ->
      let env = Param.bind_by_name "size" p (Codec.env c) in
      let b = bytes_of_hex hex in
      (* Sized from the decoded value, not from [wire_size_at]: an oracle that
         went back through the extent reader would be re-using the thing it is
         meant to check. *)
      let accepts =
        match Codec.decode ~env c b 0 with
        | Error _ -> false
        | Ok v -> (
            match
              Codec.validate ~env c b 0;
              Codec.size_of_value c v
            with
            | n -> Int.equal n (Bytes.length b)
            | exception Wire.Parse_error _ -> false)
      in
      if not (Bool.equal accepts verdict) then
        Alcotest.failf "corpus records %b for size=%d %s, the codec answers %b"
          verdict p hex accepts)
    lines;
  (* With the extent measured without the env, the only records that can span
     their whole buffer are the ones whose param is 0. *)
  Alcotest.(check bool)
    "an accepted record binds a non-zero param" true
    (List.exists (fun (p, _, v) -> p <> 0 && v) lines)

(* A whole-record where-clause has no failing field offset to repair. Refuse a
   one-sided corpus instead of allowing an always-rejecting validator to pass. *)
let corpus_unseedable_codec =
  let open Wire in
  let tag = Field.v "tag" uint32be in
  Codec.v "Opaque"
    ~where:Expr.(Field.ref tag = int 0x53504F53)
    Fun.id
    [ Codec.( $ ) tag Fun.id ]

let test_corpus_refuses_when_vacuous () =
  match corpus_verdicts ~count:64 [ Wire_3d.pack corpus_unseedable_codec ] with
  | accepted, rejected ->
      Alcotest.failf
        "generate_corpus returned a vacuous corpus (%d accepted, %d rejected) \
         instead of refusing"
        accepted rejected
  | exception Failure msg ->
      Alcotest.(check bool)
        "the refusal names the codec" true
        (Re.execp (Re.compile (Re.str "Opaque")) msg);
      Alcotest.(check bool)
        "the refusal reports the tally" true
        (Re.execp (Re.compile (Re.str "0 accepted")) msg)

let suite =
  ( "wire_3d_corpus",
    [
      Alcotest.test_case "corpus straddles a constraint" `Quick
        test_corpus_straddles_a_constraint;
      Alcotest.test_case "corpus refuses when vacuous" `Quick
        test_corpus_refuses_when_vacuous;
      Alcotest.test_case "corpus: verdict resolves input params" `Quick
        test_corpus_verdict_resolves_params;
      Alcotest.test_case "corpus: seeds a casetype tag" `Quick
        test_corpus_seeds_a_casetype_tag;
      Alcotest.test_case "corpus: damps a runaway length" `Quick
        test_corpus_damps_a_runaway_length;
    ] )
