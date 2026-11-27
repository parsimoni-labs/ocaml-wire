(* The modules the differential fuzzer generates for its codecs, against the
   codecs, through its driver. *)

open Wire

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let paths c =
  let rec go prefix (view : Wire_ocaml.view) =
    List.concat_map
      (function
        | Wire_ocaml.Int { name; _ } -> [ prefix ^ name ]
        | Record { name; view; _ } -> go (prefix ^ name ^ ".") view
        | Bool _ | Bytes _ -> [])
      view.members
  in
  go "" (Wire_ocaml.views c)

let scanner c = Wire_ocaml.(to_ml (v ~target:`OCaml ~fold:(paths c) c))

(* An Ethernet, IPv4 and TCP header of [Packet_stack.codec] in 64 bytes,
   which [edit] changes. *)
let stack_frame edit =
  let b = Bytes.make 64 '\000' in
  Bytes.set_uint16_be b 12 0x0800;
  Bytes.set_uint8 b 14 0x45;
  Bytes.set_uint16_be b 16 50;
  Bytes.set_uint8 b 22 64;
  Bytes.set_uint8 b 23 6;
  Bytes.set_int32_be b 26 0xc0a80001l;
  Bytes.set_int32_be b 30 0xc0a80002l;
  Bytes.set_uint16_be b 34 443;
  Bytes.set_uint16_be b 36 51000;
  Bytes.set_uint8 b 46 0x50;
  edit b;
  b

(* On OCaml the step functions read the Ethernet type from the word that also
   holds the start of the IPv4 header, loaded before the dispatch on the type.
   They give the codec's answers on records each check rejects and on frames
   cut around the bounds that word straddles. *)
let test_shared_words () =
  let src = scanner Packet_stack.codec in
  Alcotest.(check bool)
    "the Ethernet type from the word at 12" true
    (contains src "(Int64.shift_right_logical w12 48)");
  let case =
    List.find
      (fun (Diff_codecs.Scan c) -> c.label = "scan_packet")
      Diff_codecs.scan_cases
  in
  let reference = Diff_ocaml.of_scan_case case
  and backends = (List.assoc "scan_packet" Ocaml_index.scanners) () in
  let records =
    [
      ("TCP", true, ignore);
      ("not IPv4", true, fun b -> Bytes.set_uint16_be b 12 0x86dd);
      ("version", false, fun b -> Bytes.set_uint8 b 14 0x65);
      ("header length", false, fun b -> Bytes.set_uint8 b 14 0x44);
      ("total length", false, fun b -> Bytes.set_uint16_be b 16 16);
      ("total past the frame", false, fun b -> Bytes.set_uint16_be b 16 51);
      ("fragment", true, fun b -> Bytes.set_uint16_be b 20 1);
      ( "UDP length",
        false,
        fun b ->
          Bytes.set_uint8 b 23 17;
          Bytes.set_uint16_be b 38 7 );
      ("data offset", false, fun b -> Bytes.set_uint8 b 46 0x40);
    ]
  in
  let frames =
    List.map
      (fun (what, ok, edit) ->
        let b = stack_frame edit in
        Alcotest.(check bool)
          (what ^ " accepted") ok
          (Result.is_ok (Codec.decode Packet_stack.codec b ~off:0));
        b)
      records
  in
  Diff_ocaml.check_scan ~strides:[ 64 ] ~expand:false reference backends
    (Bytes.concat Bytes.empty frames);
  let ip = stack_frame ignore
  and other = stack_frame (fun b -> Bytes.set_uint16_be b 12 0x86dd) in
  List.iter
    (fun n ->
      let cut b = Bytes.sub b 0 n in
      Diff_ocaml.check_scan ~strides:[ n ] ~expand:false reference backends
        (Bytes.concat Bytes.empty [ cut ip; cut other; cut ip ]))
    [ 13; 14; 19; 20; 21; 33; 34; 35 ]

(* The structured cases' corpora, and the EverParse C validator when built. *)
let test_scanners () =
  List.iter
    (fun (Diff_codecs.Scan c as case) ->
      Diff_ocaml.scan_corpus
        ?oracle:(Diff_driver.oracle c.label)
        case
        ((List.assoc c.label Ocaml_index.scanners) ()))
    Diff_codecs.scan_cases

(* The codec and every scanner, alone and in a frame fold, accept the frames
   of [cases] marked accepted and reject the others. *)
let check_packet cases =
  let label = "scan_packet" in
  let codec =
    match
      List.find
        (fun (Diff_codecs.Scan c) -> c.label = label)
        Diff_codecs.scan_cases
    with
    | Diff_codecs.Scan c ->
        fun b -> Result.is_ok (Codec.decode c.codec b ~off:0)
  in
  let backends = (List.assoc label Ocaml_index.scanners) () in
  List.iter
    (fun (name, b, accepted) ->
      let len = Bytes.length b in
      Alcotest.(check bool) (name ^ ": codec") accepted (codec b);
      List.iter
        (fun (s : Diff_ocaml.scan_ops) ->
          Alcotest.(check bool)
            (Fmt.str "%s: %s" name s.scan_name)
            accepted
            (Option.is_some (s.scan b 0 len));
          Option.iter
            (fun batch ->
              let frames =
                Option.value ~default:[] (batch (Bytes.cat b b) 0 (2 * len) len)
              in
              Alcotest.(check (list bool))
                (Fmt.str "%s: %s fold_frames" name s.scan_name)
                [ accepted; accepted ]
                (List.map Option.is_some frames))
            s.scan_batch)
        backends)
    cases

(* A frame that exactly fits a bound is accepted, and one a byte over is
   rejected. *)
let test_packet_bounds () = check_packet Diff_codecs.packet_bounds

(* The IPv4 version and header length byte is accepted exactly in
   [0x45, 0x4f]. *)
let test_packet_versions () = check_packet Diff_codecs.packet_versions

let () =
  Alcotest.run "wire_ocaml_diff"
    [
      ( "diff",
        [
          Alcotest.test_case "a word shared across prefixes" `Quick
            test_shared_words;
          Alcotest.test_case "structured scanners" `Quick test_scanners;
          Alcotest.test_case "packet bounds" `Quick test_packet_bounds;
          Alcotest.test_case "packet versions" `Quick test_packet_versions;
        ] );
    ]
