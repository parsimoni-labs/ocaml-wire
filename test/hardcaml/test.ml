open Hardcaml
module H = Wire_hardcaml
module Io = Hardware_stream_io.Stream_io

let expect_rejection errors (result : Io.result) =
  if
    result.accepted || result.consumed <> 0 || result.errors <> errors
    || List.exists (fun (_, value) -> value <> 0) result.fields
  then
    Alcotest.failf "bad stream rejection: accepted=%b errors=%d expected=%d"
      result.accepted result.errors errors

let tagged_frames ~bytes frames =
  List.concat
    (List.mapi (fun tag frame -> Io.frame ~bytes ~tag ~parts:[] frame) frames)

let packet_tests () =
  List.concat_map
    (fun (Diff_codecs.Scan c) ->
      if c.label <> "scan_packet" then []
      else
        let plan = Wire_ocaml.Plan.v c.codec in
        let oracle = Diff_driver.oracle c.label in
        let sample =
          List.find
            (fun b ->
              Bytes.length b = 128
              && Result.is_ok (Wire.Codec.decode c.codec b ~off:0))
            (c.corpus ())
        in
        let create ?(bytes = 64) ?(maximum = 65549) packing =
          Io.v ~packing ~bytes ~capacity:128 ~max_length:maximum plan
        in
        let check frame (result : Io.result) =
          let expected =
            match
              ( Wire.Codec.decode c.codec frame ~off:0,
                Wire.Codec.size_at c.codec frame ~off:0 )
            with
            | Error _, _ | _, Error _ -> None
            | Ok value, Ok n -> Some (n, c.project value)
          in
          Option.iter
            (fun (o : Diff_codecs.c_oracle) ->
              let end_ = o.standalone frame 0 (Bytes.length frame) in
              if end_ <> Option.fold ~none:(-1) ~some:fst expected then
                Alcotest.fail "Codec and actual C disagree")
            oracle;
          if
            (not result.in_scope) || result.errors <> 0
            || result.length <> Bytes.length frame
          then Alcotest.fail "stream framing diverged";
          match expected with
          | None -> expect_rejection 0 result
          | Some (consumed, fields) ->
              if (not result.accepted) || result.consumed <> consumed then
                Alcotest.fail "stream validation diverged";
              List.iter
                (fun (name, value) ->
                  let expected =
                    Option.value ~default:0 (List.assoc_opt name fields)
                  in
                  if value <> expected then
                    Alcotest.failf "stream scalar %s=%d expected%d" name value
                      expected)
                result.fields
        in
        let check_frames sim frames =
          let observed = Io.run sim (tagged_frames ~bytes:64 frames) in
          if List.length observed <> List.length frames then
            Alcotest.fail "lost stream result";
          List.iter
            (fun (tag, result) -> check (List.nth frames tag) result)
            observed
        in
        [
          Alcotest.test_case "all first-beat splits and padding" `Quick
            (fun () ->
              let sim = create H.Stream.Contiguous in
              let events =
                List.concat
                  (List.init 64 (fun i ->
                       Io.frame ~bytes:64 ~tag:i
                         ~parts:[ i + 1; (i * 7 mod 64) + 1 ]
                         sample))
              in
              List.iter
                (fun seed ->
                  let observed = Io.run ~seed sim events in
                  Alcotest.(check int)
                    "one result per split" 64 (List.length observed);
                  List.iter (fun (_, result) -> check sample result) observed)
                [ 0; 1; 255 ]);
          Alcotest.test_case "one-byte fragments on a wide bus" `Quick
            (fun () ->
              let sim = create H.Stream.Contiguous in
              let events =
                Io.frame ~bytes:64 ~tag:0
                  ~parts:(List.init 128 (fun _ -> 1))
                  sample
              in
              match Io.run sim events with
              | [ (0, result) ] -> check sample result
              | _ -> Alcotest.fail "bytewise stream result count diverged");
          Alcotest.test_case "strict profile refuses partial intermediate beats"
            `Quick (fun () ->
              let events = Io.frame ~bytes:64 ~tag:0 ~parts:[ 1 ] sample in
              match Io.run (create H.Stream.Full_beats) events with
              | [ (0, result) ] -> expect_rejection 2 result
              | _ -> Alcotest.fail "strict profile lost rejected result");
          Alcotest.test_case "all final masks back-to-back" `Quick (fun () ->
              let frames = List.init 65 (fun len -> Bytes.sub sample 0 len) in
              let observed =
                Io.run ~stalls:false ~bubbles:false
                  (create H.Stream.Full_beats)
                  (tagged_frames ~bytes:64 frames)
              in
              Alcotest.(check int)
                "one result per mask" 65 (List.length observed);
              List.iter
                (fun (tag, result) -> check (List.nth frames tag) result)
                observed);
          Alcotest.test_case "zero-byte TLAST closes a nonempty frame" `Quick
            (fun () ->
              List.iter
                (fun packing ->
                  let body =
                    Io.frame ~bytes:64 ~tag:0 ~parts:[] sample
                    |> List.map (function
                      | Io.Beat beat -> Io.Beat { beat with last = false }
                      | event -> event)
                  in
                  let delimiter =
                    Io.Beat
                      {
                        data = Bytes.empty;
                        keep = Bits.zero 64;
                        last = true;
                        error = false;
                        tag = 0;
                      }
                  in
                  let next = Io.frame ~bytes:64 ~tag:1 ~parts:[] sample in
                  match
                    Io.run (create packing) (body @ [ delimiter ] @ next)
                  with
                  | [ (0, a); (1, b) ] ->
                      check sample a;
                      check sample b
                  | _ -> Alcotest.fail "zero-byte delimiter changed frame count")
                [ H.Stream.Full_beats; H.Stream.Contiguous ]);
          Alcotest.test_case "beat widths and line-rate ingress" `Quick
            (fun () ->
              List.iter
                (fun bytes ->
                  List.iter
                    (fun packing ->
                      let frames = [ sample; Bytes.sub sample 0 1; sample ] in
                      let events = tagged_frames ~bytes frames in
                      let observed =
                        Io.run ~stalls:false ~bubbles:false
                          (create ~bytes packing) events
                      in
                      Alcotest.(check int)
                        "frame count" 3 (List.length observed);
                      List.iter
                        (fun (tag, result) ->
                          check (List.nth frames tag) result)
                        observed)
                    [ H.Stream.Full_beats; H.Stream.Contiguous ])
                [ 1; 2; 3; 7; 8; 16; 31; 32; 63; 64 ]);
          Alcotest.test_case "long payloads options fragments truncations" `Slow
            (fun () ->
              let frames = Diff_codecs.packet_payload_corpus () in
              List.iter
                (fun packing -> check_frames (create packing) frames)
                [ H.Stream.Full_beats; H.Stream.Contiguous ]);
          Alcotest.test_case "bad masks and input errors drain and recover"
            `Quick (fun () ->
              List.iter
                (fun packing ->
                  let good = Io.frame ~bytes:64 ~tag:99 ~parts:[] sample in
                  let events = Io.frame ~bytes:64 ~tag:0 ~parts:[] sample in
                  let corrupt kind =
                    List.mapi
                      (fun i event ->
                        match event with
                        | Io.Beat beat when i = 0 ->
                            Io.Beat
                              (match kind with
                              | 1 -> { beat with error = true }
                              | 2 ->
                                  {
                                    beat with
                                    keep = Bits.of_int64 ~width:64 5L;
                                  }
                              | _ -> { beat with keep = Bits.zero 64 })
                        | event -> event)
                      events
                  in
                  List.iter
                    (fun kind ->
                      let observed =
                        Io.run (create packing) (corrupt kind @ good)
                      in
                      match observed with
                      | [ (0, bad); (99, recovered) ] ->
                          expect_rejection (if kind = 1 then 1 else 2) bad;
                          check sample recovered
                      | _ -> Alcotest.fail "error frame failed to drain")
                    [ 1; 2; 3 ])
                [ H.Stream.Full_beats; H.Stream.Contiguous ]);
          Alcotest.test_case "overflow saturates drains and recovers" `Quick
            (fun () ->
              let frames =
                [
                  Bytes.make 257 '\001'; sample; Bytes.make 4096 '\002'; sample;
                ]
              in
              let observed =
                Io.run
                  (create ~maximum:256 H.Stream.Full_beats)
                  (tagged_frames ~bytes:64 frames)
              in
              List.iter
                (fun (tag, result) ->
                  if tag mod 2 = 0 then (
                    expect_rejection 4 result;
                    Alcotest.(check int) "saturated length" 257 result.length;
                    Alcotest.(check bool) "out of scope" false result.in_scope)
                  else check sample result)
                observed;
              Alcotest.(check int) "recovered count" 4 (List.length observed));
          Alcotest.test_case "four reserved results drain after a long stall"
            `Quick (fun () ->
              let sim = create ~maximum:256 H.Stream.Full_beats in
              let frames =
                [ sample; Bytes.empty; Bytes.make 4096 '\000'; sample; sample ]
              in
              let observed =
                Io.run ~bubbles:false ~stall_burst:128 sim
                  (tagged_frames ~bytes:64 frames)
              in
              Alcotest.(check int) "result count" 5 (List.length observed);
              Alcotest.(check int)
                "reserved capacity" 4 (Io.statistics sim).max_pending;
              List.iter
                (fun (tag, (result : Io.result)) ->
                  Alcotest.(check int64)
                    "ordered ID" (Int64.of_int tag) result.frame_id;
                  if tag = 2 then (
                    expect_rejection 4 result;
                    Alcotest.(check bool) "overflow scope" false result.in_scope)
                  else check (List.nth frames tag) result)
                observed);
          Alcotest.test_case "reset cancels four reserved results" `Quick
            (fun () ->
              let sim = create H.Stream.Full_beats in
              let warmup =
                tagged_frames ~bytes:64 (List.init 4 (fun _ -> sample))
              in
              let good = Io.frame ~bytes:64 ~tag:99 ~parts:[] sample in
              match
                Io.run ~bubbles:false ~stall_burst:128 sim
                  (warmup @ (Io.Reset :: good))
              with
              | [ (99, recovered) ] ->
                  Alcotest.(check int)
                    "reserved capacity" 4 (Io.statistics sim).max_pending;
                  Alcotest.(check int)
                    "cancelled snapshots" 4 (Io.statistics sim).reset_cancelled;
                  Alcotest.(check int64) "reset ID" 0L recovered.frame_id;
                  check sample recovered
              | _ -> Alcotest.fail "reset leaked a queued snapshot");
          Alcotest.test_case "65549 boundary and one-byte overflow" `Slow
            (fun () ->
              let boundary =
                List.find
                  (fun b -> Bytes.length b = 65549)
                  (Diff_codecs.packet_payload_corpus ())
              in
              let overflow = Bytes.cat boundary (Bytes.make 1 '\000') in
              match
                Io.run
                  (create H.Stream.Full_beats)
                  (tagged_frames ~bytes:64 [ boundary; overflow; sample ])
              with
              | [ (0, valid); (1, bad); (2, recovered) ] ->
                  check boundary valid;
                  expect_rejection 4 bad;
                  check sample recovered
              | _ -> Alcotest.fail "maximum length lost framing");
          Alcotest.test_case "reset aborts partial and pending frames" `Quick
            (fun () ->
              let partial =
                Io.frame ~bytes:64 ~tag:0 ~parts:[] sample
                |> List.map (function
                  | Io.Beat beat -> Io.Beat { beat with last = false }
                  | e -> e)
              in
              let pending =
                Io.frame ~bytes:64 ~tag:1 ~parts:[] (Bytes.sub sample 0 14)
              in
              let good = Io.frame ~bytes:64 ~tag:2 ~parts:[] sample in
              match
                Io.run
                  (create H.Stream.Full_beats)
                  (partial @ [ Io.Reset ] @ pending @ [ Io.Reset ] @ good)
              with
              | [ (2, result) ] -> check sample result
              | _ -> Alcotest.fail "reset leaked aborted metadata");
          Alcotest.test_case "frame IDs survive long output stalls" `Quick
            (fun () ->
              let frames = List.init 8 (fun _ -> sample) in
              let observed =
                Io.run ~stall_burst:128
                  (create H.Stream.Full_beats)
                  (tagged_frames ~bytes:64 frames)
              in
              Alcotest.(check int)
                "ordered frame count" 8 (List.length observed);
              List.iter
                (fun (tag, result) ->
                  Alcotest.(check int64)
                    "EOF ID" (Int64.of_int tag) result.Io.frame_id;
                  check sample result)
                observed);
          Alcotest.test_case "capture proof fails closed" `Quick (fun () ->
              let data = Signal.input "data" 1024
              and length = Signal.input "length" 17 in
              let decoder =
                H.v ~capacity:128 ~max_length:65549 ~data ~length plan
              in
              Alcotest.(check int)
                "proven scalar prefix" 94 (H.read_prefix decoder);
              match
                H.v ~capacity:64 ~max_length:65549
                  ~data:(Signal.input "short" 512) ~length plan
              with
              | _ -> Alcotest.fail "uncaptured scalar reads were allowed"
              | exception Invalid_argument _ -> ());
        ])
    Diff_codecs.scan_cases

let configuration_tests () =
  List.concat_map
    (fun (Diff_codecs.Scan c) ->
      if c.label <> "scan_words" then []
      else
        [
          Alcotest.test_case "final beat after overflow retains its carry"
            `Quick (fun () ->
              let open Wire in
              let codec =
                Codec.v "OverflowCarry" Fun.id
                  Codec.[ Field.v "value" uint8 $ Fun.id ]
              in
              let plan = Wire_ocaml.Plan.v codec in
              List.iter
                (fun packing ->
                  let sim =
                    Io.v ~packing ~bytes:64 ~capacity:1 ~max_length:3 plan
                  in
                  let bad =
                    Io.frame ~bytes:64 ~tag:0 ~parts:[ 3; 64; 64 ]
                      (Bytes.make 131 '\000')
                  in
                  let good =
                    Io.frame ~bytes:64 ~tag:1 ~parts:[] (Bytes.of_string "\127")
                  in
                  match Io.run ~seed:17 ~stall_burst:32 sim (bad @ good) with
                  | [ (0, rejected); (1, recovered) ] ->
                      expect_rejection
                        (if packing = H.Stream.Contiguous then 4 else 6)
                        rejected;
                      Alcotest.(check bool)
                        "overflow scope" false rejected.in_scope;
                      Alcotest.(check int) "overflow sentinel" 4 rejected.length;
                      Alcotest.(check int64) "overflow ID" 0L rejected.frame_id;
                      Alcotest.(check int64) "recovery ID" 1L recovered.frame_id;
                      Alcotest.(check bool)
                        "recovered acceptance" true recovered.accepted;
                      Alcotest.(check bool)
                        "recovered scope" true recovered.in_scope;
                      Alcotest.(check int) "recovered length" 1 recovered.length;
                      Alcotest.(check int) "recovered errors" 0 recovered.errors;
                      Alcotest.(check int)
                        "recovered consumed" 1 recovered.consumed;
                      Alcotest.(check int)
                        "recovered scalar" 127
                        (List.assoc "value" recovered.fields)
                  | _ -> Alcotest.fail "overflow carry lost framing")
                [ H.Stream.Contiguous; H.Stream.Full_beats ]);
          Alcotest.test_case "byte counts exceed small logical maxima safely"
            `Quick (fun () ->
              let plan = Wire_ocaml.Plan.v c.codec in
              List.iter
                (fun maximum ->
                  List.iter
                    (fun bytes ->
                      List.iter
                        (fun packing ->
                          let sim =
                            Io.v ~packing ~bytes ~capacity:maximum
                              ~max_length:maximum plan
                          in
                          let lengths =
                            [ 0; maximum; maximum + 1; 64; (2 * maximum) + 1 ]
                          in
                          let frames =
                            List.map (fun n -> Bytes.make n '\001') lengths
                          in
                          let observed =
                            Io.run sim (tagged_frames ~bytes frames)
                          in
                          Alcotest.(check int)
                            "configuration frame count" (List.length frames)
                            (List.length observed);
                          List.iter
                            (fun (tag, (result : Io.result)) ->
                              let length = List.nth lengths tag in
                              let overflow = length > maximum in
                              Alcotest.(check bool)
                                "logical scope" (not overflow) result.in_scope;
                              Alcotest.(check int)
                                "saturated frame length"
                                (min length (maximum + 1))
                                result.length;
                              Alcotest.(check int)
                                "overflow error"
                                (if overflow then 4 else 0)
                                result.errors;
                              if overflow then expect_rejection 4 result)
                            observed)
                        [ H.Stream.Full_beats; H.Stream.Contiguous ])
                    [ 1; 2; 7; 32; 64 ])
                [ 1; 3; 16; 32; 63; 64; 65 ]);
        ])
    Diff_codecs.scan_cases

let () =
  Diff_driver.report_oracle "hardcaml-stream-transport";
  Alcotest.run "hardcaml-stream-transport"
    [
      ("transport", packet_tests ()); ("configuration", configuration_tests ());
    ]
