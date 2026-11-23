open Hardcaml
module S = Signal
module H = Wire_hardcaml

let keep_reference keep = S.(keep &: keep +:. 1 ==:. 0)

let keep_predicate ~reference =
  let keep = S.input "keep" 64 in
  let contiguous =
    if reference then keep_reference keep else H.Private.contiguous_keep keep
  in
  Circuit.create_exn ~name:"wire_keep64" [ S.output "contiguous" contiguous ]

let keep_equivalence () =
  let checks =
    List.map
      (fun width ->
        let keep = S.input (Fmt.str "keep_%d" width) width in
        let reference = keep_reference keep in
        S.(H.Private.contiguous_keep keep ==: reference))
      (List.init 64 (fun i -> i + 1) @ [ 128 ])
  in
  Circuit.create_exn ~name:"wire_stream_keep_properties"
    [ S.output "proof_ok" (S.reduce ~f:S.( &: ) checks) ]

let plan () =
  let open Wire in
  let size = Field.v "size" uint8 in
  let codec =
    Codec.v "StreamProof"
      (fun n payload -> (n, payload))
      Codec.
        [
          size $ fst;
          Field.v "payload" (byte_slice ~size:(Field.ref size)) $ snd;
        ]
  in
  Wire_ocaml.Plan.v codec

let counter_population spec finish deliver =
  let stage1 = S.reg spec finish in
  let stage2 = S.reg spec stage1 in
  let queued =
    S.reg_fb spec ~width:3 ~f:(fun count ->
        S.mux2
          S.(stage2 ^: deliver)
          (S.mux2 stage2 S.(count +:. 1) S.(count -:. 1))
          count)
  in
  let population = S.(uresize stage1 3 +: uresize stage2 3 +: queued) in
  (stage1, stage2, queued, population)

let counter_occupancy state pending output_id stage1 stage2 queued population
    input_ready output_valid =
  let reserved_actual, queued_actual = H.Stream.Private.counts state in
  let read_index, write_index = H.Stream.Private.indices state in
  let first_valid, _, _, _ = H.Stream.Private.first state in
  let second_valid, _, _, _ = H.Stream.Private.second state in
  let occupancy_ok =
    S.(
      pending ==: population &: (pending <=:. 4) &: (queued <=:. 4)
      &: (output_valid ==: (queued <>:. 0))
      &: (input_ready ==: (pending <:. 4))
      &: (reserved_actual ==: pending)
      &: (queued_actual ==: queued) &: (first_valid ==: stage1)
      &: (second_valid ==: stage2)
      &: (read_index ==: uresize output_id 2)
      &: (write_index ==: read_index +: uresize queued 2))
  in
  occupancy_ok

let counter_ids state sequence stage1 queued output_id =
  let read_index, _ = H.Stream.Private.indices state in
  let first_valid, first_id, _, _ = H.Stream.Private.first state in
  let second_valid, second_id, _, _ = H.Stream.Private.second state in
  let stages_ordered =
    S.(
      ~:first_valid
      |: (first_id ==: sequence -:. 1)
      &: (~:second_valid |: (second_id ==: sequence -: uresize stage1 64 -:. 1)))
  in
  let slots_ordered =
    List.mapi
      (fun i (id, _, _) ->
        let distance = S.(of_int ~width:2 i -: read_index) in
        S.(
          uresize distance 3 >=: queued
          |: (id ==: output_id +: uresize distance 64)))
      (H.Stream.Private.slots state)
    |> S.reduce ~f:S.( &: )
  in
  (stages_ordered, slots_ordered)

let counter_bounds state queued =
  let read_index, _ = H.Stream.Private.indices state in
  let second_valid, _, _, _ = H.Stream.Private.second state in
  let producer_bounds, slot_bounds = H.Stream.Private.bounds state in
  let bounds_ok =
    S.(~:second_valid |: producer_bounds)
    :: List.mapi
         (fun i bounded ->
           let distance = S.(of_int ~width:2 i -: read_index) in
           S.(uresize distance 3 >=: queued |: bounded))
         slot_bounds
    |> S.reduce ~f:S.( &: )
  in
  bounds_ok

let framing_counts packing spec valid keep last error input_ready =
  (* Independent small transport model: explicit legal masks, popcount and
     saturated public count, rather than the backend's prefix/packing logic. *)
  let reference_count = S.wire 3 and reference_errors = S.wire 3 in
  let bytes = S.(uresize (bit keep 0) 4 +: uresize (bit keep 1) 4) in
  let total = S.(uresize reference_count 4 +: bytes) in
  let too_long = S.(total >:. 3) in
  let count_after = S.mux2 too_long (S.of_int ~width:3 4) (S.uresize total 3) in
  let final_mask_good = S.(keep <>:. 2) in
  let middle_mask_good =
    match packing with
    | H.Stream.Full_beats -> S.(keep ==:. 3)
    | H.Stream.Contiguous -> S.(keep ==:. 1 |: (keep ==:. 3))
  in
  let mask_good = S.mux2 last final_mask_good middle_mask_good in
  let errors_after =
    S.(reference_errors |: concat_lsb [ error; ~:mask_good; too_long ])
  in
  let transfer = S.(valid &: input_ready) in
  S.assign reference_count
    (S.reg spec ~enable:transfer (S.mux2 last (S.zero 3) count_after));
  S.assign reference_errors
    (S.reg spec ~enable:transfer (S.mux2 last (S.zero 3) errors_after));
  (reference_count, reference_errors, count_after, errors_after, transfer)

let framing_queue clock spec finish deliver count_after errors_after =
  let write_index =
    S.reg_fb spec ~enable:finish ~width:2 ~f:(fun n -> S.(n +:. 1))
  in
  let read_index =
    S.reg_fb spec ~enable:deliver ~width:2 ~f:(fun n -> S.(n +:. 1))
  in
  let write_enables =
    List.init 4 (fun i -> S.(finish &: (write_index ==:. i)))
  in
  let expected_slots x =
    let data_spec = Reg_spec.create () ~clock in
    List.map (fun enable -> S.reg data_spec ~enable x) write_enables
  in
  let lengths = expected_slots count_after
  and errors = expected_slots errors_after in
  let expected_length = S.mux read_index lengths
  and expected_errors = S.mux read_index errors in
  (write_index, read_index, lengths, errors, expected_length, expected_errors)

let framing_stages clock spec finish count_after errors_after write_index
    read_index stream =
  let state = H.Stream.Private.state stream in
  let actual_read, actual_write = H.Stream.Private.indices state in
  let first_valid, _, first_length, first_errors =
    H.Stream.Private.first state
  in
  let second_valid, _, second_length, second_errors =
    H.Stream.Private.second state
  in
  let reference_first = S.reg spec finish in
  let data_spec = Reg_spec.create () ~clock in
  let first_expected_length = S.reg data_spec ~enable:finish count_after in
  let first_expected_errors = S.reg data_spec ~enable:finish errors_after in
  let second_expected_length =
    S.reg data_spec ~enable:reference_first first_expected_length
  in
  let second_expected_errors =
    S.reg data_spec ~enable:reference_first first_expected_errors
  in
  let stages_ok =
    S.(
      ~:first_valid
      |: (first_length ==: first_expected_length
         &: (first_errors ==: first_expected_errors))
      &: (~:second_valid
         |: (second_length ==: second_expected_length
            &: (second_errors ==: second_expected_errors)))
      &: (actual_read ==: read_index)
      &: (actual_write
         ==: write_index -: uresize first_valid 2 -: uresize second_valid 2))
  in
  stages_ok

let framing_slots lengths errors stream =
  let state = H.Stream.Private.state stream in
  let _, queued = H.Stream.Private.counts state in
  let actual_read, _ = H.Stream.Private.indices state in
  let slots_ok =
    List.mapi
      (fun i (_, length, errors_actual) ->
        let distance = S.(of_int ~width:2 i -: actual_read) in
        S.(
          uresize distance 3 >=: queued
          |: (length ==: List.nth lengths i
             &: (errors_actual ==: List.nth errors i))))
      (H.Stream.Private.slots state)
    |> S.reduce ~f:S.( &: )
  in
  slots_ok

let counter_checks clock reset finish deliver input_ready output_valid stream =
  let spec = Reg_spec.create ~clear:reset () ~clock in
  let pending =
    S.reg_fb spec ~width:3 ~f:(fun count ->
        S.mux2
          S.(finish ^: deliver)
          (S.mux2 finish S.(count +:. 1) S.(count -:. 1))
          count)
  in
  let output_id =
    S.reg_fb spec ~enable:deliver ~width:64 ~f:(fun n -> S.(n +:. 1))
  in
  let order_ok =
    S.(
      ~:output_valid
      |: (pending <>:. 0 &: (H.Stream.frame_id stream ==: output_id)))
  in
  let stage1, stage2, queued, population =
    counter_population spec finish deliver
  in
  let state = H.Stream.Private.state stream in
  let occupancy_ok =
    counter_occupancy state pending output_id stage1 stage2 queued population
      input_ready output_valid
  in
  let sequence = H.Stream.completed_frames stream in
  let stages_ordered, slots_ordered =
    counter_ids state sequence stage1 queued output_id
  in
  let bounds_ok = counter_bounds state queued in
  let counter_ok =
    S.(
      sequence
      ==: output_id +: uresize pending 64
      &: stages_ordered &: slots_ordered &: bounds_ok)
  in
  (spec, order_ok, occupancy_ok, counter_ok)

let framing_checks packing clock spec valid keep last error input_ready
    output_valid output_ready stream =
  let reference_count, reference_errors, count_after, errors_after, transfer =
    framing_counts packing spec valid keep last error input_ready
  in
  (* Queue the specification at accepted EOF, independently of the decoder's
     internal pipeline and result-memory writes. *)
  let finish = S.(transfer &: last) in
  let deliver = S.(output_valid &: output_ready) in
  let write_index, read_index, lengths, errors, expected_length, expected_errors
      =
    framing_queue clock spec finish deliver count_after errors_after
  in
  let stages_ok =
    framing_stages clock spec finish count_after errors_after write_index
      read_index stream
  in
  let slots_ok = framing_slots lengths errors stream in
  let assembler_ok =
    S.(
      H.Stream.current_length stream
      ==: reference_count
      &: (H.Stream.current_error stream ==: reference_errors))
  in
  let framing_ok =
    S.(
      stages_ok &: slots_ok
      &: (~:output_valid
         |: (H.Stream.frame_length stream
            ==: expected_length
            &: (H.Stream.transport_error stream ==: expected_errors))))
  in
  (assembler_ok, framing_ok)

let output_checks clock spec output_valid output_ready stream =
  let result = H.Stream.decoder stream in
  let snapshot =
    S.concat_lsb
      (output_valid :: H.Stream.frame_id stream
      :: H.Stream.frame_length stream
      :: H.Stream.transport_error stream
      :: H.in_scope result :: H.accepted result :: H.consumed result
      :: List.map snd (H.fields result))
  in
  let previous = S.reg (Reg_spec.create () ~clock) snapshot in
  let stalled = S.reg spec S.(output_valid &: ~:output_ready) in
  let stable = S.(~:stalled |: (snapshot ==: previous)) in
  let zeroes =
    S.reduce ~f:S.( &: )
      (List.map (fun (_, v) -> S.(v ==:. 0)) (H.fields result))
  in
  let rejected =
    S.(
      ~:output_valid |: H.accepted result |: (H.consumed result ==:. 0 &: zeroes))
  in
  let accepted =
    S.(
      ~:output_valid
      |: ~:(H.accepted result)
      |: (H.in_scope result
         &: (H.Stream.transport_error stream ==:. 0)
         &: (H.consumed result <=: uresize (H.Stream.frame_length stream) 63)))
  in
  (stable, rejected, accepted)

let circuit packing =
  let clock = S.input "clock" 1 and reset = S.input "reset" 1 in
  let valid = S.input "input_valid" 1 and last = S.input "input_last" 1 in
  let output_ready = S.input "output_ready" 1 in
  let keep = S.input "input_keep" 2 and error = S.input "input_error" 1 in
  let stream =
    H.Stream.v ~packing ~capacity:1 ~max_length:3 ~clock ~reset ~valid ~last
      ~output_ready ~data:(S.input "input_data" 16) ~keep ~error (plan ())
  in
  let output_valid = H.Stream.output_valid stream in
  let input_ready = H.Stream.input_ready stream in
  let finish = S.(valid &: input_ready &: last) in
  let deliver = S.(output_valid &: output_ready) in
  let spec, order_ok, occupancy_ok, counter_ok =
    counter_checks clock reset finish deliver input_ready output_valid stream
  in
  let assembler_ok, framing_ok =
    framing_checks packing clock spec valid keep last error input_ready
      output_valid output_ready stream
  in
  let stable, rejected, accepted =
    output_checks clock spec output_valid output_ready stream
  in
  Circuit.create_exn ~name:"wire_stream_properties"
    [
      S.output "proof_ok"
        S.(
          reset
          |: (order_ok &: occupancy_ok &: counter_ok &: assembler_ok
            &: framing_ok &: stable &: rejected &: accepted));
    ]
