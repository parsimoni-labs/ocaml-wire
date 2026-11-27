open Hardcaml

let mask = 0x3fffffffffffffffL
let integer value = Bits.to_int !value
let boolean value = Bits.to_bool !value
let uint64 value = Bits.to_int64 !value
let bit value = Bits.of_int ~width:1 (if value then 1 else 0)

type expected = { accepted : bool; consumed : int; contribution : int64 }

let equal_snapshot (v, s, a, n, c) (v', s', a', n', c') =
  Bool.equal v v' && Bool.equal s s' && Bool.equal a a' && Int.equal n n'
  && Int64.equal c c'

let expected bytes =
  match
    ( Wire.Codec.decode Packet_stack.codec bytes 0,
      Wire.Codec.size_at Packet_stack.codec bytes 0 )
  with
  | Error _, _ | _, Error _ ->
      { accepted = false; consumed = 0; contribution = 0L }
  | Ok _, Ok consumed ->
      {
        accepted = true;
        consumed;
        contribution =
          Int64.of_int (Stack_read.fold 0 bytes 0 (Bytes.length bytes));
      }

let run ~stalls ~resets frames =
  let sim = Cyclesim.create (Hw_bench.circuit ()) in
  let input name = Cyclesim.in_port sim name in
  let after name = Cyclesim.out_port sim name in
  let before name = Cyclesim.out_port ~clock_edge:Side.Before sim name in
  let reset = input "reset" and input_valid = input "input_valid" in
  let data = input "input_data" and length = input "input_length" in
  let ready = input "output_ready" in
  let input_ready = before "input_ready"
  and output_valid = before "output_valid" in
  let scope = before "output_scope" and accepted = before "output_accepted" in
  let consumed = before "output_consumed"
  and contribution = before "output_contribution" in
  let checksum = after "checksum" and delivered = after "frames" in
  let queue = Queue.create () in
  let index = ref 0
  and pending = ref false
  and total = ref 0L
  and count = ref 0L in
  let previous_stall = ref None in
  let limit = (20 * Array.length frames) + 100 in
  let cycle = ref 0 and done_ = ref false in
  while (not !done_) && !cycle < limit do
    let resetting = !cycle = 0 || (resets && !cycle mod 97 = 33) in
    reset := bit resetting;
    let output_ready = (not stalls) || !cycle mod 7 <> 2 in
    ready := bit output_ready;
    if
      (not !pending)
      && !index < Array.length frames
      && ((not stalls) || !cycle mod 5 <> 1)
    then pending := true;
    input_valid := bit !pending;
    if !pending then (
      let frame = frames.(!index) in
      data :=
        Hardware_test_io.word ~capacity:64
          ~padding:(fun i -> i * 73 land 255)
          frame 0 (Bytes.length frame);
      length := Bits.of_int ~width:7 (Bytes.length frame));
    Cyclesim.cycle_check sim;
    Cyclesim.cycle_before_clock_edge sim;
    let snapshot =
      ( boolean output_valid,
        boolean scope,
        boolean accepted,
        integer consumed,
        uint64 contribution )
    in
    if resetting then (
      Queue.clear queue;
      total := 0L;
      count := 0L;
      previous_stall := None)
    else (
      Option.iter
        (fun old ->
          if not (equal_snapshot snapshot old) then
            failwith "hardware metadata changed during output backpressure")
        !previous_stall;
      if !pending && boolean input_ready then (
        Queue.add (expected frames.(!index)) queue;
        incr index;
        pending := false);
      if boolean output_valid && output_ready then (
        if Queue.is_empty queue then
          failwith "hardware produced an extra result";
        let e = Queue.take queue in
        if
          (not (boolean scope))
          || (not (Bool.equal (boolean accepted) e.accepted))
          || (not (Int.equal (integer consumed) e.consumed))
          || not (Int64.equal (uint64 contribution) e.contribution)
        then failwith "registered hardware result diverged";
        total := Int64.add !total e.contribution;
        count := Int64.succ !count);
      previous_stall :=
        if boolean output_valid && not output_ready then Some snapshot else None);
    Cyclesim.cycle_at_clock_edge sim;
    Cyclesim.cycle_after_clock_edge sim;
    if
      (not (Int64.equal (uint64 checksum) (Int64.logand !total mask)))
      || not (Int64.equal (uint64 delivered) !count)
    then failwith "hardware checksum sink advanced without a transfer";
    incr cycle;
    done_ :=
      !index = Array.length frames && (not !pending) && Queue.is_empty queue
  done;
  if not !done_ then failwith "hardware failed to drain under bounded stalls";
  if
    (not stalls) && (not resets)
    && Array.length frames > 0
    && !cycle <> Array.length frames + 5
  then failwith "hardware did not sustain one frame per cycle";
  Fmt.pr "frames=%Ld cycles=%d checksum=%Ld stalls=%b resets=%b@." !count !cycle
    (Int64.logand !total mask) stalls resets;
  Int64.logand !total mask

let corpus file =
  let raw =
    In_channel.with_open_bin file In_channel.input_all |> Bytes.of_string
  in
  if Bytes.length raw mod 64 <> 0 then
    invalid_arg "hardware corpus must contain complete 64-byte frames";
  Array.init (Bytes.length raw / 64) (fun i -> Bytes.sub raw (64 * i) 64)

let () =
  match Array.to_list Sys.argv with
  | [ _; file ] ->
      let frames = corpus file in
      let expected =
        Array.fold_left
          (fun acc frame -> Int64.add acc (expected frame).contribution)
          0L frames
        |> fun x -> Int64.logand x mask
      in
      let actual = run ~stalls:false ~resets:false frames in
      if actual <> expected then failwith "corpus checksum diverged";
      ignore (run ~stalls:true ~resets:false frames);
      let sample = Array.sub frames 0 (min 128 (Array.length frames)) in
      ignore (run ~stalls:true ~resets:true sample)
  | _ -> invalid_arg "usage: test.exe SOFTWARE_CORPUS.bin"
