open Hardcaml
open Base.Int.O
module H = Wire_hardcaml
module S = Signal

type beat = {
  data : bytes;
  keep : Bits.t;
  last : bool;
  error : bool;
  tag : int;
}

type event = Beat of beat | Reset

type result = {
  in_scope : bool;
  accepted : bool;
  consumed : int;
  length : int;
  frame_id : int64;
  errors : int;
  fields : (string * int) list;
}

let equal_result a b =
  Bool.equal a.in_scope b.in_scope
  && Bool.equal a.accepted b.accepted
  && Int.equal a.consumed b.consumed
  && Int.equal a.length b.length
  && Int64.equal a.frame_id b.frame_id
  && Int.equal a.errors b.errors
  && Hardware_test_io.equal_fields a.fields b.fields

type statistics = { max_pending : int; reset_cancelled : int }

type t = {
  sim : Cyclesim.t_port_list;
  bytes : int;
  fields : string list;
  mutable statistics : statistics;
}

let statistics t = t.statistics

let v ~packing ~bytes ~capacity ~max_length plan =
  let stream =
    H.Stream.v ~packing ~capacity ~max_length ~clock:(S.input "clock" 1)
      ~reset:(S.input "reset" 1) ~valid:(S.input "input_valid" 1)
      ~data:(S.input "input_data" (bytes * 8))
      ~keep:(S.input "input_keep" bytes)
      ~last:(S.input "input_last" 1) ~error:(S.input "input_error" 1)
      ~output_ready:(S.input "output_ready" 1) plan
  in
  let fields = List.map fst (H.fields (H.Stream.decoder stream)) in
  let sim =
    Cyclesim.create (H.Stream.to_circuit ~name:"wire_stream_test" stream)
  in
  { sim; bytes; fields; statistics = { max_pending = 0; reset_cancelled = 0 } }

let bit b = if b then Bits.vdd else Bits.gnd
let bool x = Bits.to_bool !x
let native x = Bits.to_int !x

let frame ~bytes ~tag ~parts data =
  let length = Bytes.length data in
  let rec loop offset parts =
    if offset = length && offset > 0 then []
    else
      let size, rest =
        match parts with
        | n :: rest -> (Base.Int.min n (length - offset), rest)
        | [] -> (Base.Int.min bytes (length - offset), [])
      in
      if size < 0 || size > bytes || (size = 0 && length > 0) then
        invalid_arg "invalid stream split";
      let last = offset + size = length in
      let keep = Bits.concat_lsb (List.init bytes (fun i -> bit (i < size))) in
      Beat { data = Bytes.sub data offset size; keep; last; error = false; tag }
      :: (if last then [] else loop (offset + size) rest)
  in
  loop 0 parts

type ports = {
  reset : Bits.t ref;
  valid : Bits.t ref;
  data : Bits.t ref;
  keep : Bits.t ref;
  last : Bits.t ref;
  error : Bits.t ref;
  ready : Bits.t ref;
  input_ready : Bits.t ref;
  output_valid : Bits.t ref;
  scope : Bits.t ref;
  accepted : Bits.t ref;
  consumed : Bits.t ref;
  length : Bits.t ref;
  errors : Bits.t ref;
  frame_id : Bits.t ref;
  values : (string * Bits.t ref) list;
}

let ports t =
  let input name =
    match List.assoc_opt name (Cyclesim.in_ports t.sim) with
    | Some port -> port
    | None -> ref Bits.gnd
  in
  let before name = Cyclesim.out_port ~clock_edge:Side.Before t.sim name in
  {
    reset = input "reset";
    valid = input "input_valid";
    data = input "input_data";
    keep = input "input_keep";
    last = input "input_last";
    error = input "input_error";
    ready = input "output_ready";
    input_ready = before "input_ready";
    output_valid = before "output_valid";
    scope = before "in_scope";
    accepted = before "accepted";
    consumed = before "consumed";
    length = before "frame_length";
    errors = before "transport_error";
    frame_id = before "frame_id";
    values = List.map (fun n -> (n, before ("field_" ^ n))) t.fields;
  }

let snapshot (p : ports) =
  {
    in_scope = bool p.scope;
    accepted = bool p.accepted;
    consumed = native p.consumed;
    length = native p.length;
    frame_id = Bits.to_int64 !(p.frame_id);
    errors = native p.errors;
    fields = List.map (fun (n, port) -> (n, native port)) p.values;
  }

type driver = {
  device : t;
  ports : ports;
  events : event array;
  stalls : bool;
  bubbles : bool;
  seed : int;
  stall_burst : int;
  mutable index : int;
  mutable pending : bool;
  mutable cycle : int;
  expected : (int * int64 * int) Queue.t;
  mutable results : (int * result) list;
  mutable max_pending : int;
  mutable reset_cancelled : int;
  mutable stalled : result option;
  mutable idle : int;
  mutable sequence : int64;
}

let resetting d =
  d.index < Array.length d.events
  && match d.events.(d.index) with Reset -> true | Beat _ -> false

let start_beat d reset =
  if
    (not d.pending)
    && d.index < Array.length d.events
    && (not reset)
    && ((not d.bubbles) || (d.cycle + d.seed) mod 5 <> 2)
  then d.pending <- true

let drive_beat d =
  match d.events.(d.index) with
  | Reset -> assert false
  | Beat beat ->
      d.ports.data :=
        Hardware_test_io.word ~capacity:d.device.bytes
          ~padding:(fun i -> ((i * 73) + (d.index * 19) + d.seed) land 255)
          beat.data 0 (Bytes.length beat.data);
      d.ports.keep := beat.keep;
      d.ports.last := bit beat.last;
      d.ports.error := bit beat.error

let drive_inputs d =
  let reset = resetting d in
  start_beat d reset;
  d.ports.reset := bit reset;
  d.ports.valid := bit d.pending;
  let ready =
    (not d.stalls) || (d.cycle >= d.stall_burst && (d.cycle + d.seed) mod 7 <> 3)
  in
  d.ports.ready := bit ready;
  if d.pending then drive_beat d;
  (reset, ready)

let reset_driver d =
  if bool d.ports.input_ready || bool d.ports.output_valid then
    failwith "stream exposed a transfer during reset";
  d.reset_cancelled <- d.reset_cancelled + Queue.length d.expected;
  Queue.clear d.expected;
  d.sequence <- 0L;
  d.stalled <- None;
  d.pending <- false;
  d.index <- d.index + 1

let check_stalled d =
  Option.iter
    (fun old ->
      if
        (not (bool d.ports.output_valid))
        || not (equal_result (snapshot d.ports) old)
      then failwith "stream metadata changed under backpressure")
    d.stalled

let accept_input d =
  if d.pending && bool d.ports.input_ready then (
    (match d.events.(d.index) with
    | Beat beat when beat.last ->
        Queue.add (beat.tag, d.sequence, d.cycle) d.expected;
        d.sequence <- Int64.succ d.sequence
    | _ -> ());
    d.index <- d.index + 1;
    d.pending <- false)

let check_result d completed id (result : result) =
  if d.cycle - completed < 3 || ((not d.stalls) && d.cycle - completed <> 3)
  then failwith "stream result latency diverged";
  if not (Int64.equal result.frame_id id) then
    failwith "stream reordered, lost or duplicated frame IDs";
  if
    (not result.accepted)
    && (result.consumed <> 0 || List.exists (fun (_, v) -> v <> 0) result.fields)
  then failwith "stream published rejected speculative metadata"

let accept_output d ready =
  if bool d.ports.output_valid && ready then (
    if Queue.is_empty d.expected then
      failwith "stream emitted before TLAST or duplicated a result";
    let tag, id, completed = Queue.take d.expected in
    let result = snapshot d.ports in
    check_result d completed id result;
    d.results <- (tag, result) :: d.results)

let observe_cycle d ~reset ~ready =
  if reset then reset_driver d
  else (
    check_stalled d;
    if (not d.stalls) && not (bool d.ports.input_ready) then
      failwith "stream inserted an ingress bubble";
    accept_input d;
    accept_output d ready;
    d.max_pending <- max d.max_pending (Queue.length d.expected);
    d.stalled <-
      (if bool d.ports.output_valid && not ready then Some (snapshot d.ports)
       else None))

let run_cycle d =
  let reset, ready = drive_inputs d in
  Cyclesim.cycle_check d.device.sim;
  Cyclesim.cycle_before_clock_edge d.device.sim;
  observe_cycle d ~reset ~ready;
  Cyclesim.cycle_at_clock_edge d.device.sim;
  Cyclesim.cycle_after_clock_edge d.device.sim;
  if
    d.index = Array.length d.events
    && (not d.pending) && Queue.is_empty d.expected
  then d.idle <- d.idle + 1
  else d.idle <- 0;
  d.cycle <- d.cycle + 1

let run ?(stalls = true) ?(bubbles = true) ?(seed = 0) ?(stall_burst = 0) t
    events =
  let d =
    {
      device = t;
      ports = ports t;
      events = Array.of_list (Reset :: events);
      stalls;
      bubbles;
      seed;
      stall_burst;
      index = 0;
      pending = false;
      cycle = 0;
      expected = Queue.create ();
      results = [];
      max_pending = 0;
      reset_cancelled = 0;
      stalled = None;
      idle = 0;
      sequence = 0L;
    }
  in
  let limit = 20 * (Array.length d.events + 10) in
  while d.idle < 5 && d.cycle < limit do
    run_cycle d
  done;
  if d.idle < 5 then failwith "stream failed to drain under bounded stalls";
  t.statistics <-
    { max_pending = d.max_pending; reset_cancelled = d.reset_cancelled };
  List.rev d.results
