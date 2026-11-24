open Hardcaml
open Base.Int.O
module S = Signal
module H = Wire_hardcaml

let capacity =
  match Sys.getenv_opt "WIRE_HARDCAML_CAPACITY" with
  | None -> 64
  | Some value -> int_of_string value

let length_width = 9
let bool port = Bits.to_bool !port
let native port = Bits.to_int !port

type mode = Combinational | Registered | Pipelined

type ports = {
  sim : Cyclesim.t_port_list;
  stateful : bool;
  input : Bits.t ref;
  length : Bits.t ref;
  enable : Bits.t ref option;
  accepted : Bits.t ref;
  scope : Bits.t ref;
  consumed : Bits.t ref;
  outputs : (string * Bits.t ref) list;
}

let circuit mode label plan =
  let stateful = match mode with Combinational -> false | _ -> true in
  let data = S.input "data" (8 * capacity)
  and length = S.input "length" length_width in
  let controls =
    if stateful then
      let clock = S.input "clock" 1 and enable = S.input "enable" 1 in
      Some (Reg_spec.create () ~clock, enable)
    else None
  in
  let pipeline = match mode with Pipelined -> controls | _ -> None in
  let decoder = H.v ?pipeline ~capacity ~data ~length plan in
  let decoder =
    match (mode, controls) with
    | Registered, Some (spec, enable) -> H.register ~enable spec decoder
    | _ -> decoder
  in
  (stateful, H.fields decoder, H.to_circuit ~name:("wire_" ^ label) decoder)

let ports mode label plan =
  let stateful, fields, circuit = circuit mode label plan in
  let sim = Cyclesim.create circuit in
  (* A capacity below the codec's minimum size can make data irrelevant. *)
  let input =
    match List.assoc_opt "data" (Cyclesim.in_ports sim) with
    | Some port -> port
    | None -> ref (Bits.zero (8 * capacity))
  in
  {
    sim;
    stateful;
    input;
    length = Cyclesim.in_port sim "length";
    enable = (if stateful then Some (Cyclesim.in_port sim "enable") else None);
    accepted = Cyclesim.out_port sim "accepted";
    scope = Cyclesim.out_port sim "in_scope";
    consumed = Cyclesim.out_port sim "consumed";
    outputs =
      List.map
        (fun (name, _) -> (name, Cyclesim.out_port sim ("field_" ^ name)))
        fields;
  }

let capture p on =
  Option.iter (fun port -> port := if on then Bits.vdd else Bits.gnd) p.enable

let snapshot p =
  ( bool p.scope,
    bool p.accepted,
    native p.consumed,
    List.map (fun (_, port) -> native port) p.outputs )

let equal_snapshot (scope, accepted, consumed, fields)
    (scope', accepted', consumed', fields') =
  Bool.equal scope scope'
  && Bool.equal accepted accepted'
  && Int.equal consumed consumed'
  && List.equal Int.equal fields fields'

let check_capacity p label =
  List.iter
    (fun out_of_scope ->
      p.input := Bits.ones (8 * capacity);
      p.length := Bits.of_int ~width:length_width out_of_scope;
      capture p true;
      Cyclesim.cycle p.sim;
      if
        bool p.scope || bool p.accepted
        || native p.consumed <> 0
        || List.exists (fun (_, port) -> native port <> 0) p.outputs
      then
        Fuzz_gen.failf "%s: capacity violation escaped the hardware ABI" label)
    [ capacity + 1; 511 ]

let evaluate p label padding buf off len =
  p.input := Hardware_test_io.word ~capacity ~padding buf off len;
  p.length := Bits.of_int ~width:length_width len;
  capture p true;
  Cyclesim.cycle p.sim;
  if p.stateful then (
    let held = snapshot p in
    capture p false;
    (p.input := Bits.(~:(!(p.input))));
    p.length := Bits.of_int ~width:length_width 511;
    Cyclesim.cycle p.sim;
    if not (equal_snapshot (snapshot p) held) then
      Fuzz_gen.failf "%s: decoder snapshot changed while disabled" label);
  if not (bool p.scope) then
    Fuzz_gen.failf "%s: in-scope frame rejected by hardware ABI" label;
  let ok = bool p.accepted in
  if
    (not ok)
    && (native p.consumed <> 0
       || List.exists (fun (_, port) -> native port <> 0) p.outputs)
  then Fuzz_gen.failf "%s: rejected hardware result was not zeroed" label;
  let consumed = if ok then native p.consumed else -1 in
  let values =
    if ok then
      Some (List.map (fun (name, port) -> (name, native port)) p.outputs)
    else None
  in
  (consumed, values)

let reference_consumed codec oracle buf off len =
  match oracle with
  | Some (o : Diff_codecs.c_oracle) -> o.standalone buf off len
  | None -> (
      let frame = Bytes.sub buf off len in
      match Wire.Codec.decode codec frame 0 with
      | Error _ -> -1
      | Ok _ -> Wire.Codec.wire_size_at codec frame 0)

let equal_evaluation (consumed, fields) (consumed', fields') =
  Int.equal consumed consumed'
  && Option.equal Hardware_test_io.equal_fields fields fields'

let scan_input p label codec oracle buf off len =
  if not (Hardware_test_io.bounded buf off len) then None
  else (
    if len > capacity then
      invalid_arg "hardware capacity is outside this lane's scope";
    let result = evaluate p label (fun _ -> 0) buf off len in
    List.iter
      (fun padding ->
        if not (equal_evaluation (evaluate p label padding buf off len) result)
        then
          Fuzz_gen.failf "%s: padded bus bits changed the hardware result" label)
      [ (fun _ -> 255); (fun i -> ((i * 73) + (len * 19)) land 255) ];
    let expected = reference_consumed codec oracle buf off len in
    if fst result <> expected then
      Fuzz_gen.failf "%s: hardware consumed %d, reference %d" label (fst result)
        expected;
    snd result)

let simulator mode label codec plan oracle =
  let suffix =
    match mode with
    | Combinational -> ""
    | Registered -> "_registered"
    | Pipelined -> "_pipelined"
  in
  let label = label ^ suffix in
  let p = ports mode label plan in
  check_capacity p label;
  {
    Diff_ocaml.scan_name =
      (match mode with
      | Combinational -> "Hardcaml Cyclesim"
      | Registered -> "Hardcaml registered Cyclesim"
      | Pipelined -> "Hardcaml pipelined Cyclesim");
    scan_fields = List.map fst p.outputs;
    scan = scan_input p label codec oracle;
    scan_batch = None;
    scan_index = None;
  }

let counts = ref []

let selected_case =
  Option.value ~default:"all" (Sys.getenv_opt "WIRE_HARDCAML_CASE")

let cases =
  List.filter_map
    (fun (Diff_codecs.Scan c as case) ->
      if
        (not (String.equal selected_case "all"))
        && not (String.equal selected_case c.label)
      then None
      else
        let plan = Wire_ocaml.Plan.v c.codec in
        let oracle = Diff_driver.oracle c.label in
        let hardware = simulator Combinational c.label c.codec plan oracle in
        let registered = simulator Registered c.label c.codec plan oracle in
        let pipelined = simulator Pipelined c.label c.codec plan oracle in
        let software = (List.assoc c.label Ocaml_index.scanners) () in
        let check =
          Diff_ocaml.check_scan ?oracle ~expand:false
            (Diff_ocaml.of_scan_case case)
            (hardware :: registered :: pipelined :: software)
        in
        let out_of_scope = ref 0 and tested = ref 0 in
        counts := (c.label, tested, out_of_scope) :: !counts;
        let bounded b =
          if Bytes.length b > capacity then incr out_of_scope
          else (
            incr tested;
            check b)
        in
        List.iter bounded (c.corpus ());
        Some (c.label, fun raw -> List.iter bounded (c.inputs raw)))
    Diff_codecs.scan_cases

let () =
  Fmt.epr
    "Hardcaml lane capacity: %d bytes; larger frames are out of scope, not \
     codec rejections.@."
    capacity;
  at_exit (fun () ->
      List.iter
        (fun (label, tested, skipped) ->
          Fmt.epr "%s: checked=%d capacity-skipped=%d@." label !tested !skipped)
        !counts);
  (match cases with
  | [] -> Fuzz_gen.failf "no Hardcaml cases selected"
  | _ -> ());
  Diff_driver.run "hardcaml" cases
