module H = Wire_hardcaml
module Io = Hardware_stream_io.Stream_io

let frame_parts ~packing ~bytes frame =
  match packing with
  | H.Stream.Full_beats -> []
  | H.Stream.Contiguous ->
      List.init
        (min 8 (Bytes.length frame))
        (fun i -> 1 + (Bytes.get_uint8 frame i mod bytes))

let scan_frame ~bytes ~max_length ~parts label oracle sim buf off len =
  if not (Hardware_test_io.bounded buf off len) then None
  else if len > max_length then
    invalid_arg "stream frame outside lane's logical capacity"
  else
    let frame = Bytes.sub buf off len in
    let seed = if len = 0 then 0 else Bytes.get_uint8 frame 0 in
    let events = Io.frame ~bytes ~tag:0 ~parts:(parts frame) frame in
    let result =
      match Io.run ~seed ~stall_burst:(seed land 15) sim events with
      | [ (0, result) ] -> result
      | _ -> Fuzz_gen.failf "%s: streaming result count diverged" label
    in
    if (not result.in_scope) || result.errors <> 0 || result.length <> len then
      Fuzz_gen.failf "%s: streaming framing status diverged" label;
    Option.iter
      (fun (o : Diff_codecs.c_oracle) ->
        let expected = o.standalone buf off len in
        let actual = if result.accepted then result.consumed else -1 in
        if actual <> expected then
          Fuzz_gen.failf "%s: stream consumed %d, EverParse %d" label actual
            expected)
      oracle;
    if result.accepted then Some result.fields else None

let transport_fault ~bytes ~parts label sim raw frame =
  let seed = if Bytes.length raw = 0 then 0 else Bytes.get_uint8 raw 0 in
  let events tag = Io.frame ~bytes ~tag ~parts:(parts frame) frame in
  let corrupt = events 1 in
  let position = seed mod List.length corrupt in
  let input_error = Int.equal bytes 1 || seed land 1 = 0 in
  let corrupt =
    List.mapi
      (fun i event ->
        match event with
        | Io.Beat beat when i = position ->
            Io.Beat
              (if input_error then { beat with error = true }
               else { beat with keep = Hardcaml.Bits.of_int ~width:bytes 2 })
        | event -> event)
      corrupt
  in
  match
    Io.run ~seed ~stall_burst:(seed land 31) sim (events 0 @ corrupt @ events 2)
  with
  | [ (0, before); (1, bad); (2, after) ] ->
      let expected_error = if input_error then 1 else 2 in
      if bad.errors <> expected_error || bad.accepted then
        Fuzz_gen.failf "%s: transport fault escaped rejection" label;
      if
        not (Io.equal_result { before with Io.frame_id = after.frame_id } after)
      then Fuzz_gen.failf "%s: transport fault polluted the next frame" label
  | _ -> Fuzz_gen.failf "%s: transport fault lost frame boundaries" label

let adapter ~packing ~bytes ~capacity ~max_length label plan oracle =
  let sim = Io.v ~packing ~bytes ~capacity ~max_length plan in
  let fields =
    List.map
      (fun (o : Wire_ocaml.Plan.output) -> o.name)
      (Wire_ocaml.Plan.outputs plan)
  in
  let parts = frame_parts ~packing ~bytes in
  let scan = scan_frame ~bytes ~max_length ~parts label oracle sim in
  let ops =
    {
      Diff_ocaml.scan_name =
        (match packing with
        | H.Stream.Full_beats -> "Hardcaml AXIS full"
        | H.Stream.Contiguous -> "Hardcaml AXIS contiguous");
      scan_fields = fields;
      scan;
      scan_batch = None;
    }
  in
  (ops, transport_fault ~bytes ~parts label sim)

let scope_counts = ref []
let selected = Option.value ~default:"all" (Sys.getenv_opt "WIRE_HARDCAML_CASE")

let configured name default =
  match Sys.getenv_opt name with
  | None -> default
  | Some value -> int_of_string value

let bytes = configured "WIRE_HARDCAML_BYTES" 64

(* The capture and logical maximum of a case: the packet stack keeps its
   94-byte scalar prefix of frames up to 65549 bytes. *)
let extents label =
  let capacity, max_length =
    if label = "scan_packet" then (128, 65549) else (256, 256)
  in
  let capacity = configured "WIRE_HARDCAML_CAPACITY" capacity
  and max_length = configured "WIRE_HARDCAML_MAX_LENGTH" max_length in
  Fmt.epr "%s: beat-bytes=%d capture=%d logical-max=%d@." label bytes capacity
    max_length;
  (capacity, max_length)

let case (Diff_codecs.Scan c as case) =
  let plan = Wire_ocaml.Plan.v c.codec in
  let capacity, max_length = extents c.label in
  let oracle = Diff_driver.oracle c.label in
  let full, full_fault =
    adapter ~packing:H.Stream.Full_beats ~bytes ~capacity ~max_length c.label
      plan oracle
  in
  let compact, compact_fault =
    adapter ~packing:H.Stream.Contiguous ~bytes ~capacity ~max_length c.label
      plan oracle
  in
  let software = (List.assoc c.label Ocaml_index.scanners) () in
  let check =
    Diff_ocaml.check_scan ?oracle ~expand:false
      (Diff_ocaml.of_scan_case case)
      (full :: compact :: software)
  in
  let checked = ref 0 and skipped = ref 0 in
  scope_counts := (c.label, max_length, checked, skipped) :: !scope_counts;
  let bounded data =
    if Bytes.length data > max_length then incr skipped
    else (
      incr checked;
      check data)
  in
  if not (Fuzz_gen.afl_mode ()) then (
    List.iter bounded (c.corpus ());
    if c.label = "scan_packet" then
      List.iter bounded (Diff_codecs.packet_payload_corpus ()));
  ( c.label,
    fun raw ->
      let inputs = c.inputs raw in
      List.iter bounded inputs;
      match List.find_opt (fun b -> Bytes.length b <= max_length) inputs with
      | None -> ()
      | Some frame ->
          full_fault raw frame;
          compact_fault raw frame )

let cases =
  List.filter_map
    (fun (Diff_codecs.Scan c as s) ->
      if selected <> "all" && selected <> c.label then None else Some (case s))
    Diff_codecs.scan_cases

let () =
  at_exit (fun () ->
      List.iter
        (fun (label, maximum, checked, skipped) ->
          Fmt.epr "%s: stream-max=%d checked=%d scope-skipped=%d@." label
            maximum !checked !skipped)
        !scope_counts);
  if cases = [] then Fuzz_gen.failf "no stream cases selected";
  Diff_driver.run "hardcaml-stream" cases
