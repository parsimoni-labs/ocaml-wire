(* Export the existing scalar differential reference, not a second packet parser. *)

let discard channel length =
  let buffer = Bytes.create (min 65536 length) in
  let rec loop remaining =
    if remaining > 0 then (
      let count = min remaining (Bytes.length buffer) in
      really_input channel buffer 0 count;
      loop (remaining - count))
  in
  loop length

let first_byte channel =
  match input_byte channel with
  | byte -> Some byte
  | exception End_of_file -> None

let frame channel maximum slot =
  let prefix_size = if slot = 0 then 4 else slot in
  match first_byte channel with
  | None -> None
  | Some byte ->
      let prefix = Bytes.create prefix_size in
      Bytes.set_uint8 prefix 0 byte;
      really_input channel prefix 1 (prefix_size - 1);
      if slot <> 0 then Some (slot, Some prefix)
      else
        let length =
          Int64.(
            to_int (logand (of_int32 (Bytes.get_int32_le prefix 0)) 0xffffffffL))
        in
        if length > maximum then (
          discard channel length;
          Some (length, None))
        else
          let bytes = Bytes.create length in
          really_input channel bytes 0 length;
          Some (length, Some bytes)

let pp_record ppf (id, values) =
  Fmt.pf ppf "{\"frame_id\":%Lu" id;
  List.iter (fun (name, value) -> Fmt.pf ppf ",%S:%d" name value) values;
  Fmt.pf ppf "}@."

(* The reference is the codec: its decoder and the outputs the case states
   for a decoded value, an absent field read as 0. *)
let metadata (Diff_codecs.Scan case) names maximum length bytes =
  let scope = length <= maximum in
  let decoded =
    if not scope then None
    else
      Option.bind bytes (fun b ->
          match
            (Wire.Codec.decode case.codec b 0, Wire.Codec.size_at case.codec b 0)
          with
          | Ok v, Ok n -> Some (case.project v, n)
          | Error _, _ | _, Error _ -> None)
  in
  (* A zero-byte final beat is legal; the codec decides whether an empty
     frame is valid. Nonfinal zero masks are tested by the stream lanes. *)
  let transport_error = if not scope then 4 else 0 in
  let accepted = Option.is_some decoded in
  let consumed = match decoded with Some (_, n) -> n | None -> 0 in
  let fields =
    List.map
      (fun name ->
        let value =
          match decoded with
          | None -> 0
          | Some (values, _) ->
              Option.value ~default:0 (List.assoc_opt name values)
        in
        ("field_" ^ name, value))
      names
  in
  [
    ("frame_length", min length (maximum + 1));
    ("transport_error", transport_error);
    ("accepted", if accepted then 1 else 0);
    ("in_scope", if scope then 1 else 0);
    ("consumed", consumed);
  ]
  @ fields

let export label maximum slot input output =
  let (Diff_codecs.Scan c as case) =
    List.find
      (fun (Diff_codecs.Scan c) -> c.label = label)
      Diff_codecs.scan_cases
  in
  let names = List.map fst (Wire_ocaml.outputs c.codec) in
  let oracle = Diff_driver.oracle label in
  Diff_driver.report_oracle "cloud reference";
  let check =
    Diff_ocaml.check_scan ?oracle ~strides:[] ~expand:false
      (Diff_ocaml.of_scan_case case)
      ((List.assoc label Ocaml_index.scanners) ())
  in
  let rec loop id =
    match frame input maximum slot with
    | None -> id
    | Some (length, bytes) ->
        Option.iter check bytes;
        let values = metadata case names maximum length bytes in
        pp_record output (id, values);
        loop (Int64.succ id)
  in
  let count = loop 0L in
  if count = 0L then invalid_arg "cloud reference: empty corpus";
  Fmt.epr "Exported %Lu ordered reference records@." count

let () =
  match Array.to_list Sys.argv with
  | [ _; label; maximum; slot; input; output ] ->
      let maximum = int_of_string maximum and slot = int_of_string slot in
      if maximum < 0 || maximum > 16 * 1024 * 1024 || slot < 0 || slot > maximum
      then
        invalid_arg
          "cloud reference: invalid extent or slot (0 means u32le framing)";
      In_channel.with_open_bin input (fun channel ->
          Out_channel.with_open_bin output (fun destination ->
              export label maximum slot channel
                (Format.formatter_of_out_channel destination)))
  | _ ->
      invalid_arg "usage: reference.exe CASE MAX_LENGTH SLOT INPUT OUTPUT.jsonl"
