open Wire
open Demo_bench_cases

type case = { id : Demo_bench_cases.id; label : string; verify : unit -> unit }

let check_result ~label ~phase ~index = function
  | Wire_diff.Match -> ()
  | Wire_diff.Both_failed ->
      Fmt.failwith "%s: %s failed on item %d" label phase index
  | Wire_diff.Value_mismatch msg ->
      Fmt.failwith "%s: %s mismatch on item %d: %s" label phase index msg
  | Wire_diff.Only_c_ok msg ->
      Fmt.failwith "%s: %s only C succeeded on item %d: %s" label phase index
        msg
  | Wire_diff.Only_ocaml_ok msg ->
      Fmt.failwith "%s: %s only OCaml succeeded on item %d: %s" label phase
        index msg

let read_with_getter ~size get buf =
  if String.length buf < size then None else Some (get (Bytes.of_string buf) 0)

let c_read ~check ~parse ~convert buf =
  let bytes = Bytes.of_string buf in
  if not (check bytes) then None else Some (convert (parse bytes))

let project_with_getter codec get value =
  let buf = Bytes.create (Codec.wire_size codec) in
  Codec.encode codec value buf 0;
  get buf 0

let write_with_setter ~template ~offset ~set ~read ~equal value =
  let bytes = Bytes.copy template in
  set bytes offset value;
  let buf = Bytes.unsafe_to_string bytes in
  match read buf with
  | Some parsed when equal parsed value -> Some buf
  | _ -> None

let packed_harness ~name ~codec ~template ~offset ~get ~set ~parse ~check
    ~convert ~equal =
  let read = c_read ~check ~parse ~convert in
  let ocaml_read = read_with_getter ~size:(Codec.wire_size codec) get in
  let write = write_with_setter ~template ~offset ~set ~read ~equal in
  let project = project_with_getter codec get in
  Wire_diff.pack
    (Wire_diff.harness ~name ~codec ~read ~write ~project ~equal ~ocaml_read ())

let verify_items ~label packed items () =
  Array.iteri
    (fun index item ->
      let buf = Bytes.unsafe_to_string item in
      check_result ~label ~phase:"read" ~index (packed.Wire_diff.test_read buf);
      check_result ~label ~phase:"write" ~index
        (packed.Wire_diff.test_write buf);
      check_result ~label ~phase:"roundtrip" ~index
        (packed.Wire_diff.test_roundtrip buf))
    items

let make_case ~parse ~check ~convert (Demo_bench_cases.Read_case case) =
  let packed =
    packed_harness ~name:case.label ~codec:case.codec
      ~template:case.write_template ~offset:case.write_offset ~get:case.get
      ~set:case.set ~parse ~check ~convert ~equal:case.equal
  in
  let verify = verify_items ~label:case.label packed case.dataset.items in
  { id = case.id; label = case.label; verify }

type minimal_raw = { value : int }
type all_ints_raw = { u64be : int64 }
type large_mixed_raw = { timestamp : int64 }
type bf8_raw = { value : int }
type bf16_raw = { id : int }
type bf32_raw = { priority : int }
type bool_raw = { active : int }
type clcw_raw = { report_value : int }
type packet_raw = { apid : int }
type ipv4_raw = { src_addr : int }
type tcp_raw = { dst_port : int }
type tcp_syn_raw = { syn : int }
type mapped_raw = { priority : int }
type cases_raw = { packet_type : int }
type enum_raw = { status_code : int }
type constrained_raw = { version : int; data : int }

let ptype_of_int = function 0 -> Demo.Telemetry | _ -> Demo.Telecommand

let priority_of_int = function
  | 0 -> Demo.Low
  | 1 -> Demo.Medium
  | 2 -> Demo.High
  | _ -> Demo.Critical

let status_of_int = function 0 -> `Ok | 1 -> `Warn | 2 -> `Err | _ -> `Crit

let make_case_for_id : Demo_bench_cases.read_case -> case = function
  | Demo_bench_cases.Read_case ({ id = Minimal; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.minimal_parse buf : minimal_raw))
        ~check:C_stubs.minimal_check
        ~convert:(fun { value } -> value)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = All_ints; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.allints_parse buf : all_ints_raw))
        ~check:C_stubs.allints_check
        ~convert:(fun { u64be } -> u64be)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Large_mixed; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.largemixed_parse buf : large_mixed_raw))
        ~check:C_stubs.largemixed_check
        ~convert:(fun { timestamp } -> timestamp)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Bitfield8; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.bitfield8_parse buf : bf8_raw))
        ~check:C_stubs.bitfield8_check
        ~convert:(fun { value } -> value)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Bitfield16; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.bitfield16_parse buf : bf16_raw))
        ~check:C_stubs.bitfield16_check
        ~convert:(fun { id } -> id)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Bitfield32; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.bitfield32_parse buf : bf32_raw))
        ~check:C_stubs.bitfield32_check
        ~convert:(fun { priority } -> priority)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Bool_fields; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.boolfields_parse buf : bool_raw))
        ~check:C_stubs.boolfields_check
        ~convert:(fun { active } -> active <> 0)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Clcw_report; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.clcwreport_parse buf : clcw_raw))
        ~check:C_stubs.clcwreport_check
        ~convert:(fun { report_value } -> report_value)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Space_packet_apid; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.spacepacketapid_parse buf : packet_raw))
        ~check:C_stubs.spacepacketapid_check
        ~convert:(fun { apid } -> apid)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Ipv4_src; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.ipv4_parse buf : ipv4_raw))
        ~check:C_stubs.ipv4_check
        ~convert:(fun { src_addr } -> src_addr)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Tcp_dst_port; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.tcp_parse buf : tcp_raw))
        ~check:C_stubs.tcp_check
        ~convert:(fun { dst_port } -> dst_port)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Tcp_syn; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.tcpsyn_parse buf : tcp_syn_raw))
        ~check:C_stubs.tcpsyn_check
        ~convert:(fun { syn } -> syn <> 0)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Mapped_priority; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.mapped_parse buf : mapped_raw))
        ~check:C_stubs.mapped_check
        ~convert:(fun { priority } -> priority_of_int priority)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Cases_type; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.casesdemo_parse buf : cases_raw))
        ~check:C_stubs.casesdemo_check
        ~convert:(fun { packet_type } -> ptype_of_int packet_type)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Enum_status; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.enumdemo_parse buf : enum_raw))
        ~check:C_stubs.enumdemo_check
        ~convert:(fun { status_code } -> status_of_int status_code)
        (Demo_bench_cases.Read_case case)
  | Demo_bench_cases.Read_case ({ id = Constrained_data; _ } as case) ->
      make_case
        ~parse:(fun buf -> (C_stubs.constrained_parse buf : constrained_raw))
        ~check:C_stubs.constrained_check
        ~convert:(fun { data; _ } -> data)
        (Demo_bench_cases.Read_case case)

let cases = List.map make_case_for_id Demo_bench_cases.projection_cases

let find_case id =
  match List.find_opt (fun case -> case.id = id) cases with
  | Some case -> case
  | None -> Fmt.failwith "missing demo diff case"

let verify_of_id id = (find_case id).verify
