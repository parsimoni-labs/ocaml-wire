(* Differential test: demo bench cases tested against EverParse C stubs
   via Wire_diff. Each case builds a Wire_diff.packed_test directly. *)

open Wire

type case = {
  id : Demo_bench_cases.id;
  label : string;
  packed : Wire_diff.packed_test;
}

type minimal_raw = { value : int }
type all_ints_raw = { u64be : int64 }
type large_mixed_raw = { timestamp : int64 }
type bf8_raw = { value : int }
type bf16_raw = { bf16_id : int }
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

let diff (type a r) ~(id : Demo_bench_cases.id) ~(label : string)
    ~(codec : r Codec.t) ~(get : bytes -> int -> a) ~(equal : a -> a -> bool)
    ~(check : bytes -> bool) ~(parse : bytes -> a) =
  let size = Codec.wire_size codec in
  let read buf =
    let bytes = Bytes.of_string buf in
    if not (check bytes) then None else Some (parse bytes)
  in
  let ocaml_read buf =
    if String.length buf < size then None
    else Some (get (Bytes.of_string buf) 0)
  in
  let project v =
    let buf = Bytes.create size in
    Codec.encode codec v buf 0;
    get buf 0
  in
  let write _ = None in
  let packed =
    Wire_diff.pack
      (Wire_diff.harness ~name:label ~codec ~read ~write ~project ~equal
         ~ocaml_read ())
  in
  { id; label; packed }

let cases =
  let module D = Demo in
  let module S = Space in
  let module N = Net in
  [
    diff ~id:Minimal ~label:"Minimal.value (uint8)" ~codec:D.minimal_codec
      ~get:(Staged.unstage (Codec.get D.minimal_codec D.bf_minimal_value))
      ~equal:Int.equal ~check:C_stubs.minimal_check
      ~parse:(fun buf ->
        let (r : minimal_raw) = C_stubs.minimal_parse buf in
        r.value);
    diff ~id:All_ints ~label:"AllInts.u64be (uint64be)" ~codec:D.all_ints_codec
      ~get:(Staged.unstage (Codec.get D.all_ints_codec D.bf_ints_u64be))
      ~equal:Int64.equal ~check:C_stubs.allints_check
      ~parse:(fun buf ->
        let (r : all_ints_raw) = C_stubs.allints_parse buf in
        r.u64be);
    diff ~id:Large_mixed ~label:"LargeMixed.timestamp (uint64be)"
      ~codec:D.large_mixed_codec
      ~get:(Staged.unstage (Codec.get D.large_mixed_codec D.bf_mixed_timestamp))
      ~equal:Int64.equal ~check:C_stubs.largemixed_check
      ~parse:(fun buf ->
        let (r : large_mixed_raw) = C_stubs.largemixed_parse buf in
        r.timestamp);
    diff ~id:Bitfield8 ~label:"Bitfield8.value (bf5)" ~codec:D.bf8_codec
      ~get:(Staged.unstage (Codec.get D.bf8_codec D.bf_bf8_value))
      ~equal:Int.equal ~check:C_stubs.bitfield8_check
      ~parse:(fun buf ->
        let (r : bf8_raw) = C_stubs.bitfield8_parse buf in
        r.value);
    diff ~id:Bitfield16 ~label:"Bitfield16.id (bf11)" ~codec:D.bf16_codec
      ~get:(Staged.unstage (Codec.get D.bf16_codec D.bf_bf16_id))
      ~equal:Int.equal ~check:C_stubs.bitfield16_check
      ~parse:(fun buf ->
        let (r : bf16_raw) = C_stubs.bitfield16_parse buf in
        r.bf16_id);
    diff ~id:Bitfield32 ~label:"Bitfield32.priority (bf8)" ~codec:D.bf32_codec
      ~get:(Staged.unstage (Codec.get D.bf32_codec D.bf_bf32_pri))
      ~equal:Int.equal ~check:C_stubs.bitfield32_check
      ~parse:(fun buf ->
        let (r : bf32_raw) = C_stubs.bitfield32_parse buf in
        r.priority);
    diff ~id:Bool_fields ~label:"BoolFields.active (bool bf1)"
      ~codec:D.bool_fields_codec
      ~get:(Staged.unstage (Codec.get D.bool_fields_codec D.bf_bool_active))
      ~equal:Bool.equal ~check:C_stubs.boolfields_check
      ~parse:(fun buf ->
        let (r : bool_raw) = C_stubs.boolfields_parse buf in
        r.active <> 0);
    diff ~id:Clcw_report ~label:"CLCW.report (bf8)" ~codec:S.clcw_codec
      ~get:(Staged.unstage (Codec.get S.clcw_codec S.bf_cw_report))
      ~equal:Int.equal ~check:C_stubs.clcwreport_check
      ~parse:(fun buf ->
        let (r : clcw_raw) = C_stubs.clcwreport_parse buf in
        r.report_value);
    diff ~id:Space_packet_apid ~label:"SpacePacket.apid (bf11)"
      ~codec:S.packet_codec
      ~get:(Staged.unstage (Codec.get S.packet_codec S.bf_sp_apid))
      ~equal:Int.equal ~check:C_stubs.spacepacketapid_check
      ~parse:(fun buf ->
        let (r : packet_raw) = C_stubs.spacepacketapid_parse buf in
        r.apid);
    diff ~id:Ipv4_src ~label:"IPv4.src (uint32be)" ~codec:N.ipv4_codec
      ~get:(Staged.unstage (Codec.get N.ipv4_codec N.bf_ip_src))
      ~equal:Int.equal ~check:C_stubs.ipv4_check
      ~parse:(fun buf ->
        let (r : ipv4_raw) = C_stubs.ipv4_parse buf in
        r.src_addr);
    diff ~id:Tcp_dst_port ~label:"TCP.dst_port (uint16be)" ~codec:N.tcp_codec
      ~get:(Staged.unstage (Codec.get N.tcp_codec N.bf_tcp_dst_port))
      ~equal:Int.equal ~check:C_stubs.tcp_check
      ~parse:(fun buf ->
        let (r : tcp_raw) = C_stubs.tcp_parse buf in
        r.dst_port);
    diff ~id:Tcp_syn ~label:"TCP.syn (bool bf1)" ~codec:N.tcp_codec
      ~get:(Staged.unstage (Codec.get N.tcp_codec N.bf_tcp_syn))
      ~equal:Bool.equal ~check:C_stubs.tcpsyn_check
      ~parse:(fun buf ->
        let (r : tcp_syn_raw) = C_stubs.tcpsyn_parse buf in
        r.syn <> 0);
    diff ~id:Mapped_priority ~label:"Mapped.priority (map fn decode)"
      ~codec:D.mapped_codec
      ~get:(Staged.unstage (Codec.get D.mapped_codec D.bf_mp_priority))
      ~equal:( = ) ~check:C_stubs.mapped_check
      ~parse:(fun buf ->
        let (r : mapped_raw) = C_stubs.mapped_parse buf in
        priority_of_int r.priority);
    diff ~id:Cases_type ~label:"CasesDemo.type (cases variant)"
      ~codec:D.cases_demo_codec
      ~get:(Staged.unstage (Codec.get D.cases_demo_codec D.bf_cd_type))
      ~equal:( = ) ~check:C_stubs.casesdemo_check
      ~parse:(fun buf ->
        let (r : cases_raw) = C_stubs.casesdemo_parse buf in
        ptype_of_int r.packet_type);
    diff ~id:Enum_status ~label:"EnumDemo.status (enum + map)"
      ~codec:D.enum_demo_codec
      ~get:(Staged.unstage (Codec.get D.enum_demo_codec D.bf_en_status))
      ~equal:( = ) ~check:C_stubs.enumdemo_check
      ~parse:(fun buf ->
        let (r : enum_raw) = C_stubs.enumdemo_parse buf in
        status_of_int r.status_code);
    diff ~id:Constrained_data ~label:"Constrained.data (where)"
      ~codec:D.constrained_codec
      ~get:(Staged.unstage (Codec.get D.constrained_codec D.bf_co_data))
      ~equal:Int.equal ~check:C_stubs.constrained_check
      ~parse:(fun buf ->
        let (r : constrained_raw) = C_stubs.constrained_parse buf in
        r.data);
  ]

let find_case id = List.find (fun c -> c.id = id) cases

let verify_of_id id =
  let c = find_case id in
  let rc =
    List.find
      (fun (Demo_bench_cases.Read_case r) -> r.id = id)
      Demo_bench_cases.projection_cases
  in
  let (Demo_bench_cases.Read_case r) = rc in
  fun () ->
    Array.iteri
      (fun i item ->
        let buf = Bytes.unsafe_to_string item in
        match c.packed.test_read buf with
        | Wire_diff.Match -> ()
        | Wire_diff.Both_failed ->
            Fmt.failwith "%s: read failed on item %d" c.label i
        | Wire_diff.Value_mismatch msg ->
            Fmt.failwith "%s: read mismatch on item %d: %s" c.label i msg
        | Wire_diff.Only_c_ok msg ->
            Fmt.failwith "%s: only C succeeded on item %d: %s" c.label i msg
        | Wire_diff.Only_ocaml_ok msg ->
            Fmt.failwith "%s: only OCaml succeeded on item %d: %s" c.label i msg)
      r.dataset.items
