open Wire
module Slice = Bytesrw.Bytes.Slice

let n_data = 1024

(* Project a codec to a 3D struct that extracts only the given fields.
   All other fields become anonymous (validation-only, no WireSet callback).
   This derives the projection from the codec's own field list, avoiding
   hand-copied field layouts that can drift from the codec definition. *)
let project (type r) (codec : r Codec.t) ~name ~keep =
  Everparse.Raw.struct_project (Everparse.struct_of_codec codec) ~name ~keep

let project_one (type r a) (codec : r Codec.t) ~name ~keep:(f : a Field.t) =
  project codec ~name ~keep:[ Field.Named f ]

type dataset = { items : bytes array; packed : bytes; n_items : int }

type id =
  | Minimal
  | All_ints
  | Large_mixed
  | Bitfield8
  | Bitfield16
  | Bitfield32
  | Bool_fields
  | Clcw_report
  | Space_packet_apid
  | Ipv4_src
  | Tcp_dst_port
  | Tcp_syn
  | Mapped_priority
  | Cases_type
  | Enum_status
  | Constrained_data

type _ c_field_decode =
  | Of_int : (int -> 'a) -> 'a c_field_decode
  | Of_int64 : (int64 -> 'a) -> 'a c_field_decode

type 'a read_case =
  | Read_case : {
      id : id;
      label : string;
      size : int;
      dataset : dataset;
      struct_ : Everparse.struct_;
      codec : 'r Codec.t;
      get : bytes -> int -> 'a;
      set : bytes -> int -> 'a -> unit;
      write_template : bytes;
      write_offset : int;
      write_value : 'a;
      equal : 'a -> 'a -> bool;
      bench_read : bool;
      of_c_field : 'a c_field_decode;
    }
      -> 'a read_case

type packed_case = C : _ read_case -> packed_case
type write_case = { label : string; run : unit -> unit; verify : unit -> unit }

let of_int f = Of_int f
let id_int = Of_int Fun.id
let id_int64 = Of_int64 Fun.id
let bool_of_int = Of_int (fun v -> v <> 0)

let priority_decode =
  of_int (function
    | 0 -> Demo.Low
    | 1 -> Demo.Medium
    | 2 -> Demo.High
    | _ -> Demo.Critical)

let ptype_decode =
  of_int (function 0 -> Demo.Telemetry | _ -> Demo.Telecommand)

let status_decode =
  of_int (function 0 -> `Ok | 1 -> `Warn | 2 -> `Err | _ -> `Crit)

let dataset_of_array items ~size =
  let packed, n_items = Bench_lib.pack items ~size in
  { items; packed; n_items }

let dataset_of_packed packed ~size =
  let n_items = Bytes.length packed / size in
  let items = Array.init n_items (fun i -> Bytes.sub packed (i * size) size) in
  { items; packed; n_items }

let _priority_of_int = function
  | 0 -> Demo.Low
  | 1 -> Demo.Medium
  | 2 -> Demo.High
  | _ -> Demo.Critical

let _int_of_priority = function
  | Demo.Low -> 0
  | Demo.Medium -> 1
  | Demo.High -> 2
  | Demo.Critical -> 3

let minimal_dataset =
  dataset_of_array (Demo.minimal_data n_data) ~size:Demo.minimal_size

let bf8_dataset = dataset_of_array (Demo.bf8_data n_data) ~size:Demo.bf8_size
let bf16_dataset = dataset_of_array (Demo.bf16_data n_data) ~size:Demo.bf16_size

let bool_dataset =
  dataset_of_array (Demo.bool_fields_data n_data) ~size:Demo.bool_fields_size

let bf32_dataset = dataset_of_array (Demo.bf32_data n_data) ~size:Demo.bf32_size

let ints_dataset =
  dataset_of_array (Demo.all_ints_data n_data) ~size:Demo.all_ints_size

let mixed_dataset =
  dataset_of_array (Demo.large_mixed_data n_data) ~size:Demo.large_mixed_size

let clcw_dataset =
  dataset_of_array (Space.clcw_data n_data) ~size:Space.clcw_size

let pkt_dataset =
  dataset_of_array (Space.packet_data n_data) ~size:Space.packet_size

let mapped_dataset =
  dataset_of_packed (Demo.mapped_data n_data) ~size:Demo.mapped_size

let cases_dataset =
  dataset_of_packed (Demo.cases_demo_data n_data) ~size:Demo.cases_demo_size

let enum_dataset =
  dataset_of_packed (Demo.enum_demo_data n_data) ~size:Demo.enum_demo_size

let constrained_dataset =
  dataset_of_packed (Demo.constrained_data n_data) ~size:Demo.constrained_size

let tcp_frame = (Net.tcp_frame_data 1).(0)

let read_eth_payload =
  Staged.unstage (Codec.get Net.ethernet_codec Net.bf_eth_payload)

let read_ip_payload =
  Staged.unstage (Codec.get Net.ipv4_codec Net.bf_ip_payload)

(* Zero-alloc offset readers for the nested-write benches below: replaces
   [Slice.first (Codec.get c f buf base)] (which allocates a [Slice.t]
   per call). *)
let eth_payload_off =
  Staged.unstage (Codec.slice_offset Net.ethernet_codec Net.bf_eth_payload)

let ip_payload_off =
  Staged.unstage (Codec.slice_offset Net.ipv4_codec Net.bf_ip_payload)

let ip_off = Slice.first (read_eth_payload tcp_frame 0)
let tcp_off = Slice.first (read_ip_payload tcp_frame ip_off)

let ipv4_dataset =
  dataset_of_array
    [| Bytes.sub tcp_frame ip_off Net.ipv4_size |]
    ~size:Net.ipv4_size

let tcp_dataset =
  dataset_of_array
    [| Bytes.sub tcp_frame tcp_off Net.tcp_size |]
    ~size:Net.tcp_size

let minimal_struct =
  project_one Demo.minimal_codec ~name:"Minimal" ~keep:Demo.f_minimal_value

let bf8_struct =
  project_one Demo.bf8_codec ~name:"Bitfield8" ~keep:Demo.f_bf8_value

let bf16_struct =
  project_one Demo.bf16_codec ~name:"Bitfield16" ~keep:Demo.f_bf16_id

let bool_fields_struct =
  project_one Demo.bool_fields_codec ~name:"BoolFields" ~keep:Demo.f_bool_active

let bf32_struct =
  project_one Demo.bf32_codec ~name:"Bitfield32" ~keep:Demo.f_bf32_pri

let all_ints_struct =
  project_one Demo.all_ints_codec ~name:"AllInts" ~keep:Demo.f_ints_u64be

let large_mixed_struct =
  project_one Demo.large_mixed_codec ~name:"LargeMixed"
    ~keep:Demo.f_mixed_timestamp

let mapped_struct =
  project_one Demo.mapped_codec ~name:"Mapped" ~keep:Demo.f_mp_priority

let cases_demo_struct =
  project_one Demo.cases_demo_codec ~name:"CasesDemo" ~keep:Demo.f_cd_type

let enum_demo_struct =
  project_one Demo.enum_demo_codec ~name:"EnumDemo" ~keep:Demo.f_en_status

let constrained_struct =
  project_one Demo.constrained_codec ~name:"Constrained" ~keep:Demo.f_co_data

let clcw_report_struct =
  project_one Space.clcw_codec ~name:"CLCWReport" ~keep:Space.cw_report

let space_packet_apid_struct =
  project_one Space.packet_codec ~name:"SpacePacketApid" ~keep:Space.f_sp_apid

let ipv4_struct = project_one Net.ipv4_codec ~name:"IPv4" ~keep:Net.f_ip_src

let tcp_dst_port_struct =
  project_one Net.tcp_codec ~name:"TCP" ~keep:Net.f_tcp_dst_port

let tcp_syn_struct =
  project_one Net.tcp_codec ~name:"TCPSyn" ~keep:Net.f_tcp_syn

let minimal_case =
  let get =
    Staged.unstage (Codec.get Demo.minimal_codec Demo.bf_minimal_value)
  in
  let set =
    Staged.unstage (Codec.set Demo.minimal_codec Demo.bf_minimal_value)
  in
  Read_case
    {
      id = Minimal;
      label = "Minimal.value (uint8)";
      size = Demo.minimal_size;
      dataset = minimal_dataset;
      struct_ = minimal_struct;
      codec = Demo.minimal_codec;
      get;
      set;
      write_template = Bytes.copy minimal_dataset.packed;
      write_offset = 0;
      write_value = 42;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let all_ints_case =
  let get = Staged.unstage (Codec.get Demo.all_ints_codec Demo.bf_ints_u64be) in
  let set = Staged.unstage (Codec.set Demo.all_ints_codec Demo.bf_ints_u64be) in
  Read_case
    {
      id = All_ints;
      label = "AllInts.u64be (uint64be)";
      size = Demo.all_ints_size;
      dataset = ints_dataset;
      struct_ = all_ints_struct;
      codec = Demo.all_ints_codec;
      get;
      set;
      write_template = Bytes.copy ints_dataset.items.(0);
      write_offset = 0;
      write_value = 0x0102_0304_0506_0708L;
      equal = Int64.equal;
      bench_read = true;
      of_c_field = id_int64;
    }

let large_mixed_case =
  let get =
    Staged.unstage (Codec.get Demo.large_mixed_codec Demo.bf_mixed_timestamp)
  in
  let set =
    Staged.unstage (Codec.set Demo.large_mixed_codec Demo.bf_mixed_timestamp)
  in
  Read_case
    {
      id = Large_mixed;
      label = "LargeMixed.timestamp (uint64be)";
      size = Demo.large_mixed_size;
      dataset = mixed_dataset;
      struct_ = large_mixed_struct;
      codec = Demo.large_mixed_codec;
      get;
      set;
      write_template = Bytes.copy mixed_dataset.items.(0);
      write_offset = 0;
      write_value = 0x1122_3344_5566_7788L;
      equal = Int64.equal;
      bench_read = true;
      of_c_field = id_int64;
    }

let bitfield8_case =
  let get = Staged.unstage (Codec.get Demo.bf8_codec Demo.bf_bf8_value) in
  let set = Staged.unstage (Codec.set Demo.bf8_codec Demo.bf_bf8_value) in
  Read_case
    {
      id = Bitfield8;
      label = "Bitfield8.value (bf5)";
      size = Demo.bf8_size;
      dataset = bf8_dataset;
      struct_ = bf8_struct;
      codec = Demo.bf8_codec;
      get;
      set;
      write_template = Bytes.copy bf8_dataset.packed;
      write_offset = 0;
      write_value = 19;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let bitfield16_case =
  let get = Staged.unstage (Codec.get Demo.bf16_codec Demo.bf_bf16_id) in
  let set = Staged.unstage (Codec.set Demo.bf16_codec Demo.bf_bf16_id) in
  Read_case
    {
      id = Bitfield16;
      label = "Bitfield16.id (bf11)";
      size = Demo.bf16_size;
      dataset = bf16_dataset;
      struct_ = bf16_struct;
      codec = Demo.bf16_codec;
      get;
      set;
      write_template = Bytes.copy bf16_dataset.items.(0);
      write_offset = 0;
      write_value = 73;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let bitfield32_case =
  let get = Staged.unstage (Codec.get Demo.bf32_codec Demo.bf_bf32_pri) in
  let set = Staged.unstage (Codec.set Demo.bf32_codec Demo.bf_bf32_pri) in
  Read_case
    {
      id = Bitfield32;
      label = "Bitfield32.priority (bf8)";
      size = Demo.bf32_size;
      dataset = bf32_dataset;
      struct_ = bf32_struct;
      codec = Demo.bf32_codec;
      get;
      set;
      write_template = Bytes.copy bf32_dataset.items.(0);
      write_offset = 0;
      write_value = 17;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let bool_fields_case =
  let get =
    Staged.unstage (Codec.get Demo.bool_fields_codec Demo.bf_bool_active)
  in
  let set =
    Staged.unstage (Codec.set Demo.bool_fields_codec Demo.bf_bool_active)
  in
  Read_case
    {
      id = Bool_fields;
      label = "BoolFields.active (bool bf1)";
      size = Demo.bool_fields_size;
      dataset = bool_dataset;
      struct_ = bool_fields_struct;
      codec = Demo.bool_fields_codec;
      get;
      set;
      write_template = Bytes.copy bool_dataset.packed;
      write_offset = 0;
      write_value = true;
      equal = Bool.equal;
      bench_read = true;
      of_c_field = bool_of_int;
    }

let clcw_case =
  let get = Staged.unstage (Codec.get Space.clcw_codec Space.bf_cw_report) in
  let set = Staged.unstage (Codec.set Space.clcw_codec Space.bf_cw_report) in
  Read_case
    {
      id = Clcw_report;
      label = "CLCW.report (bf8)";
      size = Space.clcw_size;
      dataset = clcw_dataset;
      struct_ = clcw_report_struct;
      codec = Space.clcw_codec;
      get;
      set;
      write_template = Bytes.copy clcw_dataset.packed;
      write_offset = 0;
      write_value = 42;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let packet_case =
  let get = Staged.unstage (Codec.get Space.packet_codec Space.bf_sp_apid) in
  let set = Staged.unstage (Codec.set Space.packet_codec Space.bf_sp_apid) in
  Read_case
    {
      id = Space_packet_apid;
      label = "SpacePacket.apid (bf11)";
      size = Space.packet_size;
      dataset = pkt_dataset;
      struct_ = space_packet_apid_struct;
      codec = Space.packet_codec;
      get;
      set;
      write_template = Bytes.copy pkt_dataset.items.(0);
      write_offset = 0;
      write_value = 123;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let ipv4_case =
  let get = Staged.unstage (Codec.get Net.ipv4_codec Net.bf_ip_src) in
  let set = Staged.unstage (Codec.set Net.ipv4_codec Net.bf_ip_src) in
  Read_case
    {
      id = Ipv4_src;
      label = "IPv4.src (uint32be)";
      size = Net.ipv4_size;
      dataset = ipv4_dataset;
      struct_ = ipv4_struct;
      codec = Net.ipv4_codec;
      get;
      set;
      write_template = Bytes.copy ipv4_dataset.items.(0);
      write_offset = 0;
      write_value = 0x0A00_0001;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let tcp_case =
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_dst_port) in
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_dst_port) in
  Read_case
    {
      id = Tcp_dst_port;
      label = "TCP.dst_port (uint16be)";
      size = Net.tcp_size;
      dataset = tcp_dataset;
      struct_ = tcp_dst_port_struct;
      codec = Net.tcp_codec;
      get;
      set;
      write_template = Bytes.copy tcp_dataset.items.(0);
      write_offset = 0;
      write_value = 8080;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let tcp_syn_case =
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_syn) in
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_syn) in
  Read_case
    {
      id = Tcp_syn;
      label = "TCP.syn (bool bf1)";
      size = Net.tcp_size;
      dataset = tcp_dataset;
      struct_ = tcp_syn_struct;
      codec = Net.tcp_codec;
      get;
      set;
      write_template = Bytes.copy tcp_dataset.items.(0);
      write_offset = 0;
      write_value = true;
      equal = Bool.equal;
      bench_read = false;
      of_c_field = bool_of_int;
    }

let mapped_case =
  let get = Staged.unstage (Codec.get Demo.mapped_codec Demo.bf_mp_priority) in
  let set = Staged.unstage (Codec.set Demo.mapped_codec Demo.bf_mp_priority) in
  Read_case
    {
      id = Mapped_priority;
      label = "Mapped.priority (map fn decode)";
      size = Demo.mapped_size;
      dataset = mapped_dataset;
      struct_ = mapped_struct;
      codec = Demo.mapped_codec;
      get;
      set;
      write_template = Bytes.copy mapped_dataset.packed;
      write_offset = 0;
      write_value = Demo.High;
      equal = ( = );
      bench_read = true;
      of_c_field = priority_decode;
    }

let cases_case =
  let get = Staged.unstage (Codec.get Demo.cases_demo_codec Demo.bf_cd_type) in
  let set = Staged.unstage (Codec.set Demo.cases_demo_codec Demo.bf_cd_type) in
  Read_case
    {
      id = Cases_type;
      label = "CasesDemo.type (cases variant)";
      size = Demo.cases_demo_size;
      dataset = cases_dataset;
      struct_ = cases_demo_struct;
      codec = Demo.cases_demo_codec;
      get;
      set;
      write_template = Bytes.copy cases_dataset.packed;
      write_offset = 0;
      write_value = Demo.Telemetry;
      equal = ( = );
      bench_read = true;
      of_c_field = ptype_decode;
    }

let enum_case =
  let get = Staged.unstage (Codec.get Demo.enum_demo_codec Demo.bf_en_status) in
  let set = Staged.unstage (Codec.set Demo.enum_demo_codec Demo.bf_en_status) in
  Read_case
    {
      id = Enum_status;
      label = "EnumDemo.status (enum + map)";
      size = Demo.enum_demo_size;
      dataset = enum_dataset;
      struct_ = enum_demo_struct;
      codec = Demo.enum_demo_codec;
      get;
      set;
      write_template = Bytes.copy enum_dataset.items.(0);
      write_offset = 0;
      write_value = `Warn;
      equal = ( = );
      bench_read = true;
      of_c_field = status_decode;
    }

let constrained_case =
  let get = Staged.unstage (Codec.get Demo.constrained_codec Demo.bf_co_data) in
  let set = Staged.unstage (Codec.set Demo.constrained_codec Demo.bf_co_data) in
  Read_case
    {
      id = Constrained_data;
      label = "Constrained.data (where)";
      size = Demo.constrained_size;
      dataset = constrained_dataset;
      struct_ = constrained_struct;
      codec = Demo.constrained_codec;
      get;
      set;
      write_template = Bytes.copy constrained_dataset.items.(0);
      write_offset = 0;
      write_value = 9;
      equal = Int.equal;
      bench_read = true;
      of_c_field = id_int;
    }

let all_cases =
  [
    C minimal_case;
    C all_ints_case;
    C large_mixed_case;
    C bitfield8_case;
    C bitfield16_case;
    C bitfield32_case;
    C bool_fields_case;
    C clcw_case;
    C packet_case;
    C ipv4_case;
    C tcp_case;
    C tcp_syn_case;
    C mapped_case;
    C cases_case;
    C enum_case;
    C constrained_case;
  ]

let projection_cases = all_cases

let read_benchmark_cases =
  List.filter (fun (C (Read_case c)) -> c.bench_read) all_cases

let projection_structs =
  List.map (fun (C (Read_case case)) -> case.struct_) projection_cases

let verify_write_case ~label ~template ~offset ~get ~set ~equal value () =
  let bytes = Bytes.copy template in
  set bytes offset value;
  let actual = get bytes offset in
  if not (equal actual value) then
    Fmt.failwith "%s write verification failed" label

let write_case_of_read ~label (Read_case case) =
  {
    label;
    run =
      (fun () ->
        case.set case.write_template case.write_offset case.write_value);
    verify =
      verify_write_case ~label ~template:case.write_template
        ~offset:case.write_offset ~get:case.get ~set:case.set ~equal:case.equal
        case.write_value;
  }

let verify_nested_write ~label ~set ~get ~equal value () =
  let bytes = Bytes.copy tcp_frame in
  let ip = Slice.first (read_eth_payload bytes 0) in
  let tcp = Slice.first (read_ip_payload bytes ip) in
  set bytes tcp value;
  let actual = get bytes tcp in
  if not (equal actual value) then
    Fmt.failwith "%s write verification failed" label

let nested_tcp_dst_write_case =
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_dst_port) in
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_dst_port) in
  let value = 8080 in
  {
    label = "Eth->TCP.dst_port (3 layers)";
    run =
      (fun () ->
        let ip = eth_payload_off tcp_frame 0 in
        let tcp = ip_payload_off tcp_frame ip in
        set tcp_frame tcp value);
    verify =
      verify_nested_write ~label:"Eth->TCP.dst_port (3 layers)" ~set ~get
        ~equal:Int.equal value;
  }

let tcp_dst_port_write_case =
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_dst_port) in
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_dst_port) in
  let value = 8080 in
  {
    label = "TCP.dst_port (uint16be)";
    run = (fun () -> set tcp_frame tcp_off value);
    verify =
      verify_write_case ~label:"TCP.dst_port (uint16be)" ~template:tcp_frame
        ~offset:tcp_off ~get ~set ~equal:Int.equal value;
  }

let nested_tcp_syn_write_case =
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_syn) in
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_syn) in
  let value = true in
  {
    label = "Eth->TCP.syn (3 layers)";
    run =
      (fun () ->
        let ip = eth_payload_off tcp_frame 0 in
        let tcp = ip_payload_off tcp_frame ip in
        set tcp_frame tcp value);
    verify =
      verify_nested_write ~label:"Eth->TCP.syn (3 layers)" ~set ~get
        ~equal:Bool.equal value;
  }

let tcp_syn_write_case =
  let set = Staged.unstage (Codec.set Net.tcp_codec Net.bf_tcp_syn) in
  let get = Staged.unstage (Codec.get Net.tcp_codec Net.bf_tcp_syn) in
  let value = true in
  {
    label = "TCP.syn (bool bf1, read-mod-write)";
    run = (fun () -> set tcp_frame tcp_off value);
    verify =
      verify_write_case ~label:"TCP.syn (bool bf1, read-mod-write)"
        ~template:tcp_frame ~offset:tcp_off ~get ~set ~equal:Bool.equal value;
  }

let write_benchmark_cases =
  [
    write_case_of_read ~label:"Minimal.value (uint8)" minimal_case;
    write_case_of_read ~label:"Bitfield8.value (bf5, read-mod-write)"
      bitfield8_case;
    write_case_of_read ~label:"BoolFields.active (bool bf1, rmw)"
      bool_fields_case;
    write_case_of_read ~label:"CLCW.report (bf8, read-mod-write)" clcw_case;
    tcp_dst_port_write_case;
    tcp_syn_write_case;
    write_case_of_read ~label:"Mapped.priority (map fn encode)" mapped_case;
    write_case_of_read ~label:"CasesDemo.type (cases variant encode)" cases_case;
    nested_tcp_dst_write_case;
    nested_tcp_syn_write_case;
  ]
