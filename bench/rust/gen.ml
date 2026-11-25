(* Writes the code generated for the benchmark, for the compiler that builds
   this generator.

   With [--harness FILE...], the harness around the generated modules, each
   FILE in the current directory: stack_step.ml and selected_step.ml, the
   checksum visitors the folds [checksum] of g_stack and g_selected call by
   name, the first over every output of [Packet_stack.codec], the second over
   those the checksum reads; stack_read.ml and selected_read.ml, the checksum
   readers over them; flow_step.ml and flow_parse.ml, the visitors of the
   folds [classify] and [parse] of g_flow, which hand the flow classifier's
   fields of [Packet_stack.capture] to [Flow_table], and on OxCaml fold them
   into [Flow_sum]; flow_read.ml, the entries [classify] and [parse] over
   them; and g_eth_wire.ml and so on, with their interfaces, the bridges of
   the modules of the fixed header codecs of [Headers], whose [Global] and
   [Interpreted] the swap rows read. The batch readers take the corpus
   stride, which the harness passes as a literal so it stays a constant after
   inlining.

   With [--rust DIR], in [DIR]: the Wire_rust modules of the fixed header
   codecs, eth.rs, ipv4.rs, ipv6.rs, tcp.rs and udp.rs; the scanner Wire_rust
   writes from [Packet_stack.codec], stack.rs; the same scanner handing out
   only the fields the checksum reads, stack_selected.rs; and the scanner of
   [Packet_stack.capture] handing out the flow classifier's fields, flow.rs.

   Otherwise the modules Wire_ocaml writes, as {!Wire_ocaml.main} reads its
   command line: g_stack and g_selected, the modules of [Packet_stack.codec]
   with every output and with those the checksum reads; g_flow, that of
   [Packet_stack.capture] with the flow classifier's; g_capture, that of
   [Packet_stack.capture] with every output, whose views the check/view row
   reads; and g_eth, g_ipv4, g_ipv6, g_tcp and g_udp, those of [Headers]. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let checksum_fields =
  [
    "ether_type";
    "ipv4_present";
    "ipv4_src";
    "ipv4_dst";
    "ipv4_ttl";
    "ipv4_protocol";
    "ipv4_total_length";
    "ipv4_tcp_present";
    "ipv4_tcp_src_port";
    "ipv4_tcp_dst_port";
    "ipv4_tcp_seq";
    "ipv4_tcp_ack";
    "ipv4_tcp_window";
    "ipv4_tcp_flags";
    "ipv4_tcp_data_offset";
    "ipv4_udp_present";
    "ipv4_udp_src_port";
    "ipv4_udp_dst_port";
    "ipv4_udp_length";
  ]

let checksum_contribution ~zero get =
  let ports p =
    "((" ^ get (p ^ "_src_port") ^ " lsl 16) lor " ^ get (p ^ "_dst_port") ^ ")"
  in
  "if not " ^ get "ipv4_present" ^ " then " ^ get "ether_type" ^ " else\n"
  ^ "let ip0 = " ^ get "ipv4_src" ^ " lxor " ^ get "ipv4_dst" ^ " in\n"
  ^ "let ip1 = " ^ get "ipv4_ttl" ^ " + " ^ get "ipv4_protocol" ^ " in\n"
  ^ "let ip2 = " ^ get "ether_type" ^ " + " ^ get "ipv4_total_length" ^ " in\n"
  ^ "let transport = if " ^ get "ipv4_tcp_present" ^ " then\n" ^ "let a = "
  ^ ports "ipv4_tcp" ^ " in\n" ^ "let b = " ^ get "ipv4_tcp_seq" ^ " lxor "
  ^ get "ipv4_tcp_ack" ^ " in\n" ^ "let c = " ^ get "ipv4_tcp_window" ^ " + "
  ^ get "ipv4_tcp_flags" ^ " in\n" ^ "(a + b) + (c + "
  ^ get "ipv4_tcp_data_offset" ^ ")\n" ^ "else if " ^ get "ipv4_udp_present"
  ^ " then " ^ ports "ipv4_udp" ^ " + " ^ get "ipv4_udp_length" ^ " else "
  ^ zero ^ " in\n" ^ "(ip0 + ip1) + (ip2 + transport)"

let reader_modes name =
  ( List.mem name [ "batch"; "batch_unboxed" ],
    List.mem name [ "fold_unboxed"; "batch_unboxed" ] )

let reader_invocation ~core ~fields ~batch ~wide =
  let call =
    if batch then
      (if wide then
         ".fold_frames_unboxed buf off len ~stride ~init:(U.of_int base)"
       else ".fold_frames buf off len ~stride ~init:base")
      ^ " ~invalid:(fun x -> x) ~valid:(fun[@inline always] base "
    else
      (if wide then ".fold_unboxed" else ".fold")
      ^ " buf off len ~invalid:(fun () -> base) ~valid:(fun[@inline always] "
  in
  let args =
    List.map
      (fun (name, _) ->
        "~" ^ name ^ if List.mem name checksum_fields then "" else ":_")
      fields
  in
  core ^ call ^ String.concat " " args ^ " ->\n"

let reader_wrappers ~wide ~batch =
  let first = if wide && batch then "U.to_int (" else "" in
  let inside =
    if wide then
      if batch then "(let open Ops in\n" else "U.to_int (let open Ops in\n"
    else ""
  in
  let last =
    (if wide then ")" else "") ^ ")\n" ^ if wide && batch then ")\n" else ""
  in
  (first, inside, last)

(* The checksum visitor the fold [checksum] calls by name: OCaml without
   flambda never inlines a function passed as an argument. It takes the
   outputs as [int64] and folds them as such, as the unboxed rows of OxCaml
   fold [int64_u]. *)
let step ~fields =
  let args =
    List.map
      (fun (name, _) ->
        "~" ^ name ^ if List.mem name checksum_fields then "" else ":_")
      fields
  in
  "type t = int64\n\n\
   external ( + ) : int64 -> int64 -> int64 = \"%int64_add\"\n\
   external ( lxor ) : int64 -> int64 -> int64 = \"%int64_xor\"\n\
   external ( lor ) : int64 -> int64 -> int64 = \"%int64_or\"\n\
   external ( lsl ) : int64 -> int -> int64 = \"%int64_lsl\"\n\n\
   let[@inline always] invalid (base : t) = base\n\
   let[@inline always] valid base " ^ String.concat " " args
  ^ " =\nlet contribution = "
  ^ checksum_contribution ~zero:"0L" Fun.id
  ^ " in\nbase + contribution\n"

let checksum_reader target ~core ~fields name =
  let batch, wide = reader_modes name in
  let first, inside, last = reader_wrappers ~wide ~batch in
  (* Checked where the fold is inlined, under the bench profile: the dev
     profile compiles g_stack opaque, so the reader calls it. *)
  let annotation =
    if batch && target = `OxCaml then "[@zero_alloc opt]" else ""
  in
  let base = if wide && not batch then "U.of_int base" else "base" in
  "let[@inline always]" ^ annotation ^ " " ^ name
  ^ (if batch then " ~stride" else "")
  ^ " base buf off len =\n"
  ^
  if not wide then
    let call =
      core
      ^
      if batch then ".checksum_frames buf off len ~stride"
      else ".checksum buf off len"
    in
    "Int64.to_int (" ^ call ^ " ~init:(Int64.of_int base))\n"
  else
    first
    ^ reader_invocation ~core ~fields ~batch ~wide
    ^ inside ^ "let contribution = "
    ^ checksum_contribution ~zero:(if wide then "#0L" else "0") Fun.id
    ^ " in\n" ^ base ^ " + contribution\n" ^ last

let stack_reader target ~core ~fields =
  let reader = checksum_reader target ~core ~fields in
  "let oxcaml = "
  ^ string_of_bool (target = `OxCaml)
  ^ "\n"
  ^ (if target = `OxCaml then
       "module U = Wire_runtime.Target.Int64_u\n\
        module Ops = struct\n\
        let ( + ) = U.add\n\
        let ( lxor ) = U.logxor\n\
        let ( lor ) = U.logor\n\
        let ( lsl ) = U.shift_left\n\
        end\n"
     else "")
  ^ reader "fold" ^ reader "batch"
  ^
  if target = `OxCaml then reader "fold_unboxed" ^ reader "batch_unboxed"
  else "let fold_unboxed = fold\nlet batch_unboxed = batch\n"

(* {1 The flow classifier}

   The outputs of the scanner of [Packet_stack.capture] the classifier reads:
   the addresses, the protocol, the ports and the IP length of each packet. *)
let flow_fields =
  [
    "ipv4_present";
    "ipv4_total_length";
    "ipv4_protocol";
    "ipv4_src";
    "ipv4_dst";
    "ipv4_tcp_present";
    "ipv4_tcp_src_port";
    "ipv4_tcp_dst_port";
    "ipv4_udp_present";
    "ipv4_udp_src_port";
    "ipv4_udp_dst_port";
    "ipv6_present";
    "ipv6_payload_length";
    "ipv6_next_header";
    "ipv6_src0";
    "ipv6_src1";
    "ipv6_src2";
    "ipv6_src3";
    "ipv6_dst0";
    "ipv6_dst1";
    "ipv6_dst2";
    "ipv6_dst3";
    "ipv6_tcp_present";
    "ipv6_tcp_src_port";
    "ipv6_tcp_dst_port";
    "ipv6_udp_present";
    "ipv6_udp_src_port";
    "ipv6_udp_dst_port";
  ]

(* The arguments of [add4] and [add6] the outputs of [flow_fields] give, with
   [i] converting an output to the integer the consumer computes on and
   [ip6_bytes] adding the IPv6 header to the payload length. *)
let key4 =
  "~src:(i ipv4_src) ~dst:(i ipv4_dst)\n\
   ~proto:(i ipv4_protocol)\n\
   ~ports:(ports ipv4_tcp_present ipv4_tcp_src_port ipv4_tcp_dst_port\n\
   ipv4_udp_present ipv4_udp_src_port ipv4_udp_dst_port)\n\
   ~bytes:(i ipv4_total_length)"

let key6 =
  "~s0:(i ipv6_src0) ~s1:(i ipv6_src1) ~s2:(i ipv6_src2)\n\
   ~s3:(i ipv6_src3) ~d0:(i ipv6_dst0) ~d1:(i ipv6_dst1) ~d2:(i ipv6_dst2)\n\
   ~d3:(i ipv6_dst3) ~proto:(i ipv6_next_header)\n\
   ~ports:(ports ipv6_tcp_present ipv6_tcp_src_port ipv6_tcp_dst_port\n\
   ipv6_udp_present ipv6_udp_src_port ipv6_udp_dst_port)\n\
   ~bytes:(ip6_bytes ipv6_payload_length)"

(* The visitor of a fold of g_flow, whose accumulator has type [typ] and is [base] in [valid]: [v4 args] and [v6 args]
   are its next value for an IPv4 and an IPv6 packet whose key [args] gives,
   [other] for a packet with no key. With [wide], the key is the [int64]
   values the scanner hands out, as [Flow_sum] folds it; otherwise [int]s, as
   [Flow_table] takes them. [i] and [ip6_bytes] are defined beside [valid],
   not in it: OCaml without flambda inlines no function whose body defines
   one, and an out-of-line [valid] boxes each [int64] it is passed. *)
let flow_step ?(wide = false) ~typ ~v4 ~v6 ~other () =
  let ports =
    if wide then
      "if tcp then Int64.logor (Int64.shift_left tsp 16) tdp\n\
       else if udp then Int64.logor (Int64.shift_left usp 16) udp_\n\
       else 0L"
    else
      "if tcp then (Int64.to_int tsp lsl 16) lor Int64.to_int tdp\n\
       else if udp then (Int64.to_int usp lsl 16) lor Int64.to_int udp_\n\
       else 0"
  and conversions =
    if wide then "let i x = x\nlet ip6_bytes l = Int64.add 40L l"
    else "let i = Int64.to_int\nlet ip6_bytes l = 40 + Int64.to_int l"
  in
  "type t = " ^ typ ^ "\n\n" ^ conversions
  ^ "\n\n\
     let[@inline always] invalid (base : t) = base\n\n\
     let[@inline always] ports tcp tsp tdp udp usp udp_ =\n" ^ ports
  ^ "\n\nlet[@inline always] valid base "
  ^ String.concat " " (List.map (fun n -> "~" ^ n) flow_fields)
  ^ " =\nif ipv4_present then " ^ v4 key4 ^ "\nelse if ipv6_present then "
  ^ v6 key6 ^ "\nelse " ^ other ^ "\n"

(* The visitor that hands each key to [sink]. *)
let sink_step sink =
  flow_step ~typ:"int"
    ~v4:(fun k -> "(" ^ sink ^ ".add4 " ^ k ^ ";\nbase)")
    ~v6:(fun k -> "(" ^ sink ^ ".add6 " ^ k ^ ";\nbase)")
    ~other:"base" ()

let table_step = sink_step "Flow_table"

(* The visitor of the rows that time the parse alone and its entry, which
   calls the fold [fold]. On OxCaml it computes each key on the [int64] values
   the scanner hands out, with [Flow_sum]'s key functions, and folds the keys
   in its accumulator, an unboxed pair of the number of keys and their sum,
   which stays in registers. On OCaml an [int64] handed to a call would be
   boxed and that pair allocated for each key, so it hands each key to
   [Flow_sum] as [int]s. *)
let parse_step target =
  if target = `OxCaml then
    let next f k =
      "let #(keys, sum) = base in\n#(keys + 1, sum + Int64.to_int (Flow_sum."
      ^ f ^ " " ^ k ^ "))"
    in
    ( flow_step ~wide:true ~typ:"#(int * int)" ~v4:(next "key4")
        ~v6:(next "key6") ~other:"base" (),
      fun fold ->
        "let #(keys, sum) = " ^ fold
        ^ " ~init:#(0, 0) in\nFlow_sum.add_pass ~keys ~sum" )
  else (sink_step "Flow_sum", fun fold -> "ignore (" ^ fold ^ " ~init:0 : int)")

(* The fixed header codecs, each written by [g name codec]. *)
type g = { g : 'r. string -> 'r Wire.Codec.t -> unit }

let headers { g } =
  g "eth" Headers.eth;
  g "ipv4" Headers.ipv4;
  g "ipv6" Headers.ipv6;
  g "tcp" Headers.tcp;
  g "udp" Headers.udp

let target : Wire_ocaml.target =
  match Sys.ocaml_release.extra with Some (_, "ox") -> `OxCaml | _ -> `OCaml

(* The harness file [file]. *)
let harness file =
  let all = Wire_ocaml.outputs Packet_stack.codec in
  let selected = List.filter (fun (n, _) -> List.mem n checksum_fields) all in
  let entry name body = "let " ^ name ^ " buf offs lens =\n" ^ body ^ "\n\n" in
  let parse_visitor, parse = parse_step target in
  let bridge name c =
    let codec = "Headers." ^ name and core = "G_" ^ name in
    if Filename.check_suffix file ".mli" then
      Wire_ocaml.bridge_mli ~record:codec ~codec ~core c
    else Wire_ocaml.bridge ~record:codec ~codec ~core c
  in
  let text =
    match Filename.remove_extension (Filename.basename file) with
    | "stack_step" -> step ~fields:all
    | "selected_step" -> step ~fields:selected
    | "stack_read" -> stack_reader target ~core:"G_stack" ~fields:all
    | "selected_read" -> stack_reader target ~core:"G_selected" ~fields:selected
    | "flow_step" -> table_step
    | "flow_parse" -> parse_visitor
    | "flow_read" ->
        entry "classify"
          "ignore (G_flow.classify_index buf ~offs ~lens ~init:0 : int)"
        ^ entry "parse" (parse "G_flow.parse_index buf ~offs ~lens")
    | "g_eth_wire" -> bridge "eth" Headers.eth
    | "g_ipv4_wire" -> bridge "ipv4" Headers.ipv4
    | "g_ipv6_wire" -> bridge "ipv6" Headers.ipv6
    | "g_tcp_wire" -> bridge "tcp" Headers.tcp
    | "g_udp_wire" -> bridge "udp" Headers.udp
    | _ -> failwith ("no harness file " ^ file)
  in
  write file text

let () =
  match Array.to_list Sys.argv with
  | _ :: "--harness" :: files -> List.iter harness files
  | [ _; "--rust"; dir ] ->
      let codec = "Packet_stack.codec" in
      write
        (Filename.concat dir "stack.rs")
        (Wire_rust.scanner ~codec Packet_stack.codec);
      write
        (Filename.concat dir "stack_selected.rs")
        (Wire_rust.scanner ~fields:checksum_fields ~codec Packet_stack.codec);
      write
        (Filename.concat dir "flow.rs")
        (Wire_rust.scanner ~fields:flow_fields ~codec:"Packet_stack.capture"
           Packet_stack.capture);
      headers
        {
          g =
            (fun name c ->
              write
                (Filename.concat dir (name ^ ".rs"))
                (Wire_rust.rs ~codec:("Headers." ^ name) c));
        }
  | _ ->
      let checksum o =
        Wire_ocaml.[ fold ?outputs:o "fold"; fold ?outputs:o "checksum" ]
      and flow =
        Wire_ocaml.
          [
            fold ~outputs:flow_fields "classify";
            fold ~outputs:flow_fields "parse";
          ]
      in
      Wire_ocaml.(
        main
          [
            v ~name:"g_stack" ~folds:(checksum None) Packet_stack.codec;
            v ~name:"g_selected"
              ~folds:(checksum (Some checksum_fields))
              Packet_stack.codec;
            v ~name:"g_flow" ~folds:flow Packet_stack.capture;
            v ~name:"g_capture" Packet_stack.capture;
            v ~name:"g_eth" Headers.eth;
            v ~name:"g_ipv4" Headers.ipv4;
            v ~name:"g_ipv6" Headers.ipv6;
            v ~name:"g_tcp" Headers.tcp;
            v ~name:"g_udp" Headers.udp;
          ])
