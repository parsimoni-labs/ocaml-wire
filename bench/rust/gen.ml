(* Writes the code generated for the benchmark.

   With [--ocaml] (or [--oxcaml], for the OxCaml target), in the current
   directory: the scanner of [Packet_stack.codec], g_stack.ml, and its
   checksum consumers, stack_read.ml; the same scanner handing out only the
   fields the checksum reads, g_selected.ml and selected_read.ml; and the
   record modules of the fixed header codecs of [Headers], g_eth.ml,
   g_ipv4.ml, g_tcp.ml and g_udp.ml. The batch consumers take the corpus
   stride, which the harness passes as a literal so it stays a constant after
   inlining.

   With [--rust DIR], the Wire_rust modules of the fixed header codecs,
   eth.rs, ipv4.rs, tcp.rs and udp.rs, in [DIR]. *)

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

let checksum_contribution ~wide get =
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
  ^ (if wide then "#0L" else "0")
  ^ " in\n" ^ "(ip0 + ip1) + (ip2 + transport)"

let reader_modes name =
  ( List.mem name [ "batch"; "batch_unboxed" ],
    List.mem name [ "unboxed"; "fold_unboxed"; "batch_unboxed" ],
    List.mem name [ "fold"; "fold_unboxed"; "batch"; "batch_unboxed" ] )

let reader_invocation ~core ~fields ~decode ~fold ~batch ~wide get =
  if fold then
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
  else
    "let v = " ^ core ^ "." ^ decode ^ " buf off len in\nif not " ^ get "ok"
    ^ " then base else\n"

let reader_wrappers ~wide ~fold ~batch =
  let first = if wide && batch then "U.to_int (" else "" in
  let inside =
    if wide then
      if batch then "(let open Ops in\n" else "U.to_int (let open Ops in\n"
    else ""
  in
  let last =
    (if wide then ")" else "")
    ^ (if fold then ")\n" else "")
    ^ if wide && batch then ")\n" else ""
  in
  (first, inside, last)

let checksum_reader target ~core ~fields name decode access =
  let batch, wide, fold = reader_modes name in
  let get n = if fold then n else "v." ^ access ^ n in
  let first, inside, last = reader_wrappers ~wide ~fold ~batch in
  let annotation = if batch && target = `OxCaml then "[@zero_alloc]" else "" in
  let base = if wide && not batch then "U.of_int base" else "base" in
  "let[@inline always]" ^ annotation ^ " " ^ name
  ^ (if batch then " ~stride" else "")
  ^ " base buf off len =\n" ^ first
  ^ reader_invocation ~core ~fields ~decode ~fold ~batch ~wide get
  ^ inside ^ "let contribution = "
  ^ checksum_contribution ~wide get
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
  ^ reader "boxed" "decode" "" ^ reader "fold" "" "" ^ reader "batch" "" ""
  ^
  if target = `OxCaml then
    reader "unboxed" "decode_unboxed" "#"
    ^ reader "fold_unboxed" "" ""
    ^ reader "batch_unboxed" "" ""
  else
    "let unboxed = boxed\nlet fold_unboxed = fold\nlet batch_unboxed = batch\n"

(* The fixed header codecs, each written by [g name codec]. *)
type g = { g : 'r. string -> 'r Wire.Codec.t -> unit }

let headers { g } =
  g "eth" Headers.eth;
  g "ipv4" Headers.ipv4;
  g "tcp" Headers.tcp;
  g "udp" Headers.udp

let () =
  match Array.to_list Sys.argv with
  | [ _; (("--ocaml" | "--oxcaml") as t) ] ->
      let target = if t = "--oxcaml" then `OxCaml else `OCaml in
      let all = Wire_ocaml.scanner_fields Packet_stack.codec in
      let selected =
        List.filter (fun (n, _) -> List.mem n checksum_fields) all
      in
      write "g_stack.ml"
        (Wire_ocaml.scanner ~target ~codec:"Packet_stack.codec"
           Packet_stack.codec);
      write "stack_read.ml" (stack_reader target ~core:"G_stack" ~fields:all);
      write "g_selected.ml"
        (Wire_ocaml.scanner ~target ~fields:checksum_fields
           ~codec:"Packet_stack.codec" Packet_stack.codec);
      write "selected_read.ml"
        (stack_reader target ~core:"G_selected" ~fields:selected);
      headers
        {
          g =
            (fun name c ->
              write
                ("g_" ^ name ^ ".ml")
                (Wire_ocaml.ml ~target ~codec:("Headers." ^ name) c));
        }
  | [ _; "--rust"; dir ] ->
      headers
        {
          g =
            (fun name c ->
              write
                (Filename.concat dir (name ^ ".rs"))
                (Wire_rust.rs ~codec:("Headers." ^ name) c));
        }
  | _ ->
      prerr_endline "usage: gen.exe (--ocaml | --oxcaml | --rust DIR)";
      exit 2
