(* The EverParse C rows of the packet-stack benchmark.

   everparse.exe FILE STRIDE RUNS [ROW | --list]

   times the verified C validator of Packet_stack.codec on the corpus FILE of
   STRIDE-byte slots, as bench.exe run and cargo/src/main.rs time theirs: the
   C loops, their timing and their RESULT lines are in stack_c.c and
   stack_inline.c. With ROW, it runs only the row of that name, and fails if
   there is none; --list prints the rows' names.

   everparse.exe flow FILE RUNS [ROW | --list]

   times the verified C validator of Packet_stack.capture in the flow
   classifier, on the pcap file FILE, as bench.exe flow and wire-bench-rust
   flow time theirs: its loop and flow table are in stack_c.c too. *)

external flow_row : unit -> string = "ep_flow_row"
external flow_run : string -> bytes -> int -> unit = "ep_flow_run"

let flow file runs =
  let runs = int_of_string runs in
  if runs <= 0 then Fmt.failwith "everparse.exe: RUNS must be positive";
  let buf =
    Bytes.unsafe_of_string (In_channel.with_open_bin file In_channel.input_all)
  in
  flow_run (Filename.remove_extension (Filename.basename file)) buf runs

external rows : unit -> int = "ep_stack_rows"
external row : int -> string = "ep_stack_row"
external run : int -> string -> bytes -> int -> int -> unit = "ep_stack_run"

let time file stride runs rows =
  let stride = int_of_string stride and runs = int_of_string runs in
  let buf =
    Bytes.unsafe_of_string (In_channel.with_open_bin file In_channel.input_all)
  in
  if stride <= 0 || runs <= 0 then
    Fmt.failwith "everparse.exe: STRIDE and RUNS must be positive";
  if Bytes.length buf mod stride <> 0 then
    Fmt.failwith "everparse.exe: %s is not a whole number of %d-byte slots" file
      stride;
  let corpus = Filename.remove_extension (Filename.basename file) in
  List.iter (fun i -> run i corpus buf stride runs) rows

let () =
  let all = List.init (rows ()) Fun.id in
  match Array.to_list Sys.argv with
  | [ _; "flow"; file; runs ] -> flow file runs
  | [ _; "flow"; _; _; "--list" ] -> print_endline (flow_row ())
  | [ _; "flow"; file; runs; name ] when String.equal name (flow_row ()) ->
      flow file runs
  | [ _; "flow"; _; _; name ] ->
      Fmt.epr "no row is named %S@." name;
      exit 1
  | [ _; file; stride; runs ] -> time file stride runs all
  | [ _; _; _; _; "--list" ] -> List.iter (fun i -> print_endline (row i)) all
  | [ _; file; stride; runs; name ] -> (
      match List.filter (fun i -> String.equal name (row i)) all with
      | [] ->
          Fmt.epr "no row is named %S@." name;
          exit 1
      | rows -> time file stride runs rows)
  | _ ->
      prerr_endline
        "usage: everparse.exe (FILE STRIDE RUNS | flow FILE RUNS) [ROW | \
         --list]";
      exit 2
