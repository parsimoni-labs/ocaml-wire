(* The EverParse C row of the packet-stack benchmark.

   everparse.exe FILE STRIDE RUNS [ROW | --list]

   times the verified C validator of Packet_stack.codec on the corpus FILE of
   STRIDE-byte slots, as bench.exe run and cargo/src/main.rs time theirs: the
   C loop, its timing and its RESULT line are in stack_c.c. With ROW, it runs
   only if ROW is the row's name, and fails otherwise; --list prints the row's
   name. *)

external row : unit -> string = "ep_stack_row"
external run : string -> bytes -> int -> int -> unit = "ep_stack_run"

let time file stride runs =
  let stride = int_of_string stride and runs = int_of_string runs in
  let buf =
    Bytes.unsafe_of_string (In_channel.with_open_bin file In_channel.input_all)
  in
  if stride <= 0 || runs <= 0 then
    Fmt.failwith "everparse.exe: STRIDE and RUNS must be positive";
  if Bytes.length buf mod stride <> 0 then
    Fmt.failwith "everparse.exe: %s is not a whole number of %d-byte slots" file
      stride;
  run (Filename.remove_extension (Filename.basename file)) buf stride runs

let () =
  match Array.to_list Sys.argv with
  | [ _; file; stride; runs ] -> time file stride runs
  | [ _; _; _; _; "--list" ] -> print_endline (row ())
  | [ _; file; stride; runs; name ] when String.equal name (row ()) ->
      time file stride runs
  | [ _; _; _; _; name ] ->
      Fmt.epr "no row is named %S@." name;
      exit 1
  | _ ->
      prerr_endline "usage: everparse.exe FILE STRIDE RUNS [ROW | --list]";
      exit 2
