(* The EverParse C row of the packet-stack benchmark.

   everparse.exe FILE STRIDE RUNS [FILTER]

   times the verified C validator of Packet_stack.codec on the corpus FILE of
   STRIDE-byte slots, as bench.exe run and cargo/src/main.rs time theirs: the
   C loop, its timing and its RESULT line are in stack_c.c. *)

external run : string -> bytes -> int -> int -> string -> unit = "ep_stack_run"

let () =
  match Array.to_list Sys.argv with
  | _ :: file :: stride :: runs :: rest ->
      let stride = int_of_string stride and runs = int_of_string runs in
      let filter = match rest with [ f ] -> f | _ -> "" in
      let buf =
        Bytes.unsafe_of_string
          (In_channel.with_open_bin file In_channel.input_all)
      in
      if stride <= 0 || runs <= 0 then
        Fmt.failwith "everparse.exe: STRIDE and RUNS must be positive";
      if Bytes.length buf mod stride <> 0 then
        Fmt.failwith "everparse.exe: %s is not a whole number of %d-byte slots"
          file stride;
      run
        (Filename.remove_extension (Filename.basename file))
        buf stride runs filter
  | _ ->
      prerr_endline "usage: everparse.exe FILE STRIDE RUNS [FILTER]";
      exit 2
