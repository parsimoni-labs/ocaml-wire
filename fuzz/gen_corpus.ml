(** Generate seed corpus for fuzz testing. *)

let () =
  let dir = "corpus" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let write name data =
    let oc = open_out_bin (Filename.concat dir name) in
    output_string oc data;
    close_out oc
  in
  write "seed_000" "";
  write "seed_001" "\x00";
  write "seed_002" "\xff";
  write "seed_003" (String.make 16 '\x00');
  write "seed_004" (String.make 16 '\xff');
  write "seed_005" (String.init 256 Char.chr);
  Printf.printf "gen_corpus: wrote 6 seed files\n"
