(* Writes the Rust module Wire_rust generates for each codec of [Schemas] it
   specialises into the directory given as the first argument, and a [lib.rs]
   that compiles them as one [no_std] crate. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

let schemas =
  [
    ("ipv4", Wire_rust.rs ~codec:"Schemas.ipv4" Schemas.ipv4);
    ("checked", Wire_rust.rs ~codec:"Schemas.checked" Schemas.checked);
    ("mixed", Wire_rust.rs ~codec:"Schemas.mixed" Schemas.mixed);
    ("small", Wire_rust.rs ~codec:"Schemas.small" Schemas.small);
    ("eth", Wire_rust.rs ~codec:"Schemas.eth" Schemas.eth);
    ("wide", Wire_rust.rs ~codec:"Schemas.wide" Schemas.wide);
    ("signed", Wire_rust.rs ~codec:"Schemas.signed" Schemas.signed);
  ]

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  List.iter
    (fun (name, src) -> write (Filename.concat dir (name ^ ".rs")) src)
    schemas;
  write
    (Filename.concat dir "lib.rs")
    (String.concat ""
       ("#![no_std]\n"
       :: List.map (fun (name, _) -> Fmt.str "pub mod %s;\n" name) schemas))
