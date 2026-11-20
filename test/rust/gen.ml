(* Writes the Rust module Wire_rust generates for each codec of [Schemas] it
   specialises, and the scanners of the structured cases of
   [Diff_codecs.scan_cases] and of the scanned schemas, into the directory
   given as the first argument, and a [lib.rs] that compiles them as one
   [no_std] crate. *)

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

(* An optional field whose condition is the constant [present], under four
   others, so its outputs are mutable: always present, it overwrites their
   absent values; never present, it leaves them. Neither may warn. *)
let constant present =
  let open Wire in
  let rec under n =
    if n = 0 then
      Codec.v "Constant"
        (fun _ -> ())
        Codec.
          [
            ( Field.optional "x" ~present:(Expr.bool present) uint8 $ fun () ->
              None );
          ]
    else
      let tag = Field.v "tag" uint8 in
      Codec.v
        ("Under" ^ string_of_int n)
        (fun _ _ -> ())
        Codec.
          [
            (tag $ fun () -> UInt8.v 0);
            ( Field.optional "inner"
                ~present:Expr.(Field.ref tag = int 1)
                (codec (under (n - 1)))
            $ fun () -> None );
          ]
  in
  under 4

(* The scanners, with every output and with none. *)
let scanners =
  let both name codec c =
    [
      ("s_" ^ name, Wire_rust.scanner ~codec c);
      ("s_" ^ name ^ "_validate", Wire_rust.scanner ~fields:[] ~codec c);
    ]
  in
  List.concat_map
    (fun (Diff_codecs.Scan c) ->
      both (String.lowercase_ascii c.label) c.label c.codec)
    Diff_codecs.scan_cases
  @ both "sized" "Schemas.sized" Schemas.sized
  @ both "overflow" "Schemas.overflow" Schemas.overflow
  @ both "gated" "Schemas.gated 8" (Schemas.gated 8)
  @ both "nested" "Schemas.nested 8" (Schemas.nested 8)
  @ both "shared" "Schemas.shared" Schemas.shared
  @ both "always" "always" (constant true)
  @ both "never" "never" (constant false)

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  let all = schemas @ scanners in
  List.iter
    (fun (name, src) -> write (Filename.concat dir (name ^ ".rs")) src)
    all;
  write
    (Filename.concat dir "lib.rs")
    (String.concat ""
       ("#![no_std]\n"
       :: List.map (fun (name, _) -> Fmt.str "pub mod %s;\n" name) all))
