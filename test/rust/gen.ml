(* Writes the Rust module Wire_rust generates for each codec of [Schemas]
   and each structured case of [Diff_codecs.scan_cases], with a fold of
   every integer field, into the directory given as the first
   argument, and a [lib.rs] that compiles them as one [no_std] crate. *)

let write file s =
  let oc = open_out_bin file in
  output_string oc s;
  close_out oc

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

(* The path of every integer field of [c], as a fold names it. *)
let paths c =
  let rec go prefix (view : Wire_ocaml.Views.t) =
    List.concat_map
      (function
        | Wire_ocaml.Views.Int { name; _ } -> [ prefix ^ name ]
        | Record { name; view; _ } -> go (prefix ^ name ^ ".") view
        | Bool _ | Bytes _ -> [])
      view.members
  in
  go "" (Wire_ocaml.Views.v c)

(* Each module, with a fold of every integer field, and the packet stack's
   with a fold of none too. *)
let modules =
  let v n c = Wire_rust.v ~name:n ~fold:(paths c) c in
  [
    v "ipv4" Schemas.ipv4;
    v "checked" Schemas.checked;
    v "mixed" Schemas.mixed;
    v "small" Schemas.small;
    v "eth" Schemas.eth;
    v "wide" Schemas.wide;
    v "signed" Schemas.signed;
    v "sized" Schemas.sized;
    v "overflow" Schemas.overflow;
    v "gated" (Schemas.gated 8);
    v "nested" (Schemas.nested 8);
    v "shared" Schemas.shared;
    v "stack" Packet_stack.codec;
    Wire_rust.v ~name:"stack_checks" ~fold:[] Packet_stack.codec;
    v "capture" Packet_stack.capture;
    v "always" (constant true);
    v "never" (constant false);
  ]
  @ List.map
      (fun (Diff_codecs.Scan c) ->
        v ("s_" ^ String.lowercase_ascii c.label) c.codec)
      Diff_codecs.scan_cases

let () =
  let dir = Sys.argv.(1) in
  if not (Sys.file_exists dir) then Sys.mkdir dir 0o755;
  List.iter
    (fun m ->
      write (Filename.concat dir (Wire_rust.name m ^ ".rs")) (Wire_rust.to_rs m))
    modules;
  write
    (Filename.concat dir "lib.rs")
    (String.concat ""
       ("#![no_std]\n"
       :: List.map (fun m -> Fmt.str "pub mod %s;\n" (Wire_rust.name m)) modules
       ))
