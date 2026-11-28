(* The pipeline docs/asn1.md designs. [tool], [run_everparse] and [run] run
   everparse-asn1; the rest raises until it is implemented. *)

let todo name = Fmt.invalid_arg "Wire_asn1.%s: not implemented" name
let tool () = Wire_3d.tool "everparse-asn1.exe"

let not_found =
  "EverParse's everparse-asn1.exe not found in PATH or ~/.local/everparse/bin/"

(* What F* prints after each module it verifies. everparse-asn1 runs F* on the
   specification module and on the implementation module, and on the latter
   again to extract the C. *)
let discharged = "All verification conditions discharged"

let count_substrings ~sub s =
  let n = String.length sub in
  let rec go i acc =
    if i + n > String.length s then acc
    else if String.sub s i n = sub then go (i + n) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

(* everparse-asn1 finds F*, Z3 and the verified libraries under
   EVERPARSE_HOME, else under the parent of the directory it runs from
   (everparse-asn1/Main.ml, [everparse_home]). It is set to that prefix
   explicitly, so a tool reached through a link still finds its own. *)
let environment exe =
  match Sys.getenv_opt "EVERPARSE_HOME" with
  | Some h when h <> "" -> Unix.environment ()
  | _ ->
      let home = Filename.dirname (Filename.dirname exe) in
      Array.append [| "EVERPARSE_HOME=" ^ home |] (Unix.environment ())

let copy_file ~src ~dst =
  let contents = In_channel.with_open_bin src In_channel.input_all in
  Out_channel.with_open_bin dst (fun oc ->
      Out_channel.output_string oc contents)

(* Run [exe] with [args] and [env], both output streams to [log], without a
   shell. Returns the exit code. *)
let spawn ~env ~log exe args =
  let fd =
    Unix.openfile log [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let pid =
    Fun.protect
      ~finally:(fun () -> Unix.close fd)
      (fun () ->
        Unix.create_process_env exe
          (Array.of_list (exe :: args))
          env Unix.stdin fd fd)
  in
  match snd (Unix.waitpid [] pid) with
  | Unix.WEXITED n -> n
  | Unix.WSIGNALED n | Unix.WSTOPPED n -> 128 + n

(* everparse-asn1 runs F* as it finds it: $FSTAR_EXE, else the fstar.exe of
   its prefix, else fstar.exe in PATH (everparse-asn1/Main.ml,
   [resolve_fstar_exe]). It is handed that F* through Wire_3d's script, which
   raises Custard's budget as 3d.exe's runs need: the C extraction of a large
   module stops on it otherwise (Error 365). None on Windows, where the script
   cannot run. *)
let fstar_flags exe =
  if Sys.win32 then []
  else
    let fstar =
      match Sys.getenv_opt "FSTAR_EXE" with
      | Some f when f <> "" -> f
      | _ ->
          let own = Filename.concat (Filename.dirname exe) "fstar.exe" in
          if Sys.file_exists own then own else "fstar.exe"
    in
    [ "--fstar"; Wire_3d.fstar_wrapper fstar ]

(* One schema in a {!Wire_3d.pool} job, in [dir], a directory of its own:
   F* writes its checked files beside the modules it verifies. The job copies
   the F* modules and the C into [outdir], and the tool's output, its verdict
   last, as [<name>.log], which outlives a failed run. *)
let job ~exe ~env ~outdir s dir =
  let asn = Wire.Asn1.filename s in
  let file = Filename.concat dir asn in
  copy_file ~src:(Filename.concat outdir asn) ~dst:file;
  let log = Filename.concat dir "everparse-asn1.log" in
  let start = Unix.gettimeofday () in
  let code =
    spawn ~env ~log exe
      (fstar_flags exe
      @ [
          "--odir";
          dir;
          "--root";
          String.concat "," (Wire.Asn1.roots s);
          "--skip_compilation";
          file;
        ])
  in
  let output = In_channel.with_open_bin log In_channel.input_all in
  let verdict =
    if code <> 0 then Fmt.str "refused with code %d" code
    else if count_substrings ~sub:discharged output < 2 then
      Fmt.str "not verified: no %S for its two modules" discharged
    else Fmt.str "verified in %.0fs" (Unix.gettimeofday () -. start)
  in
  let summary = Fmt.str "%s: %s" (Wire.Asn1.name s) verdict in
  Out_channel.with_open_bin
    (Filename.concat outdir (Wire.Asn1.name s ^ ".log"))
    (fun oc -> Out_channel.output_string oc (output ^ summary ^ "\n"));
  print_string output;
  Fmt.pr "%s@." summary;
  if not (String.starts_with ~prefix:"verified" verdict) then
    Fmt.failwith "everparse-asn1 on %s: %s" asn verdict;
  Array.iter
    (fun f ->
      if List.mem (Filename.extension f) [ ".c"; ".h"; ".fst" ] then
        copy_file ~src:(Filename.concat dir f) ~dst:(Filename.concat outdir f))
    (Sys.readdir dir)

let run_everparse ?(quiet = true) ~outdir ss =
  let exe = match tool () with Some e -> e | None -> failwith not_found in
  let env = environment exe in
  let outdir =
    if Filename.is_relative outdir then Filename.concat (Sys.getcwd ()) outdir
    else outdir
  in
  match
    Wire_3d.pool
      (List.map (fun s -> (Wire.Asn1.name s, job ~exe ~env ~outdir s)) ss)
  with
  | Ok logs -> if not quiet then List.iter print_string logs
  | Error failed -> failwith failed

let run ?quiet ~outdir ss =
  Wire.Asn1.write ~outdir ss;
  run_everparse ?quiet ~outdir ss

let c_name _ = todo "c_name"
let generate_dune ~outdir:_ ~package:_ _ = todo "generate_dune"

type packed = unit

let pack _ = todo "pack"
let generate_corpus ?count:_ _ _ = todo "generate_corpus"
let generate_agree ?name:_ ~outdir:_ ~package:_ _ = todo "generate_agree"
