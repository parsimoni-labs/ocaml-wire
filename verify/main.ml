(* Translation validation of the code Wire_ocaml generates.

   main.exe [OPTION]... SPECS FILE:KIND:PARSER:BOUND...

   For each entry of the generated module FILE (KIND says which: record, or
   scanner/N and scanner-ox/N for the scanner on OCaml and on OxCaml, N the
   shortest stride fold_frames walks without frame checks), proves with Z3
   that every unchecked read is inside the buffer and inside the window the
   entry is given, that every shift amount is in range, that no input
   raises, and, when the entry answers accept or reject, that on each path
   it accepts exactly the windows the EverParse parser PARSER accepts, as
   [3d.exe --emit_smt_encoding] states it, for windows of up to BOUND bytes
   (any size with --any-size). SPECS is the directory of the .3d files.

   --bounds-only skips the specification, --only ENTRY keeps one entry,
   --expect-refuted P succeeds when a property holding P is refuted, --jobs
   and --timeout set the Z3 processes and their seconds, --keep keeps the
   scripts. *)

let read file = In_channel.with_open_bin file In_channel.input_all

(* The SMT encoding EverParse gives of the parsers of [file]. *)
let spec_prelude ~threed file =
  let outdir = Filename.temp_dir "wire-verify" "" in
  let out = Filename.concat outdir "smt.out" in
  let fd = Unix.openfile out [ O_WRONLY; O_CREAT; O_TRUNC ] 0o644 in
  let pid =
    Unix.create_process threed
      [| threed; "--emit_smt_encoding"; "--odir"; outdir; file |]
      Unix.stdin fd Unix.stderr
  in
  Unix.close fd;
  (match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> Fmt.failwith "%s --emit_smt_encoding %s failed" threed file);
  let text = read out in
  Sys.remove out;
  (try Sys.rmdir outdir with Sys_error _ -> ());
  (* 3d.exe prints its progress on the same channel as the encoding. *)
  String.split_on_char '\n' text
  |> List.filter (fun l ->
      not
        (String.starts_with ~prefix:"Processing files:" l
        || String.starts_with ~prefix:"Not emitting" l))
  |> String.concat "\n"

open Symex

let buf = Buffer_input "buf"
let b = Buffer_input "b"

(* A record module: [is_valid buf off], [View.check b len off] and every
   [view_*] accessor behind a check. *)
let record ~spec ~bound ctx =
  let view_names =
    Hashtbl.fold
      (fun k _ acc ->
        if String.starts_with ~prefix:"view_" k && not (String.contains k '.')
        then k :: acc
        else acc)
      ctx.globals []
    |> List.sort String.compare
  in
  [
    {
      Check.name = "is_valid";
      inputs = [ buf; Int_input "off" ];
      pre = "true";
      expr = "is_valid buf off";
      window = ("off", "Bytes.length buf - off");
      spec = Some spec;
      bound;
      misuse = false;
    };
    {
      Check.name = "View.check";
      inputs = [ b; Int_input "len"; Int_input "off" ];
      pre = "len = Bytes.length b";
      expr = "View.ok (View.check b len off)";
      window = ("off", "len - off");
      spec = Some spec;
      bound;
      misuse = false;
    };
  ]
  @ List.map
      (fun v ->
        {
          Check.name = v ^ " after View.check";
          inputs = [ b; Int_input "len"; Int_input "off" ];
          pre = "len = Bytes.length b";
          expr =
            Fmt.str
              "let c = View.check b len off in if View.ok c then %s b \
               (View.get c) else 0"
              v;
          window = ("off", "len - off");
          spec = None;
          bound;
          misuse = false;
        })
      view_names

(* A scanner: [fold] over one frame; [frames_record], the body of the
   [fold_frames] loop for a stride the loop walks without frame checks,
   which is what the loop establishes; and, on the OCaml target,
   [fold_frames] itself, its loop included. *)
let scanner ~ox ~spec ~bound ~min_stride _ctx =
  let at x = if ox then Fmt.str "~%s:(Int64.of_int %s)" x x else "~" ^ x in
  let frames =
    if ox then
      (* [let mutable] has no Parsetree in OCaml's compiler-libs. *)
      []
    else
      [
        {
          Check.name = "fold_frames";
          inputs = [ buf; Int_input "off"; Int_input "len"; Int_input "stride" ];
          pre =
            "stride > 0 && off >= 0 && len >= 0 && off <= Bytes.length buf && \
             len <= Bytes.length buf - off";
          expr = "fold_frames buf off len ~stride ~init:0 ~invalid ~valid";
          window = ("off", "len");
          spec = None;
          bound;
          misuse = false;
        };
        {
          Check.name = "fold_frames, any arguments";
          inputs = [ buf; Int_input "off"; Int_input "len"; Int_input "stride" ];
          pre = "true";
          expr = "fold_frames buf off len ~stride ~init:0 ~invalid ~valid";
          window = ("off", "len");
          spec = None;
          bound;
          misuse = true;
        };
      ]
  in
  frames
  @ [
      {
        Check.name = "fold";
        inputs = [ buf; Int_input "off"; Int_input "len" ];
        pre = "true";
        expr = "fold buf off len ~invalid ~valid";
        window = ("off", "len");
        spec = Some spec;
        bound;
        misuse = false;
      };
      {
        Check.name = "frames_record";
        inputs =
          [ buf; Int_input "base"; Int_input "stop"; Int_input "current" ];
        pre =
          Fmt.str "stop >= %d && base >= 0 && base <= Bytes.length buf - stop"
            min_stride;
        expr =
          Fmt.str "frames_record ~buf %s %s ~current ~invalid ~valid"
            (at "base") (at "stop");
        window = ("base", "stop");
        spec = Some spec;
        bound;
        misuse = false;
      };
    ]

(* The step scanner: [fold_step] and [frames_step_record] answer through
   [Step.valid] and [Step.invalid]; on OCaml, [fold_frames_step] too. *)
let step_scanner ~ox ~spec ~bound ~min_stride _ctx =
  let at x = if ox then Fmt.str "~%s:(Int64.of_int %s)" x x else "~" ^ x in
  let frames =
    if ox then []
    else
      [
        {
          Check.name = "fold_frames_step";
          inputs = [ buf; Int_input "off"; Int_input "len"; Int_input "stride" ];
          pre =
            "stride > 0 && off >= 0 && len >= 0 && off <= Bytes.length buf && \
             len <= Bytes.length buf - off";
          expr = "fold_frames_step buf off len ~stride ~init:0";
          window = ("off", "len");
          spec = None;
          bound;
          misuse = false;
        };
        {
          Check.name = "fold_frames_step, any arguments";
          inputs = [ buf; Int_input "off"; Int_input "len"; Int_input "stride" ];
          pre = "true";
          expr = "fold_frames_step buf off len ~stride ~init:0";
          window = ("off", "len");
          spec = None;
          bound;
          misuse = true;
        };
      ]
  in
  frames
  @ [
      {
        Check.name = "fold_step";
        inputs = [ buf; Int_input "off"; Int_input "len" ];
        pre = "true";
        expr = "fold_step buf off len ~init:0";
        window = ("off", "len");
        spec = Some spec;
        bound;
        misuse = false;
      };
      {
        Check.name = "frames_step_record";
        inputs =
          [ buf; Int_input "base"; Int_input "stop"; Int_input "current" ];
        pre =
          Fmt.str "stop >= %d && base >= 0 && base <= Bytes.length buf - stop"
            min_stride;
        expr =
          Fmt.str "frames_step_record ~buf %s %s ~current" (at "base")
            (at "stop");
        window = ("base", "stop");
        spec = Some spec;
        bound;
        misuse = false;
      };
    ]

type target = {
  file : string;
  three_d : string;
  entries : ctx -> Check.entry list;
}

let pp_verdict ppf = function
  | Check.Proved -> Fmt.string ppf "proved"
  | Check.Refuted cex -> Fmt.pf ppf "REFUTED: %s" cex
  | Check.Unknown why -> Fmt.pf ppf "UNKNOWN: %s" why

let contains p s =
  let n = String.length p in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = p || go (i + 1))
  in
  go 0

type options = {
  keep : bool;
  only : string option;
  jobs : int;
  timeout : int;
  bounds_only : bool;
  any_size : bool;
  classify : bool;
  check : string option;
  expect : string option;
  expect_refuted : string option;
}

(* The results of the entries of [t]; an entry outside the subset is a
   result that is not proved. *)
let check_target ~z3 ~threed ~specs o t =
  let raw = spec_prelude ~threed (Filename.concat specs t.three_d) in
  (* Both encodings by default; WIRE_VERIFY_ENCODING=bv or int keeps one. *)
  let encodings =
    match Sys.getenv_opt "WIRE_VERIFY_ENCODING" with
    | Some "bv" -> [ Spec.bitvectors raw ]
    | Some "int" -> [ Spec.integers raw ]
    | _ -> [ Spec.bitvectors raw; Spec.integers raw ]
  in
  let entries =
    List.filter
      (fun (e : Check.entry) ->
        match o.only with None -> true | Some n -> String.equal n e.name)
      (t.entries (Symex.v ~file:t.file (read t.file)))
  in
  List.concat_map
    (fun (e : Check.entry) ->
      Fmt.pr "%s: %s@." (Filename.basename t.file) e.name;
      let ctx = Symex.v ~file:t.file (read t.file) in
      let stop =
        Option.map (fun p property _ -> contains p property) o.expect_refuted
      in
      match
        Check.run ?stop ~z3 ~timeout:o.timeout ~jobs:o.jobs ~keep:o.keep
          ~bounds_only:o.bounds_only ~any_size:o.any_size ~encodings ctx e
      with
      | results, paths ->
          Fmt.pr "  %d paths@." paths;
          List.iter
            (fun (r : Check.result) ->
              Fmt.pr "  %-72s %a (%.2fs)@." r.property pp_verdict r.verdict
                r.seconds)
            results;
          results
      | exception Symex.Unsupported (loc, what) ->
          let why =
            Fmt.str "%s:%d: outside the subset: %s" t.file
              loc.loc_start.pos_lnum what
          in
          Fmt.pr "  %s@." why;
          [ { Check.property = e.name; verdict = Unknown why; seconds = 0. } ])
    entries

let pp_class ppf = function
  | Check.Safety (p, cex) -> Fmt.pf ppf "needed for safety: %s fails: %s" p cex
  | Check.Semantics (p, cex) ->
      Fmt.pf ppf "needed for semantics: %s fails: %s" p cex
  | Check.Redundant -> Fmt.string ppf "REDUNDANT"
  | Check.Redundant_for_safety ->
      Fmt.string ppf "redundant for safety (no answer to compare)"
  | Check.Undecided why -> Fmt.pf ppf "undecided: %s" why

(* Each check of each entry of [t], disabled in turn and classified. *)
let class_name = function
  | Check.Safety _ -> "safety"
  | Check.Semantics _ -> "semantics"
  | Check.Redundant | Check.Redundant_for_safety -> "redundant"
  | Check.Undecided _ -> "undecided"

let selected o (e : Check.entry) =
  match o.only with None -> true | Some n -> String.equal n e.name

(* Each check of each entry of [t], disabled in turn and classified; the
   names of the classes. *)
let classify_target ~z3 o t =
  let src = read t.file in
  let classify (e : Check.entry) =
    let ctx = Symex.v ~file:t.file src in
    ignore (Check.walk ctx e);
    let all = Symex.checks ctx in
    Fmt.pr "%s: %s, %d checks: %s@." (Filename.basename t.file) e.name
      (List.length all)
      (String.concat ", " (List.map (fun (c : Symex.check) -> c.id) all));
    List.filter_map
      (fun (c : Symex.check) ->
        match o.check with
        | Some id when not (String.equal id c.id || String.equal id c.text) ->
            None
        | _ ->
            let walk () = Symex.v ~file:t.file src in
            let t0 = Unix.gettimeofday () in
            let cl =
              Check.classify ~z3 ~timeout:o.timeout ~jobs:o.jobs ~walk e c.id
            in
            Fmt.pr "  %-6s %-40s %a (%.0fs)@." c.id c.text pp_class cl
              (Unix.gettimeofday () -. t0);
            Some (class_name cl))
      all
  in
  List.concat_map
    (fun e -> if selected o e then classify e else [])
    (t.entries (Symex.v ~file:t.file src))

let target_of_arg arg =
  (* FILE:KIND:SPEC:BOUND, KIND being record, or scanner/MIN_STRIDE or
     scanner-ox/MIN_STRIDE for the OxCaml target. *)
  match String.split_on_char ':' arg with
  | [ file; "record"; spec; bound ] ->
      let three_d = List.hd (String.split_on_char '.' spec) ^ ".3d" in
      let bound = int_of_string bound in
      { file; three_d; entries = record ~spec ~bound }
  | [ file; kind; spec; bound ] -> (
      match String.split_on_char '/' kind with
      | [ (("scanner" | "scanner-ox" | "step" | "step-ox") as k); stride ] ->
          let ox = String.ends_with ~suffix:"-ox" k in
          let min_stride = int_of_string stride in
          let three_d = List.hd (String.split_on_char '.' spec) ^ ".3d" in
          let bound = int_of_string bound in
          let entries =
            if String.starts_with ~prefix:"step" k then
              step_scanner ~ox ~spec ~bound ~min_stride
            else scanner ~ox ~spec ~bound ~min_stride
          in
          { file; three_d; entries }
      | _ -> Fmt.invalid_arg "bad target %S" arg)
  | _ -> Fmt.invalid_arg "bad target %S" arg

(* The first executable [name] in [PATH], then in [extra]. *)
let locate name extra =
  let path =
    match Sys.getenv_opt "PATH" with
    | Some p -> String.split_on_char ':' p
    | None -> []
  in
  List.find_map
    (fun dir ->
      let f = Filename.concat dir name in
      if Sys.file_exists f then Some f else None)
    (path @ extra)

let usage () =
  prerr_endline
    "usage: main.exe [--keep] [--only ENTRY] [--jobs N] [--timeout SECONDS] \
     [--bounds-only] [--any-size] [--classify-checks [--check LINE:COL] \
     [--expect CLASS]] [--expect-refuted PROPERTY] SPECS \
     FILE:(record|scanner/N|scanner-ox/N):SPEC:BOUND...";
  exit 2

(* With [--expect-refuted P], the run succeeds when a property whose name
   holds [P] is refuted: it checks that the checker finds a planted defect. *)
let () =
  let rec opts o = function
    | "--keep" :: rest -> opts { o with keep = true } rest
    | "--only" :: n :: rest -> opts { o with only = Some n } rest
    | "--jobs" :: n :: rest -> opts { o with jobs = int_of_string n } rest
    | "--timeout" :: n :: rest -> opts { o with timeout = int_of_string n } rest
    | "--bounds-only" :: rest -> opts { o with bounds_only = true } rest
    | "--any-size" :: rest -> opts { o with any_size = true } rest
    | "--classify-checks" :: rest -> opts { o with classify = true } rest
    | "--check" :: id :: rest -> opts { o with check = Some id } rest
    | "--expect" :: c :: rest -> opts { o with expect = Some c } rest
    | "--expect-refuted" :: p :: rest ->
        opts { o with expect_refuted = Some p } rest
    | rest -> (o, rest)
  in
  let o, args =
    opts
      {
        keep = false;
        only = None;
        jobs = 4;
        timeout = 600;
        bounds_only = false;
        any_size = false;
        classify = false;
        check = None;
        expect = None;
        expect_refuted = None;
      }
      (List.tl (Array.to_list Sys.argv))
  in
  let home = Option.value ~default:"." (Sys.getenv_opt "HOME") in
  let z3 =
    match locate "z3" [] with
    | Some z -> z
    | None -> failwith "z3 is not in PATH"
  and threed =
    match locate "3d.exe" [ Filename.concat home ".local/everparse/bin" ] with
    | Some t -> t
    | None -> failwith "3d.exe is neither in PATH nor in ~/.local/everparse/bin"
  in
  match args with
  | _ :: (_ :: _ as targets) when o.classify ->
      (* Every target is read before any runs: a misplaced option fails at
         once. *)
      let targets = List.map target_of_arg targets in
      let classes = List.concat_map (classify_target ~z3 o) targets in
      (* With [--expect CLASS], every check classified must be of it. *)
      Option.iter
        (fun c ->
          if
            classes = []
            || List.exists (fun c' -> not (String.equal c c')) classes
          then (
            Fmt.epr "a check is not %s@." c;
            exit 1))
        o.expect
  | specs :: (_ :: _ as targets) -> (
      let targets = List.map target_of_arg targets in
      let results =
        List.concat_map (fun t -> check_target ~z3 ~threed ~specs o t) targets
      in
      match o.expect_refuted with
      | None ->
          if List.exists (fun (r : Check.result) -> r.verdict <> Proved) results
          then exit 1
      | Some p ->
          if
            not
              (List.exists
                 (fun (r : Check.result) ->
                   contains p r.property
                   && match r.verdict with Refuted _ -> true | _ -> false)
                 results)
          then (
            Fmt.epr "no property holding %S is refuted@." p;
            exit 1))
  | _ -> usage ()
