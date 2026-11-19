(** Shared benchmark framework.

    Every benchmark compares up to three tiers from the same Wire DSL
    definition:
    - EverParse C: generated verified C validator in a tight C loop
    - OCaml->C FFI: calling the EverParse C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Reporting is standardized: ns/op, alloc (words), ratio vs C, throughput. *)

(* -- Timing primitives -- *)

let time_ns ?(now = Unix.gettimeofday) n f =
  Gc.compact ();
  let t0 = now () in
  f ();
  let t1 = now () in
  (t1 -. t0) *. 1e9 /. float_of_int n

let alloc_words n f =
  Gc.full_major ();
  Gc.minor ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  Gc.minor ();
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

(* -- Benchmark specification -- *)

type t = {
  label : string;
  size : int;
  ocaml : unit -> unit;
  reset : unit -> unit;
  c : ((bytes -> int -> int -> int) * bytes) option;
  ffi : ((bytes -> unit) * bytes) option;
  verify : (unit -> unit) option;
}

let v label ~size ?(reset = ignore) ocaml =
  { label; size; ocaml; reset; c = None; ffi = None; verify = None }

let with_c c_loop c_buf t = { t with c = Some (c_loop, c_buf) }
let with_ffi ffi_check ffi_buf t = { t with ffi = Some (ffi_check, ffi_buf) }
let with_verify f t = { t with verify = Some f }

let with_expect ?ffi ?c ~equal ~pp ocaml_result t =
  let verify () =
    let fail label lhs rhs =
      Fmt.failwith "%s mismatch: OCaml=%a %s=%a" t.label pp lhs label pp rhs
    in
    let ocaml_value = ocaml_result () in
    (match ffi with
    | Some ffi_result ->
        let ffi_value = ffi_result () in
        if not (equal ocaml_value ffi_value) then
          fail "FFI" ocaml_value ffi_value
    | None -> ());
    match c with
    | Some c_result ->
        let c_value = c_result () in
        if not (equal ocaml_value c_value) then fail "C" ocaml_value c_value
    | None -> ()
  in
  { t with verify = Some verify }

let cycling ~data ~n_items ~size read_fn =
  let i = ref 0 in
  let reset () = i := 0 in
  let fn () =
    let off = !i mod n_items * size in
    read_fn data off;
    incr i
  in
  (fn, reset)

(** Pack a bytes array into a contiguous buffer. *)
let pack arr ~size =
  let n = Array.length arr in
  let buf = Bytes.create (n * size) in
  Array.iteri (fun i b -> Bytes.blit b 0 buf (i * size) size) arr;
  (buf, n)

(* -- Result -- *)

type result = {
  c_ns : float option;
  ffi_ns : float option;
  ocaml_ns : float;
  alloc : float;
}

let check t =
  (* Verify OCaml tier doesn't crash *)
  t.reset ();
  t.ocaml ();
  (* Verify FFI tier doesn't crash *)
  (match t.ffi with
  | Some (ffi_fn, ffi_buf) ->
      t.reset ();
      ffi_fn ffi_buf
  | None -> ());
  (* Verify C tier accepts the data *)
  (match t.c with
  | Some (c_loop, c_buf) ->
      t.reset ();
      ignore (c_loop c_buf 0 1)
  | None -> ());
  match t.verify with
  | Some f ->
      t.reset ();
      f ()
  | None -> ()

let run_one ~prechecked ~n t =
  if not prechecked then check t;
  let c_ns =
    match t.c with
    | None -> None
    | Some (c_loop, c_buf) ->
        t.reset ();
        let c_total = n * 10 in
        Some (float (c_loop c_buf 0 c_total) /. float c_total)
  in
  let ffi_ns =
    match t.ffi with
    | None -> None
    | Some (ffi_fn, ffi_buf) ->
        t.reset ();
        Some
          (time_ns n (fun () ->
               for _ = 1 to n do
                 ffi_fn ffi_buf
               done))
  in
  t.reset ();
  let ocaml_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          t.ocaml ()
        done)
  in
  t.reset ();
  let alloc = alloc_words n t.ocaml in
  { c_ns; ffi_ns; ocaml_ns; alloc }

(* -- Table formatting -- *)

let ns_fmt_opt = function
  | None -> "-"
  | Some t -> if t < 0.05 then "<0.1" else Fmt.str "%.1f" t

let ns_fmt t = if t < 0.1 then "-" else Fmt.str "%.1f" t
let alloc_fmt w = if w < 0.5 then "0w" else Fmt.str "%.0fw" w

let ratio_fmt ocaml_ns = function
  | None -> "-"
  | Some c ->
      if c > 0.01 && ocaml_ns > 0.1 then Fmt.str "%.1fx" (ocaml_ns /. c)
      else "-"

let throughput_fmt ocaml_ns =
  if ocaml_ns > 0.1 then Fmt.str "%.1f" (1e3 /. ocaml_ns) else "-"

let cols unit_ =
  [
    ("Label", 42);
    ("Size", 5);
    ("C", 11);
    ("OCaml->C FFI", 12);
    ("OCaml", 9);
    ("alloc", 5);
    ("vs C", 5);
    (Fmt.str "M%s/s" unit_, 8);
  ]

let print_header title cols =
  Fmt.pr "\n%s\n%s\n\n" title (String.make (String.length title) '=');
  let widths =
    List.map (fun (name, w) -> (name, max w (String.length name))) cols
  in
  List.iter (fun (name, w) -> Fmt.pr "  %-*s" w name) widths;
  Fmt.pr "\n";
  List.iter (fun (_, w) -> Fmt.pr "  %s" (String.make w '-')) widths;
  Fmt.pr "\n";
  widths

let print_row widths cells =
  List.iter2 (fun (_, w) cell -> Fmt.pr "  %-*s" w cell) widths cells;
  Fmt.pr "\n"

let run_table ~title ~n ?(unit = "op") ?(prechecked = false) specs =
  let w = print_header title (cols unit) in
  List.iter
    (fun t ->
      let r = run_one ~prechecked ~n t in
      print_row w
        [
          t.label;
          (if t.size > 0 then Fmt.str "%dB" t.size else "-");
          ns_fmt_opt r.c_ns;
          ns_fmt_opt r.ffi_ns;
          ns_fmt r.ocaml_ns;
          alloc_fmt r.alloc;
          ratio_fmt r.ocaml_ns r.c_ns;
          throughput_fmt r.ocaml_ns;
        ])
    specs

let section label = Fmt.pr "\n  -- %s --\n" label

(* -- Rows measured in their own process -- *)

let compiler = (if Ox.v then "OxCaml " else "OCaml ") ^ Sys.ocaml_version

let read_file file =
  Bytes.unsafe_of_string (In_channel.with_open_bin file In_channel.input_all)

type filter = All | List | Row of { name : string; mutable matched : bool }

let filter = function
  | None -> All
  | Some "--list" -> List
  | Some name -> Row { name; matched = false }

let finish = function
  | Row { name; matched = false } ->
      Fmt.epr "no row is named %S@." name;
      exit 1
  | All | List | Row _ -> ()

(* Whether [filter] runs the row [name]; under [List], prints [name]. *)
let selects filter name =
  match filter with
  | All -> true
  | List ->
      Fmt.pr "%s@." name;
      false
  | Row r ->
      let m = String.equal r.name name in
      if m then r.matched <- true;
      m

let cpu_time () =
  let t = Unix.times () in
  t.tms_utime +. t.tms_stime

(* Best, 10th percentile, median and 90th percentile, sorted in place. *)
let quantiles times =
  Array.sort Float.compare times;
  let pct p = times.((Array.length times - 1) * p / 100) in
  (times.(0), pct 10, pct 50, pct 90)

(* One warm-up pass, which also gives the result, then [runs] timed passes.
   Each pass reads the wall clock and the process CPU time (user and system,
   from [getrusage]) around the call; CPU time leaves out the time the process
   spends descheduled, which on a loaded machine dominates the wall clock of
   the slower rows. rust/cargo/src/timing.rs does exactly the same. *)
let measure ~corpus ~filter ~name ~items ~runs ~pp f =
  if selects filter name then begin
    let items = float items in
    let result = f () in
    let cpu = Array.make runs 0. and wall = Array.make runs 0. in
    for r = 0 to runs - 1 do
      let w0 = Unix.gettimeofday () in
      let c0 = cpu_time () in
      ignore (Sys.opaque_identity (f ()));
      let c1 = cpu_time () in
      let w1 = Unix.gettimeofday () in
      cpu.(r) <- (c1 -. c0) *. 1e9 /. items;
      wall.(r) <- (w1 -. w0) *. 1e9 /. items
    done;
    let c_best, c10, c50, c90 = quantiles cpu in
    let w_best, w10, w50, w90 = quantiles wall in
    Fmt.pr
      "RESULT\t%s\t%s \
       (%s)\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%.2f\t%a@."
      corpus name compiler c_best c10 c50 c90 w_best w10 w50 w90 pp result
  end

type row = {
  corpus : string;
  name : string;
  cpu : float * float * float * float;
  wall : float * float * float * float;
  value : string;
}

let results files =
  List.concat_map
    (fun file ->
      In_channel.with_open_text file In_channel.input_lines
      |> List.filter_map (fun l ->
          match String.split_on_char '\t' l with
          | [ "RESULT"; corpus; name; c0; c1; c2; c3; w0; w1; w2; w3; value ] ->
              let f = float_of_string in
              Some
                {
                  corpus;
                  name;
                  cpu = (f c0, f c1, f c2, f c3);
                  wall = (f w0, f w1, f w2, f w3);
                  value;
                }
          | _ -> None))
    files

let best r =
  let b, _, _, _ = r.cpu in
  b

let uniq l =
  List.rev (List.fold_left (fun a x -> if List.mem x a then a else x :: a) [] l)

(* The corpus [<family>-<order>] belongs to [<family>]: both orders hold the
   same items, so they must give the same result. *)
let family corpus =
  match String.rindex_opt corpus '-' with
  | Some i -> String.sub corpus 0 i
  | None -> corpus

let corpus_table ~unit rs corpus =
  let rs = List.filter (fun r -> r.corpus = corpus) rs in
  let rs = List.sort (fun a b -> Float.compare (best a) (best b)) rs in
  let fastest = match rs with r :: _ -> best r | [] -> nan in
  Fmt.pr "@.Corpus %s, ranked by best CPU time:@.@." corpus;
  Fmt.pr "%-58s %8s %8s %8s %8s %9s %9s %8s@." "Implementation" "CPU" "CPU"
    "CPU" "CPU" "wall" "wall" "vs best";
  Fmt.pr "%-58s %8s %8s %8s %8s %9s %9s %8s@."
    ("(ns/" ^ unit corpus ^ ")")
    "best" "p10" "median" "p90" "best" "median" "(CPU)";
  Fmt.pr "%s@." (String.make 127 '-');
  List.iter
    (fun r ->
      let c0, c1, c2, c3 = r.cpu and w0, _, w2, _ = r.wall in
      Fmt.pr "%-58s %8.2f %8.2f %8.2f %8.2f %9.2f %9.2f %7.2fx@." r.name c0 c1
        c2 c3 w0 w2 (c0 /. fastest))
    rs

(* Best CPU time per corpus, one column each, ranked by the first corpus. *)
let summary ~unit rs corpora =
  let names = uniq (List.map (fun r -> r.name) rs) in
  let cell name corpus =
    List.find_opt (fun r -> r.name = name && r.corpus = corpus) rs
  in
  let first = List.hd corpora in
  let key name =
    match cell name first with Some r -> best r | None -> infinity
  in
  let names = List.sort (fun a b -> Float.compare (key a) (key b)) names in
  (match uniq (List.map unit corpora) with
  | [ u ] -> Fmt.pr "@.Best CPU ns/%s, by corpus:@.@." u
  | _ ->
      let per c = c ^ ": ns/" ^ unit c in
      Fmt.pr "@.Best CPU time (%a), by corpus:@.@."
        Fmt.(list ~sep:comma string)
        (List.map per corpora));
  Fmt.pr "%-58s" "Implementation";
  List.iter (fun c -> Fmt.pr " %15s" c) corpora;
  Fmt.pr "@.%s@." (String.make (58 + (16 * List.length corpora)) '-');
  List.iter
    (fun name ->
      Fmt.pr "%-58s" name;
      List.iter
        (fun c ->
          match cell name c with
          | Some r -> Fmt.pr " %15.2f" (best r)
          | None -> Fmt.pr " %15s" "-")
        corpora;
      Fmt.pr "@.")
    names

let agree rs =
  let families = uniq (List.map (fun r -> family r.corpus) rs) in
  let agree =
    List.map
      (fun fam ->
        let mine = List.filter (fun r -> family r.corpus = fam) rs in
        match uniq (List.map (fun r -> r.value) mine) with
        | [ v ] ->
            Fmt.pr "@.Every implementation gave the same result on %s: %s.@."
              fam v;
            true
        | _ ->
            Fmt.epr "The implementations disagree on %s:@." fam;
            List.iter
              (fun r -> Fmt.epr "  %s on %s: %s@." r.name r.corpus r.value)
              mine;
            false)
      families
  in
  List.for_all Fun.id agree

let report ~unit files =
  let rs = results files in
  if rs = [] then
    Fmt.failwith "no RESULT lines in %a" Fmt.(list ~sep:sp string) files;
  let corpora = uniq (List.map (fun r -> r.corpus) rs) in
  List.iter (corpus_table ~unit rs) corpora;
  summary ~unit rs corpora;
  if not (agree rs) then exit 1
