(** Shared benchmark framework.

    Every benchmark compares up to three tiers from the same Wire DSL
    definition:
    - EverParse C: generated verified C validator in a tight C loop
    - OCaml->C FFI: calling the EverParse C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Reporting is standardized: ns/op, alloc (words), ratio vs C, throughput. *)

(* ── Timing primitives ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
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

(* ── Benchmark specification ── *)

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

(* ── Result ── *)

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
  | Some (ffi_fn, ffi_buf) -> ffi_fn ffi_buf
  | None -> ());
  (* Verify C tier accepts the data *)
  (match t.c with
  | Some (c_loop, c_buf) -> ignore (c_loop c_buf 0 1)
  | None -> ());
  (* Run verify function if present — checks OCaml and C agree *)
  match t.verify with
  | Some f -> f ()
  | None -> ()

let run_one ~n t =
  check t;
  let c_ns =
    match t.c with
    | None -> None
    | Some (c_loop, c_buf) ->
        let c_total = n * 10 in
        Some (float (c_loop c_buf 0 c_total) /. float c_total)
  in
  let ffi_ns =
    match t.ffi with
    | None -> None
    | Some (ffi_fn, ffi_buf) ->
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

(* ── Table formatting ── *)

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

let run_table ~title ~n ?(unit = "op") specs =
  let w = print_header title (cols unit) in
  List.iter
    (fun t ->
      let r = run_one ~n t in
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
