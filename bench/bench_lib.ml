(** Shared benchmark framework.

    Every benchmark compares three tiers from the same Wire DSL definition:
    - EverParse C: generated verified C validator in a tight C loop
    - OCaml->C FFI: calling the EverParse C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Reporting is standardized: ns/op, alloc (words), ratio vs C, GB/s. *)

(* ── Timing primitives ── *)

let time_ns n f =
  Gc.compact ();
  let t0 = Unix.gettimeofday () in
  f ();
  let t1 = Unix.gettimeofday () in
  (t1 -. t0) *. 1e9 /. float_of_int n

let alloc_words n f =
  Gc.full_major ();
  let before = (Gc.quick_stat ()).minor_words in
  for _ = 1 to n do
    f ()
  done;
  let after = (Gc.quick_stat ()).minor_words in
  (after -. before) /. float_of_int n

(* ── Benchmark specification ── *)

type read_spec = {
  label : string;
  size : int;  (** Wire size per item *)
  c_loop : bytes -> int -> int -> int;  (** C loop: buf off count -> ns *)
  c_buf : bytes;  (** Buffer for C loop *)
  ffi_check : bytes -> bool;  (** FFI validator *)
  ffi_buf : bytes;  (** Single-item buffer for FFI *)
  ocaml_read : unit -> unit;  (** One OCaml get operation *)
}
(** A read benchmark spec: codec, data, and all three tiers. *)

type write_spec = { w_label : string; ocaml_write : unit -> unit }
(** A write benchmark spec. *)

(** Create a read_spec from a contiguous buffer of packed items. The OCaml read
    cycles through items in the buffer. *)
let of_contiguous ~label ~size ~data ~n_items ~c_loop ~ffi_check ~read_fn =
  let i = ref 0 in
  {
    label;
    size;
    c_loop;
    c_buf = data;
    ffi_check;
    ffi_buf = Bytes.sub data 0 size;
    ocaml_read =
      (fun () ->
        let off = !i mod n_items * size in
        read_fn data off;
        incr i);
  }

(** Pack a bytes array into a contiguous buffer. *)
let pack arr ~size =
  let n = Array.length arr in
  let buf = Bytes.create (n * size) in
  Array.iteri (fun i b -> Bytes.blit b 0 buf (i * size) size) arr;
  (buf, n)

(* ── Result ── *)

type result = { c_ns : float; ffi_ns : float; ocaml_ns : float; alloc : float }

let run_one ~n (s : read_spec) =
  let c_total = n * 10 in
  let c_ns = float (s.c_loop s.c_buf 0 c_total) /. float c_total in
  let ffi_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          ignore (s.ffi_check s.ffi_buf)
        done)
  in
  let ocaml_ns =
    time_ns n (fun () ->
        for _ = 1 to n do
          s.ocaml_read ()
        done)
  in
  let alloc = alloc_words n s.ocaml_read in
  { c_ns; ffi_ns; ocaml_ns; alloc }

(* ── Table formatting ── *)

let ns_fmt t = if t < 0.1 then "-" else Fmt.str "%.1f" t
let alloc_fmt w = if w < 0.5 then "0w" else Fmt.str "%.0fw" w

let ratio_fmt num denom =
  if denom > 0.1 && num > 0.1 then Fmt.str "%.1fx" (denom /. num) else "-"

let gbps_fmt ns size =
  if ns > 0.1 then Fmt.str "%.1f" (float size /. ns) else "-"

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

let read_cols =
  [
    ("Field", 42);
    ("Size", 5);
    ("EverParse C", 11);
    ("OCaml->C FFI", 12);
    ("OCaml get", 9);
    ("alloc", 5);
    ("vs C", 5);
    ("GB/s", 5);
  ]

(** Run and print a list of read benchmarks. *)
let run_reads ~n specs =
  let w = print_header "Read: field access (ns/op)" read_cols in
  List.iter
    (fun s ->
      let r = run_one ~n s in
      print_row w
        [
          s.label;
          Fmt.str "%dB" s.size;
          ns_fmt r.c_ns;
          ns_fmt r.ffi_ns;
          ns_fmt r.ocaml_ns;
          alloc_fmt r.alloc;
          ratio_fmt r.ocaml_ns r.c_ns;
          gbps_fmt r.ocaml_ns s.size;
        ])
    specs

(** Run and print a list of write benchmarks. *)
let run_writes ~n specs =
  let w =
    print_header "Write: in-place field mutation (ns/op)"
      [ ("Field", 42); ("OCaml set", 9); ("alloc", 5) ]
  in
  List.iter
    (fun s ->
      let ns =
        time_ns n (fun () ->
            for _ = 1 to n do
              s.ocaml_write ()
            done)
      in
      let alloc = alloc_words n s.ocaml_write in
      print_row w [ s.w_label; ns_fmt ns; alloc_fmt alloc ])
    specs

let section label = Fmt.pr "\n  -- %s --\n" label
