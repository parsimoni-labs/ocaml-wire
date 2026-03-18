(** Shared benchmark framework.

    Every benchmark compares three tiers from the same Wire DSL definition:
    - EverParse C: generated verified C validator in a tight C loop
    - OCaml->C FFI: calling the EverParse C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Reporting is standardized: ns/op, alloc (words), ratio vs C, GB/s. *)

(** {1 Timing primitives} *)

val time_ns : int -> (unit -> unit) -> float
(** [time_ns n f] runs [f ()] and returns elapsed nanoseconds per iteration. *)

val alloc_words : int -> (unit -> unit) -> float
(** [alloc_words n f] runs [f] [n] times and returns minor words allocated per
    call. *)

(** {1 Benchmark specifications} *)

type read_spec = {
  label : string;
  size : int;
  c_loop : bytes -> int -> int -> int;
  c_buf : bytes;
  ffi_check : bytes -> bool;
  ffi_buf : bytes;
  ocaml_read : unit -> unit;
}
(** A read benchmark spec: codec, data, and all three tiers. *)

type write_spec = { w_label : string; ocaml_write : unit -> unit }
(** A write benchmark spec. *)

val of_contiguous :
  label:string ->
  size:int ->
  data:bytes ->
  n_items:int ->
  c_loop:(bytes -> int -> int -> int) ->
  ffi_check:(bytes -> bool) ->
  read_fn:(bytes -> int -> unit) ->
  read_spec
(** Create a read_spec from a contiguous buffer of packed items. *)

val pack : bytes array -> size:int -> bytes * int
(** [pack arr ~size] packs an array of byte buffers into one contiguous buffer.
    Returns the buffer and the number of items. *)

(** {1 Running and reporting} *)

type result = { c_ns : float; ffi_ns : float; ocaml_ns : float; alloc : float }
(** Timing results for a single read benchmark. *)

val run_one : n:int -> read_spec -> result
(** [run_one ~n spec] runs a single read benchmark with [n] iterations per tier.
*)

val run_reads : n:int -> read_spec list -> unit
(** Run and print a table of read benchmarks. *)

val run_writes : n:int -> write_spec list -> unit
(** Run and print a table of write benchmarks. *)

val section : string -> unit
(** Print a section header. *)
