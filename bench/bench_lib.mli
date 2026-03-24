(** Shared benchmark framework.

    Every benchmark compares up to three tiers from the same Wire DSL
    definition:
    - EverParse C: generated verified C validator in a tight C loop
    - OCaml->C FFI: calling the EverParse C validator from OCaml
    - Pure OCaml: Wire.Codec.get/set (zero-copy field access)

    Reporting is standardized: ns/op, alloc (words), ratio vs C, throughput. *)

(** {1 Timing primitives} *)

val time_ns : int -> (unit -> unit) -> float
(** [time_ns n f] runs [f ()] and returns elapsed nanoseconds per iteration. *)

val alloc_words : int -> (unit -> unit) -> float
(** [alloc_words n f] runs [f] [n] times and returns minor words allocated per
    call. *)

(** {1 Benchmark specifications} *)

type t
(** A benchmark spec with up to 3 tiers. *)

val v : string -> size:int -> ?reset:(unit -> unit) -> (unit -> unit) -> t
(** [v label ~size ?reset ocaml] creates a benchmark with the OCaml tier only.
    [reset] is called before each measurement phase to reinitialize mutable
    state (cycling index, counters, etc.). *)

val with_c : (bytes -> int -> int -> int) -> bytes -> t -> t
(** [with_c c_loop buf t] adds the EverParse C tier. *)

val with_ffi : (bytes -> unit) -> bytes -> t -> t
(** [with_ffi ffi_fn buf t] adds the OCaml->C FFI tier. *)

val with_verify : (unit -> unit) -> t -> t
(** [with_verify f t] adds a verification function that runs during {!check} to
    confirm OCaml and C tiers produce the same results. *)

val cycling :
  data:bytes ->
  n_items:int ->
  size:int ->
  (bytes -> int -> unit) ->
  (unit -> unit) * (unit -> unit)
(** [cycling ~data ~n_items ~size read_fn] returns [(fn, reset)] where [fn]
    cycles through [n_items] packed items in [data], and [reset] rewinds the
    index to 0. *)

(** {1 Running and reporting} *)

val check : t -> unit
(** [check t] verifies all tiers agree on the test data: OCaml read succeeds,
    FFI validation returns [true], and C loop accepts the buffer. Raises on
    mismatch. Called automatically by {!run_table} before timing. *)

val run_table : title:string -> n:int -> ?unit:string -> t list -> unit
(** [run_table ~title ~n specs] runs all specs and prints a table. [~unit] sets
    the throughput column header (default ["op"] -> ["Mop/s"]; ["pkt"] ->
    ["Mpkt/s"]). *)

(** {1 Utilities} *)

val section : string -> unit
(** Print a section divider. *)

val pack : bytes array -> size:int -> bytes * int
(** [pack arr ~size] packs an array of byte buffers into one contiguous buffer.
    Returns the buffer and the number of items. *)
