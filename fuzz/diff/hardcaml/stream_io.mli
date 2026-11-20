(** Shared cycle/handshake adapter for the differential streaming lane. *)

type beat = {
  data : bytes;
  keep : Hardcaml.Bits.t;
  last : bool;
  error : bool;
  tag : int;
}

type event = Beat of beat | Reset

type result = {
  in_scope : bool;
  accepted : bool;
  consumed : int;
  length : int;
  frame_id : int64;
  errors : int;
  fields : (string * int) list;
}

type t
type statistics = { max_pending : int; reset_cancelled : int }

val equal_result : result -> result -> bool
(** Compare all transport and scalar metadata explicitly. *)

val statistics : t -> statistics
(** Completed inputs awaiting metadata at the peak, and completed inputs
    cancelled by reset, during the most recent run. *)

val v :
  packing:Wire_hardcaml.Stream.packing ->
  bytes:int ->
  capacity:int ->
  max_length:int ->
  Wire_ocaml.Plan.t ->
  t
(** Elaborate a generic stream simulator from the same checked plan. *)

val frame : bytes:int -> tag:int -> parts:int list -> bytes -> event list
(** Segment a frame into contiguous-low-mask beats, then full beats after
    [parts]. *)

val run :
  ?stalls:bool ->
  ?bubbles:bool ->
  ?seed:int ->
  ?stall_burst:int ->
  t ->
  event list ->
  (int * result) list
(** Reset, drive the schedule and drain. Check stable stalled outputs,
    exactly-once completion after TLAST, reset cancellation and zero rejected
    metadata. *)
