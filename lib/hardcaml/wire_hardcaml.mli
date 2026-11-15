(** Bounded hardware decoding from the shared checked plan. Byte zero occupies
    the least significant eight bits of [data]. *)

type t

val v :
  ?pipeline:Hardcaml.Reg_spec.t * Hardcaml.Signal.t ->
  ?max_length:int ->
  capacity:int ->
  data:Hardcaml.Signal.t ->
  length:Hardcaml.Signal.t ->
  Wire_ocaml.Plan.t ->
  t
(** Elaborate the checked plan for a fixed physical bus. [length] is an unsigned
    logical byte count; byte zero occupies the low bits of [data]. The host must
    have 63-bit native integers. Invalid interfaces and unsupported expressions
    are refused during elaboration. [pipeline=(spec, enable)] inserts one
    enabled register boundary between speculative extraction and validation. The
    caller must delay its valid signal by the same enabled edge.

    [max_length] defaults to [capacity]. A larger maximum allows opaque payloads
    beyond the captured prefix; every scalar read must be proven within
    [capacity] or elaboration fails. Logical prefix and region checks remain,
    and additional physical read checks prevent uncaptured data from escaping.
    [capacity] is 1–256 bytes; [max_length] is at most 1,048,576 bytes. *)

val read_prefix : t -> int
(** Proven captured prefix bound for a decoder whose maximum exceeds its bus.
    For complete-frame decoders, returns the physical capacity. *)

val in_scope : t -> Hardcaml.Signal.t
(** The configured logical length contract, separate from codec rejection. *)

val accepted : t -> Hardcaml.Signal.t
(** Every codec check and the capacity contract passed. *)

val consumed : t -> Hardcaml.Signal.t
(** Native 63-bit semantic prefix consumed, zero for rejected/out-of-scope
    input. *)

val fields : t -> (string * Hardcaml.Signal.t) list
(** Selected scalar snapshots. Absent and rejected fields are zero. *)

val to_circuit : name:string -> t -> Hardcaml.Circuit.t
(** Export status, semantic consumption and [field_<name>] output ports. Input
    ports come from the signals supplied when constructing the decoder. *)

val register : ?enable:Hardcaml.Signal.t -> Hardcaml.Reg_spec.t -> t -> t
(** Delay the complete decoder snapshot by one enabled clock edge. Scalar
    masking follows the registers, so rejected speculative values stay private.
    The caller must delay its valid signal by the same enabled edge. *)

val reduce :
  t ->
  f:((string * Hardcaml.Signal.t) list -> Hardcaml.Signal.t) ->
  Hardcaml.Signal.t
(** Fuse a combinational consumer before masking rejected results. The callback
    sees speculative wire values; only the masked result may be observed. *)

(** Unstable helpers shared with backend equivalence checks. *)
module Private : sig
  val contiguous_keep : Hardcaml.Signal.t -> Hardcaml.Signal.t
  (** True for a contiguous low-byte mask, including zero and all ones. *)
end

(** Metadata-only, multi-beat ingress. Payload forwarding is independent. *)
module Stream : sig
  type packing = Full_beats | Contiguous
  type decoder = t
  type t

  val v :
    ?packing:packing ->
    capacity:int ->
    max_length:int ->
    clock:Hardcaml.Signal.t ->
    reset:Hardcaml.Signal.t ->
    valid:Hardcaml.Signal.t ->
    data:Hardcaml.Signal.t ->
    keep:Hardcaml.Signal.t ->
    last:Hardcaml.Signal.t ->
    error:Hardcaml.Signal.t ->
    output_ready:Hardcaml.Signal.t ->
    Wire_ocaml.Plan.t ->
    t
  (** AXI4-Stream framing with byte zero in the low data bits and 1–64 byte
      beats. TKEEP must be a contiguous low mask. {!constructor-Full_beats}
      requires full nonfinal beats and uses fixed capture lanes.
      {!constructor-Contiguous} additionally accepts partial intermediate beats,
      concatenating them without gaps. Zero TKEEP is legal only with TLAST.
      [error] marks a corrupted input beat. Sparse masks and input errors reject
      the entire frame; input drains through TLAST and then recovers.

      Only the proven header prefix is stored. Length saturates at
      [max_length + 1] on overflow, which rejects and drains the frame. One beat
      per cycle is accepted with a continuously ready sink, including
      back-to-back single-beat frames. Four result slots are reserved at
      accepted TLAST. The decoder stages advance independently of output READY.
      When all slots are reserved, ingress resumes one cycle after an output
      transfer releases credit. With no earlier pending result, metadata appears
      three cycles after the final input transfer and remains stable under
      backpressure. Synchronous active-high [reset] aborts partial frames and
      pending results. *)

  val input_ready : t -> Hardcaml.Signal.t
  (** Input TREADY. *)

  val output_valid : t -> Hardcaml.Signal.t
  (** Metadata is valid only after the complete frame's TLAST. *)

  val frame_length : t -> Hardcaml.Signal.t
  (** Actual bytes received, saturated at the maximum plus one. *)

  val frame_id : t -> Hardcaml.Signal.t
  (** Unsigned 64-bit sequence assigned at accepted TLAST, starting at zero.
      Includes rejected frames, wraps modulo 2^64 and resets to zero. *)

  val completed_frames : t -> Hardcaml.Signal.t
  (** Live unsigned completed-input counter, modulo 2^64; independent of output
      transfers. Diagnostic only: this is not part of the stalled metadata. *)

  val current_length : t -> Hardcaml.Signal.t
  (** Live partial-frame byte count, saturated at the maximum plus one. Not
      stalled output metadata; resets to zero on TLAST or reset. *)

  val current_error : t -> Hardcaml.Signal.t
  (** Live sticky error flags for the partial frame, cleared at TLAST or reset.
      Diagnostic only; not part of the stalled output metadata. *)

  val transport_error : t -> Hardcaml.Signal.t
  (** Sticky error bits: input error (bit 0), malformed TKEEP (1), overflow (2).
  *)

  val decoder : t -> decoder
  (** Scalar result. Rejected fields and consumption are zero. *)

  val to_circuit : name:string -> t -> Hardcaml.Circuit.t
  (** Export the handshake, transport status and decoder snapshot. Hardcaml may
      prune unused inputs in constant plans; integration adapters must check the
      emitted manifest for the input interface they require. *)

  (** Unstable views of existing state for temporal induction. These signals are
      not exported by {!val-to_circuit}; no observation registers are added. *)
  module Private : sig
    type state

    val state : t -> state
    (** View the existing queue and decoder state without adding storage. *)

    val counts : state -> Hardcaml.Signal.t * Hardcaml.Signal.t
    (** Reserved and queued result counts. *)

    val indices : state -> Hardcaml.Signal.t * Hardcaml.Signal.t
    (** Read and write indices. *)

    val first :
      state ->
      Hardcaml.Signal.t
      * Hardcaml.Signal.t
      * Hardcaml.Signal.t
      * Hardcaml.Signal.t
    (** First in-flight snapshot: valid, ID, reported length and errors. *)

    val second :
      state ->
      Hardcaml.Signal.t
      * Hardcaml.Signal.t
      * Hardcaml.Signal.t
      * Hardcaml.Signal.t
    (** Second in-flight snapshot: valid, ID, reported length and errors. *)

    val slots :
      state -> (Hardcaml.Signal.t * Hardcaml.Signal.t * Hardcaml.Signal.t) list
    (** ID, reported length and errors for each result slot. *)

    val bounds : state -> Hardcaml.Signal.t * Hardcaml.Signal.t list
    (** Accepted-result bounds for the producer and each result slot. *)
  end
end
