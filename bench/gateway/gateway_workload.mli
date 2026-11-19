(** TM frame reassembly: a downlink processor walking the space packets each TM
    transfer frame carries, every field read by Wire's staged {!Wire.Codec.get}.
    The OCaml rows of [bench/gateway] and [bench/apps] run it. *)

val cadu_size : int
(** [cadu_size] is the size of a frame, 1115 bytes. *)

val generate_frames : int -> bytes * int
(** [generate_frames n] is [(buf, packets)]: [n] frames back to back in [buf],
    carrying [packets] space packets of 70 bytes, one spanning each frame
    boundary, except that every 64th frame is idle: its first header pointer,
    0x7FE, says that no packet starts in it. *)

type state
(** The frames and a cursor over them. *)

val state : bytes -> state
(** [state buf] reads the frames of [buf], a whole number of {!cadu_size} bytes,
    from the first. *)

val reset : state -> unit
(** [reset s] moves the cursor of [s] back to the first frame. *)

val step : state -> unit -> unit
(** [step s ()] reads the header of the frame at the cursor and the header of
    each packet starting in it, then moves to the next frame, cycling. *)

val checksum : state -> int
(** [checksum s] reads every frame as {!step} does and folds each frame's
    virtual channel and first header pointer, then each packet's APID and
    sequence count, into an FNV-1a hash modulo 2{^ 63}, as [int] arithmetic
    wraps; [Printf "%u"] prints it as the C row does. *)
