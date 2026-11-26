(** TM frame reassembly: a downlink processor walking the space packets each TM
    transfer frame carries, each header read by Wire's {!Wire.Codec.read_word}
    and its fields taken with {!Wire.Codec.of_word}. The OCaml rows of
    [bench/gateway] and [bench/apps] run it. *)

val cadu_size : int
(** [cadu_size] is the size of a frame, 1115 bytes. *)

val pkt_size : int
(** [pkt_size] is the size of each packet a frame carries, 70 bytes. *)

val checksum_init : int
(** [checksum_init] is the FNV-1a offset basis {!checksum} starts from, modulo
    2{^ 63}. *)

val checksum_prime : int
(** [checksum_prime] is the FNV-1a prime {!hash_int} multiplies by. *)

val hash_int : int -> int -> int
(** [hash_int h v] folds [v] into the FNV-1a hash [h], modulo 2{^ 63} as [int]
    arithmetic wraps; [Printf "%u"] prints it as the Rust and C rows do. *)

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
    each packet starting in it, then moves to the next frame, cycling. A header
    the buffer cuts ends the frame: its packets after it, or all of them for a
    frame header, are not read. *)

val checksum : state -> int
(** [checksum s] reads every frame as {!step} does and folds each frame's
    virtual channel and first header pointer, then each packet's APID and
    sequence count, into an FNV-1a hash. *)

val frame : out:bytes -> int -> unit
(** [frame ~out n] builds [n] frames in [out] from a stream of 70-byte packets,
    laid out as {!generate_frames} lays them out, with no idle frame: each
    frame's primary header, {!Space.tm_frame_default} with virtual channel
    [f mod 8], master channel count [f mod 256], virtual channel count
    [f / 8 mod 256] and the first header pointer of frame [f], then the header
    of each packet starting in its data field, packet [p] a telemetry packet
    with a secondary header, unsegmented, with APID [p mod 2048], sequence count
    [p mod 16384] and 64 bytes of data. Every header is written by Wire's staged
    {!Wire.Codec.writer} of its default and the fields that vary. *)
