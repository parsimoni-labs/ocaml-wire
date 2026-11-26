(** CLCW polling: a COP-1 receiver counting the anomalies in a stream of CLCW
    words, each word checked by Wire's {!Wire.Codec.Word.check} and its fields
    taken with {!Wire.Codec.Word.read}. The OCaml rows of [bench/clcw] and
    [bench/apps] run it. *)

val word_size : int
(** [word_size] is the size of a CLCW word, 4 bytes. *)

val generate_stream : int -> bytes
(** [generate_stream n] is [n] CLCW words back to back, their report values
    counting up modulo 256, with anomalies: wait set on every 61st word,
    retransmit on every 97th, lockout on every 211th, and a report value that
    skips one on every 127th. *)

type state
(** The words, a cursor over them and the anomaly count. *)

val state : bytes -> state
(** [state buf] polls the words of [buf], a whole number of {!word_size} bytes,
    from the first, with no anomaly counted. *)

val reset : state -> unit
(** [reset s] moves the cursor of [s] back to the first word and its anomaly
    count and expected report value to 0. *)

val step : state -> unit -> unit
(** [step s ()] checks the word at the cursor and moves to the next, cycling. *)

val run_all : state -> int
(** [run_all s] checks every word, counting an anomaly for a word with lockout,
    wait or retransmit set or a report value other than one past the previous
    word's, and returns the anomaly count of [s]. *)

val report : out:bytes -> int -> unit
(** [report ~out n] writes in [out] the [n] CLCW words a FARM reports, word [i]
    {!Space.clcw_default} with report value [i mod 256], lockout when the 12 low
    bits of [i] are all set, wait when its 6 low bits are, and retransmit bit 13
    of [i], by Wire's staged {!Wire.Codec.writer} of the default and those four
    fields. *)
