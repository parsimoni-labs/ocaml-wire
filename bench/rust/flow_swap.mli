(** The flow counter of the swap rows over the generated header modules and the
    sink {!Flow_table}; [Flow_swap_interpreted] is the same over the interpreted
    ones, and [Flow_swap_parse] and [Flow_swap_interpreted_parse] the two over
    {!Flow_sum}. *)

val run : bytes -> int array -> int array -> unit
(** [run buf offs lens] counts each frame of the pcap capture [buf], held whole,
    whose frames start at [offs] with the captured lengths [lens]: an IPv4 flow
    in the sink, any other packet in {!Flow_mix}. *)
