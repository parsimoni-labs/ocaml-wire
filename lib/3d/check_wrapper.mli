(** Whole-buffer hardening of EverParse's [<Base>CheckWire<Codec>] wrapper.

    EverParse's wrapper returns [TRUE] on any successful validator result, so a
    valid record followed by trailing bytes passes. These rewrite the wrapper to
    also require that the validator consumed the whole buffer. {!Wire_3d}
    re-exports this module. *)

val harden_wrapper_source : string -> string
(** [harden_wrapper_source src] is the EverParse wrapper [src] with its success
    tail rewritten to also require that the validator consumed the whole buffer.
    The validator's status variable is read back out of [src], since EverParse
    has renamed it across releases.

    @raise Failure
      if [src] is neither a wrapper shape this recognizes nor one already
      carrying its own consumption check, rather than returning a prefix
      recognizer. *)

val harden_wrapper : outdir:string -> string -> unit
(** [harden_wrapper ~outdir base] applies {!harden_wrapper_source} in place to
    [outdir]'s [<base>Wrapper.c], if that file exists.

    @raise Failure naming the file if its wrapper shape is not recognized. *)
