(** The generated code as text, one [let x = e in] binding per line. *)

val decisions : string -> int
(** [decisions s] is the number of branches the code [s] opens, as merlint
    counts them: each [if], each [&&] and [||], each handler of an exception. *)

val local_prefix : string
(** [local_prefix] starts the line that defines a local function. *)

val reads : string -> string -> bool
(** [reads x line] is [true] when [line] reads [x], other than by binding it. *)

val prune : string list -> string list
(** [prune lines] is [lines] without each binding [let x = e in] that no later
    line of its function reads. *)

val free : string -> string list -> bool
(** [free x lines] is [true] when the code of [lines] reads [x] before it binds
    it. *)

val substitute : (string * string) list -> string -> string
(** [substitute env text] is [text] with each word [x] that [env] binds,
    unqualified by a module, replaced by [(e)], [e] its value in [env]. *)
