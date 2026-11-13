(** Identifiers that generated code derives from field names.

    Each code generator spells a field's name into the identifiers of its
    target: 3D and the F* EverParse writes from it, the C of a plug, the OCaml
    of a stub. Escaping is injective: a keyword gets a trailing underscore, and
    so does every name that is a keyword followed by underscores, so [end],
    [end_] and [end__] stay three names ([end_], [end__], [end___]) in every
    target. Folding case or replacing characters a target cannot spell can still
    give two names one identifier, so a generator computes the identifiers it
    emits for each namespace and refuses with {!clash} what collides. *)

val ocaml_keywords : string list
(** OCaml's keywords, including [effect] and the infix operators spelled as
    words ([land], [mod], ...). *)

val oxcaml_keywords : string list
(** The keywords OxCaml adds, which the stock compiler reads as names. *)

val c_keywords : string list
(** The keywords of C11, C23 and GNU C, and the object-like macros ([NULL],
    [TRUE], [FALSE]) of the headers generated C includes. *)

val is_identifier : string -> bool
(** [is_identifier name] is [true] when [name] matches [[A-Za-z_][A-Za-z0-9_]*],
    the identifiers 3D, C and OCaml share. Nothing escapes any other name into
    all three. *)

val rust_keywords : string list
(** Rust's strict keywords and the words reserved for future use, up to the 2024
    edition ([async], [dyn], [gen], [try], ...). *)

val escapes : ?suffix:string -> string list -> string -> bool
(** [escapes ?suffix keywords name] is [true] when {!escape} changes [name]. *)

val escape : ?suffix:string -> string list -> string -> string
(** [escape ?suffix keywords name] is [name ^ suffix] when [name] is one of
    [keywords] followed by any number of [suffix]es and underscores, and [name]
    otherwise. [suffix] defaults to ["_"]. The map is injective, and its result
    is a keyword only if [keywords] holds a keyword followed by [suffix] (none
    of the lists here does): [escape ["end"]] maps [end] to [end_] and [end_] to
    [end__], and leaves [ending] alone. *)

val ocaml_label : ?oxcaml:bool -> string -> string
(** [ocaml_label name] is the OCaml record label or value name for [name]:
    lower-cased, with any character outside an OCaml identifier replaced by [_],
    prefixed with [f_] when it would start with a digit, and {!escape}d against
    the keywords. [oxcaml] (default [true]) escapes OxCaml's keywords too, so
    the name is valid on either compiler. *)

val rust_ident : string -> string
(** [rust_ident name] is the Rust field or function name for [name]:
    lower-cased, with any character outside a Rust identifier replaced by [_],
    prefixed with [f_] when it would start with a digit or be the lone [_]
    pattern, and {!escape}d against the keywords. *)

val clash : ('a * string) list -> ('a * 'a * string) option
(** [clash idents] is [Some (a, b, id)] for the first identifier [id] that two
    distinct sources [a] and [b] both emit, in the order of [idents], and [None]
    when every identifier has one source. A source may emit several identifiers.
*)
