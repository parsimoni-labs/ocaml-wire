(** Identifiers that generated code derives from field names.

    Each code generator spells a field's name into the identifiers of its
    target: 3D and the F* EverParse writes from it, the C of a plug, the OCaml
    of a stub. Escaping a keyword with a trailing underscore, or folding case,
    can spell two fields' names as one identifier, so a generator computes the
    identifiers it emits for each namespace and refuses with {!clash} what would
    still collide. *)

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

val escape : string list -> string -> string
(** [escape keywords name] is [name ^ "_"] when [name] is one of [keywords], and
    [name] otherwise. *)

val ocaml_label : ?oxcaml:bool -> string -> string
(** [ocaml_label name] is the OCaml record label or value name for [name]:
    lower-cased, with any character outside an OCaml identifier replaced by [_],
    prefixed with [f_] when it would start with a digit, and escaped when a
    keyword. [oxcaml] (default [true]) escapes OxCaml's keywords too, so the
    name is valid on either compiler. *)

val clash : ('a * string) list -> ('a * 'a * string) option
(** [clash idents] is [Some (a, b, id)] for the first identifier [id] that two
    distinct sources [a] and [b] both emit, in the order of [idents], and [None]
    when every identifier has one source. A source may emit several identifiers.
*)
