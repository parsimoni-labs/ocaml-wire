(** How generated code holds integers, and how it writes a literal, an operation
    and a comparison on them, as OCaml or as F*. Internal to {!Wire_ocaml}. *)

(** How the code holds integers. [Tagged] is [int]. [Boxed] is [int64] for
    values and [int] for positions. [Unboxed] is OxCaml's [int64_u]. [Fstar] is
    [Tagged] written as F*, [Fstar64] is [Boxed] and [Fstar_u] is [Unboxed], an
    [int64_u] as its 64-bit pattern. *)
type t = Tagged | Boxed | Unboxed | Fstar | Fstar64 | Fstar_u

val rep : t -> t
(** [rep num] is how [num] holds values: {!constructor-Tagged} for
    {!constructor-Fstar}, {!constructor-Boxed} for {!constructor-Fstar64},
    {!constructor-Unboxed} for {!constructor-Fstar_u}, [num] otherwise. *)

val is_fstar : t -> bool
(** [is_fstar num] is [true] when [num] is written as F*. *)

val pos_num : t -> t
(** [pos_num num] is how code that holds integers as [num] holds positions,
    sizes and bounds: as [int] where it holds values as [int64]. *)

val i64_ops : t -> string
(** [i64_ops num] is the module of the operations on values held as [num], other
    than [int]. *)

val lit64 : t -> string -> string
(** [lit64 num digits] is the 64-bit literal [digits], an OCaml literal without
    its suffix, as [num] writes it. *)

val int64_of : t -> string -> string
(** [int64_of num from] is the function that makes the [int64] (or [int64_u])
    [num] holds of a value of [from], ["int"] or ["int32"]. *)

val widen_int : t -> string -> string
(** [widen_int num x] is the [int] variable [x] as an [int64]. *)

val bits_of : t -> string
(** [bits_of num] is the function an F* walk states a bitfield with: of an
    [int], or of an [int64] pattern. *)

val values : t -> Decision.values
(** [values num] is how a walk of [num] holds integer values. *)

val lit : t -> int -> string
(** [lit num n] is the literal [n] as [num] holds it. *)

val unboxed_op : string -> string
(** [unboxed_op op] is the name of the [Int64] function of the operator [op]. *)

val fstar_op : string -> string
(** [fstar_op op] is the primitive of [P] an F* walk calls for the OCaml
    operator [op]. *)

val fstar64_op : string -> string
(** [fstar64_op op] is {!fstar_op} for the [int64] operation of [op]. *)

val binop : t -> string -> string -> string -> string
(** [binop num op a b] is [a op b] on values held as [num]. *)

val relation : t -> string -> string -> string -> string
(** [relation num op a b] is the comparison [a op b] of values held as [num]. *)

val outside_range : t -> string -> lo:int -> hi:int -> string
(** [outside_range num v ~lo ~hi] is the test that [v] lies outside [[lo, hi]].
*)

val normalise63 : t -> string -> string
(** [normalise63 num v] is the [int64] or [int64_u] [v] holding the value of a
    63-bit [int] sign-extended. *)
