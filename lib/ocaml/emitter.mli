(** The checks and reads of a plan, written as OCaml.

    One walk over a {!Planner.plan} writes every function that checks a record:
    the boolean ones, the decoders, the folds and the one that finds the codec's
    error. Internal to {!Wire_ocaml}. *)

open Planner

(** How the code holds integers: as [int], or on OxCaml as [int64_u]. *)
type num = Tagged | Unboxed

type src = {
  typ : string;
  u : string;
  len : string;
  checked : string;
  empty : string;
}
(** The buffer the generated code reads: its type, its unchecked accessors, its
    length, the module of its bounds-checked loads and string copies, and an
    expression for a fresh buffer of [size] zero bytes. *)

val rt : string
(** [rt] is the name of the runtime library the generated code links. *)

val errors : string
(** [errors] is the name of the runtime's module of parse errors. *)

val bytes_src : src
(** [bytes_src] reads [bytes]. *)

val bigstring_src : src
(** [bigstring_src] reads a bigarray of bytes. *)

(** Where a walk failed: a bound, a field's check, or a where clause. *)
type failure = Bounds | Check of field * check | Where

type mode = {
  num : num;
  src : src;
  ox : bool;
  cold : bool;
  raw32 : bool;
  live : Sset.t;
  exported : Sset.t;
  fail : failure -> string;
  overflowed : failure -> string -> string;
}
(** What a walk writes: how it holds integers, the buffer it reads, whether the
    target is OxCaml, whether it is the cold walk that raises the codec's
    errors, the variables it binds and hands out, and what it writes on a
    failure. *)

type pos = { var : string option; at : int }
(** A position in the record: a variable when the data decides it, plus a
    constant. *)

type w = {
  m : mode;
  ppf : Format.formatter;
  fresh : string -> string;
  ranges : (int * int) Smap.t;
  facts : facts;
  views : (string * int T.expr) list;
  words : (string * int) list;
  base : string;
  limit : string;
  known : int;
  tail : bool;
  copies : int;
}
(** The state of a walk: its mode and output, the ranges and facts known at its
    position, the bitfield words it holds, and the window of the buffer it is
    in. *)

val pf : w -> ('a, Format.formatter, unit) format -> 'a
(** [pf w fmt] writes to the output of [w]. *)

val lit : num -> int -> string
(** [lit num n] is the literal [n] in the integers of [num]. *)

val binop : num -> string -> string -> string -> string
(** [binop num op a b] is the text of [a op b] in the integers of [num]. *)

val endian_suffix : T.endian -> string
(** [endian_suffix e] is the suffix of the accessors of byte order [e]. *)

val bit_endian : T.bitfield_base -> T.endian
(** [bit_endian base] is the byte order of the bitfield word [base]. *)

val load : mode -> o:string -> k:int -> bytes:int -> endian:T.endian -> string
(** [load m ~o ~k ~bytes ~endian] is the text of an unchecked load of [bytes]
    bytes at [k] from [o]. *)

val covers : int -> field -> bool
(** [covers a f] is [true] when the 8-byte word at [a] holds the field [f]. *)

val words : size:int -> field list -> int list
(** [words ~size fields] is the offsets of the 8-byte words worth loading once
    to read [fields] from a prefix of [size] bytes. *)

val unsigned :
  mode ->
  ?size:int ->
  o:string ->
  words:int list ->
  k:int ->
  bytes:int ->
  endian:T.endian ->
  unit ->
  string
(** [unsigned m ?size ~o ~words ~k ~bytes ~endian ()] is the text of an unsigned
    value of [bytes] bytes at [k] from [o], from a word when one holds it;
    [size] bounds the bytes a wider load may read. *)

val value :
  mode ->
  ?size:int ->
  ?bitword:string ->
  o:string ->
  words:int list ->
  k:int ->
  field ->
  string
(** [value m ?size ?bitword ~o ~words ~k f] is the text of the plain value of
    the field [f] at [k] from [o], from the variable [bitword] of a bitfield's
    word when it has one. *)

val check : w -> failure -> bool T.expr -> unit
(** [check w failure cond] writes a check that fails with [failure] when [cond]
    is false; an overflow in [cond] fails too. *)

val bound : w -> pos -> int -> w
(** [bound w p need] is [w] after checking that the window has [need] bytes from
    [p]. *)

val field :
  w ->
  o:string ->
  k:int ->
  size:int ->
  words:int list ->
  bitwords:((int * T.bitfield_base) * string) list ->
  field ->
  w
(** [field w ~o ~k ~size ~words ~bitwords f] writes a field of a prefix at [k]
    from [o]: bound when live, then checked. *)

type tree = { tid : int; node : tnode }
(** The dispatch over a run of optional fields whose conditions exclude each
    other: a decision tree over the atoms of their conditions, whose leaves run
    one case or none. *)

and tnode = Leaf of int option | Branch of bool T.expr * tree * tree

val walk : w -> node list -> pos -> (w -> pos -> unit) -> unit
(** [walk w nodes p k] writes the checks of [nodes] from the position [p], then
    calls [k] with the state and position after them. *)

val start :
  m:mode ->
  ppf:Format.formatter ->
  ranges:(int * int) Smap.t ->
  base:string ->
  limit:string ->
  known:int ->
  node list ->
  (w -> pos -> unit) ->
  unit
(** [start ~m ~ppf ~ranges ~base ~limit ~known nodes k] is {!walk} over [nodes]
    from the start of a record at [base], in a window that ends at [limit] and
    is known to hold [known] bytes. *)

val live : ?cold:bool -> plan -> outputs:Sset.elt list -> Sset.t
(** [live ?cold plan ~outputs] is the variables a walk binds: those it reads,
    those it hands out and, for a cold walk, each checked field, whose value its
    error carries. *)

val gate_run :
  lower:(bool T.expr -> bool T.expr) ->
  facts ->
  node list ->
  (bool T.expr * output * output list * node list) list * node list
(** [gate_run ~lower facts nodes] is the optional fields at the head of [nodes]
    whose conditions, as [lower] writes them, exclude each other, and the nodes
    after them. *)

val decide : facts -> (int * bool T.expr) list -> tree
(** [decide facts cases] is the decision tree that picks the first of [cases]
    whose condition holds, or none. A subtree reached from two paths is one
    value. *)

val reach : tree -> (tree -> bool) * (int -> bool) * int
(** [reach tree] tells which subtrees of [tree] are reached more than once,
    which cases are, and how many leaves are written, a shared case once. *)
