(** A codec read as a plan, for the OCaml generator.

    A codec is read as a list of nodes, in the order the codec checks them: runs
    of fields at constant offsets, skipped spans, gates, sized regions and where
    clauses. This module builds the plan, bounds the integers it computes, and
    simplifies a condition under what is already known. Internal to
    {!Wire_ocaml}. *)

module T = Wire.Private.Types
module Smap : Map.S with type key = string
module Sset : Set.S with type elt = string

exception Unsupported of string
(** Raised with the reason when a codec has a shape the generator does not
    handle. *)

val fail_unsupported : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [fail_unsupported fmt] raises {!Unsupported} with the formatted reason. *)

type target = [ `OCaml | `OxCaml ]
(** The compiler the generated code is for. *)

(** A wire type of any OCaml type. *)
type typ = Typ : 'a Wire.typ -> typ

type maps = { find : 'a. 'a T.typ -> string option }
(** The OCaml path of each map a caller names, found by the map value itself. *)

val maps_of : (string * typ) list -> maps
(** [maps_of named] finds, for a mapped type, the path [named] gives the map it
    holds. *)

val label : string -> string
(** [label name] is the OCaml label of the field [name]: lower-cased, made an
    identifier, and escaped when a keyword of OCaml or of OxCaml. *)

val pp_list : string list Fmt.t
(** [pp_list] prints strings separated by spaces. *)

val int_lit : int -> string
(** [int_lit n] is [n] as a literal the generated code can hold on a target
    whose ints have 31 bits: a wider one goes through [int64]. *)

val mask : int -> string
(** [mask width] is the text of the mask of the [width] low bits, [width] at
    most 32. *)

(** What a failed check raises, as the codec does: a violated constraint, a
    value no case of a closed enum has, or an index past the cases of a lookup.
*)
type kind = Constraint | Enum of int list | Tag

type check = { cond : bool T.expr; kind : kind }
(** A condition a field must meet, and what its failure raises. *)

type output = {
  id : string;
  name : string;
  is_bool : bool;
  wire_width : int;
  signed : bool;
}
(** A named scalar a scan hands out: its variable, its name, whether it is a
    boolean, its width on the wire and its sign. *)

type field = {
  id : string;
  name : string;
  label : string;
  offset : int;
  store : Wire.Private.Layout.store;
  checks : check list;
  enum : (string * int list) option;
  map : string option;
  output : output option;
}
(** A field of a prefix: its variable, its codec name and OCaml label, its
    offset in the prefix, how it is stored, its checks in the order the codec
    runs them, the closed enum the encoder checks it against, the map it goes
    through and, for a named scalar, the output it is. *)

(** A step of the plan: a run of fields at constant offsets and its size, a span
    to skip, a gate over the nodes of an optional field, a sized region, or a
    where clause. *)
type node =
  | Prefix of field list * int
  | Span of int T.expr
  | Gate of bool T.expr * output * output list * node list
  | Region of int T.expr * bool * node list
  | Test of bool T.expr

type plan = {
  record : string;
  nodes : node list;
  outputs : output list;
  ranges : (int * int) Smap.t;
  wide : bool;
}
(** The plan of a codec: its record name, its nodes, the scalars it hands out,
    the range of each integer variable, and whether a value needs an [int] of 63
    bits. *)

val corners : ('a -> 'b -> int) -> 'a * 'a -> 'b * 'b -> (int * int) option
(** [corners op a b] is the least and the greatest of [op] over the ends of the
    ranges [a] and [b], or [None] when one of them overflows. *)

val bits : int -> int
(** [bits n] is the number of bits the non-negative [n] needs. *)

val range : (int * int) Smap.t -> int T.expr -> int * int
(** [range ranges e] is a range that holds every value [e] can take when its
    variables stay in [ranges]. *)

val is_leaf : 'a T.typ -> bool
(** [is_leaf typ] is [true] when [typ] is read at a constant offset, in a
    prefix. *)

type st = {
  maps : maps;
  mutable count : int;
  mutable outs : output list;
  mutable ranges : (int * int) Smap.t;
}
(** The state of a plan under construction: the maps, a counter for fresh
    variables, the outputs so far and the ranges of the variables bound. *)

val fresh : st -> string -> string -> string
(** [fresh st prefix name] is a variable for [name] no other call returns. *)

val output :
  st ->
  string list option ->
  id:string ->
  is_bool:bool ->
  Wire.Private.Layout.store option ->
  output
(** A named scalar a scan hands out: its variable, its name, whether it is a
    boolean, its width on the wire and its sign. *)

val place :
  st ->
  path:string list option ->
  env:(string * String.t) list ->
  word:Wire.Private.Layout.word option ->
  off:int ->
  name:string option ->
  own:bool T.expr list ->
  'a T.typ ->
  field * Wire.Private.Layout.word option * int * (string * String.t) list
(** [place st ~path ~env ~word ~off ~name ~own typ] is one field of a prefix at
    [off], with [own] its constraints: the field, its value under its map when
    it has one, the bitfield word and offset after it, and the scope after it.
*)

val record : st -> string list option -> T.struct_ -> node list
(** [record st path s] is the nodes of the struct [s] reached by [path]. Raises
    {!Unsupported} on a shape the generator does not handle. *)

val plan_of : ?maps:maps -> T.struct_ -> plan
(** [plan_of ?maps s] is the plan of the struct [s]. Raises {!Unsupported} on a
    shape the generator does not handle. *)

val fields_of : node list -> field list
(** [fields_of nodes] is every prefix field of [nodes], nested ones included. *)

val references : node list -> string list
(** [references nodes] is the variables the checks, sizes and conditions of
    [nodes] read. *)

type facts = { truths : (string * bool) list; integers : (string * int) list }
(** What is known where a condition is tested: predicates with their truth, and
    variables with their value. *)

val no_facts : facts
(** [no_facts] knows nothing. *)

val key : 'a T.expr -> string
(** [key e] is the identity of the predicate [e]: its text, over the plan's
    variables. *)

val assume : facts -> bool T.expr -> bool -> facts
(** [assume facts e truth] is [facts] with [e] known to be [truth], and what
    follows from it. *)

val simplify : facts -> bool T.expr -> bool T.expr
(** [simplify facts e] is [e] with every part [facts] decides replaced by its
    truth. *)

val may_overflow : 'a T.expr -> bool
(** [may_overflow e] is [true] when evaluating [e] may overflow, which rejects
    the record. *)
