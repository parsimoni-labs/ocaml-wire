(** The checks and reads of a plan, written as OCaml.

    One walk over a {!Planner.plan} writes every function that checks a record:
    the boolean ones, the decoders, the folds and the one that finds the codec's
    error. Internal to {!Wire_ocaml}. *)

open Planner

(** How the code holds integers. [Tagged] is [int]. [Boxed] is [int64] for
    values and [int] for positions; the stock compiler keeps such an [int64]
    unboxed in a register. [Unboxed] is OxCaml's [int64_u]. *)
type num = Tagged | Boxed | Unboxed

val pos_num : num -> num
(** [pos_num num] is how code that holds integers as [num] holds positions,
    sizes and bounds: as [int] where it holds values as [int64]. *)

type src = {
  typ : string;
  u : string;
  len : string;
  checked : string;
  brand : string;
  buf : string;
  unboxed : bool;
}
(** The buffer the generated code reads: its type, its unchecked accessors, its
    length, the module of its bounds-checked loads and string copies, the
    runtime module that names a buffer of that type, the parameter that binds
    it, and whether its 32-bit and 64-bit loads give unboxed values, as on
    OxCaml. *)

val i32 : src -> string
(** [i32 src] is the module of the 32-bit values [src] loads: [Int32], or
    [Int32_u] when they are unboxed. *)

val i64 : src -> string
(** [i64 src] is {!i32} for 64-bit values. *)

val rt : string
(** [rt] is the name of the runtime library the generated code links. *)

val errors : string
(** [errors] is the name of the runtime's module of parse errors. *)

val bytes_src : src
(** [bytes_src] reads [bytes]. *)

val bigstring_src : src
(** [bigstring_src] reads a bigarray of bytes. *)

val local_bytes_src : src
(** [local_bytes_src] reads [bytes] the caller may hold [local], on OxCaml. *)

val local_bigstring_src : src
(** [local_bigstring_src] reads a bigarray of bytes the caller may hold [local],
    on OxCaml. *)

(** Where a walk failed: a bound, a field's check, or a where clause. *)
type failure = Bounds | Check of field * check | Where

type pos = Decision.pos = { var : string option; at : int }
(** A position in the record: a variable when the data decides it, plus a
    constant. *)

val values : num -> Decision.values
(** [values num] is how a walk of [num] holds integer values. *)

type mode = {
  num : num;
  src : src;
  ox : bool;
  cold : bool;
  raw32 : bool;
  ranged : bool;
  live : Sset.t;
  exported : Sset.t;
  slots : Sset.t;
  ahead : Sset.t;
  fail : failure -> string;
  overflowed : failure -> string -> string;
  decide : Decision.t -> unit;
}
(** What a walk writes: how it holds integers, the buffer it reads, whether the
    target is OxCaml, whether it is the cold walk that raises the codec's
    errors, whether it may test two bitfields of a word as one range of the word
    (a scan, which names no failed field), the variables it binds, the outputs
    it hands out, those of them that are slots, the variables its caller binds
    before it by {!read_ahead}, what it writes on a failure, and what it does
    with each decision it takes. *)

type cuts = {
  prefix : string;
  defs : Buffer.t;
  taken : (string, unit) Hashtbl.t;
  poly : string;
  reserve : int;
}
(** Where the functions a walk is cut into go: their names start with [prefix],
    each distinct from those in [taken]; [defs] holds their definitions, each
    before the functions that call it; [poly] is written before the parameters
    of each, as [(type a : bits64) ]; and each keeps room for [reserve]
    characters, the call that ends the walk. *)

type fn = { mutable chars : int; mutable decisions : int }
(** The text written so far in the function being written, on all its paths: its
    length and the branches it opens. *)

type w = {
  m : mode;
  ppf : Format.formatter;
  fresh : string -> string;
  ranges : (int * int) Smap.t;
  facts : facts;
  views : (string * int T.expr) list;
  words : (string * int) list;
  raws : (string * (string * int)) list;
  deferred : (string * (string * bool)) list ref;
  bounded : (string * string * string * string) list;
  sums : (string * sum) list;
  ints : Sset.t;
  base : string;
  disp : int;
  limit : string;
  known : int;
  tail : bool;
  copies : int;
  cuts : cuts option;
  fn : fn;
  depth : int;
  scope : (string * string option) list;
  settled : (string * bool) list;
  pinned : bool;
  section : string;
  held : int list;
}
(** The state of a walk: its mode and output, the ranges and facts known at its
    position, the bitfield words it holds, the spans whose bound is checked and
    the end of each as a sum, the variables that hold an [int] in the [int64]
    form, the window of the buffer it is in, as a variable and a constant
    distance from it; and, for a walk cut into functions, where they go, the
    function being written and the conditions open around the position in it,
    the variables it binds with the type a function cut out of it gives each,
    the outputs known present or absent, whether the code here jumps to a join
    outside it, and the case being written, which names its functions; and, as
    [int64], the 64-bit words loaded at constant offsets of the window, by
    offset. *)

val pf : w -> ('a, Format.formatter, unit) format -> 'a
(** [pf w fmt] writes to the output of [w]. *)

val pp_slots : mode -> output list Fmt.t
(** [pp_slots m] binds each of the outputs that is a slot of [m] to its absent
    value: a mutable local the walk assigns where its field is present. *)

val output_value : mode -> output -> string
(** [output_value m o] is the text of the value of [o] where a walk ends. *)

val output_text : w -> output -> string
(** [output_text w o] is the text of the value of [o] at the position of [w]:
    its literal where it is known absent or present. *)

val prune : string list -> string list
(** [prune lines] is [lines] without each binding [let x = e in] that no later
    line of its function reads. *)

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

val words : num -> size:int -> field list -> int list
(** [words num ~size fields] is the offsets of the 8-byte words worth loading
    once to read [fields] from a prefix of [size] bytes, with integers held as
    [num]. *)

val unsigned :
  mode ->
  ?size:int ->
  ?lo:int ->
  ?raw:bool ->
  o:string ->
  words:int list ->
  k:int ->
  bytes:int ->
  endian:T.endian ->
  unit ->
  string
(** [unsigned m ?size ?lo ?raw ~o ~words ~k ~bytes ~endian ()] is the text of an
    unsigned value of [bytes] bytes at [k] from [o], from a word when one holds
    it; [size] bounds the bytes a wider load may read and [lo] (default [0]) is
    the first byte a load may read. With [raw] (default [false]), the value may
    hold any bits above its [bytes], for a caller that masks it below them. *)

val value :
  mode ->
  ?size:int ->
  ?lo:int ->
  ?bitword:string ->
  o:string ->
  words:int list ->
  k:int ->
  field ->
  string
(** [value m ?size ?lo ?bitword ~o ~words ~k f] is the text of the plain value
    of the field [f] at [k] from [o], read between [lo] and [size] as
    {!unsigned} does, from the variable [bitword] of a bitfield's word when it
    has one. *)

val check : w -> failure -> bool T.expr -> w
(** [check w failure cond] writes a check that fails with [failure] when [cond]
    is false, and is [w] one condition deeper; an overflow in [cond] fails too.
*)

val bound : w -> pos -> int -> w
(** [bound w p need] is [w] after checking that the window has [need] bytes from
    [p]. *)

val read_ahead :
  mode -> size:int -> k:int -> field -> (string * (string -> string)) option
(** [read_ahead m ~size ~k f] is the variable {!field} binds for the field [f]
    at [k] from the start of a frame whose first [size] bytes are known, and its
    value at a start [o], when a caller may bind it before the walk: a byte, or
    an unsigned big-endian field of 16 or 32 bits held unboxed. A walk whose
    {!field-ahead} holds the variable reads it instead of loading the field. *)

val field :
  ?checked:bool ->
  w ->
  o:string ->
  k:int ->
  size:int ->
  words:int list ->
  bitwords:((int * T.bitfield_base) * string) list ->
  field ->
  w
(** [field ?checked w ~o ~k ~size ~words ~bitwords f] writes a field of a prefix
    at [k] from [o]: bound when live, then checked. [checked] defaults to
    [true]; with [false] the caller writes the checks. *)

val walk : w -> node list -> pos -> (w -> pos -> unit) -> unit
(** [walk w nodes p k] writes the checks of [nodes] from the position [p], then
    calls [k] with the state and position after them. *)

val start :
  ?cuts:cuts ->
  ?scope:(string * string option) list ->
  ?first:string ->
  ?name:string ->
  m:mode ->
  ppf:Format.formatter ->
  ranges:(int * int) Smap.t ->
  base:string ->
  limit:string ->
  known:int ->
  node list ->
  (w -> pos -> unit) ->
  unit
(** [start ?cuts ?scope ?first ?name ~m ~ppf ~ranges ~base ~limit ~known nodes
     k] is {!walk} over [nodes] from the start of a record at [base], in a
    window that ends at [limit] and is known to hold [known] bytes. With [cuts]
    the walk is written as small functions, which take the variables of [scope]
    they read; with [first], the whole walk is one of them, named [first]. With
    [name], the function the walk is written in, it starts with a
    {!Decision.constructor-Walk} decision. *)

val live : ?cold:bool -> plan -> outputs:Sset.elt list -> Sset.t
(** [live ?cold plan ~outputs] is the variables a walk binds: those it reads,
    those it hands out and, for a cold walk, each checked field, whose value its
    error carries. *)
