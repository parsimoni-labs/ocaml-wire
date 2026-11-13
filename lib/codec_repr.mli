(** Representation of a record codec.

    The validation scratch ([slots]) and the functions that fill it from the
    buffer, the evaluation context, how a field is located ([field_access]), the
    state of a record under construction ([record]) and of a sealed codec ([t]),
    and the compilation of constraints and actions onto the scratch. Internal to
    {!Codec}. *)

val int_of_typ_value : 'a Types.typ -> 'a -> int
(** [int_of_typ_value t v] is the integer view of [v]; see {!Eval.int_of_exn}.
*)

type slots = { ints : int array; int64s : int64 array }
(** Per-decode validation scratch: one int slot and one int64 slot per field,
    action-local variable and parameter. *)

type runtime = Types.eval_ctx
(** The evaluation context an operation threads through its readers. *)

val clear_slots : slots -> int -> unit
(** [clear_slots s n] zeroes the first [n] slots of [s]. *)

val domain_local_slots : int -> unit -> slots
(** [domain_local_slots n ()] is this domain's scratch of [n] slots. *)

val at_of : int -> int -> int
(** [at_of base field_off] is the absolute offset of a field, or [base] when
    [field_off] is the dynamic sentinel [-1]. *)

val raise_field_constraint : field_idx:int -> at:int -> slots -> 'a
(** [raise_field_constraint ~field_idx ~at arr] raises a field constraint error
    at [at] carrying the value in slot [field_idx]. *)

val build_populate :
  'a Types.typ ->
  int ->
  (runtime -> Input_end.t -> bytes -> int -> 'a) ->
  slots ->
  runtime ->
  Input_end.t ->
  bytes ->
  int ->
  unit
(** [build_populate t idx reader arr runtime input_end buf base] stores the
    value [reader] reads at [base] into slot [idx] of [arr], or runs [reader]
    for its checks when [t] has no int slot. *)

type bf_info = {
  word_reader : bytes -> int -> Optint.t;
  bf_shift : int;
  bf_mask : Optint.t;
}
(** A bitfield accessor: its base word reader and the field's shift and mask. *)

val no_runtime : Types.eval_ctx
(** The context with no parameter bindings. *)

val runtime : ?env:Types.param_env -> unit -> Types.eval_ctx
(** [runtime ?env ()] resolves parameters through [env], or is {!no_runtime}. *)

(** Where a field's bytes are, relative to the record's base. *)
type field_access =
  | Fixed of int
  | Bitfield of {
      base : Types.bitfield_base;
      byte_off : int;
      shift : int;
      width : int;
    }
  | Dynamic of (runtime -> Input_end.t -> bytes -> int -> int)
  | Variable of {
      off : int;
      size_fn : runtime -> Input_end.t -> bytes -> int -> int;
    }
  | Variable_dynamic of {
      off_fn : runtime -> Input_end.t -> bytes -> int -> int;
      size_fn : runtime -> Input_end.t -> bytes -> int -> int;
    }

val access_at : field_access -> runtime -> Input_end.t -> bytes -> int -> int
(** [access_at a runtime input_end buf base] is the absolute offset of the field
    [a] locates in the record at [base]. *)

val immediate_access_off : field_access -> int
(** [immediate_access_off a] is the static offset of [a]; [0] for a dynamic one.
*)

type ('a, 'r) field = {
  id : int;
  name : string;
  typ : 'a Types.typ;
  constraint_ : bool Types.expr option;
  action : Types.action option;
  doc : string option;
  get : 'r -> 'a;
}
(** A field bound to a record projection. *)

val struct_field : ('a, 'r) field -> Types.field
(** [struct_field f] projects [f] to a struct field, with the refinement of its
    type folded into the constraint. *)

val struct' : string -> Types.field list -> Types.struct_
(** [struct'] is {!Types.struct_}. *)

(** Typed field readers in field order. *)
type (_, _) readers =
  | Nil : ('f, 'f) readers
  | Snoc :
      ('full, 'a -> 'rest) readers
      * (runtime -> Input_end.t -> bytes -> int -> 'a)
      -> ('full, 'rest) readers

type bf_codec_state = {
  base : Types.bitfield_base;
  bit_order : Types.bit_order;
  base_off : int;
  bits_used : int;
  total_bits : int;
}
(** The base word currently being packed with bitfields. *)

(** Where the next field starts: a constant until a variable-size field, then
    computed from the buffer. *)
type next_off =
  | Static of int
  | Dynamic of (runtime -> Input_end.t -> bytes -> int -> int)

type field_reader =
  string option * (runtime -> Input_end.t -> bytes -> int -> int)
(** An int reader for a field declared earlier, under its name. A shadowed field
    has no name: it keeps its place, and no reference reaches it. *)

val compile_expr :
  ?sizeof_this:(runtime -> Input_end.t -> bytes -> int -> int) ->
  field_reader list ->
  int Types.expr ->
  runtime ->
  Input_end.t ->
  bytes ->
  int ->
  int
(** [compile_expr env e runtime input_end buf base] evaluates [e] for the record
    at [base], resolving field references through [env]. *)

val compile_bool_expr :
  ?sizeof_this:(runtime -> Input_end.t -> bytes -> int -> int) ->
  field_reader list ->
  bool Types.expr ->
  runtime ->
  Input_end.t ->
  bytes ->
  int ->
  bool
(** [compile_bool_expr] is {!compile_expr} for a condition. *)

type idx = string -> int
(** Slot index of a field or action-local name. *)

type compile_ctx = {
  idx : idx;
  sizeof_this : int ref;
  field_pos : int;
  param_slots : (string, int) Hashtbl.t;
}
(** What compiling an expression onto the scratch resolves names against. *)

val mk_ctx :
  ?sizeof_this:int ref ->
  ?field_pos:int ->
  param_slots:(string, int) Hashtbl.t ->
  idx ->
  compile_ctx
(** [mk_ctx ~param_slots idx] is a compile context. *)

val compile_bool_arr : compile_ctx -> bool Types.expr -> slots -> bool
(** [compile_bool_arr cc e arr] evaluates [e] over the populated scratch [arr].
*)

val eval_arr_at : at:int -> ('a -> 'b) -> 'a -> 'b
(** [eval_arr_at ~at f arr] is [f arr] with a parse error relocated to [at]. *)

type compiled_action = slots -> unit
(** A field action compiled onto the scratch. *)

val compile_action :
  compile_ctx -> Types.action option -> compiled_action option
(** [compile_action cc a] compiles [a]; a failed [return] or an [abort] raises
    an action constraint error. *)

(** A record under construction: the fields added so far. *)
type ('f, 'r) record =
  | Record : {
      name : string;
      make : 'full;
      readers : ('full, 'f) readers;
      writers_rev : ('r -> runtime -> bytes -> int -> int -> int) list;
      size_of_value_rev : (Types.eval_ctx -> 'r -> int) list;
      min_wire_size : int;
      next_off : next_off;
      fixed_prefix : int ref;
      fields_rev : Types.field list;
      validators_rev :
        (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
      checkers_rev :
        (int * (slots -> runtime -> Input_end.t -> bytes -> int -> unit)) list;
      field_actions_rev : (string * compiled_action) list;
      n_fields : int;
      n_array_slots : int;
      r_bf : bf_codec_state option;
      field_access_rev : (string * (int * field_access)) list;
      where : bool Types.expr option;
      doc : string option;
      field_readers : field_reader list;
      param_slots : (string, int) Hashtbl.t;
    }
      -> ('f, 'r) record

(** The wire size of a record: constant, or computed from the buffer. *)
type wire_size_info =
  | Fixed of int
  | Variable of {
      min_size : int;
      compute : runtime -> Input_end.t -> bytes -> int -> int;
    }

val id_counter : int Atomic.t
(** Source of codec identities. *)

type 'r t = {
  id : int;
  name : string;
  size_of_value : Types.eval_ctx -> 'r -> int;
  field_access : (string * (int * field_access)) list;
  field_readers : field_reader list;
  field_actions : (string * compiled_action) list;
  decode : runtime -> Input_end.t -> bytes -> int -> 'r;
  encode : 'r -> runtime -> bytes -> int -> int;
  wire_size : wire_size_info;
  struct_fields : Types.field list;
  validate : ?env_slots:int array -> bytes -> int -> unit;
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
  validate_arr : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  n_array_slots : int;
  decode_scratch : unit -> slots;
  param_base : int;
  n_params : int;
  param_handles : Param.packed list;
  where : bool Types.expr option;
  doc : string option;
}
(** A sealed codec. *)

val pp : Format.formatter -> 'r t -> unit
(** [pp ppf c] prints the name of [c]. *)

val record_start :
  ?where:bool Types.expr -> ?doc:string -> string -> 'a -> ('a, 'b) record
(** [record_start name make] is the empty record for constructor [make]. *)

val ( $ ) : 'a Field.t -> ('b -> 'a) -> ('a, 'b) field
(** [f $ proj] binds [f] to the record projection [proj]. *)
