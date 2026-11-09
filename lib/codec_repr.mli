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
  bf_mask_int : int;
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
      ('full, 'a -> 'rest) readers * 'a field_read
      -> ('full, 'rest) readers

and _ field_read =
  | Data : 'a scalar -> 'a field_read
  | Mapped : 'w scalar * ('w -> 'a) -> 'a field_read
  | Span : int * int -> string field_read
  | Fn : (runtime -> Input_end.t -> bytes -> int -> 'a) -> 'a field_read

and _ scalar =
  | Byte : int -> UInt8.t scalar
  | U16le : int -> UInt16.t scalar
  | U16be : int -> UInt16.t scalar
  | U32le : int -> UInt32.t scalar
  | U32be : int -> UInt32.t scalar
  | Sbyte : int -> SInt8.t scalar
  | I16le : int -> SInt16.t scalar
  | I16be : int -> SInt16.t scalar
  | I32le : int -> SInt32.t scalar
  | I32be : int -> SInt32.t scalar
  | Bits8 : { off : int; shift : int; mask : int } -> int scalar
  | Bits16le : { off : int; shift : int; mask : int } -> int scalar
  | Bits16be : { off : int; shift : int; mask : int } -> int scalar

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

(** A comparison operator. *)
type cmp = Eq | Ne | Lt | Le | Gt | Ge

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

(** One step of the check pass: a scalar read into its slot, a condition over
    the slots, a range test of a scalar, or a general validator. *)
type step =
  | Slot : { slot : int; scalar : 'a scalar } -> step
  | Check : {
      name : string;
      slot : int;
      voff : int;
      cond : slots -> bool;
    }
      -> step
  | Range : {
      name : string;
      slot : int;
      scalar : 'a scalar;
      voff : int;
      set : (int * int) array;
      fail : runtime -> Input_end.t -> bytes -> int -> unit;
      store : bool;
    }
      -> step
  | Full of (slots -> runtime -> Input_end.t -> bytes -> int -> unit)
  | Covered of (slots -> runtime -> Input_end.t -> bytes -> int -> unit)

(** How the encoder writes a field it has a descriptor for: the field's value,
    put at the descriptor's offset with no bounds check of its own. *)
type 'r put =
  | Put : { get : 'r -> 'a; scalar : 'a scalar } -> 'r put
  | Blit : { get : 'r -> string; at : int; len : int } -> 'r put

(** How a field adds to the record's extent, for the layout pass: a fixed width,
    a byte span whose size the data decides, or the rest of the buffer. *)
type layout = Lfix of int | Lspan of int Types.expr | Lrest

(** A scalar of any type. *)
type packed_scalar = Ps : 'a scalar -> packed_scalar

(** A record under construction: the fields added so far. *)
type ('f, 'r) record =
  | Record : {
      name : string;
      make : 'full;
      readers : ('full, 'f) readers;
      checked_readers : ('full, 'f) readers;
      writers_rev : ('r -> runtime -> bytes -> int -> int -> int) list;
      puts_rev : 'r put list option;
      layout_rev : layout list option;
      prefix : (string * packed_scalar) list;
      size_of_value_rev : (Types.eval_ctx -> 'r -> int) list;
      min_wire_size : int;
      next_off : next_off;
      fixed_prefix : int ref;
      used_slots : (int, unit) Hashtbl.t;
      validators_rev : (int * step) list;
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

(** A fused interval test, for a pass made of nothing else. *)
type interval =
  | Interval : {
      scalar : 'a scalar;
      set : (int * int) array;
      fail : runtime -> Input_end.t -> bytes -> int -> unit;
      name : string;
      voff : int;
    }
      -> interval

type pass =
  | No_checks
  | Intervals of {
      flat : int array;
      tables : string array;
      intervals : interval array;
    }
  | Steps of {
      steps : step array;
      where : (slots -> bool) option;
      scratch : (unit -> slots) option;
      clear : slots -> unit;
    }

type 'r compiled = {
  size_of_value : Types.eval_ctx -> 'r -> int;
  field_access : (string * (int * field_access)) list;
  field_readers : field_reader list;
  field_actions : (string * compiled_action) list;
  decode : runtime -> Input_end.t -> bytes -> int -> 'r;
  plain_decode : (bytes -> int -> 'r) option;
  encode : 'r -> runtime -> bytes -> int -> int;
  wire_size : wire_size_info;
  validate : ?env_slots:int array -> bytes -> int -> unit;
  plain_validate : bytes -> int -> unit;
  plain_encode : ('r -> bytes -> int -> unit) option;
  fixed_frame : int;
  plain_pass : pass;
  validate_ctx : runtime -> Input_end.t -> bytes -> int -> unit;
  validate_arr : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  clear_scratch : slots -> unit;
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
  n_array_slots : int;
  has_checks : bool;
  decode_scratch : unit -> slots;
  param_base : int;
}
(** What {!Codec_seal.seal} builds from a field list; built on first use. *)

type 'r t = {
  id : int;
  name : string;
  struct_fields : Types.field list;
  n_params : int;
  param_handles : Param.packed list;
  where : bool Types.expr option;
  doc : string option;
  compiled : 'r compiled option Atomic.t;
  compile : string -> 'r compiled;
}
(** A sealed codec. *)

val pp : Format.formatter -> 'r t -> unit
(** [pp ppf c] prints the name of [c]. *)

val record_start :
  ?where:bool Types.expr -> ?doc:string -> string -> 'a -> ('a, 'b) record
(** [record_start name make] is the empty record for constructor [make]. *)

val ( $ ) : 'a Field.t -> ('b -> 'a) -> ('a, 'b) field
(** [f $ proj] binds [f] to the record projection [proj]. *)

val field_read :
  min_wire_size:int ->
  field_access ->
  'a Types.typ ->
  (runtime -> Input_end.t -> bytes -> int -> 'a) ->
  plain:bool ->
  'a field_read
(** [field_read ~min_wire_size access typ reader ~plain] is how the decoder
    reads a field: as data when it is a bare scalar at a fixed offset or packed
    in an 8- or 16-bit word, through [reader] otherwise. *)

val read_field : 'a field_read -> runtime -> Input_end.t -> bytes -> int -> 'a
(** [read_field r runtime input_end buf base] is the value [r] reads from the
    record at [base]. A {!constructor-Data} read skips the bounds check: the
    caller has checked the record's span. *)

val flip : cmp -> cmp
(** [flip c] is the comparison that holds of [(y, x)] when [c] holds of
    [(x, y)]. *)

val slot_scalar : 'a scalar -> bool
(** [slot_scalar s] is [true] when an [int] holds every value of [s] exactly, so
    its validation slot can stand for it. *)

val read_slot : 'a scalar -> bytes -> int -> int
(** [read_slot s buf base] is the integer the scalar [s] holds in the record at
    [base], read with no bounds check. *)

val int_scalar : 'a scalar -> bool
(** [int_scalar s] is [true] when an [int] holds every value of [s] exactly, so
    a write converts it with no loss. *)

val write_scalar : 'a scalar -> bytes -> int -> 'a -> unit
(** [write_scalar s buf base v] writes [v] as the scalar [s] of the record at
    [base], with no bounds check. A bitfield is or-ed into its word, which the
    encoder has zeroed first. *)

val field_scalar :
  min_wire_size:int -> field_access -> 'a Types.typ -> 'a scalar option
(** [field_scalar ~min_wire_size access typ] is the scalar a field of type [typ]
    at [access] is, when it is a bare scalar at a fixed offset inside the first
    [min_wire_size] bytes or packed in an 8- or 16-bit word. *)

val alloc_slots : int -> slots
(** [alloc_slots n] is a fresh scratch of [n] zeroed slots. *)
