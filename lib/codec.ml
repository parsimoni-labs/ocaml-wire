open Types
module Slice = Bytesrw.Bytes.Slice
module Param = Param

(** Build a specialized field encoder: writes field value to bytes at offset.
    Returns the new offset. Works directly on the slice's underlying bytes. *)
let blit_string_padded n buf off v =
  let len = min n (String.length v) in
  Bytes.blit_string v 0 buf off len;
  if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
  off + n

(* Pack a fixed-width integer setter into a [bytes -> int -> int -> int]
   field encoder that returns the offset advance. Used by the scalar cases
   in [build_field_encoder] / [build_field_reader]. *)
let setter_off n set buf off v =
  set buf off v;
  off + n

let setter_off_int32 n set buf off v =
  set buf off (Int32.of_int v);
  off + n

let rec build_field_encoder : type a. a typ -> bytes -> int -> a -> int =
 fun typ ->
  match typ with
  | Uint8 ->
      fun buf off v ->
        Bytes.set_uint8 buf off v;
        off + 1
  | Uint16 Little ->
      fun buf off v ->
        Bytes.set_uint16_le buf off v;
        off + 2
  | Uint16 Big ->
      fun buf off v ->
        Bytes.set_uint16_be buf off v;
        off + 2
  | Uint32 Little ->
      fun buf off v ->
        UInt32.set_le buf off v;
        off + 4
  | Uint32 Big ->
      fun buf off v ->
        UInt32.set_be buf off v;
        off + 4
  | Uint63 Little ->
      fun buf off v ->
        UInt63.set_le buf off v;
        off + 8
  | Uint63 Big ->
      fun buf off v ->
        UInt63.set_be buf off v;
        off + 8
  | Uint64 Little ->
      fun buf off v ->
        Bytes.set_int64_le buf off v;
        off + 8
  | Uint64 Big ->
      fun buf off v ->
        Bytes.set_int64_be buf off v;
        off + 8
  | Int8 -> setter_off 1 Bytes.set_int8
  | Int16 Little -> setter_off 2 Bytes.set_int16_le
  | Int16 Big -> setter_off 2 Bytes.set_int16_be
  | Int32 Little -> setter_off_int32 4 Bytes.set_int32_le
  | Int32 Big -> setter_off_int32 4 Bytes.set_int32_be
  | Int64 Little -> setter_off 8 Bytes.set_int64_le
  | Int64 Big -> setter_off 8 Bytes.set_int64_be
  | Float32 Little ->
      fun buf off v ->
        Bytes.set_int32_le buf off (Int32.bits_of_float v);
        off + 4
  | Float32 Big ->
      fun buf off v ->
        Bytes.set_int32_be buf off (Int32.bits_of_float v);
        off + 4
  | Float64 Little ->
      fun buf off v ->
        Bytes.set_int64_le buf off (Int64.bits_of_float v);
        off + 8
  | Float64 Big ->
      fun buf off v ->
        Bytes.set_int64_be buf off (Int64.bits_of_float v);
        off + 8
  | Uint_var { size = Int n; endian } ->
      fun buf off v ->
        Uint_var.write endian buf off n v;
        off + n
  | Byte_array { size = Int n } -> blit_string_padded n
  | Byte_array_where { size = Int n; _ } -> blit_string_padded n
  | Byte_slice { size = Int n } ->
      fun buf off v ->
        let len = min n (Slice.length v) in
        Bytes.blit (Slice.bytes v) (Slice.first v) buf off len;
        if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
        off + n
  | Where { inner; _ } -> build_field_encoder inner
  | Enum { base; _ } -> build_field_encoder base
  | Map { inner; encode; _ } ->
      let enc = build_field_encoder inner in
      fun buf off v -> enc buf off (encode v)
  | Unit -> fun _buf off () -> off
  | Casetype { tag; cases; _ } ->
      let tag_enc = build_field_encoder tag in
      fun buf off v ->
        let rec find = function
          | [] -> failwith "build_field_encoder: casetype: no matching case"
          | Case_branch { cb_tag; cb_inner; cb_project; _ } :: rest -> (
              match cb_project v with
              | Some body ->
                  let off' =
                    match cb_tag with
                    | Some t -> tag_enc buf off t
                    | None ->
                        failwith
                          "build_field_encoder: cannot encode default case"
                  in
                  build_field_encoder cb_inner buf off' body
              | None -> find rest)
        in
        find cases
  | _ ->
      (* Fallback for complex types - not specialized *)
      fun _buf _off _v -> failwith "build_field_encoder: unsupported type"

(** Build a direct field reader that reads at a fixed offset. No tuples, no refs
    \- just pure value read. Caller must ensure the buffer is large enough. *)
let rec build_field_reader : type a. a typ -> int -> bytes -> int -> a =
 fun typ field_off ->
  match typ with
  | Uint8 -> fun buf base -> Bytes.get_uint8 buf (base + field_off)
  | Uint16 Little -> fun buf base -> Bytes.get_uint16_le buf (base + field_off)
  | Uint16 Big -> fun buf base -> Bytes.get_uint16_be buf (base + field_off)
  | Uint32 Little -> fun buf base -> UInt32.le buf (base + field_off)
  | Uint32 Big -> fun buf base -> UInt32.be buf (base + field_off)
  | Uint63 Little -> fun buf base -> UInt63.le buf (base + field_off)
  | Uint63 Big -> fun buf base -> UInt63.be buf (base + field_off)
  | Uint64 Little -> fun buf base -> Bytes.get_int64_le buf (base + field_off)
  | Uint64 Big -> fun buf base -> Bytes.get_int64_be buf (base + field_off)
  | Int8 -> fun buf base -> Bytes.get_int8 buf (base + field_off)
  | Int16 Little -> fun buf base -> Bytes.get_int16_le buf (base + field_off)
  | Int16 Big -> fun buf base -> Bytes.get_int16_be buf (base + field_off)
  | Int32 Little ->
      fun buf base -> Int32.to_int (Bytes.get_int32_le buf (base + field_off))
  | Int32 Big ->
      fun buf base -> Int32.to_int (Bytes.get_int32_be buf (base + field_off))
  | Int64 Little -> fun buf base -> Bytes.get_int64_le buf (base + field_off)
  | Int64 Big -> fun buf base -> Bytes.get_int64_be buf (base + field_off)
  | Float32 Little ->
      fun buf base ->
        Int32.float_of_bits (Bytes.get_int32_le buf (base + field_off))
  | Float32 Big ->
      fun buf base ->
        Int32.float_of_bits (Bytes.get_int32_be buf (base + field_off))
  | Float64 Little ->
      fun buf base ->
        Int64.float_of_bits (Bytes.get_int64_le buf (base + field_off))
  | Float64 Big ->
      fun buf base ->
        Int64.float_of_bits (Bytes.get_int64_be buf (base + field_off))
  | Uint_var { size = Int n; endian } ->
      fun buf base -> Uint_var.read endian buf (base + field_off) n
  | Byte_array { size = Int n } ->
      fun buf base -> Bytes.sub_string buf (base + field_off) n
  | Byte_array_where { size = Int n; _ } ->
      fun buf base -> Bytes.sub_string buf (base + field_off) n
  | Byte_slice { size = Int n } ->
      fun buf base -> Slice.make buf ~first:(base + field_off) ~length:n
  | Where { inner; _ } -> build_field_reader inner field_off
  | Enum { base; _ } -> build_field_reader base field_off
  | Map { inner; decode; _ } ->
      let read = build_field_reader inner field_off in
      fun buf base -> decode (read buf base)
  | Unit -> fun _buf _base -> ()
  | _ -> fun _buf _base -> failwith "build_field_reader: unsupported type"

let int_of_typ_value = Eval.int_of

(* Build a populate function that writes a field value into the validator
   array without allocating. Resolves the type at seal time so the hot
   path is a direct arr.(idx) <- reader buf base. *)
let rec build_populate : type a.
    a typ -> int -> (bytes -> int -> a) -> int array -> bytes -> int -> unit =
 fun typ idx reader ->
  match typ with
  | Uint8 -> fun arr buf base -> arr.(idx) <- reader buf base
  | Uint16 _ -> fun arr buf base -> arr.(idx) <- reader buf base
  | Uint_var _ -> fun arr buf base -> arr.(idx) <- reader buf base
  | Uint32 _ -> fun arr buf base -> arr.(idx) <- UInt32.to_int (reader buf base)
  | Uint63 _ -> fun arr buf base -> arr.(idx) <- UInt63.to_int (reader buf base)
  | Int8 -> fun arr buf base -> arr.(idx) <- reader buf base
  | Int16 _ -> fun arr buf base -> arr.(idx) <- reader buf base
  | Int32 _ -> fun arr buf base -> arr.(idx) <- reader buf base
  | Bits _ -> fun arr buf base -> arr.(idx) <- reader buf base
  | Uint64 _ -> (
      fun arr buf base ->
        match Int64.unsigned_to_int (reader buf base) with
        | Some v -> arr.(idx) <- v
        | None -> ())
  | Int64 _ -> fun arr buf base -> arr.(idx) <- Int64.to_int (reader buf base)
  (* Floats store their IEEE 754 bit pattern in [int_array] so [Ref name]
     in a constraint sees the same value the 3D-side sees. The user-facing
     reader still returns the [float], reinterpreted via [Int*.float_of_bits]
     elsewhere. *)
  | Float32 Little ->
      fun arr buf base ->
        arr.(idx) <- Bytes.get_int32_le buf base |> Int32.to_int
  | Float32 Big ->
      fun arr buf base ->
        arr.(idx) <- Bytes.get_int32_be buf base |> Int32.to_int
  | Float64 Little ->
      fun arr buf base ->
        arr.(idx) <- Bytes.get_int64_le buf base |> Int64.to_int
  | Float64 Big ->
      fun arr buf base ->
        arr.(idx) <- Bytes.get_int64_be buf base |> Int64.to_int
  | Where { inner; _ } -> build_populate inner idx reader
  | Enum { base; _ } -> build_populate base idx reader
  | Map { inner; encode; _ } ->
      build_populate inner idx (fun buf base -> encode (reader buf base))
  | Optional_or { inner; _ } ->
      (* [Optional_or] returns the inner type directly (no [option] wrapper),
         so the reader is already of the inner's value type. *)
      build_populate inner idx reader
  | Optional { inner; _ } -> (
      (* The optional decodes as ['inner option]; populate from the inner
         value when present, leave the slot at 0 when absent. *)
      let inner_populate =
        build_populate inner idx (fun buf base ->
            match reader buf base with Some v -> v | None -> assert false)
      in
      fun arr buf base ->
        match reader buf base with
        | Some _ -> inner_populate arr buf base
        | None -> ())
  | _ ->
      (* Aggregate / string-valued types have no scalar int projection.
         [Field.ref] over them reads 0; treat as deliberate (no slot
         write) rather than an error since [ref] currently accepts any
         field type. *)
      fun _arr _buf _base -> ()

(* Bitfield extraction descriptor: word reader + packed shift/mask.
   Packing shift and mask into a single int lets [extract] be a direct
   [@inline always] function instead of an indirect closure call. *)
type bf_info = {
  bf_word_reader : bytes -> int -> int;
  bf_packed : int; (* shift in bits 0-7, mask in bits 8+ *)
}

type field_access =
  | Fixed of int
  | Bitfield of {
      base : bitfield_base;
      byte_off : int;
      shift : int;
      width : int;
    }
  | Dynamic of (bytes -> int -> int)
  | Variable of { off : int; size_fn : bytes -> int -> int }
  | Variable_dynamic of {
      off_fn : bytes -> int -> int;
      size_fn : bytes -> int -> int;
    }

type ('a, 'r) field = {
  name : string;
  typ : 'a typ;
  constraint_ : bool expr option;
  action : action option;
  get : 'r -> 'a;
}

let combine_constraint a b =
  match (a, b) with
  | None, x | x, None -> x
  | Some a, Some b -> Some Expr.(a && b)

(* Capture top-level names before field/struct_ are shadowed.
   Extract Where constraints so that Codec.to_struct produces correct 3D. *)
let struct_field : type a r. (a, r) field -> Types.field =
 fun fld ->
  match fld.typ with
  | Where { cond; inner } ->
      field fld.name
        ?constraint_:(combine_constraint fld.constraint_ (Some cond))
        ?action:fld.action inner
  | _ -> field fld.name ?constraint_:fld.constraint_ ?action:fld.action fld.typ

let struct' = struct_

(* GADT snoc-list of typed field readers, built in forward order by add_field.
   ('full, 'remaining) readers tracks:
   - 'full:      the original constructor type
   - 'remaining: what's left after consuming the readers in this list

   Snoc appends at the end, so readers are in field order.
   At seal time, pattern-matching reconstructs the full application
   without partial application closures (for up to 6 fields). *)
type (_, _) readers =
  | Nil : ('f, 'f) readers
  | Snoc :
      ('full, 'a -> 'rest) readers * (bytes -> int -> 'a)
      -> ('full, 'rest) readers

(* Bitfield group state: tracks the current base word being packed. *)
type bf_codec_state = {
  bfc_base : bitfield_base;
  bfc_bit_order : bit_order;
  bfc_base_off : int; (* byte offset of base word within record *)
  bfc_bits_used : int; (* bits consumed so far in current group *)
  bfc_total_bits : int; (* 8, 16, or 32 *)
}

(* Track the byte offset for the next field: static (constant) until we hit
   a variable-size field, then dynamic (computed from the buffer). *)
type next_off = Static of int | Dynamic of (bytes -> int -> int)
(* [compute buf base] returns the absolute byte offset where the next
         field starts. [base] is the record's base offset in [buf]. *)

type field_reader = string * (bytes -> int -> int)
(* Name + buffer-and-base reader for a previously-declared int field. *)

(* Single GADT walker for [a expr] used by both call surfaces (closure
   over bytes for variable-size sizing; int_array lookup for constraint
   evaluation). The two surfaces share every operator dispatch and only
   differ in how leaves ([Ref], [Param_ref], [Sizeof], [Sizeof_this],
   [Field_pos]) resolve. *)

type packed_param = Pack_param : ('a, 'k) param_handle -> packed_param

(* Leaves resolution strategy for a given access layer. *)
type 'ctx leaves = {
  ref_ : string -> 'ctx -> int;
  param_ref : packed_param -> 'ctx -> int;
  sizeof_typ : packed_typ -> 'ctx -> int;
  sizeof_this : 'ctx -> int;
  field_pos : 'ctx -> int;
}

let rec compile_int : type ctx. ctx leaves -> int expr -> ctx -> int =
 fun l e ->
  let rec_ = compile_int l in
  match e with
  | Int n -> fun _ -> n
  | Ref name -> l.ref_ name
  | Param_ref p -> l.param_ref (Pack_param p)
  | Sizeof t -> l.sizeof_typ (Pack_typ t)
  | Sizeof_this -> l.sizeof_this
  | Field_pos -> l.field_pos
  | Add (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c + fb c
  | Sub (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c - fb c
  | Mul (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c * fb c
  | Div (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c / fb c
  | Mod (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c mod fb c
  | Land (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c land fb c
  | Lor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c lor fb c
  | Lxor (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c lxor fb c
  | Lnot a ->
      let fa = rec_ a in
      fun c -> lnot (fa c)
  | Lsl (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c lsl fb c
  | Lsr (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> fa c lsr fb c
  | Cast (w, a) -> (
      let fa = rec_ a in
      match w with
      | `U8 -> fun c -> fa c land 0xFF
      | `U16 -> fun c -> fa c land 0xFFFF
      | `U32 -> fun c -> fa c land 0xFFFF_FFFF
      | `U64 -> fa)
  | If_then_else (c, t, e) ->
      let fc = compile_bool l c in
      let ft = rec_ t and fe = rec_ e in
      fun c' -> if fc c' then ft c' else fe c'

(* Typed-GADT projector: refines [a expr] to [int expr] / [bool expr]
   so [Eq] / [Ne] can dispatch to the right compiler at the right type. *)
and try_int : type a ctx. ctx leaves -> a expr -> (ctx -> int) option =
 fun l e ->
  let go e = Some (compile_int l e) in
  match e with
  | Int _ as e -> go e
  | Ref _ as e -> go e
  | Param_ref _ as e -> go e
  | Sizeof _ as e -> go e
  | Sizeof_this as e -> go e
  | Field_pos as e -> go e
  | Add _ as e -> go e
  | Sub _ as e -> go e
  | Mul _ as e -> go e
  | Div _ as e -> go e
  | Mod _ as e -> go e
  | Land _ as e -> go e
  | Lor _ as e -> go e
  | Lxor _ as e -> go e
  | Lnot _ as e -> go e
  | Lsl _ as e -> go e
  | Lsr _ as e -> go e
  | Cast _ as e -> go e
  | If_then_else _ as e -> go e
  | _ -> None

and try_bool : type a ctx. ctx leaves -> a expr -> (ctx -> bool) option =
 fun l e ->
  let go e = Some (compile_bool l e) in
  match e with
  | Bool _ as e -> go e
  | Eq _ as e -> go e
  | Ne _ as e -> go e
  | Lt _ as e -> go e
  | Le _ as e -> go e
  | Gt _ as e -> go e
  | Ge _ as e -> go e
  | And _ as e -> go e
  | Or _ as e -> go e
  | Not _ as e -> go e
  | _ -> None

and compile_bool : type ctx. ctx leaves -> bool expr -> ctx -> bool =
 fun l e ->
  let int_rec = compile_int l in
  let bool_rec = compile_bool l in
  match e with
  | Bool b -> fun _ -> b
  | Eq (a, b) -> (
      match (try_int l a, try_int l b, try_bool l a, try_bool l b) with
      | Some fa, Some fb, _, _ -> fun c -> fa c = fb c
      | _, _, Some fa, Some fb -> fun c -> fa c = fb c
      | _ -> assert false)
  | Ne (a, b) -> (
      match (try_int l a, try_int l b, try_bool l a, try_bool l b) with
      | Some fa, Some fb, _, _ -> fun c -> fa c <> fb c
      | _, _, Some fa, Some fb -> fun c -> fa c <> fb c
      | _ -> assert false)
  | Lt (a, b) ->
      let fa = int_rec a and fb = int_rec b in
      fun c -> fa c < fb c
  | Le (a, b) ->
      let fa = int_rec a and fb = int_rec b in
      fun c -> fa c <= fb c
  | Gt (a, b) ->
      let fa = int_rec a and fb = int_rec b in
      fun c -> fa c > fb c
  | Ge (a, b) ->
      let fa = int_rec a and fb = int_rec b in
      fun c -> fa c >= fb c
  | And (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c -> fa c && fb c
  | Or (a, b) ->
      let fa = bool_rec a and fb = bool_rec b in
      fun c -> fa c || fb c
  | Not e ->
      let fe = bool_rec e in
      fun c -> not (fe c)

(* Closure-based access layer: [ctx = bytes * int]. The pair is
   re-allocated per call site; profile if this becomes hot. *)
let bytes_leaves ?(sizeof_this : bytes -> int -> int = fun _ _ -> 0)
    (env : field_reader list) : (bytes * int) leaves =
  {
    ref_ =
      (fun name ->
        match List.assoc_opt name env with
        | Some reader -> fun (buf, base) -> reader buf base
        | None ->
            Fmt.invalid_arg "Codec: unbound field ref %S in size expression"
              name);
    param_ref = (fun (Pack_param p) _ -> !(p.ph_cell));
    sizeof_typ =
      (fun (Pack_typ t) ->
        match field_wire_size t with
        | Some n -> fun _ -> n
        | None -> invalid_arg "Codec: sizeof on variable-size type");
    sizeof_this = (fun (buf, base) -> sizeof_this buf base);
    field_pos =
      (fun _ -> invalid_arg "Codec: [field_pos] only valid inside an action");
  }

let compile_expr ?sizeof_this env e =
  let l = bytes_leaves ?sizeof_this env in
  let f = compile_int l e in
  fun buf base -> f (buf, base)

let compile_bool_expr ?sizeof_this env e =
  let l = bytes_leaves ?sizeof_this env in
  let f = compile_bool l e in
  fun buf base -> f (buf, base)

(* Int-array access layer: zero-alloc per decode. Used by field
   constraints / where clauses where the validator has already populated
   the per-decode int_array. *)
type idx = string -> int
type compile_ctx = { idx : idx; sizeof_this : int; field_pos : int }

let array_leaves (cc : compile_ctx) : int array leaves =
  {
    ref_ =
      (fun name ->
        let i = cc.idx name in
        fun a -> a.(i));
    param_ref =
      (fun (Pack_param p) a ->
        let i = p.ph_slot in
        if i >= 0 then a.(i) else !(p.ph_cell));
    sizeof_typ =
      (fun (Pack_typ t) ->
        let n = field_wire_size t |> Option.value ~default:0 in
        fun _ -> n);
    sizeof_this = (fun _ -> cc.sizeof_this);
    field_pos = (fun _ -> cc.field_pos);
  }

let compile_int_arr cc e = compile_int (array_leaves cc) e
let compile_bool_arr cc e = compile_bool (array_leaves cc) e

(* Compile action statements to operate on an int array instead of Eval.ctx.
   Assign writes to ph_cell (mutable param) and updates the array.
   Var binds a local by extending the index -- but since we can't grow the
   array, local vars in actions use ph_cell-style mutation or are inlined.

   Return true short-circuits remaining statements (the action succeeds).
   Return false and Abort raise Parse_error to abort the parse. *)
type compiled_action = int array -> unit

exception Return_true

let rec compile_stmt (cc : compile_ctx) (s : Types.action_stmt) :
    compiled_action =
  match s with
  | Assign (p, e) ->
      let fe = compile_int_arr cc e in
      fun arr ->
        let v = fe arr in
        let slot = p.Types.ph_slot in
        if slot >= 0 then arr.(slot) <- v else p.Types.ph_cell := v
  | Field_assign (_, _, _) | Extern_call (_, _) -> fun _ -> ()
  | Return e ->
      let fe = compile_bool_arr cc e in
      fun arr ->
        if fe arr then raise_notrace Return_true
        else raise (Parse_error (Constraint_failed "field action"))
  | Types.Abort ->
      fun _ -> raise (Parse_error (Constraint_failed "field action"))
  | If (cond, then_, else_) ->
      let fc = compile_bool_arr cc cond in
      let ft = compile_stmts cc then_ in
      let fe =
        match else_ with
        | Some stmts -> compile_stmts cc stmts
        | None -> fun _ -> ()
      in
      fun arr -> if fc arr then ft arr else fe arr
  | Var (name, e) ->
      (* [Action.var name e] writes [e]'s value to the int_array slot for
         [name]. The slot is auto-registered by [action_vars] before the
         index is built, so [cc.idx name] always resolves; the
         out-of-bounds and [Failure] catches that used to live here were
         dead defensive code, and matched the silent-default shape that
         hid the predicate / [Field.ref] bugs in the parallel compilers.
         Drop the catches -- if [cc.idx name] ever raises here we want it
         to surface, not write to nowhere. *)
      let i = cc.idx name in
      let fe = compile_int_arr cc e in
      fun arr -> arr.(i) <- fe arr

and compile_stmts (cc : compile_ctx) (stmts : Types.action_stmt list) :
    compiled_action =
  match stmts with
  | [] -> fun _ -> ()
  | [ s ] -> compile_stmt cc s
  | stmts ->
      let compiled = List.map (compile_stmt cc) stmts in
      fun arr -> List.iter (fun f -> f arr) compiled

let compile_action (cc : compile_ctx) (act : action option) :
    compiled_action option =
  match act with
  | None -> None
  | Some (On_success stmts | On_act stmts) ->
      let f = compile_stmts cc stmts in
      Some (fun arr -> try f arr with Return_true -> ())

type ('f, 'r) record =
  | Record : {
      r_name : string;
      r_make : 'full;
      r_readers : ('full, 'f) readers;
      r_writers_rev : ('r -> bytes -> int -> unit) list;
      r_min_wire_size : int;
          (* sum of all fixed-size fields -- minimum buffer size *)
      r_next_off : next_off; (* where the next field starts *)
      r_fields_rev : Types.field list;
      r_validators_rev :
        (int (* byte offset *) * (int array -> bytes -> int -> unit)) list;
      r_checkers_rev :
        (int (* byte offset *) * (int array -> bytes -> int -> unit)) list;
      r_field_actions_rev : (string * compiled_action) list;
      r_n_fields : int; (* count of named fields (for field indexing) *)
      r_n_array_slots : int;
          (* fields + action-local vars (for array allocation) *)
      r_bf : bf_codec_state option;
      r_field_access_rev : (string * field_access) list;
      r_field_readers : field_reader list;
      r_where : bool expr option;
    }
      -> ('f, 'r) record

type wire_size_info =
  | Fixed of int
  | Variable of { min_size : int; compute : bytes -> int -> int }

let id_counter = Atomic.make 0

type 'r t = {
  t_id : int;
  t_name : string;
  t_field_access : (string * field_access) list;
  t_field_readers : field_reader list;
  t_field_actions : (string * compiled_action) list;
  t_decode : bytes -> int -> 'r;
  t_encode : 'r -> bytes -> int -> unit;
  t_wire_size : wire_size_info;
  t_struct_fields : Types.field list;
  t_validate : bytes -> int -> unit;
  t_validate_arr : int array -> bytes -> int -> unit;
  t_populate : int array -> bytes -> int -> unit;
  t_n_array_slots : int;
  t_param_base : int;
  t_n_params : int;
  t_param_handles : Param.packed list;
  t_where : bool expr option;
}

let record_start ?where name make =
  Record
    {
      r_name = name;
      r_make = make;
      r_readers = Nil;
      r_writers_rev = [];
      r_min_wire_size = 0;
      r_next_off = Static 0;
      r_fields_rev = [];
      r_validators_rev = [];
      r_checkers_rev = [];
      r_field_actions_rev = [];
      r_n_fields = 0;
      r_n_array_slots = 0;
      r_bf = None;
      r_field_access_rev = [];
      r_field_readers = [];
      r_where = where;
    }

let bind (f : 'a Field.t) get =
  {
    name = Field.name f;
    typ = Field.typ f;
    constraint_ = Field.constraint_ f;
    action = Field.action f;
    get;
  }

let ( $ ) = bind

(* Bitfield helpers -- shared module for base operations, specialized closures
   for performance-critical read/write dispatched at codec construction time. *)

let bf_base_byte_size = Bitfield.byte_size
let bf_base_total_bits = Bitfield.total_bits
let bf_base_equal = Bitfield.equal
let bf_write_base = Bitfield.write_word

(* Build-time dispatch: pattern match on base happens once at codec
   construction, not on every read/write call. *)
let build_bf_reader base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | BF_U8 ->
      fun buf off -> (Bytes.get_uint8 buf (off + byte_off) lsr shift) land mask
  | BF_U16 Little ->
      fun buf off ->
        (Bytes.get_uint16_le buf (off + byte_off) lsr shift) land mask
  | BF_U16 Big ->
      fun buf off ->
        (Bytes.get_uint16_be buf (off + byte_off) lsr shift) land mask
  | BF_U32 Little ->
      fun buf off -> (UInt32.le buf (off + byte_off) lsr shift) land mask
  | BF_U32 Big ->
      fun buf off -> (UInt32.be buf (off + byte_off) lsr shift) land mask

let err_bf_overflow width value =
  Fmt.invalid_arg "Codec.encode: value 0x%X exceeds %d-bit field width" value
    width

let[@inline] check_bf_overflow width value =
  if value lsr width <> 0 then err_bf_overflow width value

let build_bf_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | BF_U8 ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U16 Little ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U16 Big ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U32 Little ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = UInt32.le buf (off + byte_off) in
        UInt32.set_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U32 Big ->
      fun buf off value ->
        check_bf_overflow width value;
        let cur = UInt32.be buf (off + byte_off) in
        UInt32.set_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))

let build_bf_accessor_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  let clear_mask = lnot (mask lsl shift) in
  match base with
  | BF_U8 ->
      fun buf off value ->
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | BF_U16 Little ->
      fun buf off value ->
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | BF_U16 Big ->
      fun buf off value ->
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | BF_U32 Little ->
      fun buf off value ->
        let cur = UInt32.le buf (off + byte_off) in
        UInt32.set_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | BF_U32 Big ->
      fun buf off value ->
        let cur = UInt32.be buf (off + byte_off) in
        UInt32.set_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))

let build_bf_clear base byte_off =
 fun buf off -> bf_write_base base buf (off + byte_off) 0

let build_idx readers =
  let rev_readers = List.rev readers in
  fun name ->
    let rec find i = function
      | [] -> failwith ("unbound field: " ^ name)
      | (n, _) :: _ when n = name -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 rev_readers

(* Collect local variable names from action statements *)
let rec action_vars acc = function
  | Types.Var (name, _) -> name :: acc
  | Types.If (_, then_, else_) -> (
      let acc = List.fold_left action_vars acc then_ in
      match else_ with
      | Some stmts -> List.fold_left action_vars acc stmts
      | None -> acc)
  | _ -> acc

let rec iter_param_refs : type a. (Param.packed -> unit) -> a expr -> unit =
 fun f -> function
  | Param_ref p -> f (Param.Pack p)
  | Add (a, b)
  | Sub (a, b)
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Land (a, b)
  | Lor (a, b)
  | Lxor (a, b)
  | Lsl (a, b)
  | Lsr (a, b)
  | Lt (a, b)
  | Le (a, b)
  | Gt (a, b)
  | Ge (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Eq (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Ne (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | And (a, b) | Or (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Not a -> iter_param_refs f a
  | Lnot a -> iter_param_refs f a
  | Cast (_, a) -> iter_param_refs f a
  | If_then_else (c, t, e) ->
      iter_param_refs f c;
      iter_param_refs f t;
      iter_param_refs f e
  | Int _ | Int64 _ | Bool _ | Ref _ | Sizeof _ | Sizeof_this | Field_pos -> ()

let rec iter_param_refs_stmt f = function
  | Types.Assign (p, e) ->
      f (Param.Pack p);
      iter_param_refs f e
  | Types.Field_assign (_, _, e) -> iter_param_refs f e
  | Types.Extern_call (_, _) -> ()
  | Types.Return e -> iter_param_refs f e
  | Types.Abort -> ()
  | Types.If (c, t, e) ->
      iter_param_refs f c;
      List.iter (iter_param_refs_stmt f) t;
      Option.iter (List.iter (iter_param_refs_stmt f)) e
  | Types.Var (_, e) -> iter_param_refs f e

let iter_param_refs_action f = function
  | Types.On_success stmts | Types.On_act stmts ->
      List.iter (iter_param_refs_stmt f) stmts

let rec iter_param_refs_typ : type a. (Param.packed -> unit) -> a typ -> unit =
 fun f typ ->
  match typ with
  | Byte_array { size } | Byte_slice { size } -> iter_param_refs f size
  | Byte_array_where { size; cond; _ } ->
      iter_param_refs f size;
      iter_param_refs f cond
  | Uint_var { size; _ } -> iter_param_refs f size
  | Single_elem { size; elem; _ } ->
      iter_param_refs f size;
      iter_param_refs_typ f elem
  | Array { len; elem; _ } ->
      iter_param_refs f len;
      iter_param_refs_typ f elem
  | Repeat { size; elem; _ } ->
      iter_param_refs f size;
      iter_param_refs_typ f elem
  | Where { cond; inner } ->
      iter_param_refs f cond;
      iter_param_refs_typ f inner
  | Optional { present; inner } ->
      iter_param_refs f present;
      iter_param_refs_typ f inner
  | Optional_or { present; inner; _ } ->
      iter_param_refs f present;
      iter_param_refs_typ f inner
  | Apply { typ; args } ->
      iter_param_refs_typ f typ;
      List.iter (fun (Types.Pack_expr e) -> iter_param_refs f e) args
  | Map { inner; _ } -> iter_param_refs_typ f inner
  | Enum { base; _ } -> iter_param_refs_typ f base
  | Casetype { tag; cases; _ } ->
      iter_param_refs_typ f tag;
      List.iter
        (fun (Types.Case_branch { cb_inner; _ }) ->
          iter_param_refs_typ f cb_inner)
        cases
  | Uint8 | Uint16 _ | Uint32 _ | Uint63 _ | Uint64 _ | Int8 | Int16 _ | Int32 _
  | Int64 _ | Float32 _ | Float64 _ | Bits _ | Unit | All_bytes | All_zeros
  | Struct _ | Type_ref _ | Qualified_ref _ | Codec _ ->
      ()

let iter_param_refs_fields f fields where =
  List.iter
    (fun (Types.Field fld) ->
      iter_param_refs_typ f fld.field_typ;
      Option.iter (iter_param_refs f) fld.constraint_;
      Option.iter (iter_param_refs_action f) fld.action)
    fields;
  Option.iter (iter_param_refs f) where

(* Read one element of a typ at a given buffer position. Used by Repeat. *)
let rec read_elem : type a. a typ -> bytes -> int -> a =
 fun typ buf off ->
  match typ with
  | Uint8 -> Bytes.get_uint8 buf off
  | Uint16 Little -> Bytes.get_uint16_le buf off
  | Uint16 Big -> Bytes.get_uint16_be buf off
  | Uint32 Little -> UInt32.le buf off
  | Uint32 Big -> UInt32.be buf off
  | Uint63 Little -> UInt63.le buf off
  | Uint63 Big -> UInt63.be buf off
  | Uint64 Little -> Bytes.get_int64_le buf off
  | Uint64 Big -> Bytes.get_int64_be buf off
  | Int8 -> Bytes.get_int8 buf off
  | Int16 Little -> Bytes.get_int16_le buf off
  | Int16 Big -> Bytes.get_int16_be buf off
  | Int32 Little -> Int32.to_int (Bytes.get_int32_le buf off)
  | Int32 Big -> Int32.to_int (Bytes.get_int32_be buf off)
  | Int64 Little -> Bytes.get_int64_le buf off
  | Int64 Big -> Bytes.get_int64_be buf off
  | Float32 Little -> Int32.float_of_bits (Bytes.get_int32_le buf off)
  | Float32 Big -> Int32.float_of_bits (Bytes.get_int32_be buf off)
  | Float64 Little -> Int64.float_of_bits (Bytes.get_int64_le buf off)
  | Float64 Big -> Int64.float_of_bits (Bytes.get_int64_be buf off)
  | Uint_var { size = Int n; endian } -> Uint_var.read endian buf off n
  | Codec { codec_decode; _ } -> codec_decode buf off
  | Map { inner; decode; _ } -> decode (read_elem inner buf off)
  | Where { inner; _ } -> read_elem inner buf off
  | Enum { base; _ } -> read_elem base buf off
  | Casetype { tag; cases; _ } ->
      let tag_size =
        match field_wire_size tag with
        | Some n -> n
        | None -> assert false (* casetype tag is always fixed-size int *)
      in
      let tag_val = read_elem tag buf off in
      let body_off = off + tag_size in
      let rec find = function
        | [] -> raise (Parse_error (Invalid_tag tag_val))
        | Case_branch { cb_tag = Some t; cb_inner; cb_inject; _ } :: _
          when t = tag_val ->
            cb_inject (read_elem cb_inner buf body_off)
        | Case_branch { cb_tag = None; cb_inner; cb_inject; _ } :: _ ->
            cb_inject (read_elem cb_inner buf body_off)
        | _ :: rest -> find rest
      in
      find cases
  | _ -> failwith "read_elem: unsupported element type in repeat"

(* Write one element of a typ at a given buffer position. Used by Repeat. *)
let rec write_elem : type a. a typ -> bytes -> int -> a -> unit =
 fun typ buf off v ->
  match typ with
  | Uint8 -> Bytes.set_uint8 buf off v
  | Uint16 Little -> Bytes.set_uint16_le buf off v
  | Uint16 Big -> Bytes.set_uint16_be buf off v
  | Uint32 Little -> UInt32.set_le buf off v
  | Uint32 Big -> UInt32.set_be buf off v
  | Uint63 Little -> UInt63.set_le buf off v
  | Uint63 Big -> UInt63.set_be buf off v
  | Uint64 Little -> Bytes.set_int64_le buf off v
  | Uint64 Big -> Bytes.set_int64_be buf off v
  | Int8 -> Bytes.set_int8 buf off v
  | Int16 Little -> Bytes.set_int16_le buf off v
  | Int16 Big -> Bytes.set_int16_be buf off v
  | Int32 Little -> Bytes.set_int32_le buf off (Int32.of_int v)
  | Int32 Big -> Bytes.set_int32_be buf off (Int32.of_int v)
  | Int64 Little -> Bytes.set_int64_le buf off v
  | Int64 Big -> Bytes.set_int64_be buf off v
  | Float32 Little -> Bytes.set_int32_le buf off (Int32.bits_of_float v)
  | Float32 Big -> Bytes.set_int32_be buf off (Int32.bits_of_float v)
  | Float64 Little -> Bytes.set_int64_le buf off (Int64.bits_of_float v)
  | Float64 Big -> Bytes.set_int64_be buf off (Int64.bits_of_float v)
  | Uint_var { size = Int n; endian } -> Uint_var.write endian buf off n v
  | Codec { codec_encode; _ } -> codec_encode v buf off
  | Map { inner; encode; _ } -> write_elem inner buf off (encode v)
  | Where { inner; _ } -> write_elem inner buf off v
  | Enum { base; _ } -> write_elem base buf off v
  | _ -> failwith "write_elem: unsupported element type in repeat"

(* Compute the wire size of one element at a buffer position. Used by Repeat
   for variable-size elements. *)
let rec elem_size_of : type a. a typ -> bytes -> int -> int =
 fun typ buf off ->
  match typ with
  | Codec { codec_size_of; _ } -> codec_size_of buf off
  | Casetype { tag; cases; _ } ->
      let tag_size =
        match field_wire_size tag with Some n -> n | None -> assert false
      in
      let tag_val = read_elem tag buf off in
      let body_off = off + tag_size in
      let rec find = function
        | [] -> raise (Parse_error (Invalid_tag tag_val))
        | Case_branch { cb_tag = Some t; cb_inner; _ } :: _ when t = tag_val ->
            tag_size + elem_size_of cb_inner buf body_off
        | Case_branch { cb_tag = None; cb_inner; _ } :: _ ->
            tag_size + elem_size_of cb_inner buf body_off
        | _ :: rest -> find rest
      in
      find cases
  | Map { inner; _ } -> elem_size_of inner buf off
  | Where { inner; _ } -> elem_size_of inner buf off
  | _ -> (
      match field_wire_size typ with
      | Some n -> n
      | None -> failwith "elem_size_of: cannot determine element size")

(* -- Compiled field: intermediate plan for one field's contribution -- *)

(* A [compiled_field] is the self-contained plan for appending one field to a
   record state. The per-type [compile_*] helpers build one; [apply_compiled]
   consumes it and produces the updated record state. *)
type ('a, 'r) compiled_field = {
  raw_reader : bytes -> int -> 'a;
  raw_writer : 'r -> bytes -> int -> unit;
  extra_writers : ('r -> bytes -> int -> unit) list;
      (* Writers that run strictly before [raw_writer] -- e.g. a bf_clear that
         opens a new packed base word. *)
  field_access : field_access;
  size_delta : int; (* added to [r_min_wire_size] *)
  next_off : next_off; (* new [r_next_off] *)
  bf_after : bf_codec_state option;
  int_reader : bytes -> int -> int;
      (* Entry stored in [r_field_readers] under the field name; const 0 for
         composite types that have no int-array slot of their own. *)
  nested_readers : field_reader list;
      (* Embedded sub-codec field readers, shifted into the parent frame. *)
  validator_off : int; (* [-1] when the byte offset is dynamic *)
  populate : int array -> bytes -> int -> unit;
      (* Pre-built populate function for the int-array validator path; the
         slot index (r_n_fields at the time the field is compiled) is baked
         in. Composite types use a no-op. *)
}

(* Parameter-free slice of the record state that [compile_field] needs. Kept
   separate so the per-type helpers do not need polymorphic recursion across
   the record's 'f type. *)
type layout_ctx = {
  lc_next_off : next_off;
  lc_bf : bf_codec_state option;
  lc_field_readers : field_reader list;
  lc_n_fields : int;
}

let layout_ctx_of : type f r. (f, r) record -> layout_ctx =
 fun (Record r) ->
  {
    lc_next_off = r.r_next_off;
    lc_bf = r.r_bf;
    lc_field_readers = r.r_field_readers;
    lc_n_fields = r.r_n_fields;
  }

(* -- Layout helpers shared by per-type plans -- *)

let static_off_of (ctx : layout_ctx) : int option =
  match ctx.lc_next_off with Static n -> Some n | Dynamic _ -> None

let off_fn_of (ctx : layout_ctx) : bytes -> int -> int =
  match ctx.lc_next_off with
  | Static n -> fun _buf _base -> n
  | Dynamic f -> fun buf base -> f buf base - base

(* Byte offset used as [sizeof_this] when compiling constraints/actions: a
   real static offset when known, the [-1] sentinel otherwise. *)
let validator_off_of (ctx : layout_ctx) : int =
  match ctx.lc_next_off with Static n -> n | Dynamic _ -> -1

(* Field access that stores a constant or dynamic offset, based on [ctx]. *)
let fixed_or_dynamic_fa (ctx : layout_ctx) : field_access =
  match ctx.lc_next_off with
  | Static n -> Fixed n
  | Dynamic _ -> Dynamic (off_fn_of ctx)

let require_static_off (ctx : layout_ctx) ~what : int =
  match ctx.lc_next_off with
  | Static n -> n
  | Dynamic _ ->
      invalid_arg
        ("add_field: " ^ what ^ " after variable-size field not supported")

(* New [next_off] after appending a fixed-size contribution of [n] bytes. *)
let advance_next_off (no : next_off) (n : int) : next_off =
  match no with
  | Static k -> Static (k + n)
  | Dynamic f -> Dynamic (fun buf base -> f buf base + n)

let null_int_reader : bytes -> int -> int = fun _buf _base -> 0
let no_populate : int array -> bytes -> int -> unit = fun _arr _buf _base -> ()

(* Reader+writer pair for a Codec-or-scalar inner type placed at the current
   frame offset. Extracted so [compile_optional] and [compile_optional_or]
   share it. *)
let inner_codec_accessors : type w.
    w typ -> layout_ctx -> (bytes -> int -> w) * (bytes -> int -> w -> unit) =
 fun inner ctx ->
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  let reader : bytes -> int -> w =
    match inner with
    | Codec { codec_decode; _ } -> (
        match field_off_static with
        | Some fo -> fun buf base -> codec_decode buf (base + fo)
        | None ->
            fun buf base ->
              let fo = field_off_fn buf base in
              codec_decode buf (base + fo))
    | _ -> (
        match field_off_static with
        | Some fo -> build_field_reader inner fo
        | None ->
            let reader_at_0 = build_field_reader inner 0 in
            fun buf base ->
              let fo = field_off_fn buf base in
              reader_at_0 buf (base + fo))
  in
  let writer : bytes -> int -> w -> unit =
    match inner with
    | Codec { codec_encode; _ } -> (
        match field_off_static with
        | Some fo -> fun buf off iv -> codec_encode iv buf (off + fo)
        | None ->
            fun buf off iv ->
              let fo = field_off_fn buf off in
              codec_encode iv buf (off + fo))
    | _ -> (
        let enc = build_field_encoder inner in
        match field_off_static with
        | Some fo -> fun buf off iv -> ignore (enc buf (off + fo) iv)
        | None ->
            fun buf off iv ->
              let fo = field_off_fn buf off in
              ignore (enc buf (off + fo) iv))
  in
  (reader, writer)

let build_nested_readers ctx readers =
  match static_off_of ctx with
  | Some fo ->
      List.map
        (fun (n, reader) -> (n, fun buf base -> reader buf (base + fo)))
        readers
  | None ->
      let off_fn = off_fn_of ctx in
      List.map
        (fun (n, reader) ->
          (n, fun buf base -> reader buf (base + off_fn buf base)))
        readers

(* -- Per-type [compile_field] helpers -- *)

(* Dispatch on the field's typ once, then delegate to the relevant per-type
   helper. Each helper builds the [compiled_field] for its case; neither the
   helpers nor [compile_field] itself apply constraints or actions -- that
   happens in [apply_compiled]. *)
let rec compile_field : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  match fld.typ with
  | Map { inner; decode; encode } -> compile_map ctx fld inner decode encode
  | Enum { base; _ } -> compile_field ctx { fld with typ = base }
  | Where { inner; _ } -> compile_field ctx { fld with typ = inner }
  | Bits { width; base; bit_order } -> compile_bits ctx fld width base bit_order
  | Codec
      {
        codec_decode;
        codec_encode;
        codec_fixed_size;
        codec_size_of;
        codec_field_readers;
        _;
      } ->
      compile_codec ctx fld ~codec_decode ~codec_encode ~codec_fixed_size
        ~codec_size_of ~codec_field_readers
  | Optional { present; inner } -> compile_optional ctx fld present inner
  | Optional_or { present; inner; default } ->
      compile_optional_or ctx fld present inner default
  | Repeat { size; elem; seq } -> compile_repeat ctx fld size elem seq
  | _ -> compile_scalar_or_var ctx fld

and compile_map : type a w r.
    layout_ctx ->
    (a, r) field ->
    w typ ->
    (w -> a) ->
    (a -> w) ->
    (a, r) compiled_field =
 fun ctx fld inner decode encode ->
  let outer_get = fld.get in
  let inner_fld =
    {
      name = fld.name;
      typ = inner;
      constraint_ = fld.constraint_;
      action = fld.action;
      get = (fun v -> encode (outer_get v));
    }
  in
  let cf = compile_field ctx inner_fld in
  { cf with raw_reader = (fun buf off -> decode (cf.raw_reader buf off)) }

and compile_bits : type r.
    layout_ctx ->
    (int, r) field ->
    int ->
    bitfield_base ->
    bit_order ->
    (int, r) compiled_field =
 fun ctx fld width base bit_order ->
  let total = bf_base_total_bits base in
  let static_off = require_static_off ctx ~what:"bitfields" in
  let base_off, bits_used, size_delta, extra_writers =
    match ctx.lc_bf with
    | Some bf
      when bf_base_equal bf.bfc_base base
           && bf.bfc_bit_order = bit_order
           && bf.bfc_bits_used + width <= bf.bfc_total_bits ->
        (bf.bfc_base_off, bf.bfc_bits_used, 0, [])
    | _ ->
        let clear = build_bf_clear base static_off in
        ( static_off,
          0,
          bf_base_byte_size base,
          [ (fun _v buf off -> clear buf off) ] )
  in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used ~width in
  let raw_reader = build_bf_reader base base_off shift width in
  let raw_writer_inner = build_bf_writer base base_off shift width in
  let get = fld.get in
  let raw_writer v buf off = raw_writer_inner buf off (get v) in
  let field_access = Bitfield { base; byte_off = base_off; shift; width } in
  let bf_after =
    Some
      {
        bfc_base = base;
        bfc_bit_order = bit_order;
        bfc_base_off = base_off;
        bfc_bits_used = bits_used + width;
        bfc_total_bits = total;
      }
  in
  let populate = build_populate fld.typ ctx.lc_n_fields raw_reader in
  {
    raw_reader;
    raw_writer;
    extra_writers;
    field_access;
    size_delta;
    next_off = Static (static_off + size_delta);
    bf_after;
    int_reader = raw_reader;
    nested_readers = [];
    validator_off = static_off;
    populate;
  }

and compile_codec : type a r.
    layout_ctx ->
    (a, r) field ->
    codec_decode:(bytes -> int -> a) ->
    codec_encode:(a -> bytes -> int -> unit) ->
    codec_fixed_size:int option ->
    codec_size_of:(bytes -> int -> int) ->
    codec_field_readers:field_reader list ->
    (a, r) compiled_field =
 fun ctx fld ~codec_decode ~codec_encode ~codec_fixed_size ~codec_size_of
     ~codec_field_readers ->
  let nested_readers = build_nested_readers ctx codec_field_readers in
  let get = fld.get in
  match codec_fixed_size with
  | Some fsize ->
      let raw_reader, inner_writer = inner_codec_accessors fld.typ ctx in
      let raw_writer v buf off = inner_writer buf off (get v) in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.lc_next_off fsize;
        bf_after = None;
        int_reader = null_int_reader;
        nested_readers;
        validator_off = validator_off_of ctx;
        populate = no_populate;
      }
  | None ->
      compile_codec_variable ctx ~get ~codec_decode ~codec_encode ~codec_size_of
        ~nested_readers

(* Variable-size sub-codec: dispatch on whether we sit at a static or a
   dynamic running offset. Mirrors [compile_var_bytes] -- both flavours
   produce a [Variable]/[Variable_dynamic] [field_access] that downstream
   [build_staged_reader]/[build_staged_writer] already know how to thread.
   Without the dynamic case, two consecutive variable-size sub-codec fields
   trip [require_static_off]. *)
and compile_codec_variable : type a r.
    layout_ctx ->
    get:(r -> a) ->
    codec_decode:(bytes -> int -> a) ->
    codec_encode:(a -> bytes -> int -> unit) ->
    codec_size_of:(bytes -> int -> int) ->
    nested_readers:field_reader list ->
    (a, r) compiled_field =
 fun ctx ~get ~codec_decode ~codec_encode ~codec_size_of ~nested_readers ->
  let off_fn, (field_access : field_access), validator_off =
    match ctx.lc_next_off with
    | Static n ->
        let size_fn buf base = codec_size_of buf (base + n) in
        ((fun _buf _base -> n), Variable { off = n; size_fn }, n)
    | Dynamic prev_end ->
        let off_fn buf base = prev_end buf base - base in
        let size_fn buf base = codec_size_of buf (base + off_fn buf base) in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  {
    raw_reader = (fun buf base -> codec_decode buf (base + off_fn buf base));
    raw_writer =
      (fun v buf off -> codec_encode (get v) buf (off + off_fn buf off));
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic (fun buf base -> base + off_fn buf base + size_fn buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers;
    validator_off;
    populate = no_populate;
  }

and dynamic_optional_next_off ctx present_fn fsize =
  let base_off = ctx.lc_next_off in
  Dynamic
    (fun buf base ->
      let off =
        match base_off with Static n -> base + n | Dynamic f -> f buf base
      in
      if present_fn buf base then off + fsize else off)

and optional_compiled : type a r.
    layout_ctx ->
    raw_reader:(bytes -> int -> a) ->
    raw_writer:(r -> bytes -> int -> unit) ->
    size_delta:int ->
    next_off:next_off ->
    populate:(int array -> bytes -> int -> unit) ->
    (a, r) compiled_field =
 fun ctx ~raw_reader ~raw_writer ~size_delta ~next_off ~populate ->
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access = fixed_or_dynamic_fa ctx;
    size_delta;
    next_off;
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers = [];
    validator_off = validator_off_of ctx;
    populate;
  }

and compile_optional : type a r.
    layout_ctx ->
    (a option, r) field ->
    bool expr ->
    a typ ->
    (a option, r) compiled_field =
 fun ctx fld present inner ->
  let inner_size = field_wire_size inner in
  let nfields = ctx.lc_n_fields in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate = build_populate inner nfields inner_reader in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun buf base -> Some (inner_reader buf base))
        ~raw_writer:(fun v buf off ->
          match get v with Some iv -> inner_writer buf off iv | None -> ())
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.lc_next_off fsize)
        ~populate:inner_populate
  | Bool false, _ ->
      optional_compiled ctx
        ~raw_reader:(fun _buf _base -> None)
        ~raw_writer:(fun _v _buf _off -> ())
        ~size_delta:0 ~next_off:ctx.lc_next_off ~populate:no_populate
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.lc_field_readers present in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate = build_populate inner nfields inner_reader in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun buf base ->
          if present_fn buf base then Some (inner_reader buf base) else None)
        ~raw_writer:(fun v buf off ->
          match get v with Some iv -> inner_writer buf off iv | None -> ())
        ~size_delta:0
        ~next_off:(dynamic_optional_next_off ctx present_fn fsize)
        ~populate:(fun arr buf base ->
          if present_fn buf base then inner_populate arr buf base)
  | _ ->
      invalid_arg
        "add_field: dynamic optional with variable-size inner not yet supported"

and compile_optional_or : type a r.
    layout_ctx ->
    (a, r) field ->
    bool expr ->
    a typ ->
    a ->
    (a, r) compiled_field =
 fun ctx fld present inner default ->
  let inner_size = field_wire_size inner in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let populate = build_populate fld.typ ctx.lc_n_fields inner_reader in
      optional_compiled ctx ~raw_reader:inner_reader
        ~raw_writer:(fun v buf off -> inner_writer buf off (get v))
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.lc_next_off fsize)
        ~populate
  | Bool false, _ ->
      optional_compiled ctx
        ~raw_reader:(fun _buf _base -> default)
        ~raw_writer:(fun _v _buf _off -> ())
        ~size_delta:0 ~next_off:ctx.lc_next_off ~populate:no_populate
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.lc_field_readers present in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let raw_reader buf base =
        if present_fn buf base then inner_reader buf base else default
      in
      let populate = build_populate fld.typ ctx.lc_n_fields raw_reader in
      optional_compiled ctx ~raw_reader
        ~raw_writer:(fun v buf off ->
          if present_fn buf off then inner_writer buf off (get v))
        ~size_delta:0
        ~next_off:(dynamic_optional_next_off ctx present_fn fsize)
        ~populate
  | _ ->
      invalid_arg
        "add_field: dynamic optional_or with variable-size inner not yet \
         supported"

and repeat_raw_reader : type elt seq.
    (elt, seq) seq_map ->
    elt typ ->
    elem_size:int option ->
    off_fn:(bytes -> int -> int) ->
    size_fn:(bytes -> int -> int) ->
    bytes ->
    int ->
    seq =
 fun (Seq_map seq) elem ~elem_size ~off_fn ~size_fn ->
  match elem_size with
  | Some esz ->
      (* Fixed-size elements: direct fill via builder. *)
      fun buf base ->
        let budget = size_fn buf base in
        let n = if esz > 0 then budget / esz else 0 in
        let start = base + off_fn buf base in
        let rec loop acc i =
          if i >= n then seq.finish acc
          else
            loop (seq.add acc (read_elem elem buf (start + (i * esz)))) (i + 1)
        in
        loop seq.empty 0
  | None ->
      (* Variable-size elements: builder accumulation. *)
      fun buf base ->
        let budget = size_fn buf base in
        let rec loop acc pos remaining =
          if remaining <= 0 then seq.finish acc
          else
            let v = read_elem elem buf pos in
            let esz = elem_size_of elem buf pos in
            loop (seq.add acc v) (pos + esz) (remaining - esz)
        in
        loop seq.empty (base + off_fn buf base) budget

and compile_repeat : type elt seq r.
    layout_ctx ->
    (seq, r) field ->
    int expr ->
    elt typ ->
    (elt, seq) seq_map ->
    (seq, r) compiled_field =
 fun ctx fld size_expr elem (Seq_map seq) ->
  (* Same dynamic-offset dispatch as [compile_var_bytes] / [compile_codec]:
     when a [Repeat] sits after a variable-size field, the running offset
     is [Dynamic], which previously tripped [require_static_off]. *)
  let off_fn, (field_access : field_access), validator_off =
    let sizeof_this : bytes -> int -> int =
      match ctx.lc_next_off with
      | Static n -> fun _buf _base -> n
      | Dynamic prev_end -> fun buf base -> prev_end buf base - base
    in
    let size_fn = compile_expr ~sizeof_this ctx.lc_field_readers size_expr in
    match ctx.lc_next_off with
    | Static n -> ((fun _buf _base -> n), Variable { off = n; size_fn }, n)
    | Dynamic prev_end ->
        let off_fn buf base = prev_end buf base - base in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  let elem_size = field_wire_size elem in
  let raw_reader =
    repeat_raw_reader (Seq_map seq) elem ~elem_size ~off_fn ~size_fn
  in
  let get = fld.get in
  let step =
    match elem_size with
    | Some esz -> fun _buf _pos -> esz
    | None -> fun buf pos -> elem_size_of elem buf pos
  in
  let raw_writer v buf off =
    let items = get v in
    let pos = Stdlib.ref (off + off_fn buf off) in
    seq.iter
      (fun item ->
        write_elem elem buf !pos item;
        pos := !pos + step buf !pos)
      items
  in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic (fun buf base -> base + off_fn buf base + size_fn buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers = [];
    validator_off;
    populate = no_populate;
  }

and compile_scalar_or_var : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  match field_wire_size typ with
  | Some fsize ->
      let field_off = match field_off_static with Some n -> n | None -> -1 in
      let raw_reader =
        match field_off_static with
        | Some _ -> build_field_reader typ field_off
        | None ->
            let reader_at_0 = build_field_reader typ 0 in
            fun buf base ->
              let off = field_off_fn buf base in
              reader_at_0 buf (base + off)
      in
      let raw_encoder = build_field_encoder typ in
      let get = fld.get in
      let raw_writer : r -> bytes -> int -> unit =
        match field_off_static with
        | Some fo ->
            fun v buf off ->
              let _ = raw_encoder buf (off + fo) (get v) in
              ()
        | None ->
            fun v buf off ->
              let fo = field_off_fn buf off in
              let _ = raw_encoder buf (off + fo) (get v) in
              ()
      in
      let int_reader buf base =
        match int_of_typ_value typ (raw_reader buf base) with
        | Some v -> v
        | None -> 0
      in
      let populate = build_populate typ ctx.lc_n_fields raw_reader in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.lc_next_off fsize;
        bf_after = None;
        int_reader;
        nested_readers = [];
        validator_off = field_off;
        populate;
      }
  | None -> compile_var_bytes ctx fld

and var_bytes_reader : type a.
    a typ -> (bytes -> int -> int) -> (bytes -> int -> int) -> bytes -> int -> a
    =
 fun typ off_fn size_fn buf base ->
  let fo = off_fn buf base in
  let sz = size_fn buf base in
  match typ with
  | Byte_slice _ -> Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _ -> Bytes.sub_string buf (base + fo) sz
  | Byte_array_where _ -> Bytes.sub_string buf (base + fo) sz
  | All_bytes -> Bytes.sub_string buf (base + fo) sz
  | All_zeros ->
      let s = Bytes.sub_string buf (base + fo) sz in
      String.iter
        (fun c ->
          if c <> '\000' then
            invalid_arg "add_field: [all_zeros] field has a non-zero byte")
        s;
      s
  | Casetype _ -> read_elem typ buf (base + fo)
  | Single_elem { elem; _ } -> read_elem elem buf (base + fo)
  | _ -> assert false

and var_bytes_writer : type a r.
    a typ -> (r -> a) -> (bytes -> int -> int) -> r -> bytes -> int -> unit =
 fun typ get off_fn v buf off ->
  let fo = off_fn buf off in
  let value = get v in
  match typ with
  | Byte_slice _ ->
      let src = (value : Slice.t) in
      Bytes.blit (Slice.bytes src) (Slice.first src) buf (off + fo)
        (Slice.length src)
  | Byte_array _ ->
      let s = (value : string) in
      Bytes.blit_string s 0 buf (off + fo) (String.length s)
  | Byte_array_where _ ->
      let s = (value : string) in
      Bytes.blit_string s 0 buf (off + fo) (String.length s)
  | All_bytes ->
      let s = (value : string) in
      Bytes.blit_string s 0 buf (off + fo) (String.length s)
  | All_zeros ->
      let s = (value : string) in
      Bytes.blit_string s 0 buf (off + fo) (String.length s)
  | Casetype _ -> ignore (build_field_encoder typ buf (off + fo) value)
  | Single_elem { elem; _ } ->
      ignore (build_field_encoder elem buf (off + fo) value)
  | _ -> assert false

and compile_var_size_fn : type a.
    layout_ctx -> a typ -> off_fn:(bytes -> int -> int) -> bytes -> int -> int =
 fun ctx typ ~off_fn ->
  match typ with
  | All_bytes | All_zeros ->
      (* Trailing "rest of buffer": size is whatever bytes remain past the
         current offset. The OCaml decoder gets the buffer length via
         [Bytes.length buf]; the 3D projection emits [all_bytes], which
         3D handles natively. *)
      fun buf base -> Bytes.length buf - (base + off_fn buf base)
  | Casetype _ ->
      (* Tag is decoded first; the case body's size is whatever the
         selected case's inner typ reports. *)
      fun buf base -> elem_size_of typ buf (base + off_fn buf base)
  | _ ->
      let size_expr =
        match typ with
        | Byte_slice { size } -> size
        | Byte_array { size } -> size
        | Byte_array_where { size; _ } -> size
        | Uint_var { size; _ } -> size
        | Single_elem { size; _ } -> size
        | _ -> invalid_arg "add_field: unsupported variable-size field type"
      in
      let sizeof_this : bytes -> int -> int =
        match ctx.lc_next_off with
        | Static n -> fun _buf _base -> n
        | Dynamic prev_end -> fun buf base -> prev_end buf base - base
      in
      compile_expr ~sizeof_this ctx.lc_field_readers size_expr

and compile_var_bytes : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let off_fn, validator_off =
    match ctx.lc_next_off with
    | Static n -> ((fun (_buf : bytes) (_base : int) -> n), n)
    | Dynamic prev_end -> ((fun buf base -> prev_end buf base - base), -1)
  in
  let size_fn = compile_var_size_fn ctx typ ~off_fn in
  let field_access : field_access =
    match ctx.lc_next_off with
    | Static n -> Variable { off = n; size_fn }
    | Dynamic _ -> Variable_dynamic { off_fn; size_fn }
  in
  let raw_reader, raw_writer, int_reader =
    match typ with
    | Uint_var { endian; _ } ->
        let get = fld.get in
        let raw_reader : bytes -> int -> a =
         fun buf base ->
          let fo = off_fn buf base in
          let sz = size_fn buf base in
          Uint_var.read endian buf (base + fo) sz
        in
        let raw_writer : r -> bytes -> int -> unit =
         fun v buf off ->
          let fo = off_fn buf off in
          let sz = size_fn buf off in
          Uint_var.write endian buf (off + fo) sz (get v)
        in
        let int_reader : bytes -> int -> int = raw_reader in
        (raw_reader, raw_writer, int_reader)
    | _ ->
        let raw_reader = var_bytes_reader typ off_fn size_fn in
        let raw_writer = var_bytes_writer typ fld.get off_fn in
        let int_reader buf base =
          match int_of_typ_value typ (raw_reader buf base) with
          | Some v -> v
          | None -> 0
        in
        (raw_reader, raw_writer, int_reader)
  in
  let populate = build_populate typ ctx.lc_n_fields raw_reader in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic
        (fun buf base ->
          let fo = off_fn buf base in
          base + fo + size_fn buf base);
    bf_after = None;
    int_reader;
    nested_readers = [];
    validator_off;
    populate;
  }

(* -- Apply a compiled plan to the record state -- *)

(* The single place that mutates the record accumulator: assemble constraint
   and action checkers, then fold the [compiled_field] into a new record
   state. *)
let apply_compiled : type a f r.
    (a -> f, r) record -> (a, r) field -> (a, r) compiled_field -> (f, r) record
    =
 fun (Record r) fld cf ->
  let action_var_names =
    match fld.action with
    | None -> []
    | Some (Types.On_success stmts | Types.On_act stmts) ->
        List.fold_left action_vars [] stmts
  in
  let n_extra_vars = List.length action_var_names in
  let field_idx = r.r_n_fields in
  let dummy_reader _buf _base = 0 in
  let cc_readers =
    let base = (fld.name, dummy_reader) :: r.r_field_readers in
    List.fold_left
      (fun acc vn -> (vn, dummy_reader) :: acc)
      base action_var_names
  in
  let idx = build_idx cc_readers in
  let cc = { idx; sizeof_this = cf.validator_off; field_pos = field_idx } in
  let check =
    match fld.constraint_ with
    | None -> None
    | Some c -> Some (compile_bool_arr cc c)
  in
  let act = compile_action cc fld.action in
  let populate = cf.populate in
  let full arr buf base =
    populate arr buf base;
    (match check with
    | Some f when not (f arr) ->
        raise (Parse_error (Constraint_failed "field constraint"))
    | _ -> ());
    match act with Some f -> f arr | None -> ()
  in
  let check_only arr buf base =
    populate arr buf base;
    match check with
    | Some f when not (f arr) ->
        raise (Parse_error (Constraint_failed "field constraint"))
    | _ -> ()
  in
  let faction =
    match act with None -> None | Some act_fn -> Some (fld.name, act_fn)
  in
  let byte_off = cf.validator_off in
  let new_writers_rev = (cf.raw_writer :: cf.extra_writers) @ r.r_writers_rev in
  let new_field_readers =
    cf.nested_readers @ ((fld.name, cf.int_reader) :: r.r_field_readers)
  in
  Record
    {
      r_name = r.r_name;
      r_make = r.r_make;
      r_readers = Snoc (r.r_readers, cf.raw_reader);
      r_writers_rev = new_writers_rev;
      r_min_wire_size = r.r_min_wire_size + cf.size_delta;
      r_next_off = cf.next_off;
      r_fields_rev = struct_field fld :: r.r_fields_rev;
      r_validators_rev = (byte_off, full) :: r.r_validators_rev;
      r_checkers_rev = (byte_off, check_only) :: r.r_checkers_rev;
      r_field_actions_rev =
        (match faction with
        | Some fa -> fa :: r.r_field_actions_rev
        | None -> r.r_field_actions_rev);
      r_n_fields = List.length new_field_readers;
      r_n_array_slots = List.length new_field_readers + n_extra_vars;
      r_bf = cf.bf_after;
      r_field_access_rev = (fld.name, cf.field_access) :: r.r_field_access_rev;
      r_field_readers = new_field_readers;
      r_where = r.r_where;
    }

let add_field : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record
    =
 fun (Record _ as record) fld ->
  let ctx = layout_ctx_of record in
  let cf = compile_field ctx fld in
  apply_compiled record fld cf

(* Forward reader list: cons-list dual of [readers] snoc-list.
   Built once at seal time by [to_fwd]; applied at decode time by
   [apply_fwd] which unrolls 8 fields per step (1 partial application
   per 8 fields, e.g. 16 fields = 8 + merge + 8 = 1 partial). *)
type (_, _) readers_fwd =
  | FNil : ('r, 'r) readers_fwd
  | FCons :
      (bytes -> int -> 'a) * ('rest, 'result) readers_fwd
      -> ('a -> 'rest, 'result) readers_fwd

(* Convert a [readers] snoc-list to a [readers_fwd] cons-list. O(n), runs
   once at seal time. *)
let to_fwd : type full result.
    (full, result) readers -> (full, result) readers_fwd =
 fun readers ->
  let rec go : type mid.
      (full, mid) readers ->
      (mid, result) readers_fwd ->
      (full, result) readers_fwd =
   fun readers acc ->
    match readers with
    | Nil -> acc
    | Snoc (rest, reader) -> go rest (FCons (reader, acc))
  in
  go readers FNil

(* Apply a forward reader list to [make]. Unrolled up to 6 fields per step;
   beyond that, one partial application per 8 fields. *)
let rec apply_fwd : type mid result.
    mid -> (mid, result) readers_fwd -> bytes -> int -> result =
 fun f fwd buf off ->
  match fwd with
  | FNil -> f
  | FCons (r1, FNil) -> f (r1 buf off)
  | FCons (r1, FCons (r2, FNil)) -> f (r1 buf off) (r2 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FNil))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FNil)))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FNil))))) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
  | FCons (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FNil))))))
    ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
        (r6 buf off)
  | FCons
      ( r1,
        FCons
          (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FNil))))))
      ) ->
      f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
        (r6 buf off) (r7 buf off)
  | FCons
      ( r1,
        FCons
          ( r2,
            FCons
              ( r3,
                FCons (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, rest)))))
              ) ) ) ->
      apply_fwd
        (f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
           (r6 buf off) (r7 buf off) (r8 buf off))
        rest buf off

let build_decode : type full r. full -> (full, r) readers -> bytes -> int -> r =
 fun make readers ->
  match readers with
  | Nil -> fun _buf _off -> make
  | Snoc (Nil, r1) -> fun buf off -> make (r1 buf off)
  | Snoc (Snoc (Nil, r1), r2) -> fun buf off -> make (r1 buf off) (r2 buf off)
  | Snoc (Snoc (Snoc (Nil, r1), r2), r3) ->
      fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off)
  | Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4) ->
      fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
  | Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
  | Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off)
  | Snoc
      (Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6), r7)
    ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off) (r7 buf off)
  | Snoc
      ( Snoc
          ( Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
            r7 ),
        r8 ) ->
      fun buf off ->
        make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off) (r7 buf off) (r8 buf off)
  | readers ->
      let fwd = to_fwd readers in
      fun buf off -> apply_fwd make fwd buf off

let compile_where_clause field_readers where =
  match where with
  | None -> None
  | Some cond ->
      let rev_readers = List.rev field_readers in
      let idx name =
        let rec find i = function
          | [] -> failwith ("unbound field: " ^ name)
          | (n, _) :: _ when n = name -> i
          | _ :: rest -> find (i + 1) rest
        in
        find 0 rev_readers
      in
      let cc = { idx; sizeof_this = 0; field_pos = 0 } in
      Some (compile_bool_arr cc cond)

let build_validators validators_rev checkers_rev compiled_where struct_fields
    n_total =
  let validator_fns = Array.of_list (List.map snd validators_rev) in
  let n_validators = Array.length validator_fns in
  let validate_arr arr buf off =
    for i = 0 to n_validators - 1 do
      validator_fns.(i) arr buf off
    done;
    match compiled_where with
    | Some f when not (f arr) ->
        raise (Parse_error (Constraint_failed "where clause"))
    | _ -> ()
  in
  let checker_fns = Array.of_list (List.map snd checkers_rev) in
  let n_checkers = Array.length checker_fns in
  let populate arr buf off =
    for i = 0 to n_checkers - 1 do
      checker_fns.(i) arr buf off
    done
  in
  let has_checks =
    compiled_where <> None
    || List.exists (fun (Types.Field f) -> f.constraint_ <> None) struct_fields
  in
  let validate =
    if has_checks then (
      let arr = Array.make n_total 0 in
      fun buf off ->
        Array.fill arr 0 n_total 0;
        populate arr buf off;
        match compiled_where with
        | Some f when not (f arr) ->
            raise (Parse_error (Constraint_failed "where clause"))
        | _ -> ())
    else fun _buf _off -> ()
  in
  (validate_arr, populate, validate)

let collect_param_handles struct_fields where =
  let seen = Hashtbl.create 4 in
  let handles = Stdlib.ref ([] : Param.packed list) in
  let visit (Param.Pack p as packed) =
    if not (Hashtbl.mem seen p.ph_name) then begin
      Hashtbl.add seen p.ph_name ();
      handles := packed :: !handles
    end
  in
  iter_param_refs_fields visit struct_fields where;
  List.rev !handles

let build_checked_decode raw_decode wire_size_info min_size =
 fun buf off ->
  if off + min_size > Bytes.length buf then
    raise_eof ~expected:min_size ~got:(Bytes.length buf - off);
  (match wire_size_info with
  | Fixed _ -> ()
  | Variable { compute; _ } ->
      let end_off =
        try compute buf off
        with Invalid_argument _ ->
          raise_eof ~expected:min_size ~got:(Bytes.length buf - off)
      in
      if end_off < off + min_size then
        raise_eof ~expected:min_size ~got:(Bytes.length buf - off);
      if end_off > Bytes.length buf then
        raise_eof ~expected:(end_off - off) ~got:(Bytes.length buf - off));
  raw_decode buf off

let seal : type r. (r, r) record -> r t =
 fun (Record r) ->
  let codec_id = Atomic.fetch_and_add id_counter 1 in
  let field_access = List.rev r.r_field_access_rev in
  let wire_size_info =
    match r.r_next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = r.r_min_wire_size; compute = f }
  in
  let min_size = r.r_min_wire_size in
  let writers = Array.of_list (List.rev r.r_writers_rev) in
  let n_writers = Array.length writers in
  let raw_decode = build_decode r.r_make r.r_readers in
  let validators = List.rev r.r_validators_rev in
  let param_base = r.r_n_array_slots in
  (* Collect and index params *)
  let struct_fields = List.rev r.r_fields_rev in
  let param_handles = collect_param_handles struct_fields r.r_where in
  let n_params = List.length param_handles in
  List.iteri
    (fun i (Param.Pack p) ->
      p.ph_slot <- param_base + i;
      p.ph_env_idx <- i)
    param_handles;
  let n_total = param_base + n_params in
  let compiled_where = compile_where_clause r.r_field_readers r.r_where in
  let validate_arr, populate, validate =
    build_validators validators
      (List.rev r.r_checkers_rev)
      compiled_where struct_fields n_total
  in
  (* Per-field action runners *)
  let field_actions = List.rev r.r_field_actions_rev in
  {
    t_id = codec_id;
    t_name = r.r_name;
    t_field_access = field_access;
    t_field_readers = List.rev r.r_field_readers;
    t_field_actions = field_actions;
    t_decode = build_checked_decode raw_decode wire_size_info min_size;
    t_encode =
      (fun v buf off ->
        if off + min_size > Bytes.length buf then
          Fmt.invalid_arg "Codec.encode %s: buffer too short (need %d, got %d)"
            r.r_name min_size
            (Bytes.length buf - off);
        for i = 0 to n_writers - 1 do
          writers.(i) v buf off
        done);
    t_wire_size = wire_size_info;
    t_struct_fields = struct_fields;
    t_validate = validate;
    t_validate_arr = validate_arr;
    t_populate = populate;
    t_n_array_slots = n_total;
    t_param_base = param_base;
    t_n_params = n_params;
    t_param_handles = param_handles;
    t_where = r.r_where;
  }

(* -- Validator-only path: build a struct validator from [Types.struct_]
      without requiring a record constructor. Reuses the same int-array
      validation kernel ([compile_field], [build_validators]) that
      [Codec.v]/[Codec.decode] uses; only the writer/reader projections
      are skipped. Lets [Wire.of_string]/[Wire.decode] for [Struct]
      types share one code path with [Codec.decode]. *)

type validator = {
  vt_min_size : int;
  vt_wire_size : wire_size_info;
  vt_validate : bytes -> int -> unit;
}

(* Internal accumulator -- the validator-relevant subset of [record]. *)
type validator_acc = {
  va_validators_rev : (int * (int array -> bytes -> int -> unit)) list;
  va_checkers_rev : (int * (int array -> bytes -> int -> unit)) list;
  va_field_readers : field_reader list;
  va_n_fields : int;
  va_n_array_slots : int;
  va_min_size : int;
  va_next_off : next_off;
  va_bf : bf_codec_state option;
}

let empty_validator_acc =
  {
    va_validators_rev = [];
    va_checkers_rev = [];
    va_field_readers = [];
    va_n_fields = 0;
    va_n_array_slots = 0;
    va_min_size = 0;
    va_next_off = Static 0;
    va_bf = None;
  }

let layout_ctx_of_validator_acc acc =
  {
    lc_next_off = acc.va_next_off;
    lc_bf = acc.va_bf;
    lc_field_readers = acc.va_field_readers;
    lc_n_fields = acc.va_n_fields;
  }

(* Per-field step: open [Types.Field]'s existential ['a], build a fake
   [('a, unit) field] with an [assert false] projection (never invoked on
   the validator path), call [compile_field], and accumulate the
   validator-relevant pieces of the resulting [compiled_field].

   [Struct]-typed fields are handled specially -- [compile_field] does
   not support them (a struct has no [field_wire_size] without its inner
   fields being walked). For nested structs we build a sub-validator
   recursively and inline its [vt_validate] at the right offset. Inner
   field references stay scoped to the inner struct, matching the old
   [parse_struct_fields] semantics. *)
(* Common access patterns over [va_next_off]: a fresh-buffer offset
   function and a "previous end" helper that hides the static/dynamic
   split. *)
let acc_off_fn (acc : validator_acc) : bytes -> int -> int =
  match acc.va_next_off with
  | Static n -> fun _buf _base -> n
  | Dynamic f -> fun buf base -> f buf base - base

let acc_prev_end (acc : validator_acc) : bytes -> int -> int =
  match acc.va_next_off with
  | Static n -> fun _buf base -> base + n
  | Dynamic f -> f

(* Validator step for a [Struct]-typed field. [compile_field] doesn't
   accept [Struct] (it has no [field_wire_size] without walking inner
   fields), so build a sub-validator recursively and inline its
   [vt_validate] at the right offset. *)
let rec apply_struct_field acc inner_struct =
  let inner_v = validator_of_struct inner_struct in
  let static_off =
    match acc.va_next_off with Static n -> n | Dynamic _ -> -1
  in
  let off_fn = acc_off_fn acc in
  let validator _arr buf base =
    inner_v.vt_validate buf (base + off_fn buf base)
  in
  let prev_end = acc_prev_end acc in
  let size_delta, next_off =
    match (acc.va_next_off, inner_v.vt_wire_size) with
    | Static n, Fixed sz -> (sz, Static (n + sz))
    | _, Fixed sz -> (sz, Dynamic (fun buf base -> prev_end buf base + sz))
    | _, Variable { min_size; compute } ->
        (min_size, Dynamic (fun buf base -> compute buf (prev_end buf base)))
  in
  {
    va_validators_rev = (static_off, validator) :: acc.va_validators_rev;
    va_checkers_rev = (static_off, validator) :: acc.va_checkers_rev;
    va_field_readers = acc.va_field_readers;
    va_n_fields = acc.va_n_fields;
    va_n_array_slots = acc.va_n_array_slots;
    va_min_size = acc.va_min_size + size_delta;
    va_next_off = next_off;
    va_bf = None;
  }

(* Build the [full] / [check_only] per-field validator functions and
   return the action var count. Takes only the non-existential fields
   of [compiled_field] ([populate], [validator_off]) so the caller can
   open the [Types.Field] existential, call [compile_field], and pass
   the relevant pieces here without leaking the field's ['a]. *)
and build_field_checks acc ~populate ~validator_off ~name ~action ~constraint_ =
  let action_var_names =
    match action with
    | None -> []
    | Some (Types.On_success stmts | Types.On_act stmts) ->
        List.fold_left action_vars [] stmts
  in
  let dummy_reader _buf _base = 0 in
  let cc_readers =
    let base = (name, dummy_reader) :: acc.va_field_readers in
    List.fold_left
      (fun acc' vn -> (vn, dummy_reader) :: acc')
      base action_var_names
  in
  let idx = build_idx cc_readers in
  let cc = { idx; sizeof_this = validator_off; field_pos = acc.va_n_fields } in
  let check = Option.map (compile_bool_arr cc) constraint_ in
  let act = compile_action cc action in
  let raise_check_failed () =
    raise (Parse_error (Constraint_failed "field constraint"))
  in
  let full arr buf base =
    populate arr buf base;
    (match check with
    | Some f when not (f arr) -> raise_check_failed ()
    | _ -> ());
    Option.iter (fun f -> f arr) act
  in
  let check_only arr buf base =
    populate arr buf base;
    match check with
    | Some f when not (f arr) -> raise_check_failed ()
    | _ -> ()
  in
  (full, check_only, List.length action_var_names)

and apply_field_to_validator_acc acc (Types.Field f) =
  match f.field_typ with
  | Types.Struct inner_struct -> apply_struct_field acc inner_struct
  | _ ->
      let name = Option.value f.field_name ~default:"" in
      let codec_field : (_, unit) field =
        {
          name;
          typ = f.field_typ;
          constraint_ = f.constraint_;
          action = f.action;
          get = (fun () -> assert false);
        }
      in
      let layout = layout_ctx_of_validator_acc acc in
      let cf = compile_field layout codec_field in
      let full, check_only, n_extra_vars =
        build_field_checks acc ~populate:cf.populate
          ~validator_off:cf.validator_off ~name ~action:f.action
          ~constraint_:f.constraint_
      in
      let new_field_readers =
        cf.nested_readers @ ((name, cf.int_reader) :: acc.va_field_readers)
      in
      {
        va_validators_rev = (cf.validator_off, full) :: acc.va_validators_rev;
        va_checkers_rev = (cf.validator_off, check_only) :: acc.va_checkers_rev;
        va_field_readers = new_field_readers;
        va_n_fields = List.length new_field_readers;
        va_n_array_slots = List.length new_field_readers + n_extra_vars;
        va_min_size = acc.va_min_size + cf.size_delta;
        va_next_off = cf.next_off;
        va_bf = cf.bf_after;
      }

and validator_of_struct (s : Types.struct_) : validator =
  let acc =
    List.fold_left apply_field_to_validator_acc empty_validator_acc s.fields
  in
  let wire_size_info =
    match acc.va_next_off with
    | Static n -> Fixed n
    | Dynamic f -> Variable { min_size = acc.va_min_size; compute = f }
  in
  let param_handles = collect_param_handles s.fields s.where in
  let n_params = List.length param_handles in
  let param_base = acc.va_n_array_slots in
  List.iteri
    (fun i (Param.Pack p) ->
      p.ph_slot <- param_base + i;
      p.ph_env_idx <- i)
    param_handles;
  let n_total = param_base + n_params in
  let compiled_where = compile_where_clause acc.va_field_readers s.where in
  let validate_arr, _populate, _validate =
    build_validators
      (List.rev acc.va_validators_rev)
      (List.rev acc.va_checkers_rev)
      compiled_where s.fields n_total
  in
  (* Full validation: fire field actions and check the where clause.
     [build_validators]'s third return [validate] is checkers-only (no
     actions). [validate_arr] runs full validators -- pre-allocate one
     scratch [int array], zero it on entry. *)
  let arr = Array.make n_total 0 in
  let validate buf off =
    if n_total > 0 then Array.fill arr 0 n_total 0;
    validate_arr arr buf off
  in
  {
    vt_min_size = acc.va_min_size;
    vt_wire_size = wire_size_info;
    vt_validate = validate;
  }

let validate_struct v buf off = v.vt_validate buf off

let struct_size_of v buf off =
  match v.vt_wire_size with
  | Fixed n -> n
  | Variable { compute; _ } -> compute buf off - off

let struct_min_size v = v.vt_min_size

let wire_size_info_of_validator v =
  match v.vt_wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } -> `Variable compute

(* Heterogeneous field list. [] seals the view; (::) adds a field.
   Tracks the constructor type: ('a -> 'b -> 'r, 'r) matches a
   constructor (fun a b -> ...). *)
type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

let view : type f r. string -> ?where:bool expr -> f -> (f, r) fields -> r t =
 fun name ?where constructor flds ->
  let rec add : type g. (g, r) record -> (g, r) fields -> r t =
   fun r flds ->
    match flds with [] -> seal r | f :: rest -> add (add_field r f) rest
  in
  add (record_start ?where name constructor) flds

let v = view

let wire_size t =
  match t.t_wire_size with
  | Fixed n -> n
  | Variable _ ->
      invalid_arg
        "Codec.wire_size: variable-size codec (use min_wire_size or \
         compute_wire_size instead)"

let min_wire_size t =
  match t.t_wire_size with Fixed n -> n | Variable { min_size; _ } -> min_size

let wire_size_at t buf off =
  match t.t_wire_size with
  | Fixed n -> n
  | Variable { compute; _ } -> compute buf off - off

let is_fixed t =
  match t.t_wire_size with Fixed _ -> true | Variable _ -> false

let raw_decode t buf off = t.t_decode buf off
let raw_encode t v buf off = t.t_encode v buf off

let wire_size_info t =
  match t.t_wire_size with
  | Fixed n -> `Fixed n
  | Variable { compute; _ } -> `Variable (fun buf off -> compute buf off - off)

let env t : Param.env =
  { Types.pe_codec_id = t.t_id; pe_slots = Array.make t.t_n_params 0 }

let decode_exn ?env:e t buf off =
  let v = t.t_decode buf off in
  let arr = Array.make t.t_n_array_slots 0 in
  (match e with
  | Some (e : Param.env) when t.t_n_params > 0 ->
      Array.blit e.pe_slots 0 arr t.t_param_base t.t_n_params
  | _ -> ());
  t.t_validate_arr arr buf off;
  (match e with
  | Some (e : Param.env) ->
      List.iter
        (fun (Param.Pack p) ->
          let v = arr.(p.ph_slot) in
          e.pe_slots.(p.ph_env_idx) <- v;
          p.ph_cell := v)
        t.t_param_handles
  | None -> ());
  v

let decode ?env t buf off =
  try Ok (decode_exn ?env t buf off) with Types.Parse_error e -> Error e

let encode t v buf off = t.t_encode v buf off

let collect_params (fields : Types.field list) where =
  let seen = Hashtbl.create 4 in
  let params = Stdlib.ref ([] : param list) in
  let visit (Param.Pack p) =
    if not (Hashtbl.mem seen p.ph_name) then begin
      Hashtbl.add seen p.ph_name ();
      params :=
        {
          param_name = p.ph_name;
          param_typ = p.ph_packed_typ;
          mutable_ = p.ph_mutable;
        }
        :: !params
    end
  in
  iter_param_refs_fields visit fields where;
  List.rev !params

let to_struct t =
  let formals = collect_params t.t_struct_fields t.t_where in
  match (formals, t.t_where) with
  | [], None -> struct' t.t_name t.t_struct_fields
  | _ -> param_struct t.t_name formals ?where:t.t_where t.t_struct_fields

let validate t buf off = t.t_validate buf off

(* Build a staged reader from field type and access info.
   For Fixed: use build_field_reader which handles Where/Enum/Map.
   For Bitfield: the GADT ensures 'a = int via Bits constructor.
   For Dynamic: compute offset at read time. *)
let rec build_staged_reader : type a. a typ -> field_access -> bytes -> int -> a
    =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      build_bf_reader base byte_off shift width
  | _, Fixed off -> build_field_reader typ off
  | _, Dynamic fn ->
      let reader_at_0 = build_field_reader typ 0 in
      fun buf base ->
        let off = fn buf base in
        reader_at_0 buf (base + off)
  | Byte_slice _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Slice.make_or_eod buf ~first:(base + off) ~length:sz
  | Byte_array _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + off) sz
  | Byte_array_where _, Variable { off; size_fn } ->
      fun buf base ->
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + off) sz
  | Byte_slice _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + fo) sz
  | Byte_array_where _, Variable_dynamic { off_fn; size_fn } ->
      fun buf base ->
        let fo = off_fn buf base in
        let sz = size_fn buf base in
        Bytes.sub_string buf (base + fo) sz
  | Where { inner; _ }, _ -> build_staged_reader inner access
  | Enum { base; _ }, _ -> build_staged_reader base access
  | Map { inner; decode; _ }, _ ->
      let read = build_staged_reader inner access in
      fun buf base -> decode (read buf base)
  | _, Bitfield _ ->
      invalid_arg "Codec.get: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.get: unsupported variable-size field type"

(* Build a staged writer from field type and access info. *)
let rec build_staged_writer : type a.
    a typ -> field_access -> bytes -> int -> a -> unit =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      build_bf_accessor_writer base byte_off shift width
  | _, Fixed off ->
      let enc = build_field_encoder typ in
      fun buf base value ->
        let _ = enc buf (base + off) value in
        ()
  | _, Dynamic fn ->
      let enc = build_field_encoder typ in
      fun buf base value ->
        let off = fn buf base in
        let _ = enc buf (base + off) value in
        ()
  | Byte_slice _, Variable { off; _ } ->
      fun buf base value ->
        let src = (value : Slice.t) in
        let len = Slice.length src in
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + off) len
  | Byte_array _, Variable { off; _ } ->
      fun buf base value ->
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + off) (String.length s)
  | Byte_array_where _, Variable { off; _ } ->
      fun buf base value ->
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + off) (String.length s)
  | Byte_slice _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let src = (value : Slice.t) in
        let len = Slice.length src in
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + fo) len
  | Byte_array _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + fo) (String.length s)
  | Byte_array_where _, Variable_dynamic { off_fn; _ } ->
      fun buf base value ->
        let fo = off_fn buf base in
        let s = (value : string) in
        Bytes.blit_string s 0 buf (base + fo) (String.length s)
  | Where { inner; _ }, _ -> build_staged_writer inner access
  | Enum { base; _ }, _ -> build_staged_writer base access
  | Map { inner; encode; _ }, _ ->
      let write = build_staged_writer inner access in
      fun buf base value -> write buf base (encode value)
  | _, Bitfield _ ->
      invalid_arg "Codec.set: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.set: unsupported variable-size field type"

let field_access codec name =
  match List.assoc_opt name codec.t_field_access with
  | Some a -> a
  | None ->
      Fmt.invalid_arg "Codec: field %S not found in codec %S" name codec.t_name

let[@inline] get (type a r) ?env (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a) Staged.t =
  let access = field_access codec f.name in
  let read = build_staged_reader f.typ access in
  match List.assoc_opt f.name codec.t_field_actions with
  | None -> Staged.stage read
  | Some act ->
      let arr = Array.make codec.t_n_array_slots 0 in
      let n = Array.length arr in
      let populate = codec.t_populate in
      let n_params = codec.t_n_params in
      let sync, blit_input =
        match env with
        | None -> ((fun _arr -> ()), fun _arr -> ())
        | Some (e : Param.env) ->
            if e.pe_codec_id <> codec.t_id then
              Fmt.invalid_arg
                "Codec.get: env was not created by Codec.env for %S"
                codec.t_name;
            let param_handles = codec.t_param_handles in
            let param_base = codec.t_param_base in
            ( (fun arr ->
                List.iter
                  (fun (Param.Pack p) ->
                    let v = arr.(p.ph_slot) in
                    e.pe_slots.(p.ph_env_idx) <- v;
                    p.ph_cell := v)
                  param_handles),
              fun arr ->
                if n_params > 0 then
                  Array.blit e.pe_slots 0 arr param_base n_params )
      in
      Staged.stage (fun buf off ->
          let v = read buf off in
          Array.fill arr 0 n 0;
          blit_input arr;
          populate arr buf off;
          act arr;
          sync arr;
          v)

let[@inline] set (type a r) (codec : r t) (f : (a, r) field) :
    (bytes -> int -> a -> unit) Staged.t =
  let access = field_access codec f.name in
  Staged.stage (build_staged_writer f.typ access)

let name t = t.t_name
let field_readers t = t.t_field_readers
let pp ppf t = Fmt.string ppf t.t_name
let field_ref (type a r) (f : (a, r) field) : int expr = Ref f.name

(* -- Slice navigation: zero-copy access to the offset/length of a
      [Byte_slice] field. Used to descend into a nested codec without
      allocating a [Slice.t]. The type signature constrains the field's
      payload type to [Slice.t], so passing a non-slice field is a
      compile-time error. -- *)

(* All access flavours produce an absolute byte offset for a slice field:
   - [Fixed off]: byte_slice with static size (and no preceding variable
     fields) -- offset is [base + off].
   - [Dynamic fn]: byte_slice with static size after a variable field --
     offset is [base + fn buf base].
   - [Variable { off; _ }]: byte_slice with variable size, statically
     positioned -- offset is [base + off].
   - [Variable_dynamic { off_fn; _ }]: variable size, dynamic position. *)
let[@inline] slice_offset (type r) (codec : r t) (f : (Slice.t, r) field) :
    (bytes -> int -> int) Staged.t =
  match field_access codec f.name with
  | Fixed off -> Staged.stage (fun _buf base -> base + off)
  | Dynamic fn -> Staged.stage (fun buf base -> base + fn buf base)
  | Variable { off; _ } -> Staged.stage (fun _buf base -> base + off)
  | Variable_dynamic { off_fn; _ } ->
      Staged.stage (fun buf base -> base + off_fn buf base)
  | Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_offset: field %S is a bitfield, not a byte slice" f.name

let[@inline] slice_length (type r) (codec : r t) (f : (Slice.t, r) field) :
    (bytes -> int -> int) Staged.t =
  match (f.typ, field_access codec f.name) with
  | Byte_slice { size }, Fixed _ | Byte_slice { size }, Dynamic _ -> (
      (* Static-size byte_slice: size is a constant expression. *)
      match size with
      | Int n -> Staged.stage (fun _buf _base -> n)
      | _ ->
          Fmt.invalid_arg
            "Codec.slice_length: field %S has dynamic size in a static access \
             -- internal inconsistency"
            f.name)
  | _, Variable { size_fn; _ } | _, Variable_dynamic { size_fn; _ } ->
      Staged.stage size_fn
  | _, (Fixed _ | Dynamic _) ->
      Fmt.invalid_arg "Codec.slice_length: field %S is not a byte slice" f.name
  | _, Bitfield _ ->
      Fmt.invalid_arg
        "Codec.slice_length: field %S is a bitfield, not a byte slice" f.name

(* -- Bitfield batch access -- *)

type bitfield = bf_info

let bitfield (type r) (codec : r t) (f : (int, r) field) : bitfield =
  match field_access codec f.name with
  | Bitfield { base; byte_off; shift; width } ->
      (* Dispatch on [base] once at construction so the resulting closure
         is a direct read of the right width with [byte_off] baked in.
         Avoids the partial-application + runtime [match] that
         [Bitfield.read_word base] would create. *)
      let word_reader =
        match base with
        | BF_U8 -> fun buf off -> Bytes.get_uint8 buf (off + byte_off)
        | BF_U16 Little -> fun buf off -> Bitfield.u16_le buf (off + byte_off)
        | BF_U16 Big -> fun buf off -> Bitfield.u16_be buf (off + byte_off)
        | BF_U32 Little -> fun buf off -> Bitfield.u32_le buf (off + byte_off)
        | BF_U32 Big -> fun buf off -> Bitfield.u32_be buf (off + byte_off)
      in
      let mask = (1 lsl width) - 1 in
      { bf_word_reader = word_reader; bf_packed = shift lor (mask lsl 8) }
  | _ -> Fmt.invalid_arg "Codec.bitfield: field %S is not a bitfield" f.name

let load_word (bf : bitfield) : (bytes -> int -> int) Staged.t =
  Staged.stage bf.bf_word_reader

let[@inline always] extract (bf : bitfield) word =
  let p = bf.bf_packed in
  (word lsr (p land 0xFF)) land (p lsr 8)

(* -- Snapshot: batch bitfield access -- *)
