open Types
module Slice = Bytesrw.Bytes.Slice
module Param = Param

(** Build a specialized field encoder: writes field value to bytes at offset.
    Returns the new offset. Works directly on the slice's underlying bytes. *)
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
  | Byte_array { size = Int n } ->
      fun buf off v ->
        let len = min n (String.length v) in
        Bytes.blit_string v 0 buf off len;
        if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
        off + n
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
  | Byte_array { size = Int n } ->
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

(* Type equality witness for GADT-safe accessor setting *)
type (_, _) eq = Refl : ('a, 'a) eq

(* Bitfield extraction descriptor: word reader + packed shift/mask.
   Packing shift and mask into a single int lets [extract] be a direct
   [@inline always] function instead of an indirect closure call. *)
type bf_info = {
  bf_word_reader : bytes -> int -> int;
  bf_packed : int; (* shift in bits 0-7, mask in bits 8+ *)
}

type ('a, 'r) field = {
  name : string;
  typ : 'a typ;
  constraint_ : bool expr option;
  action : action option;
  get : 'r -> 'a;
  mutable f_reader : bytes -> int -> 'a;
  mutable f_writer : bytes -> int -> 'a -> unit;
  mutable f_bf : bf_info option;
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
  bfc_base_off : int; (* byte offset of base word within record *)
  bfc_bits_used : int; (* bits consumed so far in current group *)
  bfc_total_bits : int; (* 8, 16, or 32 *)
}

(* Track the byte offset for the next field: static (constant) until we hit
   a variable-size field, then dynamic (computed from the buffer). *)
type next_off = Static_next of int | Dynamic_next of (bytes -> int -> int)
(* [compute buf base] returns the absolute byte offset where the next
         field starts. [base] is the record's base offset in [buf]. *)

(* Compile an [int expr] into a closure that evaluates it at runtime by
   reading previously-declared fields from the buffer. Built once at [add_field]
   time, called at every [get]/[set]/[decode]. *)
let rec compile_expr (env : (string * (bytes -> int -> int)) list)
    (e : int expr) : bytes -> int -> int =
  match e with
  | Int n -> fun _buf _base -> n
  | Ref name -> (
      match List.assoc_opt name env with
      | Some reader -> reader
      | None ->
          invalid_arg
            (Fmt.str "Codec: unbound field ref %S in size expression" name))
  | Add (a, b) ->
      let fa = compile_expr env a in
      let fb = compile_expr env b in
      fun buf base -> fa buf base + fb buf base
  | Sub (a, b) ->
      let fa = compile_expr env a in
      let fb = compile_expr env b in
      fun buf base -> fa buf base - fb buf base
  | Mul (a, b) ->
      let fa = compile_expr env a in
      let fb = compile_expr env b in
      fun buf base -> fa buf base * fb buf base
  | Div (a, b) ->
      let fa = compile_expr env a in
      let fb = compile_expr env b in
      fun buf base -> fa buf base / fb buf base
  | Sizeof t -> (
      match field_wire_size t with
      | Some n -> fun _buf _base -> n
      | None -> invalid_arg "Codec: sizeof on variable-size type")
  | _ -> invalid_arg "Codec: unsupported expression in dependent size"

(* Compile expressions to read from a per-decode int array instead of a Map.
   The [idx] function maps field names to array indices. Built at [seal] time,
   used at every [decode]. Zero allocation per decode. *)
type idx = string -> int

(* Per-field compile context: field name→index mapping plus sizeof_this
   and field_pos constants (known at seal time for each field). *)
type compile_ctx = { idx : idx; sizeof_this : int; field_pos : int }

let rec compile_int_arr (cc : compile_ctx) (e : int expr) : int array -> int =
  match e with
  | Int n -> fun _ -> n
  | Ref name ->
      let i = cc.idx name in
      fun a -> a.(i)
  | Param_ref p ->
      fun a ->
        let i = p.ph_slot in
        if i >= 0 then a.(i) else !(p.ph_cell)
  | Sizeof t ->
      let n = field_wire_size t |> Option.value ~default:0 in
      fun _ -> n
  | Sizeof_this ->
      let n = cc.sizeof_this in
      fun _ -> n
  | Field_pos ->
      let n = cc.field_pos in
      fun _ -> n
  | Add (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a + fb a
  | Sub (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a - fb a
  | Mul (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a * fb a
  | Div (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a / fb a
  | Mod (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a mod fb a
  | Land (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a land fb a
  | Lor (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a lor fb a
  | Lxor (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a lxor fb a
  | Lnot e ->
      let fe = compile_int_arr cc e in
      fun a -> lnot (fe a)
  | Lsl (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a lsl fb a
  | Lsr (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun a -> fa a lsr fb a
  | Cast (w, e) -> (
      let fe = compile_int_arr cc e in
      match w with
      | `U8 -> fun a -> fe a land 0xFF
      | `U16 -> fun a -> fe a land 0xFFFF
      | `U32 -> fun a -> fe a land 0xFFFF_FFFF
      | `U64 -> fun a -> fe a)

(* Try to compile an expression of unknown type as int.
   Returns None if the expression is not an int expr. *)
and try_compile_int : type a. compile_ctx -> a expr -> (int array -> int) option
    =
 fun cc -> function
  | Int _ as e -> Some (compile_int_arr cc e)
  | Ref _ as e -> Some (compile_int_arr cc e)
  | Param_ref _ as e -> Some (compile_int_arr cc e)
  | Sizeof _ as e -> Some (compile_int_arr cc e)
  | Sizeof_this as e -> Some (compile_int_arr cc e)
  | Field_pos as e -> Some (compile_int_arr cc e)
  | Add _ as e -> Some (compile_int_arr cc e)
  | Sub _ as e -> Some (compile_int_arr cc e)
  | Mul _ as e -> Some (compile_int_arr cc e)
  | Div _ as e -> Some (compile_int_arr cc e)
  | Mod _ as e -> Some (compile_int_arr cc e)
  | Land _ as e -> Some (compile_int_arr cc e)
  | Lor _ as e -> Some (compile_int_arr cc e)
  | Lxor _ as e -> Some (compile_int_arr cc e)
  | Lnot _ as e -> Some (compile_int_arr cc e)
  | Lsl _ as e -> Some (compile_int_arr cc e)
  | Lsr _ as e -> Some (compile_int_arr cc e)
  | Cast _ as e -> Some (compile_int_arr cc e)
  | _ -> None

and compile_bool_arr (cc : compile_ctx) (e : bool expr) : int array -> bool =
  match e with
  | Bool b -> fun _ -> b
  | Eq (a, b) -> (
      match (try_compile_int cc a, try_compile_int cc b) with
      | Some fa, Some fb -> fun arr -> fa arr = fb arr
      | _ -> fun _ -> true)
  | Ne (a, b) -> (
      match (try_compile_int cc a, try_compile_int cc b) with
      | Some fa, Some fb -> fun arr -> fa arr <> fb arr
      | _ -> fun _ -> true)
  | Lt (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun arr -> fa arr < fb arr
  | Le (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun arr -> fa arr <= fb arr
  | Gt (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun arr -> fa arr > fb arr
  | Ge (a, b) ->
      let fa = compile_int_arr cc a and fb = compile_int_arr cc b in
      fun arr -> fa arr >= fb arr
  | And (a, b) ->
      let fa = compile_bool_arr cc a and fb = compile_bool_arr cc b in
      fun arr -> fa arr && fb arr
  | Or (a, b) ->
      let fa = compile_bool_arr cc a and fb = compile_bool_arr cc b in
      fun arr -> fa arr || fb arr
  | Not e ->
      let fe = compile_bool_arr cc e in
      fun arr -> not (fe arr)

(* Compile action statements to operate on an int array instead of Eval.ctx.
   Assign writes to ph_cell (mutable param) and updates the array.
   Var binds a local by extending the index — but since we can't grow the
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
  | Var (name, e) -> (
      (* Extend index for subsequent statements — but since Var only affects
         later statements in the same block, we handle it at the block level *)
      let fe = compile_int_arr cc e in
      fun arr ->
        (* Store in array if name is a known field *)
        match cc.idx name with
        | i when i < Array.length arr -> arr.(i) <- fe arr
        | _ -> ()
        | exception Failure _ -> ())

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
          (* sum of all fixed-size fields — minimum buffer size *)
      r_next_off : next_off; (* where the next field starts *)
      r_fields_rev : Types.field list;
      r_validators_rev :
        (int (* byte offset *) * (int array -> bytes -> int -> unit)) list;
      r_n_fields : int; (* count of named fields (for field indexing) *)
      r_n_array_slots : int;
          (* fields + action-local vars (for array allocation) *)
      r_bf : bf_codec_state option;
      r_configurators_rev : (unit -> unit) list;
      r_field_readers : (string * (bytes -> int -> int)) list;
      r_where : bool expr option;
    }
      -> ('f, 'r) record

type wire_size_info =
  | Fixed of int
  | Variable of { min_size : int; compute : bytes -> int -> int }

type 'r t = {
  t_name : string;
  t_decode : bytes -> int -> 'r;
  t_encode : 'r -> bytes -> int -> unit;
  t_wire_size : wire_size_info;
  t_struct_fields : Types.field list;
  t_validate : bytes -> int -> unit;
  t_validate_arr : int array -> bytes -> int -> unit;
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
      r_next_off = Static_next 0;
      r_fields_rev = [];
      r_validators_rev = [];
      r_n_fields = 0;
      r_n_array_slots = 0;
      r_bf = None;
      r_configurators_rev = [];
      r_field_readers = [];
      r_where = where;
    }

let bind (f : 'a Field.t) get =
  let not_ready _ _ = failwith "field: not added to a record yet" in
  {
    name = Field.name f;
    typ = Field.typ f;
    constraint_ = Field.constraint_ f;
    action = Field.action f;
    get;
    f_reader = not_ready;
    f_writer = (fun _ _ _ -> failwith "field: not added to a record yet");
    f_bf = None;
  }

let ( $ ) = bind

(* Bitfield helpers — shared module for base operations, specialized closures
   for performance-critical read/write dispatched at codec construction time. *)

(* Configure a field's reader/writer from a GADT equality witness.
   When eq = Some Refl, a = w so we can assign the raw reader/writer directly.
   Otherwise we wrap through the field's reader/writer adaptor. *)
let configure_field_rw : type a r w.
    (a, r) field ->
    (a, w) eq option ->
    (bytes -> int -> w) ->
    (bytes -> int -> w -> unit) ->
    ((bytes -> int -> w) -> bytes -> int -> a) ->
    ((bytes -> int -> w -> unit) -> bytes -> int -> a -> unit) ->
    unit =
 fun fld eq raw_reader raw_writer wrap_reader wrap_writer ->
  match eq with
  | Some Refl ->
      fld.f_reader <- raw_reader;
      fld.f_writer <- raw_writer
  | None ->
      fld.f_reader <- wrap_reader raw_reader;
      fld.f_writer <- wrap_writer raw_writer

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

let build_bf_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | BF_U8 ->
      fun buf off value ->
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U16 Little ->
      fun buf off value ->
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U16 Big ->
      fun buf off value ->
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U32 Little ->
      fun buf off value ->
        let cur = UInt32.le buf (off + byte_off) in
        UInt32.set_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | BF_U32 Big ->
      fun buf off value ->
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

let add_field : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record
    =
 fun (Record r) ({ name; typ; get; _ } as fld) ->
  let field_idx = r.r_n_fields in
  let action_var_names =
    match fld.action with
    | None -> []
    | Some (Types.On_success stmts | Types.On_act stmts) ->
        List.fold_left action_vars [] stmts
  in
  (* Build compile context including current field and action locals in the index *)
  let build_cc ~byte_off readers =
    let dummy_reader _buf _base = 0 in
    (* Add current field and action-local vars to the index *)
    let readers = (name, dummy_reader) :: readers in
    let readers =
      List.fold_left
        (fun acc vn -> (vn, dummy_reader) :: acc)
        readers action_var_names
    in
    let idx = build_idx readers in
    { idx; sizeof_this = byte_off; field_pos = field_idx }
  in
  let n_extra_vars = List.length action_var_names in
  let build_validator ~byte_off typ reader =
    let cc = build_cc ~byte_off r.r_field_readers in
    let check =
      match fld.constraint_ with
      | None -> None
      | Some c -> Some (compile_bool_arr cc c)
    in
    let act = compile_action cc fld.action in
    let idx = field_idx in
    let v arr buf base =
      (match int_of_typ_value typ (reader buf base) with
      | Some v -> arr.(idx) <- v
      | None -> ());
      (match check with
      | Some f when not (f arr) ->
          raise (Parse_error (Constraint_failed "field constraint"))
      | _ -> ());
      match act with Some f -> f arr | None -> ()
    in
    (byte_off, v)
  in
  let extend ~readers ~writers_rev ~min_wire_size ~next_off ~fields_rev
      ~validators_rev ~bf ~configurators_rev ~field_readers =
    Record
      {
        r_name = r.r_name;
        r_make = r.r_make;
        r_readers = readers;
        r_writers_rev = writers_rev;
        r_min_wire_size = min_wire_size;
        r_next_off = next_off;
        r_fields_rev = fields_rev;
        r_validators_rev = validators_rev;
        r_n_fields = List.length field_readers;
        r_n_array_slots = List.length field_readers + n_extra_vars;
        r_bf = bf;
        r_configurators_rev = configurators_rev;
        r_field_readers = field_readers;
        r_where = r.r_where;
      }
  in
  let rec add : type w.
      w typ ->
      (r -> w) ->
      ((bytes -> int -> w) -> bytes -> int -> a) ->
      ((bytes -> int -> w -> unit) -> bytes -> int -> a -> unit) ->
      (a, w) eq option ->
      (f, r) record =
   fun typ get_wire wrap_reader wrap_writer eq ->
    match typ with
    | Map { inner; decode; encode } ->
        add inner
          (fun v -> encode (get_wire v))
          (fun reader -> wrap_reader (fun buf off -> decode (reader buf off)))
          (fun writer ->
            wrap_writer (fun buf off value -> writer buf off (encode value)))
          None
    | Enum { base; _ } -> add base get_wire wrap_reader wrap_writer eq
    | Bits { width; base } ->
        let total = bf_base_total_bits base in
        let static_off =
          match r.r_next_off with
          | Static_next n -> n
          | Dynamic_next _ ->
              invalid_arg
                "add_field: bitfields after variable-size fields not supported"
        in
        let base_off, bits_used, size_delta, extra_writers =
          match r.r_bf with
          | Some bf
            when bf_base_equal bf.bfc_base base
                 && bf.bfc_bits_used + width <= bf.bfc_total_bits ->
              (bf.bfc_base_off, bf.bfc_bits_used, 0, [])
          | _ ->
              let clear = build_bf_clear base static_off in
              ( static_off,
                0,
                bf_base_byte_size base,
                [ (fun _v buf off -> clear buf off) ] )
        in
        let shift =
          if Bitfield.is_lsb_first base then bits_used
          else total - bits_used - width
        in
        let raw_reader = build_bf_reader base base_off shift width in
        let raw_writer = build_bf_writer base base_off shift width in
        let accessor_writer =
          build_bf_accessor_writer base base_off shift width
        in
        let int_reader buf off = (raw_reader buf off : int) in
        let mask = (1 lsl width) - 1 in
        let word_reader = Bitfield.read_word base in
        let configurator () =
          fld.f_bf <-
            Some
              {
                bf_word_reader =
                  (fun buf off -> word_reader buf (off + base_off));
                bf_packed = shift lor (mask lsl 8);
              };
          configure_field_rw fld eq raw_reader accessor_writer wrap_reader
            wrap_writer
        in
        let new_bf =
          {
            bfc_base = base;
            bfc_base_off = base_off;
            bfc_bits_used = bits_used + width;
            bfc_total_bits = total;
          }
        in
        extend
          ~readers:(Snoc (r.r_readers, wrap_reader raw_reader))
          ~writers_rev:
            ((fun v buf off -> raw_writer buf off (get_wire v))
            :: (extra_writers @ r.r_writers_rev))
          ~min_wire_size:(r.r_min_wire_size + size_delta)
          ~next_off:(Static_next (static_off + size_delta))
          ~fields_rev:(struct_field fld :: r.r_fields_rev)
          ~validators_rev:
            (build_validator ~byte_off:static_off typ raw_reader
            :: r.r_validators_rev)
          ~bf:(Some new_bf)
          ~configurators_rev:(configurator :: r.r_configurators_rev)
          ~field_readers:((name, int_reader) :: r.r_field_readers)
    | _ ->
        let field_off_static =
          match r.r_next_off with
          | Static_next n -> Some n
          | Dynamic_next _ -> None
        in
        let field_off_fn =
          match r.r_next_off with
          | Static_next n -> fun (_buf : bytes) (_base : int) -> n
          | Dynamic_next f -> fun buf base -> f buf base - base
        in
        add_fixed_or_variable typ get_wire wrap_reader wrap_writer
          field_off_static field_off_fn
  and add_fixed_or_variable : type w.
      w typ ->
      (r -> w) ->
      ((bytes -> int -> w) -> bytes -> int -> a) ->
      ((bytes -> int -> w -> unit) -> bytes -> int -> a -> unit) ->
      int option ->
      (bytes -> int -> int) ->
      (f, r) record =
   fun typ get_wire wrap_reader wrap_writer field_off_static field_off_fn ->
    match field_wire_size typ with
    | Some fsize ->
        let field_off =
          match field_off_static with Some n -> n | None -> -1
        in
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
        let configurator () =
          fld.f_reader <- wrap_reader raw_reader;
          fld.f_writer <-
            (match field_off_static with
            | Some fo ->
                wrap_writer (fun buf off value ->
                    let _ = raw_encoder buf (off + fo) value in
                    ())
            | None ->
                wrap_writer (fun buf off value ->
                    let fo = field_off_fn buf off in
                    let _ = raw_encoder buf (off + fo) value in
                    ()))
        in
        let new_next_off =
          match r.r_next_off with
          | Static_next n -> Static_next (n + fsize)
          | Dynamic_next f -> Dynamic_next (fun buf base -> f buf base + fsize)
        in
        let int_reader buf base =
          match int_of_typ_value typ (raw_reader buf base) with
          | Some v -> v
          | None -> 0
        in
        let encode_writer =
          match field_off_static with
          | Some fo ->
              fun v buf off ->
                let _ = raw_encoder buf (off + fo) (get_wire v) in
                ()
          | None ->
              fun v buf off ->
                let fo = field_off_fn buf off in
                let _ = raw_encoder buf (off + fo) (get_wire v) in
                ()
        in
        extend
          ~readers:(Snoc (r.r_readers, wrap_reader raw_reader))
          ~writers_rev:(encode_writer :: r.r_writers_rev)
          ~min_wire_size:(r.r_min_wire_size + fsize)
          ~next_off:new_next_off
          ~fields_rev:(struct_field fld :: r.r_fields_rev)
          ~validators_rev:
            (build_validator ~byte_off:field_off typ raw_reader
            :: r.r_validators_rev)
          ~bf:None
          ~configurators_rev:(configurator :: r.r_configurators_rev)
          ~field_readers:((name, int_reader) :: r.r_field_readers)
    | None ->
        let size_expr =
          match typ with
          | Byte_slice { size } -> size
          | Byte_array { size } -> size
          | _ -> invalid_arg "add_field: unsupported variable-size field type"
        in
        let size_fn = compile_expr r.r_field_readers size_expr in
        let field_off =
          match field_off_static with
          | Some n -> n
          | None ->
              invalid_arg
                "add_field: multiple variable-size fields not yet supported"
        in
        let raw_reader : w typ -> bytes -> int -> w =
         fun typ buf base ->
          let sz = size_fn buf base in
          match typ with
          | Byte_slice _ ->
              Slice.make_or_eod buf ~first:(base + field_off) ~length:sz
          | Byte_array _ -> Bytes.sub_string buf (base + field_off) sz
          | _ -> assert false
        in
        let reader = raw_reader typ in
        let raw_writer : w typ -> bytes -> int -> w -> unit =
         fun typ buf base v ->
          match typ with
          | Byte_slice _ ->
              let src = (v : Slice.t) in
              let len = Slice.length src in
              Bytes.blit (Slice.bytes src) (Slice.first src) buf
                (base + field_off) len
          | Byte_array _ ->
              let s = (v : string) in
              Bytes.blit_string s 0 buf (base + field_off) (String.length s)
          | _ -> assert false
        in
        let writer = raw_writer typ in
        let configurator () =
          fld.f_reader <- wrap_reader reader;
          fld.f_writer <- wrap_writer writer
        in
        let new_next_off =
          Dynamic_next (fun buf base -> base + field_off + size_fn buf base)
        in
        let int_reader buf base =
          match int_of_typ_value typ (reader buf base) with
          | Some v -> v
          | None -> 0
        in
        extend
          ~readers:(Snoc (r.r_readers, wrap_reader reader))
          ~writers_rev:
            ((fun v buf off -> writer buf off (get_wire v)) :: r.r_writers_rev)
          ~min_wire_size:r.r_min_wire_size ~next_off:new_next_off
          ~fields_rev:(struct_field fld :: r.r_fields_rev)
          ~validators_rev:
            (build_validator ~byte_off:field_off typ reader
            :: r.r_validators_rev)
          ~bf:None
          ~configurators_rev:(configurator :: r.r_configurators_rev)
          ~field_readers:((name, int_reader) :: r.r_field_readers)
  in
  add typ get (fun reader -> reader) (fun writer -> writer) (Some Refl)

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

let seal : type r. (r, r) record -> r t =
 fun (Record r) ->
  List.iter (fun f -> f ()) (List.rev r.r_configurators_rev);
  let wire_size_info =
    match r.r_next_off with
    | Static_next n -> Fixed n
    | Dynamic_next f -> Variable { min_size = r.r_min_wire_size; compute = f }
  in
  let min_size = r.r_min_wire_size in
  let writers = Array.of_list (List.rev r.r_writers_rev) in
  let n_writers = Array.length writers in
  let build_decode : type full. full -> (full, r) readers -> bytes -> int -> r =
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
        ( Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
          r7 ) ->
        fun buf off ->
          make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
            (r6 buf off) (r7 buf off)
    | Snoc
        ( Snoc
            ( Snoc
                (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
              r7 ),
          r8 ) ->
        fun buf off ->
          make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
            (r6 buf off) (r7 buf off) (r8 buf off)
    | readers ->
        (* 9+ fields: convert to forward list at seal time, apply at decode
           time. Cost: ceil(n/8) - 1 partial applications. *)
        let fwd = to_fwd readers in
        fun buf off -> apply_fwd make fwd buf off
  in
  let raw_decode = build_decode r.r_make r.r_readers in
  let validators = List.rev r.r_validators_rev in
  let param_base = r.r_n_array_slots in
  (* Collect and index params *)
  let struct_fields = List.rev r.r_fields_rev in
  let param_handles =
    let seen = Hashtbl.create 4 in
    let handles = Stdlib.ref ([] : Param.packed list) in
    let rec scan_expr : type a. a expr -> unit = function
      | Param_ref p ->
          if not (Hashtbl.mem seen p.ph_name) then begin
            Hashtbl.add seen p.ph_name ();
            handles := Param.Pack p :: !handles
          end
      | Add (a, b) ->
          scan_expr a;
          scan_expr b
      | Sub (a, b) ->
          scan_expr a;
          scan_expr b
      | Mul (a, b) ->
          scan_expr a;
          scan_expr b
      | Div (a, b) ->
          scan_expr a;
          scan_expr b
      | Mod (a, b) ->
          scan_expr a;
          scan_expr b
      | Land (a, b) ->
          scan_expr a;
          scan_expr b
      | Lor (a, b) ->
          scan_expr a;
          scan_expr b
      | Lxor (a, b) ->
          scan_expr a;
          scan_expr b
      | Lsl (a, b) ->
          scan_expr a;
          scan_expr b
      | Lsr (a, b) ->
          scan_expr a;
          scan_expr b
      | Eq (a, b) ->
          scan_expr a;
          scan_expr b
      | Ne (a, b) ->
          scan_expr a;
          scan_expr b
      | Lt (a, b) ->
          scan_expr a;
          scan_expr b
      | Le (a, b) ->
          scan_expr a;
          scan_expr b
      | Gt (a, b) ->
          scan_expr a;
          scan_expr b
      | Ge (a, b) ->
          scan_expr a;
          scan_expr b
      | And (a, b) ->
          scan_expr a;
          scan_expr b
      | Or (a, b) ->
          scan_expr a;
          scan_expr b
      | Not e -> scan_expr e
      | Lnot e -> scan_expr e
      | Cast (_, e) -> scan_expr e
      | Int _ | Int64 _ | Bool _ | Ref _ | Sizeof _ | Sizeof_this | Field_pos ->
          ()
    in
    let rec scan_stmt = function
      | Types.Assign (p, e) ->
          if not (Hashtbl.mem seen p.ph_name) then begin
            Hashtbl.add seen p.ph_name ();
            handles := Param.Pack p :: !handles
          end;
          scan_expr e
      | Types.Field_assign (_, _, e) -> scan_expr e
      | Types.Extern_call (_, _) -> ()
      | Types.Return e -> scan_expr e
      | Types.Abort -> ()
      | Types.If (c, t, e) ->
          scan_expr c;
          List.iter scan_stmt t;
          Option.iter (List.iter scan_stmt) e
      | Types.Var (_, e) -> scan_expr e
    in
    let scan_action = function
      | Types.On_success stmts | Types.On_act stmts -> List.iter scan_stmt stmts
    in
    List.iter
      (fun (Types.Field f) ->
        Option.iter scan_expr f.constraint_;
        Option.iter scan_action f.action)
      struct_fields;
    Option.iter scan_expr r.r_where;
    List.rev !handles
  in
  let n_params = List.length param_handles in
  List.iteri
    (fun i (Param.Pack p) ->
      p.ph_slot <- param_base + i;
      p.ph_env_idx <- i)
    param_handles;
  let n_total = param_base + n_params in
  let compiled_where =
    match r.r_where with
    | None -> None
    | Some cond ->
        let rev_readers = List.rev r.r_field_readers in
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
  in
  let validate_arr arr buf off =
    List.iter (fun (_byte_off, f) -> f arr buf off) validators;
    match compiled_where with
    | Some f when not (f arr) ->
        raise (Parse_error (Constraint_failed "where clause"))
    | _ -> ()
  in
  let validate buf off =
    let arr = Array.make n_total 0 in
    validate_arr arr buf off
  in
  {
    t_name = r.r_name;
    t_decode =
      (fun buf off ->
        if off + min_size > Bytes.length buf then
          raise_eof ~expected:min_size ~got:(Bytes.length buf - off);
        raw_decode buf off);
    t_encode =
      (fun v buf off ->
        if off + min_size > Bytes.length buf then
          invalid_arg
            (Fmt.str "Codec.encode %s: buffer too short (need %d, got %d)"
               r.r_name min_size
               (Bytes.length buf - off));
        for i = 0 to n_writers - 1 do
          writers.(i) v buf off
        done);
    t_wire_size = wire_size_info;
    t_struct_fields = struct_fields;
    t_validate = validate;
    t_validate_arr = validate_arr;
    t_n_array_slots = n_total;
    t_param_base = param_base;
    t_n_params = n_params;
    t_param_handles = param_handles;
    t_where = r.r_where;
  }

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

let decode t buf off =
  let v = t.t_decode buf off in
  t.t_validate buf off;
  v

let env t : Param.env = { Types.pe_slots = Array.make t.t_n_params 0 }

let decode_with t (e : Param.env) buf off =
  let v = t.t_decode buf off in
  let arr = Array.make t.t_n_array_slots 0 in
  if t.t_n_params > 0 then
    Array.blit e.pe_slots 0 arr t.t_param_base t.t_n_params;
  t.t_validate_arr arr buf off;
  (* Sync output params back to env and ph_cell *)
  List.iter
    (fun (Param.Pack p) ->
      let v = arr.(p.ph_slot) in
      e.pe_slots.(p.ph_env_idx) <- v;
      p.ph_cell := v)
    t.t_param_handles;
  v

let encode t v buf off = t.t_encode v buf off

(* Collect param declarations from Param_ref/Assign nodes in fields and where *)
let collect_params (fields : Types.field list) where =
  let module L = Stdlib.List in
  let seen = Hashtbl.create 4 in
  let params = Stdlib.ref ([] : param list) in
  let add_param name decl =
    if not (Hashtbl.mem seen name) then begin
      Hashtbl.add seen name ();
      params := L.cons decl !params
    end
  in
  let rec scan_expr : type a. a expr -> unit =
   fun e ->
    match e with
    | Param_ref p ->
        add_param p.ph_name
          {
            param_name = p.ph_name;
            param_typ = p.ph_packed_typ;
            mutable_ = p.ph_mutable;
          }
    | Add (a, b) ->
        scan_expr a;
        scan_expr b
    | Sub (a, b) ->
        scan_expr a;
        scan_expr b
    | Mul (a, b) ->
        scan_expr a;
        scan_expr b
    | Div (a, b) ->
        scan_expr a;
        scan_expr b
    | Mod (a, b) ->
        scan_expr a;
        scan_expr b
    | Land (a, b) ->
        scan_expr a;
        scan_expr b
    | Lor (a, b) ->
        scan_expr a;
        scan_expr b
    | Lxor (a, b) ->
        scan_expr a;
        scan_expr b
    | Lsl (a, b) ->
        scan_expr a;
        scan_expr b
    | Lsr (a, b) ->
        scan_expr a;
        scan_expr b
    | Lt (a, b) ->
        scan_expr a;
        scan_expr b
    | Le (a, b) ->
        scan_expr a;
        scan_expr b
    | Gt (a, b) ->
        scan_expr a;
        scan_expr b
    | Ge (a, b) ->
        scan_expr a;
        scan_expr b
    | Eq (a, b) ->
        scan_expr a;
        scan_expr b
    | Ne (a, b) ->
        scan_expr a;
        scan_expr b
    | And (a, b) ->
        scan_expr a;
        scan_expr b
    | Or (a, b) ->
        scan_expr a;
        scan_expr b
    | Not a -> scan_expr a
    | Lnot a -> scan_expr a
    | Cast (_, e) -> scan_expr e
    | Sizeof _ | Sizeof_this | Field_pos | Int _ | Int64 _ | Bool _ | Ref _ ->
        ()
  in
  let rec scan_action_stmt = function
    | Assign (p, e) ->
        add_param p.ph_name
          {
            param_name = p.ph_name;
            param_typ = p.ph_packed_typ;
            mutable_ = p.ph_mutable;
          };
        scan_expr e
    | Field_assign (_, _, e) -> scan_expr e
    | Extern_call (_, _) -> ()
    | Return e -> scan_expr e
    | Abort -> ()
    | If (c, t, e) ->
        scan_expr c;
        L.iter scan_action_stmt t;
        Option.iter (L.iter scan_action_stmt) e
    | Var (_, e) -> scan_expr e
  in
  let scan_action = function
    | On_success stmts | On_act stmts -> L.iter scan_action_stmt stmts
  in
  L.iter
    (fun (Field f) ->
      Option.iter scan_expr f.constraint_;
      Option.iter scan_action f.action)
    fields;
  Option.iter scan_expr where;
  L.rev !params

let to_struct t =
  let formals = collect_params t.t_struct_fields t.t_where in
  match (formals, t.t_where) with
  | [], None -> struct' t.t_name t.t_struct_fields
  | _ -> param_struct t.t_name formals ?where:t.t_where t.t_struct_fields

let[@inline] get (type a r) (_codec : r t) (f : (a, r) field) :
    (bytes -> int -> a) Staged.t =
  Staged.stage f.f_reader

let[@inline] set (type a r) (_codec : r t) (f : (a, r) field) :
    (bytes -> int -> a -> unit) Staged.t =
  Staged.stage f.f_writer

let pp ppf t = Fmt.string ppf t.t_name
let field_ref (type a r) (f : (a, r) field) : int expr = Ref f.name

(* ── Bitfield batch access ── *)

type bitfield = bf_info

let bitfield (type r) (_codec : r t) (f : (int, r) field) : bitfield =
  match f.f_bf with
  | Some info -> info
  | None -> invalid_arg "Codec.bitfield: field is not a bitfield"

let load_word (bf : bitfield) : (bytes -> int -> int) Staged.t =
  Staged.stage bf.bf_word_reader

let[@inline always] extract (bf : bitfield) word =
  let p = bf.bf_packed in
  (word lsr (p land 0xFF)) land (p lsr 8)

(* ── Snapshot: batch bitfield access ── *)
