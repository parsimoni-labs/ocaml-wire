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

module Fields = Map.Make (String)

type ctx = { fields : int Fields.t; sizeof_this : int; field_pos : int }

let empty_ctx = { fields = Fields.empty; sizeof_this = 0; field_pos = 0 }

let ctx_of_params params =
  List.fold_left
    (fun ctx (name, v) -> { ctx with fields = Fields.add name v ctx.fields })
    empty_ctx (Param.to_ctx params)

let commit_params ctx params =
  Fields.iter (fun name v -> Param.store_name params name v) ctx.fields

let ctx_add name v ctx = { ctx with fields = Fields.add name v ctx.fields }

let rec int_of_typ_value : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ -> Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> int_of_typ_value base v
  | Where { inner; _ } -> int_of_typ_value inner v
  | Single_elem { elem; _ } -> int_of_typ_value elem v
  | Apply { typ; _ } -> int_of_typ_value typ v
  | Map { inner; encode; _ } -> int_of_typ_value inner (encode v)
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      0

let rec eval_expr_ctx : type a. ctx -> a expr -> a =
 fun ctx -> function
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref name -> Fields.find name ctx.fields
  | Sizeof t -> field_wire_size t |> Option.value ~default:0
  | Sizeof_this -> ctx.sizeof_this
  | Field_pos -> ctx.field_pos
  | Add (a, b) -> eval_expr_ctx ctx a + eval_expr_ctx ctx b
  | Sub (a, b) -> eval_expr_ctx ctx a - eval_expr_ctx ctx b
  | Mul (a, b) -> eval_expr_ctx ctx a * eval_expr_ctx ctx b
  | Div (a, b) -> eval_expr_ctx ctx a / eval_expr_ctx ctx b
  | Mod (a, b) -> eval_expr_ctx ctx a mod eval_expr_ctx ctx b
  | Land (a, b) -> eval_expr_ctx ctx a land eval_expr_ctx ctx b
  | Lor (a, b) -> eval_expr_ctx ctx a lor eval_expr_ctx ctx b
  | Lxor (a, b) -> eval_expr_ctx ctx a lxor eval_expr_ctx ctx b
  | Lnot a -> lnot (eval_expr_ctx ctx a)
  | Lsl (a, b) -> eval_expr_ctx ctx a lsl eval_expr_ctx ctx b
  | Lsr (a, b) -> eval_expr_ctx ctx a lsr eval_expr_ctx ctx b
  | Eq (a, b) -> eval_expr_ctx ctx a = eval_expr_ctx ctx b
  | Ne (a, b) -> eval_expr_ctx ctx a <> eval_expr_ctx ctx b
  | Lt (a, b) -> eval_expr_ctx ctx a < eval_expr_ctx ctx b
  | Le (a, b) -> eval_expr_ctx ctx a <= eval_expr_ctx ctx b
  | Gt (a, b) -> eval_expr_ctx ctx a > eval_expr_ctx ctx b
  | Ge (a, b) -> eval_expr_ctx ctx a >= eval_expr_ctx ctx b
  | And (a, b) -> eval_expr_ctx ctx a && eval_expr_ctx ctx b
  | Or (a, b) -> eval_expr_ctx ctx a || eval_expr_ctx ctx b
  | Not a -> not (eval_expr_ctx ctx a)
  | Cast (_, e) -> eval_expr_ctx ctx e

type action_outcome =
  | Action_continue of ctx
  | Action_return of bool * ctx
  | Action_abort

let rec exec_action_stmt ctx = function
  | Assign (name, e) ->
      Action_continue
        { ctx with fields = Fields.add name (eval_expr_ctx ctx e) ctx.fields }
  | Return e -> Action_return (eval_expr_ctx ctx e, ctx)
  | Abort -> Action_abort
  | If (cond, then_, else_) ->
      exec_action_stmts ctx
        (if eval_expr_ctx ctx cond then then_
         else Option.value else_ ~default:[])
  | Var (name, e) ->
      Action_continue
        { ctx with fields = Fields.add name (eval_expr_ctx ctx e) ctx.fields }

and exec_action_stmts ctx = function
  | [] -> Action_continue ctx
  | stmt :: rest -> (
      match exec_action_stmt ctx stmt with
      | Action_continue ctx' -> exec_action_stmts ctx' rest
      | Action_return _ as r -> r
      | Action_abort -> Action_abort)

let apply_action ctx = function
  | None -> ctx
  | Some (On_success stmts | On_act stmts) -> (
      match exec_action_stmts ctx stmts with
      | Action_continue ctx' -> ctx'
      | Action_return (true, ctx') -> ctx'
      | Action_return (false, _) ->
          raise (Parse_error (Constraint_failed "field action"))
      | Action_abort -> raise (Parse_error (Constraint_failed "field action")))

(* Type equality witness for GADT-safe accessor setting *)
type (_, _) eq = Refl : ('a, 'a) eq

type ('a, 'r) field = {
  name : string;
  typ : 'a typ;
  constraint_ : bool expr option;
  action : action option;
  get : 'r -> 'a;
  mutable f_reader : bytes -> int -> 'a;
  mutable f_writer : bytes -> int -> 'a -> unit;
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
            (Printf.sprintf "Codec: unbound field ref %S in size expression"
               name))
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
      r_validators_rev : (ctx -> bytes -> int -> ctx) list;
      r_bf : bf_codec_state option;
      r_configurators_rev : (unit -> unit) list;
      r_field_readers : (string * (bytes -> int -> int)) list;
      r_params : param list;
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
  t_validate : ctx -> bytes -> int -> ctx;
  t_params : param list;
  t_where : bool expr option;
}

let record_start ?(params = []) ?where name make =
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
      r_bf = None;
      r_configurators_rev = [];
      r_field_readers = [];
      r_params = params;
      r_where = where;
    }

let field name ?constraint_ ?action typ get =
  let not_ready _ _ = failwith "field: not added to a record yet" in
  {
    name;
    typ;
    constraint_;
    action;
    get;
    f_reader = not_ready;
    f_writer = (fun _ _ _ -> failwith "field: not added to a record yet");
  }

(* Bitfield helpers *)

let bf_base_byte_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4
let bf_base_total_bits = function BF_U8 -> 8 | BF_U16 _ -> 16 | BF_U32 _ -> 32

let bf_base_equal a b =
  match (a, b) with
  | BF_U8, BF_U8 -> true
  | BF_U16 e1, BF_U16 e2 -> e1 = e2
  | BF_U32 e1, BF_U32 e2 -> e1 = e2
  | _ -> false

let bf_write_base base buf off v =
  match base with
  | BF_U8 -> Bytes.set_uint8 buf off v
  | BF_U16 Little -> Bytes.set_uint16_le buf off v
  | BF_U16 Big -> Bytes.set_uint16_be buf off v
  | BF_U32 Little -> UInt32.set_le buf off v
  | BF_U32 Big -> UInt32.set_be buf off v

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

let add_field : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record
    =
 fun (Record r) ({ name; typ; get; _ } as fld) ->
  (* Recursively unwrap Map layers to reach the wire-level type, composing
     encode/decode conversions along the way. Returns the updated record;
     field accessors are configured later by [seal]. *)
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
    | Bits { width; base } ->
        let total = bf_base_total_bits base in
        let need_new_group =
          match r.r_bf with
          | None -> true
          | Some bf ->
              (not (bf_base_equal bf.bfc_base base))
              || bf.bfc_bits_used + width > bf.bfc_total_bits
        in
        let static_off =
          match r.r_next_off with
          | Static_next n -> n
          | Dynamic_next _ ->
              invalid_arg
                "add_field: bitfields after variable-size fields not supported"
        in
        let base_off, bits_used, size_delta, extra_writers =
          if need_new_group then
            let base_off = static_off in
            let clear = build_bf_clear base base_off in
            ( base_off,
              0,
              bf_base_byte_size base,
              [ (fun _v buf off -> clear buf off) ] )
          else
            let bf = Option.get r.r_bf in
            (bf.bfc_base_off, bf.bfc_bits_used, 0, [])
        in
        let shift = total - bits_used - width in
        let raw_reader = build_bf_reader base base_off shift width in
        let raw_writer = build_bf_writer base base_off shift width in
        let accessor_writer =
          build_bf_accessor_writer base base_off shift width
        in
        let int_reader buf off = (raw_reader buf off : int) in
        let configurator () =
          match eq with
          | Some Refl ->
              fld.f_reader <- raw_reader;
              fld.f_writer <- accessor_writer
          | None ->
              fld.f_reader <- wrap_reader raw_reader;
              fld.f_writer <- wrap_writer accessor_writer
        in
        let new_bf =
          {
            bfc_base = base;
            bfc_base_off = base_off;
            bfc_bits_used = bits_used + width;
            bfc_total_bits = total;
          }
        in
        Record
          {
            r_name = r.r_name;
            r_make = r.r_make;
            r_readers = Snoc (r.r_readers, wrap_reader raw_reader);
            r_writers_rev =
              (fun v buf off -> raw_writer buf off (get_wire v))
              :: (extra_writers @ r.r_writers_rev);
            r_min_wire_size = r.r_min_wire_size + size_delta;
            r_next_off = Static_next (static_off + size_delta);
            r_fields_rev = struct_field fld :: r.r_fields_rev;
            r_validators_rev =
              (fun ctx buf base ->
                let ctx' =
                  ctx_add name (int_of_typ_value typ (raw_reader buf base)) ctx
                in
                let ctx' =
                  match fld.constraint_ with
                  | Some c when not (eval_expr_ctx ctx' c) ->
                      raise (Parse_error (Constraint_failed "field constraint"))
                  | _ -> ctx'
                in
                apply_action ctx' fld.action)
              :: r.r_validators_rev;
            r_bf = Some new_bf;
            r_configurators_rev = configurator :: r.r_configurators_rev;
            r_field_readers = (name, int_reader) :: r.r_field_readers;
            r_params = r.r_params;
            r_where = r.r_where;
          }
    | _ -> (
        (* Helper: get the absolute offset for this field *)
        let field_off_static =
          match r.r_next_off with
          | Static_next n -> Some n
          | Dynamic_next _ -> None
        in
        let field_off_fn =
          match r.r_next_off with
          | Static_next n -> fun (_buf : bytes) (_base : int) -> n
          | Dynamic_next f -> fun buf base -> f buf base - base
          (* convert absolute -> relative to base *)
        in
        match field_wire_size typ with
        | Some fsize ->
            (* Fixed-size field — may be at static or dynamic offset *)
            let field_off =
              match field_off_static with
              | Some n -> n
              | None -> -1 (* sentinel: will use dynamic path *)
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
            (* Track int reader for expression evaluation *)
            let new_next_off =
              match r.r_next_off with
              | Static_next n -> Static_next (n + fsize)
              | Dynamic_next f ->
                  Dynamic_next (fun buf base -> f buf base + fsize)
            in
            let int_reader buf base =
              int_of_typ_value typ (raw_reader buf base)
            in
            Record
              {
                r_name = r.r_name;
                r_make = r.r_make;
                r_readers = Snoc (r.r_readers, wrap_reader raw_reader);
                r_writers_rev =
                  (match field_off_static with
                  | Some fo ->
                      fun v buf off ->
                        let _ = raw_encoder buf (off + fo) (get_wire v) in
                        ()
                  | None ->
                      fun v buf off ->
                        let fo = field_off_fn buf off in
                        let _ = raw_encoder buf (off + fo) (get_wire v) in
                        ())
                  :: r.r_writers_rev;
                r_min_wire_size = r.r_min_wire_size + fsize;
                r_next_off = new_next_off;
                r_fields_rev = struct_field fld :: r.r_fields_rev;
                r_validators_rev =
                  (fun ctx buf base ->
                    let ctx' =
                      ctx_add name
                        (int_of_typ_value typ (raw_reader buf base))
                        ctx
                    in
                    let ctx' =
                      match fld.constraint_ with
                      | Some c when not (eval_expr_ctx ctx' c) ->
                          raise
                            (Parse_error (Constraint_failed "field constraint"))
                      | _ -> ctx'
                    in
                    apply_action ctx' fld.action)
                  :: r.r_validators_rev;
                r_bf = None;
                r_configurators_rev = configurator :: r.r_configurators_rev;
                r_field_readers = (name, int_reader) :: r.r_field_readers;
                r_params = r.r_params;
                r_where = r.r_where;
              }
        | None ->
            (* Variable-size field (byte_slice/byte_array with dependent
                size). The field's start offset is known; its size is
                evaluated at runtime from previously-declared fields. *)
            let size_expr =
              match typ with
              | Byte_slice { size } -> size
              | Byte_array { size } -> size
              | _ ->
                  invalid_arg "add_field: unsupported variable-size field type"
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
            let int_reader buf base = int_of_typ_value typ (reader buf base) in
            Record
              {
                r_name = r.r_name;
                r_make = r.r_make;
                r_readers = Snoc (r.r_readers, wrap_reader reader);
                r_writers_rev =
                  (fun v buf off -> writer buf off (get_wire v))
                  :: r.r_writers_rev;
                r_min_wire_size = r.r_min_wire_size;
                r_next_off = new_next_off;
                r_fields_rev = struct_field fld :: r.r_fields_rev;
                r_validators_rev =
                  (fun ctx buf base ->
                    let v = reader buf base in
                    let ctx' = ctx_add name (int_of_typ_value typ v) ctx in
                    let ctx' =
                      match fld.constraint_ with
                      | Some c when not (eval_expr_ctx ctx' c) ->
                          raise
                            (Parse_error (Constraint_failed "field constraint"))
                      | _ -> ctx'
                    in
                    apply_action ctx' fld.action)
                  :: r.r_validators_rev;
                r_bf = None;
                r_configurators_rev = configurator :: r.r_configurators_rev;
                r_field_readers = (name, int_reader) :: r.r_field_readers;
                r_params = r.r_params;
                r_where = r.r_where;
              })
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
  let validate ctx buf off =
    let ctx =
      List.fold_left
        (fun ctx f -> f ctx buf off)
        ctx
        (List.rev r.r_validators_rev)
    in
    match r.r_where with
    | Some cond when not (eval_expr_ctx ctx cond) ->
        raise (Parse_error (Constraint_failed "where clause"))
    | _ -> ctx
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
    t_struct_fields = List.rev r.r_fields_rev;
    t_validate = validate;
    t_params = r.r_params;
    t_where = r.r_where;
  }

(* Heterogeneous field list. [] seals the view; (::) adds a field.
   Tracks the constructor type: ('a -> 'b -> 'r, 'r) matches a
   constructor (fun a b -> ...). *)
type ('f, 'r) fields =
  | [] : ('r, 'r) fields
  | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

let view : type f r.
    string ->
    ?params:param list ->
    ?where:bool expr ->
    f ->
    (f, r) fields ->
    r t =
 fun name ?(params = []) ?where constructor flds ->
  let rec add : type g. (g, r) record -> (g, r) fields -> r t =
   fun r flds ->
    match flds with [] -> seal r | f :: rest -> add (add_field r f) rest
  in
  add (record_start ~params ?where name constructor) flds

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

let decode ?(env = Param.empty) t buf off =
  let v = t.t_decode buf off in
  let ctx = t.t_validate (ctx_of_params env) buf off in
  commit_params ctx env;
  v

let encode t v buf off = t.t_encode v buf off

let to_struct t =
  match (t.t_params, t.t_where) with
  | [], None -> struct' t.t_name t.t_struct_fields
  | _ -> param_struct t.t_name t.t_params ?where:t.t_where t.t_struct_fields

let get (type a r) (_codec : r t) (f : (a, r) field) :
    (bytes -> int -> a) Staged.t =
  Staged.stage f.f_reader

let set (type a r) (_codec : r t) (f : (a, r) field) :
    (bytes -> int -> a -> unit) Staged.t =
  Staged.stage f.f_writer

let pp ppf t = Fmt.string ppf t.t_name
let field_ref (type a r) (f : (a, r) field) : int expr = Ref f.name
