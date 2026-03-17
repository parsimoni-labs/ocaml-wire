(* Wire: Dependent Data Descriptions for EverParse 3D *)

module Staged = Staged
module UInt32 = UInt32
module UInt63 = UInt63
include Types

(* Pretty printing using Fmt *)

let pp_endian ppf = function Little -> () | Big -> Fmt.string ppf "BE"

let pp_bitfield_base ppf = function
  | BF_U8 -> Fmt.string ppf "UINT8"
  | BF_U16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | BF_U32 e -> Fmt.pf ppf "UINT32%a" pp_endian e

let pp_cast_type ppf = function
  | `U8 -> Fmt.string ppf "UINT8"
  | `U16 -> Fmt.string ppf "UINT16"
  | `U32 -> Fmt.string ppf "UINT32"
  | `U64 -> Fmt.string ppf "UINT64"

let rec pp_expr : type a. a expr Fmt.t =
 fun ppf expr ->
  match expr with
  | Int n when n < 0 -> Fmt.pf ppf "(%d)" n
  | Int n -> Fmt.int ppf n
  | Int64 n -> Fmt.pf ppf "%LduL" n
  | Bool true -> Fmt.string ppf "true"
  | Bool false -> Fmt.string ppf "false"
  | Ref name -> Fmt.string ppf name
  | Sizeof t -> Fmt.pf ppf "sizeof (%a)" pp_typ t
  | Sizeof_this -> Fmt.string ppf "sizeof (this)"
  | Field_pos -> Fmt.string ppf "field_pos"
  | Add (a, b) -> Fmt.pf ppf "(%a + %a)" pp_expr a pp_expr b
  | Sub (a, b) -> Fmt.pf ppf "(%a - %a)" pp_expr a pp_expr b
  | Mul (a, b) -> Fmt.pf ppf "(%a * %a)" pp_expr a pp_expr b
  | Div (a, b) -> Fmt.pf ppf "(%a / %a)" pp_expr a pp_expr b
  | Mod (a, b) -> Fmt.pf ppf "(%a %% %a)" pp_expr a pp_expr b
  | Land (a, b) -> Fmt.pf ppf "(%a & %a)" pp_expr a pp_expr b
  | Lor (a, b) -> Fmt.pf ppf "(%a | %a)" pp_expr a pp_expr b
  | Lxor (a, b) -> Fmt.pf ppf "(%a ^ %a)" pp_expr a pp_expr b
  | Lnot a -> Fmt.pf ppf "(~%a)" pp_expr a
  | Lsl (a, b) -> Fmt.pf ppf "(%a << %a)" pp_expr a pp_expr b
  | Lsr (a, b) -> Fmt.pf ppf "(%a >> %a)" pp_expr a pp_expr b
  | Eq (a, b) -> Fmt.pf ppf "(%a == %a)" pp_expr a pp_expr b
  | Ne (a, b) -> Fmt.pf ppf "(%a != %a)" pp_expr a pp_expr b
  | Lt (a, b) -> Fmt.pf ppf "(%a < %a)" pp_expr a pp_expr b
  | Le (a, b) -> Fmt.pf ppf "(%a <= %a)" pp_expr a pp_expr b
  | Gt (a, b) -> Fmt.pf ppf "(%a > %a)" pp_expr a pp_expr b
  | Ge (a, b) -> Fmt.pf ppf "(%a >= %a)" pp_expr a pp_expr b
  | And (a, b) -> Fmt.pf ppf "(%a && %a)" pp_expr a pp_expr b
  | Or (a, b) -> Fmt.pf ppf "(%a || %a)" pp_expr a pp_expr b
  | Not a -> Fmt.pf ppf "(!%a)" pp_expr a
  | Cast (t, e) -> Fmt.pf ppf "((%a) %a)" pp_cast_type t pp_expr e

and pp_typ : type a. a typ Fmt.t =
 fun ppf typ ->
  match typ with
  | Uint8 -> Fmt.string ppf "UINT8"
  | Uint16 e -> Fmt.pf ppf "UINT16%a" pp_endian e
  | Uint32 e -> Fmt.pf ppf "UINT32%a" pp_endian e
  | Uint63 e -> Fmt.pf ppf "UINT63%a" pp_endian e
  | Uint64 e -> Fmt.pf ppf "UINT64%a" pp_endian e
  | Bits { base; _ } -> pp_bitfield_base ppf base
  | Unit -> Fmt.string ppf "unit"
  | All_bytes -> Fmt.string ppf "all_bytes"
  | All_zeros -> Fmt.string ppf "all_zeros"
  | Where { cond; inner } -> Fmt.pf ppf "%a { %a }" pp_typ inner pp_expr cond
  | Array { len; elem } -> Fmt.pf ppf "%a[%a]" pp_typ elem pp_expr len
  | Byte_array { size } | Byte_slice { size } ->
      Fmt.pf ppf "UINT8[:byte-size %a]" pp_expr size
  | Single_elem { size; elem; at_most = false } ->
      Fmt.pf ppf "%a[:byte-size-single-element-array %a]" pp_typ elem pp_expr
        size
  | Single_elem { size; elem; at_most = true } ->
      Fmt.pf ppf "%a[:byte-size-single-element-array-at-most %a]" pp_typ elem
        pp_expr size
  | Enum { name; _ } -> Fmt.string ppf name
  | Casetype { name; _ } -> Fmt.string ppf name
  | Struct { name; _ } -> Fmt.string ppf name
  | Type_ref name -> Fmt.string ppf name
  | Qualified_ref { module_; name } -> Fmt.pf ppf "%s::%s" module_ name
  | Apply { typ; args } ->
      Fmt.pf ppf "%a(%a)" pp_typ typ Fmt.(list ~sep:comma pp_packed_expr) args
  | Map { inner; _ } -> pp_typ ppf inner

and pp_packed_expr ppf (Pack_expr e) = pp_expr ppf e

let rec pp_action_stmt ppf = function
  | Assign (ptr, e) -> Fmt.pf ppf "*%s = %a;" ptr pp_expr e
  | Return e -> Fmt.pf ppf "return %a;" pp_expr e
  | Abort -> Fmt.string ppf "abort;"
  | If (cond, then_, None) ->
      Fmt.pf ppf "if (%a) { %a }" pp_expr cond
        Fmt.(list ~sep:sp pp_action_stmt)
        then_
  | If (cond, then_, Some else_) ->
      Fmt.pf ppf "if (%a) { %a } else { %a }" pp_expr cond
        Fmt.(list ~sep:sp pp_action_stmt)
        then_
        Fmt.(list ~sep:sp pp_action_stmt)
        else_
  | Var (name, e) -> Fmt.pf ppf "var %s = %a;" name pp_expr e

let pp_action ppf = function
  | On_success stmts ->
      Fmt.pf ppf "{:on-success %a }" Fmt.(list ~sep:sp pp_action_stmt) stmts
  | On_act stmts ->
      Fmt.pf ppf "{:act %a }" Fmt.(list ~sep:sp pp_action_stmt) stmts

(* Extract field suffix for arrays - the modifier goes after the field name *)
type field_suffix =
  | No_suffix
  | Bitwidth of int
  | Byte_array of int expr
  | Single_elem of { size : int expr; at_most : bool }
  | Array of int expr

let field_suffix : type a. a typ -> field_suffix * (Format.formatter -> unit) =
 fun typ ->
  match typ with
  | Bits { width; base } ->
      (Bitwidth width, fun ppf -> pp_bitfield_base ppf base)
  | Byte_array { size } | Byte_slice { size } ->
      (Byte_array size, fun ppf -> Fmt.string ppf "UINT8")
  | Single_elem { size; elem; at_most } ->
      (Single_elem { size; at_most }, fun ppf -> pp_typ ppf elem)
  | Array { len; elem } -> (Array len, fun ppf -> pp_typ ppf elem)
  | _ -> (No_suffix, fun ppf -> pp_typ ppf typ)

let pp_field ppf (Field f) =
  match f.field_name with
  | Some name ->
      let suffix, pp_base = field_suffix f.field_typ in
      Fmt.pf ppf "@,%t %s" pp_base name;
      (* Print suffix after field name *)
      (match suffix with
      | No_suffix -> ()
      | Bitwidth w -> Fmt.pf ppf " : %d" w
      | Byte_array size -> Fmt.pf ppf "[:byte-size %a]" pp_expr size
      | Single_elem { size; at_most = false } ->
          Fmt.pf ppf "[:byte-size-single-element-array %a]" pp_expr size
      | Single_elem { size; at_most = true } ->
          Fmt.pf ppf "[:byte-size-single-element-array-at-most %a]" pp_expr size
      | Array len -> Fmt.pf ppf "[%a]" pp_expr len);
      Option.iter (Fmt.pf ppf " { %a }" pp_expr) f.constraint_;
      Option.iter (Fmt.pf ppf " %a" pp_action) f.action;
      Fmt.string ppf ";"
  | None -> Fmt.pf ppf "@,%a;" pp_typ f.field_typ

let pp_param ppf p =
  let (Pack_typ t) = p.param_typ in
  if p.mutable_ then Fmt.pf ppf "mutable %a *%s" pp_typ t p.param_name
  else Fmt.pf ppf "%a %s" pp_typ t p.param_name

let pp_params ppf params =
  if not (List.is_empty params) then
    Fmt.pf ppf "(%a)" Fmt.(list ~sep:comma pp_param) params

let pp_struct ppf (s : struct_) =
  Fmt.pf ppf "typedef struct _%s%a" s.name pp_params s.params;
  Option.iter (Fmt.pf ppf "@,where (%a)" pp_expr) s.where;
  Fmt.pf ppf "@,{@[<v 2>";
  List.iter (pp_field ppf) s.fields;
  Fmt.pf ppf "@]@,} %s" s.name

let pp_decl ppf = function
  | Typedef { entrypoint; export; doc; struct_ = st } ->
      Option.iter (Fmt.pf ppf "/*++ %s --*/@,") doc;
      if export then Fmt.pf ppf "export@,";
      if entrypoint then Fmt.pf ppf "entrypoint@,";
      Fmt.pf ppf "%a;@,@," pp_struct st
  | Define { name; value } ->
      if value < 0 then Fmt.pf ppf "#define %s (%d)@," name value
      else Fmt.pf ppf "#define %s 0x%x@," name value
  | Extern_fn { name; params; ret = Pack_typ ret } ->
      Fmt.pf ppf "extern %a %s(%a);@,@," pp_typ ret name
        Fmt.(list ~sep:comma pp_param)
        params
  | Extern_probe { init; name } ->
      if init then Fmt.pf ppf "extern probe (INIT) %s;@,@," name
      else Fmt.pf ppf "extern probe %s;@,@," name
  | Enum_decl { name; cases; base = Pack_typ base } ->
      Fmt.pf ppf "%a enum %s {@[<v 2>" pp_typ base name;
      List.iteri
        (fun i (cname, value) ->
          if not (Int.equal i 0) then Fmt.string ppf ",";
          Fmt.pf ppf "@,%s = %d" cname value)
        cases;
      Fmt.pf ppf "@]@,}@,@,"
  | Casetype_decl { name; params; tag = Pack_typ _; cases } ->
      (* First param is the switch discriminant *)
      let disc_name =
        match params with p :: _ -> p.param_name | [] -> "tag"
      in
      (* Internal name has underscore prefix, public name doesn't *)
      let internal_name, public_name =
        if String.length name > 0 && name.[0] = '_' then
          (name, String.sub name 1 (String.length name - 1))
        else ("_" ^ name, name)
      in
      Fmt.pf ppf "casetype %s%a {@[<v 2>@,switch (%s) {" internal_name pp_params
        params disc_name;
      List.iteri
        (fun i (tag_opt, Pack_typ typ) ->
          let field_name = Fmt.str "v%d" i in
          match tag_opt with
          | Some e ->
              Fmt.pf ppf "@,case %a: %a %s;" pp_packed_expr e pp_typ typ
                field_name
          | None -> Fmt.pf ppf "@,default: %a %s;" pp_typ typ field_name)
        cases;
      Fmt.pf ppf "@,}@]@,} %s;@,@," public_name

let pp_module ppf m =
  Option.iter (Fmt.pf ppf "/*++ %s --*/@,@,") m.doc;
  List.iter (pp_decl ppf) m.decls

let to_3d m = Fmt.str "@[<v>%a@]" pp_module m

let to_3d_file path m =
  let oc = open_out path in
  let ppf = Format.formatter_of_out_channel oc in
  Fmt.pf ppf "@[<v>%a@]@." pp_module m;
  close_out oc

(* Binary parsing with bytesrw *)

module Reader = Bytesrw.Bytes.Reader
module Slice = Bytesrw.Bytes.Slice

type parse_error =
  | Unexpected_eof of { expected : int; got : int }
  | Constraint_failed of string
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | All_zeros_failed of { offset : int }

exception Parse_error of parse_error

let raise_eof ~expected ~got =
  raise (Parse_error (Unexpected_eof { expected; got }))

let pp_parse_error ppf = function
  | Unexpected_eof { expected; got } ->
      Fmt.pf ppf "unexpected EOF: expected %d bytes, got %d" expected got
  | Constraint_failed msg -> Fmt.pf ppf "constraint failed: %s" msg
  | Invalid_enum { value; valid } ->
      Fmt.pf ppf "invalid enum value %d, valid: [%a]" value
        Fmt.(list ~sep:comma int)
        valid
  | Invalid_tag tag -> Fmt.pf ppf "invalid tag: %d" tag
  | All_zeros_failed { offset } ->
      Fmt.pf ppf "non-zero byte at offset %d" offset

(* Parsing context - tracks field values for dependent types.

   All field values are stored as [int] after conversion via [val_to_int].
   This is sound because constraint expressions (the only consumers of
   context values) operate on integers. *)
module Ctx = Map.Make (String)

type ctx = int Ctx.t

let empty_ctx = Ctx.empty

(* Convert a typed value to [int] for context storage. All types that
   appear in constraint expressions are numeric, so this conversion is
   lossless for practical schemas. Non-numeric types store 0. *)
let rec val_to_int : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> v
  | Uint16 _ -> v
  | Uint32 _ -> UInt32.to_int v
  | Uint63 _ -> UInt63.to_int v
  | Uint64 _ ->
      (* Unsigned interpretation — values >= 2^62 don't fit in OCaml int,
         return max_int so constraints [value <= K] fail correctly *)
      Int64.unsigned_to_int v |> Option.value ~default:max_int
  | Bits _ -> v
  | Enum { base; _ } -> val_to_int base v
  | Where { inner; _ } -> val_to_int inner v
  | Single_elem { elem; _ } -> val_to_int elem v
  | Apply { typ; _ } -> val_to_int typ v
  | Map { inner; encode; _ } -> val_to_int inner (encode v)
  | Unit | All_bytes | All_zeros | Array _ | Byte_array _ | Byte_slice _
  | Casetype _ | Struct _ | Type_ref _ | Qualified_ref _ ->
      0

let ctx_get ctx name =
  match Ctx.find_opt name ctx with
  | Some v -> v
  | None -> failwith ("unbound field: " ^ name)

(** Compute wire size of a type (None for variable-size types). *)
let rec field_wire_size : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint64 _ -> Some 8
  | Bits { base; _ } -> (
      match base with
      | BF_U8 -> Some 1
      | BF_U16 _ -> Some 2
      | BF_U32 _ -> Some 4)
  | Unit -> Some 0
  | Byte_array { size = Int n } | Byte_slice { size = Int n } -> Some n
  | Where { inner; _ } -> field_wire_size inner
  | Enum { base; _ } -> field_wire_size base
  | Map { inner; _ } -> field_wire_size inner
  | _ -> None

exception Parse_exn of parse_error

(* Buffered decoder — all reads go through a local buffer [i].
   [i_next] and [i_len] track the valid region. Two construction modes:
   - [decoder reader]: streaming from a Reader, refills by copying slices in
   - [decoder_of_bytes buf len]: direct from bytes, no Reader, no copy *)
let i_size = 256

type decoder = {
  reader : Reader.t option;
  i : bytes;
  mutable i_next : int;
  mutable i_len : int;
  mutable is_eod : bool;
  mutable position : int;
}

let decoder reader =
  {
    reader = Some reader;
    i = Bytes.create i_size;
    i_next = 0;
    i_len = 0;
    is_eod = false;
    position = 0;
  }

let decoder_of_bytes buf len =
  {
    reader = None;
    i = buf;
    i_next = 0;
    i_len = len;
    is_eod = true;
    position = 0;
  }

(* Compact remaining bytes to front, then fill from reader. *)
let refill dec =
  match dec.reader with
  | None -> () (* direct mode: no reader to refill from *)
  | Some reader ->
      let rem = dec.i_len - dec.i_next in
      if rem > 0 then Bytes.blit dec.i dec.i_next dec.i 0 rem;
      dec.i_next <- 0;
      dec.i_len <- rem;
      let cap = Bytes.length dec.i in
      let rec fill () =
        if dec.i_len >= cap then ()
        else
          let slice = Reader.read reader in
          if Slice.is_eod slice then dec.is_eod <- true
          else begin
            let len = Slice.length slice in
            let space = cap - dec.i_len in
            let n = min len space in
            Bytes.blit (Slice.bytes slice) (Slice.first slice) dec.i dec.i_len n;
            dec.i_len <- dec.i_len + n;
            if n < len then
              Reader.push_back reader
                (Slice.make (Slice.bytes slice)
                   ~first:(Slice.first slice + n)
                   ~length:(len - n));
            if dec.i_len < cap && not dec.is_eod then fill ()
          end
      in
      fill ()

let[@inline] available dec = dec.i_len - dec.i_next

(* Read n bytes for small fixed-size fields (n <= 8).
   Returns offset into dec.i. Raises Parse_exn on EOF. *)
let[@inline] read_small dec n =
  if available dec >= n then begin
    let off = dec.i_next in
    dec.i_next <- dec.i_next + n;
    dec.position <- dec.position + n;
    off
  end
  else begin
    refill dec;
    if available dec >= n then begin
      let off = dec.i_next in
      dec.i_next <- dec.i_next + n;
      dec.position <- dec.position + n;
      off
    end
    else
      raise (Parse_exn (Unexpected_eof { expected = n; got = available dec }))
  end

(* Read exactly n bytes into a fresh buffer (for variable-size fields).
   Raises Parse_exn on EOF. *)
let read_bytes dec n =
  if n = 0 then Bytes.empty
  else if n <= 8 then begin
    let off = read_small dec n in
    Bytes.sub dec.i off n
  end
  else
    let buf = Bytes.create n in
    let rec loop off remaining =
      if remaining = 0 then buf
      else if available dec = 0 then begin
        refill dec;
        if dec.is_eod && available dec = 0 then
          raise (Parse_exn (Unexpected_eof { expected = n; got = off }))
        else loop off remaining
      end
      else
        let avail = available dec in
        let to_copy = min avail remaining in
        Bytes.blit dec.i dec.i_next buf off to_copy;
        dec.i_next <- dec.i_next + to_copy;
        dec.position <- dec.position + to_copy;
        loop (off + to_copy) (remaining - to_copy)
    in
    loop 0 n

(* Read all remaining bytes *)
let read_all dec =
  let buf = Buffer.create 256 in
  let rec loop () =
    if available dec = 0 then begin
      refill dec;
      if dec.is_eod && available dec = 0 then Buffer.contents buf else loop ()
    end
    else begin
      let len = available dec in
      Buffer.add_subbytes buf dec.i dec.i_next len;
      dec.position <- dec.position + len;
      dec.i_next <- dec.i_len;
      loop ()
    end
  in
  loop ()

(* Evaluate an expression in context *)
let rec eval_expr : type a. ctx -> a expr -> a =
 fun ctx expr ->
  match expr with
  | Int n -> n
  | Int64 n -> n
  | Bool b -> b
  | Ref name -> ctx_get ctx name
  | Sizeof _ -> 0 (* TODO: compute actual size *)
  | Sizeof_this -> 0
  | Field_pos -> 0
  | Add (a, b) -> eval_expr ctx a + eval_expr ctx b
  | Sub (a, b) -> eval_expr ctx a - eval_expr ctx b
  | Mul (a, b) -> eval_expr ctx a * eval_expr ctx b
  | Div (a, b) -> eval_expr ctx a / eval_expr ctx b
  | Mod (a, b) -> eval_expr ctx a mod eval_expr ctx b
  | Land (a, b) -> eval_expr ctx a land eval_expr ctx b
  | Lor (a, b) -> eval_expr ctx a lor eval_expr ctx b
  | Lxor (a, b) -> eval_expr ctx a lxor eval_expr ctx b
  | Lnot a -> lnot (eval_expr ctx a)
  | Lsl (a, b) -> eval_expr ctx a lsl eval_expr ctx b
  | Lsr (a, b) -> eval_expr ctx a lsr eval_expr ctx b
  | Eq (a, b) -> eval_expr ctx a = eval_expr ctx b
  | Ne (a, b) -> eval_expr ctx a <> eval_expr ctx b
  | Lt (a, b) -> eval_expr ctx a < eval_expr ctx b
  | Le (a, b) -> eval_expr ctx a <= eval_expr ctx b
  | Gt (a, b) -> eval_expr ctx a > eval_expr ctx b
  | Ge (a, b) -> eval_expr ctx a >= eval_expr ctx b
  | And (a, b) -> eval_expr ctx a && eval_expr ctx b
  | Or (a, b) -> eval_expr ctx a || eval_expr ctx b
  | Not a -> not (eval_expr ctx a)
  | Cast (_, e) -> eval_expr ctx e (* TODO: proper casting *)

(* Bitfield accumulator for packed struct parsing.
   Consecutive bitfields sharing the same base type are packed together.
   Bits are extracted from MSB to LSB (big-endian style) per EverParse 3D. *)
type bf_accum = {
  bf_base : bitfield_base;
  bf_word : int;
  bf_bits_used : int;
  bf_total_bits : int;
}

let bf_total_bits = function BF_U8 -> 8 | BF_U16 _ -> 16 | BF_U32 _ -> 32
let bf_base_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4

let bf_compatible base1 base2 =
  match (base1, base2) with
  | BF_U8, BF_U8 -> true
  | BF_U16 e1, BF_U16 e2 -> e1 = e2
  | BF_U32 e1, BF_U32 e2 -> e1 = e2
  | _ -> false

let bf_read_word dec base =
  let size = bf_base_size base in
  let off = read_small dec size in
  let buf = dec.i in
  match base with
  | BF_U8 -> Bytes.get_uint8 buf off
  | BF_U16 Little -> Bytes.get_uint16_le buf off
  | BF_U16 Big -> Bytes.get_uint16_be buf off
  | BF_U32 Little -> Int32.to_int (Bytes.get_int32_le buf off)
  | BF_U32 Big -> Int32.to_int (Bytes.get_int32_be buf off)

(* Extract bits from accumulated word (MSB first, big-endian style) *)
let bf_extract accum width =
  let shift = accum.bf_total_bits - accum.bf_bits_used - width in
  let mask = (1 lsl width) - 1 in
  let value = (accum.bf_word lsr shift) land mask in
  let new_accum = { accum with bf_bits_used = accum.bf_bits_used + width } in
  (value, new_accum)

(* Check if accumulator can provide more bits *)
let bf_has_room accum width = accum.bf_bits_used + width <= accum.bf_total_bits

(* All parse helpers raise Parse_exn on error — no Result on the hot path. *)

let parse_bits dec base width = bf_read_word dec base land ((1 lsl width) - 1)

let[@inline] parse_int dec n get =
  let off = read_small dec n in
  get dec.i off

let parse_bf_field dec accum_opt base width =
  match accum_opt with
  | Some accum when bf_compatible accum.bf_base base && bf_has_room accum width
    ->
      let v, new_accum = bf_extract accum width in
      let accum_opt' =
        if new_accum.bf_bits_used = new_accum.bf_total_bits then None
        else Some new_accum
      in
      (v, accum_opt')
  | _ ->
      let word = bf_read_word dec base in
      let total = bf_total_bits base in
      let accum =
        {
          bf_base = base;
          bf_word = word;
          bf_bits_used = 0;
          bf_total_bits = total;
        }
      in
      let v, new_accum = bf_extract accum width in
      let accum_opt' =
        if new_accum.bf_bits_used = new_accum.bf_total_bits then None
        else Some new_accum
      in
      (v, accum_opt')

let check_constraint ctx cond =
  match cond with
  | Some c when not (eval_expr ctx c) ->
      raise (Parse_exn (Constraint_failed "field constraint"))
  | _ -> ()

let parse_all_zeros dec =
  let s = read_all dec in
  let rec check i =
    if i >= String.length s then s
    else if s.[i] <> '\000' then
      raise (Parse_exn (All_zeros_failed { offset = i }))
    else check (i + 1)
  in
  check 0

let rec parse_with : type a. decoder -> ctx -> a typ -> a * ctx =
 fun dec ctx typ ->
  match typ with
  | Uint8 -> (parse_int dec 1 Bytes.get_uint8, ctx)
  | Uint16 Little -> (parse_int dec 2 Bytes.get_uint16_le, ctx)
  | Uint16 Big -> (parse_int dec 2 Bytes.get_uint16_be, ctx)
  | Uint32 Little -> (parse_int dec 4 UInt32.get_le, ctx)
  | Uint32 Big -> (parse_int dec 4 UInt32.get_be, ctx)
  | Uint63 Little -> (parse_int dec 8 UInt63.get_le, ctx)
  | Uint63 Big -> (parse_int dec 8 UInt63.get_be, ctx)
  | Uint64 Little -> (parse_int dec 8 Bytes.get_int64_le, ctx)
  | Uint64 Big -> (parse_int dec 8 Bytes.get_int64_be, ctx)
  | Bits { width; base } -> (parse_bits dec base width, ctx)
  | Unit -> ((), ctx)
  | All_bytes -> (read_all dec, ctx)
  | All_zeros -> (parse_all_zeros dec, ctx)
  | Where { cond; inner } ->
      let v, ctx' = parse_with dec ctx inner in
      if eval_expr ctx' cond then (v, ctx')
      else raise (Parse_exn (Constraint_failed "where clause"))
  | Array { len; elem } ->
      let n = eval_expr ctx len in
      let rec loop acc i ctx' =
        if i >= n then (List.rev acc, ctx')
        else
          let v, ctx'' = parse_with dec ctx' elem in
          loop (v :: acc) (i + 1) ctx''
      in
      loop [] 0 ctx
  | Byte_array { size } ->
      let n = eval_expr ctx size in
      let buf = read_bytes dec n in
      (Bytes.to_string buf, ctx)
  | Byte_slice { size } ->
      let n = eval_expr ctx size in
      let buf = read_bytes dec n in
      (Slice.make buf ~first:0 ~length:n, ctx)
  | Single_elem { size = _; elem; at_most = _ } -> parse_with dec ctx elem
  | Enum { cases; base; _ } ->
      let v, ctx' = parse_with dec ctx base in
      let valid = List.map snd cases in
      if List.mem v valid then (v, ctx')
      else raise (Parse_exn (Invalid_enum { value = v; valid }))
  | Casetype { cases; tag; _ } ->
      let tag_val, ctx' = parse_with dec ctx tag in
      let rec find_case = function
        | [] -> raise (Parse_exn (Invalid_tag (val_to_int tag tag_val)))
        | (Some expected, case_typ) :: rest ->
            if expected = tag_val then parse_with dec ctx' case_typ
            else find_case rest
        | (None, case_typ) :: _ -> parse_with dec ctx' case_typ
      in
      find_case cases
  | Struct { fields; _ } -> (parse_struct_fields dec ctx fields : unit * ctx)
  | Map { inner; decode; _ } ->
      let v, ctx' = parse_with dec ctx inner in
      (decode v, ctx')
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

and parse_struct_fields dec ctx fields =
  let parse_field_with_bf : type a.
      bf_accum option -> a typ -> a * bf_accum option =
   fun accum_opt typ ->
    match typ with
    | Bits { width; base } -> parse_bf_field dec accum_opt base width
    | _ ->
        let v, _ = parse_with dec ctx typ in
        (v, None)
  in
  let rec go ctx' accum_opt = function
    | [] -> ((), ctx')
    | Field { field_name; field_typ = Bits _ as ft; constraint_; _ } :: rest ->
        let v, accum_opt' = parse_field_with_bf accum_opt ft in
        let ctx'' =
          match field_name with Some n -> Ctx.add n v ctx' | None -> ctx'
        in
        check_constraint ctx'' constraint_;
        go ctx'' accum_opt' rest
    | Field { field_name; field_typ; constraint_; _ } :: rest ->
        let v, ctx'' = parse_with dec ctx' field_typ in
        let ctx'' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int field_typ v) ctx''
          | None -> ctx''
        in
        check_constraint ctx'' constraint_;
        go ctx'' None rest
  in
  go ctx None fields

let parse typ reader =
  let dec = decoder reader in
  match parse_with dec empty_ctx typ with
  | v, _ -> Ok v
  | exception Parse_exn e -> Error e

let[@inline] check_eof len need =
  if need > len then
    raise (Parse_exn (Unexpected_eof { expected = need; got = len }))

let rec parse_direct : type a. a typ -> bytes -> int -> int -> a =
 fun typ buf off len ->
  match typ with
  | Uint8 ->
      check_eof len (off + 1);
      Bytes.get_uint8 buf off
  | Uint16 Little ->
      check_eof len (off + 2);
      Bytes.get_uint16_le buf off
  | Uint16 Big ->
      check_eof len (off + 2);
      Bytes.get_uint16_be buf off
  | Uint32 Little ->
      check_eof len (off + 4);
      UInt32.get_le buf off
  | Uint32 Big ->
      check_eof len (off + 4);
      UInt32.get_be buf off
  | Uint63 Little ->
      check_eof len (off + 8);
      UInt63.get_le buf off
  | Uint63 Big ->
      check_eof len (off + 8);
      UInt63.get_be buf off
  | Uint64 Little ->
      check_eof len (off + 8);
      Bytes.get_int64_le buf off
  | Uint64 Big ->
      check_eof len (off + 8);
      Bytes.get_int64_be buf off
  | Bits { width; base } ->
      let size = match base with BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4 in
      check_eof len (off + size);
      let word =
        match base with
        | BF_U8 -> Bytes.get_uint8 buf off
        | BF_U16 Little -> Bytes.get_uint16_le buf off
        | BF_U16 Big -> Bytes.get_uint16_be buf off
        | BF_U32 Little -> Int32.to_int (Bytes.get_int32_le buf off)
        | BF_U32 Big -> Int32.to_int (Bytes.get_int32_be buf off)
      in
      word land ((1 lsl width) - 1)
  | Unit -> ()
  | Map { inner; decode; _ } -> decode (parse_direct inner buf off len)
  | Where { inner; _ } -> parse_direct inner buf off len
  | Enum { base; cases; _ } ->
      let v = parse_direct base buf off len in
      let valid = List.map snd cases in
      if List.mem v valid then v
      else raise (Parse_exn (Invalid_enum { value = v; valid }))
  | _ ->
      (* Variable-size or complex types: fall back to decoder *)
      let dec = decoder_of_bytes buf len in
      dec.i_next <- off;
      let v, _ = parse_with dec empty_ctx typ in
      v

let parse_string typ s =
  let buf = Bytes.unsafe_of_string s in
  let len = Bytes.length buf in
  match parse_direct typ buf 0 len with
  | v -> Ok v
  | exception Parse_exn e -> Error e

let parse_bytes typ b =
  match parse_direct typ b 0 (Bytes.length b) with
  | v -> Ok v
  | exception Parse_exn e -> Error e

(* Binary encoding with Bytesrw.Bytes.Writer *)

module Writer = Bytesrw.Bytes.Writer

(* Encoder state *)
(* Buffered encoder — writes accumulate in o, flushed as a single Slice.t.
   Mirrors the decoder's destructured-slice pattern. *)
type encoder = {
  writer : Writer.t;
  o : bytes;
  o_max : int;
  mutable o_next : int;
}

let o_size = 4096

let encoder writer =
  { writer; o = Bytes.create o_size; o_max = o_size - 1; o_next = 0 }

let[@inline] flush enc =
  if enc.o_next > 0 then begin
    Writer.write enc.writer (Slice.make enc.o ~first:0 ~length:enc.o_next);
    enc.o_next <- 0
  end

let[@inline] ensure enc n = if enc.o_next + n > enc.o_max + 1 then flush enc

let[@inline] write_byte enc b =
  ensure enc 1;
  Bytes.set_uint8 enc.o enc.o_next b;
  enc.o_next <- enc.o_next + 1

let[@inline] write_uint16_le enc v =
  ensure enc 2;
  Bytes.set_uint16_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 2

let[@inline] write_uint16_be enc v =
  ensure enc 2;
  Bytes.set_uint16_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 2

let[@inline] write_int32_le enc v =
  ensure enc 4;
  Bytes.set_int32_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 4

let[@inline] write_int32_be enc v =
  ensure enc 4;
  Bytes.set_int32_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 4

let[@inline] write_uint32_le enc v =
  ensure enc 4;
  UInt32.set_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 4

let[@inline] write_uint32_be enc v =
  ensure enc 4;
  UInt32.set_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 4

let[@inline] write_int64_le enc v =
  ensure enc 8;
  Bytes.set_int64_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 8

let[@inline] write_int64_be enc v =
  ensure enc 8;
  Bytes.set_int64_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 8

let[@inline] write_uint63_le enc v =
  ensure enc 8;
  UInt63.set_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 8

let[@inline] write_uint63_be enc v =
  ensure enc 8;
  UInt63.set_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 8

let write_string enc s =
  let len = String.length s in
  if len <= enc.o_max + 1 - enc.o_next then begin
    (* Fits in current buffer *)
    Bytes.blit_string s 0 enc.o enc.o_next len;
    enc.o_next <- enc.o_next + len
  end
  else begin
    (* Flush current buffer, then write string directly *)
    flush enc;
    Writer.write_string enc.writer s
  end

let rec encode_with_ctx : type a. ctx -> a typ -> a -> encoder -> ctx =
 fun ctx typ v enc ->
  match typ with
  | Uint8 ->
      write_byte enc v;
      ctx
  | Uint16 Little ->
      write_uint16_le enc v;
      ctx
  | Uint16 Big ->
      write_uint16_be enc v;
      ctx
  | Uint32 Little ->
      write_uint32_le enc v;
      ctx
  | Uint32 Big ->
      write_uint32_be enc v;
      ctx
  | Uint63 Little ->
      write_uint63_le enc v;
      ctx
  | Uint63 Big ->
      write_uint63_be enc v;
      ctx
  | Uint64 Little ->
      write_int64_le enc v;
      ctx
  | Uint64 Big ->
      write_int64_be enc v;
      ctx
  | Bits { width; base } ->
      let mask = (1 lsl width) - 1 in
      let masked = v land mask in
      (match base with
      | BF_U8 -> write_byte enc masked
      | BF_U16 Little -> write_uint16_le enc masked
      | BF_U16 Big -> write_uint16_be enc masked
      | BF_U32 Little -> write_int32_le enc (Int32.of_int masked)
      | BF_U32 Big -> write_int32_be enc (Int32.of_int masked));
      ctx
  | Unit -> ctx
  | All_bytes ->
      write_string enc v;
      ctx
  | All_zeros ->
      write_string enc v;
      ctx
  | Where { inner; _ } -> encode_with_ctx ctx inner v enc
  | Array { elem; _ } ->
      List.fold_left
        (fun ctx' elem_v -> encode_with_ctx ctx' elem elem_v enc)
        ctx v
  | Byte_array _ ->
      write_string enc v;
      ctx
  | Byte_slice _ ->
      let src = Slice.bytes v in
      let off = Slice.first v in
      let len = Slice.length v in
      write_string enc (Bytes.sub_string src off len);
      ctx
  | Single_elem { elem; _ } -> encode_with_ctx ctx elem v enc
  | Enum { base; _ } -> encode_with_ctx ctx base v enc
  | Map { inner; encode; _ } -> encode_with_ctx ctx inner (encode v) enc
  | Casetype _ -> failwith "casetype encoding: use Record module"
  | Struct _ -> failwith "struct encoding: use Record module"
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

let encode typ v writer =
  let enc = encoder writer in
  ignore (encode_with_ctx empty_ctx typ v enc);
  flush enc

(* Direct-to-bytes encode: no Writer, no Buffer, no encoder.
   For fixed-size types, allocates only the output bytes. *)
let rec encode_direct : type a. a typ -> bytes -> int -> a -> int =
 fun typ buf off v ->
  match typ with
  | Uint8 ->
      Bytes.set_uint8 buf off v;
      off + 1
  | Uint16 Little ->
      Bytes.set_uint16_le buf off v;
      off + 2
  | Uint16 Big ->
      Bytes.set_uint16_be buf off v;
      off + 2
  | Uint32 Little ->
      UInt32.set_le buf off v;
      off + 4
  | Uint32 Big ->
      UInt32.set_be buf off v;
      off + 4
  | Uint63 Little ->
      UInt63.set_le buf off v;
      off + 8
  | Uint63 Big ->
      UInt63.set_be buf off v;
      off + 8
  | Uint64 Little ->
      Bytes.set_int64_le buf off v;
      off + 8
  | Uint64 Big ->
      Bytes.set_int64_be buf off v;
      off + 8
  | Bits { width; base } -> (
      let mask = (1 lsl width) - 1 in
      let masked = v land mask in
      match base with
      | BF_U8 ->
          Bytes.set_uint8 buf off masked;
          off + 1
      | BF_U16 Little ->
          Bytes.set_uint16_le buf off masked;
          off + 2
      | BF_U16 Big ->
          Bytes.set_uint16_be buf off masked;
          off + 2
      | BF_U32 Little ->
          Bytes.set_int32_le buf off (Int32.of_int masked);
          off + 4
      | BF_U32 Big ->
          Bytes.set_int32_be buf off (Int32.of_int masked);
          off + 4)
  | Unit -> off
  | Map { inner; encode; _ } -> encode_direct inner buf off (encode v)
  | Where { inner; _ } -> encode_direct inner buf off v
  | Enum { base; _ } -> encode_direct base buf off v
  | _ ->
      (* Variable-size: fall back to streaming encoder *)
      let tmp = Buffer.create 64 in
      let writer = Writer.of_buffer tmp in
      let enc = encoder writer in
      ignore (encode_with_ctx empty_ctx typ v enc);
      flush enc;
      let s = Buffer.contents tmp in
      let len = String.length s in
      Bytes.blit_string s 0 buf off len;
      off + len

let encode_to_bytes typ v =
  match field_wire_size typ with
  | Some n ->
      let buf = Bytes.create n in
      ignore (encode_direct typ buf 0 v);
      buf
  | None ->
      let buf = Buffer.create 64 in
      let writer = Writer.of_buffer buf in
      encode typ v writer;
      Buffer.to_bytes buf

let encode_to_string typ v =
  match field_wire_size typ with
  | Some n ->
      let buf = Bytes.create n in
      ignore (encode_direct typ buf 0 v);
      Bytes.unsafe_to_string buf
  | None ->
      let buf = Buffer.create 64 in
      let writer = Writer.of_buffer buf in
      encode typ v writer;
      Buffer.contents buf

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

(** Context for zero-allocation encoding. The [buffer] is shared and reused
    between calls to [encode]. Copy the bytes before the next [encode] if you
    need to keep them. *)

(** Build a direct field reader that reads at a fixed offset. No tuples, no refs
    \- just pure value read. Caller must ensure the buffer is large enough. *)
let rec build_field_reader : type a. a typ -> int -> bytes -> int -> a =
 fun typ field_off ->
  match typ with
  | Uint8 -> fun buf base -> Bytes.get_uint8 buf (base + field_off)
  | Uint16 Little -> fun buf base -> Bytes.get_uint16_le buf (base + field_off)
  | Uint16 Big -> fun buf base -> Bytes.get_uint16_be buf (base + field_off)
  | Uint32 Little -> fun buf base -> UInt32.get_le buf (base + field_off)
  | Uint32 Big -> fun buf base -> UInt32.get_be buf (base + field_off)
  | Uint63 Little -> fun buf base -> UInt63.get_le buf (base + field_off)
  | Uint63 Big -> fun buf base -> UInt63.get_be buf (base + field_off)
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

(* ==================== EverParse FFI Helpers ==================== *)

(* NOTE: wire does NOT generate C parsing code. C parsers come from EverParse.
   This section provides helpers for generating OCaml FFI stubs that call
   EverParse-generated C code.

   Workflow:
   1. Define schema in OCaml using wire
   2. Generate .3d file with to_3d
   3. Run EverParse to generate C parser (.h with struct + read/write)
   4. Use to_c_stubs to generate OCaml FFI bindings to call EverParse C *)

(** Compute the fixed wire size of a struct (None if variable-length) *)
let rec size_of_typ : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Uint32 _ -> Some 4
  | Uint64 _ -> Some 8
  | Byte_array { size = Int n } | Byte_slice { size = Int n } -> Some n
  | Enum { base; _ } -> size_of_int_typ base
  | Where { inner; _ } -> size_of_typ inner
  | Map { inner; _ } -> size_of_typ inner
  | _ -> None

and size_of_int_typ : int typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | Enum { base; _ } -> size_of_int_typ base
  | Where { inner; _ } -> size_base inner
  | _ -> None

and size_base : type a. a typ -> int option = function
  | Uint8 -> Some 1
  | Uint16 _ -> Some 2
  | _ -> None

let size_of_struct (s : struct_) =
  List.fold_left
    (fun acc (Field f) ->
      match (acc, size_of_typ f.field_typ) with
      | Some a, Some b -> Some (a + b)
      | _ -> None)
    (Some 0) s.fields

(** Compute the EverParse-normalized identifier for a struct name.

    EverParse 3D normalizes C identifiers: names that start with two or more
    consecutive uppercase letters are lowercased with only the first letter
    capitalized (e.g., [CLCW] becomes [Clcw], [TMFrame] becomes [Tmframe]).
    Names with standard camelCase are preserved (e.g., [AllInts] stays
    [AllInts]). *)
let everparse_name name =
  let is_upper c = Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c in
  let len = String.length name in
  let rec count_upper i =
    if i < len && is_upper name.[i] then count_upper (i + 1) else i
  in
  if len > 0 && count_upper 0 >= 2 then
    String.init len (fun i ->
        let c = Char.lowercase_ascii name.[i] in
        if i = 0 then Char.uppercase_ascii c else c)
  else name

(** OCaml type name for a wire type (for generated external declarations). *)
let rec ml_type_of : type a. a typ -> string = function
  | Uint8 -> "int"
  | Uint16 _ -> "int"
  | Uint32 _ -> "int32"
  | Uint64 _ -> "int64"
  | Bits _ -> "int"
  | Enum { base; _ } -> ml_type_of_int base
  | Where { inner; _ } -> ml_type_of inner
  | Map { inner; _ } -> ml_type_of inner
  | Apply { typ; _ } -> ml_type_of typ
  | _ -> failwith "ml_type_of: unsupported type"

and ml_type_of_int : int typ -> string = function
  | Uint8 -> "int"
  | Uint16 _ -> "int"
  | Enum { base; _ } -> ml_type_of_int base
  | Where { inner; _ } -> ml_type_of_int inner
  | Map { inner; _ } -> ml_type_of inner
  | _ -> failwith "ml_type_of_int: unsupported type"

(** Generate C check stub: [bytes -> bool]. Calls EverParse-generated
    [FooCheckFoo] validation function. *)
let c_stub_check ppf (s : struct_) =
  let ep = everparse_name s.name in
  let lower = String.lowercase_ascii s.name in
  (* Define a static error handler for this schema *)
  Fmt.pf ppf
    "static void %s_err(const char *t, const char *f, const char *r,@\n" lower;
  Fmt.pf ppf
    "  uint64_t c, uint8_t *ctx, EVERPARSE_INPUT_BUFFER i, uint64_t p) {@\n";
  Fmt.pf ppf
    "  (void)t; (void)f; (void)r; (void)c; (void)ctx; (void)i; (void)p;@\n";
  Fmt.pf ppf "}@\n";
  (* Call Validate directly instead of going through the Wrapper.
     No OCaml allocation or GC interaction, so skip CAMLparam/CAMLreturn. *)
  Fmt.pf ppf "CAMLprim value caml_wire_%s_check(value v_buf) {@\n" lower;
  Fmt.pf ppf "  uint8_t *data = (uint8_t *)Bytes_val(v_buf);@\n";
  Fmt.pf ppf "  uint32_t len = caml_string_length(v_buf);@\n";
  Fmt.pf ppf "  uint64_t r = %sValidate%s(NULL, %s_err, data, len, 0);@\n" ep ep
    lower;
  Fmt.pf ppf "  return Val_bool(EverParseIsSuccess(r));@\n";
  Fmt.pf ppf "}@\n@\n"

(** Generate C FFI stubs that call EverParse-generated validators.

    For each struct [Foo], generates:
    - Validation stub: [caml_wire_foo_check(v_buf)] calling [Validate] directly

    The generated code expects EverParse headers and sources to be available via
    [-I] include path. EverParse identifier normalization is handled
    automatically (e.g., [CLCW] becomes [ClcwCheckClcw]). *)
let to_c_stubs (structs : struct_ list) =
  let buf = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf
    "/* wire_stubs.c - OCaml FFI stubs for EverParse-generated C */@\n@\n";
  Fmt.pf ppf "#include <caml/mlvalues.h>@\n";
  Fmt.pf ppf "#include <caml/memory.h>@\n";
  Fmt.pf ppf "#include <stdint.h>@\n@\n";
  Fmt.pf ppf "/* EverParse headers and sources */@\n";
  List.iteri
    (fun i (s : struct_) ->
      if i = 0 then Fmt.pf ppf "#include \"EverParse.h\"@\n";
      Fmt.pf ppf "#include \"%s.h\"@\n" s.name;
      Fmt.pf ppf "#include \"%s.c\"@\n" s.name)
    structs;
  Fmt.pf ppf "@\n/* Validation stubs */@\n";
  List.iter (fun (s : struct_) -> c_stub_check ppf s) structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(** Generate OCaml [external] declarations matching the C stubs from
    {!to_c_stubs}. For each struct [Foo], generates:
    {[
      external foo_check : bytes -> bool = "caml_wire_foo_check"
    ]} *)
let to_ml_stubs (structs : struct_ list) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  List.iter
    (fun (s : struct_) ->
      let lower = String.lowercase_ascii s.name in
      Fmt.pf ppf "external %s_check : bytes -> bool@\n" lower;
      Fmt.pf ppf "  = \"caml_wire_%s_check\" [@@@@noalloc]@\n@\n" lower)
    structs;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(** Module name for a generated per-struct OCaml stub file. Converts CamelCase
    to snake_case, e.g., [SimpleHeader] maps to ["simple_header"]. *)
let to_ml_stub_name (s : struct_) =
  let name = s.name in
  let buf = Buffer.create (String.length name + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && Char.uppercase_ascii c = c && Char.lowercase_ascii c <> c then
        Buffer.add_char buf '_';
      Buffer.add_char buf (Char.lowercase_ascii c))
    name;
  Buffer.contents buf

(** Generate a flat OCaml stub module for a single struct. Produces a file with
    an [external check] declaration:
    {[
      (* Generated by wire *)
      external check : bytes -> bool = "caml_wire_foo_check"
    ]} *)
let to_ml_stub (s : struct_) =
  let buf = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer buf in
  let lower = String.lowercase_ascii s.name in
  Fmt.pf ppf "(* Generated by wire (do not edit) *)@\n@\n";
  Fmt.pf ppf "external check : bytes -> bool@\n";
  Fmt.pf ppf "  = \"caml_wire_%s_check\"@\n" lower;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* ==================== Struct-level read/write ==================== *)

type parsed_value = Parsed : 'a typ * 'a -> parsed_value
type parsed_struct = (string option * parsed_value) list

let read_struct (s : struct_) buf =
  let reader = Reader.of_string buf in
  let dec = decoder reader in
  let rec go ctx acc = function
    | [] -> Ok (List.rev acc)
    | Field { field_name; field_typ; constraint_; _ } :: rest -> (
        let v, ctx' = parse_with dec ctx field_typ in
        let ctx' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int field_typ v) ctx'
          | None -> ctx'
        in
        match constraint_ with
        | Some cond when not (eval_expr ctx' cond) ->
            Error (Constraint_failed "field constraint")
        | _ -> go ctx' ((field_name, Parsed (field_typ, v)) :: acc) rest)
  in
  match go empty_ctx [] s.fields with
  | r -> r
  | exception Parse_exn e -> Error e

let write_struct (s : struct_) (ps : parsed_struct) =
  let buf = Buffer.create 64 in
  let writer = Writer.of_buffer buf in
  let enc = encoder writer in
  let fields_with_vals = List.combine s.fields ps in
  let rec go ctx = function
    | [] ->
        flush enc;
        Ok (Buffer.contents buf)
    | (Field { field_name; constraint_; _ }, (_, Parsed (typ, v))) :: rest -> (
        let ctx' = encode_with_ctx ctx typ v enc in
        let ctx' =
          match field_name with
          | Some n -> Ctx.add n (val_to_int typ v) ctx'
          | None -> ctx'
        in
        match constraint_ with
        | Some cond when not (eval_expr ctx' cond) ->
            Error (Constraint_failed "field constraint")
        | _ -> go ctx' rest)
  in
  go empty_ctx fields_with_vals

(* Capture top-level names before Codec shadows them *)
type struct_field = field

let struct_field = field
let struct' = struct_

module Codec = struct
  (* Type equality witness for GADT-safe accessor setting *)
  type (_, _) eq = Refl : ('a, 'a) eq

  type ('a, 'r) field = {
    name : string;
    typ : 'a typ;
    get : 'r -> 'a;
    mutable f_reader : bytes -> int -> 'a;
    mutable f_writer : bytes -> int -> 'a -> unit;
  }

  (* GADT snoc-list of typed field readers, built in forward order by |+.
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
     reading previously-declared fields from the buffer. Built once at [|+]
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
        r_fields_rev : struct_field list;
        r_bf : bf_codec_state option;
        r_configurators_rev : (unit -> unit) list;
        r_field_readers : (string * (bytes -> int -> int)) list;
            (* name -> int reader, for evaluating Ref expressions *)
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
    t_struct_fields : struct_field list;
  }

  let record_start name make =
    Record
      {
        r_name = name;
        r_make = make;
        r_readers = Nil;
        r_writers_rev = [];
        r_min_wire_size = 0;
        r_next_off = Static_next 0;
        r_fields_rev = [];
        r_bf = None;
        r_configurators_rev = [];
        r_field_readers = [];
      }

  let field name typ get =
    let not_ready _ _ = failwith "field: not added to a record yet" in
    {
      name;
      typ;
      get;
      f_reader = not_ready;
      f_writer = (fun _ _ _ -> failwith "field: not added to a record yet");
    }

  (* Bitfield helpers *)

  let bf_base_byte_size = function BF_U8 -> 1 | BF_U16 _ -> 2 | BF_U32 _ -> 4

  let bf_base_total_bits = function
    | BF_U8 -> 8
    | BF_U16 _ -> 16
    | BF_U32 _ -> 32

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
        fun buf off ->
          (Bytes.get_uint8 buf (off + byte_off) lsr shift) land mask
    | BF_U16 Little ->
        fun buf off ->
          (Bytes.get_uint16_le buf (off + byte_off) lsr shift) land mask
    | BF_U16 Big ->
        fun buf off ->
          (Bytes.get_uint16_be buf (off + byte_off) lsr shift) land mask
    | BF_U32 Little ->
        fun buf off -> (UInt32.get_le buf (off + byte_off) lsr shift) land mask
    | BF_U32 Big ->
        fun buf off -> (UInt32.get_be buf (off + byte_off) lsr shift) land mask

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
          let cur = UInt32.get_le buf (off + byte_off) in
          UInt32.set_le buf (off + byte_off)
            (cur lor ((value land mask) lsl shift))
    | BF_U32 Big ->
        fun buf off value ->
          let cur = UInt32.get_be buf (off + byte_off) in
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
          let cur = UInt32.get_le buf (off + byte_off) in
          UInt32.set_le buf (off + byte_off)
            (cur land clear_mask lor ((value land mask) lsl shift))
    | BF_U32 Big ->
        fun buf off value ->
          let cur = UInt32.get_be buf (off + byte_off) in
          UInt32.set_be buf (off + byte_off)
            (cur land clear_mask lor ((value land mask) lsl shift))

  let build_bf_clear base byte_off =
   fun buf off -> bf_write_base base buf (off + byte_off) 0

  let ( |+ ) : type a f r. (a -> f, r) record -> (a, r) field -> (f, r) record =
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
                  "Codec.(|+): bitfields after variable-size fields not \
                   supported"
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
              r_fields_rev = struct_field name typ :: r.r_fields_rev;
              r_bf = Some new_bf;
              r_configurators_rev = configurator :: r.r_configurators_rev;
              r_field_readers = (name, int_reader) :: r.r_field_readers;
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
              let field_readers =
                match field_off_static with
                | Some fo -> (
                    match typ with
                    | Uint8 ->
                        (name, fun buf base -> Bytes.get_uint8 buf (base + fo))
                        :: r.r_field_readers
                    | Uint16 Little ->
                        ( name,
                          fun buf base -> Bytes.get_uint16_le buf (base + fo) )
                        :: r.r_field_readers
                    | Uint16 Big ->
                        ( name,
                          fun buf base -> Bytes.get_uint16_be buf (base + fo) )
                        :: r.r_field_readers
                    | Uint32 Little ->
                        (name, fun buf base -> UInt32.get_le buf (base + fo))
                        :: r.r_field_readers
                    | Uint32 Big ->
                        (name, fun buf base -> UInt32.get_be buf (base + fo))
                        :: r.r_field_readers
                    | _ -> r.r_field_readers)
                | None -> r.r_field_readers
              in
              let new_next_off =
                match r.r_next_off with
                | Static_next n -> Static_next (n + fsize)
                | Dynamic_next f ->
                    Dynamic_next (fun buf base -> f buf base + fsize)
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
                  r_fields_rev = struct_field name typ :: r.r_fields_rev;
                  r_bf = None;
                  r_configurators_rev = configurator :: r.r_configurators_rev;
                  r_field_readers = field_readers;
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
                    invalid_arg
                      "Codec.(|+): unsupported variable-size field type"
              in
              let size_fn = compile_expr r.r_field_readers size_expr in
              let field_off =
                match field_off_static with
                | Some n -> n
                | None ->
                    invalid_arg
                      "Codec.(|+): multiple variable-size fields not yet \
                       supported"
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
                    Bytes.blit_string s 0 buf (base + field_off)
                      (String.length s)
                | _ -> assert false
              in
              let writer = raw_writer typ in
              let configurator () =
                fld.f_reader <- wrap_reader reader;
                fld.f_writer <- wrap_writer writer
              in
              let new_next_off =
                Dynamic_next
                  (fun buf base -> base + field_off + size_fn buf base)
              in
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
                  r_fields_rev = struct_field name typ :: r.r_fields_rev;
                  r_bf = None;
                  r_configurators_rev = configurator :: r.r_configurators_rev;
                  r_field_readers = r.r_field_readers;
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
    | FCons
        (r1, FCons (r2, FCons (r3, FCons (r4, FCons (r5, FCons (r6, FNil))))))
      ->
        f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off)
    | FCons
        ( r1,
          FCons
            ( r2,
              FCons (r3, FCons (r4, FCons (r5, FCons (r6, FCons (r7, FNil)))))
            ) ) ->
        f (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off) (r5 buf off)
          (r6 buf off) (r7 buf off)
    | FCons
        ( r1,
          FCons
            ( r2,
              FCons
                ( r3,
                  FCons
                    (r4, FCons (r5, FCons (r6, FCons (r7, FCons (r8, rest)))))
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
    let build_decode : type full. full -> (full, r) readers -> bytes -> int -> r
        =
     fun make readers ->
      match readers with
      | Nil -> fun _buf _off -> make
      | Snoc (Nil, r1) -> fun buf off -> make (r1 buf off)
      | Snoc (Snoc (Nil, r1), r2) ->
          fun buf off -> make (r1 buf off) (r2 buf off)
      | Snoc (Snoc (Snoc (Nil, r1), r2), r3) ->
          fun buf off -> make (r1 buf off) (r2 buf off) (r3 buf off)
      | Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
      | Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off)
      | Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off)
      | Snoc
          ( Snoc (Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5), r6),
            r7 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off)
      | Snoc
          ( Snoc
              ( Snoc
                  ( Snoc (Snoc (Snoc (Snoc (Snoc (Nil, r1), r2), r3), r4), r5),
                    r6 ),
                r7 ),
            r8 ) ->
          fun buf off ->
            make (r1 buf off) (r2 buf off) (r3 buf off) (r4 buf off)
              (r5 buf off) (r6 buf off) (r7 buf off) (r8 buf off)
      | readers ->
          (* 9+ fields: convert to forward list at seal time, apply at decode
             time. Cost: ceil(n/8) - 1 partial applications. *)
          let fwd = to_fwd readers in
          fun buf off -> apply_fwd make fwd buf off
    in
    let raw_decode = build_decode r.r_make r.r_readers in
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
    }

  (* Heterogeneous field list. [] seals the view; (::) adds a field.
     Tracks the constructor type: ('a -> 'b -> 'r, 'r) matches a
     constructor (fun a b -> ...). *)
  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  let view : type f r. string -> f -> (f, r) fields -> r t =
   fun name constructor flds ->
    let rec add : type g. (g, r) record -> (g, r) fields -> r t =
     fun r flds ->
      match flds with [] -> seal r | f :: rest -> add (r |+ f) rest
    in
    add (record_start name constructor) flds

  let wire_size t =
    match t.t_wire_size with
    | Fixed n -> n
    | Variable _ ->
        invalid_arg
          "Codec.wire_size: variable-size codec (use min_wire_size or \
           compute_wire_size instead)"

  let min_wire_size t =
    match t.t_wire_size with
    | Fixed n -> n
    | Variable { min_size; _ } -> min_size

  let compute_wire_size t buf off =
    match t.t_wire_size with
    | Fixed n -> n
    | Variable { compute; _ } -> compute buf off - off

  let is_fixed t =
    match t.t_wire_size with Fixed _ -> true | Variable _ -> false

  let decode t buf off = t.t_decode buf off
  let encode t v buf off = t.t_encode v buf off
  let to_struct t = struct' t.t_name t.t_struct_fields

  let get (type a r) (_codec : r t) (f : (a, r) field) :
      (bytes -> int -> a) Staged.t =
    Staged.stage f.f_reader

  let set (type a r) (_codec : r t) (f : (a, r) field) :
      (bytes -> int -> a -> unit) Staged.t =
    Staged.stage f.f_writer

  let ref (type a r) (f : (a, r) field) : int expr = Ref f.name
end
