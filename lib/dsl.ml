type endian = Little | Big
type bit_order = Msb_first | Lsb_first

let equal_endian a b =
  match (a, b) with
  | Little, Little | Big, Big -> true
  | Little, Big | Big, Little -> false

let equal_bit_order a b =
  match (a, b) with
  | Msb_first, Msb_first | Lsb_first, Lsb_first -> true
  | Msb_first, Lsb_first | Lsb_first, Msb_first -> false

(* Sequence builder for Array/Repeat -- Jsont-style accumulator pattern.
   Existentially hides the builder type so callers control the output container. *)
type ('elt, 'seq) seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

(* Param handles -- defined here so expr and action_stmt can reference them *)
type param_input
type param_output
type bindings = { param : string -> int; set_param : string -> int -> unit }

(* The buffer travels as its own argument rather than inside the context, so
   the parameter-free hot path is the constant constructor [Unbound]. Being a
   constant, it is an immediate: an encode or decode without parameters builds
   no context block and allocates nothing. [Checked] carries the same bindings
   over bytes a check pass has accepted; [checked_unbound] is a static block,
   so a parameter-free read through it allocates nothing either. *)
type eval_ctx = Unbound | Bound of bindings | Checked of eval_ctx

let unbound_eval_ctx = Unbound
let eval_ctx ?(set_param = fun _ _ -> ()) param = Bound { param; set_param }
let checked_unbound = Checked Unbound

let checked_eval_ctx = function
  | Unbound -> checked_unbound
  | Checked _ as ctx -> ctx
  | Bound _ as ctx -> Checked ctx

let is_checked = function Checked _ -> true | Unbound | Bound _ -> false

let rec eval_param ctx name =
  match ctx with
  | Unbound -> 0
  | Bound b -> b.param name
  | Checked ctx -> eval_param ctx name

let rec eval_set_param ctx name value =
  match ctx with
  | Unbound -> ()
  | Bound b -> b.set_param name value
  | Checked ctx -> eval_set_param ctx name value

(* Parse errors, defined before [typ] so [typ]'s own [Where] / [Field]
   constructors win type-directed disambiguation everywhere below; the
   [predicate] constructors are reached only through a [predicate]-typed
   context (a [~which] argument or an annotated match). *)

(* Which predicate a [Constraint_failed] came from: a field [~self_constraint]
   or [~constraint_] ([Field]), a codec/typ [~where] ([Where]), a field
   [~action] ([Action]), or a [byte_array_where] per-byte refinement
   ([Per_byte]). *)
type predicate = Errors.predicate = Where | Field | Action | Per_byte
type consumption = [ `Prefix | `All ]

type error_kind = Errors.error_kind =
  | Unexpected_eof of { expected : int; got : int }
  | Trailing_bytes of int
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | Missing_terminator
  | Non_zero_padding
  | Value_out_of_range of { value : int64 }
  | Zero_divisor
  | Constraint_failed of { which : predicate; value : int64 option }

(* [at] is the absolute byte offset of the failing field in the input; [field]
   is the root-to-leaf path of field names to it (e.g. ["header"; "vcid"]),
   empty at a top-level or anonymous position. *)
type parse_error = Errors.parse_error = {
  at : int;
  field : string list;
  kind : error_kind;
}

exception Parse_error = Errors.Parse_error

let parse_error ?(at = 0) ?(field = []) kind = { at; field; kind }

let eof ?(at = 0) ?(field = []) ~expected ~got () =
  { at; field; kind = Unexpected_eof { expected; got } }

let err ~at kind = parse_error ~at kind
let raise_error ~at kind = raise (Parse_error (err ~at kind))

let check_consumption mode ~consumed ~available =
  match mode with
  | `Prefix -> ()
  | `All when consumed = available -> ()
  | `All -> raise_error ~at:consumed (Trailing_bytes (available - consumed))

let raise_eof ~at ~expected ~got =
  raise_error ~at (Unexpected_eof { expected; got })

let raise_invalid_tag ~at tag = raise_error ~at (Invalid_tag tag)

(* A [Map]'s [decode] is handed a value and nothing else, so a lookup that
   rejects an out-of-range index has no offset to name and reports 0. Every
   reader knows where the value it just read sits, and [at] is documented as
   the failing field's absolute offset, so relocate the failure there: a caller
   that seeks to [at] otherwise lands on the start of the buffer whatever the
   frame's base. [index_bound] marks exactly the maps whose [decode] rejects
   anything, so every other map keeps the bare call. *)
let map_decode ~index_bound decode =
  match index_bound with
  | None -> fun ~at:_ v -> decode v
  | Some _ -> (
      fun ~at v ->
        try decode v with Parse_error e -> raise (Parse_error { e with at }))

(* [int_of_exn] is handed a value and nothing else, so an out-of-range one has
   no offset to name and reports 0, the same blind spot [map_decode] covers for
   a lookup. A reader knows where it found the value, so relocate the failure
   there; only a carrier wider than the native int can raise, and the handler is
   installed only for those, so a byte- or word-wide read pays nothing. *)
let relocate_at ~at f =
  try f () with Parse_error e -> raise (Parse_error { e with at })

let raise_invalid_enum ~at ~value ~valid =
  raise_error ~at (Invalid_enum { value; valid })

let raise_missing_terminator ~at = raise_error ~at Missing_terminator
let raise_non_zero_padding ~at = raise_error ~at Non_zero_padding

let raise_out_of_range ~at value =
  raise_error ~at (Value_out_of_range { value })

let raise_zero_divisor ~at = raise_error ~at Zero_divisor

let raise_constraint ~at ~which ?value () =
  raise_error ~at (Constraint_failed { which; value })

type ('a, 'k) param_handle = {
  id : int;
  name : string;
  typ : 'a typ;
  packed_typ : packed_typ;
  mutable_ : bool;
}

and packed_typ = Pack_typ : 'a typ -> packed_typ

(* Expressions *)
and _ ref_kind = I : int ref_kind | I64 : int64 ref_kind

and _ expr =
  | Int : int -> int expr
  | Int64 : int64 -> int64 expr
  | Bool : bool -> bool expr
  | Ref : 'a ref_kind * string -> 'a expr
  | Param_ref : ('a, 'k) param_handle -> int expr
  | Sizeof : 'a typ -> int expr
  | Sizeof_this : int expr
  | Field_pos : int expr
  | Add : int expr * int expr -> int expr
  | Sub : int expr * int expr -> int expr
  | Mul : int expr * int expr -> int expr
  | Div : int expr * int expr -> int expr
  | Mod : int expr * int expr -> int expr
  | Land : int expr * int expr -> int expr
  | Lor : int expr * int expr -> int expr
  | Lxor : int expr * int expr -> int expr
  | Lnot : int expr -> int expr
  | Lsl : int expr * int expr -> int expr
  | Lsr : int expr * int expr -> int expr
  | Land64 : int64 expr * int64 expr -> int64 expr
      (** Bitwise AND on full-width [int64] operands, for masking a [uint64]
          field before a comparison (the native-[int] [Land] cannot, since it
          truncates the field). Projects to 3D as [&]. *)
  | Lsr64 : int64 expr * int expr -> int64 expr
      (** Logical shift right on a full-width [int64] operand, for isolating
          high bits (a float64 exponent) that a native-[int] shift would lose on
          a narrow-int platform. Projects to 3D as [>>]. *)
  | Eq : 'a expr * 'a expr -> bool expr
  | Ne : 'a expr * 'a expr -> bool expr
  | Lt : 'a expr * 'a expr -> bool expr
  | Le : 'a expr * 'a expr -> bool expr
  | Gt : 'a expr * 'a expr -> bool expr
  | Ge : 'a expr * 'a expr -> bool expr
  | And : bool expr * bool expr -> bool expr
  | Or : bool expr * bool expr -> bool expr
  | Not : bool expr -> bool expr
  | Cast : [ `U8 | `U16 | `U32 | `U64 ] * int expr -> int expr
  | If_then_else : bool expr * int expr * int expr -> int expr

(* Bitfield base types - standalone, not mutually recursive *)
and bitfield_base = U8 | U16 of endian | U32 of endian

(* Types *)
and _ typ =
  | Uint8 : UInt8.t typ
  | Uint16 : endian -> UInt16.t typ
  | Uint32 : endian -> UInt32.t typ
  | Uint64 : endian -> UInt64.t typ (* boxed, for full 64-bit *)
  | Int8 : SInt8.t typ
  | Int16 : endian -> SInt16.t typ
  | Int32 : endian -> SInt32.t typ
  | Int64 : endian -> int64 typ
  | Float32 :
      endian
      -> float typ (* IEEE 754 binary32, widened to OCaml float *)
  | Float64 : endian -> float typ (* IEEE 754 binary64 *)
  | Uint_var : { size : int expr; endian : endian } -> UInt63.t typ
  | Bits : {
      width : int;
      base : bitfield_base;
      bit_order : bit_order;
    }
      -> int typ
  | Unit : unit typ
  | All_bytes : string typ
  | All_zeros : string typ
  | Zeroterm : string typ
  | Zeroterm_at_most : { size : int expr } -> string typ
  | Where : { cond : bool expr; inner : 'a typ } -> 'a typ
  | Array : {
      len : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ
  | Byte_array : { size : int expr } -> string typ
  | Byte_array_where : {
      size : int expr;
      elt_var : string;
      cond : bool expr;
    }
      -> string typ
  | Byte_slice : { size : int expr } -> Bytesrw.Bytes.Slice.t typ
  | Single_elem : { size : int expr; elem : 'a typ; at_most : bool } -> 'a typ
  | Enum : {
      name : string;
      cases : (string * int) list;
      base : 'a typ;
          (* Any type with an integer view: the case values name integers, and
             [int_of_exn] is what reads one out of a decoded [base] value. *)
      closed : bool;
          (* [true]: only the listed values are valid (decode rejects others, the
             3D projection emits a membership refinement). [false]: an open set,
             the names document known values but any value is accepted. *)
    }
      -> 'a typ
  | Casetype : {
      name : string;
      tag : 'k typ;
      cases : ('a, 'k) case_branch list;
    }
      -> 'a typ
  | Union : {
      name : string;
      tag : 'k typ;
          (* The type of the field [on] reads: what the case indices are
             written in, and the parameter type of the 3D casetype. *)
      on : int expr;
      cases : ('a, 'k) case_branch list;
    }
      -> 'a typ
    (* A casetype whose tag is not its own first bytes but an earlier field
         of the enclosing struct. The union occupies the selected body only. *)
  | Struct : struct_ -> unit typ
  | Type_ref : string -> 'a typ
  | Qualified_ref : { module_ : string; name : string } -> 'a typ
  | Map : {
      inner : 'w typ;
      decode : 'w -> 'a;
      encode : 'a -> 'w;
      index_bound : int option;
      map_id : int;
          (* Each map value its own: a generated module names the map it calls
             and checks the codec still holds that very one. *)
          (* When [Some n] the raw [inner] value is a valid index only when
             [< n] (set by {!cases} / lookup). [decode] enforces this on the
             OCaml side; the 3D projection emits it as a field refinement so the
             generated C validator rejects the same out-of-range inputs. *)
    }
      -> 'a typ
  | Apply : { typ : 'a typ; args : packed_expr list } -> 'a typ
  | Codec : {
      codec_name : string;
      codec_decode : eval_ctx -> Input_end.t -> bytes -> int -> 'r;
      codec_validate : eval_ctx -> Input_end.t -> bytes -> int -> unit;
          (* Everything [codec_decode] checks, none of what it builds. The
             OCaml counterpart of the call to a nested struct's validator that
             EverParse generates for a sub-struct field: a use site runs the
             sub-codec's constraints, [where], enum membership, span refinements
             and actions without materialising the record. *)
      codec_validate_sized :
        eval_ctx -> Input_end.t -> bytes -> int -> int -> unit;
          (* [codec_validate] of a record whose end, the last argument, the
             caller has just measured with [codec_size_of] over the same
             bytes: the frame checks that end instead of measuring again. *)
      codec_encode : 'r -> eval_ctx -> bytes -> int -> int;
          (* Returns the offset after the bytes the encoder wrote. *)
      codec_fixed_size : int option;
      codec_min_size : int;
          (* Bytes the codec occupies whatever its variable-size fields hold,
             hence the extent [codec_size_of] has to be able to read before it
             can resolve a span. A decoder bounds this against the region it
             was given, so a region too short to carry the length fields fails
             on their own extent rather than on a span sized from bytes outside
             it. *)
      codec_size_of : eval_ctx -> Input_end.t -> bytes -> int -> int;
      codec_size_of_value : eval_ctx -> 'r -> int;
          (* Encoded byte length of a value, computed from the value rather
             than by re-reading the buffer. The buffer-driven [codec_size_of]
             is wrong for variable-size codecs ending in [all_bytes] /
             [rest_bytes] / [all_zeros]: it reads "remaining buffer space",
             not the value's actual tail length. *)
      codec_field_readers :
        (string * (eval_ctx -> Input_end.t -> bytes -> int -> int)) list;
      codec_returns_slice : bool;
          (* Whether a decoded value can retain a mutable byte slice. Computed
             once from [codec_struct] when the codec typ is constructed. *)
      codec_struct : struct_;
          (** Structural representation of the codec. Mirrors [codec_decode] /
              [codec_encode] but in a form 3D projection can walk. *)
    }
      -> 'r typ
  | Optional : { present : bool expr; inner : 'a typ } -> 'a option typ
  | Optional_or : {
      present : bool expr;
      inner : 'a typ;
      default : 'a;
    }
      -> 'a typ
  | Repeat : {
      size : int expr;
      elem : 'a typ;
      seq : ('a, 'seq) seq_map;
    }
      -> 'seq typ

and ('a, 'k) case_branch =
  | Case_branch : {
      cb_tag : 'k option;
          (* Decode-match tag: [Some t] for an explicit case, [None] for the
             default branch (matches any unclaimed tag). *)
      cb_inner : 'w typ;
      cb_inject : 'k -> 'w -> 'a;
          (* Receives the matched tag (so a default branch can recover the
             actual tag it caught) and the decoded body. *)
      cb_project : 'a -> ('k * 'w) option;
          (* Yields the tag to encode and the body. An explicit case pairs its
             own fixed tag; the default branch yields the tag carried by the
             value, so an arbitrary unclaimed tag round-trips. *)
    }
      -> ('a, 'k) case_branch

and packed_expr = Pack_expr : 'a expr -> packed_expr

(* Structs *)
and struct_ = {
  name : string;
  params : param list;
  where : bool expr option;
  fields : field list;
}

and field =
  | Field : {
      field_name : string option;
      field_typ : 'a typ;
      constraint_ : bool expr option;
      action : action option;
      field_doc : string option;
    }
      -> field

and param = { param_name : string; param_typ : packed_typ; mutable_ : bool }

(* Actions *)
and action = Success of action_stmt list | Act of action_stmt list

and action_stmt =
  | Assign : ('a, param_output) param_handle * int expr -> action_stmt
  | Field_assign of string * string * int expr
    (* [Field_assign (ptr, field_name, expr)] emits [ptr->field_name = expr;] *)
  | Extern_call of string * string list
    (* [Extern_call (fn, args)] emits [fn(arg1, arg2, ...);] *)
  | Return of bool expr
  | Abort
  | If of bool expr * action_stmt list * action_stmt list option
  | Var of string * int expr

type param_env = {
  codec_id : int;
  names : string array;
      (* Parallel to [slots]: the param name at each slot. [Param.bind] / [get]
         resolve a handle to its slot by name, so resolution is per-env (hence
         per-codec) rather than via a mutable field on the shared handle. *)
  slots : int array;
  bound : bool array;
      (* Parallel to [slots]: one bit per slot, set by [Param.bind] so
         encoders can reject envs that left an input param at its
         default-zero value. *)
}

(* Expression constructors *)
let int n = Int n
let true_ = Bool true
let false_ = Bool false
let ref name = Ref (I, name)
let sizeof t = Sizeof t
let sizeof_this = Sizeof_this
let field_pos = Field_pos

module Expr = struct
  let int64 n = Int64 n
  let ( + ) a b = Add (a, b)
  let ( - ) a b = Sub (a, b)
  let ( * ) a b = Mul (a, b)
  let ( / ) a b = Div (a, b)
  let ( mod ) a b = Mod (a, b)
  let ( land ) a b = Land (a, b)
  let land64 a b = Land64 (a, b)
  let lsr64 a b = Lsr64 (a, b)
  let ( lor ) a b = Lor (a, b)
  let ( lxor ) a b = Lxor (a, b)
  let lnot a = Lnot a
  let ( lsl ) a b = Lsl (a, b)
  let ( lsr ) a b = Lsr (a, b)
  let ( = ) a b = Eq (a, b)
  let ( <> ) a b = Ne (a, b)
  let ( < ) a b = Lt (a, b)
  let ( <= ) a b = Le (a, b)
  let ( > ) a b = Gt (a, b)
  let ( >= ) a b = Ge (a, b)
  let ( && ) a b = And (a, b)
  let ( || ) a b = Or (a, b)
  let not a = Not a
  let if_then_else c t e = If_then_else (c, t, e)
  let to_uint8 e = Cast (`U8, e)
  let to_uint16 e = Cast (`U16, e)
  let to_uint32 e = Cast (`U32, e)
  let to_uint64 e = Cast (`U64, e)
end

(* Constant size and length expressions are normalised to [Int n] at the
   construction boundary below. The rest of the library keys its fast paths and
   its range guards on a literal [Int n] size, so [Expr.(int 2 + int 2)] has to
   arrive as the same node as [int 4] or it silently falls off every one of
   them.

   Only reference-free arithmetic folds. A [Ref], [Param_ref], [Sizeof_this] or
   [Field_pos] resolves against a buffer at decode time, and [Sizeof] must stay
   symbolic so the 3D projection keeps emitting [sizeof(T)] rather than a
   baked-in width. *)

(* Native-int arithmetic that reports overflow instead of wrapping: a constant
   folded to a value its expression does not denote would be worse than leaving
   the expression symbolic. Generated code shares these with the codec. *)
let const_add a b =
  let s = a + b in
  if Arith.add_overflows a b s then None else Some s

let const_sub a b =
  let d = a - b in
  if Arith.sub_overflows a b d then None else Some d

let const_mul a b =
  let p = a * b in
  if Arith.mul_overflows a b p then None else Some p

let checked_add = Arith.checked_add
let checked_sub = Arith.checked_sub
let checked_mul = Arith.checked_mul
let[@inline] checked_div a b = if b = 0 then raise_zero_divisor ~at:0 else a / b

let[@inline] checked_mod a b =
  if b = 0 then raise_zero_divisor ~at:0 else a mod b

(* [min_int / -1] is the one division whose true result is not representable;
   OCaml returns [min_int] for it rather than trapping. *)
let const_div a b =
  if b = 0 || (a = min_int && b = -1) then None else Some (a / b)

let const_rem a b =
  if b = 0 || (a = min_int && b = -1) then None else Some (a mod b)

let const_shift_left a b =
  if b >= 0 && b < Sys.int_size && (a lsl b) asr b = a then Some (a lsl b)
  else None

let const_shift_right a b =
  if b >= 0 && b < Sys.int_size then Some (a lsr b) else None

let const_cast width v =
  match width with
  | `U8 -> v land 0xFF
  | `U16 -> v land 0xFFFF
  | `U32 -> v land UInt32.mask32
  | `U64 -> v

(* The arithmetic is shared between two readers of a size expression: constant
   folding at construction, where no leaf resolves, and value-driven sizing at
   encode, where a bound parameter does. [leaf] is what each makes of the
   irreducible nodes. *)
let rec int_expr_with : (int expr -> int option) -> int expr -> int option =
 fun leaf e ->
  match e with
  | Int n -> Some n
  | Ref _ | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos -> leaf e
  | Add (a, b) -> int_binop leaf const_add a b
  | Sub (a, b) -> int_binop leaf const_sub a b
  | Mul (a, b) -> int_binop leaf const_mul a b
  | Div (a, b) -> int_binop leaf const_div a b
  | Mod (a, b) -> int_binop leaf const_rem a b
  | Land (a, b) -> int_binop leaf (fun a b -> Some (a land b)) a b
  | Lor (a, b) -> int_binop leaf (fun a b -> Some (a lor b)) a b
  | Lxor (a, b) -> int_binop leaf (fun a b -> Some (a lxor b)) a b
  | Lnot a -> Option.map lnot (int_expr_with leaf a)
  | Lsl (a, b) -> int_binop leaf const_shift_left a b
  | Lsr (a, b) -> int_binop leaf const_shift_right a b
  | Cast (width, e) -> Option.map (const_cast width) (int_expr_with leaf e)
  (* A constant condition would need the same pass over [bool expr]; leave the
     whole node symbolic. *)
  | If_then_else _ -> None

and int_binop leaf f a b =
  match (int_expr_with leaf a, int_expr_with leaf b) with
  | Some a, Some b -> f a b
  | None, _ | _, None -> None

let const_int_expr e = int_expr_with (fun _ -> None) e

(* A size expression resolved as far as the encode-side context allows. A bound
   input parameter answers; a reference to a sibling field or to a position does
   not, because a value on its own does not carry the record around it. *)
let rec size_in_ctx ctx e =
  match ctx with
  | Unbound -> None
  | Checked ctx -> size_in_ctx ctx e
  | Bound _ ->
      int_expr_with
        (function Param_ref p -> Some (eval_param ctx p.name) | _ -> None)
        e

(* [fold_size e] is [e] with a reference-free constant folded to its literal.
   Every smart constructor taking a size or length expression runs its argument
   through it. *)
let fold_size e = match const_int_expr e with Some n -> Int n | None -> e

(* A literal size is a count of bytes in the buffer: never negative, and never
   more than a buffer can hold. *)
let literal_size ~combinator e =
  match fold_size e with
  | Int n when n < 0 || n > Sys.max_string_length ->
      Fmt.invalid_arg "Wire.%s: size %d is not a byte count a buffer can hold"
        combinator n
  | e -> e

(* Type constructors *)
let uint8 = Uint8
let uint16 = Uint16 Little
let uint16be = Uint16 Big
let uint32 = Uint32 Little
let uint32be = Uint32 Big
let uint64 = Uint64 Little
let uint64be = Uint64 Big
let int8 = Int8
let int16 = Int16 Little
let int16be = Int16 Big
let int32 = Int32 Little
let int32be = Int32 Big
let (int64 : int64 typ) = Int64 Little
let (int64be : int64 typ) = Int64 Big
let float32 = Float32 Little
let float32be = Float32 Big
let float64 = Float64 Little
let float64be = Float64 Big

let uint_typ name endian size =
  let size = fold_size size in
  (match size with
  | Int n when n < 1 || n > 7 ->
      Fmt.invalid_arg "%s: size must be 1-7, got %d" name n
  | _ -> ());
  Uint_var { size; endian }

let uint ?(endian = Big) size = uint_typ "uint" endian (Int size)
let uint_var ?(endian = Big) size = uint_typ "uint_var" endian size

(* Bitfield bases *)
let bf_uint8 = U8
let bf_uint16 = U16 Little
let bf_uint16be = U16 Big
let bf_uint32 = U32 Little
let bf_uint32be = U32 Big

let bitfield_base_bits : bitfield_base -> int = function
  | U8 -> 8
  | U16 _ -> 16
  | U32 _ -> 32

let equal_bitfield_base a b =
  match (a, b) with
  | U8, U8 -> true
  | U16 e1, U16 e2 | U32 e1, U32 e2 -> equal_endian e1 e2
  | (U8 | U16 _ | U32 _), _ -> false

(* EverParse's own bit order for a base: LSB-first for the little-endian
   ones (MSVC C bit-field packing), MSB-first for the big-endian ones. *)
let native_bit_order = function
  | U8 | U16 Little | U32 Little -> Lsb_first
  | U16 Big | U32 Big -> Msb_first

let bits ?(bit_order = Msb_first) ~width base =
  (* A bitfield must fit its base word: a [width] above the base size, or below
     1, has no faithful wire meaning and the OCaml shift and the 3D [base F :
     width] field would read different values. Reject it at construction. *)
  let total = bitfield_base_bits base in
  if width < 1 || width > total then
    Fmt.invalid_arg
      "Wire.bits: width %d does not fit a %d-bit base (must be 1..%d)" width
      total total;
  (* The extracted value must fit the native positive int: only reachable on
     a narrow-int platform (wasm_of_ocaml, js_of_ocaml), where a 31- or
     32-bit field of a U32 base has nowhere to live. *)
  if width > Sys.int_size - 1 then
    Fmt.invalid_arg
      "Wire.bits: width %d does not fit this platform's %d-bit int" width
      Sys.int_size;
  Bits { width; base; bit_order }

let bit b = Bool.to_int b
let is_set n = n <> 0

(* -- Integer views --

   [bool], [cases], [enum] and [variants] refine the integer a field decodes to
   and need nothing else from their base. Which OCaml type carries that integer
   is the base's own business: [uint32] keeps a 32-bit unsigned value in a
   module of its own, [uint64] a 64-bit one, and both read as the same [int] a
   [uint8] does. Deciding that reading here, once, is what keeps [Eval], the
   codec's enum checks, [Param]'s environments and the 3D projection agreeing
   on which types have an integer view and what it is. *)

(* Convert a typed value to [int]. Returns [None] for types that don't
   fit in OCaml int (uint64 over 2^63, non-numeric). *)
let rec int_of : type a. a typ -> a -> int option =
 fun typ v ->
  match typ with
  | Uint8 -> Some (UInt8.to_int v)
  | Uint16 _ -> Some (UInt16.to_int v)
  | Uint_var _ -> Int64.unsigned_to_int (Optint.Int63.to_int64 v)
  (* On a narrow-int platform a u32 value may not fit either; the unsigned
     conversion returns [None] exactly then and is identity-exact on a 64-bit
     host. *)
  | Uint32 _ -> Int32.unsigned_to_int (UInt32.to_int32 v)
  | Uint64 _ -> UInt64.to_int_opt v
  | Int8 -> Some (SInt8.to_int v)
  | Int16 _ -> Some (SInt16.to_int v)
  | Int32 _ -> SInt32.to_int_opt v
  | Int64 _ -> Int64.unsigned_to_int v
  | Float32 _ -> None
  | Float64 _ -> None
  | Bits _ -> Some v
  | Enum { base; _ } -> int_of base v
  | Where { inner; _ } -> int_of inner v
  | Single_elem { elem; _ } -> int_of elem v
  | Apply { typ; _ } -> int_of typ v
  | Map { inner; encode; _ } -> int_of inner (encode v)
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Union _
  | Struct _ | Type_ref _ | Qualified_ref _ | Codec _ | Optional _
  | Optional_or _ | Repeat _ ->
      None

(* Whether every value of a carrier fits the native int, so its integer view
   cannot fail. A reader over one of these needs no relocating handler: the
   narrow widths always fit, and the wide ones depend on [Sys.int_size], which
   is 63 on a native build and 31 under wasm_of_ocaml. *)
let rec int_view_is_total : type a. a typ -> bool = function
  | Uint8 | Int8 | Uint16 _ | Int16 _ | Bits _ -> true
  | Uint32 _ | Int32 _ -> Sys.int_size > 32
  | Uint64 _ | Int64 _ | Uint_var _ -> false
  | Enum { base; _ } -> int_view_is_total base
  | Where { inner; _ } -> int_view_is_total inner
  | Single_elem { elem; _ } -> int_view_is_total elem
  | Apply { typ; _ } -> int_view_is_total typ
  | Map { inner; _ } -> int_view_is_total inner
  | _ -> false

(* Hot-path variant of [int_of] for the cross-field size/offset/present
   readers, which need a plain [int]. Returns it directly (no [Some] box on the
   numeric path). A [uint64]/[int64] beyond the native int range is adversarial
   input and raises [Parse_error] ([Value_out_of_range]); a non-integer field
   referenced where an integer is required is a schema error and raises
   [Invalid_argument]. *)
let int_overflow v = raise_out_of_range ~at:0 v

let not_an_integer () =
  invalid_arg "Wire: non-integer field referenced where an integer is required"

let rec int_of_exn : type a. a typ -> a -> int =
 fun typ v ->
  match typ with
  | Uint8 -> UInt8.to_int v
  | Uint16 _ -> UInt16.to_int v
  | Uint_var _ -> (
      if Sys.int_size > 56 then UInt63.to_int v
      else
        match Int64.unsigned_to_int (Optint.Int63.to_int64 v) with
        | Some n -> n
        | None -> int_overflow (Optint.Int63.to_int64 v))
  (* The narrow-int branches raise the typed overflow error instead of
     letting [Optint.to_int]'s [Failure] escape; a 64-bit host keeps the
     direct allocation-free conversion. *)
  | Uint32 _ -> (
      if Sys.int_size > 32 then UInt32.to_int v
      else
        match Int32.unsigned_to_int (UInt32.to_int32 v) with
        | Some n -> n
        | None ->
            int_overflow
              (Int64.logand (Int64.of_int32 (UInt32.to_int32 v)) 0xFFFF_FFFFL))
  | Uint64 _ -> (
      match UInt64.to_int_opt v with
      | Some n -> n
      | None -> int_overflow (UInt64.to_int64 v))
  | Int8 -> SInt8.to_int v
  | Int16 _ -> SInt16.to_int v
  | Int32 _ -> (
      match SInt32.to_int_opt v with
      | Some n -> n
      | None -> int_overflow (Int64.of_int32 (SInt32.to_int32 v)))
  | Int64 _ -> (
      match Int64.unsigned_to_int v with Some n -> n | None -> int_overflow v)
  | Float32 _ -> not_an_integer ()
  | Float64 _ -> not_an_integer ()
  | Bits _ -> v
  | Enum { base; _ } -> int_of_exn base v
  | Where { inner; _ } -> int_of_exn inner v
  | Single_elem { elem; _ } -> int_of_exn elem v
  | Apply { typ; _ } -> int_of_exn typ v
  | Map { inner; encode; _ } -> int_of_exn inner (encode v)
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Union _
  | Struct _ | Type_ref _ | Qualified_ref _ | Codec _ | Optional _
  | Optional_or _ | Repeat _ ->
      not_an_integer ()

(* The other direction: the value a type carries for a given integer. The
   narrow constructors raise [Invalid_argument] on a number their field cannot
   hold, so a case value or table index that does not fit its base is refused
   where it is written down rather than silently reshaped. *)
let rec of_int : type a. a typ -> int -> a =
 fun typ n ->
  match typ with
  | Uint8 -> UInt8.v n
  | Uint16 _ -> UInt16.v n
  | Uint_var _ -> UInt63.of_int n
  | Uint32 _ -> UInt32.of_int n
  | Uint64 _ -> UInt64.of_int n
  | Int8 -> SInt8.v n
  | Int16 _ -> SInt16.v n
  | Int32 _ -> SInt32.of_int n
  | Int64 _ -> Int64.of_int n
  | Float32 _ -> not_an_integer ()
  | Float64 _ -> not_an_integer ()
  | Bits _ -> n
  | Enum { base; _ } -> of_int base n
  | Where { inner; _ } -> of_int inner n
  | Single_elem { elem; _ } -> of_int elem n
  | Apply { typ; _ } -> of_int typ n
  | Map { inner; decode; _ } -> decode (of_int inner n)
  | Unit | All_bytes | All_zeros | Zeroterm | Zeroterm_at_most _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Casetype _ | Union _
  | Struct _ | Type_ref _ | Qualified_ref _ | Codec _ | Optional _
  | Optional_or _ | Repeat _ ->
      not_an_integer ()

(* The types [int_of_exn] and [of_int] have a conversion for. Checked at
   construction by every combinator that needs one, so an unusable base fails
   at the description rather than on the first byte decoded. *)
let rec is_int_representable : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint_var _ | Uint32 _ | Uint64 _ | Int8 | Int16 _
  | Int32 _ | Int64 _ | Bits _ ->
      true
  | Enum { base; _ } -> is_int_representable base
  | Map { inner; _ } -> is_int_representable inner
  | Where { inner; _ } -> is_int_representable inner
  | Single_elem { elem; _ } -> is_int_representable elem
  | Apply { typ; _ } -> is_int_representable typ
  | _ -> false

let reject_non_integer ~combinator typ =
  if not (is_int_representable typ) then
    Fmt.invalid_arg "Wire.%s: base type must be an integer type" combinator

(* Field decorations [Optional], [Optional_or], [Repeat] only have a 3D
   projection when they appear at the top of a struct field's type --
   anywhere else (array elem, casetype case body, [where]/[map]/[apply]
   wrapper) there is no 3D shape that captures the semantics. Reject the
   construction at the user's call site so the error points at the
   wrapping combinator rather than surfacing later inside [pp_typ]. *)
let rec is_field_decoration : type a. a typ -> bool = function
  | Optional _ | Optional_or _ | Repeat _ | Union _ -> true
  | Map { inner; _ } -> is_field_decoration inner
  | Where { inner; _ } -> is_field_decoration inner
  | Apply { typ; _ } -> is_field_decoration typ
  | _ -> false

let reject_decoration ~combinator t =
  if is_field_decoration t then
    Fmt.invalid_arg
      "Wire.%s: [optional]/[optional_or]/[repeat]/[union] are field \
       decorations and cannot appear nested inside [%s] -- attach them as the \
       field type directly via [Field.v]."
      combinator combinator

let map_ids = Atomic.make 0
let fresh_map_id () = Atomic.fetch_and_add map_ids 1

let map decode encode inner =
  reject_decoration ~combinator:"map" inner;
  Map { inner; decode; encode; index_bound = None; map_id = fresh_map_id () }

let bool inner =
  reject_non_integer ~combinator:"bit" inner;
  let to_int = int_of_exn inner and of_int = of_int inner in
  Map
    {
      inner;
      decode = (fun v -> is_set (to_int v));
      encode = (fun b -> of_int (bit b));
      index_bound = None;
      map_id = fresh_map_id ();
    }

(* Top-level so [variants_lookup] does not allocate a closure per call.
   Returns -1 if [v] is not in [arr] (used as a non-allocating sentinel
   instead of [int option]). Shared with [variants] below. *)
let rec variants_lookup arr v i =
  if i >= Array.length arr then -1
  else if arr.(i) = v then i
  else variants_lookup arr v (i + 1)

let cases variants inner =
  reject_non_integer ~combinator:"lookup" inner;
  let to_int = int_of_exn inner and of_int = of_int inner in
  let arr = Array.of_list variants in
  let len = Array.length arr in
  let decode v =
    (* Offset 0 is relative to the value handed in; [map_decode] moves the
       failure to where the reader found that value. *)
    let n = to_int v in
    if n >= 0 && n < len then arr.(n) else raise_invalid_tag ~at:0 n
  in
  let encode v =
    let i = variants_lookup arr v 0 in
    if i < 0 then invalid_arg "Wire.lookup: unknown variant" else of_int i
  in
  Map
    { inner; decode; encode; index_bound = Some len; map_id = fresh_map_id () }

let unit = Unit
let all_bytes = All_bytes
let all_zeros = All_zeros
let zeroterm = Zeroterm

let zeroterm_at_most ~size =
  Zeroterm_at_most { size = literal_size ~combinator:"zeroterm_at_most" size }

let where cond inner =
  reject_decoration ~combinator:"where" inner;
  Where { cond; inner }

let seq_list : ('a, 'a list) seq_map =
  Seq_map
    {
      empty = [];
      add = (fun acc x -> x :: acc);
      finish = List.rev;
      iter = List.iter;
    }

let sequence_elements (type a seq) (Seq_map s : (a, seq) seq_map) (values : seq)
    =
  let elements = Stdlib.ref [] in
  s.iter (fun value -> elements := value :: !elements) values;
  List.rev !elements

let exact_array_elements seq ~expected values =
  let elements = sequence_elements seq values in
  let actual = List.length elements in
  if actual <> expected then
    Fmt.invalid_arg "Wire.array: expected %d elements, got %d" expected actual;
  elements

let exact_repeat_elements seq ~expected ~size_of values =
  let sized =
    sequence_elements seq values
    |> List.map (fun value -> (value, size_of value))
  in
  let actual = List.fold_left (fun total (_, size) -> total + size) 0 sized in
  if actual <> expected then
    Fmt.invalid_arg
      "Wire.repeat: byte budget is %d but the elements span %d. The budget is \
       the region size the description declares, and encode holds the values \
       to it; if that size is not known where the codec is built, take it as a \
       Param.input and bind it per call rather than baking in a literal."
      expected actual;
  sized

(* A fixed-size byte field is exact. Truncating a long value or zero-padding a
   short one would make the bytes on the wire differ from the value the caller
   signed, hashed or length-prefixed, with nothing to signal it. *)
let check_byte_field_size ~expected ~actual =
  if actual <> expected then Errors.byte_length ~expected ~actual

let check_nested_size ~at_most ~expected ~actual =
  if actual > expected then
    Fmt.invalid_arg "Wire.nested%s: region is %d bytes, value needs %d"
      (if at_most then "_at_most" else "")
      expected actual;
  if (not at_most) && actual <> expected then
    Fmt.invalid_arg "Wire.nested: expected %d bytes, got %d" expected actual

(* The 3D projection of [array]/[optional]/[optional_or]/[repeat] turns
   their length / predicate / byte budget into a [byte-size] suffix.
   That works as long as the wrapped element exposes a wire size we can
   plumb into the suffix expression -- either a fixed scalar width, an
   already-sized payload like [byte_array {size}], or a codec with a
   fixed [wire_size]. Reject everything else at the smart constructor
   so the error fires at the user's call site. *)
let rec has_wire_size_expr : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  | Bits _ -> true
  | Unit -> true
  | Uint_var _ -> true
  | Byte_array _ | Byte_slice _ | Byte_array_where _ -> true
  | Single_elem _ -> true
  | Map { inner; _ } -> has_wire_size_expr inner
  | Enum { base; _ } -> has_wire_size_expr base
  | Where { inner; _ } -> has_wire_size_expr inner
  | Apply { typ; _ } -> has_wire_size_expr typ
  | Array { elem; _ } -> has_wire_size_expr elem
  | Codec { codec_fixed_size; _ } -> codec_fixed_size <> None
  | _ -> false

(* EverParse's byte-budget array ([T_nlist], the [array]/[repeat] lowering)
   requires its element parser to consume a positive minimum number of bytes:
   the [nz] index of [parser_kind nz wk]. A byte-budget array, a [nested] region
   ([T_exact]), and the all-bytes / all-zeros tails all have kind
   [parser_kind false _] -- their parser may consume zero bytes -- regardless of
   a positive constant budget, so none of them is [nz]. A struct/codec is [nz]
   iff some field is (sizes add, so one positive-minimum field anchors the rest);
   a casetype is [nz] iff its tag is or every case body is (it parses [tag] then
   a switch, and the tag alone anchors it). [map]/[where]/[enum] are transparent.
   This is what makes a named composite usable as an element: a [T_nlist] over a
   non-[nz] element fails EverParse extraction. *)
let rec nz : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  | Bits _ -> true
  | Zeroterm -> true
  | Uint_var _ -> false (* lowers to a UINT8 byte-budget array *)
  | Zeroterm_at_most _ -> false (* T_at_most kind: parser_kind false _ *)
  | Byte_array _ | Byte_array_where _ | Byte_slice _ -> false
  | Unit -> false
  | All_bytes | All_zeros -> false
  | Array _ | Repeat _ -> false (* nlist kind *)
  | Single_elem _ -> false (* T_exact kind *)
  | Optional _ | Optional_or _ -> false
  | Where { inner; _ } -> nz inner
  | Map { inner; _ } -> nz inner
  | Enum { base; _ } -> nz base
  | Apply { typ; _ } -> nz typ
  | Codec { codec_struct; _ } -> struct_nz codec_struct
  | Struct s -> struct_nz s
  | Casetype { tag; cases; _ } ->
      nz tag
      || List.for_all (fun (Case_branch { cb_inner; _ }) -> nz cb_inner) cases
  | Union { cases; _ } ->
      List.for_all (fun (Case_branch { cb_inner; _ }) -> nz cb_inner) cases
  | Type_ref _ | Qualified_ref _ -> false (* opaque ref: be conservative *)

and struct_nz (s : struct_) =
  List.exists (fun (Field f) -> nz f.field_typ) s.fields

(* An [array]/[array_seq] element must be fixed-width AND decodable one element
   at a time by the array loop: a scalar, a fixed-size byte span, or a
   fixed-size sub-codec. [Map]/[Where]/[Enum] are transparent wrappers, so look
   through them. A [nested] region, a refined byte span ([byte_array_where]), a
   nested [array], or a variable / self-delimiting inner has no array
   projection (3D rejects it) and the decoder has no element reader, so they are
   refused at construction. A fixed-size sub-codec must additionally be [nz]:
   EverParse projects the array as a byte-budget list of the codec's named
   struct, and a list over a possibly-empty element does not extract. This is
   stricter than [has_wire_size_expr], which admits sized-but-undecodable
   elements for the [optional] byte-size suffix. *)
let rec is_array_element : type a. a typ -> bool = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ -> true
  | Int8 | Int16 _ | Int32 _ | Int64 _ -> true
  | Float32 _ | Float64 _ -> true
  (* [Unit] is 0-width: an array of it carries no bytes and projects to a
     zero-size 3D array EverParse rejects, so it is not a valid element. A byte
     span whose declared size is a literal zero (or less) is 0-width for the
     same reason, so admit only a positive one. [uint] and [uint_var] already
     refuse a
     non-positive literal size, and a symbolic one is not [Int _]. *)
  | Uint_var { size = Int _; _ } -> true
  | Byte_array { size = Int n } | Byte_slice { size = Int n } -> n > 0
  | Codec { codec_fixed_size; codec_struct; _ } ->
      codec_fixed_size <> None && struct_nz codec_struct
  | Map { inner; _ } -> is_array_element inner
  | Where { inner; _ } -> is_array_element inner
  | Enum { base; _ } -> is_array_element base
  | _ -> false

let reject_non_array_element ~combinator t =
  if not (is_array_element t) then
    Fmt.invalid_arg
      "Wire.%s: element type is not a fixed-width array element -- 3D projects \
       %s as a count of fixed-size elements (a scalar, a byte span of at least \
       one byte, or a fixed-size sub-codec with at least one fixed-size \
       field); a nested region, refined span, zero-width span, variable inner, \
       or a sub-codec made only of byte-span fields has no array projection."
      combinator combinator

(* A self-delimiting inner carries its own length / structure, so it decodes a
   known span without an external size. These project as a gate-dispatched
   casetype (present case = the inner, absent case = a 0-byte field), unlike the
   byte-size suffix used for [has_wire_size_expr] inners. *)
let rec is_self_delimiting : type a. a typ -> bool = function
  | Codec _ -> true
  | Casetype _ -> true
  | Map { inner; _ } -> is_self_delimiting inner
  | Where { inner; _ } -> is_self_delimiting inner
  | Apply { typ; _ } -> is_self_delimiting typ
  | _ -> false

(* [optional]/[optional_or] accept any inner that projects to 3D: either a
   byte-sized inner (conditional [byte-size] region) or a self-delimiting one
   (gate-dispatched casetype). Everything else has no clean projection. *)
let reject_unprojectable_optional ~combinator t =
  if not (has_wire_size_expr t || is_self_delimiting t) then
    Fmt.invalid_arg
      "Wire.%s: inner type does not project to 3D -- it is neither byte-sized \
       nor self-delimiting."
      combinator combinator

(* A bitfield is packed within an enclosing base word in a record; it has no
   standalone wire form, so it cannot be an [array] / [repeat] element (there is
   no byte boundary per element, and no 3D element type to project to). Reject
   it at construction rather than crash at decode. *)
let rec is_bitfield_element : type a. a typ -> bool = function
  | Bits _ -> true
  | Map { inner; _ } -> is_bitfield_element inner
  | Where { inner; _ } -> is_bitfield_element inner
  | Enum { base; _ } -> is_bitfield_element base
  | _ -> false

let reject_bitfield_element ~combinator t =
  if is_bitfield_element t then
    Fmt.invalid_arg
      "Wire.%s: a bitfield has no standalone element form (bits are packed \
       within an enclosing word); it cannot be an element of %s."
      combinator combinator

(* A bare greedy field ([all_bytes] / [all_zeros]) reads "the rest of the
   buffer". It has no 3D type of its own, so it only projects as the trailing
   field of a struct or sub-codec, never as a casetype case body (the switch
   case needs a determinate type) and never with a determinate element size. *)
let rec is_greedy : type a. a typ -> bool = function
  | All_bytes | All_zeros -> true
  | Map { inner; _ } -> is_greedy inner
  | Where { inner; _ } -> is_greedy inner
  | Enum { base; _ } -> is_greedy base
  | _ -> false

let reject_greedy_case_body t =
  if is_greedy t then
    invalid_arg
      "Wire.casetype: a case body cannot be a bare all_bytes / all_zeros \
       greedy field; it has no determinate type. Wrap it in a sub-codec."

(* Whether a decoder for [t] runs to the end of the buffer: a greedy leaf, a
   sub-codec whose own last field does, a casetype with such a case body (if
   that case is selected its greedy tail has no boundary), or a gated optional
   wrapping one. Each is as unbounded as a bare greedy field, so it is only
   meaningful as the last thing in the buffer: nothing after it, and nothing
   that iterates it. *)
let rec ends_greedy : type a. a typ -> bool =
 fun t ->
  is_greedy t
  ||
  match t with
  | Codec { codec_struct; _ } -> struct_ends_greedy codec_struct
  | Casetype { cases; _ } ->
      List.exists
        (fun (Case_branch { cb_inner; _ }) -> ends_greedy cb_inner)
        cases
  | Map { inner; _ } -> ends_greedy inner
  | Where { inner; _ } -> ends_greedy inner
  | Enum { base; _ } -> ends_greedy base
  | Optional { present = Bool false; _ }
  | Optional_or { present = Bool false; _ } ->
      false
  | Optional { inner; _ } -> ends_greedy inner
  | Optional_or { inner; _ } -> ends_greedy inner
  | _ -> false

and struct_ends_greedy (s : struct_) =
  match List.rev s.fields with
  | Field f :: _ -> ends_greedy f.field_typ
  | [] -> false

let byte_array ~size =
  Byte_array { size = literal_size ~combinator:"byte_array" size }

let byte_array_where_counter = Atomic.make 0
let elt_var_prefix = "__elt_"

let byte_array_where ~size ~per_byte =
  let n = Atomic.fetch_and_add byte_array_where_counter 1 in
  let elt_var = elt_var_prefix ^ string_of_int n in
  let cond = per_byte (Ref (I, elt_var)) in
  Byte_array_where
    { size = literal_size ~combinator:"byte_array_where" size; elt_var; cond }

(* The 3D synthesized typedef name derived from an elt_var. The element
   field inside the synthesized struct keeps the elt_var as its name so the
   captured [cond] (which references [elt_var] via [Ref]) renders as a
   constraint on that field directly. *)
let synth_name_of_elt_var ev =
  let plen = String.length elt_var_prefix in
  if String.length ev > plen && String.sub ev 0 plen = elt_var_prefix then
    "_RefByte_" ^ String.sub ev plen (String.length ev - plen)
  else "_RefByte_" ^ ev

let byte_slice ~size =
  Byte_slice { size = literal_size ~combinator:"byte_slice" size }

let optional present inner =
  reject_bitfield_element ~combinator:"optional" inner;
  reject_unprojectable_optional ~combinator:"optional" inner;
  Optional { present; inner }

let optional_or present ~default inner =
  reject_bitfield_element ~combinator:"optional_or" inner;
  reject_unprojectable_optional ~combinator:"optional_or" inner;
  Optional_or { present; inner; default }

(* [repeat ~size elem] is more permissive than [array]: 3D's
   [<elem>[:byte-size <size>]] parses elements until the byte budget is
   exhausted, so variable-size elements are fine as long as each one is
   self-bounded by its 3D type (struct, codec, ...). The only thing we
   refuse is a field decoration nested inside [elem]. *)
let repeat ~size elem =
  reject_decoration ~combinator:"repeat" elem;
  reject_bitfield_element ~combinator:"repeat" elem;
  Repeat { size = literal_size ~combinator:"repeat" size; elem; seq = seq_list }

let repeat_seq seq ~size elem =
  reject_decoration ~combinator:"repeat_seq" elem;
  reject_bitfield_element ~combinator:"repeat_seq" elem;
  Repeat { size = literal_size ~combinator:"repeat_seq" size; elem; seq }

let nested ~size elem =
  reject_bitfield_element ~combinator:"nested" elem;
  Single_elem
    { size = literal_size ~combinator:"nested" size; elem; at_most = false }

let nested_at_most ~size elem =
  reject_bitfield_element ~combinator:"nested_at_most" elem;
  Single_elem
    {
      size = literal_size ~combinator:"nested_at_most" size;
      elem;
      at_most = true;
    }

let enum name cases base =
  reject_non_integer ~combinator:"enum" base;
  Enum { name; cases; base; closed = true }

let enum_open name cases base =
  reject_non_integer ~combinator:"enum_open" base;
  Enum { name; cases; base; closed = false }

let variants name cases base =
  reject_non_integer ~combinator:"variants" base;
  let to_int = int_of_exn base and of_int = of_int base in
  let enum_cases = List.mapi (fun i (s, _) -> (s, i)) cases in
  let arr = Array.of_list (List.map snd cases) in
  let len = Array.length arr in
  (* [enum name enum_cases base] is closed, so [enum_check] rejects any value
     outside [0, len) before [decode] runs; the [else] is a defensive fallback. *)
  let decode v =
    let n = to_int v in
    if n >= 0 && n < len then arr.(n)
    else raise_invalid_enum ~at:0 ~value:n ~valid:(List.init len Fun.id)
  in
  let encode v =
    let i = variants_lookup arr v 0 in
    if i < 0 then Fmt.invalid_arg "Wire.variants %s: unknown variant" name
    else of_int i
  in
  map decode encode (enum name enum_cases base)

(* Casetype *)
type ('a, 'k) case_def =
  | Case_def : {
      cd_index : 'k option;
      cd_inner : 'w typ;
      cd_inject : 'w -> 'a;
      cd_project : 'a -> 'w option;
    }
      -> ('a, 'k) case_def
  | Default_def : {
      dd_inner : 'w typ;
      dd_inject : 'k -> 'w -> 'a;
      dd_project : 'a -> ('k * 'w) option;
    }
      -> ('a, 'k) case_def

let case ?index inner ~inject ~project =
  Case_def
    {
      cd_index = index;
      cd_inner = inner;
      cd_inject = inject;
      cd_project = project;
    }

let default inner ~inject ~project =
  Default_def { dd_inner = inner; dd_inject = inject; dd_project = project }

(* An enum over a big-endian multi-byte base cannot be a 3D [enum] type:
   EverParse types the integer constants as the native (little-endian) width and
   rejects the declaration ("Expected UINT16BE, got UINT16"). Such an enum is
   projected as its base scalar with a membership refinement (closed) or bare
   base (open) instead, the same shape used outside the doc projection. *)
let enum_base_is_be : type a. a typ -> bool = function
  | Uint16 Big | Uint32 Big | Uint64 Big -> true
  | Int16 Big | Int32 Big | Int64 Big -> true
  | Uint_var { endian = Big; _ } -> true
  | _ -> false

(* A casetype tag must project to a 3D type the generated [switch] dispatches on.
   A [uint size] or [uint_var size] (Uint_var) tag renders as [UINTBE(n)], which
   is not a 3D type;
   an enum over a big-endian base has no 3D enum type to name in the case labels.
   Neither projects, so reject it at construction (seen through the transparent
   [Map] / [Where] wrappers a [variants] tag carries). *)
let rec reject_unprojectable_casetype_tag : type k. k typ -> unit = function
  | Uint_var _ ->
      invalid_arg
        "Wire.casetype: a [uint size] or [uint_var size] tag has no 3D \
         projection; use a fixed-width integer, bitfield, or enum tag."
  | Enum { base; _ } when enum_base_is_be base ->
      invalid_arg
        "Wire.casetype: an enum over a big-endian base has no 3D casetype-tag \
         projection; use a little-endian or 1-byte enum base."
  | Map { inner; _ } -> reject_unprojectable_casetype_tag inner
  | Where { inner; _ } -> reject_unprojectable_casetype_tag inner
  | _ -> ()

let resolve_case : type a k.
    combinator:string -> (a, k) case_def -> (a, k) case_branch =
 fun ~combinator -> function
  | Case_def { cd_index; cd_inner; cd_inject; cd_project } ->
      reject_decoration ~combinator cd_inner;
      reject_greedy_case_body cd_inner;
      let tag_val =
        match cd_index with
        | Some k -> k
        | None ->
            Fmt.invalid_arg
              "Wire.%s: every case must supply an explicit [~index]" combinator
      in
      Case_branch
        {
          cb_tag = Some tag_val;
          (* [case] knows its tag from [~index], so its [inject] takes only the
             body and its [project] yields only the body; pair the fixed tag
             back on for the unified tag-aware branch. *)
          cb_inner = cd_inner;
          cb_inject = (fun _matched body -> cd_inject body);
          cb_project =
            (fun a -> Option.map (fun w -> (tag_val, w)) (cd_project a));
        }
  | Default_def { dd_inner; dd_inject; dd_project } ->
      reject_decoration ~combinator dd_inner;
      reject_greedy_case_body dd_inner;
      Case_branch
        {
          cb_tag = None;
          cb_inner = dd_inner;
          cb_inject = dd_inject;
          cb_project = dd_project;
        }

let casetype name tag defs =
  reject_unprojectable_casetype_tag tag;
  Casetype
    { name; tag; cases = List.map (resolve_case ~combinator:"casetype") defs }

(* A union switches on the integer its tag field holds, in OCaml and in 3D
   alike, so a tag with no integer view has nothing to dispatch on. *)
let union name ~tag ~on defs =
  if not (is_int_representable tag) then
    invalid_arg "Wire.Field.union: the tag field must be integer-valued";
  reject_unprojectable_casetype_tag tag;
  Union
    { name; tag; on; cases = List.map (resolve_case ~combinator:"union") defs }

(* Struct fields *)
let field name ?constraint_ ?action ?doc typ =
  Field
    {
      field_name = Some name;
      field_typ = typ;
      constraint_;
      action;
      field_doc = doc;
    }

let anon_field typ =
  Field
    {
      field_name = None;
      field_typ = typ;
      constraint_ = None;
      action = None;
      field_doc = None;
    }

(* Struct constructors *)
let struct_ name fields = { name; params = []; where = None; fields }
let struct_name (s : struct_) = s.name
let struct_typ s = Struct s
let field_names s = List.filter_map (fun (Field f) -> f.field_name) s.fields

let struct_project s ~name ~keep =
  let keep_names = List.filter_map (fun (Field f) -> f.field_name) keep in
  let fields =
    List.map
      (fun (Field f) ->
        match f.field_name with
        | Some n when List.mem n keep_names -> Field f
        | _ ->
            Field
              { f with field_name = None; constraint_ = None; action = None })
      s.fields
  in
  { s with name; fields }

(* What kind of OCaml value a field produces -- used by Wire_stubs to
   generate the right C-to-OCaml conversion in output stubs. *)
type ocaml_kind = Int | Int64 | Float32 | Float64 | Bool | String | Unit

let rec ocaml_kind_of : type a. a typ -> ocaml_kind = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint_var _ -> Int
  | Uint64 _ -> Int64
  | Int8 | Int16 _ | Int32 _ -> Int
  | Int64 _ -> Int64
  | Float32 _ -> Float32
  | Float64 _ -> Float64
  | Bits _ -> Int
  | Map { inner = Bits _; decode = _; encode = _; _ } ->
      (* bool (bits ~width:1 ...) maps to bool; other maps stay int *)
      (* We can't distinguish bool from other maps here without checking
         the decode function. Use Int as safe default -- the EverParse
         output struct stores the raw int anyway. *)
      Int
  | Map { inner; _ } -> ocaml_kind_of inner
  | Enum { base; _ } -> ocaml_kind_of base
  | Where { inner; _ } -> ocaml_kind_of inner
  | Byte_array _ -> String
  | Byte_array_where _ -> String
  | Byte_slice _ -> String (* approximate: slice becomes string in output *)
  | Zeroterm | Zeroterm_at_most _ -> String
  | Unit | All_bytes | All_zeros -> Unit
  | _ -> Int (* fallback *)

let field_kinds s =
  List.filter_map
    (fun (Field f) ->
      match f.field_name with
      | Some name -> Some (name, ocaml_kind_of f.field_typ)
      | None -> None)
    s.fields

(* Parameters *)
let param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = false }

let mutable_param name typ =
  { param_name = name; param_typ = Pack_typ typ; mutable_ = true }

let param_struct name params ?where fields = { name; params; where; fields }

let apply typ args =
  reject_decoration ~combinator:"apply" typ;
  Apply { typ; args = List.map (fun e -> Pack_expr e) args }

(* Type references *)
let type_ref name = Type_ref name
let qualified_ref module_ name = Qualified_ref { module_; name }

(* Actions *)
let on_success stmts = Success stmts
let on_act stmts = Act stmts
let assign (p : ('a, param_output) param_handle) e = Assign (p, e)
let return_bool e = Return e
let abort = Abort
let action_if cond then_ else_ = If (cond, then_, else_)
let var name e = Var (name, e)

(* Declarations *)
type decl =
  | Typedef of {
      entrypoint : bool;
      export : bool;
      output : bool;
      extern_ : bool;
      doc : string option;
      struct_ : struct_;
    }
  | Define of { name : string; value : int }
  | Extern_fn of { name : string; params : param list; ret : packed_typ }
  | Extern_probe of { init : bool; name : string }
  | Enum_decl of {
      name : string;
      cases : (string * int) list;
      base : packed_typ;
    }
  | Casetype_decl of {
      name : string;
      params : param list;
      tag : packed_typ;
      cases : (packed_expr option * packed_typ) list;
    }

let typedef ?(entrypoint = false) ?(export = false) ?(output = false)
    ?(extern_ = false) ?doc struct_ =
  Typedef { entrypoint; export; output; extern_; doc; struct_ }

let define name value = Define { name; value }
let extern_fn name params ret = Extern_fn { name; params; ret = Pack_typ ret }
let extern_probe ?(init = false) name = Extern_probe { init; name }
let enum_decl name cases base = Enum_decl { name; cases; base = Pack_typ base }

type decl_case = packed_expr option * packed_typ

let decl_case tag typ = (Some (Pack_expr (Int tag)), Pack_typ typ)
let decl_default typ = (None, Pack_typ typ)

let casetype_decl name params tag cases =
  Casetype_decl { name; params; tag = Pack_typ tag; cases }

(* Module *)
type module_ = { doc : string option; decls : decl list }

let pp_error_kind = Errors.pp_error_kind
let pp_parse_error = Errors.pp_parse_error

let predicate_rank (p : predicate) =
  match p with Where -> 0 | Field -> 1 | Action -> 2 | Per_byte -> 3

let compare_predicate a b = Int.compare (predicate_rank a) (predicate_rank b)

let kind_rank = function
  | Unexpected_eof _ -> 0
  | Trailing_bytes _ -> 1
  | Invalid_enum _ -> 2
  | Invalid_tag _ -> 3
  | Missing_terminator -> 4
  | Non_zero_padding -> 5
  | Value_out_of_range _ -> 6
  | Zero_divisor -> 7
  | Constraint_failed _ -> 8

let compare_error_kind a b =
  match (a, b) with
  | Unexpected_eof a, Unexpected_eof b ->
      let c = Int.compare a.expected b.expected in
      if c <> 0 then c else Int.compare a.got b.got
  | Trailing_bytes a, Trailing_bytes b -> Int.compare a b
  | Invalid_enum a, Invalid_enum b ->
      let c = Int.compare a.value b.value in
      if c <> 0 then c else List.compare Int.compare a.valid b.valid
  | Invalid_tag a, Invalid_tag b -> Int.compare a b
  | Value_out_of_range a, Value_out_of_range b -> Int64.compare a.value b.value
  | Constraint_failed a, Constraint_failed b ->
      let c = compare_predicate a.which b.which in
      if c <> 0 then c else Option.compare Int64.compare a.value b.value
  | Missing_terminator, Missing_terminator
  | Non_zero_padding, Non_zero_padding
  | Zero_divisor, Zero_divisor ->
      0
  | _ -> Int.compare (kind_rank a) (kind_rank b)

let equal_error_kind a b = compare_error_kind a b = 0

let compare_parse_error a b =
  let c = compare_error_kind a.kind b.kind in
  if c <> 0 then c
  else
    let c = Int.compare a.at b.at in
    if c <> 0 then c else List.compare String.compare a.field b.field

let equal_parse_error a b = compare_parse_error a b = 0
