(* The walk of a record without parameters: one pass over its fields that
   checks and reads each once, in order. A size or a gate is a closure of one
   argument, the cursor, so each is one direct call; a sub-codec hands back its
   value and leaves the cursor at its end. *)

open Types
open Codec_repr
module Slice = Bytesrw.Bytes.Slice

let unwalked () = raise_notrace Unwalked

(* A field at a fixed offset of the record being walked, as an int. *)
let leaf : type a. a scalar -> cursor -> int = function
  | Byte o -> fun c -> UInt8.to_int (UInt8.unsafe_get c.wbuf (c.wbase + o))
  | U16le o -> fun c -> UInt16.to_int (UInt16.unsafe_le c.wbuf (c.wbase + o))
  | U16be o -> fun c -> UInt16.to_int (UInt16.unsafe_be c.wbuf (c.wbase + o))
  | U32le o -> fun c -> UInt32.to_int (UInt32.unsafe_le c.wbuf (c.wbase + o))
  | U32be o -> fun c -> UInt32.to_int (UInt32.unsafe_be c.wbuf (c.wbase + o))
  | Sbyte o -> fun c -> SInt8.to_int (SInt8.unsafe_get c.wbuf (c.wbase + o))
  | I16le o -> fun c -> SInt16.to_int (SInt16.unsafe_le c.wbuf (c.wbase + o))
  | I16be o -> fun c -> SInt16.to_int (SInt16.unsafe_be c.wbuf (c.wbase + o))
  | I32le o -> fun c -> SInt32.to_int (SInt32.unsafe_le c.wbuf (c.wbase + o))
  | I32be o -> fun c -> SInt32.to_int (SInt32.unsafe_be c.wbuf (c.wbase + o))
  | Bits8 { off; shift; mask } ->
      fun c -> (Unchecked.u8 c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      fun c -> (Unchecked.u16le c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      fun c -> (Unchecked.u16be c.wbuf (c.wbase + off) lsr shift) land mask

type env = { prefix : (string * packed_scalar) list; sizeof : bool }

let field env name =
  match List.assoc_opt name env.prefix with
  | Some (Ps s) -> leaf s
  | None -> raise Exit

(* The operators of [Expr_compiler], on the cursor. A leaf is a field at a
   fixed offset, read from the buffer; a [sizeof] is the type's width, as the
   layout pass reads it. Any other leaf, and a 64-bit operand, raises [Exit]:
   the record then has no walk. A literal operand is applied in place, as
   [Expr_compiler] does. *)
let rec int_expr env (e : int expr) : cursor -> int =
  let rec_ = int_expr env in
  match e with
  | Int n -> fun _ -> n
  | Ref (I, name) -> field env name
  | Sizeof t when env.sizeof ->
      let n = Option.value ~default:0 (field_wire_size t) in
      fun _ -> n
  | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos -> raise Exit
  | Add (a, Int k) ->
      let fa = rec_ a in
      fun c -> checked_add (fa c) k
  | Sub (a, Int k) ->
      let fa = rec_ a in
      fun c -> checked_sub (fa c) k
  | Mul (a, Int k) ->
      let fa = rec_ a in
      fun c -> checked_mul (fa c) k
  | Land (a, Int k) ->
      let fa = rec_ a in
      fun c -> fa c land k
  | Lsl (a, Int k) ->
      let fa = rec_ a in
      fun c -> fa c lsl k
  | Lsr (a, Int k) ->
      let fa = rec_ a in
      fun c -> fa c lsr k
  | Add (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> checked_add (fa c) (fb c)
  | Sub (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> checked_sub (fa c) (fb c)
  | Mul (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> checked_mul (fa c) (fb c)
  | Div (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> checked_div (fa c) (fb c)
  | Mod (a, b) ->
      let fa = rec_ a and fb = rec_ b in
      fun c -> checked_mod (fa c) (fb c)
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
      | `U32 -> fun c -> fa c land UInt32.mask32
      | `U64 -> fa)
  | If_then_else (b, t, e) ->
      let fb = bool_expr env b and ft = rec_ t and fe = rec_ e in
      fun c -> if fb c then ft c else fe c

and as_int : type a. env -> a expr -> (cursor -> int) option =
 fun env e ->
  let go e = Some (int_expr env e) in
  match e with
  | Int _ as e -> go e
  | Ref (I, _) as e -> go e
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

and as_bool : type a. env -> a expr -> (cursor -> bool) option =
 fun env e ->
  let go e = Some (bool_expr env e) in
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

and int_operands : type a. env -> a expr -> a expr -> _ =
 fun env a b ->
  match (as_int env a, as_int env b) with
  | Some fa, Some fb -> (fa, fb)
  | _ -> raise Exit

and bool_expr env (e : bool expr) : cursor -> bool =
  match e with
  | Bool b -> fun _ -> b
  | Eq (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c = k
  | Ne (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c <> k
  | Lt (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c < k
  | Le (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c <= k
  | Gt (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c > k
  | Ge (a, Int k) ->
      let fa = int_expr env a in
      fun c -> fa c >= k
  | Eq (a, b) -> (
      match (as_bool env a, as_bool env b) with
      | Some fa, Some fb -> fun c -> fa c = fb c
      | _ ->
          let fa, fb = int_operands env a b in
          fun c -> fa c = fb c)
  | Ne (a, b) -> (
      match (as_bool env a, as_bool env b) with
      | Some fa, Some fb -> fun c -> fa c <> fb c
      | _ ->
          let fa, fb = int_operands env a b in
          fun c -> fa c <> fb c)
  | Lt (a, b) ->
      let fa, fb = int_operands env a b in
      fun c -> fa c < fb c
  | Le (a, b) ->
      let fa, fb = int_operands env a b in
      fun c -> fa c <= fb c
  | Gt (a, b) ->
      let fa, fb = int_operands env a b in
      fun c -> fa c > fb c
  | Ge (a, b) ->
      let fa, fb = int_operands env a b in
      fun c -> fa c >= fb c
  | And (a, b) ->
      let fa = bool_expr env a and fb = bool_expr env b in
      fun c -> fa c && fb c
  | Or (a, b) ->
      let fa = bool_expr env a and fb = bool_expr env b in
      fun c -> fa c || fb c
  | Not a ->
      let fa = bool_expr env a in
      fun c -> not (fa c)
  | Ref _ -> .

let size prefix e = int_expr { prefix; sizeof = true } e
let gate prefix e = bool_expr { prefix; sizeof = true } e
let cond prefix e = bool_expr { prefix; sizeof = false } e

(* {1 Fields the cursor moves over}

   Each starts where the cursor is ([wpos]) and leaves it where the field
   ends, after the checks the layout pass makes of the same field. *)

type _ span = Slice_span : Slice.t span | String_span : string span

(* A byte span of [size] bytes, which must fit before [wend]. *)
let span (type a) (kind : a span) size : cursor -> a =
 fun c ->
  let cur = c.wpos and sz = size c in
  if sz < 0 || cur > Input_end.to_int c.wend - sz then unwalked ();
  c.wpos <- cur + sz;
  match kind with
  | Slice_span -> Slice.make_or_eod c.wbuf ~first:cur ~length:sz
  | String_span -> Bytes.sub_string c.wbuf cur sz

(* A field present when [present] holds; an absent one takes no bytes. *)
let gated present (item : cursor -> 'a) : cursor -> 'a option =
 fun c -> if present c then Some (item c) else None

(* A sub-codec in a region of [size] bytes, which it must not pass, nor fall
   short of unless [at_most]; the cursor goes on at the region's end. *)
let region ~at_most size (sub : cursor -> 'a) : cursor -> 'a =
 fun c ->
  let cur = c.wpos and sz = size c and stop = c.wend and base = c.wbase in
  if sz < 0 || cur > Input_end.to_int stop - sz then unwalked ();
  c.wend <- Input_end.narrow stop (cur + sz);
  let v = sub c in
  let n = c.wpos - cur in
  if n > sz || ((not at_most) && n <> sz) then unwalked ();
  c.wbase <- base;
  c.wend <- stop;
  c.wpos <- cur + sz;
  v

(* A sub-codec measuring itself. *)
let sub (sub : cursor -> 'a) : cursor -> 'a =
 fun c ->
  let base = c.wbase in
  let v = sub c in
  c.wbase <- base;
  v

(* A fixed-size sub-codec at offset [off] of the record: the cursor stays
   where it was. *)
let fixed_sub off (sub : cursor -> 'a) : cursor -> 'a =
 fun c ->
  let base = c.wbase and pos = c.wpos in
  c.wpos <- base + off;
  let v = sub c in
  c.wbase <- base;
  c.wpos <- pos;
  v

(* {1 The record}

   A field at a fixed offset of the record reads in place, the frame having
   been checked; any other the cursor moves over. *)

let scalar : type a. a scalar -> cursor -> a = function
  | Byte o -> fun c -> UInt8.unsafe_get c.wbuf (c.wbase + o)
  | U16le o -> fun c -> UInt16.unsafe_le c.wbuf (c.wbase + o)
  | U16be o -> fun c -> UInt16.unsafe_be c.wbuf (c.wbase + o)
  | U32le o -> fun c -> UInt32.unsafe_le c.wbuf (c.wbase + o)
  | U32be o -> fun c -> UInt32.unsafe_be c.wbuf (c.wbase + o)
  | Sbyte o -> fun c -> SInt8.unsafe_get c.wbuf (c.wbase + o)
  | I16le o -> fun c -> SInt16.unsafe_le c.wbuf (c.wbase + o)
  | I16be o -> fun c -> SInt16.unsafe_be c.wbuf (c.wbase + o)
  | I32le o -> fun c -> SInt32.unsafe_le c.wbuf (c.wbase + o)
  | I32be o -> fun c -> SInt32.unsafe_be c.wbuf (c.wbase + o)
  | Bits8 { off; shift; mask } ->
      fun c -> (Unchecked.u8 c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      fun c -> (Unchecked.u16le c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      fun c -> (Unchecked.u16be c.wbuf (c.wbase + off) lsr shift) land mask

let read : type a. a walk_read -> cursor -> a = function
  | Scalar s -> scalar s
  | String (off, n) -> fun c -> Bytes.sub_string c.wbuf (c.wbase + off) n
  | Slice (off, n) ->
      fun c -> Slice.make_or_eod c.wbuf ~first:(c.wbase + off) ~length:n
  | Walk w -> w

type (_, _) readers =
  | Nil : ('r, 'r) readers
  | Cons :
      (cursor -> 'a) * ('rest, 'result) readers
      -> ('a -> 'rest, 'result) readers

let readers : type full result.
    (full, result) walk_readers -> (full, result) readers =
 fun readers ->
  let rec go : type mid.
      (full, mid) walk_readers ->
      (mid, result) readers ->
      (full, result) readers =
   fun readers acc ->
    match readers with
    | WNil -> acc
    | WSnoc (rest, r) -> go rest (Cons (read r, acc))
  in
  go readers Nil

(* The application of [f] to the fields the readers read, in field order:
   one closure per arity, which makes a direct call per field and one
   saturated call of [f]. [None] past 32 fields. One row per arity; the
   formatter would staircase the nested [Cons] patterns. *)
let stage : type mid result.
    mid -> (mid, result) readers -> (cursor -> result) option =
 fun f readers ->
  match readers with
  | Nil -> Some (fun _ -> f)
  | Cons (r1, Nil) ->
      Some (fun c -> let v1 = r1 c in f v1)
  | Cons (r1, Cons (r2, Nil)) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in f v1 v2)
  | Cons (r1, Cons (r2, Cons (r3, Nil))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in f v1 v2 v3)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Nil)))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in f v1 v2 v3 v4)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Nil))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in f v1 v2 v3 v4 v5)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Nil)))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in f v1 v2 v3 v4 v5 v6)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Nil))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in f v1 v2 v3 v4 v5 v6 v7)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Nil)))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in f v1 v2 v3 v4 v5 v6 v7 v8)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Nil))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Nil)))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Nil))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Nil)))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Nil))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Nil)))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Nil))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Nil)))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Nil))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Nil)))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Nil))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Nil)))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Nil))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Nil)))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Nil))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Nil)))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Nil))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Nil)))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Nil))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Nil)))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Nil))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Nil)))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Cons (r31, Nil))))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in let v31 = r31 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Cons (r31, Cons (r32, Nil)))))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in let v31 = r31 c in let v32 = r32 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32)
  | _ -> None
[@@ocamlformat "disable"]

(* {1 The walk of a record}

   The frame, then the checks of the fixed-offset fields, then each field read
   in order: in place, or by moving the cursor over it, which checks it as the
   layout pass would. So the walk accepts what the layout pass and the check
   pass it leaves accept, and reads each field where the read pass would, with
   each size and gate evaluated once. A sub-codec's walk builds its record
   before the walk of the record around it has finished checking: a
   constructor can see a record the walk then rejects. *)

let walk_checks checks =
  let within =
    List.filter_map
      (function Within (s, set) -> Some (s, set) | Holds _ -> None)
      checks
  and holds =
    List.filter_map (function Holds f -> Some f | Within _ -> None) checks
  in
  let flat, tables = Codec_intervals.flatten_intervals (Array.of_list within) in
  (flat, tables, Array.of_list holds)

let rec holds_all holds c i =
  i >= Array.length holds
  || ((Array.unsafe_get holds i) c && holds_all holds c (i + 1))

let record_walk ~min_size ~start checks body =
  let flat, tables, holds = walk_checks checks in
  let n = Array.length flat in
  fun c ->
    let base = c.wpos and stop = c.wend in
    if base < 0 || base > Input_end.to_int stop - min_size then unwalked ();
    let buf = c.wbuf in
    if n > 0 && Codec_intervals.scan_intervals flat tables buf base n 0 >= 0
    then unwalked ();
    c.wbase <- base;
    if not (holds_all holds c 0) then unwalked ();
    c.wpos <- base + start;
    body c

(* The walk of a sealed record without parameters, while every field has one
   and its [where] clause reads only fixed-offset fields. *)
let record ~n_params ~min_size ~prefix ~where make = function
  | Some w when n_params = 0 -> (
      match Option.map (cond prefix) where with
      | exception Exit -> None
      | where ->
          let checks =
            Option.fold ~none:[] ~some:(fun f -> [ Holds f ]) where
            @ List.rev w.wchecks_rev
          in
          let start = Option.value ~default:min_size w.wstart in
          Option.map
            (record_walk ~min_size ~start checks)
            (stage make (readers w.wreaders)))
  | Some _ | None -> None

(* A decode through [walk], or [general] when the walk does not accept. *)
let decode walk general buf off =
  let c =
    { wbuf = buf; wbase = off; wpos = off; wend = Input_end.of_bytes buf }
  in
  match walk c with
  | v -> v
  | exception (Types.Unwalked | Types.Parse_error _) -> general buf off
