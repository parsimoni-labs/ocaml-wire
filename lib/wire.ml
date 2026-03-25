(* Wire: Dependent Data Descriptions for EverParse 3D *)

module Staged = Staged
module UInt32 = UInt32
module UInt63 = UInt63
module Action = Action
module Param = Param
module Field = Field
module Codec_backend = Codec
module Everparse_backend = Everparse
include Types

type bitfield = U8 | U16 | U16be | U32 | U32be
type param = Types.param

let param_name (p : param) = p.param_name
let param_is_mutable (p : param) = p.mutable_

let param_c_type (p : param) =
  let (Types.Pack_typ t) = p.param_typ in
  Types.c_type_of t

let _field_ref = Types.ref
let map ~decode ~encode inner = Types.map decode encode inner
let bool = Types.bool
let empty = Types.unit
let size = Types.field_wire_size
let lookup = Types.cases

let bits ~width = function
  | U8 -> Types.bits ~width Types.bf_uint8
  | U16 -> Types.bits ~width Types.bf_uint16
  | U16be -> Types.bits ~width Types.bf_uint16be
  | U32 -> Types.bits ~width Types.bf_uint32
  | U32be -> Types.bits ~width Types.bf_uint32be

module Expr = struct
  include Expr

  let true_ = Types.true_
  let false_ = Types.false_
end

module Reader = Bytesrw.Bytes.Reader
module Slice = Bytesrw.Bytes.Slice

type ctx = Eval.ctx

let empty_ctx = Eval.empty
let val_to_int = Eval.int_of
let eval_expr = Eval.expr

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

(* Bitfield accumulator for packed struct parsing. *)
type bf_accum = {
  bf_base : bitfield_base;
  bf_word : int;
  bf_bits_used : int;
  bf_total_bits : int;
}

let bf_read_word dec base =
  let off = read_small dec (Bitfield.byte_size base) in
  Bitfield.read_word base dec.i off

let bf_extract accum width =
  let value =
    Bitfield.extract ~base:accum.bf_base ~total:accum.bf_total_bits
      ~bits_used:accum.bf_bits_used ~width accum.bf_word
  in
  (value, { accum with bf_bits_used = accum.bf_bits_used + width })

let bf_has_room accum width = accum.bf_bits_used + width <= accum.bf_total_bits

let parse_bits dec base width =
  let word = bf_read_word dec base in
  let total = Bitfield.total_bits base in
  Bitfield.extract ~base ~total ~bits_used:0 ~width word

let[@inline] parse_int dec n get =
  let off = read_small dec n in
  get dec.i off

let parse_bf_field dec accum_opt base width =
  match accum_opt with
  | Some accum when Bitfield.equal accum.bf_base base && bf_has_room accum width
    ->
      let v, new_accum = bf_extract accum width in
      let accum_opt' =
        if new_accum.bf_bits_used = new_accum.bf_total_bits then None
        else Some new_accum
      in
      (v, accum_opt')
  | _ ->
      let word = bf_read_word dec base in
      let total = Bitfield.total_bits base in
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

let apply_action ctx action =
  try Eval.action ctx action with Parse_error e -> raise (Parse_exn e)

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
  | Uint32 Little -> (parse_int dec 4 UInt32.le, ctx)
  | Uint32 Big -> (parse_int dec 4 UInt32.be, ctx)
  | Uint63 Little -> (parse_int dec 8 UInt63.le, ctx)
  | Uint63 Big -> (parse_int dec 8 UInt63.be, ctx)
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
        | [] ->
            raise
              (Parse_exn
                 (Invalid_tag (val_to_int tag tag_val |> Option.value ~default:0)))
        | (Some expected, case_typ) :: rest ->
            if expected = tag_val then parse_with dec ctx' case_typ
            else find_case rest
        | (None, case_typ) :: _ -> parse_with dec ctx' case_typ
      in
      find_case cases
  | Struct { fields; where; _ } ->
      let (), ctx' = parse_struct_fields dec ctx fields in
      (match where with
      | Some cond when not (eval_expr ctx' cond) ->
          raise (Parse_exn (Constraint_failed "where clause"))
      | _ -> ());
      ((), ctx')
  | Map { inner; decode; _ } -> (
      let v, ctx' = parse_with dec ctx inner in
      match decode v with
      | r -> (r, ctx')
      | exception Parse_error e -> raise (Parse_exn e))
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

and parse_struct_fields dec ctx fields =
  let start_pos = dec.position in
  let parse_field_with_bf : type a.
      bf_accum option -> a typ -> a * bf_accum option =
   fun accum_opt typ ->
    match typ with
    | Bits { width; base } -> parse_bf_field dec accum_opt base width
    | _ ->
        let v, _ = parse_with dec ctx typ in
        (v, None)
  in
  let rec go ctx' accum_opt field_idx = function
    | [] -> ((), ctx')
    | Field { field_name; field_typ = Bits _ as ft; constraint_; action }
      :: rest ->
        let ctx' =
          Eval.set_pos ctx' ~sizeof_this:(dec.position - start_pos)
            ~field_pos:field_idx
        in
        let v, accum_opt' = parse_field_with_bf accum_opt ft in
        let ctx'' =
          match field_name with Some n -> Eval.bind ctx' n v | None -> ctx'
        in
        check_constraint ctx'' constraint_;
        let ctx''' = apply_action ctx'' action in
        go ctx''' accum_opt' (field_idx + 1) rest
    | Field { field_name; field_typ; constraint_; action } :: rest ->
        let ctx' =
          Eval.set_pos ctx' ~sizeof_this:(dec.position - start_pos)
            ~field_pos:field_idx
        in
        let v, ctx'' = parse_with dec ctx' field_typ in
        let ctx'' =
          match field_name with
          | Some n -> (
              match val_to_int field_typ v with
              | Some iv -> Eval.bind ctx'' n iv
              | None -> ctx'')
          | None -> ctx''
        in
        check_constraint ctx'' constraint_;
        let ctx''' = apply_action ctx'' action in
        go ctx''' None (field_idx + 1) rest
  in
  go ctx None 0 fields

let decode typ reader =
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
      UInt32.le buf off
  | Uint32 Big ->
      check_eof len (off + 4);
      UInt32.be buf off
  | Uint63 Little ->
      check_eof len (off + 8);
      UInt63.le buf off
  | Uint63 Big ->
      check_eof len (off + 8);
      UInt63.be buf off
  | Uint64 Little ->
      check_eof len (off + 8);
      Bytes.get_int64_le buf off
  | Uint64 Big ->
      check_eof len (off + 8);
      Bytes.get_int64_be buf off
  | Bits { width; base } ->
      check_eof len (off + Bitfield.byte_size base);
      Bitfield.read_word base buf off land ((1 lsl width) - 1)
  | Unit -> ()
  | Map { inner; decode; _ } -> (
      match decode (parse_direct inner buf off len) with
      | r -> r
      | exception Parse_error e -> raise (Parse_exn e))
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

let decode_string typ s =
  let buf = Bytes.unsafe_of_string s in
  let len = Bytes.length buf in
  match parse_direct typ buf 0 len with
  | v -> Ok v
  | exception Parse_exn e -> Error e

let decode_bytes typ b =
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

module Codec = struct
  include Codec_backend

  let decode t buf off =
    try Ok (Codec_backend.decode t buf off) with Parse_error e -> Error e
end

module Everparse = Everparse_backend
module Ascii = Ascii

module Private = struct
  module UInt32 = UInt32
  module UInt63 = UInt63
  module Types = Types
  module Eval = Eval

  let param_name = param_name
  let param_is_mutable = param_is_mutable
  let param_c_type = param_c_type
  let ml_type_of = Types.ml_type_of
  let c_type_of = Types.c_type_of
end
