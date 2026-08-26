(* Wire: Dependent Data Descriptions for EverParse 3D *)

module Staged = Staged
module UInt32 = UInt32
module UInt63 = UInt63
module Action = Action
module Param = Param
module Field = Field
module Codec = Codec
module Everparse = Everparse
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

let bool (b : Stdlib.Bool.t) : _ Types.expr =
  if b then Types.true_ else Types.false_

let bit = Types.bool
let empty = Types.unit
let size = Types.field_wire_size
let lookup = Types.cases

(* IEEE 754 predicates compile to bit-mask checks over the float's bit
   pattern, which [build_populate] stores for float fields: a float32's in
   the int slot (its 31 low bits carry the whole exponent and mantissa on any
   platform), a float64's in the int64 slot, where the exponent bits 52-62
   stay exact even on a narrow-int platform. Both produce the same
   [(v >> N) & M] / [== M] checks on wire's OCaml decoder and EverParse's
   verified C decoder. *)

let is_float64 f =
  match Field.typ f with
  | Types.Float32 _ -> false
  | Types.Float64 _ -> true
  | _ -> invalid_arg "Wire: not a float field"

let is_finite (f : float Field.t) : bool Types.expr =
  if is_float64 f then
    let r = Types.Ref (Types.I64, Field.name f) in
    Ne (Land64 (Lsr64 (r, Int 52), Int64 0x7FFL), Int64 0x7FFL)
  else
    let r = Field.ref f in
    Expr.(Land (Lsr (r, Int 23), Int 0xFF) <> Int 0xFF)

let is_nan (f : float Field.t) : bool Types.expr =
  if is_float64 f then
    let r = Types.Ref (Types.I64, Field.name f) in
    And
      ( Eq (Land64 (Lsr64 (r, Int 52), Int64 0x7FFL), Int64 0x7FFL),
        Ne (Land64 (r, Int64 0xF_FFFF_FFFF_FFFFL), Int64 0L) )
  else
    let r = Field.ref f in
    Expr.(
      Land (Lsr (r, Int 23), Int 0xFF) = Int 0xFF
      && Land (r, Int 0x007F_FFFF) <> Int 0)

let codec (c : 'r Codec.t) : 'r typ =
  let codec_decode = Codec.embed_decode_ctx c in
  let codec_validate = Codec.embed_validate_ctx c in
  let codec_encode = Codec.embed_encode_ctx c in
  let codec_field_readers = Codec.field_readers_ctx c in
  let codec_struct = Codec.to_struct c in
  let codec_size_of_value = Codec.size_of_value c in
  match Codec.wire_size_info_ctx c with
  | `Fixed n ->
      Codec
        {
          codec_name = Codec.name c;
          codec_decode;
          codec_validate;
          codec_encode;
          codec_fixed_size = Some n;
          codec_size_of = (fun _ctx _buf _off -> n);
          codec_size_of_value;
          codec_field_readers;
          codec_struct;
        }
  | `Variable size_of ->
      Codec
        {
          codec_name = Codec.name c;
          codec_decode;
          codec_validate;
          codec_encode;
          codec_fixed_size = None;
          codec_size_of = size_of;
          codec_size_of_value;
          codec_field_readers;
          codec_struct;
        }

type ('elt, 'seq) seq_map = ('elt, 'seq) Types.seq_map =
  | Seq_map : {
      empty : 'b;
      add : 'b -> 'elt -> 'b;
      finish : 'b -> 'seq;
      iter : ('elt -> unit) -> 'seq -> unit;
    }
      -> ('elt, 'seq) seq_map

let seq_list = Types.seq_list
let array_seq = Types.array_seq

let rest_bytes total =
  Types.byte_array ~size:Types.(Sub (Param_ref total, Sizeof_this))

let bits ?(bit_order = Types.Msb_first) ~width bf =
  let base =
    match bf with
    | U8 -> Types.bf_uint8
    | U16 -> Types.bf_uint16
    | U16be -> Types.bf_uint16be
    | U32 -> Types.bf_uint32
    | U32be -> Types.bf_uint32be
  in
  Types.bits ~bit_order ~width base

module Expr = struct
  include Types.Expr

  let true_ = Types.true_
  let false_ = Types.false_
  let bool b = if b then Types.true_ else Types.false_
end

module Reader = Bytesrw.Bytes.Reader
module Slice = Bytesrw.Bytes.Slice

(* [n] bytes starting at [off] must fit inside the region ending at [limit].
   {!Types.Unexpected_eof} counts bytes, not positions: the value needed [n] of
   them and the region had only [limit - off] left. *)
let[@inline] check_eof limit ~off ~n =
  if off + n > limit then
    raise_eof ~at:limit ~expected:n ~got:(max 0 (limit - off))

(* A span of [n] bytes starting at [off]. [n] comes from a size expression, so
   an underflowing subtraction or a negative literal reaches here as data: a
   bare [check_eof] would wave it through and [Bytes.sub] would then raise an
   [Invalid_argument] that escapes [of_string]'s result. *)
let[@inline] check_span len ~off ~n =
  if n < 0 then raise_out_of_range ~at:off (Int64.of_int n);
  check_eof len ~off ~n

(* The single decoder kernel. Bytes-based, returns [(value, end_off)].
   All types handled here -- no fallback. Expressions are evaluated in
   [Eval.empty] (no field bindings); types using [Ref]/[Sizeof_this]/
   [Field_pos] only make sense inside a [Struct], which goes through
   [Codec.validator_of_struct] where the int-array context is wired up. *)
(* Helpers extracted from [parse_direct] to keep the dispatch readable
   and short. Each handles one composite case. *)

let parse_all_zeros buf off len =
  let n = len - off in
  let s = Bytes.sub_string buf off n in
  let rec check i =
    if i >= n then s
    else if s.[i] <> '\000' then raise_non_zero_padding ~at:(off + i)
    else check (i + 1)
  in
  (check 0, len)

let parse_codec_typ codec_decode fixed_size size_of buf off len =
  (* A variable-size codec computes its span by reading length / gate fields
     from the buffer. Those reads bound-check themselves, so a buffer too short
     to hold them already fails as end-of-input at the field that was missing;
     a misuse in the size expression, or an exception from a user [map]
     callback, reaches the caller unchanged. *)
  let sz = match fixed_size with Some n -> n | None -> size_of buf off in
  check_span len ~off ~n:sz;
  (codec_decode buf off, off + sz)

(* Only a closed enum enforces membership; an open enum names known codes but
   accepts any value. [Codec.decode] gates on [closed] the same way, so the two
   decode paths agree on an unlisted code. *)
let check_enum_membership ~at ~closed cases v =
  if closed then Types.check_enum_decode ~at ~cases v

(* [Codec.validator_of_struct] compiles a struct into a validator whose scratch
   is domain-local, and a domain-local scratch is backed by a [Domain.DLS] key
   that is never reclaimed. Compiling a fresh validator per decode would leak a
   key per call, so cache the validator per struct. The cache is itself
   domain-local, which keeps it lock-free (each domain compiles its structs
   once) and avoids sharing a validator, hence its scratch, across domains.
   Keyed by physical identity, so the usual case of a codec built once and
   decoded in a loop hits the cache.

   Entries are never evicted. The [Domain.DLS] key outlives the validator that
   allocated it, so dropping an entry does not give the key back: the next
   decode of that struct compiles again and burns a second key. Physical
   identity is also unhashable here, since two structs built from one schema are
   indistinguishable to any hash, so no table turns the lookup sublinear either.
   What the cache costs is therefore one entry per distinct struct the domain
   has decoded, which is what compiling those structs already cost. *)
let struct_validators =
  Domain.DLS.new_key (fun () ->
      (Stdlib.ref [] : (Types.struct_ * Codec.validator) list Stdlib.ref))

exception Uncached

(* Raises rather than returning an option: a hit is on the decode path, and the
   option would allocate on every decode of a struct type. *)
let rec cached_validator s = function
  | [] -> raise_notrace Uncached
  | (k, v) :: rest -> if k == s then v else cached_validator s rest

let validator_for_struct s =
  let cache = Domain.DLS.get struct_validators in
  match cached_validator s !cache with
  | v -> v
  | exception Uncached ->
      let v = Codec.validator_of_struct s in
      cache := (s, v) :: !cache;
      v

let parse_struct_typ s buf off len =
  let v = validator_for_struct s in
  let sz = Codec.struct_size_of v buf off in
  check_span len ~off ~n:sz;
  Codec.validate_struct v buf off;
  ((), off + sz)

let parse_fixed size get buf off len =
  check_eof len ~off ~n:size;
  (get buf off, off + size)

let int32_le = SInt32.le
let int32_be = SInt32.be
let float32_le buf off = Int32.float_of_bits (Bytes.get_int32_le buf off)
let float32_be buf off = Int32.float_of_bits (Bytes.get_int32_be buf off)
let float64_le buf off = Int64.float_of_bits (Bytes.get_int64_le buf off)
let float64_be buf off = Int64.float_of_bits (Bytes.get_int64_be buf off)

let rec parse_direct : type a. a typ -> bytes -> int -> int -> a * int =
 fun typ buf off len ->
  match typ with
  | Uint8 -> parse_fixed 1 Bytes.get_uint8 buf off len
  | Uint16 Little -> parse_fixed 2 Bytes.get_uint16_le buf off len
  | Uint16 Big -> parse_fixed 2 Bytes.get_uint16_be buf off len
  | Uint32 Little -> parse_fixed 4 UInt32.le buf off len
  | Uint32 Big -> parse_fixed 4 UInt32.be buf off len
  | Uint64 Little -> parse_fixed 8 Bytes.get_int64_le buf off len
  | Uint64 Big -> parse_fixed 8 Bytes.get_int64_be buf off len
  | Int8 -> parse_fixed 1 Bytes.get_int8 buf off len
  | Int16 Little -> parse_fixed 2 Bytes.get_int16_le buf off len
  | Int16 Big -> parse_fixed 2 Bytes.get_int16_be buf off len
  | Int32 Little -> parse_fixed 4 int32_le buf off len
  | Int32 Big -> parse_fixed 4 int32_be buf off len
  | Int64 Little -> parse_fixed 8 Bytes.get_int64_le buf off len
  | Int64 Big -> parse_fixed 8 Bytes.get_int64_be buf off len
  | Float32 Little -> parse_fixed 4 float32_le buf off len
  | Float32 Big -> parse_fixed 4 float32_be buf off len
  | Float64 Little -> parse_fixed 8 float64_le buf off len
  | Float64 Big -> parse_fixed 8 float64_be buf off len
  | Uint_var { size; endian } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      (Uint_var.read endian buf off n, off + n)
  | Bits { width; base; bit_order } ->
      let sz = Bitfield.byte_size base in
      check_eof len ~off ~n:sz;
      let total = Bitfield.total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      let mask = (1 lsl width) - 1 in
      let v =
        match base with
        | U32 Little when not Bitfield.int_holds_u32 ->
            Bitfield.u32_field_le buf off shift mask
        | U32 Big when not Bitfield.int_holds_u32 ->
            Bitfield.u32_field_be buf off shift mask
        | _ -> (Bitfield.read_word base buf off lsr shift) land mask
      in
      (v, off + sz)
  | Unit -> ((), off)
  | All_bytes -> (Bytes.sub_string buf off (len - off), len)
  | All_zeros -> parse_all_zeros buf off len
  | Zeroterm ->
      let nul = Codec.zeroterm_nul_pos buf ~first:off ~limit:len in
      (Bytes.sub_string buf off (nul - off), nul + 1)
  | Zeroterm_at_most { size } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      let nul = Codec.zeroterm_nul_pos buf ~first:off ~limit:(off + n) in
      (Bytes.sub_string buf off (nul - off), off + n)
  | Byte_array { size } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      (Bytes.sub_string buf off n, off + n)
  | Byte_array_where { size; elt_var; cond } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      let bad = Eval.bad_byte ~elt_var ~cond buf ~first:off ~len:n in
      if bad >= 0 then raise_constraint ~at:(off + bad) ~which:Per_byte ();
      (Bytes.sub_string buf off n, off + n)
  | Byte_slice { size } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      (Slice.make_or_eod buf ~first:off ~length:n, off + n)
  | Single_elem { size; elem; at_most } ->
      let n = Eval.expr Eval.empty size in
      check_span len ~off ~n;
      let v, inner_end = parse_direct elem buf off (off + n) in
      let consumed = inner_end - off in
      if (not at_most) && consumed <> n then
        raise_eof ~at:inner_end ~expected:n ~got:consumed;
      (v, off + n)
  | Map { inner; decode; index_bound; _ } ->
      let v, off' = parse_direct inner buf off len in
      (Types.map_decode ~index_bound decode ~at:off v, off')
  | Where { cond; inner } -> parse_where inner cond buf off len
  | Enum { base; cases; closed; _ } ->
      let v, off' = parse_direct base buf off len in
      check_enum_membership ~at:off ~closed cases v;
      (v, off')
  | Codec { codec_decode; codec_fixed_size; codec_size_of; _ } ->
      parse_codec_typ
        (codec_decode Types.unbound_eval_ctx)
        codec_fixed_size
        (codec_size_of Types.unbound_eval_ctx)
        buf off len
  | Struct s -> parse_struct_typ s buf off len
  | Casetype { cases; tag; _ } -> parse_casetype tag cases buf off len
  | Optional { present; inner } ->
      if Eval.expr Eval.empty present then
        let v, off' = parse_direct inner buf off len in
        (Some v, off')
      else (None, off)
  | Optional_or { present; inner; default } ->
      if Eval.expr Eval.empty present then parse_direct inner buf off len
      else (default, off)
  | Array { len = len_expr; elem; seq } ->
      let n = Eval.expr Eval.empty len_expr in
      parse_array_loop ~elem ~seq buf off len ~n
  | Repeat { size; elem; seq } ->
      let budget = Eval.expr Eval.empty size in
      parse_repeat_loop ~elem ~seq buf off len ~budget
  | Type_ref _ -> invalid_arg "Wire.type_ref: decoding needs a type registry"
  | Qualified_ref _ ->
      invalid_arg "Wire.qualified_ref: decoding needs a type registry"
  | Apply _ -> invalid_arg "Wire.apply: decoding needs a type registry"

and parse_where : type a. a typ -> bool expr -> bytes -> int -> int -> a * int =
 fun inner cond buf off len ->
  let v, off' = parse_direct inner buf off len in
  if Eval.expr Eval.empty cond then (v, off')
  else raise_constraint ~at:off ~which:Where ()

and parse_casetype : type a k.
    k typ -> (a, k) case_branch list -> bytes -> int -> int -> a * int =
 fun tag cases buf off len ->
  let tag_val, off' = parse_direct tag buf off len in
  let rec find_case = function
    | [] ->
        raise_invalid_tag ~at:off
          (Option.value ~default:0 (Eval.int_of tag tag_val))
    | Case_branch { cb_tag = Some expected; cb_inner; cb_inject; _ } :: rest ->
        if expected = tag_val then
          let body, off'' = parse_direct cb_inner buf off' len in
          (cb_inject tag_val body, off'')
        else find_case rest
    | Case_branch { cb_tag = None; cb_inner; cb_inject; _ } :: _ ->
        let body, off'' = parse_direct cb_inner buf off' len in
        (cb_inject tag_val body, off'')
  in
  find_case cases

and parse_array_loop : type elt seq.
    elem:elt typ ->
    seq:(elt, seq) seq_map ->
    bytes ->
    int ->
    int ->
    n:int ->
    seq * int =
 fun ~elem ~seq:(Seq_map s) buf off len ~n ->
  let rec loop acc off' i =
    if i >= n then (s.finish acc, off')
    else
      let v, off'' = parse_direct elem buf off' len in
      loop (s.add acc v) off'' (i + 1)
  in
  loop s.empty off 0

and parse_repeat_loop : type elt seq.
    elem:elt typ ->
    seq:(elt, seq) seq_map ->
    bytes ->
    int ->
    int ->
    budget:int ->
    seq * int =
 fun ~elem ~seq:(Seq_map s) buf off len ~budget ->
  let start = off in
  if budget < 0 then raise_eof ~at:off ~expected:budget ~got:(max 0 (len - off));
  check_eof len ~off:start ~n:budget;
  let region_end = start + budget in
  let rec loop acc off' =
    if off' = region_end then (s.finish acc, off')
    else
      let v, off'' = parse_direct elem buf off' region_end in
      (* An element that consumes nothing never moves the cursor to
         [region_end], so the byte-budget loop would spin. A literal zero-size
         span is refused by [Field.repeat], but a [uint ~size] whose size
         expression evaluates to zero is only detectable here, so report the
         same eof [Codec.decode]'s compiled repeat reports for it. *)
      if off'' <= off' then
        raise_eof ~at:off' ~expected:(off'' - off') ~got:(region_end - off');
      loop (s.add acc v) off''
  in
  loop s.empty off

exception Parse_error = Parse_error

let of_string_exn typ s =
  let buf = Bytes.unsafe_of_string s in
  fst (parse_direct typ buf 0 (Bytes.length buf))

let of_string typ s =
  match of_string_exn typ s with
  | v -> Ok v
  | exception Parse_error e -> Error e

let of_bytes_exn typ b = fst (parse_direct typ b 0 (Bytes.length b))

let of_bytes typ b =
  match of_bytes_exn typ b with v -> Ok v | exception Parse_error e -> Error e

let drain_reader reader =
  let buf = Buffer.create 256 in
  let rec loop () =
    let slice = Reader.read reader in
    if Slice.is_eod slice then Buffer.to_bytes buf
    else begin
      Buffer.add_subbytes buf (Slice.bytes slice) (Slice.first slice)
        (Slice.length slice);
      loop ()
    end
  in
  loop ()

let rec typ_consumes_rest : type a. a typ -> bool = function
  | All_bytes | All_zeros -> true
  | Map { inner; _ } -> typ_consumes_rest inner
  | Where { inner; _ } -> typ_consumes_rest inner
  | Enum { base; _ } -> typ_consumes_rest base
  | Optional { inner; _ } -> typ_consumes_rest inner
  | Optional_or { inner; _ } -> typ_consumes_rest inner
  | Array { elem; _ } -> typ_consumes_rest elem
  | Repeat { elem; _ } -> typ_consumes_rest elem
  | Codec { codec_struct; _ } | Struct codec_struct ->
      struct_consumes_rest codec_struct
  | Casetype { tag; cases; _ } ->
      typ_consumes_rest tag
      || List.exists
           (fun (Case_branch { cb_inner; _ }) -> typ_consumes_rest cb_inner)
           cases
  | Apply { typ; _ } -> typ_consumes_rest typ
  | Single_elem _ -> false
  | _ -> false

and struct_consumes_rest (s : struct_) =
  List.exists (fun (Field f) -> typ_consumes_rest f.field_typ) s.fields

let push_back_bytes reader bytes first (length : int) =
  if length > 0 then Reader.push_back reader (Slice.make bytes ~first ~length)

let read_exact reader (n : int) =
  let buf = Bytes.create n in
  let rec loop off =
    if off >= n then buf
    else
      let slice = Reader.read reader in
      if Slice.is_eod slice then begin
        push_back_bytes reader buf 0 off;
        raise_eof ~at:off ~expected:n ~got:off
      end
      else
        let slice_len = Slice.length slice in
        let need = n - off in
        let take = Int.min need slice_len in
        Bytes.blit (Slice.bytes slice) (Slice.first slice) buf off take;
        if slice_len > take then
          Reader.push_back reader (Slice.drop_first_or_eod take slice);
        loop (off + take)
  in
  loop 0

(* Parse [bytes] and keep the reader transactional: on success push back the
   bytes past the decoded value, on parse error push back everything so the
   reader is restored to its position before the failed decode. *)
let parse_or_rewind typ reader bytes len =
  match parse_direct typ bytes 0 len with
  | v, off ->
      push_back_bytes reader bytes off (len - off);
      v
  | exception Parse_error e ->
      push_back_bytes reader bytes 0 len;
      raise (Parse_error e)

let missing_more_input e =
  (* A truncated zeroterm reports [Missing_terminator] (from
     [Codec.zeroterm_nul_pos]); both it and an [Unexpected_eof] mean the reader
     should fetch more input before giving up. *)
  match e.kind with
  | Unexpected_eof _ | Missing_terminator -> true
  | _ -> false

let of_reader_incremental typ reader =
  let buf = Buffer.create 256 in
  let rec loop () =
    let bytes = Buffer.to_bytes buf in
    let len = Bytes.length bytes in
    let read_more on_eod =
      let slice = Reader.read reader in
      if Slice.is_eod slice then on_eod ()
      else begin
        Buffer.add_subbytes buf (Slice.bytes slice) (Slice.first slice)
          (Slice.length slice);
        loop ()
      end
    in
    match parse_direct typ bytes 0 len with
    | v, off ->
        push_back_bytes reader bytes off (len - off);
        v
    | exception Parse_error e when missing_more_input e ->
        read_more (fun () ->
            push_back_bytes reader bytes 0 len;
            raise (Parse_error e))
    | exception Parse_error e ->
        push_back_bytes reader bytes 0 len;
        raise (Parse_error e)
  in
  loop ()

let of_reader_exn typ reader =
  if typ_consumes_rest typ then
    let bytes = drain_reader reader in
    parse_or_rewind typ reader bytes (Bytes.length bytes)
  else
    match Types.field_wire_size typ with
    | Some n ->
        let bytes = read_exact reader n in
        parse_or_rewind typ reader bytes n
    | None -> of_reader_incremental typ reader

let of_reader typ reader =
  match of_reader_exn typ reader with
  | v -> Ok v
  | exception Parse_error e -> Error e
(* Binary encoding with Bytesrw.Bytes.Writer *)

module Writer = Bytesrw.Bytes.Writer

(* Encoder state *)
(* Buffered encoder -- writes accumulate in o, flushed as a single Slice.t.
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

let[@inline] write_int8 enc v =
  ensure enc 1;
  Bytes.set_int8 enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 1

let[@inline] write_int16_le enc v =
  ensure enc 2;
  Bytes.set_int16_le enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 2

let[@inline] write_int16_be enc v =
  ensure enc 2;
  Bytes.set_int16_be enc.o enc.o_next v;
  enc.o_next <- enc.o_next + 2

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

let encode_codec ~encode ~fixed_size ~size_of_value v enc =
  let sz = match fixed_size with Some n -> n | None -> size_of_value v in
  let tmp = Bytes.create sz in
  let _ : int = encode v tmp 0 in
  write_string enc (Bytes.unsafe_to_string tmp)

let check_byte_field_size size ~actual =
  Types.check_byte_field_size ~expected:(Eval.expr Eval.empty size) ~actual

(* The single encoder kernel. Writes [v] to [enc]. Top-level expressions
   are evaluated in [Eval.empty]; [Struct] is rejected (encode goes
   through [Codec.encode] for records). *)
let rec encode_into : type a. a typ -> a -> encoder -> unit =
 fun typ v enc ->
  match typ with
  | Uint8 ->
      Types.check_unsigned_encode ~bits:8 v;
      write_byte enc v
  | Uint16 Little ->
      Types.check_unsigned_encode ~bits:16 v;
      write_uint16_le enc v
  | Uint16 Big ->
      Types.check_unsigned_encode ~bits:16 v;
      write_uint16_be enc v
  | Uint32 Little -> write_uint32_le enc v
  | Uint32 Big -> write_uint32_be enc v
  | Uint64 Little -> write_int64_le enc v
  | Uint64 Big -> write_int64_be enc v
  | Int8 ->
      Types.check_signed_encode ~bits:8 v;
      write_int8 enc v
  | Int16 Little ->
      Types.check_signed_encode ~bits:16 v;
      write_int16_le enc v
  | Int16 Big ->
      Types.check_signed_encode ~bits:16 v;
      write_int16_be enc v
  | Int32 Little ->
      SInt32.check_encode v;
      write_int32_le enc (SInt32.to_int32 v)
  | Int32 Big ->
      SInt32.check_encode v;
      write_int32_be enc (SInt32.to_int32 v)
  | Int64 Little -> write_int64_le enc v
  | Int64 Big -> write_int64_be enc v
  | Float32 Little -> write_int32_le enc (Int32.bits_of_float v)
  | Float32 Big -> write_int32_be enc (Int32.bits_of_float v)
  | Float64 Little -> write_int64_le enc (Int64.bits_of_float v)
  | Float64 Big -> write_int64_be enc (Int64.bits_of_float v)
  | Uint_var { size; endian } ->
      let n = Eval.expr Eval.empty size in
      ensure enc n;
      Uint_var.write endian enc.o enc.o_next n v;
      enc.o_next <- enc.o_next + n
  | Bits { width; base; bit_order } -> (
      Types.check_unsigned_encode ~bits:width v;
      let mask = (1 lsl width) - 1 in
      let total = Bitfield.total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      let masked = (v land mask) lsl shift in
      match base with
      | U8 -> write_byte enc masked
      | U16 Little -> write_uint16_le enc masked
      | U16 Big -> write_uint16_be enc masked
      (* Shift in [Int32]: a native-int shift would drop bit 31 of the word
         on a narrow-int platform. *)
      | U32 Little ->
          write_int32_le enc
            (Int32.shift_left (Int32.of_int (v land mask)) shift)
      | U32 Big ->
          write_int32_be enc
            (Int32.shift_left (Int32.of_int (v land mask)) shift))
  | Unit -> ()
  | All_bytes -> write_string enc v
  | All_zeros ->
      Types.check_all_zeros_encode v;
      write_string enc v
  | Zeroterm ->
      Types.check_zeroterm_encode v;
      write_string enc v;
      write_byte enc 0
  | Zeroterm_at_most { size } ->
      Types.check_zeroterm_encode v;
      let n = Eval.expr Eval.empty size in
      let len = String.length v in
      Types.check_zeroterm_region ~region:n ~len;
      write_string enc v;
      (* Remaining bytes = NUL terminator plus any trailing padding. *)
      for _ = len to n - 1 do
        write_byte enc 0
      done
  | Where { cond; inner } ->
      Types.check_where_encode cond (Eval.expr Eval.empty cond);
      encode_into inner v enc
  | Array { len; elem; seq } ->
      let expected = Eval.expr Eval.empty len in
      Types.exact_array_elements seq ~expected v
      |> List.iter (fun elem_v -> encode_into elem elem_v enc)
  | Byte_array { size } ->
      check_byte_field_size size ~actual:(String.length v);
      write_string enc v
  | Byte_array_where { size; elt_var; cond } ->
      check_byte_field_size size ~actual:(String.length v);
      Eval.check_byte_refinement ~elt_var ~cond v;
      write_string enc v
  | Byte_slice { size } ->
      let src = Slice.bytes v in
      let off = Slice.first v in
      let len = Slice.length v in
      check_byte_field_size size ~actual:len;
      write_string enc (Bytes.sub_string src off len)
  | Single_elem { size; elem; at_most } ->
      let n = Eval.expr Eval.empty size in
      let inner_sz = Types.size_of_typ_value elem v in
      Types.check_nested_size ~at_most ~expected:n ~actual:inner_sz;
      encode_into elem v enc;
      for _ = inner_sz to n - 1 do
        write_byte enc 0
      done
  | Enum { name; base; cases; closed } ->
      if closed then
        Types.check_enum_encode ~name ~valid:(Types.enum_values cases) v;
      encode_into base v enc
  | Map { inner; encode; _ } -> encode_into inner (encode v) enc
  | Codec { codec_encode; codec_fixed_size; codec_size_of_value; _ } ->
      encode_codec
        ~encode:(fun v buf off -> codec_encode v Types.unbound_eval_ctx buf off)
        ~fixed_size:codec_fixed_size ~size_of_value:codec_size_of_value v enc
  | Optional { present; inner } ->
      if Eval.expr Eval.empty present then encode_into inner (Option.get v) enc
  | Optional_or { present; inner; _ } ->
      if Eval.expr Eval.empty present then encode_into inner v enc
  | Repeat { size; elem; seq } ->
      let expected = Eval.expr Eval.empty size in
      Types.exact_repeat_elements seq ~expected
        ~size_of:(Types.size_of_typ_value elem)
        v
      |> List.iter (fun (elem_v, _) -> encode_into elem elem_v enc)
  | Casetype { tag; cases; _ } -> encode_casetype tag cases v enc
  | Struct _ -> invalid_arg "Wire.struct_: encoding a struct goes through Codec"
  | Type_ref _ -> invalid_arg "Wire.type_ref: encoding needs a type registry"
  | Qualified_ref _ ->
      invalid_arg "Wire.qualified_ref: encoding needs a type registry"
  | Apply _ -> invalid_arg "Wire.apply: encoding needs a type registry"

and encode_casetype : type a k.
    k typ -> (a, k) case_branch list -> a -> encoder -> unit =
 fun tag cases v enc ->
  let rec find_case = function
    | [] -> Types.raise_no_matching_case ()
    | Case_branch { cb_inner; cb_project; _ } :: rest -> (
        match cb_project v with
        | Some (t, body) ->
            encode_into tag t enc;
            encode_into cb_inner body enc
        | None -> find_case rest)
  in
  find_case cases

let to_writer typ v writer =
  let enc = encoder writer in
  encode_into typ v enc;
  flush enc

(* Direct-to-bytes encode: no Writer, no Buffer, no encoder.
   For fixed-size types, allocates only the output bytes. *)
(* Helpers extracted from [encode_direct] to keep the dispatch readable. *)

let encode_bits buf off v width base bit_order =
  Types.check_unsigned_encode ~bits:width v;
  let mask = (1 lsl width) - 1 in
  let total = Bitfield.total_bits base in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
  let masked = (v land mask) lsl shift in
  match base with
  | U8 ->
      Bytes.set_uint8 buf off masked;
      off + 1
  | U16 Little ->
      Bytes.set_uint16_le buf off masked;
      off + 2
  | U16 Big ->
      Bytes.set_uint16_be buf off masked;
      off + 2
  (* Shift in [Int32]: a native-int shift would drop bit 31 of the word on a
     narrow-int platform. *)
  | U32 Little ->
      Bytes.set_int32_le buf off
        (Int32.shift_left (Int32.of_int (v land mask)) shift);
      off + 4
  | U32 Big ->
      Bytes.set_int32_be buf off
        (Int32.shift_left (Int32.of_int (v land mask)) shift);
      off + 4

(* Variable-size fallback: encode via the writer kernel into a Buffer,
   then blit. Used by [encode_direct]'s catch-all. *)
let encode_via_writer typ buf off v =
  let tmp = Buffer.create 64 in
  let writer = Writer.of_buffer tmp in
  let enc = encoder writer in
  encode_into typ v enc;
  flush enc;
  let s = Buffer.contents tmp in
  let n = String.length s in
  Bytes.blit_string s 0 buf off n;
  off + n

let rec encode_direct : type a. a typ -> bytes -> int -> a -> int =
 fun typ buf off v ->
  match typ with
  | Uint8 ->
      Types.set_uint8 buf off v;
      off + 1
  | Uint16 Little ->
      Types.set_uint16_le buf off v;
      off + 2
  | Uint16 Big ->
      Types.set_uint16_be buf off v;
      off + 2
  | Uint32 Little ->
      UInt32.set_le buf off v;
      off + 4
  | Uint32 Big ->
      UInt32.set_be buf off v;
      off + 4
  | Uint64 Little ->
      Bytes.set_int64_le buf off v;
      off + 8
  | Uint64 Big ->
      Bytes.set_int64_be buf off v;
      off + 8
  | Uint_var { size = Int n; endian } ->
      Uint_var.write endian buf off n v;
      off + n
  | Uint_var _ ->
      invalid_arg "Wire.uint: encoding a field-dependent size needs Codec"
  | Bits { width; base; bit_order } ->
      encode_bits buf off v width base bit_order
  | Unit -> off
  | All_bytes ->
      let n = String.length v in
      Bytes.blit_string v 0 buf off n;
      off + n
  | All_zeros ->
      Types.check_all_zeros_encode v;
      let n = String.length v in
      Bytes.blit_string v 0 buf off n;
      off + n
  | Byte_array { size = Int n } -> Codec.blit_string_exact n buf off v
  | Byte_slice { size = Int n } -> Codec.blit_slice_exact n buf off v
  | Single_elem { size = Int n; elem; at_most } ->
      let inner_sz = Types.size_of_typ_value elem v in
      Types.check_nested_size ~at_most ~expected:n ~actual:inner_sz;
      let off' = encode_direct elem buf off v in
      if off' < off + n then Bytes.fill buf off' (off + n - off') '\x00';
      off + n
  | Map { inner; encode; _ } -> encode_direct inner buf off (encode v)
  | Where { cond; inner } ->
      Types.check_where_encode cond (Eval.expr Eval.empty cond);
      encode_direct inner buf off v
  | Enum { name; base; cases; closed } ->
      if closed then
        Types.check_enum_encode ~name ~valid:(Types.enum_values cases) v;
      encode_direct base buf off v
  | Codec { codec_encode; _ } -> codec_encode v Types.unbound_eval_ctx buf off
  | _ -> encode_via_writer typ buf off v

let to_bytes typ v =
  match field_wire_size typ with
  | Some n ->
      let buf = Bytes.create n in
      ignore (encode_direct typ buf 0 v);
      buf
  | None ->
      let buf = Buffer.create 64 in
      let writer = Writer.of_buffer buf in
      to_writer typ v writer;
      Buffer.to_bytes buf

let to_string typ v = Bytes.unsafe_to_string (to_bytes typ v)

type 'r codec = 'r Codec.t

let pp_value (type r) (c : r Codec.t) ppf (v : r) =
  let buf = to_bytes (codec c) v in
  let readers = Codec.field_readers c in
  Fmt.pf ppf "@[<hv 2>%s {" (Codec.name c);
  List.iter
    (fun (name, reader) -> Fmt.pf ppf "@ %s = %d;" name (reader buf 0))
    readers;
  Fmt.pf ppf "@ }@]"

module SInt32 = SInt32
module Ascii = Ascii

module Private = struct
  module UInt32 = UInt32
  module UInt63 = UInt63
  module Types = Types
  module Eval = Eval
  module Bitfield = Bitfield
  module Uint_var = Uint_var

  let param_name = param_name
  let param_is_mutable = param_is_mutable
  let param_c_type = param_c_type
  let ml_type_of = Types.ml_type_of
  let c_type_of = Types.c_type_of
end
