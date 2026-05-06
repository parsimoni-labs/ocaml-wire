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

let bool (b : Stdlib.Bool.t) : _ Types.expr =
  if b then Types.true_ else Types.false_

let bit = Types.bool
let empty = Types.unit
let size = Types.field_wire_size
let lookup = Types.cases

let codec (c : 'r Codec_backend.t) : 'r typ =
  let codec_decode = Codec_backend.raw_decode c in
  let codec_encode = Codec_backend.raw_encode c in
  let codec_field_readers = Codec_backend.field_readers c in
  match Codec_backend.wire_size_info c with
  | `Fixed n ->
      Codec
        {
          codec_name = Codec_backend.name c;
          codec_decode;
          codec_encode;
          codec_fixed_size = Some n;
          codec_size_of = (fun _buf _off -> n);
          codec_field_readers;
        }
  | `Variable size_of ->
      Codec
        {
          codec_name = Codec_backend.name c;
          codec_decode;
          codec_encode;
          codec_fixed_size = None;
          codec_size_of = size_of;
          codec_field_readers;
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
let optional = Types.optional
let optional_or = Types.optional_or
let repeat = Types.repeat
let repeat_seq = Types.repeat_seq

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
  include Expr

  let true_ = Types.true_
  let false_ = Types.false_
  let bool b = if b then Types.true_ else Types.false_
end

module Reader = Bytesrw.Bytes.Reader
module Slice = Bytesrw.Bytes.Slice

exception Parse_exn of parse_error
exception Validation_error = Parse_error

let[@inline] check_eof len need =
  if need > len then
    raise (Parse_exn (Unexpected_eof { expected = need; got = len }))

(* The single decoder kernel. Bytes-based, returns [(value, end_off)].
   All types handled here -- no fallback. Expressions are evaluated in
   [Eval.empty] (no field bindings); types using [Ref]/[Sizeof_this]/
   [Field_pos] only make sense inside a [Struct], which goes through
   [Codec.validator_of_struct] where the int-array context is wired up. *)
let rec parse_direct : type a. a typ -> bytes -> int -> int -> a * int =
 fun typ buf off len ->
  match typ with
  | Uint8 ->
      check_eof len (off + 1);
      (Bytes.get_uint8 buf off, off + 1)
  | Uint16 Little ->
      check_eof len (off + 2);
      (Bytes.get_uint16_le buf off, off + 2)
  | Uint16 Big ->
      check_eof len (off + 2);
      (Bytes.get_uint16_be buf off, off + 2)
  | Uint32 Little ->
      check_eof len (off + 4);
      (UInt32.le buf off, off + 4)
  | Uint32 Big ->
      check_eof len (off + 4);
      (UInt32.be buf off, off + 4)
  | Uint63 Little ->
      check_eof len (off + 8);
      (UInt63.le buf off, off + 8)
  | Uint63 Big ->
      check_eof len (off + 8);
      (UInt63.be buf off, off + 8)
  | Uint64 Little ->
      check_eof len (off + 8);
      (Bytes.get_int64_le buf off, off + 8)
  | Uint64 Big ->
      check_eof len (off + 8);
      (Bytes.get_int64_be buf off, off + 8)
  | Uint_var { size; endian } ->
      let n = Eval.expr Eval.empty size in
      check_eof len (off + n);
      (Uint_var.read endian buf off n, off + n)
  | Bits { width; base; bit_order } ->
      let sz = Bitfield.byte_size base in
      check_eof len (off + sz);
      let total = Bitfield.total_bits base in
      let word = Bitfield.read_word base buf off in
      (Bitfield.extract ~bit_order ~total ~bits_used:0 ~width word, off + sz)
  | Unit -> ((), off)
  | All_bytes ->
      let n = len - off in
      (Bytes.sub_string buf off n, len)
  | All_zeros ->
      let n = len - off in
      let s = Bytes.sub_string buf off n in
      let rec check i =
        if i >= n then s
        else if s.[i] <> '\000' then
          raise (Parse_exn (All_zeros_failed { offset = off + i }))
        else check (i + 1)
      in
      (check 0, len)
  | Byte_array { size } ->
      let n = Eval.expr Eval.empty size in
      check_eof len (off + n);
      (Bytes.sub_string buf off n, off + n)
  | Byte_slice { size } ->
      let n = Eval.expr Eval.empty size in
      check_eof len (off + n);
      (Slice.make buf ~first:off ~length:n, off + n)
  | Single_elem { size; elem; at_most = _ } ->
      let n = Eval.expr Eval.empty size in
      check_eof len (off + n);
      let v, _ = parse_direct elem buf off (off + n) in
      (v, off + n)
  | Map { inner; decode; _ } -> (
      let v, off' = parse_direct inner buf off len in
      match decode v with
      | r -> (r, off')
      | exception Parse_error e -> raise (Parse_exn e))
  | Where { cond; inner } ->
      let v, off' = parse_direct inner buf off len in
      if Eval.expr Eval.empty cond then (v, off')
      else raise (Parse_exn (Constraint_failed "where clause"))
  | Enum { base; cases; _ } ->
      let v, off' = parse_direct base buf off len in
      let valid = List.map snd cases in
      if List.mem v valid then (v, off')
      else raise (Parse_exn (Invalid_enum { value = v; valid }))
  | Codec { codec_decode; codec_fixed_size; codec_size_of; _ } ->
      let sz =
        match codec_fixed_size with
        | Some n -> n
        | None -> codec_size_of buf off
      in
      check_eof len (off + sz);
      (codec_decode buf off, off + sz)
  | Struct s -> (
      (* Struct validation goes through the same int-array kernel as
         [Codec.decode]. Returns [unit] -- the [Struct] result type.
         [validate_struct] can raise [Parse_error] from constraint or
         action failures; translate to [Parse_exn] for the outer
         result-returning wrappers. *)
      let v = Codec_backend.validator_of_struct s in
      let sz = Codec_backend.struct_size_of v buf off in
      check_eof len (off + sz);
      try
        Codec_backend.validate_struct v buf off;
        ((), off + sz)
      with Parse_error e -> raise (Parse_exn e))
  | Casetype { cases; tag; _ } ->
      let tag_val, off' = parse_direct tag buf off len in
      let rec find_case = function
        | [] -> raise (Parse_exn (Invalid_tag tag_val))
        | Case_branch { cb_tag = Some expected; cb_inner; cb_inject; _ } :: rest
          ->
            if expected = tag_val then
              let body, off'' = parse_direct cb_inner buf off' len in
              (cb_inject body, off'')
            else find_case rest
        | Case_branch { cb_tag = None; cb_inner; cb_inject; _ } :: _ ->
            let body, off'' = parse_direct cb_inner buf off' len in
            (cb_inject body, off'')
      in
      find_case cases
  | Optional { present; inner } ->
      if Eval.expr Eval.empty present then
        let v, off' = parse_direct inner buf off len in
        (Some v, off')
      else (None, off)
  | Optional_or { present; inner; default } ->
      if Eval.expr Eval.empty present then parse_direct inner buf off len
      else (default, off)
  | Array { len = len_expr; elem; seq = Seq_map seq } ->
      let n = Eval.expr Eval.empty len_expr in
      let rec loop acc off' i =
        if i >= n then (seq.finish acc, off')
        else
          let v, off'' = parse_direct elem buf off' len in
          loop (seq.add acc v) off'' (i + 1)
      in
      loop seq.empty off 0
  | Repeat { size; elem; seq = Seq_map seq } ->
      let budget = Eval.expr Eval.empty size in
      let start = off in
      let rec loop acc off' =
        if off' - start >= budget then (seq.finish acc, off')
        else
          let v, off'' = parse_direct elem buf off' len in
          loop (seq.add acc v) off''
      in
      loop seq.empty off
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

let decode_string typ s =
  let buf = Bytes.unsafe_of_string s in
  let len = Bytes.length buf in
  match parse_direct typ buf 0 len with
  | v, _ -> Ok v
  | exception Parse_exn e -> Error e

let decode_bytes typ b =
  match parse_direct typ b 0 (Bytes.length b) with
  | v, _ -> Ok v
  | exception Parse_exn e -> Error e

let decode typ reader =
  (* Streaming becomes a thin buffering layer: drain the reader, then run
     [parse_direct]. *)
  let buf = Buffer.create 256 in
  let rec drain () =
    let slice = Reader.read reader in
    if Slice.is_eod slice then Buffer.to_bytes buf
    else begin
      Buffer.add_subbytes buf (Slice.bytes slice) (Slice.first slice)
        (Slice.length slice);
      drain ()
    end
  in
  let bytes = drain () in
  let len = Bytes.length bytes in
  match parse_direct typ bytes 0 len with
  | v, _ -> Ok v
  | exception Parse_exn e -> Error e
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

let encode_codec ~encode ~fixed_size ~size_of v enc =
  let sz =
    match fixed_size with
    | Some n -> n
    | None ->
        let tmp = Bytes.create 4096 in
        encode v tmp 0;
        size_of tmp 0
  in
  let tmp = Bytes.create sz in
  encode v tmp 0;
  write_string enc (Bytes.unsafe_to_string tmp)

(* The single encoder kernel. Writes [v] to [enc]. Top-level expressions
   are evaluated in [Eval.empty]; [Struct] is rejected (encode goes
   through [Codec.encode] for records). *)
let rec encode_to_writer : type a. a typ -> a -> encoder -> unit =
 fun typ v enc ->
  match typ with
  | Uint8 -> write_byte enc v
  | Uint16 Little -> write_uint16_le enc v
  | Uint16 Big -> write_uint16_be enc v
  | Uint32 Little -> write_uint32_le enc v
  | Uint32 Big -> write_uint32_be enc v
  | Uint63 Little -> write_uint63_le enc v
  | Uint63 Big -> write_uint63_be enc v
  | Uint64 Little -> write_int64_le enc v
  | Uint64 Big -> write_int64_be enc v
  | Uint_var { size; endian } ->
      let n = Eval.expr Eval.empty size in
      ensure enc n;
      Uint_var.write endian enc.o enc.o_next n v;
      enc.o_next <- enc.o_next + n
  | Bits { width; base; bit_order } -> (
      let mask = (1 lsl width) - 1 in
      let total = Bitfield.total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      let masked = (v land mask) lsl shift in
      match base with
      | BF_U8 -> write_byte enc masked
      | BF_U16 Little -> write_uint16_le enc masked
      | BF_U16 Big -> write_uint16_be enc masked
      | BF_U32 Little -> write_int32_le enc (Int32.of_int masked)
      | BF_U32 Big -> write_int32_be enc (Int32.of_int masked))
  | Unit -> ()
  | All_bytes -> write_string enc v
  | All_zeros -> write_string enc v
  | Where { inner; _ } -> encode_to_writer inner v enc
  | Array { elem; seq = Seq_map seq; _ } ->
      seq.iter (fun elem_v -> encode_to_writer elem elem_v enc) v
  | Byte_array _ -> write_string enc v
  | Byte_slice _ ->
      let src = Slice.bytes v in
      let off = Slice.first v in
      let len = Slice.length v in
      write_string enc (Bytes.sub_string src off len)
  | Single_elem { elem; _ } -> encode_to_writer elem v enc
  | Enum { base; _ } -> encode_to_writer base v enc
  | Map { inner; encode; _ } -> encode_to_writer inner (encode v) enc
  | Codec { codec_encode; codec_fixed_size; codec_size_of; _ } ->
      encode_codec ~encode:codec_encode ~fixed_size:codec_fixed_size
        ~size_of:codec_size_of v enc
  | Optional { present; inner } ->
      if Eval.expr Eval.empty present then
        encode_to_writer inner (Option.get v) enc
  | Optional_or { present; inner; _ } ->
      if Eval.expr Eval.empty present then encode_to_writer inner v enc
  | Repeat { elem; seq = Seq_map seq; _ } ->
      seq.iter (fun elem_v -> encode_to_writer elem elem_v enc) v
  | Casetype { tag; cases; _ } -> encode_casetype tag cases v enc
  | Struct _ -> failwith "struct encoding: use Codec.encode"
  | Type_ref _ -> failwith "type_ref requires a type registry"
  | Qualified_ref _ -> failwith "qualified_ref requires a type registry"
  | Apply _ -> failwith "apply requires a type registry"

and encode_casetype : type a.
    int typ -> a case_branch list -> a -> encoder -> unit =
 fun tag cases v enc ->
  let rec find_case = function
    | [] -> failwith "casetype encoding: no matching case"
    | Case_branch { cb_tag; cb_inner; cb_project; _ } :: rest -> (
        match cb_project v with
        | Some body ->
            (match cb_tag with
            | Some t -> encode_to_writer tag t enc
            | None -> failwith "casetype encoding: cannot encode default case");
            encode_to_writer cb_inner body enc
        | None -> find_case rest)
  in
  find_case cases

let encode typ v writer =
  let enc = encoder writer in
  encode_to_writer typ v enc;
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
  | Uint_var { size = Int n; endian } ->
      Uint_var.write endian buf off n v;
      off + n
  | Uint_var _ -> failwith "encode_direct: Uint_var with dynamic size"
  | Bits { width; base; bit_order } -> (
      let mask = (1 lsl width) - 1 in
      let total = Bitfield.total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      let masked = (v land mask) lsl shift in
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
  | All_bytes ->
      let s = (v : string) in
      let n = String.length s in
      Bytes.blit_string s 0 buf off n;
      off + n
  | All_zeros ->
      let s = (v : string) in
      let n = String.length s in
      Bytes.blit_string s 0 buf off n;
      off + n
  | Byte_array { size = Int n } ->
      let s = (v : string) in
      let len = min n (String.length s) in
      Bytes.blit_string s 0 buf off len;
      if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
      off + n
  | Byte_slice { size = Int n } ->
      let src = (v : Slice.t) in
      let len = min n (Slice.length src) in
      Bytes.blit (Slice.bytes src) (Slice.first src) buf off len;
      if len < n then Bytes.fill buf (off + len) (n - len) '\x00';
      off + n
  | Single_elem { size = Int n; elem; at_most = _ } ->
      let off' = encode_direct elem buf off v in
      (* Pad up to [n] if the inner write was shorter. *)
      if off' < off + n then Bytes.fill buf off' (off + n - off') '\x00';
      off + n
  | Map { inner; encode; _ } -> encode_direct inner buf off (encode v)
  | Where { inner; _ } -> encode_direct inner buf off v
  | Enum { base; _ } -> encode_direct base buf off v
  | Codec { codec_encode; _ } ->
      codec_encode v buf off;
      (* Codecs are responsible for advancing through the full struct.
         The wire size determines how many bytes they wrote. *)
      let sz =
        match field_wire_size typ with
        | Some n -> n
        | None -> failwith "encode_direct: Codec without static wire size"
      in
      off + sz
  | _ ->
      (* Variable-size: encode through the unified writer kernel into a
         Buffer, then blit. *)
      let tmp = Buffer.create 64 in
      let writer = Writer.of_buffer tmp in
      let enc = encoder writer in
      encode_to_writer typ v enc;
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

  let decode_with t env buf off =
    try Ok (Codec_backend.decode_with t env buf off)
    with Parse_error e -> Error e

  let validate = Codec_backend.validate
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
