open Types
module Slice = Bytesrw.Bytes.Slice
open Codec_leaf
open Codec_repr

(* Bitfield helpers -- shared module for base operations, specialized closures
   for performance-critical read/write dispatched at codec construction time. *)

let bf_base_byte_size = Bitfield.byte_size
let bf_base_total_bits = Bitfield.total_bits
let bf_base_equal = Bitfield.equal
let bf_write_base = Bitfield.write_word

let equal_bit_order (a : Types.bit_order) (b : Types.bit_order) =
  match (a, b) with
  | Msb_first, Msb_first | Lsb_first, Lsb_first -> true
  | Msb_first, Lsb_first | Lsb_first, Msb_first -> false

(* Build-time dispatch: pattern match on base happens once at codec
   construction, not on every read/write call. The word reads are unchecked
   past the [U8]/[U16] primitives' own bounds check, so the caller has to know
   the word is in the buffer: the decoders have checked their frame's extent,
   and the field accessors go through {!build_bf_accessor_reader}. *)
let build_bf_reader base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | U8 ->
      fun buf off -> (Bytes.get_uint8 buf (off + byte_off) lsr shift) land mask
  | U16 Little ->
      fun buf off ->
        (Bytes.get_uint16_le buf (off + byte_off) lsr shift) land mask
  | U16 Big ->
      fun buf off ->
        (Bytes.get_uint16_be buf (off + byte_off) lsr shift) land mask
  | U32 Little when not Bitfield.int_holds_u32 ->
      fun buf off -> Bitfield.u32_field_le buf (off + byte_off) shift mask
  | U32 Big when not Bitfield.int_holds_u32 ->
      fun buf off -> Bitfield.u32_field_be buf (off + byte_off) shift mask
  | U32 Little ->
      fun buf off -> (Bitfield.u32_le buf (off + byte_off) lsr shift) land mask
  | U32 Big ->
      fun buf off -> (Bitfield.u32_be buf (off + byte_off) lsr shift) land mask

(* A field accessor's offset is the application's, so its base word has still
   to be shown to be in the buffer. A u32 word is assembled from unchecked byte
   reads, so [Codec.get] on a frame shorter than the word used to answer with
   whatever followed it: the same bytes read one value where the frame lay and
   another once it moved along the buffer, and neither was in the frame. *)
let build_bf_accessor_reader base byte_off shift width =
  let read = build_bf_reader base byte_off shift width in
  let word = bf_base_byte_size base in
  fun buf off ->
    Bitfield.check_word buf (off + byte_off) word;
    read buf off

let build_bf_writer base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  match base with
  | U8 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U16 Little ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U16 Big ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U32 Little when not Bitfield.int_holds_u32 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        Bitfield.u32_field_or_le buf (off + byte_off) shift (value land mask)
  | U32 Big when not Bitfield.int_holds_u32 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        Bitfield.u32_field_or_be buf (off + byte_off) shift (value land mask)
  | U32 Little ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bitfield.u32_le buf (off + byte_off) in
        Bitfield.set_u32_le buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))
  | U32 Big ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bitfield.u32_be buf (off + byte_off) in
        Bitfield.set_u32_be buf (off + byte_off)
          (cur lor ((value land mask) lsl shift))

(* The [Codec.set] writer. It range-checks like {!build_bf_writer}: masking here
   is the one silent truncation no later check can catch, because the masked
   result is a legal field value that [Codec.get] and [validate] both accept.
   [set] promises the buffer is untouched when it raises, so the word's span is
   checked before any of it is written: a u32 goes down as two halves, and a
   short buffer used to take the first before the second raised. *)
let build_bf_accessor_writer_raw base byte_off shift width =
  let mask = (1 lsl width) - 1 in
  let clear_mask = lnot (mask lsl shift) in
  match base with
  | U8 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint8 buf (off + byte_off) in
        Bytes.set_uint8 buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U16 Little ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint16_le buf (off + byte_off) in
        Bytes.set_uint16_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U16 Big ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bytes.get_uint16_be buf (off + byte_off) in
        Bytes.set_uint16_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U32 Little when not Bitfield.int_holds_u32 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        Bitfield.u32_field_set_le buf (off + byte_off) shift mask
          (value land mask)
  | U32 Big when not Bitfield.int_holds_u32 ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        Bitfield.u32_field_set_be buf (off + byte_off) shift mask
          (value land mask)
  | U32 Little ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bitfield.u32_le buf (off + byte_off) in
        Bitfield.set_u32_le buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))
  | U32 Big ->
      fun buf off value ->
        Types.check_unsigned_encode ~bits:width value;
        let cur = Bitfield.u32_be buf (off + byte_off) in
        Bitfield.set_u32_be buf (off + byte_off)
          (cur land clear_mask lor ((value land mask) lsl shift))

let build_bf_accessor_writer base byte_off shift width =
  let write = build_bf_accessor_writer_raw base byte_off shift width in
  let word = bf_base_byte_size base in
  fun buf off value ->
    Bitfield.check_word buf (off + byte_off) word;
    write buf off value

let build_bf_clear base byte_off =
 fun buf off -> bf_write_base base buf (off + byte_off) 0

let build_idx readers =
  let rev_readers = List.rev readers in
  fun name ->
    let rec find i = function
      | [] ->
          Fmt.invalid_arg
            "Codec: unbound field ref %S in a field constraint or action" name
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
  | Lsr (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Land64 (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Lsr64 (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Lt (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Le (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
  | Gt (a, b) ->
      iter_param_refs f a;
      iter_param_refs f b
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
  | Types.Success stmts | Types.Act stmts ->
      List.iter (iter_param_refs_stmt f) stmts

let rec iter_param_refs_typ : type a. (Param.packed -> unit) -> a typ -> unit =
 fun f typ ->
  match typ with
  | Byte_array { size } | Byte_slice { size } -> iter_param_refs f size
  | Byte_array_where { size; cond; _ } ->
      iter_param_refs f size;
      iter_param_refs f cond
  | Uint_var { size; _ } -> iter_param_refs f size
  | Zeroterm_at_most { size } -> iter_param_refs f size
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
  (* An embedded sub-codec carries its own params (in field sizes / where) that
     the outer codec must surface so its env can bind them. Walk the sub-codec's
     structural form. *)
  | Codec { codec_struct; _ } ->
      iter_param_refs_fields f codec_struct.fields codec_struct.where
  | Uint8 | Uint16 _ | Uint32 _ | Uint64 _ | Int8 | Int16 _ | Int32 _ | Int64 _
  | Float32 _ | Float64 _ | Bits _ | Unit | All_bytes | All_zeros | Zeroterm
  | Struct _ | Type_ref _ | Qualified_ref _ ->
      ()

and iter_param_refs_fields f fields where =
  List.iter
    (fun (Types.Field fld) ->
      iter_param_refs_typ f fld.field_typ;
      Option.iter (iter_param_refs f) fld.constraint_;
      Option.iter (iter_param_refs_action f) fld.action)
    fields;
  Option.iter (iter_param_refs f) where

(* Kept at top level: as a local closure the scan would be heap-allocated on
   every terminator search, and a repeat of NUL-terminated elements searches
   once per element on a count the sender picks through the byte budget. *)

(* Byte size of a casetype tag. The tag is always a fixed-size scalar int, so
   the common widths answer directly: [field_wire_size] would box a [Some n]
   per call, and a casetype element reads its tag size on every iteration. *)
let tag_byte_size : type a. a typ -> int =
 fun typ ->
  match typ with
  | Uint8 | Int8 -> 1
  | Uint16 _ | Int16 _ -> 2
  | Uint32 _ | Int32 _ -> 4
  | Uint64 _ | Int64 _ -> 8
  | _ -> ( match field_wire_size typ with Some n -> n | None -> assert false)

(* Read one element of a typ at a given buffer position. Used by Repeat. *)
let rec read_elem : type a. a typ -> runtime -> Input_end.t -> bytes -> int -> a
    =
 fun typ runtime input_end buf off ->
  match typ with
  | Uint8 -> UInt8.get buf off
  | Uint16 Little -> UInt16.le buf off
  | Uint16 Big -> UInt16.be buf off
  | Uint32 Little -> UInt32.le buf off
  | Uint32 Big -> UInt32.be buf off
  | Uint64 Little -> UInt64.le buf off
  | Uint64 Big -> UInt64.be buf off
  | Int8 -> SInt8.get buf off
  | Int16 Little -> SInt16.le buf off
  | Int16 Big -> SInt16.be buf off
  | Int32 Little -> SInt32.le buf off
  | Int32 Big -> SInt32.be buf off
  | Int64 Little -> Bytes.get_int64_le buf off
  | Int64 Big -> Bytes.get_int64_be buf off
  | Float32 Little -> Int32.float_of_bits (Bytes.get_int32_le buf off)
  | Float32 Big -> Int32.float_of_bits (Bytes.get_int32_be buf off)
  | Float64 Little -> Int64.float_of_bits (Bytes.get_int64_le buf off)
  | Float64 Big -> Int64.float_of_bits (Bytes.get_int64_be buf off)
  | Uint_var { size = Int n; endian } -> Uint_var.read endian buf off n
  | Codec { codec_decode; _ } -> codec_decode runtime input_end buf off
  | Map { inner; decode; index_bound; _ } ->
      Types.map_decode ~index_bound decode ~at:off
        (read_elem inner runtime input_end buf off)
  | Where { inner; _ } -> read_elem inner runtime input_end buf off
  | Enum { base; cases; closed; _ } ->
      (* Applied whole rather than through [enum_check]: an element's check runs
         per element, and staging one here would allocate the closure per
         element too (the integer view included), on a count the sender picks
         through the byte budget. *)
      let v = read_elem base runtime input_end buf off in
      if closed then
        (* A base whose values all fit the native int cannot raise on the way to
           its integer view, so it skips the relocating handler entirely. *)
        if Types.int_view_is_total base then
          Types.check_enum_decode ~at:off ~cases (Types.int_of_exn base v)
        else
          Types.relocate_at ~at:off (fun () ->
              Types.check_enum_decode ~at:off ~cases (Types.int_of_exn base v));
      v
  | Casetype { tag; cases; _ } ->
      let tag_val = read_elem tag runtime input_end buf off in
      read_case_body ~at:off ~tag cases tag_val runtime input_end buf
        (off + tag_byte_size tag)
  | Unit -> ()
  (* NUL-terminated string element: the bytes up to (not including) the NUL. *)
  | Zeroterm ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(Bytes.length buf) in
      Bytes.sub_string buf off (nul - off)
  (* Same, bounded to a fixed [n]-byte region (the element spans the whole
     region; the value is the bytes up to the NUL within it). *)
  | Zeroterm_at_most { size = Int n } ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(off + n) in
      Bytes.sub_string buf off (nul - off)
  (* Fixed-size byte spans as repeat / array elements: a list of n-byte
     chunks. The size is the element's own constant width. *)
  | Byte_array { size = Int n } -> Bytes.sub_string buf off n
  | Byte_array_where { size = Int n; elt_var; cond } ->
      refined_span_reader ~elt_var ~cond buf ~first:off ~len:n
  | Byte_slice { size = Int n } -> Slice.make_or_eod buf ~first:off ~length:n
  (* A lone bitfield occupies its base word; extract the value at its bit
     offset. *)
  | Bits { width; base; bit_order } ->
      let total = bf_base_total_bits base in
      let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
      build_bf_reader base 0 shift width buf off
  (* A fixed-count array inside a [nested] region (or a casetype case body):
     decode each element at its stride. *)
  | Array { len = Int n; elem; seq = Seq_map s } -> (
      match field_wire_size elem with
      | None ->
          Fmt.invalid_arg "Wire.array: element %s has no stride to read at"
            (field_shape elem)
      | Some esz ->
          let acc = Stdlib.ref s.empty in
          for i = 0 to n - 1 do
            acc :=
              s.add !acc
                (read_elem elem runtime input_end buf (off + (i * esz)))
          done;
          s.finish !acc)
  (* A nested region as an element: decode its inner at the region start; the
     enclosing region size (padding) is accounted for by the caller. *)
  | Single_elem { elem; _ } -> read_elem elem runtime input_end buf off
  | _ ->
      Fmt.invalid_arg "Wire.repeat: no element reader for %s" (field_shape typ)

(* Dispatch a casetype's matched case body. A top-level recursion rather than a
   [let rec find] inside [read_elem]: the inner form would allocate a fresh
   closure on every casetype element decoded. *)
and read_case_body : type a k.
    at:int ->
    tag:k typ ->
    (a, k) case_branch list ->
    k ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    a =
 fun ~at ~tag cases tag_val runtime input_end buf body_off ->
  match cases with
  | [] ->
      (* No branch matched the discriminant. [tag] is integer-typed in the
         common case (a version/type field); a non-integer (string) tag has no
         [int] form and reports index 0. *)
      raise_invalid_tag ~at (Option.value ~default:0 (Eval.int_of tag tag_val))
  | Case_branch { cb_tag = Some t; cb_inner; cb_inject; _ } :: _
    when t = tag_val ->
      cb_inject tag_val (read_elem cb_inner runtime input_end buf body_off)
  | Case_branch { cb_tag = None; cb_inner; cb_inject; _ } :: _ ->
      cb_inject tag_val (read_elem cb_inner runtime input_end buf body_off)
  | _ :: rest ->
      read_case_body ~at ~tag rest tag_val runtime input_end buf body_off

(* Write one element of a typ at a given buffer position. Used by Repeat. *)
let rec write_elem : type a. a typ -> runtime -> bytes -> int -> a -> unit =
 fun typ runtime buf off v ->
  match typ with
  | Uint8 -> UInt8.set buf off v
  | Uint16 Little -> UInt16.set_le buf off v
  | Uint16 Big -> UInt16.set_be buf off v
  | Uint32 Little -> UInt32.set_le buf off v
  | Uint32 Big -> UInt32.set_be buf off v
  | Uint64 Little -> UInt64.set_le buf off v
  | Uint64 Big -> UInt64.set_be buf off v
  | Int8 -> SInt8.set buf off v
  | Int16 Little -> SInt16.set_le buf off v
  | Int16 Big -> SInt16.set_be buf off v
  | Int32 Little -> Types.set_int32_le buf off v
  | Int32 Big -> Types.set_int32_be buf off v
  | Int64 Little -> Bytes.set_int64_le buf off v
  | Int64 Big -> Bytes.set_int64_be buf off v
  | Float32 Little -> Bytes.set_int32_le buf off (Int32.bits_of_float v)
  | Float32 Big -> Bytes.set_int32_be buf off (Int32.bits_of_float v)
  | Float64 Little -> Bytes.set_int64_le buf off (Int64.bits_of_float v)
  | Float64 Big -> Bytes.set_int64_be buf off (Int64.bits_of_float v)
  | Uint_var { size = Int n; endian } -> Uint_var.write endian buf off n v
  | Codec { codec_encode; _ } -> ignore (codec_encode v runtime buf off : int)
  | Map { inner; encode; _ } -> write_elem inner runtime buf off (encode v)
  | Where { inner; _ } -> write_elem inner runtime buf off v
  | Enum { base; _ } -> write_elem base runtime buf off v
  | Unit -> ()
  (* Casetype reuses the full field encoder (tag + dispatched body); the
     repeat loop advances by [elem_size_of], so the returned offset is
     discarded here. *)
  | Casetype _ ->
      (* Two steps, as in [Codec_compile]: one over-saturated call across an
         opaque module boundary allocates per extra argument. *)
      let encode = build_field_encoder_ctx typ in
      ignore (encode runtime buf off v : int)
  (* NUL-terminated string element: the bytes followed by a NUL terminator. *)
  | Zeroterm ->
      let n = String.length v in
      Bytes.blit_string v 0 buf off n;
      Bytes.set_uint8 buf (off + n) 0
  (* Fixed-size byte spans: the field encoder blits the constant width and
     returns the advanced offset, which the repeat loop recomputes. *)
  | Byte_array { size = Int _ } ->
      let encode = build_field_encoder typ in
      ignore (encode buf off v : int)
  | Byte_slice { size = Int _ } ->
      let encode = build_field_encoder typ in
      ignore (encode buf off v : int)
  | _ ->
      Fmt.invalid_arg "Wire.repeat: no element writer for %s" (field_shape typ)

(* Compute the wire size of one element at a buffer position. Used by Repeat
   for variable-size elements. *)
let rec elem_size_of : type a.
    a typ -> runtime -> Input_end.t -> bytes -> int -> int =
 fun typ runtime input_end buf off ->
  match typ with
  | Codec { codec_size_of; _ } -> codec_size_of runtime input_end buf off
  | Casetype { tag; cases; _ } ->
      let tag_size = tag_byte_size tag in
      (* The position comes from a preceding length field, so a truncated
         buffer can put the tag past the end. *)
      check_read_bounds input_end ~at:off ~width:tag_size;
      let tag_val = read_elem tag runtime input_end buf off in
      tag_size
      + size_case_body ~at:off ~tag cases tag_val runtime input_end buf
          (off + tag_size)
  | Map { inner; _ } -> elem_size_of inner runtime input_end buf off
  | Where { inner; _ } -> elem_size_of inner runtime input_end buf off
  (* Bytes up to the NUL plus the one-byte terminator. *)
  | Zeroterm ->
      let nul = zeroterm_nul_pos buf ~first:off ~limit:(Bytes.length buf) in
      nul - off + 1
  (* A bounded NUL-terminated string spans its whole fixed [n]-byte region. *)
  | Zeroterm_at_most { size = Int n } -> n
  (* A lone bitfield occupies its base word. *)
  | Bits { base; _ } -> bf_base_byte_size base
  (* A nested region spans its whole fixed size, regardless of the inner. *)
  | Single_elem { size = Int n; _ } -> n
  | _ -> (
      match field_wire_size typ with
      | Some n -> n
      | None ->
          Fmt.invalid_arg "Wire.repeat: element %s has no determinable size"
            (field_shape typ))

(* Wire size of a casetype's matched case body. Top-level for the same reason
   as [read_case_body]: a per-call [let rec find] would allocate a closure on
   every element. *)
and size_case_body : type a k.
    at:int ->
    tag:k typ ->
    (a, k) case_branch list ->
    k ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    int =
 fun ~at ~tag cases tag_val runtime input_end buf body_off ->
  match cases with
  | [] ->
      raise_invalid_tag ~at (Option.value ~default:0 (Eval.int_of tag tag_val))
  | Case_branch { cb_tag = Some t; cb_inner; _ } :: _ when t = tag_val ->
      elem_size_of cb_inner runtime input_end buf body_off
  | Case_branch { cb_tag = None; cb_inner; _ } :: _ ->
      elem_size_of cb_inner runtime input_end buf body_off
  | _ :: rest ->
      size_case_body ~at ~tag rest tag_val runtime input_end buf body_off
