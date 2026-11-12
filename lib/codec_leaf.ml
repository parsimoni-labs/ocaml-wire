open Types
module Slice = Bytesrw.Bytes.Slice

let blit_string_exact n buf off v =
  check_byte_field_size ~expected:n ~actual:(String.length v);
  Bytes.blit_string v 0 buf off n;
  off + n

let blit_slice_exact n buf off src =
  check_byte_field_size ~expected:n ~actual:(Slice.length src);
  Bytes.blit (Slice.bytes src) (Slice.first src) buf off n;
  off + n

(* Bytes a read starting at [off] can have, which is what
   {!Types.Unexpected_eof}'s [got] counts. A start before the input or past
   where it stops has none of them; the count is never negative. *)
let[@inline] bytes_from input_end off =
  let stop = Input_end.to_int input_end in
  if off < 0 || off > stop then 0 else stop - off

(* A read of [width] bytes at [at] that must stay inside the bytes the parse
   was handed. Reads whose position was itself computed from the buffer can
   land past the end on truncated input; reporting the span here keeps the
   failure a precise end-of-input instead of a raw out-of-bounds
   [Invalid_argument], which a caller cannot tell apart from a misuse or from a
   user [map] callback. *)
let[@inline] check_read_bounds input_end ~at ~width =
  if at < 0 || at + width > Input_end.to_int input_end then
    raise_eof ~at ~expected:width ~got:(bytes_from input_end at)

(* Reader for a byte span of constant width. A negative width comes from a size
   expression that underflows or names a negative literal, so it is data rather
   than a span the codec can read: report it at the field instead of crashing
   [Bytes.sub] with an [Invalid_argument] that escapes the decode result. The
   width is resolved once, while the reader is staged, so the read itself keeps
   no test. *)
let const_span_reader ~field_off n read =
  if n >= 0 then read
  else fun _runtime _input_end _buf base ->
    raise_out_of_range ~at:(base + field_off) (Int64.of_int n)

let rec nul_scan buf ~at ~limit i =
  if i >= limit then raise_missing_terminator ~at
  else if Bytes.get_uint8 buf i = 0 then i
  else nul_scan buf ~at ~limit (i + 1)

let zeroterm_nul_pos buf ~first ~limit = nul_scan buf ~at:first ~limit first

(* The refinement of a [byte_array_where] lives in its reader: every compiled
   path that yields the span runs it, so [Codec.decode] and [Codec.validate]
   reject the same bytes as [Wire.of_string] and as the EverParse validator
   built from the same schema. *)
let refined_span_check ~elt_var ~cond buf ~first ~len =
  let bad = Eval.bad_byte ~elt_var ~cond buf ~first ~len in
  if bad >= 0 then raise_constraint ~at:(first + bad) ~which:Per_byte ()

let refined_span_reader ~elt_var ~cond buf ~first ~len =
  refined_span_check ~elt_var ~cond buf ~first ~len;
  Bytes.sub_string buf first len

(* Index of the first byte of [buf.[first .. first + len - 1]] that is not NUL,
   or [-1] when the span is all zeros. Scans the buffer where it lies, so the
   check costs nothing beyond the walk it has to do anyway. *)
let all_zeros_bad_byte buf ~first ~len =
  let limit = first + len in
  let rec go i =
    if i >= limit then -1
    else if Bytes.get_uint8 buf i <> 0 then i
    else go (i + 1)
  in
  go first

let all_zeros_check buf ~first ~len =
  let bad = all_zeros_bad_byte buf ~first ~len in
  if bad >= 0 then raise_non_zero_padding ~at:bad

let all_zeros_reader buf ~first ~len =
  all_zeros_check buf ~first ~len;
  Bytes.sub_string buf first len

(* A refinement decode enforces has to gate encode too, or the encoder emits
   bytes its own decoder (and the EverParse validator built from the same
   schema) rejects. One helper per refined type, so the encoder dispatch below
   stays a flat table. *)
let refined_bytes_encoder n ~elt_var ~cond buf off v =
  Eval.check_byte_refinement ~elt_var ~cond v;
  blit_string_exact n buf off v

let where_field_encoder cond enc runtime buf off v =
  Types.check_where_encode cond (Eval.expr Eval.empty cond);
  enc runtime buf off v

let enum_field_encoder ~name ~cases ~closed ~to_int enc =
  if not closed then enc
  else
    let valid = Types.enum_values cases in
    fun runtime buf off v ->
      Types.check_enum_encode ~name ~valid (to_int v);
      enc runtime buf off v

(* Pack a fixed-width integer setter into a [bytes -> int -> int -> int]
   field encoder that returns the offset advance. Used by the scalar cases
   in [build_field_encoder] / [build_field_reader]. *)
let setter_off n set buf off v =
  set buf off v;
  off + n

(* Encode a value into a fixed [n]-byte region: write it with [enc], then
   zero-pad the remainder. Used for [nested] regions. *)
let single_elem_region ~at_most n typ enc buf off v =
  let actual = Types.size_of_typ_value Types.unbound_eval_ctx typ v in
  Types.check_nested_size ~at_most ~expected:n ~actual;
  let inner_end = enc buf off v in
  if inner_end < off + n then
    Bytes.fill buf inner_end (off + n - inner_end) '\x00';
  off + n

(* Encode a lone bitfield occupying its base word: range-check, place the value
   at its bit offset, advance by the base width. *)
let bits_field_encoder ~width ~base ~bit_order buf off v =
  let total = Bitfield.total_bits base in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used:0 ~width in
  let mask = (1 lsl width) - 1 in
  Types.check_unsigned_encode ~bits:width v;
  (match base with
  | U32 Little when not Bitfield.int_holds_u32 ->
      Bitfield.u32_field_word_le buf off shift v
  | U32 Big when not Bitfield.int_holds_u32 ->
      Bitfield.u32_field_word_be buf off shift v
  | _ -> Bitfield.write_word base buf off ((v land mask) lsl shift));
  off + Bitfield.byte_size base

let float32_field_encoder setter buf off v =
  setter buf off (Int32.bits_of_float v);
  off + 4

let float64_field_encoder setter buf off v =
  setter buf off (Int64.bits_of_float v);
  off + 8

let uint_var_field_encoder endian n buf off v =
  Uint_var.write endian buf off n v;
  off + n

(* Name the field type for an "unsupported shape" refusal. [Types.pp_typ]
   renders 3D syntax and asserts on a field decoration, which is the one shape
   that actually reaches those refusals, so say what it is in prose instead. *)
let field_shape : type a. a typ -> string = function
  | Optional _ -> "an optional field behind a runtime gate"
  | Optional_or _ -> "an optional_or field behind a runtime gate"
  | Repeat _ -> "a repeat field"
  | t -> Fmt.str "%a" pp_typ t

let rec build_casetype_encoder_ctx : type a k.
    k typ ->
    (a, k) case_branch list ->
    Types.eval_ctx ->
    bytes ->
    int ->
    a ->
    int =
 fun tag cases ->
  let tag_enc = build_field_encoder_ctx tag in
  fun runtime ->
    let rec find buf off v = function
      | [] -> Types.raise_no_matching_case ()
      | Case_branch { cb_inner; cb_project; _ } :: rest -> (
          (* [cb_project] yields the tag to write (its fixed index for an explicit
           case, or the value-carried tag for the default) and the body. *)
          match cb_project v with
          | Some (t, body) ->
              let off' = tag_enc runtime buf off t in
              build_field_encoder_ctx cb_inner runtime buf off' body
          | None -> find buf off v rest)
    in
    fun buf off v -> find buf off v cases

and build_field_encoder_ctx : type a.
    a typ -> Types.eval_ctx -> bytes -> int -> a -> int =
 fun typ ->
  match typ with
  | Uint8 ->
      fun _runtime buf off v ->
        UInt8.set buf off v;
        off + 1
  | Uint16 Little ->
      fun _runtime buf off v ->
        UInt16.set_le buf off v;
        off + 2
  | Uint16 Big ->
      fun _runtime buf off v ->
        UInt16.set_be buf off v;
        off + 2
  | Uint32 Little ->
      fun _runtime buf off v ->
        UInt32.set_le buf off v;
        off + 4
  | Uint32 Big ->
      fun _runtime buf off v ->
        UInt32.set_be buf off v;
        off + 4
  | Uint64 Little ->
      fun _runtime buf off v ->
        UInt64.set_le buf off v;
        off + 8
  | Uint64 Big ->
      fun _runtime buf off v ->
        UInt64.set_be buf off v;
        off + 8
  | Int8 -> fun _runtime -> setter_off 1 SInt8.set
  | Int16 Little -> fun _runtime -> setter_off 2 SInt16.set_le
  | Int16 Big -> fun _runtime -> setter_off 2 SInt16.set_be
  | Int32 Little -> fun _runtime -> setter_off 4 Types.set_int32_le
  | Int32 Big -> fun _runtime -> setter_off 4 Types.set_int32_be
  | Int64 Little -> fun _runtime -> setter_off 8 Bytes.set_int64_le
  | Int64 Big -> fun _runtime -> setter_off 8 Bytes.set_int64_be
  | Float32 Little -> fun _runtime -> float32_field_encoder Bytes.set_int32_le
  | Float32 Big -> fun _runtime -> float32_field_encoder Bytes.set_int32_be
  | Float64 Little -> fun _runtime -> float64_field_encoder Bytes.set_int64_le
  | Float64 Big -> fun _runtime -> float64_field_encoder Bytes.set_int64_be
  | Uint_var { size = Int n; endian } ->
      fun _runtime -> uint_var_field_encoder endian n
  | Byte_array { size = Int n } -> fun _runtime -> blit_string_exact n
  | Byte_array_where { size = Int n; elt_var; cond } ->
      fun _runtime -> refined_bytes_encoder n ~elt_var ~cond
  | Byte_slice { size = Int n } -> fun _runtime -> blit_slice_exact n
  | Where { cond; inner } ->
      where_field_encoder cond (build_field_encoder_ctx inner)
  | Enum { name; base; cases; closed } ->
      enum_field_encoder ~name ~cases ~closed ~to_int:(Types.int_of_exn base)
        (build_field_encoder_ctx base)
  | Map { inner; encode; _ } ->
      let enc = build_field_encoder_ctx inner in
      fun runtime buf off v -> enc runtime buf off (encode v)
  | Unit -> fun _runtime _buf off () -> off
  | Codec { codec_encode; _ } ->
      fun runtime buf off v -> codec_encode v runtime buf off
  | Casetype { tag; cases; _ } -> build_casetype_encoder_ctx tag cases
  | Array { len = Int expected; elem; seq } ->
      let enc_elem = build_field_encoder_ctx elem in
      fun runtime buf start_off vs ->
        let cur = Stdlib.ref start_off in
        Types.exact_array_elements seq ~expected vs
        |> List.iter (fun v -> cur := enc_elem runtime buf !cur v);
        !cur
  (* NUL-terminated string element / casetype case body: the bytes then a NUL.
     [zeroterm_at_most] pads the rest of its fixed [n]-byte region with zeros. *)
  | Zeroterm ->
      fun _runtime buf off v ->
        Types.check_zeroterm_encode v;
        let n = String.length v in
        Bytes.blit_string v 0 buf off n;
        Bytes.set_uint8 buf (off + n) 0;
        off + n + 1
  | Zeroterm_at_most { size = Int region } ->
      fun _runtime buf off v ->
        Types.check_zeroterm_encode v;
        let n = String.length v in
        Types.check_zeroterm_region ~region ~len:n;
        Bytes.blit_string v 0 buf off n;
        Bytes.fill buf (off + n) (region - n) '\x00';
        off + region
  (* A lone bitfield (e.g. a casetype case body) occupies its whole base word;
     write the value at its bit offset into an otherwise-zero word. *)
  | Bits { width; base; bit_order } ->
      fun _runtime -> bits_field_encoder ~width ~base ~bit_order
  (* A nested region (e.g. a casetype case body): encode the inner at the
     region start, then zero-pad the rest of the fixed [n]-byte region. *)
  | Single_elem { size = Int n; elem; at_most } ->
      let enc = build_field_encoder_ctx elem in
      fun runtime -> single_elem_region ~at_most n elem (enc runtime)
  (* Mirror of the reader's statically gated optionals, so [Codec.set] can
     write the field [Codec.get] can read. Presence is fixed at construction:
     present writes the inner in place, absent writes nothing. An [optional]
     handed [None] while its gate says present writes nothing either, which is
     what the record encoder does with the same value. *)
  | Optional { present = Bool true; inner } -> (
      let enc = build_field_encoder_ctx inner in
      let width = Option.value ~default:0 (field_wire_size inner) in
      fun runtime buf off v ->
        match v with Some iv -> enc runtime buf off iv | None -> off + width)
  | Optional { present = Bool false; _ } -> fun _runtime _buf off _v -> off
  | Optional_or { present = Bool true; inner; _ } ->
      build_field_encoder_ctx inner
  | Optional_or { present = Bool false; _ } -> fun _runtime _buf off _v -> off
  | _ -> Fmt.invalid_arg "Codec.encode: no writer for %s" (field_shape typ)

let build_field_encoder typ =
  let encode = build_field_encoder_ctx typ in
  fun buf off v -> encode Types.unbound_eval_ctx buf off v

(* An [enum] decodes through its base integer type; validate that the value is
   one of the named cases, raising [Invalid_enum] otherwise, so the decoder
   rejects exactly the inputs the EverParse-generated validator (and the
   [parse_direct] path) does, instead of accepting any base value. The scan
   reads the cases where they lie, so an accepted value costs nothing however
   many cases the enum names. *)
let enum_checker ~to_int cases ~at v =
  Types.check_enum_decode ~at ~cases (to_int v);
  v

(* [to_int] is handed the value alone, so a carrier wider than the native int
   reports its refusal at byte 0. Relocate it to the field, as the element
   reader does. Only a wide carrier can raise, so the narrow ones keep the bare
   checker and pay nothing for the handler. *)
let enum_checker_wide ~to_int cases ~at v =
  Types.relocate_at ~at (fun () ->
      Types.check_enum_decode ~at ~cases (to_int v));
  v

(* An open enum ([closed = false]) accepts any base value: the names only
   document known members, so decode does not reject the rest. *)
let enum_check ~base ~to_int cases closed =
  if not closed then fun ~at:_ v -> v
  else if Types.int_view_is_total base then enum_checker ~to_int cases
  else enum_checker_wide ~to_int cases

(* Encode-side twin of [enum_check], gated on [closed] by the same rule so the
   two halves admit exactly the same values. Encoding an unlisted value raises
   [Invalid_argument]: it is a caller error, not malformed input. *)
let enum_encode_check ~name ~cases ~closed ~to_int =
  if not closed then fun _ -> ()
  else
    let valid = Types.enum_values cases in
    fun v -> Types.check_enum_encode ~name ~valid (to_int v)

(* No reader exists for this shape. [Codec.get] is the only caller that can
   reach it, since every field the record decoder reads has a case in the table
   below, and [get] documents [Invalid_argument]. Refuse while the accessor is
   being staged rather than let a [Failure] escape from inside it on every
   read. *)
let no_field_reader : type a b. a typ -> b =
 fun typ -> Fmt.invalid_arg "Codec.get: no reader for %s" (field_shape typ)

(** Build a direct field reader that reads at a fixed offset. No tuples, no refs
    \- just pure value read. Caller must ensure the buffer is large enough. *)
let rec build_field_reader_ctx : type a.
    a typ -> int -> Types.eval_ctx -> Input_end.t -> bytes -> int -> a =
 fun typ field_off ->
  match typ with
  | Uint8 -> fun _runtime _input_end buf base -> UInt8.get buf (base + field_off)
  | Uint16 Little ->
      fun _runtime _input_end buf base -> UInt16.le buf (base + field_off)
  | Uint16 Big ->
      fun _runtime _input_end buf base -> UInt16.be buf (base + field_off)
  | Uint32 Little ->
      fun _runtime _input_end buf base -> UInt32.le buf (base + field_off)
  | Uint32 Big ->
      fun _runtime _input_end buf base -> UInt32.be buf (base + field_off)
  | Uint64 Little ->
      fun _runtime _input_end buf base -> UInt64.le buf (base + field_off)
  | Uint64 Big ->
      fun _runtime _input_end buf base -> UInt64.be buf (base + field_off)
  | Int8 -> fun _runtime _input_end buf base -> SInt8.get buf (base + field_off)
  | Int16 Little ->
      fun _runtime _input_end buf base -> SInt16.le buf (base + field_off)
  | Int16 Big ->
      fun _runtime _input_end buf base -> SInt16.be buf (base + field_off)
  | Int32 Little ->
      fun _runtime _input_end buf base -> SInt32.le buf (base + field_off)
  | Int32 Big ->
      fun _runtime _input_end buf base -> SInt32.be buf (base + field_off)
  | Int64 Little ->
      fun _runtime _input_end buf base ->
        Bytes.get_int64_le buf (base + field_off)
  | Int64 Big ->
      fun _runtime _input_end buf base ->
        Bytes.get_int64_be buf (base + field_off)
  | Float32 Little ->
      fun _runtime _input_end buf base ->
        Int32.float_of_bits (Bytes.get_int32_le buf (base + field_off))
  | Float32 Big ->
      fun _runtime _input_end buf base ->
        Int32.float_of_bits (Bytes.get_int32_be buf (base + field_off))
  | Float64 Little ->
      fun _runtime _input_end buf base ->
        Int64.float_of_bits (Bytes.get_int64_le buf (base + field_off))
  | Float64 Big ->
      fun _runtime _input_end buf base ->
        Int64.float_of_bits (Bytes.get_int64_be buf (base + field_off))
  | Uint_var { size = Int n; endian } ->
      fun _runtime _input_end buf base ->
        Uint_var.read endian buf (base + field_off) n
  | Byte_array { size = Int n } ->
      const_span_reader ~field_off n (fun _runtime _input_end buf base ->
          Bytes.sub_string buf (base + field_off) n)
  | Byte_array_where { size = Int n; elt_var; cond } ->
      const_span_reader ~field_off n (fun _runtime _input_end buf base ->
          refined_span_reader ~elt_var ~cond buf ~first:(base + field_off)
            ~len:n)
  | Byte_slice { size = Int n } ->
      const_span_reader ~field_off n (fun _runtime _input_end buf base ->
          Slice.make_or_eod buf ~first:(base + field_off) ~length:n)
  (* The region is fixed, so the field compiles on this path like any other
     constant span; the value is the bytes up to the NUL within it, and
     [zeroterm_nul_pos] refuses a region with no terminator. *)
  | Zeroterm_at_most { size = Int n } ->
      const_span_reader ~field_off n (fun _runtime _input_end buf base ->
          let first = base + field_off in
          let nul = zeroterm_nul_pos buf ~first ~limit:(first + n) in
          Bytes.sub_string buf first (nul - first))
  | Where { inner; _ } -> build_field_reader_ctx inner field_off
  | Enum { base; cases; closed; _ } ->
      let read = build_field_reader_ctx base field_off in
      let check =
        enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
      in
      fun runtime input_end buf base ->
        check ~at:(base + field_off) (read runtime input_end buf base)
  | Map { inner; decode; index_bound; _ } ->
      let read = build_field_reader_ctx inner field_off in
      let decode = Types.map_decode ~index_bound decode in
      fun runtime input_end buf base ->
        decode ~at:(base + field_off) (read runtime input_end buf base)
  | Unit -> fun _runtime _input_end _buf _base -> ()
  (* A fixed-size sub-record / sub-codec as an array or nested element: decode
     it at the element offset. The element must be fixed-width (the Array case
     already gates on [field_wire_size]), so the sub-codec is too. *)
  | Codec { codec_decode; _ } ->
      fun runtime input_end buf base ->
        codec_decode runtime input_end buf (base + field_off)
  | Array { len = Int n; elem; seq = Seq_map s } -> (
      match field_wire_size elem with
      | Some elem_sz ->
          let read_elem = build_field_reader_ctx elem 0 in
          fun runtime input_end buf base ->
            let acc = Stdlib.ref s.empty in
            for i = 0 to n - 1 do
              let v =
                read_elem runtime input_end buf
                  (base + field_off + (i * elem_sz))
              in
              acc := s.add !acc v
            done;
            s.finish !acc
      | None -> no_field_reader typ)
  (* A statically gated optional is transparent: present is the inner at the
     same offset, absent is no bytes at all and the value is fixed. That is
     what [compile_optional] / [compile_optional_or] read on the decode path,
     so an accessor over the same field agrees with {!decode}. A runtime gate
     is not readable here: deciding presence means evaluating an expression
     over sibling fields, which only the compiled record plan holds. *)
  | Optional { present = Bool true; inner } ->
      let read = build_field_reader_ctx inner field_off in
      fun runtime input_end buf base -> Some (read runtime input_end buf base)
  | Optional { present = Bool false; _ } ->
      fun _runtime _input_end _buf _base -> None
  | Optional_or { present = Bool true; inner; _ } ->
      build_field_reader_ctx inner field_off
  | Optional_or { present = Bool false; default; _ } ->
      fun _runtime _input_end _buf _base -> default
  | _ -> no_field_reader typ

(* [eval_ctx] is necessary for parameter-dependent layouts, but routing every
   fixed scalar access through it costs an extra argument and a match per read.
   Keep a direct bytes-only reader for the types whose layout cannot consult an
   environment. The option is resolved once, while [Codec.get] is staged. *)
let rec build_immediate_reader : type a.
    a typ -> int -> (bytes -> int -> a) option =
 fun typ field_off ->
  let at base = base + field_off in
  match typ with
  | Uint8 -> Some (fun buf base -> UInt8.get buf (at base))
  | Uint16 Little -> Some (fun buf base -> UInt16.le buf (at base))
  | Uint16 Big -> Some (fun buf base -> UInt16.be buf (at base))
  | Uint32 Little -> Some (fun buf base -> UInt32.le buf (at base))
  | Uint32 Big -> Some (fun buf base -> UInt32.be buf (at base))
  | Uint64 Little -> Some (fun buf base -> UInt64.le buf (at base))
  | Uint64 Big -> Some (fun buf base -> UInt64.be buf (at base))
  | Int8 -> Some (fun buf base -> SInt8.get buf (at base))
  | Int16 Little -> Some (fun buf base -> SInt16.le buf (at base))
  | Int16 Big -> Some (fun buf base -> SInt16.be buf (at base))
  | Int32 Little -> Some (fun buf base -> SInt32.le buf (at base))
  | Int32 Big -> Some (fun buf base -> SInt32.be buf (at base))
  | Int64 Little -> Some (fun buf base -> Bytes.get_int64_le buf (at base))
  | Int64 Big -> Some (fun buf base -> Bytes.get_int64_be buf (at base))
  | Float32 Little ->
      Some
        (fun buf base -> Int32.float_of_bits (Bytes.get_int32_le buf (at base)))
  | Float32 Big ->
      Some
        (fun buf base -> Int32.float_of_bits (Bytes.get_int32_be buf (at base)))
  | Float64 Little ->
      Some
        (fun buf base -> Int64.float_of_bits (Bytes.get_int64_le buf (at base)))
  | Float64 Big ->
      Some
        (fun buf base -> Int64.float_of_bits (Bytes.get_int64_be buf (at base)))
  | Uint_var { size = Int n; endian } ->
      Some (fun buf base -> Uint_var.read endian buf (at base) n)
  | Where { inner; _ } -> build_immediate_reader inner field_off
  | Enum { base; cases; closed; _ } -> (
      match build_immediate_reader base field_off with
      | None -> None
      | Some read ->
          let check =
            enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
          in
          Some (fun buf base -> check ~at:(at base) (read buf base)))
  | Map { inner; decode; index_bound; _ } -> (
      match build_immediate_reader inner field_off with
      | None -> None
      | Some read ->
          let decode = Types.map_decode ~index_bound decode in
          Some (fun buf base -> decode ~at:(at base) (read buf base)))
  | Unit -> Some (fun _buf _base -> ())
  | _ -> None

(* Symmetric bytes-only writer for parameter-independent scalar fields. *)
let rec build_immediate_encoder : type a.
    a typ -> (bytes -> int -> a -> int) option = function
  | Uint8 -> Some (setter_off 1 UInt8.set)
  | Uint16 Little -> Some (setter_off 2 UInt16.set_le)
  | Uint16 Big -> Some (setter_off 2 UInt16.set_be)
  | Uint32 Little -> Some (setter_off 4 UInt32.set_le)
  | Uint32 Big -> Some (setter_off 4 UInt32.set_be)
  | Uint64 Little -> Some (setter_off 8 UInt64.set_le)
  | Uint64 Big -> Some (setter_off 8 UInt64.set_be)
  | Int8 -> Some (setter_off 1 SInt8.set)
  | Int16 Little -> Some (setter_off 2 SInt16.set_le)
  | Int16 Big -> Some (setter_off 2 SInt16.set_be)
  | Int32 Little -> Some (setter_off 4 Types.set_int32_le)
  | Int32 Big -> Some (setter_off 4 Types.set_int32_be)
  | Int64 Little -> Some (setter_off 8 Bytes.set_int64_le)
  | Int64 Big -> Some (setter_off 8 Bytes.set_int64_be)
  | Float32 Little -> Some (float32_field_encoder Bytes.set_int32_le)
  | Float32 Big -> Some (float32_field_encoder Bytes.set_int32_be)
  | Float64 Little -> Some (float64_field_encoder Bytes.set_int64_le)
  | Float64 Big -> Some (float64_field_encoder Bytes.set_int64_be)
  | Uint_var { size = Int n; endian } -> Some (uint_var_field_encoder endian n)
  | Where { inner; _ } -> build_immediate_encoder inner
  | Enum { base; _ } -> build_immediate_encoder base
  | Map { inner; encode; _ } -> (
      match build_immediate_encoder inner with
      | None -> None
      | Some write -> Some (fun buf off v -> write buf off (encode v)))
  | Unit -> Some (fun _buf off () -> off)
  | Bits { width; base; bit_order } ->
      Some (bits_field_encoder ~width ~base ~bit_order)
  | _ -> None
