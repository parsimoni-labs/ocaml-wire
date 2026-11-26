(* How a single field is read and written in place, staged from its type and
   its place in the record: the readers and writers of [Codec.get] and
   [Codec.set]. *)

open Types
module Slice = Bytesrw.Bytes.Slice
open Codec_leaf
open Codec_repr
open Codec_elem
open Codec_compile

(* The [width] bytes of a field of type [typ] at [at] lie in the input, where
   the type has a width; a type without one checks the bytes it reads. *)
let field_bounds typ =
  match field_wire_size typ with
  | Some width -> fun input_end at -> check_read_bounds input_end ~at ~width
  | None -> fun _ _ -> ()

(* The value of the [len] bytes at [first] of a byte field. *)
let span_reader : type a. a typ -> bytes -> first:int -> len:int -> a = function
  | Byte_slice _ ->
      fun buf ~first ~len -> Slice.make_or_eod buf ~first ~length:len
  | Byte_array _ -> fun buf ~first ~len -> Bytes.sub_string buf first len
  | Byte_array_where { elt_var; cond; test; _ } ->
      refined_span_reader ~elt_var ~cond ~test
  | _ -> invalid_arg "Codec.get: not a byte field"

(* Build a staged reader from field type and access info. Each read checks
   that the bytes it takes lie in the input, so bytes too short for the field
   are a [Parse_error] like any other bad input.
   For Fixed: use build_field_reader which handles Where/Enum/Map.
   For Bitfield: the GADT ensures 'a = int via Bits constructor.
   For Dynamic: compute offset at read time. *)
let rec build_staged_reader : type a.
    a typ -> field_access -> runtime -> Input_end.t -> bytes -> int -> a =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      let read = build_bf_accessor_reader base byte_off shift width in
      let n = bf_base_byte_size base in
      fun _runtime input_end buf base ->
        check_read_bounds input_end ~at:(base + byte_off) ~width:n;
        read buf base
  | _, Fixed off ->
      let read = build_field_reader_ctx typ off in
      let bounds = field_bounds typ in
      fun runtime input_end buf base ->
        bounds input_end (base + off);
        read runtime input_end buf base
  | _, Dynamic fn ->
      let reader_at_0 = build_field_reader_ctx typ 0 in
      let bounds = field_bounds typ in
      fun runtime input_end buf base ->
        let at = base + fn runtime input_end buf base in
        bounds input_end at;
        reader_at_0 runtime input_end buf at
  | ( (Byte_slice _ | Byte_array _ | Byte_array_where _),
      Variable { off; size_fn } ) ->
      let read = span_reader typ in
      fun runtime input_end buf base ->
        let first = base + off and sz = size_fn runtime input_end buf base in
        check_span_bounds input_end ~first ~sz;
        read buf ~first ~len:sz
  | ( (Byte_slice _ | Byte_array _ | Byte_array_where _),
      Variable_dynamic { off_fn; size_fn } ) ->
      let read = span_reader typ in
      fun runtime input_end buf base ->
        let first = base + off_fn runtime input_end buf base in
        let sz = size_fn runtime input_end buf base in
        check_span_bounds input_end ~first ~sz;
        read buf ~first ~len:sz
  | Where { inner; _ }, _ -> build_staged_reader inner access
  | Enum { base; cases; closed; _ }, _ ->
      let read = build_staged_reader base access in
      let check =
        enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
      in
      let at = access_at access in
      fun runtime input_end buf base ->
        check
          ~at:(at runtime input_end buf base)
          (read runtime input_end buf base)
  | Map { inner; decode; index_bound; _ }, _ -> (
      let read = build_staged_reader inner access in
      match index_bound with
      | None ->
          fun runtime input_end buf base ->
            decode (read runtime input_end buf base)
      | Some _ ->
          let decode = Types.map_decode ~index_bound decode in
          let at = access_at access in
          fun runtime input_end buf base ->
            decode
              ~at:(at runtime input_end buf base)
              (read runtime input_end buf base))
  | _, Bitfield _ ->
      invalid_arg "Codec.get: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.get: unsupported variable-size field type"

(* Every leaf encoder checks the value before it writes a byte, so it puts
   down the whole field or nothing. A composite one writes part by part: a
   sub-codec validates the record it has just laid down, a casetype writes its
   tag ahead of its body, an array walks its elements one at a time. Any of
   those can raise with bytes already in the buffer. *)
let rec encoder_writes_before_checking : type a. a typ -> bool = function
  | Codec _ | Casetype _ | Union _ | Array _ | Repeat _ -> true
  | Single_elem { elem; _ } -> encoder_writes_before_checking elem
  | Map { inner; _ } -> encoder_writes_before_checking inner
  | Where { inner; _ } -> encoder_writes_before_checking inner
  | Enum { base; _ } -> encoder_writes_before_checking base
  | Optional { inner; _ } -> encoder_writes_before_checking inner
  | Optional_or { inner; _ } -> encoder_writes_before_checking inner
  | Apply { typ; _ } -> encoder_writes_before_checking typ
  | _ -> false

(* [Codec.set] promises the buffer is untouched when it raises, so a writer
   that can raise mid-field keeps a copy of the bytes it is about to replace
   and puts them back. The width is the value-driven size the record encoder
   budgets the same field with, clipped to what the buffer holds so a short
   buffer still fails through the encoder's own message. *)
let field_writer : type a. a typ -> Types.eval_ctx -> bytes -> int -> a -> unit
    =
 fun typ ->
  let encode = build_field_encoder_ctx typ in
  if not (encoder_writes_before_checking typ) then fun runtime buf off value ->
    ignore (encode runtime buf off value : int)
  else fun runtime buf off value ->
    let room = Bytes.length buf - off in
    let saved =
      if off >= 0 && room > 0 then
        Bytes.sub buf off (min (Types.size_of_typ_value runtime typ value) room)
      else Bytes.empty
    in
    match encode runtime buf off value with
    | _ -> ()
    | exception e ->
        let n = Bytes.length saved in
        if n > 0 then Bytes.blit saved 0 buf off n;
        raise e

(* Build a staged writer from field type and access info. *)
let rec build_staged_writer : type a.
    a typ -> field_access -> runtime -> bytes -> int -> a -> unit =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      let write = build_bf_accessor_writer base byte_off shift width in
      fun _runtime buf base value -> write buf base value
  | _, Fixed off ->
      let encode = field_writer typ in
      fun runtime buf base value -> encode runtime buf (base + off) value
  | _, Dynamic fn ->
      let encode = field_writer typ in
      fun runtime buf base value ->
        let off = fn runtime (Input_end.of_bytes buf) buf base in
        encode runtime buf (base + off) value
  (* A field whose size is only known at run time still owns exactly that many
     bytes: writing the value's length instead would overwrite the fields that
     follow. Check before blitting, with the same error [Codec.encode] gives. *)
  | Byte_slice _, Variable { off; size_fn } ->
      fun runtime buf base value ->
        let src = (value : Slice.t) in
        let len = Slice.length src in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + off) len
  | Byte_array _, Variable { off; size_fn } ->
      fun runtime buf base value ->
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + off) len
  | Byte_array_where { elt_var; cond; test; _ }, Variable { off; size_fn } ->
      let check = Eval.check_byte_refinement ~elt_var ~cond ~test in
      fun runtime buf base value ->
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        check s;
        Bytes.blit_string s 0 buf (base + off) len
  | Byte_slice _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let src = (value : Slice.t) in
        let len = Slice.length src in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit (Slice.bytes src) (Slice.first src) buf (base + fo) len
  | Byte_array _, Variable_dynamic { off_fn; size_fn } ->
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        Bytes.blit_string s 0 buf (base + fo) len
  | ( Byte_array_where { elt_var; cond; test; _ },
      Variable_dynamic { off_fn; size_fn } ) ->
      let check = Eval.check_byte_refinement ~elt_var ~cond ~test in
      fun runtime buf base value ->
        let fo = off_fn runtime (Input_end.of_bytes buf) buf base in
        let s = (value : string) in
        let len = String.length s in
        var_bytes_check_len size_fn runtime buf base ~actual:len;
        check s;
        Bytes.blit_string s 0 buf (base + fo) len
  | Where { inner; _ }, _ -> build_staged_writer inner access
  | Enum { base; _ }, _ -> build_staged_writer base access
  | Map { inner; encode; _ }, _ ->
      let write = build_staged_writer inner access in
      fun runtime buf base value -> write runtime buf base (encode value)
  | _, Bitfield _ ->
      invalid_arg "Codec.set: non-bitfield type with bitfield access"
  | _, Variable _ | _, Variable_dynamic _ ->
      invalid_arg "Codec.set: unsupported variable-size field type"

(* Fast accessor variants never construct or inspect an evaluation context.
   Returning [None] keeps parameter-dependent and compound layouts on the
   context-threaded path above. *)
let rec build_immediate_staged_reader : type a.
    a typ -> field_access -> (bytes -> int -> a) option =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      Some (build_bf_accessor_reader base byte_off shift width)
  | _, Fixed off -> build_immediate_reader typ off
  | Where { inner; _ }, _ -> build_immediate_staged_reader inner access
  | Enum { base; cases; closed; _ }, _ -> (
      match build_immediate_staged_reader base access with
      | None -> None
      | Some read ->
          let check =
            enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
          in
          let off = immediate_access_off access in
          Some (fun buf base -> check ~at:(base + off) (read buf base)))
  | Map { inner; decode; index_bound; _ }, _ -> (
      match build_immediate_staged_reader inner access with
      | None -> None
      | Some read ->
          let decode = Types.map_decode ~index_bound decode in
          let off = immediate_access_off access in
          Some (fun buf base -> decode ~at:(base + off) (read buf base)))
  | _ -> None

let rec build_immediate_staged_writer : type a.
    a typ -> field_access -> (bytes -> int -> a -> unit) option =
 fun typ access ->
  match (typ, access) with
  | Bits _, Bitfield { base; byte_off; shift; width } ->
      Some (build_bf_accessor_writer base byte_off shift width)
  | _, Fixed off -> build_immediate_setter typ off
  | Where { inner; _ }, _ -> build_immediate_staged_writer inner access
  | Enum { base; _ }, _ -> build_immediate_staged_writer base access
  | Map { inner; encode; _ }, _ -> (
      match build_immediate_staged_writer inner access with
      | None -> None
      | Some write -> Some (fun buf base value -> write buf base (encode value))
      )
  | _ -> None

(* The bytes from a record's offset to the end of a field at a constant
   offset, for a reader that needs no context: the field's own bytes. *)
let immediate_extent : type a. a typ -> field_access -> (int * int) option =
 fun typ access ->
  match access with
  | Bitfield { base; byte_off; _ } -> Some (byte_off, bf_base_byte_size base)
  | Fixed off -> Option.map (fun w -> (off, w)) (field_wire_size typ)
  | Dynamic _ | Variable _ | Variable_dynamic _ -> None
