open Dsl

(* Encoded byte size of [v] under [typ], computed from the value rather
   than from a buffer. Mirrors [field_wire_size] for fixed cases and
   reads the value for variable byte fields. Sub-codecs delegate to the
   value-driven [codec_size_of_value] baked in at codec construction.

   Typs whose size depends on a parameter or sibling field ([Uint_var]
   with non-constant size, dynamic [Optional]/[Optional_or], parametric
   [Repeat] / [Single_elem]) are not handled here -- they only appear
   inside a codec, which threads its own value-driven size at seal. *)

(* A field whose byte extent is an input parameter cannot be measured from the
   value alone. Returning 0 would under-report the whole field, and a caller
   sizing a buffer from that answer writes past it, so refuse instead. *)

(** Compute wire size of a type (None for variable-size types). *)
let unsized_field what =
  Fmt.invalid_arg
    "Codec.size_of_value: the %s field's size is an input parameter; pass ?env \
     so it can be resolved."
    what

let resolved_size ctx size what =
  match size_in_ctx ctx size with Some n -> n | None -> unsized_field what

let rec size_of_typ_value : type a. eval_ctx -> a typ -> a -> int =
 fun ctx typ v ->
  match typ with
  | Uint8 -> 1
  | Uint16 _ -> 2
  | Uint32 _ -> 4
  | Uint64 _ -> 8
  | Int8 -> 1
  | Int16 _ -> 2
  | Int32 _ -> 4
  | Int64 _ -> 8
  | Float32 _ -> 4
  | Float64 _ -> 8
  | Bits { base = U8; _ } -> 1
  | Bits { base = U16 _; _ } -> 2
  | Bits { base = U32 _; _ } -> 4
  | Unit -> 0
  | All_bytes -> String.length v
  | All_zeros -> String.length v
  | Zeroterm -> String.length v + 1
  | Zeroterm_at_most { size = Int n } -> n
  | Zeroterm_at_most { size } -> resolved_size ctx size "zeroterm_at_most"
  | Byte_array { size = Int n } -> n
  | Byte_array _ -> String.length v
  | Byte_array_where { size = Int n; _ } -> n
  | Byte_array_where _ -> String.length v
  | Byte_slice { size = Int n } -> n
  | Byte_slice _ -> Bytesrw.Bytes.Slice.length v
  | Uint_var { size = Int n; _ } -> n
  | Uint_var { size; _ } -> resolved_size ctx size "uint_var"
  | Map { inner; encode; _ } -> size_of_typ_value ctx inner (encode v)
  | Where { inner; _ } -> size_of_typ_value ctx inner v
  | Enum { base; _ } -> size_of_typ_value ctx base v
  | Optional { present = Bool true; inner } ->
      size_of_typ_value ctx inner (Option.get v)
  | Optional { present = Bool false; _ } -> 0
  | Optional { inner; _ } -> (
      (* Dynamic gate: value drives presence at encode time. The
         buffer-driven [present_fn] is the decode-side oracle and would
         disagree with [v] here, so consult [v] directly. *)
      match v with
      | Some inner_v -> size_of_typ_value ctx inner inner_v
      | None -> 0)
  | Optional_or { present = Bool true; inner; _ } ->
      size_of_typ_value ctx inner v
  | Optional_or { present = Bool false; _ } -> 0
  | Optional_or { inner; _ } ->
      (* Dynamic gate: encode always has a value (the default fills in
         for [None]); the encoder writes the inner regardless of what
         the runtime gate would have said at decode. *)
      size_of_typ_value ctx inner v
  | Codec { codec_size_of_value; _ } -> codec_size_of_value ctx v
  | Single_elem { size = Int n; elem; at_most } ->
      let actual = size_of_typ_value ctx elem v in
      check_nested_size ~at_most ~expected:n ~actual;
      n
  | Single_elem { size; elem; at_most } ->
      let n = resolved_size ctx size "nested" in
      let actual = size_of_typ_value ctx elem v in
      check_nested_size ~at_most ~expected:n ~actual;
      n
  | Repeat { size = Int expected; elem; seq } ->
      exact_repeat_elements seq ~expected
        ~size_of:(size_of_typ_value ctx elem)
        v
      |> List.fold_left (fun total (_, size) -> total + size) 0
  | Repeat { size; elem; seq = Seq_map s } -> (
      (* The budget is what the description declares the region to be; when the
         context resolves it, encode is held to it here as it is for a literal.
         A budget naming a sibling field does not resolve from a value, so the
         elements' own extent is the answer, which is what encode writes. *)
      match size_in_ctx ctx size with
      | Some expected ->
          exact_repeat_elements (Seq_map s) ~expected
            ~size_of:(size_of_typ_value ctx elem)
            v
          |> List.fold_left (fun total (_, size) -> total + size) 0
      | None ->
          let total = Stdlib.ref 0 in
          s.iter (fun e -> total := !total + size_of_typ_value ctx elem e) v;
          !total)
  | Array { len = Int expected; elem; seq } ->
      exact_array_elements seq ~expected v
      |> List.fold_left
           (fun total value -> total + size_of_typ_value ctx elem value)
           0
  | Array { len; elem; seq = Seq_map s } -> (
      match size_in_ctx ctx len with
      | Some expected ->
          exact_array_elements (Seq_map s) ~expected v
          |> List.fold_left
               (fun total value -> total + size_of_typ_value ctx elem value)
               0
      | None ->
          let total = Stdlib.ref 0 in
          s.iter (fun e -> total := !total + size_of_typ_value ctx elem e) v;
          !total)
  | Apply { typ; _ } -> size_of_typ_value ctx typ v
  | Casetype { tag; cases; _ } ->
      (* Tag bytes plus the matched case's body: find the branch whose
         [project] accepts [v], then size its inner from the projected body.
         Without this the value-driven size dropped the whole casetype to 0,
         so [Codec.size_of_value] under-allocated and [encode] ran off the
         buffer. A value no branch projects has no size for the same reason it
         has no bytes, and says so rather than answering with the tag width a
         caller would then allocate for. *)
      let tag_size = Option.value ~default:0 (Pp_3d.inner_wire_size tag) in
      let rec find = function
        | [] -> Encode_guard.raise_no_matching_case ()
        | Case_branch { cb_inner; cb_project; _ } :: rest -> (
            match cb_project v with
            | Some (_tag, body) ->
                tag_size + size_of_typ_value ctx cb_inner body
            | None -> find rest)
      in
      find cases
  | Union { cases; _ } ->
      (* The tag is another field's bytes: the union is its body alone. *)
      let rec find = function
        | [] -> Encode_guard.raise_no_matching_case ()
        | Case_branch { cb_inner; cb_project; _ } :: rest -> (
            match cb_project v with
            | Some (_tag, body) -> size_of_typ_value ctx cb_inner body
            | None -> find rest)
      in
      find cases
  | Struct _ -> 0
  | Type_ref _ -> 0
  | Qualified_ref _ -> 0

let rec field_wire_size : type a. a typ -> int option = function
  | Uint_var { size = Int n; _ } -> Some n
  | Uint_var _ -> None
  | Bits { base; _ } -> (
      match base with U8 -> Some 1 | U16 _ -> Some 2 | U32 _ -> Some 4)
  | Unit -> Some 0
  | Byte_array { size = Int n }
  | Byte_array_where { size = Int n; _ }
  | Byte_slice { size = Int n }
  (* A bounded zero-terminated string spans its whole region whatever the
     terminator's position, as [elem_size_of] already reports it. *)
  | Zeroterm_at_most { size = Int n } ->
      Some n
  | Where { inner; _ } -> field_wire_size inner
  | Enum { base; _ } -> field_wire_size base
  | Map { inner; _ } -> field_wire_size inner
  | Codec { codec_fixed_size; _ } -> codec_fixed_size
  | Optional { present = Bool true; inner } -> field_wire_size inner
  | Optional { present = Bool false; _ } -> Some 0
  | Optional _ -> None
  | Optional_or { present = Bool true; inner; _ } -> field_wire_size inner
  | Optional_or { present = Bool false; _ } -> Some 0
  | Optional_or _ -> None
  | Repeat _ -> None
  | Array { len = Int n; elem; _ } -> (
      match field_wire_size elem with Some k -> Some (n * k) | None -> None)
  | t -> Lower_3d.primitive_width t

(* An array of a literal count of fixed-width elements is a byte count too. *)
let array_len ~combinator len elem =
  match (literal_size ~combinator len, field_wire_size elem) with
  | Int n, Some k when k > 0 && n > Sys.max_string_length / k ->
      Fmt.invalid_arg
        "Wire.%s: %d elements of %d bytes are more than a buffer can hold"
        combinator n k
  | len, _ -> len

let array ~len elem =
  reject_decoration ~combinator:"array" elem;
  reject_bitfield_element ~combinator:"array" elem;
  reject_non_array_element ~combinator:"array" elem;
  (Array { len = array_len ~combinator:"array" len elem; elem; seq = seq_list }
    : _ typ)

let array_seq seq ~len elem =
  reject_decoration ~combinator:"array_seq" elem;
  reject_bitfield_element ~combinator:"array_seq" elem;
  reject_non_array_element ~combinator:"array_seq" elem;
  (Array { len = array_len ~combinator:"array_seq" len elem; elem; seq }
    : _ typ)

let c_type_of : type a. a typ -> string = function
  | Uint8 | Bits { base = U8; _ } -> "uint8_t"
  | Uint16 _ | Bits { base = U16 _; _ } -> "uint16_t"
  | Uint32 _ | Bits { base = U32 _; _ } -> "uint32_t"
  | Uint64 _ -> "uint64_t"
  (* Signed types project to UINT* in 3D so EverParse only sees unsigned
     widths; the same width drives the C field type. *)
  | Int8 -> "uint8_t"
  | Int16 _ -> "uint16_t"
  | Int32 _ -> "uint32_t"
  | Int64 _ -> "uint64_t"
  (* Floats are also projected as the same-width UINT* in 3D, since the
     verified parser sees the bit pattern; the float reinterpretation is
     OCaml-side. *)
  | Float32 _ -> "uint32_t"
  | Float64 _ -> "uint64_t"
  | Uint_var _ -> "uint32_t"
  | _ -> "uint32_t"

let ml_type_of : type a. a typ -> string = function
  | Uint8 | Uint16 _ | Uint32 _ | Uint_var _ | Bits _ -> "int"
  | Uint64 _ -> "int64"
  | Int8 | Int16 _ | Int32 _ -> "int"
  | Int64 _ -> "int64"
  | Float32 _ | Float64 _ -> "float"
  | _ -> "int"
