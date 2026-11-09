open Types
module Slice = Bytesrw.Bytes.Slice
open Codec_leaf
open Codec_repr
open Codec_elem

(* -- Compiled field: intermediate plan for one field's contribution -- *)

(* A [compiled_field] is the self-contained plan for appending one field to a
   record state. The per-type [compile_*] helpers build one; [apply_compiled]
   consumes it and produces the updated record state. *)
type ('a, 'r) compiled_field = {
  raw_reader : runtime -> Input_end.t -> bytes -> int -> 'a;
  raw_writer : 'r -> runtime -> bytes -> int -> int -> int;
      (* [raw_writer v runtime buf base write_off] writes the field's bytes
         starting at [write_off] in [buf] and returns the offset just
         after them. [base] is the record's base offset, used by
         buffer-driven helpers (size cross-checks, dynamic-gate cross
         checks) -- the field's own write position comes from
         [write_off]. *)
  extra_writers : ('r -> runtime -> bytes -> int -> int -> int) list;
      (* Writers that run strictly before [raw_writer] -- e.g. a bf_clear that
         opens a new packed base word. Returns the input [write_off] unchanged. *)
  field_access : field_access;
  size_delta : int; (* added to [min_wire_size] *)
  next_off : next_off; (* new [next_off] *)
  bf_after : bf_codec_state option;
  int_reader : runtime -> Input_end.t -> bytes -> int -> int;
      (* Entry stored in [field_readers] under the field name; const 0 for
         composite types that have no int-array slot of their own. *)
  nested_readers : field_reader list;
      (* Embedded sub-codec field readers, shifted into the parent frame. *)
  validator_off : int; (* [-1] when the byte offset is dynamic *)
  populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit;
      (* Pre-built populate function for the int-array validator path; the
         slot index (n_fields at the time the field is compiled) is baked
         in. Composite types use a no-op. *)
}

(* Parameter-free slice of the record state that [compile_field] needs. Kept
   separate so the per-type helpers do not need polymorphic recursion across
   the record's 'f type. *)
type layout_ctx = {
  next_off : next_off;
  fixed_prefix : int ref;
  bf : bf_codec_state option;
  field_readers : field_reader list;
  field_at : string -> (runtime -> Input_end.t -> bytes -> int -> int) option;
      (* Where an earlier field's bytes start, by name: what a field that
         rejects another field's value reports as the place of the failure. *)
  n_fields : int;
}

let layout_ctx_of : type f r. (f, r) record -> layout_ctx =
 fun (Record r) ->
  {
    next_off = r.next_off;
    fixed_prefix = r.fixed_prefix;
    bf = r.r_bf;
    field_readers = r.field_readers;
    field_at =
      (fun name ->
        Option.map
          (fun (_id, access) -> access_at access)
          (List.assoc_opt name r.field_access_rev));
    n_fields = r.n_fields;
  }

(* -- Layout helpers shared by per-type plans -- *)

let static_off_of (ctx : layout_ctx) : int option =
  match ctx.next_off with Static n -> Some n | Dynamic _ -> None

(* Where the next field starts, counted from the record base. The relative
   twin of [next_off_pos], which counts from the buffer. *)
let next_off_rel : next_off -> runtime -> Input_end.t -> bytes -> int -> int =
  function
  | Static n -> fun _runtime _input_end _buf _base -> n
  | Dynamic f ->
      fun runtime input_end buf base -> f runtime input_end buf base - base

let off_fn_of (ctx : layout_ctx) = next_off_rel ctx.next_off

(* Byte offset used as [sizeof_this] when compiling constraints/actions: a
   real static offset when known, the [-1] sentinel otherwise. *)
let validator_off_of (ctx : layout_ctx) : int =
  match ctx.next_off with Static n -> n | Dynamic _ -> -1

(* Field access that stores a constant or dynamic offset, based on [ctx]. *)
let fixed_or_dynamic_fa (ctx : layout_ctx) : field_access =
  match ctx.next_off with
  | Static n -> Fixed n
  | Dynamic _ -> Dynamic (off_fn_of ctx)

let require_static_off (ctx : layout_ctx) ~what : int =
  match ctx.next_off with
  | Static n -> n
  | Dynamic _ ->
      invalid_arg
        ("add_field: " ^ what ^ " after variable-size field not supported")

(* New [next_off] after appending a fixed-size contribution of [n] bytes. *)
(* Mirrors EverParse's [sum_size], which is what resolves 3D's [sizeof(this)]:
   while every field so far is fixed the count is the running offset, the first
   field with a dynamic extent freezes it at the offset that field starts from,
   and nothing after it moves the count again. *)
let note_fixed_prefix ~before ~after cell =
  match (before, after) with
  | Static _, Static n -> cell := n
  | Static n, Dynamic _ -> cell := n
  | Dynamic _, _ -> ()

let advance_next_off (no : next_off) (n : int) : next_off =
  match no with
  | Static k -> Static (k + n)
  | Dynamic f ->
      Dynamic
        (fun runtime input_end buf base -> f runtime input_end buf base + n)

let null_int_reader : runtime -> Input_end.t -> bytes -> int -> int =
 fun _runtime _input_end _buf _base -> 0

let no_populate : slots -> runtime -> Input_end.t -> bytes -> int -> unit =
 fun _arr _runtime _input_end _buf _base -> ()

(* A sub-codec field's check pass: run the sub-codec's own validator where the
   field sits. This is the whole of what the generated C emits for a sub-struct
   field, a call to the nested struct's validator, and it is what makes
   [Codec.validate] reject at a use site everything the sub-codec's [decode]
   rejects. No int slot is filled: a record-typed field has no scalar value an
   enclosing expression could reference. *)
let sub_codec_populate ~codec_validate ~off_fn :
    slots -> runtime -> Input_end.t -> bytes -> int -> unit =
 fun _arr runtime input_end buf base ->
  codec_validate runtime input_end buf (base + off_fn runtime input_end buf base)

(* Reader+writer pair for a Codec-or-scalar inner type placed at the current
   frame offset. Extracted so [compile_optional] and [compile_optional_or]
   share it. *)
let inner_codec_accessors : type w.
    w typ ->
    layout_ctx ->
    (runtime -> Input_end.t -> bytes -> int -> w)
    * (runtime -> bytes -> int -> w -> int) =
 fun inner ctx ->
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  let reader : runtime -> Input_end.t -> bytes -> int -> w =
    match inner with
    | Codec { codec_decode; _ } -> (
        match field_off_static with
        | Some fo ->
            fun runtime input_end buf base ->
              codec_decode runtime input_end buf (base + fo)
        | None ->
            fun runtime input_end buf base ->
              let fo = field_off_fn runtime input_end buf base in
              codec_decode runtime input_end buf (base + fo))
    | _ -> (
        match field_off_static with
        | Some fo ->
            let reader = build_field_reader_ctx inner fo in
            fun runtime input_end buf base -> reader runtime input_end buf base
        | None ->
            let reader_at_0 = build_field_reader_ctx inner 0 in
            fun runtime input_end buf base ->
              let fo = field_off_fn runtime input_end buf base in
              reader_at_0 runtime input_end buf (base + fo))
  in
  (* Writer takes the absolute byte position to write at. The caller
     gets this from the threaded [write_off]; no buffer-driven
     [off_fn] lookup happens on the encode path. *)
  let writer : runtime -> bytes -> int -> w -> int =
    match inner with
    | Codec { codec_encode; _ } ->
        fun runtime buf off iv -> codec_encode iv runtime buf off
    | _ ->
        fun runtime buf off iv ->
          let encode = build_field_encoder_ctx inner in
          encode runtime buf off iv
  in
  (reader, writer)

let build_nested_readers ctx readers =
  match static_off_of ctx with
  | Some fo ->
      List.map
        (fun (n, reader) ->
          ( n,
            fun runtime input_end buf base ->
              reader runtime input_end buf (base + fo) ))
        readers
  | None ->
      let off_fn = off_fn_of ctx in
      List.map
        (fun (n, reader) ->
          ( n,
            fun runtime input_end buf base ->
              reader runtime input_end buf
                (base + off_fn runtime input_end buf base) ))
        readers

(* -- Per-type [compile_field] helpers -- *)

(* Dispatch on the field's typ once, then delegate to the relevant per-type
   helper. Each helper builds the [compiled_field] for its case; neither the
   helpers nor [compile_field] itself apply constraints or actions -- that
   happens in [apply_compiled]. *)
let compile_bits : type r.
    layout_ctx ->
    (int, r) field ->
    int ->
    bitfield_base ->
    bit_order ->
    (int, r) compiled_field =
 fun ctx fld width base bit_order ->
  let total = bf_base_total_bits base in
  let static_off = require_static_off ctx ~what:"bitfields" in
  let base_byte_size = bf_base_byte_size base in
  let is_continuation =
    match ctx.bf with
    | Some bf ->
        bf_base_equal bf.base base
        && equal_bit_order bf.bit_order bit_order
        && bf.bits_used + width <= bf.total_bits
    | None -> false
  in
  let base_off, bits_used, size_delta, extra_writers =
    if is_continuation then
      match ctx.bf with
      | Some bf -> (bf.base_off, bf.bits_used, 0, [])
      | None -> assert false
    else
      let clear_at = build_bf_clear base 0 in
      ( static_off,
        0,
        base_byte_size,
        [
          (fun _v _runtime buf _base write_off ->
            clear_at buf write_off;
            write_off);
        ] )
  in
  let shift = Bitfield.shift ~bit_order ~total ~bits_used ~width in
  let read = build_bf_reader base base_off shift width in
  let raw_reader _runtime _input_end buf base = read buf base in
  let raw_writer_inner = build_bf_writer base 0 shift width in
  let get = fld.get in
  let back, advance =
    if is_continuation then (base_byte_size, 0) else (0, base_byte_size)
  in
  let raw_writer v _runtime buf _base write_off =
    raw_writer_inner buf (write_off - back) (get v);
    write_off + advance
  in
  {
    raw_reader;
    raw_writer;
    extra_writers;
    field_access = Bitfield { base; byte_off = base_off; shift; width };
    size_delta;
    next_off = Static (static_off + size_delta);
    bf_after =
      Some
        {
          base;
          bit_order;
          base_off;
          bits_used = bits_used + width;
          total_bits = total;
        };
    int_reader = raw_reader;
    nested_readers = [];
    validator_off = static_off;
    populate = build_populate fld.typ ctx.n_fields raw_reader;
  }

let compile_codec_variable : type a r.
    layout_ctx ->
    get:(r -> a) ->
    codec_decode:(runtime -> Input_end.t -> bytes -> int -> a) ->
    codec_validate:(runtime -> Input_end.t -> bytes -> int -> unit) ->
    codec_encode:(a -> runtime -> bytes -> int -> int) ->
    codec_size_of:(runtime -> Input_end.t -> bytes -> int -> int) ->
    nested_readers:field_reader list ->
    (a, r) compiled_field =
 fun ctx ~get ~codec_decode ~codec_validate ~codec_encode ~codec_size_of
     ~nested_readers ->
  let off_fn, (field_access : field_access), validator_off =
    match ctx.next_off with
    | Static n ->
        let size_fn runtime input_end buf base =
          codec_size_of runtime input_end buf (base + n)
        in
        ( (fun _runtime _input_end _buf _base -> n),
          Variable { off = n; size_fn },
          n )
    | Dynamic prev_end ->
        let off_fn runtime input_end buf base =
          prev_end runtime input_end buf base - base
        in
        let size_fn runtime input_end buf base =
          codec_size_of runtime input_end buf
            (base + off_fn runtime input_end buf base)
        in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  {
    raw_reader =
      (fun runtime input_end buf base ->
        codec_decode runtime input_end buf
          (base + off_fn runtime input_end buf base));
    raw_writer =
      (fun v runtime buf _base write_off ->
        codec_encode (get v) runtime buf write_off);
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic
        (fun runtime input_end buf base ->
          base
          + off_fn runtime input_end buf base
          + size_fn runtime input_end buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers;
    validator_off;
    populate = sub_codec_populate ~codec_validate ~off_fn;
  }

let compile_codec : type a r.
    layout_ctx ->
    (a, r) field ->
    codec_decode:(runtime -> Input_end.t -> bytes -> int -> a) ->
    codec_validate:(runtime -> Input_end.t -> bytes -> int -> unit) ->
    codec_encode:(a -> runtime -> bytes -> int -> int) ->
    codec_fixed_size:int option ->
    codec_size_of:(runtime -> Input_end.t -> bytes -> int -> int) ->
    codec_field_readers:
      (string * (runtime -> Input_end.t -> bytes -> int -> int)) list ->
    (a, r) compiled_field =
 fun ctx fld ~codec_decode ~codec_validate ~codec_encode ~codec_fixed_size
     ~codec_size_of ~codec_field_readers ->
  let nested_readers =
    build_nested_readers ctx
      (List.map (fun (n, r) -> (Some n, r)) codec_field_readers)
  in
  let get = fld.get in
  match codec_fixed_size with
  | Some fsize ->
      let raw_reader, inner_writer = inner_codec_accessors fld.typ ctx in
      let raw_writer v runtime buf _base write_off =
        inner_writer runtime buf write_off (get v)
      in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.next_off fsize;
        bf_after = None;
        int_reader = null_int_reader;
        nested_readers;
        validator_off = validator_off_of ctx;
        populate = sub_codec_populate ~codec_validate ~off_fn:(off_fn_of ctx);
      }
  | None ->
      compile_codec_variable ctx ~get ~codec_decode ~codec_validate
        ~codec_encode ~codec_size_of ~nested_readers

(* Variable-size sub-codec: dispatch on whether we sit at a static or a
   dynamic running offset. Mirrors [compile_var_bytes] -- both flavours
   produce a [Variable]/[Variable_dynamic] [field_access] that downstream
   [build_staged_reader]/[build_staged_writer] already know how to thread.
   Without the dynamic case, two consecutive variable-size sub-codec fields
   trip [require_static_off]. *)
let dynamic_optional_next_off ctx present_fn fsize =
  let base_off = ctx.next_off in
  Dynamic
    (fun runtime input_end buf base ->
      let off =
        match base_off with
        | Static n -> base + n
        | Dynamic f -> f runtime input_end buf base
      in
      if present_fn runtime input_end buf base then off + fsize else off)

(* Absolute byte position a [next_off] points at, given the record [base]. *)
let next_off_pos : next_off -> runtime -> Input_end.t -> bytes -> int -> int =
  function
  | Static n -> fun _runtime _input_end _buf base -> base + n
  | Dynamic f -> f

(* [present_fn] plus the bounds check the field's read needs when the gate is
   set. [read_guard] cannot install that check: it takes the extent from
   [field_wire_size], which is [None] for a conditional optional precisely
   because an absent one occupies nothing. A present one occupies exactly
   [fsize] bytes, so gate the extent check on the same condition as the read:
   an optional pushed past the end by an overlong predecessor then reports
   end-of-input at its own span instead of crashing. *)
let present_in_bounds ctx present_fn fsize =
  let pos = next_off_pos ctx.next_off in
  fun runtime input_end buf base ->
    let present = present_fn runtime input_end buf base in
    if present then
      check_read_bounds input_end
        ~at:(pos runtime input_end buf base)
        ~width:fsize;
    present

let optional_compiled : type a r.
    layout_ctx ->
    ?int_reader:(runtime -> Input_end.t -> bytes -> int -> int) ->
    raw_reader:(runtime -> Input_end.t -> bytes -> int -> a) ->
    raw_writer:(r -> runtime -> bytes -> int -> int -> int) ->
    size_delta:int ->
    next_off:next_off ->
    populate:(slots -> runtime -> Input_end.t -> bytes -> int -> unit) ->
    unit ->
    (a, r) compiled_field =
 fun ctx ?(int_reader = null_int_reader) ~raw_reader ~raw_writer ~size_delta
     ~next_off ~populate () ->
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access = fixed_or_dynamic_fa ctx;
    size_delta;
    next_off;
    bf_after = None;
    (* [int_reader] is the entry cross-field size/offset expressions read this
       field through. For an [optional_or] with an int inner it must read the
       present-or-default value, not the [null_int_reader] const 0 that left a
       byte_array sized by an optional_or field resolving to length 0. *)
    int_reader;
    nested_readers = [];
    validator_off = validator_off_of ctx;
    populate;
  }

let repeat_raw_fixed : type elt seq.
    (elt, seq) seq_map ->
    read:(runtime -> Input_end.t -> bytes -> int -> elt) ->
    int ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    seq =
 fun (Seq_map seq) ~read esz ~off_fn ~size_fn runtime input_end buf base ->
  let budget = size_fn runtime input_end buf base in
  let start = base + off_fn runtime input_end buf base in
  (* The budget comes from a length field, i.e. untrusted input. Bound it to the
     buffer before reading so an oversized length fails cleanly instead of
     crashing [read_elem] with an out-of-range [Bytes.sub]. *)
  if budget < 0 || start + budget > Bytes.length buf then
    raise_eof ~at:start ~expected:budget
      ~got:(bytes_from (Input_end.of_bytes buf) start);
  let remainder = if esz > 0 then budget mod esz else budget in
  if remainder <> 0 then
    raise_eof ~at:(start + budget - remainder) ~expected:esz ~got:remainder;
  let n = if esz > 0 then budget / esz else 0 in
  let rec loop acc i =
    if i >= n then seq.finish acc
    else
      loop (seq.add acc (read runtime input_end buf (start + (i * esz)))) (i + 1)
  in
  loop seq.empty 0

let repeat_raw_variable : type elt seq.
    (elt, seq) seq_map ->
    read:(runtime -> Input_end.t -> bytes -> int -> elt) ->
    size_of_elem:(runtime -> Input_end.t -> bytes -> int -> int) ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    seq =
 fun (Seq_map seq) ~read ~size_of_elem ~off_fn ~size_fn runtime input_end buf
     base ->
  let budget = size_fn runtime input_end buf base in
  let start = base + off_fn runtime input_end buf base in
  (* Bound the untrusted budget to the buffer (see [repeat_raw_fixed]). *)
  if budget < 0 || start + budget > Bytes.length buf then
    raise_eof ~at:start ~expected:budget
      ~got:(bytes_from (Input_end.of_bytes buf) start);
  let rec loop acc pos remaining =
    if remaining <= 0 then seq.finish acc
    else
      let esz = size_of_elem runtime input_end buf pos in
      if esz <= 0 || esz > remaining then
        raise_eof ~at:pos ~expected:esz ~got:remaining;
      let v = read runtime input_end buf pos in
      loop (seq.add acc v) (pos + esz) (remaining - esz)
  in
  loop seq.empty start budget

(* Accumulates nothing. [Codec.validate] wants a repeat's elements read for
   their checks and does not want the sequence, so the walk it runs drops each
   element as it goes: a list built and thrown away is allocation the sender
   sizes through the byte budget, on the path a server runs once per packet.
   Feeding the reader's own walk keeps the budget and split checks identical to
   what decode makes. *)
let seq_discard : type elt. (elt, unit) seq_map =
  Seq_map
    {
      empty = ();
      add = (fun () _elt -> ());
      finish = (fun () -> ());
      iter = (fun _f () -> ());
    }

let repeat_raw_reader : type elt seq.
    (elt, seq) seq_map ->
    read:(runtime -> Input_end.t -> bytes -> int -> elt) ->
    size_of_elem:(runtime -> Input_end.t -> bytes -> int -> int) ->
    elem_size:int option ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    seq =
 fun seq ~read ~size_of_elem ~elem_size ~off_fn ~size_fn ->
  match elem_size with
  | Some esz -> repeat_raw_fixed seq ~read esz ~off_fn ~size_fn
  | None -> repeat_raw_variable seq ~read ~size_of_elem ~off_fn ~size_fn

(* How the check walk gets at an element's checks without keeping the value.

   [By_reading] is the derived answer: run the element's own reader and drop
   what it built. It cannot disagree with decode, because it is decode, so it is
   the default for anything not deliberately classified otherwise.

   [Value_only] says the reader's only product is the value it hands back, so on
   the check walk there is nothing left for it to do: a fixed-width byte span
   re-slices or copies bytes the budget prologue has already bounded to the
   buffer, a NUL-terminated one's terminator search has run in [size_of_elem]
   before the reader is called, and a 64-bit or floating-point scalar is one
   unchecked read of a word inside the same bounded budget.

   [By] says the element type carries its own validator, which a sub-codec does.

   The last two are departures from derivation, and each is a claim about a
   reader that [Codec.validate] agreeing with [Codec.decode] is what keeps
   honest. *)
type 'a elem_strategy =
  | By_reading
  | Value_only
  | By of (runtime -> Input_end.t -> bytes -> int -> unit)

(* An array's walk: every element checked at its own stride. Top level and
   partially applied while the strategy is chosen, so no closure is built per
   element. *)
let array_elem_check ~n ~esz check runtime input_end buf off =
  for i = 0 to n - 1 do
    check runtime input_end buf (off + (i * esz))
  done

(* Run the matched case's check. Mirrors [read_case_body], on cases whose
   bodies already carry their check, and is a top-level recursion for the same
   reason: an inner [let rec find] would allocate a closure per casetype
   checked. *)
let rec dispatch_case_check : type k.
    at:int ->
    tag:k typ ->
    (k option * (runtime -> Input_end.t -> bytes -> int -> unit)) list ->
    k ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    unit =
 fun ~at ~tag cases tag_val runtime input_end buf body_off ->
  match cases with
  | [] ->
      raise_invalid_tag ~at (Option.value ~default:0 (Eval.int_of tag tag_val))
  | (Some t, _) :: rest when t <> tag_val ->
      dispatch_case_check ~at ~tag rest tag_val runtime input_end buf body_off
  | (_, check) :: _ -> check runtime input_end buf body_off

(* A casetype's check: read the discriminant, then run the selected body's
   check where the body starts. The tag's own extent was bounded before the
   walk was reached -- by the budget for a repeat element, by the field's size
   expression ([elem_size_of], which guards the tag) for a field. *)
let casetype_elem_check : type k.
    tag:k typ ->
    (k option * (runtime -> Input_end.t -> bytes -> int -> unit)) list ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    unit =
 fun ~tag cases runtime input_end buf off ->
  let tag_val = read_elem tag runtime input_end buf off in
  dispatch_case_check ~at:off ~tag cases tag_val runtime input_end buf
    (off + tag_byte_size tag)

(* Every constructor is listed and there is deliberately no [| _ -> ...]: a typ
   added later must not pick a strategy by falling through, since picking
   [Value_only] for a reader that checks something is how [Codec.validate] comes
   to accept what [Codec.decode] rejects. Adding one stops the build until
   somebody chooses.

   Exhaustiveness only catches a typ arriving. Moving one that is already here,
   [Codec] out of [By] into [By_reading] above all, type-checks and keeps every
   verdict intact, because reading an element and dropping it reaches the same
   answer by building the record first. What catches that is
   [test_repeat_validate_allocation_is_bounded] in [test/test_codec.ml], which
   validates one repeat at two byte budgets and requires the words per call to
   come out equal, with a sub-codec among the element types it measures.

   A container has nothing of its own to check, so its strategy is its
   contents': reading one to drop it builds the sequence, the region's value or
   the case body first, and for a sub-codec body that is exactly the record
   [codec_validate] exists not to build. Putting one back on [By_reading] also
   type-checks and keeps every verdict;
   [test_container_subcodec_validate_no_alloc] is what catches it, by measuring
   a sub-codec through each container against the same sub-codec as a plain
   field. *)
let rec elem_strategy : type a. a typ -> a elem_strategy = function
  | Codec { codec_validate; _ } -> By codec_validate
  | Byte_array { size = Int _ } | Byte_slice { size = Int _ } | Zeroterm ->
      Value_only
  | Uint64 _ | Int64 _ | Float32 _ | Float64 _ -> Value_only
  | Single_elem { elem; _ } -> By (elem_check elem)
  | Array { len = Int n; elem; _ } -> (
      match field_wire_size elem with
      (* No stride to walk at. Leave it to [read_elem], which is where the
         missing element size is reported. *)
      | None -> By_reading
      | Some esz -> (
          match elem_strategy elem with
          | Value_only -> Value_only
          | By_reading | By _ -> By (array_elem_check ~n ~esz (elem_check elem))
          ))
  | Casetype { tag; cases; _ } ->
      By (casetype_elem_check ~tag (case_checks cases))
  | Uint8 | Uint16 _ | Uint32 _ | Int8 | Int16 _ | Int32 _ | Uint_var _ | Bits _
  | Unit | All_bytes | All_zeros | Zeroterm_at_most _ | Where _ | Array _
  | Byte_array _ | Byte_array_where _ | Byte_slice _ | Enum _ | Struct _
  | Type_ref _ | Qualified_ref _ | Map _ | Apply _ | Optional _ | Optional_or _
  | Repeat _ | Union _ ->
      By_reading

(* The element check the walk runs. Skipping a value-only reader keeps the span
   out of the heap, and letting a sub-codec check itself keeps the record out of
   it: the element count comes from the byte budget, which comes from a length
   field, so anything built per element is a multiplier the sender sizes on the
   path a server runs once per packet. This is the C's element loop, which calls
   the element validator and keeps one position. *)
and elem_check : type a. a typ -> runtime -> Input_end.t -> bytes -> int -> unit
    =
 fun typ ->
  match elem_strategy typ with
  | Value_only -> fun _runtime _input_end _buf _off -> ()
  | By check -> check
  | By_reading ->
      fun runtime input_end buf off ->
        ignore (read_elem typ runtime input_end buf off : a)

(* A casetype's cases with each body's check already chosen, so dispatching one
   is a tag comparison and a call. *)
and case_checks : type a k.
    (a, k) case_branch list ->
    (k option * (runtime -> Input_end.t -> bytes -> int -> unit)) list =
 fun cases ->
  List.map
    (fun (Case_branch { cb_tag; cb_inner; _ }) -> (cb_tag, elem_check cb_inner))
    cases

(* Write a repeat's elements at their own strides. Kept out of
   [compile_repeat] so the writer is staged once, with the sequence, the element
   type and the field's projection already fixed. *)
let repeat_raw_writer : type elt seq r.
    (elt, seq) seq_map ->
    elt typ ->
    (r -> seq) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    r ->
    runtime ->
    bytes ->
    int ->
    int ->
    int =
 fun seq elem get ~size_fn v runtime buf base write_off ->
  let expected = size_fn runtime (Input_end.of_bytes buf) buf base in
  let sized_items =
    Types.exact_repeat_elements seq ~expected
      ~size_of:(Types.size_of_typ_value runtime elem)
      (get v)
  in
  let pos = Stdlib.ref write_off in
  List.iter
    (fun (item, size) ->
      write_elem elem runtime buf !pos item;
      pos := !pos + size)
    sized_items;
  !pos

let compile_repeat : type elt seq r.
    layout_ctx ->
    (seq, r) field ->
    int expr ->
    elt typ ->
    (elt, seq) seq_map ->
    (seq, r) compiled_field =
 fun ctx fld size_expr elem seq ->
  (* Same dynamic-offset dispatch as [compile_var_bytes] / [compile_codec]:
     when a [Repeat] sits after a variable-size field, the running offset is
     [Dynamic] and is resolved at runtime rather than from a static offset. *)
  let off_fn, (field_access : field_access), validator_off =
    let sizeof_this : runtime -> Input_end.t -> bytes -> int -> int =
      (* One constant for the whole description, as 3D's [sizeof(this)] is,
         resolved once the last field is in. *)
      let cell = ctx.fixed_prefix in
      fun _runtime _input_end _buf _base -> !cell
    in
    let size_fn = compile_expr ~sizeof_this ctx.field_readers size_expr in
    match ctx.next_off with
    | Static n ->
        ( (fun _runtime _input_end _buf _base -> n),
          Variable { off = n; size_fn },
          n )
    | Dynamic prev_end ->
        let off_fn runtime input_end buf base =
          prev_end runtime input_end buf base - base
        in
        (off_fn, Variable_dynamic { off_fn; size_fn }, -1)
  in
  let size_fn =
    match field_access with
    | Variable { size_fn; _ } -> size_fn
    | Variable_dynamic { size_fn; _ } -> size_fn
    | _ -> assert false
  in
  let elem_size = field_wire_size elem in
  let size_of_elem = elem_size_of elem in
  let raw_reader =
    repeat_raw_reader seq ~read:(read_elem elem) ~size_of_elem ~elem_size
      ~off_fn ~size_fn
  in
  {
    raw_reader;
    raw_writer = repeat_raw_writer seq elem fld.get ~size_fn;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic
        (fun runtime input_end buf base ->
          base
          + off_fn runtime input_end buf base
          + size_fn runtime input_end buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers = [];
    validator_off;
    (* [Codec.validate] reaches a repeat only through its populate, and the
       budget checks live in [repeat_raw_reader]. Run the same walk over
       {!seq_discard}, which checks every element and keeps none. *)
    populate =
      (let check =
         repeat_raw_reader seq_discard ~read:(elem_check elem) ~size_of_elem
           ~elem_size ~off_fn ~size_fn
       in
       fun _arr runtime input_end buf base -> check runtime input_end buf base);
  }

(* [expected] is a byte count, so a span whose offset overran the buffer cannot
   report its size: that size comes out negative, and every consumer does
   arithmetic on the count (the streaming reader's retry target, the corpus
   seeder's growth target), which a negative turns into a position behind what
   is already buffered. Report the bytes missing before the read could start
   instead, and keep the size for the ordinary case of a span that simply
   reaches too far. *)
let missing_before ~first ~sz ~len =
  if sz < 0 || first < 0 then max 0 (first - len) else sz

(* The span [first, first + sz) must lie inside [buf]. Both ends derive from
   length fields, i.e. untrusted input: a span sized past the buffer, or a
   negative size from an overrun offset, would crash the read with a raw
   [Invalid_argument] that escapes the decode result. Report the missing bytes
   as end-of-input instead. *)
let[@inline] check_span_bounds buf ~first ~sz =
  let len = Bytes.length buf in
  if sz < 0 || first < 0 || first + sz > len then
    raise_eof ~at:first
      ~expected:(missing_before ~first ~sz ~len)
      ~got:(bytes_from (Input_end.of_bytes buf) first)

(* Absolute index of the first NUL at or after [first], searching up to (but
   not including) [limit]. Raises [Parse_error] when the region ends before a
   terminator is found -- a truncated / unterminated zero-terminated string. *)
let var_bytes_reader : type a.
    a typ ->
    (runtime -> Input_end.t -> bytes -> int -> int) ->
    (runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    a =
 fun typ off_fn size_fn runtime input_end buf base ->
  let fo = off_fn runtime input_end buf base in
  let sz = size_fn runtime input_end buf base in
  let first = base + fo in
  (match typ with
  | Byte_slice _ ->
      (* An empty byte_slice at or past the end is an eod slice. A negative size
         or offset, or a length past the end, would make [make_or_eod] raise a
         raw [Invalid_argument]: the record's bounds check does not catch the
         last, since a later span that takes the rest can take the bytes back. *)
      if sz < 0 || first < 0 then
        raise_eof ~at:first
          ~expected:(missing_before ~first ~sz ~len:(Bytes.length buf))
          ~got:(bytes_from (Input_end.of_bytes buf) first)
      else if sz > 0 then check_span_bounds buf ~first ~sz
  | _ -> check_span_bounds buf ~first ~sz);
  match typ with
  | Byte_slice _ -> Slice.make_or_eod buf ~first:(base + fo) ~length:sz
  | Byte_array _ -> Bytes.sub_string buf (base + fo) sz
  | Byte_array_where { elt_var; cond; _ } ->
      refined_span_reader ~elt_var ~cond buf ~first:(base + fo) ~len:sz
  | All_bytes -> Bytes.sub_string buf (base + fo) sz
  (* [sz] already counts the terminator (see [compile_var_size_fn]). *)
  | Zeroterm -> Bytes.sub_string buf (base + fo) (sz - 1)
  | Zeroterm_at_most _ ->
      let first = base + fo in
      let nul = zeroterm_nul_pos buf ~first ~limit:(first + sz) in
      Bytes.sub_string buf first (nul - first)
  | All_zeros -> all_zeros_reader buf ~first:(base + fo) ~len:sz
  | Casetype _ -> read_elem typ runtime input_end buf (base + fo)
  | Single_elem { elem; at_most; _ } ->
      let first = base + fo in
      (* The region hands its value [sz] bytes and no more, so the walk that
         sizes it stops there: a length field the data puts past the region is
         missing, not read from whatever follows. *)
      let region_end = Input_end.narrow input_end (first + sz) in
      let consumed = elem_size_of elem runtime region_end buf first in
      if consumed > sz || ((not at_most) && consumed <> sz) then
        raise_eof ~at:(first + min consumed sz) ~expected:sz ~got:consumed;
      read_elem elem runtime input_end buf first
  | _ -> assert false

(* The scan a span type's reader performs, without the string it hands back.
   [Codec.validate] returns unit, so running the reader for its checks alone
   turns over a copy of the span and drops it: allocation that scales with a
   length the sender chose, on the path a server runs once per packet.
   [None] for the spans whose reader only re-slices the buffer and has nothing
   to scan. *)
let span_check : type a. a typ -> (bytes -> first:int -> len:int -> unit) option
    = function
  | Byte_array_where { elt_var; cond; _ } ->
      Some
        (fun buf ~first ~len ->
          check_span_bounds buf ~first ~sz:len;
          refined_span_check ~elt_var ~cond buf ~first ~len)
  | All_zeros ->
      Some
        (fun buf ~first ~len ->
          check_span_bounds buf ~first ~sz:len;
          all_zeros_check buf ~first ~len)
  | Zeroterm_at_most _ ->
      Some
        (fun buf ~first ~len ->
          check_span_bounds buf ~first ~sz:len;
          ignore (zeroterm_nul_pos buf ~first ~limit:(first + len) : int))
  | Zeroterm ->
      (* The terminator search is the size computation, so [len] could not have
         been reached without it. Only the span itself is left to check. *)
      Some (fun buf ~first ~len -> check_span_bounds buf ~first ~sz:len)
  | Byte_array _ | Byte_slice _ | All_bytes ->
      (* Nothing to scan, since the reader only re-slices the buffer, but the
         span is still checked: a size taken from a length field can reach past
         the end, and a span that skipped the walk left the overrun to surface
         against whichever later field did have a check, blamed at the position
         the overrun had already produced. *)
      Some (fun buf ~first ~len -> check_span_bounds buf ~first ~sz:len)
  | _ -> None

(* Populate for a span field: the reader's checks, none of its copying, and no
   slot to fill (a span has no int slot). [None] leaves the field to the
   generic [build_populate]. *)
let span_populate : type a.
    a typ ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    (slots -> runtime -> Input_end.t -> bytes -> int -> unit) option =
 fun typ ~off_fn ~size_fn ->
  match span_check typ with
  | None -> None
  | Some check ->
      Some
        (fun _slots runtime input_end buf base ->
          check buf
            ~first:(base + off_fn runtime input_end buf base)
            ~len:(size_fn runtime input_end buf base))

(* Populate for a container field: the walk [elem_check] runs over what the
   container holds, in place of a reader that decodes the whole of it and drops
   the result. The region checks are the reader's own (see
   [var_bytes_reader]), so a truncated or misdeclared region still fails where
   decode fails; what is left out is the sequence, the nested region's value
   and the case body's record. [None] leaves the field to the generic
   [build_populate]. *)
let container_populate : type a.
    a typ ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    (slots -> runtime -> Input_end.t -> bytes -> int -> unit) option =
 fun typ ~off_fn ~size_fn ->
  match typ with
  | Array _ | Casetype _ ->
      let check = elem_check typ in
      Some
        (fun _slots runtime input_end buf base ->
          let first = base + off_fn runtime input_end buf base in
          check_span_bounds buf ~first ~sz:(size_fn runtime input_end buf base);
          check runtime input_end buf first)
  | Single_elem { elem; at_most; _ } ->
      let check = elem_check elem in
      Some
        (fun _slots runtime input_end buf base ->
          let first = base + off_fn runtime input_end buf base in
          let sz = size_fn runtime input_end buf base in
          check_span_bounds buf ~first ~sz;
          (* Sized against the region's own bytes, as the reader sizes it: a
             validate that answered from bytes the reader is not given would
             accept and reject on different grounds than the decode it stands
             in for. *)
          let region_end = Input_end.narrow input_end (first + sz) in
          let consumed = elem_size_of elem runtime region_end buf first in
          if consumed > sz || ((not at_most) && consumed <> sz) then
            raise_eof ~at:(first + min consumed sz) ~expected:sz ~got:consumed;
          check runtime input_end buf first)
  | _ -> None

(* What a field's validate runs in place of its reader: a span's scan, or a
   container's walk over its contents. *)
let check_populate : type a.
    a typ ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    size_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    (slots -> runtime -> Input_end.t -> bytes -> int -> unit) option =
 fun typ ~off_fn ~size_fn ->
  match span_populate typ ~off_fn ~size_fn with
  | Some p -> Some p
  | None -> container_populate typ ~off_fn ~size_fn

(* Kept at top level: as local closures they would be heap-allocated on
   every call (two allocations per variable-bytes field on each encode). *)
let var_bytes_check_len size_fn runtime buf base ~actual =
  let expected = size_fn runtime (Input_end.of_bytes buf) buf base in
  check_byte_field_size ~expected ~actual

let var_bytes_write_str buf write_off s =
  let len = String.length s in
  Bytes.blit_string s 0 buf write_off len;
  write_off + len

let var_bytes_writer : type a r.
    a typ ->
    (r -> a) ->
    (runtime -> Input_end.t -> bytes -> int -> int) ->
    r ->
    runtime ->
    bytes ->
    int ->
    int ->
    int =
 fun typ get size_fn v runtime buf base write_off ->
  let value = get v in
  match typ with
  | Byte_slice _ ->
      let src = (value : Slice.t) in
      let len = Slice.length src in
      var_bytes_check_len size_fn runtime buf base ~actual:len;
      Bytes.blit (Slice.bytes src) (Slice.first src) buf write_off len;
      write_off + len
  | Byte_array _ ->
      let s = (value : string) in
      var_bytes_check_len size_fn runtime buf base ~actual:(String.length s);
      var_bytes_write_str buf write_off s
  | Byte_array_where { elt_var; cond; _ } ->
      let s = (value : string) in
      var_bytes_check_len size_fn runtime buf base ~actual:(String.length s);
      Eval.check_byte_refinement ~elt_var ~cond s;
      var_bytes_write_str buf write_off s
  | All_bytes -> var_bytes_write_str buf write_off (value : string)
  | All_zeros ->
      let s = (value : string) in
      Types.check_all_zeros_encode s;
      var_bytes_write_str buf write_off s
  | Zeroterm ->
      let s = (value : string) in
      Types.check_zeroterm_encode s;
      let e = var_bytes_write_str buf write_off s in
      Bytes.set_uint8 buf e 0;
      e + 1
  | Zeroterm_at_most _ ->
      let s = (value : string) in
      Types.check_zeroterm_encode s;
      let region = size_fn runtime (Input_end.of_bytes buf) buf base in
      let len = String.length s in
      Types.check_zeroterm_region ~region ~len;
      Bytes.blit_string s 0 buf write_off len;
      (* Single fill covers the NUL terminator and any trailing padding. *)
      Bytes.fill buf (write_off + len) (region - len) '\x00';
      write_off + region
  | Casetype _ ->
      let encode = build_field_encoder_ctx typ in
      encode runtime buf write_off value
  | Single_elem { elem; at_most; _ } ->
      let n = size_fn runtime (Input_end.of_bytes buf) buf base in
      let actual = Types.size_of_typ_value runtime elem value in
      Types.check_nested_size ~at_most ~expected:n ~actual;
      let inner_end =
        let encode = build_field_encoder_ctx elem in
        encode runtime buf write_off value
      in
      if inner_end < write_off + n then
        Bytes.fill buf inner_end (write_off + n - inner_end) '\x00';
      write_off + n
  | _ -> assert false

let compile_var_size_fn : type a.
    layout_ctx ->
    a typ ->
    off_fn:(runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    int =
 fun ctx typ ~off_fn ->
  match typ with
  | All_bytes | All_zeros ->
      (* Trailing "rest of buffer": size is whatever bytes remain past the
         current offset. The OCaml decoder gets the buffer length via
         [Bytes.length buf]; the 3D projection emits [all_bytes], which
         3D handles natively. *)
      fun runtime input_end buf base ->
        Bytes.length buf - (base + off_fn runtime input_end buf base)
  | Casetype _ ->
      (* Tag is decoded first; the case body's size is whatever the
         selected case's inner typ reports. *)
      fun runtime input_end buf base ->
        elem_size_of typ runtime input_end buf
          (base + off_fn runtime input_end buf base)
  | Zeroterm ->
      (* Consumed bytes = string length plus the one-byte NUL terminator. *)
      fun runtime input_end buf base ->
        let first = base + off_fn runtime input_end buf base in
        zeroterm_nul_pos buf ~first ~limit:(Bytes.length buf) - first + 1
  | _ ->
      let size_expr =
        match typ with
        | Byte_slice { size } -> size
        | Byte_array { size } -> size
        | Byte_array_where { size; _ } -> size
        | Uint_var { size; _ } -> size
        | Single_elem { size; _ } -> size
        | Zeroterm_at_most { size } -> size
        | _ -> invalid_arg "add_field: unsupported variable-size field type"
      in
      let sizeof_this : runtime -> Input_end.t -> bytes -> int -> int =
        let cell = ctx.fixed_prefix in
        fun _runtime _input_end _buf _base -> !cell
      in
      compile_expr ~sizeof_this ctx.field_readers size_expr

(* Prepend [name] to the field path of a parse error escaping [f], so a failure
   deep in a nested decode accumulates its root-to-leaf path as the exception
   unwinds. Only fields that can raise an attributable error are wrapped; a plain
   scalar read raises only [Unexpected_eof] from the record's bounds check, which
   is recorded without a path. *)
let prepend_field name f runtime input_end buf base =
  try f runtime input_end buf base
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

let prepend_field_check name f arr runtime input_end buf base =
  try f arr runtime input_end buf base
  with Types.Parse_error e ->
    raise (Types.Parse_error { e with field = name :: e.field })

(* Some extents are resolved by walking the field's own bytes, and the record's
   bounds check does that walk before any field reader runs, so the failure
   surfaces from the size walk rather than from the wrapped reader: a casetype
   dispatching on an unknown tag, a zero-terminated string with no terminator.
   Name the field there too, or the failure reaches the caller with no path at
   all. Every other extent is arithmetic over fields already read and raises
   nothing of its own. *)
let attribute_size_errors : type a.
    a typ ->
    string ->
    (runtime -> Input_end.t -> bytes -> int -> int) ->
    runtime ->
    Input_end.t ->
    bytes ->
    int ->
    int =
 fun typ name size_fn ->
  match typ with
  | Casetype _ | Zeroterm -> prepend_field name size_fn
  | _ -> size_fn

let compile_var_bytes : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let off_fn = off_fn_of ctx in
  let validator_off = validator_off_of ctx in
  let size_fn = compile_var_size_fn ctx typ ~off_fn in
  let size_fn = attribute_size_errors typ fld.name size_fn in
  let field_access : field_access =
    match ctx.next_off with
    | Static n -> Variable { off = n; size_fn }
    | Dynamic _ -> Variable_dynamic { off_fn; size_fn }
  in
  let raw_reader, raw_writer, int_reader =
    match typ with
    | Uint_var { endian; _ } ->
        let get = fld.get in
        let raw_reader : runtime -> Input_end.t -> bytes -> int -> a =
         fun runtime input_end buf base ->
          let fo = off_fn runtime input_end buf base in
          let sz = size_fn runtime input_end buf base in
          let first = base + fo in
          (* This branch bypasses [var_bytes_reader]; run the same span check it
             would have run, so a [uint_var] sized past the end reports
             truncation the way a [byte_array] does instead of crashing. *)
          check_span_bounds buf ~first ~sz;
          Uint_var.read endian buf first sz
        in
        let raw_writer : r -> runtime -> bytes -> int -> int -> int =
         fun v runtime buf base write_off ->
          let sz = size_fn runtime (Input_end.of_bytes buf) buf base in
          Uint_var.write endian buf write_off sz (get v);
          write_off + sz
        in
        let int_reader runtime input_end buf base =
          int_of_typ_value typ (raw_reader runtime input_end buf base)
        in
        (raw_reader, raw_writer, int_reader)
    | _ ->
        let raw_reader = var_bytes_reader typ off_fn size_fn in
        let raw_writer = var_bytes_writer typ fld.get size_fn in
        let int_reader runtime input_end buf base =
          int_of_typ_value typ (raw_reader runtime input_end buf base)
        in
        (raw_reader, raw_writer, int_reader)
  in
  let populate =
    match check_populate typ ~off_fn ~size_fn with
    | Some p -> p
    | None -> build_populate typ ctx.n_fields raw_reader
  in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access;
    size_delta = 0;
    next_off =
      Dynamic
        (fun runtime input_end buf base ->
          let fo = off_fn runtime input_end buf base in
          base + fo + size_fn runtime input_end buf base);
    bf_after = None;
    int_reader;
    nested_readers = [];
    validator_off;
    populate;
  }

(* {2 Unions}

   A union is a casetype whose tag is an earlier field: the tag comes from that
   field's reader as an integer and the union's bytes are the selected body
   alone. Everything a case needs is staged when the codec is built, so
   decoding one is an integer lookup and a call. *)

type 'a union_case = {
  read : int -> runtime -> Input_end.t -> bytes -> int -> 'a;
      (* Takes the tag too: a default branch hands it to its [inject]. *)
  size : runtime -> Input_end.t -> bytes -> int -> int;
  check : runtime -> Input_end.t -> bytes -> int -> unit;
  write : int -> runtime -> bytes -> int -> 'a -> int;
      (* [-1] when the value is not this case's; otherwise the offset after the
         body, having checked the value's tag against the one on the wire. *)
}

(* How a tag selects its case. Small non-negative tags, the common shape of a
   type or version field, index a table; anything else is compared in turn. *)
type union_select =
  | Table of { index : int array; default : int }
  | Scan of { tags : int array; default : int }

let union_table_limit = 256

let union_select tags ~default =
  let fits t = t >= 0 && t < union_table_limit in
  if Array.for_all fits tags then begin
    let size = Array.fold_left (fun m t -> max m (t + 1)) 0 tags in
    let index = Array.make size default in
    (* Last to first, so the first case to claim a tag keeps it. *)
    for i = Array.length tags - 1 downto 0 do
      index.(tags.(i)) <- i
    done;
    Table { index; default }
  end
  else Scan { tags; default }

let rec scan_tags tags n i =
  if i = Array.length tags then -1
  else if tags.(i) = n then i
  else scan_tags tags n (i + 1)

(* The index of the case a tag selects, or [-1]. *)
let select_case select n =
  match select with
  | Table { index; default } ->
      if n >= 0 && n < Array.length index then Array.unsafe_get index n
      else default
  | Scan { tags; default } ->
      let i = scan_tags tags n 0 in
      if i < 0 then default else i

let union_tag_mismatch ~value ~field =
  Fmt.invalid_arg
    "Codec.encode: union value belongs to case %d but its tag field holds %d"
    value field

(* The default branch writes whatever tag its value carries. One an explicit
   case claims would decode as that case, with the default's body read as the
   other's, so the value is refused. *)
let union_default_claimed tag =
  Fmt.invalid_arg
    "Codec.encode: union value takes the default branch with tag %d, which a \
     case claims"
    tag

let union_case : type a k.
    k typ -> claimed:int array -> (a, k) case_branch -> a union_case =
 fun tag ~claimed (Case_branch { cb_tag; cb_inner; cb_inject; cb_project }) ->
  let encode = build_field_encoder_ctx cb_inner in
  let is_default = Option.is_none cb_tag in
  {
    read =
      (match cb_tag with
      | Some t ->
          fun _n runtime input_end buf off ->
            cb_inject t (read_elem cb_inner runtime input_end buf off)
      | None ->
          fun n runtime input_end buf off ->
            cb_inject (Types.of_int tag n)
              (read_elem cb_inner runtime input_end buf off));
    size = elem_size_of cb_inner;
    check = elem_check cb_inner;
    write =
      (fun expected runtime buf off v ->
        match cb_project v with
        | None -> -1
        | Some (t, body) ->
            let value = int_of_typ_value tag t in
            if value <> expected then union_tag_mismatch ~value ~field:expected;
            if is_default && Array.mem value claimed then
              union_default_claimed value;
            encode runtime buf off body);
  }

let rec write_union_case cases i expected runtime buf off v =
  if i = Array.length cases then Types.raise_no_matching_case ()
  else
    let off' = (Array.unsafe_get cases i).write expected runtime buf off v in
    if off' < 0 then write_union_case cases (i + 1) expected runtime buf off v
    else off'

(* A union's cases as an array and the selector into it: the explicit cases
   first, then the default if there is one, so a case's position is its tag's
   position among the tags. *)
let union_dispatch : type a k.
    k typ -> (a, k) case_branch list -> a union_case array * union_select =
 fun tag branches ->
  let explicit, defaults =
    List.partition
      (fun (Case_branch { cb_tag; _ }) -> Option.is_some cb_tag)
      branches
  in
  let tags =
    Array.of_list
      (List.filter_map
         (fun (Case_branch { cb_tag; _ }) ->
           Option.map (int_of_typ_value tag) cb_tag)
         explicit)
  in
  let default, last =
    match defaults with d :: _ -> (Array.length tags, [ d ]) | [] -> (-1, [])
  in
  let cases =
    Array.of_list (List.map (union_case tag ~claimed:tags) (explicit @ last))
  in
  (cases, union_select tags ~default)

let compile_union : type a k r.
    layout_ctx ->
    (a, r) field ->
    k typ ->
    int expr ->
    (a, k) case_branch list ->
    (a, r) compiled_field =
 fun ctx fld tag on branches ->
  let off_fn = off_fn_of ctx in
  let tag_fn = compile_expr ctx.field_readers on in
  let cases, select = union_dispatch tag branches in
  let off_abs runtime input_end buf base =
    base + off_fn runtime input_end buf base
  in
  (* An unclaimed tag is reported where the tag is, as a casetype reports its
     own: those are the bytes to change for the record to be accepted. *)
  let tag_at =
    match on with
    | Ref (_, name) -> (
        match ctx.field_at name with Some at -> at | None -> off_abs)
    | _ -> off_abs
  in
  let case_at n runtime input_end buf base =
    let i = select_case select n in
    if i < 0 then
      Types.raise_invalid_tag ~at:(tag_at runtime input_end buf base) n
    else Array.unsafe_get cases i
  in
  let size_fn runtime input_end buf base =
    let first = base + off_fn runtime input_end buf base in
    let n = tag_fn runtime input_end buf base in
    (case_at n runtime input_end buf base).size runtime input_end buf first
  in
  let size_fn = prepend_field fld.name size_fn in
  let raw_reader runtime input_end buf base =
    let first = base + off_fn runtime input_end buf base in
    let n = tag_fn runtime input_end buf base in
    (case_at n runtime input_end buf base).read n runtime input_end buf first
  in
  let get = fld.get in
  let raw_writer v runtime buf base write_off =
    let expected = tag_fn runtime (Input_end.of_bytes buf) buf base in
    write_union_case cases 0 expected runtime buf write_off (get v)
  in
  let populate _slots runtime input_end buf base =
    let first = base + off_fn runtime input_end buf base in
    let n = tag_fn runtime input_end buf base in
    let case = case_at n runtime input_end buf base in
    check_span_bounds buf ~first ~sz:(case.size runtime input_end buf first);
    case.check runtime input_end buf first
  in
  {
    raw_reader;
    raw_writer;
    extra_writers = [];
    field_access =
      (match ctx.next_off with
      | Static n -> Variable { off = n; size_fn }
      | Dynamic _ -> Variable_dynamic { off_fn; size_fn });
    size_delta = 0;
    next_off =
      Dynamic
        (fun runtime input_end buf base ->
          let fo = off_fn runtime input_end buf base in
          base + fo + size_fn runtime input_end buf base);
    bf_after = None;
    int_reader = null_int_reader;
    nested_readers = [];
    validator_off = validator_off_of ctx;
    populate;
  }

let compile_scalar_or_var : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  let typ = fld.typ in
  let field_off_static = static_off_of ctx in
  let field_off_fn = off_fn_of ctx in
  match field_wire_size typ with
  | Some fsize ->
      let field_off = match field_off_static with Some n -> n | None -> -1 in
      let raw_reader =
        match field_off_static with
        | Some _ ->
            let reader = build_field_reader_ctx typ field_off in
            fun runtime input_end buf base -> reader runtime input_end buf base
        | None ->
            let reader_at_0 = build_field_reader_ctx typ 0 in
            fun runtime input_end buf base ->
              let off = field_off_fn runtime input_end buf base in
              reader_at_0 runtime input_end buf (base + off)
      in
      let get = fld.get in
      let raw_writer : r -> runtime -> bytes -> int -> int -> int =
       fun v runtime buf _base write_off ->
        let value = get v in
        (* The encoder is applied in two steps throughout this module: called
           across a module boundary that dune's dev profile makes opaque, one
           over-saturated application allocates a curried closure per extra
           argument. *)
        let encode = build_field_encoder_ctx typ in
        encode runtime buf write_off value
      in
      let int_reader runtime input_end buf base =
        int_of_typ_value typ (raw_reader runtime input_end buf base)
      in
      let populate =
        (* A negative literal width never reaches a scan: the reader refuses it
           on sight ([const_span_reader]) and allocates nothing doing so, and
           validate has to refuse it the same way decode does. *)
        match
          if fsize < 0 then None
          else
            check_populate typ ~off_fn:field_off_fn
              ~size_fn:(fun _runtime _input_end _buf _base -> fsize)
        with
        | Some p -> p
        | None -> build_populate typ ctx.n_fields raw_reader
      in
      {
        raw_reader;
        raw_writer;
        extra_writers = [];
        field_access = fixed_or_dynamic_fa ctx;
        size_delta = fsize;
        next_off = advance_next_off ctx.next_off fsize;
        bf_after = None;
        int_reader;
        nested_readers = [];
        validator_off = field_off;
        populate;
      }
  | None -> compile_var_bytes ctx fld

(* The scan a fixed-width span inner needs in place of its reader. A gate lays
   out its inner itself rather than going through the plain field path, so
   without this a validator put back the copy {!span_populate} took out and grew
   with a length the sender chose. [None] for an inner with nothing to scan; the
   caller keeps its reader-driven populate then. *)
let inner_span_scan ctx inner fsize =
  if fsize < 0 then None
  else
    span_populate inner ~off_fn:(off_fn_of ctx)
      ~size_fn:(fun _runtime _input_end _buf _base -> fsize)

(* An absent field is not there to be checked, so its populate runs only when
   the gate says the bytes are. *)
let gated_populate gate populate arr runtime input_end buf base =
  if gate runtime input_end buf base then
    populate arr runtime input_end buf base

(* An [optional] field whose value disagrees with its presence predicate cannot
   be encoded: reject it rather than write a record decode cannot read back. *)
let optional_presence_mismatch : type a. bool -> a =
 fun present ->
  if present then
    invalid_arg
      "Codec.encode: optional field absent but presence predicate is true"
  else
    invalid_arg
      "Codec.encode: optional field present but presence predicate is false"

(* An [optional_or] whose gate is fixed absent: the field occupies no bytes,
   every read answers the default and encode writes nothing. *)
let absent_optional_or : type a r.
    layout_ctx -> a typ -> a -> (a, r) compiled_field =
 fun ctx inner default ->
  optional_compiled ctx
    ~raw_reader:(fun _runtime _input_end _buf _base -> default)
    ~int_reader:(fun _runtime _input_end _buf _base ->
      int_of_typ_value inner default)
    ~raw_writer:(fun _v _runtime _buf _base write_off -> write_off)
    ~size_delta:0 ~next_off:ctx.next_off ~populate:no_populate ()

let rec compile_field : type a r.
    layout_ctx -> (a, r) field -> (a, r) compiled_field =
 fun ctx fld ->
  match fld.typ with
  | Map { inner; decode; encode; index_bound } ->
      compile_map ctx fld inner decode encode ~index_bound
  | Enum { name; base; cases; closed } ->
      (* Compile as the base integer, then validate the decoded value is one of
         the named cases. [compile_field] would otherwise strip the cases and
         accept any base value, unlike the EverParse validator. *)
      let cf = compile_field ctx { fld with typ = base } in
      let check =
        enum_check ~base ~to_int:(Types.int_of_exn base) cases closed
      in
      (* [populate] reads the field's integer slot rather than its value, so it
         checks membership on the integer the slot already holds. *)
      let int_check = enum_check ~base ~to_int:Fun.id cases closed in
      let get = fld.get in
      let encode_check =
        enum_encode_check ~name ~cases ~closed ~to_int:(Types.int_of_exn base)
      in
      {
        cf with
        raw_reader =
          (fun runtime input_end buf off ->
            check
              ~at:(at_of off cf.validator_off)
              (cf.raw_reader runtime input_end buf off));
        raw_writer =
          (fun v runtime buf base write_off ->
            encode_check (get v);
            cf.raw_writer v runtime buf base write_off);
        populate =
          (fun arr runtime input_end buf base ->
            cf.populate arr runtime input_end buf base;
            ignore
              (int_check
                 ~at:(at_of base cf.validator_off)
                 (cf.int_reader runtime input_end buf base)));
      }
  | Where { inner; _ } -> compile_field ctx { fld with typ = inner }
  | Bits { width; base; bit_order } -> compile_bits ctx fld width base bit_order
  | Codec
      {
        codec_decode;
        codec_validate;
        codec_encode;
        codec_fixed_size;
        codec_size_of;
        codec_field_readers;
        _;
      } ->
      compile_codec ctx fld ~codec_decode ~codec_validate ~codec_encode
        ~codec_fixed_size ~codec_size_of ~codec_field_readers
  | Optional { present; inner } -> compile_optional ctx fld present inner
  | Optional_or { present; inner; default } ->
      compile_optional_or ctx fld present inner default
  | Repeat { size; elem; seq } -> compile_repeat ctx fld size elem seq
  | Union { tag; on; cases; _ } -> compile_union ctx fld tag on cases
  | _ -> compile_scalar_or_var ctx fld

and compile_map : type a w r.
    layout_ctx ->
    (a, r) field ->
    w typ ->
    (w -> a) ->
    (a -> w) ->
    index_bound:int option ->
    (a, r) compiled_field =
 fun ctx fld inner decode encode ~index_bound ->
  let outer_get = fld.get in
  let inner_fld =
    {
      id = fld.id;
      name = fld.name;
      typ = inner;
      constraint_ = fld.constraint_;
      action = fld.action;
      doc = fld.doc;
      get = (fun v -> encode (outer_get v));
    }
  in
  let cf = compile_field ctx inner_fld in
  let decode = Types.map_decode ~index_bound decode in
  let checked_reader runtime input_end buf off =
    decode
      ~at:(at_of off cf.validator_off)
      (cf.raw_reader runtime input_end buf off)
  in
  {
    cf with
    raw_reader = checked_reader;
    (* [decode] is where a lookup checks its index is in range; run it in
       populate too so [Codec.validate] enforces the bound decode does. The inner
       populate still fills the int slot from the raw value. *)
    populate =
      (fun arr runtime input_end buf base ->
        cf.populate arr runtime input_end buf base;
        ignore (checked_reader runtime input_end buf base));
  }

and compile_optional : type a r.
    layout_ctx ->
    (a option, r) field ->
    bool expr ->
    a typ ->
    (a option, r) compiled_field =
 fun ctx fld present inner ->
  let inner_size = field_wire_size inner in
  let nfields = ctx.n_fields in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate =
        match inner_span_scan ctx inner fsize with
        | Some scan -> scan
        | None -> build_populate inner nfields inner_reader
      in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun runtime input_end buf base ->
          Some (inner_reader runtime input_end buf base))
        ~raw_writer:(fun v runtime buf _base write_off ->
          match get v with
          | Some iv -> inner_writer runtime buf write_off iv
          | None -> write_off + fsize)
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate:inner_populate ()
  | Bool false, _ ->
      optional_compiled ctx
        ~raw_reader:(fun _runtime _input_end _buf _base -> None)
        ~raw_writer:(fun _v _runtime _buf _base write_off -> write_off)
        ~size_delta:0 ~next_off:ctx.next_off ~populate:no_populate ()
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.field_readers present in
      let readable = present_in_bounds ctx present_fn fsize in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let inner_populate =
        match inner_span_scan ctx inner fsize with
        | Some scan -> scan
        | None -> build_populate inner nfields inner_reader
      in
      let get = fld.get in
      optional_compiled ctx
        ~raw_reader:(fun runtime input_end buf base ->
          if readable runtime input_end buf base then
            Some (inner_reader runtime input_end buf base)
          else None)
        ~raw_writer:(fun v runtime buf base write_off ->
          match
            (present_fn runtime (Input_end.of_bytes buf) buf base, get v)
          with
          | true, Some iv -> inner_writer runtime buf write_off iv
          | false, None -> write_off
          | present, _ -> optional_presence_mismatch present)
        ~size_delta:0
        ~next_off:(dynamic_optional_next_off ctx present_fn fsize)
        ~populate:(gated_populate readable inner_populate)
        ()
  | _ -> compile_optional_variable ctx fld present inner

(* Variable-size inner: compile it as its own field and gate that compiled plan
   on [present]. This reuses every per-type path (outer-sized byte slices,
   self-delimiting sub-codecs, casetypes), so the inner decodes and encodes
   exactly as it would unwrapped; an absent gate advances no bytes. *)
and compile_optional_variable : type a r.
    layout_ctx ->
    (a option, r) field ->
    bool expr ->
    a typ ->
    (a option, r) compiled_field =
 fun ctx fld present inner ->
  let present_fn = compile_bool_expr ctx.field_readers present in
  let get = fld.get in
  let inner_fld =
    {
      id = fld.id;
      name = fld.name;
      typ = inner;
      constraint_ = None;
      action = None;
      doc = fld.doc;
      get =
        (fun v ->
          match get v with
          | Some iv -> iv
          | None -> optional_presence_mismatch true);
    }
  in
  let cf = compile_field ctx inner_fld in
  optional_compiled ctx
    ~raw_reader:(fun runtime input_end buf base ->
      if present_fn runtime input_end buf base then
        Some (cf.raw_reader runtime input_end buf base)
      else None)
    ~raw_writer:(fun v runtime buf base write_off ->
      match (present_fn runtime (Input_end.of_bytes buf) buf base, get v) with
      | true, Some _ -> cf.raw_writer v runtime buf base write_off
      | false, None -> write_off
      | present, _ -> optional_presence_mismatch present)
    ~size_delta:0
    ~next_off:
      (Dynamic
         (fun runtime input_end buf base ->
           if present_fn runtime input_end buf base then
             next_off_pos cf.next_off runtime input_end buf base
           else next_off_pos ctx.next_off runtime input_end buf base))
    ~populate:(gated_populate present_fn cf.populate)
    ()

and compile_optional_or : type a r.
    layout_ctx ->
    (a, r) field ->
    bool expr ->
    a typ ->
    a ->
    (a, r) compiled_field =
 fun ctx fld present inner default ->
  let inner_size = field_wire_size inner in
  match (present, inner_size) with
  | Bool true, Some fsize ->
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let populate =
        match inner_span_scan ctx inner fsize with
        | Some scan -> scan
        | None -> build_populate fld.typ ctx.n_fields inner_reader
      in
      optional_compiled ctx ~raw_reader:inner_reader
        ~int_reader:(fun runtime input_end buf base ->
          int_of_typ_value inner (inner_reader runtime input_end buf base))
        ~raw_writer:(fun v runtime buf _base write_off ->
          inner_writer runtime buf write_off (get v))
        ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate ()
  | Bool false, _ -> absent_optional_or ctx inner default
  | _, Some fsize ->
      let present_fn = compile_bool_expr ctx.field_readers present in
      let readable = present_in_bounds ctx present_fn fsize in
      let inner_reader, inner_writer = inner_codec_accessors inner ctx in
      let get = fld.get in
      let raw_reader runtime input_end buf base =
        if readable runtime input_end buf base then
          inner_reader runtime input_end buf base
        else default
      in
      let populate =
        match inner_span_scan ctx inner fsize with
        (* The reader the generic populate would run answers [default] for an
           absent gate, so a scan standing in for it takes the same gate. *)
        | Some scan -> gated_populate readable scan
        | None -> build_populate fld.typ ctx.n_fields raw_reader
      in
      (* Encode is value-driven: the user always has a value, so always
         write [fsize] bytes. The gate is the decode-side oracle that
         chooses between the decoded inner and [default]. Round-trips
         exactly when the gate is set consistently with the user's
         choice; if the user sets the gate to [absent] while the value
         differs from [default], decoding loses the value and falls
         back to [default]. *)
      let raw_writer v runtime buf _base write_off =
        inner_writer runtime buf write_off (get v)
      in
      optional_compiled ctx ~raw_reader
        ~int_reader:(fun runtime input_end buf base ->
          int_of_typ_value inner (raw_reader runtime input_end buf base))
        ~raw_writer ~size_delta:fsize
        ~next_off:(advance_next_off ctx.next_off fsize)
        ~populate ()
  | _ -> compile_optional_or_variable ctx fld present inner default

(* Variable-size inner. As in the fixed case [optional_or] is value-driven and
   always occupies the inner's bytes; the gate only chooses the decoded inner
   vs [default] on the read side, so the layout always advances past it. *)
and compile_optional_or_variable : type a r.
    layout_ctx ->
    (a, r) field ->
    bool expr ->
    a typ ->
    a ->
    (a, r) compiled_field =
 fun ctx fld present inner default ->
  let present_fn = compile_bool_expr ctx.field_readers present in
  let inner_fld =
    {
      id = fld.id;
      name = fld.name;
      typ = inner;
      constraint_ = None;
      action = None;
      doc = fld.doc;
      get = fld.get;
    }
  in
  let cf = compile_field ctx inner_fld in
  optional_compiled ctx
    ~raw_reader:(fun runtime input_end buf base ->
      if present_fn runtime input_end buf base then
        cf.raw_reader runtime input_end buf base
      else default)
    ~raw_writer:(fun v runtime buf base write_off ->
      cf.raw_writer v runtime buf base write_off)
    ~int_reader:cf.int_reader ~size_delta:0 ~next_off:cf.next_off
    ~populate:cf.populate ()
