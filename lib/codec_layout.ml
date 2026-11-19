(* The layout pass of a variable-size record without parameters: one walk
   over its fields that measures it and checks what it can, and accepts or
   leaves the record to the general passes. *)

open Types
open Codec_repr

type item =
  | Fix of int
  | Span of (bytes -> int -> unit -> unit -> int)
  | Rest
  | Opt of (bytes -> int -> unit -> unit -> bool) * item
  | Region of {
      size : bytes -> int -> unit -> unit -> int;
      at_most : bool;
      sub : sub;
    }
  | Sub of sub

(* Where a span of [size] bytes at [cur] ends, or [-1] past [len]. *)
let span_end size buf off len cur =
  let sz = size buf off () () in
  if sz >= 0 && cur <= len - sz then cur + sz else -1

(* The rest of the buffer, as the general extent measures it, which must be
   within [len]. *)
let rest_end buf len cur =
  let e = Bytes.length buf in
  if cur <= e && e <= len then e else -1

(* Whether a sub-codec of [n] bytes fits a region of [sz]. *)
let fits ~sz ~at_most (n : int) = n <= sz && (at_most || n = sz)

(* A sub-codec with no walk of its own, measured against the region [region]
   of [sz] bytes, and checked when it fits. *)
let region_measure (sub : sub) ~sz ~at_most region input_end buf cur =
  let n = sub.size_of no_runtime region buf cur in
  if fits ~sz ~at_most n then
    sub.validate_sized no_runtime input_end buf cur (cur + n);
  n

(* A sub-codec in a region of [size] bytes at [cur], measured against the
   region's end, which it must not pass, nor fall short of unless [at_most];
   the region ends where the walk goes on. *)
let region_end ~size ~at_most ~(sub : sub) input_end buf off len cur =
  let sz = size buf off () () in
  if sz < 0 || cur > len - sz then -1
  else
    let region = Input_end.narrow input_end (cur + sz) in
    let e = sub.check_end region buf cur in
    if e = -1 then -1
    else
      let n =
        if e >= 0 then e - cur
        else region_measure sub ~sz ~at_most region input_end buf cur
      in
      if fits ~sz ~at_most n then cur + sz else -1

(* Where a sub-codec with no walk of its own ends, measuring itself, checked
   when it ends within [len]. *)
let sub_measure (sub : sub) input_end buf len cur =
  let n = sub.size_of no_runtime input_end buf cur in
  if n >= 0 && cur <= len - n then
    sub.validate_sized no_runtime input_end buf cur (cur + n);
  cur + n

(* A sub-codec at [cur], measuring itself. *)
let sub_end (sub : sub) input_end buf len cur =
  let e = sub.check_end input_end buf cur in
  if e = -1 then -1
  else
    let e = if e >= 0 then e else sub_measure sub input_end buf len cur in
    if e >= cur && e <= len then e else -1

(* Where [item] at [cur] ends, or [-1] when the walk does not accept it. *)
let rec item_end item input_end buf off len cur =
  match item with
  | Fix k -> cur + k
  | Span size -> span_end size buf off len cur
  | Rest -> rest_end buf len cur
  | Opt (present, item) ->
      if present buf off () () then item_end item input_end buf off len cur
      else cur
  | Region { size; at_most; sub } ->
      region_end ~size ~at_most ~sub input_end buf off len cur
  | Sub sub -> sub_end sub input_end buf len cur

(* The walk of the layout pass: [cur] is where the next field starts, and the
   answer is where the record ends, or [-1] when the walk does not accept. A
   sub-codec is measured and checked as the check pass does, on the bytes the
   same checks give it: in a region, measured against the region's end; else
   measuring itself. Its own walk answers both at once ([check_end]); when that
   walk does not accept, neither does this one, which leaves the sub-codec to
   the general passes, to be checked once; a sub-codec with no walk is
   measured and checked here. Anything they raise rejects. *)
let rec layout_walk items input_end buf off len cur =
  match items with
  | [] -> if cur <= len then cur else -1
  | item :: rest ->
      let next = item_end item input_end buf off len cur in
      if next < 0 then -1 else layout_walk rest input_end buf off len next

(* A span's size: a field, or a field times, plus or minus a literal, is one
   closure that reads the field; any other expression goes through
   [Expr_compiler]. The arithmetic is checked, as [decode]'s is: a product
   that wraps on a 31-bit int is a size [decode] refuses. *)
let span_size prefix leaves (e : int expr) =
  let field name =
    match List.assoc_opt name prefix with
    | Some (Ps s) -> Some (fun buf off -> read_slot s buf off)
    | None -> None
  in
  let general () = Expr_compiler.compile_int leaves e in
  match e with
  | Ref (I, a) -> (
      match field a with
      | Some r -> fun buf off () () -> r buf off
      | None -> general ())
  | Mul (Ref (I, a), Int k) | Mul (Int k, Ref (I, a)) -> (
      match field a with
      | Some r when k >= 0 && k <= 0xffff ->
          fun buf off () () -> checked_mul (r buf off) k
      | _ -> general ())
  | Add (Ref (I, a), Int k) | Add (Int k, Ref (I, a)) -> (
      match field a with
      | Some r when abs k <= 0xffff ->
          fun buf off () () -> checked_add (r buf off) k
      | _ -> general ())
  | Sub (Ref (I, a), Int k) -> (
      match field a with
      | Some r when abs k <= 0xffff ->
          fun buf off () () -> checked_sub (r buf off) k
      | _ -> general ())
  | _ -> general ()

(* The layout items of a variable-size record without parameters whose
   variable fields are byte spans, gated fields and sub-codecs sized by earlier
   fixed-offset fields, in field order, adjacent widths merged; [None] when a
   field is outside that shape. *)
let layout_items prefix layout =
  let unplanned _ = raise Exit in
  let leaves : (bytes, int, unit, unit) Expr_compiler.leaves =
    {
      ref_ =
        (fun name ->
          match List.assoc_opt name prefix with
          | Some (Ps s) -> fun buf off () () -> read_slot s buf off
          | None -> raise Exit);
      i64 = unplanned;
      param_ref = unplanned;
      sizeof_typ =
        (fun (Pack_typ t) ->
          let n = Option.value ~default:0 (field_wire_size t) in
          fun _ _ () () -> n);
      sizeof_this = (fun _ _ () () -> raise Exit);
      field_pos = (fun _ _ () () -> raise Exit);
    }
  in
  let rec item = function
    | Lfix k -> Fix k
    | Lspan e -> Span (span_size prefix leaves e)
    | Lrest -> Rest
    | Lopt (present, i) ->
        Opt (Expr_compiler.compile_bool leaves present, item i)
    | Lregion { size; at_most; sub } ->
        Region { size = span_size prefix leaves size; at_most; sub }
    | Lsub sub -> Sub sub
  in
  let merge item items =
    match (item, items) with
    | Fix a, Fix b :: rest -> Fix (a + b) :: rest
    | _ -> item :: items
  in
  match layout with
  | None -> None
  | Some layout -> (
      match List.fold_left (fun items l -> merge (item l) items) [] layout with
      | exception Exit -> None
      | items -> Some items)

(* The layout pass: one walk that reads each length field once and checks each
   span against the end of input it is given, the buffer's or a nested
   record's region, and answers where the record ends. It only ever accepts,
   with an end; anything it does not accept ([-1]) goes to the general bounds
   check, which raises the error it always raised. *)
let build_layout ~min_size items =
  Option.map
    (fun items input_end buf off ->
      let len = Input_end.to_int input_end in
      if off < 0 || off > len - min_size then -1
      else
        try layout_walk items input_end buf off len off
        with Types.Parse_error _ | Exit -> -1)
    items

(* The extent walk: where the record ends, as the general extent computes it,
   from the same fields and sub-codecs: a region adds its size, a sub-codec its
   own extent. *)
let rec layout_end items input_end buf off cur =
  match items with
  | [] -> cur
  | Fix k :: rest -> layout_end rest input_end buf off (cur + k)
  | Span size :: rest ->
      layout_end rest input_end buf off (checked_add cur (size buf off () ()))
  | Rest :: rest -> layout_end rest input_end buf off (Bytes.length buf)
  | Opt (present, item) :: rest ->
      if present buf off () () then
        layout_end (item :: rest) input_end buf off cur
      else layout_end rest input_end buf off cur
  | Region { size; _ } :: rest ->
      layout_end rest input_end buf off (checked_add cur (size buf off () ()))
  | Sub sub :: rest ->
      layout_end rest input_end buf off
        (checked_add cur (sub.size_of no_runtime input_end buf cur))

(* The record's extent through the extent walk, when its length fields are in
   the bytes and it answers; anything else is the general extent's to answer,
   value or error, so either way the answer is the one it always was. *)
let layout_extent ~min_size items compute runtime input_end buf off =
  let len = Input_end.to_int input_end in
  if is_extent_fallback runtime || off < 0 || off > len - min_size then
    compute runtime input_end buf off
  else
    match layout_end items input_end buf off off with
    | e -> e
    | exception (Types.Parse_error _ | Exit | Invalid_argument _) ->
        (* The walk has been tried: the general extent does not try the walks
           of the records under it again. *)
        compute extent_fallback_runtime input_end buf off

type items = item list
