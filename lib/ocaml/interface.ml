open Wire.Private
open Layout
module T = Types
open Planner
open Record_module

(* {1 The interfaces}

   The interface of a generated module states its public API and nothing
   else: the types and values a caller uses, with the modes and the
   allocation checks the OxCaml target gives them, each documented from the
   codec. It is written from the plan the implementation is written from, so
   the two cannot disagree on what a module holds. *)

type target = [ `OCaml | `OxCaml ]

let rt = Emitter.rt
let parse_error = rt ^ ".Error.t"

(* The buffer a function takes: its type, the runtime module that names it,
   and whether the function takes it [local]. *)
type buf = { typ : string; brand : string; local : bool }

let bytes_buf target =
  { typ = "bytes"; brand = rt ^ ".Buf"; local = target = `OxCaml }

let local_mode local = if local then " @ local" else ""
let buf b = b.typ ^ local_mode b.local
let named b = Fmt.str "'b %s.t%s" b.brand (local_mode b.local)
let arrows ts = String.concat " -> " ts

(* [val name : typ], with its doc, and on OxCaml the allocation check and the
   portability of the implementation: a value is [portable] unless it calls
   the caller's code, a constructor, a map or a step module, whose
   portability the generator does not know. *)
let pp_val ?(zero_alloc = false) ?(portable = true) target ppf name typ doc =
  let ox = target = `OxCaml in
  Fmt.pf ppf "val%s %s : %s%s@\n(** %s *)@\n@\n"
    (if ox && zero_alloc then "[@zero_alloc]" else "")
    name typ
    (if ox && portable then " @@ portable" else "")
    doc

let pp_doc ppf doc = Fmt.pf ppf "(** %s *)@\n" doc

let pp_section ppf anchor title =
  Fmt.pf ppf "(** {1:%s %s} *)@\n@\n" anchor title

(* {2 What the docs say of a field} *)

let endian = function T.Big -> "big-endian" | T.Little -> "little-endian"
let sign signed = if signed then "signed" else "unsigned"

let word = function
  | T.U8 -> "the byte"
  | T.U16 e -> Fmt.str "the %s 16-bit word" (endian e)
  | T.U32 e -> Fmt.str "the %s 32-bit word" (endian e)

let where f =
  match f.store with
  | Int { width = 8; signed; _ } ->
      Fmt.str "%s 8-bit integer at offset %d" (sign signed) f.offset
  | Int { width; endian = e; signed } ->
      Fmt.str "%s %s %d-bit integer at offset %d" (endian e) (sign signed) width
        f.offset
  | Bits { base; shift; width = 1 } ->
      Fmt.str "bit %d of %s at offset %d" shift (word base) f.offset
  | Bits { base; shift; width } ->
      Fmt.str "bits %d to %d of %s at offset %d" shift
        (shift + width - 1)
        (word base) f.offset
  | Bytes n -> Fmt.str "%d bytes at offset %d" n f.offset

(* An expression of the codec in OCaml syntax, each variable under the name
   [name] gives it. *)
let rec pp_expr : type a. (string -> string) -> a T.expr Fmt.t =
 fun name ppf e ->
  let operand : type b. b T.expr Fmt.t =
   fun ppf e ->
    match e with
    | T.Int n when n >= 0 -> pp_expr name ppf e
    | T.Int64 _ | T.Bool _ | T.Ref _ | T.Param_ref _ | T.Sizeof _
    | T.Sizeof_this | T.Field_pos ->
        pp_expr name ppf e
    | _ -> Fmt.pf ppf "(%a)" (pp_expr name) e
  in
  let infix : type b c. b T.expr -> string -> c T.expr -> unit =
   fun a op b -> Fmt.pf ppf "%a %s %a" operand a op operand b
  in
  match e with
  | T.Int n when n < 0 -> Fmt.pf ppf "(%d)" n
  | T.Int n -> Fmt.int ppf n
  | T.Int64 n -> Fmt.pf ppf "%LdL" n
  | T.Bool b -> Fmt.bool ppf b
  | T.Ref (_, v) -> Fmt.string ppf (name v)
  | T.Param_ref p -> Fmt.string ppf p.name
  | T.Sizeof t -> (
      match T.field_wire_size t with
      | Some n -> Fmt.int ppf n
      | None -> Fmt.string ppf "size")
  | T.Sizeof_this -> Fmt.string ppf "size"
  | T.Field_pos -> Fmt.string ppf "offset"
  | T.Add (a, b) -> infix a "+" b
  | T.Sub (a, b) -> infix a "-" b
  | T.Mul (a, b) -> infix a "*" b
  | T.Div (a, b) -> infix a "/" b
  | T.Mod (a, b) -> infix a "mod" b
  | T.Land (a, b) -> infix a "land" b
  | T.Land64 (a, b) -> infix a "land" b
  | T.Lor (a, b) -> infix a "lor" b
  | T.Lxor (a, b) -> infix a "lxor" b
  | T.Lnot a -> Fmt.pf ppf "lnot %a" operand a
  | T.Lsl (a, b) -> infix a "lsl" b
  | T.Lsr (a, b) -> infix a "lsr" b
  | T.Lsr64 (a, b) -> infix a "lsr" b
  | T.Eq (a, b) -> infix a "=" b
  | T.Ne (a, b) -> infix a "<>" b
  | T.Lt (a, b) -> infix a "<" b
  | T.Le (a, b) -> infix a "<=" b
  | T.Gt (a, b) -> infix a ">" b
  | T.Ge (a, b) -> infix a ">=" b
  | T.And (a, b) -> infix a "&&" b
  | T.Or (a, b) -> infix a "||" b
  | T.Not a -> Fmt.pf ppf "not %a" operand a
  | T.Cast (`U8, a) -> Fmt.pf ppf "%a land 0xff" operand a
  | T.Cast (`U16, a) -> Fmt.pf ppf "%a land 0xffff" operand a
  | T.Cast (`U32, a) -> Fmt.pf ppf "%a land 0xffffffff" operand a
  | T.Cast (`U64, a) -> pp_expr name ppf a
  | T.If_then_else (c, a, b) ->
      Fmt.pf ppf "if %a then %a else %a" (pp_expr name) c operand a operand b

(* The OCaml label of each plan variable. *)
let namer fields =
  let labels = List.map (fun f -> (f.id, f.label)) fields in
  fun v -> Option.value ~default:v (List.assoc_opt v labels)

let or_list = function
  | [] -> ""
  | [ x ] -> x
  | xs ->
      let rev = List.rev xs in
      String.concat ", " (List.rev (List.tl rev)) ^ " or " ^ List.hd rev

(* What a field holds: where it sits, then what the codec checks of it. *)
let describe ~name ~maps f =
  let checks =
    List.filter_map
      (fun c ->
        match c.kind with
        | Enum _ -> None
        | Constraint | Tag -> Fmt.kstr Option.some "[%a]" (pp_expr name) c.cond)
      f.checks
  in
  let enum =
    match f.enum with
    | Some (_, valid) ->
        [ "one of " ^ or_list (List.map (fun v -> Fmt.str "[%d]" v) valid) ]
    | None -> []
  in
  let map =
    match f.map with
    | Some path ->
        [ Fmt.str "read through [%s] as a [%s]" path (List.assoc path maps) ]
    | None -> []
  in
  let clauses =
    (match checks with
      | [] -> []
      | cs -> [ "where " ^ String.concat " and " cs ])
    @ enum @ map
  in
  Fmt.str "{- [%s], %s%s.}" f.label (where f)
    (String.concat "" (List.map (( ^ ) ", ") clauses))

(* The arguments of a function taking one value per field, as its doc
   brackets them. *)
let args fields = String.concat "" (List.map (fun f -> " " ^ f.label) fields)

let labelled_args fields =
  String.concat "" (List.map (fun f -> " ~" ^ f.label) fields)

(* {2 Field types} *)

(* The type of a field's typed value: its map's, or its carrier's. *)
let typed ~maps f =
  match f.map with Some path -> List.assoc path maps | None -> type_of f

let no_map fields = List.for_all (fun f -> Option.is_none f.map) fields

(* {2 The record module} *)

type record = {
  target : target;
  codec : string;
  plan : plan;
  size : int;
  fields : field list;
  maps : (string * string) list;
  name : string -> string;
  simd : bool;
}

let decoded _ = "t"

(* What a read of [f] returns, from the record's start. *)
let read_what f =
  match f.map with
  | Some path -> Fmt.str "%s, read through [%s]" (where f) path
  | None -> where f

let pp_record_type ppf r =
  pp_section ppf "records" "Records";
  Fmt.pf ppf "type t = {@\n";
  List.iter
    (fun f -> Fmt.pf ppf "  %s : %s;@\n" f.label (typed ~maps:r.maps f))
    r.fields;
  Fmt.pf ppf "}@\n";
  pp_doc ppf (Fmt.str "The type for %s records." r.plan.record);
  Fmt.pf ppf "@\n";
  pp_val r.target ppf "pp" "Format.formatter -> t -> unit"
    "[pp ppf r] prints an unspecified representation of [r] on [ppf]."

(* [validate] and [decode], on the buffer [b]. *)
let pp_checks ppf r b =
  let target = r.target in
  let t = decoded r in
  pp_val target ppf "validate"
    (arrows [ buf b; "off:int"; Fmt.str "(unit, %s) result" parse_error ])
    "[validate buf ~off] is [Ok ()] if the record at [off] in [buf] passes the \
     codec's checks, and the codec's error otherwise. Raises \
     [Invalid_argument] if [off] is negative.";
  let portable = no_map r.fields in
  pp_val ~portable target ppf "decode"
    (arrows [ buf b; "off:int"; Fmt.str "(%s, %s) result" t parse_error ])
    "[decode buf ~off] is the record at [off] in [buf], or the codec's error. \
     Raises [Invalid_argument] if [off] is negative."

let pp_writes ppf r b =
  let target = r.target in
  let checked = no_map r.fields in
  let a = labelled_args r.fields in
  let label f t = f.label ^ ":" ^ t in
  let typed = List.map (fun f -> label f (typed ~maps:r.maps f)) r.fields in
  let ranged =
    if
      List.exists
        (fun f ->
          match (f.map, f.store) with
          | None, Int _ -> Option.is_some f.enum
          | _ -> true)
        r.fields
    then ", if a value does not fit its field"
    else ""
  in
  let rejected =
    if has_checks r.plan then ", or if the record breaks a constraint" else ""
  in
  pp_val ~portable:checked target ppf "encode"
    (arrows [ "t"; buf b; "off:int"; "unit" ])
    "[encode r buf ~off] writes the record [r] at [off] in [buf]. Raises as \
     {!write} does.";
  pp_val ~zero_alloc:checked ~portable:checked target ppf "write"
    (arrows ([ buf b; "off:int" ] @ typed @ [ "unit" ]))
    (Fmt.str
       "[write buf ~off%s] writes the record of these fields at [off] in \
        [buf]. Raises [Invalid_argument] if it does not fit%s%s."
       a ranged rejected)

(* The views of a module with no walk: [check], [is_ok], [get] and the
   accessor [accessor f] of each field [f]. *)
let pp_views ppf r ~accessor =
  let target = r.target in
  let b = bytes_buf target in
  Fmt.pf ppf
    "type 'b check = private int@\n\
     (** The type for checks of a record of [%s] in the buffer named ['b]. *)@\n\
     @\n\
     type 'b view = private int@\n\
     (** The type for records of [%s] that passed their check in the buffer \
     named ['b]. *)@\n\
     @\n"
    r.codec r.codec;
  pp_val target ppf "check"
    (arrows [ named b; Fmt.str "'b %s.length" b.brand; "off:int"; "'b check" ])
    (Fmt.str
       "[check b length ~off] is the check of the record at [off], whose %d \
        bytes must lie in [b], where [length] is [Wire.Buf.length b], read \
        once per buffer. It never raises and allocates nothing: a negative \
        [off] or a record [b] does not hold is refused."
       r.size);
  pp_val target ppf "is_ok" "'b check -> bool"
    "[is_ok k] is [true] iff the check [k] passed.";
  pp_val target ppf "get" "'b check -> 'b view"
    "[get k] is the view of the record the check [k] passed. Raises \
     [Invalid_argument] if [is_ok k] is [false]: a refused record is input, \
     test {!is_ok} first.";
  List.iter
    (fun f ->
      let l = accessor f in
      pp_val ~portable:(Option.is_none f.map) target ppf l
        (arrows [ named b; "'b view"; typed ~maps:r.maps f ])
        (Fmt.str "[%s b v] is %s of the record [v]." l (read_what f)))
    r.fields

let pp_x4_writes ppf r lanes =
  let target = r.target in
  let b = bytes_buf r.target in
  let v = rt ^ ".Vec.t" in
  pp_val target ppf "store"
    (arrows [ named b; "off:int"; "part"; "unit" ])
    "[store b ~off p] writes the four records of [p] at [off] in [b]. Raises \
     [Invalid_argument] for the first record that does not fit, once the ones \
     before it are written.";
  pp_val target ppf "fill"
    (arrows
       [ named b; "off:int"; "count:int"; "f:(int -> part) @ local"; "unit" ])
    "[fill b ~off ~count ~f] stores [f k] at [off + (16 * k)] for each [k] \
     from [0] to [count - 1].";
  pp_val target ppf "write_int_at"
    (arrows ([ named b; "off:int" ] @ List.map (fun _ -> v) lanes @ [ "unit" ]))
    (Fmt.str
       "[write_int_at b ~off%s] writes the four records whose fields are in \
        the lanes of these, in field order. Raises [Invalid_argument] for the \
        first record that is out of range or does not fit."
       (args (List.map fst lanes)))

(* The SIMD lanes, as [Record_module.pp_x4_sig] states them. *)
let pp_x4 ppf r lanes =
  let target = r.target in
  let b = bytes_buf r.target in
  let v = rt ^ ".Vec.t" in
  pp_section ppf "lanes" "Four records at once";
  pp_doc ppf
    "Four records in the 32-bit lanes of a SIMD register. Every field is \
     unsigned with no check, so any bits in place are four valid records.";
  Fmt.pf ppf "module X4 : sig@\ntype part@\n";
  pp_doc ppf "The type for field bits in place in each lane, each in range.";
  Fmt.pf ppf "@\n";
  pp_val target ppf "with_"
    (arrows
       [
         named b;
         "off:int";
         "ok:(" ^ v ^ " -> 'a) @ local";
         "fail:(unit -> 'a) @ local";
         "'a";
       ])
    "[with_ b ~off ~ok ~fail] is [ok w] for the 16 bytes [w] at [off] in [b] \
     if they fit, and [fail ()] otherwise.";
  List.iter
    (fun (f, _) ->
      let l = f.label in
      pp_val target ppf l
        (arrows [ v; v ])
        (Fmt.str "[%s w] is %s of the record in each lane of [w]." l (where f));
      pp_val target ppf (l ^ "_bits")
        (arrows [ v; "part" ])
        (Fmt.str "[%s_bits w] is the low bits of each lane of [w], in place." l);
      pp_val target ppf (l ^ "_of")
        (arrows [ v; "part" ])
        (Fmt.str
           "[%s_of w] is like {!%s_bits} but raises [Invalid_argument] if a \
            lane does not fit the field."
           l l))
    lanes;
  pp_val target ppf "zero" "part" "[zero] is every field [0] in each lane.";
  pp_val target ppf "( ++ )" "part -> part -> part"
    "[p ++ q] is the fields of [p] and of [q].";
  pp_x4_writes ppf r lanes;
  Fmt.pf ppf "end@\n@\n"

let pp_buffer ?accessor ppf r b =
  pp_section ppf "checks" "Validating and decoding";
  pp_checks ppf r b;
  pp_section ppf "writes" "Writing";
  pp_writes ppf r b;
  Option.iter
    (fun accessor ->
      pp_section ppf "views" "Views";
      pp_views ppf r ~accessor)
    accessor

let pp_preamble ppf r =
  let bits =
    List.exists
      (fun f -> match f.store with Bits _ -> true | _ -> false)
      r.fields
  in
  let wheres =
    List.filter_map (function Test e -> Some e | _ -> None) r.plan.nodes
  in
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from the codec %s. Do not edit: generate it \
     again when the codec changes. *)@\n\
     @\n"
    r.codec;
  Fmt.pf ppf
    "(** %s records.@\n\
     @\n\
     The records of [%s], checked, decoded and encoded as the codec does, with \
     its errors, by code that links [wire.runtime] only%s. A record is %d \
     bytes; its fields, in wire order%s:@\n\
     {ul@\n\
     %s}@\n"
    r.plan.record r.codec
    (if r.maps = [] then "" else " and the maps of its fields")
    r.size
    (if bits then ", bits numbered from 0 the least significant" else "")
    (String.concat "\n"
       (List.map (describe ~name:r.name ~maps:r.maps) r.fields));
  List.iter
    (fun e -> Fmt.pf ppf "A record is valid iff [%a].@\n" (pp_expr r.name) e)
    wheres;
  Fmt.pf ppf
    "@\n\
     A check or a decoder answers the codec's error, a [%s], and never raises \
     it; a write raises [Invalid_argument] where the codec's encoder does. *)@\n\
     @\n"
    parse_error

(* The members of a fixed record: its size, record, checks, decoders,
   writes, and with [accessor] its views. *)
let pp_record ?accessor ppf r =
  pp_val r.target ppf "min_size" "int"
    (Fmt.str "[min_size] is the size of a record in bytes, [%d]." r.size);
  pp_record_type ppf r;
  pp_val r.target ppf "size" "t -> int"
    (Fmt.str "[size r] is {!min_size}: every record takes %d bytes." r.size);
  pp_buffer ?accessor ppf r (bytes_buf r.target);
  if r.simd then
    match lanes r.plan ~size:r.size with Some l -> pp_x4 ppf r l | None -> ()

let record ~target ~simd ~maps ~codec ~plan ~size =
  let fields = all_fields plan in
  {
    target;
    codec;
    plan;
    size;
    fields;
    maps = List.map (fun (path, t, _) -> (path, t)) maps;
    name = namer fields;
    simd;
  }

let record_preamble ppf ~target ~simd ~maps ~codec ~plan ~size =
  pp_preamble ppf (record ~target ~simd ~maps ~codec ~plan ~size)

let record_members ?accessor ppf ~target ~simd ~maps ~codec ~plan ~size =
  pp_record ?accessor ppf (record ~target ~simd ~maps ~codec ~plan ~size)

(* {2 The module of any codec} *)

let scan_preamble ppf ~codec ~(plan : plan) =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from the codec %s. Do not edit: generate it \
     again when the codec changes. *)@\n\
     @\n"
    codec;
  Fmt.pf ppf
    "(** %s records.@\n\
     @\n\
     The records of [%s], checked, decoded and encoded as the codec does, with \
     its errors, by code that links [wire.runtime] only: a check, its view and \
     the accessor of each field, which read a record in place, and the decoder \
     and encoder of whole records. A check, a decoder or an accessor never \
     raises on bad input: a check refuses it, a decoder answers the codec's \
     error. *)@\n\
     @\n"
    plan.record codec

(* The fold, its [f] taking the fields [paths], of labelled types [types]. *)
let pp_fold ppf ~target ~paths ~types =
  pp_section ppf "folds" "Folds";
  let f = arrows (("'acc" :: types) @ [ "'acc" ]) in
  pp_val ~portable:false target ppf "fold"
    (arrows
       [
         "?stride:int";
         "?offs:int array";
         "?lens:int array";
         "?invalid:('acc -> 'acc)";
         "bytes";
         "init:'acc";
         Fmt.str "f:(%s)" f;
         Fmt.str "('acc, %s * 'acc) result" parse_error;
       ])
    (Fmt.str
       "[fold buf ~init ~f] checks each record of [buf], as {!val-check} does, \
        and folds [f] over those that pass, from [init]: [f acc%s], the fields \
        %s of the record, read during its check and labelled by their paths, a \
        field under an optional layer [None] where the layer is absent. The \
        records lie back to back from offset [0], each where the one before \
        ends, up to the end of [buf]; in slots of [stride] bytes, the [i]-th \
        at [i * stride], over the whole slots of [buf]; or in the frames of \
        [lens.(i)] bytes at [offs.(i)]. A record in a slot or a frame is \
        checked in its window alone.@\n\
        @\n\
        A refused record stops the fold with [Error (e, acc)], [e] the error \
        {!validate} answers on its window and [acc] the fold of the records \
        before it; in slots or frames, [invalid acc] is the accumulator after \
        a refused record and the fold goes on. Raises [Invalid_argument] if \
        [stride] is not positive, if [offs] and [lens] are not given together \
        or differ in length, if [stride] is given with [offs], and if \
        [invalid] is given with records back to back."
       (String.concat ""
          (List.map
             (fun p -> " ~" ^ String.concat "_" (String.split_on_char '.' p))
             paths))
       (match paths with
       | [] -> "(none)"
       | ps -> String.concat ", " (List.map (fun p -> "[" ^ p ^ "]") ps)))
