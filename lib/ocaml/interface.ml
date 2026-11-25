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
let parse_exn = "[" ^ rt ^ ".Errors.Parse_error]"

(* The buffer a function takes: its type, the runtime module that names it,
   and whether the function takes it [local]. *)
type buf = { typ : string; brand : string; local : bool }

let bytes_buf target =
  { typ = "bytes"; brand = rt ^ ".Buf"; local = target = `OxCaml }

let bigstring_buf target =
  {
    typ = rt ^ ".Unchecked.Bigstring.t";
    brand = rt ^ ".Buf.Bigstring";
    local = target = `OxCaml;
  }

let local_mode local = if local then " @ local" else ""
let buf b = b.typ ^ local_mode b.local
let named b = Fmt.str "'b %s.t%s" b.brand (local_mode b.local)
let length b = Fmt.str "'b %s.length" b.brand
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

let bits_text = function 1 -> "1 bit" | n -> Fmt.str "%d bits" n

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

(* The type of a field's plain value, as [write_int] takes it. *)
let plain f =
  match f.store with
  | Int { width = 32; _ } -> "int32"
  | Int _ | Bits _ -> "int"
  | Bytes _ -> "string"

(* The implementation's allocation checks, as [Record_module] writes them:
   a read of an integer or a bitfield with no map, and a write that calls no
   map. *)
let read_checked f =
  match f.store with
  | (Int _ | Bits _) when Option.is_none f.map -> true
  | _ -> false

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

let pp_values ppf r =
  match List.filter has_carrier r.fields with
  | [] -> ()
  | fields ->
      pp_section ppf "values" "Field values";
      pp_doc ppf
        "Field values held in range: a bitfield's [int] in its width and a \
         byte array's [string] at its length, each checked once, where it is \
         made.";
      Fmt.pf ppf "module Value : sig@\n";
      List.iter
        (fun f ->
          let l = f.label in
          match f.store with
          | Bits { width; _ } ->
              Fmt.pf ppf "type %s = private int@\n" l;
              pp_doc ppf
                (Fmt.str "The type for the values of a field of %s."
                   (bits_text width));
              Fmt.pf ppf "@\n";
              pp_val r.target ppf l
                (arrows [ "int"; l ])
                (Fmt.str
                   "[%s n] is [n] as a {!type-%s}. Raises [Invalid_argument] \
                    if [n] does not fit in %s."
                   l l (bits_text width));
              pp_val r.target ppf (l ^ "_low")
                (arrows [ "int"; l ])
                (Fmt.str "[%s_low n] is the %s of [n] as a {!type-%s}." l
                   (if width = 1 then "low bit" else Fmt.str "%d low bits" width)
                   l)
          | Bytes n ->
              Fmt.pf ppf "type %s = private string@\n" l;
              pp_doc ppf
                (Fmt.str "The type for the values of a field of %d bytes." n);
              Fmt.pf ppf "@\n";
              pp_val r.target ppf l
                (arrows [ "string"; l ])
                (Fmt.str
                   "[%s s] is [s] as a {!type-%s}. Raises [Invalid_argument] \
                    if [s] is not %d bytes long."
                   l l n)
          | Int _ -> ())
        fields;
      Fmt.pf ppf "end@\n@\n"

let pp_record_type ppf r =
  pp_section ppf "records" "Records";
  Fmt.pf ppf "type t = {@\n";
  List.iter
    (fun f -> Fmt.pf ppf "  %s : %s;@\n" f.label (typed ~maps:r.maps f))
    r.fields;
  Fmt.pf ppf "}@\n";
  pp_doc ppf (Fmt.str "The type for %s records." r.plan.record);
  Fmt.pf ppf "@\n";
  pp_val r.target ppf "v"
    (arrows (List.map (typed ~maps:r.maps) r.fields @ [ "t" ]))
    (Fmt.str "[v%s] is the record of these fields." (args r.fields));
  pp_val r.target ppf "pp" "Format.formatter -> t -> unit"
    "[pp ppf r] prints an unspecified representation of [r] on [ppf]."

(* [is_valid] to [decode], on the buffer [b]. *)
let pp_checks ppf r b =
  let target = r.target in
  let t = decoded r in
  pp_val ~zero_alloc:true target ppf "is_valid"
    (arrows [ buf b; "int"; "bool" ])
    "[is_valid buf off] is [true] iff the record at [off] in [buf] fits and \
     passes its checks.";
  pp_val target ppf "validate"
    (arrows [ buf b; "int"; Fmt.str "(unit, %s) result" parse_error ])
    "[validate buf off] is [Ok ()] if the record at [off] in [buf] fits and \
     passes its checks, and the codec's error otherwise. Raises \
     [Invalid_argument] if [off] is negative.";
  let portable = no_map r.fields in
  pp_val ~portable target ppf "decode"
    (arrows [ buf b; "int"; Fmt.str "(%s, %s) result" t parse_error ])
    "[decode buf off] is the record at [off] in [buf], or the codec's error. \
     Raises [Invalid_argument] if [off] is negative."

let pp_oxcaml_decoders ppf r =
  let b = bytes_buf r.target in
  let k = arrows (List.map (typed ~maps:r.maps) r.fields @ [ "'a" ]) in
  pp_val ~portable:(no_map r.fields) r.target ppf "decode_with"
    (arrows
       [
         "(" ^ k ^ ") @ local"; "fail:(unit -> 'a) @ local"; buf b; "int"; "'a";
       ])
    "[decode_with k ~fail buf off] is [k] applied to the fields of the record \
     at [off] in [buf], in order, if it fits and passes its checks, and [fail \
     ()] otherwise. [k] and [fail] are [local], so flambda2 inlines them. \
     Raises [Invalid_argument] if [off] is negative."

let pp_writes ppf r b =
  let target = r.target in
  let checked = no_map r.fields in
  let a = labelled_args r.fields in
  let label f t = f.label ^ ":" ^ t in
  let typed = List.map (fun f -> label f (typed ~maps:r.maps f)) r.fields in
  let plain = List.map (fun f -> label f (plain f)) r.fields in
  let rejected =
    if has_checks r.plan then ", or if the record breaks a constraint" else ""
  in
  pp_val ~portable:checked target ppf "encode"
    (arrows [ "t"; buf b; "int"; "unit" ])
    "[encode r buf off] writes the record [r] at [off] in [buf]. Raises as \
     {!write} does.";
  pp_val ~zero_alloc:checked ~portable:checked target ppf "write"
    (arrows ([ buf b; "int" ] @ typed @ [ "unit" ]))
    (Fmt.str
       "[write buf off%s] writes the record of these fields at [off] in [buf]. \
        Raises [Invalid_argument] if it does not fit%s."
       a rejected);
  pp_val ~zero_alloc:true target ppf "write_int"
    (arrows ([ buf b; "int" ] @ plain @ [ "unit" ]))
    (Fmt.str
       "[write_int buf off%s] is like {!write} but takes plain values: an \
        [int] for an integer of up to 16 bits or a bitfield, an [int32] for a \
        32-bit integer and a [string] for a byte array. It also raises \
        [Invalid_argument] if a value does not fit its field."
       a);
  pp_val ~zero_alloc:checked ~portable:checked target ppf "write_at"
    (arrows ([ named b; length b; "int" ] @ typed @ [ "unit" ]))
    (Fmt.str
       "[write_at b len off%s] is like {!write} but writes into the buffer [b] \
        of length [len]."
       a);
  pp_val ~zero_alloc:true target ppf "write_int_at"
    (arrows ([ named b; length b; "int" ] @ plain @ [ "unit" ]))
    (Fmt.str
       "[write_int_at b len off%s] is like {!write_int} but writes into the \
        buffer [b] of length [len]."
       a)

let pp_get ppf r b =
  let target = r.target in
  let enums = List.exists (fun f -> Option.is_some f.enum) r.fields in
  pp_doc ppf
    (Fmt.str
       "Reads of one field of the record at [off] in [buf], each one load that \
        checks its bounds and raises [Invalid_argument] outside them. A read \
        checks none of the record's constraints%s."
       (if enums then
          Fmt.str
            ", but a value that is no case of its field's closed enum raises %s"
            parse_exn
        else ""));
  Fmt.pf ppf "module Get : sig@\n";
  List.iter
    (fun f ->
      let l = f.label in
      let zero_alloc = read_checked f in
      let portable = Option.is_none f.map in
      pp_val ~zero_alloc ~portable target ppf l
        (arrows [ buf b; "int"; typed ~maps:r.maps f ])
        (Fmt.str "[%s buf off] is %s of the record at [off]." l (read_what f));
      match f.store with
      | Int { width = 32; _ } ->
          pp_val ~zero_alloc target ppf (l ^ "_int")
            (arrows [ buf b; "int"; "int" ])
            (Fmt.str
               "[%s_int buf off] is %s of the record at [off], as an [int]." l
               (where f))
      | _ -> ())
    r.fields;
  List.iter
    (fun f ->
      let l = f.label in
      match f.store with
      | Bits { base; _ } ->
          pp_val ~zero_alloc:true target ppf (l ^ "_word")
            (arrows [ buf b; "int"; "int" ])
            (Fmt.str
               "[%s_word buf off] is %s at offset %d of the record at [off], \
                for {!%s_of_word}."
               l (word base) f.offset l)
      | _ -> ())
    r.fields;
  Fmt.pf ppf "end@\n@\n"

let pp_of_words ppf r =
  let target = r.target in
  List.iter
    (fun f ->
      let l = f.label in
      match f.store with
      | Bits { shift; width; _ } ->
          pp_val ~zero_alloc:true target ppf (l ^ "_of_word")
            (arrows [ "int"; "int" ])
            (Fmt.str "[%s_of_word w] is %s of [w], a word {!Get.%s_word} reads."
               l
               (if width = 1 then Fmt.str "bit %d" shift
                else Fmt.str "bits %d to %d" shift (shift + width - 1))
               l)
      | _ -> ())
    r.fields

let pp_valid ppf r b =
  let target = r.target in
  pp_doc ppf
    "Buffers whose record at offset 0 passed its checks. A read of one is one \
     load with no check: the buffer's length cannot change, so a read stays in \
     it, but if its bytes change after the check a read may return a value \
     that breaks a constraint.";
  Fmt.pf ppf "module Valid : sig@\ntype t = private %s@\n" b.typ;
  pp_doc ppf "The type for buffers whose record at offset 0 passed its checks.";
  Fmt.pf ppf "@\n";
  pp_val target ppf "v"
    (arrows [ b.typ; Fmt.str "(t, %s) result" parse_error ])
    "[v buf] is [buf], once its record at offset 0 passed its checks, and the \
     codec's error otherwise.";
  Fmt.pf ppf "end@\n@\n";
  List.iter
    (fun f ->
      let l = f.label in
      let zero_alloc = read_checked f in
      let portable = Option.is_none f.map in
      pp_val ~zero_alloc ~portable target ppf ("read_" ^ l)
        (arrows [ "Valid.t"; typed ~maps:r.maps f ])
        (Fmt.str "[read_%s v] is %s of the record in [v]." l (read_what f));
      match f.store with
      | Int { width = 32; _ } ->
          pp_val ~zero_alloc target ppf
            ("read_" ^ l ^ "_int")
            (arrows [ "Valid.t"; "int" ])
            (Fmt.str "[read_%s_int v] is %s of the record in [v], as an [int]."
               l (where f))
      | _ -> ())
    r.fields

let pp_view_module ppf r b =
  let target = r.target in
  let k = local_mode (target = `OxCaml) in
  pp_doc ppf
    "Records checked once in a buffer named by [Wire_runtime.Buf.v], whose \
     length is read once. A view is an [int], so it allocates nothing, and a \
     read of one is one load with no check, with the promise of {!Valid}.";
  Fmt.pf ppf "module View : sig@\n";
  Fmt.pf ppf "type 'b t = private int@\n";
  pp_doc ppf "The type for records that passed their checks in the buffer ['b].";
  Fmt.pf ppf "@\ntype 'b check = private int@\n";
  pp_doc ppf "The type for the answers of {!val-check}.";
  Fmt.pf ppf "@\n";
  pp_val target ppf "check"
    (arrows [ named b; length b; "int"; "'b check" ])
    "[check b len off] checks the record at [off] in the buffer [b] of length \
     [len], without raising.";
  pp_val target ppf "ok"
    (arrows [ "'b check"; "bool" ])
    "[ok c] is [true] iff the check [c] passed.";
  pp_val target ppf "get"
    (arrows [ "'b check"; "'b t" ])
    "[get c] is the record the check [c] passed. Raises [Invalid_argument] if \
     [c] failed.";
  pp_val target ppf "with_"
    (arrows
       [
         named b;
         length b;
         "int";
         Fmt.str "ok:('b t -> 'a)%s" k;
         Fmt.str "fail:(unit -> 'a)%s" k;
         "'a";
       ])
    "[with_ b len off ~ok ~fail] is [ok r] for the record [r] at [off] in the \
     buffer [b] of length [len] if it passes its checks, and [fail ()] \
     otherwise.";
  Fmt.pf ppf "end@\n@\n"

let pp_view ?(views = false) ppf r b =
  let target = r.target in
  if not views then pp_view_module ppf r b;
  let view_t = if views then "'b view" else "'b View.t" in
  List.iter
    (fun f ->
      let l = f.label in
      let zero_alloc = read_checked f in
      let view ?(portable = true) suffix typ doc =
        pp_val ~zero_alloc ~portable target ppf
          ("view_" ^ l ^ suffix)
          (arrows [ named b; view_t; typ ])
          doc
      in
      if not views then
        view ~portable:(Option.is_none f.map) "" (typed ~maps:r.maps f)
          (Fmt.str "[view_%s b v] is %s of the record [v] in [b]." l
             (read_what f));
      match f.store with
      | Int { width = 32; _ } ->
          view "_int" "int"
            (Fmt.str
               "[view_%s_int b v] is %s of the record [v] in [b], as an [int]."
               l (where f))
      | Bits { base; _ } ->
          view "_word" "int"
            (Fmt.str
               "[view_%s_word b v] is %s at offset %d of the record [v] in \
                [b], for {!%s_of_word}."
               l (word base) f.offset l)
      | _ -> ())
    r.fields

let pp_x4_writes ppf r lanes =
  let target = r.target in
  let b = bytes_buf r.target in
  let v = rt ^ ".Vec.t" in
  pp_val target ppf "store"
    (arrows [ named b; length b; "int"; "part"; "unit" ])
    "[store b len off p] writes the four records of [p] at [off] in [b]. \
     Raises [Invalid_argument] for the first record that does not fit, once \
     the ones before it are written.";
  pp_val target ppf "fill"
    (arrows
       [
         named b;
         length b;
         "int";
         "count:int";
         "f:(int -> part) @ local";
         "unit";
       ])
    "[fill b len off ~count ~f] stores [f k] at [off + (16 * k)] for each [k] \
     from [0] to [count - 1].";
  pp_val target ppf "write_int_at"
    (arrows
       ([ named b; length b; "int" ] @ List.map (fun _ -> v) lanes @ [ "unit" ]))
    (Fmt.str
       "[write_int_at b len off%s] writes the four records whose fields are in \
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
         length b;
         "int";
         "ok:(" ^ v ^ " -> 'a) @ local";
         "fail:(unit -> 'a) @ local";
         "'a";
       ])
    "[with_ b len off ~ok ~fail] is [ok w] for the 16 bytes [w] at [off] in \
     [b] if they fit, and [fail ()] otherwise.";
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

let pp_buffer ?(views = false) ppf r b =
  pp_section ppf "checks" "Validating and decoding";
  pp_checks ppf r b;
  if b.typ = "bytes" && r.target = `OxCaml then pp_oxcaml_decoders ppf r;
  pp_section ppf "writes" "Writing";
  pp_writes ppf r b;
  pp_section ppf "reads" "Reading fields";
  pp_get ppf r b;
  pp_of_words ppf r;
  if not views then begin
    pp_valid ppf r b;
    pp_section ppf "views" "Views";
    pp_view ppf r b
  end

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

let pp_digest target ppf =
  pp_val target ppf "digest" "string"
    "[digest] is the digest of the shape of the codec this module was \
     generated from."

(* The members of a fixed record: its size, values, record, checks,
   decoders, writes, reads and views, on [bytes] and on a bigarray. *)
let pp_record ?(views = false) ?(buffer = `Bytes) ppf r =
  pp_val r.target ppf "min_size" "int"
    (Fmt.str "[min_size] is the size of a record in bytes, [%d]." r.size);
  pp_values ppf r;
  pp_record_type ppf r;
  pp_val r.target ppf "size" "t -> int"
    (Fmt.str "[size r] is {!min_size}: every record takes %d bytes." r.size);
  match buffer with
  | `Bigstring -> pp_buffer ppf r (bigstring_buf r.target)
  | `Bytes -> (
      pp_buffer ~views ppf r (bytes_buf r.target);
      if r.simd then
        match lanes r.plan ~size:r.size with
        | Some l -> pp_x4 ppf r l
        | None -> ())

(* What reads through the module's views, after them. *)
let pp_after_views ppf r =
  pp_section ppf "words" "Words of a view";
  pp_view ~views:true ppf r (bytes_buf r.target)

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

let record_members ?views ?buffer ppf ~target ~simd ~maps ~codec ~plan ~size =
  pp_record ?views ?buffer ppf (record ~target ~simd ~maps ~codec ~plan ~size)

let record_after_views ppf ~target ~simd ~maps ~codec ~plan ~size =
  pp_after_views ppf (record ~target ~simd ~maps ~codec ~plan ~size)

(* {2 The scanner} *)

let describe_output (o : output) =
  if o.is_bool then
    let suffix = "_present" in
    let field =
      String.sub o.name 0 (String.length o.name - String.length suffix)
    in
    Fmt.str "{- [%s], [true] iff the optional field [%s] is present.}" o.name
      field
  else
    Fmt.str "{- [%s], %s %s.}" o.name
      (if o.signed then "a signed integer of" else "an integer of")
      (bits_text o.wire_width)

(* The labelled outputs a consumer takes, each of [int] (or [num] on
   OxCaml's unboxed forms), or [unit] when there is none. *)
let outputs ~num selected =
  match selected with
  | [] -> [ "unit" ]
  | os ->
      List.map
        (fun (o : output) -> o.name ^ ":" ^ if o.is_bool then "bool" else num)
        os

let scan_preamble ppf ~codec ~(plan : plan) =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from the codec %s. Do not edit: generate it \
     again when the codec changes. *)@\n\
     @\n"
    codec;
  Fmt.pf ppf
    "(** %s scans.@\n\
     @\n\
     A scan checks a record of [%s] in a frame of bytes, as \
     [Wire.Codec.decode] does on those bytes alone, by code that links \
     [wire.runtime] only, and hands out its scalar fields with no value built \
     and no byte copied. "
    plan.record codec;
  (match plan.outputs with
  | [] -> Fmt.pf ppf "It hands out no field: it only checks."
  | os ->
      Fmt.pf ppf
        "Its outputs, in declaration order, each an [int] or a [bool]:@\n\
         {ul@\n\
         %s}@\n\
         An absent field is [0]."
        (String.concat "\n" (List.map describe_output os)));
  Fmt.pf ppf
    " A scan never raises on bad input: it calls [invalid] instead. *)@\n@\n"

(* What a fold hands out, when it is not every output. *)
let handed ~(plan : plan) selected =
  if List.length selected = List.length plan.outputs then ""
  else
    match selected with
    | [] -> " It hands out no output."
    | os ->
        Fmt.str " It hands out %s."
          (String.concat ", "
             (List.map (fun (o : output) -> "[" ^ o.name ^ "]") os))

(* The fold [n] calling the visitor [m] by name: one frame, the frames of a
   stride and those of an index. *)
let pp_visitor_fold ppf ~target ~plan b n m selected =
  let t = m ^ ".t" in
  (* The visitor takes the integers as [int64]. *)
  let outputs = ", each integer output an [int64]" in
  pp_val ~portable:false target ppf n
    (arrows [ b; "int"; "int"; "init:" ^ t; t ])
    (Fmt.str
       "[%s buf off len ~init] is [%s.valid init] applied to the outputs of \
        the record at [off] in the [len] bytes there%s, or [%s.invalid init] \
        if the record or the bounds are bad, with no closure built.%s"
       n m outputs m (handed ~plan selected));
  pp_val ~portable:false target ppf (n ^ "_index")
    (arrows [ b; "offs:int array"; "lens:int array"; "init:" ^ t; t ])
    (Fmt.str
       "[%s_index buf ~offs ~lens ~init] folds [%s] over the frames of \
        [lens.(i)] bytes at [offs.(i)], in order, from [init]: a frame whose \
        bounds are bad gives [%s.invalid]. The length of [buf] is read once. \
        Raises [Invalid_argument] if [offs] and [lens] differ in length."
       n n m);
  pp_val ~portable:false target ppf (n ^ "_frames")
    (arrows [ b; "int"; "int"; "stride:int"; "init:" ^ t; t ])
    (Fmt.str
       "[%s_frames buf off len ~stride ~init] folds [%s] over the [len / \
        stride] frames of [stride] bytes at [off], each checked on its own, \
        from [init]. Raises [Invalid_argument] if [stride] is not positive or \
        the bounds are bad."
       n n)

(* The unboxed forms of the fold [n], on OxCaml. *)
let pp_unboxed_fold ppf ~target b n selected =
  let valid = arrows (outputs ~num:"int64_u" selected @ [ "'a" ]) in
  let valid_acc =
    arrows (("'a" :: outputs ~num:"int64_u" selected) @ [ "'a" ])
  in
  pp_val target ppf (n ^ "_unboxed")
    (arrows
       [
         b; "int"; "int"; "invalid:(unit -> 'a)"; "valid:(" ^ valid ^ ")"; "'a";
       ])
    (Fmt.str
       "[%s_unboxed buf off len ~invalid ~valid] is [%s buf off len ~invalid \
        ~valid] with the outputs unboxed."
       n n);
  pp_val target ppf (n ^ "_frames_unboxed")
    ("('a : bits64). "
    ^ arrows
        [
          b;
          "int";
          "int";
          "stride:int";
          "init:'a";
          "invalid:('a -> 'a)";
          "valid:(" ^ valid_acc ^ ")";
          "'a";
        ])
    (Fmt.str
       "[%s_frames_unboxed buf off len ~stride ~init ~invalid ~valid] is \
        [%s_frames] with the outputs unboxed and an accumulator of layout \
        [bits64]."
       n n)

(* The fold [n] taking its consumer as arguments. *)
let pp_closure_fold ppf ~target ~plan b n selected =
  let valid = arrows (outputs ~num:"int" selected @ [ "'a" ]) in
  let valid_acc = arrows (("'a" :: outputs ~num:"int" selected) @ [ "'a" ]) in
  pp_val target ppf n
    (arrows
       [
         b; "int"; "int"; "invalid:(unit -> 'a)"; "valid:(" ^ valid ^ ")"; "'a";
       ])
    (Fmt.str
       "[%s buf off len ~invalid ~valid] is [valid] applied to the outputs of \
        the record at [off] in the [len] bytes there, or [invalid ()] if the \
        record or the bounds are bad.%s"
       n (handed ~plan selected));
  pp_val target ppf (n ^ "_frames")
    (arrows
       [
         b;
         "int";
         "int";
         "stride:int";
         "init:'a";
         "invalid:('a -> 'a)";
         "valid:(" ^ valid_acc ^ ")";
         "'a";
       ])
    (Fmt.str
       "[%s_frames buf off len ~stride ~init ~invalid ~valid] folds [valid] \
        with the outputs of each record, or [invalid], over the [len / stride] \
        frames of [stride] bytes at [off], each checked on its own, from \
        [init]. Raises [Invalid_argument] if [stride] is not positive or the \
        bounds are bad."
       n);
  if target = `OxCaml then pp_unboxed_fold ppf ~target b n selected

let scan_members ppf ~target ~(plan : plan) ~(folds : Scan_module.fold list) =
  let b = buf (bytes_buf target) in
  if folds <> [] then pp_section ppf "scans" "Scans";
  List.iter
    (fun (f : Scan_module.fold) ->
      match f.visitor with
      | Some m -> pp_visitor_fold ppf ~target ~plan b f.name m f.selected
      | None -> pp_closure_fold ppf ~target ~plan b f.name f.selected)
    folds
