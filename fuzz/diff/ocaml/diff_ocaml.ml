open Wire.Private
module T = Types

type view = Get | Get_int | Of_word

type ops = {
  decode : bytes -> int -> string;
  validate : bytes -> int -> unit;
  encode : string -> bytes -> int -> unit;
  gets : (string * view * (bytes -> int -> string)) list;
  reads : (string * view) list;
  valid : bytes -> string list;
  is_valid : bytes -> int -> bool;
  view : bytes -> int -> string list;
  checked : bytes -> int -> bool * string list;
}

type t = { bytes : ops; bigstring : ops }

let big b =
  Bigarray.Array1.init Bigarray.char Bigarray.c_layout (Bytes.length b)
    (Bytes.get b)

let blit_back bb b = Bytes.iteri (fun i _ -> Bytes.set b i bb.{i}) b

(* {1 Rendering}

   A field's value as both sides show it: an integer in decimal, a byte array
   quoted. The generator writes the code that shows a generated getter's
   value; [show] shows the value the codec's own reader returns. *)

let rec show_code : type a. a T.typ -> string option = function
  | T.Uint8 -> Some "(fun v -> string_of_int (Wire.UInt8.to_int v))"
  | T.Int8 -> Some "(fun v -> string_of_int (Wire.SInt8.to_int v))"
  | T.Uint16 _ -> Some "(fun v -> string_of_int (Wire.UInt16.to_int v))"
  | T.Int16 _ -> Some "(fun v -> string_of_int (Wire.SInt16.to_int v))"
  | T.Uint32 _ -> Some "(fun v -> string_of_int (Wire.UInt32.to_int v))"
  | T.Int32 _ -> Some "(fun v -> string_of_int (Wire.SInt32.to_int v))"
  | T.Bits _ -> Some "string_of_int"
  | T.Byte_array _ -> Some "(fun v -> Fmt.str \"%S\" v)"
  | T.Where { inner; _ } -> show_code inner
  | T.Enum { base; _ } -> show_code base
  | _ -> None

let rec show : type a. a T.typ -> (a -> string) option = function
  | T.Uint8 -> Some (fun v -> string_of_int (Wire.UInt8.to_int v))
  | T.Int8 -> Some (fun v -> string_of_int (Wire.SInt8.to_int v))
  | T.Uint16 _ -> Some (fun v -> string_of_int (Wire.UInt16.to_int v))
  | T.Int16 _ -> Some (fun v -> string_of_int (Wire.SInt16.to_int v))
  | T.Uint32 _ -> Some (fun v -> string_of_int (Wire.UInt32.to_int v))
  | T.Int32 _ -> Some (fun v -> string_of_int (Wire.SInt32.to_int v))
  | T.Bits _ -> Some string_of_int
  | T.Byte_array _ -> Some (fun v -> Fmt.str "%S" v)
  | T.Where { inner; _ } -> show inner
  | T.Enum { base; _ } -> show base
  | _ -> None

let rec views : type a. a T.typ -> view list = function
  | T.Uint32 _ | T.Int32 _ -> [ Get; Get_int ]
  | T.Bits _ -> [ Get; Of_word ]
  | T.Where { inner; _ } -> views inner
  | T.Enum { base; _ } -> views base
  | _ -> [ Get ]

(* {1 What the generator handles}

   Written from its documentation rather than from its code: a fixed-size,
   parameter-free record of named, action-free fields, each an integer of up to
   32 bits (an [int] that holds one), a bitfield or a constant-size byte array,
   under closed or open enums and refinements over integer expressions, and a
   byte array without a constraint. *)

let rec expr_ok : type a. a T.expr -> bool = function
  | T.Int _ | T.Bool _ | T.Ref (T.I, _) -> true
  | T.Add (a, b)
  | T.Sub (a, b)
  | T.Mul (a, b)
  | T.Land (a, b)
  | T.Lor (a, b)
  | T.Lxor (a, b)
  | T.Lsl (a, b)
  | T.Lsr (a, b) ->
      expr_ok a && expr_ok b
  | T.Lnot a -> expr_ok a
  | T.Eq (a, b) -> expr_ok a && expr_ok b
  | T.Ne (a, b) -> expr_ok a && expr_ok b
  | T.Lt (a, b) -> expr_ok a && expr_ok b
  | T.Le (a, b) -> expr_ok a && expr_ok b
  | T.Gt (a, b) -> expr_ok a && expr_ok b
  | T.Ge (a, b) -> expr_ok a && expr_ok b
  | T.And (a, b) | T.Or (a, b) -> expr_ok a && expr_ok b
  | T.Not a -> expr_ok a
  | T.Cast ((`U8 | `U16), a) -> expr_ok a
  | T.If_then_else (c, a, b) -> expr_ok c && expr_ok a && expr_ok b
  | _ -> false

let wide = Sys.int_size > 32

let rec integer : type a. a T.typ -> bool = function
  | T.Uint8 | T.Int8 | T.Uint16 _ | T.Int16 _ -> true
  | T.Uint32 _ | T.Int32 _ -> wide
  | T.Where { cond; inner } -> expr_ok cond && integer inner
  | T.Enum { base; _ } -> integer base
  | _ -> false

let field_ok (T.Field f) =
  Option.is_some f.field_name
  && Option.is_none f.action
  && Option.fold ~none:true ~some:expr_ok f.constraint_
  &&
  match f.field_typ with
  | T.Bits { base = T.U32 _; _ } -> wide
  | T.Bits _ -> true
  | T.Byte_array { size = T.Int n } -> n >= 0 && Option.is_none f.constraint_
  | t -> integer t

let specialisable c =
  let s = Gen.to_struct c in
  s.params = []
  && Option.fold ~none:true ~some:expr_ok s.where
  && List.for_all field_ok s.fields
  && Option.is_some (Wire.Codec.wire_size_opt c)

(* A refusal is expected of a codec outside that set, and of one whose field
   names would spell the same OCaml name twice. *)
let find sub s =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.equal (String.sub s i n) sub then Some i
    else go (i + 1)
  in
  go 0

let name_clash msg = Option.is_some (find " define" msg)

(* {1 The codecs} *)

(* The flat records of a sample this long that are in the generator's scope
   join the EverParse backend's codecs: a sample of [Diff_codecs.sample_count]
   holds almost none, and they are the records the generator specialises
   whole, adversarial field names included.
   Generating OCaml costs nothing like a run of EverParse. *)
let flat_count = 4096

(* A record holding two leaves that embed the same sub-codec does not project
   to 3D, so it is out of the EverParse backend's scope too. *)
let specialisable_packed (Fuzz_gen.Pack g) = specialisable (Fuzz_gen.codec g)

(* The EverParse backend's scope, less its refusal of field names the FFI
   pipeline spells alike: the generator names fields its own way and refuses
   its own clashes, so a codec the plug alone refuses ([restrict] and
   [restrict_]) is still compared here. *)
let in_scope p =
  match Diff_codecs.scope_exclusion p with
  | None | Some Name_clash -> true
  | Some _ -> false
  | exception Invalid_argument _ -> false

let sampled = List.filter (fun (_, p) -> in_scope p) Diff_codecs.candidates

let codecs =
  sampled
  @ List.filter
      (fun (label, p) ->
        (not (List.mem_assoc label sampled))
        && in_scope p && specialisable_packed p)
      (Fuzz_gen.flat_records ~seed:Diff_codecs.sample_seed ~count:flat_count)

(* {1 The comparison} *)

let outcome f =
  match f () with
  | s -> "ok " ^ s
  | exception Wire.Parse_error e ->
      Fmt.str "parse error %a" Wire.pp_parse_error e
  | exception Invalid_argument m -> "invalid " ^ m

let view_name name = function
  | Get -> Fmt.str "get %s" name
  | Get_int -> Fmt.str "get %s as an int" name
  | Of_word -> Fmt.str "get %s from its word" name

(* The reader [Codec.get] stages for a field, its value shown. *)
let reference codec name =
  let k typ read =
    match show typ with
    | Some show -> fun buf off -> show (read buf off)
    | None ->
        Fmt.failwith "diff: field %s has a type the backend cannot show" name
  in
  match Gen.with_field_reader codec name { k } with
  | Some read -> read
  | None -> Fmt.failwith "diff: the codec has no field %s" name

let same label what expected got =
  if not (String.equal expected got) then
    Alcobar.failf "%s: %s: Wire.Codec %s, but the generated code %s" label what
      expected got

(* One codec and its generated module, with the codec's reader of each field
   staged once. *)
type case = {
  label : string;
  m : t;
  refs : (string * (bytes -> int -> string)) list;
}

let both case f =
  f "bytes" case.m.bytes;
  f "bigstring" case.m.bigstring

(* [is_valid], [View.at] and [View.check] at [off], against [Codec.validate]
   and each field's reader there. *)
let views_at case c ~what ~validated ops buf off =
  same case.label (what "is_valid")
    (string_of_bool (validated = "ok "))
    (string_of_bool (ops.is_valid buf off));
  let expected =
    outcome (fun () ->
        Wire.Codec.validate c buf off;
        String.concat ","
          (List.map
             (fun (name, _) -> List.assoc name case.refs buf off)
             ops.reads))
  in
  same case.label
    (what "View.at then each read")
    expected
    (outcome (fun () -> String.concat "," (ops.view buf off)));
  let checked =
    outcome (fun () ->
        match ops.checked buf off with
        | true, reads -> String.concat "," reads
        | false, _ ->
            Wire.Codec.validate c buf off;
            "")
  in
  same case.label (what "View.check then each read") expected checked

(* Decode, validate and every getter at [off]. *)
let reads_at case (Fuzz_gen.Pack g) buf off =
  let c = Fuzz_gen.codec g in
  let decoded =
    outcome (fun () -> Wire.Codec.to_string c (Wire.Codec.decode_exn c buf off))
  in
  let validated =
    outcome (fun () ->
        Wire.Codec.validate c buf off;
        "")
  in
  both case (fun kind ops ->
      let what w = Fmt.str "%s %s at offset %d" kind w off in
      same case.label (what "decode") decoded
        (outcome (fun () -> ops.decode buf off));
      same case.label (what "validate") validated
        (outcome (fun () ->
             ops.validate buf off;
             ""));
      List.iter
        (fun (name, view, read) ->
          same case.label
            (what (view_name name view))
            (outcome (fun () -> List.assoc name case.refs buf off))
            (outcome (fun () -> read buf off)))
        ops.gets;
      views_at case c ~what ~validated ops buf off)

(* The record [buf] holds at [off], when it decodes, encoded into buffers
   around it, too short, and at an offset out of range. *)
let encodes_at case (Fuzz_gen.Pack g) buf off =
  let c = Fuzz_gen.codec g in
  let size = Wire.Codec.wire_size c in
  match Wire.Codec.decode_exn c buf off with
  | exception (Wire.Parse_error _ | Invalid_argument _) -> ()
  | v ->
      let canonical = Wire.Codec.to_string c v in
      let into (len, off) =
        let run f =
          outcome (fun () ->
              let out = Bytes.make (max 0 len) '\xaa' in
              f out off;
              Bytes.to_string out)
        in
        let expected = run (fun out off -> Wire.Codec.encode c v out off) in
        both case (fun kind ops ->
            same case.label
              (Fmt.str "%s encode into %d bytes at offset %d" kind len off)
              expected
              (run (ops.encode canonical)))
      in
      List.iter into
        [
          (size, 0);
          (Bytes.length buf, off);
          (size + 2, 1);
          (size - 1, 0);
          (size, -1);
        ]

(* [Valid.v] on the buffer and each read, against [Codec.validate] and each
   field's reader at offset 0. *)
let valid case (Fuzz_gen.Pack g) buf =
  let c = Fuzz_gen.codec g in
  let expected keys =
    outcome (fun () ->
        Wire.Codec.validate c buf 0;
        String.concat ","
          (List.map (fun (name, _) -> List.assoc name case.refs buf 0) keys))
  in
  both case (fun kind ops ->
      same case.label
        (Fmt.str "%s Valid.v then each read" kind)
        (expected ops.reads)
        (outcome (fun () -> String.concat "," (ops.valid buf))))

let check label (Fuzz_gen.Pack g as codec) m =
  let c = Fuzz_gen.codec g in
  let size = Wire.Codec.wire_size c in
  let refs =
    List.sort_uniq compare (List.map (fun (n, _, _) -> n) m.bytes.gets)
    |> List.map (fun n -> (n, reference c n))
  in
  let case = { label; m; refs } in
  fun buf ->
    let n = Bytes.length buf in
    List.iter
      (fun off ->
        reads_at case codec buf off;
        encodes_at case codec buf off)
      (List.sort_uniq compare [ 0; 1; -1; n - size; n - size + 1 ]);
    valid case codec buf

let pp ppf m =
  Fmt.pf ppf "@[<h>%a@]"
    Fmt.(list ~sep:sp (fun ppf (n, v, _) -> string ppf (view_name n v)))
    m.bytes.gets

(* {1 Refusals} *)

let reason msg =
  (* "Wire_ocaml: cannot specialise codec: field f1: its type, UINT64" reads as
     "its type, UINT64". *)
  let after sep s =
    match find sep s with
    | Some i ->
        let j = i + String.length sep in
        String.sub s j (String.length s - j)
    | None -> s
  in
  let r = after "cannot specialise codec: " msg in
  if String.starts_with ~prefix:"field " r then after ": " r else r

let report name refused =
  let sampled_refused, flat =
    List.partition (fun (label, _, _) -> List.mem_assoc label sampled) refused
  in
  let total = List.length sampled and n = List.length sampled_refused in
  let tally =
    List.fold_left
      (fun acc (_, _, msg) ->
        let k = reason msg in
        let c = Option.value ~default:0 (List.assoc_opt k acc) in
        (k, c + 1) :: List.remove_assoc k acc)
      [] sampled_refused
  in
  Fmt.epr "%s: %d of the %d sampled codecs specialised, %d refused (%d%%)@."
    name (total - n) total n
    (if total = 0 then 0 else 100 * n / total);
  List.iter (fun (k, c) -> Fmt.epr "%s:   %d %s@." name c k) (List.rev tally);
  let extra = List.length codecs - total in
  Fmt.epr "%s: %d of the %d flat records in scope of %d samples specialised@."
    name
    (extra - List.length flat)
    extra flat_count;
  let unexpected =
    List.filter
      (fun (_, Fuzz_gen.Pack g, msg) ->
        specialisable (Fuzz_gen.codec g) && not (name_clash msg))
      refused
  in
  List.iter
    (fun (label, _, msg) ->
      Fmt.epr
        "%s: FATAL %s is in the generator's documented scope but was refused: \
         %s@."
        name label msg)
    unexpected;
  unexpected = []
