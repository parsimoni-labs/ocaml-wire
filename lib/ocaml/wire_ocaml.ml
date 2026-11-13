open Wire.Private
open Layout
module T = Types

exception Unsupported of string

let fail_unsupported fmt = Fmt.kstr (fun s -> raise (Unsupported s)) fmt

(* How a field is stored is a [Layout.store]. Integers up to 16 bits and
   bitfields are handled as an [int], 32-bit integers as an [int32], byte
   arrays as a [string]. *)

(* What a failed test raises, as the codec does: a violated constraint
   (carrying the field's value), a value no case of a closed enum has, or an
   index past the cases of a lookup. *)
type kind = Constraint | Enum of int list | Tag

(* A test as OCaml: [fast] is false when the test fails or its arithmetic
   overflows; [slow], present when it has arithmetic, raises the codec's error
   on overflow. *)
type test = { fast : string; slow : string option }

(* A field of the record: its name, the OCaml name its functions and record
   field derive from, the variable that holds its plain value, where it sits,
   how it is stored, the OCaml tests its value must pass in the order the codec
   runs them, each with what its failure raises, and the closed enum the
   encoder checks it against. A failed check names [off], as the codec does:
   for a bitfield, the offset of its word. *)
type field = {
  name : string;
  label : string;
  var : string;
  off : int;
  store : store;
  checks : (test * kind) list;
  enum : (string * int list) option;
  map : string option;
}

let tests f = List.map (fun (t, _) -> t.fast) f.checks

type typ = Typ : 'a Wire.typ -> typ

(* The OCaml path of each map a caller names, found by the map value itself. *)
type maps = { find : 'a. 'a T.typ -> string option }

let maps_of named =
  {
    find =
      (fun typ ->
        List.find_map
          (fun (path, Typ t) ->
            let id = Wire.Private.Gen.map_id typ in
            if Option.is_some id && Wire.Private.Gen.map_id t = id then
              Some path
            else None)
          named);
  }

type target = [ `OCaml | `OxCaml ]

(* A field's label: lower-cased, made an identifier, and escaped when a
   keyword of OCaml or of OxCaml, whatever the target, so the code written for
   the default target compiles on an OxCaml compiler too. *)
let label name = Wire.Private.Names.ocaml_label name
let pp_list = Fmt.(list ~sep:(any " ") string)
let pp_vars ppf fields = pp_list ppf (List.map (fun f -> f.var) fields)

(* The generated code is compiled for its target, whose [int] may have 31 bits
   (wasm_of_ocaml) or 32 (js_of_ocaml), so no literal it holds needs more than
   31 bits: a wider integer is written through [int64], and the mask of 32 bits
   is [mask32], bound by the module. *)
let max31 = (1 lsl 30) - 1

let int_lit n =
  if n > max31 || n < -max31 - 1 then
    Fmt.str "(Int64.to_int %LdL)" (Int64.of_int n)
  else if n < 0 then Fmt.str "(%d)" n
  else string_of_int n

(* The mask of the [width] low bits, [width] at most 32. *)
let mask width =
  if width <= 30 then Fmt.str "0x%x" ((1 lsl width) - 1)
  else Fmt.str "(mask32 lsr %d)" (32 - width)

let rt = "Wire_runtime"
let errors = rt ^ ".Errors"
let arith = rt ^ ".Arith"

(* How [+], [-] and [*] are written: raising [Arith.Overflow], or the codec's
   error. [used] is set once one is written. *)
type ops = { checked : bool; mutable used : bool }

(* An OCaml expression for [e], over the plain values of the fields read so
   far, which are what the codec's constraints see. *)
let rec expr : type a. ops -> (string -> string) -> a T.expr -> string =
 fun ops var e ->
  let expr e = expr ops var e in
  let cmp op a b = cmp ops var op a b in
  let bin op a b = Fmt.str "(%s %s %s)" (expr a) op (expr b) in
  let arith_op fn a b =
    ops.used <- true;
    Fmt.str "(%s.%s%s %s %s)" arith
      (if ops.checked then "checked_" else "")
      fn (expr a) (expr b)
  in
  match e with
  | T.Int n -> int_lit n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, name) -> var name
  | T.Add (a, b) -> arith_op "add" a b
  | T.Sub (a, b) -> arith_op "sub" a b
  | T.Mul (a, b) -> arith_op "mul" a b
  | T.Land (a, b) -> bin "land" a b
  | T.Lor (a, b) -> bin "lor" a b
  | T.Lxor (a, b) -> bin "lxor" a b
  | T.Lnot a -> Fmt.str "(lnot %s)" (expr a)
  | T.Lsl (a, b) -> bin "lsl" a b
  | T.Lsr (a, b) -> bin "lsr" a b
  | T.Eq (a, b) -> cmp "=" a b
  | T.Ne (a, b) -> cmp "<>" a b
  | T.Lt (a, b) -> cmp "<" a b
  | T.Le (a, b) -> cmp "<=" a b
  | T.Gt (a, b) -> cmp ">" a b
  | T.Ge (a, b) -> cmp ">=" a b
  | T.And (a, b) -> bin "&&" a b
  | T.Or (a, b) -> bin "||" a b
  | T.Not a -> Fmt.str "(not %s)" (expr a)
  | T.Cast (`U8, a) -> Fmt.str "(%s land 0xff)" (expr a)
  | T.Cast (`U16, a) -> Fmt.str "(%s land 0xffff)" (expr a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s then %s else %s)" (expr c) (expr a) (expr b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* Comparisons are typed, so the compiler emits a machine comparison. *)
and cmp : type a.
    ops -> (string -> string) -> string -> a T.expr -> a T.expr -> string =
 fun ops var op a b ->
  let ty : type a. a T.expr -> string = function
    | T.Bool _ | T.Eq _ | T.Ne _ | T.Lt _ | T.Le _ | T.Gt _ | T.Ge _ | T.And _
    | T.Or _ | T.Not _ ->
        "bool"
    | _ -> "int"
  in
  Fmt.str "((%s : %s) %s %s)" (expr ops var a) (ty a) op (expr ops var b)

let test var e =
  let fast = { checked = false; used = false } in
  let s = expr fast var e in
  if not fast.used then { fast = s; slow = None }
  else
    {
      fast =
        Fmt.str "(match %s with b -> b | exception %s.Overflow -> false)" s
          arith;
      slow = Some (expr { checked = true; used = false } var e);
    }

(* A field's store, and the conditions its type adds on its value, which a
   field's own constraint refers to by the field's name: the outermost first,
   as the codec checks them. *)
let rec classify : type a.
    a T.typ -> store * (string -> (bool T.expr * kind) list) =
 fun typ ->
  let none _ = [] in
  match typ with
  | T.Where { cond; inner } ->
      let s, t = classify inner in
      (s, fun self -> (cond, Constraint) :: t self)
  | T.Enum { cases; base; closed = true; _ } ->
      let s, t = classify base in
      let member self =
        List.fold_left
          (fun acc (_, v) ->
            let eq = T.(Eq (Ref (I, self), Int v)) in
            match acc with None -> Some eq | Some a -> Some T.(Or (a, eq)))
          None cases
        |> Option.value ~default:(T.Bool false : bool T.expr)
      in
      let valid = List.map snd cases in
      (s, fun self -> (member self, Enum valid) :: t self)
  | T.Enum { base; closed = false; _ } -> classify base
  | T.Map _ -> fail_unsupported "a map inside its type"
  | typ -> (
      match leaf typ with
      | Some s -> (s, none)
      | None -> fail_unsupported "its type, %a" T.pp_typ typ)

let rec enum_of : type a. a T.typ -> (string * int list) option = function
  | T.Enum { name; cases; closed = true; _ } -> Some (name, List.map snd cases)
  | T.Where { inner; _ } -> enum_of inner
  | T.Map { inner; _ } -> enum_of inner
  | _ -> None

(* One field laid out at [off], its value under [map] when it has one. *)
(* [extra] are the checks between the type's and the field's own: a map's
   index bound. *)
let field_of : type b.
    var:(string -> string) ->
    label:string ->
    ?extra:(test * kind) list ->
    string ->
    string ->
    _ ->
    int ->
    b T.typ ->
    bool T.expr list ->
    string option ->
    field * word option * int =
 fun ~var ~label ?(extra = []) name v word off typ own map ->
  let own = List.map (fun e -> (test var e, Constraint)) own in
  match typ with
  | T.Bits { width; base; bit_order } ->
      let w, shift, next =
        place_bits ~off ~word ~base ~order:bit_order ~width
      in
      let store = Bits { base; shift; width } in
      let checks = extra @ own in
      ( { name; label; var = v; off = w.at; store; checks; enum = None; map },
        Some { w with used = w.used + width },
        next )
  | typ ->
      let store, refine = classify typ in
      let refine = List.map (fun (e, k) -> (test var e, k)) (refine name) in
      let checks = refine @ extra @ own in
      (match (store, checks) with
      | Bytes _, _ :: _ -> fail_unsupported "a constraint on a byte array"
      | _ -> ());
      ( { name; label; var = v; off; store; checks; enum = enum_of typ; map },
        None,
        off + width_bytes store )

(* The fields of a fixed record in order, with their offsets, and its size. A
   field under a [map] is laid out as the map's inner type, and only for a map
   the caller names, since its functions are called by that name. *)
let layout ~maps (s : T.struct_) =
  if s.params <> [] then fail_unsupported "the codec has parameters";
  let names = Hashtbl.create 16 in
  let var name =
    match Hashtbl.find_opt names name with
    | Some v -> v
    | None -> fail_unsupported "a reference to %s" name
  in
  let field i word off (T.Field f) =
    let name =
      match f.field_name with
      | Some n -> n
      | None -> fail_unsupported "an anonymous field"
    in
    try
      if Option.is_some f.action then fail_unsupported "an action";
      let label = label name in
      let v = Fmt.str "f%d_%s" i label in
      Hashtbl.replace names name v;
      let own = Option.to_list f.constraint_ in
      match f.field_typ with
      | T.Map { inner; index_bound; _ } as m -> (
          match maps.find m with
          | None ->
              fail_unsupported "a map; name it in ~maps to generate this codec"
          | Some path ->
              let extra =
                Option.to_list
                  (Option.map
                     (fun n ->
                       ({ fast = Fmt.str "(%s < %d)" v n; slow = None }, Tag))
                     index_bound)
              in
              field_of ~var ~label ~extra name v word off inner own (Some path))
      | typ -> field_of ~var ~label name v word off typ own None
    with Unsupported why -> fail_unsupported "field %s: %s" name why
  in
  let rec go i word off acc = function
    | [] -> (List.rev acc, off)
    | f :: rest ->
        let fld, word, off = field i word off f in
        go (i + 1) word off (fld :: acc) rest
  in
  let fields, size = go 0 None 0 [] s.fields in
  (fields, size, Option.map (test var) s.where)

(* Reads. A frame of at least 8 bytes is loaded as big-endian 64-bit words; a
   byte, a big-endian 16-bit value or a bitfield word of either is taken out of
   one by a shift. Other fields are loaded on their own. *)
let in_word f =
  match f.store with
  | Int { width = 8; _ } | Int { width = 16; endian = T.Big; _ } -> true
  | Bits { base = T.U8 | T.U16 T.Big; _ } -> true
  | _ -> false

let covers a f = a <= f.off && f.off + width_bytes f.store <= a + 8

let words ~size fields =
  if size < 8 then []
  else
    List.init ((size + 7) / 8) (fun i -> min (8 * i) (size - 8))
    |> List.filter (fun a ->
        List.exists (fun f -> in_word f && covers a f) fields)

let endian_suffix = function T.Big -> "be" | T.Little -> "le"

(* The buffer the generated code reads: its type, its unchecked accessors, its
   length, and the module of its bounds-checked loads and string copies. *)
type src = { typ : string; u : string; len : string; checked : string }

let bytes_src =
  {
    typ = "bytes";
    u = rt ^ ".Unchecked";
    len = "Bytes.length";
    checked = "Bytes";
  }

let bigstring_src =
  {
    typ = rt ^ ".Unchecked.Bigstring.t";
    u = rt ^ ".Unchecked.Bigstring";
    len = rt ^ ".Unchecked.Bigstring.length";
    checked = rt ^ ".Unchecked.Bigstring";
  }

let load ~src ~off ~bytes ~endian =
  match bytes with
  | 1 -> Fmt.str "(%s.u8 buf (off + %d))" src.u off
  | 2 -> Fmt.str "(%s.u16%s buf (off + %d))" src.u (endian_suffix endian) off
  | _ -> Fmt.str "(%s.i32%s buf (off + %d))" src.u (endian_suffix endian) off

(* An unsigned value of [bytes] bytes at [off], from a word when one covers it. *)
let unsigned ~src ~words ~off ~bytes ~endian =
  match List.find_opt (fun a -> a <= off && off + bytes <= a + 8) words with
  | Some a when bytes = 1 || endian = T.Big ->
      Fmt.str "(Int64.to_int (Int64.shift_right_logical w%d %d) land 0x%x)" a
        (8 * (a + 8 - off - bytes))
        ((1 lsl (8 * bytes)) - 1)
  | _ when bytes = 4 ->
      Fmt.str "(Int32.to_int %s land mask32)" (load ~src ~off ~bytes ~endian)
  | _ -> load ~src ~off ~bytes ~endian

let read ~src ~words f =
  match f.store with
  | Int { width = 32; endian; _ } -> load ~src ~off:f.off ~bytes:4 ~endian
  | Int { width; endian; signed } ->
      let v = unsigned ~src ~words ~off:f.off ~bytes:(width / 8) ~endian in
      if signed then
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
      else v
  | Bits { base; shift; width } ->
      let endian = match base with T.U8 -> T.Big | T.U16 e | T.U32 e -> e in
      let v =
        unsigned ~src ~words ~off:f.off ~bytes:(Bitfield.byte_size base) ~endian
      in
      Fmt.str "((%s lsr %d) land %s)" v shift (mask width)
  | Bytes n -> Fmt.str "(%s.sub_string buf (off + %d) %d)" src.checked f.off n

(* The [int] a test sees of a 32-bit field, read as an [int32]. *)
let int_view f =
  match f.store with
  | Int { width = 32; signed; _ } ->
      Fmt.str "(Int32.to_int %s_raw%s)" f.var
        (if signed then "" else " land mask32")
  | _ -> f.var

let map_var f = "map_" ^ f.var

(* The value handed to the constructor: through the field's map when it has
   one, the failure placed at the field. *)
let typed_value f =
  match (f.map, f.store) with
  | Some _, Int { width = 32; _ } ->
      Fmt.str "(%s.dec ~at:(off + %d) %s_raw)" (map_var f) f.off f.var
  | Some _, _ -> Fmt.str "(%s.dec ~at:(off + %d) %s)" (map_var f) f.off f.var
  | None, _ -> (
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.str "(cast %s.%sInt%d.int_eq %s)" rt
            (if signed then "S" else "U")
            w f.var
      | Int { signed; _ } ->
          Fmt.str "(%s.%s.of_int32 %s_raw)" rt
            (if signed then "SInt32" else "UInt32")
            f.var
      | Bits _ | Bytes _ -> f.var)

let all_tests ~fields ~where =
  List.concat_map tests fields
  @ Option.to_list (Option.map (fun t -> t.fast) where)

let pp_frame ~src ppf on_short =
  Fmt.pf ppf "off < 0 || off > %s buf - size then %s buf off" src.len on_short

let pp_reads ~src ppf ~words fields =
  List.iter
    (fun a ->
      Fmt.pf ppf "    let w%d = %s.i64be buf (off + %d) in@\n" a src.u a)
    words;
  List.iter
    (fun f ->
      match f.store with
      | Int { width = 32; _ } ->
          Fmt.pf ppf "    let %s_raw = %s in@\n" f.var (read ~src ~words f);
          Fmt.pf ppf "    let %s = %s in@\n" f.var (int_view f)
      | _ -> Fmt.pf ppf "    let %s = %s in@\n" f.var (read ~src ~words f))
    fields

(* The attributes the OxCaml target adds: an allocation check on a hot path,
   and on a cold one the promise that it only raises. *)
let zero_alloc = function `OxCaml -> "[@zero_alloc]" | `OCaml -> ""

let cold = function
  | `OxCaml -> "[@inline never][@zero_alloc assume error]"
  | `OCaml -> "[@inline never]"

let pp_tests ppf tests = Fmt.(list ~sep:(any "@\n      && ") string) ppf tests

let pp_decode ?(src = bytes_src) ppf ~head ~call ~size ~fields ~where =
  let words = words ~size fields in
  Fmt.pf ppf "%s =@\n  if %a@\n  else begin@\n" head (pp_frame ~src) "fail";
  pp_reads ~src ppf ~words fields;
  (match all_tests ~fields ~where with
  | [] -> ()
  | tests ->
      Fmt.pf ppf "    if not (%a) then fail buf off else@\n" pp_tests tests);
  Fmt.pf ppf "    %s@\n  end@\n@\n" (call (List.map typed_value fields))

let pp_validate ~src ppf ~target ~fields ~where =
  Fmt.pf ppf "let%s validate buf off =@\n  if %a" (zero_alloc target)
    (pp_frame ~src) "fail";
  match all_tests ~fields ~where with
  | [] -> Fmt.pf ppf "@\n@\n"
  | tests ->
      let fields =
        List.filter
          (fun f -> match f.store with Bytes _ -> false | _ -> true)
          fields
      in
      Fmt.pf ppf "@\n  else begin@\n";
      pp_reads ~src ppf ~words:[] fields;
      Fmt.pf ppf "    ignore (%a);@\n"
        Fmt.(list ~sep:(any ", ") string)
        (List.map (fun f -> f.var) fields);
      Fmt.pf ppf "    if not (%a) then fail buf off@\n  end@\n@\n" pp_tests
        tests

let type_of f =
  let t = Fmt.str "%s.%s.t" rt in
  match f.store with
  | Int { width = 8; signed = false; _ } -> t "UInt8"
  | Int { width = 8; signed = true; _ } -> t "SInt8"
  | Int { width = 16; signed = false; _ } -> t "UInt16"
  | Int { width = 16; signed = true; _ } -> t "SInt16"
  | Int { signed = false; _ } -> t "UInt32"
  | Int { signed = true; _ } -> t "SInt32"
  | Bits _ -> "int"
  | Bytes _ -> "string"

(* Writes take each value in its plain form: an [int] for a field up to 16 bits
   or a bitfield, an [int32] for a 32-bit field, a [string] for a byte array. *)

(* The bits that put a plain value out of its field's range, or-ed with the
   others' so one test, with no branch per field, finds any. *)
let out_of_range f =
  let x = f.var in
  match f.store with
  | Int { width = (8 | 16) as w; signed = false; _ } ->
      Fmt.kstr Option.some "(%s lsr %d)" x w
  | Int { width = (8 | 16) as w; signed = true; _ } ->
      Fmt.kstr Option.some "((%s + %d) lsr %d)" x (1 lsl (w - 1)) w
  | Int _ -> None
  | Bits { width; _ } -> Fmt.kstr Option.some "(%s lsr %d)" x width
  | Bytes n -> Fmt.kstr Option.some "(String.length %s lxor %d)" x n

let member f valid =
  Fmt.str "(%a)"
    Fmt.(
      list ~sep:(any " || ") (fun ppf v -> pf ppf "%s = %s" f.var (int_lit v)))
    valid

(* [first_error] raises what the codec raises for these values: converting each
   to its field's type, in field order; then the frame; then each bitfield's
   width, byte array's length and enum case, in field order. *)
let pp_first_error ~src ppf ~target fields =
  Fmt.pf ppf "let%s first_error buf off %a =@\n" (cold target) pp_vars fields;
  List.iter
    (fun f ->
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.pf ppf "  ignore (%s.%sInt%d.v %s);@\n" rt
            (if signed then "S" else "U")
            w f.var
      | _ -> ())
    fields;
  Fmt.pf ppf "  if %a;@\n" (pp_frame ~src) "too_short";
  List.iter
    (fun f ->
      (match f.store with
      | Bits { width; _ } ->
          Fmt.pf ppf
            "  if %s lsr %d <> 0 then %s.unsigned_too_wide ~bits:%d %s;@\n"
            f.var width errors width f.var
      | Bytes n ->
          Fmt.pf ppf
            "  if String.length %s <> %d then %s.byte_length ~expected:%d \
             ~actual:(String.length %s);@\n"
            f.var n errors n f.var
      | Int _ -> ());
      match f.enum with
      | Some (name, valid) ->
          Fmt.pf ppf
            "  if not %s then %s.enum_encode ~name:%S ~valid:[%a] %s;@\n"
            (member f valid) errors name
            Fmt.(list ~sep:(any "; ") (using int_lit string))
            valid f.var
      | None -> ())
    fields;
  Fmt.pf ppf "  no_error ()@\n@\n"

let store ~src ppf f =
  let o = f.off in
  match f.store with
  | Int { width = 8; _ } ->
      Fmt.pf ppf "    %s.set_u8 buf (off + %d) %s;@\n" src.u o f.var
  | Int { width = 16; endian; _ } ->
      Fmt.pf ppf "    %s.set_u16%s buf (off + %d) %s;@\n" src.u
        (endian_suffix endian) o f.var
  | Int { endian; _ } ->
      Fmt.pf ppf "    %s.set_i32%s buf (off + %d) %s;@\n" src.u
        (endian_suffix endian) o f.var
  | Bits _ -> ()
  | Bytes n ->
      Fmt.pf ppf "    %s.unsafe_blit_string %s 0 buf (off + %d) %d;@\n"
        src.checked f.var o n

(* Each bitfield word, stored once: the values of its fields, each in range,
   shifted into place and or-ed. *)
let bit_words ~src ppf fields =
  let words =
    List.filter_map
      (fun f ->
        match f.store with Bits { base; _ } -> Some (f.off, base) | _ -> None)
      fields
    |> List.sort_uniq compare
  in
  List.iter
    (fun (o, base) ->
      let parts =
        List.filter_map
          (fun f ->
            match f.store with
            | Bits { shift; _ } when f.off = o ->
                Fmt.kstr Option.some "(%s lsl %d)" f.var shift
            | _ -> None)
          fields
      in
      let value = String.concat " lor " parts in
      match base with
      | T.U8 -> Fmt.pf ppf "    %s.set_u8 buf (off + %d) (%s);@\n" src.u o value
      | T.U16 e ->
          Fmt.pf ppf "    %s.set_u16%s buf (off + %d) (%s);@\n" src.u
            (endian_suffix e) o value
      | T.U32 e ->
          Fmt.pf ppf "    %s.set_i32%s buf (off + %d) (Int32.of_int (%s));@\n"
            src.u (endian_suffix e) o value)
    words

let pp_write_int ?(src = bytes_src) ppf ~target ~fields ~where =
  pp_first_error ~src ppf ~target fields;
  Fmt.pf ppf "let[@@inline]%s write_int buf off %a =@\n" (zero_alloc target)
    pp_vars fields;
  let bad = List.filter_map out_of_range fields in
  let guard =
    (match bad with
      | [] -> []
      | _ -> [ Fmt.str "(%s) <> 0" (String.concat " lor " bad) ])
    @ List.filter_map
        (fun f -> Option.map (fun (_, v) -> "not " ^ member f v) f.enum)
        fields
  in
  (match guard with
  | [] -> ()
  | _ ->
      Fmt.pf ppf "  if %s then first_error buf off %a@\n  else"
        (String.concat " || " guard)
        pp_vars fields);
  Fmt.pf ppf "  if %a@\n  else begin@\n" (pp_frame ~src) "too_short";
  bit_words ~src ppf fields;
  List.iter (store ~src ppf) fields;
  (match all_tests ~fields ~where with
  | [] -> Fmt.pf ppf "    ()@\n"
  | _ -> Fmt.pf ppf "    validate_written buf off@\n");
  Fmt.pf ppf "  end@\n@\n"

(* [write] takes the codec's typed values, in range by construction, and
   stores them through [write_int]. *)
let plain_value f =
  match (f.map, f.store) with
  | Some _, _ -> Fmt.str "(%s.enc %s)" (map_var f) f.var
  | None, _ -> (
      match f.store with
      | Int { width = 32; signed; _ } ->
          Fmt.str "(%s.%s.to_int32 %s)" rt
            (if signed then "SInt32" else "UInt32")
            f.var
      | Int _ -> Fmt.str "(%s : %s :> int)" f.var (type_of f)
      | Bits _ | Bytes _ -> f.var)

(* A map's functions are the caller's code, so a write through one makes no
   promise about allocation. *)
let pp_write ppf ~target fields =
  let target =
    if List.exists (fun f -> Option.is_some f.map) fields then `OCaml
    else target
  in
  Fmt.pf ppf "let[@@inline]%s write buf off %a =@\n  write_int buf off %a@\n@\n"
    (zero_alloc target) pp_vars fields pp_list
    (List.map plain_value fields)

(* A 32-bit field is read into an [int], which a target with ints of 32 bits
   or fewer cannot hold, so the module refuses to load there. *)
let wide fields =
  List.exists
    (fun f ->
      match f.store with
      | Int { width = 32; _ } | Bits { base = T.U32 _; _ } -> true
      | Int _ | Bits _ | Bytes _ -> false)
    fields

let pp_wide ppf ~codec fields =
  if wide fields then
    Fmt.pf ppf
      "let () =@\n\
      \  if Sys.int_size <= 32 then@\n\
      \    invalid_arg@\n\
      \      %S@\n\
       @\n\
       let mask32 = Int64.to_int 0xFFFF_FFFFL@\n\
       @\n"
      (Fmt.str
         "Wire_ocaml: the code generated from %s reads a 32-bit field into an \
          int, which needs ints wider than 32 bits"
         codec)

(* The module links wire.runtime and no more: it names the shape of its codec
   with [digest], which only {!bridge} checks, and raises each error itself. *)
let pp_header ppf ~codec ~name ~digest =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from %s. Do not edit: generate it again when \
     the codec changes. *)@\n\
     @\n\
     [@@@@@@warning \"-32-26-27\"]@\n\
     @\n\
     let digest = %S@\n\
     let codec_name = %S@\n\
     let[@@inline] cast : type a. (a, int) Type.eq -> int -> a = fun \
     Type.Equal v -> v@\n\
     @\n\
     let[@@inline never] no_error () =@\n\
    \  invalid_arg (codec_name ^ \": the generated code found no error to \
     report\")@\n\
     @\n"
    codec digest name

(* Each named map, bound once. A map's functions are a [Wire] value, so a
   module with a map links wire; {!bridge} checks that the field still holds
   that very map. *)
let pp_maps ppf fields =
  List.iter
    (fun f ->
      match f.map with
      | Some path ->
          let make =
            match f.store with
            | Int { width = 32; _ } -> "map_int32"
            | _ -> "map_int"
          in
          Fmt.pf ppf "let %s = Wire.Private.Gen.%s %s@\n" (map_var f) make path
      | None -> ())
    fields;
  Fmt.pf ppf "@\n"

let pp_record ppf fields =
  Fmt.pf ppf "type t = {@\n";
  List.iter (fun f -> Fmt.pf ppf "  %s : %s;@\n" f.label (type_of f)) fields;
  Fmt.pf ppf "}@\n@\nlet make %a = { %a }@\n@\n" pp_vars fields
    Fmt.(list ~sep:(any "; ") string)
    (List.map (fun f -> Fmt.str "%s = %s" f.label f.var) fields)

let pp_encode ppf fields =
  Fmt.pf ppf "let encode t buf off = write buf off %a@\n" pp_list
    (List.map (fun f -> "t." ^ f.label) fields)

(* [get_<field> buf off] reads one field as [Codec.get] does, with the same
   bounds-checked load: the compiler's check jumps to a handler that does not
   return, so nothing is kept live across it, and an offset out of range raises
   what [Codec.get] raises. A closed enum's value is then checked to be a case. *)
let checked_load ~src ~off ~bytes ~endian =
  match bytes with
  | 1 -> Fmt.str "(%s.get_uint8 buf (off + %d))" src.checked off
  | 2 ->
      Fmt.str "(%s.get_uint16_%s buf (off + %d))" src.checked
        (endian_suffix endian) off
  | _ ->
      Fmt.str "(%s.get_int32_%s buf (off + %d))" src.checked
        (endian_suffix endian) off

let checked_read ~src f =
  match f.store with
  | Int { width = 32; endian; _ } ->
      checked_load ~src ~off:f.off ~bytes:4 ~endian
  | Int { width; endian; signed } ->
      let v = checked_load ~src ~off:f.off ~bytes:(width / 8) ~endian in
      if signed then
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
      else v
  | Bits { base; shift; width } ->
      let endian = match base with T.U8 -> T.Big | T.U16 e | T.U32 e -> e in
      let bytes = Bitfield.byte_size base in
      let word = checked_load ~src ~off:f.off ~bytes ~endian in
      let word =
        if bytes = 4 then Fmt.str "(Int32.to_int %s land mask32)" word else word
      in
      Fmt.str "((%s lsr %d) land %s)" word shift (mask width)
  | Bytes n -> Fmt.str "(%s.sub_string buf (off + %d) %d)" src.checked f.off n

let pp_getter ~src ppf ~target f =
  let attr =
    match (target, f.store) with
    | `OxCaml, (Int _ | Bits _) when Option.is_none f.map ->
        "[@inline][@zero_alloc]"
    | _ -> "[@inline]"
  in
  Fmt.pf ppf "let%s get_%s buf off =@\n" attr f.label;
  (match f.store with
  | Int { width = 32; _ } ->
      Fmt.pf ppf "  let %s_raw = %s in@\n  let %s = %s in@\n" f.var
        (checked_read ~src f) f.var (int_view f)
  | _ -> Fmt.pf ppf "  let %s = %s in@\n" f.var (checked_read ~src f));
  (match f.enum with
  | Some (_, valid) ->
      Fmt.pf ppf "  if not %s then get_failed (off + %d) %s [%a] else@\n"
        (member f valid) f.off f.var
        Fmt.(list ~sep:(any "; ") (using int_lit string))
        valid
  | None -> ());
  Fmt.pf ppf "  %s@\n@\n" (typed_value f);
  match f.store with
  | Int { width = 32; _ } ->
      (* The [int] view of a 32-bit field, with no conversion through its
         typed form. *)
      Fmt.pf ppf
        "let%s get_%s_int buf off =@\n  let %s_raw = %s in@\n  %s@\n@\n" attr
        f.label f.var (checked_read ~src f) (int_view f)
  | _ -> ()

(* A bitfield's word, read once for all the fields packed in it:
   [get_<field>_word] is the checked load of the word that holds the field, and
   [<field>_of_word] takes the field out of it. *)
let pp_word_getters ~src ppf ~target f =
  match f.store with
  | Bits { base; shift; width } ->
      let attr =
        match target with
        | `OxCaml -> "[@inline][@zero_alloc]"
        | `OCaml -> "[@inline]"
      in
      let endian = match base with T.U8 -> T.Big | T.U16 e | T.U32 e -> e in
      let bytes = Bitfield.byte_size base in
      let word = checked_load ~src ~off:f.off ~bytes ~endian in
      let word =
        if bytes = 4 then Fmt.str "(Int32.to_int %s land mask32)" word else word
      in
      Fmt.pf ppf "let%s get_%s_word buf off = %s@\n@\n" attr f.label word;
      Fmt.pf ppf "let%s %s_of_word w = (w lsr %d) land %s@\n@\n" attr f.label
        shift (mask width)
  | _ -> ()

let pp_getters ?(src = bytes_src) ppf ~target fields =
  Fmt.pf ppf
    "let%s get_failed at value valid =@\n\
    \  %s.invalid_enum ~at ~field:[] ~value ~valid@\n\
     @\n"
    (cold target) errors;
  List.iter (pp_getter ~src ppf ~target) fields;
  List.iter (pp_word_getters ~src ppf ~target) fields

(* [Valid.v buf] validates the record at offset 0. A buffer's length never
   changes, so each [read_<field>] after it is one load at a constant offset
   with no check of its own, and stays in the buffer even if its bytes change;
   only then may it read a value that breaks a constraint. This is EverParse's
   validate-then-read, with the proof a type. *)
let pp_valid ?(src = bytes_src) ppf ~target ~fields ~where =
  Fmt.pf ppf
    "module Valid : sig@\n\
    \  type t = private %s@\n\
     @\n\
    \  val v : %s -> t@\n\
     end = struct@\n\
    \  type t = %s@\n\
     @\n\
    \  let[@inline] v buf =@\n\
    \    if %s buf < size then fail buf 0@\n\
    \    else begin@\n"
    src.typ src.typ src.typ src.len;
  (match all_tests ~fields ~where with
  | [] -> ()
  | _ -> Fmt.pf ppf "      validate buf 0;@\n");
  Fmt.pf ppf "      buf@\n    end@\nend@\n@\n";
  List.iter
    (fun f ->
      let attr =
        match (target, f.store) with
        | `OxCaml, (Int _ | Bits _) when Option.is_none f.map ->
            "[@inline][@zero_alloc]"
        | _ -> "[@inline]"
      in
      Fmt.pf ppf
        "let%s read_%s (v : Valid.t) =@\n\
        \  let buf = (v :> %s) and off = 0 in@\n"
        attr f.label src.typ;
      pp_reads ~src ppf ~words:[] [ f ];
      Fmt.pf ppf "  %s@\n@\n" (typed_value f);
      match f.store with
      | Int { width = 32; _ } ->
          Fmt.pf ppf
            "let%s read_%s_int (v : Valid.t) =@\n\
            \  let buf = (v :> %s) and off = 0 in@\n"
            attr f.label src.typ;
          pp_reads ~src ppf ~words:[] [ f ];
          Fmt.pf ppf "  %s@\n@\n" f.var
      | _ -> ())
    fields

(* What the codec raises for a rejected record, found again off the hot path:
   the frame, then each field's checks in field order, each in the order the
   codec runs them, then the where clause. [fail] runs only once a fast test
   failed, so it always finds an error; [too_short] is the encoder's. *)
let raise_check (f : field) (_, kind) =
  let at = Fmt.str "(off + %d)" f.off in
  match kind with
  | Constraint ->
      Fmt.str "%s.field_constraint ~at:%s ~field:%S %s" errors at f.name f.var
  | Enum valid ->
      Fmt.str "%s.invalid_enum ~at:%s ~field:[ %S ] ~value:%s ~valid:[%a]"
        errors at f.name f.var
        Fmt.(list ~sep:(any "; ") (using int_lit string))
        valid
  | Tag -> Fmt.str "%s.invalid_tag ~at:%s ~field:%S %s" errors at f.name f.var

(* [test] in [fail], an overflow in it raised as found at [at] in [field]. *)
let slow_test ~at ~field t =
  match t.slow with
  | None -> t.fast
  | Some slow ->
      Fmt.str
        "(match %s with b -> b | exception %s.Parse_error e -> %s.relocate \
         ~at:%s ~field:[%a] e)"
        slow errors errors at
        Fmt.(list ~sep:(any "; ") (fmt "%S"))
        field

let pp_cold ~src ppf ~target ~fields ~where =
  Fmt.pf ppf
    "let%s fail buf off =@\n\
    \  if off < 0 || off > %s buf - size then@\n\
    \    %s.frame ~size ~len:(%s buf) off;@\n"
    (cold target) src.len errors src.len;
  let read =
    List.filter
      (fun f -> match f.store with Bytes _ -> false | _ -> true)
      fields
  in
  pp_reads ~src ppf ~words:[] read;
  List.iter
    (fun (f : field) ->
      List.iter
        (fun ((test, _) as c) ->
          let at = Fmt.str "(off + %d)" f.off in
          Fmt.pf ppf "    if not %s then %s;@\n"
            (slow_test ~at ~field:[ f.name ] test)
            (raise_check f c))
        f.checks)
    fields;
  Option.iter
    (fun w ->
      Fmt.pf ppf "    if not %s then %s.where_clause ~at:off;@\n"
        (slow_test ~at:"off" ~field:[] w)
        errors)
    where;
  Fmt.pf ppf
    "    no_error ()@\n\
     @\n\
     let%s too_short buf off =@\n\
    \  %s.encode_too_short ~name:codec_name ~size ~len:(%s buf) off@\n\
     @\n"
    (cold target) errors src.len

(* A record the encoder wrote that fails its own constraints. *)
let pp_validate_written ppf ~target =
  Fmt.pf ppf
    "let%s validate_written buf off =@\n\
    \  match validate buf off with@\n\
    \  | () -> ()@\n\
    \  | exception %s.Parse_error e -> %s.encode_rejected ~name:codec_name e@\n\
     @\n"
    (match target with
    | `OxCaml -> "[@inline never][@zero_alloc assume]"
    | `OCaml -> "[@inline never]")
    errors errors

(* [decode_local] builds the generated record in the caller's frame, so a
   decode of integer fields allocates nothing on the heap; a byte array is
   still a string on the heap, so with one it makes no such promise.
   [decode_with] takes the constructor as an argument, which flambda2 inlines
   with it. *)
let pp_oxcaml ppf ~record ~fields ~size ~where =
  pp_decode ppf ~head:"let[@inline] decode_with make buf off"
    ~call:(fun args -> Fmt.str "make %a" pp_list args)
    ~size ~fields ~where;
  let bytes =
    List.exists
      (fun f -> match f.store with Bytes _ -> true | _ -> false)
      fields
  in
  if record then
    pp_decode ppf
      ~head:
        (Fmt.str "let%s decode_local buf off"
           (if bytes then "" else "[@zero_alloc]"))
      ~call:(fun args ->
        Fmt.str "exclave_ stack_ { %a }"
          Fmt.(list ~sep:(any "; ") string)
          (List.map2 (fun f a -> Fmt.str "%s = %s" f.label a) fields args))
      ~size ~fields ~where

(* The same reads and writes on a bigarray of bytes, with the same errors. *)
let pp_bigstring ppf ~target ~make ~record ~fields ~size ~where =
  let src = bigstring_src in
  Fmt.pf ppf "module Bigstring = struct@\n";
  pp_cold ~src ppf ~target ~fields ~where;
  pp_validate ~src ppf ~target ~fields ~where;
  pp_validate_written ppf ~target;
  pp_decode ~src ppf ~head:"let decode buf off"
    ~call:(fun args -> Fmt.str "%s %a" make pp_list args)
    ~size ~fields ~where;
  pp_write_int ~src ppf ~target ~fields ~where;
  pp_write ppf ~target fields;
  pp_getters ~src ppf ~target fields;
  pp_valid ~src ppf ~target ~fields ~where;
  if record then pp_encode ppf fields;
  Fmt.pf ppf "end@\n@\n"

let pp_body ppf ~target ~make ~fields ~size ~where =
  Fmt.pf ppf "let size = %d@\n@\n" size;
  let record = Option.is_none make in
  if record then pp_record ppf fields;
  pp_cold ~src:bytes_src ppf ~target ~fields ~where;
  pp_validate ~src:bytes_src ppf ~target ~fields ~where;
  pp_validate_written ppf ~target;
  let make = Option.value ~default:"make" make in
  pp_decode ppf ~head:"let decode buf off"
    ~call:(fun args -> Fmt.str "%s %a" make pp_list args)
    ~size ~fields ~where;
  if target = `OxCaml then pp_oxcaml ppf ~record ~fields ~size ~where;
  pp_write_int ppf ~target ~fields ~where;
  pp_write ppf ~target fields;
  pp_getters ppf ~target fields;
  pp_valid ppf ~target ~fields ~where;
  if record then pp_encode ppf fields;
  pp_bigstring ppf ~target ~make ~record ~fields ~size ~where

let refuse ~codec reason =
  Fmt.invalid_arg "Wire_ocaml: cannot specialise %s: %s" codec reason

(* The names the generated module defines for itself, at top level or in
   [Bigstring]. *)
let own_names =
  [
    "digest";
    "codec_name";
    "cast";
    "no_error";
    "fail";
    "too_short";
    "size";
    "validate";
    "validate_written";
    "decode";
    "decode_with";
    "decode_local";
    "first_error";
    "write_int";
    "write";
    "get_failed";
    "make";
    "encode";
    "t";
    "Valid";
    "Bigstring";
    "mask32";
  ]

(* The names a field's functions and record field take. *)
let names_of f =
  let l = f.label in
  let derived =
    match f.store with
    | Int { width = 32; _ } -> [ "get_" ^ l ^ "_int"; "read_" ^ l ^ "_int" ]
    | Bits _ -> [ "get_" ^ l ^ "_word"; l ^ "_of_word" ]
    | Int _ | Bytes _ -> []
  in
  ("get_" ^ l) :: ("read_" ^ l) :: derived

(* Who defines a name: the generated module itself, or a field. *)
type source = Own | Field of string

(* A later definition would shadow an earlier one of the same name without a
   word, as the generated code silences unused values. Two fields of one label
   also define one getter, so this covers the record's fields too. *)
let check_names ~codec fields =
  let own = List.map (fun n -> (Own, n)) own_names in
  let defined =
    List.concat_map
      (fun f -> List.map (fun n -> (Field f.name, n)) (names_of f))
      fields
  in
  match Wire.Private.Names.clash (own @ defined) with
  | Some (Own, Field f, n) | Some (Field f, Own, n) ->
      Fmt.kstr (refuse ~codec)
        "field %S defines %s, which the generated module defines for itself" f n
  | Some (Field g, Field f, n) ->
      Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
  | Some (Own, Own, _) | None -> ()

let pp ?(target = `OCaml) ?make ?(maps = []) ~codec ppf c =
  let digest = Wire.Private.Gen.digest c in
  match layout ~maps:(maps_of maps) (Wire.Private.Gen.to_struct c) with
  | exception Unsupported reason -> refuse ~codec reason
  | _, size, _ when Wire.Codec.wire_size_opt c <> Some size ->
      refuse ~codec "its size is not fixed"
  | fields, _, _
    when Option.is_none make
         && List.exists (fun f -> Option.is_some f.map) fields ->
      refuse ~codec
        "a field is a map, and the generated record cannot name its type; pass \
         ~make"
  | fields, size, where ->
      check_names ~codec fields;
      pp_header ppf ~codec ~name:(Wire.Private.Gen.to_struct c).name ~digest;
      pp_wide ppf ~codec fields;
      pp_maps ppf fields;
      pp_body ppf ~target ~make ~fields ~size ~where

let ident = label

let ml ?target ?make ?maps ~codec c =
  Fmt.str "%a" (pp ?target ?make ?maps ~codec) c

let pp_bridge ?(maps = []) ~codec ~core ppf c =
  let fields =
    match layout ~maps:(maps_of maps) (Wire.Private.Gen.to_struct c) with
    | fields, _, _ -> fields
    | exception Unsupported reason -> refuse ~codec reason
  in
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from %s. Do not edit. Checks, when it loads, \
     that %s still has the shape %s was generated from. *)@\n\
     @\n\
     let () = Wire.Private.Gen.check %s %s.digest@\n"
    codec codec core codec core;
  List.iter
    (fun f ->
      Option.iter
        (fun path ->
          Fmt.pf ppf "let () = Wire.Private.Gen.check_map %s %S %s@\n" codec
            f.name path)
        f.map)
    fields

let bridge ?maps ~codec ~core c = Fmt.str "%a" (pp_bridge ?maps ~codec ~core) c
