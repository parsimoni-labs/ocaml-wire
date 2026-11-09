open Wire.Private
module T = Types

exception Unsupported of string

let fail_unsupported fmt = Fmt.kstr (fun s -> raise (Unsupported s)) fmt

(* How a field is stored. Integers up to 16 bits and bitfields are handled as
   an [int], 32-bit integers as an [int32], byte arrays as a [string]. *)
type store =
  | Int of { width : int; endian : T.endian; signed : bool }
  | Bits of { base : T.bitfield_base; shift : int; width : int }
  | Bytes of int

(* A field of the record: its name, the variable that holds its plain value,
   where it sits, how it is stored, the OCaml tests its value must pass, and
   the closed enum the encoder checks it against. *)
type field = {
  name : string;
  var : string;
  off : int;
  store : store;
  tests : string list;
  enum : (string * int list) option;
}

let label name =
  let l =
    String.map
      (function
        | ('a' .. 'z' | '0' .. '9' | '_') as c -> c
        | 'A' .. 'Z' as c -> Char.lowercase_ascii c
        | _ -> '_')
      name
  in
  if l = "" || (l.[0] >= '0' && l.[0] <= '9') then "f_" ^ l else l

let pp_list = Fmt.(list ~sep:(any " ") string)
let pp_vars ppf fields = pp_list ppf (List.map (fun f -> f.var) fields)

(* An OCaml expression for [e], over the plain values of the fields read so
   far, which are what the codec's constraints see. *)
let rec expr : type a. (string -> string) -> a T.expr -> string =
 fun var e ->
  let bin op a b = Fmt.str "(%s %s %s)" (expr var a) op (expr var b) in
  match e with
  | T.Int n -> if n < 0 then Fmt.str "(%d)" n else string_of_int n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, name) -> var name
  | T.Add (a, b) -> bin "+" a b
  | T.Sub (a, b) -> bin "-" a b
  | T.Mul (a, b) -> bin "*" a b
  | T.Land (a, b) -> bin "land" a b
  | T.Lor (a, b) -> bin "lor" a b
  | T.Lxor (a, b) -> bin "lxor" a b
  | T.Lnot a -> Fmt.str "(lnot %s)" (expr var a)
  | T.Lsl (a, b) -> bin "lsl" a b
  | T.Lsr (a, b) -> bin "lsr" a b
  | T.Eq (a, b) -> cmp var "=" a b
  | T.Ne (a, b) -> cmp var "<>" a b
  | T.Lt (a, b) -> cmp var "<" a b
  | T.Le (a, b) -> cmp var "<=" a b
  | T.Gt (a, b) -> cmp var ">" a b
  | T.Ge (a, b) -> cmp var ">=" a b
  | T.And (a, b) -> bin "&&" a b
  | T.Or (a, b) -> bin "||" a b
  | T.Not a -> Fmt.str "(not %s)" (expr var a)
  | T.Cast (`U8, a) -> Fmt.str "(%s land 0xff)" (expr var a)
  | T.Cast (`U16, a) -> Fmt.str "(%s land 0xffff)" (expr var a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s then %s else %s)" (expr var c) (expr var a) (expr var b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* Comparisons are typed, so the compiler emits a machine comparison. *)
and cmp : type a. (string -> string) -> string -> a T.expr -> a T.expr -> string
    =
 fun var op a b ->
  let ty : type a. a T.expr -> string = function
    | T.Bool _ | T.Eq _ | T.Ne _ | T.Lt _ | T.Le _ | T.Gt _ | T.Ge _ | T.And _
    | T.Or _ | T.Not _ ->
        "bool"
    | _ -> "int"
  in
  Fmt.str "((%s : %s) %s %s)" (expr var a) (ty a) op (expr var b)

(* A field's store, and the conditions its type adds on its value, which a
   field's own constraint refers to by the field's name. *)
let rec classify : type a. a T.typ -> store * (string -> bool T.expr list) =
 fun typ ->
  let none _ = [] in
  let int width endian signed = (Int { width; endian; signed }, none) in
  match typ with
  | T.Uint8 -> int 8 T.Big false
  | T.Int8 -> int 8 T.Big true
  | T.Uint16 e -> int 16 e false
  | T.Int16 e -> int 16 e true
  | T.Uint32 e ->
      if Sys.int_size <= 32 then fail_unsupported "uint32 on this host";
      int 32 e false
  | T.Int32 e -> int 32 e true
  | T.Byte_array { size = T.Int n } when n >= 0 -> (Bytes n, none)
  | T.Where { cond; inner } ->
      let s, t = classify inner in
      (s, fun self -> cond :: t self)
  | T.Enum { cases; base; closed = true; _ } ->
      let s, t = classify base in
      let member self =
        List.fold_left
          (fun acc (_, v) ->
            let eq = T.(Eq (Ref (I, self), Int v)) in
            match acc with None -> Some eq | Some a -> Some T.(Or (a, eq)))
          None cases
      in
      (s, fun self -> Option.to_list (member self) @ t self)
  | T.Enum { base; closed = false; _ } -> classify base
  | _ -> fail_unsupported "field type %a" T.pp_typ typ

let rec enum_of : type a. a T.typ -> (string * int list) option = function
  | T.Enum { name; cases; closed = true; _ } -> Some (name, List.map snd cases)
  | T.Where { inner; _ } -> enum_of inner
  | _ -> None

let width_bytes = function
  | Int { width; _ } -> width / 8
  | Bits { base; _ } -> Bitfield.byte_size base
  | Bytes n -> n

(* The bitfield word being filled: its base, bit order, offset and the bits
   taken so far. *)
type word = {
  base : T.bitfield_base;
  order : T.bit_order;
  at : int;
  used : int;
}

(* A bitfield goes into the current word when it has the same base and bit
   order and still fits, as the codec packs it; else it opens a new word. *)
let place_bits ~off ~word ~base ~order ~width =
  let total = Bitfield.total_bits base in
  let w =
    match word with
    | Some w
      when Bitfield.equal w.base base && w.order = order
           && w.used + width <= total ->
        w
    | _ -> { base; order; at = off; used = 0 }
  in
  let shift = Bitfield.shift ~bit_order:order ~total ~bits_used:w.used ~width in
  let next = if w.used = 0 then off + Bitfield.byte_size base else off in
  (w, shift, next)

(* The fields of a fixed record in order, with their offsets, and its size. *)
let layout (s : T.struct_) =
  if s.params <> [] then fail_unsupported "parameters";
  let names = Hashtbl.create 16 in
  let var name =
    match Hashtbl.find_opt names name with
    | Some v -> v
    | None -> fail_unsupported "reference to %s" name
  in
  let field i word off (T.Field f) =
    if Option.is_some f.action then fail_unsupported "actions";
    let name =
      match f.field_name with
      | Some n -> n
      | None -> fail_unsupported "anonymous fields"
    in
    let v = Fmt.str "f%d_%s" i (label name) in
    Hashtbl.replace names name v;
    let own = Option.to_list f.constraint_ in
    match f.field_typ with
    | T.Bits { base = T.U32 _; _ } -> fail_unsupported "32-bit bitfields"
    | T.Bits { width; base; bit_order } ->
        let w, shift, next =
          place_bits ~off ~word ~base ~order:bit_order ~width
        in
        let store = Bits { base; shift; width } in
        let tests = List.map (expr var) own in
        ( { name; var = v; off = w.at; store; tests; enum = None },
          Some { w with used = w.used + width },
          next )
    | typ ->
        let store, refine = classify typ in
        let conds = refine name @ own in
        (match (store, conds) with
        | Bytes _, _ :: _ -> fail_unsupported "a constraint on a byte array"
        | _ -> ());
        let tests = List.map (expr var) conds in
        ( { name; var = v; off; store; tests; enum = enum_of typ },
          None,
          off + width_bytes store )
  in
  let rec go i word off acc = function
    | [] -> (List.rev acc, off)
    | f :: rest ->
        let fld, word, off = field i word off f in
        go (i + 1) word off (fld :: acc) rest
  in
  let fields, size = go 0 None 0 [] s.fields in
  (fields, size, Option.map (expr var) s.where)

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

let load ~off ~bytes ~endian =
  match bytes with
  | 1 -> Fmt.str "(Wire.Private.Unchecked.u8 buf (off + %d))" off
  | 2 ->
      Fmt.str "(Wire.Private.Unchecked.u16%s buf (off + %d))"
        (endian_suffix endian) off
  | _ ->
      Fmt.str "(Wire.Private.Unchecked.i32%s buf (off + %d))"
        (endian_suffix endian) off

(* An unsigned value of [bytes] bytes at [off], from a word when one covers it. *)
let unsigned ~words ~off ~bytes ~endian =
  match List.find_opt (fun a -> a <= off && off + bytes <= a + 8) words with
  | Some a when bytes = 1 || endian = T.Big ->
      Fmt.str "(Int64.to_int (Int64.shift_right_logical w%d %d) land 0x%x)" a
        (8 * (a + 8 - off - bytes))
        ((1 lsl (8 * bytes)) - 1)
  | _ -> load ~off ~bytes ~endian

let read ~words f =
  match f.store with
  | Int { width = 32; endian; _ } -> load ~off:f.off ~bytes:4 ~endian
  | Int { width; endian; signed } ->
      let v = unsigned ~words ~off:f.off ~bytes:(width / 8) ~endian in
      if signed then
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
      else v
  | Bits { base; shift; width } ->
      let endian = match base with T.U8 -> T.Big | T.U16 e | T.U32 e -> e in
      let v =
        unsigned ~words ~off:f.off ~bytes:(Bitfield.byte_size base) ~endian
      in
      Fmt.str "((%s lsr %d) land 0x%x)" v shift ((1 lsl width) - 1)
  | Bytes n -> Fmt.str "(Bytes.sub_string buf (off + %d) %d)" f.off n

(* The [int] a test sees of a 32-bit field, read as an [int32]. *)
let int_view f =
  match f.store with
  | Int { width = 32; signed; _ } ->
      Fmt.str "(Int32.to_int %s_raw%s)" f.var
        (if signed then "" else " land 0xffff_ffff")
  | _ -> f.var

(* The value handed to the constructor. *)
let typed_value f =
  match f.store with
  | Int { width = 8; signed; _ } ->
      Fmt.str "(Wire.Private.Gen.%s %s)"
        (if signed then "int8" else "uint8")
        f.var
  | Int { width = 16; signed; _ } ->
      Fmt.str "(Wire.Private.Gen.%s %s)"
        (if signed then "int16" else "uint16")
        f.var
  | Int { signed; _ } ->
      Fmt.str "(Wire.%s.of_int32 %s_raw)"
        (if signed then "SInt32" else "UInt32")
        f.var
  | Bits _ | Bytes _ -> f.var

let all_tests ~fields ~where =
  List.concat_map (fun f -> f.tests) fields @ Option.to_list where

let pp_frame ppf on_short =
  Fmt.pf ppf "off < 0 || off > Bytes.length buf - size then %s buf off" on_short

let pp_reads ppf ~words fields =
  List.iter
    (fun a ->
      Fmt.pf ppf
        "    let w%d = Wire.Private.Unchecked.i64be buf (off + %d) in@\n" a a)
    words;
  List.iter
    (fun f ->
      match f.store with
      | Int { width = 32; _ } ->
          Fmt.pf ppf "    let %s_raw = %s in@\n" f.var (read ~words f);
          Fmt.pf ppf "    let %s = %s in@\n" f.var (int_view f)
      | _ -> Fmt.pf ppf "    let %s = %s in@\n" f.var (read ~words f))
    fields

let pp_tests ppf tests = Fmt.(list ~sep:(any "@\n      && ") string) ppf tests

let pp_decode ppf ~size ~make ~fields ~where =
  let words = words ~size fields in
  Fmt.pf ppf "let decode buf off =@\n  if %a@\n  else begin@\n" pp_frame "fail";
  pp_reads ppf ~words fields;
  (match all_tests ~fields ~where with
  | [] -> ()
  | tests ->
      Fmt.pf ppf "    if not (%a) then fail buf off else@\n" pp_tests tests);
  Fmt.pf ppf "    %s %a@\n  end@\n@\n" make pp_list
    (List.map typed_value fields)

let pp_validate ppf ~fields ~where =
  Fmt.pf ppf "let validate buf off =@\n  if %a" pp_frame "fail";
  match all_tests ~fields ~where with
  | [] -> Fmt.pf ppf "@\n@\n"
  | tests ->
      let fields =
        List.filter
          (fun f -> match f.store with Bytes _ -> false | _ -> true)
          fields
      in
      Fmt.pf ppf "@\n  else begin@\n";
      pp_reads ppf ~words:[] fields;
      Fmt.pf ppf "    ignore (%a);@\n"
        Fmt.(list ~sep:(any ", ") string)
        (List.map (fun f -> f.var) fields);
      Fmt.pf ppf "    if not (%a) then fail buf off@\n  end@\n@\n" pp_tests
        tests

let type_of f =
  match f.store with
  | Int { width = 8; signed = false; _ } -> "Wire.UInt8.t"
  | Int { width = 8; signed = true; _ } -> "Wire.SInt8.t"
  | Int { width = 16; signed = false; _ } -> "Wire.UInt16.t"
  | Int { width = 16; signed = true; _ } -> "Wire.SInt16.t"
  | Int { signed = false; _ } -> "Wire.UInt32.t"
  | Int { signed = true; _ } -> "Wire.SInt32.t"
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
    Fmt.(list ~sep:(any " || ") (fun ppf v -> pf ppf "%s = %d" f.var v))
    valid

(* [first_error] raises what the codec raises for these values: converting each
   to its field's type, in field order; then the frame; then each bitfield's
   width, byte array's length and enum case, in field order. *)
let pp_first_error ppf fields =
  Fmt.pf ppf "let[@@inline never] first_error buf off %a =@\n" pp_vars fields;
  List.iter
    (fun f ->
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.pf ppf "  ignore (Wire.%sInt%d.v %s);@\n"
            (if signed then "S" else "U")
            w f.var
      | _ -> ())
    fields;
  Fmt.pf ppf "  if %a;@\n" pp_frame "Wire.Private.Gen.encode_too_short codec";
  List.iter
    (fun f ->
      (match f.store with
      | Bits { width; _ } ->
          Fmt.pf ppf
            "  if %s lsr %d <> 0 then Wire.Private.Gen.bits_too_wide \
             ~mask:0x%x %s;@\n"
            f.var width
            ((1 lsl width) - 1)
            f.var
      | Bytes n ->
          Fmt.pf ppf
            "  if String.length %s <> %d then Wire.Private.Gen.byte_length \
             ~expected:%d ~actual:(String.length %s);@\n"
            f.var n n f.var
      | Int _ -> ());
      match f.enum with
      | Some (name, valid) ->
          Fmt.pf ppf
            "  if not %s then Wire.Private.Types.check_enum_encode ~name:%S \
             ~valid:[%a] %s;@\n"
            (member f valid) name
            Fmt.(list ~sep:(any "; ") int)
            valid f.var
      | None -> ())
    fields;
  Fmt.pf ppf "  Wire.Private.Gen.disagrees codec@\n@\n"

let store ppf f =
  let o = f.off in
  match f.store with
  | Int { width = 8; _ } ->
      Fmt.pf ppf "    Wire.Private.Unchecked.set_u8 buf (off + %d) %s;@\n" o
        f.var
  | Int { width = 16; endian; _ } ->
      Fmt.pf ppf "    Wire.Private.Unchecked.set_u16%s buf (off + %d) %s;@\n"
        (endian_suffix endian) o f.var
  | Int { endian; _ } ->
      Fmt.pf ppf "    Wire.Private.Unchecked.set_i32%s buf (off + %d) %s;@\n"
        (endian_suffix endian) o f.var
  | Bits _ -> ()
  | Bytes n ->
      Fmt.pf ppf "    Bytes.unsafe_blit_string %s 0 buf (off + %d) %d;@\n" f.var
        o n

(* Each bitfield word, stored once: the values of its fields, each in range,
   shifted into place and or-ed. *)
let bit_words ppf fields =
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
      let suffix =
        match base with
        | T.U8 -> "8"
        | T.U16 e | T.U32 e -> "16" ^ endian_suffix e
      in
      Fmt.pf ppf "    Wire.Private.Unchecked.set_u%s buf (off + %d) (%s);@\n"
        suffix o
        (String.concat " lor " parts))
    words

let pp_write_int ppf ~fields ~where =
  pp_first_error ppf fields;
  Fmt.pf ppf "let[@@inline] write_int buf off %a =@\n" pp_vars fields;
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
  Fmt.pf ppf "  if %a@\n  else begin@\n" pp_frame
    "Wire.Private.Gen.encode_too_short codec";
  bit_words ppf fields;
  List.iter (store ppf) fields;
  (match all_tests ~fields ~where with
  | [] -> Fmt.pf ppf "    ()@\n"
  | _ -> Fmt.pf ppf "    validate_written buf off@\n");
  Fmt.pf ppf "  end@\n@\n"

(* [write] takes the codec's typed values, in range by construction, and
   stores them through [write_int]. *)
let plain_value f =
  match f.store with
  | Int { width = 32; signed; _ } ->
      Fmt.str "(Wire.%s.to_int32 %s)"
        (if signed then "SInt32" else "UInt32")
        f.var
  | Int _ -> Fmt.str "(%s : %s :> int)" f.var (type_of f)
  | Bits _ | Bytes _ -> f.var

let pp_write ppf fields =
  Fmt.pf ppf "let[@@inline] write buf off %a =@\n  write_int buf off %a@\n@\n"
    pp_vars fields pp_list
    (List.map plain_value fields)

let pp_header ppf ~codec ~digest =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from %s. Do not edit: generate it again when \
     the codec changes. *)@\n\
     @\n\
     [@@@@@@warning \"-32-26-27\"]@\n\
     @\n\
     let codec = %s@\n\
     let () = Wire.Private.Gen.check codec %S@\n\
     let[@@inline never] fail buf off = Wire.Private.Gen.fail codec buf off@\n\
     @\n"
    codec codec digest

let fallback ppf ~codec ~digest reason =
  pp_header ppf ~codec ~digest;
  Fmt.pf ppf
    "(* Not specialised: %s. *)@\n\
     @\n\
     let size = Wire.Codec.min_wire_size codec@\n\
     let decode buf off = Wire.Codec.decode_exn codec buf off@\n\
     let validate buf off = Wire.Codec.validate codec buf off@\n"
    reason

let pp_record ppf fields =
  Fmt.pf ppf "type t = {@\n";
  List.iter
    (fun f -> Fmt.pf ppf "  %s : %s;@\n" (label f.name) (type_of f))
    fields;
  Fmt.pf ppf "}@\n@\nlet make %a = { %a }@\n@\n" pp_vars fields
    Fmt.(list ~sep:(any "; ") string)
    (List.map (fun f -> Fmt.str "%s = %s" (label f.name) f.var) fields)

let pp_encode ppf fields =
  Fmt.pf ppf "let encode t buf off = write buf off %a@\n" pp_list
    (List.map (fun f -> "t." ^ label f.name) fields)

let pp_body ppf ~make ~fields ~size ~where =
  Fmt.pf ppf "let size = %d@\n@\n" size;
  let record = Option.is_none make in
  if record then pp_record ppf fields;
  pp_validate ppf ~fields ~where;
  Fmt.pf ppf
    "let[@@inline never] validate_written buf off =@\n\
    \  match validate buf off with@\n\
    \  | () -> ()@\n\
    \  | exception Wire.Parse_error _ -> Wire.Private.Gen.encode_rejected \
     codec buf off@\n\
     @\n";
  pp_decode ppf ~size ~make:(Option.value ~default:"make" make) ~fields ~where;
  pp_write_int ppf ~fields ~where;
  pp_write ppf fields;
  if record then pp_encode ppf fields

let pp ?make ~codec ppf c =
  let digest = Wire.Private.Gen.digest c in
  match layout (Wire.Private.Gen.to_struct c) with
  | exception Unsupported reason -> fallback ppf ~codec ~digest reason
  | _, size, _ when Wire.Codec.wire_size_opt c <> Some size ->
      fallback ppf ~codec ~digest "its size is not the layout's"
  | fields, size, where ->
      pp_header ppf ~codec ~digest;
      pp_body ppf ~make ~fields ~size ~where

let ml ?make ~codec c = Fmt.str "%a" (pp ?make ~codec) c
