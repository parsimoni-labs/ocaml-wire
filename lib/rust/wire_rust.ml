open Wire.Private
open Layout
module T = Types

exception Unsupported of string

let fail_unsupported fmt = Fmt.kstr (fun s -> raise (Unsupported s)) fmt

(* How a field is stored is a [Layout.store], the codec's layout. *)

(* A check on a field's value, in the order the codec makes them: a
   refinement of its type or its own constraint, or the membership of a closed
   enum. *)
type test = Constraint of string | Enum of int list

(* A field of the record: its name, the Rust name its functions and record
   field derive from, the variable that holds its value as an [i64] in the
   checks, where it sits, how it is stored, its checks and the cases of its
   closed enum. *)
type field = {
  name : string;
  lit : string;
  doc : string;
  label : string;
  var : string;
  off : int;
  store : store;
  tests : test list;
  enum : int list option;
}

let ident = Wire.Private.Names.rust_ident

(* A Rust string literal of [s]. Rust's are UTF-8, so a name that is not
   cannot be spelled. *)
let rust_string s =
  if not (String.is_valid_utf_8 s) then
    fail_unsupported "the name %S, which is not UTF-8" s;
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 0x20 || Char.code c = 0x7f ->
          let hex = "0123456789abcdef" in
          Buffer.add_string b "\\x";
          Buffer.add_char b hex.[Char.code c lsr 4];
          Buffer.add_char b hex.[Char.code c land 0xf]
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

(* [s] as it can appear in a comment: on one line, its bytes escaped. *)
let doc_string s = String.escaped s

(* {1 Expressions}

   A constraint is OCaml arithmetic on 63-bit [int]s, which the codec checks:
   a sum, difference or product that leaves them is an error. The generated
   code computes on [i64] values that always hold such an [int], so [add],
   [sub] and [mul] return a [Result] and the shifts follow what OCaml's native
   [lsl] and [lsr] do on the tagged integer. *)

let int_lit n = if n < 0 then Fmt.str "(%di64)" n else Fmt.str "%di64" n

let rec expr : type a. (string -> string) -> a T.expr -> string =
 fun var e ->
  let op o a b = Fmt.str "(%s %s %s)" (expr var a) o (expr var b) in
  let call f a b = Fmt.str "%s(%s, %s)" f (expr var a) (expr var b) in
  match e with
  | T.Int n -> int_lit n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, name) -> var name
  | T.Add (a, b) -> call "add" a b ^ "?"
  | T.Sub (a, b) -> call "sub" a b ^ "?"
  | T.Mul (a, b) -> call "mul" a b ^ "?"
  | T.Land (a, b) -> op "&" a b
  | T.Lor (a, b) -> op "|" a b
  | T.Lxor (a, b) -> op "^" a b
  | T.Lnot a -> Fmt.str "(!%s)" (expr var a)
  | T.Lsl (a, b) -> call "lsl" a b
  | T.Lsr (a, b) -> call "lsr" a b
  | T.Eq (a, b) -> op "==" a b
  | T.Ne (a, b) -> op "!=" a b
  | T.Lt (a, b) -> op "<" a b
  | T.Le (a, b) -> op "<=" a b
  | T.Gt (a, b) -> op ">" a b
  | T.Ge (a, b) -> op ">=" a b
  | T.And (a, b) -> op "&&" a b
  | T.Or (a, b) -> op "||" a b
  | T.Not a -> Fmt.str "(!%s)" (expr var a)
  | T.Cast (`U8, a) -> Fmt.str "(%s & 0xff)" (expr var a)
  | T.Cast (`U16, a) -> Fmt.str "(%s & 0xffff)" (expr var a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s { %s } else { %s })" (expr var c) (expr var a)
        (expr var b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* {1 Layout} *)

(* How a field of type [typ] is stored. Its checks are [type_tests]. *)
let rec classify : type a. a T.typ -> store = function
  | T.Where { inner; _ } -> classify inner
  | T.Enum { base; _ } -> classify base
  | T.Map _ -> fail_unsupported "a map, which Rust code cannot call"
  | typ -> (
      match leaf typ with
      | Some s -> s
      | None -> fail_unsupported "its type, %a" T.pp_typ typ)

let rec enum_of : type a. a T.typ -> int list option = function
  | T.Enum { cases; closed = true; _ } -> Some (List.map snd cases)
  | T.Where { inner; _ } -> enum_of inner
  | _ -> None

(* The checks a field's type makes on its value, outermost first: a
   refinement, then the membership of a closed enum. *)
let rec type_tests : type a. (string -> string) -> a T.typ -> test list =
 fun var typ ->
  match typ with
  | T.Where { cond; inner } ->
      Constraint (expr var cond) :: type_tests var inner
  | T.Enum { cases; base; closed = true; _ } ->
      Enum (List.map snd cases) :: type_tests var base
  | T.Enum { base; closed = false; _ } -> type_tests var base
  | _ -> []

let field_of : type b.
    var:(string -> string) ->
    label:string ->
    string ->
    string ->
    word option ->
    int ->
    b T.typ ->
    bool T.expr list ->
    field * word option * int =
 fun ~var ~label name v word off typ own ->
  let own = List.map (fun c -> Constraint (expr var c)) own in
  match typ with
  | T.Bits { width; base; bit_order } ->
      let w, shift, next =
        place_bits ~off ~word ~base ~order:bit_order ~width
      in
      let store = Bits { base; shift; width } in
      ( {
          name;
          lit = rust_string name;
          doc = doc_string name;
          label;
          var = v;
          off = w.at;
          store;
          tests = own;
          enum = None;
        },
        Some { w with used = w.used + width },
        next )
  | typ ->
      let store = classify typ in
      let tests = type_tests var typ @ own in
      (match (store, tests) with
      | Bytes _, _ :: _ -> fail_unsupported "a constraint on a byte array"
      | _ -> ());
      ( {
          name;
          lit = rust_string name;
          doc = doc_string name;
          label;
          var = v;
          off;
          store;
          tests;
          enum = enum_of typ;
        },
        None,
        off + width_bytes store )

(* The fields of a fixed record in order, with their offsets, its size and its
   [where] clause. A constraint may refer to a field before it, or to itself. *)
let layout (s : T.struct_) =
  if s.params <> [] then fail_unsupported "the codec has parameters";
  let names = Hashtbl.create 16 in
  let var name =
    match Hashtbl.find_opt names name with
    | Some (Some v) -> v
    | Some None -> fail_unsupported "a reference to the byte array %s" name
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
      let label = ident name in
      let v = Fmt.str "f%d_%s" i label in
      (match f.field_typ with
      | T.Byte_array _ -> Hashtbl.replace names name None
      | _ -> Hashtbl.replace names name (Some v));
      let own = Option.to_list f.constraint_ in
      field_of ~var ~label name v word off f.field_typ own
    with Unsupported why -> fail_unsupported "field %s: %s" name why
  in
  let rec go i word off acc = function
    | [] -> (List.rev acc, off)
    | f :: rest ->
        let fld, word, off = field i word off f in
        go (i + 1) word off (fld :: acc) rest
  in
  let fields, size = go 0 None 0 [] s.fields in
  (fields, size, Option.map (expr var) s.where)

(* {1 Rust} *)

let endian_suffix = function T.Big -> "be" | T.Little -> "le"
let base_endian = function T.U8 -> T.Big | T.U16 e | T.U32 e -> e
let base_type = function T.U8 -> "u8" | T.U16 _ -> "u16" | T.U32 _ -> "u32"

let bits_type width =
  if width <= 8 then "u8" else if width <= 16 then "u16" else "u32"

let type_bits width = if width <= 8 then 8 else if width <= 16 then 16 else 32

let rust_type f =
  match f.store with
  | Int { width = 8; signed; _ } -> if signed then "i8" else "u8"
  | Int { width = 16; signed; _ } -> if signed then "i16" else "u16"
  | Int { signed; _ } -> if signed then "i32" else "u32"
  | Bits { width; _ } -> bits_type width
  | Bytes n -> Fmt.str "[u8; %d]" n

(* The parameter of [write] that takes a field's value: prefixed, so no field
   name shadows the buffer, the offset or a function the body calls. *)
let arg f = "v_" ^ f.label

(* The value a write or a setter takes. *)
let arg_type f =
  match f.store with Bytes n -> Fmt.str "&[u8; %d]" n | _ -> rust_type f

(* The [n] bytes at [k] of the array [b] as an array expression. *)
let bytes_at b k n =
  Fmt.str "[%a]"
    Fmt.(list ~sep:(any ", ") string)
    (List.init n (fun i -> Fmt.str "%s[%d]" b (k + i)))

let load b k bytes endian ty =
  match bytes with
  | 1 -> Fmt.str "%s[%d]" b k
  | _ ->
      Fmt.str "%s::from_%s_bytes(%s)" ty (endian_suffix endian)
        (bytes_at b k bytes)

let mask width = Fmt.str "0x%x" ((1 lsl width) - 1)

(* A bitfield's value out of its word [w]. *)
let of_word ~base ~shift ~width w =
  let v = Fmt.str "((%s >> %d) & %s)" w shift (mask width) in
  if base_type base = bits_type width then v
  else Fmt.str "(%s as %s)" v (bits_type width)

(* A field's value, read from the array [b] holding the record (at [base]
   0) or the field alone (at the field's offset). *)
let read ?(rel = 0) b f =
  let k = f.off - rel in
  match f.store with
  | Int { width = 8; signed; _ } ->
      if signed then Fmt.str "(%s[%d] as i8)" b k else Fmt.str "%s[%d]" b k
  | Int { width; endian; _ } -> load b k (width / 8) endian (rust_type f)
  | Bits { base; shift; width } ->
      of_word ~base ~shift ~width
        (load b k (Bitfield.byte_size base) (base_endian base) (base_type base))
  | Bytes n ->
      Fmt.str "{ let mut a = [0u8; %d]; a.copy_from_slice(&%s[%d..%d]); a }" n b
        k (k + n)

let pp_lines ppf l = List.iter (Fmt.pf ppf "%s@\n") l

let header ~codec ~digest ~size =
  [
    Fmt.str
      "//! Generated by Wire_rust from %s. Do not edit: generate it again when"
      (doc_string codec);
    "//! the codec changes.";
    "//!";
    "//! Every function takes the buffer as a slice and the record's offset in";
    "//! it, and reads or writes no byte outside the record. The code needs";
    "//! nothing but `core`, and no unsafe code.";
    "";
    "#![forbid(unsafe_code)]";
    "#![allow(dead_code, non_snake_case, unused_parens, unused_variables, \
     clippy::all)]";
    "";
    Fmt.str "/// The codec this module was generated from.";
    Fmt.str "pub const CODEC: &str = %s;" (rust_string codec);
    "";
    "/// The digest of the codec's description, which changes whenever the";
    "/// bytes it accepts or the values it reads do.";
    Fmt.str "pub const DIGEST: &str = %s;" (rust_string digest);
    "";
    "/// The size of the record in bytes.";
    Fmt.str "pub const SIZE: usize = %d;" size;
    "";
  ]

let prelude =
  {|/// Why a read or a write was refused.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Error {
    /// The buffer holds fewer than `expected` bytes, the end of the record
    /// or of the field, but `got`.
    TooShort { expected: usize, got: usize },
    /// The constraint of `field` fails, or the record's `where` clause when
    /// `field` is empty. `at` is the offset of the field in the buffer, or
    /// of the record.
    Constraint { field: &'static str, at: usize },
    /// `field`, a closed enum at offset `at`, holds `value`, which is none of
    /// its cases.
    InvalidEnum { field: &'static str, at: usize, value: i64 },
    /// A constraint's arithmetic leaves the 63-bit integers of the codec,
    /// which rejects the record.
    Overflow,
    /// A value handed to a write does not fit its bitfield, or is not a case
    /// of its closed enum.
    OutOfRange { field: &'static str, value: i64 },
}

const INT_MIN: i64 = -(1 << 62);
const INT_MAX: i64 = (1 << 62) - 1;

#[inline(always)]
fn int(v: i64) -> Result<i64, Error> {
    if v < INT_MIN || v > INT_MAX { Err(Error::Overflow) } else { Ok(v) }
}

// Both operands hold a 63-bit integer, so the [i64] sum cannot overflow.
#[inline(always)]
fn add(a: i64, b: i64) -> Result<i64, Error> { int(a + b) }

#[inline(always)]
fn sub(a: i64, b: i64) -> Result<i64, Error> { int(a - b) }

#[inline(always)]
fn mul(a: i64, b: i64) -> Result<i64, Error> {
    match a.checked_mul(b) { Some(p) => int(p), None => Err(Error::Overflow) }
}

// OCaml's [lsl] and [lsr] on the tagged integer [2a + 1], with the count
// taken modulo 64 as the machine does.
#[inline(always)]
fn lsl(a: i64, b: i64) -> i64 { (a << 1).wrapping_shl(b as u32) >> 1 }

#[inline(always)]
fn lsr(a: i64, b: i64) -> i64 {
    ((((a << 1) | 1) as u64).wrapping_shr(b as u32) as i64) >> 1
}

#[inline(always)]
fn frame(buf: &[u8], off: usize) -> Result<&[u8; SIZE], Error> {
    match buf.get(off..).and_then(|b| b.first_chunk::<SIZE>()) {
        Some(b) => Ok(b),
        None => Err(Error::TooShort { expected: off.saturating_add(SIZE), got: buf.len() }),
    }
}

#[inline(always)]
fn frame_mut(buf: &mut [u8], off: usize) -> Result<&mut [u8; SIZE], Error> {
    let got = buf.len();
    match buf.get_mut(off..).and_then(|b| b.first_chunk_mut::<SIZE>()) {
        Some(b) => Ok(b),
        None => Err(Error::TooShort { expected: off.saturating_add(SIZE), got }),
    }
}

#[inline(always)]
fn field<const N: usize>(buf: &[u8], off: usize, at: usize) -> Result<&[u8; N], Error> {
    match buf.get(off..).and_then(|b| b.get(at..)).and_then(|b| b.first_chunk::<N>()) {
        Some(b) => Ok(b),
        None => Err(Error::TooShort { expected: off.saturating_add(at + N), got: buf.len() }),
    }
}

#[inline(always)]
fn field_mut<const N: usize>(buf: &mut [u8], off: usize, at: usize) -> Result<&mut [u8; N], Error> {
    let got = buf.len();
    match buf.get_mut(off..).and_then(|b| b.get_mut(at..)).and_then(|b| b.first_chunk_mut::<N>()) {
        Some(b) => Ok(b),
        None => Err(Error::TooShort { expected: off.saturating_add(at + N), got }),
    }
}
|}

let member v cases =
  Fmt.str "(%a)"
    Fmt.(list ~sep:(any " || ") (fun ppf c -> pf ppf "%s == %s" v (int_lit c)))
    cases

let pp_record ppf fields =
  Fmt.pf ppf
    "/// The record's field values.@\n\
     #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]@\n\
     pub struct Record {@\n";
  List.iter
    (fun f -> Fmt.pf ppf "    pub %s: %s,@\n" f.label (rust_type f))
    fields;
  Fmt.pf ppf "}@\n@\n"

(* [check] makes the codec's checks on a record in an array, in its order:
   each field's in field order, then the [where] clause. *)
let pp_check ppf ~fields ~where =
  Fmt.pf ppf
    "#[inline(always)]@\n\
     fn check(b: &[u8; SIZE], off: usize) -> Result<(), Error> {@\n";
  List.iter
    (fun f ->
      match f.store with
      | Bytes _ -> ()
      | _ -> Fmt.pf ppf "    let %s = %s as i64;@\n" f.var (read "b" f))
    fields;
  List.iter
    (fun f ->
      List.iter
        (function
          | Constraint c ->
              Fmt.pf ppf
                "    if !%s { return Err(Error::Constraint { field: %s, at: \
                 off + %d }); }@\n"
                c f.lit f.off
          | Enum cases ->
              Fmt.pf ppf
                "    if !%s { return Err(Error::InvalidEnum { field: %s, at: \
                 off + %d, value: %s }); }@\n"
                (member f.var cases) f.lit f.off f.var)
        f.tests)
    fields;
  Option.iter
    (fun w ->
      Fmt.pf ppf
        "    if !%s { return Err(Error::Constraint { field: \"\", at: off }); }@\n"
        w)
    where;
  Fmt.pf ppf "    Ok(())@\n}@\n@\n"

let pp_record_of ppf ~b fields =
  Fmt.pf ppf "Record {@\n";
  List.iter (fun f -> Fmt.pf ppf "        %s: %s,@\n" f.label (read b f)) fields;
  Fmt.pf ppf "    }"

let pp_entry ppf ~fields =
  Fmt.pf ppf
    "/// Checks the record at `off` as the codec's `validate` does.@\n\
     #[inline]@\n\
     pub fn validate(buf: &[u8], off: usize) -> Result<(), Error> {@\n\
    \    check(frame(buf, off)?, off)@\n\
     }@\n\
     @\n\
     /// Checks and reads the record at `off`, as the codec's `decode` does.@\n\
     #[inline]@\n\
     pub fn decode(buf: &[u8], off: usize) -> Result<Record, Error> {@\n\
    \    let b = frame(buf, off)?;@\n\
    \    check(b, off)?;@\n\
    \    Ok(%a)@\n\
     }@\n\
     @\n"
    (pp_record_of ~b:"b") fields

(* A write stores each value of a bitfield word once, or-ed into place. *)
let bit_words fields =
  List.filter_map
    (fun f ->
      match f.store with Bits { base; _ } -> Some (f.off, base) | _ -> None)
    fields
  |> List.sort_uniq compare

let store_word ppf ~b (o, base) fields =
  let ty = base_type base in
  let parts =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { shift; _ } when f.off = o ->
            Fmt.kstr Option.some "((%s as %s) << %d)" (arg f) ty shift
        | _ -> None)
      fields
  in
  let value = String.concat " | " parts in
  match base with
  | T.U8 -> Fmt.pf ppf "    %s[%d] = %s;@\n" b o value
  | T.U16 e | T.U32 e ->
      let n = Bitfield.byte_size base in
      Fmt.pf ppf "    %s[%d..%d].copy_from_slice(&(%s).to_%s_bytes());@\n" b o
        (o + n) value (endian_suffix e)

let store ppf ~b ~v ~rel f =
  let k = f.off - rel in
  match f.store with
  | Int { width = 8; signed; _ } ->
      Fmt.pf ppf "    %s[%d] = %s%s;@\n" b k v (if signed then " as u8" else "")
  | Int { width; endian; _ } ->
      Fmt.pf ppf "    %s[%d..%d].copy_from_slice(&%s.to_%s_bytes());@\n" b k
        (k + (width / 8))
        v (endian_suffix endian)
  | Bytes n -> Fmt.pf ppf "    %s[%d..%d].copy_from_slice(%s);@\n" b k (k + n) v
  | Bits _ -> ()

(* The refusals a write makes before it stores anything: a bitfield's value
   wider than the field, and a value outside a closed enum, in field order. *)
let pp_range ppf ~v f =
  (match f.store with
  | Bits { width; _ } when width < type_bits width ->
      Fmt.pf ppf
        "    if %s >> %d != 0 { return Err(Error::OutOfRange { field: %s, \
         value: %s as i64 }); }@\n"
        v width f.lit v
  | _ -> ());
  match f.enum with
  | Some cases ->
      Fmt.pf ppf
        "    if !%s { return Err(Error::OutOfRange { field: %s, value: %s as \
         i64 }); }@\n"
        (member (Fmt.str "(%s as i64)" v) cases)
        f.lit v
  | None -> ()

let pp_write ppf ~fields =
  Fmt.pf ppf
    "/// Writes a record of these values at `off`, as the codec's `encode`@\n\
     /// does: a value that does not fit its field is refused before any byte@\n\
     /// is written, and a record that fails its constraints after.@\n\
     #[inline]@\n\
     pub fn write(buf: &mut [u8], off: usize%a) -> Result<(), Error> {@\n\
    \    let b = frame_mut(buf, off)?;@\n"
    Fmt.(list ~sep:nop (fun ppf f -> pf ppf ", %s: %s" (arg f) (arg_type f)))
    fields;
  List.iter (fun f -> pp_range ppf ~v:(arg f) f) fields;
  List.iter (fun w -> store_word ppf ~b:"b" w fields) (bit_words fields);
  List.iter (fun f -> store ppf ~b:"b" ~v:(arg f) ~rel:0 f) fields;
  Fmt.pf ppf "    check(b, off)@\n}@\n@\n";
  Fmt.pf ppf
    "/// Writes `r` at `off`, as [`write`] does.@\n\
     #[inline]@\n\
     pub fn encode(r: &Record, buf: &mut [u8], off: usize) -> Result<(), \
     Error> {@\n\
    \    write(buf, off%a)@\n\
     }@\n\
     @\n"
    Fmt.(
      list ~sep:nop (fun ppf f ->
          match f.store with
          | Bytes _ -> pf ppf ", &r.%s" f.label
          | _ -> pf ppf ", r.%s" f.label))
    fields

(* [get_<field>] reads one field as the codec's getter does: from the bytes of
   the field alone, with no check of the other fields or of constraints; a
   closed enum's value is checked to be a case. *)
let pp_getter ppf f =
  let n = width_bytes f.store in
  match f.store with
  | Bytes _ ->
      Fmt.pf ppf
        "/// Reads the field `%s`.@\n\
         #[inline]@\n\
         pub fn get_%s(buf: &[u8], off: usize) -> Result<&[u8; %d], Error> {@\n\
        \    field::<%d>(buf, off, %d)@\n\
         }@\n\
         @\n"
        f.doc f.label n n f.off
  | _ ->
      let enum_check =
        match f.enum with
        | Some cases ->
            Fmt.str
              "    if !%s { return Err(Error::InvalidEnum { field: %s, at: \
               off.saturating_add(%d), value: v as i64 }); }@\n"
              (member "(v as i64)" cases)
              f.lit f.off
        | None -> ""
      in
      Fmt.pf ppf
        "/// Reads the field `%s`.@\n\
         #[inline]@\n\
         pub fn get_%s(buf: &[u8], off: usize) -> Result<%s, Error> {@\n\
        \    let s = field::<%d>(buf, off, %d)?;@\n\
        \    let v = %s;@\n\
         %s    Ok(v)@\n\
         }@\n\
         @\n"
        f.doc f.label (rust_type f) n f.off (read ~rel:f.off "s" f) enum_check

(* A bitfield's word, read once for all the fields packed in it:
   [get_<field>_word] loads the word that holds the field, and
   [<field>_of_word] takes the field out of it. *)
let pp_word_getters ppf f =
  match f.store with
  | Bits { base; shift; width } ->
      let n = width_bytes f.store and wt = base_type base in
      Fmt.pf ppf
        "/// Reads the word that holds the bitfield `%s`, for [`%s_of_word`].@\n\
         #[inline]@\n\
         pub fn get_%s_word(buf: &[u8], off: usize) -> Result<%s, Error> {@\n\
        \    let s = field::<%d>(buf, off, %d)?;@\n\
        \    Ok(%s)@\n\
         }@\n\
         @\n\
         /// Takes the bitfield `%s` out of its word.@\n\
         #[inline]@\n\
         pub fn %s_of_word(w: %s) -> %s {@\n\
        \    %s@\n\
         }@\n\
         @\n"
        f.doc f.label f.label wt n f.off
        (load "s" 0 n (base_endian base) wt)
        f.doc f.label wt (bits_type width)
        (of_word ~base ~shift ~width "w")
  | _ -> ()

(* [set_<field>] writes one field from its bytes alone, a bitfield's
   neighbours kept, with no check of constraints. *)
let pp_setter ppf f =
  let n = width_bytes f.store in
  Fmt.pf ppf
    "/// Writes the field `%s`, with no check of the record's constraints.@\n\
     #[inline]@\n\
     pub fn set_%s(buf: &mut [u8], off: usize, v: %s) -> Result<(), Error> {@\n\
    \    let s = field_mut::<%d>(buf, off, %d)?;@\n"
    f.doc f.label (arg_type f) n f.off;
  pp_range ppf ~v:"v" f;
  (match f.store with
  | Bits { base; shift; width } ->
      let wt = base_type base in
      let cur = load "s" 0 n (base_endian base) wt in
      let next =
        Fmt.str "(%s & !(%s%s << %d)) | ((v as %s) << %d)" cur (mask width) wt
          shift wt shift
      in
      if n = 1 then Fmt.pf ppf "    s[0] = %s;@\n" next
      else
        Fmt.pf ppf "    *s = (%s).to_%s_bytes();@\n" next
          (endian_suffix (base_endian base))
  | _ -> store ppf ~b:"s" ~v:"v" ~rel:f.off f);
  Fmt.pf ppf "    Ok(())@\n}@\n@\n"

let pp_accessors ppf f =
  pp_getter ppf f;
  pp_word_getters ppf f;
  pp_setter ppf f

(* A read of [Valid]: a load at a constant offset of its array. *)
let pp_valid_read ppf f =
  match f.store with
  | Bytes n ->
      Fmt.pf ppf
        "@\n\
        \    /// The field `%s`.@\n\
        \    #[inline]@\n\
        \    pub fn %s(&self) -> &'a [u8; %d] {@\n\
        \        let b: &'a [u8; SIZE] = self.0;@\n\
        \        b[%d..%d].try_into().unwrap()@\n\
        \    }@\n"
        f.doc f.label n f.off (f.off + n)
  | _ ->
      Fmt.pf ppf
        "@\n\
        \    /// The field `%s`.@\n\
        \    #[inline]@\n\
        \    pub fn %s(&self) -> %s {@\n\
        \        let b = self.0;@\n\
        \        %s@\n\
        \    }@\n"
        f.doc f.label (rust_type f) (read "b" f)

(* [Valid] holds a record that passed every check. Its reads index an array of
   [SIZE] bytes at constant offsets, which the compiler proves in bounds, and
   the shared borrow keeps the bytes from changing while it lives. *)
let pp_valid ppf ~fields =
  Fmt.pf ppf
    "/// A record that passed every check, as the codec's `valid` returns: its@\n\
     /// reads are loads at constant offsets with no check of their own, and@\n\
     /// the borrow keeps its bytes from changing, so they keep holding.@\n\
     #[derive(Clone, Copy, Debug)]@\n\
     pub struct Valid<'a>(&'a [u8; SIZE]);@\n\
     @\n\
     impl<'a> Valid<'a> {@\n\
    \    /// Checks the record at offset 0 of `buf`.@\n\
    \    #[inline]@\n\
    \    pub fn new(buf: &'a [u8]) -> Result<Self, Error> {@\n\
    \        Self::at(buf, 0)@\n\
    \    }@\n\
     @\n\
    \    /// Checks the record at `off` in `buf`.@\n\
    \    #[inline]@\n\
    \    pub fn at(buf: &'a [u8], off: usize) -> Result<Self, Error> {@\n\
    \        let b = frame(buf, off)?;@\n\
    \        check(b, off)?;@\n\
    \        Ok(Valid(b))@\n\
    \    }@\n\
     @\n\
    \    /// The record's bytes.@\n\
    \    #[inline]@\n\
    \    pub fn as_bytes(&self) -> &'a [u8; SIZE] {@\n\
    \        self.0@\n\
    \    }@\n\
     @\n\
    \    /// The record's field values.@\n\
    \    #[inline]@\n\
    \    pub fn to_record(&self) -> Record {@\n\
    \        let b = self.0;@\n\
    \    %a@\n\
    \    }@\n\
     %a}@\n"
    (pp_record_of ~b:"b") fields
    Fmt.(list ~sep:nop pp_valid_read)
    fields

(* {1 Names} *)

let refuse ~codec reason =
  Fmt.invalid_arg "Wire_rust: cannot specialise %s: %s" codec reason

(* The functions the module defines for itself. *)
let own_values =
  [
    "validate";
    "decode";
    "encode";
    "write";
    "check";
    "frame";
    "frame_mut";
    "field";
    "field_mut";
    "int";
    "add";
    "sub";
    "mul";
    "lsl";
    "lsr";
  ]

(* The methods [Valid] defines for itself. *)
let own_methods = [ "new"; "at"; "as_bytes"; "to_record" ]

let values_of f =
  let l = f.label in
  let derived =
    match f.store with
    | Bits _ -> [ "get_" ^ l ^ "_word"; l ^ "_of_word" ]
    | Int _ | Bytes _ -> []
  in
  ("get_" ^ l) :: ("set_" ^ l) :: derived

type source = Own | Field of string

let check_clash ~codec idents =
  match Wire.Private.Names.clash idents with
  | Some (Own, Field f, n) | Some (Field f, Own, n) ->
      Fmt.kstr (refuse ~codec)
        "field %S defines %s, which the generated module defines for itself" f n
  | Some (Field g, Field f, n) ->
      Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
  | Some (Own, Own, _) | None -> ()

let check_names ~codec fields =
  let own l = List.map (fun n -> (Own, n)) l in
  check_clash ~codec
    (own own_values
    @ List.concat_map
        (fun f -> List.map (fun n -> (Field f.name, n)) (values_of f))
        fields);
  check_clash ~codec
    (own own_methods @ List.map (fun f -> (Field f.name, f.label)) fields)

let digest c = Wire.Private.Gen.digest c

let pp ~codec ppf c =
  match layout (Wire.Private.Gen.to_struct c) with
  | exception Unsupported reason -> refuse ~codec reason
  | _, size, _ when Wire.Codec.wire_size_opt c <> Some size ->
      refuse ~codec "its size is not fixed"
  | fields, size, where ->
      check_names ~codec fields;
      pp_lines ppf (header ~codec ~digest:(digest c) ~size);
      Fmt.pf ppf "%s@\n" prelude;
      pp_record ppf fields;
      pp_check ppf ~fields ~where;
      pp_entry ppf ~fields;
      pp_write ppf ~fields;
      List.iter (pp_accessors ppf) fields;
      pp_valid ppf ~fields

let rs ~codec c = Fmt.str "%a" (pp ~codec) c
