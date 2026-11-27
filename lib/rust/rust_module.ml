open Wire.Private
open Layout
module T = Types
module P = Wire_ocaml.Plan
open Walk

(* {1 The one module of a codec}

   The module of a codec has the members of the OCaml module [Wire_ocaml]
   writes, spelled in Rust: a check, its view and an accessor per named field
   of every record the codec reaches, a module per record a field holds, the
   record's values, its error, and its fold when it declares one. The records and
   their plans are [Wire_ocaml.Plan.records], the ones the OCaml views are
   written from; the check and the fold walk [Wire_ocaml.Plan.v], as the
   OCaml fold does. Positions and sizes are [i64]s, as the plan computes them:
   a codec keeps each size below 2^32, so their sums from an offset inside a
   buffer cannot wrap. One becomes a [usize] only through a checked
   conversion, so none is cut where a [usize] is narrower, and an offset
   past the end of the address space saturates, as the codec's do. A load
   goes through [get], which answers [None] past the buffer, and a store
   through [put], which writes nothing there, so no input makes the module
   panic. *)

let refuse ~codec reason =
  Fmt.invalid_arg "Wire_rust: cannot specialise %s: %s" codec reason

(* A buffer of Rust lines. *)
let pr b fmt =
  Fmt.kstr
    (fun s ->
      Buffer.add_string b s;
      Buffer.add_char b '\n')
    fmt

let counter () =
  let n = ref 0 in
  fun prefix ->
    incr n;
    Fmt.str "%s%d" prefix !n

let rec fields_of nodes =
  List.concat_map
    (function
      | P.Prefix (fs, _) -> fs
      | P.Gate (_, _, _, b) | P.Region (_, _, b) -> fields_of b
      | P.Span _ | P.Test _ -> [])
    nodes

let refs e = T.collect_refs e

(* {2 The records} *)

type ctx = {
  all : P.records;
  r : P.record;
  scope : P.Scope.t;
  up : string;  (** The path to the module that holds every record's. *)
}

let module_of (r : P.record) =
  match r.module_name with
  | Some n -> String.lowercase_ascii n
  | None -> invalid_arg "Rust_module.module_of: the root"

let record_of_id all id =
  List.find (fun (r : P.record) -> r.id = id) all.P.records

(* The path of the module of the codec [id], from the module of [ctx]. *)
let sub_ref ctx id = ctx.up ^ module_of (record_of_id ctx.all id)
let sub_of ctx v = List.find_opt (fun (s : P.sub) -> s.var = v) ctx.r.subs

let field_of (s : T.struct_) label =
  List.find_map
    (fun (T.Field f as fld) ->
      match f.field_name with
      | Some n when Names.ocaml_label n = label -> Some (n, fld)
      | _ -> None)
    s.fields

(* The fields of a record, by OCaml label, with their codec name. *)
let named (s : T.struct_) =
  List.filter_map
    (fun (T.Field f as fld) ->
      Option.map (fun n -> (Names.ocaml_label n, n, fld)) f.field_name)
    s.fields

let min_size (r : P.record) =
  Wire.Private.struct_min_size (Wire.Private.validator_of_struct r.struct_)

(* Whether a value of [typ] holds a borrow of the buffer it was read from. *)
let rec typ_borrows : type a. P.records -> a T.typ -> bool =
 fun all typ ->
  match typ with
  | T.Optional { inner; _ } -> typ_borrows all inner
  | T.Single_elem { elem; _ } -> typ_borrows all elem
  | T.Where { inner; _ } -> typ_borrows all inner
  | T.Enum { base; _ } -> typ_borrows all base
  | T.Codec { codec_id; _ } -> borrows all (record_of_id all codec_id)
  | T.Byte_slice _ -> true
  | T.Byte_array { size = T.Int _ } -> false
  | T.Byte_array _ -> true
  | _ -> false

and borrows all (r : P.record) =
  List.exists
    (fun (_, _, T.Field f) -> typ_borrows all f.field_typ)
    (named r.struct_)

let record_type all (r : P.record) =
  if borrows all r then "Record<'a>" else "Record"

(* The Rust type of a value of [typ], as the record holds it. *)
let rec typ_text : type a. ctx -> a T.typ -> string =
 fun ctx typ ->
  match typ with
  | T.Optional { inner; _ } -> Fmt.str "Option<%s>" (typ_text ctx inner)
  | T.Single_elem { elem; _ } -> typ_text ctx elem
  | T.Where { inner; _ } -> typ_text ctx inner
  | T.Enum { base; _ } -> typ_text ctx base
  | T.Codec { codec_id; _ } ->
      Fmt.str "%s::%s" (sub_ref ctx codec_id)
        (record_type ctx.all (record_of_id ctx.all codec_id))
  | T.Byte_slice { size = T.Int n } -> Fmt.str "&'a [u8; %d]" n
  | T.Byte_slice _ -> "&'a [u8]"
  | T.Byte_array { size = T.Int n } -> Fmt.str "[u8; %d]" n
  | T.Byte_array _ -> "&'a [u8]"
  | T.Unit -> "()"
  | T.Bits { width; _ } -> bits_type width
  | typ -> (
      match leaf typ with
      | Some s -> store_type s
      | None -> fail_unsupported "its type, %a" T.pp_typ typ)

let member_name = function
  | P.Scalar { name; _ }
  | P.Present { name; _ }
  | P.Sub { name; _ }
  | P.Bytes { name; _ } ->
      name

let name_in (r : P.record) label =
  match field_of r.struct_ label with Some (n, _) -> n | None -> ""

let label_of_presence (o : P.output) =
  let suffix = "_present" in
  let n = String.length o.name - String.length suffix in
  if n > 0 && String.ends_with ~suffix o.name then Some (String.sub o.name 0 n)
  else None

(* An accessor of [r] is named after its field, as the field of [r]'s record
   is: [ident n] for the field [n], [ident (n ^ "_present")] for its
   presence. *)
let acc (r : P.record) m =
  match m with
  | P.Scalar { field; _ } -> ident field.name
  | P.Bytes { label; _ } -> ident (name_in r label)
  | P.Sub { sub = { path = Some [ l ]; _ }; _ } -> ident (name_in r l)
  | P.Present { presence; _ } -> (
      match label_of_presence presence with
      | Some l -> ident (name_in r l ^ "_present")
      | None -> ident (member_name m))
  | P.Sub _ -> ident (member_name m)

let member (r : P.record) p = List.find_opt p r.P.members

let scalar r l =
  member r (function P.Scalar { field; _ } -> field.label = l | _ -> false)

let bytes r l = member r (function P.Bytes b -> b.label = l | _ -> false)

let sub r l =
  member r (function P.Sub { sub; _ } -> sub.path = Some [ l ] | _ -> false)

let present r l =
  member r (function
    | P.Present { presence; _ } -> label_of_presence presence = Some l
    | _ -> false)

(* A field read with one load from the view's array: at a constant offset
   inside the bytes a view of [r] holds. *)
let direct (r : P.record) (f : P.field) =
  match P.nodes r.plan with
  | P.Prefix (fields, _) :: _ ->
      List.memq f fields && f.offset + width_bytes f.store <= r.fixed
  | _ -> false

(* {2 The prelude} *)

let depth_of (all : P.records) =
  let rec go (r : P.record) =
    List.fold_left
      (fun acc (s : P.sub) -> max acc (1 + go (record_of_id all s.codec_id)))
      1 r.subs
  in
  go all.root

(* The error of the module and the helpers its code calls. [DEPTH], the
   most names a path holds, is the module's own. *)
let error_prelude =
  {|/// The predicate a failed constraint comes from: a `where` clause, a
/// field's constraint, an action, or a per-byte refinement.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Predicate {
    Where,
    Field,
    Action,
    PerByte,
}

/// Why the input is refused, as the codec's `Wire.Error.kind` says it.
/// `UnexpectedEof` counts bytes: `expected` the value needed, `got` were
/// left where it starts.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Kind {
    UnexpectedEof { expected: usize, got: usize },
    TrailingBytes(usize),
    InvalidEnum { value: i64, valid: &'static [i64] },
    InvalidTag(i64),
    MissingTerminator,
    NonZeroPadding,
    ValueOutOfRange { value: i64 },
    ZeroDivisor,
    ConstraintFailed { which: Predicate, value: Option<i64> },
}

/// The path of field names to a field, root to leaf.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    names: [&'static str; DEPTH],
    len: usize,
}

impl Path {
    /// The empty path: the record itself.
    pub const ROOT: Path = Path { names: [""; DEPTH], len: 0 };

    #[inline(always)]
    fn push(self, name: &'static str) -> Path {
        let mut p = self;
        if !name.is_empty() && p.len < p.names.len() {
            p.names[p.len] = name;
            p.len += 1;
        }
        p
    }

    /// The names of the path, root to leaf.
    pub fn names(&self) -> &[&'static str] {
        &self.names[..self.len]
    }
}

/// An error of the codec, as `Wire.Error.t`: the absolute offset of the
/// failing field, the path of field names to it, and why.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Error {
    pub at: usize,
    pub field: Path,
    pub kind: Kind,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for (i, n) in self.field.names().iter().enumerate() {
            if i > 0 {
                f.write_str(".")?;
            }
            f.write_str(n)?;
        }
        write!(f, "@{} {:?}", self.at, self.kind)
    }
}

impl core::error::Error for Error {}

// A position or a count as a `usize`: 0 below 0, and the largest `usize`,
// past the end of any buffer, above it.
#[inline(always)]
fn pos(x: i64) -> usize {
    if x <= 0 { 0 } else { usize::try_from(x).unwrap_or(usize::MAX) }
}

#[inline(always)]
fn fail(at: i64, field: Path, kind: Kind) -> Error {
    Error { at: pos(at), field, kind }
}

#[inline(always)]
fn eof_if(c: bool, at: i64, expected: i64, got: i64, field: Path) -> Result<(), Error> {
    if c {
        Err(fail(at, field, Kind::UnexpectedEof { expected: pos(expected), got: pos(got) }))
    } else {
        Ok(())
    }
}

// A size computed on 64 bits, saturated at the largest [i64].
#[inline(always)]
fn sat(x: u64) -> i64 {
    if x > i64::MAX as u64 { i64::MAX } else { x as i64 }
}

#[inline(always)]
fn refused(at: i64, field: Path, which: Predicate) -> Error {
    fail(at, field, Kind::ConstraintFailed { which, value: None })
}

// The `N` bytes at `at` of `b`, or `None` past its end.
#[inline(always)]
fn get<const N: usize>(b: &[u8], at: i64) -> Option<&[u8; N]> {
    b.get(usize::try_from(at).ok()?..)?.first_chunk::<N>()
}

// The `N` bytes at `at` of `b`, zeros past its end: for a read the bounds
// checked before it keep inside.
#[inline(always)]
fn rd<const N: usize>(b: &[u8], at: i64) -> [u8; N] {
    match get::<N>(b, at) {
        Some(a) => *a,
        None => [0; N],
    }
}

// The `n` bytes at `at` of `b`, or `None` past its end.
#[inline(always)]
fn span(b: &[u8], at: i64, n: i64) -> Option<&[u8]> {
    b.get(usize::try_from(at).ok()?..)?.get(..usize::try_from(n).ok()?)
}

// Writes `v` at `at` of `b`, nothing past its end.
#[inline(always)]
fn put(b: &mut [u8], at: i64, v: &[u8]) {
    if let Ok(at) = usize::try_from(at) {
        if let Some(d) = b.get_mut(at..).and_then(|d| d.get_mut(..v.len())) {
            d.copy_from_slice(v);
        }
    }
}

// Writes `n` zeros at `at` of `b`, nothing past its end.
#[inline(always)]
fn zeros(b: &mut [u8], at: i64, n: i64) {
    if let (Ok(at), Ok(n)) = (usize::try_from(at), usize::try_from(n)) {
        if let Some(d) = b.get_mut(at..).and_then(|d| d.get_mut(..n)) {
            d.fill(0);
        }
    }
}
|}

let header ~codec ~min ~slots =
  [
    Fmt.str
      "//! Generated by Wire_rust from the codec %s. Do not edit: generate it \
       again"
      (doc_string codec);
    "//! when the codec changes.";
    "//!";
    "//! The module checks, reads, decodes and encodes the records of the \
     codec as";
    "//! it does, with its errors. It needs nothing but `core`, no unsafe code,";
    "//! and no input makes it panic.";
    "";
    "#![forbid(unsafe_code)]";
    "#![allow(dead_code, non_snake_case, non_camel_case_types, unused_parens, \
     unused_variables, unused_imports, unused_braces, unreachable_code, \
     clippy::all)]";
  ]
  @ (if slots then
       [
         "// An optional field that is always present overwrites its outputs' \
          absent";
         "// values, and one that never is leaves them.";
         "#![allow(unused_assignments, unused_mut)]";
       ]
     else [])
  @ [
      "";
      "/// The fewest bytes a record takes.";
      Fmt.str "pub const MIN_SIZE: usize = %d;" min;
      "";
    ]

(* {2 Reads} *)

(* A field's value as an [i64], from the array [a] that holds its bytes. *)
let value_of a (f : P.field) =
  match f.store with
  | Bytes _ -> "0i64"
  | s -> Fmt.str "(%s as i64)" (read_at a 0 s)

(* Binds [f] at [at] of [buf], read with [rd]: zeros past the end, which
   the bounds tested before it keep out. *)
let bind_rd b (f : P.field) ~at =
  match f.store with
  | Bytes _ -> ()
  | _ ->
      pr b "let %s: i64 = { let a = rd::<%d>(buf, %s); %s };" f.id
        (width_bytes f.store) at (value_of "a" f)

(* {2 Views}

   An accessor reads its field, or places its sub-record, from the view: a
   field at a constant offset inside the bytes a view holds is one load from
   its array; anything else is placed again from the bytes that place it, as
   the check placed them, and a presence tested again. The view borrows its
   buffer, so its bytes cannot change: placed again, a field is where the
   check found it. *)

type target =
  | Field of string
  | Gate of string
  | Marker of string
  | Span_of of P.node
  | End

let rec contains target node =
  match (target, node) with
  | Field id, P.Prefix (fields, _) ->
      List.exists (fun (f : P.field) -> f.id = id) fields
  | Marker v, P.Span (T.Ref (T.I, x)) -> x = v
  | Span_of n, (P.Span _ as m) -> n == m
  | Gate id, P.Gate (_, p, _, _) when p.id = id -> true
  | _, (P.Gate (_, _, _, body) | P.Region (_, _, body)) ->
      List.exists (contains target) body
  | _ -> false

type loc = { c : ctx; b : Buffer.t; fresh : string -> string }

(* Binds each scalar of a prefix at [p], or answers [None] when its bytes
   are past the buffer. *)
let bind_prefix l fields p =
  List.iter
    (fun (f : P.field) ->
      match f.store with
      | Bytes _ -> ()
      | _ ->
          pr l.b "let %s: i64 = { let a = get::<%d>(buf, %s + %d)?; %s };" f.id
            (width_bytes f.store) p f.offset (value_of "a" f))
    fields

(* A size, or [None] where it is negative. *)
let size l (e : int T.expr) =
  let s = l.fresh "s" in
  pr l.b "let %s: i64 = %s;" s (size_expr l.c.scope e);
  pr l.b "if %s < 0 { return None; }" s;
  s

(* The position after [node] at [p]. *)
let rec after l node p =
  match node with
  | P.Prefix (fields, n) ->
      bind_prefix l fields p;
      Fmt.str "(%s + %d)" p n
  | P.Test _ -> p
  | P.Span (T.Ref (T.I, v)) when Option.is_some (sub_of l.c v) -> (
      let s = Option.get (sub_of l.c v) in
      match s.fixed_size with
      | Some k -> Fmt.str "(%s + %d)" p k
      | None ->
          let q = l.fresh "p" in
          pr l.b "let %s = %s::end_at(buf, %s)?;" q (sub_ref l.c s.codec_id) p;
          q)
  | P.Span e | P.Region (e, _, _) ->
      let s = size l e in
      Fmt.str "(%s + %s)" p s
  | P.Gate (c, _, _, body) ->
      let q = l.fresh "p" in
      pr l.b "let %s = if %s {" q (Walk.expr l.c.scope c);
      let e = List.fold_left (fun p n -> after l n p) p body in
      pr l.b "%s } else { %s };" e p;
      q

(* Walks [nodes] from [p] to [target] and writes [finish] at its position,
   or the end of the record for [End]. *)
let rec find l target nodes p ~finish =
  match nodes with
  | [] -> finish ~size:None p
  | node :: _ when target <> End && contains target node -> (
      match node with
      | P.Gate (c, pres, _, body) -> (
          pr l.b "if !(%s) { return None; }" (Walk.expr l.c.scope c);
          match target with
          | Gate id when pres.id = id -> finish ~size:None p
          | _ -> find l target body p ~finish)
      | P.Region (_, _, body) -> find l target body p ~finish
      | P.Prefix (fields, _) ->
          bind_prefix l fields p;
          finish ~size:None p
      | P.Span e -> (
          match target with
          | Span_of _ ->
              let s = size l e in
              finish ~size:(Some s) p
          | _ -> finish ~size:None p)
      | P.Test _ -> finish ~size:None p)
  | node :: rest ->
      let q = after l node p in
      let x = l.fresh "p" in
      pr l.b "let %s: i64 = %s;" x q;
      find l target rest x ~finish

(* The function [locate] of an accessor: the walk from the view at [v] in
   [buf] to its target, returning [ret]. *)
let locate c ~ret ~target ~finish =
  let b = Buffer.create 256 in
  let l = { c; b; fresh = counter () } in
  find l target (P.nodes c.r.plan) "v" ~finish:(fun ~size p ->
      pr b "%s" (finish ~size p));
  Fmt.str
    "        #[inline]\n\
    \        fn locate<'b>(buf: &'b [u8], v: i64) -> Option<%s> {\n\
     %s        }\n"
    ret (Buffer.contents b)

(* The accessor [name] of a field of bytes. *)
let pp_bytes_accessor ppf c ~name (m : P.member) =
  let pf fmt = Fmt.pf ppf fmt in
  match m with
  | P.Bytes { at = Fixed f; _ } when direct c.r f -> (
      match f.store with
      | Bytes n ->
          pf
            "    /// The bytes of the field `%s`.\n\
            \    #[inline(always)]\n\
            \    pub fn %s(&self) -> &'a [u8; %d] {\n\
            \        let b: &'a [u8; FIXED] = self.head;\n\
            \        match b[%d..].first_chunk::<%d>() {\n\
            \            Some(a) => a,\n\
            \            None => &[0; %d],\n\
            \        }\n\
            \    }\n\n"
            (doc_string f.name) name n f.offset n n
      | _ -> ())
  | P.Bytes { at = Fixed f; _ } -> (
      match f.store with
      | Bytes n ->
          pf
            "    /// The bytes of the field `%s`.\n\
            \    #[inline]\n\
            \    pub fn %s(&self) -> &'a [u8; %d] {\n\
             %s        locate(self.buf, self.at as i64).unwrap_or(&[0; %d])\n\
            \    }\n\n"
            (doc_string f.name) name n
            (locate c ~ret:(Fmt.str "&'b [u8; %d]" n) ~target:(Field f.id)
               ~finish:(fun ~size:_ p ->
                 Fmt.str "get::<%d>(buf, %s + %d)" n p f.offset))
            n
      | _ -> ())
  | P.Bytes { at = Sized node; label; _ } ->
      pf
        "    /// The bytes of the field `%s`.\n\
        \    #[inline]\n\
        \    pub fn %s(&self) -> &'a [u8] {\n\
         %s        locate(self.buf, self.at as i64).unwrap_or(&[])\n\
        \    }\n\n"
        (doc_string label) name
        (locate c ~ret:"&'b [u8]" ~target:(Span_of node) ~finish:(fun ~size p ->
             Fmt.str "span(buf, %s, %s)" p (Option.get size)))
  | P.Scalar _ | P.Present _ | P.Sub _ -> ()

let pp_accessor ppf c (m : P.member) =
  let name = acc c.r m in
  let pf fmt = Fmt.pf ppf fmt in
  match m with
  | P.Scalar { field = f; _ } when direct c.r f ->
      pf
        "    /// The field `%s`.\n\
        \    #[inline(always)]\n\
        \    pub fn %s(&self) -> %s {\n\
        \        let b = self.head;\n\
        \        %s\n\
        \    }\n\n"
        (doc_string f.name) name (store_type f.store)
        (read_at "b" f.offset f.store)
  | P.Scalar { field = f; _ } ->
      let ty = store_type f.store in
      pf
        "    /// The field `%s`, 0 where it is absent.\n\
        \    #[inline]\n\
        \    pub fn %s(&self) -> %s {\n\
         %s        locate(self.buf, self.at as i64).unwrap_or(0)\n\
        \    }\n\n"
        (doc_string f.name) name ty
        (locate c ~ret:ty ~target:(Field f.id) ~finish:(fun ~size:_ p ->
             Fmt.str "let a = get::<%d>(buf, %s + %d)?;\nSome(%s)"
               (width_bytes f.store) p f.offset (read_at "a" 0 f.store)))
  | P.Present { presence; _ } ->
      pf
        "    /// Whether the optional field it names is present.\n\
        \    #[inline]\n\
        \    pub fn %s(&self) -> bool {\n\
         %s        locate(self.buf, self.at as i64).is_some()\n\
        \    }\n\n"
        name
        (locate c ~ret:"()" ~target:(Gate presence.id) ~finish:(fun ~size:_ _ ->
             "Some(())"))
  | P.Sub { sub = s; _ } ->
      let m = sub_ref c s.codec_id in
      pf
        "    /// The record `%s`, as a check of [`%s`] that passes iff it is \
         present.\n\
        \    #[inline]\n\
        \    pub fn %s(&self) -> %s::Check<'a> {\n\
         %s        match locate(self.buf, self.at as i64) {\n\
        \            Some(p) => %s::at(self.buf, p),\n\
        \            None => %s::Check(None),\n\
        \        }\n\
        \    }\n\n"
        (doc_string (Option.fold ~none:"" ~some:(String.concat ".") s.path))
        m name m
        (locate c ~ret:"i64" ~target:(Marker s.var) ~finish:(fun ~size:_ p ->
             Fmt.str "Some(%s)" p))
        m m
  | P.Bytes _ -> pp_bytes_accessor ppf c ~name m

(* [set_<field>] writes a field at a constant offset inside the bytes a view
   holds, in place, with no check of the record's constraints. *)
let pp_setter ppf (_, (f : P.field)) =
  let name = ident f.name in
  let n = width_bytes f.store in
  let ty = store_type f.store in
  Fmt.pf ppf
    "/// Writes `x` in place as [`View::%s`] reads it in the record at `at`, \
     with\n\
     /// no check of the record's constraints: check it again after a batch of\n\
     /// writes. Refuses a value the field cannot hold, and a record past the\n\
     /// end of `buf`.\n\
     #[inline]\n\
     pub fn set_%s(buf: &mut [u8], at: usize, x: %s) -> Result<(), Error> {\n\
    \    let k = at.saturating_add(%d);\n\
    \    let got = buf.len().saturating_sub(k);\n\
    \    let field = Path::ROOT.push(%s);\n"
    name name ty f.offset (rust_string f.name);
  (match f.store with
  | Bits { width; _ } when width < type_bits width ->
      Fmt.pf ppf
        "    if x >> %d != 0 { return Err(Error { at: k, field, kind: \
         Kind::ValueOutOfRange { value: x as i64 } }); }\n"
        width
  | _ -> ());
  Fmt.pf ppf
    "    let Some(s) = buf.get_mut(k..).and_then(|s| \
     s.first_chunk_mut::<%d>()) else {\n\
    \        return Err(Error { at: k, field, kind: Kind::UnexpectedEof { \
     expected: %d, got } });\n\
    \    };\n"
    n n;
  (match f.store with
  | Bits { base; shift; width } ->
      let wt = base_type base in
      let cur = load "s" 0 n (base_endian base) wt in
      let next =
        Fmt.str "(%s & !(%s%s << %d)) | ((x as %s) << %d)" cur (mask width) wt
          shift wt shift
      in
      if n = 1 then Fmt.pf ppf "    s[0] = %s;\n" next
      else
        Fmt.pf ppf "    *s = (%s).to_%s_bytes();\n" next
          (endian_suffix (base_endian base))
  | Int { width = 8; _ } -> Fmt.pf ppf "    s[0] = x as u8;\n"
  | Int { endian; _ } ->
      Fmt.pf ppf "    *s = x.to_%s_bytes();\n" (endian_suffix endian)
  | Bytes _ -> ());
  Fmt.pf ppf "    Ok(())\n}\n\n"

(* {2 Values} *)

(* The value of the field [l] of type [typ] from the view [v]. *)
let rec value : type a. ctx -> string -> a T.typ -> string =
 fun c l typ ->
  let missing () = Fmt.invalid_arg "Rust_module: no accessor for %s" l in
  match typ with
  | T.Optional { inner; _ } -> (
      match sub c.r l with
      | Some (P.Sub { sub = s; _ } as m) ->
          let md = sub_ref c s.codec_id in
          Fmt.str
            "(match v.%s().0 { Some(s) => Some(%s::of_view(s)?), None => None \
             })"
            (acc c.r m) md
      | _ -> (
          match present c.r l with
          | Some p ->
              Fmt.str "(if v.%s() { Some(%s) } else { None })" (acc c.r p)
                (value c l inner)
          | None -> missing ()))
  | T.Single_elem { elem; _ } -> value c l elem
  | T.Where { inner; _ } -> value c l inner
  | T.Enum { base; _ } -> value c l base
  | T.Unit -> "()"
  | T.Codec { codec_id; _ } -> (
      match sub c.r l with
      | Some m ->
          Fmt.str "%s::of_view(v.%s().0?)?" (sub_ref c codec_id) (acc c.r m)
      | None -> missing ())
  | T.Byte_slice { size } | T.Byte_array { size } -> (
      let array = match typ with T.Byte_array _ -> true | _ -> false in
      match (bytes c.r l, size) with
      | Some (P.Bytes { at = Fixed _; _ } as m), _ ->
          if array then Fmt.str "*v.%s()" (acc c.r m)
          else Fmt.str "v.%s()" (acc c.r m)
      | Some m, T.Int n ->
          Fmt.str "%sv.%s().first_chunk::<%d>()?"
            (if array then "*" else "")
            (acc c.r m) n
      | Some m, _ -> Fmt.str "v.%s()" (acc c.r m)
      | None, _ -> missing ())
  | _ -> (
      match scalar c.r l with
      | Some m -> Fmt.str "v.%s()" (acc c.r m)
      | None -> missing ())

let pp_record ppf c =
  let rt = record_type c.all c.r in
  Fmt.pf ppf
    "/// The record's values: an optional field is an option, a field of bytes\n\
     /// borrows the buffer it was decoded from or holds its array, a record a\n\
     /// field holds its module's.\n\
     #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]\n\
     pub struct %s {\n"
    rt;
  List.iter
    (fun (_, n, T.Field f) ->
      Fmt.pf ppf "    pub %s: %s,\n" (ident n) (typ_text c f.field_typ))
    (named c.r.struct_);
  Fmt.pf ppf "}\n\n";
  Fmt.pf ppf
    "#[inline]\n\
     pub(crate) fn of_view<'a>(v: View<'a>) -> Option<%s> {\n\
    \    Some(Record {\n"
    rt;
  List.iter
    (fun (l, n, T.Field f) ->
      Fmt.pf ppf "        %s: %s,\n" (ident n) (value c l f.field_typ))
    (named c.r.struct_);
  Fmt.pf ppf "    })\n}\n\n"

(* {2 Encoding}

   [encode_at r buf p write path] walks [r] from [p] and answers where it
   ends, writing it when [write]: [size] is the same walk with no write, so
   the two cannot disagree on where a field goes. A size or a presence the
   values break is refused as a failed constraint at the field, and a value
   its field cannot hold as out of range, as the codec's encoder refuses
   them. A where clause the values break is refused at the record's start, as
   the codec names it, by [size] and by the encoder of a flat record
   ({!flat_record}); any other encoder leaves it to the check of the bytes
   written, which names the codec's first error. *)

(* The failure of the check [k] of [f] at [at], as the codec answers it. *)
let fail_check c ~at ~field (f : P.field) (k : P.check) =
  let kind =
    match k.kind with
    | Constraint ->
        Fmt.str
          "Kind::ConstraintFailed { which: Predicate::Field, value: Some(%s) }"
          f.id
    | Enum valid ->
        Fmt.str "Kind::InvalidEnum { value: %s, valid: &[%s] }" f.id
          (String.concat ", " (List.map int_lit valid))
    | Tag -> Fmt.str "Kind::InvalidTag(%s)" f.id
  in
  Fmt.str "if !(%s) { return Err(fail(%s, %s, %s)); }"
    (Walk.expr c.scope k.cond) at field kind

(* Whether the root is a record of constant size whose fields' values decide
   its checks alone: fields at constant offsets, where clauses, no record a
   field holds and no check on bytes. Its encoder then checks the values
   before it writes them, as the bytes would be checked once written. *)
let flat_record (c : ctx) =
  c.r.module_name = None && Option.is_some c.all.size && c.r.subs = []
  && List.for_all
       (function
         | P.Prefix (fields, _) ->
             List.for_all
               (fun (f : P.field) ->
                 match f.store with Bytes _ -> f.checks = [] | _ -> true)
               fields
         | P.Test _ -> true
         | P.Span _ | P.Gate _ | P.Region _ -> false)
       (P.nodes c.r.plan)

type enc = { ec : ctx; eb : Buffer.t; efresh : string -> string; flat : bool }

let rec plain : type a. store -> string -> a T.typ -> string =
 fun store e typ ->
  match typ with
  | T.Optional { inner; _ } ->
      Fmt.str "(match &%s { Some(x) => %s, None => 0 })" e
        (plain store "(*x)" inner)
  | T.Single_elem { elem; _ } -> plain store e elem
  | T.Where { inner; _ } -> plain store e inner
  | T.Enum { base; _ } -> plain store e base
  | _ -> ( match store with Bytes _ -> "0" | _ -> Fmt.str "(%s as i64)" e)

let pp_bindings e =
  List.iter
    (fun (f : P.field) ->
      match f.store with
      | Bytes _ -> ()
      | store -> (
          match field_of e.ec.r.struct_ f.label with
          | Some (n, T.Field fd) when f.name <> "" ->
              pr e.eb "let %s: i64 = %s;" f.id
                (plain store ("r." ^ ident n) fd.field_typ)
          | _ -> pr e.eb "let %s: i64 = 0;" f.id))
    (fields_of (P.nodes e.ec.r.plan))

let refuse_if e cond ~at ~field which =
  pr e.eb "if %s { return Err(refused(%s, %s, Predicate::%s)); }" cond at field
    which

let path_push path name =
  if name = "" then path else Fmt.str "%s.push(%s)" path (rust_string name)

let pp_word e ~p (o, base) bits =
  let ty = base_type base in
  let v =
    String.concat " | "
      (List.map (fun (id, sh) -> Fmt.str "((%s as %s) << %d)" id ty sh) bits)
  in
  match base with
  | T.U8 -> pr e.eb "put(buf, %s + %d, &[%s]);" p o v
  | T.U16 en | T.U32 en ->
      pr e.eb "put(buf, %s + %d, &(%s).to_%s_bytes());" p o v (endian_suffix en)

let pp_prefix e env ~p ~path fields n =
  pr e.eb "if write {";
  let words = ref [] in
  List.iter
    (fun (f : P.field) ->
      match f.store with
      | Int { width = 8; _ } ->
          pr e.eb "put(buf, %s + %d, &[%s as u8]);" p f.offset f.id
      | Int { width; endian; _ } ->
          pr e.eb "put(buf, %s + %d, &(%s as u%d).to_%s_bytes());" p f.offset
            f.id width (endian_suffix endian)
      | Bits { base; shift; width } ->
          pr e.eb
            "if %s < 0 || %s >> %d != 0 { return Err(fail(%s + %d, %s, \
             Kind::ValueOutOfRange { value: %s })); }"
            f.id f.id width p f.offset (path_push path f.name) f.id;
          let k = (f.offset, base) in
          let prev = Option.value ~default:[] (List.assoc_opt k !words) in
          words := (k, (f.id, shift) :: prev) :: List.remove_assoc k !words
      | Bytes k -> (
          match (f.name, List.assoc_opt f.label env) with
          | nm, Some x when nm <> "" ->
              refuse_if e
                (Fmt.str "%s.len() != %d" x k)
                ~at:(Fmt.str "%s + %d" p f.offset)
                ~field:(path_push path f.name) "Field";
              pr e.eb "put(buf, %s + %d, &%s[..]);" p f.offset x
          | _ -> pr e.eb "zeros(buf, %s + %d, %d);" p f.offset k))
    fields;
  List.iter (fun (k, bits) -> pp_word e ~p k (List.rev bits)) (List.rev !words);
  if e.flat then
    List.iter
      (fun (f : P.field) ->
        List.iter
          (fun k ->
            pr e.eb "%s"
              (fail_check e.ec
                 ~at:(Fmt.str "%s + %d" p f.offset)
                 ~field:(path_push path f.name) f k))
          f.checks)
      fields;
  pr e.eb "}";
  Fmt.str "(%s + %d)" p n

let rec pp_nodes e env ~p ~path nodes =
  List.fold_left
    (fun p node ->
      let next = pp_node e env ~p ~path node in
      let q = e.efresh "q" in
      pr e.eb "let %s: i64 = %s;" q next;
      q)
    p nodes

and pp_node e env ~p ~path node =
  let text c = Walk.expr e.ec.scope c in
  match node with
  | P.Prefix (fields, n) -> pp_prefix e env ~p ~path fields n
  | P.Test cond ->
      refuse_if e
        (Fmt.str "%s!(%s)" (if e.flat then "" else "!write && ") (text cond))
        ~at:"p" ~field:path "Where";
      p
  | P.Span (T.Ref (T.I, var)) when Option.is_some (sub_of e.ec var) -> (
      let s = Option.get (sub_of e.ec var) in
      match
        Option.bind s.path (function
          | [ l ] -> List.assoc_opt l env
          | _ -> None)
      with
      | Some x ->
          let name =
            match s.path with Some [ l ] -> name_in e.ec.r l | _ -> ""
          in
          Fmt.str "%s::encode_at(&%s, buf, %s, write, %s)?"
            (sub_ref e.ec s.codec_id) x p (path_push path name)
      | None -> invalid_arg "Rust_module: a record of no field")
  | P.Span size ->
      let named =
        List.find_map
          (function
            | P.Bytes { label; at = Sized n; _ } when n == node ->
                Option.map (fun x -> (x, label)) (List.assoc_opt label env)
            | _ -> None)
          e.ec.r.members
      in
      let n = e.efresh "n" in
      pr e.eb "let %s: i64 = %s;" n (size_expr e.ec.scope size);
      refuse_if e (n ^ " < 0") ~at:p ~field:path "Field";
      (match named with
      | Some (x, label) ->
          refuse_if e
            (Fmt.str "%s.len() as i64 != %s" x n)
            ~at:p
            ~field:(path_push path (name_in e.ec.r label))
            "Field";
          pr e.eb "if write { put(buf, %s, &%s[..]); }" p x
      | None -> pr e.eb "if write { zeros(buf, %s, %s); }" p n);
      Fmt.str "(%s + %s)" p n
  | P.Gate (cond, presence, _, body) ->
      pp_gate e env ~p ~path cond presence body
  | P.Region (size, at_most, body) ->
      let n = e.efresh "n" in
      pr e.eb "let %s: i64 = %s;" n (size_expr e.ec.scope size);
      refuse_if e (n ^ " < 0") ~at:p ~field:path "Field";
      let q = pp_nodes e env ~p ~path body in
      refuse_if e
        (Fmt.str "%s %s %s + %s" q (if at_most then ">" else "!=") p n)
        ~at:p ~field:path "Field";
      pr e.eb "if write { zeros(buf, %s, %s + %s - %s); }" q p n q;
      Fmt.str "(%s + %s)" p n

(* An optional field: its body where its condition holds, nothing where it
   does not; a value of the record's that its presence decides must agree. *)
and pp_gate e env ~p ~path cond presence body =
  let text c = Walk.expr e.ec.scope c in
  let walk env =
    let b = Buffer.create 256 in
    let inner = { e with eb = b } in
    let q = pp_nodes inner env ~p ~path body in
    (Buffer.contents b, q)
  in
  let g = e.efresh "g" in
  match label_of_presence presence with
  | Some l when Option.is_some (List.assoc_opt l env) ->
      let x = List.assoc l env in
      let inner = e.efresh "x" in
      let code, q = walk ((l, "(*" ^ inner ^ ")") :: env) in
      let field = path_push path (name_in e.ec.r l) in
      pr e.eb "let %s: i64 = match &%s {" g x;
      pr e.eb "Some(%s) => {" inner;
      refuse_if e (Fmt.str "!(%s)" (text cond)) ~at:p ~field "Field";
      Buffer.add_string e.eb code;
      pr e.eb "%s" q;
      pr e.eb "}";
      pr e.eb "None => {";
      refuse_if e (text cond) ~at:p ~field "Field";
      pr e.eb "%s" p;
      pr e.eb "}";
      pr e.eb "};";
      g
  | _ ->
      let code, q = walk env in
      pr e.eb "let %s: i64 = if %s {" g (text cond);
      Buffer.add_string e.eb code;
      pr e.eb "%s" q;
      pr e.eb "} else { %s };" p;
      g

let pp_encode ppf c =
  let b = Buffer.create 1024 in
  let e = { ec = c; eb = b; efresh = counter (); flat = flat_record c } in
  pp_bindings e;
  let env =
    List.map (fun (l, n, _) -> (l, "r." ^ ident n)) (named c.r.struct_)
  in
  let q = pp_nodes e env ~p:"p" ~path:"path" (P.nodes c.r.plan) in
  let rt = if borrows c.all c.r then "Record<'_>" else "Record" in
  Fmt.pf ppf
    "pub(crate) fn encode_at(r: &%s, buf: &mut [u8], p: i64, write: bool, \
     path: Path) -> Result<i64, Error> {\n\
     %s    Ok(%s)\n\
     }\n\n\
     /// The number of bytes [`encode`] writes for `r`, or the error it \
     refuses `r`\n\
     /// with.\n\
     #[inline]\n\
     pub fn size(r: &%s) -> Result<usize, Error> {\n\
    \    let n = encode_at(r, &mut [], 0, false, Path::ROOT)?;\n\
    \    usize::try_from(n).map_err(|_| fail(0, Path::ROOT, \
     Kind::ValueOutOfRange { value: n }))\n\
     }\n\n"
    rt (Buffer.contents b) q rt

(* {2 The codec's error}

   [error_at buf length p path] walks the record at [p] as the codec does and
   answers the first error it finds, as the codec names it: the record's
   fixed bytes, then each field in order, its bytes and then its checks, then
   the where clause. It answers where the record ends when it finds none.
   [check_pass] makes the checks of the fields at constant offsets whose bytes
   lie in the buffer first, as the codec does: of every field for the record
   it decodes, of the bitfields only for a record it enters. *)

let read_all known ids =
  let fields =
    List.filter (fun ((g : P.field), _) -> List.mem g.id ids) known
  in
  let stop =
    List.fold_left
      (fun a ((g : P.field), at) -> max a (at + width_bytes g.store))
      0 fields
  in
  (stop, fields)

let pp_check_pass b c =
  let bind fields =
    List.iter
      (fun ((g : P.field), at) -> bind_rd b g ~at:(Fmt.str "p0 + %d" at))
      fields
  in
  let pass_field known ~at (f : P.field) =
    if f.checks <> [] then begin
      let ids =
        f.id :: List.concat_map (fun (k : P.check) -> refs k.cond) f.checks
      in
      let stop, bound = read_all known ids in
      let bits = match f.store with Bits _ -> true | _ -> false in
      pr b "if %sp0 + %d <= length {" (if bits then "" else "all && ") stop;
      bind bound;
      List.iter
        (fun k ->
          pr b "%s"
            (fail_check c
               ~at:(Fmt.str "p0 + %d" (at + f.offset))
               ~field:(path_push "path" f.name) f k))
        f.checks;
      pr b "}"
    end
  in
  let rec go known at = function
    | P.Prefix (fields, n) :: rest ->
        let known =
          known @ List.map (fun (f : P.field) -> (f, at + f.offset)) fields
        in
        List.iter (pass_field known ~at) fields;
        go known (at + n) rest
    | P.Span (T.Ref (T.I, var)) :: rest when Option.is_some (sub_of c var) -> (
        let s = Option.get (sub_of c var) in
        let name = match s.path with Some [ l ] -> name_in c.r l | _ -> "" in
        pr b "%s::check_pass(all, buf, length, p0 + %d, %s)?;"
          (sub_ref c s.codec_id) at (path_push "path" name);
        match s.fixed_size with Some k -> go known (at + k) rest | None -> ())
    | P.Gate (cond, _, _, body) :: _ ->
        let stop, bound = read_all known (refs cond) in
        pr b "if p0 + %d <= length {" stop;
        bind bound;
        pr b "if %s {" (Walk.expr c.scope cond);
        go known at body;
        pr b "}";
        pr b "}"
    | P.Test _ :: rest -> go known at rest
    | _ -> ()
  in
  go [] 0 (P.nodes c.r.plan)

type err = { xc : ctx; xb : Buffer.t; xfresh : string -> string }

(* The label of the field a region holds. *)
let rec region_label c nodes =
  List.find_map
    (function
      | P.Prefix (f :: _, _) when f.name <> "" -> Some f.label
      | P.Span (T.Ref (T.I, var)) -> (
          match sub_of c var with
          | Some { path = Some [ l ]; _ } -> Some l
          | _ -> None)
      | P.Span _ as node ->
          List.find_map
            (function
              | P.Bytes { label; at = Sized n; _ } when n == node -> Some label
              | _ -> None)
            c.r.members
      | P.Gate (_, _, _, body) | P.Region (_, _, body) -> region_label c body
      | P.Prefix _ | P.Test _ -> None)
    nodes

(* Writes in [b] the check that answers [Kind::UnexpectedEof] where the
   condition the format gives holds. *)
let eof b ~at ~expected ~got ~field fmt =
  Fmt.kstr
    (fun cond -> pr b "eof_if(%s, %s, %s, %s, %s)?;" cond at expected got field)
    fmt

let pp_error_field ?short x ~p ~path (f : P.field) =
  let at = Fmt.str "(%s + %d)" p f.offset in
  let w = width_bytes f.store in
  let field = path_push path f.name in
  eof x.xb ~at ~expected:(string_of_int w)
    ~got:(Fmt.str "(length - %s)" at)
    ~field:
      (match short with
      | Some region -> region
      | None -> if f.checks = [] then path else field)
    "%s > length - %d" at w;
  match f.store with
  | Bytes _ -> ()
  | _ ->
      bind_rd x.xb f ~at;
      List.iter
        (fun k -> pr x.xb "%s" (fail_check x.xc ~at ~field f k))
        f.checks

let rec pp_errors ?short x ~p ~path nodes =
  List.fold_left
    (fun p node ->
      let next = pp_error ?short x ~p ~path node in
      let q = x.xfresh "q" in
      pr x.xb "let %s: i64 = %s;" q next;
      q)
    p nodes

and pp_error ?short x ~p ~path node =
  let text c = Walk.expr x.xc.scope c in
  match node with
  | P.Prefix (fields, n) ->
      List.iter (pp_error_field ?short x ~p ~path) fields;
      Fmt.str "(%s + %d)" p n
  | P.Test cond ->
      pr x.xb
        "if !(%s) { return Err(fail(p0, %s, Kind::ConstraintFailed { which: \
         Predicate::Where, value: None })); }"
        (text cond) path;
      p
  | P.Span (T.Ref (T.I, var)) when Option.is_some (sub_of x.xc var) ->
      let s = Option.get (sub_of x.xc var) in
      let name = match s.path with Some [ l ] -> name_in x.xc.r l | _ -> "" in
      Fmt.str "%s::error_at(buf, length, %s, %s)?" (sub_ref x.xc s.codec_id) p
        (path_push path name)
  | P.Span size ->
      let n = x.xfresh "n" in
      pr x.xb "let %s: i64 = %s;" n (size_expr x.xc.scope size);
      eof x.xb ~at:p ~expected:n
        ~got:(Fmt.str "(length - %s)" p)
        ~field:path "%s < 0 || %s > length - %s" n n p;
      Fmt.str "(%s + %s)" p n
  | P.Gate (cond, _, _, body) ->
      let g = x.xfresh "g" in
      pr x.xb "let %s: i64 = if %s {" g (text cond);
      let q = pp_errors x ~p ~path body in
      pr x.xb "%s } else { %s };" q p;
      g
  | P.Region (size, at_most, body) ->
      pp_error_region x ~p ~path size at_most body

and pp_error_region x ~p ~path size at_most body =
  let c = x.xc in
  let holds_record =
    match body with
    | [ P.Span (T.Ref (T.I, var)) ] -> Option.is_some (sub_of c var)
    | _ -> false
  in
  let scalar =
    match body with
    | [ P.Prefix ([ { store = Int _ | Bits _; _ } ], w) ] -> Some w
    | _ -> None
  in
  let n = x.xfresh "n" in
  let region =
    path_push path
      (Option.fold ~none:"" ~some:(name_in c.r) (region_label c body))
  in
  pr x.xb "let %s: i64 = %s;" n (size_expr c.scope size);
  eof x.xb ~at:p ~expected:n
    ~got:(Fmt.str "(length - %s)" p)
    ~field:(if Option.is_some scalar then path else region)
    "%s < 0 || %s > length - %s" n n p;
  let fill used =
    let exact = if at_most then "" else Fmt.str " || %s != %s" used n in
    eof x.xb
      ~at:(Fmt.str "(%s + %s.min(%s))" p used n)
      ~expected:n ~got:used ~field:path "%s > %s%s" used n exact
  in
  Option.iter (fun w -> fill (string_of_int w ^ "i64")) scalar;
  let window = x.xfresh "window" and used = x.xfresh "used" in
  pr x.xb "let %s: i64 = length.min(%s + %s);" window p n;
  pr x.xb "let %s: i64 = {" used;
  pr x.xb "let length = %s;" window;
  let short = if holds_record then None else Some region in
  let q = pp_errors ?short x ~p ~path body in
  pr x.xb "%s - %s" q p;
  pr x.xb "};";
  fill used;
  Fmt.str "(%s + %s)" p n

let pp_error_at ppf c =
  let pass = Buffer.create 256 in
  pp_check_pass pass c;
  let b = Buffer.create 1024 in
  let x = { xc = c; xb = b; xfresh = counter () } in
  let min = min_size c.r in
  eof b ~at:"p0" ~expected:(string_of_int min) ~got:"(length - p0)"
    ~field:"path" "p0 > length - %d" min;
  let q = pp_errors x ~p:"p0" ~path:"path" (P.nodes c.r.plan) in
  Fmt.pf ppf
    "pub(crate) fn check_pass(all: bool, buf: &[u8], length: i64, p0: i64, \
     path: Path) -> Result<(), Error> {\n\
     %s    Ok(())\n\
     }\n\n\
     pub(crate) fn error_at(buf: &[u8], length: i64, p0: i64, path: Path) -> \
     Result<i64, Error> {\n\
    \    check_pass(%b, buf, length, p0, path)?;\n\
     %s    Ok(%s)\n\
     }\n\n"
    (Buffer.contents pass) (c.r.module_name = None) (Buffer.contents b) q

(* [end_at buf p] is where the record at [p] ends, placed from its bytes. *)
let pp_end_at ppf c =
  let b = Buffer.create 256 in
  let l = { c; b; fresh = counter () } in
  find l End (P.nodes c.r.plan) "p" ~finish:(fun ~size:_ p -> pr b "Some(%s)" p);
  Fmt.pf ppf
    "#[inline]\n\
     pub(crate) fn end_at(buf: &[u8], p: i64) -> Option<i64> {\n\
     %s}\n\n"
    (Buffer.contents b)

(* {2 A record's module} *)

let pp_view ppf c =
  Fmt.pf ppf
    "/// The bytes at constant offsets a view of the record holds.\n\
     const FIXED: usize = %d;\n\n\
     /// A record that passed its check, in the buffer it borrows: the borrow\n\
     /// keeps its bytes from changing while the view lives.\n\
     #[derive(Clone, Copy, Debug)]\n\
     pub struct View<'a> {\n\
    \    buf: &'a [u8],\n\
    \    at: usize,\n\
    \    head: &'a [u8; FIXED],\n\
     }\n\n\
     /// The answer of a check: a view of the record, or none.\n\
     #[derive(Clone, Copy, Debug)]\n\
     pub struct Check<'a>(pub(crate) Option<View<'a>>);\n\n\
     impl<'a> Check<'a> {\n\
    \    /// Whether the check passed.\n\
    \    #[inline(always)]\n\
    \    pub fn is_ok(&self) -> bool {\n\
    \        self.0.is_some()\n\
    \    }\n\n\
    \    /// The view of the record the check passed.\n\
    \    ///\n\
    \    /// # Panics\n\
    \    ///\n\
    \    /// If the check failed: a refused record is input, test\n\
    \    /// [`Check::is_ok`] first.\n\
    \    #[inline(always)]\n\
    \    pub fn get(self) -> View<'a> {\n\
    \        match self.0 {\n\
    \            Some(v) => v,\n\
    \            None => failed(),\n\
    \        }\n\
    \    }\n\
     }\n\n\
     #[cold]\n\
     #[inline(never)]\n\
     fn failed() -> ! {\n\
    \    panic!(\"Check::get: the check failed\")\n\
     }\n\n\
     /// The record at `p` in `buf`, its fixed bytes in `buf`, with no check.\n\
     #[inline(always)]\n\
     pub(crate) fn at(buf: &[u8], p: i64) -> Check<'_> {\n\
    \    let Ok(at) = usize::try_from(p) else {\n\
    \        return Check(None);\n\
    \    };\n\
    \    match buf.get(at..).and_then(|s| s.first_chunk::<FIXED>()) {\n\
    \        Some(head) => Check(Some(View { buf, at, head })),\n\
    \        None => Check(None),\n\
    \    }\n\
     }\n\n\
     /// The offset in its buffer of the record `v`, which the setters take.\n\
     #[inline(always)]\n\
     pub fn offset(v: &View<'_>) -> usize {\n\
    \    v.at\n\
     }\n\n\
     impl<'a> View<'a> {\n"
    c.r.fixed;
  List.iter (pp_accessor ppf c) c.r.members;
  Fmt.pf ppf "}\n\n";
  List.iter (pp_setter ppf) (P.settable c.r)

let pp_record_module ppf c =
  pp_view ppf c;
  pp_record ppf c;
  pp_encode ppf c;
  pp_error_at ppf c;
  if Option.is_none c.r.fixed_size then pp_end_at ppf c

(* {2 The root} *)

(* The root's [check], and the call of it over the bytes from [off]. *)
let root_check ~size =
  match size with
  | Some _ ->
      ( "/// The check of the record at `off` in `buf`, its bytes from there:\n\
         /// the checks the codec makes, in its order. It never panics: bad\n\
         /// bounds or a bad record give a check [`Check::is_ok`] refuses.\n\
         #[inline]\n\
         pub fn check(buf: &[u8], off: usize) -> Check<'_> {\n\
        \    match buf.get(off..) {\n\
        \        Some(frame) if check_frame(frame).is_some() => at(buf, off as \
         i64),\n\
        \        _ => Check(None),\n\
        \    }\n\
         }\n\n",
        "check(buf, off)" )
  | None ->
      ( "/// The check of the record at `off` in the `len` bytes there of \
         `buf`: the\n\
         /// checks the codec makes, in its order. It never panics: bad bounds \
         or a\n\
         /// bad record give a check [`Check::is_ok`] refuses.\n\
         #[inline]\n\
         pub fn check(buf: &[u8], off: usize, len: usize) -> Check<'_> {\n\
        \    match off.checked_add(len).and_then(|end| buf.get(off..end)) {\n\
        \        Some(frame) if check_frame(frame).is_some() => at(buf, off as \
         i64),\n\
        \        _ => Check(None),\n\
        \    }\n\
         }\n\n",
        "check(buf, off, buf.len().saturating_sub(off))" )

(* The root's error where its check fails: the first the codec finds. *)
let first_error =
  {|#[cold]
#[inline(never)]
fn first_error(buf: &[u8], length: usize, off: usize) -> Error {
    if off > length {
        return Error { at: off, field: Path::ROOT, kind: Kind::UnexpectedEof { expected: MIN_SIZE, got: 0 } };
    }
    match error_at(buf, length as i64, off as i64, Path::ROOT) {
        Err(e) => e,
        Ok(_) => Error { at: off, field: Path::ROOT, kind: Kind::ConstraintFailed { which: Predicate::Where, value: None } },
    }
}

|}

(* The body of [encode]. A record of constant size is [size] bytes, so its
   bounds are tested before its values, as the codec tests them. A flat
   record ({!flat_record}) is written into the array of its bytes, whose
   stores need no bound of their own, its values checked before; any other
   is written into [buf] and its bytes checked again. A record of variable
   size is measured first, from 0: an error of the measure is moved to
   [off]. *)
let written c ~size ~own_check =
  let eof =
    "        return Err(Error { at: off, field: Path::ROOT, kind: \
     Kind::UnexpectedEof { expected: n, got } });\n"
  in
  let into_buf =
    Fmt.str
      "    if off > buf.len() || n > got {\n\
       %s    }\n\
      \    encode_at(r, buf, off as i64, true, Path::ROOT)?;\n\
      \    let buf: &[u8] = buf;\n\
      \    if %s.is_ok() {\n\
      \        Ok(())\n\
      \    } else {\n\
      \        Err(first_error(buf, off + n, off))\n\
      \    }\n"
      eof own_check
  in
  match size with
  | Some k when flat_record c ->
      Fmt.str
        "    let n: usize = %d;\n\
        \    let got = buf.len().saturating_sub(off);\n\
        \    let Some(b) = buf.get_mut(off..).and_then(|s| \
         s.first_chunk_mut::<%d>()) else {\n\
         %s    };\n\
        \    match encode_at(r, b, 0, true, Path::ROOT) {\n\
        \        Ok(_) => Ok(()),\n\
        \        Err(e) => Err(Error { at: e.at + off, ..e }),\n\
        \    }\n"
        k k eof
  | Some k ->
      Fmt.str
        "    let n: usize = %d;\n\
        \    let got = buf.len().saturating_sub(off);\n\
         %s"
        k into_buf
  | None ->
      Fmt.str
        "    let n = match size(r) {\n\
        \        Ok(n) => n,\n\
        \        Err(e) => return Err(match e.at.checked_add(off) {\n\
        \            Some(at) => Error { at, ..e },\n\
        \            None => Error { at: off, field: Path::ROOT, kind: \
         Kind::UnexpectedEof { expected: MIN_SIZE, got: 0 } },\n\
        \        }),\n\
        \    };\n\
        \    let got = buf.len().saturating_sub(off);\n\
         %s"
        into_buf

let pp_root ppf ~size c =
  let rt = if borrows c.all c.r then "Record<'_>" else "Record" in
  let check, window = root_check ~size in
  let own_check =
    match size with Some _ -> "check(buf, off)" | None -> "check(buf, off, n)"
  in
  Fmt.pf ppf
    "%s%s/// Checks the record at `off` in `buf` as the codec's `validate` \
     does, with\n\
     /// its error.\n\
     #[inline]\n\
     pub fn validate(buf: &[u8], off: usize) -> Result<(), Error> {\n\
    \    if %s.is_ok() {\n\
    \        Ok(())\n\
    \    } else {\n\
    \        Err(first_error(buf, buf.len(), off))\n\
    \    }\n\
     }\n\n\
     /// The record at `off` in `buf`, checked as [`validate`] checks it, or the\n\
     /// codec's error.\n\
     #[inline]\n\
     pub fn decode(buf: &[u8], off: usize) -> Result<%s, Error> {\n\
    \    match %s.0.and_then(of_view) {\n\
    \        Some(r) => Ok(r),\n\
    \        None => Err(first_error(buf, buf.len(), off)),\n\
    \    }\n\
     }\n\n\
     /// Writes `r` at `off` in `buf`, as the codec's `encode` does. Refuses a\n\
     /// record that does not fit, a value its field cannot hold, values that\n\
     /// break a size, a presence or a where clause, and a record that fails\n\
     /// its own checks once written; the bytes written are then scrap.\n\
     #[inline]\n\
     pub fn encode(r: &%s, buf: &mut [u8], off: usize) -> Result<(), Error> {\n\
     %s}\n\n"
    check first_error window rt window rt
    (written c ~size ~own_check)

let pp_check_frame ppf plan =
  let code, _ = body ~depth:1 plan [] ~frame:"frame" ~finish:[ "Some(())" ] in
  Fmt.pf ppf
    "#[inline(always)]\nfn check_frame(frame: &[u8]) -> Option<()> {\n%s}\n\n"
    code

(* {2 The fold}

   A path names a field by the accessor names from the codec down, as
   [Wire_ocaml] reads it. The fold hands [f] its value, an option where the
   field is optional, and one option more through a layer, [None] where the
   innermost optional layer of the path is absent. *)

type arg = {
  value : P.output;
  own : P.output option;
  layered : bool;
  layer : P.output option;
}

(* Each output of [nodes] with the presences of the optional fields around
   it, outermost first. *)
let rec placed around = function
  | [] -> []
  | P.Gate (_, p, _, body) :: rest ->
      placed (around @ [ p ]) body @ placed around rest
  | P.Region (_, _, body) :: rest -> placed around body @ placed around rest
  | P.Prefix (fields, _) :: rest ->
      List.filter_map
        (fun (f : P.field) ->
          Option.map (fun (o : P.output) -> (o.name, (o, around))) f.output)
        fields
      @ placed around rest
  | (P.Span _ | P.Test _) :: rest -> placed around rest

(* A step of a path names a member by its accessor, which [Wire_ocaml] may
   have suffixed with ['_'] for a name of its own module. *)
let unescaped n =
  let k = ref (String.length n) in
  while !k > 0 && n.[!k - 1] = '_' do
    decr k
  done;
  String.sub n 0 !k

let fold_arg ~codec plan (all : P.records) path =
  let refuse fmt = Fmt.kstr (refuse ~codec) fmt in
  let global prefix n = if prefix = "" then n else prefix ^ "_" ^ n in
  let member (r : P.record) step =
    match List.find_opt (fun m -> member_name m = step) r.members with
    | Some m -> Some m
    | None ->
        List.find_opt
          (fun m -> unescaped (member_name m) = unescaped step)
          r.members
  in
  let rec go prefix (r : P.record) layers = function
    | [] -> refuse "the fold path %S names no field" path
    | [ leaf ] -> (
        match member r leaf with
        | Some (P.Scalar { field = { output = Some o; _ }; _ }) ->
            (global prefix o.name, layers)
        | Some _ -> refuse "the fold path %S names a field no fold reads" path
        | None -> refuse "the fold path %S names no field %S" path leaf)
    | step :: rest -> (
        match member r step with
        | Some (P.Sub { sub = { path = Some p; _ } as sub; presence; _ }) ->
            let layer =
              Option.map (fun (o : P.output) -> global prefix o.name) presence
            in
            go
              (global prefix (String.concat "_" p))
              (record_of_id all sub.codec_id)
              (Option.to_list layer @ layers)
              rest
        | Some _ ->
            refuse "the fold path %S goes through %S, which holds no record"
              path step
        | None -> refuse "the fold path %S names no field %S" path step)
  in
  let steps = String.split_on_char '.' path in
  let name, layers = go "" all.root [] steps in
  let o, around =
    match List.assoc_opt name (placed [] (P.nodes plan)) with
    | Some x -> x
    | None -> refuse "the fold path %S names a field no fold reads" path
  in
  let is_layer (g : P.output) = List.mem g.name layers in
  let own =
    match List.rev around with
    | g :: _ when not (is_layer g) -> Some g
    | _ -> None
  in
  {
    value = o;
    own;
    layered = List.length steps > 1;
    layer = List.find_opt is_layer (List.rev around);
  }

let arg_type a =
  let t = output_type a.value in
  let t = if Option.is_some a.own then Fmt.str "Option<%s>" t else t in
  if a.layered then Fmt.str "Option<%s>" t else t

let arg_value a =
  let opt p v =
    match p with
    | None -> Fmt.str "Some(%s)" v
    | Some (p : P.output) -> Fmt.str "if %s { Some(%s) } else { None }" p.id v
  in
  let v = Fmt.str "(%s)" (output_value a.value) in
  let v = if Option.is_some a.own then opt a.own v else v in
  if a.layered then opt a.layer v else v

(* The outputs the walk of the fold binds: each field's and the presences
   that make it an option, in the plan's order. *)
let fold_outputs plan args =
  let used =
    List.concat_map
      (fun a -> (a.value :: Option.to_list a.own) @ Option.to_list a.layer)
      args
  in
  List.filter
    (fun (o : P.output) -> List.exists (fun (u : P.output) -> u.id = o.id) used)
    (P.outputs plan)

let tuple = function
  | [] -> "()"
  | [ x ] -> Fmt.str "(%s,)" x
  | xs -> Fmt.str "(%s)" (String.concat ", " xs)

(* [fold]: [fold_frame], the walk of one record that answers the fields its
   [f] takes, and the loop over the records as [records] places them, with
   [window_error] the error [validate] answers on a record's window alone.
   A record of constant [size] back to back is one slot of that size each. *)
let pp_fold ppf ~codec ~size plan all paths =
  let args = List.map (fold_arg ~codec plan all) paths in
  let outputs = fold_outputs plan args in
  let code, _ =
    body ~depth:1 plan outputs ~frame:"frame"
      ~finish:[ Fmt.str "Some(%s)" (tuple (List.map arg_value args)) ]
  in
  let types = List.map arg_type args in
  let xs = List.mapi (fun i _ -> Fmt.str "x%d" i) args in
  let call = Fmt.str "f(%s)" (String.concat ", " ("acc" :: xs)) in
  let bind = Fmt.str "let %s = t;" (tuple xs) in
  let paths_doc =
    match paths with
    | [] -> "no field"
    | ps -> String.concat ", " (List.map (Fmt.str "`%s`") ps)
  in
  let back =
    match size with
    | Some 0 ->
        "            panic!(\"fold: records of no bytes back to back\");\n"
    | Some n ->
        Fmt.str
          "            let mut off = 0usize;\n\
          \            while off < buf.len() {\n\
          \                match buf.get(off..off + %d).and_then(fold_frame) {\n\
          \                    Some(t) => {\n\
          \                        %s\n\
          \                        acc = %s;\n\
          \                    }\n\
          \                    None => return Err((window_error(buf, off, \
           buf.len() - off), acc)),\n\
          \                }\n\
          \                off += %d;\n\
          \            }\n\
          \            Ok(acc)\n"
          n bind call n
    | None ->
        Fmt.str
          "            let mut off = 0usize;\n\
          \            while off < buf.len() {\n\
          \                match fold_frame(&buf[off..]) {\n\
          \                    Some(t) => {\n\
          \                        %s\n\
          \                        acc = %s;\n\
          \                    }\n\
          \                    None => return Err((window_error(buf, off, \
           buf.len() - off), acc)),\n\
          \                }\n\
          \                match end_at(buf, off as i64).and_then(|e| \
           usize::try_from(e).ok()) {\n\
          \                    Some(next) if next > off => off = next,\n\
          \                    _ => return Ok(acc),\n\
          \                }\n\
          \            }\n\
          \            Ok(acc)\n"
          bind call
  in
  let refused at len =
    Fmt.str
      "match invalid.as_mut() {\n\
      \                        Some(g) => acc = g(acc),\n\
      \                        None => return Err((window_error(buf, %s, %s), \
       acc)),\n\
      \                    }"
      at len
  in
  Fmt.pf ppf
    "#[inline(always)]\n\
     fn fold_frame(frame: &[u8]) -> Option<%s> {\n\
     %s}\n\n\
     /// The error [`validate`] answers on the `len` bytes at `off` of `buf`\n\
     /// alone, at its offset in `buf`.\n\
     #[cold]\n\
     #[inline(never)]\n\
     fn window_error(buf: &[u8], off: usize, len: usize) -> Error {\n\
    \    let first = off.min(buf.len());\n\
    \    let stop = first.saturating_add(len).min(buf.len());\n\
    \    match validate(&buf[first..stop], 0) {\n\
    \        Err(e) => Error { at: e.at.saturating_add(first), ..e },\n\
    \        Ok(()) => Error { at: off, field: Path::ROOT, kind: \
     Kind::ConstraintFailed { which: Predicate::Where, value: None } },\n\
    \    }\n\
     }\n\n\
     /// Where the records of a [`fold`] lie in its buffer.\n\
     #[derive(Clone, Copy, Debug)]\n\
     pub enum Records<'r> {\n\
    \    /// Back to back from offset 0, each where the one before ends, up to\n\
    \    /// the end of the buffer.\n\
    \    BackToBack,\n\
    \    /// In slots of this many bytes, the `i`-th at `i * stride`, over the\n\
    \    /// whole slots of the buffer.\n\
    \    Slots(usize),\n\
    \    /// In frames, the `i`-th the `lens[i]` bytes at `offs[i]`.\n\
    \    Frames(&'r [usize], &'r [usize]),\n\
     }\n\n\
     /// Checks each record of `buf`, as [`check`] does, and folds `f` over\n\
     /// those that pass, from `init`: `f(acc, x1, ..., xn)`, the fields %s of\n\
     /// the record, read during its check, a field under a layer `None` where\n\
     /// the layer is absent. A record in a slot or a frame is checked in its\n\
     /// window alone.\n\
     ///\n\
     /// A refused record stops the fold with `Err((e, acc))`, `e` the error\n\
     /// [`validate`] answers on its window and `acc` the fold of the records\n\
     /// before it; in slots or frames, with `invalid`, `invalid(acc)` is the\n\
     /// accumulator after a refused record and the fold goes on.\n\
     ///\n\
     /// # Panics\n\
     ///\n\
     /// If the stride of [`Records::Slots`] is 0, if the slices of\n\
     /// [`Records::Frames`] differ in length, and if `invalid` is given with\n\
     /// [`Records::BackToBack`].\n\
     #[inline]\n\
     pub fn fold<A, F>(\n\
    \    buf: &[u8],\n\
    \    records: Records<'_>,\n\
    \    init: A,\n\
    \    mut invalid: Option<&mut dyn FnMut(A) -> A>,\n\
    \    mut f: F,\n\
     ) -> Result<A, (Error, A)>\n\
     where\n\
    \    F: FnMut(%s) -> A,\n\
     {\n\
    \    let mut acc = init;\n\
    \    match records {\n\
    \        Records::BackToBack => {\n\
    \            assert!(invalid.is_none(), \"fold: invalid with records back \
     to back\");\n\
     %s        }\n\
    \        Records::Slots(stride) => {\n\
    \            assert!(stride > 0, \"fold: a stride of 0\");\n\
    \            for (i, frame) in buf.chunks_exact(stride).enumerate() {\n\
    \                match fold_frame(frame) {\n\
    \                    Some(t) => {\n\
    \                        %s\n\
    \                        acc = %s;\n\
    \                    }\n\
    \                    None => %s\n\
    \                }\n\
    \            }\n\
    \            Ok(acc)\n\
    \        }\n\
    \        Records::Frames(offs, lens) => {\n\
    \            assert!(offs.len() == lens.len(), \"fold: offs and lens \
     differ in length\");\n\
    \            for (&off, &len) in offs.iter().zip(lens) {\n\
    \                match off.checked_add(len).and_then(|end| \
     buf.get(off..end)).and_then(fold_frame) {\n\
    \                    Some(t) => {\n\
    \                        %s\n\
    \                        acc = %s;\n\
    \                    }\n\
    \                    None => %s\n\
    \                }\n\
    \            }\n\
    \            Ok(acc)\n\
    \        }\n\
    \    }\n\
     }\n\n"
    (tuple types) code paths_doc
    (String.concat ", " ("A" :: types))
    back bind call
    (refused "i * stride" "stride")
    bind call (refused "off" "len")

(* {2 Names} *)

type source = Own | Field of string

(* The names a record's module defines for itself: its functions, its
   setters and, for the root, its fold's. *)
let record_values ~fold root (r : P.record) =
  let own l = List.map (fun n -> (Own, n)) l in
  own
    ([
       "at";
       "offset";
       "of_view";
       "encode_at";
       "size";
       "check_pass";
       "error_at";
       "end_at";
       "get";
       "rd";
       "span";
       "put";
       "zeros";
       "fail";
       "eof_if";
       "refused";
       "sat";
       "pos";
       "lsl";
       "lsr";
     ]
    @
    if root then
      [ "check"; "check_frame"; "first_error"; "validate"; "decode"; "encode" ]
      @
      if fold then [ "fold"; "fold_frame"; "window_error"; "failed" ]
      else [ "failed" ]
    else [ "failed" ])
  @ List.map
      (fun (_, (f : P.field)) -> (Field f.name, "set_" ^ ident f.name))
      (P.settable r)

let check_names ~codec ~(all : P.records) ~fold =
  let clash l =
    match Wire.Private.Names.clash l with
    | Some (Own, Field f, n) | Some (Field f, Own, n) ->
        Fmt.kstr (refuse ~codec)
          "field %S defines %s, which the generated module defines for itself" f
          n
    | Some (Field g, Field f, n) ->
        Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
    | Some (Own, Own, _) | None -> ()
  in
  let own l = List.map (fun n -> (Own, n)) l in
  let accessors (r : P.record) =
    List.map (fun m -> (Field (member_name m), acc r m)) r.members
  in
  List.iter
    (fun (r : P.record) ->
      clash (record_values ~fold (r.module_name = None) r);
      clash (accessors r);
      clash (List.map (fun (_, n, _) -> (Field n, ident n)) (named r.struct_)))
    (all.root :: all.records);
  (* Modules: each record's and the types. *)
  clash
    (own
       ([ "View"; "Check"; "Record"; "Error"; "Kind"; "Path"; "Predicate" ]
       @ if fold then [ "Records" ] else [])
    @ List.map (fun (r : P.record) -> (Own, module_of r)) all.records)

(* {1 The module} *)

let uses_up =
  "use super::{fail, eof_if, refused, get, rd, span, put, zeros, sat, pos, \
   lsl, lsr, Error, Kind, Path, Predicate};"

(* Whether a walk of [plan], the check's or the fold's of [outputs], binds a
   slot. *)
let has_slots plan outputs = P.slots plan outputs <> [] || P.slots plan [] <> []

let source ~codec ~fold c =
  let planned f =
    try f () with Invalid_argument why -> raise (Unsupported why)
  in
  let all = planned (fun () -> P.records c) in
  let plan = planned (fun () -> P.v c) in
  check_names ~codec ~all ~fold:(Option.is_some fold);
  let outputs =
    match fold with
    | None -> []
    | Some paths ->
        fold_outputs plan (List.map (fold_arg ~codec plan all) paths)
  in
  let b = Buffer.create 65536 in
  let ppf = Format.formatter_of_buffer b in
  let ctx (r : P.record) ~up = { all; r; scope = P.Scope.v r.plan; up } in
  let root = ctx all.root ~up:"" in
  pp_root ppf ~size:all.size root;
  pp_record_module ppf root;
  pp_check_frame ppf plan;
  Option.iter (pp_fold ppf ~codec ~size:all.size plan all) fold;
  List.iter
    (fun (r : P.record) ->
      Fmt.pf ppf "/// The records of the codec `%s`.\npub mod %s {\n%s\n\n"
        (doc_string r.codec_name) (module_of r) uses_up;
      pp_record_module ppf (ctx r ~up:"super::");
      Fmt.pf ppf "}\n\n")
    all.records;
  Format.pp_print_flush ppf ();
  let head =
    header ~codec ~min:(min_size all.root) ~slots:(has_slots plan outputs)
  in
  Fmt.str
    "%a\n\
     /// The most field names on the path to a field.\n\
     const DEPTH: usize = %d;\n\n\
     %s\n\
     %s\n\
     %s"
    pp_lines head (depth_of all) error_prelude shift_prelude (Buffer.contents b)

let rs ~codec ?fold c =
  try source ~codec ~fold c with Unsupported why -> refuse ~codec why
