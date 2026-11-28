open Wire.Private
open Layout
module T = Types
module P = Wire_ocaml.Plan

exception Unsupported of string

let fail_unsupported fmt = Fmt.kstr (fun s -> raise (Unsupported s)) fmt
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

let doc_string s = String.escaped s

(* {1 Expressions}

   A constraint is OCaml arithmetic on 63-bit [int]s, which the generated code
   computes on [i64] values that always hold such an [int]. An integer
   expression that can leave them ([P.Scope.wide]) is compared on [u64], as
   the codec computes it on 64 bits and 3D at the type that holds it; any
   other stays inside them. OCaml's [lsl] and [lsr] are Rust shifts where the
   ranges show they agree (a nonnegative operand, a constant count below 63
   and, for [lsl], no bit shifted out), and otherwise follow what OCaml does on
   the tagged integer. *)

let int_lit n = if n < 0 then Fmt.str "(%di64)" n else Fmt.str "%di64" n

(* [e] as an [int] expression when it is one. *)
let as_int : type a. a T.expr -> int T.expr option = function
  | T.Int _ as e -> Some e
  | T.Ref (T.I, _) as e -> Some e
  | T.Add _ as e -> Some e
  | T.Sub _ as e -> Some e
  | T.Mul _ as e -> Some e
  | T.Mod _ as e -> Some e
  | T.Land _ as e -> Some e
  | T.Lor _ as e -> Some e
  | T.Lxor _ as e -> Some e
  | T.Lnot _ as e -> Some e
  | T.Lsl _ as e -> Some e
  | T.Lsr _ as e -> Some e
  | T.Cast _ as e -> Some e
  | T.If_then_else _ as e -> Some e
  | _ -> None

let rec expr : type a. P.Scope.t -> a T.expr -> string =
 fun scope e ->
  let r : type b. b T.expr -> string = fun e -> expr scope e in
  let op o a b = Fmt.str "(%s %s %s)" (r a) o (r b) in
  let cmp : type b. string -> b T.expr -> b T.expr -> string =
   fun o a b ->
    match (as_int a, as_int b) with
    | Some x, Some y when P.Scope.wide scope x || P.Scope.wide scope y ->
        Fmt.str "(%s %s %s)" (wide scope x) o (wide scope y)
    | _ -> op o a b
  in
  let range e = P.Scope.range scope e in
  match e with
  | T.Int n -> int_lit n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, v) -> v
  | T.Add (a, b) -> op "+" a b
  | T.Sub (a, b) -> op "-" a b
  | T.Mul (a, b) -> op "*" a b
  | T.Mod (a, b) -> op "%" a b
  | T.Land (a, b) -> op "&" a b
  | T.Lor (a, b) -> op "|" a b
  | T.Lxor (a, b) -> op "^" a b
  | T.Lnot a -> Fmt.str "(!%s)" (r a)
  | T.Lsl (a, T.Int n)
    when n >= 0 && n < 63
         &&
         let lo, hi = range a in
         lo >= 0 && hi <= max_int lsr n ->
      Fmt.str "(%s << %d)" (r a) n
  | T.Lsl (a, b) -> Fmt.str "lsl(%s, %s)" (r a) (r b)
  | T.Lsr (a, T.Int n) when n >= 0 && n < 63 && fst (range a) >= 0 ->
      Fmt.str "(%s >> %d)" (r a) n
  | T.Lsr (a, b) -> Fmt.str "lsr(%s, %s)" (r a) (r b)
  | T.Eq (a, b) -> cmp "==" a b
  | T.Ne (a, b) -> cmp "!=" a b
  | T.Lt (a, b) -> cmp "<" a b
  | T.Le (a, b) -> cmp "<=" a b
  | T.Gt (a, b) -> cmp ">" a b
  | T.Ge (a, b) -> cmp ">=" a b
  | T.And (a, b) -> op "&&" a b
  | T.Or (a, b) -> op "||" a b
  | T.Not a -> Fmt.str "(!%s)" (r a)
  | T.Cast (`U8, a) -> Fmt.str "(%s & 0xff)" (r a)
  | T.Cast (`U16, a) -> Fmt.str "(%s & 0xffff)" (r a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s { %s } else { %s })" (r c) (r a) (r b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* [e] as a [u64], which 3D proves holds its value: its variables are below
   [2^32], so the [u64] operations compute it exactly. *)
and wide scope (e : int T.expr) =
  let r = wide scope in
  let op o a b = Fmt.str "(%s %s %s)" (r a) o (r b) in
  let distance (b : int T.expr) =
    match b with
    | T.Int n -> string_of_int n
    | _ -> Fmt.str "(%s as u32)" (expr scope b)
  in
  match e with
  | T.Int n -> Fmt.str "%du64" n
  | T.Ref (T.I, v) -> Fmt.str "(%s as u64)" v
  | T.Add (a, b) -> op "+" a b
  | T.Sub (a, b) -> op "-" a b
  | T.Mul (a, b) -> op "*" a b
  | T.Mod (a, b) -> op "%" a b
  | T.Land (a, b) -> op "&" a b
  | T.Lor (a, b) -> op "|" a b
  | T.Lxor (a, b) -> op "^" a b
  | T.Lnot a -> Fmt.str "(!%s)" (r a)
  | T.Lsl (a, b) -> Fmt.str "(%s << %s)" (r a) (distance b)
  | T.Lsr (a, b) -> Fmt.str "(%s >> %s)" (r a) (distance b)
  | T.Cast (`U8, a) -> Fmt.str "(%s & 0xff)" (r a)
  | T.Cast (`U16, a) -> Fmt.str "(%s & 0xffff)" (r a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s { %s } else { %s })" (expr scope c) (r a) (r b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* A size as an [i64]: one that can leave the native int is computed on
   [u64] and saturated at the largest [i64], past any buffer. *)
let size_expr scope e =
  if P.Scope.wide scope e then Fmt.str "sat(%s)" (wide scope e)
  else expr scope e

let shift_prelude =
  {|// OCaml's [lsl] and [lsr] on the tagged integer [2a + 1], with the count
// taken modulo 64 as the machine does.
#[inline(always)]
fn lsl(a: i64, b: i64) -> i64 { (a << 1).wrapping_shl(b as u32) >> 1 }

#[inline(always)]
fn lsr(a: i64, b: i64) -> i64 {
    ((((a << 1) | 1) as u64).wrapping_shr(b as u32) as i64) >> 1
}
|}

(* {1 Rust types and loads} *)

let endian_suffix = function T.Big -> "be" | T.Little -> "le"
let base_endian = function T.U8 -> T.Big | T.U16 e | T.U32 e -> e
let base_type = function T.U8 -> "u8" | T.U16 _ -> "u16" | T.U32 _ -> "u32"

let bits_type width =
  if width <= 8 then "u8" else if width <= 16 then "u16" else "u32"

let type_bits width = if width <= 8 then 8 else if width <= 16 then 16 else 32

let store_type = function
  | Int { width = 8; signed; _ } -> if signed then "i8" else "u8"
  | Int { width = 16; signed; _ } -> if signed then "i16" else "u16"
  | Int { signed; _ } -> if signed then "i32" else "u32"
  | Bits { width; _ } -> bits_type width
  | Bytes n -> Fmt.str "[u8; %d]" n

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

(* A field's value, in its Rust type, from the array [b] that holds its bytes
   from [k]. *)
let read_at b k (store : store) =
  match store with
  | Int { width = 8; signed; _ } ->
      if signed then Fmt.str "(%s[%d] as i8)" b k else Fmt.str "%s[%d]" b k
  | Int { width; endian; _ } -> load b k (width / 8) endian (store_type store)
  | Bits { base; shift; width } ->
      of_word ~base ~shift ~width
        (load b k (Bitfield.byte_size base) (base_endian base) (base_type base))
  | Bytes n ->
      Fmt.str "{ let mut a = [0u8; %d]; a.copy_from_slice(&%s[%d..%d]); a }" n b
        k (k + n)

let pp_lines ppf l = List.iter (Fmt.pf ppf "%s@\n") l

(* {1 The walk}

   The plan is [Wire_ocaml.Plan], the one the folds of [Wire_ocaml] write out,
   and so are its decisions: which variables are bound, which arithmetic is
   computed on [u64], and the decision tree of a run of optional fields. This
   section only spells them in Rust. A region is a slice of exactly its bytes
   and the cursor is that slice's remaining tail: a prefix's check against its
   region is [split_first_chunk], which hands back the prefix as an array its
   loads index at constant offsets, and a sized region is the slice
   [split_at] cuts once its size was checked against the tail. Every scalar is
   an [i64] holding the 63-bit [int] the codec computes with. The code after a
   run of optional fields is written once: the run binds the tail it leaves,
   and its outputs that are not slots, in one [let]. A slot ([Plan.slots], an
   output under more than [Plan.slot_depth] optional fields) is a [let mut]
   bound to its absent value where the walk starts, which only a field present
   assigns. An output is thus written by at most [Plan.slot_depth] runs, and
   the code grows linearly with the optional fields, nested or not. *)

type scan = {
  out : Buffer.t;
  depth : int;
  scope : P.Scope.t;
  live : string -> bool;
  exported : string -> bool;
  slot : string -> bool;
  fresh : string -> string;
}

let line s fmt =
  Fmt.kstr
    (fun l ->
      Buffer.add_string s.out (String.make (4 * s.depth) ' ');
      Buffer.add_string s.out l;
      Buffer.add_char s.out '\n')
    fmt

let indent s = { s with depth = s.depth + 1 }

(* A check: the record is rejected where [cond] is false. *)
let scan_check s cond = line s "if !(%s) { return None; }" (expr s.scope cond)

let is_scalar (f : P.field) =
  match f.store with Bytes _ -> false | Int _ | Bits _ -> true

(* A field's value as an [i64], out of the prefix array [a], or out of the
   variable of its bitfield's word. *)
let scan_value a words (f : P.field) =
  match f.store with
  | Int { width = 8; signed; _ } ->
      Fmt.str "(%s[%d]%s as i64)" a f.offset (if signed then " as i8" else "")
  | Int { width; endian; signed } ->
      let ty = Fmt.str "%s%d" (if signed then "i" else "u") width in
      Fmt.str "(%s as i64)" (load a f.offset (width / 8) endian ty)
  | Bits { base; shift; width } ->
      let w = List.assoc (f.offset, base) words in
      if shift + width = 8 * Bitfield.byte_size base then
        Fmt.str "(%s >> %d)" w shift
      else Fmt.str "((%s >> %d) & %s)" w shift (mask width)
  | Bytes _ -> invalid_arg "Walk.scan_value: a byte array"

(* Binds each word of the prefix array [a] that holds a live bitfield, once,
   keyed by its offset and base. *)
let scan_words s a live =
  List.filter_map
    (fun (f : P.field) ->
      match f.store with
      | Bits { base; _ } -> Some (f.offset, base)
      | Int _ | Bytes _ -> None)
    live
  |> List.sort_uniq compare
  |> List.map (fun ((at, base) as key) ->
      let w = s.fresh "w" in
      line s "let %s: i64 = %s as i64;" w
        (load a at (Bitfield.byte_size base) (base_endian base) (base_type base));
      (key, w))

(* A prefix: the tail [rest] must hold its [size] bytes, which come back as an
   array; each field the walk binds is read from it, and each field's checks
   follow it in the codec's order. *)
let scan_prefix s (fields : P.field list) size rest =
  let live =
    List.filter (fun (f : P.field) -> is_scalar f && s.live f.id) fields
  in
  let a = s.fresh "a" and next = s.fresh "r" in
  line s
    "let Some((%s, %s)) = %s.split_first_chunk::<%d>() else { return None; };"
    (if live = [] then "_" else a)
    next rest size;
  let words = scan_words s a live in
  let bind (f : P.field) =
    if List.memq f live then
      line s "%s = %s;"
        (if s.slot f.id then f.id else Fmt.str "let %s: i64" f.id)
        (scan_value a words f)
  in
  let refine s conditions =
    { s with scope = P.Scope.refine s.scope conditions }
  in
  let range f g =
    Option.bind (P.word_range f g) (fun (at, lo, hi) ->
        Option.map (fun w -> (w, lo, hi)) (List.assoc_opt at words))
  in
  let rec go s = function
    | [] -> s
    | f :: g :: rest when Option.is_some (range f g) ->
        let w, lo, hi = Option.get (range f g) in
        bind f;
        bind g;
        line s "if !(%s >= %di64 && %s <= %di64) { return None; }" w lo w hi;
        go (refine s (P.conditions f @ P.conditions g)) rest
    | f :: rest ->
        bind f;
        let conditions = P.conditions f in
        List.iter (scan_check s) conditions;
        go (refine s conditions) rest
  in
  (go s fields, next)

(* A size checked against the tail [rest], compared as [u64], which holds
   both the size and any slice length, so no cast narrows it. A negative size
   is a [u64] of at least 2^63, above any slice length. *)
let scan_size s size rest =
  let n = s.fresh "n" in
  line s "let %s: i64 = %s;" n (expr s.scope size);
  line s "if (%s as u64) > (%s.len() as u64) { return None; }" n rest;
  n

let tuple = function [ x ] -> x | l -> "(" ^ String.concat ", " l ^ ")"

let rec any_shared shared (t : P.tree) =
  shared t
  ||
  match t.node with
  | Branch (_, a, b) -> any_shared shared a || any_shared shared b
  | Leaf _ -> false

(* A decision tree whose cases are each reached once, as nested [if]s whose
   leaves run the case, or [absent]. *)
let rec scan_tree s ~case ~absent (t : P.tree) =
  match t.node with
  | Leaf None -> absent s
  | Leaf (Some i) -> case s i
  | Branch (atom, yes, no) ->
      line s "if %s {" (expr s.scope atom);
      scan_tree (indent s) ~case ~absent yes;
      line s "} else {";
      scan_tree (indent s) ~case ~absent no;
      line s "}"

(* A decision tree that reaches a subtree or a case twice: it picks the
   number of the case ([cases] for none), each shared subtree a closure
   defined after the ones it calls, then a [match] runs the case. *)
let scan_pick s ~bound ~shared ~cases ~case ~absent tree =
  let sel = s.fresh "sel" in
  let closure (t : P.tree) = Fmt.str "d%d_%s" t.tid sel in
  let rec pick ~top s (t : P.tree) =
    if (not top) && shared t then line s "%s()?" (closure t)
    else
      match t.node with
      | Leaf None -> line s "%d" cases
      | Leaf (Some i) -> line s "%d" i
      | Branch (atom, yes, no) ->
          line s "if %s {" (expr s.scope atom);
          pick ~top:false (indent s) yes;
          line s "} else {";
          pick ~top:false (indent s) no;
          line s "}"
  in
  let defined = Hashtbl.create 16 in
  let rec define s (t : P.tree) =
    if not (Hashtbl.mem defined t.tid) then begin
      Hashtbl.replace defined t.tid ();
      match t.node with
      | Leaf _ -> ()
      | Branch (_, a, b) ->
          define s a;
          define s b;
          if shared t then begin
            line s "let %s = || -> Option<usize> { Some({" (closure t);
            pick ~top:true (indent s) t;
            line s "}) };"
          end
    end
  in
  line s "let %s: usize = {" sel;
  define (indent s) tree;
  pick ~top:false (indent s) tree;
  line s "};";
  line s "let %s = match %s {" bound sel;
  for i = 0 to cases - 1 do
    line (indent s) "%d => {" i;
    case (indent (indent s)) i;
    line (indent s) "}"
  done;
  line (indent s) "_ => {";
  absent (indent (indent s));
  line (indent s) "}";
  line s "};"

let rec scan_walk s nodes rest k =
  match nodes with
  | [] -> k s rest
  | P.Prefix (fields, size) :: nodes ->
      let s, rest = scan_prefix s fields size rest in
      scan_walk s nodes rest k
  | P.Span size :: nodes ->
      let n = scan_size s size rest in
      let next = s.fresh "r" in
      line s "let %s = &%s[%s as usize..];" next rest n;
      scan_walk s nodes next k
  | P.Test cond :: nodes ->
      scan_check s cond;
      scan_walk { s with scope = P.Scope.refine s.scope [ cond ] } nodes rest k
  | P.Region (size, at_most, body) :: nodes ->
      let n = scan_size s size rest in
      let child = s.fresh "r" and next = s.fresh "r" in
      line s "let (%s, %s) = %s.split_at(%s as usize);" child next rest n;
      scan_walk s body child (fun s left ->
          if not at_most then line s "if !%s.is_empty() { return None; }" left;
          scan_walk s nodes next k)
  | P.Gate _ :: _ -> scan_gates s nodes rest k

(* A run of optional fields whose conditions exclude each other: the decision
   tree of their conditions picks the case that runs, which binds the outputs
   of every field of the run that are not slots, absent ones for the others,
   and the tail after it in one [let], and assigns its presence when a slot;
   the code after the run follows that [let] once. When a subtree or a case
   of the tree is reached twice, the tree picks the case's number, each
   shared subtree a closure, and a [match] runs the case. *)
and scan_gates s nodes rest k =
  let gates, after = P.gates (P.Scope.facts s.scope) nodes in
  let indexed = List.mapi (fun i g -> (i, g)) gates in
  let tree =
    P.decide (P.Scope.facts s.scope)
      (List.map (fun (i, (c, _, _, _)) -> (i, c)) indexed)
  in
  let shared, case_shared = P.shared tree in
  let outputs =
    List.concat_map
      (fun (i, (_, (present : P.output), children, _)) ->
        List.filter_map
          (fun (o : P.output) ->
            if s.exported o.id && not (s.slot o.id) then Some (i, present, o)
            else None)
          (present :: children))
      indexed
  in
  let tail = s.fresh "r" in
  let values ~fired rest =
    List.map
      (fun (i, (present : P.output), (o : P.output)) ->
        if Some i = fired then if o.id = present.id then "true" else o.id
        else if o.is_bool then "false"
        else "0i64")
      outputs
    @ [ rest ]
  in
  let case s i =
    let cond, (present : P.output), _, body = List.nth gates i in
    scan_walk
      { s with scope = P.Scope.refine s.scope [ cond ] }
      body rest
      (fun s rest ->
        if s.slot present.id then line s "%s = true;" present.id;
        line s "%s" (tuple (values ~fired:(Some i) rest)))
  in
  let absent s = line s "%s" (tuple (values ~fired:None rest)) in
  let bound =
    tuple (List.map (fun (_, _, (o : P.output)) -> o.id) outputs @ [ tail ])
  in
  let cases = List.length gates in
  if any_shared shared tree || List.exists case_shared (List.init cases Fun.id)
  then scan_pick s ~bound ~shared ~cases ~case ~absent tree
  else begin
    line s "let %s = {" bound;
    scan_tree (indent s) ~case ~absent tree;
    line s "};"
  end;
  scan_walk s after tail k

(* The slots, each bound to its absent value where the walk starts. *)
let scan_slots s slots =
  List.iter
    (fun (o : P.output) ->
      if o.is_bool then line s "let mut %s: bool = false;" o.id
      else line s "let mut %s: i64 = 0;" o.id)
    slots

let output_type (o : P.output) =
  if o.is_bool then "bool"
  else if o.signed then
    if o.wire_width <= 8 then "i8"
    else if o.wire_width <= 16 then "i16"
    else "i32"
  else bits_type o.wire_width

(* An output as its Rust type. *)
let output_value (o : P.output) =
  if o.is_bool then o.id else Fmt.str "%s as %s" o.id (output_type o)

let body ~depth plan outputs ~frame ~finish =
  let out = Buffer.create 8192 in
  let count = ref 0 in
  let fresh prefix =
    incr count;
    Fmt.str "%s%d" prefix !count
  in
  let slots = P.slots plan outputs in
  let ids = List.map (fun (o : P.output) -> o.id) outputs in
  let s =
    {
      out;
      depth;
      scope = P.Scope.v plan;
      live = P.live plan outputs;
      exported = (fun v -> List.mem v ids);
      slot = (fun v -> List.exists (fun (o : P.output) -> o.id = v) slots);
      fresh;
    }
  in
  scan_slots s slots;
  scan_walk s (P.nodes plan) frame (fun s _ -> List.iter (line s "%s") finish);
  (Buffer.contents out, slots <> [])
