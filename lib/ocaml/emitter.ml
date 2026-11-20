open Wire.Private
open Layout
module T = Types
open Planner

(* {1 The emitter}

   One walk over the plan writes every function that checks a record: the
   boolean ones, the decoders, the folds and the one that finds the codec's
   error. It writes each node once: after optional fields the code joins in a
   local function the compiler turns into a jump, and the outputs under more
   than [slot_depth] of them are mutable locals that only the fields present
   assign, so the code for [n] optional fields grows with [n], nested or
   not. *)

(* How the code holds integers. [Tagged] is [int]. [Boxed] is [int64] for
   values and [int] for positions; the stock compiler keeps such an [int64]
   unboxed in a register. [Unboxed] is OxCaml's [int64_u]. *)
type num = Tagged | Boxed | Unboxed

(* How the code holds positions, sizes and bounds: a load takes an [int]. *)
let pos_num = function Boxed -> Tagged | num -> num

(* The module of the operations on values held as [num], other than [int]. *)
let i64_ops = function Unboxed -> "U" | Boxed | Tagged -> "Int64"

(* A 64-bit literal written as [digits], an OCaml literal without its
   suffix. *)
let lit64 num digits =
  match num with Unboxed -> "#" ^ digits ^ "L" | _ -> digits ^ "L"

(* The buffer the generated code reads: its type, its unchecked accessors, its
   length, the module of its bounds-checked loads and string copies, the
   runtime module that names a buffer of that type, and the parameter that binds it. *)
type src = {
  typ : string;
  u : string;
  len : string;
  checked : string;
  brand : string;
  buf : string;
  unboxed : bool;
      (* Its 32-bit and 64-bit loads give [int32_u] and [int64_u], as on
         OxCaml, where a boxed one would allocate across a module boundary. *)
}

(* The modules of the 32-bit and 64-bit values [src] loads. *)
let i32 src = if src.unboxed then "Int32_u" else "Int32"
let i64 src = if src.unboxed then "Int64_u" else "Int64"
let rt = "Wire_runtime"
let errors = rt ^ ".Errors"
let arith = rt ^ ".Arith"

let bytes_src =
  {
    typ = "bytes";
    u = rt ^ ".Unchecked";
    len = "Bytes.length";
    checked = "Bytes";
    brand = rt ^ ".Buf";
    buf = "buf";
    unboxed = false;
  }

let bigstring_src =
  {
    typ = rt ^ ".Unchecked.Bigstring.t";
    u = rt ^ ".Unchecked.Bigstring";
    len = rt ^ ".Unchecked.Bigstring.length";
    checked = rt ^ ".Unchecked.Bigstring";
    brand = rt ^ ".Buf.Bigstring";
    buf = "buf";
    unboxed = false;
  }

(* On OxCaml a function that reads or writes [buf] without keeping it takes it
   [local], so a caller may pass a buffer on its stack. *)
let local_bytes_src =
  {
    bytes_src with
    u = rt ^ ".Target.Local";
    checked = rt ^ ".Target.Local";
    buf = "(buf @ local)";
    unboxed = true;
  }

let local_bigstring_src =
  {
    bigstring_src with
    u = rt ^ ".Target.Local.Bigstring";
    len = rt ^ ".Target.Local.Bigstring.length";
    checked = rt ^ ".Target.Local.Bigstring";
    buf = "(buf @ local)";
    unboxed = true;
  }

(* Where a walk failed: a bound, a field's check, or a where clause. *)
type failure = Bounds | Check of field * check | Where

type mode = {
  num : num;
  src : src;
  ox : bool;
      (* A 16-bit field is read through a 32-bit load: flambda2 keeps the
         [int32] unboxed, where the stock compiler would box it. *)
  cold : bool;
      (* The walk raises the codec's errors, and its arithmetic the codec's
         overflow. *)
  raw32 : bool; (* A 32-bit field is bound as its [int32] too. *)
  ranged : bool;
      (* Whether two bitfields of a word may be tested as one range of the
         word. Only a walk that names no failed field may. *)
  live : Sset.t; (* The variables the walk binds. *)
  exported : Sset.t; (* The outputs the walk hands out. *)
  slots : Sset.t; (* Those of them that are slots. *)
  fail : failure -> string;
  overflowed : failure -> string -> string;
      (* What a cold walk raises for an overflow, given the exception. *)
}

type pos = { var : string option; at : int }

type cuts = {
  prefix : string;
  defs : Buffer.t;
  taken : (string, unit) Hashtbl.t;
  poly : string;
  reserve : int;
}

(* The text written so far in the current function, on all its paths: what
   merlint measures its length and its complexity by. *)
type fn = { mutable chars : int; mutable decisions : int }

type w = {
  m : mode;
  ppf : Format.formatter;
  fresh : string -> string;
  ranges : (int * int) Smap.t;
  facts : facts;
  views : (string * int T.expr) list;
      (* A bitfield as an expression over its word. *)
  words : (string * int) list; (* A bitfield word and its mask. *)
  raws : (string * (string * int)) list;
      (* A big-endian field as the variable holding its bytes read
         little-endian, and its width in bits. *)
  deferred : (string * (string * bool)) list ref;
      (* A field bound where code first reads it, its value and whether that
         loads from the buffer: a test of its raw bytes reads it in neither
         branch, and the branch where it holds binds the constant. *)
  bounded : (string * string * string * string) list;
      (* The spans already checked before a dispatch, as [span_bound] keys. *)
  sums : (string * sum) list;
      (* The end of each span, by its variable, as a sum over the plan's
         variables. *)
  ints : Sset.t;
      (* With [Boxed], the variables that hold an [int]: the window's length
         and the positions and sizes the walk binds. An expression over values
         reads them through [Int64.of_int]. *)
  base : string; (* The offset in the buffer of the current window. *)
  limit : string; (* The length of the current window. *)
  known : int; (* Bytes known to be in the window after the position. *)
  tail : bool;
      (* The code after the current walk is short and holds no optional
         field, so a dispatch may copy it into its branches. *)
  copies : int; (* How many copies of that code the path already makes. *)
  cuts : cuts option;
  fn : fn;
  depth : int; (* The conditions open around the position in its function. *)
  scope : (string * string option) list;
  settled : (string * bool) list;
      (* The outputs known here to be present, or absent. *)
  pinned : bool;
  section : string; (* The case being written, which names its functions. *)
  held : int list;
      (* With [Boxed], the words already loaded at constant offsets of the
         window, by offset. Later prefixes at constant offsets read them. *)
}

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

(* The branches the code [s] opens, as merlint counts them: each [if], each
   [&&] and [||], each handler of an exception. *)
let decisions s =
  let n = String.length s in
  let word i k =
    i + String.length k <= n
    && String.sub s i (String.length k) = k
    && (i = 0 || not (is_ident_char s.[i - 1]))
    && (i + String.length k = n || not (is_ident_char s.[i + String.length k]))
  in
  let rec go i acc =
    if i >= n then acc
    else if word i "if" || word i "exception" then go (i + 2) (acc + 1)
    else if
      i + 1 < n
      && ((s.[i] = '&' && s.[i + 1] = '&') || (s.[i] = '|' && s.[i + 1] = '|'))
    then go (i + 2) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let is_variable x =
  x <> ""
  && (match x.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all is_ident_char x

(* The variable a line [let x = e in] binds. *)
let binding line =
  let is_let =
    String.starts_with ~prefix:"let " line
    && String.ends_with ~suffix:" in" line
  in
  match if is_let then String.index_from_opt line 4 ' ' else None with
  | Some i when String.length line >= i + 3 ->
      let x = String.sub line 4 (i - 4) in
      if is_variable x && String.sub line i 3 = " = " then Some x else None
  | _ -> None

let local_prefix = "let[@local] "

(* The occurrences of [x] in [line] as a word: none qualified by a module,
   none as the name of a label. *)
let occurrences x line =
  let n = String.length line and k = String.length x in
  let rec from i acc =
    if i + k > n then List.rev acc
    else
      let word =
        String.sub line i k = x
        && (i = 0 || not (is_ident_char line.[i - 1] || line.[i - 1] = '.'))
        && (i + k = n || not (is_ident_char line.[i + k]))
      in
      let label =
        word && i > 0 && line.[i - 1] = '~' && i + k < n && line.[i + k] = ':'
      in
      from (i + 1) (if word && not label then i :: acc else acc)
  in
  from 0 []

(* Whether [line] binds [x]: as a [let], or as a parameter of a local
   function. *)
let binds x line =
  binding line = Some x
  || (String.starts_with ~prefix:local_prefix line && occurrences x line <> [])

(* Whether [line] reads [x], other than by binding it. *)
let reads x line =
  if String.starts_with ~prefix:local_prefix line then false
  else
    List.exists
      (fun i -> not (i = 4 && binding line = Some x))
      (occurrences x line)

(* [s] written in the current function, after the binding of each deferred
   field it reads. *)
let rec write w s =
  let lines = String.split_on_char '\n' s in
  let read, kept =
    List.partition (fun (x, _) -> List.exists (reads x) lines) !(w.deferred)
  in
  w.deferred := kept;
  List.iter (fun (x, (v, _)) -> write w (Fmt.str "let %s = %s in\n" x v)) read;
  w.fn.chars <- w.fn.chars + String.length s;
  w.fn.decisions <- w.fn.decisions + decisions s;
  Format.pp_print_string w.ppf s

let pf w fmt = Fmt.kstr (write w) fmt

(* The deferred loads written here, before a local function that may read
   them: one that loaded them itself would close over the buffer, which may be
   [local], and could not be called in tail position. *)
let flush w =
  let loads, kept = List.partition (fun (_, (_, l)) -> l) !(w.deferred) in
  w.deferred := kept;
  List.iter
    (fun (x, (v, _)) -> write w (Fmt.str "let %s = %s in\n" x v))
    (List.rev loads)

(* [f ()] with the deferred bindings as they are now, which the code it writes
   binds in a scope of its own; they are deferred again after it. *)
let scoped w f =
  let saved = !(w.deferred) in
  let r = f () in
  w.deferred := saved;
  r

(* {2 Slots}

   An output under more than [slot_depth] optional fields is a slot: a
   mutable local bound once to its absent value where the walk starts,
   assigned where its field is present and read after the joins. Any other
   output is an immutable binding, which a join takes as a parameter and an
   absent field binds to its absent value. The generated code only reads and
   assigns a slot in place, in the walk or in a join, and only applies a join
   in tail position: the compiler then makes the slot a mutable variable and
   the join a jump. A slot captured by any other function would be a heap
   block, and a join applied elsewhere a closure. *)

let is_slot m id = Sset.mem id m.slots

(* An output the walk hands out as an immutable binding. *)
let immutable m (o : output) = Sset.mem o.id m.exported && not (is_slot m o.id)

(* On OxCaml an [int64_u] has no [ref]: its slot is a [Target.cell]. *)
let cell num ~is_bool = num = Unboxed && not is_bool

let slot_read num ~is_bool id =
  if cell num ~is_bool then id ^ ".B.cell" else "!" ^ id

(* The assignment of [v] to the slot [id], where a binding goes. *)
let slot_assign num ~is_bool id v =
  if cell num ~is_bool then Fmt.str "let () = %s.B.cell <- %s in" id v
  else Fmt.str "let () = %s := %s in" id v

(* The slots of [outputs], each bound to its absent value. *)
let pp_slots m ppf outputs =
  List.iter
    (fun (o : output) ->
      if is_slot m o.id then
        Fmt.pf ppf "let %s = %s in@\n" o.id
          (if o.is_bool then "ref false"
           else if cell m.num ~is_bool:false then "{ B.cell = #0L }"
           else if m.num = Boxed then "ref 0L"
           else "ref 0"))
    outputs

(* An output's value where the walk ends. *)
let output_value m (o : output) =
  if is_slot m o.id then slot_read m.num ~is_bool:o.is_bool o.id else o.id

(* {2 Numbers} *)

let lit num n =
  match num with
  | Tagged -> int_lit n
  | Boxed -> if n < 0 then Fmt.str "(%dL)" n else Fmt.str "%dL" n
  | Unboxed ->
      if n < 0 then Fmt.str "(-#%LdL)" (Int64.neg (Int64.of_int n))
      else Fmt.str "#%dL" n

let unboxed_op = function
  | "+" -> "add"
  | "-" -> "sub"
  | "*" -> "mul"
  | "land" -> "logand"
  | "lor" -> "logor"
  | "lxor" -> "logxor"
  | op -> invalid_arg op

let binop num op a b =
  match num with
  | Tagged -> Fmt.str "(%s %s %s)" a op b
  | Boxed | Unboxed -> Fmt.str "(%s.%s %s %s)" (i64_ops num) (unboxed_op op) a b

(* The type annotation lets the stock compiler compare two [int64] inline. *)
let relation num op a b =
  match num with
  | Tagged -> Fmt.str "((%s : int) %s %s)" a op b
  | Boxed -> Fmt.str "((%s : int64) %s %s)" a op b
  | Unboxed -> Fmt.str "(U.compare %s %s %s 0)" a b op

(* An [int64] or [int64_u] holds the value of a 63-bit [int]
   sign-extended. *)
let normalise63 num v =
  let u = i64_ops num in
  Fmt.str "(%s.shift_right (%s.shift_left %s 1) 1)" u u v

(* [e] as OCaml over the variables bound so far. [ov] is set when it holds an
   operation that may overflow, which then raises: [Arith.Overflow] in a walk
   that only accepts or rejects, the codec's error in a cold one. *)
let rec expr : type a. w -> bool ref -> a T.expr -> string =
 fun w ov e ->
  let p : type b. b T.expr -> string = fun x -> expr w ov x in
  let num = w.m.num in
  let arith_op op fn sym a b =
    if fits w.ranges op a b then binop num sym (p a) (p b)
    else begin
      ov := true;
      let fn =
        Fmt.str "%s.%s%s" arith (if w.m.cold then "checked_" else "") fn
      in
      match num with
      | Tagged -> Fmt.str "(%s %s %s)" fn (p a) (p b)
      | Boxed | Unboxed ->
          let u = i64_ops num in
          Fmt.str "(%s.of_int (%s (%s.to_int %s) (%s.to_int %s)))" u fn u (p a)
            u (p b)
    end
  in
  (* A shift's distance is an [int]. With [Boxed], a literal is written as
     is. *)
  let distance (b : int T.expr) =
    match (num, b) with
    | Boxed, T.Int n -> string_of_int n
    | _ -> Fmt.str "(%s.to_int %s)" (i64_ops num) (p b)
  in
  let cmp : type b. string -> b T.expr -> b T.expr -> string =
   fun op a b ->
    let is_bool : type c. c T.expr -> bool = function
      | T.Bool _ | T.Eq _ | T.Ne _ | T.Lt _ | T.Le _ | T.Gt _ | T.Ge _ | T.And _
      | T.Or _ | T.Not _ ->
          true
      | _ -> false
    in
    if is_bool a then Fmt.str "((%s : bool) %s %s)" (p a) op (p b)
    else relation num op (p a) (p b)
  in
  match e with
  | T.Int n -> lit num n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, v) -> (
      match List.assoc_opt v w.settled with
      | Some _ -> lit num 0
      | None when is_slot w.m v -> slot_read num ~is_bool:false v
      | None when num = Boxed && Sset.mem v w.ints ->
          Fmt.str "(Int64.of_int %s)" v
      | None -> v)
  | T.Add (a, b) -> arith_op T.checked_add "add" "+" a b
  | T.Sub (a, b) -> arith_op T.checked_sub "sub" "-" a b
  | T.Mul (a, b) -> arith_op T.checked_mul "mul" "*" a b
  | T.Land (a, b) -> binop num "land" (p a) (p b)
  | T.Lor (a, b) -> binop num "lor" (p a) (p b)
  | T.Lxor (a, b) -> binop num "lxor" (p a) (p b)
  | T.Lnot a -> (
      match num with
      | Tagged -> Fmt.str "(lnot %s)" (p a)
      | Boxed | Unboxed -> Fmt.str "(%s.lognot %s)" (i64_ops num) (p a))
  | T.Lsl (a, b) -> (
      match num with
      | Tagged -> Fmt.str "(%s lsl %s)" (p a) (p b)
      | Boxed | Unboxed ->
          let v =
            Fmt.str "(%s.shift_left %s %s)" (i64_ops num) (p a) (distance b)
          in
          if fst (range w.ranges e) >= 0 then v else normalise63 num v)
  | T.Lsr (a, b) -> (
      match num with
      | Tagged -> Fmt.str "(%s lsr %s)" (p a) (p b)
      | Boxed | Unboxed ->
          let u = i64_ops num in
          let nonnegative = fst (range w.ranges a) >= 0 in
          let input =
            if nonnegative then p a
            else
              Fmt.str "(%s.logand %s %s)" u (p a)
                (lit64 num "0x7fffffffffffffff")
          in
          let v =
            Fmt.str "(%s.shift_right_logical %s %s)" u input (distance b)
          in
          let lo, hi = range w.ranges b in
          if nonnegative || (lo > 0 && hi < 64) then v else normalise63 num v)
  | T.Eq (a, b) -> cmp "=" a b
  | T.Ne (a, b) -> cmp "<>" a b
  | T.Lt (a, b) -> cmp "<" a b
  | T.Le (a, b) -> cmp "<=" a b
  | T.Gt (a, b) -> cmp ">" a b
  | T.Ge (a, b) -> cmp ">=" a b
  | T.And (a, b) -> Fmt.str "(%s && %s)" (p a) (p b)
  | T.Or (a, b) -> Fmt.str "(%s || %s)" (p a) (p b)
  | T.Not a -> Fmt.str "(not %s)" (p a)
  | T.Cast (`U8, a) -> binop num "land" (p a) (lit num 0xff)
  | T.Cast (`U16, a) -> binop num "land" (p a) (lit num 0xffff)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s then %s else %s)" (p c) (p a) (p b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* {2 Loads} *)

let endian_suffix = function T.Big -> "be" | T.Little -> "le"
let bit_endian = function T.U8 -> T.Big | T.U16 e | T.U32 e -> e

(* The address [o + k]. *)
let addr num o k =
  if k = 0 then o
  else
    match num with
    | Tagged | Boxed -> Fmt.str "(%s + %d)" o k
    | Unboxed -> Fmt.str "(U.add %s #%dL)" o k

(* A load at [o + k]. An [int64_u] position is the offset of the wider loads
   as it is: the address is the buffer plus the position, with the distance
   [k] in the load itself, so a block's loads share one addition. A byte is
   read at the position narrowed to an [int]. *)
let load m ~o ~k ~bytes ~endian =
  let e = endian_suffix endian in
  match (m.num, bytes) with
  | Tagged, (2 | 4 | 8) when m.src.unboxed && m.src.typ = "bytes" -> (
      (* An OxCaml record read untags its offset once and puts the field's
         distance in the load, as the scanner's reads do. *)
      let at =
        if k = 0 then Fmt.str "(Int64_u.of_int %s)" o
        else Fmt.str "(Int64_u.add (Int64_u.of_int %s) #%dL)" o k
      in
      match bytes with
      | 2 -> Fmt.str "(%s.u16%s_at buf %s)" m.src.u e at
      | 4 -> Fmt.str "(%s.i32%s_at buf %s)" m.src.u e at
      | _ -> Fmt.str "(%s.i64be_at buf %s)" m.src.u at)
  | (Tagged | Boxed), _ | Unboxed, 1 -> (
      let at =
        match m.num with
        | Tagged | Boxed -> addr Tagged o k
        | Unboxed when k = 0 -> Fmt.str "(B.offset %s)" o
        | Unboxed -> Fmt.str "(B.offset %s + %d)" o k
      in
      match bytes with
      | 1 -> Fmt.str "(%s.u8 buf %s)" m.src.u at
      | 2 -> Fmt.str "(%s.u16%s buf %s)" m.src.u e at
      | 4 -> Fmt.str "(%s.i32%s buf %s)" m.src.u e at
      | _ -> Fmt.str "(%s.i64be buf %s)" m.src.u at)
  | Unboxed, _ -> (
      let at = addr m.num o k in
      match bytes with
      | 2 -> Fmt.str "(%s.u16%s_at buf %s)" m.src.u e at
      | 4 -> Fmt.str "(%s.i32%s_at buf %s)" m.src.u e at
      | _ -> Fmt.str "(%s.i64be_at buf %s)" m.src.u at)

(* The 64-bit words of a prefix of [size] bytes worth loading once: those
   holding two or more of [fields] a shift takes out. An [int64_u] position
   reads a byte by a 16-bit load that needs no swap, which a test or an
   offset of the byte has sooner than a shift of a swapped word, so a byte is
   not taken from a word. *)
let in_word num f =
  match f.store with
  | Int { width = 8; _ } | Bits { base = T.U8; _ } -> num <> Unboxed
  | Int { width = 16 | 32; endian = T.Big; _ } -> true
  | Bits { base = T.U16 T.Big | T.U32 T.Big; _ } -> true
  | _ -> false

let covers a f = a <= f.offset && f.offset + width_bytes f.store <= a + 8

(* One 64-bit load serves every field it holds, as the load combining of Go's
   [memcombine] pass turns adjacent narrow loads into a wide one. A word
   starts at a multiple of 8, or ends with the prefix. Candidates are computed
   from the fields, so the cost does not grow with [size]. *)
let words num ~size fields =
  if size < 8 then []
  else
    let last = size - 8 in
    List.concat_map
      (fun f ->
        if not (in_word num f) then []
        else
          List.filter
            (fun a -> covers a f)
            [ min (f.offset / 8 * 8) last; last ])
      fields
    |> List.sort_uniq Int.compare
    |> List.filter (fun a ->
        let held =
          List.filter_map
            (fun f ->
              if in_word num f && covers a f then Some f.offset else None)
            fields
        in
        List.length (List.sort_uniq Int.compare held) > 1)

(* A 16-bit field read through the 32-bit load that ends or starts on it, when
   that load stays in a prefix of [size] bytes: one load and one byte swap,
   with none of the 16-bit sign and zero extensions a 16-bit swap costs. Only
   OxCaml reads so, into an [int] of 63 bits or an [int64_u]. The high half of
   a [raw] [int] read is shifted on the [int], where the caller's mask makes
   the [int32]'s sign irrelevant, so the word needs no zero extension first;
   otherwise the shift is on the [int32], which zero-extends and needs no
   mask. *)
let u16_in_word m ~size ~raw ~o ~k ~endian =
  let i32 = i32 m.src in
  let low, high =
    match m.num with
    | Tagged | Boxed ->
        let mask v =
          if raw then Fmt.str "(%s)" v else Fmt.str "(%s land 0xffff)" v
        in
        ( (fun i -> Fmt.kstr mask "%s.to_int %s" i32 i),
          fun i ->
            if raw then Fmt.str "(%s.to_int %s lsr 16)" i32 i
            else
              Fmt.str "(%s.to_int (%s.shift_right_logical %s 16) land 0xffff)"
                i32 i32 i )
    | Unboxed ->
        let mask v = if raw then v else Fmt.str "(U.logand %s #0xffffL)" v in
        ( (fun i -> Fmt.kstr mask "(U.of_int32_u %s)" i),
          fun i ->
            Fmt.kstr mask "(U.shift_right_logical (U.of_int32_u %s) 16)" i )
  in
  let at k = load m ~o ~k ~bytes:4 ~endian in
  match (m.ox || m.num = Unboxed, size, endian) with
  | false, _, _ | _, None, _ -> None
  | _, Some _, T.Big when k >= 2 -> Some (low (at (k - 2)))
  | _, Some size, T.Big when k + 4 <= size -> Some (high (at k))
  | _, Some size, T.Little when k + 4 <= size -> Some (low (at k))
  | _, Some _, T.Little when k >= 2 -> Some (high (at (k - 2)))
  | _ -> None

(* An unsigned value of [bytes] bytes at [k] from [o], from a word when one
   holds it; [size] bounds the bytes a wider load may read. With [raw], the
   value may keep whatever its [int] holds above its [bytes], for a caller that
   masks below them: that mask would cost an instruction the compiler does not
   remove. *)
let unsigned m ?size ?(raw = false) ~o ~words ~k ~bytes ~endian () =
  let mask32 v = if raw then v else Fmt.str "(%s land mask32)" v in
  let word = List.find_opt (fun a -> a <= k && k + bytes <= a + 8) words in
  match (m.num, word) with
  | Tagged, Some a when bytes = 1 || endian = T.Big ->
      let i64 = i64 m.src and shift = 8 * (a + 8 - k - bytes) in
      let v =
        Fmt.str "%s.to_int (%s.shift_right_logical w%d %d)" i64 i64 a shift
      in
      if a = k || raw then Fmt.str "(%s)" v
      else if bytes = 4 then mask32 v
      else Fmt.str "(%s land 0x%x)" v ((1 lsl (8 * bytes)) - 1)
  | Unboxed, Some a when bytes > 1 && endian = T.Big ->
      let shifted =
        Fmt.str "(U.shift_right_logical w%d %d)" a (8 * (a + 8 - k - bytes))
      in
      if a = k then shifted
      else
        Fmt.str "(U.logand %s #%LdL)" shifted
          (Int64.pred (Int64.shift_left 1L (8 * bytes)))
  | Boxed, Some a when bytes = 1 || endian = T.Big ->
      let shift = 8 * (a + 8 - k - bytes) in
      let shifted =
        if shift = 0 then Fmt.str "w%d" a
        else Fmt.str "(Int64.shift_right_logical w%d %d)" a shift
      in
      if a = k || raw then shifted
      else
        Fmt.str "(Int64.logand %s %LdL)" shifted
          (Int64.pred (Int64.shift_left 1L (8 * bytes)))
  | Boxed, _ when bytes = 4 ->
      let v = Fmt.str "(Int64.of_int32 %s)" (load m ~o ~k ~bytes ~endian) in
      if raw then v else Fmt.str "(Int64.logand %s 0xffffffffL)" v
  | Boxed, _ -> Fmt.str "(Int64.of_int %s)" (load m ~o ~k ~bytes ~endian)
  | Tagged, _ when bytes = 4 ->
      Fmt.kstr mask32 "%s.to_int %s" (i32 m.src) (load m ~o ~k ~bytes ~endian)
  | Tagged, _ when bytes = 2 -> (
      match u16_in_word m ~size ~raw ~o ~k ~endian with
      | Some v -> v
      | None -> load m ~o ~k ~bytes ~endian)
  | Tagged, _ -> load m ~o ~k ~bytes ~endian
  | Unboxed, _ when bytes = 1 ->
      (* A byte from the 16-bit load that ends on it, or else the one that
         starts on it, inside the prefix: no swap, and not at the address of
         a word, which the compiler would share instead of putting the
         distance in the load. *)
      let le16 k =
        Fmt.str "(U.of_int %s)" (load m ~o ~k ~bytes:2 ~endian:T.Little)
      in
      let starts = match size with Some s -> k + 2 <= s | None -> false in
      let ends = Option.is_some size && k >= 1 in
      if ends && not (starts && List.mem (k - 1) words) then
        Fmt.str "(U.shift_right_logical %s 8)" (le16 (k - 1))
      else if starts then Fmt.str "(U.logand %s #255L)" (le16 k)
      else Fmt.str "(U.of_int %s)" (load m ~o ~k ~bytes ~endian)
  | Unboxed, _ when bytes = 4 && raw ->
      Fmt.str "(U.of_int32_u %s)" (load m ~o ~k ~bytes ~endian)
  | Unboxed, _ when bytes = 4 ->
      Fmt.str "(U.logand (U.of_int32_u %s) #0xffffffffL)"
        (load m ~o ~k ~bytes ~endian)
  | Unboxed, _ when bytes = 2 -> (
      match u16_in_word m ~size ~raw ~o ~k ~endian with
      | Some v -> v
      | None -> Fmt.str "(U.of_int %s)" (load m ~o ~k ~bytes ~endian))
  | Unboxed, _ -> Fmt.str "(U.of_int %s)" (load m ~o ~k ~bytes ~endian)

(* A field's plain value, at [k] from [o]: from the variable [bitword] of a
   bitfield's word when it has one. *)
let value m ?size ?bitword ~o ~words ~k f =
  let sign v width =
    match m.num with
    | Tagged ->
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
    | Boxed | Unboxed ->
        let u = i64_ops m.num in
        Fmt.str "(%s.shift_right (%s.shift_left %s %d) %d)" u u v (64 - width)
          (64 - width)
  in
  match f.store with
  | Bits { base; shift; width } -> (
      let whole = shift + width = 8 * Bitfield.byte_size base in
      let word =
        match bitword with
        | Some v -> v
        | None ->
            unsigned m ?size ~raw:(not whole) ~o ~words ~k:(k + f.offset)
              ~bytes:(Bitfield.byte_size base) ~endian:(bit_endian base) ()
      in
      match m.num with
      | Tagged ->
          let v =
            if shift = 0 then word else Fmt.str "(%s lsr %d)" word shift
          in
          if whole then v else Fmt.str "(%s land %s)" v (mask width)
      | Boxed | Unboxed ->
          let u = i64_ops m.num in
          let v =
            if shift = 0 then word
            else Fmt.str "(%s.shift_right_logical %s %d)" u word shift
          in
          if whole then v
          else
            Fmt.str "(%s.logand %s %s)" u v
              (lit64 m.num
                 (Int64.to_string (Int64.pred (Int64.shift_left 1L width)))))
  | Int { width = 32; signed = true; endian } when m.num = Tagged ->
      Fmt.str "(%s.to_int %s)" (i32 m.src)
        (load m ~o ~k:(k + f.offset) ~bytes:4 ~endian)
  | Int { width; signed; endian } ->
      (* The sign extension drops what an [int64] holds above the field. *)
      let raw = signed && m.num = Boxed in
      let v =
        unsigned m ?size ~raw ~o ~words ~k:(k + f.offset) ~bytes:(width / 8)
          ~endian ()
      in
      if signed then sign v width else v
  | Bytes n ->
      Fmt.str "(%s.sub_string buf %s %d)" m.src.checked
        (addr Tagged o (k + f.offset))
        n

(* {2 Walking the plan} *)

let pos_expr num p =
  let num = pos_num num in
  match p.var with
  | None -> lit num p.at
  | Some v -> if p.at = 0 then v else binop num "+" v (lit num p.at)

(* A check that runs [fail] when [cond] is false; an overflow in [cond]
   fails too. The code after it is one condition deeper. *)
let check w failure (cond : bool T.expr) =
  let ov = ref false in
  let s = expr w ov cond in
  if not !ov then pf w "if not %s then %s else@\n" s (w.m.fail failure)
  else if w.m.cold then
    pf w
      "if not (match %s with b -> b | exception %s.Parse_error e -> %s) then \
       %s else@\n"
      s errors
      (w.m.overflowed failure "e")
      (w.m.fail failure)
  else
    pf w
      "if not (match %s with b -> b | exception %s.Overflow -> false) then %s \
       else@\n"
      s arith (w.m.fail failure);
  { w with depth = w.depth + 1 }

(* {3 Scope}

   The variables bound so far in the function being written, each with the
   type a function cut out of it writes for it, when inference needs one. *)

let num_type w =
  match w.m.num with Unboxed -> Some "int64_u" | Tagged | Boxed -> None

let bind ?ty w v = { w with scope = (v, ty) :: w.scope }
let bind_num w v = bind ?ty:(num_type w) w v

(* The text of the value of [o]: its literal where it is settled. An integer
   output is only ever settled absent. *)
let output_text w (o : output) =
  match List.assoc_opt o.id w.settled with
  | Some present when o.is_bool -> string_of_bool present
  | Some _ -> lit w.m.num 0
  | None -> output_value w.m o

(* {3 Dead bindings}

   A walk binds an offset, an end or a field as it reaches it, before it
   knows whether a later check, a call or the consumer reads it. Each binding
   is one line [let x = e in] whose [e] only loads or computes, so a line
   whose [x] no later line of its function reads is dropped, until none
   is. *)

let is_function line = String.starts_with ~prefix:"let[@inline always]" line

let rec prune lines =
  let lines = Array.of_list lines in
  let n = Array.length lines in
  let rec used x j =
    j < n
    && (not (is_function lines.(j)))
    && (reads x lines.(j) || used x (j + 1))
  in
  let dead i =
    match binding lines.(i) with
    | Some x -> not (used x (i + 1))
    | None -> false
  in
  let kept = List.filteri (fun i _ -> not (dead i)) (Array.to_list lines) in
  if List.length kept = n then kept else prune kept

(* Whether the code of [lines] reads [x] before it binds it. *)
let rec free x = function
  | [] -> false
  | line :: rest -> reads x line || ((not (binds x line)) && free x rest)

(* {3 Cuts}

   A walk is written as small functions: where the function being written
   holds as many conditions, branches or lines as merlint allows one, the
   rest of the walk becomes a function of its own, defined before the
   functions that call it, which takes the variables it reads as labelled
   arguments. Each is [[@inline always]], so the compiled walk is the one
   function it would be without the cuts. A cut is refused where the rest
   jumps to a join outside it, which must stay in the function that defines
   it, and in a walk with slots, which a function would capture. *)

let max_depth = 4
let max_decisions = 9

(* The characters of walk a function holds before merlint finds it longer
   than 50 lines: unformatted, a line of walk is some 35 characters. Each
   function also keeps room for its parameters and for the call that ends a
   walk, [reserve] characters long, whose lines ocamlformat fills more. *)
let max_chars = 1600
let room c = max_chars - 200 - (c.reserve * 4 / 5)

let can_cut w =
  Option.is_some w.cuts && (not w.pinned) && Sset.is_empty w.m.slots

(* Whether the code from here, which opens [opens] conditions, belongs in a
   function of its own. *)
let over w opens =
  match w.cuts with
  | Some c when can_cut w ->
      w.fn.chars > 0
      && (w.depth + opens > max_depth
         || w.fn.decisions + opens > max_decisions
         || w.fn.chars > room c)
  | _ -> false

(* The name of a function cut out under [label]: no more than four
   underscores, which merlint allows a name. *)
let function_name c label =
  let underscores s =
    String.fold_left (fun n ch -> if ch = '_' then n + 1 else n) 0 s
  in
  let label =
    if underscores c.prefix + 1 + underscores label <= 4 then label
    else String.concat "" (String.split_on_char '_' label)
  in
  let base = c.prefix ^ "_" ^ label in
  let rec go i =
    let name = if i = 1 then base else Fmt.str "%s%d" base i in
    if Hashtbl.mem c.taken name then go (i + 1)
    else begin
      Hashtbl.replace c.taken name ();
      name
    end
  in
  go 1

(* [body] written as the function [label], and a call of it in its place. *)
let cut w ~label body =
  match w.cuts with
  | Some c when can_cut w ->
      let b = Buffer.create 4096 in
      let ppf = Format.formatter_of_buffer b in
      scoped w (fun () ->
          body { w with ppf; fn = { chars = 0; decisions = 0 }; depth = 0 });
      Format.pp_print_flush ppf ();
      let lines = prune (String.split_on_char '\n' (Buffer.contents b)) in
      let seen = Hashtbl.create 16 in
      let params =
        List.filter
          (fun (v, _) ->
            (not (Hashtbl.mem seen v))
            && begin
              Hashtbl.replace seen v ();
              free v lines
            end)
          w.scope
        |> List.rev
      in
      let name = function_name c label in
      let param (v, ty) =
        match ty with None -> "~" ^ v | Some t -> Fmt.str "~(%s : %s)" v t
      in
      let formal, actual =
        if params = [] then ("()", "()")
        else
          ( String.concat " " (List.map param params),
            String.concat " " (List.map (fun (v, _) -> "~" ^ v) params) )
      in
      Fmt.pf (Fmt.with_buffer c.defs) "let[@inline always] %s %s%s =\n%s\n\n%!"
        name c.poly formal (String.concat "\n" lines);
      pf w "%s %s@\n" name actual
  | _ -> body w

(* The ranges and facts passed checks establish, for the code after them. *)
let refine w conditions =
  let ranges, facts = refined w.ranges w.facts conditions in
  { w with ranges; facts }

let nonnegative w e = nonnegative_in w.ranges w.facts e

(* The variable [x] as a value. With [Boxed], an [int] variable is widened. *)
let widen w x =
  if w.m.num = Boxed && Sset.mem x w.ints then Fmt.str "(Int64.of_int %s)" x
  else x

(* The bytes of the window after the variable part of [p], and how that
   expression holds integers. With [Boxed], a window whose length is a span's
   size is measured as an [int64]. *)
let room w p =
  let num =
    if w.m.num = Boxed && not (Sset.mem w.limit w.ints) then Boxed
    else pos_num w.m.num
  in
  let room =
    match p.var with
    | None -> w.limit
    | Some v -> binop num "-" w.limit (if num = Boxed then widen w v else v)
  in
  (num, room)

(* The window has [need] bytes from [p]. An end at [Planner.beyond] always
   fails the check, so the code after it is unreachable and checks nothing. *)
let bound w p need =
  if w.known >= need then w
  else begin
    let num, room = room w p in
    let ends = extent p.at need in
    pf w "if %s then %s else@\n"
      (relation num "<" room (lit num ends))
      (w.m.fail Bounds);
    let known = if ends >= beyond then beyond else need in
    { w with known; depth = w.depth + 1 }
  end

(* The offset in the buffer of the prefix at [p], as a variable and a
   constant to add. *)
let address w p =
  match p.var with
  | None -> (w, w.base, p.at)
  | Some v ->
      let o = w.fresh "o" in
      pf w "let %s = %s in@\n" o (binop (pos_num w.m.num) "+" w.base v);
      (bind_num w o, o, p.at)

(* Bitfields read through their word, by the variable holding it: a test of
   several bitfields of one word becomes one test of the word. *)
let rec masked words (e : int T.expr) =
  match e with
  | T.Ref (T.I, name) ->
      Option.map (fun mask -> (name, 0, mask)) (List.assoc_opt name words)
  | T.Land (a, T.Int n) | T.Land (T.Int n, a) ->
      Option.map
        (fun (word, shift, mask) -> (word, shift, mask land n))
        (masked words a)
  | T.Land (a, T.Lnot (T.Int n)) | T.Land (T.Lnot (T.Int n), a) ->
      Option.map
        (fun (word, shift, mask) -> (word, shift, mask land lnot n))
        (masked words a)
  | T.Lsr (a, T.Int n) when n >= 0 && n < Sys.int_size ->
      Option.bind (masked words a) (fun (word, shift, mask) ->
          let mask = mask lsr n in
          if mask = 0 then Some (word, 0, 0)
          else if shift + n < Sys.int_size then Some (word, shift + n, mask)
          else None)
  | T.Cast (`U8, a) -> masked words (T.Land (a, T.Int 255))
  | T.Cast (`U16, a) -> masked words (T.Land (a, T.Int 65535))
  | _ -> None

let zero_test words (e : bool T.expr) =
  let extract zero a =
    Option.map
      (fun (word, shift, mask) ->
        (word, (if mask = 0 then 0 else mask lsl shift), zero))
      (masked words a)
  in
  match e with
  | T.Eq (a, T.Int 0) -> extract true a
  | T.Eq (T.Int 0, a) -> extract true a
  | T.Ne (a, T.Int 0) -> extract false a
  | T.Ne (T.Int 0, a) -> extract false a
  | _ -> None

let mask_test word mask zero : bool T.expr =
  let bits = T.Land (T.Ref (T.I, word), T.Int mask) in
  if mask = 0 then T.Bool zero
  else if zero then T.Eq (bits, T.Int 0)
  else T.Ne (bits, T.Int 0)

let rec bit_predicate words (e : bool T.expr) : bool T.expr =
  let each = bit_predicate words in
  let combine want make a b =
    let a = each a and b = each b in
    match (zero_test words a, zero_test words b) with
    | Some (x, m, p), Some (y, n, q) when x = y && p = want && q = want ->
        mask_test x (m lor n) want
    | _ -> make a b
  in
  match e with
  | T.And (a, b) -> combine true (fun a b -> T.And (a, b)) a b
  | T.Or (a, b) -> combine false (fun a b -> T.Or (a, b)) a b
  | T.Not a -> (
      let a = each a in
      match zero_test words a with
      | Some (word, mask, zero) -> mask_test word mask (not zero)
      | None -> T.Not a)
  | _ -> (
      match zero_test words e with
      | Some (word, mask, zero) -> mask_test word mask zero
      | None -> e)

(* A gate's condition, simplified by the facts, its bitfields read through
   their words. *)
let lower w cond =
  let c = T.subst_expr w.views (simplify w.facts cond) in
  simplify w.facts (bit_predicate w.words c)

(* The variable [v] holding the word of the bitfields at [base], whose range
   and mask the views of those bitfields read. *)
let bind_bitword w ((_, base), v) =
  let w = bind_num w v in
  let width = 8 * Bitfield.byte_size base in
  if width < Sys.int_size then
    {
      w with
      ranges = Smap.add v (0, (1 lsl width) - 1) w.ranges;
      words = (v, (1 lsl width) - 1) :: w.words;
    }
  else w

(* The words a prefix loads with [Boxed], where a word costs one load and a
   field a shift and a mask. A word is loaded when it holds two or more
   fields that no word of [held] holds, counting the fields of the later
   prefixes, [later]. It starts at the first such field, moved back to stay
   between [lo] and [hi], the bytes known to be in the window. The fields are
   sorted by offset and scanned once, so the cost is linear in their number. *)
let shared_words ~held ~lo ~hi ~later live =
  let worded = List.filter (in_word Boxed) in
  let by_offset = List.sort (fun a b -> Int.compare a.offset b.offset) in
  let live = by_offset (worded live) in
  let candidates = Array.of_list (by_offset (live @ worded later)) in
  let starts = Hashtbl.create 16 in
  List.iter (fun a -> Hashtbl.replace starts a ()) held;
  let unheld g =
    let last = g.offset and first = g.offset + width_bytes g.store - 8 in
    let rec free a =
      a > last || ((not (Hashtbl.mem starts a)) && free (a + 1))
    in
    free (max 0 first)
  in
  let next = ref 0 in
  List.fold_left
    (fun chosen f ->
      let a = max lo (min f.offset (hi - 8)) in
      while !next < Array.length candidates && candidates.(!next).offset < a do
        incr next
      done;
      let rec served i acc =
        if i >= Array.length candidates || candidates.(i).offset >= a + 8 then
          acc
        else
          let g = candidates.(i) in
          served (i + 1)
            (if covers a g && unheld g && not (List.mem g.offset acc) then
               g.offset :: acc
             else acc)
      in
      if
        unheld f
        && a + 8 <= hi
        && covers a f
        && List.compare_length_with (served !next []) 1 > 0
      then begin
        Hashtbl.replace starts a ();
        a :: chosen
      end
      else chosen)
    [] live
  |> List.rev

(* The words a prefix at [o + k] of [size] bytes loads once: the 64-bit ones
   two or more live fields share, then each word of a live bitfield, which
   the bitfields' views read. With [Boxed], the words are [shared_words]
   between [lo] and [hi]. A prefix at a constant offset of the window
   ([constant]) reads the words loaded before it and keeps its own for the
   prefixes after it. *)
let load_words w ~o ~k ~size ~constant ~lo ~hi ~later live =
  let m = w.m in
  (* A 32-bit field bound as its [int32] is one load of its own. *)
  let worded =
    List.filter
      (fun f ->
        match f.store with Int { width = 32; _ } -> not m.raw32 | _ -> true)
      live
  in
  let held = if constant then w.held else [] in
  let fresh =
    match m.num with
    | Boxed ->
        let at f = { f with offset = extent k f.offset } in
        shared_words ~held ~lo ~hi ~later (List.map at worded)
    | Tagged | Unboxed -> List.map (fun a -> k + a) (words m.num ~size worded)
  in
  let w =
    List.fold_left
      (fun w a ->
        let v = load m ~o ~k:a ~bytes:8 ~endian:T.Big in
        let name = Fmt.str "w%d" a in
        pf w "let %s = %s in@\n" name v;
        bind_num w name)
      w fresh
  in
  let words = held @ fresh in
  let w = if constant && m.num = Boxed then { w with held = words } else w in
  let bitwords =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { base; _ } -> Some (f.offset, base)
        | _ -> None)
      live
    |> List.sort_uniq compare
    |> List.map (fun (at, base) ->
        let v = w.fresh "bw" in
        pf w "let %s = %s in@\n" v
          (unsigned m ~size:(k + size) ~o ~words ~k:(k + at)
             ~bytes:(Bitfield.byte_size base) ~endian:(bit_endian base) ());
        ((at, base), v))
  in
  (List.fold_left bind_bitword w bitwords, words, bitwords)

(* The checks of the field [f], and what they establish after them. *)
let checks w f =
  let w =
    List.fold_left (fun w c -> check w (Check (f, c)) c.cond) w f.checks
  in
  refine w (List.map (fun c -> c.cond) f.checks)

(* A field of a prefix at [o + k]: bound when live, then checked. *)
let field ?(checked = true) w ~o ~k ~size ~words ~bitwords f =
  let m = w.m in
  let is_live = Sset.mem f.id m.live in
  let shifted = { f with offset = k + f.offset } in
  let typed = match f.store with Bytes _ -> None | _ -> num_type w in
  let bind_field w v =
    if is_slot m f.id then begin
      pf w "%s@\n" (slot_assign m.num ~is_bool:false f.id v);
      w
    end
    else begin
      pf w "let %s = %s in@\n" f.id v;
      bind ?ty:typed w f.id
    end
  in
  let w =
    match f.store with
    | Int { width = 32; signed; endian } when m.raw32 ->
        pf w "let %s_raw = %s in@\n" f.id
          (load m ~o ~k:(k + f.offset) ~bytes:4 ~endian);
        let w = bind w (f.id ^ "_raw") in
        if is_live then begin
          pf w "let %s = (%s.to_int %s_raw%s) in@\n" f.id (i32 w.m.src) f.id
            (if signed then "" else " land mask32");
          bind w f.id
        end
        else w
    | _ when not is_live -> w
    | Bits { base; _ } ->
        let bitword = List.assoc (f.offset, base) bitwords in
        bind_field w (value m ~bitword ~o ~words ~k:0 shifted)
    | Int { width = (16 | 32) as width; signed = false; endian = T.Big }
      when m.num = Unboxed ->
        let v = value m ~size:(k + size) ~o ~words ~k:0 shifted in
        let w =
          if is_slot m f.id then bind_field w v
          else begin
            w.deferred := (f.id, (v, true)) :: !(w.deferred);
            bind ?ty:typed w f.id
          end
        in
        let raw = f.id ^ "_le" in
        let v =
          match width with
          | 16 ->
              Fmt.str "(U.of_int %s)"
                (load m ~o ~k:shifted.offset ~bytes:2 ~endian:T.Little)
          | _ ->
              Fmt.str "(U.of_int32_u %s)"
                (load m ~o ~k:shifted.offset ~bytes:4 ~endian:T.Little)
        in
        pf w "let %s = %s in@\n" raw v;
        { (bind_num w raw) with raws = (f.id, (raw, width)) :: w.raws }
    | _ -> bind_field w (value m ~size:(k + size) ~o ~words ~k:0 shifted)
  in
  let w = if checked then checks w f else w in
  match f.store with
  | Bits { base; shift; width }
    when is_live && 8 * Bitfield.byte_size base < Sys.int_size ->
      let word = List.assoc (f.offset, base) bitwords in
      let view =
        T.Land
          (T.Lsr (T.Ref (T.I, word), T.Int shift), T.Int ((1 lsl width) - 1))
      in
      { w with views = (f.id, view) :: w.views }
  | _ -> w

(* The name of the function cut out before the field [f]: its own name. *)
let field_label w f =
  let own = if f.name = "" then f.label else label f.name in
  if w.section = "" then own else w.section ^ "_" ^ own

(* [Planner.word_range f g] over the variable that holds the shared word,
   when the walk is [ranged]. *)
let word_range w bitwords f g =
  if not w.m.ranged then None
  else
    Option.bind (Planner.word_range f g) (fun (at, lo, hi) ->
        Option.map (fun word -> (word, lo, hi)) (List.assoc_opt at bitwords))

(* The live fields that start before [until] in the prefixes of [nodes], at
   constant offsets from [at]. The prefix that starts each optional field of
   a following run counts too. *)
let rec following live ~until at nodes =
  if at >= until then []
  else
    match nodes with
    | Prefix (fields, n) :: rest ->
        List.filter_map
          (fun f ->
            let offset = extent at f.offset in
            if Sset.mem f.id live && offset < until then Some { f with offset }
            else None)
          fields
        @ following live ~until (extent at n) rest
    | Test _ :: rest -> following live ~until at rest
    | Gate _ :: _ as gates -> starts live ~until at gates
    | (Span _ | Region _) :: _ | [] -> []

and starts live ~until at = function
  | Gate (_, _, _, body) :: rest ->
      following live ~until at body @ starts live ~until at rest
  | _ -> []

(* A prefix, then [k] with the state and position after it, [after] the
   nodes that follow it. The fields after one that would make the function
   too deep or too long go in a function of their own. *)
let prefix w fields size p ~need ~after k =
  let w = bound w p need in
  let w, o, at = address w p in
  let live = List.filter (fun f -> Sset.mem f.id w.m.live) fields in
  (* The bytes known to be in the window start at 0 for a prefix at a
     constant offset, and at the prefix otherwise. *)
  let constant = Option.is_none p.var in
  let lo = if constant then 0 else at and hi = extent at w.known in
  (* A word that holds a field of this prefix ends within 8 bytes of it. *)
  let ends = extent at size in
  let later =
    if constant then following w.m.live ~until:(extent ends 8) ends after
    else []
  in
  let w, words, bitwords =
    load_words w ~o ~k:at ~size ~constant ~lo ~hi ~later live
  in
  let field ?checked w f = field ?checked w ~o ~k:at ~size ~words ~bitwords f in
  let rec go w = function
    | [] ->
        k
          {
            w with
            known =
              (if w.known >= beyond then beyond else max 0 (w.known - size));
          }
          { p with at = extent p.at size }
    | f :: rest as fields ->
        let range =
          match rest with g :: _ -> word_range w bitwords f g | [] -> None
        in
        if over w (List.length f.checks) then
          cut w ~label:(field_label w f) (fun w -> go w fields)
        else
          begin match (range, rest) with
          | Some (word, lo, hi), g :: rest ->
              let w = field ~checked:false (field ~checked:false w f) g in
              let num = w.m.num in
              pf w "if %s || %s then %s else@\n"
                (relation num "<" word (lit num lo))
                (relation num ">" word (lit num hi))
                (w.m.fail Bounds);
              let conditions =
                List.map (fun c -> c.cond) (f.checks @ g.checks)
              in
              go (refine { w with depth = w.depth + 1 } conditions) rest
          | _ -> go (field w f) rest
          end
  in
  go w fields

(* The key a checked span is remembered under. It names the window as well as
   the size and position, because positions restart at 0 in a region: a
   remembered bound is valid only in the window it was checked against. *)
let span_bound w size p = (w.base, w.limit, key size, pos_expr w.m.num p)

(* The position [p] as a sum. With [expand], the position's variable is
   replaced by its own sum when one is known. *)
let pos_sum w ~expand p =
  let var =
    match p.var with
    | None -> sum_int 0
    | Some v -> (
        match List.assoc_opt v w.sums with
        | Some s when expand -> s
        | _ -> sum_atom (T.Ref (T.I, v)))
  in
  sum_add var (sum_int p.at)

(* The test that [size] bytes at [p] overflow the window, with the terms
   shared by the position and the size cancelled. [None] when cancelling does
   not shorten the test, or when the result needs checked arithmetic. *)
let cancelled w size p =
  let num = w.m.num in
  let used expand =
    match (pos_sum w ~expand p, sum_of size) with
    | Some pos, Some n -> sum_add pos n
    | _ -> None
  in
  let plain =
    Option.fold ~none:0 ~some:sum_size (sum_of size)
    + (if p.var = None then 0 else 1)
    + if p.at = 0 then 0 else 1
  in
  let room u = sum_sub (sum_atom (T.Ref (T.I, w.limit))) u in
  match
    List.filter_map
      (fun expand -> Option.bind (used expand) room)
      [ false; true ]
    |> List.sort (fun a b -> compare (sum_size a) (sum_size b))
  with
  | d :: _ when sum_size d - 1 < plain -> (
      match sum_fails d with
      | None -> None
      | Some (lhs, rhs) ->
          let ov = ref false in
          let l = expr w ov lhs and r = expr w ov rhs in
          if !ov then None else Some (relation num ">" l r))
  | _ -> None

(* The test that a span of [n] bytes at [p] ends past the window, or that [n]
   is negative unless [nonnegative]. *)
let outside w ~ov ~nonnegative size p n =
  let num, room = room w p in
  (* An [int64] size is compared as one. *)
  let num, room =
    if w.m.num = Boxed && num <> Boxed then
      (Boxed, Fmt.str "(Int64.of_int %s)" room)
    else (num, room)
  in
  let room = if p.at = 0 then room else binop num "-" room (lit num p.at) in
  let upper =
    match if ov then None else cancelled w size p with
    | Some test -> test
    | None -> relation num ">" n room
  in
  if nonnegative then upper
  else Fmt.str "%s || %s" (relation num "<" n (lit num 0)) upper

(* The range of the end of a span at [p] whose size is in [n_range], or
   [None] when the end may overflow the native int. *)
let end_range w n_range p =
  let pos =
    match p.var with
    | None -> Some (p.at, p.at)
    | Some v ->
        corners T.checked_add
          (Option.value ~default:(min_int, max_int) (Smap.find_opt v w.ranges))
          (p.at, p.at)
  in
  Option.bind pos (fun r -> corners T.checked_add r n_range)

(* [w] after binding the size [n] and the end [e] of a span at [p]. It records
   their ranges and, unless the size may overflow, the end as a sum. An end
   that was checked is not negative. *)
let bind_span w ~ov ~implied ~n_range ~ends size p n e =
  let w = bind_num (bind_num w n) e in
  let e_range =
    match ends with
    | Some r when implied -> r
    | Some (lo, hi) -> (max 0 lo, hi)
    | None -> (0, max_int)
  in
  let sum =
    if ov then None
    else
      Option.bind (pos_sum w ~expand:true p) (fun pos ->
          Option.bind (sum_of size) (sum_add pos))
  in
  {
    w with
    ranges = Smap.add e e_range (Smap.add n n_range w.ranges);
    sums = (match sum with Some s -> (e, s) :: w.sums | None -> w.sums);
    ints = Sset.add e w.ints;
  }

(* A span of [size] bytes at [p]: its size, bound to a variable, and where it
   ends. With [implied], the next check covers this span too, so the span's
   own check is dropped when ranges prove its size not negative and its end
   inside the native int. This is bounds-check elimination decided on the
   plan; compilers do it on code, as Go's [prove] pass and ABCD (Bodik, Gupta
   and Sarkar, PLDI 2000). *)
let span ?(implied = false) w size p =
  let num = pos_num w.m.num in
  let ov = ref false in
  let s = expr w ov size in
  let ov = !ov in
  let n = w.fresh "n" in
  if ov then
    pf w "let %s = match %s with n -> n | exception %s.Overflow -> %s in@\n" n s
      arith (lit w.m.num (-1))
  else pf w "let %s = %s in@\n" n s;
  let nonnegative = (not ov) && nonnegative w size in
  let lo, hi = range w.ranges size in
  let n_range = (max 0 lo, max 0 hi) in
  let ends = end_range w n_range p in
  let implied = implied && nonnegative && Option.is_some ends in
  if not (implied || List.mem (span_bound w size p) w.bounded) then
    pf w "if %s then %s else@\n"
      (outside w ~ov ~nonnegative size p n)
      (w.m.fail Bounds);
  let e = w.fresh "e" in
  (* With [Boxed], the end is a position, so an [int]. *)
  let n_int = if w.m.num = Boxed then Fmt.str "(Int64.to_int %s)" n else n in
  pf w "let %s = %s in@\n" e (binop num "+" (pos_expr num p) n_int);
  let w = bind_span w ~ov ~implied ~n_range ~ends size p n e in
  ({ w with known = 0; depth = w.depth + 1 }, n, { var = Some e; at = 0 })

(* The bytes a run of prefixes and tests from here needs, checked at once. *)
let rec ahead = function
  | Prefix (_, n) :: rest -> extent n (ahead rest)
  | Test _ :: rest -> ahead rest
  | _ -> 0

(* {3 Optional fields}

   A run of optional fields whose conditions exclude each other, such as the
   cases of a tag, is one dispatch: a decision tree over the atoms of their
   conditions, each tested at most once on a path, whose leaves run one case
   or none. A subtree reached twice is written once, in a local function, as
   is a case reached twice; the code after the run is written once too. *)

type tree = { tid : int; node : tnode }
and tnode = Leaf of int option | Branch of bool T.expr * tree * tree

let decide facts conds =
  let memo = Hashtbl.create 16 in
  let count = ref 0 in
  let mk key node =
    match Hashtbl.find_opt memo key with
    | Some t -> t
    | None ->
        incr count;
        let t = { tid = !count; node = node () } in
        Hashtbl.replace memo key t;
        t
  in
  let leaf i = mk (Fmt.str "leaf %a" Fmt.(option int) i) (fun () -> Leaf i) in
  let rec choose facts c yes no =
    match simplify facts c with
    | T.Bool true -> yes facts
    | T.Bool false -> no facts
    | T.And (a, b) -> choose facts a (fun f -> choose f b yes no) no
    | T.Or (a, b) -> choose facts a yes (fun f -> choose f b yes no)
    | T.Not a -> choose facts a no yes
    | atom ->
        let y = yes (assume facts atom true) in
        let n = no (assume facts atom false) in
        incr count;
        { tid = !count; node = Branch (atom, y, n) }
  in
  let rec among facts conds =
    let residual =
      List.filter_map
        (fun (i, c) ->
          match simplify facts c with T.Bool false -> None | c -> Some (i, c))
        conds
    in
    let k =
      String.concat ";"
        (List.map (fun (i, c) -> Fmt.str "%d:%s" i (key c)) residual)
    in
    match residual with
    | [] -> leaf None
    | (i, c) :: rest ->
        mk k (fun () ->
            (choose facts c (fun _ -> leaf (Some i)) (fun f -> among f rest))
              .node)
  in
  among facts conds

(* Whether the conditions [a] and [b], [b] after [a], exclude each other, so
   [b] need not be evaluated where [a] holds: the codec evaluates it there, so
   it must not be one that may overflow, unless what [a] establishes decides
   it before any overflow. *)
let exclusive facts a b =
  simplify (assume facts a true) b = T.Bool false
  || (simplify (assume facts b true) a = T.Bool false && not (may_overflow b))

(* The optional fields at the head of [nodes] whose conditions, as [lower]
   writes them, exclude each other, and the nodes after them. *)
let gate_run ~lower facts nodes =
  let rec go acc = function
    | Gate (c, p, o, body) :: rest
      when let c = lower c in
           List.for_all (fun (c', _, _, _) -> exclusive facts c' c) acc ->
        go ((lower c, p, o, body) :: acc) rest
    | rest -> (List.rev acc, rest)
  in
  go [] nodes

(* A jump to the join [j] from the case [fired] of [gates], or from none:
   the outputs of the case that ran, absent ones for the others. *)
let jump w ~j ~exported gates ~fired p =
  let num = w.m.num in
  let args =
    List.concat_map
      (fun (i, (_, (present : output), children, _)) ->
        List.filter_map
          (fun (o : output) ->
            if not (exported o) then None
            else if Some i = fired then
              Some (if o == present then "true" else output_text w o)
            else if o.is_bool then Some "false"
            else Some (lit num 0))
          (present :: children))
      gates
  in
  (* On OxCaml the join closes over a buffer that may be [local], which only
     a call that is not a tail call may apply: the compiler still makes the
     join a jump. *)
  pf w "%s %a %s%s@\n" j pp_list args (pos_expr num p)
    (match num with Unboxed -> " [@nontail]" | Tagged | Boxed -> "")

(* The outputs of [gates] a copied continuation reads: those of the case
   [fired], or of none, are absent. A walk cut into functions writes them as
   literals, which no function then takes as arguments; any other binds
   them. *)
let bind_absent w ~exported gates ~fired =
  List.fold_left
    (fun w (i, (_, (present : output), children, _)) ->
      List.fold_left
        (fun w (o : output) ->
          let value =
            if not (exported o) then None
            else if Some i = fired then if o == present then Some true else None
            else Some false
          in
          match value with
          | None -> w
          | Some v when Option.is_some w.cuts ->
              { w with settled = (o.id, v) :: w.settled }
          | Some v ->
              pf w "let %s = %s in@\n" o.id
                (if o.is_bool then string_of_bool v else lit w.m.num 0);
              w)
        w (present :: children))
    w gates

(* The parameters of the join after [gates]: their outputs. *)
let joined ~exported gates =
  List.concat_map
    (fun (_, (present : output), children, _) ->
      List.filter (fun (o : output) -> exported o) (present :: children))
    gates

(* The nodes a continuation may be copied over: a few, none optional. *)
let rec copyable budget = function
  | [] -> budget >= 0
  | Gate _ :: _ -> false
  | Prefix (fields, _) :: rest -> copyable (budget - List.length fields) rest
  | (Span _ | Test _) :: rest -> copyable (budget - 1) rest
  | Region (_, _, body) :: rest ->
      copyable (budget - 1) body && copyable (budget - 1) rest

let copyable = copyable 8

(* The most copies of one continuation the dispatches on a path make. *)
let max_copies = 16

(* Whether a subtree of [tree] is reached more than once, and a case; and
   how many leaves are written, a shared case once. *)
let reach tree =
  let refs = Hashtbl.create 16 and cases = Hashtbl.create 16 in
  let rec count t =
    let n = Option.value ~default:0 (Hashtbl.find_opt refs t.tid) in
    Hashtbl.replace refs t.tid (n + 1);
    match t.node with
    | Leaf (Some i) ->
        Hashtbl.replace cases i
          (1 + Option.value ~default:0 (Hashtbl.find_opt cases i))
    | Leaf None -> ()
    | Branch (_, a, b) ->
        if n = 0 then begin
          count a;
          count b
        end
  in
  count tree;
  let shared t =
    match t.node with
    | Branch _ -> Hashtbl.find refs t.tid > 1
    | Leaf _ -> false
  in
  let rec leaves t =
    match t.node with
    | Leaf None -> 1
    | Leaf (Some _) -> 0
    | Branch (_, a, b) -> leaves a + leaves b
  in
  ( shared,
    (fun i -> Option.value ~default:0 (Hashtbl.find_opt cases i) > 1),
    leaves tree + Hashtbl.length cases )

(* The atom [id = c] on an [int64_u] variable [id], which is [c] where it
   holds; on a field held by its little-endian bytes too, the atom as a test of
   those bytes against [c] swapped, which needs no swap of the load. *)
let literal_test w atom =
  let raw id c =
    match List.assoc_opt id w.raws with
    | Some (raw, width) when c >= 0 && c < 1 lsl width ->
        let swapped =
          List.fold_left
            (fun acc i ->
              acc lor (((c lsr (8 * i)) land 0xff) lsl (width - 8 - (8 * i))))
            0
            (List.init (width / 8) Fun.id)
        in
        (* A 32-bit load is sign-extended. *)
        let swapped =
          if width = 32 && swapped land 0x8000_0000 <> 0 then
            swapped - (1 lsl 32)
          else swapped
        in
        Some (relation w.m.num "=" raw (lit w.m.num swapped))
    | _ -> None
  in
  match atom with
  | (T.Eq (T.Ref (T.I, id), T.Int c) | T.Eq (T.Int c, T.Ref (T.I, id)))
    when w.m.num = Unboxed && List.mem_assoc id w.scope
         && (not (is_slot w.m id))
         && not (List.mem_assoc id w.settled) ->
      Some (id, c, raw id c)
  | _ -> None

(* The test [atom], then [yes] where it holds and [no] where it does not. An
   atom that may overflow rejects. An atom on a variable that it fixes makes
   the variable that constant under [yes]. *)
let pp_branch w atom ~yes ~no =
  let ov = ref false in
  let literal = literal_test w atom in
  let s = match literal with Some (_, _, Some s) -> s | _ -> expr w ov atom in
  if !ov then begin
    pf w "match %s with@\n| true -> (@\n" s;
    yes ();
    pf w ")@\n| false -> (@\n";
    no ();
    pf w ")@\n| exception %s.Overflow -> %s@\n" arith (w.m.fail Bounds)
  end
  else begin
    pf w "if %s then (@\n" s;
    (match literal with
    | Some (id, c, _) ->
        scoped w (fun () ->
            w.deferred :=
              (id, (lit w.m.num c, false)) :: List.remove_assoc id !(w.deferred);
            yes ())
    | _ -> yes ());
    pf w ") else (@\n";
    no ();
    pf w ")@\n"
  end

(* [tree], each shared subtree written once in a local function after the
   ones it reaches; [leaf] writes a leaf. *)
let pp_tree w ~j ~shared ~leaf tree =
  let rec emit w t =
    match t.node with
    | Leaf i -> leaf w i
    | Branch (atom, yes, no) ->
        let inner = { w with depth = w.depth + 1 } in
        let branch t =
          scoped w (fun () ->
              if shared t then pf w "d%d_%s ()@\n" t.tid j else emit inner t)
        in
        pp_branch w atom ~yes:(fun () -> branch yes) ~no:(fun () -> branch no)
  in
  let defined = Hashtbl.create 16 in
  let rec define t =
    if not (Hashtbl.mem defined t.tid) then begin
      Hashtbl.replace defined t.tid ();
      match t.node with
      | Leaf _ -> ()
      | Branch (_, a, b) ->
          define a;
          define b;
          if shared t then begin
            flush w;
            pf w "let[@local] d%d_%s () =@\n" t.tid j;
            scoped w (fun () -> emit { w with depth = w.depth + 1 } t);
            pf w "in@\n"
          end
    end
  in
  define tree;
  if shared tree then pf w "d%d_%s ()@\n" tree.tid j else emit w tree

(* The conditions a path through [tree] opens, a shared subtree as one
   call. *)
let rec tree_depth shared t =
  match t.node with
  | Leaf _ -> 0
  | Branch (_, a, b) ->
      let sub t = if shared t then 0 else tree_depth shared t in
      1 + max (sub a) (sub b)

(* The name of the function a case of [o] is cut into: its presence
   without the prefix that numbers it. *)
let case_label (o : output) =
  match String.index_opt o.id '_' with
  | Some i when i > 1 && o.id.[0] = 'p' ->
      String.sub o.id (i + 1) (String.length o.id - i - 1)
  | _ -> label o.name

let node_label w = function
  | Prefix (f :: _, _) -> field_label w f
  | Prefix ([], _) -> "prefix"
  | Span _ -> "span"
  | Test _ -> "where"
  | Region _ -> "region"
  | Gate (_, o, _, _) -> case_label o ^ "_cases"

(* The conditions a node opens in the function that writes it; a dispatch
   counts its own. *)
let opens = function Prefix _ | Span _ | Test _ | Region _ -> 1 | Gate _ -> 0

(* The join [j] after [gates], whose body is [k], and the jump to it from a
   case that ran, or from none. *)
let define_join w ~j ~exported indexed gates k =
  let e = w.fresh "e" in
  let outputs = joined ~exported gates in
  flush w;
  pf w "let[@local] %s %a %s =@\n" j pp_list
    (List.map (fun (o : output) -> o.id) outputs)
    e;
  let inner =
    List.fold_left
      (fun w (o : output) ->
        if o.is_bool then bind ~ty:"bool" w o.id else bind_num w o.id)
      { w with known = 0; depth = w.depth + 1 }
      outputs
  in
  let inner = { (bind_num inner e) with ints = Sset.add e inner.ints } in
  scoped w (fun () -> k inner { var = Some e; at = 0 });
  pf w "in@\n";
  fun ~fired w' p -> jump w' ~j ~exported indexed ~fired p

(* Whether some path through [tree] runs no case. *)
let rec runs_none t =
  match t.node with
  | Leaf None -> true
  | Leaf (Some _) -> false
  | Branch (_, a, b) -> runs_none a || runs_none b

(* The size of the span every case of [gates] starts with, when a case
   always runs: its bound is then checked once before the dispatch. *)
let hoisted gates tree =
  let first (_, _, _, body) =
    match body with
    | (Region (size, _, _) | Span size) :: _ -> Some size
    | _ -> None
  in
  match List.map first gates with
  | Some size :: rest
    when List.for_all (( = ) (Some (key size))) (List.map (Option.map key) rest)
         && not (runs_none tree) ->
      Some size
  | _ -> None

(* The run of gates at the head of [nodes], the nodes after it, and its
   decision tree. *)
let dispatch_tree w nodes =
  let gates, rest = gate_run ~lower:(lower w) w.facts nodes in
  let tree = decide w.facts (List.mapi (fun i (c, _, _, _) -> (i, c)) gates) in
  (gates, rest, tree)

(* The bound of the span every case of [gates] starts with, at [p], checked
   once before the dispatch when a case always runs: the branch on the tag
   then waits on no length, and the cases skip the check. *)
let hoist_bound w gates tree p =
  match hoisted gates tree with
  | Some size when not (List.mem (span_bound w size p) w.bounded) ->
      let w', _, _ = span w size p in
      { w with depth = w'.depth; bounded = span_bound w size p :: w.bounded }
  | _ -> w

(* Whether the code after a span checks the bytes right after it before any
   read. A prefix, a span, a region and a dispatch that hoists its cases'
   bound do. Such a check covers the span too when its size is not negative. *)
let checks_next w = function
  | Prefix _ :: _ as nodes -> ahead nodes > 0
  | (Span _ | Region _) :: _ -> true
  | Gate _ :: _ as nodes ->
      let gates, _, tree = dispatch_tree w nodes in
      Option.is_some (hoisted gates tree)
  | Test _ :: _ | [] -> false

let rec walk w nodes p k =
  match nodes with
  | [] -> k w p
  | node :: _ when over w (opens node) ->
      cut w ~label:(node_label w node) (fun w -> step w nodes p k)
  | _ -> step w nodes p k

and step w nodes p k =
  match nodes with
  | [] -> k w p
  | Prefix (fields, size) :: rest ->
      prefix w fields size p ~need:(ahead nodes) ~after:rest (fun w p ->
          walk w rest p k)
  | Span size :: rest ->
      let w, _, p = span ~implied:(checks_next w rest) w size p in
      walk w rest p k
  | Test cond :: rest ->
      let w = check w Where cond in
      walk (refine w [ cond ]) rest p k
  | Region (size, at_most, body) :: rest ->
      let w', n, stop = span w size p in
      let b = w.fresh "b" in
      pf w' "let %s = %s in@\n" b
        (binop (pos_num w.m.num) "+" w.base (pos_expr w.m.num p));
      let w' = bind_num w' b in
      walk
        {
          w' with
          base = b;
          limit = n;
          known = 0;
          tail = w.tail && copyable rest;
          held = [];
        }
        body { var = None; at = 0 }
        (fun w'' cp ->
          let w'' =
            if at_most then w''
            else begin
              pf w'' "if %s then %s else@\n"
                (let at = pos_expr w.m.num cp in
                 if w.m.num = Boxed then
                   relation Boxed "<>" (Fmt.str "(Int64.of_int %s)" at) n
                 else relation w.m.num "<>" at n)
                (w.m.fail Bounds);
              { w'' with depth = w''.depth + 1 }
            end
          in
          walk
            {
              w'' with
              base = w.base;
              limit = w.limit;
              known = 0;
              tail = w.tail;
              held = w.held;
            }
            rest stop k)
  | Gate _ :: _ ->
      let gates, rest, tree = dispatch_tree w nodes in
      dispatch w gates tree p
        ~copy:(w.tail && copyable rest)
        (fun w p -> walk w rest p k)

(* A run of gates, and the code after it: written once in a join, or, when
   it is short and holds no optional field, copied into each branch, which
   keeps what the branch established. The outputs that are not slots reach
   it as bindings or as the join's parameters; a case that runs assigns its
   presence when a slot, and the slots of the others keep their absent
   value. Each case a branch copies the code after the run into is a
   function of its own. *)
and dispatch w gates tree p ~copy k =
  let exported = immutable w.m in
  let indexed = List.mapi (fun i g -> (i, g)) gates in
  let w = hoist_bound w gates tree p in
  let shared, case_shared, leaves = reach tree in
  if over w (tree_depth shared tree) then
    let _, (present : output), _, _ = List.hd gates in
    cut w
      ~label:(case_label present ^ "_cases")
      (fun w -> dispatch w gates tree p ~copy k)
  else
    let copy = copy && w.copies * leaves <= max_copies in
    let j = w.fresh "j" in
    let finish =
      if copy then fun ~fired w' p ->
        let w' = bind_absent w' ~exported indexed ~fired in
        k { w' with known = 0; tail = w.tail; copies = w.copies } p
      else define_join w ~j ~exported indexed gates k
    in
    let case w i =
      let copies = if copy then w.copies * leaves else 1 in
      dispatch_case w ~copy ~copies (List.nth gates i) p
        (finish ~fired:(Some i))
    in
    List.iter
      (fun (i, _) ->
        if case_shared i then begin
          flush w;
          pf w "let[@local] c%d_%s () =@\n" i j;
          scoped w (fun () -> case { w with depth = w.depth + 1 } i);
          pf w "in@\n"
        end)
      indexed;
    pp_tree w ~j ~shared tree ~leaf:(fun w -> function
      | None -> finish ~fired:None w p
      | Some i -> if case_shared i then pf w "c%d_%s ()@\n" i j else case w i)

(* The case [gate] of a dispatch, then [finish]: a function of its own when
   the branch copies the code after the dispatch. *)
and dispatch_case w ~copy ~copies (cond, (present : output), _, body) p finish =
  let go w =
    walk
      {
        (refine w [ cond ]) with
        tail = true;
        copies;
        pinned = w.pinned || not copy;
        section = case_label present;
      }
      body p
      (fun w' p ->
        if is_slot w.m present.id then
          pf w' "%s@\n" (slot_assign w.m.num ~is_bool:true present.id "true");
        finish w' p)
  in
  if copy then cut w ~label:(case_label present) go else go w

let start ?cuts ?(scope = []) ?first ~m ~ppf ~ranges ~base ~limit ~known nodes k
    =
  let n = ref 0 in
  let fresh s =
    incr n;
    Fmt.str "%s%d" s !n
  in
  let w =
    {
      m;
      ppf;
      fresh;
      ranges = Smap.add limit (0, max_int) ranges;
      facts = no_facts;
      views = [];
      words = [];
      raws = [];
      deferred = ref [];
      bounded = [];
      sums = [];
      ints = Sset.singleton limit;
      base;
      limit;
      known;
      tail = true;
      copies = 1;
      cuts;
      fn = { chars = 0; decisions = 0 };
      depth = 0;
      scope;
      settled = [];
      pinned = false;
      section = "";
      held = [];
    }
  in
  let p = { var = None; at = 0 } in
  match first with
  | Some label -> cut w ~label (fun w -> walk w nodes p k)
  | None -> walk w nodes p k

(* The variables a walk binds: those it reads, those it hands out and, for a
   cold walk, each checked field, whose value its error carries. *)
let live ?(cold = false) (plan : plan) ~outputs =
  Sset.of_list
    (references plan.nodes @ outputs
    @ List.filter_map
        (fun f -> if cold && f.checks <> [] then Some f.id else None)
        (fields_of plan.nodes))
