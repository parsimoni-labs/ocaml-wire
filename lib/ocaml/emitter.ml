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

(* How the code holds integers: as [int], or on OxCaml as [int64_u]. *)
type num = Tagged | Unboxed

(* The buffer the generated code reads: its type, its unchecked accessors, its
   length, the module of its bounds-checked loads and string copies, an
   expression for a buffer of no bytes, and the parameter that binds it. *)
type src = {
  typ : string;
  u : string;
  len : string;
  checked : string;
  empty : string;
  buf : string;
}

let rt = "Wire_runtime"
let errors = rt ^ ".Errors"
let arith = rt ^ ".Arith"

let bytes_src =
  {
    typ = "bytes";
    u = rt ^ ".Unchecked";
    len = "Bytes.length";
    checked = "Bytes";
    empty = "Bytes.empty";
    buf = "buf";
  }

let bigstring_src =
  {
    typ = rt ^ ".Unchecked.Bigstring.t";
    u = rt ^ ".Unchecked.Bigstring";
    len = rt ^ ".Unchecked.Bigstring.length";
    checked = rt ^ ".Unchecked.Bigstring";
    empty = "Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0";
    buf = "buf";
  }

(* On OxCaml a function that reads or writes [buf] without keeping it takes it
   [local], so a caller may pass a buffer on its stack. *)
let local_bytes_src =
  {
    bytes_src with
    u = rt ^ ".Target.Local";
    checked = rt ^ ".Target.Local";
    buf = "(buf @ local)";
  }

let local_bigstring_src =
  {
    bigstring_src with
    u = rt ^ ".Target.Local.Bigstring";
    len = rt ^ ".Target.Local.Bigstring.length";
    checked = rt ^ ".Target.Local.Bigstring";
    buf = "(buf @ local)";
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

let pf w fmt =
  Fmt.kstr
    (fun s ->
      w.fn.chars <- w.fn.chars + String.length s;
      w.fn.decisions <- w.fn.decisions + decisions s;
      Format.pp_print_string w.ppf s)
    fmt

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
           else "ref 0"))
    outputs

(* An output's value where the walk ends. *)
let output_value m (o : output) =
  if is_slot m o.id then slot_read m.num ~is_bool:o.is_bool o.id else o.id

(* {2 Numbers} *)

let lit num n =
  match num with
  | Tagged -> int_lit n
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
  | Unboxed -> Fmt.str "(U.%s %s %s)" (unboxed_op op) a b

let relation num op a b =
  match num with
  | Tagged -> Fmt.str "((%s : int) %s %s)" a op b
  | Unboxed -> Fmt.str "(U.compare %s %s %s 0)" a b op

(* An [int64_u] holds the value of a 63-bit [int] sign-extended. *)
let normalise63 v = Fmt.str "(U.shift_right (U.shift_left %s 1) 1)" v

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
      | Unboxed ->
          Fmt.str "(U.of_int (%s (U.to_int %s) (U.to_int %s)))" fn (p a) (p b)
    end
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
      | None -> if is_slot w.m v then slot_read num ~is_bool:false v else v)
  | T.Add (a, b) -> arith_op T.checked_add "add" "+" a b
  | T.Sub (a, b) -> arith_op T.checked_sub "sub" "-" a b
  | T.Mul (a, b) -> arith_op T.checked_mul "mul" "*" a b
  | T.Land (a, b) -> binop num "land" (p a) (p b)
  | T.Lor (a, b) -> binop num "lor" (p a) (p b)
  | T.Lxor (a, b) -> binop num "lxor" (p a) (p b)
  | T.Lnot a -> (
      match num with
      | Tagged -> Fmt.str "(lnot %s)" (p a)
      | Unboxed -> Fmt.str "(U.lognot %s)" (p a))
  | T.Lsl (a, b) -> (
      match num with
      | Tagged -> Fmt.str "(%s lsl %s)" (p a) (p b)
      | Unboxed ->
          let v = Fmt.str "(U.shift_left %s (U.to_int %s))" (p a) (p b) in
          if fst (range w.ranges e) >= 0 then v else normalise63 v)
  | T.Lsr (a, b) -> (
      match num with
      | Tagged -> Fmt.str "(%s lsr %s)" (p a) (p b)
      | Unboxed ->
          let nonnegative = fst (range w.ranges a) >= 0 in
          let input =
            if nonnegative then p a
            else Fmt.str "(U.logand %s #0x7fffffffffffffffL)" (p a)
          in
          let v =
            Fmt.str "(U.shift_right_logical %s (U.to_int %s))" input (p b)
          in
          let lo, hi = range w.ranges b in
          if nonnegative || (lo > 0 && hi < 64) then v else normalise63 v)
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
    | Tagged -> Fmt.str "(%s + %d)" o k
    | Unboxed -> Fmt.str "(U.add %s #%dL)" o k

let load m ~o ~k ~bytes ~endian =
  let at = addr m.num o k in
  let e = endian_suffix endian in
  match (m.num, bytes) with
  | Tagged, 1 -> Fmt.str "(%s.u8 buf %s)" m.src.u at
  | Tagged, 2 -> Fmt.str "(%s.u16%s buf %s)" m.src.u e at
  | Tagged, 4 -> Fmt.str "(%s.i32%s buf %s)" m.src.u e at
  | Tagged, _ -> Fmt.str "(%s.i64be buf %s)" m.src.u at
  | Unboxed, 1 -> Fmt.str "(B.u8 buf %s)" at
  | Unboxed, 2 -> Fmt.str "(B.u16%s buf %s)" e at
  | Unboxed, 4 -> Fmt.str "(B.i32%s buf %s)" e at
  | Unboxed, _ -> Fmt.str "(B.i64be buf %s)" at

(* The 64-bit words of a prefix of [size] bytes worth loading once: those
   holding two or more of [fields] a shift takes out. *)
let in_word f =
  match f.store with
  | Int { width = 8; _ } | Int { width = 16 | 32; endian = T.Big; _ } -> true
  | Bits { base = T.U8 | T.U16 T.Big | T.U32 T.Big; _ } -> true
  | _ -> false

let covers a f = a <= f.offset && f.offset + width_bytes f.store <= a + 8

let words ~size fields =
  if size < 8 then []
  else
    List.init ((size + 7) / 8) (fun i -> min (8 * i) (size - 8))
    |> List.sort_uniq Int.compare
    |> List.filter (fun a ->
        let held =
          List.filter_map
            (fun f -> if in_word f && covers a f then Some f.offset else None)
            fields
        in
        List.length (List.sort_uniq Int.compare held) > 1)

(* A 16-bit field read through the 32-bit load that ends or starts on it, when
   that load stays in a prefix of [size] bytes: one load and one byte swap,
   with none of the 16-bit sign and zero extensions a 16-bit swap costs. The
   shift happens on the [int32], so a 31-bit [int] loses no bit. *)
let u16_in_word m ~size ~o ~k ~endian =
  let low i = Fmt.str "(Int32.to_int %s land 0xffff)" i
  and high i =
    Fmt.str "(Int32.to_int (Int32.shift_right_logical %s 16) land 0xffff)" i
  in
  let at k = load m ~o ~k ~bytes:4 ~endian in
  match (m.ox, m.num, size, endian) with
  | false, _, _, _ | _, Unboxed, _, _ | _, _, None, _ -> None
  | _, _, Some _, T.Big when k >= 2 -> Some (low (at (k - 2)))
  | _, _, Some size, T.Big when k + 4 <= size -> Some (high (at k))
  | _, _, Some size, T.Little when k + 4 <= size -> Some (low (at k))
  | _, _, Some _, T.Little when k >= 2 -> Some (high (at (k - 2)))
  | _ -> None

(* An unsigned value of [bytes] bytes at [k] from [o], from a word when one
   holds it; [size] bounds the bytes a wider load may read. *)
let unsigned m ?size ~o ~words ~k ~bytes ~endian () =
  let word = List.find_opt (fun a -> a <= k && k + bytes <= a + 8) words in
  match (m.num, word) with
  | Tagged, Some a when bytes = 1 || endian = T.Big ->
      Fmt.str "(Int64.to_int (Int64.shift_right_logical w%d %d) land %s)" a
        (8 * (a + 8 - k - bytes))
        (if bytes = 4 then "mask32" else Fmt.str "0x%x" ((1 lsl (8 * bytes)) - 1))
  | Unboxed, Some a when bytes = 1 || endian = T.Big ->
      let shifted =
        Fmt.str "(U.shift_right_logical w%d %d)" a (8 * (a + 8 - k - bytes))
      in
      if a = k then shifted
      else
        Fmt.str "(U.logand %s #%LdL)" shifted
          (Int64.pred (Int64.shift_left 1L (8 * bytes)))
  | Tagged, _ when bytes = 4 ->
      Fmt.str "(Int32.to_int %s land mask32)" (load m ~o ~k ~bytes ~endian)
  | Tagged, _ when bytes = 2 -> (
      match u16_in_word m ~size ~o ~k ~endian with
      | Some v -> v
      | None -> load m ~o ~k ~bytes ~endian)
  | Tagged, _ -> load m ~o ~k ~bytes ~endian
  | Unboxed, _ when bytes = 4 ->
      Fmt.str "(U.logand (U.of_int32 %s) #0xffffffffL)"
        (load m ~o ~k ~bytes ~endian)
  | Unboxed, _ -> Fmt.str "(U.of_int %s)" (load m ~o ~k ~bytes ~endian)

(* A field's plain value, at [k] from [o]: from the variable [bitword] of a
   bitfield's word when it has one. *)
let value m ?size ?bitword ~o ~words ~k f =
  let sign v width =
    match m.num with
    | Tagged ->
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
    | Unboxed ->
        Fmt.str "(U.shift_right (U.shift_left %s %d) %d)" v (64 - width)
          (64 - width)
  in
  match f.store with
  | Bits { base; shift; width } -> (
      let word =
        match bitword with
        | Some v -> v
        | None ->
            unsigned m ?size ~o ~words ~k:(k + f.offset)
              ~bytes:(Bitfield.byte_size base) ~endian:(bit_endian base) ()
      in
      let whole = shift + width = 8 * Bitfield.byte_size base in
      match m.num with
      | Tagged ->
          let v =
            if shift = 0 then word else Fmt.str "(%s lsr %d)" word shift
          in
          if whole then v else Fmt.str "(%s land %s)" v (mask width)
      | Unboxed ->
          let v =
            if shift = 0 then word
            else Fmt.str "(U.shift_right_logical %s %d)" word shift
          in
          if whole then v
          else
            Fmt.str "(U.logand %s #%LdL)" v
              (Int64.pred (Int64.shift_left 1L width)))
  | Int { width = 32; signed = true; endian } when m.num = Tagged ->
      Fmt.str "(Int32.to_int %s)" (load m ~o ~k:(k + f.offset) ~bytes:4 ~endian)
  | Int { width; signed; endian } ->
      let v =
        unsigned m ?size ~o ~words ~k:(k + f.offset) ~bytes:(width / 8) ~endian
          ()
      in
      if signed then sign v width else v
  | Bytes n ->
      Fmt.str "(%s.sub_string buf %s %d)" m.src.checked
        (addr Tagged o (k + f.offset))
        n

(* {2 Walking the plan} *)

let pos_expr num p =
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

let num_type w = match w.m.num with Unboxed -> Some "int64_u" | Tagged -> None
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
      body { w with ppf; fn = { chars = 0; decisions = 0 }; depth = 0 };
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

(* The window has [need] bytes from [p]. *)
let bound w p need =
  if w.known >= need then w
  else begin
    let num = w.m.num in
    let room =
      match p.var with None -> w.limit | Some v -> binop num "-" w.limit v
    in
    pf w "if %s then %s else@\n"
      (relation num "<" room (lit num (p.at + need)))
      (w.m.fail Bounds);
    { w with known = need; depth = w.depth + 1 }
  end

(* The offset in the buffer of the prefix at [p], as a variable and a
   constant to add. *)
let address w p =
  match p.var with
  | None -> (w, w.base, p.at)
  | Some v ->
      let o = w.fresh "o" in
      pf w "let %s = %s in@\n" o (binop w.m.num "+" w.base v);
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

(* The words a prefix at [o + k] of [size] bytes loads once: the 64-bit ones
   two or more live fields share, then each word of a live bitfield, which
   the bitfields' views read. *)
let load_words w ~o ~k ~size live =
  let m = w.m in
  (* A 32-bit field bound as its [int32] is one load of its own. *)
  let worded =
    List.filter
      (fun f ->
        match f.store with Int { width = 32; _ } -> not m.raw32 | _ -> true)
      live
  in
  let words = words ~size worded in
  let w =
    List.fold_left
      (fun w a ->
        let v = load m ~o ~k:(k + a) ~bytes:8 ~endian:T.Big in
        let name = Fmt.str "w%d" (k + a) in
        pf w "let %s = %s in@\n" name
          (match m.num with Tagged -> v | Unboxed -> "U.of_int64 " ^ v);
        bind_num w name)
      w words
  in
  let words = List.map (fun a -> k + a) words in
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

(* A field of a prefix at [o + k]: bound when live, then checked. *)
let field w ~o ~k ~size ~words ~bitwords f =
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
          pf w "let %s = (Int32.to_int %s_raw%s) in@\n" f.id f.id
            (if signed then "" else " land mask32");
          bind w f.id
        end
        else w
    | _ when not is_live -> w
    | Bits { base; _ } ->
        let bitword = List.assoc (f.offset, base) bitwords in
        bind_field w (value m ~bitword ~o ~words ~k:0 shifted)
    | _ -> bind_field w (value m ~size:(k + size) ~o ~words ~k:0 shifted)
  in
  let w =
    List.fold_left (fun w c -> check w (Check (f, c)) c.cond) w f.checks
  in
  let w = refine w (List.map (fun c -> c.cond) f.checks) in
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

(* A prefix, then [k] with the state and position after it. The fields after
   one that would make the function too deep or too long go in a function of
   their own. *)
let prefix w fields size p ~need k =
  let w = bound w p need in
  let w, o, at = address w p in
  let live = List.filter (fun f -> Sset.mem f.id w.m.live) fields in
  let w, words, bitwords = load_words w ~o ~k:at ~size live in
  let rec go w = function
    | [] -> k { w with known = w.known - size } { p with at = p.at + size }
    | f :: rest as fields ->
        let next w = go (field w ~o ~k:at ~size ~words ~bitwords f) rest in
        if over w (List.length f.checks) then
          cut w ~label:(field_label w f) (fun w -> go w fields)
        else next w
  in
  go w fields

(* A span of [size] bytes at [p]: its size, bound to a variable, and where it
   ends. *)
let span w size p =
  let num = w.m.num in
  let ov = ref false in
  let s = expr w ov size in
  let n = w.fresh "n" in
  if !ov then
    pf w "let %s = match %s with n -> n | exception %s.Overflow -> %s in@\n" n s
      arith (lit num (-1))
  else pf w "let %s = %s in@\n" n s;
  let room =
    match p.var with None -> w.limit | Some v -> binop num "-" w.limit v
  in
  let room = if p.at = 0 then room else binop num "-" room (lit num p.at) in
  let upper = relation num ">" n room in
  let outside =
    if (not !ov) && nonnegative w size then upper
    else Fmt.str "%s || %s" (relation num "<" n (lit num 0)) upper
  in
  pf w "if %s then %s else@\n" outside (w.m.fail Bounds);
  let e = w.fresh "e" in
  pf w "let %s = %s in@\n" e (binop num "+" (pos_expr num p) n);
  let w = bind_num (bind_num w n) e in
  ({ w with known = 0; depth = w.depth + 1 }, n, { var = Some e; at = 0 })

(* The bytes a run of prefixes and tests from here needs, checked at once. *)
let rec ahead = function
  | Prefix (_, n) :: rest -> n + ahead rest
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
  pf w "%s %a %s@\n" j pp_list args (pos_expr num p)

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

(* [tree], each shared subtree written once in a local function after the
   ones it reaches; [leaf] writes a leaf. An atom that may overflow rejects. *)
let pp_tree w ~j ~shared ~leaf tree =
  let rec emit w t =
    match t.node with
    | Leaf i -> leaf w i
    | Branch (atom, yes, no) ->
        let inner = { w with depth = w.depth + 1 } in
        let branch t =
          if shared t then pf w "d%d_%s ()@\n" t.tid j else emit inner t
        in
        let ov = ref false in
        let s = expr w ov atom in
        if !ov then begin
          pf w "match %s with@\n| true -> (@\n" s;
          branch yes;
          pf w ")@\n| false -> (@\n";
          branch no;
          pf w ")@\n| exception %s.Overflow -> %s@\n" arith (w.m.fail Bounds)
        end
        else begin
          pf w "if %s then (@\n" s;
          branch yes;
          pf w ") else (@\n";
          branch no;
          pf w ")@\n"
        end
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
            pf w "let[@local] d%d_%s () =@\n" t.tid j;
            emit { w with depth = w.depth + 1 } t;
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
  k (bind_num inner e) { var = Some e; at = 0 };
  pf w "in@\n";
  fun ~fired w' p -> jump w' ~j ~exported indexed ~fired p

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
      prefix w fields size p ~need:(ahead nodes) (fun w p -> walk w rest p k)
  | Span size :: rest ->
      let w, _, p = span w size p in
      walk w rest p k
  | Test cond :: rest ->
      let w = check w Where cond in
      walk (refine w [ cond ]) rest p k
  | Region (size, at_most, body) :: rest ->
      let w', n, stop = span w size p in
      let b = w.fresh "b" in
      pf w' "let %s = %s in@\n" b
        (binop w.m.num "+" w.base (pos_expr w.m.num p));
      let w' = bind_num w' b in
      walk
        {
          w' with
          base = b;
          limit = n;
          known = 0;
          tail = w.tail && copyable rest;
        }
        body { var = None; at = 0 }
        (fun w'' cp ->
          let w'' =
            if at_most then w''
            else begin
              pf w'' "if %s then %s else@\n"
                (relation w.m.num "<>" (pos_expr w.m.num cp) n)
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
            }
            rest stop k)
  | Gate _ :: _ ->
      let gates, rest = gate_run ~lower:(lower w) w.facts nodes in
      dispatch w gates p
        ~copy:(w.tail && copyable rest)
        (fun w p -> walk w rest p k)

(* A run of gates, and the code after it: written once in a join, or, when
   it is short and holds no optional field, copied into each branch, which
   keeps what the branch established. The outputs that are not slots reach
   it as bindings or as the join's parameters; a case that runs assigns its
   presence when a slot, and the slots of the others keep their absent
   value. Each case a branch copies the code after the run into is a
   function of its own. *)
and dispatch w gates p ~copy k =
  let exported = immutable w.m in
  let indexed = List.mapi (fun i g -> (i, g)) gates in
  let tree =
    decide w.facts (List.map (fun (i, (c, _, _, _)) -> (i, c)) indexed)
  in
  let shared, case_shared, leaves = reach tree in
  if over w (tree_depth shared tree) then
    let _, (present : output), _, _ = List.hd gates in
    cut w
      ~label:(case_label present ^ "_cases")
      (fun w -> dispatch w gates p ~copy k)
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
          pf w "let[@local] c%d_%s () =@\n" i j;
          case { w with depth = w.depth + 1 } i;
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
      ranges;
      facts = no_facts;
      views = [];
      words = [];
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
