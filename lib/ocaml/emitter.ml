open Wire.Private
open Layout
module T = Types
open Planner

(* {1 The emitter}

   One walk over the plan writes every function that checks a record: the
   boolean ones, the decoders, the folds and the one that finds the codec's
   error. It writes each node once: after optional fields the code joins in a
   local function the compiler turns into a jump, so the code for [n]
   optional fields grows with [n]. *)

(* How the code holds integers: as [int], or on OxCaml as [int64_u]. *)
type num = Tagged | Unboxed

(* The buffer the generated code reads: its type, its unchecked accessors, its
   length, the module of its bounds-checked loads and string copies, and an
   expression for a buffer of no bytes. *)
type src = {
  typ : string;
  u : string;
  len : string;
  checked : string;
  empty : string;
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
  }

let bigstring_src =
  {
    typ = rt ^ ".Unchecked.Bigstring.t";
    u = rt ^ ".Unchecked.Bigstring";
    len = rt ^ ".Unchecked.Bigstring.length";
    checked = rt ^ ".Unchecked.Bigstring";
    empty = "Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0";
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
  exported : Sset.t; (* The outputs that cross a join. *)
  fail : failure -> string;
  overflowed : failure -> string -> string;
      (* What a cold walk raises for an overflow, given the exception. *)
}

type pos = { var : string option; at : int }

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
}

let pf w fmt = Fmt.pf w.ppf fmt

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
  | T.Ref (T.I, v) -> v
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
   fails too. *)
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
      s arith (w.m.fail failure)

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
    { w with known = need }
  end

(* The offset in the buffer of the prefix at [p], as a variable and a
   constant to add. *)
let address w p =
  match p.var with
  | None -> (w.base, p.at)
  | Some v ->
      let o = w.fresh "o" in
      pf w "let %s = %s in@\n" o (binop w.m.num "+" w.base v);
      (o, p.at)

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

(* The words a prefix at [o + k] of [size] bytes loads once: the 64-bit ones
   two or more live fields share, then each word of a live bitfield, which
   the bitfields' views read. *)
let load_words w ~o ~k ~size live =
  let m = w.m in
  let words = words ~size live in
  List.iter
    (fun a ->
      let v = load m ~o ~k:(k + a) ~bytes:8 ~endian:T.Big in
      pf w "let w%d = %s in@\n" (k + a)
        (match m.num with Tagged -> v | Unboxed -> "U.of_int64 " ^ v))
    words;
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
  let w =
    List.fold_left
      (fun w ((_, base), v) ->
        let width = 8 * Bitfield.byte_size base in
        if width < Sys.int_size then
          {
            w with
            ranges = Smap.add v (0, (1 lsl width) - 1) w.ranges;
            words = (v, (1 lsl width) - 1) :: w.words;
          }
        else w)
      w bitwords
  in
  (w, words, bitwords)

(* A field of a prefix at [o + k]: bound when live, then checked. *)
let field w ~o ~k ~size ~words ~bitwords f =
  let m = w.m in
  let is_live = Sset.mem f.id m.live in
  let shifted = { f with offset = k + f.offset } in
  (if is_live then
     match f.store with
     | Int { width = 32; signed; endian } when m.raw32 ->
         pf w "let %s_raw = %s in@\n" f.id
           (load m ~o ~k:(k + f.offset) ~bytes:4 ~endian);
         pf w "let %s = (Int32.to_int %s_raw%s) in@\n" f.id f.id
           (if signed then "" else " land mask32")
     | Bits { base; _ } ->
         let bitword = List.assoc (f.offset, base) bitwords in
         pf w "let %s = %s in@\n" f.id (value m ~bitword ~o ~words ~k:0 shifted)
     | _ ->
         pf w "let %s = %s in@\n" f.id
           (value m ~size:(k + size) ~o ~words ~k:0 shifted));
  List.iter (fun c -> check w (Check (f, c)) c.cond) f.checks;
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

let prefix w fields size p ~need =
  let w = bound w p need in
  let o, k = address w p in
  let live = List.filter (fun f -> Sset.mem f.id w.m.live) fields in
  let w, words, bitwords = load_words w ~o ~k ~size live in
  let w = List.fold_left (field ~o ~k ~size ~words ~bitwords) w fields in
  ({ w with known = w.known - size }, { p with at = p.at + size })

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
  ({ w with known = 0 }, n, { var = Some e; at = 0 })

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
              Some (if o == present then "true" else o.id)
            else if o.is_bool then Some "false"
            else Some (lit num 0))
          (present :: children))
      gates
  in
  pf w "%s %a %s@\n" j pp_list args (pos_expr num p)

(* The outputs of [gates] a copied continuation reads: those of the case
   [fired], or of none, are bound absent. *)
let bind_absent w ~exported gates ~fired =
  List.iter
    (fun (i, (_, (present : output), children, _)) ->
      List.iter
        (fun (o : output) ->
          if exported o then
            if Some i = fired then
              begin if o == present then pf w "let %s = true in@\n" o.id
              end
            else
              pf w "let %s = %s in@\n" o.id
                (if o.is_bool then "false" else lit w.m.num 0))
        (present :: children))
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
  let rec emit t =
    match t.node with
    | Leaf i -> leaf i
    | Branch (atom, yes, no) ->
        let branch t =
          if shared t then pf w "d%d_%s ()@\n" t.tid j else emit t
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
            emit t;
            pf w "in@\n"
          end
    end
  in
  define tree;
  if shared tree then pf w "d%d_%s ()@\n" tree.tid j else emit tree

let rec walk w nodes p k =
  match nodes with
  | [] -> k w p
  | Prefix (fields, size) :: rest ->
      let w, p = prefix w fields size p ~need:(ahead nodes) in
      walk w rest p k
  | Span size :: rest ->
      let w, _, p = span w size p in
      walk w rest p k
  | Test cond :: rest ->
      check w Where cond;
      walk (refine w [ cond ]) rest p k
  | Region (size, at_most, body) :: rest ->
      let w', n, stop = span w size p in
      let b = w.fresh "b" in
      pf w "let %s = %s in@\n" b (binop w.m.num "+" w.base (pos_expr w.m.num p));
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
          if not at_most then
            pf w "if %s then %s else@\n"
              (relation w.m.num "<>" (pos_expr w.m.num cp) n)
              (w.m.fail Bounds);
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
   keeps what the branch established. *)
and dispatch w gates p ~copy k =
  let exported (o : output) = Sset.mem o.id w.m.exported in
  let indexed = List.mapi (fun i g -> (i, g)) gates in
  let tree =
    decide w.facts (List.map (fun (i, (c, _, _, _)) -> (i, c)) indexed)
  in
  let shared, case_shared, leaves = reach tree in
  let copy = copy && w.copies * leaves <= max_copies in
  let j = w.fresh "j" in
  let finish =
    if copy then (fun ~fired w' p ->
      bind_absent w' ~exported indexed ~fired;
      k { w' with known = 0; tail = w.tail; copies = w.copies } p)
    else begin
      let params =
        List.concat_map
          (fun (_, (present : output), children, _) ->
            List.filter exported (present :: children))
          gates
      in
      let e = w.fresh "e" in
      pf w "let[@local] %s %a %s =@\n" j pp_list
        (List.map (fun (o : output) -> o.id) params)
        e;
      k { w with known = 0 } { var = Some e; at = 0 };
      pf w "in@\n";
      fun ~fired w' p -> jump w' ~j ~exported indexed ~fired p
    end
  in
  let case i =
    let cond, _, _, body = List.nth gates i in
    let copies = if copy then w.copies * leaves else 1 in
    walk
      { (refine w [ cond ]) with tail = true; copies }
      body p
      (fun w' p -> finish ~fired:(Some i) w' p)
  in
  List.iter
    (fun (i, _) ->
      if case_shared i then begin
        pf w "let[@local] c%d_%s () =@\n" i j;
        case i;
        pf w "in@\n"
      end)
    indexed;
  pp_tree w ~j ~shared tree ~leaf:(function
    | None -> finish ~fired:None w p
    | Some i -> if case_shared i then pf w "c%d_%s ()@\n" i j else case i)

let start ~m ~ppf ~ranges ~base ~limit ~known nodes k =
  let n = ref 0 in
  let fresh s =
    incr n;
    Fmt.str "%s%d" s !n
  in
  walk
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
    }
    nodes { var = None; at = 0 } k

(* The variables a walk binds: those it reads, those it hands out and, for a
   cold walk, each checked field, whose value its error carries. *)
let live ?(cold = false) (plan : plan) ~outputs =
  Sset.of_list
    (references plan.nodes @ outputs
    @ List.filter_map
        (fun f -> if cold && f.checks <> [] then Some f.id else None)
        (fields_of plan.nodes))
