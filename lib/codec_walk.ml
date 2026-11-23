(* The walk of a record without parameters: one pass over its fields that
   checks and reads each once, in order. A size or a gate is a closure of one
   argument, the cursor, so each is one direct call; a sub-codec hands back its
   value and leaves the cursor at its end. *)

open Types
open Codec_repr
module Slice = Bytesrw.Bytes.Slice

let unwalked () = raise_notrace Unwalked

(* A field at a fixed offset of the record being walked, as an int. *)
let leaf : type a. a scalar -> cursor -> int = function
  | Byte o -> fun c -> UInt8.to_int (UInt8.unsafe_get c.wbuf (c.wbase + o))
  | U16le o -> fun c -> UInt16.to_int (UInt16.unsafe_le c.wbuf (c.wbase + o))
  | U16be o -> fun c -> UInt16.to_int (UInt16.unsafe_be c.wbuf (c.wbase + o))
  | U32le o -> fun c -> UInt32.to_int (UInt32.unsafe_le c.wbuf (c.wbase + o))
  | U32be o -> fun c -> UInt32.to_int (UInt32.unsafe_be c.wbuf (c.wbase + o))
  | Sbyte o -> fun c -> SInt8.to_int (SInt8.unsafe_get c.wbuf (c.wbase + o))
  | I16le o -> fun c -> SInt16.to_int (SInt16.unsafe_le c.wbuf (c.wbase + o))
  | I16be o -> fun c -> SInt16.to_int (SInt16.unsafe_be c.wbuf (c.wbase + o))
  | I32le o -> fun c -> SInt32.to_int (SInt32.unsafe_le c.wbuf (c.wbase + o))
  | I32be o -> fun c -> SInt32.to_int (SInt32.unsafe_be c.wbuf (c.wbase + o))
  | Bits8 { off; shift; mask } ->
      fun c -> (Unchecked.u8 c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      fun c -> (Unchecked.u16le c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      fun c -> (Unchecked.u16be c.wbuf (c.wbase + off) lsr shift) land mask

(* {1 Expressions}

   An expression compiles to as few closures as its shape allows, each reading
   the fields it needs itself: a call costs more than a read, so the number of
   calls is the cost. A field of the frame, scaled and offset ([ihl * 4 - 20]),
   is read in the closure of the operator that uses it; a conjunction of
   interval tests of fields is the staged word test of the check pass, one
   closure; the answers of earlier gates are bits of the cursor, tested
   together. *)

(* How a field of a frame of at least 8 bytes reads on a target whose [int]
   holds 63 bits: the 8-byte word at [q] of the record, shifted right by [s]
   and masked by [m]. *)
type word = { q : int; s : int; m : int }

let[@inline] word_value c q s m =
  Int64.to_int (Int64.shift_right_logical (Codec_words.load c.wbuf c.wbase q) s)
  land m

(* An integer operand: a literal, a fixed-offset field [f] of the record as
   [(f lsl k) + a], or a closure. A field holds at most 32 bits, [k] is at
   most 16 and [abs a] under [2^40], so the value lies far inside the native
   int and the checked arithmetic it stands for cannot overflow. *)
type operand =
  | Const of int
  | Lin of { f : packed_scalar; w : word option; k : int; a : int }
  | Fn of (cursor -> int)

let max_shift = 16
let small n = n > -(1 lsl 40) && n < 1 lsl 40

type env = {
  prefix : (string * packed_scalar) list;
  size : int;
      (* The bytes of the record's frame, which the walk checks before any
         expression reads a field. *)
  sizeof : bool;
  known : (bool expr * int) list;
      (* The gates of the record evaluated before, each with its bit of
         [wgates]. *)
  mutable checked : bool;  (** Whether an operation can raise. *)
}

let word env (Ps s) =
  match Codec_view.bits_of_scalar s with
  | Some { o; n; s; w; _ }
    when Sys.int_size >= 63 && env.size >= 8 && w > 0 && w <= 32 && o >= 0
         && o + n <= env.size ->
      let { Codec_words.q; l; r; m; _ } =
        Codec_words.word ~size:env.size o ~bytes:n ~s ~w ~signed:false
      in
      Some { q; s = r - l; m }
  | _ -> None

let lin env f = Lin { f; w = word env f; k = 0; a = 0 }

let lin_fn (Ps s) w k a =
  match (w, k, a) with
  | Some { q; s; m }, 0, 0 -> fun c -> word_value c q s m
  | Some { q; s; m }, _, _ -> fun c -> (word_value c q s m lsl k) + a
  | None, 0, 0 -> leaf s
  | None, _, _ ->
      let l = leaf s in
      fun c -> (l c lsl k) + a

let fn = function
  | Const n -> fun _ -> n
  | Lin { f; w; k; a } -> lin_fn f w k a
  | Fn f -> f

let field env name =
  match List.assoc_opt name env.prefix with
  | Some p -> lin env p
  | None -> raise Exit

let add env o n =
  match o with
  | Const m when small m && small n -> Const (m + n)
  | Lin l when small n && small (l.a + n) -> Lin { l with a = l.a + n }
  | o ->
      env.checked <- true;
      let f = fn o in
      Fn (fun c -> checked_add (f c) n)

let shift o n =
  match o with
  | Lin l when l.a = 0 && n >= 0 && l.k + n <= max_shift ->
      Lin { l with k = l.k + n }
  | o ->
      let f = fn o in
      Fn (fun c -> f c lsl n)

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n lsr 1)

let mul env o n =
  match o with
  | Lin l
    when l.a = 0 && n > 0 && n land (n - 1) = 0 && l.k + log2 n <= max_shift ->
      Lin { l with k = l.k + log2 n }
  | o ->
      env.checked <- true;
      let f = fn o in
      Fn (fun c -> checked_mul (f c) n)

(* [f land n] as a field of its own, when [f] is unsigned and the bits [n]
   keeps of it are its lowest. *)
let masked (Ps s) n =
  let bits m = if m land (m + 1) = 0 then Some m else None in
  match s with
  | Byte off ->
      Option.map
        (fun mask -> Ps (Bits8 { off; shift = 0; mask }))
        (bits (0xff land n))
  | U16be off ->
      Option.map
        (fun mask -> Ps (Bits16be { off; shift = 0; mask }))
        (bits (0xffff land n))
  | Bits8 { off; shift; mask } ->
      Option.map
        (fun mask -> Ps (Bits8 { off; shift; mask }))
        (bits (mask land n))
  | Bits16be { off; shift; mask } ->
      Option.map
        (fun mask -> Ps (Bits16be { off; shift; mask }))
        (bits (mask land n))
  | _ -> None

let mask env o n =
  match o with
  | Lin { f; k = 0; a = 0; _ } when n >= 0 -> (
      match masked f n with
      | Some (Ps (Bits8 { mask = 0; _ } | Bits16be { mask = 0; _ })) -> Const 0
      | Some f -> lin env f
      | None ->
          let f = fn o in
          Fn (fun c -> f c land n))
  | o ->
      let f = fn o in
      Fn (fun c -> f c land n)

(* [a + b] or [a - b] of two fields read as words: plain arithmetic, their
   bounds keeping it exact. *)
let sum env ~neg a b checked =
  match (a, b) with
  | ( Lin { w = Some { q = q1; s = s1; m = m1 }; k = k1; a = a1; _ },
      Lin { w = Some { q = q2; s = s2; m = m2 }; k = k2; a = a2; _ } ) ->
      if neg then
        Fn
          (fun c ->
            (word_value c q1 s1 m1 lsl k1)
            + a1
            - ((word_value c q2 s2 m2 lsl k2) + a2))
      else
        Fn
          (fun c ->
            (word_value c q1 s1 m1 lsl k1)
            + a1
            + (word_value c q2 s2 m2 lsl k2)
            + a2)
  | _ ->
      env.checked <- true;
      let fa = fn a and fb = fn b in
      Fn (fun c -> checked (fa c) (fb c))

let binary op a b =
  let fa = fn a and fb = fn b in
  Fn (fun c -> op (fa c) (fb c))

let checked_binary env op a b =
  env.checked <- true;
  binary op a b

(* A condition: a literal; whether the answers of earlier gates in [mask] are
   not all false ([set]) or all false; a conjunction of interval tests of
   fixed-offset fields, each one interval; or a closure. *)
type test =
  | Is of bool
  | Gates of { mask : int; set : bool }
  | Intervals of (packed_scalar * (int * int) array) list
  | Cond of (cursor -> bool)

let max_tests = 8

(* The answer [v] of a condition, recorded in [wgates] as the bit [bit] when
   it is a gate's ([bit <> 0]). *)
let[@inline] answer (c : cursor) bit v =
  if bit <> 0 then
    c.wgates <- c.wgates land lnot bit lor (-Bool.to_int v land bit);
  v

let bit i = if i >= 0 then 1 lsl i else 0
let[@inline] load (c : cursor) q = Codec_words.load c.wbuf c.wbase q
let holds = Codec_intervals.holds

(* The interval tests [tests] as tests of words of the frame, one closure of
   the cursor, which records its answer as the bit [gate] when it is not
   negative; [None] when a test is not one of a word. One row per number of
   tests; the formatter would staircase them. *)
let staged ~gate ~size tests =
  let gate = bit gate in
  match Codec_intervals.word_tests ~size tests with
  | Some [ (_, q1, s1, m1, lo1, n1) ] ->
      Some (fun c -> answer c gate (holds (load c q1) s1 m1 lo1 n1))
  | Some [ (_, q1, s1, m1, lo1, n1); (k2, q2, s2, m2, lo2, n2) ] ->
      Some (fun c -> let x1 = load c q1 in answer c gate (holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else load c q2 in holds x2 s2 m2 lo2 n2))
  | Some [ (_, q1, s1, m1, lo1, n1); (k2, q2, s2, m2, lo2, n2); (k3, q3, s3, m3, lo3, n3) ] ->
      Some (fun c -> let x1 = load c q1 in answer c gate (holds x1 s1 m1 lo1 n1 && let x2 = if k2 then x1 else load c q2 in holds x2 s2 m2 lo2 n2 && let x3 = if k3 then x2 else load c q3 in holds x3 s3 m3 lo3 n3))
  | Some _ ->
      Option.map
        (fun st c -> answer c gate (st c.wbuf c.wbase))
        (Codec_intervals.staged ~size tests)
  | None -> None
[@@ocamlformat "disable"]

let test env = function
  | Is b -> fun _ -> b
  | Gates { mask; set = true } -> fun (c : cursor) -> c.wgates land mask <> 0
  | Gates { mask; set = false } -> fun (c : cursor) -> c.wgates land mask = 0
  | Intervals tests -> (
      match staged ~gate:(-1) ~size:env.size (Array.of_list tests) with
      | Some st -> st
      | None ->
          let one (Ps s, set) =
            let l = leaf s in
            fun c -> Codec_intervals.in_set set (l c)
          in
          List.fold_right
            (fun t rest ->
              let t = one t in
              fun c -> t c && rest c)
            tests
            (fun _ -> true))
  | Cond f -> f

let negate env = function
  | Is b -> Is (not b)
  | Gates b -> Gates { b with set = not b.set }
  | t ->
      let f = test env t in
      Cond (fun c -> not (f c))

let range v lo hi =
  match v with
  | Const n -> Is (n >= lo && n <= hi)
  | Lin { f; k = 0; a = 0; _ } -> Intervals [ (f, [| (lo, hi) |]) ]
  | v ->
      let f = fn v in
      Cond
        (fun c ->
          let x = f c in
          x >= lo && x <= hi)

(* How two operands compare. *)
type order = Lt | Ge | Eq | Ne

let[@inline] ordered op (x : int) y =
  match op with Lt -> x < y | Ge -> x >= y | Eq -> x = y | Ne -> x <> y

(* Two fields read as words compare in a closure of the comparison, which
   tests no [op] when it runs. *)
let compare_words op q1 s1 m1 k1 a1 q2 s2 m2 k2 a2 =
  let[@inline] x c = (word_value c q1 s1 m1 lsl k1) + a1
  and[@inline] y c = (word_value c q2 s2 m2 lsl k2) + a2 in
  match op with
  | Lt -> fun c -> x c < y c
  | Ge -> fun c -> x c >= y c
  | Eq -> fun c -> x c = y c
  | Ne -> fun c -> x c <> y c

let compare op a b =
  match (a, b) with
  | ( Lin { w = Some { q = q1; s = s1; m = m1 }; k = k1; a = a1; _ },
      Lin { w = Some { q = q2; s = s2; m = m2 }; k = k2; a = a2; _ } ) ->
      Cond (compare_words op q1 s1 m1 k1 a1 q2 s2 m2 k2 a2)
  | _ ->
      let fa = fn a and fb = fn b in
      Cond (fun c -> ordered op (fa c) (fb c))

let rec int_expr env (e : int expr) : operand =
  let rec_ = int_expr env in
  match e with
  | Int n -> Const n
  | Ref (I, name) -> field env name
  | Sizeof t when env.sizeof ->
      Const (Option.value ~default:0 (field_wire_size t))
  | Param_ref _ | Sizeof _ | Sizeof_this | Field_pos -> raise Exit
  | Add (a, Int k) -> add env (rec_ a) k
  | Sub (a, Int k) when k <> min_int -> add env (rec_ a) (-k)
  | Mul (a, Int k) -> mul env (rec_ a) k
  | Lsl (a, Int k) -> shift (rec_ a) k
  | Land (a, Int k) -> mask env (rec_ a) k
  | Lsr (a, Int k) ->
      let fa = fn (rec_ a) in
      Fn (fun c -> fa c lsr k)
  | Add (a, b) -> sum env ~neg:false (rec_ a) (rec_ b) checked_add
  | Sub (a, b) -> sum env ~neg:true (rec_ a) (rec_ b) checked_sub
  | Mul (a, b) -> checked_binary env checked_mul (rec_ a) (rec_ b)
  | Div (a, b) -> checked_binary env checked_div (rec_ a) (rec_ b)
  | Mod (a, b) -> checked_binary env checked_mod (rec_ a) (rec_ b)
  | Land (a, b) -> binary ( land ) (rec_ a) (rec_ b)
  | Lor (a, b) -> binary ( lor ) (rec_ a) (rec_ b)
  | Lxor (a, b) -> binary ( lxor ) (rec_ a) (rec_ b)
  | Lnot a ->
      let fa = fn (rec_ a) in
      Fn (fun c -> lnot (fa c))
  | Lsl (a, b) -> binary ( lsl ) (rec_ a) (rec_ b)
  | Lsr (a, b) -> binary ( lsr ) (rec_ a) (rec_ b)
  | Cast (w, a) -> (
      let fa = fn (rec_ a) in
      match w with
      | `U8 -> Fn (fun c -> fa c land 0xFF)
      | `U16 -> Fn (fun c -> fa c land 0xFFFF)
      | `U32 -> Fn (fun c -> fa c land UInt32.mask32)
      | `U64 -> Fn fa)
  | If_then_else (b, t, e) ->
      let fb = test env (bool_expr env b)
      and ft = fn (rec_ t)
      and fe = fn (rec_ e) in
      Fn (fun c -> if fb c then ft c else fe c)

and as_int : type a. env -> a expr -> operand option =
 fun env e ->
  let go e = Some (int_expr env e) in
  match e with
  | Int _ as e -> go e
  | Ref (I, _) as e -> go e
  | Param_ref _ as e -> go e
  | Sizeof _ as e -> go e
  | Sizeof_this as e -> go e
  | Field_pos as e -> go e
  | Add _ as e -> go e
  | Sub _ as e -> go e
  | Mul _ as e -> go e
  | Div _ as e -> go e
  | Mod _ as e -> go e
  | Land _ as e -> go e
  | Lor _ as e -> go e
  | Lxor _ as e -> go e
  | Lnot _ as e -> go e
  | Lsl _ as e -> go e
  | Lsr _ as e -> go e
  | Cast _ as e -> go e
  | If_then_else _ as e -> go e
  | _ -> None

and as_bool : type a. env -> a expr -> test option =
 fun env e ->
  let go e = Some (bool_expr env e) in
  match e with
  | Bool _ as e -> go e
  | Eq _ as e -> go e
  | Ne _ as e -> go e
  | Lt _ as e -> go e
  | Le _ as e -> go e
  | Gt _ as e -> go e
  | Ge _ as e -> go e
  | And _ as e -> go e
  | Or _ as e -> go e
  | Not _ as e -> go e
  | _ -> None

and int_operands : type a. env -> a expr -> a expr -> _ =
 fun env a b ->
  match (as_int env a, as_int env b) with
  | Some fa, Some fb -> (fa, fb)
  | _ -> raise Exit

and bool_expr env (e : bool expr) : test =
  match List.assq_opt e env.known with
  | Some i -> Gates { mask = 1 lsl i; set = true }
  | None -> bool_cases env e

and bools : type a. env -> a expr -> a expr -> same:bool -> test option =
 fun env a b ~same ->
  match (as_bool env a, as_bool env b) with
  | Some ta, Some tb ->
      let fa = test env ta and fb = test env tb in
      Some
        (if same then Cond (fun c -> fa c = fb c)
         else Cond (fun c -> fa c <> fb c))
  | _ -> None

and bool_cases env (e : bool expr) : test =
  let int a = int_expr env a in
  match e with
  | Bool b -> Is b
  | Eq (a, Int k) -> range (int a) k k
  | Ne (a, Int k) -> negate env (range (int a) k k)
  | Lt (a, Int k) ->
      if k = min_int then Is false else range (int a) min_int (k - 1)
  | Le (a, Int k) -> range (int a) min_int k
  | Gt (a, Int k) ->
      if k = max_int then Is false else range (int a) (k + 1) max_int
  | Ge (a, Int k) -> range (int a) k max_int
  | Eq (a, b) -> (
      match bools env a b ~same:true with
      | Some t -> t
      | None ->
          let a, b = int_operands env a b in
          compare Eq a b)
  | Ne (a, b) -> (
      match bools env a b ~same:false with
      | Some t -> t
      | None ->
          let a, b = int_operands env a b in
          compare Ne a b)
  | Lt (a, b) ->
      let a, b = int_operands env a b in
      compare Lt a b
  | Le (a, b) ->
      let a, b = int_operands env a b in
      compare Ge b a
  | Gt (a, b) ->
      let a, b = int_operands env a b in
      compare Lt b a
  | Ge (a, b) ->
      let a, b = int_operands env a b in
      compare Ge a b
  | And (a, b) -> (
      match (bool_expr env a, bool_expr env b) with
      | Is false, _ -> Is false
      | Is true, t | t, Is true -> t
      | Intervals ta, Intervals tb
        when List.length ta + List.length tb <= max_tests ->
          Intervals (ta @ tb)
      | Gates { mask = m1; set = false }, Gates { mask = m2; set = false } ->
          Gates { mask = m1 lor m2; set = false }
      | ta, tb ->
          let fa = test env ta and fb = test env tb in
          Cond (fun c -> fa c && fb c))
  | Or (a, b) -> (
      match (bool_expr env a, bool_expr env b) with
      | Is true, _ -> Is true
      | Is false, t | t, Is false -> t
      | Gates { mask = m1; set = true }, Gates { mask = m2; set = true } ->
          Gates { mask = m1 lor m2; set = true }
      | ta, tb ->
          let fa = test env ta and fb = test env tb in
          Cond (fun c -> fa c || fb c))
  | Not a -> negate env (bool_expr env a)
  | Ref _ -> .

let env ?(sizeof = false) ?(known = []) ~size prefix =
  { prefix; size; sizeof; known; checked = false }

let size ~size prefix e = fn (int_expr (env ~sizeof:true ~size prefix) e)

(* A condition that raises leaves the record to the general path, whose check
   pass names its error at the field it constrains; a size that raises names
   its own, rebased to the record by [decode]. *)
let guard env f =
  if not env.checked then f
  else fun c -> match f c with b -> b | exception Parse_error _ -> unwalked ()

let cond ~size prefix e =
  let env = env ~size prefix in
  let f = test env (bool_expr env e) in
  guard env f

let cond_at prefix e =
  ignore (cond ~size:0 prefix e : cursor -> bool);
  fun ~size -> cond ~size prefix e

(* The gates a record has evaluated so far, each the bit of [wgates] that holds
   its answer, while there are bits. *)
type gates = (bool expr * int) list

let no_gates = []

let gate ~size ~gates prefix e =
  let env = env ~sizeof:true ~known:gates ~size prefix in
  let g = bool_expr env e in
  let i = List.length gates in
  let plain = guard env (test env g) in
  if List.memq e (List.map fst gates) || i >= Sys.int_size - 1 then
    (plain, plain, gates)
  else
    let staged =
      match g with
      | Intervals tests -> staged ~gate:i ~size:env.size (Array.of_list tests)
      | _ -> None
    in
    let b = bit i in
    ( (match (staged, g) with
      | Some st, _ -> st
      | None, Gates { mask; set } ->
          fun c -> answer c b (c.wgates land mask <> 0 = set)
      | None, _ -> fun c -> answer c b (plain c)),
      plain,
      (e, i) :: gates )

(* {1 Fields the cursor moves over}

   Each starts where the cursor is ([wpos]) and leaves it where the field
   ends, after the checks the layout pass makes of the same field. Each is a
   walk in its two modes: [wvalue] reads the field, [wskip] makes the same
   checks and moves the cursor the same way, building nothing. *)

type _ span = Slice_span : Slice.t span | String_span : string span

(* A byte span of [size] bytes, which must fit before [wend]. *)
let[@inline] bound size c =
  let cur = c.wpos and sz = size c in
  if sz < 0 || cur > Input_end.to_int c.wend - sz then
    Codec_leaf.check_span_bounds c.wend ~first:cur ~sz;
  c.wpos <- cur + sz;
  sz

let[@inline] span_value (type a) (kind : a span) size c : a =
  let cur = c.wpos in
  let sz = bound size c in
  match kind with
  | Slice_span ->
      if sz = 0 then Slice.eod else Slice.make c.wbuf ~first:cur ~length:sz
  | String_span -> Bytes.sub_string c.wbuf cur sz

let span kind size : _ Types.walk =
  {
    wvalue = (fun c -> span_value kind size c);
    wskip = (fun c -> ignore (bound size c : int));
  }

let with_name name (e : parse_error) =
  Parse_error { e with field = name :: e.field }

(* Each field the cursor moves over names the errors under it with [name],
   as the check pass does, when the cursor asks for names. A walk asks for
   them only once it has failed, so the walk that accepts sets up no handler. *)
let named name (w : 'a Types.walk) : 'a Types.walk =
  match name with
  | None -> w
  | Some name ->
      let in_name run c =
        if not c.wnamed then run c
        else try run c with Parse_error e -> raise (with_name name e)
      in
      { wvalue = in_name w.wvalue; wskip = in_name w.wskip }

(* A sub-codec in a region of [size] bytes, checked against the region's end,
   which it must reach unless [at_most]; the cursor goes on at the region's
   end. *)
let[@inline] inside ~at_most size c run =
  let cur = c.wpos and sz = size c and stop = c.wend and base = c.wbase in
  let gates = c.wgates in
  if sz < 0 || cur > Input_end.to_int stop - sz then
    Codec_leaf.check_span_bounds stop ~first:cur ~sz;
  c.wend <- Input_end.narrow stop (cur + sz);
  let v = run c in
  let n = c.wpos - cur in
  if n > sz || ((not at_most) && n <> sz) then
    raise_eof ~at:(cur + min n sz) ~expected:sz ~got:n;
  c.wbase <- base;
  c.wgates <- gates;
  c.wend <- stop;
  c.wpos <- cur + sz;
  v

let region ~at_most ~name size (sub : 'a Types.walk) : 'a Types.walk =
  named name
    {
      wvalue = (fun c -> inside ~at_most size c sub.wvalue);
      wskip = (fun c -> inside ~at_most size c sub.wskip);
    }

(* A sub-codec measuring itself. *)
let[@inline] keep c run =
  let base = c.wbase and gates = c.wgates in
  let v = run c in
  c.wbase <- base;
  c.wgates <- gates;
  v

let sub ~name (sub : 'a Types.walk) : 'a Types.walk =
  named name
    {
      wvalue = (fun c -> keep c sub.wvalue);
      wskip = (fun c -> keep c sub.wskip);
    }

(* The same fields present when [present] holds, each one closure with its
   gate; an absent one takes no bytes. *)
let gated_span present kind size : _ option Types.walk =
  {
    wvalue =
      (fun c -> if present c then Some (span_value kind size c) else None);
    wskip = (fun c -> if present c then ignore (bound size c : int));
  }

let gated_region present ~at_most ~name size (sub : 'a Types.walk) :
    'a option Types.walk =
  named name
    {
      wvalue =
        (fun c ->
          if present c then Some (inside ~at_most size c sub.wvalue) else None);
      wskip = (fun c -> if present c then inside ~at_most size c sub.wskip);
    }

let gated_sub present ~name (sub : 'a Types.walk) : 'a option Types.walk =
  named name
    {
      wvalue = (fun c -> if present c then Some (keep c sub.wvalue) else None);
      wskip = (fun c -> if present c then keep c sub.wskip);
    }

(* The bytes a field takes under its gate [present]: none when it is
   absent. *)
let gated_moves present size = Moves (fun c -> if present c then size c else 0)

(* A field the cursor moves over: a byte span, a sub-codec in a region, or one
   measuring itself, each with a walk of its own. *)
let moving : type a.
    name:string option ->
    frame:int ->
    (string * packed_scalar) list ->
    a typ ->
    a Types.walk * walk_move =
 fun ~name ~frame prefix typ ->
  let size = size ~size:frame prefix in
  match typ with
  | Byte_slice { size = e } -> (span Slice_span (size e), Moves (size e))
  | Byte_array { size = e } -> (span String_span (size e), Moves (size e))
  | Single_elem { size = e; elem = Codec { codec_walk = Some w; _ }; at_most }
    ->
      (region ~at_most ~name (size e) w, Moves (size e))
  | Codec { codec_walk = Some w; _ } ->
      let w = sub ~name w in
      (w, Walks w.wskip)
  | _ -> raise Exit

(* [moving] under the gate [present], in the closures of the field. *)
let gated_moving : type a.
    name:string option ->
    frame:int ->
    (string * packed_scalar) list ->
    (cursor -> bool) ->
    a typ ->
    a option Types.walk * walk_move =
 fun ~name ~frame prefix present typ ->
  let size = size ~size:frame prefix in
  let moves e = gated_moves present (size e) in
  match typ with
  | Byte_slice { size = e } -> (gated_span present Slice_span (size e), moves e)
  | Byte_array { size = e } -> (gated_span present String_span (size e), moves e)
  | Single_elem { size = e; elem = Codec { codec_walk = Some w; _ }; at_most }
    ->
      (gated_region present ~at_most ~name (size e) w, moves e)
  | Codec { codec_walk = Some w; _ } ->
      let w = gated_sub present ~name w in
      (w, Walks w.wskip)
  | _ -> raise Exit

(* A sub-codec at offset [off] of the record, in a region of [size] bytes when
   [size >= 0], which it must not pass, nor fall short of unless [at_most]:
   the cursor stays where it was. *)
let fixed_sub ?(at_most = true) ?(size = -1) ~name off (sub : 'a Types.walk) :
    'a Types.walk =
  let[@inline] at c run =
    let base = c.wbase and pos = c.wpos and stop = c.wend in
    let gates = c.wgates in
    let cur = base + off in
    c.wpos <- cur;
    if size >= 0 then c.wend <- Input_end.narrow stop (cur + size);
    let v = run c in
    (if size >= 0 then
       let n = c.wpos - cur in
       if n > size || ((not at_most) && n <> size) then unwalked ());
    c.wbase <- base;
    c.wgates <- gates;
    c.wend <- stop;
    c.wpos <- pos;
    v
  in
  named name
    { wvalue = (fun c -> at c sub.wvalue); wskip = (fun c -> at c sub.wskip) }

(* {1 The record}

   A field at a fixed offset of the record reads in place, the frame having
   been checked; any other the cursor moves over. *)

let scalar : type a. a scalar -> cursor -> a = function
  | Byte o -> fun c -> UInt8.unsafe_get c.wbuf (c.wbase + o)
  | U16le o -> fun c -> UInt16.unsafe_le c.wbuf (c.wbase + o)
  | U16be o -> fun c -> UInt16.unsafe_be c.wbuf (c.wbase + o)
  | U32le o -> fun c -> UInt32.unsafe_le c.wbuf (c.wbase + o)
  | U32be o -> fun c -> UInt32.unsafe_be c.wbuf (c.wbase + o)
  | Sbyte o -> fun c -> SInt8.unsafe_get c.wbuf (c.wbase + o)
  | I16le o -> fun c -> SInt16.unsafe_le c.wbuf (c.wbase + o)
  | I16be o -> fun c -> SInt16.unsafe_be c.wbuf (c.wbase + o)
  | I32le o -> fun c -> SInt32.unsafe_le c.wbuf (c.wbase + o)
  | I32be o -> fun c -> SInt32.unsafe_be c.wbuf (c.wbase + o)
  | Bits8 { off; shift; mask } ->
      fun c -> (Unchecked.u8 c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16le { off; shift; mask } ->
      fun c -> (Unchecked.u16le c.wbuf (c.wbase + off) lsr shift) land mask
  | Bits16be { off; shift; mask } ->
      fun c -> (Unchecked.u16be c.wbuf (c.wbase + off) lsr shift) land mask

let read : type a. a walk_read -> cursor -> a = function
  | Scalar s -> scalar s
  | String (off, n) -> fun c -> Bytes.sub_string c.wbuf (c.wbase + off) n
  | Slice (_, 0) -> fun _ -> Slice.eod
  | Slice (off, n) ->
      fun c -> Slice.make c.wbuf ~first:(c.wbase + off) ~length:n
  | Walk w -> w.wvalue

type (_, _) readers =
  | Nil : ('r, 'r) readers
  | Cons :
      (cursor -> 'a) * ('rest, 'result) readers
      -> ('a -> 'rest, 'result) readers

let readers : type full result.
    (full, result) walk_readers -> (full, result) readers =
 fun readers ->
  let rec go : type mid.
      (full, mid) walk_readers ->
      (mid, result) readers ->
      (full, result) readers =
   fun readers acc ->
    match readers with
    | WNil -> acc
    | WSnoc (rest, r) -> go rest (Cons (read r, acc))
  in
  go readers Nil

(* The application of [f] to the fields the readers read, in field order:
   one closure per arity, which makes a direct call per field and one
   saturated call of [f]. [None] past 32 fields. One row per arity; the
   formatter would staircase the nested [Cons] patterns. *)
let stage : type mid result.
    mid -> (mid, result) readers -> (cursor -> result) option =
 fun f readers ->
  match readers with
  | Nil -> Some (fun _ -> f)
  | Cons (r1, Nil) ->
      Some (fun c -> let v1 = r1 c in f v1)
  | Cons (r1, Cons (r2, Nil)) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in f v1 v2)
  | Cons (r1, Cons (r2, Cons (r3, Nil))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in f v1 v2 v3)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Nil)))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in f v1 v2 v3 v4)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Nil))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in f v1 v2 v3 v4 v5)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Nil)))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in f v1 v2 v3 v4 v5 v6)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Nil))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in f v1 v2 v3 v4 v5 v6 v7)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Nil)))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in f v1 v2 v3 v4 v5 v6 v7 v8)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Nil))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Nil)))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Nil))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Nil)))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Nil))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Nil)))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Nil))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Nil)))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Nil))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Nil)))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Nil))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Nil)))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Nil))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Nil)))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Nil))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Nil)))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Nil))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Nil)))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Nil))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Nil)))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Nil))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Nil)))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Cons (r31, Nil))))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in let v31 = r31 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31)
  | Cons (r1, Cons (r2, Cons (r3, Cons (r4, Cons (r5, Cons (r6, Cons (r7, Cons (r8, Cons (r9, Cons (r10, Cons (r11, Cons (r12, Cons (r13, Cons (r14, Cons (r15, Cons (r16, Cons (r17, Cons (r18, Cons (r19, Cons (r20, Cons (r21, Cons (r22, Cons (r23, Cons (r24, Cons (r25, Cons (r26, Cons (r27, Cons (r28, Cons (r29, Cons (r30, Cons (r31, Cons (r32, Nil)))))))))))))))))))))))))))))))) ->
      Some (fun c -> let v1 = r1 c in let v2 = r2 c in let v3 = r3 c in let v4 = r4 c in let v5 = r5 c in let v6 = r6 c in let v7 = r7 c in let v8 = r8 c in let v9 = r9 c in let v10 = r10 c in let v11 = r11 c in let v12 = r12 c in let v13 = r13 c in let v14 = r14 c in let v15 = r15 c in let v16 = r16 c in let v17 = r17 c in let v18 = r18 c in let v19 = r19 c in let v20 = r20 c in let v21 = r21 c in let v22 = r22 c in let v23 = r23 c in let v24 = r24 c in let v25 = r25 c in let v26 = r26 c in let v27 = r27 c in let v28 = r28 c in let v29 = r29 c in let v30 = r30 c in let v31 = r31 c in let v32 = r32 c in f v1 v2 v3 v4 v5 v6 v7 v8 v9 v10 v11 v12 v13 v14 v15 v16 v17 v18 v19 v20 v21 v22 v23 v24 v25 v26 v27 v28 v29 v30 v31 v32)
  | _ -> None
[@@ocamlformat "disable"]

(* {1 The walk of a record}

   The frame, then the checks of the fixed-offset fields, then each field read
   in order: in place, or by moving the cursor over it, which checks it as the
   layout pass would, then the [where] clause. So the walk accepts what the
   layout pass and the check pass it leaves accept, and reads each field where
   the read pass would, with each size and gate evaluated once. A sub-codec's
   walk builds its record before the walk of the record around it has finished
   checking: a constructor can see a record the walk then rejects.

   A walk that does not accept raises the error the check pass raises, in the
   same order: the frame against the input the record is given, its fields in
   order, which all fixed-offset ones precede, and its [where] clause last. The
   frame, a span, a region and the [where] clause raise their own; a check of a
   fixed-offset field that fails leaves the error to the record's check pass
   ([fail]), which finds the first field to fail, and a condition that raises
   leaves the record to the general path. *)

(* The error of the record at [base], which the walk did not accept, from its
   check pass; [Unwalked] if that pass accepts it. *)
let settle fail c base =
  fail c.wend c.wbuf base;
  unwalked ()

(* The conditions of [checks], compiled for a frame of [size] bytes, as one
   test. *)
let conditions ~size checks =
  match
    List.filter_map
      (function Holds f -> Some (f ~size) | Within _ -> None)
      checks
  with
  | [] -> None
  | [ f ] -> Some f
  | fs -> Some (fun c -> List.for_all (fun f -> f c) fs)

(* The walk tests the fixed-offset fields in the form the check pass picks:
   one word test, the staged word tests, or the scan of the flat tests. The
   frame check comes first, so the unchecked reads stay in the record. *)
let record_walk ~min_size ~start ~fail ~where checks body =
  let within =
    Array.of_list
      (List.filter_map
         (function Within (s, set) -> Some (s, set) | Holds _ -> None)
         checks)
  and holds = conditions ~size:min_size checks in
  let has_where = Option.is_some where in
  let where =
    match where with Some f -> f ~size:min_size | None -> fun _ -> true
  in
  let[@inline] frame c =
    let base = c.wpos and stop = c.wend in
    if base < 0 || base > Input_end.to_int stop - min_size then
      Codec_leaf.check_min_bounds min_size stop base;
    base
  in
  let[@inline] rest c base =
    c.wbase <- base;
    (match holds with Some f when not (f c) -> settle fail c base | _ -> ());
    c.wpos <- base + start;
    let v = body c in
    if has_where && not (where c) then raise_constraint ~at:base ~which:Where ();
    v
  in
  match Codec_intervals.single ~size:min_size within with
  | Some (q, l, r, m, lo, hi) ->
      fun c ->
        let base = frame c in
        if not (Codec_intervals.within c.wbuf base q l r m lo hi) then
          settle fail c base;
        rest c base
  | None when within = [||] ->
      fun c ->
        let base = frame c in
        rest c base
  | None -> (
      match Codec_intervals.word_tests ~size:min_size within with
      | Some [ (_, q, s, m, lo, n) ] ->
          fun c ->
            let base = frame c in
            if
              not
                (Codec_intervals.holds
                   (Codec_words.load c.wbuf base q)
                   s m lo n)
            then settle fail c base;
            rest c base
      | _ -> (
          match staged ~gate:(-1) ~size:min_size within with
          | Some test ->
              fun c ->
                let base = frame c in
                c.wbase <- base;
                if not (test c) then settle fail c base;
                rest c base
          | None ->
              let flat, tables = Codec_intervals.flatten_intervals within in
              let n = Array.length flat in
              fun c ->
                let base = frame c in
                if
                  Codec_intervals.scan_intervals flat tables c.wbuf base n 0
                  >= 0
                then settle fail c base;
                rest c base))

(* The walks of a list in order, one closure per four. *)
let rec seq = function
  | [] -> fun _ -> ()
  | [ a ] -> a
  | [ a; b ] ->
      fun c ->
        a c;
        b c
  | [ a; b; d ] ->
      fun c ->
        a c;
        b c;
        d c
  | [ a; b; d; e ] ->
      fun c ->
        a c;
        b c;
        d c;
        e c
  | a :: b :: d :: rest ->
      let rest = seq rest in
      fun c ->
        a c;
        b c;
        d c;
        rest c

(* The checks of the fields of [places], in field order: those of each field
   the cursor walks, as its walk makes them, and nothing for a field read in
   place, whose checks the record's frame and tests make. *)
let skips places = seq (List.filter_map (fun p -> p.pskip) places)

(* The offset [o], not negative, moved by a size [s] a check has passed: a
   negative one, which the bytes give when they changed since, refuses the
   place, and a sum past [max_int] is [max_int], past any buffer. *)
let[@inline] moved o s =
  if s < 0 then unwalked ()
  else
    let o = o + s in
    if o < 0 then max_int else o

(* The offset after the fields that take [sizes] bytes, from [o]: a closed
   form, one closure. *)
let rec advance = function
  | [] -> fun _ o -> o
  | [ f ] -> fun c o -> moved o (f c)
  | [ f; g ] -> fun c o -> moved (moved o (f c)) (g c)
  | f :: g :: rest ->
      let rest = advance rest in
      fun c o -> rest c (moved (moved o (f c)) (g c))

(* Where each field the cursor walks, or a gate guards, starts in the record at
   the cursor's [wbase]: past the fields before it, then its gate. The
   record's checks have passed, so the cursor moves over a span or a region
   by its size, with no test of its bounds, and walks only a sub-codec that
   measures itself; [enter] tests that the place's frame lies in the
   buffer. *)
let place ~start before gate =
  let moves =
    List.filter
      (fun p -> match p.pmove with Stays -> false | Moves _ | Walks _ -> true)
      before
    |> List.map (fun p -> p.pmove)
  in
  let sizes =
    List.filter_map
      (function Moves f -> Some f | Stays | Walks _ -> None)
      moves
  in
  if List.length sizes = List.length moves then
    match (sizes, gate) with
    | [], None -> fun c -> c.wbase + start
    | [], Some gate -> fun c -> if gate c then c.wbase + start else -1
    | [ f ], None -> fun c -> moved (c.wbase + start) (f c)
    | [ f ], Some gate ->
        fun c ->
          let o = moved (c.wbase + start) (f c) in
          if gate c then o else -1
    | [ f; g ], Some gate ->
        fun c ->
          let o = moved (moved (c.wbase + start) (f c)) (g c) in
          if gate c then o else -1
    | sizes, None ->
        let adv = advance sizes in
        fun c -> adv c (c.wbase + start)
    | sizes, Some gate ->
        let adv = advance sizes in
        fun c ->
          let o = adv c (c.wbase + start) in
          if gate c then o else -1
  else
    let walk =
      seq
        (List.map
           (function
             | Moves f -> fun c -> c.wpos <- moved c.wpos (f c)
             | Walks w -> w
             | Stays -> fun _ -> ())
           moves)
    in
    let gate = Option.value gate ~default:(fun _ -> true) in
    fun c ->
      c.wpos <- c.wbase + start;
      walk c;
      if gate c then c.wpos else -1

let places ~start places =
  let rec go before acc = function
    | [] -> List.rev acc
    | p :: rest ->
        let acc =
          match (p.pskip, p.pgate) with
          | None, None -> acc
          | _ -> (p.pname, place ~start (List.rev before) p.pgate) :: acc
        in
        go (p :: before) acc rest
  in
  go [] [] places

(* The walk of a sealed record without parameters, while every field has one
   and its [where] clause reads only fixed-offset fields. *)
let record ~n_params ~min_size ~prefix ~where ~fail make = function
  | Some w when n_params = 0 -> (
      match Option.map (cond_at prefix) where with
      | exception Exit -> None
      | where -> (
          let start = Option.value ~default:min_size w.wstart in
          let checks = List.rev w.wchecks_rev in
          let fields = List.rev w.wplaces_rev in
          let walk body =
            record_walk ~min_size ~start ~fail ~where checks body
          in
          match stage make (readers w.wreaders) with
          | Some value ->
              Some
                ( { wvalue = walk value; wskip = walk (skips fields) },
                  places ~start fields )
          | None -> None))
  | Some _ | None -> None

let[@inline] cursor ~named buf off wend =
  { wbuf = buf; wbase = off; wpos = off; wend; wgates = 0; wnamed = named }

(* The error of a walk that failed, from the same walk with names on. A size
   that does not evaluate is reported at the record it sizes, the one the
   cursor is in, as the check pass reports it. *)
let named_error walk general buf off =
  let c = cursor ~named:true buf off (Input_end.of_bytes buf) in
  match walk.wvalue c with
  | v -> v
  | exception Types.Unwalked -> general buf off
  | exception
      Parse_error
        ({ kind = Value_out_of_range _ | Zero_divisor; at = 0; _ } as e) ->
      raise (Parse_error { e with at = c.wbase })

(* A decode through [walk], or [general] when the walk can neither accept nor
   name an error. *)
let decode walk general buf off =
  match walk.wvalue (cursor ~named:false buf off (Input_end.of_bytes buf)) with
  | v -> v
  | exception Types.Unwalked -> general buf off
  | exception Parse_error _ -> named_error walk general buf off

(* Whether [walk] accepts the record at [off], the bytes stopping at [wend]. *)
let accepts walk buf off wend =
  match walk.wskip (cursor ~named:false buf off wend) with
  | () -> true
  | exception (Types.Unwalked | Types.Parse_error _) -> false
