open Wire.Private
open Layout
module T = Types
module Smap = Map.Make (String)
module Sset = Set.Make (String)

exception Unsupported of string

let fail_unsupported fmt = Fmt.kstr (fun s -> raise (Unsupported s)) fmt

type target = [ `OCaml | `OxCaml ]
type typ = Typ : 'a Wire.typ -> typ

(* The OCaml path of each map a caller names, found by the map value itself. *)
type maps = { find : 'a. 'a T.typ -> string option }

let no_maps = { find = (fun _ -> None) }

let maps_of named =
  {
    find =
      (fun typ ->
        List.find_map
          (fun (path, Typ t) ->
            let id = Gen.map_id typ in
            if Option.is_some id && Gen.map_id t = id then Some path else None)
          named);
  }

(* A field's label: lower-cased, made an identifier, and escaped when a
   keyword of OCaml or of OxCaml, whatever the target, so the code written for
   the default target compiles on an OxCaml compiler too. *)
let label name = Names.ocaml_label name
let pp_list = Fmt.(list ~sep:(any " ") string)

(* The generated code is compiled for its target, whose [int] may have 31 bits
   (wasm_of_ocaml) or 32 (js_of_ocaml), so no literal it holds needs more than
   31 bits: a wider integer is written through [int64], and the mask of 32 bits
   is [mask32], bound by the module. *)
let max31 = (1 lsl 30) - 1
let narrow (lo, hi) = lo >= -max31 - 1 && hi <= max31

let int_lit n =
  if n > max31 || n < -max31 - 1 then
    Fmt.str "(Int64.to_int %LdL)" (Int64.of_int n)
  else if n < 0 then Fmt.str "(%d)" n
  else string_of_int n

(* The mask of the [width] low bits, [width] at most 32. *)
let mask width =
  if width <= 30 then Fmt.str "0x%x" ((1 lsl width) - 1)
  else Fmt.str "(mask32 lsr %d)" (32 - width)

(* {1 The plan}

   A codec is read as a list of nodes, in the order the codec checks them. A
   [Prefix] is a run of fields at constant offsets, checked to fit with one
   bound check; a [Span] skips the number of bytes an expression gives; a
   [Gate] runs its nodes when its condition holds, as an optional field does; a
   [Region] runs its nodes in a sized window of their own, as a field with a
   size does; a [Test] is a where clause. Expressions refer to the variables
   the fields are bound to, unique across the plan, so a nested codec's field
   never stands for an outer field of the same name. *)

(* What a failed check raises, as the codec does: a violated constraint
   (carrying the field's value), a value no case of a closed enum has, or an
   index past the cases of a lookup. *)
type kind = Constraint | Enum of int list | Tag
type check = { cond : bool T.expr; kind : kind }

type output = {
  id : string;
  name : string;
  is_bool : bool;
  wire_width : int;
  signed : bool;
}

(* A field of a prefix: its variable, its codec name and the OCaml label its
   functions derive from, its offset in the prefix (for a bitfield, its
   word's), how it is stored, its checks in the order the codec runs them,
   the closed enum the encoder checks it against, the map it goes through
   and, for a named scalar, the output it is. *)
type field = {
  id : string;
  name : string;
  label : string;
  offset : int;
  store : store;
  checks : check list;
  enum : (string * int list) option;
  map : string option;
  output : output option;
}

type node =
  | Prefix of field list * int
  | Span of int T.expr
  | Gate of bool T.expr * output * output list * node list
  | Region of int T.expr * bool * node list
  | Test of bool T.expr

type plan = {
  record : string;
  nodes : node list;
  outputs : output list;
  ranges : (int * int) Smap.t;
  wide : bool;
}

(* {2 Ranges}

   The values an integer expression may take, from the ranges of the
   variables it reads. An addition, subtraction or product whose ends fit the
   native int cannot overflow and is written as plain arithmetic; any other
   one fails where it overflows, as the codec's does. *)

let full = (min_int, max_int)

let store_range = function
  | Int { width; signed = true; _ } ->
      let n = 1 lsl (width - 1) in
      (-n, n - 1)
  | Int { width; _ } | Bits { width; _ } -> (0, (1 lsl width) - 1)
  | Bytes _ -> full

let corners op (lo, hi) (lo', hi') =
  let apply a b =
    match op a b with v -> Some v | exception Wire.Parse_error _ -> None
  in
  let ends = [ apply lo lo'; apply lo hi'; apply hi lo'; apply hi hi' ] in
  if List.mem None ends then None
  else
    let ends = List.filter_map Fun.id ends in
    Some (List.fold_left min max_int ends, List.fold_left max min_int ends)

let rec bits n = if n = 0 then 0 else 1 + bits (n lsr 1)

let rec range ranges (e : int T.expr) =
  let r = range ranges in
  let arith op a b = Option.value ~default:full (corners op (r a) (r b)) in
  match e with
  | T.Int n -> (n, n)
  | T.Ref (T.I, v) -> Option.value ~default:full (Smap.find_opt v ranges)
  | T.Add (a, b) -> arith T.checked_add a b
  | T.Sub (a, b) -> arith T.checked_sub a b
  | T.Mul (a, b) -> arith T.checked_mul a b
  | T.Land (a, b) -> (
      match (r a, r b) with
      | (lo, hi), (lo', hi') when lo >= 0 && lo' >= 0 -> (0, min hi hi')
      | (lo, hi), _ when lo >= 0 -> (0, hi)
      | _, (lo, hi) when lo >= 0 -> (0, hi)
      | _ -> full)
  | T.Lor (a, b) | T.Lxor (a, b) -> (
      match (r a, r b) with
      | (lo, hi), (lo', hi') when lo >= 0 && lo' >= 0 ->
          let n = bits (max hi hi') in
          if n >= Sys.int_size - 1 then (0, max_int) else (0, (1 lsl n) - 1)
      | _ -> full)
  | T.Lnot a ->
      let lo, hi = r a in
      (lnot hi, lnot lo)
  | T.Lsl (a, T.Int n) when n >= 0 && n < Sys.int_size - 1 ->
      let lo, hi = r a in
      if lo >= 0 && hi <= max_int asr n then (lo lsl n, hi lsl n) else full
  | T.Lsr (a, T.Int 0) -> r a
  | T.Lsr (a, T.Int n) when n > 0 && n < Sys.int_size ->
      let lo, hi = r a in
      if lo >= 0 then (lo lsr n, hi lsr n) else (0, max_int lsr (n - 1))
  | T.Cast (`U8, _) -> (0, 0xff)
  | T.Cast (`U16, _) -> (0, 0xffff)
  | T.If_then_else (_, a, b) ->
      let lo, hi = r a and lo', hi' = r b in
      (min lo lo', max hi hi')
  | _ -> full

(* Whether [op a b] cannot overflow the native int, so is written as plain
   arithmetic. *)
let fits ranges op a b =
  Option.is_some (corners op (range ranges a) (range ranges b))

(* Whether [e] holds a value, or shifts by an amount, that only an [int] of 63
   bits gets right. *)
let rec wide_expr : type a. (int * int) Smap.t -> a T.expr -> bool =
 fun ranges e ->
  let w : type b. b T.expr -> bool = fun x -> wide_expr ranges x in
  let int (x : int T.expr) = not (narrow (range ranges x)) in
  let shift (s : int T.expr) =
    match s with T.Int n -> n < 0 || n > 30 | _ -> true
  in
  match e with
  | T.Int n -> not (narrow (n, n))
  | T.Bool _ -> false
  | T.Ref (T.I, _) -> int e
  | T.Add (a, b) -> int e || w a || w b
  | T.Sub (a, b) -> int e || w a || w b
  | T.Mul (a, b) -> int e || w a || w b
  | T.Land (a, b) -> int e || w a || w b
  | T.Lor (a, b) -> int e || w a || w b
  | T.Lxor (a, b) -> int e || w a || w b
  | T.Lnot a -> int e || w a
  | T.Lsl (a, b) -> shift b || int e || w a
  | T.Lsr (a, b) -> shift b || int e || w a
  | T.Eq (a, b) -> w a || w b
  | T.Ne (a, b) -> w a || w b
  | T.Lt (a, b) -> w a || w b
  | T.Le (a, b) -> w a || w b
  | T.Gt (a, b) -> w a || w b
  | T.Ge (a, b) -> w a || w b
  | T.And (a, b) -> w a || w b
  | T.Or (a, b) -> w a || w b
  | T.Not a -> w a
  | T.Cast (_, a) -> w a
  | T.If_then_else (c, a, b) -> w c || w a || w b
  | _ -> false

(* The expressions the generated code can write. *)
let rec supported : type a. a T.expr -> unit =
 fun e ->
  let both : type b. b T.expr -> b T.expr -> unit =
   fun a b ->
    supported a;
    supported b
  in
  match e with
  | T.Int _ | T.Bool _ | T.Ref (T.I, _) -> ()
  | T.Add (a, b) | T.Sub (a, b) | T.Mul (a, b) -> both a b
  | T.Land (a, b) | T.Lor (a, b) | T.Lxor (a, b) -> both a b
  | T.Lsl (a, b) | T.Lsr (a, b) -> both a b
  | T.Lnot a -> supported a
  | T.Eq (a, b) -> both a b
  | T.Ne (a, b) -> both a b
  | T.Lt (a, b) -> both a b
  | T.Le (a, b) -> both a b
  | T.Gt (a, b) -> both a b
  | T.Ge (a, b) -> both a b
  | T.And (a, b) | T.Or (a, b) -> both a b
  | T.Not a -> supported a
  | T.Cast ((`U8 | `U16), a) -> supported a
  | T.If_then_else (c, a, b) ->
      supported c;
      both a b
  | _ -> fail_unsupported "expression %a" T.pp_expr e

(* {2 Building the plan} *)

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

(* A type read at a constant offset, in a prefix. *)
let rec is_leaf : type a. a T.typ -> bool = function
  | T.Bits _ -> true
  | T.Byte_slice { size = T.Int n } -> n >= 0
  | T.Where { inner; _ } -> is_leaf inner
  | T.Enum { base; _ } -> is_leaf base
  | T.Map { inner; _ } -> is_leaf inner
  | t -> Option.is_some (leaf t)

type st = {
  maps : maps;
  mutable count : int;
  mutable outs : output list;
  mutable ranges : (int * int) Smap.t;
}

let fresh st prefix name =
  st.count <- st.count + 1;
  Fmt.str "%s%d_%s" prefix st.count name

(* An output's name is its path, its labels joined by ['_']. A field under an
   anonymous one has no path: it is checked, but not handed out. *)
let output st path ~id ~is_bool store =
  let name = String.concat "_" (Option.value ~default:[] path) in
  let wire_width, signed =
    match store with
    | None -> (1, false)
    | Some (Int { width; signed; _ }) -> (width, signed)
    | Some (Bits { width; _ }) -> (width, false)
    | Some (Bytes n) -> (8 * n, false)
  in
  let o = { id; name; is_bool; wire_width; signed } in
  if Option.is_some path then st.outs <- o :: st.outs;
  o

(* [e] over the variables of [env], the scalars bound so far. *)
let bind env e =
  supported e;
  let e = T.subst_expr (List.map (fun (n, v) -> (n, T.Ref (T.I, v))) env) e in
  List.iter
    (fun r ->
      if not (List.exists (fun (_, v) -> String.equal v r) env) then
        fail_unsupported "a reference to %s" r)
    (T.collect_refs e);
  e

(* One field of a prefix at [off], its value under its map when it has one,
   and the scope after it: a scalar is bound under its name. *)
let place st ~path ~env ~word ~off ~name ~own typ =
  let lbl =
    match path with Some (_ :: _ as p) -> String.concat "_" p | _ -> "anon"
  in
  let id = fresh st "f" lbl in
  let self = Option.value name ~default:id in
  let layout : type b. b T.typ -> _ =
   fun typ ->
    match typ with
    | T.Bits { width; base; bit_order } ->
        let w, shift, next =
          place_bits ~off ~word ~base ~order:bit_order ~width
        in
        ( w.at,
          Bits { base; shift; width },
          [],
          None,
          Some { w with used = w.used + width },
          next )
    | T.Byte_slice { size = T.Int n } -> (off, Bytes n, [], None, None, off + n)
    | typ ->
        let store, refine = classify typ in
        (off, store, refine self, enum_of typ, None, off + width_bytes store)
  in
  let mapped : type b. b T.typ -> _ =
   fun typ ->
    match typ with
    | T.Map { inner; index_bound; _ } as m -> (
        match st.maps.find m with
        | None ->
            fail_unsupported "a map; name it in ~maps to generate this codec"
        | Some p ->
            let bound =
              Option.to_list
                (Option.map
                   (fun n -> (T.(Lt (Ref (I, self), Int n)), Tag))
                   index_bound)
            in
            (Some p, bound, layout inner))
    | typ -> (None, [], layout typ)
  in
  let map, extra, (offset, store, refine, enum, word, next) = mapped typ in
  let scalar = match store with Bytes _ -> false | _ -> true in
  let env = if scalar then (self, id) :: env else env in
  let checks =
    List.map
      (fun (cond, kind) -> { cond = bind env cond; kind })
      (refine @ extra @ List.map (fun e -> (e, Constraint)) own)
  in
  if (not scalar) && checks <> [] then
    fail_unsupported "a constraint on a byte array";
  if scalar then st.ranges <- Smap.add id (store_range store) st.ranges;
  let output =
    match (name, scalar) with
    | Some _, true -> Some (output st path ~id ~is_bool:false (Some store))
    | _ -> None
  in
  let name = Option.value name ~default:"" in
  ( { id; name; label = lbl; offset; store; checks; enum; map; output },
    word,
    next,
    env )

let flush off prefix acc =
  if prefix = [] then acc else acc @ [ Prefix (List.rev prefix, off) ]

let rec record st path (s : T.struct_) =
  if s.params <> [] then fail_unsupported "the codec has parameters";
  let rec go env word off prefix acc = function
    | [] -> (
        let acc = flush off prefix acc in
        match s.where with None -> acc | Some e -> acc @ [ Test (bind env e) ])
    | T.Field f :: rest -> (
        let path =
          Option.bind path (fun p ->
              Option.map (fun n -> p @ [ label n ]) f.field_name)
        in
        let step =
          try
            if Option.is_some f.action then fail_unsupported "an action";
            let own = Option.to_list f.constraint_ in
            if is_leaf f.field_typ then
              let fld, word, next, env =
                place st ~path ~env ~word ~off ~name:f.field_name ~own
                  f.field_typ
              in
              `Leaf (fld, word, next, env)
            else begin
              if own <> [] then
                fail_unsupported "a constraint on a composite field";
              `Nodes (composite st path ~name:f.field_name env f.field_typ)
            end
          with Unsupported why ->
            fail_unsupported "field %s: %s"
              (Option.value f.field_name ~default:"(anonymous)")
              why
        in
        match step with
        | `Leaf (fld, word, next, env) ->
            go env word next (fld :: prefix) acc rest
        | `Nodes nodes -> go env None 0 [] (flush off prefix acc @ nodes) rest)
  in
  go [] None 0 [] [] s.fields

and composite : type a.
    st ->
    string list option ->
    name:string option ->
    (string * string) list ->
    a T.typ ->
    node list =
 fun st path ~name env t ->
  match t with
  | T.Codec { codec_struct; _ } -> record st path codec_struct
  | T.Single_elem { size; elem; at_most } ->
      let size = bind env size in
      [ Region (size, at_most, element st path ~name env elem) ]
  | T.Optional { present; inner } ->
      let present = bind env present in
      let p =
        output st
          (Option.map (fun p -> p @ [ "present" ]) path)
          ~id:(fresh st "p" (Option.fold ~none:"anon" ~some:label name))
          ~is_bool:true None
      in
      let before = st.outs in
      let body = element st path ~name env inner in
      let rec added acc = function
        | l when l == before -> acc
        | o :: l -> added (o :: acc) l
        | [] -> acc
      in
      [ Gate (present, p, added [] st.outs, body) ]
  | T.Byte_slice { size } -> [ Span (bind env size) ]
  | T.Byte_array { size } -> [ Span (bind env size) ]
  | T.Unit -> []
  | t -> fail_unsupported "its type, %a" T.pp_typ t

and element : type a.
    st ->
    string list option ->
    name:string option ->
    (string * string) list ->
    a T.typ ->
    node list =
 fun st path ~name env t ->
  if is_leaf t then
    let f, _, next, _ = place st ~path ~env ~word:None ~off:0 ~name ~own:[] t in
    [ Prefix ([ f ], next) ]
  else composite st path ~name env t

(* Adjacent prefixes are one: one bound check covers both. *)
let rec fuse = function
  | Prefix (a, n) :: Prefix (b, m) :: rest ->
      let b = List.map (fun f -> { f with offset = f.offset + n }) b in
      fuse (Prefix (a @ b, n + m) :: rest)
  | Gate (c, p, o, body) :: rest -> Gate (c, p, o, fuse body) :: fuse rest
  | Region (s, a, body) :: rest -> Region (s, a, fuse body) :: fuse rest
  | n :: rest -> n :: fuse rest
  | [] -> []

(* Whether the code holds a value only an [int] of 63 bits holds: a 32-bit
   field, or an expression [wide_expr] finds. *)
let rec plan_wide ranges nodes =
  List.exists
    (function
      | Prefix (fields, _) ->
          List.exists
            (fun f ->
              (match f.store with
                | Int { width = 32; _ } | Bits { base = T.U32 _; _ } -> true
                | Int _ | Bits _ | Bytes _ -> false)
              || List.exists (fun c -> wide_expr ranges c.cond) f.checks)
            fields
      | Span e -> wide_expr ranges e
      | Test e -> wide_expr ranges e
      | Gate (e, _, _, body) -> wide_expr ranges e || plan_wide ranges body
      | Region (e, _, body) -> wide_expr ranges e || plan_wide ranges body)
    nodes

let plan_of ?(maps = no_maps) (s : T.struct_) =
  let st = { maps; count = 0; outs = []; ranges = Smap.empty } in
  let nodes = fuse (record st (Some []) s) in
  let outputs = List.rev st.outs in
  {
    record = s.name;
    nodes;
    outputs;
    ranges = st.ranges;
    wide = plan_wide st.ranges nodes;
  }

let rec fields_of nodes =
  List.concat_map
    (function
      | Prefix (fs, _) -> fs
      | Gate (_, _, _, b) | Region (_, _, b) -> fields_of b
      | Span _ | Test _ -> [])
    nodes

(* The variables the checks, sizes and conditions read. *)
let rec references nodes =
  List.concat_map
    (function
      | Prefix (fields, _) ->
          List.concat_map
            (fun f -> List.concat_map (fun c -> T.collect_refs c.cond) f.checks)
            fields
      | Span e -> T.collect_refs e
      | Test e -> T.collect_refs e
      | Gate (e, _, _, body) -> T.collect_refs e @ references body
      | Region (e, _, body) -> T.collect_refs e @ references body)
    nodes

(* {2 Facts}

   What a branch has established about the variables, which hold immutable
   values: the truth of the predicates tested on the way, and the integers an
   equality fixed. A fact holds only on the branch that established it. *)

type facts = { truths : (string * bool) list; integers : (string * int) list }

let no_facts = { truths = []; integers = [] }

(* A predicate's identity: its text, over the plan's variables. *)
let rec key : type a. a T.expr -> string =
 fun e ->
  let op o a b = Fmt.str "(%s %s %s)" (key a) o (key b) in
  match e with
  | T.Int n -> string_of_int n
  | T.Bool b -> string_of_bool b
  | T.Ref (T.I, v) -> v
  | T.Add (a, b) -> op "+" a b
  | T.Sub (a, b) -> op "-" a b
  | T.Mul (a, b) -> op "*" a b
  | T.Land (a, b) -> op "land" a b
  | T.Lor (a, b) -> op "lor" a b
  | T.Lxor (a, b) -> op "lxor" a b
  | T.Lsl (a, b) -> op "lsl" a b
  | T.Lsr (a, b) -> op "lsr" a b
  | T.Lnot a -> Fmt.str "(lnot %s)" (key a)
  | T.Eq (a, b) -> op "=" a b
  | T.Ne (a, b) -> op "<>" a b
  | T.Lt (a, b) -> op "<" a b
  | T.Le (a, b) -> op "<=" a b
  | T.Gt (a, b) -> op ">" a b
  | T.Ge (a, b) -> op ">=" a b
  | T.And (a, b) -> op "&&" a b
  | T.Or (a, b) -> op "||" a b
  | T.Not a -> Fmt.str "(not %s)" (key a)
  | T.Cast (`U8, a) -> Fmt.str "(u8 %s)" (key a)
  | T.Cast (`U16, a) -> Fmt.str "(u16 %s)" (key a)
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s then %s else %s)" (key c) (key a) (key b)
  | _ -> fail_unsupported "expression %a" T.pp_expr e

let rec assume facts (e : bool T.expr) truth =
  let facts = { facts with truths = (key e, truth) :: facts.truths } in
  match (e, truth) with
  | T.Not a, b -> assume facts a (not b)
  | T.And (a, b), true | T.Or (a, b), false ->
      assume (assume facts a truth) b truth
  | T.Eq (T.Ref (T.I, n), T.Int v), true
  | T.Eq (T.Int v, T.Ref (T.I, n)), true
  | T.Ne (T.Ref (T.I, n), T.Int v), false
  | T.Ne (T.Int v, T.Ref (T.I, n)), false ->
      { facts with integers = (n, v) :: facts.integers }
  | _ -> facts

(* Whether evaluating [e] may overflow, which rejects the record. *)
let rec may_overflow : type a. a T.expr -> bool = function
  | T.Add _ | T.Sub _ | T.Mul _ -> true
  | T.Int _ | T.Bool _ | T.Ref _ -> false
  | T.Land (a, b) | T.Lor (a, b) | T.Lxor (a, b) | T.Lsl (a, b) | T.Lsr (a, b)
    ->
      may_overflow a || may_overflow b
  | T.Lnot a -> may_overflow a
  | T.Eq (a, b) -> may_overflow a || may_overflow b
  | T.Ne (a, b) -> may_overflow a || may_overflow b
  | T.Lt (a, b) -> may_overflow a || may_overflow b
  | T.Le (a, b) -> may_overflow a || may_overflow b
  | T.Gt (a, b) -> may_overflow a || may_overflow b
  | T.Ge (a, b) -> may_overflow a || may_overflow b
  | T.And (a, b) | T.Or (a, b) -> may_overflow a || may_overflow b
  | T.Not a -> may_overflow a
  | T.Cast (_, a) -> may_overflow a
  | T.If_then_else (c, a, b) ->
      may_overflow c || may_overflow a || may_overflow b
  | _ -> true

(* [e] with what [facts] decides folded away. The code evaluates what is left
   from left to right, as the codec does: a left operand that may overflow
   stays, even when the right one decides, so the record is rejected where
   the codec's evaluation overflows. *)
let rec simplify facts (e : bool T.expr) : bool T.expr =
  match List.assoc_opt (key e) facts.truths with
  | Some b -> T.Bool b
  | None -> (
      let each = simplify facts in
      match e with
      | T.Not a -> (
          match each a with T.Bool b -> T.Bool (not b) | a -> T.Not a)
      | T.And (a, b) -> (
          match (each a, each b) with
          | T.Bool false, _ -> T.Bool false
          | a, T.Bool false when not (may_overflow a) -> T.Bool false
          | T.Bool true, b -> b
          | a, T.Bool true -> a
          | a, b -> T.And (a, b))
      | T.Or (a, b) -> (
          match (each a, each b) with
          | T.Bool true, _ -> T.Bool true
          | a, T.Bool true when not (may_overflow a) -> T.Bool true
          | T.Bool false, b -> b
          | a, T.Bool false -> a
          | a, b -> T.Or (a, b))
      | T.Eq (T.Ref (T.I, n), T.Int v) | T.Eq (T.Int v, T.Ref (T.I, n)) -> (
          match List.assoc_opt n facts.integers with
          | Some x -> T.Bool (x = v)
          | None -> e)
      | T.Ne (T.Ref (T.I, n), T.Int v) | T.Ne (T.Int v, T.Ref (T.I, n)) -> (
          match List.assoc_opt n facts.integers with
          | Some x -> T.Bool (x <> v)
          | None -> e)
      | _ -> e)

(* The ranges and facts passed checks establish, for the code after them. *)
let refined ranges facts conditions =
  let restrict ranges n lower upper =
    match Smap.find_opt n ranges with
    | None -> ranges
    | Some (lo, hi) -> Smap.add n (max lo lower, min hi upper) ranges
  in
  let rec go ranges : bool T.expr -> _ = function
    | T.And (a, b) -> go (go ranges a) b
    | T.Ge (T.Ref (T.I, n), T.Int v) | T.Le (T.Int v, T.Ref (T.I, n)) ->
        restrict ranges n v max_int
    | T.Le (T.Ref (T.I, n), T.Int v) | T.Ge (T.Int v, T.Ref (T.I, n)) ->
        restrict ranges n min_int v
    | T.Eq (T.Ref (T.I, n), T.Int v) | T.Eq (T.Int v, T.Ref (T.I, n)) ->
        restrict ranges n v v
    | _ -> ranges
  in
  ( List.fold_left go ranges conditions,
    List.fold_left (fun f e -> assume f e true) facts conditions )

(* Whether [e] is known not to be negative. *)
let nonnegative_in ranges facts (e : int T.expr) =
  let known e =
    match simplify facts e with T.Bool true -> true | _ -> false
  in
  fst (range ranges e) >= 0
  ||
  match e with
  | T.Sub (a, b) ->
      known (T.Ge (a, b))
      || known (T.Le (b, a))
      || known (T.Gt (a, b))
      || known (T.Lt (b, a))
  | _ -> false
