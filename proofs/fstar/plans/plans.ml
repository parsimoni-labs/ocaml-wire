(* Print the plans Wire_ocaml.Plan makes of the sampled codecs, the
   structured scan cases and the packet stack as Plan.Scan nodes, with an
   F* check that each is well formed: every variable bound once and read
   after it is bound, every run of prefixes no longer than
   Sys.max_string_length. These are the premises Plan.Proof.scanner_correct
   takes of the planner; F* evaluates them with its own integers.

   The conversion keeps what [wf] reads: the variables each expression and
   condition mentions, the size and field offsets of each prefix, the
   nesting of optional fields and regions. An expression keeps its
   variables, not its value; a field's width is its bytes, at most 7. *)

module P = Wire_ocaml.Plan
module T = Wire.Private.Types
module L = Wire.Private.Layout

let ids : (string, int) Hashtbl.t = Hashtbl.create 64
let next = ref 1

let fresh () =
  let v = !next in
  incr next;
  v

let var id =
  match Hashtbl.find_opt ids id with
  | Some v -> v
  | None ->
      let v = fresh () in
      Hashtbl.replace ids id v;
      v

(* The variables of an expression, as a sum the model can close over. *)
let rec vars : type a. a T.expr -> string list = function
  | T.Ref (_, id) -> [ id ]
  | T.Add (a, b)
  | T.Sub (a, b)
  | T.Mul (a, b)
  | T.Div (a, b)
  | T.Mod (a, b)
  | T.Land (a, b)
  | T.Lor (a, b)
  | T.Lxor (a, b)
  | T.Lsl (a, b)
  | T.Lsr (a, b) ->
      vars a @ vars b
  | T.Land64 (a, b) -> vars a @ vars b
  | T.Lsr64 (a, b) -> vars a @ vars b
  | T.Lnot a | T.Cast (_, a) -> vars a
  | T.Eq (a, b) -> vars a @ vars b
  | T.Ne (a, b) -> vars a @ vars b
  | T.Lt (a, b) -> vars a @ vars b
  | T.Le (a, b) -> vars a @ vars b
  | T.Gt (a, b) -> vars a @ vars b
  | T.Ge (a, b) -> vars a @ vars b
  | T.And (a, b) | T.Or (a, b) -> vars a @ vars b
  | T.Not a -> vars a
  | T.If_then_else (c, a, b) -> vars c @ vars a @ vars b
  | _ -> []

let sum = function
  | [] -> "Lit 0"
  | v :: vs ->
      List.fold_left
        (fun acc id -> Fmt.str "Add (%s) (V %d)" acc (var id))
        (Fmt.str "V %d" (var v))
        vs

let expr e = sum (vars e)

let width (f : P.field) =
  match f.store with
  | L.Int { width; _ } -> width / 8
  | L.Bits { base = T.U8; _ } -> 1
  | L.Bits { base = T.U16 _; _ } -> 2
  | L.Bits { base = T.U32 _; _ } -> 4
  | L.Bytes n -> min n 7

let field (f : P.field) =
  match width f with
  | 0 -> None
  | w ->
      Some (Fmt.str "{ id = %d; off = %d; width = %d }" (var f.id) f.offset w)

let rec node = function
  | P.Prefix (fields, size) ->
      let fs = List.filter_map field fields in
      let checks =
        List.concat_map
          (fun (f : P.field) ->
            List.map
              (fun (c : P.check) ->
                Fmt.str "Test (Ge (%s) (Lit 0))" (expr c.cond))
              f.checks)
          fields
      in
      Fmt.str "Prefix [ %s ] %d" (String.concat "; " fs) size :: checks
  | P.Span e ->
      let n = fresh () in
      let e' = fresh () in
      [ Fmt.str "Span %d %d (%s)" n e' (expr e) ]
  | P.Test e -> [ Fmt.str "Test (Ge (%s) (Lit 0))" (expr e) ]
  | P.Gate (c, _, _, body) ->
      [
        Fmt.str "Test (Ge (%s) (Lit 0))" (expr c);
        Fmt.str "Gate 0 [ (0, %s) ] None" (nodes body);
      ]
  | P.Region (e, at_most, body) ->
      let n = fresh () in
      let e' = fresh () in
      [ Fmt.str "Region %d %d (%s) %b %s" n e' (expr e) at_most (nodes body) ]

and nodes ns = Fmt.str "[ %s ]" (String.concat "; " (List.concat_map node ns))

(* {1 The plan with its values, for the decisions}

   Here an expression keeps its value where the model's expressions can say
   it: literals, variables, [+], [-], [*] and a shift left by a constant.
   Any other expression is a variable of its own, one per distinct
   expression, which the model knows nothing of, so it decides less. Field
   ranges and constraints are tests after the prefix, as the planner's
   ranges and Emitter.checks refine them. *)

let rec key : type a. a T.expr -> string = function
  | T.Int n -> string_of_int n
  | T.Int64 n -> Int64.to_string n ^ "L"
  | T.Bool b -> string_of_bool b
  | T.Ref (_, id) -> id
  | T.Add (a, b) -> Fmt.str "(+ %s %s)" (key a) (key b)
  | T.Sub (a, b) -> Fmt.str "(- %s %s)" (key a) (key b)
  | T.Mul (a, b) -> Fmt.str "(* %s %s)" (key a) (key b)
  | T.Div (a, b) -> Fmt.str "(/ %s %s)" (key a) (key b)
  | T.Mod (a, b) -> Fmt.str "(mod %s %s)" (key a) (key b)
  | T.Land (a, b) -> Fmt.str "(land %s %s)" (key a) (key b)
  | T.Lor (a, b) -> Fmt.str "(lor %s %s)" (key a) (key b)
  | T.Lxor (a, b) -> Fmt.str "(lxor %s %s)" (key a) (key b)
  | T.Lnot a -> Fmt.str "(lnot %s)" (key a)
  | T.Lsl (a, b) -> Fmt.str "(lsl %s %s)" (key a) (key b)
  | T.Lsr (a, b) -> Fmt.str "(lsr %s %s)" (key a) (key b)
  | T.Land64 (a, b) -> Fmt.str "(land64 %s %s)" (key a) (key b)
  | T.Lsr64 (a, b) -> Fmt.str "(lsr64 %s %s)" (key a) (key b)
  | T.Cast (_, a) -> Fmt.str "(cast %s)" (key a)
  | T.If_then_else (c, a, b) -> Fmt.str "(if %s %s %s)" (key c) (key a) (key b)
  | T.Eq (a, b) -> Fmt.str "(= %s %s)" (key a) (key b)
  | T.Ne (a, b) -> Fmt.str "(<> %s %s)" (key a) (key b)
  | T.Lt (a, b) -> Fmt.str "(< %s %s)" (key a) (key b)
  | T.Le (a, b) -> Fmt.str "(<= %s %s)" (key a) (key b)
  | T.Gt (a, b) -> Fmt.str "(> %s %s)" (key a) (key b)
  | T.Ge (a, b) -> Fmt.str "(>= %s %s)" (key a) (key b)
  | T.And (a, b) -> Fmt.str "(&& %s %s)" (key a) (key b)
  | T.Or (a, b) -> Fmt.str "(|| %s %s)" (key a) (key b)
  | T.Not a -> Fmt.str "(not %s)" (key a)
  | _ -> "?"

module M = Plan_Expr
module S = Plan_Scan
module D = Plan_Decide

let z = Prims.of_int

let rec value (e : int T.expr) : M.expr =
  match e with
  | T.Int n -> M.Lit (z n)
  | T.Ref (T.I, id) -> M.V (z (var id))
  | T.Add (a, b) -> M.Add (value a, value b)
  | T.Sub (a, b) -> M.Sub (value a, value b)
  | T.Mul (a, b) -> M.Mul (value a, value b)
  | T.Lsl (a, T.Int k) when k >= 0 -> M.Shl (value a, z k)
  | e -> M.V (z (var ("opaque " ^ key e)))

let rec pp_value ppf (e : M.expr) =
  match e with
  | M.Lit n -> Fmt.string ppf (Prims.to_string n)
  | M.V v -> Fmt.pf ppf "v%s" (Prims.to_string v)
  | M.Add (a, b) -> Fmt.pf ppf "(%a + %a)" pp_value a pp_value b
  | M.Sub (a, b) -> Fmt.pf ppf "(%a - %a)" pp_value a pp_value b
  | M.Mul (a, b) -> Fmt.pf ppf "(%a * %a)" pp_value a pp_value b
  | M.Shl (a, k) -> Fmt.pf ppf "(%a lsl %s)" pp_value a (Prims.to_string k)

(* [e] when it is an integer expression. *)
let as_int : type a. a T.expr -> int T.expr option = function
  | T.Int _ as e -> Some e
  | T.Ref (T.I, _) as e -> Some e
  | (T.Add _ | T.Sub _ | T.Mul _ | T.Div _ | T.Mod _) as e -> Some e
  | (T.Land _ | T.Lor _ | T.Lxor _ | T.Lnot _ | T.Lsl _ | T.Lsr _) as e ->
      Some e
  | (T.Cast _ | T.If_then_else _) as e -> Some e
  | _ -> None

let ge a b = [ S.Test (S.Ge (value a, value b)) ]

(* The tests [c] establishes, as the model reads them: [a >= b] and what
   reduces to it. *)
let rec tests (c : bool T.expr) =
  let ints a b =
    match (as_int a, as_int b) with Some a, Some b -> Some (a, b) | _ -> None
  in
  match c with
  | T.And (a, b) -> tests a @ tests b
  | T.Ge (a, b) -> ( match ints a b with Some (a, b) -> ge a b | None -> [])
  | T.Le (a, b) -> ( match ints a b with Some (a, b) -> ge b a | None -> [])
  | T.Gt (a, b) -> (
      match ints a b with Some (a, T.Int n) -> ge a (T.Int (n + 1)) | _ -> [])
  | T.Lt (a, b) -> (
      match ints a b with Some (T.Int n, b) -> ge b (T.Int (n + 1)) | _ -> [])
  | _ -> []

let field_tests (f : P.field) =
  let upper w =
    [ S.Test (S.Ge (M.Lit (z ((1 lsl w) - 1)), M.V (z (var f.id)))) ]
  in
  let range =
    match f.store with
    | L.Bits { width; _ } -> upper width
    | L.Int { width; signed = false; _ } when width < 62 -> upper width
    | _ -> []
  in
  range @ List.concat_map (fun (c : P.check) -> tests c.cond) f.checks

let mfield (f : P.field) =
  match width f with
  | 0 -> None
  | w -> Some { S.id = z (var f.id); off = z f.offset; width = z w }

(* With [runs], a run of optional fields is one dispatch, the last case its
   default: some case always runs, which the hoisted bounds need. Without, a
   run may run none of its cases. The decisions of both readings are the
   model's: the emitter hoists only where no path runs none. *)
let rec dnode ~runs = function
  | P.Prefix (fields, size) ->
      S.Prefix (List.filter_map mfield fields, z size)
      :: List.concat_map field_tests fields
  | P.Span e ->
      let n = fresh () in
      let e' = fresh () in
      [ S.Span (z n, z e', value e) ]
  | P.Test e -> tests e
  | P.Gate _ -> assert false
  | P.Region (e, at_most, body) ->
      let n = fresh () in
      let e' = fresh () in
      [ S.Region (z n, z e', value e, at_most, dnodes ~runs body) ]

and dnodes ~runs ns =
  let rec go acc = function
    | [] -> List.rev acc
    | P.Gate _ :: _ as ns ->
        let rec gates bodies = function
          | P.Gate (_, _, _, body) :: rest -> gates (body :: bodies) rest
          | rest -> (List.rev bodies, rest)
        in
        let bodies, rest = gates [] ns in
        let case i b = (z i, dnodes ~runs b) in
        let g =
          if runs then
            let cases = List.rev (List.tl (List.rev bodies)) in
            let dflt = List.hd (List.rev bodies) in
            S.Gate (z 0, List.mapi case cases, Some (dnodes ~runs dflt))
          else S.Gate (z 0, List.mapi case bodies, None)
        in
        go (g :: acc) rest
    | n :: rest -> go (List.rev_append (dnode ~runs n) acc) rest
  in
  go [] ns

let kind (bound : P.bound) =
  match bound with
  | P.Next -> D.KNext
  | P.Hoisted -> D.KHoisted
  | P.Checked { negative; cancelled } ->
      D.KChecked (negative, Option.is_some cancelled)

let pp_kind ppf = function
  | D.KNext -> Fmt.string ppf "left to the next check"
  | D.KHoisted -> Fmt.string ppf "hoisted"
  | D.KChecked (neg, c) ->
      Fmt.pf ppf "checked%s%s"
        (if neg then "" else ", no negative test")
        (if c then ", cancelled" else "")

let plans = ref []
let refused = ref []

(* The emitter's decisions for a codec the model does not justify. *)
let findings = ref []

(* The control: claim every bound is left to the next check, which the model
   must refuse somewhere. *)
let fabricate = ref false

let add : type a. string -> a Wire.Codec.t -> unit =
 fun name c ->
  match P.v c with
  | p ->
      Hashtbl.reset ids;
      next := 1;
      let wf = nodes (P.nodes p) in
      Hashtbl.reset ids;
      next := 1;
      let d = dnodes ~runs:true (P.nodes p) in
      let d' = dnodes ~runs:false (P.nodes p) in
      (* The decisions of the scanner and of its step form. *)
      let all = P.decisions c @ P.decisions ~step:"Step" c in
      (* A window's length is the size variable of the region that opened
         it, or the record's own: so many regions deep. *)
      let rec depth window =
        match
          List.find_map
            (function
              | P.Span_bound s when s.size_var = window -> Some s.window
              | _ -> None)
            all
        with
        | Some outer -> 1 + depth outer
        | None -> 0
      in
      let spans =
        List.filter_map
          (function
            | P.Span_bound s ->
                Some
                  {
                    D.dsize = value s.size;
                    dkind = (if !fabricate then D.KNext else kind s.bound);
                    depth = z (depth s.window);
                    dat =
                      (match s.at.var with
                      | None -> Some (z s.at.at)
                      | Some _ -> None);
                  }
            | _ -> None)
          all
      in
      let model = D.decisions (z 0) d @ D.decisions (z 0) d' in
      let bad = D.unjustified model spans in
      if bad <> [] then findings := (name, bad, model) :: !findings;
      plans := (name, wf, List.length spans) :: !plans
  | exception Invalid_argument why -> refused := (name, why) :: !refused

(* [add name (mk ())], or the refusal of [mk ()]: Wire refuses some codecs
   when they are built. *)
let build : type a. string -> (unit -> a Wire.Codec.t) -> unit =
 fun name mk ->
  match mk () with
  | c -> add name c
  | exception Invalid_argument why -> refused := (name, why) :: !refused

(* The control the check must refuse: 33 variable-size codecs nested in each
   other, each ending with a slice of 2^57 - 10 bytes, whose slices the
   planner fuses into one prefix longer than any record (Defect.PrefixWrap). *)
module Deep = struct
  open Wire

  type t = { inner : t option; big : Bytesrw.Bytes.Slice.t }

  let big = 144115188075855862

  let rec level k : t Codec.t =
    let slice = Field.v "big" (byte_slice ~size:(int big)) in
    if k = 0 then
      let len = Field.v "len" uint8 in
      let data = Field.v "data" (byte_slice ~size:(Field.ref len)) in
      Codec.v "L0"
        (fun _ _ big -> { inner = None; big })
        Codec.
          [
            (len $ fun _ -> UInt8.zero);
            (data $ fun r -> r.big);
            (slice $ fun r -> r.big);
          ]
    else
      let inner = Field.v "inner" (codec (level (k - 1))) in
      Codec.v (Fmt.str "L%d" k)
        (fun inner big -> { inner = Some inner; big })
        Codec.
          [ (inner $ fun r -> Option.get r.inner); (slice $ fun r -> r.big) ]

  type top = { body : t; tail : UInt8.t }

  let codec () =
    let body = Field.v "body" (codec (level 32)) in
    let tail = Field.v "tail" uint8 in
    Codec.v "Top"
      (fun body tail -> { body; tail })
      Codec.[ (body $ fun r -> r.body); (tail $ fun r -> r.tail) ]
end

(* The region of #504: a case's region of 4 bytes at offset 2 holds a region
   of 4 bytes at its own offset 2, which a bound hoisted before the dispatch
   is taken for. *)
module Leak = struct
  open Wire

  type leaf = { l : UInt32.t }

  let leaf =
    let l = Field.v "l" uint32be in
    Codec.v "Leaf" (fun l -> { l }) Codec.[ (l $ fun r -> r.l) ]

  type inner = { x : UInt8.t; y : UInt8.t; w : leaf }

  let inner =
    let x = Field.v "x" uint8 and y = Field.v "y" uint8 in
    let w = Field.v "w" (nested_at_most ~size:(int 4) (codec leaf)) in
    Codec.v "Inner"
      (fun x y w -> { x; y; w })
      Codec.[ (x $ fun r -> r.x); (y $ fun r -> r.y); (w $ fun r -> r.w) ]

  type outer = {
    tag : UInt8.t;
    pad : UInt8.t;
    a : inner option;
    b : inner option;
  }

  let codec =
    let tag = Field.v "tag" uint8 and pad = Field.v "pad" uint8 in
    let a =
      Field.optional "a"
        ~present:Expr.(Field.ref tag = int 0)
        (nested_at_most ~size:(int 4) (codec inner))
    in
    let b =
      Field.optional "b"
        ~present:Expr.(not (Field.ref tag = int 0))
        (nested_at_most ~size:(int 4) (codec inner))
    in
    Codec.v "Outer"
      (fun tag pad a b -> { tag; pad; a; b })
      Codec.
        [
          (tag $ fun r -> r.tag);
          (pad $ fun r -> r.pad);
          (a $ fun r -> r.a);
          (b $ fun r -> r.b);
        ]
end

let () =
  let module_name =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "Plans"
  in
  let mode = if Array.length Sys.argv > 2 then Sys.argv.(2) else "wf" in
  fabricate := mode = "control";
  if mode = "leak" then add "hoisted bound in a region (#504)" Leak.codec
  else begin
    List.iter
      (fun (name, Fuzz_gen.Pack g) -> add name (Fuzz_gen.codec g))
      Diff_codecs.candidates;
    List.iter
      (fun (Diff_codecs.Scan { label; codec; _ }) -> add label codec)
      Diff_codecs.scan_cases;
    build "prefix wrap (Defect.PrefixWrap)" Deep.codec
  end;
  Fmt.pr "module %s@.@." module_name;
  Fmt.pr
    "(* Generated by proofs/fstar/plans/plans.exe: %d plans, %d codecs the@."
    (List.length !plans) (List.length !refused);
  Fmt.pr "   planner refuses. *)@.@.";
  Fmt.pr "open Plan.Expr@.open Plan.Scan@.open Plan.Proof@.@.";
  Fmt.pr "#set-options \"--fuel 0 --ifuel 0\"@.@.";
  List.iteri
    (fun i (name, wf, _) ->
      Fmt.pr "(* %s *)@.let p%d : list node = %s@." name i wf;
      Fmt.pr "let _ = assert_norm (wf (start 0).sc p%d)@.@." i)
    (List.rev !plans);
  (* The decisions, checked here by the model as F* extracts it. *)
  let decisions = List.fold_left (fun n (_, _, k) -> n + k) 0 !plans in
  let pp_decision ppf (d : D.decision) =
    Fmt.pf ppf "%a bytes, %s regions deep%s: %a" pp_value d.dsize
      (Prims.to_string d.depth)
      (match d.dat with Some at -> ", at " ^ Prims.to_string at | None -> "")
      pp_kind d.dkind
  in
  List.iter
    (fun (name, bad, model) ->
      Fmt.epr "@[<v2>%s: the emitter takes decisions the model does not:@,%a@,"
        name
        Fmt.(list ~sep:cut pp_decision)
        bad;
      Fmt.epr "@[<v2>the model takes:@,%a@]@]@."
        Fmt.(list ~sep:cut pp_decision)
        model)
    (List.rev !findings);
  Fmt.epr
    "decisions: %d plans, %d emitter decisions, %d plans with one the model \
     does not justify@."
    (List.length !plans) decisions (List.length !findings)
