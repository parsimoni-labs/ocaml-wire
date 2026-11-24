(* The properties of one entry of a generated module, as Z3 queries. *)

type entry = {
  name : string;
  inputs : Symex.input list;
  pre : string;
  expr : string;
  window : string * string;
  spec : string option;
  bound : int;
  misuse : bool;
  contracts : (string * string) list;
      (* functions proved as entries of their own, with their precondition:
         a call checks the precondition instead of walking the body *)
      (* the entry takes any arguments: [Invalid_argument] for a misuse of
         the API is an answer, not a failure *)
}

type verdict = Proved | Refuted of string | Unknown of string
type result = { property : string; verdict : verdict; seconds : float }

type query = {
  ctx : Symex.ctx;
  entry : entry;
  assumptions : Smt.t list;
  win_start : Smt.t;
  win_size : Smt.t;
}

let int_names inputs =
  List.filter_map
    (function Symex.Int_input x -> Some x | Symex.Buffer_input _ -> None)
    inputs

(* {1 Scripts} *)

(* With [~abstract], each load is a value of its own: that only forgets that
   two loads of one byte agree, so a proof stays a proof, and Z3 has no
   array to reason about. With [~spec], the script holds the encoding of the
   specification and links its input to the window's bytes. *)
let script q ?(abstract = false) ?spec goal ~extra =
  let b = Buffer.create 65536 in
  let ppf = Format.formatter_of_buffer b in
  let define (n, t) =
    Fmt.pf ppf "(define-fun %s () %a %a)@." n Smt.pp_sort (Smt.sort t) Smt.pp t
  in
  let declare n sort =
    Fmt.pf ppf "(declare-const %s %a)@." n Smt.pp_sort sort
  in
  Fmt.pf ppf "(set-option :produce-models true)@.";
  declare "buf" Smt.Bytes;
  declare "buflen" (Smt.Bv 63);
  List.iter (fun x -> declare x (Smt.Bv 63)) (int_names q.entry.inputs);
  List.iter (fun (n, s) -> declare n s) (Symex.declared q.ctx);
  Option.iter (fun (e : Spec.encoding) -> Fmt.pf ppf "%s@." e.prelude) spec;
  let loads = Symex.loads q.ctx in
  List.iter
    (fun (n, t) ->
      if abstract && List.mem n loads then declare n (Smt.sort t)
      else define (n, t))
    (Symex.defs q.ctx);
  define ("win_start", q.win_start);
  define ("win_size", q.win_size);
  for k = 0 to q.entry.bound - 1 do
    Fmt.pf ppf
      "(define-fun byte_%d () (_ BitVec 8) (select buf (bvadd win_start (_ \
       bv%d 64))))@."
      k k
  done;
  List.iter define extra;
  List.iter (fun t -> Fmt.pf ppf "(assert %a)@." Smt.pp t) q.assumptions;
  Fmt.pf ppf "(assert %a)@." Smt.pp goal;
  Fmt.pf ppf "(check-sat)@.";
  let names =
    [ "buflen" ] @ int_names q.entry.inputs
    @ [ "win_start"; "win_size" ]
    @ List.map fst extra
    @ List.init q.entry.bound (Fmt.str "byte_%d")
  in
  Fmt.pf ppf "(get-value (%s))@." (String.concat " " names);
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* {1 Counterexamples} *)

let wide = [ "win_start"; "win_size"; "offset" ]

let show_values ~bound values =
  let signed n =
    match List.assoc_opt n values with
    | Some v ->
        Option.map (Smt.signed (if List.mem n wide then 64 else 63)) (Z3.bits v)
    | None -> None
  in
  let scalars =
    List.filter_map
      (fun (n, v) ->
        if String.starts_with ~prefix:"byte_" n then None
        else
          match v with
          | Z3.Atom (("true" | "false") as b) ->
              Fmt.kstr Option.some "%s = %s" n b
          | _ -> Option.map (fun v -> Fmt.str "%s = %Ld" n v) (signed n))
      values
  in
  let size =
    match signed "win_size" with
    | Some s when Int64.compare s 0L >= 0 -> min (Int64.to_int s) bound
    | _ -> 0
  in
  let bytes =
    List.init size (fun k ->
        match
          Option.bind (List.assoc_opt (Fmt.str "byte_%d" k) values) Z3.bits
        with
        | Some x -> Fmt.str "%02Lx" x
        | None -> "??")
  in
  Fmt.str "%s; window bytes [%s]"
    (String.concat ", " scalars)
    (String.concat " " bytes)

(* {1 Jobs} *)

type job = {
  property : string;
  scripts : string list; (* encodings of one query; the first answer wins *)
  names : string list; (* the encodings' names, when there are several *)
  confirm : string option;
      (* the query on the bytes, when [scripts] abstract the loads *)
  bound : int;
  witness : bool; (* the job shows the goal satisfiable, not unsat *)
  linear : (string * string) option;
      (* the query in linear integer arithmetic, and its side conditions on
         bit-vectors, tried when [scripts] give no answer (see [Linear]) *)
}

let bounds_job q ?(witness = false) ?linear ~property ?(extra = []) goal =
  let linear =
    Option.bind linear (fun (pc, cond) ->
        Option.map
          (fun (ints, sides) ->
            ( ints,
              script q ~abstract:true
                (Smt.and_ [ pc; Smt.not_ sides ])
                ~extra:[] ))
          (Linear.query ~defs:(Symex.defs q.ctx) ~pc ~cond))
  in
  {
    property;
    scripts = [ script q ~abstract:true goal ~extra ];
    names = [];
    confirm = Some (script q goal ~extra);
    bound = q.entry.bound;
    witness;
    linear;
  }

let spec_job q ~property ~extra goals =
  {
    property;
    scripts = List.map (fun (e, goal) -> script q ~spec:e goal ~extra) goals;
    names = List.map (fun ((e : Spec.encoding), _) -> e.name) goals;
    confirm = None;
    bound = q.entry.bound;
    witness = false;
    linear = None;
  }

let verdict j (answer, file, _, _) =
  match answer with
  | Z3.Unsat when j.witness -> Refuted "no input satisfies it"
  | Z3.Sat _ when j.witness -> Proved
  | Z3.Unsat -> Proved
  | Z3.Sat values -> Refuted (show_values ~bound:j.bound values)
  | Z3.Unknown out when String.equal file "" -> Unknown out
  | Z3.Unknown out ->
      Fmt.kstr (fun s -> Unknown s) "%s (script %s)" (String.trim out) file

(* A query with no answer is tried again in linear arithmetic: proved when
   the integer query and its side conditions are both unsat. *)
let retry_linear check verdicts =
  let retry =
    List.filter_map
      (fun (j, (r : result)) ->
        match (r.verdict, j.linear) with
        | Unknown _, Some (ints, sides) -> Some (j, ints, sides)
        | _ -> None)
      verdicts
  in
  let answers =
    check
      (List.concat_map (fun (_, ints, sides) -> [ [ ints ]; [ sides ] ]) retry)
  in
  let rec pair = function
    | (j, _, _) :: rest, (a, _, s1, _) :: (b, _, s2, _) :: answers ->
        (j, (a, b, s1 +. s2)) :: pair (rest, answers)
    | _ -> []
  in
  let linear = pair (retry, answers) in
  List.map
    (fun (j, (r : result)) ->
      match List.assq_opt j linear with
      | Some (Z3.Unsat, Z3.Unsat, s) ->
          {
            property = r.property ^ ", in linear arithmetic";
            verdict = Proved;
            seconds = r.seconds +. s;
          }
      | _ -> r)
    verdicts

(* A refutation found with abstract loads is searched again on the bytes,
   which gives coherent bytes to show, or says that it needs two loads of
   one byte to disagree. *)
let solve_all ?(stop = fun _ _ -> false) ~z3 ~timeout ~jobs ~keep js =
  let check = Z3.check_all ~z3 ~timeout ~jobs ~keep in
  let js_a = Array.of_list js in
  let stop g answer =
    let j = js_a.(g) in
    match verdict j (answer, "", 0., 0) with
    | Refuted _ -> stop j.property true
    | _ -> false
  in
  let first = check ~stop (List.map (fun j -> j.scripts) js) in
  let again =
    List.filter_map
      (fun (j, a) ->
        match (verdict j a, j.confirm) with
        | Refuted _, Some s when not j.witness -> Some (j, [ s ])
        | _ -> None)
      (List.combine js first)
  in
  let confirmed =
    List.combine (List.map fst again) (check (List.map snd again))
  in
  let verdicts =
    List.map2
      (fun j ((answer, _, seconds, alt) as a) ->
        let verdict =
          match List.assq_opt j confirmed with
          | Some (Z3.Unsat, _, _, _) ->
              Unknown "refuted only where two loads of one byte disagree"
          | Some c -> verdict j c
          | None -> verdict j a
        in
        let property =
          match (answer, List.nth_opt j.names alt) with
          | (Z3.Sat _ | Z3.Unsat), Some name when List.length j.names > 1 ->
              Fmt.str "%s, on %s" j.property name
          | _ -> j.property
        in
        (j, { property; verdict; seconds }))
      js first
  in
  retry_linear check verdicts

(* {1 Properties} *)

let window_ok q =
  let zero = Smt.bv 64 0L in
  Smt.and_
    [
      Smt.sge q.win_start zero;
      Smt.sge q.win_size zero;
      Smt.sle q.win_start (Smt.sub (Smt.sign_extend 1 Symex.buflen) q.win_size);
    ]

let answer_of entry = function
  | Symex.Returned (Symex.Bool b) -> b
  | Symex.Returned (Symex.Answer b) -> Smt.bool b
  | Symex.Returned v ->
      Fmt.failwith "%s returns %s, not a bool" entry.name (Symex.type_name v)
  | Symex.Raised _ -> Smt.False

(* One query per path: the paths split the inputs, so the answer agrees with
   the specification everywhere iff it does on each path. Each query is
   stated in every encoding of the specification, whose first [bound] input
   bytes are the window's. With [~any_size], the window may be longer than
   that: the specification's later bytes are then free, which only adds
   models, so an unsat answer covers windows of every size. *)
let equivalence q ~any_size ~encodings ~parser outcomes =
  let entry = q.entry in
  let bounded =
    if any_size then Smt.True
    else Smt.sle q.win_size (Smt.bv 64 (Int64.of_int entry.bound))
  in
  let sizes =
    if any_size then Fmt.str "windows of any size, %d bytes linked" entry.bound
    else Fmt.str "windows of 0 to %d bytes" entry.bound
  in
  List.mapi
    (fun i (pc, o) ->
      let answer = answer_of entry o in
      let goal (e : Spec.encoding) =
        let raw s = Smt.Raw (s, Smt.Bool) in
        let expected =
          Smt.and_ [ window_ok q; raw (e.accepts ~parser ~size:"win_size") ]
        in
        let links =
          List.init entry.bound (fun k ->
              raw (e.link ~index:k ~byte:(Fmt.str "byte_%d" k)))
        in
        Smt.and_
          ((pc :: bounded :: links) @ [ Smt.not_ (Smt.eq answer expected) ])
      in
      spec_job q
        ~property:(Fmt.str "path %d answers as %s, %s" i parser sizes)
        ~extra:[ ("ocaml_accepts", answer) ]
        (List.map (fun e -> (e, goal e)) encodings))
    outcomes

(* The query of [entry]: its window and precondition, evaluated in [ctx],
   and its obligations and outcomes. *)
let walk ctx (entry : entry) =
  List.iter (fun (f, pre) -> Symex.add_contract ctx f pre) entry.contracts;
  let env = Symex.entry_env entry.inputs in
  let start, size = entry.window in
  let win_start = Smt.sign_extend 1 (Symex.term ctx env start)
  and win_size = Smt.sign_extend 1 (Symex.term ctx env size) in
  let pre = Symex.formula ctx env entry.pre in
  Symex.set_window ctx
    (Some (Smt.extract 62 0 win_start, Smt.extract 62 0 win_size));
  let obligations, outcomes = Symex.run ctx env entry.expr in
  Symex.set_window ctx None;
  let assumptions =
    [
      Smt.sge Symex.buflen (Smt.int63 0);
      Smt.sle Symex.buflen Symex.max_string_length;
      pre;
    ]
  in
  ({ ctx; entry; assumptions; win_start; win_size }, obligations, outcomes)

let no_raise_property = "no input raises"

(* That the precondition is satisfiable, that no path raises, and that each
   obligation holds. *)
let bounds_jobs q obligations raised =
  let raised =
    if q.entry.misuse then
      List.filter
        (fun (_, msg) -> not (String.starts_with ~prefix:"invalid_arg" msg))
        raised
    else raised
  in
  let satisfiable =
    bounds_job q ~witness:true ~property:"the precondition holds of some input"
      Smt.True
  in
  let no_raise =
    bounds_job q ~property:no_raise_property (Smt.or_ (List.map fst raised))
  in
  let reads =
    List.map
      (fun (o : Symex.obligation) ->
        bounds_job q ~linear:(o.pc, o.cond)
          ~property:
            (Fmt.str "%s (%a)" (Symex.describe_kind o.kind) Symex.pp_loc o.loc)
          ~extra:
            (match o.offset with Some t -> [ ("offset", t) ] | None -> [])
          (Smt.and_ [ o.pc; Smt.not_ o.cond ]))
      obligations
  in
  satisfiable :: no_raise :: reads

let run ?stop ?(spec_only = false) ~z3 ~timeout ~jobs ~keep ~bounds_only
    ~any_size ~encodings ctx (entry : entry) =
  let q, obligations, outcomes = walk ctx entry in
  let raised =
    List.filter_map
      (function
        | pc, Symex.Raised msg -> Some (pc, msg) | _, Symex.Returned _ -> None)
      outcomes
  in
  let equivalence =
    match entry.spec with
    | Some parser when not bounds_only ->
        equivalence q ~any_size ~encodings ~parser outcomes
    | _ -> []
  in
  let results =
    solve_all ?stop ~z3 ~timeout ~jobs ~keep
      ((if spec_only then [] else bounds_jobs q obligations raised)
      @ equivalence)
  in
  let raises = String.concat " | " (List.map snd raised) in
  let results =
    List.map
      (fun (r : result) ->
        match r.verdict with
        | Refuted cex when String.equal r.property no_raise_property ->
            {
              r with
              verdict =
                Fmt.kstr (fun s -> Refuted s) "%s; raises one of: %s" cex raises;
            }
        | _ -> r)
      results
  in
  (results, List.length outcomes)

(* {1 Checks} *)

type class_ =
  | Safety of string * string
    (* without the check, the property refuted, and its counterexample *)
  | Semantics of string * string
  | Redundant
  | Redundant_for_safety (* no specification to compare the answers with *)
  | Undecided of string

let refuted results =
  List.find_map
    (fun r ->
      match r.verdict with Refuted cex -> Some (r.property, cex) | _ -> None)
    results

let undecided results =
  List.find_map
    (fun r -> match r.verdict with Unknown why -> Some why | _ -> None)
    results

(* Whether an entry that takes any arguments raises [Invalid_argument]: the
   answer of its argument checks. *)
let misuse_of _ = function
  | Symex.Raised msg -> Smt.bool (String.starts_with ~prefix:"invalid_arg" msg)
  | Symex.Returned _ -> Smt.False

let accepts_of answer entry outcomes =
  Smt.or_ (List.map (fun (pc, o) -> Smt.and_ [ pc; answer entry o ]) outcomes)

(* Whether the walk of [entry] with the check [id] disabled answers as the
   walk with every check does, on every input: one query per path of the
   former, against the whole answer of the latter. Both walks read the
   window only, so the answer holds for every length. *)
let same_answers ?stop ~answer ~z3 ~timeout ~jobs ctx (entry : entry) id =
  let q, _, original = walk ctx entry in
  Symex.set_disabled ctx (Some id);
  let _, _, modified = walk ctx entry in
  Symex.set_disabled ctx None;
  let before = accepts_of answer entry original in
  let jobs_ =
    List.mapi
      (fun i (pc, o) ->
        let after = answer entry o in
        let goal = Smt.and_ [ pc; Smt.not_ (Smt.eq after before) ] in
        {
          property =
            Fmt.str "path %d of the code without it answers as the code" i;
          scripts =
            [
              script q goal
                ~extra:[ ("with_check", before); ("without_check", after) ];
            ];
          names = [];
          confirm = None;
          bound = q.entry.bound;
          witness = false;
          linear = None;
        })
      modified
  in
  solve_all ?stop ~z3 ~timeout ~jobs ~keep:false jobs_

(* [classify ~walk entry id] disables the check [id] and asks whether every
   read stays in the buffer and in the window and no input raises, then
   whether every answer stays the answer of the code with the check. The
   code with every check answers as the specification (the equivalence
   lane proves it), so an unchanged answer is the specification's. The
   first refutation decides. *)
let classify ~z3 ~timeout ~jobs ~walk (entry : entry) (check : Symex.check) =
  let id = check.id in
  let stop _ _ = true in
  (* A loop invariant the code without the check does not keep fails the
     proof, not the code: only a read or a raise is a refutation. *)
  let is_access p = not (String.starts_with ~prefix:"the loop keeps" p) in
  let ctx = walk () in
  Symex.set_disabled ctx (Some id);
  let bounds =
    fst
      (run
         ~stop:(fun p r -> r && is_access p)
         ~z3 ~timeout ~jobs ~keep:false ~bounds_only:true ~any_size:false
         ~encodings:[] ctx entry)
  in
  let access = List.filter (fun (r : result) -> is_access r.property) bounds in
  let invariant =
    List.filter (fun (r : result) -> not (is_access r.property)) bounds
  in
  let answer =
    match entry.spec with
    | Some _ -> Some answer_of
    | None when entry.misuse && check.check_kind = Argument -> Some misuse_of
    | None -> None
  in
  (* The answers are compared even where the invariant fails: an argument
     check answers before the loop, so an input it raises on and the code
     without it does not is a counterexample whatever the loop does. *)
  let answers () =
    Option.map
      (fun answer ->
        same_answers ~stop ~answer ~z3 ~timeout ~jobs (walk ()) entry id)
      answer
  in
  match (refuted access, undecided access, refuted invariant) with
  | Some (p, cex), _, _ -> Safety (p, cex)
  | None, Some why, _ -> Undecided why
  | None, None, Some (p, cex) -> (
      match Option.bind (answers ()) refuted with
      | Some (p, cex) -> Semantics (p, cex)
      | None ->
          Fmt.kstr
            (fun s -> Undecided s)
            "no read is refuted, but %s fails: %s" p cex)
  | None, None, None -> (
      match answers () with
      | None -> Redundant_for_safety
      | Some answers -> (
          match (refuted answers, undecided answers) with
          | Some (p, cex), _ -> Semantics (p, cex)
          | None, Some why -> Undecided why
          | None, None -> Redundant))

(* Whether [entry] answers as [reference], an expression over the same
   inputs, on every input: one query per path of [entry], against the whole
   answer of [reference]. Both read the window only, so the answer holds for
   every length. *)
let same_as ~z3 ~timeout ~jobs ctx (entry : entry) reference =
  let q, _, mine = walk ctx entry in
  let _, _, theirs = walk ctx { entry with expr = reference } in
  let before = accepts_of answer_of entry theirs in
  solve_all ~z3 ~timeout ~jobs ~keep:false
    (List.mapi
       (fun i (pc, o) ->
         let after = answer_of entry o in
         {
           property = Fmt.str "path %d answers as the reference" i;
           scripts =
             [
               script q
                 (Smt.and_ [ pc; Smt.not_ (Smt.eq after before) ])
                 ~extra:[ ("reference", before); ("this", after) ];
             ];
           names = [];
           confirm = None;
           bound = q.entry.bound;
           witness = false;
           linear = None;
         })
       mine)
