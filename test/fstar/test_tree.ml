(* The F* the walks of a fold with a visitor are proved in ([<M>.Step.fst]
   of [Wire_ocaml.fstar] and the walks of a frame) and the OCaml that runs
   ([Wire_ocaml.to_ml]) are one tree up to the token table below. Both texts are read by the OCaml parser,
   since the bodies of the F* functions keep to the F* that is also OCaml,
   into [tree], the OCaml side through the table, with in place each
   function the OCaml cuts out of the walk; the trees must be equal. The
   OCaml fold calls its arguments [f] and [invalid] where the F* calls the
   visitor [S] by name, [f] with each field labelled and in its carrier:
   both calls are read as [S.valid] and [S.invalid] of the outputs they
   hand, in order ([handed]), and [f]'s labels are checked against the
   paths of the fold. *)

open Asttypes
open Parsetree

(* {1 The token table}

   Each name the OCaml scanner calls, as the primitive of [WireGen.OCaml]
   the F* calls instead. Besides the names: an [int64] literal [nL] is the
   F* literal [n], an ascription [(e : int)] is [e], and a labelled argument
   is positional, where the labels are those the callee declares, in order
   ([labels] checks). *)

let infix =
  [
    ("+", "add");
    ("-", "sub");
    ("*", "mul");
    ("land", "land");
    ("lsr", "lsr");
    ("lsl", "lsl");
    ("<", "lt");
    ("<=", "le");
    (">", "gt");
    (">=", "ge");
    ("=", "eq");
    ("<>", "ne");
  ]

let qualified =
  [
    ("Int64.to_int", "to_int");
    ("Int64.shift_right_logical", "shift_right_logical");
    ("Int32.to_int", "int32_to_int");
    ("Int64.of_int", "int64_of_int");
    ("Int64.of_int32", "int64_of_int32");
    ("Int64.add", "int64_add");
    ("Int64.sub", "int64_sub");
    ("Int64.mul", "int64_mul");
    ("Int64.logand", "int64_logand");
    ("Int64.shift_left", "shift_left");
    ("Bytes.length", "length");
    (* OxCaml's [int64_u], an [int64] pattern in F*. *)
    ("U.add", "int64_add");
    ("U.sub", "int64_sub");
    ("U.mul", "int64_mul");
    ("U.logand", "int64_logand");
    ("U.logor", "int64_logor");
    ("U.shift_right_logical", "shift_right_logical");
    ("U.shift_left", "shift_left");
    ("U.of_int", "int64_of_int");
    ("U.of_int32_u", "int64_of_int32");
    ("U.to_int", "to_int");
    ("U.to_int64", "to_int64");
    ("B.offset", "offset");
  ]

(* The unchecked loads, [Wire_runtime.Unchecked.u16be] and the others, keep
   their name. *)
let loads = "Wire_runtime.Unchecked."

(* Those of OxCaml, [L.u16le_at] and the others, too. *)
let ox_loads = "L."

(* What the two texts write alike: the step functions call their module
   [S] by name in both. *)
let common =
  [ "&&"; "||"; "not"; "()"; "true"; "false"; "S.invalid"; "S.valid" ]

(* A comparison of two [int64], whose first operand the OCaml ascribes
   [(a : int64)]: [P.int64_lt] and the others. *)
let comparisons = [ "<"; "<="; ">"; ">="; "="; "<>" ]

(* {1 The trees} *)

type tree =
  | Id of string
  | Int of string
  | App of tree * tree list
  | Let of string * tree * tree
  | Fun of string * string list * tree * tree
  | If of tree * tree * tree option

let rec pp ppf = function
  | Id x | Int x -> Fmt.string ppf x
  | App (f, args) ->
      Fmt.pf ppf "@[<2>(%a@ %a)@]" pp f Fmt.(list ~sep:sp pp) args
  | Let (x, e, body) -> Fmt.pf ppf "@[<v>let %s = %a in@ %a@]" x pp e pp body
  | Fun (f, params, e, body) ->
      Fmt.pf ppf "@[<v>let %s %a = %a in@ %a@]" f
        Fmt.(list ~sep:sp string)
        params pp e pp body
  | If (c, a, None) -> Fmt.pf ppf "@[<2>if %a@ then %a@]" pp c pp a
  | If (c, a, Some b) ->
      Fmt.pf ppf "@[<v>@[<2>if %a@ then %a@]@ else %a@]" pp c pp a pp b

exception Outside of string

let fail_outside fmt = Fmt.kstr (fun s -> raise (Outside s)) fmt
let operators = [ "land"; "lor"; "lxor"; "lsl"; "lsr"; "asr"; "mod"; "or" ]

(* Whether [x] is a variable, not an operator. *)
let is_variable x =
  x <> ""
  && (match x.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
       x
  && not (List.mem x operators)

let suffix ~prefix x =
  if String.starts_with ~prefix x then
    Some
      (String.sub x (String.length prefix)
         (String.length x - String.length prefix))
  else None

let ocaml_name x =
  match
    (List.assoc_opt x infix, List.assoc_opt x qualified, suffix ~prefix:loads x)
  with
  | Some f, _, _ | None, Some f, _ -> "P." ^ f
  | None, None, Some f when is_variable f -> "P." ^ f
  | None, None, None when Option.is_some (suffix ~prefix:ox_loads x) ->
      "P." ^ Option.get (suffix ~prefix:ox_loads x)
  | _ ->
      if List.mem x common || is_variable x then x
      else fail_outside "the OCaml name %s, which the table does not map" x

let fstar_name x =
  match suffix ~prefix:"P." x with
  | Some f when is_variable f || List.exists (fun (_, g) -> g = f) infix -> x
  | _ ->
      if List.mem x common || is_variable x then x
      else fail_outside "the F* name %s, fail_outside the subset" x

(* Where [s] holds [sub], and [s] with the first [from], or [all], as
   [into]. *)
let find s sub =
  let n = String.length sub in
  let rec go i =
    if i + n > String.length s then None
    else if String.sub s i n = sub then Some i
    else go (i + 1)
  in
  go 0

let contains s sub = Option.is_some (find s sub)

let rec replace ?(all = false) ~from ~into s =
  match find s from with
  | None -> s
  | Some k ->
      let n = String.length from in
      let rest = String.sub s (k + n) (String.length s - k - n) in
      String.sub s 0 k ^ into
      ^ if all then replace ~all ~from ~into rest else rest

(* [P.land], [P.lsr] and [P.lsl] are F* primitives, but [land], [lsr] and
   [lsl] OCaml keywords: the F* is read with them as raw identifiers,
   [P.\#land]. *)
let raw text =
  List.fold_left
    (fun text (_, f) ->
      if is_variable f then text
      else
        replace ~all:true ~from:("P." ^ f ^ " ") ~into:("P.\\#" ^ f ^ " ") text)
    text infix

(* An integer literal as the decimal of its 64-bit pattern: an [int64] is
   one in F*, which a negative literal of the OCaml is written as. *)
let digits s =
  let s = String.concat "" (String.split_on_char '_' s) in
  match Int64.of_string_opt s with
  | Some v -> Fmt.kstr (fun s -> Int s) "%Lu" v
  | None -> (
      match Int64.of_string_opt ("0u" ^ s) with
      | Some v -> Fmt.kstr (fun s -> Int s) "%Lu" v
      | None -> fail_outside "the literal %s" s)

(* The OxCaml text as the stock parser reads it: an unboxed literal [#5L] as
   [5L], a [local] mode dropped. Neither changes the tree. *)
let ox_normalise text =
  let text = replace ~all:true ~from:" @ local" ~into:"" text in
  let b = Buffer.create (String.length text) in
  String.iteri
    (fun i c ->
      let digit_next =
        i + 1 < String.length text
        && match text.[i + 1] with '0' .. '9' -> true | _ -> false
      in
      if not (c = '#' && digit_next) then Buffer.add_char b c)
    text;
  Buffer.contents b

(* How a side reads its text, and each application it makes of a named
   function, with its labels. *)
type side = { ocaml : bool; calls : (string * arg_label list) list ref }

let ident txt = String.concat "." (Longident.flatten txt)

let rec spine e args =
  match e.pexp_desc with
  | Pexp_apply (f, more) -> spine f (more @ args)
  | _ -> (e, args)

let rec param_name p =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | Ppat_any -> "_"
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> "()"
  | Ppat_constraint (p, _) -> param_name p
  | _ -> fail_outside "the parameter %a" Pprintast.pattern p

(* [U.compare a b op 0], an [int64_u] comparison, as [P.int64_op a b], and
   [B.unsigned_compare a b > 0] as [P.int64_unsigned_gt a b]. *)
let ox_comparison e =
  match e.pexp_desc with
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = op; _ }; _ },
        [
          ( Nolabel,
            {
              pexp_desc =
                Pexp_apply
                  ( { pexp_desc = Pexp_ident { txt = cmp; _ }; _ },
                    [ (Nolabel, a); (Nolabel, b) ] );
              _;
            } );
          ( Nolabel,
            {
              pexp_desc =
                Pexp_constant { pconst_desc = Pconst_integer ("0", None); _ };
              _;
            } );
        ] ) -> (
      match (ident cmp, ident op) with
      | "U.compare", op when List.mem op comparisons ->
          Some ("P.int64_" ^ List.assoc op infix, a, b)
      | "B.unsigned_compare", ">" -> Some ("P.int64_unsigned_gt", a, b)
      | _ -> None)
  | _ -> None

(* OxCaml's [[@nontail]], a hint to its compiler, is outside the tree. *)
let hints side e =
  List.filter
    (fun a -> not (side.ocaml && a.attr_name.txt = "nontail"))
    e.pexp_attributes

(* The continuations a fold takes as its arguments [f] and [invalid], which
   the F* calls as its visitor [S] by name: an argument [~f] or [~invalid]
   passed on is outside the tree. *)
let continuation = function
  | ( Labelled ("f" | "invalid"),
      { pexp_desc = Pexp_ident { txt = Lident _; _ }; _ } ) ->
      true
  | _ -> false

(* What the OCaml hands [f] for an output and the F* hands [S.valid]: the
   variables it reads, in order. The carrier conversions and the options of
   the OCaml, [Some], [None] and [if p then Some v else None], are not in the
   F*, which hands the presence [p] and the value [v] as outputs of their
   own; nor is the F*'s [P.to_int64] of a value held as [int64_u]. A presence
   the walk knows is a literal in the F*, which the OCaml folds into the
   option: it is left out of the F*, and the value of an absent field, which
   the F* hands as [0], is the OCaml's [None]. *)
let conversions = [ "Int64.to_int"; "U.to_int"; "P.to_int64"; "cast" ]

let rec outputs_of tree side e =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "Some"; _ }, Some x) ->
      outputs_of tree side x
  | Pexp_construct ({ txt = Lident "None"; _ }, None) -> [ Int "0" ]
  | Pexp_ifthenelse
      ( c,
        { pexp_desc = Pexp_construct ({ txt = Lident "Some"; _ }, Some x); _ },
        Some
          { pexp_desc = Pexp_construct ({ txt = Lident "None"; _ }, None); _ }
      ) ->
      tree side c :: outputs_of tree side x
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt; _ }; _ }, args)
    when let f = ident txt in
         List.mem f conversions || String.ends_with ~suffix:".unsafe_of_int" f
    ->
      outputs_of tree side (snd (List.nth args (List.length args - 1)))
  | _ -> [ tree side e ]

(* The outputs of a call of [f], each variable once, where the first field
   that reads it hands it, as [S.valid] takes its outputs in their order;
   [()] for none. *)
let handed tree side fields =
  let seen = Hashtbl.create 8 in
  let keep = function
    | Id x when is_variable x ->
        if Hashtbl.mem seen x then false
        else (
          Hashtbl.replace seen x ();
          true)
    | _ -> true
  in
  match List.filter keep (List.concat_map (outputs_of tree side) fields) with
  | [] -> [ Id "()" ]
  | outputs -> outputs

let rec tree side e =
  if hints side e <> [] then
    fail_outside "the attribute of %a" Pprintast.expression e;
  match e.pexp_desc with
  | Pexp_apply _ when side.ocaml && Option.is_some (ox_comparison e) ->
      let f, a, b = Option.get (ox_comparison e) in
      App (Id f, [ tree side a; tree side b ])
  | Pexp_ident { txt; _ } ->
      Id ((if side.ocaml then ocaml_name else fstar_name) (ident txt))
  | Pexp_constant { pconst_desc = Pconst_integer (s, None); _ } -> digits s
  | Pexp_constant { pconst_desc = Pconst_integer (s, Some 'L'); _ }
    when side.ocaml ->
      digits s
  | Pexp_construct ({ txt = Lident c; _ }, None) when List.mem c common -> Id c
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident op; _ }; _ },
        [
          ( Nolabel,
            {
              pexp_desc =
                Pexp_constraint
                  ( a,
                    {
                      ptyp_desc = Ptyp_constr ({ txt = Lident "int64"; _ }, []);
                      _;
                    } );
              _;
            } );
          (Nolabel, b);
        ] )
    when side.ocaml && List.mem op comparisons ->
      App (Id ("P.int64_" ^ List.assoc op infix), [ tree side a; tree side b ])
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident "f"; _ }; _ },
        (Nolabel, acc) :: fields )
    when side.ocaml ->
      side.calls := ("f", Nolabel :: List.map fst fields) :: !(side.calls);
      App (Id "S.valid", tree side acc :: handed tree side (List.map snd fields))
  | Pexp_apply
      ( { pexp_desc = Pexp_ident { txt = Lident "invalid"; _ }; _ },
        [ (Nolabel, acc) ] )
    when side.ocaml ->
      App (Id "S.invalid", [ tree side acc ])
  | Pexp_apply ({ pexp_desc = Pexp_ident { txt; _ }; _ }, args)
    when (not side.ocaml) && ident txt = "S.valid" -> (
      side.calls := ("S.valid", List.map fst args) :: !(side.calls);
      match args with
      | (_, acc) :: outputs ->
          let settled = function
            | {
                pexp_desc =
                  Pexp_construct ({ txt = Lident ("true" | "false"); _ }, None);
                _;
              } ->
                true
            | _ -> false
          in
          App
            ( Id "S.valid",
              tree side acc
              :: handed tree side
                   (List.filter
                      (fun e -> not (settled e))
                      (List.map snd outputs)) )
      | [] -> fail_outside "S.valid applied to nothing")
  | Pexp_apply _ ->
      let f, args = spine e [] in
      let args = List.filter (fun a -> not (continuation a)) args in
      (match f.pexp_desc with
      | Pexp_ident { txt; _ } ->
          side.calls := (ident txt, List.map fst args) :: !(side.calls)
      | _ -> fail_outside "the application of %a" Pprintast.expression f);
      if (not side.ocaml) && List.exists (fun (l, _) -> l <> Nolabel) args then
        fail_outside "the labelled argument of %a" Pprintast.expression e;
      App (tree side f, List.map (fun (_, a) -> tree side a) args)
  | Pexp_let
      ( Nonrecursive,
        [
          {
            pvb_pat =
              { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ };
            pvb_expr =
              { pexp_desc = Pexp_function (params, None, Pfunction_body e); _ };
            pvb_constraint = None;
            pvb_attributes;
            _;
          };
        ],
        body )
    when List.for_all
           (fun a -> side.ocaml && a.attr_name.txt = "local")
           pvb_attributes ->
      let param p =
        match p.pparam_desc with
        | Pparam_val (Nolabel, None, pat) -> param_name pat
        | _ -> fail_outside "a parameter of the local function %s" txt
      in
      Fun (txt, List.map param params, tree side e, tree side body)
  | Pexp_let
      ( Nonrecursive,
        [
          {
            pvb_pat =
              { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ };
            pvb_expr;
            pvb_constraint = None;
            pvb_attributes = [];
            _;
          };
        ],
        body ) ->
      Let (txt, tree side pvb_expr, tree side body)
  | Pexp_ifthenelse (c, a, b) ->
      If (tree side c, tree side a, Option.map (tree side) b)
  | Pexp_constraint
      (inner, { ptyp_desc = Ptyp_constr ({ txt = Lident "int"; _ }, []); _ })
    when side.ocaml ->
      tree side inner
  | _ -> fail_outside "%a" Pprintast.expression e

let children = function
  | Id _ | Int _ -> []
  | App (f, args) -> f :: args
  | Let (_, e, body) | Fun (_, _, e, body) -> [ e; body ]
  | If (c, a, b) -> c :: a :: Option.to_list b

let node = function
  | Id x -> `Id x
  | Int x -> `Int x
  | App (_, args) -> `App (List.length args)
  | Let (x, _, _) -> `Let x
  | Fun (f, params, _, _) -> `Fun (f, params)
  | If (_, _, b) -> `If (Option.is_some b)

(* The smallest subtrees where [a] and [b] differ. *)
let rec difference a b =
  if node a <> node b then (a, b)
  else
    match
      List.find_opt
        (fun (x, y) -> x <> y)
        (List.combine (children a) (children b))
    with
    | Some (x, y) -> difference x y
    | None -> (a, b)

let rec names acc = function
  | Id x -> x :: acc
  | Int _ -> acc
  | App (f, args) -> List.fold_left names (names acc f) args
  | Let (_, e, body) | Fun (_, _, e, body) -> names (names acc e) body
  | If (c, a, b) ->
      let acc = names (names acc c) a in
      Option.fold ~none:acc ~some:(names acc) b

(* {1 The functions of each text} *)

(* A top-level definition: its parameters, each with the label OCaml gives
   it, and its body. *)
type def = { name : string; params : (arg_label * string) list; body : tree }

let parse_expression what text =
  try Parse.expression (Lexing.from_string (raw text))
  with (Syntaxerr.Error _ | Lexer.Error _) as e ->
    fail_outside "%s does not parse: %s" what (Printexc.to_string e)

let paragraphs text =
  let flush acc = function [] -> acc | p -> List.rev p :: acc in
  let acc, last =
    List.fold_left
      (fun (acc, p) line ->
        if String.trim line = "" then (flush acc p, []) else (acc, line :: p))
      ([], [])
      (String.split_on_char '\n' text)
  in
  List.rev (flush acc last)

(* The top-level definitions of the OCaml scanner, each read where the walk
   compared reaches it: the others, the frame loop among them, are fail_outside the
   subset. Attributes such as [[@inline always]] are hints to the compiler,
   fail_outside the tree. *)
let ocaml_defs side text =
  let parse text = Parse.implementation (Lexing.from_string text) in
  let structure =
    try parse text
    with (Syntaxerr.Error _ | Lexer.Error _) as e -> (
      (* OxCaml: each definition read on its own, those the stock parser
         does not read left out, as those a walk compared reaches never
         are. *)
      match String.index_opt text '#' with
      | None ->
          fail_outside "the OCaml scanner does not parse: %s"
            (Printexc.to_string e)
      | Some _ ->
          List.concat_map
            (fun lines ->
              try parse (ox_normalise (String.concat "\n" lines))
              with Syntaxerr.Error _ | Lexer.Error _ -> [])
            (paragraphs text))
  in
  List.filter_map
    (fun item ->
      match item.pstr_desc with
      | Pstr_value
          ( Nonrecursive,
            [
              {
                pvb_pat = { ppat_desc = Ppat_var { txt = name; _ }; _ };
                pvb_expr;
                _;
              };
            ] ) ->
          let params, body =
            match pvb_expr.pexp_desc with
            | Pexp_function (params, None, Pfunction_body body) ->
                ( List.filter_map
                    (fun p ->
                      match p.pparam_desc with
                      | Pparam_val (Labelled ("f" | "invalid"), None, _) -> None
                      | Pparam_val (label, None, pat) ->
                          Some (label, param_name pat)
                      | _ -> fail_outside "a parameter of %s" name)
                    params,
                  body )
            | _ -> ([], pvb_expr)
          in
          Some (name, lazy { name; params; body = tree side body })
      | _ -> None)
    structure

(* [fold] and the definitions it reads, in the order of the text: those before
   it, as a definition reads only those before it. The views after it may be
   named as a variable of [fold] is. *)
let reachable ~root defs =
  let rec upto = function
    | [] -> []
    | ((x, _) as d) :: rest -> if x = root then [ d ] else d :: upto rest
  in
  let defs = upto defs in
  let rec close seen = function
    | [] -> seen
    | x :: rest when List.mem x seen || not (List.mem_assoc x defs) ->
        close seen rest
    | x :: rest ->
        let d = Lazy.force (List.assoc x defs) in
        close (x :: seen) (names rest d.body)
  in
  let seen = close [] [ root ] in
  List.filter_map
    (fun (x, d) -> if List.mem x seen then Some (Lazy.force d) else None)
    defs

(* The parameter names of an F* header, from the text after the function's
   name: [(x:t)] is [x], [(#r:Type)] is implicit, [(_:unit)] is [()]. *)
let fstar_params header =
  let n = String.length header in
  let rec go i depth start acc =
    if i >= n then List.rev acc
    else
      match header.[i] with
      | '(' when depth = 0 -> go (i + 1) 1 (i + 1) acc
      | '(' | '{' -> go (i + 1) (depth + 1) start acc
      | '}' -> go (i + 1) (depth - 1) start acc
      | ')' when depth = 1 ->
          let group = String.sub header start (i - start) in
          let acc =
            match String.index_opt group ':' with
            | None -> fail_outside "the F* parameter %s" group
            | Some k -> (
                match String.trim (String.sub group 0 k) with
                | "#r" -> acc
                | "_" -> "()" :: acc
                | x -> x :: acc)
          in
          go (i + 1) 0 start acc
      | ')' -> go (i + 1) (depth - 1) start acc
      | _ -> go (i + 1) depth start acc
  in
  go 0 0 0 []

(* The definitions of an F* walk ([<M>.Step.fst], [<M>.Frames.fst],
   [<M>.Index.fst]): each function, whose header ends in [: S.t =], and each
   constant [let x = e]. The module's other paragraphs are its header. *)
let fstar_defs side text =
  List.filter_map
    (fun lines ->
      let header = List.hd lines in
      let starts prefix = String.starts_with ~prefix header in
      if starts "module " || starts "(*" then None
      else
        let rest =
          match
            (suffix ~prefix:"unfold let " header, suffix ~prefix:"let " header)
          with
          | Some r, _ | None, Some r -> r
          | None, None -> fail_outside "the F* paragraph %S" header
        in
        let name = List.hd (String.split_on_char ' ' rest) in
        let after =
          String.sub rest (String.length name)
            (String.length rest - String.length name)
        in
        match
          (suffix ~prefix:" = " after, String.ends_with ~suffix:" : S.t =" after)
        with
        | Some e, _ when List.tl lines = [] ->
            Some
              { name; params = []; body = tree side (parse_expression name e) }
        | _, true ->
            let body = String.concat "\n" (List.tl lines) in
            Some
              {
                name;
                params = List.map (fun x -> (Nolabel, x)) (fstar_params after);
                body = tree side (parse_expression name body);
              }
        | _ -> fail_outside "the F* definition %S" header)
    (paragraphs text)

(* {1 The comparison} *)

let expect what pp a b =
  if a <> b then fail_outside "%s: the OCaml has %a, the F* %a" what pp a pp b

let same what a b =
  if a <> b then
    let a, b = difference a b in
    fail_outside "%s: the OCaml has %a, the F* %a" what pp a pp b

(* Dropping a label keeps the argument's position: a named function's
   application carries the labels its definition declares, in order, and
   [f]'s those of the fold's fields, in the order of their paths. *)
let pp_labels =
  Fmt.(
    list ~sep:sp (fun ppf -> function
      | Nolabel -> string ppf "_"
      | Labelled l -> pf ppf "~%s" l
      | Optional l -> pf ppf "?%s" l))

let equal_label a b =
  match (a, b) with
  | Nolabel, Nolabel -> true
  | Labelled a, Labelled b | Optional a, Optional b -> String.equal a b
  | _ -> false

let labels ~outputs defs calls =
  let declared f =
    match List.find_opt (fun d -> d.name = f) defs with
    | Some d -> Some (List.map fst d.params)
    | None when f = "f" ->
        Some (Nolabel :: List.map (fun o -> Labelled o) outputs)
    | None -> None
  in
  List.iter
    (fun (f, got) ->
      let want =
        Option.value (declared f) ~default:(List.map (fun _ -> Nolabel) got)
      in
      if not (List.equal equal_label want got) then
        fail_outside "%s is given the labels %a, where it declares %a" f
          pp_labels got pp_labels want)
    calls

(* The identifiers [t] reads that it does not bind. *)
let rec free bound acc = function
  | Id x -> if List.mem x bound then acc else x :: acc
  | Int _ -> acc
  | App (f, args) -> List.fold_left (free bound) (free bound acc f) args
  | Let (x, e, body) -> free (x :: bound) (free bound acc e) body
  | Fun (f, params, e, body) ->
      free (f :: bound) (free (params @ bound) acc e) body
  | If (c, a, b) ->
      let acc = free bound (free bound acc c) a in
      Option.fold ~none:acc ~some:(free bound acc) b

(* A function of the OCaml that the F* writes in place: the cuts the
   emitter makes so that merlint accepts the OCaml, each applied to its
   parameters, which the emitter names alike at the call. Its body read
   in place is the call, as it reads no variable but its parameters and
   the definitions around it. *)
let rec inline ~globals defs = function
  | App (Id f, args) when List.mem_assoc f defs ->
      let d = List.assoc f defs in
      let params = List.map snd d.params in
      if List.map (function Id x -> x | _ -> "") args <> params then
        fail_outside "%s is applied to other than its parameters" f;
      List.iter
        (fun x ->
          if not (List.mem x globals || String.contains x '.') then
            fail_outside "%s reads %s, which is not a parameter" f x)
        (free (params @ common) [] d.body);
      inline ~globals defs d.body
  | (Id _ | Int _) as t -> t
  | App (f, args) ->
      App (inline ~globals defs f, List.map (inline ~globals defs) args)
  | Let (x, e, body) ->
      Let (x, inline ~globals defs e, inline ~globals defs body)
  | Fun (f, params, e, body) ->
      Fun (f, params, inline ~globals defs e, inline ~globals defs body)
  | If (c, a, b) ->
      If
        ( inline ~globals defs c,
          inline ~globals defs a,
          Option.map (inline ~globals defs) b )

(* Whether the OCaml scanner [ocaml] from [root] and the F* of its proof,
   [scan], are one tree; if not, where they differ. *)
let agree ~root ~outputs ~ocaml ~scan =
  match
    let o = { ocaml = true; calls = ref [] } in
    let f = { ocaml = false; calls = ref [] } in
    let all = reachable ~root (ocaml_defs o ocaml) in
    let fdefs = fstar_defs f scan in
    let defined d = List.exists (fun e -> e.name = d.name) fdefs in
    let placed =
      List.filter_map
        (fun d -> if defined d then None else Some (d.name, d))
        all
    in
    let globals = List.map (fun d -> d.name) all in
    let odefs =
      List.filter_map
        (fun d ->
          if defined d then Some { d with body = inline ~globals placed d.body }
          else None)
        all
    in
    expect "the definitions"
      Fmt.(Dump.list string)
      (List.map (fun d -> d.name) odefs)
      (List.map (fun d -> d.name) fdefs);
    List.iter2
      (fun od fd ->
        expect
          ("the parameters of " ^ od.name)
          Fmt.(Dump.list string)
          (List.map snd od.params) (List.map snd fd.params);
        same od.name od.body fd.body)
      odefs fdefs;
    labels ~outputs all !(o.calls)
  with
  | () -> Ok ()
  | exception Outside why -> Error why

(* {1 The codecs} *)

type codec = Codec : string * 'a Wire.Codec.t -> codec

let corpus =
  List.map (fun (Proved.Codec (n, c)) -> Codec (n, c)) Proved.all
  @ [
      Codec ("Packet_stack.codec", Packet_stack.codec);
      Codec ("Headers.tcp", Headers.tcp);
      Codec ("Headers.udp", Headers.udp);
      Codec ("Space.packet", Space.packet_codec);
      Codec ("Space.full_packet", Space.full_packet_codec);
      Codec ("Space.tm_frame", Space.tm_frame_codec);
      Codec ("Space.tm_with_ocf", Space.tm_with_ocf_codec);
      Codec ("Space.inner_cmd", Space.inner_cmd_codec);
      Codec ("Space.outer_hdr", Space.outer_hdr_codec);
      Codec ("Schemas.ipv4", Schemas.ipv4);
      Codec ("Schemas.checked", Schemas.checked);
      Codec ("Schemas.mixed", Schemas.mixed);
      Codec ("Schemas.small", Schemas.small);
      Codec ("Schemas.lanes", Schemas.lanes);
      Codec ("Schemas.eth", Schemas.eth);
      Codec ("Schemas.wide", Schemas.wide);
      Codec ("Schemas.mapped", Schemas.mapped);
      Codec ("Schemas.signed", Schemas.signed);
      Codec ("Schemas.packed", Schemas.packed);
      Codec ("Schemas.refined", Schemas.refined);
      Codec ("Schemas.overflow", Schemas.overflow);
      Codec ("Schemas.sized", Schemas.sized);
      Codec ("Schemas.skipped", Schemas.skipped);
      Codec ("Schemas.huge", Schemas.huge);
      Codec ("Schemas.shared", Schemas.shared);
      Codec ("Schemas.stacked", Schemas.stacked);
    ]
  @ List.map
      (fun (label, Fuzz_gen.Pack g) ->
        Codec ("registry " ^ label, Fuzz_gen.codec g))
      Fuzz_gen.registry
  @ List.map
      (fun (label, Fuzz_gen.Pack g) ->
        Codec ("sample " ^ label, Fuzz_gen.codec g))
      Diff_codecs.included

(* The texts of a codec the F* flavour accepts: the OCaml of its fold
   [fold_step], whose visitor is a module [S], and the F* of its walks; then
   the same of the OxCaml scanner. *)
type walks = {
  ocaml : string;
  step : string;
  frames : string;
  index : string option;
}

type texts = { stock : walks; ox : walks }

(* The path of every integer field of [c], each under the records that hold
   it: the fields of a fold that reads them all. *)
let paths c =
  let rec go prefix (view : Wire_ocaml.Views.t) =
    List.concat_map
      (function
        | Wire_ocaml.Views.Int { name; _ } -> [ prefix ^ name ]
        | Record { name; view; _ } -> go (prefix ^ name ^ ".") view
        | Bool _ | Bytes _ -> [])
      view.members
  in
  go "" (Wire_ocaml.Views.v c)

let texts c =
  match Wire_ocaml.fstar ~codec:"M" c with
  | exception Invalid_argument _ -> None
  | files ->
      let walks target ox =
        let file name = List.assoc_opt ("M." ^ ox ^ name ^ ".fst") files in
        {
          ocaml = Wire_ocaml.(to_ml (v ~target ~fold:(paths c) c));
          step = Option.get (file "Step");
          frames = Option.get (file "Frames");
          index = file "Index";
        }
      in
      Some { stock = walks `OCaml ""; ox = walks `OxCaml "Ox" }

(* The labels [f] takes the fields of [paths c] with, in order. *)
let outputs c = List.map (String.map (function '.' -> '_' | c -> c)) (paths c)

(* Whether the walks of [w] agree with their F*: the step functions and the
   walks of a frame of their loops. *)
let agree_walks ~outputs w =
  let ( let* ) = Result.bind in
  let* () = agree ~root:"fold_step" ~outputs ~ocaml:w.ocaml ~scan:w.step in
  let* () =
    agree ~root:"step_frames_record" ~outputs ~ocaml:w.ocaml ~scan:w.frames
  in
  match w.index with
  | None -> Ok ()
  | Some index ->
      agree ~root:"step_index_record" ~outputs ~ocaml:w.ocaml ~scan:index

(* Whether the walks of both scanners agree with their F*. *)
let agree_both ~outputs t =
  Result.bind (agree_walks ~outputs t.stock) (fun () ->
      Result.map_error (fun why -> "OxCaml: " ^ why) (agree_walks ~outputs t.ox))

(* The codecs of the corpus the F* flavour accepts today: a change that
   makes it refuse some shows here. *)
let at_least = 88

let test_corpus () =
  let compared =
    List.fold_left
      (fun n (Codec (name, c)) ->
        match texts c with
        | None -> n
        | Some t -> (
            match agree_both ~outputs:(outputs c) t with
            | Ok _ -> n + 1
            | Error why -> Alcotest.failf "%s: %s" name why))
      0 corpus
  in
  Fmt.pr "%d codecs compared@." compared;
  if compared < at_least then
    Alcotest.failf "%d codecs compared, expected at least %d" compared at_least

(* The IPv4 scanner with one token of its OCaml changed, in the step
   functions or, with [index], in the walk of a frame of the loop of
   [fold_step_index]; with [ox], of the OxCaml scanner. *)
let mutant ?(ox = false) ?(index = false) ?(all = false) ~from ~into () =
  match texts Headers.ipv4 with
  | None -> Alcotest.fail "the F* flavour refuses IPv4"
  | Some t -> (
      let w = if ox then t.ox else t.stock in
      if not (contains w.ocaml from) then
        Alcotest.failf "no %S in the IPv4 scanner" from;
      let outputs = outputs Headers.ipv4 in
      (match agree_both ~outputs t with
      | Ok () -> ()
      | Error why -> Alcotest.failf "IPv4: %s" why);
      let ocaml = replace ~all ~from ~into w.ocaml in
      let root, scan =
        if index then ("step_index_record", Option.get w.index)
        else ("fold_step", w.step)
      in
      match agree ~root ~outputs ~ocaml ~scan with
      | Ok () -> Alcotest.failf "%S as %S: the trees still agree" from into
      | Error why -> Fmt.pr "%S as %S: %s@." from into why)

let suite =
  ( "tree",
    [
      Alcotest.test_case "corpus" `Quick test_corpus;
      Alcotest.test_case "mutant mask" `Quick
        (mutant ~from:"(Int64.logand bw1 15L)" ~into:"(Int64.logand bw1 7L)");
      Alcotest.test_case "mutant operator" `Quick
        (mutant ~from:"(Int64.shift_right_logical bw1 4)"
           ~into:"(Int64.shift_left bw1 4)");
      Alcotest.test_case "mutant shift" `Quick
        (mutant ~from:"(Int64.shift_right_logical bw1 4)"
           ~into:"(Int64.shift_right_logical bw1 3)");
      Alcotest.test_case "mutant comparison" `Quick
        (mutant ~from:"((bw1 : int64) < 69L)" ~into:"((bw1 : int64) <= 69L)");
      Alcotest.test_case "mutant label" `Quick
        (mutant
           ~from:
             "init ~version:(Int64.to_int f1_version) ~ihl:(Int64.to_int \
              f2_ihl)"
           ~into:
             "init ~ihl:(Int64.to_int f1_version) ~version:(Int64.to_int \
              f2_ihl)");
      Alcotest.test_case "mutant field" `Quick
        (mutant
           ~from:
             "init ~version:(Int64.to_int f1_version) ~ihl:(Int64.to_int \
              f2_ihl)"
           ~into:
             "init ~version:(Int64.to_int f2_ihl) ~ihl:(Int64.to_int \
              f1_version)");
      Alcotest.test_case "OxCaml mutant literal" `Quick
        (mutant ~ox:true ~from:"(U.add base #9L)" ~into:"(U.add base #10L)");
      Alcotest.test_case "OxCaml mutant comparison" `Quick
        (mutant ~ox:true ~from:"(U.compare stop #20L < 0)"
           ~into:"(U.compare stop #20L <= 0)");
      Alcotest.test_case "index mutant label" `Quick
        (mutant ~index:true
           ~from:
             "f current ~version:(Int64.to_int f1_version) ~ihl:(Int64.to_int \
              f2_ihl)"
           ~into:
             "f current ~ihl:(Int64.to_int f1_version) ~version:(Int64.to_int \
              f2_ihl)");
      Alcotest.test_case "OxCaml index mutant shift" `Quick
        (mutant ~ox:true ~index:true ~all:true
           ~from:"let f1_version = (U.shift_right_logical bw1 4)"
           ~into:"let f1_version = (U.shift_right_logical bw1 3)");
      Alcotest.test_case "OxCaml mutant conversion" `Quick
        (mutant ~ox:true ~from:"~version:(U.to_int f1_version)"
           ~into:"~version:(U.to_int f2_ihl)");
    ] )
