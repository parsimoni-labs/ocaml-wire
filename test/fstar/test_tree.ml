(* The F* a scanner's [fold] is proved in ([<M>.Scan.fst] of
   [Wire_ocaml.fstar]) and the OCaml that runs ([Wire_ocaml.scanner]) are one
   tree up to the token table below. Both texts are read by the OCaml parser,
   since the bodies of the F* functions keep to the F* that is also OCaml,
   into [tree], the OCaml side through the table; the trees must be equal. *)

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
    ("Bytes.length", "length");
  ]

(* The unchecked loads, [Wire_runtime.Unchecked.u16be] and the others, keep
   their name. *)
let loads = "Wire_runtime.Unchecked."

(* What the two texts write alike. *)
let common = [ "&&"; "||"; "not"; "()"; "true"; "false" ]

(* {1 The trees} *)

type tree =
  | Id of string
  | Int of string
  | App of tree * tree list
  | Let of string * tree * tree
  | If of tree * tree * tree option

let rec pp ppf = function
  | Id x | Int x -> Fmt.string ppf x
  | App (f, args) ->
      Fmt.pf ppf "@[<2>(%a@ %a)@]" pp f Fmt.(list ~sep:sp pp) args
  | Let (x, e, body) -> Fmt.pf ppf "@[<v>let %s = %a in@ %a@]" x pp e pp body
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

let digits s =
  Int (String.lowercase_ascii (String.concat "" (String.split_on_char '_' s)))

(* How a side reads its text, and each application it makes of a named
   function, with its labels. *)
type side = { ocaml : bool; calls : (string * arg_label list) list ref }

let ident txt = String.concat "." (Longident.flatten txt)

let rec spine e args =
  match e.pexp_desc with
  | Pexp_apply (f, more) -> spine f (more @ args)
  | _ -> (e, args)

let rec tree side e =
  if e.pexp_attributes <> [] then
    fail_outside "the attribute of %a" Pprintast.expression e;
  match e.pexp_desc with
  | Pexp_ident { txt; _ } ->
      Id ((if side.ocaml then ocaml_name else fstar_name) (ident txt))
  | Pexp_constant { pconst_desc = Pconst_integer (s, None); _ } -> digits s
  | Pexp_constant { pconst_desc = Pconst_integer (s, Some 'L'); _ }
    when side.ocaml ->
      digits s
  | Pexp_construct ({ txt = Lident c; _ }, None) when List.mem c common -> Id c
  | Pexp_apply _ ->
      let f, args = spine e [] in
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
  | Let (_, e, body) -> [ e; body ]
  | If (c, a, b) -> c :: a :: Option.to_list b

let node = function
  | Id x -> `Id x
  | Int x -> `Int x
  | App (_, args) -> `App (List.length args)
  | Let (x, _, _) -> `Let x
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

let rec bindings acc = function
  | Id _ | Int _ -> acc
  | App (f, args) -> List.fold_left bindings (bindings acc f) args
  | Let (x, e, body) -> bindings ((x, e) :: bindings acc e) body
  | If (c, a, b) ->
      let acc = bindings (bindings acc c) a in
      Option.fold ~none:acc ~some:(bindings acc) b

let rec names acc = function
  | Id x -> x :: acc
  | Int _ -> acc
  | App (f, args) -> List.fold_left names (names acc f) args
  | Let (_, e, body) -> names (names acc e) body
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

let rec param_name p =
  match p.ppat_desc with
  | Ppat_var { txt; _ } -> txt
  | Ppat_construct ({ txt = Lident "()"; _ }, None) -> "()"
  | Ppat_constraint (p, _) -> param_name p
  | _ -> fail_outside "the parameter %a" Pprintast.pattern p

(* The top-level definitions of the OCaml scanner, each read where [fold]
   reaches it: the others, the frame loop among them, are fail_outside the
   subset. Attributes such as [[@inline always]] are hints to the compiler,
   fail_outside the tree. *)
let ocaml_defs side text =
  let structure =
    try Parse.implementation (Lexing.from_string text)
    with (Syntaxerr.Error _ | Lexer.Error _) as e ->
      fail_outside "the OCaml scanner does not parse: %s" (Printexc.to_string e)
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
                ( List.map
                    (fun p ->
                      match p.pparam_desc with
                      | Pparam_val (label, None, pat) -> (label, param_name pat)
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
let reachable defs =
  let rec upto = function
    | [] -> []
    | ((x, _) as d) :: rest -> if x = "fold" then [ d ] else d :: upto rest
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
  let seen = close [] [ "fold" ] in
  List.filter_map
    (fun (x, d) -> if List.mem x seen then Some (Lazy.force d) else None)
    defs

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

(* The definitions of [<M>.Scan.fst]: each function, whose header ends in
   [: r =], and each constant [let x = e]. The module's other paragraphs
   are its header, the window a cut reads and the type of [valid], which
   the proof states. *)
let fstar_defs side text =
  List.filter_map
    (fun lines ->
      let header = List.hd lines in
      let starts prefix = String.starts_with ~prefix header in
      if
        starts "module " || starts "(*" || starts "let window "
        || starts "type valid_t "
      then None
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
          (suffix ~prefix:" = " after, String.ends_with ~suffix:" : r =" after)
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

(* The bindings [let x = e in] that [<M>.Proof.fst] states facts under. *)
let proof_lets text =
  List.filter_map
    (fun line ->
      match suffix ~prefix:"  let " line with
      | Some rest when String.ends_with ~suffix:" in" rest -> (
          match String.index_opt rest '=' with
          | Some k ->
              let x = String.trim (String.sub rest 0 k) in
              let e = String.sub rest (k + 1) (String.length rest - k - 4) in
              Some (x, e)
          | None -> None)
      | _ -> None)
    (String.split_on_char '\n' text)

(* {1 The comparison} *)

let expect what pp a b =
  if a <> b then fail_outside "%s: the OCaml has %a, the F* %a" what pp a pp b

let same what a b =
  if a <> b then
    let a, b = difference a b in
    fail_outside "%s: the OCaml has %a, the F* %a" what pp a pp b

(* Dropping a label keeps the argument's position: a named function's
   application carries the labels its definition declares, in order, and
   [valid]'s those of the outputs, the scanner's fields in order. *)
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
    | None when f = "valid" ->
        Some
          (if outputs = [] then [ Nolabel ]
           else List.map (fun o -> Labelled o) outputs)
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

(* Whether the OCaml scanner [ocaml] and the F* of its proof, [scan] and
   [proof], are one tree; if not, where they differ. *)
let agree ~outputs ~ocaml ~scan ~proof =
  match
    let o = { ocaml = true; calls = ref [] } in
    let f = { ocaml = false; calls = ref [] } in
    let odefs = reachable (ocaml_defs o ocaml) in
    let fdefs = fstar_defs f scan in
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
    labels ~outputs odefs !(o.calls);
    let scanned = List.concat_map (fun d -> bindings [] d.body) fdefs in
    List.iter
      (fun (x, e) ->
        let e = tree f (parse_expression x e) in
        match List.filter (fun (y, _) -> x = y) scanned with
        | [] -> fail_outside "the fact's binding %s, which the scanner lacks" x
        | bound ->
            List.iter
              (fun (_, rhs) ->
                if rhs <> e then
                  let rhs, e = difference rhs e in
                  fail_outside "the binding %s: Scan has %a, Proof %a" x pp rhs
                    pp e)
              bound)
      (proof_lets proof)
  with
  | () -> Ok ()
  | exception Outside why -> Error why

(* {1 The codecs} *)

type codec = Codec : string * 'a Wire.Codec.t -> codec

let corpus =
  List.map (fun (Proved.Codec (n, c)) -> Codec (n, c)) Proved.all
  @ [
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

(* The texts of a codec the F* flavour accepts. *)
let texts c =
  match Wire_ocaml.fstar ~codec:"M" c with
  | exception Invalid_argument _ -> None
  | files ->
      Some
        ( Wire_ocaml.scanner ~codec:"M" c,
          List.assoc "M.Scan.fst" files,
          List.assoc "M.Proof.fst" files )

let outputs c = List.map fst (Wire_ocaml.scanner_fields c)

(* The codecs of the corpus the F* flavour accepts today: a change that
   makes it refuse some shows here. *)
let at_least = 70

let test_corpus () =
  let compared =
    List.fold_left
      (fun n (Codec (name, c)) ->
        match texts c with
        | None -> n
        | Some (ocaml, scan, proof) -> (
            match agree ~outputs:(outputs c) ~ocaml ~scan ~proof with
            | Ok _ -> n + 1
            | Error why -> Alcotest.failf "%s: %s" name why))
      0 corpus
  in
  Fmt.pr "%d codecs compared@." compared;
  if compared < at_least then
    Alcotest.failf "%d codecs compared, expected at least %d" compared at_least

(* The IPv4 scanner with one token of its OCaml changed. *)
let mutant ~from ~into () =
  match texts Headers.ipv4 with
  | None -> Alcotest.fail "the F* flavour refuses IPv4"
  | Some (ocaml, scan, proof) -> (
      if not (contains ocaml from) then
        Alcotest.failf "no %S in the IPv4 scanner" from;
      let outputs = outputs Headers.ipv4 in
      (match agree ~outputs ~ocaml ~scan ~proof with
      | Ok () -> ()
      | Error why -> Alcotest.failf "IPv4: %s" why);
      let ocaml = replace ~from ~into ocaml in
      match agree ~outputs ~ocaml ~scan ~proof with
      | Ok () -> Alcotest.failf "%S as %S: the trees still agree" from into
      | Error why -> Fmt.pr "%S as %S: %s@." from into why)

let suite =
  ( "tree",
    [
      Alcotest.test_case "corpus" `Quick test_corpus;
      Alcotest.test_case "mutant literal" `Quick
        (mutant ~from:"(bw1 land 0xf)" ~into:"(bw1 land 0x7)");
      Alcotest.test_case "mutant operator" `Quick
        (mutant ~from:"(bw1 lsr 4)" ~into:"(bw1 lsl 4)");
      Alcotest.test_case "mutant label" `Quick
        (mutant ~from:"~version:f1_version ~ihl:f2_ihl"
           ~into:"~ihl:f1_version ~version:f2_ihl");
    ] )
