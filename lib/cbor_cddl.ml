(* The .cddl printer of Cbor (docs/cbor.md, Constructs and their projections).
   EverParse paths are from its root at v2026.11.13: [Lexer.mll] is
   src/cddl/tool/ocaml/evercddl-lib/CDDLLexer.mll, [Parser.ml] its
   CDDLParser.ml, [Gen.fst] src/cddl/tool/CDDL.Tool.Gen.fst and
   [postlude.cddl] lib/everparse/cddl/spec/postlude.cddl. *)

type literal = [ `Int of int | `Text of string ]

type typ =
  | Uint
  | Nint
  | Int
  | Bytes
  | Text
  | Bool
  | Null
  | Any
  | Literal of literal
  | Range of int64 * int64
  | Size of { min : int; max : int; inner : typ }
  | Tag of int * typ
  | Array of { non_empty : bool; elem : typ }
  | Tuple of { name : string; entries : entry list }
  | Assoc of typ * typ
  | Record of { name : string; members : member list }
  | Choice of (string * typ) list
  | Nullable of typ
  | Det_cbor of typ
  | Containing of binary

and entry = { name : string; optional : bool; typ : typ }

and member =
  | Mem of { name : string; key : literal; optional : bool; typ : typ }
  | Exclusive of { name : string; members : (literal * typ) list }
  | Keep_unknown of { name : string; key : typ; value : typ }

and binary = Binary : 'a Types.typ -> binary

(* Names. A rule name is a CDDL id (Lexer.mll [raw_id]) of the characters
   evercddl keeps in a C name, [A-Za-z0-9_] (Gen.fst:5-15), so two rules never
   share a C name: every other run of characters becomes one [_], and a name
   that does not start with a letter or [_] takes a [t_]. *)
let ident s =
  let b = Buffer.create (String.length s) in
  let gap = ref false in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
          if !gap && Buffer.length b > 0 then Buffer.add_char b '_';
          gap := false;
          Buffer.add_char b c
      | _ -> gap := true)
    s;
  match Buffer.contents b with
  | "" -> "t"
  | s -> (
      match s.[0] with 'a' .. 'z' | 'A' .. 'Z' | '_' -> s | _ -> "t_" ^ s)

(* The names postlude.cddl defines, and the two evercddl defines itself
   (postlude.cddl:47-49): a rule of one of them would define it twice. *)
let prelude =
  [
    "tdate";
    "number";
    "eb64url";
    "eb64legacy";
    "eb16";
    "encoded-cbor";
    "uri";
    "b64url";
    "b64legacy";
    "regexp";
    "mime-message";
    "cbor-any";
    "int";
    "uint";
    "nint";
    "text";
    "bytes";
    "bstr";
    "tstr";
    "bool";
    "false";
    "true";
    "null";
    "nil";
    "undefined";
    "any";
    "everparse-no-match";
  ]

(* evercddl names the C type of a rule by its name lower-cased (Gen.fst:27-39),
   so two names are one when they are one lower-cased. *)
let same_name a b =
  String.equal (String.lowercase_ascii a) (String.lowercase_ascii b)

(* [base] if [taken] does not hold it, else the first free [base_2],
   [base_3], ... *)
let fresh ~taken base =
  if not (taken base) then base
  else
    let rec go i =
      let n = Fmt.str "%s_%d" base i in
      if taken n then go (i + 1) else n
    in
    go 2

(* Paths name what has no name of its own: a binary type under a
   [Containing], and a CBOR root a binary record holds. *)
let under path n = path ^ "_" ^ ident n

let pp_literal ppf = function
  | `Int n -> Fmt.int ppf n
  | `Text s -> Fmt.pf ppf "\"%s\"" s

(* The text of a type, and its precedence in RFC 8610's grammar: a [type2]
   is atomic, a [type1] takes a range or a control operator, a [type] is a
   choice. A type is put in parentheses where the grammar needs a tighter
   one: a map key is a [type1], an operand of a control a [type2]
   (Parser.ml:309-327). *)
type level = Type2 | Type1 | Type

let paren ~at (level, s) =
  let rank = function Type2 -> 0 | Type1 -> 1 | Type -> 2 in
  if rank level > rank at then "(" ^ s ^ ")" else s

(* A module being printed: its rules in reverse order, each after the rules
   it names. [body] is what a rule assigns, so a tuple or a record met twice
   is printed once and two different ones of one name get two names; [base]
   is the name before it was made unique. *)
type rule = { rname : string; base : string; body : string }
type printer = { mutable rules : rule list }

let taken p n =
  List.exists (same_name n) prelude
  || List.exists (fun r -> same_name r.rname n) p.rules

let assign p ~base body =
  let base = ident base in
  match
    List.find_opt
      (fun r -> String.equal r.base base && String.equal r.body body)
      p.rules
  with
  | Some r -> r.rname
  | None ->
      let rname = fresh ~taken:(taken p) base in
      p.rules <- { rname; base; body } :: p.rules;
      rname

let rec ty p t : level * string =
  match t with
  | Uint -> (Type2, "uint")
  | Nint -> (Type2, "nint")
  | Int -> (Type2, "int")
  | Bytes -> (Type2, "bstr")
  | Text -> (Type2, "tstr")
  | Bool -> (Type2, "bool")
  | Null -> (Type2, "nil")
  | Any -> (Type2, "any")
  | Containing _ -> (Type2, "bstr")
  | Literal l -> (Type2, Fmt.str "%a" pp_literal l)
  | Range (lo, hi) -> (Type1, Fmt.str "%Ld..%Ld" lo hi)
  | Size { min; max; inner } ->
      let inner = paren ~at:Type2 (ty p inner) in
      if Int.equal min max then (Type1, Fmt.str "%s .size %d" inner min)
      else (Type1, Fmt.str "%s .size (%d..%d)" inner min max)
  | Tag (n, inner) -> (Type2, Fmt.str "#6.%d(%s)" n (snd (ty p inner)))
  | Array { non_empty; elem } ->
      ( Type2,
        Fmt.str "[%s %s]" (if non_empty then "+" else "*") (snd (ty p elem)) )
  | Assoc (k, v) ->
      (Type2, Fmt.str "{ * %s => %s }" (paren ~at:Type1 (ty p k)) (snd (ty p v)))
  | Choice cases ->
      ( Type,
        String.concat " / "
          (List.map (fun (_, t) -> paren ~at:Type1 (ty p t)) cases) )
  | Nullable t -> (Type, paren ~at:Type1 (ty p t) ^ " / nil")
  | Det_cbor t -> (Type1, "bstr .everparse-det-cbor " ^ paren ~at:Type2 (ty p t))
  | Tuple { name; entries } -> (Type2, tuple p name entries)
  | Record { name; members } -> (Type2, record p name members)

and tuple p name entries =
  let labels =
    List.rev
      (List.fold_left
         (fun acc (e : entry) ->
           fresh ~taken:(fun n -> List.exists (same_name n) acc) (ident e.name)
           :: acc)
         [] entries)
  in
  let line (e : entry) label =
    Fmt.str "  %s%s : %s"
      (if e.optional then "? " else "")
      label
      (snd (ty p e.typ))
  in
  assign p ~base:name
    (Fmt.str "[\n%s\n]" (String.concat ",\n" (List.map2 line entries labels)))

and record p name members =
  let body =
    match members with
    | [] -> "{}"
    | _ ->
        Fmt.str "{\n%s\n}"
          (String.concat ",\n" (List.map (fun m -> "  " ^ member p m) members))
  in
  assign p ~base:name body

(* [exclusive] is one alternative per member, with the other keys matching
   nothing, and one with no key, as cose-sign.cddl:21-30 writes keys 5 and
   6. *)
and member p = function
  | Mem { key; optional; typ; _ } ->
      Fmt.str "%s%a => %s"
        (if optional then "? " else "")
        pp_literal key
        (snd (ty p typ))
  | Exclusive { members; _ } ->
      let absent k = Fmt.str "? %a : everparse-no-match" pp_literal k in
      let alt i (k, t) =
        Fmt.str "(%s)"
          (String.concat ", "
             (Fmt.str "%a => %s" pp_literal k (snd (ty p t))
             :: List.filteri
                  (fun j _ -> j <> i)
                  (List.map absent (List.map fst members))))
      in
      let none =
        Fmt.str "(%s)"
          (String.concat ", " (List.map (fun (k, _) -> absent k) members))
      in
      Fmt.str "(%s)"
        (String.concat " //\n   " (List.mapi alt members @ [ none ]))
  | Keep_unknown { key; value; _ } ->
      Fmt.str "* %s => %s" (paren ~at:Type1 (ty p key)) (snd (ty p value))

let pp_typ ppf t = Fmt.string ppf (snd (ty { rules = [] } t))

(* A root is a rule: a tuple or a record under its own name, any other type
   under [base]. *)
let root p ~base t =
  match t with
  | Tuple _ | Record _ -> snd (ty p t)
  | _ ->
      let body = snd (ty p t) in
      assign p ~base body

(* The CDDL types of a binary type, each with the path of the field that
   holds it, and those [whole] finds in a codec of another backend. *)
let rec held_in : type a.
    whole:(string -> Types.struct_ -> (string * typ) list option) ->
    string ->
    a Types.typ ->
    (string * typ) list =
 fun ~whole path -> function
  | Types.Codec { codec_struct; _ } -> (
      match whole path codec_struct with
      | Some ts -> ts
      | None -> struct_held ~whole codec_struct)
  | Types.Map { inner; _ } -> held_in ~whole path inner
  | Types.Where { inner; _ } -> held_in ~whole path inner
  | Types.Optional { inner; _ } -> held_in ~whole path inner
  | Types.Optional_or { inner; _ } -> held_in ~whole path inner
  | Types.Enum { base; _ } -> held_in ~whole path base
  | Types.Apply { typ; _ } -> held_in ~whole path typ
  | Types.Array { elem; _ } -> held_in ~whole path elem
  | Types.Repeat { elem; _ } -> held_in ~whole path elem
  | Types.Single_elem { elem; _ } -> held_in ~whole path elem
  | Types.Casetype { tag; cases; _ } ->
      held_in ~whole path tag @ cases_held ~whole path cases
  | Types.Union { tag; cases; _ } ->
      held_in ~whole path tag @ cases_held ~whole path cases
  | Types.Struct s -> struct_held ~whole s
  | _ -> []

and cases_held : type a k.
    whole:(string -> Types.struct_ -> (string * typ) list option) ->
    string ->
    (a, k) Types.case_branch list ->
    (string * typ) list =
 fun ~whole path cases ->
  List.concat_map
    (fun (Types.Case_branch { cb_inner; _ }) -> held_in ~whole path cb_inner)
    cases

and struct_held ~whole (s : Types.struct_) =
  List.concat_map
    (fun (Types.Field f) ->
      let field = Option.value f.field_name ~default:"field" in
      held_in ~whole (under (ident s.name) field) f.field_typ)
    s.fields

let held ~whole ~base s =
  match whole base s with Some ts -> ts | None -> struct_held ~whole s

type schema = { name : string; text : string }

let name s = s.name
let filename s = s.name ^ ".cddl"
let pp ppf s = Fmt.string ppf s.text

(* evercddl takes the module name as the F* module it writes and the prefix of
   every C name, with no period (Main.ml:278-281). *)
let check_module_name m =
  let ok =
    String.length m > 0
    && (match m.[0] with 'A' .. 'Z' -> true | _ -> false)
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
         m
  in
  if not ok then
    Fmt.invalid_arg
      "Wire.Cbor.project: %S is not an evercddl module name, a capital letter \
       then letters, digits and underscores"
      m

let project m types =
  check_module_name m;
  let p = { rules = [] } in
  List.iter (fun (base, t) -> ignore (root p ~base t : string)) types;
  let text =
    String.concat ""
      (List.rev_map (fun r -> Fmt.str "%s = %s\n" r.rname r.body) p.rules)
  in
  { name = m; text }

(* The binary types under a [Containing], each with its path. *)
let rec binaries ~path = function
  | Uint | Nint | Int | Bytes | Text | Bool | Null | Any | Literal _ | Range _
    ->
      []
  | Size { inner; _ } -> binaries ~path inner
  | Tag (_, t) | Nullable t | Det_cbor t -> binaries ~path t
  | Array { elem; _ } -> binaries ~path:(under path "elt") elem
  | Assoc (k, v) ->
      binaries ~path:(under path "key") k
      @ binaries ~path:(under path "value") v
  | Choice cases ->
      List.concat_map (fun (n, t) -> binaries ~path:(under path n) t) cases
  | Tuple { name; entries } ->
      List.concat_map
        (fun (e : entry) -> binaries ~path:(under (ident name) e.name) e.typ)
        entries
  | Record { name; members } -> List.concat_map (member_binaries name) members
  | Containing b -> [ (under path "contents", b) ]

and member_binaries owner = function
  | Mem { name; typ; _ } -> binaries ~path:(under (ident owner) name) typ
  | Exclusive { name; members } ->
      List.concat_map
        (fun (k, t) ->
          binaries
            ~path:(under (under (ident owner) name) (Fmt.str "%a" pp_literal k))
            t)
        members
  | Keep_unknown { name; key; value } ->
      let path = under (ident owner) name in
      binaries ~path:(under path "key") key
      @ binaries ~path:(under path "value") value

(* A binary codec projects as [Everparse.project] projects it; any other
   binary type as a struct of one field, named by its path. *)
let contained types =
  let found =
    List.concat_map (fun (base, t) -> binaries ~path:(ident base) t) types
  in
  let id (path, Binary t) =
    match t with
    | Types.Codec { codec_id; _ } -> Fmt.str "codec %d" codec_id
    | _ -> "path " ^ path
  in
  let unique =
    List.rev
      (List.fold_left
         (fun acc b ->
           if List.exists (fun b' -> String.equal (id b) (id b')) acc then acc
           else b :: acc)
         [] found)
  in
  List.map
    (fun (path, Binary t) ->
      match t with
      | Types.Codec { codec_struct; _ } ->
          Everparse.Raw.project_struct codec_struct
      | _ ->
          Everparse.Raw.project_struct
            (Everparse.Raw.struct_ path [ Everparse.Raw.field "value" t ]))
    unique

let write ~outdir ss =
  let rec check = function
    | [] -> ()
    | s :: rest ->
        if
          List.exists
            (fun s' ->
              String.equal s.name s'.name && not (String.equal s.text s'.text))
            rest
        then
          Fmt.invalid_arg "Wire.Cbor.write: two different modules are named %S"
            s.name;
        check rest
  in
  check ss;
  List.iter
    (fun s ->
      Out_channel.with_open_bin
        (Filename.concat outdir (filename s))
        (fun oc -> Out_channel.output_string oc s.text))
    ss
