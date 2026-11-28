(* Deterministic CBOR types (docs/cbor.md). The combinators build the syntax
   the codec and the .cddl printer are functions over, and refuse, when they
   are built, every shape evercddl does not elaborate. EverParse paths are from
   lib/everparse/cddl/ at v2026.11.13: [spec/] is its spec/ directory,
   [Elab.fst] spec/CDDL.Spec.AST.Elab.fst, [Disjoint.fst]
   spec/CDDL.Spec.AST.Elab.Disjoint.Type.fst, [Disjoint.Array.fst]
   spec/CDDL.Spec.AST.Elab.Disjoint.Array.fst and [Elab.Base.fst]
   spec/CDDL.Spec.AST.Elab.Base.fst. *)

type integer = Uint of UInt64.t | Nint of UInt64.t
type literal = [ `Int of int | `Text of string ]

type _ t =
  | Uint : UInt64.t t
  | Nint : UInt64.t t
  | Int : integer t
  | Range : { min : int64; max : int64 } -> int64 t
  | Bytes : string t
  | Text : string t
  | Bool : bool t
  | Null : unit t
  | Literal : literal -> unit t
  | Any : string t
  | Size : { min : int; max : int; inner : string t } -> string t
  | Tag : { tag : int; inner : 'a t } -> 'a t
  | Array : { non_empty : bool; elem : 'a t } -> 'a list t
  | Tuple : 'r group -> 'r t
  | Assoc : { key : 'k t; value : 'v t } -> ('k * 'v) list t
  | Record : 'r group -> 'r t
  | Choice : 'a case list -> 'a t
  | Nullable : 'a t -> 'a option t
  | Encoded : 'a t -> 'a t
  | Containing : 'a Types.typ -> 'a t
  | Keep_bytes : 'a t -> ('a * string) t
  | Map : { inner : 'w t; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a t

(* A tuple or a record: [fields] builds the record with [make], [entries] are
   the same fields as this module reads them, in order. *)
and 'r group =
  | Group : {
      name : string;
      make : 'f;
      fields : ('f, 'r) Codec.fields;
      entries : entry list;
    }
      -> 'r group

and 'a case =
  | Case : {
      name : string;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> 'a case

and 'a member =
  | Member : {
      key : literal;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> 'a member

(* What a [Field.t] of a tuple or a record holds. *)
and _ kind =
  | Elem : 'a t -> 'a kind
  | Elem_opt : 'a t -> 'a option kind
  | Mem : literal * 'a t -> 'a kind
  | Mem_opt : literal * 'a t -> 'a option kind
  | Exclusive : 'a member list -> 'a option kind
  | Keep_unknown : 'k t * 'v t -> ('k * 'v) list kind

and entry = Entry : { name : string; id : int; kind : 'a kind } -> entry

type _ Types.cbor += Member_of : 'a kind -> 'a Types.cbor

let pp_literal ppf = function
  | `Int n -> Fmt.int ppf n
  | `Text s -> Fmt.pf ppf "%S" s

(* What a type matches, as evercddl's elaborator sees it (spec/
   CDDL.Spec.AST.Base.fst:66-109), and its disjointness checks over it. *)
module Shape = struct
  type t =
    | Any
    | Uint
    | Nint
    | Bytes
    | Text
    | Bool
    | Int of int64 (* an integer literal *)
    | Text_lit of string
    | Simple of int (* a simple value literal *)
    | Range of int64 * int64
    | Size of t * int * int
    | Tagged of int * t
    | Array of item list
    | Map
    | Choice of t * t
    | Det_cbor of t

  (* An array group: an entry, or [?], [*] or [+] of a group. *)
  and item =
    | Elem of string option * t
    | Opt of item list
    | Star of item list
    | Plus of item list

  let nil = Simple 22

  (* evercddl's verdicts: [Meet why] is a failure, [Out_of_fuel] an answer
     more unfolding may change. *)
  type verdict = Disjoint | Meet of string | Out_of_fuel

  let first_failure a b =
    match a () with Disjoint -> b () | (Meet _ | Out_of_fuel) as v -> v

  let unless_meet v k =
    match v with (Disjoint | Out_of_fuel) as v -> v | Meet _ -> k ()

  let lt a b = Int64.compare a b < 0

  (* [typ_disjoint] (Disjoint.fst:13-214). evercddl compares two maps
     structurally first, then refuses them; either way they meet. *)
  let rec disjoint fuel t1 t2 =
    if fuel = 0 then Out_of_fuel
    else
      let fuel = fuel - 1 in
      match (t1, t2) with
      | Any, _ | _, Any -> Meet "any matches every data item"
      | Map, Map -> Meet "two maps are never proved disjoint"
      | _ when t1 = t2 -> Meet "they are one type"
      | Choice (l, r), t | t, Choice (l, r) -> choice fuel l r t
      | Tagged (a, x), Tagged (b, y) ->
          if Int.equal a b then disjoint fuel x y else Disjoint
      | Tagged _, _ | _, Tagged _ -> Disjoint
      | Size (base, lo, hi), t | t, Size (base, lo, hi) ->
          unless_meet (disjoint fuel base t) (fun () -> size lo hi t)
      | Det_cbor dest, t | t, Det_cbor dest ->
          unless_meet (disjoint fuel Bytes t) (fun () -> det_cbor fuel dest t)
      | Range (lo, hi), t | t, Range (lo, hi) -> range fuel lo hi t
      | Bool, Simple v | Simple v, Bool ->
          if v = 20 || v = 21 then Meet "bool matches false and true"
          else Disjoint
      | Int v, Uint | Uint, Int v ->
          if lt v 0L then Disjoint else Meet "the literal is an uint"
      | Int v, Nint | Nint, Int v ->
          if lt v 0L then Meet "the literal is an nint" else Disjoint
      | Text_lit _, Bytes | Bytes, Text_lit _ -> Disjoint
      | Text_lit _, Text | Text, Text_lit _ -> Meet "the literal is a tstr"
      | (Uint | Nint | Bytes | Text | Bool | Int _ | Text_lit _ | Simple _), _
      | _, (Uint | Nint | Bytes | Text | Bool | Int _ | Text_lit _ | Simple _)
        ->
          Disjoint
      | Map, _ | _, Map -> Disjoint
      | Array a1, Array a2 -> group_disjoint fuel (true, a1) (true, a2)

  (* A range across zero is split at zero against a choice. *)
  and choice fuel l r t =
    let split =
      match t with
      | Range (lo, hi) when lt lo 0L && not (lt hi 0L) ->
          first_failure
            (fun () -> disjoint fuel (Range (lo, -1L)) (Choice (l, r)))
            (fun () -> disjoint fuel (Range (0L, hi)) (Choice (l, r)))
      | _ -> Meet ""
    in
    match split with
    | Disjoint -> Disjoint
    | Meet _ | Out_of_fuel ->
        first_failure
          (fun () -> disjoint fuel l t)
          (fun () -> disjoint fuel r t)

  (* A string of [lo] to [hi] bytes against a type its base meets. *)
  and size lo hi = function
    | Text_lit s ->
        let n = String.length s in
        if n > hi || n < lo then Disjoint
        else Meet "the literal has a length the size allows"
    | Det_cbor _ ->
        if hi <= 0 then Disjoint else Meet "the size allows an encoding"
    | Size (_, lo', hi') ->
        if hi < lo' || hi' < lo then Disjoint else Meet "the sizes overlap"
    | _ -> Meet "a string of that size matches both"

  and det_cbor fuel dest = function
    | Det_cbor dest' -> disjoint fuel dest dest'
    | _ -> Meet "a byte string holding an encoding matches both"

  and range fuel lo hi = function
    | Int x ->
        if lt x lo || lt hi x then Disjoint
        else Meet "the literal is in the range"
    | Range (lo', hi') ->
        if lt hi lo' || lt hi' lo then Disjoint else Meet "the ranges overlap"
    | t ->
        if not (lt lo 0L) then disjoint fuel t Uint
        else if lt hi 0L then disjoint fuel t Nint
        else disjoint fuel t (Choice (Uint, Nint))

  (* [array_group_disjoint] (Disjoint.Array.fst:10-121), on groups as lists:
     [close] says nothing may follow the group. *)
  and group_disjoint fuel ((c1, a1) as s1) ((c2, a2) as s2) =
    if fuel = 0 then Out_of_fuel
    else
      let fuel = fuel - 1 in
      if a1 = a2 then Meet "the entries are one group"
      else
        match (s1, s2) with
        | (c, Star g :: r), s | s, (c, Star g :: r) -> star fuel c g r s
        | (c, Plus g :: r), s | s, (c, Plus g :: r) ->
            group_disjoint fuel (c, g @ (Star g :: r)) s
        | (c, Opt g :: r), s | s, (c, Opt g :: r) ->
            first_failure
              (fun () -> group_disjoint fuel (c, r) s)
              (fun () -> group_disjoint fuel (c, g @ r) s)
        | (c, []), (_, Elem _ :: _) | (_, Elem _ :: _), (c, []) ->
            if c then Disjoint else Meet "an array may go on after the entries"
        | (_, Elem (_, t1) :: r1), (_, Elem (_, t2) :: r2) -> (
            match disjoint fuel t1 t2 with
            | Meet _ -> group_disjoint fuel (c1, r1) (c2, r2)
            | v -> v)
        | _ -> Meet "the entries are one group"

  (* [* g] then [r], against [s]: without [g], then [g] alone, then unrolled
     once. *)
  and star fuel c g r s =
    first_failure
      (fun () -> group_disjoint fuel (c, r) s)
      (fun () ->
        match group_disjoint fuel (false, g) s with
        | Disjoint -> Disjoint
        | Meet _ | Out_of_fuel -> group_disjoint fuel (c, g @ (Star g :: r)) s)

  (* evercddl reruns the elaboration with one more unit of fuel until it
     answers (src/cddl/tool/CDDL.Tool.Gen.fst:134-149). Wire doubles it up to
     a bound and refuses past it: the star rewrite can loop, and a combinator
     must answer. *)
  let max_fuel = 1 lsl 10

  let settle check =
    let rec go fuel =
      match check fuel with
      | Out_of_fuel when fuel < max_fuel -> go (2 * fuel)
      | v -> v
    in
    go 64

  let why = function
    | Disjoint -> ""
    | Meet why -> why
    | Out_of_fuel -> "evercddl does not settle it"
end

let shape_of_literal : literal -> Shape.t = function
  | `Int n -> Shape.Int (Int64.of_int n)
  | `Text s -> Shape.Text_lit s

(* A binary or DER type under [containing] is a [bstr], [keep_bytes] and [map]
   are their inner type, [nullable t] is [t / nil]. *)
let rec shape : type a. a t -> Shape.t = function
  | Uint -> Shape.Uint
  | Nint -> Shape.Nint
  | Int -> Shape.Choice (Uint, Nint)
  | Range { min; max } -> Shape.Range (min, max)
  | Bytes -> Shape.Bytes
  | Text -> Shape.Text
  | Bool -> Shape.Bool
  | Null -> Shape.nil
  | Literal l -> shape_of_literal l
  | Any -> Shape.Any
  | Size { min; max; inner } -> Shape.Size (shape inner, min, max)
  | Tag { tag; inner } -> Shape.Tagged (tag, shape inner)
  | Array { non_empty; elem } ->
      let g = [ Shape.Elem (None, shape elem) ] in
      Shape.Array [ (if non_empty then Plus g else Star g) ]
  | Tuple (Group { entries; _ }) -> Shape.Array (List.map item entries)
  | Assoc _ | Record _ -> Shape.Map
  | Choice cases -> choice_shape cases
  | Nullable t -> Shape.Choice (shape t, Shape.nil)
  | Encoded t -> Shape.Det_cbor (shape t)
  | Containing _ -> Shape.Bytes
  | Keep_bytes t -> shape t
  | Map { inner; _ } -> shape inner

and choice_shape : type a. a case list -> Shape.t = function
  | [] -> invalid_arg "Wire.Cbor.choice: no case"
  | [ Case c ] -> shape c.typ
  | Case c :: rest -> Shape.Choice (shape c.typ, choice_shape rest)

and item (Entry e) : Shape.item =
  match e.kind with
  | Elem t -> Shape.Elem (Some e.name, shape t)
  | Elem_opt t -> Shape.Opt [ Shape.Elem (Some e.name, shape t) ]
  | Mem _ | Mem_opt _ | Exclusive _ | Keep_unknown _ ->
      invalid_arg "Wire.Cbor: a map member in an array"

(* Primitives. *)

let uint = Uint
let nint = Nint
let int = Int

(* An empty range does not elaborate (Elab.fst:1263-1264); bounds that are
   [int64] are always representable (Elab.fst:1265-1266). *)
let range ~min ~max =
  if Int64.compare min max > 0 then
    Fmt.invalid_arg
      "Wire.Cbor.range: %Ld..%Ld is empty, which evercddl refuses \
       (spec/CDDL.Spec.AST.Elab.fst:1264)"
      min max;
  Range { min; max }

let bytes = Bytes
let text = Text
let bool = Bool
let null = Null
let any = Any

(* A text literal is ASCII and shorter than 2^16 bytes
   (spec/CDDL.Spec.AST.Base.fst:156). *)
let check_literal ~combinator = function
  | `Int _ -> ()
  | `Text s ->
      if String.length s >= 0x10000 then
        Fmt.invalid_arg
          "Wire.Cbor.%s: a text literal of %d bytes; evercddl takes one \
           shorter than 2^16 (spec/CDDL.Spec.AST.Base.fst:156)"
          combinator (String.length s);
      if String.exists (fun c -> Char.code c >= 0x80) s then
        Fmt.invalid_arg
          "Wire.Cbor.%s: the text literal %S is not ASCII, which evercddl \
           requires (spec/CDDL.Spec.AST.Base.fst:156)"
          combinator s;
      (* evercddl's lexer reads a text literal as printable ASCII with no
         escape (src/cddl/tool/ocaml/evercddl-lib/CDDLLexer.mll, schar). *)
      if
        String.exists
          (fun c ->
            Char.code c < 0x20 || Char.code c > 0x7e || c = '"' || c = '\\')
          s
      then
        Fmt.invalid_arg
          "Wire.Cbor.%s: the text literal %S holds a control character, a \
           quote or a backslash, which evercddl's lexer does not read \
           (CDDLLexer.mll, schar)"
          combinator s

let literal l =
  check_literal ~combinator:"literal" l;
  Literal l

(* Constraints: [.size] on a byte or text string, with a non-empty range of
   lengths (Elab.fst:1300-1334). *)
let size ~min ~max (inner : string t) =
  (match inner with
  | Bytes | Text -> ()
  | _ ->
      invalid_arg
        "Wire.Cbor.size: the type is not bytes or text; evercddl takes .size \
         on bstr and tstr only (spec/CDDL.Spec.AST.Elab.fst:1300-1334)");
  if min < 0 || min > max then
    Fmt.invalid_arg
      "Wire.Cbor.size: %d..%d is no range of lengths; evercddl requires 0 <= \
       min <= max (spec/CDDL.Spec.AST.Elab.fst:1323,1333)"
      min max;
  Size { min; max; inner }

let tag n inner =
  if n < 0 then
    Fmt.invalid_arg "Wire.Cbor.tag: tag %d is negative; a CBOR tag is unsigned"
      n;
  Tag { tag = n; inner }

(* Arrays. *)

let array ?(non_empty = false) elem = Array { non_empty; elem }
let member_field name kind = Field.v name (Types.Cbor (Member_of kind))
let elem name t = member_field name (Elem t)
let elem_opt name t = member_field name (Elem_opt t)

(* The kinds a tuple or a record takes. *)
type accepts = { accepts : 'a. 'a kind -> bool }

(* The fields of a tuple or a record, not paths, as entries. *)
let entries ~combinator ~made_by { accepts } name defs =
  let fields =
    match Codec.fields_of_defs defs with
    | fields -> fields
    | exception Invalid_argument _ ->
        Fmt.invalid_arg
          "Wire.Cbor.%s %S: a path names a field of a layer, not a member"
          combinator name
  in
  let rec go : type f r. (f, r) Codec.fields -> entry list = function
    | Codec.[] -> []
    | Codec.(f :: rest) ->
        let e =
          match Codec.field_typ f with
          | Types.Cbor (Member_of kind) when accepts kind ->
              Entry { name = Codec.field_name f; id = Codec.field_id f; kind }
          | _ ->
              Fmt.invalid_arg "Wire.Cbor.%s %S: field %S is not made by %s"
                combinator name (Codec.field_name f) made_by
        in
        e :: go rest
  in
  let es = go fields in
  let rec distinct = function
    | [] -> ()
    | Entry e :: rest ->
        if List.exists (fun (Entry e') -> String.equal e.name e'.name) rest then
          Fmt.invalid_arg "Wire.Cbor.%s %S: member %S is given twice" combinator
            name e.name;
        distinct rest
  in
  distinct es;
  (fields, es)

let is_array_entry : type a. a kind -> bool = function
  | Elem _ | Elem_opt _ -> true
  | Mem _ | Mem_opt _ | Exclusive _ | Keep_unknown _ -> false

(* An array group has one way to split: an entry [? n : t] and the entries
   after it never start alike, else a parser that takes [t] greedily
   refuses what the rest matches (Elab.fst:1367-1377, Elab.Base.fst:1038-1158,
   [array_group_concat_unique_weak]). An empty group does not elaborate
   (Elab.fst:1402). *)
let reject_ambiguous name entries =
  if List.is_empty entries then
    Fmt.invalid_arg
      "Wire.Cbor.tuple %S: no entry; evercddl does not elaborate an empty \
       array group (spec/CDDL.Spec.AST.Elab.fst:1402)"
      name;
  let rec go = function
    | [] -> ()
    | (Entry e as entry) :: rest -> (
        match e.kind with
        | Elem_opt _ -> (
            let opt = match item entry with Shape.Opt g -> g | g -> [ g ] in
            match
              Shape.settle (fun fuel ->
                  Shape.group_disjoint fuel (false, opt)
                    (true, List.map item rest))
            with
            | Shape.Disjoint -> go rest
            | v ->
                Fmt.invalid_arg
                  "Wire.Cbor.tuple %S: optional entry %S can also start the \
                   entries after it (%s); evercddl requires an array to split \
                   one way (spec/CDDL.Spec.AST.Elab.fst:1372)"
                  name e.name (Shape.why v))
        | _ -> go rest)
  in
  go entries

let tuple name make defs =
  let fields, es =
    entries ~combinator:"tuple" ~made_by:"Cbor.elem or Cbor.elem_opt"
      { accepts = is_array_entry }
      name defs
  in
  reject_ambiguous name es;
  Tuple (Group { name; make; fields; entries = es })

(* Maps. *)

let assoc key value = Assoc { key; value }

let mem name k t =
  check_literal ~combinator:"mem" k;
  member_field name (Mem (k, t))

let mem_opt name k t =
  check_literal ~combinator:"mem_opt" k;
  member_field name (Mem_opt (k, t))

let member key typ ~inject ~project =
  check_literal ~combinator:"member" key;
  Member { key; typ; inject; project }

let equal_literal a b =
  match (a, b) with
  | `Int a, `Int b -> Int.equal a b
  | `Text a, `Text b -> String.equal a b
  | `Int _, `Text _ | `Text _, `Int _ -> false

let rec first_repeated = function
  | [] -> None
  | k :: rest ->
      if List.exists (equal_literal k) rest then Some k else first_repeated rest

let exclusive name members =
  if List.compare_length_with members 2 < 0 then
    Fmt.invalid_arg
      "Wire.Cbor.exclusive %S: %d member; exclusive takes at least two, and \
       one optional member is mem_opt"
      name (List.length members);
  (match first_repeated (List.map (fun (Member m) -> m.key) members) with
  | None -> ()
  | Some k ->
      Fmt.invalid_arg "Wire.Cbor.exclusive %S: the key %a is given twice" name
        pp_literal k);
  member_field name (Exclusive members)

let keep_unknown name k v = member_field name (Keep_unknown (k, v))

let is_map_member : type a. a kind -> bool = function
  | Mem _ | Mem_opt _ | Exclusive _ | Keep_unknown _ -> true
  | Elem _ | Elem_opt _ -> false

let keys_of (Entry e) =
  match e.kind with
  | Mem (k, _) | Mem_opt (k, _) -> [ (e.name, k) ]
  | Exclusive ms -> List.map (fun (Member m) -> (e.name, m.key)) ms
  | Keep_unknown _ | Elem _ | Elem_opt _ -> []

(* The members of a map group have disjoint footprints (Elab.fst:1480-1485):
   two literal keys meet when they are equal, and two [* k => v] tables are
   not told apart. *)
let record name make defs =
  let fields, es =
    entries ~combinator:"record"
      ~made_by:"Cbor.mem, Cbor.mem_opt, Cbor.exclusive or Cbor.keep_unknown"
      { accepts = is_map_member }
      name defs
  in
  let keys = List.concat_map keys_of es in
  let rec distinct = function
    | [] -> ()
    | (n, k) :: rest -> (
        match List.find_opt (fun (_, k') -> equal_literal k k') rest with
        | None -> distinct rest
        | Some (n', _) ->
            Fmt.invalid_arg
              "Wire.Cbor.record %S: members %S and %S share the key %a; \
               evercddl requires the members of a map to have disjoint keys \
               (spec/CDDL.Spec.AST.Elab.fst:1480-1485)"
              name n n' pp_literal k)
  in
  distinct keys;
  (match
     List.filter
       (fun (Entry e) ->
         match e.kind with Keep_unknown _ -> true | _ -> false)
       es
   with
  | Entry a :: Entry b :: _ ->
      Fmt.invalid_arg
        "Wire.Cbor.record %S: members %S and %S both keep unknown entries; \
         their keys meet, and evercddl requires the members of a map to have \
         disjoint keys (spec/CDDL.Spec.AST.Elab.fst:1480-1485)"
        name a.name b.name
  | _ -> ());
  Record (Group { name; make; fields; entries = es })

(* Choices: every case is disjoint from the cases after it, as evercddl
   elaborates [t1 / t2 / ...] with its choices guarded (Elab.fst:1232-1233,
   Elab.fst:1683). *)

let case name typ ~inject ~project = Case { name; typ; inject; project }

let choice cases =
  if List.is_empty cases then
    invalid_arg "Wire.Cbor.choice: a choice has at least one case";
  let rec go = function
    | [] | [ _ ] -> ()
    | Case c :: rest -> (
        if List.exists (fun (Case c') -> String.equal c.name c'.name) rest then
          Fmt.invalid_arg "Wire.Cbor.choice: case %S is given twice" c.name;
        let sc = shape c.typ in
        match
          Shape.settle (fun fuel -> Shape.disjoint fuel sc (choice_shape rest))
        with
        | Shape.Disjoint -> go rest
        | v ->
            let other =
              List.find_map
                (fun (Case c') ->
                  match
                    Shape.settle (fun fuel ->
                        Shape.disjoint fuel sc (shape c'.typ))
                  with
                  | Shape.Disjoint -> None
                  | _ -> Some c'.name)
                rest
              |> Option.value ~default:"the cases after it"
            in
            Fmt.invalid_arg
              "Wire.Cbor.choice: cases %S and %S can match one data item (%s); \
               evercddl requires the cases of a choice to be disjoint \
               (spec/CDDL.Spec.AST.Elab.fst:1232-1233)"
              c.name other (Shape.why v))
  in
  go cases;
  Choice cases

let nullable t =
  match Shape.settle (fun fuel -> Shape.disjoint fuel (shape t) Shape.nil) with
  | Shape.Disjoint -> Nullable t
  | v ->
      Fmt.invalid_arg
        "Wire.Cbor.nullable: the type matches nil (%s); evercddl requires the \
         cases of t / nil to be disjoint \
         (spec/CDDL.Spec.AST.Elab.fst:1232-1233)"
        (Shape.why v)

(* Bytes. *)

let encoded t = Encoded t

(* A DER or CBOR codec under a byte string, through [map], would be checked
   by no backend: evercddl sees a bstr, Asn1.project does not read a CBOR
   codec, and the CDDL of a CBOR value in a byte string is [encoded]'s. *)
let containing t =
  match Types.backend_value t with
  | None -> Containing t
  | Some `Der ->
      invalid_arg
        "Wire.Cbor.containing: a DER codec in a byte string has no projection: \
         evercddl sees a bstr and Asn1.project does not read a CBOR codec"
  | Some `Cbor ->
      invalid_arg
        "Wire.Cbor.containing: a CBOR codec in a byte string has no \
         projection; Cbor.encoded is the byte string evercddl checks \
         (.everparse-det-cbor)"

let keep_bytes t = Keep_bytes t
let map ~decode ~encode inner = Map { inner; decode; encode }

(* The interpreted codec (docs/cbor.md, Deterministic encoding and who
   enforces it). [Cbor_det.skip] first checks the one data item against every
   rule of deterministic encoding, as [cbor_det_parse] does before
   [M_validate_T]; [check] then matches the type and is where the item ends,
   and [read] builds the value of an item [check] accepted. Both raise the
   parse error of the first rule the bytes break. *)

let fail ~at =
  Types.raise_error ~at
    (Constraint_failed { which = (Field : Error.predicate); value = None })

let det_fail lim (e : Cbor_det.error) =
  match e.reason with
  | Eof n -> Types.raise_eof ~at:e.at ~expected:n ~got:(max 0 (lim - e.at))
  | Trailing n -> Types.raise_error ~at:e.at (Trailing_bytes n)
  | Reserved | Indefinite | Not_shortest | Float | Simple | Utf8 | Unsorted
  | Duplicate ->
      fail ~at:e.at

let det lim = function Ok x -> x | Error e -> det_fail lim e
let head buf s lim = det lim (Cbor_det.head buf ~off:s ~limit:lim)
let skip buf s lim = det lim (Cbor_det.skip buf ~off:s ~limit:lim)
let string_end buf h lim = det lim (Cbor_det.string_end buf h ~limit:lim)

let head_of major buf s lim =
  let h = head buf s lim in
  if h.major <> major then fail ~at:s;
  h

(* The count of an array or map head, at most one item per byte left. *)
let count (h : Cbor_det.head) lim =
  if Int64.unsigned_compare h.arg (Int64.of_int (lim - h.next)) > 0 then
    Types.raise_eof ~at:h.next ~expected:(lim - h.next + 1) ~got:(lim - h.next)
  else Int64.to_int h.arg

(* The value of an integer head, if an [int64] holds it: [-1 - n] is
   [lognot n]. *)
let int64_of_head (h : Cbor_det.head) =
  match h.major with
  | 0 when Int64.compare h.arg 0L >= 0 -> Some h.arg
  | 1 when Int64.compare h.arg 0L >= 0 -> Some (Int64.lognot h.arg)
  | _ -> None

let in_member name f =
  try f ()
  with exn when Types.is_parse_error exn ->
    let e = Types.parse_error_of exn in
    Types.raise_parse_error { e with field = name :: e.field }

let attempt f =
  match f () with
  | v -> Some v
  | exception exn when Types.is_parse_error exn -> None

let equal_sub buf s e str =
  e - s = String.length str
  &&
  let rec go i =
    i = e - s || (Char.equal (Bytes.get buf (s + i)) str.[i] && go (i + 1))
  in
  go 0

(* Where the literal [l] at [s] ends. *)
let literal_end buf s lim l =
  let h = head buf s lim in
  match l with
  | `Int n ->
      if
        not (Option.equal Int64.equal (int64_of_head h) (Some (Int64.of_int n)))
      then fail ~at:s;
      h.next
  | `Text t ->
      if h.major <> 3 then fail ~at:s;
      let e = string_end buf h lim in
      if not (equal_sub buf h.next e t) then fail ~at:s;
      e

let is_literal buf s e l =
  match literal_end buf s e l with
  | e' -> e' = e
  | exception exn when Types.is_parse_error exn -> false

(* The bytes of a binary or DER codec, for [containing]: its own check, read
   and write, on exactly the content of the byte string. *)
type 'a contents = {
  validate : Input_end.t -> bytes -> int -> unit;
  decode : Input_end.t -> bytes -> int -> 'a;
  measure : Input_end.t -> bytes -> int -> int;
  encode : 'a -> bytes -> int -> int;
  encoded_size : 'a -> int;
}

let contents : type a. a Types.typ -> a contents = function
  | Types.Codec c ->
      let ctx = Types.unbound_eval_ctx in
      {
        validate = c.codec_validate ctx;
        decode = c.codec_decode ctx;
        measure = c.codec_size_of ctx;
        encode = (fun v buf off -> c.codec_encode v ctx buf off);
        encoded_size = c.codec_size_of_value ctx;
      }
  | typ ->
      let c =
        Codec.v "content" Fun.id Codec.[ Field.v "content" typ $ Fun.id ]
      in
      let ctx = Types.unbound_eval_ctx in
      let size =
        match Codec.wire_size_info_ctx c with
        | `Fixed n -> fun _ _ _ -> n
        | `Variable f -> f ctx
      in
      {
        validate = Codec.embed_validate_ctx c ctx;
        decode = Codec.embed_decode_ctx c ctx;
        measure = size;
        encode = (fun v buf off -> Codec.embed_encode_ctx c v ctx buf off);
        encoded_size = Codec.encoded_size_ctx c ctx;
      }

let within buf ce = Input_end.narrow (Input_end.of_bytes buf) ce

(* The content [cs] to [ce] is exactly one value of [c]. *)
let check_contents c buf cs ce =
  let ie = within buf ce in
  c.validate ie buf cs;
  let e = cs + c.measure ie buf cs in
  if e <> ce then Types.raise_error ~at:e (Trailing_bytes (ce - e))

(* Where the members of a tuple or a record lie, by entry: absent, the value
   at [s] to [e], member [j] of an [exclusive] at [s] to [e], or the entries
   a [keep_unknown] keeps, each a key at [ks] and a value from [ke] to
   [ve]. *)
type slot =
  | Absent
  | Span of int * int
  | Chosen of int * int * int
  | Pairs of (int * int * int) list

let rec check : type a. a t -> bytes -> int -> int -> int =
 fun t buf s lim ->
  match t with
  | Uint -> (head_of 0 buf s lim).next
  | Nint -> (head_of 1 buf s lim).next
  | Int ->
      let h = head buf s lim in
      if h.major > 1 then fail ~at:s;
      h.next
  | Range { min; max } -> (
      let h = head buf s lim in
      if h.major > 1 then fail ~at:s;
      match int64_of_head h with
      | Some v when Int64.compare min v <= 0 && Int64.compare v max <= 0 ->
          h.next
      | Some v -> Types.raise_error ~at:s (Value_out_of_range { value = v })
      | None -> fail ~at:s)
  | Bytes -> string_end buf (head_of 2 buf s lim) lim
  | Text -> string_end buf (head_of 3 buf s lim) lim
  | Bool ->
      let h = head_of 7 buf s lim in
      if not (Int64.equal h.arg 20L || Int64.equal h.arg 21L) then fail ~at:s;
      h.next
  | Null ->
      let h = head_of 7 buf s lim in
      if not (Int64.equal h.arg 22L) then fail ~at:s;
      h.next
  | Literal l -> literal_end buf s lim l
  | Any -> skip buf s lim
  | Size { min; max; inner } ->
      let e = check inner buf s lim in
      let n = e - (head buf s lim).next in
      if n < min || n > max then fail ~at:s;
      e
  | Tag { tag; inner } ->
      let h = head_of 6 buf s lim in
      if not (Int64.equal h.arg (Int64.of_int tag)) then fail ~at:s;
      check inner buf h.next lim
  | Array { non_empty; elem } ->
      let h = head_of 4 buf s lim in
      let n = count h lim in
      if non_empty && n = 0 then fail ~at:s;
      let rec go p i = if i = 0 then p else go (check elem buf p lim) (i - 1) in
      go h.next n
  | Tuple g -> snd (locate_tuple g buf s lim)
  | Assoc { key; value } ->
      let h = head_of 5 buf s lim in
      let rec go p i =
        if i = 0 then p
        else go (check value buf (check key buf p lim) lim) (i - 1)
      in
      go h.next (count h lim)
  | Record g -> snd (locate_record g buf s lim)
  | Choice cases -> (
      match
        List.find_map
          (fun (Case c) -> attempt (fun () -> check c.typ buf s lim))
          cases
      with
      | Some e -> e
      | None -> fail ~at:s)
  | Nullable t -> (
      match attempt (fun () -> check Null buf s lim) with
      | Some e -> e
      | None -> check t buf s lim)
  | Encoded t ->
      let h = head_of 2 buf s lim in
      let ce = string_end buf h lim in
      let e = skip buf h.next ce in
      if e <> ce then Types.raise_error ~at:e (Trailing_bytes (ce - e));
      ignore (check t buf h.next ce : int);
      ce
  | Containing typ ->
      let h = head_of 2 buf s lim in
      let ce = string_end buf h lim in
      check_contents (contents typ) buf h.next ce;
      ce
  | Keep_bytes t -> check t buf s lim
  | Map { inner; _ } -> check inner buf s lim

(* The entries of a tuple in order: an [elem_opt] takes the next item when it
   matches, as evercddl's array parser does; no item is left over. *)
and locate_tuple : type r. r group -> bytes -> int -> int -> slot array * int =
 fun (Group g) buf s lim ->
  let h = head_of 4 buf s lim in
  let entries = Array.of_list g.entries in
  let slots = Array.make (Array.length entries) Absent in
  let rec go i p left =
    if i = Array.length entries then begin
      if left > 0 then fail ~at:p;
      p
    end
    else
      let (Entry e) = entries.(i) in
      match e.kind with
      | Elem t ->
          if left = 0 then fail ~at:p;
          let p' = in_member e.name (fun () -> check t buf p lim) in
          slots.(i) <- Span (p, p');
          go (i + 1) p' (left - 1)
      | Elem_opt t -> (
          match
            if left = 0 then None else attempt (fun () -> check t buf p lim)
          with
          | Some p' ->
              slots.(i) <- Span (p, p');
              go (i + 1) p' (left - 1)
          | None -> go (i + 1) p left)
      | Mem _ | Mem_opt _ | Exclusive _ | Keep_unknown _ -> fail ~at:p
  in
  let e = go 0 h.next (count h lim) in
  (slots, e)

(* The entries of a record: an entry whose key is a member's goes to that
   member when its value matches, as [k => t] does; an entry under a key of
   an [exclusive] must match that member, as its cut does; every other entry
   goes to the [keep_unknown] member, whose key and value it must match. *)
and locate_record : type r. r group -> bytes -> int -> int -> slot array * int =
 fun (Group g) buf s lim ->
  let h = head_of 5 buf s lim in
  let entries = Array.of_list g.entries in
  let slots = Array.make (Array.length entries) Absent in
  let unknown = ref [] in
  let rec entry p i =
    if i = 0 then p
    else
      let ke = skip buf p lim in
      let ve = skip buf ke lim in
      if not (take_member entries slots buf (p, ke, ve) 0) then
        take_unknown entries buf (p, ke, ve) unknown;
      entry ve (i - 1)
  in
  let e = entry h.next (count h lim) in
  Array.iteri
    (fun i (Entry en) ->
      match en.kind with
      | Mem _ when match slots.(i) with Absent -> true | _ -> false ->
          in_member en.name (fun () -> fail ~at:s)
      | Keep_unknown _ -> slots.(i) <- Pairs (List.rev !unknown)
      | _ -> ())
    entries;
  (slots, e)

(* Whether the member [i] or one after it takes the entry whose key lies at
   [ks] to [ke] and value at [ke] to [ve]. *)
and take_member entries slots buf ((ks, ke, ve) as kv) i =
  i < Array.length entries
  &&
  let (Entry e) = entries.(i) in
  let taken =
    match e.kind with
    | Mem (k, t) -> take_mem slots buf kv i k t
    | Mem_opt (k, t) -> take_mem slots buf kv i k t
    | Exclusive ms -> take_exclusive slots buf (ks, ke, ve) i e.name ms
    | Keep_unknown _ | Elem _ | Elem_opt _ -> false
  in
  taken || take_member entries slots buf kv (i + 1)

and take_mem : type a.
    slot array -> bytes -> int * int * int -> int -> literal -> a t -> bool =
 fun slots buf (ks, ke, ve) i k t ->
  is_literal buf ks ke k
  && Option.is_some (attempt (fun () -> check t buf ke ve))
  && begin
    slots.(i) <- Span (ke, ve);
    true
  end

and take_exclusive : type a.
    slot array ->
    bytes ->
    int * int * int ->
    int ->
    string ->
    a member list ->
    bool =
 fun slots buf (ks, ke, ve) i name ms ->
  let rec go j = function
    | [] -> false
    | Member m :: rest ->
        if is_literal buf ks ke m.key then begin
          (match slots.(i) with Absent -> () | _ -> fail ~at:ks);
          ignore (in_member name (fun () -> check m.typ buf ke ve) : int);
          slots.(i) <- Chosen (j, ke, ve);
          true
        end
        else go (j + 1) rest
  in
  go 0 ms

(* An entry no member takes goes to the [keep_unknown] member, if any, whose
   key and value it must match. *)
and take_unknown entries buf (ks, ke, ve) unknown =
  match
    Array.find_map
      (fun (Entry e) ->
        match e.kind with
        | Keep_unknown (k, v) ->
            Some
              (attempt (fun () ->
                   check k buf ks ke = ke && check v buf ke ve = ve))
        | _ -> None)
      entries
  with
  | Some (Some true) -> unknown := (ks, ke, ve) :: !unknown
  | Some (Some false | None) | None -> fail ~at:ks

(* The value of an item [check] accepted, and where it ends. *)
let rec read : type a. a t -> bytes -> int -> int -> a * int =
 fun t buf s lim ->
  match t with
  | Uint ->
      let h = head buf s lim in
      (UInt64.of_int64 h.arg, h.next)
  | Nint ->
      let h = head buf s lim in
      (UInt64.of_int64 h.arg, h.next)
  | Int ->
      let h = head buf s lim in
      let n = UInt64.of_int64 h.arg in
      ((if h.major = 0 then Uint n else Nint n), h.next)
  | Range _ -> (
      let h = head buf s lim in
      match int64_of_head h with Some v -> (v, h.next) | None -> fail ~at:s)
  | Bytes -> read_string buf s lim
  | Text -> read_string buf s lim
  | Bool ->
      let h = head buf s lim in
      (Int64.equal h.arg 21L, h.next)
  | Null -> ((), (head buf s lim).next)
  | Literal l -> ((), literal_end buf s lim l)
  | Any ->
      let e = skip buf s lim in
      (Bytes.sub_string buf s (e - s), e)
  | Size { inner; _ } -> read inner buf s lim
  | Tag { inner; _ } -> read inner buf (head buf s lim).next lim
  | Array { elem; _ } ->
      let h = head buf s lim in
      let rec go acc p i =
        if i = 0 then (List.rev acc, p)
        else
          let v, p = read elem buf p lim in
          go (v :: acc) p (i - 1)
      in
      go [] h.next (count h lim)
  | Tuple g ->
      let slots, e = locate_tuple g buf s lim in
      (build g slots buf, e)
  | Assoc { key; value } ->
      let h = head buf s lim in
      let rec go acc p i =
        if i = 0 then (List.rev acc, p)
        else
          let k, p = read key buf p lim in
          let v, p = read value buf p lim in
          go ((k, v) :: acc) p (i - 1)
      in
      go [] h.next (count h lim)
  | Record g ->
      let slots, e = locate_record g buf s lim in
      (build g slots buf, e)
  | Choice cases -> (
      match
        List.find_map
          (fun (Case c) ->
            match attempt (fun () -> check c.typ buf s lim) with
            | Some _ ->
                let v, e = read c.typ buf s lim in
                Some (c.inject v, e)
            | None -> None)
          cases
      with
      | Some r -> r
      | None -> fail ~at:s)
  | Nullable t -> (
      match attempt (fun () -> check Null buf s lim) with
      | Some e -> (None, e)
      | None ->
          let v, e = read t buf s lim in
          (Some v, e))
  | Encoded t ->
      let h = head buf s lim in
      let ce = string_end buf h lim in
      (fst (read t buf h.next ce), ce)
  | Containing typ ->
      let h = head buf s lim in
      let ce = string_end buf h lim in
      ((contents typ).decode (within buf ce) buf h.next, ce)
  | Keep_bytes t ->
      let v, e = read t buf s lim in
      ((v, Bytes.sub_string buf s (e - s)), e)
  | Map { inner; decode; _ } ->
      let w, e = read inner buf s lim in
      (decode w, e)

and read_string : bytes -> int -> int -> string * int =
 fun buf s lim ->
  let h = head buf s lim in
  let e = string_end buf h lim in
  (Bytes.sub_string buf h.next (e - h.next), e)

and read_slot : type a. a kind -> slot -> bytes -> a =
 fun kind slot buf ->
  match (kind, slot) with
  | Elem t, Span (s, e) -> fst (read t buf s e)
  | Mem (_, t), Span (s, e) -> fst (read t buf s e)
  | Elem_opt t, Span (s, e) -> Some (fst (read t buf s e))
  | Mem_opt (_, t), Span (s, e) -> Some (fst (read t buf s e))
  | Elem_opt _, Absent -> None
  | Mem_opt _, Absent -> None
  | Exclusive _, Absent -> None
  | Exclusive ms, Chosen (j, s, e) ->
      let (Member m) = List.nth ms j in
      Some (m.inject (fst (read m.typ buf s e)))
  | Keep_unknown (k, v), Pairs ps ->
      List.map
        (fun (ks, ke, ve) -> (fst (read k buf ks ke), fst (read v buf ke ve)))
        ps
  | (Elem _ | Mem _ | Elem_opt _ | Mem_opt _ | Exclusive _ | Keep_unknown _), _
    ->
      fail ~at:0

(* The record of a tuple or a record whose members lie at [slots]. *)
and build : type r. r group -> slot array -> bytes -> r =
 fun (Group g) slots buf ->
  let rec apply : type f. f -> (f, r) Codec.fields -> int -> r =
   fun make fields i ->
    match fields with
    | Codec.[] -> make
    | Codec.(f :: rest) -> (
        match Codec.field_typ f with
        | Types.Cbor (Member_of kind) ->
            apply (make (read_slot kind slots.(i) buf)) rest (i + 1)
        | _ -> fail ~at:0)
  in
  apply g.make g.fields 0

(* Writing: the one deterministic encoding of a value, or [Invalid_argument]
   for a value [check] refuses. *)

let cannot_write fmt =
  Fmt.kstr (fun m -> invalid_arg ("Wire.Cbor.codec: " ^ m)) fmt

let write_int64 b v =
  if Int64.compare v 0L >= 0 then Cbor_det.write_head b 0 v
  else Cbor_det.write_head b 1 (Int64.lognot v)

let write_string b major s =
  Cbor_det.write_head b major (Int64.of_int (String.length s));
  Buffer.add_string b s

let literal_bytes = function
  | `Int n ->
      let b = Buffer.create 9 in
      write_int64 b (Int64.of_int n);
      Buffer.contents b
  | `Text s -> Cbor_det.to_string (Text s)

let write_map b entries =
  match Cbor_det.write_map b entries with
  | () -> ()
  | exception Invalid_argument _ -> cannot_write "two map entries share a key"

(* Whether [s] is exactly one value of [t]. *)
let holds t s =
  let buf = Bytes.of_string s in
  let n = Bytes.length buf in
  match check t buf 0 (skip buf 0 n) with
  | e -> e = n
  | exception exn when Types.is_parse_error exn -> false

let rec write_item : type a. Buffer.t -> a t -> a -> unit =
 fun b t v ->
  match t with
  | Uint -> Cbor_det.write_head b 0 (UInt64.to_int64 v)
  | Nint -> Cbor_det.write_head b 1 (UInt64.to_int64 v)
  | Int -> (
      match v with
      | Uint n -> Cbor_det.write_head b 0 (UInt64.to_int64 n)
      | Nint n -> Cbor_det.write_head b 1 (UInt64.to_int64 n))
  | Range { min; max } ->
      if Int64.compare v min < 0 || Int64.compare v max > 0 then
        cannot_write "%Ld is outside %Ld..%Ld" v min max;
      write_int64 b v
  | Bytes -> write_string b 2 v
  | Text -> (
      match Cbor_det.encode b (Text v) with
      | () -> ()
      | exception Invalid_argument _ -> cannot_write "%S is not UTF-8" v)
  | Bool -> Cbor_det.write_head b 7 (if v then 21L else 20L)
  | Null -> Cbor_det.write_head b 7 22L
  | Literal l -> Buffer.add_string b (literal_bytes l)
  | Any ->
      if not (holds Any v) then
        cannot_write "%S is not one deterministic data item" v;
      Buffer.add_string b v
  | Size { min; max; inner } ->
      let n = String.length v in
      if n < min || n > max then
        cannot_write "%d bytes are outside .size (%d..%d)" n min max;
      write_item b inner v
  | Tag { tag; inner } ->
      Cbor_det.write_head b 6 (Int64.of_int tag);
      write_item b inner v
  | Array { non_empty; elem } ->
      if non_empty && List.is_empty v then
        cannot_write "an empty list for a non-empty array";
      Cbor_det.write_head b 4 (Int64.of_int (List.length v));
      List.iter (write_item b elem) v
  | Tuple g -> write_tuple b g v
  | Assoc { key; value } ->
      write_map b
        (List.map (fun (k, x) -> (to_string key k, to_string value x)) v)
  | Record g -> write_record b g v
  | Choice cases -> (
      match
        List.find_map
          (fun (Case c) ->
            Option.map (fun w () -> write_item b c.typ w) (c.project v))
          cases
      with
      | Some w -> w ()
      | None -> cannot_write "no case of the choice takes the value")
  | Nullable t -> (
      match v with
      | None -> Cbor_det.write_head b 7 22L
      | Some x -> write_item b t x)
  | Encoded t -> write_string b 2 (to_string t v)
  | Containing typ ->
      let c = contents typ in
      let buf = Bytes.create (c.encoded_size v) in
      let e = c.encode v buf 0 in
      (match check_contents c buf 0 e with
      | () -> ()
      | exception exn when Types.is_parse_error exn ->
          cannot_write "the containing value is refused by its codec (%a)"
            Error.pp (Types.parse_error_of exn));
      write_string b 2 (Bytes.sub_string buf 0 e)
  | Keep_bytes t -> write_item b t (fst v)
  | Map { inner; encode; _ } -> write_item b inner (encode v)

and to_string : type a. a t -> a -> string =
 fun t v ->
  let b = Buffer.create 16 in
  write_item b t v;
  Buffer.contents b

and write_tuple : type r. Buffer.t -> r group -> r -> unit =
 fun b (Group g) r ->
  let items = Buffer.create 16 in
  let rec go : type f. int -> (f, r) Codec.fields -> int =
   fun n -> function
     | Codec.[] -> n
     | Codec.(f :: rest) -> (
         match (Codec.field_typ f, Codec.field_proj f r) with
         | Types.Cbor (Member_of (Elem t)), x ->
             write_item items t x;
             go (n + 1) rest
         | Types.Cbor (Member_of (Elem_opt t)), Some x ->
             write_item items t x;
             go (n + 1) rest
         | _ -> go n rest)
  in
  let n = go 0 g.fields in
  Cbor_det.write_head b 4 (Int64.of_int n);
  Buffer.add_buffer b items

(* The entries of a record. An entry [keep_unknown] holds must not be one
   another member takes back on reading. *)
and write_record : type r. Buffer.t -> r group -> r -> unit =
 fun b (Group g) r ->
  let taken (kb, vb) =
    List.exists
      (fun (Entry e) ->
        match e.kind with
        | Mem (k, t) -> String.equal kb (literal_bytes k) && holds t vb
        | Mem_opt (k, t) -> String.equal kb (literal_bytes k) && holds t vb
        | Exclusive ms ->
            List.exists
              (fun (Member m) -> String.equal kb (literal_bytes m.key))
              ms
        | Keep_unknown _ | Elem _ | Elem_opt _ -> false)
      g.entries
  in
  let rec go : type f.
      (string * string) list -> (f, r) Codec.fields -> (string * string) list =
   fun acc -> function
     | Codec.[] -> acc
     | Codec.(f :: rest) -> (
         let name = Codec.field_name f in
         match (Codec.field_typ f, Codec.field_proj f r) with
         | Types.Cbor (Member_of (Mem (k, t))), x ->
             go ((literal_bytes k, to_string t x) :: acc) rest
         | Types.Cbor (Member_of (Mem_opt (k, t))), Some x ->
             go ((literal_bytes k, to_string t x) :: acc) rest
         | Types.Cbor (Member_of (Exclusive ms)), Some x -> (
             match
               List.find_map
                 (fun (Member m) ->
                   Option.map
                     (fun w -> (literal_bytes m.key, to_string m.typ w))
                     (m.project x))
                 ms
             with
             | Some entry -> go (entry :: acc) rest
             | None -> cannot_write "no member of %S takes the value" name)
         | Types.Cbor (Member_of (Keep_unknown (k, v))), xs ->
             let entries =
               List.map (fun (kx, vx) -> (to_string k kx, to_string v vx)) xs
             in
             if List.exists taken entries then
               cannot_write "an entry of %S is one another member reads" name;
             go (List.rev_append entries acc) rest
         | _ -> go acc rest)
  in
  write_map b (go [] g.fields)

(* The root of a CBOR codec, its CBOR type, as the typ of the codec's one
   field: how a CBOR codec is told from a binary one, by its views and under
   [nested]. *)
type _ Types.cbor += Root : 'a t -> 'a Types.cbor

(* The tuple or record at the root of a type, under [depth] tags, and how to
   locate its members. *)
type 'r root = {
  group : 'r group;
  depth : int;
  locate : bytes -> int -> int -> slot array * int;
}

let rec root_of : type a. int -> a t -> a root option =
 fun depth -> function
  | Tuple g -> Some { group = g; depth; locate = locate_tuple g }
  | Record g -> Some { group = g; depth; locate = locate_record g }
  | Tag { inner; _ } -> root_of (depth + 1) inner
  | _ -> None

let rec type_name : type a. a t -> string = function
  | Tuple (Group { name; _ }) | Record (Group { name; _ }) -> name
  | Tag { inner; _ } -> type_name inner
  | Keep_bytes inner -> type_name inner
  | Map { inner; _ } -> type_name inner
  | _ -> "CBOR"

(* The one data item at [off], before [lim], checked against [t]. *)
let check_item t buf off lim =
  let e = skip buf off lim in
  ignore (check t buf off e : int);
  e

(* A member of the record at the root, read in place after a check of the
   whole item, as [Codec.get] reads a field. *)
let components (type r) (t : r t) : r Codec.components =
  let find (type a) (f : (a, r) Codec.field) :
      (bytes -> int -> (a, Error.t) result) option =
    match (root_of 0 t, Codec.field_typ f) with
    | Some root, Types.Cbor (Member_of kind) -> (
        let (Group g) = root.group in
        let rec index i = function
          | [] -> None
          | Entry e :: rest ->
              if e.id = Codec.field_id f then Some i else index (i + 1) rest
        in
        match index 0 g.entries with
        | None -> None
        | Some i ->
            Some
              (fun buf off ->
                match
                  let e = check_item t buf off (Bytes.length buf) in
                  let rec unwrap s d =
                    if d = 0 then s else unwrap (head buf s e).next (d - 1)
                  in
                  let slots, _ = root.locate buf (unwrap off root.depth) e in
                  read_slot kind slots.(i) buf
                with
                | v -> Ok v
                | exception exn when Types.is_parse_error exn ->
                    Error (Types.parse_error_of exn)))
    | _ -> None
  in
  { Codec.find }

let codec (type a) (t : a t) : a Codec.t =
  let name = type_name t in
  let field =
    Types.Field
      {
        field_name = Some name;
        field_typ = Types.Cbor (Root t);
        constraint_ = None;
        action = None;
        field_doc = None;
      }
  in
  let lim ie = Input_end.to_int ie in
  Codec.opaque ~name field ~min_size:1
    ~check:(fun ie buf off -> ignore (check_item t buf off (lim ie) : int))
    ~read:(fun ie buf off -> fst (read t buf off (skip buf off (lim ie))))
    ~extent:(fun ie buf off -> skip buf off (lim ie))
    ~write:(fun v buf off ->
      let s = to_string t v in
      let len = String.length s in
      if off > Bytes.length buf - len then
        Fmt.invalid_arg "Codec.encode %s: %d bytes do not fit at %d in %d" name
          len off (Bytes.length buf);
      Bytes.blit_string s 0 buf off len;
      off + len)
    ~size_of_value:(fun v -> String.length (to_string t v))
    ~components:(components t)
