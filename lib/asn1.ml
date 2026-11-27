(* An ASN.1 type is a GADT of its own, [t]: tagging, OPTIONAL and CHOICE need
   the tags of their argument, which a binary [Types.typ] does not carry. A
   SEQUENCE component is still a [Field.t], so that [Codec.( $ )] binds it to a
   record as [Codec.v] does: its [Types.typ] is the one node [Types.Der], which
   holds the component's [decorated] here and which no binary path reads.

   Every combinator refuses, with [Invalid_argument], a shape everparse-asn1
   does not express (docs/asn1.md, "Refused when built"). The decoder, the
   encoder and the [.asn] printer are functions over [t]. *)

let todo ~lane name =
  Fmt.invalid_arg "Wire.Asn1.%s: not implemented, lane %s brings it" name lane

type cls = [ `Universal | `Application | `Private ]

module Oid = struct
  (* The arcs, at least two, each below 2^32 as everparse-asn1 reads them
     (spec/ASN1.Base.fst:193-202). A [UInt32.t] holds 32 bits on every target;
     an [int] holds 31 on wasm_of_ocaml. *)
  type t = UInt32.t array

  let max_arc = 0xFFFF_FFFFL
  let in_range n = Int64.compare n 0L >= 0 && Int64.compare n max_arc <= 0
  let arc n = UInt32.of_int32 (Int64.to_int32 n)
  let int64_of_arc a = Int64.logand (Int64.of_int32 (UInt32.to_int32 a)) max_arc

  (* X.690 8.19.4 packs the first two arcs into one subidentifier, which
     everparse-asn1 reads below 2^32: the second arc is below 40 under 0 and
     1, and below 2^32 - 80 under 2 (spec/ASN1.Base.fst:193-194). *)
  let head_ok a b =
    Int64.compare b 0L >= 0
    && (Int64.compare a 0L >= 0
        && Int64.compare a 2L < 0
        && Int64.compare b 40L < 0
       || (Int64.equal a 2L && Int64.compare b (Int64.sub max_arc 79L) < 0))

  let base a b =
    let a64 = Int64.of_int a and b64 = Int64.of_int b in
    if not (head_ok a64 b64) then
      Fmt.invalid_arg
        "Wire.Asn1.Oid.base %d %d: the first arc is 0, 1 or 2, and the second \
         is below 40 under 0 or 1 and below 2^32 - 80 under 2 (X.690 8.19.4)"
        a b;
    [| arc a64; arc b64 |]

  let checked_arc n =
    let n64 = Int64.of_int n in
    if not (in_range n64) then
      Fmt.invalid_arg
        "Wire.Asn1.Oid.( <| ) %d: an arc is between 0 and 2^32 - 1, the \
         subidentifiers everparse-asn1 reads"
        n;
    arc n64

  let ( <| ) o n = Array.append o [| checked_arc n |]
  let ( <|| ) o ns = Array.append o (Array.of_list (List.map checked_arc ns))

  let compare a b =
    let la = Array.length a and lb = Array.length b in
    let rec go i =
      if i = la || i = lb then Int.compare la lb
      else match UInt32.compare a.(i) b.(i) with 0 -> go (i + 1) | c -> c
    in
    go 0

  let equal a b = compare a b = 0

  (* A decimal arc: digits only, no leading zero, below 2^32. Ten digits do not
     overflow an [int64]. *)
  let arc_of_string s =
    let n = String.length s in
    let digit c = Int64.of_int (Char.code c - Char.code '0') in
    let rec go v i =
      if i = n then if in_range v then Some v else None
      else
        match s.[i] with
        | '0' .. '9' as c -> go (Int64.add (Int64.mul v 10L) (digit c)) (i + 1)
        | _ -> None
    in
    if n = 0 || n > 10 || (n > 1 && Char.equal s.[0] '0') then None else go 0L 0

  let of_string s =
    let rec arcs acc = function
      | [] -> Some (List.rev acc)
      | s :: rest -> (
          match arc_of_string s with
          | None -> None
          | Some a -> arcs (a :: acc) rest)
    in
    match arcs [] (String.split_on_char '.' s) with
    | Some (a :: b :: _ as l) when head_ok a b ->
        Some (Array.of_list (List.map arc l))
    | _ -> None

  let to_string o =
    String.concat "."
      (Array.to_list (Array.map (fun a -> Int64.to_string (int64_of_arc a)) o))

  let pp ppf o = Fmt.string ppf (to_string o)
end

(* Tags. An identifier is a class, a number and the constructed bit, as
   everparse-asn1's [asn1_id_t] (spec/ASN1.Base.fst): a SEQUENCE and a CHOICE
   tell components apart by all three, a SET by the first two
   (spec/ASN1.Base.fst:522-525). *)

type tag_class = Universal | Application | Context | Private
type tag = { cls : tag_class; constructed : bool; number : int }

let equal_class a b =
  match (a, b) with
  | Universal, Universal
  | Application, Application
  | Context, Context
  | Private, Private ->
      true
  | (Universal | Application | Context | Private), _ -> false

let same_tag a b = equal_class a.cls b.cls && Int.equal a.number b.number
let same_id a b = same_tag a b && Bool.equal a.constructed b.constructed

let universal ?(constructed = false) number =
  { cls = Universal; constructed; number }

let pp_tag ppf t =
  match t.cls with
  | Universal -> Fmt.pf ppf "[UNIVERSAL %d]" t.number
  | Application -> Fmt.pf ppf "[APPLICATION %d]" t.number
  | Context -> Fmt.pf ppf "[%d]" t.number
  | Private -> Fmt.pf ppf "[PRIVATE %d]" t.number

(* The ASN.1 types. *)

type chars =
  | Utf8
  | Printable
  | Ia5
  | Numeric
  | Visible
  | Teletex
  | Bmp
  | Universal_chars

type time = Utc | Generalized

type _ prim =
  | Bool : bool prim
  | Integer : string prim
  | Bounded_integer : { min : int64; max : int64 } -> int64 prim
  | Enumerated : (string * int) list -> int prim
  | Null : unit prim
  | Oid : Oid.t prim
  | Bit_string : (int * string) prim
  | Octet_string : string prim
  | Chars : chars -> string prim
  | Time : time -> string prim

type _ t =
  | Prim : 'a prim -> 'a t
  | Any : string t
  | Size : { min : int; max : int; inner : string t } -> string t
  | Implicit : { tag : tag; inner : 'a t } -> 'a t
  | Explicit : { tag : tag; inner : 'a t } -> 'a t
  | Sequence : 'r record -> 'r t
  | Set : 'r record -> 'r t
  | Sequence_of : { non_empty : bool; elem : 'a t } -> 'a list t
  | Set_of : { non_empty : bool; elem : 'a t } -> 'a list t
  | Choice : 'a alternative list -> 'a t
  | Keep_bytes : 'a t -> ('a * string) t
  | Containing : 'a Types.typ -> 'a t
  | Map : { inner : 'w t; decode : 'w -> 'a; encode : 'a -> 'w } -> 'a t

(* A SEQUENCE or SET: [fields] builds the record with [make], [components] are
   the same fields as this module reads them, in order. *)
and 'r record =
  | Record : {
      name : string;
      make : 'f;
      fields : ('f, 'r) Codec.fields;
      components : component list;
    }
      -> 'r record

and 'a alternative =
  | Alternative : {
      name : string;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> 'a alternative

(* A component of a SEQUENCE or SET, as [required], [optional], [default] and
   [defined_by] make it. The key of a [defined_by] component is the field
   whose value selects the case, known by its [Field.id]. *)
and _ decorated =
  | Required : 'a t -> 'a decorated
  | Optional : 'a t -> 'a option decorated
  | Default : { value : 'a; typ : 'a t } -> 'a decorated
  | Defined_by : {
      key_name : string;
      key_id : int;
      key_typ : 'k t;
      cases : ('a, 'k) case list;
    }
      -> 'a decorated

and ('a, 'k) case =
  | Case : {
      key : 'k;
      typ : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> ('a, 'k) case
  | Absent : { key : 'k; inject : 'a; project : 'a -> bool } -> ('a, 'k) case
  | Fallback : {
      optional : bool;
      typ : 'w t;
      inject : 'k -> 'w option -> 'a;
      project : 'a -> ('k * 'w option) option;
    }
      -> ('a, 'k) case

and component =
  | Component : {
      name : string;
      id : int;
      decorated : 'a decorated;
    }
      -> component

type _ Types.der += Component_of : 'a decorated -> 'a Types.der

(* The tags a value of a type can start with. [All] is ANY's: every tag. *)
type tags = All | Tags of tag list

let prim_number : type a. a prim -> int = function
  | Bool -> 1
  | Integer | Bounded_integer _ -> 2
  | Bit_string -> 3
  | Octet_string -> 4
  | Null -> 5
  | Oid -> 6
  | Enumerated _ -> 10
  | Chars Utf8 -> 12
  | Chars Numeric -> 18
  | Chars Printable -> 19
  | Chars Teletex -> 20
  | Chars Ia5 -> 22
  | Time Utc -> 23
  | Time Generalized -> 24
  | Chars Visible -> 26
  | Chars Universal_chars -> 28
  | Chars Bmp -> 30

let union a b =
  match (a, b) with All, _ | _, All -> All | Tags a, Tags b -> Tags (a @ b)

let rec tags : type a. a t -> tags = function
  | Prim p -> Tags [ universal (prim_number p) ]
  | Any -> All
  | Size { inner; _ } -> tags inner
  | Implicit { tag; _ } | Explicit { tag; _ } -> Tags [ tag ]
  | Sequence _ | Sequence_of _ -> Tags [ universal ~constructed:true 16 ]
  | Set _ | Set_of _ -> Tags [ universal ~constructed:true 17 ]
  | Choice alts ->
      List.fold_left
        (fun acc (Alternative a) -> union acc (tags a.typ))
        (Tags []) alts
  | Keep_bytes t -> tags t
  | Containing _ -> Tags [ universal (prim_number Octet_string) ]
  | Map { inner; _ } -> tags inner

(* The tag the two sets share, as text, or [None] if they are disjoint under
   [same]. *)
let meet same a b =
  match (a, b) with
  | Tags [], _ | _, Tags [] -> None
  | All, All -> Some "any tag"
  | All, Tags (t :: _) | Tags (t :: _), All ->
      Fmt.kstr Option.some "%a" pp_tag t
  | Tags a, Tags b ->
      List.find_opt (fun x -> List.exists (same x) b) a
      |> Option.map (Fmt.str "%a" pp_tag)

(* Primitives. *)

let bool = Prim Bool
let integer = Prim Integer

let bounded_integer ~min ~max =
  if Int64.compare min max > 0 then
    Fmt.invalid_arg
      "Wire.Asn1.bounded_integer: INTEGER (%Ld..%Ld) is empty, its lower bound \
       is above its upper bound"
      min max;
  Prim (Bounded_integer { min; max })

let enumerated ns =
  if List.is_empty ns then
    invalid_arg "Wire.Asn1.enumerated: an ENUMERATED has at least one value";
  let rec check = function
    | [] -> ()
    | (name, n) :: rest ->
        if List.exists (fun (name', _) -> String.equal name name') rest then
          Fmt.invalid_arg
            "Wire.Asn1.enumerated: the name %S is given twice; each named \
             number has its own name"
            name;
        if List.exists (fun (_, n') -> Int.equal n n') rest then
          Fmt.invalid_arg
            "Wire.Asn1.enumerated: the number %d is given twice; each named \
             number has its own value"
            n;
        check rest
  in
  check ns;
  Prim (Enumerated ns)

let null = Prim Null
let oid = Prim Oid
let bit_string = Prim Bit_string
let octet_string = Prim Octet_string
let utf8_string = Prim (Chars Utf8)
let printable_string = Prim (Chars Printable)
let ia5_string = Prim (Chars Ia5)
let numeric_string = Prim (Chars Numeric)
let visible_string = Prim (Chars Visible)
let teletex_string = Prim (Chars Teletex)
let bmp_string = Prim (Chars Bmp)
let universal_string = Prim (Chars Universal_chars)
let utc_time = Prim (Time Utc)
let generalized_time = Prim (Time Generalized)
let any = Any

(* Constraints. SIZE bounds are below 65536 and form one range
   (everparse-asn1 README:460-461,487); SIZE applies to OCTET STRING and the
   character strings (README:452-458). *)

let size_limit = 65536

let size ~min ~max t =
  (match t with
  | Prim (Octet_string | Chars _) -> ()
  | _ ->
      Fmt.invalid_arg
        "Wire.Asn1.size: SIZE (%d..%d) applies to an OCTET STRING or a \
         character string only (everparse-asn1 README:452-458); a SEQUENCE OF \
         or SET OF takes ?non_empty, the one SIZE it supports \
         (README:499-500), and a type takes one SIZE"
        min max);
  if min < 0 || min > max || max >= size_limit then
    Fmt.invalid_arg
      "Wire.Asn1.size: SIZE (%d..%d) is not a range 0 <= min <= max < 65536, \
       the bounds everparse-asn1 supports (README:460-461,487)"
      min max;
  Size { min; max; inner = t }

(* Tags. *)

let tag_number ~combinator n =
  if not (Oid.in_range (Int64.of_int n)) then
    Fmt.invalid_arg
      "Wire.Asn1.%s: tag number %d is not between 0 and 2^32 - 1, the tag \
       numbers everparse-asn1 reads (spec/ASN1.Spec.IdentifierU32.fst)"
      combinator n

let tag_class ~combinator ?cls n =
  let c =
    match cls with
    | None -> Context
    | Some `Universal -> Universal
    | Some `Application -> Application
    | Some `Private -> Private
  in
  tag_number ~combinator n;
  if equal_class c Universal && n = 0 then
    Fmt.invalid_arg
      "Wire.Asn1.%s: tag [UNIVERSAL 0] is reserved for the encoding rules \
       (X.680 8.6, spec/ASN1.Syntax.fst:10-12)"
      combinator;
  c

(* The constructed bit an IMPLICIT tag takes from the type it replaces the tag
   of, or why it has no tag to replace. *)
let rec implicit_flag : type a. a t -> (bool, string) result = function
  | Prim _ | Size _ -> Ok false
  | Implicit { tag; _ } -> Ok tag.constructed
  | Explicit _ | Sequence _ | Set _ | Sequence_of _ | Set_of _ -> Ok true
  | Keep_bytes t -> implicit_flag t
  | Map { inner; _ } -> implicit_flag inner
  | Choice _ ->
      Error
        "a CHOICE, which has no tag of its own to replace \
         (spec/ASN1.Syntax.fst:136); tag it EXPLICIT"
  | Any ->
      Error
        "ANY, which has no tag of its own to replace \
         (spec/ASN1.Syntax.fst:136); tag it EXPLICIT"
  | Containing _ ->
      Error
        "an OCTET STRING (CONTAINING), whose content everparse-asn1 reads \
         under the UNIVERSAL 4 tag only (spec/ASN1.Syntax.fst:40-48)"

let implicit ?cls n t =
  let cls = tag_class ~combinator:"implicit" ?cls n in
  match implicit_flag t with
  | Ok constructed ->
      Implicit { tag = { cls; constructed; number = n }; inner = t }
  | Error why -> Fmt.invalid_arg "Wire.Asn1.implicit: [%d] IMPLICIT on %s" n why

let explicit ?cls n t =
  let cls = tag_class ~combinator:"explicit" ?cls n in
  Explicit { tag = { cls; constructed = true; number = n }; inner = t }

(* Components. *)

let component name decorated = Field.v name (Types.Der (Component_of decorated))
let required name t = component name (Required t)
let optional name t = component name (Optional t)

(* The characters SIZE counts (README:452-458): code points for UTF8String,
   BMPString and UniversalString, octets otherwise. *)
let char_count k s =
  match k with
  | Some Utf8 ->
      String.fold_left
        (fun n c -> if Char.code c land 0xC0 = 0x80 then n else n + 1)
        0 s
  | Some Bmp -> String.length s / 2
  | Some Universal_chars -> String.length s / 4
  | Some (Printable | Ia5 | Numeric | Visible | Teletex) | None ->
      String.length s

let in_size ~min ~max (inner : string t) v =
  let k = match inner with Prim (Chars k) -> Some k | _ -> None in
  let n = char_count k v in
  n >= min && n <= max

(* DEFAULT applies to a terminal type other than OBJECT IDENTIFIER, under at
   most one EXPLICIT tag and then unconstrained (everparse-asn1
   README:464-467,496-498; a constrained DEFAULT under EXPLICIT has no
   literal, spec/ASN1.Base.fst:688), with a value its constraint admits
   (spec/ASN1.Base.fst:745). *)
let rec check_default : type a. string -> explicit:bool -> a t -> a -> unit =
 fun name ~explicit t v ->
  let fail fmt =
    Fmt.kstr (fun m -> Fmt.invalid_arg "Wire.Asn1.default %S: %s" name m) fmt
  in
  let unconstrained what =
    if explicit then
      fail
        "DEFAULT on %s under an EXPLICIT tag, which everparse-asn1 supports \
         unconstrained only (README:464-467,496-498)"
        what
  in
  match t with
  | Map { inner; encode; _ } -> check_default name ~explicit inner (encode v)
  | Implicit { inner; _ } -> check_default name ~explicit inner v
  | Explicit { inner; _ } ->
      if explicit then
        fail
          "DEFAULT under two EXPLICIT tags; everparse-asn1 supports one \
           (README:464-467)";
      check_default name ~explicit:true inner v
  | Keep_bytes _ ->
      fail
        "keep_bytes on a DEFAULT component, which everparse-asn1 refuses \
         (README:234-240)"
  | Prim Oid ->
      fail
        "DEFAULT on an OBJECT IDENTIFIER, which everparse-asn1 does not \
         support (README:464-467)"
  | Prim (Bounded_integer { min; max }) ->
      unconstrained "a constrained INTEGER";
      if Int64.compare v min < 0 || Int64.compare v max > 0 then
        fail "the default %Ld is outside INTEGER (%Ld..%Ld)" v min max
  | Prim (Enumerated ns) ->
      unconstrained "an ENUMERATED";
      if not (List.exists (fun (_, n) -> Int.equal n v) ns) then
        fail "the default %d is not a value of the ENUMERATED" v
  | Prim _ -> ()
  | Size { min; max; inner } ->
      unconstrained "a SIZE-constrained string";
      if not (in_size ~min ~max inner v) then
        fail "the default is outside SIZE (%d..%d)" min max
  | Any | Sequence _ | Set _ | Sequence_of _ | Set_of _ | Choice _
  | Containing _ ->
      fail
        "DEFAULT on a constructed type or ANY; everparse-asn1 supports DEFAULT \
         on terminal types only (README:464-467,496-498)"

let default name v t =
  check_default name ~explicit:false t v;
  component name (Default { value = v; typ = t })

(* SEQUENCE and SET. *)

let rec components : type f r.
    combinator:string -> string -> (f, r) Codec.fields -> component list =
 fun ~combinator name fields ->
  match fields with
  | Codec.[] -> []
  | Codec.(f :: rest) ->
      let c =
        match Codec.field_typ f with
        | Types.Der (Component_of decorated) ->
            Component
              { name = Codec.field_name f; id = Codec.field_id f; decorated }
        | _ ->
            Fmt.invalid_arg
              "Wire.Asn1.%s %S: field %S is not an ASN.1 component; make it \
               with Asn1.required, optional, default or defined_by"
              combinator name (Codec.field_name f)
      in
      c :: components ~combinator name rest

let reject_repeated_names ~combinator name comps =
  let rec go = function
    | [] -> ()
    | Component c :: rest ->
        if List.exists (fun (Component c') -> String.equal c.name c'.name) rest
        then
          Fmt.invalid_arg "Wire.Asn1.%s %S: component %S is given twice"
            combinator name c.name;
        go rest
  in
  go comps

(* An item of a SEQUENCE as a decoder meets it: whether it must be present,
   and the tags it can start with. *)
type item = { iname : string; mandatory : bool; itags : tags }

let case_item : type a k. string -> (a, k) case -> item option =
 fun name -> function
  | Case { typ; _ } -> Some { iname = name; mandatory = true; itags = tags typ }
  | Absent _ -> None
  | Fallback { optional; typ; _ } ->
      Some { iname = name; mandatory = not optional; itags = tags typ }

let items_of comps =
  (* One list of items per case of the [defined_by] component, if any: each
     case puts its own type in the component's place (README:298-312). *)
  let plain (Component c) =
    match c.decorated with
    | Required t -> Some { iname = c.name; mandatory = true; itags = tags t }
    | Optional t -> Some { iname = c.name; mandatory = false; itags = tags t }
    | Default { typ; _ } ->
        Some { iname = c.name; mandatory = false; itags = tags typ }
    | Defined_by _ -> None
  in
  let expand (Component c) =
    match c.decorated with
    | Defined_by { cases; _ } ->
        Some (List.map (fun case -> case_item c.name case) cases)
    | Required _ | Optional _ | Default _ -> None
  in
  match List.find_map expand comps with
  | None | Some [] -> [ List.filter_map plain comps ]
  | Some cases ->
      List.map
        (fun item ->
          List.filter_map
            (fun (Component c as comp) ->
              match c.decorated with
              | Defined_by _ -> item
              | Required _ | Optional _ | Default _ -> plain comp)
            comps)
        cases

(* An OPTIONAL or DEFAULT component starts a run that every later component
   joins until a mandatory one ends it, and the components of a run have
   pairwise disjoint tags (spec/ASN1.Base.fst:479-496): else the decoder
   cannot tell which one is present. everparse-asn1 finds an overlap only when
   F* fails (README:502-504). *)
let reject_ambiguous_runs name items =
  let rec go run = function
    | [] -> ()
    | it :: rest ->
        List.iter
          (fun prev ->
            match meet same_id prev.itags it.itags with
            | None -> ()
            | Some t ->
                Fmt.invalid_arg
                  "Wire.Asn1.sequence %S: component %S and the OPTIONAL or \
                   DEFAULT component %S before it can both start with %s, so a \
                   decoder cannot tell which is present \
                   (spec/ASN1.Base.fst:479-496)"
                  name it.iname prev.iname t)
          run;
        if it.mandatory then go [] rest else go (it :: run) rest
  in
  go [] items

(* A SEQUENCE holds at most one [defined_by], whose key is a component before
   it in the same SEQUENCE (asn1.rst:648-652). *)
let reject_misplaced_tables name comps =
  let tables =
    List.filter_map
      (fun (Component c) ->
        match c.decorated with
        | Defined_by { key_name; key_id; _ } -> Some (c.name, key_name, key_id)
        | Required _ | Optional _ | Default _ -> None)
      comps
  in
  let index id =
    let rec go i = function
      | [] -> None
      | Component c :: rest ->
          if Int.equal c.id id then Some i else go (i + 1) rest
    in
    go 0 comps
  in
  let position cname =
    let rec go i = function
      | [] -> i
      | Component c :: rest ->
          if String.equal c.name cname then i else go (i + 1) rest
    in
    go 0 comps
  in
  match tables with
  | [] -> ()
  | (cname, kname, kid) :: rest -> (
      (match rest with
      | [] -> ()
      | (cname', _, _) :: _ ->
          Fmt.invalid_arg
            "Wire.Asn1.sequence %S: components %S and %S are both defined by a \
             key; everparse-asn1 takes one table per SEQUENCE \
             (asn1.rst:648-652)"
            name cname cname');
      match index kid with
      | None ->
          Fmt.invalid_arg
            "Wire.Asn1.sequence %S: the key %S of component %S is not a \
             component of this SEQUENCE; everparse-asn1 takes the key from the \
             SEQUENCE of its component (asn1.rst:648-652)"
            name kname cname
      | Some k when k > position cname ->
          Fmt.invalid_arg
            "Wire.Asn1.sequence %S: the key %S comes after component %S, which \
             it defines; everparse-asn1 reads the key first (asn1.rst:648-652)"
            name kname cname
      | Some _ -> ())

let sequence name make fields =
  let comps = components ~combinator:"sequence" name fields in
  reject_repeated_names ~combinator:"sequence" name comps;
  reject_misplaced_tables name comps;
  List.iter (reject_ambiguous_runs name) (items_of comps);
  Sequence (Record { name; make; fields; components = comps })

(* A SET has at most 64 components (README:463) with pairwise distinct tags,
   whatever their constructed bit (spec/ASN1.Syntax.fst:281,
   spec/ASN1.Base.fst:522-539), and no [defined_by] (asn1.rst:648). *)
let set_limit = 64

let set name make fields =
  let comps = components ~combinator:"set" name fields in
  reject_repeated_names ~combinator:"set" name comps;
  let n = List.length comps in
  if n > set_limit then
    Fmt.invalid_arg
      "Wire.Asn1.set %S: %d components; everparse-asn1 takes at most 64 in a \
       SET (README:463)"
      name n;
  List.iter
    (fun (Component c) ->
      match c.decorated with
      | Defined_by _ ->
          Fmt.invalid_arg
            "Wire.Asn1.set %S: component %S is defined by a key; \
             everparse-asn1 takes a table on a SEQUENCE only (asn1.rst:648)"
            name c.name
      | Required _ | Optional _ | Default _ -> ())
    comps;
  let items = List.concat (items_of comps) in
  let rec distinct = function
    | [] -> ()
    | it :: rest ->
        List.iter
          (fun other ->
            match meet same_tag it.itags other.itags with
            | None -> ()
            | Some t ->
                Fmt.invalid_arg
                  "Wire.Asn1.set %S: components %S and %S can both start with \
                   %s; the components of a SET have distinct tags \
                   (spec/ASN1.Syntax.fst:281)"
                  name it.iname other.iname t)
          rest;
        distinct rest
  in
  distinct items;
  Set (Record { name; make; fields; components = comps })

let sequence_of ?(non_empty = false) t = Sequence_of { non_empty; elem = t }
let set_of ?(non_empty = false) t = Set_of { non_empty; elem = t }

(* CHOICE. *)

let rec kept : type a. a t -> bool = function
  | Keep_bytes _ -> true
  | Map { inner; _ } -> kept inner
  | _ -> false

let alternative name t ~inject ~project =
  if kept t then
    Fmt.invalid_arg
      "Wire.Asn1.alternative %S: keep_bytes on a CHOICE alternative, which \
       everparse-asn1 refuses (README:234-240)"
      name;
  Alternative { name; typ = t; inject; project }

let choice alts =
  if List.is_empty alts then
    invalid_arg "Wire.Asn1.choice: a CHOICE has at least one alternative";
  let rec check = function
    | [] -> ()
    | Alternative a :: rest ->
        (match tags a.typ with
        | All ->
            Fmt.invalid_arg
              "Wire.Asn1.choice: alternative %S is ANY, which everparse-asn1 \
               refuses in a CHOICE (README:493)"
              a.name
        | Tags _ -> ());
        List.iter
          (fun (Alternative b) ->
            if String.equal a.name b.name then
              Fmt.invalid_arg "Wire.Asn1.choice: alternative %S is given twice"
                a.name;
            match meet same_id (tags a.typ) (tags b.typ) with
            | None -> ()
            | Some t ->
                Fmt.invalid_arg
                  "Wire.Asn1.choice: alternatives %S and %S can both start \
                   with %s; the alternatives of a CHOICE have distinct tags \
                   (spec/ASN1.Syntax.fst:149)"
                  a.name b.name t)
          rest;
        check rest
  in
  check alts;
  Choice alts

(* Components defined by a key. *)

let case k t ~inject ~project = Case { key = k; typ = t; inject; project }
let absent k ~inject ~project = Absent { key = k; inject; project }

let fallback ?(optional = false) t ~inject ~project =
  Fallback { optional; typ = t; inject; project }

let equal_prim : type a. a prim -> a -> a -> bool =
 fun p a b ->
  match p with
  | Bool -> Bool.equal a b
  | Integer -> String.equal a b
  | Bounded_integer _ -> Int64.equal a b
  | Enumerated _ -> Int.equal a b
  | Null -> true
  | Oid -> Oid.equal a b
  | Bit_string ->
      let ua, sa = a and ub, sb = b in
      Int.equal ua ub && String.equal sa sb
  | Octet_string -> String.equal a b
  | Chars _ -> String.equal a b
  | Time _ -> String.equal a b

(* A key is a mandatory component of a terminal type other than RELATIVE-OID,
   without constraints, under any tags (README:300-302). Its values are
   compared as they are encoded. *)
type 'k key_shape = Key of ('k -> 'k -> bool) | Not_key of string

let rec key_shape : type k. k t -> k key_shape = function
  | Prim (Bounded_integer _ | Enumerated _) | Size _ ->
      Not_key "constrained; everparse-asn1 takes an unconstrained key"
  | Prim p -> Key (equal_prim p)
  | Implicit { inner; _ } | Explicit { inner; _ } -> key_shape inner
  | Map { inner; encode; _ } -> (
      match key_shape inner with
      | Key eq -> Key (fun a b -> eq (encode a) (encode b))
      | Not_key _ as n -> n)
  | Keep_bytes _ ->
      Not_key "under keep_bytes, which everparse-asn1 refuses on a key"
  | Any | Sequence _ | Set _ | Sequence_of _ | Set_of _ | Choice _
  | Containing _ ->
      Not_key "not a primitive type; everparse-asn1 takes a terminal key"

let defined_by : type a k.
    string -> on:k Field.t -> (a, k) case list -> a Field.t =
 fun name ~on cases ->
  let fail fmt =
    Fmt.kstr
      (fun m ->
        Fmt.invalid_arg
          "Wire.Asn1.defined_by %S: %s (README:298-302, asn1.rst:648-652)" name
          m)
      fmt
  in
  let kname = Field.name on in
  let key_typ : k t =
    match Field.typ on with
    | Types.Der (Component_of (Required t)) -> t
    | Types.Der (Component_of (Optional _)) ->
        fail "the key %S is OPTIONAL" kname
    | Types.Der (Component_of (Default _)) -> fail "the key %S is DEFAULT" kname
    | Types.Der (Component_of (Defined_by _)) ->
        fail "the key %S is itself defined by a key" kname
    | _ -> fail "the key %S is not an ASN.1 component" kname
  in
  let equal =
    match key_shape key_typ with
    | Key eq -> eq
    | Not_key why -> fail "the key %S is %s" kname why
  in
  let key_of : (a, k) case -> k option = function
    | Case { key; _ } | Absent { key; _ } -> Some key
    | Fallback _ -> None
  in
  let same_key k c = Option.fold ~none:false ~some:(equal k) (key_of c) in
  let rec check ~fallback = function
    | [] -> ()
    | c :: rest ->
        (match key_of c with
        | Some k when List.exists (same_key k) rest ->
            fail "a key is given twice"
        | Some _ | None -> ());
        (match c with
        | Case { typ; _ } when kept typ ->
            fail
              "keep_bytes on the type of a case, which everparse-asn1 refuses"
        | Fallback _ when fallback -> fail "a second fallback"
        | Fallback { typ; _ } when kept typ ->
            fail "keep_bytes on the fallback, which everparse-asn1 refuses"
        | Case _ | Absent _ | Fallback _ -> ());
        check ~fallback:(fallback || Option.is_none (key_of c)) rest
  in
  check ~fallback:false cases;
  component name
    (Defined_by { key_name = kname; key_id = Field.id on; key_typ; cases })

(* Bytes. *)

let keep_bytes t = Keep_bytes t
let containing t = Containing t
let map ~decode ~encode t = Map { inner = t; decode; encode }

(* The decoder and encoder, lane 2. *)

let codec _ = todo ~lane:"2 (the DER decoder and encoder)" "codec"

let fold _ _ _ _ ~init:_ ~f:_ =
  todo ~lane:"2 (the DER decoder and encoder)" "fold"

(* The projection to everparse-asn1, lane 3. *)

type schema = unit

let printer = "3 (the .asn printer)"
let project _ _ = todo ~lane:printer "project"
let contained _ = todo ~lane:printer "contained"
let name _ = todo ~lane:printer "name"
let filename _ = todo ~lane:printer "filename"
let roots _ = todo ~lane:printer "roots"
let pp _ _ = todo ~lane:printer "pp"
let write ~outdir:_ _ = todo ~lane:printer "write"
