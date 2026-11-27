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

(* The DER codec.

   [codec] compiles an ASN.1 type once into a [node] of closures, then reads,
   checks and writes through them. It accepts exactly what everparse-asn1's
   specification accepts (lib/everparse/asn1/spec/ at EverParse 49875441c,
   [spec/] below): docs/asn1.md, "DER rules and who enforces them", lists the
   rules and where the specification states each. A check raises the parse
   error of the first rule the bytes break; a read trusts bytes a check
   accepted; a write raises [Invalid_argument] on a value whose bytes a check
   would refuse. *)

open Der

let class_of id0 =
  match id0 lsr 6 with
  | 0 -> Universal
  | 1 -> Application
  | 2 -> Context
  | _ -> Private

let id_matches (t : tag) id =
  equal_class t.cls (class_of id.id0)
  && Bool.equal t.constructed (constructed_of id.id0)
  && Int64.equal (Int64.of_int t.number) id.number

let tags_match ts id =
  match ts with
  | All -> true
  | Tags ts -> List.exists (fun t -> id_matches t id) ts

let expect (t : tag) buf pos lim =
  let h = header buf pos lim in
  if not (id_matches t h.hid) then
    Types.raise_invalid_tag ~at:pos (Int64.to_int h.hid.number);
  h

let class_bits = function
  | Universal -> 0
  | Application -> 1
  | Context -> 2
  | Private -> 3

let chars_ok k s =
  match k with
  | Utf8 -> utf8_ok s
  | Printable -> String.for_all printable_char s
  | Ia5 -> String.for_all (fun c -> Char.code c < 128) s
  | Numeric -> String.for_all numeric_char s
  | Visible -> String.for_all visible_char s
  | Teletex -> true
  | Bmp -> bmp_ok s
  | Universal_chars -> universal_ok s

let time_ok k s =
  match k with Utc -> utc_ok s | Generalized -> generalized_ok s

let tlv (t : tag) content =
  Der.tlv ~cls:(class_bits t.cls) ~constructed:t.constructed ~number:t.number
    content

let oid_content (o : Oid.t) =
  let b = Buffer.create 16 in
  let arc i = Oid.int64_of_arc o.(i) in
  base128 b (Int64.add (Int64.mul (arc 0) 40L) (arc 1));
  for i = 2 to Array.length o - 1 do
    base128 b (arc i)
  done;
  Buffer.contents b

(* Primitives. *)

let check_prim : type a. a prim -> bytes -> int -> int -> unit =
 fun p buf cs ce ->
  let n = ce - cs in
  match p with
  | Bool ->
      (* X.690 11.1, spec/ASN1.Spec.Content.BOOLEAN.fst:13-22 *)
      if not (n = 1 && (byte buf cs = 0 || byte buf cs = 0xFF)) then
        rule_broken ~at:cs
  | Integer ->
      (* X.690 8.3.2, spec/ASN1.Spec.Content.INTEGER.fst:682-692 *)
      if not (integer_ok buf cs ce) then rule_broken ~at:cs
  | Bounded_integer { min; max } -> (
      if not (integer_ok buf cs ce) then rule_broken ~at:cs;
      match int64_of_content buf cs ce with
      | Some v when Int64.compare v min >= 0 && Int64.compare v max <= 0 -> ()
      | Some v -> Types.raise_error ~at:cs (Value_out_of_range { value = v })
      | None ->
          let value =
            if byte buf cs >= 0x80 then Int64.min_int else Int64.max_int
          in
          Types.raise_error ~at:cs (Value_out_of_range { value }))
  | Enumerated ns -> (
      (* X.690 8.4, spec/ASN1.Syntax.fst:250-253 *)
      if not (integer_ok buf cs ce) then rule_broken ~at:cs;
      let valid = List.map snd ns in
      match int64_of_content buf cs ce with
      | Some v when List.exists (fun k -> Int64.equal (Int64.of_int k) v) valid
        ->
          ()
      | v ->
          Types.raise_error ~at:cs
            (Invalid_enum
               { value = Option.fold ~none:(-1) ~some:Int64.to_int v; valid }))
  | Null -> if n <> 0 then rule_broken ~at:cs
  | Oid -> check_subidentifiers buf cs ce
  | Bit_string ->
      (* X.690 11.2.1, spec/ASN1.Spec.Content.BITSTRING.fst:13-24 *)
      if n = 0 then rule_broken ~at:cs;
      let u = byte buf cs in
      if u > 7 || (u > 0 && n < 2) then rule_broken ~at:cs;
      if u > 0 && byte buf (ce - 1) land ((1 lsl u) - 1) <> 0 then
        Types.raise_error ~at:(ce - 1) Non_zero_padding
  | Octet_string -> ()
  | Chars k ->
      if not (chars_ok k (Bytes.sub_string buf cs n)) then rule_broken ~at:cs
  | Time k ->
      if not (time_ok k (Bytes.sub_string buf cs n)) then rule_broken ~at:cs

let read_oid buf cs ce =
  let first, p = subidentifier buf cs ce in
  let a, b =
    if Int64.compare first 40L < 0 then (0L, first)
    else if Int64.compare first 80L < 0 then (1L, Int64.sub first 40L)
    else (2L, Int64.sub first 80L)
  in
  let rec arcs acc p =
    if p >= ce then List.rev acc
    else
      let v, p = subidentifier buf p ce in
      arcs (Oid.arc v :: acc) p
  in
  Array.of_list (Oid.arc a :: Oid.arc b :: arcs [] p)

let read_prim : type a. a prim -> bytes -> int -> int -> a =
 fun p buf cs ce ->
  let n = ce - cs in
  match p with
  | Bool -> byte buf cs = 0xFF
  | Integer -> Bytes.sub_string buf cs n
  | Bounded_integer _ -> Option.value ~default:0L (int64_of_content buf cs ce)
  | Enumerated ns -> (
      let v = Option.value ~default:0L (int64_of_content buf cs ce) in
      match List.find_opt (fun (_, k) -> Int64.equal (Int64.of_int k) v) ns with
      | Some (_, k) -> k
      | None -> Types.raise_invalid_tag ~at:cs (Int64.to_int v))
  | Null -> ()
  | Oid -> read_oid buf cs ce
  | Bit_string -> (byte buf cs, Bytes.sub_string buf (cs + 1) (n - 1))
  | Octet_string -> Bytes.sub_string buf cs n
  | Chars _ -> Bytes.sub_string buf cs n
  | Time _ -> Bytes.sub_string buf cs n

let prim_content : type a. a prim -> a -> string =
 fun p v ->
  match p with
  | Bool -> if v then "\xff" else "\x00"
  | Integer ->
      if not (minimal_integer v) then
        cannot_write
          "INTEGER content %S is not minimal two's complement (X.690 8.3.2)" v;
      v
  | Bounded_integer { min; max } ->
      if Int64.compare v min < 0 || Int64.compare v max > 0 then
        cannot_write "%Ld is outside INTEGER (%Ld..%Ld)" v min max;
      integer_of_int64 v
  | Enumerated ns ->
      if not (List.exists (fun (_, k) -> Int.equal k v) ns) then
        cannot_write "%d is not a value of the ENUMERATED" v;
      integer_of_int64 (Int64.of_int v)
  | Null -> ""
  | Oid -> oid_content v
  | Bit_string ->
      let u, s = v in
      let n = String.length s in
      if u < 0 || u > 7 || (u > 0 && n = 0) then
        cannot_write "BIT STRING with %d unused bits in %d octets (X.690 8.6.2)"
          u n;
      if u > 0 && Char.code s.[n - 1] land ((1 lsl u) - 1) <> 0 then
        cannot_write
          "BIT STRING whose %d unused bits are not zero (X.690 11.2.1)" u;
      String.make 1 (Char.chr u) ^ s
  | Octet_string -> v
  | Chars k ->
      if not (chars_ok k v) then
        cannot_write "%S is outside the alphabet of its string type (X.680 41)"
          v;
      v
  | Time k ->
      if not (time_ok k v) then
        cannot_write "%S is not a DER time (X.690 11.7, 11.8)" v;
      v

(* Nodes. [check buf pos lim] checks the value at [pos], which ends before
   [lim], and is where it ends; [read buf s e] reads the value [check]
   accepted between [s] and [e]; [der v] is the encoding of [v]. A type with a
   tag of its own ([own]) also has the same three over its contents, which
   IMPLICIT keeps under another tag: [read_content buf s cs ce] reads the
   contents [cs]..[ce] of the value at [s]. *)
type 'a node = {
  own : tag option;
  starts : tags;
  check : bytes -> int -> int -> int;
  read : bytes -> int -> int -> 'a;
  der : 'a -> string;
  check_content : bytes -> int -> int -> unit;
  read_content : bytes -> int -> int -> int -> 'a;
  content : 'a -> string;
}

let no_contents ~at = Types.raise_invalid_tag ~at 0
let no_content _ = cannot_write "a CHOICE or ANY has no contents of its own"

let tagged tag ~check_content ~read_content ~content =
  {
    own = Some tag;
    starts = Tags [ tag ];
    check =
      (fun buf pos lim ->
        let h = expect tag buf pos lim in
        check_content buf h.cstart h.cend;
        h.cend);
    read =
      (fun buf s e ->
        let h = header buf s e in
        read_content buf s h.cstart h.cend);
    der = (fun v -> tlv tag (content v));
    check_content;
    read_content;
    content;
  }

let retag tag n =
  tagged tag ~check_content:n.check_content ~read_content:n.read_content
    ~content:n.content

(* The bytes of a binary codec, for CONTAINING: its own check, read and write,
   on exactly the contents. *)
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

let containing_node typ =
  let c = contents typ in
  let within buf ce = Input_end.narrow (Input_end.of_bytes buf) ce in
  let check_content buf cs ce =
    let ie = within buf ce in
    c.validate ie buf cs;
    let e = cs + c.measure ie buf cs in
    if e <> ce then trailing ~at:e (ce - e)
  in
  let content v =
    let b = Bytes.create (c.encoded_size v) in
    let e = c.encode v b 0 in
    (match check_content b 0 e with
    | () -> ()
    | exception exn when Types.is_parse_error exn ->
        cannot_write "the CONTAINING value is refused by its codec (%a)"
          Error.pp (Types.parse_error_of exn));
    Bytes.sub_string b 0 e
  in
  tagged (universal 4) ~check_content
    ~read_content:(fun buf _ cs ce -> c.decode (within buf ce) buf cs)
    ~content

(* SEQUENCE OF and SET OF: a list of values, at least one if [non_empty], a SET
   OF sorted by encoding (X.690 11.6, spec/ASN1.Spec.Raw.fst:13-14,655-668,
   spec/ASN1.Spec.Interpreter.fst:52-65). *)
let list_node ~set ~non_empty n =
  let tag = universal ~constructed:true (if set then 17 else 16) in
  let check_content buf cs ce =
    let rec go pos count ps pe =
      if pos < ce then begin
        let e = n.check buf pos ce in
        if set && ps >= 0 && not (lex_le buf ps pe pos e) then
          rule_broken ~at:pos;
        go e (count + 1) pos e
      end
      else if non_empty && count = 0 then rule_broken ~at:cs
    in
    go cs 0 (-1) (-1)
  in
  let read_content buf _ cs ce =
    let rec go pos acc =
      if pos >= ce then List.rev acc
      else
        let e = (header buf pos ce).cend in
        go e (n.read buf pos e :: acc)
    in
    go cs []
  in
  let content l =
    if non_empty && List.is_empty l then
      cannot_write "a SIZE (1..MAX) list is empty";
    let ders = List.map n.der l in
    String.concat "" (if set then List.sort String.compare ders else ders)
  in
  tagged tag ~check_content ~read_content ~content

(* SEQUENCE and SET. A component compiles to its node, under its decoration;
   [part] is what locating it needs, whatever its type. *)

type ('f, 'r) cfields =
  | Nil : ('r, 'r) cfields
  | Cons : {
      comp : 'a comp;
      proj : 'r -> 'a;
      rest : ('f, 'r) cfields;
    }
      -> ('a -> 'f, 'r) cfields

and _ comp =
  | Needed : 'a node -> 'a comp
  | Maybe : 'a node -> 'a option comp
  | Defaulted : { value : 'a; node : 'a node; der : string } -> 'a comp
  | Keyed : {
      key : int;
      key_node : 'k node;
      cases : ('a, 'k) ccase array;
      fallback : ('a, 'k) cfallback option;
    }
      -> 'a comp

and ('a, 'k) ccase =
  | Body : {
      case_key : string;
      node : 'w node;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> ('a, 'k) ccase
  | No_body : {
      case_key : string;
      inject : 'a;
      project : 'a -> bool;
    }
      -> ('a, 'k) ccase

and ('a, 'k) cfallback =
  | Otherwise : {
      optional : bool;
      node : 'w node;
      inject : 'k -> 'w option -> 'a;
      project : 'a -> ('k * 'w option) option;
    }
      -> ('a, 'k) cfallback

type body = { bstarts : tags; bcheck : bytes -> int -> int -> int }

type part =
  | Plain of {
      iname : string;
      mandatory : bool;
      body : body;
      default : string option;
    }
  | By_key of {
      iname : string;
      key : int;
      keys : (string * body option) array;
      other : (bool * body) option;
    }

let body n = { bstarts = n.starts; bcheck = n.check }

let part_of : type a. string -> a comp -> part =
 fun iname -> function
  | Needed n -> Plain { iname; mandatory = true; body = body n; default = None }
  | Maybe n -> Plain { iname; mandatory = false; body = body n; default = None }
  | Defaulted { node; der; _ } ->
      Plain { iname; mandatory = false; body = body node; default = Some der }
  | Keyed { key; cases; fallback; _ } ->
      let keys =
        Array.map
          (function
            | Body { case_key; node; _ } -> (case_key, Some (body node))
            | No_body { case_key; _ } -> (case_key, None))
          cases
      in
      let other =
        Option.map
          (fun (Otherwise { optional; node; _ }) -> (optional, body node))
          fallback
      in
      By_key { iname; key; keys; other }

(* Where each component lies in the contents [cs]..[ce], [-1] when absent,
   and for a component defined by a key, the case of that key, [-1] for the
   fallback. *)
type located = { first : int array; last : int array; case : int array }

let located n =
  {
    first = Array.make n (-1);
    last = Array.make n (-1);
    case = Array.make n (-1);
  }

(* A component is present when the next identifier is one of its tags: an
   OPTIONAL or DEFAULT absent there leaves it to the next
   (spec/ASN1.Spec.Sequence.fst:196-218). A DEFAULT component present with
   its default breaks X.690 11.5 (spec/ASN1.Spec.Sequence.fst:32-60). *)
let locate_sequence ~check parts buf cs ce =
  let loc = located (Array.length parts) in
  let pos = ref cs in
  let present b = !pos < ce && tags_match b.bstarts (identifier buf !pos ce) in
  let take i b ~default =
    let s = !pos in
    let e = if check then b.bcheck buf s ce else (header buf s ce).cend in
    (match default with
    | Some d when check && region_is buf s e d -> rule_broken ~at:s
    | _ -> ());
    loc.first.(i) <- s;
    loc.last.(i) <- e;
    pos := e
  in
  Array.iteri
    (fun i part ->
      match part with
      | Plain { iname; mandatory; body; default } ->
          in_component iname (fun () ->
              if mandatory || present body then take i body ~default)
      | By_key { iname; key; keys; other } ->
          in_component iname (fun () ->
              let ks = loc.first.(key) and ke = loc.last.(key) in
              let rec find j =
                if j = Array.length keys then None
                else if region_is buf ks ke (fst keys.(j)) then Some j
                else find (j + 1)
              in
              match (find 0, other) with
              | Some j, _ -> (
                  loc.case.(i) <- j;
                  match snd keys.(j) with
                  | Some b -> take i b ~default:None
                  | None -> ())
              | None, Some (optional, b) ->
                  if (not optional) || present b then take i b ~default:None
              | None, None -> rule_broken ~at:ks))
    parts;
  if !pos <> ce then trailing ~at:!pos (ce - !pos);
  loc

(* A SET: its components in any declared order, written in the canonical order
   of their tags with no repeat, each mandatory one present (X.690 10.3,
   spec/ASN1.Spec.SetComponents.fst:4-13,151-190). *)
let locate_set ~check parts buf cs ce =
  let loc = located (Array.length parts) in
  let rec go pos prev =
    if pos < ce then begin
      let id = identifier buf pos ce in
      let rec find i =
        if i = Array.length parts then
          Types.raise_invalid_tag ~at:pos (Int64.to_int id.number)
        else
          match parts.(i) with
          | Plain { iname; body; default; _ } when tags_match body.bstarts id ->
              (i, iname, body, default)
          | Plain _ | By_key _ -> find (i + 1)
      in
      let i, iname, body, default = find 0 in
      in_component iname (fun () ->
          let order = order_of id in
          if check then begin
            (match prev with
            | Some o when not (tag_lt o order) -> rule_broken ~at:pos
            | _ -> ());
            if loc.first.(i) >= 0 then rule_broken ~at:pos
          end;
          let e =
            if check then body.bcheck buf pos ce else (header buf pos ce).cend
          in
          (match default with
          | Some d when check && region_is buf pos e d -> rule_broken ~at:pos
          | _ -> ());
          loc.first.(i) <- pos;
          loc.last.(i) <- e;
          go e (Some order))
    end
  in
  go cs None;
  Array.iteri
    (fun i part ->
      match part with
      | Plain { mandatory = true; iname; _ } when loc.first.(i) < 0 ->
          in_component iname (fun () -> rule_broken ~at:ce)
      | Plain _ | By_key _ -> ())
    parts;
  loc

let read_comp : type a. a comp -> bytes -> located -> int -> a =
 fun comp buf loc i ->
  let s = loc.first.(i) and e = loc.last.(i) in
  match comp with
  | Needed n -> n.read buf s e
  | Maybe n -> if s < 0 then None else Some (n.read buf s e)
  | Defaulted { value; node; _ } -> if s < 0 then value else node.read buf s e
  | Keyed { key; key_node; cases; fallback } -> (
      let j = loc.case.(i) in
      if j >= 0 then
        match cases.(j) with
        | Body { node; inject; _ } -> inject (node.read buf s e)
        | No_body { inject; _ } -> inject
      else
        match fallback with
        | Some (Otherwise { node; inject; _ }) ->
            let k = key_node.read buf loc.first.(key) loc.last.(key) in
            inject k (if s < 0 then None else Some (node.read buf s e))
        | None -> Types.raise_invalid_tag ~at:s 0)

let rec read_fields : type f r.
    f -> (f, r) cfields -> bytes -> located -> int -> r =
 fun make fields buf loc i ->
  match fields with
  | Nil -> make
  | Cons { comp; rest; _ } ->
      let v = read_comp comp buf loc i in
      read_fields (make v) rest buf loc (i + 1)

let write_comp : type a. a comp -> a -> string option array -> string option =
 fun comp v ders ->
  match comp with
  | Needed n -> Some (n.der v)
  | Maybe n -> Option.map n.der v
  | Defaulted { node; der; _ } ->
      let s = node.der v in
      if String.equal s der then None else Some s
  | Keyed { key; key_node; cases; fallback } -> (
      let kder = Option.value ~default:"" ders.(key) in
      let case =
        Array.find_opt
          (function
            | Body { case_key; _ } | No_body { case_key; _ } ->
                String.equal case_key kder)
          cases
      in
      match (case, fallback) with
      | Some (Body { node; project; _ }), _ -> (
          match project v with
          | Some w -> Some (node.der w)
          | None -> cannot_write "the value is not the case of its key")
      | Some (No_body { project; _ }), _ ->
          if project v then None
          else cannot_write "the value is not the case of its key"
      | None, Some (Otherwise { optional; node; project; _ }) -> (
          match project v with
          | None -> cannot_write "the value is not the fallback of its key"
          | Some (k, w) -> (
              if not (String.equal (key_node.der k) kder) then
                cannot_write "the fallback value carries another key";
              match w with
              | Some w -> Some (node.der w)
              | None when optional -> None
              | None -> cannot_write "the fallback component is not OPTIONAL"))
      | None, None ->
          cannot_write "the key has no case and there is no fallback")

let rec write_fields : type f r.
    (f, r) cfields -> r -> string option array -> int -> unit =
 fun fields r ders i ->
  match fields with
  | Nil -> ()
  | Cons { comp; proj; rest } ->
      ders.(i) <- write_comp comp (proj r) ders;
      write_fields rest r ders (i + 1)

let rec cfields_length : type f r. (f, r) cfields -> int = function
  | Nil -> 0
  | Cons { rest; _ } -> 1 + cfields_length rest

let rec parts_of : type f r. string list -> (f, r) cfields -> part list =
 fun names fields ->
  match (names, fields) with
  | name :: names, Cons { comp; rest; _ } ->
      part_of name comp :: parts_of names rest
  | _ -> []

(* The tag a written SET component starts with, for the canonical order. *)
let written_order s =
  let id = identifier (Bytes.unsafe_of_string s) 0 (String.length s) in
  order_of id

let record_node ~set ~make fields names =
  let parts = Array.of_list (parts_of names fields) in
  let n = cfields_length fields in
  let locate = if set then locate_set else locate_sequence in
  let tag = universal ~constructed:true (if set then 17 else 16) in
  let content r =
    let ders = Array.make n None in
    write_fields fields r ders 0;
    let present = List.filter_map Fun.id (Array.to_list ders) in
    let present =
      if set then
        List.stable_sort
          (fun a b ->
            let oa = written_order a and ob = written_order b in
            if tag_lt oa ob then -1 else if tag_lt ob oa then 1 else 0)
          present
      else present
    in
    String.concat "" present
  in
  tagged tag
    ~check_content:(fun buf cs ce ->
      ignore (locate ~check:true parts buf cs ce))
    ~read_content:(fun buf _ cs ce ->
      read_fields make fields buf (locate ~check:false parts buf cs ce) 0)
    ~content

let rec index_of id i = function
  | [] -> None
  | Component c :: rest ->
      if Int.equal c.id id then Some i else index_of id (i + 1) rest

type 'a alt = Alt : 'w node * ('w -> 'a) * ('a -> 'w option) -> 'a alt

(* ANY: one whole value, written as given once it is one. *)
let any_node =
  let der s =
    let n = String.length s in
    match any_end (Bytes.unsafe_of_string s) 0 n with
    | e when e = n -> s
    | _ -> cannot_write "ANY value %S is not one DER value" s
    | exception exn when Types.is_parse_error exn ->
        cannot_write "ANY value %S is not one DER value" s
  in
  {
    own = None;
    starts = All;
    check = any_end;
    read = (fun buf s e -> Bytes.sub_string buf s (e - s));
    der;
    check_content = (fun _ cs _ -> no_contents ~at:cs);
    read_content = (fun _ _ cs _ -> no_contents ~at:cs);
    content = no_content;
  }

(* SIZE, counted as X.680 51.5 counts (spec/ASN1.Base.fst:577-610). *)
let size_node ~min ~max (inner : string t) n =
  let in_range v = in_size ~min ~max inner v in
  let tag = Option.value ~default:(universal 4) n.own in
  tagged tag
    ~check_content:(fun buf cs ce ->
      n.check_content buf cs ce;
      if not (in_range (Bytes.sub_string buf cs (ce - cs))) then
        rule_broken ~at:cs)
    ~read_content:n.read_content
    ~content:(fun v ->
      if not (in_range v) then
        cannot_write "%S is outside SIZE (%d..%d)" v min max;
      n.content v)

let explicit_node tag n =
  tagged tag
    ~check_content:(fun buf cs ce ->
      let e = n.check buf cs ce in
      if e <> ce then trailing ~at:e (ce - e))
    ~read_content:(fun buf _ cs ce -> n.read buf cs ce)
    ~content:n.der

(* keep_bytes: the value and the octets it was read from, the tags included. *)
let kept_node n =
  let kept buf s e v = (v, Bytes.sub_string buf s (e - s)) in
  {
    own = n.own;
    starts = n.starts;
    check = n.check;
    read = (fun buf s e -> kept buf s e (n.read buf s e));
    der = (fun (v, _) -> n.der v);
    check_content = n.check_content;
    read_content =
      (fun buf s cs ce -> kept buf s ce (n.read_content buf s cs ce));
    content = (fun (v, _) -> n.content v);
  }

let mapped_node ~decode ~encode n =
  {
    own = n.own;
    starts = n.starts;
    check = n.check;
    read = (fun buf s e -> decode (n.read buf s e));
    der = (fun v -> n.der (encode v));
    check_content = n.check_content;
    read_content = (fun buf s cs ce -> decode (n.read_content buf s cs ce));
    content = (fun v -> n.content (encode v));
  }

let rec compile : type a. a t -> a node = function
  | Prim p ->
      tagged
        (universal (prim_number p))
        ~check_content:(check_prim p)
        ~read_content:(fun buf _ cs ce -> read_prim p buf cs ce)
        ~content:(prim_content p)
  | Any -> any_node
  | Size { min; max; inner } -> size_node ~min ~max inner (compile inner)
  | Implicit { tag; inner } -> retag tag (compile inner)
  | Explicit { tag; inner } -> explicit_node tag (compile inner)
  | Sequence r -> record ~set:false r
  | Set r -> record ~set:true r
  | Sequence_of { non_empty; elem } ->
      list_node ~set:false ~non_empty (compile elem)
  | Set_of { non_empty; elem } -> list_node ~set:true ~non_empty (compile elem)
  | Choice alts -> choice_node alts
  | Keep_bytes inner -> kept_node (compile inner)
  | Containing typ -> containing_node typ
  | Map { inner; decode; encode } -> mapped_node ~decode ~encode (compile inner)

and record : type r. set:bool -> r record -> r node =
 fun ~set (Record { make; fields; components; _ }) ->
  let cfields = compile_fields components fields in
  let names = List.map (fun (Component c) -> c.name) components in
  record_node ~set ~make cfields names

and compile_fields : type f r.
    component list -> (f, r) Codec.fields -> (f, r) cfields =
 fun comps -> function
  | Codec.[] -> Nil
  | Codec.(f :: rest) ->
      let comp =
        match Codec.field_typ f with
        | Types.Der (Component_of d) -> compile_decorated comps d
        | _ -> invalid_arg "Wire.Asn1.codec: a field is not an ASN.1 component"
      in
      Cons { comp; proj = Codec.field_proj f; rest = compile_fields comps rest }

and compile_decorated : type a. component list -> a decorated -> a comp =
 fun comps -> function
  | Required t -> Needed (compile t)
  | Optional t -> Maybe (compile t)
  | Default { value; typ } ->
      let node = compile typ in
      Defaulted { value; node; der = node.der value }
  | Defined_by { key_id; key_typ; cases; key_name } ->
      let key =
        match index_of key_id 0 comps with
        | Some i -> i
        | None -> Fmt.invalid_arg "Wire.Asn1.codec: no key %S" key_name
      in
      let key_node = compile key_typ in
      let compiled =
        List.filter_map
          (function
            | Case { key; typ; inject; project } ->
                Some
                  (Body
                     {
                       case_key = key_node.der key;
                       node = compile typ;
                       inject;
                       project;
                     })
            | Absent { key; inject; project } ->
                Some (No_body { case_key = key_node.der key; inject; project })
            | Fallback _ -> None)
          cases
      in
      let fallback =
        List.find_map
          (function
            | Fallback { optional; typ; inject; project } ->
                Some
                  (Otherwise { optional; node = compile typ; inject; project })
            | Case _ | Absent _ -> None)
          cases
      in
      Keyed { key; key_node; cases = Array.of_list compiled; fallback }

(* CHOICE: the alternative whose tag is next (spec/ASN1.Spec.Choice.fst). *)
and choice_node : type a. a alternative list -> a node =
 fun alts ->
  let compiled =
    List.map
      (fun (Alternative a) ->
        let n = compile a.typ in
        (n.starts, Alt (n, a.inject, a.project)))
      alts
  in
  let find buf pos lim =
    let id = identifier buf pos lim in
    match List.find_opt (fun (ts, _) -> tags_match ts id) compiled with
    | Some (_, alt) -> alt
    | None -> Types.raise_invalid_tag ~at:pos (Int64.to_int id.number)
  in
  {
    own = None;
    starts = List.fold_left (fun acc (ts, _) -> union acc ts) (Tags []) compiled;
    check =
      (fun buf pos lim ->
        let (Alt (n, _, _)) = find buf pos lim in
        n.check buf pos lim);
    read =
      (fun buf s e ->
        let (Alt (n, inject, _)) = find buf s e in
        inject (n.read buf s e));
    der =
      (fun v ->
        match
          List.find_map
            (fun (_, Alt (n, _, project)) -> Option.map n.der (project v))
            compiled
        with
        | Some s -> s
        | None -> cannot_write "no alternative of the CHOICE takes the value");
    check_content = (fun _ cs _ -> no_contents ~at:cs);
    read_content = (fun _ _ cs _ -> no_contents ~at:cs);
    content = no_content;
  }

(* The root of a DER codec: the codec's one field, which the [.asn] printer
   reads too. *)
type _ Types.der += Root : 'a t -> 'a Types.der

let rec type_name : type a. a t -> string = function
  | Sequence (Record { name; _ }) | Set (Record { name; _ }) -> name
  | Implicit { inner; _ } | Explicit { inner; _ } -> type_name inner
  | Map { inner; _ } -> type_name inner
  | Keep_bytes inner -> type_name inner
  | Prim _ | Any | Size _ | Sequence_of _ | Set_of _ | Choice _ | Containing _
    ->
      "DER"

(* The SEQUENCE or SET at the root of a codec, under [depth] EXPLICIT tags:
   what locating its components needs. *)
type root = {
  components : component list;
  parts : part array;
  set : bool;
  depth : int;
}

let root_of_record ~set depth (Record { fields; components; _ }) =
  let names = List.map (fun (Component c) -> c.name) components in
  let parts =
    Array.of_list (parts_of names (compile_fields components fields))
  in
  { components; parts; set; depth }

let rec root_of : type a. int -> a t -> root option =
 fun depth -> function
  | Sequence r -> Some (root_of_record ~set:false depth r)
  | Set r -> Some (root_of_record ~set:true depth r)
  | Implicit { inner; _ } -> root_of depth inner
  | Explicit { inner; _ } -> root_of (depth + 1) inner
  | _ -> None

(* The contents of the value at [s], under [depth] EXPLICIT tags. *)
let rec unwrap buf s e depth =
  let h = header buf s e in
  if depth = 0 then h else unwrap buf h.cstart h.cend (depth - 1)

(* Where the components of the record at [off] lie, in bytes a check
   accepted. *)
let locate_root root buf off =
  let h = unwrap buf off (Bytes.length buf) root.depth in
  let locate = if root.set then locate_set else locate_sequence in
  locate ~check:false root.parts buf h.cstart h.cend

(* A component read in place, after the check of the whole record, as
   [Codec.get] reads a field. *)
let components (type r) (t : r t) (n : r node) : r Codec.components =
  let root = root_of 0 t in
  let find (type a) (f : (a, r) Codec.field) :
      (bytes -> int -> (a, Error.t) result) option =
    match (root, Codec.field_typ f) with
    | Some root, Types.Der (Component_of d) -> (
        match index_of (Codec.field_id f) 0 root.components with
        | None -> None
        | Some i ->
            let comp = compile_decorated root.components d in
            Some
              (fun buf off ->
                match
                  ignore (n.check buf off (Bytes.length buf) : int);
                  read_comp comp buf (locate_root root buf off) i
                with
                | v -> Ok v
                | exception exn when Types.is_parse_error exn ->
                    Error (Types.parse_error_of exn)))
    | _ -> None
  in
  { Codec.find }

let codec (type a) (t : a t) : a Codec.t =
  let n = compile t in
  let name = type_name t in
  let field =
    Types.Field
      {
        field_name = Some name;
        field_typ = Types.Der (Root t);
        constraint_ = None;
        action = None;
        field_doc = None;
      }
  in
  let lim ie = Input_end.to_int ie in
  Codec.opaque ~name field ~min_size:2
    ~check:(fun ie buf off -> ignore (n.check buf off (lim ie) : int))
    ~read:(fun ie buf off -> n.read buf off (header buf off (lim ie)).cend)
    ~extent:(fun ie buf off -> (header buf off (lim ie)).cend)
    ~write:(fun v buf off ->
      let s = n.der v in
      let len = String.length s in
      if off > Bytes.length buf - len then
        Fmt.invalid_arg "Codec.encode %s: %d bytes do not fit at %d in %d" name
          len off (Bytes.length buf);
      Bytes.blit_string s 0 buf off len;
      off + len)
    ~size_of_value:(fun v -> String.length (n.der v))
    ~components:(components t n)

(* [fold]: the elements of a SEQUENCE OF or SET OF component of the record a
   view holds, each read as a value (docs/asn1.md Q1). *)

(* The element type of a list component, and how many EXPLICIT tags wrap the
   list. *)
let rec list_elem : type a. int -> a list t -> (a t * int) option =
 fun depth -> function
  | Sequence_of { elem; _ } | Set_of { elem; _ } -> Some (elem, depth)
  | Implicit { inner; _ } -> list_elem depth inner
  | Explicit { inner; _ } -> list_elem (depth + 1) inner
  | _ -> None

let fold (type a r c) (v : (c, r) Codec.View.codec)
    (l : (a list, r) Codec.field) =
  let codec = Codec.View.record_codec v in
  let not_a_list () =
    Fmt.invalid_arg
      "Wire.Asn1.fold: field %S of codec %S is not a SEQUENCE OF or SET OF \
       component of its record"
      (Codec.field_name l) (Codec.name codec)
  in
  let root =
    List.find_map
      (fun (Types.Field f) ->
        match f.field_typ with Types.Der (Root t) -> root_of 0 t | _ -> None)
      (Codec.to_struct codec).fields
  in
  let root = match root with Some r -> r | None -> not_a_list () in
  let index =
    match index_of (Codec.field_id l) 0 root.components with
    | Some i -> i
    | None -> not_a_list ()
  in
  let elem, depth =
    match Codec.field_typ l with
    | Types.Der (Component_of (Required t)) -> (
        match list_elem 0 t with Some x -> x | None -> not_a_list ())
    | _ -> not_a_list ()
  in
  let n = compile elem in
  (* The contents of the list, or [None] when the component is absent. *)
  let elements buf off =
    let loc = locate_root root buf off in
    let s = loc.first.(index) in
    if s < 0 then None
    else
      let h = unwrap buf s loc.last.(index) depth in
      Some (h.cstart, h.cend)
  in
  (* The element at [pos], or [None] at bytes written after the check. *)
  let next buf pos ce =
    match n.check buf pos ce with
    | e -> Some (n.read buf pos e, e)
    | exception exn when Types.is_parse_error exn -> None
  in
  fun b view ~init ~f ->
    let buf = (b : _ Buf.t :> bytes) in
    match elements buf (Codec.View.offset view) with
    | exception exn when Types.is_parse_error exn -> init
    | None -> init
    | Some (cs, ce) ->
        let rec go acc pos =
          if pos >= ce then acc
          else
            match next buf pos ce with
            | None -> acc
            | Some (x, e) -> go (f acc x) e
        in
        go init cs

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
