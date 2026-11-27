(* An ASN.1 type is a GADT of its own, [t]: tagging, OPTIONAL and CHOICE need
   the tags of their argument, which a binary [Types.typ] does not carry. A
   SEQUENCE component is still a [Field.t], so that [Codec.( $ )] binds it to a
   record as [Codec.v] does: its [Types.typ] is the one node [Types.Der], which
   holds the component's [decorated] here and which no binary path reads.

   Every combinator refuses, with [Invalid_argument], a shape everparse-asn1
   does not express (docs/asn1.md, "Refused when built"). The DER codec and the
   [.asn] printer of [Asn1] are functions over [t]. *)

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

  let checked_arc ~combinator n =
    let n64 = Int64.of_int n in
    if not (in_range n64) then
      Fmt.invalid_arg
        "Wire.Asn1.%s %d: an arc is between 0 and 2^32 - 1, the subidentifiers \
         everparse-asn1 reads"
        combinator n;
    arc n64

  let ( <| ) o n = Array.append o [| checked_arc ~combinator:"Oid.( <| )" n |]

  let ( <|| ) o ns =
    Array.append o
      (Array.of_list (List.map (checked_arc ~combinator:"Oid.( <|| )") ns))

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

  let arcs_of_string s =
    let rec arcs acc = function
      | [] -> Some (List.rev acc)
      | s :: rest -> (
          match arc_of_string s with
          | None -> None
          | Some a -> arcs (a :: acc) rest)
    in
    arcs [] (String.split_on_char '.' s)

  let of_string s =
    match arcs_of_string s with
    | Some (a :: b :: _ as l) when head_ok a b ->
        Some (Array.of_list (List.map arc l))
    | _ -> None

  let to_string o =
    String.concat "."
      (Array.to_list (Array.map (fun a -> Int64.to_string (int64_of_arc a)) o))

  let pp ppf o = Fmt.string ppf (to_string o)
end

module Relative_oid = struct
  (* The arcs, at least one, each below 2^32 (X.690 8.20,
     spec/ASN1.Base.fst:204-207): those of an object identifier without the
     rule on the first two. *)
  type t = UInt32.t array

  let v ns =
    if List.is_empty ns then
      invalid_arg
        "Wire.Asn1.Relative_oid.v: a RELATIVE-OID has at least one arc (X.690 \
         8.20.2, spec/ASN1.Base.fst:204-207)";
    Array.of_list (List.map (Oid.checked_arc ~combinator:"Relative_oid.v") ns)

  let equal = Oid.equal
  let compare = Oid.compare

  let of_string s =
    match Oid.arcs_of_string s with
    | Some (_ :: _ as l) -> Some (Array.of_list (List.map Oid.arc l))
    | _ -> None

  let to_string = Oid.to_string
  let pp = Oid.pp
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
  | Videotex
  | Graphic
  | General
  | Object_descriptor
  | Bmp
  | Universal_chars

type time = Utc | Generalized | Date | Time_of_day | Date_time
type iri = Oid_iri | Relative_oid_iri

type _ prim =
  | Bool : bool prim
  | Integer : string prim
  | Bounded_integer : { min : int64; max : int64 } -> int64 prim
  | Enumerated : (string * int) list -> int prim
  | Null : unit prim
  | Oid : Oid.t prim
  | Relative_oid : Relative_oid.t prim
  | Bit_string : (int * string) prim
  | Octet_string : string prim
  | Chars : chars -> string prim
  | Time : time -> string prim
  | Iri : iri -> string prim

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
  | Relative_oid -> 13
  | Enumerated _ -> 10
  | Chars Utf8 -> 12
  | Chars Numeric -> 18
  | Chars Printable -> 19
  | Chars Teletex -> 20
  | Chars Videotex -> 21
  | Chars Graphic -> 25
  | Chars General -> 27
  | Chars Object_descriptor -> 7
  | Chars Ia5 -> 22
  | Time Utc -> 23
  | Time Generalized -> 24
  | Chars Visible -> 26
  | Chars Universal_chars -> 28
  | Chars Bmp -> 30
  | Time Date -> 31
  | Time Time_of_day -> 32
  | Time Date_time -> 33
  | Iri Oid_iri -> 35
  | Iri Relative_oid_iri -> 36

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

(* everparse-asn1 reads a number of magnitude below 2^61
   (everparse-asn1/Lexer.mll:63), in a bound, a named number or a value. *)
let number_limit = Int64.(sub (shift_left 1L 61) 1L)

let in_number_range n =
  Int64.compare n (Int64.neg number_limit) >= 0
  && Int64.compare n number_limit <= 0

(* Primitives. *)

let bool = Prim Bool
let integer = Prim Integer

let bounded_integer ~min ~max =
  if Int64.compare min max > 0 then
    Fmt.invalid_arg
      "Wire.Asn1.bounded_integer: INTEGER (%Ld..%Ld) is empty, its lower bound \
       is above its upper bound"
      min max;
  if not (in_number_range min && in_number_range max) then
    Fmt.invalid_arg
      "Wire.Asn1.bounded_integer: INTEGER (%Ld..%Ld) has a bound of magnitude \
       2^61 or more, a number everparse-asn1 does not read \
       (everparse-asn1/Lexer.mll:63)"
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
        if not (in_number_range (Int64.of_int n)) then
          Fmt.invalid_arg
            "Wire.Asn1.enumerated: the number %d of %S has magnitude 2^61 or \
             more, a number everparse-asn1 does not read \
             (everparse-asn1/Lexer.mll:63)"
            n name;
        check rest
  in
  check ns;
  Prim (Enumerated ns)

let null = Prim Null
let oid = Prim Oid
let relative_oid = Prim Relative_oid
let bit_string = Prim Bit_string
let octet_string = Prim Octet_string
let utf8_string = Prim (Chars Utf8)
let printable_string = Prim (Chars Printable)
let ia5_string = Prim (Chars Ia5)
let numeric_string = Prim (Chars Numeric)
let visible_string = Prim (Chars Visible)
let teletex_string = Prim (Chars Teletex)
let videotex_string = Prim (Chars Videotex)
let graphic_string = Prim (Chars Graphic)
let general_string = Prim (Chars General)
let object_descriptor = Prim (Chars Object_descriptor)
let bmp_string = Prim (Chars Bmp)
let universal_string = Prim (Chars Universal_chars)
let utc_time = Prim (Time Utc)
let generalized_time = Prim (Time Generalized)
let date = Prim (Time Date)
let time_of_day = Prim (Time Time_of_day)
let date_time = Prim (Time Date_time)
let oid_iri = Prim (Iri Oid_iri)
let relative_oid_iri = Prim (Iri Relative_oid_iri)
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

(* Names and values in the .asn text, which a DEFAULT and a defined_by key need
   when they are built, and the printer below. *)

(* An ASN.1 name (X.680 12.2-12.4): a letter, then letters and digits with
   single hyphens between them. Every other run of characters of [s] becomes
   one hyphen; [upper] names a type or a module, else a component, an
   alternative, a named number or a value. *)
let ident ~upper s =
  let b = Buffer.create (String.length s) in
  let gap = ref false in
  String.iter
    (fun c ->
      match c with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
          if !gap && Buffer.length b > 0 then Buffer.add_char b '-';
          gap := false;
          Buffer.add_char b c
      | _ -> gap := true)
    s;
  let s = Buffer.contents b in
  let s =
    match s with
    | "" -> if upper then "T" else "x"
    | _ -> (
        match s.[0] with
        | 'a' .. 'z' | 'A' .. 'Z' -> s
        | _ -> (if upper then "T-" else "x-") ^ s)
  in
  if upper then String.capitalize_ascii s else String.uncapitalize_ascii s

(* [base] if [taken] does not hold it, else the first free [base-2],
   [base-3], ... *)
let fresh ~taken base =
  if not (taken base) then base
  else
    let rec go i =
      let n = Fmt.str "%s-%d" base i in
      if taken n then go (i + 1) else n
    in
    go 2

(* The names of one scope, in order, each made an ASN.1 name and unique. *)
let unique_idents names =
  List.rev
    (List.fold_left
       (fun acc n ->
         fresh ~taken:(fun x -> List.mem x acc) (ident ~upper:false n) :: acc)
       [] names)

let enum_names ns =
  List.combine (unique_idents (List.map fst ns)) (List.map snd ns)

let hex s =
  let digits = "0123456789ABCDEF" in
  let b = Buffer.create (2 * String.length s) in
  String.iter
    (fun c ->
      Buffer.add_char b digits.[Char.code c lsr 4];
      Buffer.add_char b digits.[Char.code c land 0xF])
    s;
  Buffer.contents b

(* Why a value has no .asn literal. *)
let err_literal fmt = Fmt.kstr (fun s -> Error s) fmt

(* A character string literal: a doubled quote for a quote
   (everparse-asn1/Lexer.mll:80). The lexer drops a newline with the white
   space around it (Lexer.mll:82), so a control character has no literal. *)
let quoted s =
  if String.exists (fun c -> Char.code c < 0x20 || Char.code c = 0x7F) s then
    Error
      "a character string with a control character, which an .asn literal \
       cannot hold (everparse-asn1/Lexer.mll:82)"
  else Ok ("\"" ^ String.concat "\"\"" (String.split_on_char '"' s) ^ "\"")

let printable_ascii what s =
  if String.exists (fun c -> Char.code c > 0x7E) s then
    err_literal
      "a %s with a character outside printable ASCII, the literals \
       everparse-asn1 reads for it (README \"IMPORTS and values\")"
      what
  else quoted s

(* The UTF-8 text of the code points of [s], [width] octets each, big-endian:
   everparse-asn1 reads BMPString and UniversalString literals as UTF-8 and
   re-encodes them. *)
let code_points ~width what s =
  if String.length s mod width <> 0 then
    err_literal "a %s whose length is not a multiple of %d" what width
  else
    let b = Buffer.create (String.length s) in
    let rec go i =
      if i = String.length s then quoted (Buffer.contents b)
      else
        let cp =
          if width = 2 then String.get_uint16_be s i
          else Int32.to_int (String.get_int32_be s i)
        in
        if Uchar.is_valid cp then (
          Buffer.add_utf_8_uchar b (Uchar.of_int cp);
          go (i + width))
        else
          err_literal "a %s holding a code point that is not a character" what
    in
    go 0

let chars_literal k s =
  match k with
  | Utf8 ->
      if String.is_valid_utf_8 s then quoted s
      else Error "a UTF8String that is not valid UTF-8"
  | Printable -> printable_ascii "PrintableString" s
  | Ia5 -> printable_ascii "IA5String" s
  | Numeric -> printable_ascii "NumericString" s
  | Visible -> printable_ascii "VisibleString" s
  | Teletex -> printable_ascii "TeletexString" s
  | Videotex -> printable_ascii "VideotexString" s
  | Graphic -> printable_ascii "GraphicString" s
  | General -> printable_ascii "GeneralString" s
  | Object_descriptor -> printable_ascii "ObjectDescriptor" s
  | Bmp -> code_points ~width:2 "BMPString" s
  | Universal_chars -> code_points ~width:4 "UniversalString" s

(* An INTEGER from its two's complement content octets. *)
let integer_literal s =
  let n = String.length s in
  if n = 0 then Error "an INTEGER with no content octets"
  else if n > 8 then
    Error
      "an INTEGER of magnitude 2^61 or more, a number everparse-asn1 does not \
       read (everparse-asn1/Lexer.mll:63)"
  else
    let v =
      String.fold_left
        (fun v c ->
          Int64.logor (Int64.shift_left v 8) (Int64.of_int (Char.code c)))
        (if Char.code s.[0] >= 0x80 then -1L else 0L)
        s
    in
    if in_number_range v then Ok (Int64.to_string v)
    else
      Error
        "an INTEGER of magnitude 2^61 or more, a number everparse-asn1 does \
         not read (everparse-asn1/Lexer.mll:63)"

(* A BIT STRING as its binary digits, the unused bits left out. *)
let bits_literal (unused, s) =
  let n = String.length s in
  if unused < 0 || unused > 7 || (n = 0 && unused > 0) then
    Error "a BIT STRING whose count of unused bits is not 0 to 7"
  else if n > 0 && Char.code s.[n - 1] land ((1 lsl unused) - 1) <> 0 then
    Error "a BIT STRING whose unused bits are not zero"
  else
    let b = Buffer.create ((8 * n) + 3) in
    Buffer.add_char b '\'';
    for i = 0 to (8 * n) - unused - 1 do
      let bit = (Char.code s.[i / 8] lsr (7 - (i mod 8))) land 1 in
      Buffer.add_char b (if bit = 1 then '1' else '0')
    done;
    Buffer.add_string b "'B";
    Ok (Buffer.contents b)

(* The characters of the times and IRIs, as their contents rules state them
   (spec/ASN1.Spec.Time.fst:166-210, spec/ASN1.Spec.IRI.fst:50-58). *)
let time_ok k s =
  match k with
  | Utc -> Der.utc_ok s
  | Generalized -> Der.generalized_ok s
  | Date -> Der.date_ok s
  | Time_of_day -> Der.time_of_day_ok s
  | Date_time -> Der.date_time_ok s

let time_keyword = function
  | Utc -> "UTCTime"
  | Generalized -> "GeneralizedTime"
  | Date -> "DATE"
  | Time_of_day -> "TIME-OF-DAY"
  | Date_time -> "DATE-TIME"

let iri_ok k s =
  match k with
  | Oid_iri -> Der.oid_iri_ok s
  | Relative_oid_iri -> Der.relative_oid_iri_ok s

let iri_keyword = function
  | Oid_iri -> "OID-IRI"
  | Relative_oid_iri -> "RELATIVE-OID-IRI"

(* A time literal. everparse-asn1 reads UTCTime and GeneralizedTime as their
   DER contents, and DATE, TIME-OF-DAY and DATE-TIME in the extended format
   ("YYYY-MM-DD", "hh:mm:ss", "YYYY-MM-DDThh:mm:ss", README:431-434), which
   this writes from the basic format of the DER contents (X.690 8.26.2). *)
let time_literal k v =
  let date s =
    String.concat "-" [ String.sub s 0 4; String.sub s 4 2; String.sub s 6 2 ]
  in
  let tod s =
    String.concat ":" [ String.sub s 0 2; String.sub s 2 2; String.sub s 4 2 ]
  in
  match k with
  | Utc | Generalized -> printable_ascii (time_keyword k) v
  | (Date | Time_of_day | Date_time) when not (time_ok k v) ->
      err_literal "%S, which is not a DER %s" v (time_keyword k)
  | Date -> quoted (date v)
  | Time_of_day -> quoted (tod v)
  | Date_time -> quoted (date (String.sub v 0 8) ^ "T" ^ tod (String.sub v 8 6))

let prim_literal : type a.
    oid:(Oid.t -> string) -> a prim -> a -> (string, string) result =
 fun ~oid p v ->
  match p with
  | Bool -> Ok (if v then "TRUE" else "FALSE")
  | Null -> Ok "NULL"
  | Oid -> Ok (oid v)
  | Integer -> integer_literal v
  | Bounded_integer _ -> Ok (Int64.to_string v)
  | Enumerated ns -> (
      match List.find_opt (fun (_, n) -> Int.equal n v) (enum_names ns) with
      | Some (name, _) -> Ok name
      | None -> err_literal "%d, which is not a value of the ENUMERATED" v)
  | Bit_string -> bits_literal v
  | Octet_string -> Ok ("'" ^ hex v ^ "'H")
  | Chars k -> chars_literal k v
  | Relative_oid ->
      Error
        "a RELATIVE-OID value, which everparse-asn1 does not read \
         (README:435,495)"
  | Time k -> time_literal k v
  | Iri k ->
      if iri_ok k v then quoted v
      else err_literal "%S, which is not a DER %s" v (iri_keyword k)

(* The .asn literal of the value [v] of [t], or why it has none. [oid] names an
   OBJECT IDENTIFIER value. *)
let rec literal : type a.
    oid:(Oid.t -> string) -> a t -> a -> (string, string) result =
 fun ~oid t v ->
  match t with
  | Prim p -> prim_literal ~oid p v
  | Size { inner; _ } -> literal ~oid inner v
  | Implicit { inner; _ } | Explicit { inner; _ } -> literal ~oid inner v
  | Map { inner; encode; _ } -> literal ~oid inner (encode v)
  | Any | Sequence _ | Set _ | Sequence_of _ | Set_of _ | Choice _
  | Keep_bytes _ | Containing _ ->
      Error
        "a value of a constructed type, ANY, keep_bytes or CONTAINING, which \
         everparse-asn1 takes as a DEFAULT or a key of no table \
         (README:464-467, \"ANY DEFINED BY tables\")"

let oid_value o =
  Fmt.str "{ %s }"
    (String.concat " " (String.split_on_char '.' (Oid.to_string o)))

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
  | Some
      ( Printable | Ia5 | Numeric | Visible | Teletex | Videotex | Graphic
      | General | Object_descriptor )
  | None ->
      String.length s

let in_size ~min ~max (inner : string t) v =
  let k = match inner with Prim (Chars k) -> Some k | _ -> None in
  let n = char_count k v in
  n >= min && n <= max

(* DEFAULT applies to a terminal type other than OBJECT IDENTIFIER and
   RELATIVE-OID, under at most one EXPLICIT tag and then unconstrained
   (everparse-asn1 README:464-467,495-498; a constrained DEFAULT under
   EXPLICIT has no literal, spec/ASN1.Base.fst:688), with a value its
   constraint admits (spec/ASN1.Base.fst:745); a time or IRI default is a
   DER value of its type (README:431-434). *)
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
  | Prim Relative_oid ->
      fail
        "DEFAULT on a RELATIVE-OID, which everparse-asn1 does not support \
         (README:464-467,495)"
  | Prim (Time k) ->
      if not (time_ok k v) then
        fail "the default %S is not a DER %s (README:431-434)" v
          (time_keyword k)
  | Prim (Iri k) ->
      if not (iri_ok k v) then
        fail "the default %S is not a DER %s (README:431-434)" v (iri_keyword k)
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
  (match literal ~oid:oid_value t v with
  | Ok _ -> ()
  | Error why ->
      Fmt.invalid_arg
        "Wire.Asn1.default %S: the default is %s; everparse-asn1 reads a \
         DEFAULT as an .asn literal"
        name why);
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
  | Relative_oid -> Relative_oid.equal a b
  | Bit_string ->
      let ua, sa = a and ub, sb = b in
      Int.equal ua ub && String.equal sa sb
  | Octet_string -> String.equal a b
  | Chars _ -> String.equal a b
  | Time _ -> String.equal a b
  | Iri _ -> String.equal a b

(* A key is a mandatory component of a terminal type other than RELATIVE-OID,
   without constraints, under any tags (README:308-309). Its values are
   compared as they are encoded. *)
type 'k key_shape = Key of ('k -> 'k -> bool) | Not_key of string

let rec key_shape : type k. k t -> k key_shape = function
  | Prim (Bounded_integer _ | Enumerated _) | Size _ ->
      Not_key "constrained; everparse-asn1 takes an unconstrained key"
  | Prim Relative_oid ->
      Not_key
        "a RELATIVE-OID, which everparse-asn1 does not take as a key \
         (README:308-309)"
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
  if List.is_empty cases then
    fail
      "no case and no fallback, so every key is refused, a table \
       everparse-asn1 refuses (everparse-asn1/Elab.ml:1226)";
  let rec check ~fallback = function
    | [] -> ()
    | c :: rest ->
        (match key_of c with
        | Some k when List.exists (same_key k) rest ->
            fail "a key is given twice"
        | Some k -> (
            match literal ~oid:oid_value key_typ k with
            | Ok _ -> ()
            | Error why ->
                fail
                  "a key is %s; everparse-asn1 reads a key as an .asn literal"
                  why)
        | None -> ());
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
