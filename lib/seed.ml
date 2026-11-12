open Dsl

(* -- Seed values: what a field's own declaration says its wire bytes may be.

   A differential test over uniformly drawn bytes is only as good as the odds
   of drawing something the parser accepts. A field pinned to a constant --
   [magic == 0x53504F53] -- leaves one accepting value in 2^32, so the whole
   corpus is rejects and both implementations agree on garbage. The values are
   already in the description, so expose them rather than making every corpus
   generator interpret refinements independently. -- *)

type int_slot = { width : int; endian : endian }
type field_seed = { field : string; slot : int_slot; values : int64 list }

(* Look through decorations that do not change the wire encoding. Bitfields are
   omitted deliberately: their base word is shared with neighbouring fields,
   so a byte offset alone is not enough to seed one correctly. *)
let rec int_slot_of_typ : type a. a typ -> int_slot option = function
  | Uint8 | Int8 -> Some { width = 1; endian = Big }
  | Uint16 e | Int16 e -> Some { width = 2; endian = e }
  | Uint32 e | Int32 e -> Some { width = 4; endian = e }
  | Uint64 e | Int64 e -> Some { width = 8; endian = e }
  | Uint_var { size = Int width; endian } -> Some { width; endian }
  | Enum { base; _ } -> int_slot_of_typ base
  | Where { inner; _ } -> int_slot_of_typ inner
  | Map { inner; _ } -> int_slot_of_typ inner
  | Optional { inner; _ } -> int_slot_of_typ inner
  | Optional_or { inner; _ } -> int_slot_of_typ inner
  | _ -> None

let neighbours k =
  let before = if Int64.equal k Int64.min_int then [] else [ Int64.pred k ] in
  let after = if Int64.equal k Int64.max_int then [] else [ Int64.succ k ] in
  before @ (k :: after)

(* An equality names the value itself; an ordering names the values either side
   of its boundary. Conjunction and disjunction both contribute their operands'
   values. This is intentionally an over-approximation: consumers must ask the
   codec for the verdict, so an inadmissible candidate costs one retry rather
   than producing a wrong oracle. *)
(* Which field a comparison names, and the value it names for it. The generated
   validator checks a refinement at the field it is attached to but compares
   whatever fields the expression references, so a literal belongs to the field
   on the other side of the comparison rather than to the field carrying the
   constraint: [UINT8 d { (len < 2) }] singles out a value for [len], not for
   [d]. *)
let constraint_bindings (e : bool expr) : (string * int64 list) list =
  let named : type a. a expr -> string option = function
    | Ref (_, n) -> Some n
    | _ -> None
  in
  let literal : type a. a expr -> int64 option = function
    | Int k -> Some (Int64.of_int k)
    | Int64 k -> Some k
    | _ -> None
  in
  let against : type a. a expr -> a expr -> (string * int64) option =
   fun x y ->
    match (named x, literal y) with
    | Some n, Some k -> Some (n, k)
    | _ -> (
        match (named y, literal x) with
        | Some n, Some k -> Some (n, k)
        | _ -> None)
  in
  let bind f a b =
    match against a b with Some (n, k) -> [ (n, f k) ] | None -> []
  in
  let rec go : bool expr -> (string * int64 list) list = function
    | And (a, b) | Or (a, b) -> go a @ go b
    | Not a -> go a
    | Eq (a, b) -> bind (fun k -> [ k ]) a b
    | Ne (a, b) -> bind (fun k -> [ k ]) a b
    | Lt (a, b) -> bind neighbours a b
    | Le (a, b) -> bind neighbours a b
    | Gt (a, b) -> bind neighbours a b
    | Ge (a, b) -> bind neighbours a b
    | _ -> []
  in
  go e

(* A closed enumeration lists every value the field may take. *)
let rec enum_seed_values : type a. a typ -> int64 list = function
  | Enum { cases; closed = true; _ } ->
      List.map (fun (_, value) -> Int64.of_int value) cases
  (* A [lookup] admits exactly the indices into its table. Naming both ends is
     enough to straddle it: the seeder needs one accepting value, and the
     boundary cases it derives already step either side of each. Listing the
     whole table would multiply out through those without saying more. *)
  | Map { index_bound = Some n; _ } when n > 0 ->
      if n = 1 then [ 0L ] else [ 0L; Int64.of_int (n - 1) ]
  | Where { inner; _ } -> enum_seed_values inner
  | Map { inner; _ } -> enum_seed_values inner
  | Optional { inner; _ } -> enum_seed_values inner
  | Optional_or { inner; _ } -> enum_seed_values inner
  | _ -> []

let rec typ_bindings : type a. a typ -> (string * int64 list) list = function
  | Where { cond; inner } -> constraint_bindings cond @ typ_bindings inner
  | Map { inner; _ } -> typ_bindings inner
  | Optional { inner; _ } -> typ_bindings inner
  | Optional_or { inner; _ } -> typ_bindings inner
  | _ -> []

let int_slots s =
  List.filter_map
    (fun (Field f) ->
      match (f.field_name, int_slot_of_typ f.field_typ) with
      | Some name, Some slot -> Some (name, slot)
      | _ -> None)
    s.fields

(* The wire bytes a tag constant stands for, as the [int64] a seed carries.
   Deliberately not routed through [int_of]: a four-byte magic overflows a
   native [int] on a 31-bit target, and a seed has to survive there -- which is
   why [field_seed] holds [int64] in the first place. A signed value keeps its
   sign here and [write_slot] lays down its two's complement. *)
let rec tag_wire_value : type k. k typ -> k -> int64 option =
 fun typ k ->
  match typ with
  | Uint8 -> Some (Int64.of_int (UInt8.to_int k))
  | Int8 -> Some (Int64.of_int (SInt8.to_int k))
  | Uint16 _ -> Some (Int64.of_int (UInt16.to_int k))
  | Int16 _ -> Some (Int64.of_int (SInt16.to_int k))
  | Uint32 _ ->
      Some (Int64.logand (Int64.of_int32 (UInt32.to_int32 k)) 0xFFFF_FFFFL)
  | Int32 _ -> Some (Int64.of_int32 (SInt32.to_int32 k))
  | Uint64 _ -> Some (UInt64.to_int64 k)
  | Int64 _ -> Some k
  | Uint_var _ -> Some (Optint.Int63.to_int64 k)
  | Bits _ -> Some (Int64.of_int k)
  | Enum { base; _ } -> tag_wire_value base k
  | Where { inner; _ } -> tag_wire_value inner k
  | Map { inner; encode; _ } -> tag_wire_value inner (encode k)
  | Single_elem { elem; _ } -> tag_wire_value elem k
  | Apply { typ; _ } -> tag_wire_value typ k
  | _ -> None

(* A casetype parses its tag inline, at the start of the field's own bytes, so
   the field seeds like an integer even though it is not one: the slot is the
   tag's and the values it singles out are the case indices. A [default] branch
   claims every remaining tag and names no value, so it contributes none. *)
let rec casetype_tag_seed : type a. a typ -> (int_slot * int64 list) option =
  function
  | Casetype { tag; cases; _ } ->
      let index (Case_branch { cb_tag; _ }) =
        Option.bind cb_tag (tag_wire_value tag)
      in
      Option.map
        (fun slot -> (slot, List.filter_map index cases))
        (int_slot_of_typ tag)
  | Where { inner; _ } -> casetype_tag_seed inner
  | Map { inner; _ } -> casetype_tag_seed inner
  | Optional { inner; _ } -> casetype_tag_seed inner
  | Optional_or { inner; _ } -> casetype_tag_seed inner
  | _ -> None

let field_seeds s =
  (* Read the record the way the projection does. A struct-level [where] is
     lowered onto the last field it references before EverParse sees it, so
     lower it here rather than keeping a second rule about where a constraint
     lives, and collect every refinement's values across the whole record: the
     generated validator checks a refinement at the field carrying it but
     compares whatever it references. *)
  let _, fields = Lower_3d.lower_where_to_field_constraint s.where s.fields in
  let bindings =
    List.concat_map
      (fun (Field f) ->
        (match f.constraint_ with
          | Some c -> constraint_bindings c
          | None -> [])
        @ typ_bindings f.field_typ)
      fields
  in
  let named name =
    List.concat_map
      (fun (n, vs) -> if String.equal n name then vs else [])
      bindings
  in
  let seed name slot values =
    match List.sort_uniq Int64.compare values with
    | [] -> None
    | values -> Some { field = name; slot; values }
  in
  let of_field (Field f) =
    match (f.field_name, int_slot_of_typ f.field_typ) with
    | Some name, Some slot ->
        seed name slot (named name @ enum_seed_values f.field_typ)
    | Some name, None -> (
        match casetype_tag_seed f.field_typ with
        | Some (slot, values) -> seed name slot values
        | None -> None)
    | None, _ -> None
  in
  List.filter_map of_field fields
