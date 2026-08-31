(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: a deterministic, well-distributed Boltzmann {!Fuzz_gen.sample}
    of the shape space (not a handpicked list, and not {!Fuzz_gen.registry},
    which the OCaml-only suites drive off), filtered to the ones with a
    standalone fixed-size validator. The sampled vocabulary spans scalars of
    every width and endianness, bitfields, enums and lookups, and the fixed-size
    refined shapes -- byte spans with and without a per-byte predicate, range
    bounds, cross-field constraints, variable-width integers.

    A codec is includable iff it has a positive fixed wire size (variable-size
    codecs have no standalone validator; zero-size unit codecs hit a setter
    arity mismatch), binds no [Param.env], and is not a [casetype]. Codecs that
    share a synthesised type (an [enum], a [_RefByte_*] or [Se_*] element
    struct) are all included: each validator is compiled as its own translation
    unit and linked, so the shared types stay translation-unit-local and cannot
    collide. See the [libdiffcodecs] archive rule in [fuzz/diff/dune]. *)

(* Sample size: each includable codec costs one EverParse run at generation
   time, so this is the knob trading differential breadth against CI time. *)
let sample_count = 64
let sample_seed = 1

let starts_with s p =
  let s = String.trim s in
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

let lines_of (Fuzz_gen.Pack g) =
  String.split_on_char '\n'
    (Wire.Everparse.Raw.to_3d
       (Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g))
         .Wire.Everparse.module_)

type exclusion = Variable_size | Zero_size | Parameterised | Casetype

let string_of_exclusion = function
  | Variable_size -> "variable size, no standalone validator"
  | Zero_size -> "zero size, setter arity mismatch"
  | Parameterised -> "parameterised, binds Param.env"
  | Casetype -> "casetype, projects an embedded sub-validator"

(* The scope filter: [None] is includable, [Some r] is out of the differential's
   scope for reason [r]. A casetype or variable/zero-size codec has no standalone
   fixed-size validator; a parameterised one needs an env the harness does not
   set up. *)
let scope_exclusion (Fuzz_gen.Pack g as p) =
  let s = Wire.Everparse.project ~mode:`Ffi (Fuzz_gen.codec g) in
  match s.Wire.Everparse.wire_size with
  | None -> Some Variable_size
  | Some 0 -> Some Zero_size
  | Some _ when Fuzz_gen.binds_env p -> Some Parameterised
  | Some _ when List.exists (fun l -> starts_with l "casetype ") (lines_of p) ->
      Some Casetype
  | Some _ -> None

let candidates = Fuzz_gen.sample ~seed:sample_seed ~count:sample_count

(* Every candidate with a standalone fixed-size validator is included; the rest
   carry an explicit out-of-scope reason. A shared synthesised type does not
   force a drop: each validator is its own translation unit (see the module
   doc). *)
let included, excluded =
  let inc, exc =
    List.fold_left
      (fun (inc, exc) (label, p) ->
        match scope_exclusion p with
        | Some r -> (inc, (label, p, r) :: exc)
        | None -> ((label, p) :: inc, exc))
      ([], []) candidates
  in
  (List.rev inc, List.rev exc)

type compared_field = { name : string; width : int }

(* Which fields the differential can compare by value. Two conditions, and both
   are about what the two sides can actually hand back rather than about what is
   interesting:

   - The field must occupy whole bytes of its own on the wire
     ([Everparse.Raw.int_slots]). A bitfield shares its base word with its
     neighbours, so it has no slot; a float is not an integer; a byte span, an
     array and a nested record are not scalars. None of those are compared.
   - The default plug must report the field's value. A byte span, a nested
     region and a variable-width integer all route to [<Name>SetBytes], which
     hands back the field's byte offset instead, so there is no value on the C
     side to compare. That drops [uint], the one whole-byte integer the
     plug does not carry.

   What is left is the fixed-width scalars -- [uint8] .. [uint64], [int8] ..
   [int64], and the enums, lookups, maps and refinements layered over them. *)
let compared_fields (Fuzz_gen.Pack g) =
  let s = Wire.Everparse.Raw.struct_of_codec (Fuzz_gen.codec g) in
  let schema = Wire.Everparse.Raw.project_struct ~mode:`Ffi s in
  let reported_as_offset (p : Wire.Everparse.plug_field) =
    if String.ends_with ~suffix:"SetBytes" p.setter then Some p.name else None
  in
  let offsets =
    List.filter_map reported_as_offset (Wire.Everparse.plug_fields schema)
  in
  List.filter_map
    (fun (name, (slot : Wire.Everparse.Raw.int_slot)) ->
      if List.mem name offsets then None else Some { name; width = slot.width })
    (Wire.Everparse.Raw.int_slots s)

let summary =
  let tally =
    List.fold_left
      (fun acc (_, _, r) ->
        let k =
          match r with
          | Variable_size -> "variable-size"
          | Zero_size -> "zero-size"
          | Parameterised -> "parameterised"
          | Casetype -> "casetype"
        in
        let n = try List.assoc k acc with Not_found -> 0 in
        (k, n + 1) :: List.remove_assoc k acc)
      [] excluded
  in
  let by_reason =
    String.concat ", " (List.rev_map (fun (k, n) -> Fmt.str "%d %s" n k) tally)
  in
  Fmt.str "%d candidates: %d included, %d excluded%s" (List.length candidates)
    (List.length included) (List.length excluded)
    (if by_reason = "" then "" else Fmt.str " (%s)" by_reason)
