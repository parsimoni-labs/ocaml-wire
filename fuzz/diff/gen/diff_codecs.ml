(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: a deterministic, well-distributed Boltzmann {!Fuzz_gen.sample}
    of the shape space (not a handpicked list), filtered to the ones with a
    standalone fixed-size validator.

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
