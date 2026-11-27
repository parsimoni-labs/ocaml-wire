(* The symbolic executor's primitives against the definitions they model.

   test_symex.exe Z3

   Each primitive is proved by Z3 equal, on every input, to the definition
   the OCaml standard library gives of it in primitives the walk already
   reads, and each is evaluated on the inputs at the edges of its domain
   against the OCaml function itself. *)

let err fmt = Fmt.kstr (fun s -> Error s) fmt
let x = Smt.name "x" (Smt.Bv 64)
let y = Smt.name "y" (Smt.Bv 64)

let env =
  {
    Symex.locals =
      Symex.SMap.(empty |> add "x" (Symex.I64 x) |> add "y" (Symex.I64 y));
    scope = [];
  }

let term src = Symex.term (Symex.v ~file:"<test>" "") env src

(* Whether Z3 proves [a] and [b] equal for every [x] and [y]. *)
let equal ~z3 a b =
  let script =
    Fmt.str
      "(declare-const x (_ BitVec 64))@.(declare-const y (_ BitVec \
       64))@.(assert (not (= %a %a)))@.(check-sat)@.(get-value (x y))@."
      Smt.pp a Smt.pp b
  in
  match
    Z3.check_all ~z3 ~timeout:60 ~jobs:1 ~keep:false [ [ Z3.Query script ] ]
  with
  | [ (Z3.Unsat, _, _, _) ] -> Ok ()
  | [ (Z3.Sat values, _, _, _) ] ->
      err "differ at %a"
        Fmt.(list ~sep:comma (pair ~sep:(any " = ") string Z3.pp_sexp))
        values
  | [ (Z3.Unknown why, _, _, _) ] -> Error why
  | _ -> Error "no answer"

(* Int64.unsigned_compare n m = compare (sub n min_int) (sub m min_int), in
   the standard library's int64.ml. *)
let stdlib_unsigned_compare =
  "Int64.compare (Int64.sub x 0x8000000000000000L) (Int64.sub y \
   0x8000000000000000L)"

let unsigned_compares =
  [
    "Wire_runtime.Target.unsigned_compare";
    "Int64.unsigned_compare";
    "Wire_runtime.Target.Int64_u.unsigned_compare";
  ]

let edges = [ 0L; 1L; 2L; 69L; -1L; -2L; Int64.max_int; Int64.min_int; -69L ]

(* On constants the walk folds the comparison to the integer it answers. *)
let on_edges f =
  List.concat_map
    (fun a ->
      List.filter_map
        (fun b ->
          let src = Fmt.str "%s (%LdL) (%LdL)" f a b in
          let want = Int64.unsigned_compare a b in
          match term src with
          | t when Smt.equal t (Smt.int63 want) -> None
          | t -> Fmt.kstr Option.some "%s is %a, not %d" src Smt.pp t want)
        edges)
    edges

let () =
  let z3 = Sys.argv.(1) in
  let failures =
    List.concat_map
      (fun f ->
        let proof =
          match
            equal ~z3 (term (f ^ " x y")) (term stdlib_unsigned_compare)
          with
          | Ok () -> []
          | Error e -> [ Fmt.str "%s x y and the standard library's: %s" f e ]
        in
        proof @ on_edges f)
      unsigned_compares
  in
  List.iter (Fmt.epr "%s@.") failures;
  if failures <> [] then exit 1;
  Fmt.pr "%d unsigned comparisons proved equal to Int64.unsigned_compare@."
    (List.length unsigned_compares)
