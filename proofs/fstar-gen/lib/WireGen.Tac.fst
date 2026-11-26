module WireGen.Tac

(* Written once: unfold a 3d.exe parser denotation (as_parser of the typ
   3d.exe wrote) to the LowParse combinators it denotes, then hand the goal
   to Z3 with the equations of WireGen.Lib and WireGen.Lib3d. *)

module T = FStar.Tactics
open FStar.List.Tot

(* The combinators that only retype a LowParse parser at a 3D kind stay
   folded (parse_nlist, parse_t_exact, parse_t_at_most): unfolded, Z3 would
   lose the kind their type gives the parser. Each reading states them. *)
let interp : list string = [
  `%EverParse3d.Interpreter.as_parser;
  `%EverParse3d.Interpreter.dtyp_as_parser;
  `%EverParse3d.Interpreter.itype_as_parser;
  `%EverParse3d.Interpreter.coerce;
  `%EverParse3d.Interpreter.as_type;
  `%EverParse3d.Interpreter.dtyp_as_type;
  `%EverParse3d.Interpreter.itype_as_type;
  `%EverParse3d.Interpreter.parser_kind_of_itype;
  `%EverParse3d.Interpreter.parser_kind_nz_of_itype;
  `%EverParse3d.Interpreter.parser_weak_kind_of_itype;
  `%EverParse3d.Interpreter.allow_reader_of_itype;
  `%EverParse3d.Interpreter.mk_dtyp_app;
  `%EverParse3d.Interpreter.parser_of_binding;
  `%EverParse3d.Interpreter.type_of_binding;
  `%FStar.Pervasives.coerce_eq;
  `%EverParse3d.Prelude.parse_pair;
  `%EverParse3d.Prelude.parse_filter;
  `%EverParse3d.Prelude.parse_dep_pair;
  `%EverParse3d.Prelude.parse_weaken_left;
  `%EverParse3d.Prelude.parse_weaken_right;
  `%EverParse3d.Prelude.parse_ite;
  `%EverParse3d.Prelude.parse____UINT8;
  `%EverParse3d.Prelude.parse____UINT8BE;
  `%EverParse3d.Prelude.parse____UINT16BE;
  `%EverParse3d.Prelude.parse____UINT32BE;
  `%EverParse3d.Prelude.parse____UINT64BE;
]

(* For a Flat lemma: the 3D parser against the field-by-field reading.
   [ns] are the codec's modules: the one 3d.exe wrote, whose definitions
   denote each type of the codec's 3D, and its reading. The interpreter is
   unfolded in full (zeta_full): a casetype's cases are under a test of a
   value the bytes give, which zeta leaves folded. *)
let unfold_flat (ns:list string) : T.Tac unit =
  T.norm [delta_only interp; delta_namespace ns; zeta_full; iota; primops];
  T.smt ()

(* The scanner's tests, each split into its two branches down to the
   answers, the test rewritten to its value in each: Z3 proves each answer
   equal to the reading on the path that leads to it, so that a query holds
   one path, not the whole walk. *)
let rec split_tests () : T.Tac unit =
  match T.cur_formula () with
  | T.Comp (T.Eq _) l _ -> (
      match T.inspect l with
      | T.Tv_Match c _ _ ->
          T.cases_bool c;
          T.iseq [ branch; branch ]
      | _ -> T.smt ())
  | _ -> T.smt ()

and branch () : T.Tac unit =
  T.or_else
    (fun () -> let h = T.implies_intro () in T.grewrite_eq h)
    T.idtac;
  T.norm [iota];
  split_tests ()

(* For a Proof lemma: the rendering against the Flat reading, with the
   rules of WireGen.OCaml at the emitter's literals. *)
let unfold_scan (names:list string) : T.Tac unit =
  ignore (T.pose_lemma (`WireGen.Tables.tables ()));
  T.norm [delta_only names; zeta; iota; primops];
  split_tests ()

(* A query of one path of a walk whose values are [int64]: no fuel, since
   the walk and the reading are unfolded already, and linear arithmetic,
   since its values are bytes and bits at constant offsets. *)
let smt_path () : T.Tac unit =
  T.set_options "--fuel 0 --ifuel 0 --z3smtopt '(set-option :smt.arith.nl false)'";
  T.smt ()

(* [f x == g y] from [f == g] and [x == y]. *)
let congr (#a:Type0) (#b:Type) (f g:a -> b) (x y:a)
    (_:squash (f == g)) (_:squash (x == y))
  : Lemma (f x == g y)
  = ()

(* Whether [l] and [r] apply one function to as many arguments, at least
   one. *)
let same_head (l r:T.term) : T.Tac bool =
  let hl, al = T.collect_app l in
  let hr, ar = T.collect_app r in
  Cons? al && length al = length ar && T.term_eq hl hr

(* An equation of two terms, by Z3 unless they are the same. *)
let same_or_smt () : T.Tac unit =
  match T.cur_formula () with
  | T.Comp (T.Eq _) l r -> if T.term_eq l r then T.trefl () else smt_path ()
  | _ -> smt_path ()

(* An equation of two applications of one function, one query per pair of
   arguments that differ; a goal of another shape goes to Z3 whole. *)
let rec congruence () : T.Tac unit =
  match T.cur_formula () with
  | T.Comp (T.Eq _) l r ->
      if T.term_eq l r then T.trefl ()
      else if same_head l r then begin
        T.apply_lemma (`congr);
        T.iseq [ congruence; same_or_smt ]
      end
      else smt_path ()
  | _ -> smt_path ()

(* [split_tests] on both sides: the walk's tests and the reading's, so that
   a query holds one path through both. Where the two answer alike, each
   output is a query of its own; where they do not, Z3 refutes the path. No
   query holds all the outputs of a record at once, which Z3 proves only by
   the luck of its search when the record has many (IPv4's thirteen). The
   reading is unfolded again on each branch: the normaliser leaves it folded
   under the test of the window's bounds. *)
let rec split_paths (names:list string) : T.Tac unit =
  match T.cur_formula () with
  | T.Comp (T.Eq _) l r -> (
      match (T.inspect l, T.inspect r) with
      | T.Tv_Match c _ _, _ | _, T.Tv_Match c _ _ ->
          T.cases_bool c;
          T.iseq [ path names; path names ]
      | _ -> congruence ())
  | _ -> smt_path ()

and path (names:list string) () : T.Tac unit =
  T.or_else
    (fun () -> let h = T.implies_intro () in T.grewrite_eq h)
    T.idtac;
  T.norm [delta_only names; zeta; iota; primops];
  split_paths names

(* The same for a walk whose values are [int64] or [int64_u] (the step
   walk, the OxCaml scanner, the walks of a frame): with the rules of its
   masks too, and each path split through the reading. *)
let unfold_step (names:list string) : T.Tac unit =
  ignore (T.pose_lemma (`WireGen.Tables.tables64 ()));
  T.norm [delta_only names; zeta; iota; primops];
  split_paths names
