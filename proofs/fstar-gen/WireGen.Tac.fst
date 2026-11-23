module WireGen.Tac

(* Written once: unfold a 3d.exe parser denotation (as_parser of the typ
   3d.exe wrote) to the LowParse combinators it denotes, then hand the goal
   to Z3 with the equations of WireGen.Lib. [names] are the codec's own
   definitions: its proof's spec and the def/def'/parser 3d.exe wrote. *)

module T = FStar.Tactics
open FStar.List.Tot

(* The combinators that only retype a LowParse parser at a 3D kind stay
   folded (parse_nlist, parse_t_exact, parse_t_at_most): unfolded, Z3 would
   lose the kind their type gives the parser. WireGen.Lib3d states them. *)
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

(* For a Flat lemma: the 3D parser against the field-by-field reading. *)
let unfold_flat (names:list string) : T.Tac unit =
  T.norm [delta_only (names @ interp); zeta; iota; primops];
  T.smt ()

(* For a Proof lemma: the rendering against the Flat reading, with the
   rules of WireGen.OCaml at the emitter's literals. *)
let unfold_scan (names:list string) : T.Tac unit =
  ignore (T.pose_lemma (`WireGen.Tables.tables ()));
  T.norm [delta_only names; zeta; iota; primops];
  T.smt ()

(* The same, with the facts of a lemma [lets buf base] about each let of
   the walk posed instead of the tables: the variables of the goal named
   [buf] and [base] are its arguments. *)
let unfold_lets (lets:T.term) (names:list string) : T.Tac unit =
  let rec pick (n:string) (bs:list T.binding) : T.Tac T.term =
    match bs with
    | b :: rest -> if T.unseal b.ppname = n then T.binding_to_term b else pick n rest
    | [] -> T.fail ("no variable " ^ n) in
  let find (n:string) : T.Tac T.term = pick n (List.Tot.rev (T.cur_vars ())) in
  ignore (T.pose_lemma (T.mk_e_app lets [find "buf"; find "base"]));
  T.norm [delta_only names; zeta; iota; primops];
  T.smt ()
