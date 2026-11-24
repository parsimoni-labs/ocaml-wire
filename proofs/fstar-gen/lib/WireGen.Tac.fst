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

(* For a Proof lemma: the rendering against the Flat reading, with the
   rules of WireGen.OCaml at the emitter's literals. *)
let unfold_scan (names:list string) : T.Tac unit =
  ignore (T.pose_lemma (`WireGen.Tables.tables ()));
  T.norm [delta_only names; zeta; iota; primops];
  T.smt ()
