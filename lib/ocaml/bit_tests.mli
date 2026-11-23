(** Tests of bitfields as tests of their word. Internal to {!Wire_ocaml}. *)

val bit_predicate :
  (string * int) list ->
  bool Wire.Private.Types.expr ->
  bool Wire.Private.Types.expr
(** [bit_predicate words e] is [e] with each test that bitfields of one word are
    zero, or not, read as one test of the word under a mask: [words] are the
    variables that hold a word, each with its mask. *)
