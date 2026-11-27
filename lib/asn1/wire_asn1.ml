(* The interface docs/asn1.md designs. Every function raises until it is
   implemented. *)

let todo name = Fmt.invalid_arg "Wire_asn1.%s: not implemented" name
let tool () = todo "tool"
let c_name _ = todo "c_name"
let run_everparse ?quiet:_ ~outdir:_ _ = todo "run_everparse"
let run ?quiet:_ ~outdir:_ _ = todo "run"
let generate_dune ~outdir:_ ~package:_ _ = todo "generate_dune"

type packed = unit

let pack _ = todo "pack"
let generate_corpus ?count:_ _ _ = todo "generate_corpus"
let generate_agree ?name:_ ~outdir:_ ~package:_ _ = todo "generate_agree"
