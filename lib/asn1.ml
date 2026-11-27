(* The interface docs/asn1.md designs. Every function raises until it is
   implemented. *)

let todo name = Fmt.invalid_arg "Wire.Asn1.%s: not implemented" name

type 'a t = Todo
type cls = [ `Universal | `Application | `Private ]

module Oid = struct
  type t = unit

  let base _ _ = todo "Oid.base"
  let ( <| ) _ _ = todo "Oid.( <| )"
  let ( <|| ) _ _ = todo "Oid.( <|| )"
  let equal _ _ = todo "Oid.equal"
  let compare _ _ = todo "Oid.compare"
  let of_string _ = todo "Oid.of_string"
  let to_string _ = todo "Oid.to_string"
  let pp _ _ = todo "Oid.pp"
end

let bool = Todo
let integer = Todo
let bounded_integer ~min:_ ~max:_ = todo "bounded_integer"
let enumerated _ = todo "enumerated"
let null = Todo
let oid = Todo
let bit_string = Todo
let octet_string = Todo
let utf8_string = Todo
let printable_string = Todo
let ia5_string = Todo
let numeric_string = Todo
let visible_string = Todo
let teletex_string = Todo
let bmp_string = Todo
let universal_string = Todo
let utc_time = Todo
let generalized_time = Todo
let any = Todo
let size ~min:_ ~max:_ _ = todo "size"
let implicit ?cls:_ _ _ = todo "implicit"
let explicit ?cls:_ _ _ = todo "explicit"
let required _ _ = todo "required"
let optional _ _ = todo "optional"
let default _ _ _ = todo "default"
let sequence _ _ _ = todo "sequence"
let set _ _ _ = todo "set"
let sequence_of ?non_empty:_ _ = todo "sequence_of"
let set_of ?non_empty:_ _ = todo "set_of"

type 'a alternative = unit

let alternative _ _ ~inject:_ ~project:_ = todo "alternative"
let choice _ = todo "choice"

type ('a, 'k) case = unit

let case _ _ ~inject:_ ~project:_ = todo "case"
let absent _ ~inject:_ ~project:_ = todo "absent"
let fallback ?optional:_ _ ~inject:_ ~project:_ = todo "fallback"
let defined_by _ ~on:_ _ = todo "defined_by"
let keep_bytes _ = todo "keep_bytes"
let containing _ = todo "containing"
let map ~decode:_ ~encode:_ _ = todo "map"
let codec _ = todo "codec"
let fold _ _ _ _ ~init:_ ~f:_ = todo "fold"

type schema = unit

let project _ _ = todo "project"
let contained _ = todo "contained"
let name _ = todo "name"
let filename _ = todo "filename"
let roots _ = todo "roots"
let pp _ _ = todo "pp"
let write ~outdir:_ _ = todo "write"
