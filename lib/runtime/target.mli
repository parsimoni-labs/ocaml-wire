(** What generated code needs of the compiler it is written for: loads at an
    [int64_u] offset on OxCaml, nothing on OCaml. *)

include module type of struct
  include Loads
end
