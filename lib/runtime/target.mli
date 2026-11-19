(** What generated code needs of the compiler it is written for: on OxCaml, the
    loads and stores of a buffer that may be [local], with unboxed 32-bit and
    64-bit values; nothing on OCaml. *)

include module type of struct
  include Loads
end
