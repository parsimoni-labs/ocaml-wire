(** Staged computations: {!Wire_runtime.Staged}, so a staged value of a
    generated module and of a codec are one type. *)

include module type of struct
  include Wire_runtime.Staged
end
