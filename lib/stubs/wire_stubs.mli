(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators. The generated stubs call
    a validator that fills an output struct (from {!Wire.Everparse.schema}),
    returning field values as an OCaml record via extern callbacks ([WireSet*]).

    {b Typical usage}:
    {[
      let schema = Wire.Everparse.schema codec
      let () = Wire_3d.run ~outdir:"schemas" [ schema ]
      let struct_ = Wire.Everparse.struct_of_codec codec
      let c = Wire_stubs.to_c_stubs [ struct_ ]
      let ml = Wire_stubs.to_ml_stubs [ struct_ ]
    ]} *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier. *)

val ml_type_of : 'a Wire.typ -> string
(** [ml_type_of typ] returns the OCaml type name for a wire type. *)

val to_c_stubs : Wire.Everparse.Raw.struct_ list -> string
(** [to_c_stubs structs] generates C FFI stubs. Each stub calls Validate with a
    [WIRECTX] context and returns field values as an OCaml record. *)

val to_ml_stubs : Wire.Everparse.Raw.struct_ list -> string
(** [to_ml_stubs structs] generates matching OCaml [external] declarations. *)

val to_ml_stub_name : Wire.Everparse.Raw.struct_ -> string
(** Lowercase filename stem for one struct's stub module. *)

val to_ml_stub : Wire.Everparse.Raw.struct_ -> string
(** [to_ml_stub s] generates a single-struct stub module. *)

val to_external_typedefs : string -> string
(** [to_external_typedefs name] generates the [*_ExternalTypedefs.h] header that
    EverParse expects, defining [WIRECTX]. *)

val to_wire_setters : unit -> string
(** [to_wire_setters ()] generates C implementations of all [WireSet*] functions
    that store field values into an OCaml record via the [WIRECTX] context. *)
