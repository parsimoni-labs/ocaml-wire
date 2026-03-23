(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators.

    With [~output:true], the generated stubs call a validator that fills an
    output struct (from {!Wire.C.schema} [~output:true]), returning field values
    as an [int array]. Without it, stubs just validate ([bytes -> bool]).

    {b Typical usage}:
    {[
      let schema = Wire.C.schema ~output:true codec
      let () = Wire_3d.generate ~outdir:"schemas" [ schema ]
      let struct_ = Wire.C.struct_of_codec codec
      let c = Wire_stubs.to_c_stubs ~output:true [ struct_ ]
      let ml = Wire_stubs.to_ml_stubs ~output:true [ struct_ ]
    ]} *)

val everparse_name : string -> string
(** [everparse_name name] returns the EverParse-normalized identifier. *)

val ml_type_of : 'a Wire.typ -> string
(** [ml_type_of typ] returns the OCaml type name for a wire type. *)

val to_c_stubs : ?output:bool -> Wire.C.Raw.struct_ list -> string
(** [to_c_stubs ?output structs] generates C FFI stubs. When [~output:true],
    each stub calls Validate with an output struct and returns field values as
    an OCaml [int array]. *)

val to_ml_stubs : ?output:bool -> Wire.C.Raw.struct_ list -> string
(** [to_ml_stubs ?output structs] generates matching OCaml [external]
    declarations. *)

val to_ml_stub_name : Wire.C.Raw.struct_ -> string
(** Lowercase filename stem for one struct's stub module. *)

val to_ml_stub : ?output:bool -> Wire.C.Raw.struct_ -> string
(** [to_ml_stub ?output s] generates a single-struct stub module. *)

val to_external_typedefs : string -> string
(** [to_external_typedefs name] generates the [*_ExternalTypedefs.h] header that
    EverParse expects, defining [WIRECTX]. *)

val to_wire_setters : unit -> string
(** [to_wire_setters ()] generates C implementations of all [WireSet*] functions
    that store field values into an OCaml record via the [WIRECTX] context. *)
