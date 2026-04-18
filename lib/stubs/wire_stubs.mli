(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators. Each stub stack-
    allocates the schema's default plug struct ([<Name>Fields] from {!Wire_3d}),
    runs the EverParse validator against it, then marshals the populated struct
    members into an OCaml record. On validation failure, [Failure] is raised.

    {b Typical usage} (in a code-generation executable):
    {[
      let () =
        Wire_stubs.generate ~schema_dir:"schemas" ~outdir:"." [ C my_codec ]
    ]}

    This writes [wire_ffi.c] + [stubs.ml] into [outdir]. The [WIRECTX] socket
    and the [<Name>_Fields] plug come from {!Wire_3d}; this module is just one
    particular consumer of that plug. *)

type packed_codec = C : _ Wire.Codec.t -> packed_codec

val generate : schema_dir:string -> outdir:string -> packed_codec list -> unit
(** [generate ~schema_dir ~outdir codecs] writes all FFI artifacts. *)

val of_structs :
  schema_dir:string -> outdir:string -> Wire.Everparse.Raw.struct_ list -> unit
(** Same as {!generate} but takes raw structs directly. *)

(** {1 Individual generators}

    These are the building blocks used by {!generate}. Most users should not
    need them directly. *)

val to_c_stubs : Wire.Everparse.Raw.struct_ list -> string

val to_ml_stubs : Wire.Everparse.Raw.struct_ list -> string
(** Generate OCaml external declarations and record types for all structs. *)

val to_ml_stub : Wire.Everparse.Raw.struct_ -> string
(** Generate a standalone OCaml module with parse and parse_k for one struct. *)

val to_ml_stub_name : Wire.Everparse.Raw.struct_ -> string
(** Derive the OCaml snake_case module name from a struct's CamelCase name. *)

val everparse_name : string -> string
(** Convert a Wire struct name to the EverParse CamelCase convention. *)

val ml_type_of : 'a Wire.typ -> string
(** Return the OCaml type name corresponding to a Wire type. *)
