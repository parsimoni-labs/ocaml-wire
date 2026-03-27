(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators. The generated stubs call
    a validator that fills an output struct (from {!Wire.Everparse.schema}),
    returning field values as an OCaml record option via extern callbacks
    ([WireSet*]). On validation failure, [None] is returned.

    {b Typical usage} (in a code-generation executable):
    {[
      let () =
        Wire_stubs.generate ~schema_dir:"schemas" ~outdir:"." [ C my_codec ]
    ]}

    This writes [*_ExternalTypedefs.h] into [schema_dir], and [wire_ffi.c] +
    [stubs.ml] into [outdir]. *)

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
val to_ml_stub : Wire.Everparse.Raw.struct_ -> string
val to_ml_stub_name : Wire.Everparse.Raw.struct_ -> string
val to_external_typedefs : string -> string
val to_wire_setters : unit -> string
val everparse_name : string -> string
val ml_type_of : 'a Wire.typ -> string
