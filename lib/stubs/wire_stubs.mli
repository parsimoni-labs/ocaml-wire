(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators. Each stub stack-
    allocates the schema's default plug struct ([<Name>Fields] from {!Wire_3d}),
    runs the EverParse validator against it, then marshals the populated struct
    members into an OCaml record. On validation failure, [Failure] is raised.

    {b Typical usage} (in a code-generation executable):
    {[
    open Wire

    type header = { version : int; length : int }

    let codec =
      Codec.v "Header"
        (fun version length -> { version; length })
        Codec.
          [
            (Field.v "Version" (bits ~width:4 U8) $ fun h -> h.version);
            (Field.v "Length" uint16be $ fun h -> h.length);
          ]

    let run () =
      Wire_stubs.generate ~schema_dir:"schemas" ~outdir:"."
        [ Wire_stubs.C codec ]
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
(** Generate C stubs that call the EverParse validators directly. *)

val build_codec_archive : schema_dir:string -> archive:string -> unit
(** [build_codec_archive ~schema_dir ~archive] compiles each generated validator
    and [_Fields] plug in [schema_dir] as its own translation unit and archives
    them into [archive] (pass [lib<name>.a] to consume it from a dune
    [(foreign_archives <name>)]). The [wire_ffi.c] from {!to_c_stubs} includes
    only the headers and calls the validators across this link, so per-codec
    shared types stay translation-unit-local and any set of codecs links.
    [test.c] and [<Name>Wrapper.c] are skipped. Requires [cc] and [ar]. *)

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
