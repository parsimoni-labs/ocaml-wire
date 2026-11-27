(** OCaml FFI stub generation for EverParse-produced C validators.

    [Wire_stubs] generates C stubs and matching OCaml [external] declarations so
    OCaml code can call EverParse-generated validators. Each stub stack-
    allocates the schema's default visitor struct ([<Name>Fields] from
    {!Wire_3d}), runs the EverParse validator against it, then marshals the
    populated struct members into an OCaml record.

    The stubs of a codec [Name] are the module [Stubs.Name]: its record type
    [t], the type [CODEC] of the codec value and a functor [Make] over it, whose
    [parse] answers [(t, Wire.Error.t) result]. The validator only accepts or
    refuses, and a refused record is input: its error is the one
    {!Wire.Codec.validate} answers for the codec, the bytes and the offset,
    computed in OCaml out of the validator's path. Where [Wire.Codec.validate]
    accepts a record the validator refused, the two disagree, a defect of one of
    them, and [parse] answers that the record fails as a whole: a
    [Constraint_failed { which = Where; value = None }] at the offset, with no
    field. A negative offset is misuse and raises [Invalid_argument].

    [Make] raises [Invalid_argument] when applied to a codec other than the one
    {!generate} wrote the stubs from (the codec's digest differs).

    {b Typical usage} (in a code-generation executable):
    {[
    open Wire

    type header = { version : int; length : UInt16.t }

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

    The program that links the stubs applies their functor to the same codec,
    [module H = Stubs.Header.Make (struct type r = header let codec = codec
     end)], and calls [H.parse buf off].

    This writes [wire_ffi.c] + [stubs.ml] into [outdir]. The [WIRECTX] socket
    and the [<Name>_Fields] visitor come from {!Wire_3d}; this module is just
    one particular consumer of that visitor. *)

type packed_codec = C : _ Wire.Codec.t -> packed_codec

val generate : schema_dir:string -> outdir:string -> packed_codec list -> unit
(** [generate ~schema_dir ~outdir codecs] writes all FFI artifacts. *)

val of_structs :
  schema_dir:string -> outdir:string -> Wire.Everparse.Raw.struct_ list -> unit
(** Same as {!generate} but takes raw structs directly. With no codec to take a
    digest of, the stubs' [Make] does not check the codec it is applied to. *)

val check_names : Wire.Everparse.Raw.struct_ -> unit
(** [check_names s] raises [Invalid_argument] naming both fields when
    {!of_structs} would refuse [s] because two of its fields spell one
    identifier: one 3D name in its FFI projection, one macro or member of its
    visitor ({!Wire_3d.check_visitor_names}), or one record label of its stubs.
    It writes nothing. *)

(** {1 Individual generators}

    These are the building blocks used by {!generate}. Most users should not
    need them directly. *)

val to_c_stubs : Wire.Everparse.Raw.struct_ list -> string
(** Generate C stubs that call the EverParse validators directly.

    @raise Invalid_argument
      naming both codecs when two names fold to one stub name, as [foo] and
      [FOO] do. *)

val build_codec_archive :
  ?cflags:string list -> schema_dir:string -> archive:string -> unit -> unit
(** [build_codec_archive ~schema_dir ~archive ()] compiles each generated
    validator and [_Fields] visitor in [schema_dir] as its own translation unit
    and archives them into [archive] (pass [lib<name>.a] to consume it from a
    dune [(foreign_archives <name>)]). The [wire_ffi.c] from {!to_c_stubs}
    includes only the headers and calls the validators across this link, so
    per-codec shared types stay translation-unit-local and any set of codecs
    links. [cflags] (default none, so [cc]'s own optimisation level) are passed
    to every compilation. [test.c] and [<Name>Wrapper.c] are skipped. Requires
    [cc] and [ar]. *)

val to_ml_stubs : Wire.Everparse.Raw.struct_ list -> string
(** Generate the OCaml stubs of all structs, each the module {!module_name}
    names, holding its record type [t] and its functor [Make], which checks no
    digest.

    @raise Invalid_argument
      naming both codecs when two names give one stub, as [foo] and [FOO] do, or
      naming a codec whose name gives no module. *)

val to_ml_stub : Wire.Everparse.Raw.struct_ -> string
(** Generate the contents of a standalone OCaml module, its record type [t] and
    its functor [Make], for one struct. *)

val module_name : Wire.Everparse.Raw.struct_ -> string
(** [module_name s] is the module of [s]'s stubs in {!to_ml_stubs}: its name,
    capitalised. Raises [Invalid_argument] if the name does not start with a
    letter or is not an identifier. *)

val to_ml_stub_name : Wire.Everparse.Raw.struct_ -> string
(** Derive the OCaml snake_case module name from a struct's CamelCase name. *)

val everparse_name : string -> string
(** Convert a Wire struct name to the EverParse CamelCase convention. *)

val ml_type_of : 'a Wire.typ -> string
(** Return the OCaml type name corresponding to a Wire type. *)
