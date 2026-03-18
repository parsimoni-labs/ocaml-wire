(** Code generation for EverParse differential testing.

    Generates .3d files from Wire schemas, invokes EverParse, and produces C
    stubs, OCaml externals, and a test runner for comparing OCaml codecs against
    EverParse-generated C parsers.

    {b Typical usage:}
    {[
      (* In gen_c.ml *)
      let schemas =
        [
          Diff_gen.schema ~name:"MyFrame" ~struct_:My_3d.frame_struct
            ~module_:My_3d.frame_module
          |> Option.get;
        ]

      let () = Diff_gen.generate ~schema_dir:"schemas" ~outdir:"." schemas
    ]} *)

type schema
(** A schema bundles a Wire struct, module, and computed wire size. *)

val schema :
  name:string ->
  struct_:Wire.C.struct_ ->
  module_:Wire.C.module_ ->
  schema option
(** [schema ~name ~struct_ ~module_] creates a schema if the struct has a known
    fixed wire size. Returns [None] for variable-length structs. *)

(** {1 Full Pipeline} *)

val generate :
  schema_dir:string -> outdir:string -> ?num_values:int -> schema list -> unit
(** [generate ~schema_dir ~outdir schemas] runs the full pipeline: 1. Generates
    .3d files in [schema_dir] 2. Invokes EverParse to produce C parsers 3.
    Generates [stubs.c], [stubs.ml], [diff_test.ml] in [outdir]

    Requires EverParse installed at [~/.local/everparse/]. *)

(** {1 Individual Steps} *)

val generate_3d_files : schema_dir:string -> schema list -> unit

val run_everparse : schema_dir:string -> unit
(** [run_everparse ~schema_dir] invokes EverParse on all [.3d] files in
    [schema_dir]. *)

val generate_c_stubs : schema_dir:string -> outdir:string -> schema list -> unit
(** [generate_c_stubs ~schema_dir ~outdir schemas] generates [stubs.c] with
    OCaml C stubs. *)

val generate_ml_stubs : outdir:string -> schema list -> unit
(** [generate_ml_stubs ~outdir schemas] generates [stubs.ml] with OCaml
    externals. *)

val generate_test_runner :
  outdir:string -> ?num_values:int -> schema list -> unit
(** [generate_test_runner ~outdir schemas] generates [diff_test.ml] test runner.
*)
