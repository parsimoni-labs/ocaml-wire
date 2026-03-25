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

      let () = Diff_gen.generate ~outdir:"schemas" ~outdir:"." schemas
    ]} *)

type schema = Wire.Everparse.t
(** Same as {!Wire.Everparse.t}. *)

val schema :
  name:string ->
  struct_:Wire.Everparse.Raw.struct_ ->
  module_:Wire.Everparse.Raw.module_ ->
  schema option
(** [schema ~name ~struct_ ~module_] creates a schema if the struct has a known
    fixed wire size. Returns [None] for variable-length structs. *)

(** {1 Full Pipeline} *)

val generate :
  schema_dir:string -> outdir:string -> ?num_values:int -> schema list -> unit
(** [generate ~schema_dir ~outdir schemas] runs the full pipeline: 1. Generate
    .3d files in [schema_dir] 2. Invoke EverParse to produce C parsers 3.
    Generate [stubs.c], [stubs.ml], [diff_test.ml] in [outdir]

    Requires EverParse installed at [~/.local/everparse/]. *)

(** {1 Individual Steps} *)

val generate_3d_files : outdir:string -> schema list -> unit
(** Delegates to {!Wire_3d.generate_3d}. *)

val run_everparse : ?quiet:bool -> outdir:string -> schema list -> unit
(** Delegates to {!Wire_3d.run_everparse}. *)

val generate_c_stubs : schema_dir:string -> outdir:string -> schema list -> unit
(** Generate [stubs.c] with OCaml C stubs using {!Wire_3d.everparse_name}. *)

val generate_ml_stubs : outdir:string -> schema list -> unit
(** Generate [stubs.ml] with OCaml externals. *)

val generate_test_runner :
  outdir:string -> ?num_values:int -> schema list -> unit
(** [generate_test_runner ~outdir schemas] generates [diff_test.ml] test runner.
*)
