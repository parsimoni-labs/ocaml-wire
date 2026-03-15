(** Benchmark schemas covering all Wire API constructs. *)

type 'a schema = private {
  name : string;
  codec : 'a Wire.Codec.t;
  struct_ : Wire.struct_;
  size : int;
  default : 'a;
  make_data : int -> bytes array;
  decode : bytes -> int -> 'a;
  encode : 'a -> bytes -> int -> unit;
}
(** Type-erased schema for benchmark iteration. *)

type any_schema = Any : 'a schema -> any_schema

val all_schemas : any_schema list
(** [all_schemas] is the list of all benchmark schemas. *)

val all_structs : Wire.struct_ list
(** [all_structs] is the list of all benchmark structs. *)

(** {2 CLCW field accessors for zero-copy benchmarks} *)

type clcw

val cw_report : (int, clcw) Wire.Codec.field
(** Zero-copy accessor for the CLCW ReportValue field. *)
