(** The OCaml backend of the differential fuzzer: a module {!Wire_ocaml}
    generates from a codec, compared with the codec on the same inputs.

    The generated modules differ in their functions' names and types, so the
    glue written with each one hands them over as a {!t}, every value shown as a
    string. *)

type t = {
  decode : bytes -> int -> (string, Wire.Error.t) result;
      (** The generated decoder, the record it returns written back at offset 0
          by the generated encoder. *)
  validate : bytes -> int -> (unit, Wire.Error.t) result;
      (** The generated validator. *)
  encode : string -> bytes -> int -> unit;
      (** [encode s buf off] writes, through the generated encoder, the record
          the bytes [s] decode to. *)
  reads : string list;  (** The fields the accessors read, in order. *)
  valid : bytes -> (string list, Wire.Error.t) result;
      (** [validate buf 0], then the module's check at offset 0 and each of
          {!field-reads} through its accessor. *)
  view : bytes -> int -> (string list, Wire.Error.t) result;
      (** [validate buf off], then the module's check and each of {!field-reads}
          through its accessor. *)
  checked : bytes -> int -> bool * string list;
      (** [check buf off]: whether the check passed, then each of {!field-reads}
          through its accessor. *)
}
(** A generated module's functions, every value shown as a string. *)

exception Refused of Wire.Error.t
(** The error {!ok} raises. *)

val ok : ('a, Wire.Error.t) result -> 'a
(** [ok r] is the value of [r], its error raised as {!Refused} for {!reference}
    to compare as the codec's reader answers it. *)

val pp : t Fmt.t
(** [pp] prints the fields a generated module's accessors read. *)

val show_code : 'a Wire.Private.Types.typ -> string option
(** [show_code t] is the source of the function that shows a value of type [t]
    as a generated getter returns it: an integer in decimal, a byte array
    quoted. [None] for a type the backend does not show. *)

val reference : 'r Wire.Codec.t -> string -> bytes -> int -> string
(** [reference c name buf off] reads the field [name] of [c] at [off] in [buf]
    with the reader {!Wire.Codec.get} stages, and shows its value as
    {!show_code} shows a generated getter's: an integer in decimal, a byte array
    quoted. The reader's error is raised as {!Refused}. *)

val codecs : (string * Fuzz_gen.packed) list
(** [codecs] is the codecs the backend compares: the {!Diff_codecs.candidates}
    {!Diff_codecs.included} takes or excludes only for a
    {!Diff_codecs.Name_clash} of the FFI pipeline, then the flat records
    ({!Fuzz_gen.flat_records}) of a longer run of the same sample that are
    {!specialisable}. *)

val specialisable : 'r Wire.Codec.t -> bool
(** [specialisable c] is [true] when [c] is in the scope {!Wire_ocaml}
    documents: a fixed-size record without parameters of named fields without
    actions, each an integer of up to 32 bits, a bitfield or a byte array,
    refined by integer expressions. *)

val check : string -> Fuzz_gen.packed -> t -> bytes -> unit
(** [check label p m buf] compares [m], generated from [p]'s codec, with the
    codec on [buf] at several offsets: decode and validate give the same value
    or the same error, encode writes the same bytes or raises the same error
    into buffers around the record, and [check] with its accessors agrees with
    {!Wire.Codec.validate} then {!Wire.Codec.get}. Fails with {!Alcobar.failf}.
*)

val report : string -> (string * Fuzz_gen.packed * string) list -> bool
(** [report name refused] prints how many of the sampled {!codecs}, and of the
    flat records, the generator refused and why, and is [false] when one of
    [refused] (a label, the codec and the refusal) is {!specialisable} and was
    refused for another reason than a clash of its field names. *)

(** {1 Scanners} *)

type scan_ops = {
  scan_name : string;
  scan_fields : string list;  (** The outputs it hands out. *)
  scan : bytes -> int -> int -> (string * int) list option;
      (** [scan buf off len] is the outputs of the record in the [len] bytes at
          [off], a presence as 0 or 1, or [None] when it rejects them. *)
  scan_batch :
    (bytes -> int -> int -> int -> (string * int) list option list option)
    option;
      (** The frame fold with a stride: each frame's outputs, or [None] when it
          rejects the bounds. *)
  scan_index :
    (bytes -> int array -> int array -> (string * int) list option list) option;
      (** The fold over an index, [offs] and [lens]: each frame's outputs, in
          order. *)
}
(** A generated scanner, through its decoder and its frame fold, or their
    unboxed forms. *)

val view_scan :
  name:string ->
  codec:'r Wire.Codec.t ->
  fields:string list ->
  full:(bytes -> int -> int -> (string * int) list option) ->
  (bytes -> int -> int -> (string * int) list option) ->
  scan_ops
(** [view_scan ~name ~codec ~fields ~full view] is the backend of the views of a
    scanner of [codec]: [view buf off len] checks the record at [off] in the
    [len] bytes there and reads each of [fields] through the accessors, an
    absent one as 0. Each answer is compared, before {!check_scan} compares it
    with the reference, with [Wire.Codec.View.check] on the frame's bytes alone
    and with [full], the scanner's own outputs, every one of them, those in
    records a field holds included. Fails with {!Alcobar.failf}. *)

type values = {
  decode : bytes -> int -> (string, Wire.Error.t) result;
      (** The generated decoder, the record it returns written back by the
          generated encoder in the bytes the generated size measures, or the
          error. *)
  validate : bytes -> int -> (unit, Wire.Error.t) result;
      (** The generated [validate]. *)
}
(** The values of a generated module of any shape. *)

val values_scan :
  name:string ->
  codec:'r Wire.Codec.t ->
  fields:string list ->
  full:(bytes -> int -> int -> (string * int) list option) ->
  values ->
  scan_ops
(** [values_scan ~name ~codec ~fields ~full v] is the backend of the values of a
    module of [codec]: on each frame, the bytes alone, [v]'s decode gives what
    {!Wire.Codec.decode} gives, written back by the generated and the codec's
    encoders alike, or the same error, and its validate agrees with
    {!Wire.Codec.validate}; it then hands out [full], the module's own outputs.
    Fails with {!Alcobar.failf}. *)

(** What a scanner is compared with: the codec, and the outputs it states for a
    frame it decodes ([known], or all of them when [None]). *)
type reference =
  | Reference : {
      label : string;
      codec : 'a Wire.Codec.t;
      known : string list option;
      project : bytes -> 'a -> (string * int) list;
      inputs : bytes -> bytes list;
    }
      -> reference

val of_scan_case : Diff_codecs.scan_case -> reference
(** [of_scan_case c] states every output of [c]. *)

val of_sampled : string -> Fuzz_gen.packed -> reference
(** [of_sampled label p] states the top-level integer fields of [p]'s codec,
    read by the reader {!Wire.Codec.get} stages. *)

val check_scan :
  ?oracle:Diff_codecs.c_oracle ->
  ?strides:int list ->
  ?expand:bool ->
  reference ->
  scan_ops list ->
  bytes ->
  unit
(** [check_scan ?oracle r backends buf] compares each scanner with [r]'s codec
    on frames of [buf] at several offsets, bounds past the buffer included: the
    outputs, acceptance against {!Wire.Codec.decode}, its general path and
    {!Wire.Codec.validate}, [fold_frames] at several strides, and, with
    [oracle], the acceptance and consumed prefix of the EverParse C validators
    of the standalone and FFI projections, and the visitor scalars of the FFI
    one. A read must not change [buf]. Fails with {!Alcobar.failf}. *)

val scan_corpus :
  ?oracle:Diff_codecs.c_oracle -> Diff_codecs.scan_case -> scan_ops list -> unit
(** [scan_corpus case backends] runs {!check_scan} on [case]'s corpus, and on
    good, bad and partial frames joined for [fold_frames]. *)

val scannable : 'r Wire.Codec.t -> bool
(** [scannable c] is [true] when [c] is in the scope {!Wire_ocaml.v} documents
    for a fold. *)

val scan_codecs : (string * Fuzz_gen.packed) list
(** [scan_codecs] is every sampled codec without parameters, fixed or not, and
    the flat records of {!codecs}. *)

val scan_report : string -> (string * Fuzz_gen.packed * string) list -> bool
(** [scan_report name refused] prints how many of {!scan_codecs} the scanner
    took, and is [false] when one it refused is {!scannable} and was refused for
    another reason than an ambiguous output name. *)
