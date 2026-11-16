(** The OCaml backend of the differential fuzzer: a module {!Wire_ocaml}
    generates from a codec, compared with the codec on the same inputs.

    The generated modules differ in their functions' names and types, so the
    glue written with each one hands them over as a {!t}, every value shown as a
    string. *)

(** Which of a field's getters: [get_<f>], [get_<f>_int] for a 32-bit field, or
    [<f>_of_word] on [get_<f>_word] for a bitfield. *)
type view = Get | Get_int | Of_word

type ops = {
  decode : bytes -> int -> string;
      (** The generated decoder, the record it returns written back at offset 0
          by the generated encoder. *)
  validate : bytes -> int -> unit;  (** The generated validator. *)
  encode : string -> bytes -> int -> unit;
      (** [encode s buf off] writes, through the generated encoder, the record
          the bytes [s] decode to. *)
  gets : (string * view * (bytes -> int -> string)) list;
      (** Each field's name, a getter of it and that getter. *)
  reads : (string * view) list;
      (** The reads {!field-valid} makes, in order: [read_<f>] as
          {!constructor-Get}, [read_<f>_int] as {!constructor-Get_int}. *)
  valid : bytes -> string list;
      (** [Valid.v] on the buffer, then each of {!field-reads}. *)
  is_valid : bytes -> int -> bool;  (** The generated [is_valid]. *)
  view : bytes -> int -> string list;
      (** [View.at buf off], then each of {!field-reads} through [view_<f>]. *)
  checked : bytes -> int -> bool * string list;
      (** [View.check buf off]: [View.ok] of it, then each of {!field-reads}
          through [view_<f>]. *)
}
(** One generated module's functions over one kind of buffer, taking [bytes]:
    the bigarray ones copy the bytes in, and back out after a write. *)

type t = { bytes : ops; bigstring : ops }
(** A generated module: its functions and those of its [Bigstring]. *)

val pp : t Fmt.t
(** [pp] prints the getters of a generated module. *)

val big : bytes -> Wire.Private.Unchecked.Bigstring.t
(** [big b] is a bigarray copy of [b]. *)

val blit_back : Wire.Private.Unchecked.Bigstring.t -> bytes -> unit
(** [blit_back bb b] copies [bb] over [b], of the same length. *)

val show_code : 'a Wire.Private.Types.typ -> string option
(** [show_code t] is the source of the function that shows a value of type [t]
    as a generated getter returns it: an integer in decimal, a byte array
    quoted. [None] for a type the backend does not show. *)

val views : 'a Wire.Private.Types.typ -> view list
(** [views t] is the getters the generated module has for a field of type [t].
*)

val reference : 'r Wire.Codec.t -> string -> bytes -> int -> string
(** [reference c name buf off] reads the field [name] of [c] at [off] in [buf]
    with the reader {!Wire.Codec.get} stages, and shows its value as
    {!show_code} shows a generated getter's: an integer in decimal, a byte array
    quoted. *)

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
    or raise the same error, every getter reads what {!Wire.Codec.get} reads or
    raises what it raises, encode writes the same bytes or raises the same error
    into buffers around the record, and [Valid.v] with its reads agrees with
    {!Wire.Codec.validate} then {!Wire.Codec.get}, on [bytes] and on a bigarray.
    Fails with {!Alcobar.failf}. *)

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
}
(** A generated scanner, through its decoder and its frame fold, or their
    unboxed forms. *)

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
    of the standalone and FFI projections, and the plug scalars of the FFI one.
    A read must not change [buf]. Fails with {!Alcobar.failf}. *)

val scan_corpus :
  ?oracle:Diff_codecs.c_oracle -> Diff_codecs.scan_case -> scan_ops list -> unit
(** [scan_corpus case backends] runs {!check_scan} on [case]'s corpus, and on
    good, bad and partial frames joined for [fold_frames]. *)

val scannable : 'r Wire.Codec.t -> bool
(** [scannable c] is [true] when [c] is in the scope {!Wire_ocaml.scanner}
    documents. *)

val scan_codecs : (string * Fuzz_gen.packed) list
(** [scan_codecs] is every sampled codec without parameters, fixed or not, and
    the flat records of {!codecs}. *)

val scan_report : string -> (string * Fuzz_gen.packed * string) list -> bool
(** [scan_report name refused] prints how many of {!scan_codecs} the scanner
    took, and is [false] when one it refused is {!scannable} and was refused for
    another reason than an ambiguous output name. *)
