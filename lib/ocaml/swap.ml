open Planner

(* The constructor of [Wire.Private.Gen.kind] for what a handle of [f]
   reads. *)
let kind f =
  match f.store with
  | Wire.Private.Layout.Int { width = 8; signed = false; _ } -> "UInt8"
  | Int { width = 8; signed = true; _ } -> "SInt8"
  | Int { width = 16; signed = false; _ } -> "UInt16"
  | Int { width = 16; signed = true; _ } -> "SInt16"
  | Int { signed = false; _ } -> "UInt32"
  | Int { signed = true; _ } -> "SInt32"
  | Bits _ -> "Int"
  | Bytes _ -> "String"

let gen = "Wire.Private.Gen"

let str pp =
  let b = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer b in
  pp ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* [Interpreted]: [Wire.Codec.Make] with a handle of each of [fields]. *)
let pp_interpreted ppf ~record ~codec fields =
  Fmt.pf ppf
    "@\n\
     module Interpreted = struct@\n\
    \  module M = Wire.Codec.Make (struct@\n\
    \    type r = %s@\n\
     @\n\
    \    let codec = %s@\n\
    \  end)@\n\
     @\n\
    \  type t = M.t@\n\
    \  type r = M.r@\n\
     @\n\
    \  let min_size = M.min_size@\n\
    \  let decode = M.decode@\n\
    \  let decode_exn = M.decode_exn@\n\
    \  let encode = M.encode@\n\
    \  let validate = M.validate@\n\
    \  let validate_exn = M.validate_exn@\n\
    \  let is_valid = M.is_valid@\n\
    \  let field = M.field@\n\
     @\n\
    \  module View = M.View@\n\
     @\n\
    \  module Field = struct@\n\
    \    include M.Field@\n\
     @\n"
    record codec;
  List.iter
    (fun f ->
      Fmt.pf ppf "    let %s = M.field (%s.field %s %S %s.%s)@\n" f.label gen
        codec f.name gen (kind f))
    fields;
  Fmt.pf ppf "  end@\nend@\n"

(* [Global]: the [Global] of [core] with [field], which maps a field of the
   codec to its handle. *)
let pp_global ppf ~record ~core fields =
  Fmt.pf ppf
    "@\n\
     module Global = struct@\n\
    \  include %s.Global@\n\
     @\n\
    \  type r = %s@\n\
     @\n\
    \  let field : type a. (a, r) Wire.Codec.field -> a Field.t =@\n\
    \   fun f ->@\n\
    \    ignore (Interpreted.field f : a Interpreted.Field.t);@\n\
    \    let no () =@\n\
    \      invalid_arg@\n\
    \        (%S ^ %s.field_name f)@\n\
    \    in@\n\
    \    match %s.field_name f with@\n"
    core record
    (Fmt.str "%s.Global.field: no handle for the field " core)
    gen gen;
  List.iter
    (fun f ->
      Fmt.pf ppf
        "    | %S -> (@\n\
        \        match %s.field_eq f %s.%s with@\n\
        \        | Some Type.Equal -> Field.%s@\n\
        \        | None -> no ())@\n"
        f.name gen gen (kind f) f.label)
    fields;
  Fmt.pf ppf "    | _ -> no ()@\nend@\n"

let record_ml ~record ~codec ~core fields =
  str (fun ppf ->
      pp_interpreted ppf ~record ~codec fields;
      pp_global ppf ~record ~core fields)

(* The interface of [Interpreted.View], that of [Wire.Codec.Make]'s. *)
let view_mli =
  {|  (** Records checked once in a buffer. *)
  module View : sig
    type 'b t = private int
    (** The type for records that passed their checks in the buffer ['b]. *)

    type 'b check = private int
    (** The type for the answers of {!val-check}. *)

    val at : 'b Wire_runtime.Buf.t -> 'b Wire_runtime.Buf.length -> int -> 'b t
    (** [at b len off] is the record at [off] in [b]. *)

    val check :
      'b Wire_runtime.Buf.t -> 'b Wire_runtime.Buf.length -> int -> 'b check
    (** [check b len off] checks the record at [off] in [b], without raising. *)

    val ok : 'b check -> bool
    (** [ok c] is [true] iff the check [c] passed. *)

    val get : 'b check -> 'b t
    (** [get c] is the record the check [c] passed. *)
  end

|}

(* The interface of [Interpreted]: its views and handles, then the rest of a
   [Wire.S]. *)
let pp_interpreted_mli ppf ~record ~core fields =
  Fmt.pf ppf
    "@\n\
     (** [%s] as a [Wire.S] read by [Wire.Codec.Make], with the same named \
     handles: code written against one runs on the other by changing one line. \
     *)@\n\
     module Interpreted : sig@\n\
     %s  (** Field handles. *)@\n\
    \  module Field : sig@\n\
    \    type 'a t = private {@\n\
    \      get : bytes -> int -> 'a;@\n\
    \      view : 'b. 'b Wire_runtime.Buf.t -> 'b View.t -> 'a;@\n\
    \    }@\n\
    \    (** The type for fields holding an ['a]. *)@\n\
     @\n"
    core view_mli;
  List.iter
    (fun f ->
      Fmt.pf ppf
        "    val %s : %s t@\n    (** [%s] reads the field [%s]. *)@\n@\n"
        f.label
        (Record_module.schema_type f)
        f.label f.name)
    fields;
  Fmt.pf ppf
    "  end@\n\
     @\n\
    \  include@\n\
    \    Wire.S@\n\
    \      with type t = %s@\n\
    \       and type r = %s@\n\
    \       and module View := View@\n\
    \       and module Field := Field@\n\
     end@\n"
    record record

let pp_global_mli ppf ~record ~core =
  Fmt.pf ppf
    "@\n\
     (** [%s.Global] as a [Wire.S]: [field f] is the module's own handle of \
     [f]. *)@\n\
     module Global : sig@\n\
    \  include module type of struct@\n\
    \    include %s.Global@\n\
    \  end@\n\
     @\n\
    \  type r = %s@\n\
    \  (** The type for the records of the codec. *)@\n\
     @\n\
    \  val field : ('a, r) Wire.Codec.field -> 'a Field.t@\n\
    \  (** [field f] is the handle of [f]. Raises [Invalid_argument] if [f] is \
     not a field of the codec or has no handle. *)@\n\
     end@\n"
    core core record

let record_mli ~record ~core fields =
  str (fun ppf ->
      pp_interpreted_mli ppf ~record ~core fields;
      pp_global_mli ppf ~record ~core)

let consumer outputs =
  match outputs with
  | [] -> "unit"
  | os ->
      String.concat " -> "
        (List.map
           (fun (o : output) ->
             o.name ^ ":" ^ if o.is_bool then "bool" else "int64")
           os)

let step_type outputs =
  Fmt.str
    "module type STEP = sig@\n\
    \  type t@\n\
     @\n\
    \  val invalid : t -> t@\n\
    \  val valid : t -> %s -> t@\n\
     end@\n"
    (consumer outputs)

(* [Global], the scanner [core] as a [Wire.Scan]: on OxCaml its step functions
   take their buffer [local], so [Global] states them for a buffer on the
   heap. *)
let global_ml ~target ~core =
  match target with
  | `OCaml -> Fmt.str "@\nmodule Global = %s@\n" core
  | `OxCaml ->
      Fmt.str
        "@\n\
         module Global = struct@\n\
        \  type step = %s.step@\n\
         @\n\
        \  let fold_step buf off len ~init = %s.fold_step buf off len ~init@\n\
         @\n\
        \  let fold_step_frames buf off len ~stride ~init =@\n\
        \    %s.fold_step_frames buf off len ~stride ~init@\n\
         @\n\
        \  let fold_step_index buf ~offs ~lens ~init =@\n\
        \    %s.fold_step_index buf ~offs ~lens ~init@\n\
         end@\n"
        core core core core

let scanner_ml ~target ~codec ~core outputs =
  global_ml ~target ~core
  ^
  let args =
    match outputs with
    | [] -> " ()"
    | os ->
        String.concat ""
          (List.map
             (fun (o : output) ->
               Fmt.str " ~%s:(%s %S)" o.name
                 (if o.is_bool then "b" else "i")
                 o.name)
             os)
  in
  let bind =
    if outputs = [] then ""
    else
      Fmt.str "      let i = %s.Outputs.int o and b = %s.Outputs.bool o in@\n"
        gen gen
  in
  let o = if outputs = [] then "_" else "o" in
  Fmt.str
    "@\n\
     %s@\n\
     module Interpreted_scan (K : STEP) = %s.Scan (struct@\n\
    \  type t = K.t@\n\
     @\n\
    \  let scanner = %s.scanner %s@\n\
    \  let invalid = K.invalid@\n\
     @\n\
    \  let valid acc %s =@\n\
     %s      K.valid acc%s@\n\
     end)@\n"
    (step_type outputs) gen gen codec o bind args

let global_mli ~target ~core =
  match target with
  | `OCaml ->
      Fmt.str
        "@\n\
         (** [%s], a [Wire.Scan] with its own step module. *)@\n\
         module Global = %s@\n"
        core core
  | `OxCaml ->
      Fmt.str
        "@\n\
         (** [%s] as a [Wire.Scan], on buffers on the heap. *)@\n\
         module Global : Wire.Scan with type step = %s.step@\n"
        core core

let scanner_mli ~target ~core outputs =
  global_mli ~target ~core
  ^ Fmt.str
      "@\n\
       (** The consumer of [%s]'s step functions. *)@\n\
       %s@\n\
       (** [Interpreted_scan (K)] is the scanner of the codec read by \
       [Wire.Codec], folding [K]: code written against [Wire.Scan] runs on it \
       or on [Global] with [K] as the step module of [%s] by changing one \
       line. *)@\n\
       module Interpreted_scan (K : STEP) : Wire.Scan with type step = K.t@\n"
      core (step_type outputs) core
