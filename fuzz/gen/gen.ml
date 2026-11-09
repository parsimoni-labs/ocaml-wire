(** Fuzz generators that mirror Wire's surface.

    Each ['a t] is the codec it tests paired with three Alcobar generators:
    [positive] produces a value together with bytes that decode to it, [random]
    produces arbitrary bytes the decoder must handle without raising, and
    [adversarial] produces bytes crafted at the boundary of every constraint the
    codec checks (lengths, gate flips, bit-pattern extremes). [run] turns one
    ['a t] into a set of Alcobar test cases that exercise round-trip on
    positives and crash-safety on the other two streams.

    [Codec.v] composes leaves into record gens the same way [Wire.Codec.v]
    composes typs into record codecs -- a record gen is itself an ['a t], so
    composition is recursive. *)

type 'a t = {
  codec : 'a Wire.Codec.t;
  positive : ('a * bytes) Alcobar.gen;
  random : bytes Alcobar.gen;
  adversarial : bytes Alcobar.gen;
  equal : 'a -> 'a -> bool;
  typ : 'a Wire.typ;
      (* Wire typ form, used when this gen slots into a parent record's
         field list. For leaves it is the bare typ; for records it is
         [Wire.codec built_codec], i.e. a sub-codec wrap. *)
  env : env_strategy option;
      (* When the codec references [Param.input], the test driver pulls
         a fresh env from [positive] for the round-trip case and from
         the [fuzz] gen for random and adversarial streams. *)
  adversarial_value : 'a Alcobar.gen option;
      (* Hostile VALUES, the counterpart of the hostile BYTES in [random] /
         [adversarial]. Each is a well-typed value of the codec's own OCaml
         type that violates something decode enforces: enum membership,
         all-zero padding, a per-byte span predicate, a declared byte length,
         a where cond or a field constraint. [None] where the shape has no
         refinement left to violate (a plain scalar, an open enum). *)
  fields : 'a field_check list;
      (* The record's own field list, kept so a property can address one field
         at a time. [Codec.v] holds each field's Wire field and projection while
         it builds the codec and would otherwise drop both once the codec is
         sealed, which is why the registry has [Codec.get] / [Codec.set]
         coverage only through hand-built codecs. [] where a shape has no
         addressable fields; [addressed] then falls back to the one-field
         wrapper [codec_of_typ] builds over the gen's typ. *)
}

(* One addressable field of a record gen. The field type ['a] differs from
   entry to entry, so a list of them only exists once ['a] is packed away
   behind the three things a per-field property needs: the Wire field that
   names the slot to [Codec.get] / [Codec.set], the projection out of the
   record, and the equality to compare two reads with. *)
and 'r field_check =
  | Field_check : {
      name : string;
      field : ('a, 'r) Wire.Codec.field;
      proj : 'r -> 'a;
      equal : 'a -> 'a -> bool;
    }
      -> 'r field_check

and env_strategy = {
  positive : Wire.Param.env -> Wire.Param.env;
      (* Binder for the positive stream: binds every [Param.input] (by name) on
         a fresh env built for the codec under test. Taking the env as an
         argument (rather than building it from a fixed codec) lets a leaf's
         param survive composition: when the leaf is nested in a bigger codec,
         the param is rebound on that codec's own env. *)
  fuzz : (Wire.Param.env -> Wire.Param.env) Alcobar.gen;
      (* Binder gen for the random and adversarial streams. *)
}

let bytes_of_string s = Bytes.unsafe_of_string s
let string_of_bytes b = Bytes.unsafe_to_string b

let corpus_generation_mode () =
  Array.exists (String.equal "--gen-corpus") Sys.argv

let file_input_mode () =
  let argv = Sys.argv in
  let n = Array.length argv in
  n > 1
  && (not (corpus_generation_mode ()))
  &&
  let path = argv.(n - 1) in
  Sys.file_exists path
  && try not (Sys.is_directory path) with Sys_error _ -> false

let afl_mode () = corpus_generation_mode () || file_input_mode ()

(* Alcobar's AFL runner turns a failed property into a bare exception and
   prints nothing, so replaying a crashing input in CI would not say what
   diverged. On a file input, print the failure before raising it. *)
let file_input = lazy (file_input_mode ())
let reported = ref false

module Alcobar = struct
  include Alcobar

  let fail msg =
    if Lazy.force file_input then begin
      reported := true;
      Fmt.epr "FAIL %s@." msg
    end;
    Alcobar.fail msg

  let failf fmt = Fmt.kstr fail fmt
end

let failure_reported () = !reported
let failf = Alcobar.failf

let slice_of_string s =
  let b = Bytes.of_string s in
  Bytesrw.Bytes.Slice.make_or_eod b ~first:0 ~length:(Bytes.length b)

let string_of_slice s =
  Bytes.sub_string
    (Bytesrw.Bytes.Slice.bytes s)
    (Bytesrw.Bytes.Slice.first s)
    (Bytesrw.Bytes.Slice.length s)

let slice_equal a b = String.equal (string_of_slice a) (string_of_slice b)

let bytes_of_octets xs =
  let b = Bytes.create (List.length xs) in
  List.iteri (fun i x -> Bytes.set_uint8 b i (x land 0xFF)) xs;
  b

let pp_hex ppf s = String.iter (fun c -> Fmt.pf ppf "%02x" (Char.code c)) s

let bytes_fixed n =
  Alcobar.map Alcobar.[ Alcobar.bytes_fixed n ] bytes_of_string

let bytes_any = Alcobar.map Alcobar.[ Alcobar.bytes ] bytes_of_string

(* Every leaf gen wraps its typ in this one-field record, so the name and the
   single field are defined once: [addressed] rebuilds the same pair to reach
   the slot through [Codec.get] / [Codec.set]. *)
let leaf_codec_name = "_leaf"
let leaf_slot_name = "v"
let leaf_field typ = Wire.Codec.(Wire.Field.v leaf_slot_name typ $ fun v -> v)

let codec_of_typ typ =
  Wire.Codec.v leaf_codec_name (fun v -> v) Wire.Codec.[ leaf_field typ ]

(* Checkers for a hand-built record that projects with [fst] / [snd] (or the
   three-way equivalent). These gens seal their own codec instead of going
   through [Codec.v], so without a field list written down here their slots are
   invisible to the per-field properties and the record is only ever addressed
   as a whole -- which loses exactly the shapes worth addressing one field at a
   time: bitfields sharing a base word, and a span whose size a preceding field
   supplies. The names are for failure messages; the accessors resolve the slot
   from the Wire field. *)
let slot name f proj =
  Field_check { name; field = Wire.Codec.( $ ) f proj; proj; equal = ( = ) }

let pair_fields (name_a, f_a) (name_b, f_b) =
  [ slot name_a f_a fst; slot name_b f_b snd ]

let triple_fields (name_a, f_a) (name_b, f_b) (name_c, f_c) =
  let proj_a (a, _, _) = a in
  let proj_b (_, b, _) = b in
  let proj_c (_, _, c) = c in
  [ slot name_a f_a proj_a; slot name_b f_b proj_b; slot name_c f_c proj_c ]

(* The canonical bytes for [v]: a buffer sized by [size_of_value] then
   filled by [encode]. *)
let encode_via_codec codec v =
  let sz = Wire.Codec.size_of_value codec v in
  let buf = Bytes.create sz in
  Wire.Codec.encode codec v buf 0;
  buf

(* Hostile-value streams. [check_encode_safety] requires encode to refuse each
   of these outright or to write bytes that decode back unchanged, so each
   stream must yield values that violate a refinement decode enforces. *)

(* A value a closed enum does not list. Steps up from the drawn number until it
   lands outside the case set, so the stream is hostile by construction rather
   than by luck. *)
let unlisted_enum_gen ~mask valid =
  let rec step n tries =
    if tries > mask || not (List.mem n valid) then n
    else step ((n + 1) land mask) (tries + 1)
  in
  Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> step (n land mask) 0)

(* A value with bits set above [width], for a [bits ~width] slice, the one
   unsigned shape still carried in a bare [int]. Only the low [width] bits reach
   the wire, and the masked remainder is itself a legal slice value, so encode
   has to refuse rather than truncate. *)
let above_bit_width width =
  let mask = (1 lsl width) - 1 in
  Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land mask lor (mask + 1))

(* The [uint size] counterpart: a value needing more than [size] bytes. *)
let above_byte_width size =
  let max_v = (1 lsl (size * 8)) - 1 in
  Alcobar.map
    Alcobar.[ Alcobar.int ]
    (fun n -> Wire.Private.UInt63.of_int (n land max_v lor (max_v + 1)))

(* A byte string of one of the lengths around [n], for a byte span whose
   declared size is [n]: only the exact length may encode. *)
let byte_lengths_around n =
  Alcobar.choose
    (List.map
       (fun k -> Alcobar.bytes_fixed k)
       (List.sort_uniq Int.compare [ 0; max 0 (n - 1); n; n + 1; n * 2 ]))

(* Build a leaf [t] from a typ and three Alcobar generators. *)
let leaf ~equal ~typ ~value_gen ~random ~adversarial =
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode_via_codec codec v))
  in
  let adversarial_bytes =
    Alcobar.map Alcobar.[ adversarial ] (fun v -> encode_via_codec codec v)
  in
  {
    codec;
    typ;
    positive;
    random;
    adversarial = adversarial_bytes;
    equal;
    env = None;
    fields = [];
    adversarial_value = None;
  }

(* A leaf whose positives draw a value from [value_gen] and encode it through
   the codec, with caller-chosen raw [random] / [adversarial] byte streams (so
   the adversarial stream can carry invalid tags the encoder would never
   produce). *)
let enum_like ~typ ~value_gen ~random ~adversarial =
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode_via_codec codec v))
  in
  {
    codec;
    typ;
    positive;
    random;
    adversarial;
    equal = ( = );
    env = None;
    fields = [];
    adversarial_value = None;
  }

(* A gen prints as the bit diagram of the codec it generates for. *)
let pp ppf g = Wire.Ascii.pp_codec ppf g.codec
