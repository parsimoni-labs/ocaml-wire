(** Differential AFL fuzzer: OCaml wire codec vs EverParse C validator.

    The codecs come from {!Diff_codecs.included}, the in-scope part of the
    Boltzmann {!Fuzz_gen.sample} -- not from {!Fuzz_gen.registry}, which the
    OCaml-only suites drive off. For each codec EverParse could generate a
    validator for ({!Diff_index.covered}, looked up in {!Diff_codecs.included}
    by label), feed the AFL input to both the OCaml codec and the generated C
    validator and flag any divergence.

    There are two of those. Both sides must agree on which inputs are valid, and
    on an input they both accept they must agree on what they read out of it:
    every field {!Diff_codecs.compared_fields} says the two can both hand back
    is compared value against value, the OCaml side through
    {!Wire.Codec.field_readers} and the C side through {!Diff_index.values}. An
    accept/reject check on its own is blind to the more dangerous case, two
    decoders that accept the same bytes and read a different number out of them.

    Codecs the candidate filter dropped (variable-size, zero-size, parameterised
    or casetype) are reported via {!Diff_codecs.excluded}. *)

open Fuzz_gen

let accepts f =
  match f () with () -> true | exception Wire.Parse_error _ -> false

(* The EverParse validator accepts iff the bytes parse (enough input, valid
   structure) and every refinement holds. [Codec.decode_exn] is exactly that:
   it decodes (length + structure) and validates (refinements), raising
   [Parse_error] on a clean rejection. (Plain [Codec.decode]
   returns a [result], so its rejection is easy to drop by mistake.) Any other
   exception surfaces as a crash, which is itself a divergence to report.

   [Codec.validate] is the gate a caller actually runs on untrusted input and
   the closer counterpart of a C validator, since it checks without building a
   record. Run it alongside, so a refinement that reaches one of the two OCaml
   paths but not the other is caught here rather than papered over by whichever
   one the C side happens to agree with. *)
let ocaml_accepts label (Pack g) b =
  let c = codec g in
  let decoded = accepts (fun () -> ignore (Wire.Codec.decode_exn c b 0)) in
  let validated = accepts (fun () -> Wire.Codec.validate c b 0) in
  if decoded <> validated then
    Alcobar.failf "%s: OCaml decode accepts=%b but Codec.validate accepts=%b"
      label decoded validated;
  decoded

type field_check = {
  fname : string;
  width : int;  (** Bytes the field occupies on the wire. *)
  read : bytes -> int -> int;  (** The OCaml decoder's reader for it. *)
}
(** One field both sides report, with what the OCaml side needs to read it. *)

(* Both sides report a field as a host integer, so the only thing left to
   reconcile is the width: an [int8] the OCaml decoder returns as [-1] and the C
   plug reports as [255] carry the same wire byte. Compare the field's own bytes
   and nothing above them. *)
let to_slot width v =
  if width >= 8 then v
  else Int64.logand v (Int64.pred (Int64.shift_left 1L (8 * width)))

let diff_field label b c_fields f =
  match List.assoc_opt f.fname c_fields with
  | None ->
      Alcobar.failf "%s: EverParse C reported no value for field %s" label
        f.fname
  | Some raw -> (
      let c = to_slot f.width raw in
      match f.read b 0 with
      | v ->
          let o = to_slot f.width (Int64.of_int v) in
          if not (Int64.equal o c) then
            Alcobar.failf
              "%s: field %s: OCaml decoded 0x%Lx but EverParse C decoded 0x%Lx"
              label f.fname o c
      | exception Wire.Parse_error _ ->
          (* [Codec.field_readers] reports through an OCaml [int], one bit short
             of an 8-byte field, so past 2^62 it has no [int] to report and
             raises instead. All the comparison can pin there is that the C side
             does not fit an [int] either, which still holds the field's top two
             bits to what the OCaml decoder made of them. *)
          if Int64.unsigned_to_int c <> None then
            Alcobar.failf
              "%s: field %s: OCaml decoded a value too large for an int but \
               EverParse C decoded 0x%Lx"
              label f.fname c)

let diff_values label fields c_values b =
  if fields <> [] then begin
    let c_fields = c_values b in
    if List.compare_lengths c_fields fields <> 0 then
      Alcobar.failf "%s: EverParse C reported %d field values, expected %d"
        label (List.length c_fields) (List.length fields);
    List.iter (diff_field label b c_fields) fields
  end

type case = {
  label : string;
  packed : packed;
  c_check : bytes -> bool;
  c_values : bytes -> (string * int64) list;
  fields : field_check list;
}
(** One covered codec with both its decoders: the OCaml side ([packed], from
    {!Diff_codecs.included}) and the generated C validator ([c_check] and
    [c_values], from {!Diff_index}). *)

(* Compare both decoders on one codec: first on the verdict, then, when both
   accepted, on every field value they both report. *)
let diff_check case b =
  let o = ocaml_accepts case.label case.packed b and c = case.c_check b in
  if o <> c then
    Alcobar.failf "%s: OCaml accepts=%b but EverParse C accepts=%b" case.label o
      c;
  if o then diff_values case.label case.fields case.c_values b

(* The OCaml half of the value comparison. [Codec.field_readers] keys the
   decoder's own per-field readers by name, which is what makes this reachable
   at all: the record type is hidden behind [Pack], so there is no field handle
   to hand [Codec.get]. A compared field with no reader would mean the two sides
   disagree about what the schema's fields are, so it fails the run rather than
   quietly comparing one field fewer. *)
let field_checks label (Pack g as p) =
  let readers = Wire.Codec.field_readers (codec g) in
  List.map
    (fun (cf : Diff_codecs.compared_field) ->
      match List.assoc_opt cf.name readers with
      | Some read -> { fname = cf.name; width = cf.width; read }
      | None ->
          Fmt.failwith "diff: %s has no OCaml field reader for %s" label cf.name)
    (Diff_codecs.compared_fields p)

let covered_cases =
  let values = Array.to_list Diff_index.values in
  Array.to_list Diff_index.covered
  |> List.map (fun (label, c_check) ->
      let packed = List.assoc label Diff_codecs.included in
      ( label,
        {
          label;
          packed;
          c_check;
          c_values = List.assoc label values;
          fields = field_checks label packed;
        } ))

(* Normal [dune test]: one case per covered codec, each on its own random bytes,
   so a single run touches every codec. *)
let sample_case (label, case) =
  Alcobar.test_case label [ Alcobar.bytes ] (fun buf ->
      diff_check case (Bytes.of_string buf))

(* AFL file-input mode: the shared framed input picks one codec per input and
   strips the selector/mode header before the bytes reach the decoders. *)
let afl_case ?(max_len = 256) () =
  afl_contract_cases ~max_len "diff" covered_cases
    ~check:(fun _label case input -> diff_check case input.payload)

let cases =
  if file_input_mode () then afl_case () else List.map sample_case covered_cases

let () =
  List.iter
    (fun (label, _, reason) ->
      Fmt.epr "diff: skipping %s (%s)@." label
        (Diff_codecs.string_of_exclusion reason))
    Diff_codecs.excluded;
  Alcobar.run "diff" [ ("diff", cases) ]
