open Gen

(* {1 Test driver} *)

(* Wrap each of a gen's three streams in an Alcobar test_case. Positive
   asserts round-trip; the other two assert crash-safety. *)
let positive_env g =
  match g.env with
  | Some s -> Some (s.positive (Wire.Codec.env g.codec))
  | None -> None

(* Typ-level vs codec-level agreement. [Wire.of_string] (typ level) and
   [Codec.decode] (codec level) are two independent reader constructions over
   the same schema, so they can drift apart: a refinement enforced by one and
   dropped by the other accepts input the other -- and the EverParse C built
   from the same schema -- rejects. The two may legitimately report different
   errors (offsets and kinds are not part of the contract), so only the
   accept/reject verdict is compared, plus the decoded value where both
   accept. *)

let direct_decodes g bs =
  match Wire.of_string g.typ (string_of_bytes bs) with
  | Ok v -> `Accept v
  | Error _ -> `Reject
  | exception Invalid_argument _ -> `Crash

let codec_decodes g bs =
  match Wire.Codec.decode g.codec bs 0 with
  | Ok v -> `Accept v
  | Error _ -> `Reject
  | exception Invalid_argument _ -> `Crash

(* Decode the same bytes both ways and require the same verdict. Codecs that
   bind a [Param.env] are skipped: the typ-level entry points cannot thread
   one. *)
let check_direct_codec_agree label kind g bs =
  if Option.is_none g.env then
    match (direct_decodes g bs, codec_decodes g bs) with
    | `Crash, _ -> Alcobar.failf "%s %s of_string crashed" label kind
    | _, `Crash -> Alcobar.failf "%s %s Codec.decode crashed" label kind
    | `Reject, `Reject -> ()
    | `Accept direct, `Accept codec ->
        if not (g.equal direct codec) then
          Alcobar.failf "%s %s of_string and Codec.decode decoded differently"
            label kind
    | `Accept _, `Reject ->
        Alcobar.failf "%s %s of_string accepted but Codec.decode rejected" label
          kind
    | `Reject, `Accept _ ->
        Alcobar.failf "%s %s Codec.decode accepted but of_string rejected" label
          kind

(* The encode-side twin. [Wire.to_bytes] (typ level) and [Codec.encode] (codec
   level) are, like the two decoders, separate constructions over one schema:
   they must refuse the same values and, when they accept, write the same bytes.
   A value one path writes and the other refuses is a frame the library produces
   through one entry point and rejects through the other. *)

let direct_encodes g value =
  match Wire.to_bytes g.typ value with
  | bs -> `Wrote (string_of_bytes bs)
  | exception Invalid_argument _ -> `Refused

let codec_encodes g value =
  match Wire.Codec.size_of_value g.codec value with
  | exception Invalid_argument _ -> `Refused
  | sz -> (
      let buf = Bytes.create sz in
      match Wire.Codec.encode g.codec value buf 0 with
      | () -> `Wrote (string_of_bytes buf)
      | exception Invalid_argument _ -> `Refused)

let check_direct_codec_encode_agree label kind g value =
  if Option.is_none g.env then
    match (direct_encodes g value, codec_encodes g value) with
    | `Refused, `Refused -> ()
    | `Wrote direct, `Wrote codec ->
        if not (String.equal direct codec) then
          Alcobar.failf "%s %s to_bytes and Codec.encode wrote different bytes"
            label kind
    | `Wrote _, `Refused ->
        Alcobar.failf "%s %s to_bytes wrote a value Codec.encode refuses" label
          kind
    | `Refused, `Wrote _ ->
        Alcobar.failf "%s %s Codec.encode wrote a value to_bytes refuses" label
          kind

(* Round-trip one positive sample: decode the canonical bytes back to the
   value, then re-encode into a buffer sized from [size_of_value] and require
   the two to agree. Sizing from [size_of_value] (rather than from the
   canonical bytes) drives the value-driven size for every combinator,
   including ones whose [positive] bytes are hand-assembled (casetype,
   nested_at_most) and so never otherwise exercise [encode] / [size_of_value].
   A wrong [size_of_value] then shows up here as a mismatch -- or, if it
   under-counts, as an [encode] overrun -- rather than silently. *)
let check_positive_decode label g value bs env =
  (match Wire.Codec.decode ?env g.codec bs 0 with
  | Ok decoded ->
      if not (g.equal decoded value) then
        Alcobar.failf "%s positive decode mismatch" label
  | Error error ->
      Alcobar.failf "%s positive decode failed: %a" label Wire.pp_parse_error
        error);
  match Wire.Codec.decode_exn ?env g.codec bs 0 with
  | decoded ->
      if not (g.equal decoded value) then
        Alcobar.failf "%s positive decode_exn mismatch" label
  | exception Wire.Parse_error e ->
      Alcobar.failf "%s positive decode_exn failed: %a" label
        Wire.pp_parse_error e

(* The names of the input params a codec reads out of an env, [[]] for one that
   reads none. An [~action] codec writes output params and needs an env without
   reading one, so "has an env strategy" is not the same question. *)
let input_param_names g =
  Wire.Everparse.Raw.input_param_names
    (Wire.Everparse.Raw.struct_of_codec g.codec)

let check_positive_size label g value bs env =
  let sz = Wire.Codec.size_of_value g.codec value in
  if sz <> Bytes.length bs then
    Alcobar.failf "%s size_of_value = %d but canonical encoding is %d" label sz
      (Bytes.length bs);
  (* [size_of_value] measures the value, not the buffer, so it answers without
     an env; threading one must not change the answer. *)
  match env with
  | None -> sz
  | Some env ->
      let with_env = Wire.Codec.size_of_value ~env g.codec value in
      if with_env <> sz then
        Alcobar.failf "%s size_of_value = %d with an env bound, %d without"
          label with_env sz;
      sz

(* [wire_size_at] measures the buffer, so a parametric field's width comes from
   the env: with one bound it must span exactly the canonical encoding, the same
   contract the env-free codecs are held to. The env branch used to be skipped
   whole, which is how a param-sized field measured as empty stayed invisible. *)
let check_positive_size_metadata label g bs env =
  let len = Bytes.length bs in
  let min_sz = Wire.Codec.min_wire_size g.codec in
  if min_sz > len then
    Alcobar.failf "%s min_wire_size = %d but canonical encoding is %d" label
      min_sz len;
  (match Wire.Codec.wire_size_at ?env g.codec bs 0 with
  | actual ->
      if actual <> len then
        Alcobar.failf "%s wire_size_at = %d but canonical encoding is %d" label
          actual len
  | exception Invalid_argument m ->
      Alcobar.failf "%s wire_size_at raised on a positive: %s" label m);
  match Wire.Codec.wire_size_opt g.codec with
  | Some fixed when fixed <> len ->
      Alcobar.failf "%s wire_size = %d but canonical encoding is %d" label fixed
        len
  | Some _ | None -> ()

(* The other half of the parametric-size contract: with no env, a codec that
   reads an input param must refuse outright. Resolving an unbound param to 0
   would measure a param-sized field as empty and answer a short size instead of
   failing, which reads as a valid short record rather than as an error. *)
let check_env_required label g bs =
  match input_param_names g with
  | [] -> ()
  | _ :: _ -> (
      (match Wire.Codec.wire_size_at g.codec bs 0 with
      | n ->
          Alcobar.failf "%s wire_size_at answered %d with no env bound" label n
      | exception Invalid_argument _ -> ());
      (match Wire.Codec.decode g.codec bs 0 with
      | _ -> Alcobar.failf "%s decode answered with no env bound" label
      | exception Invalid_argument _ -> ());
      match Wire.Codec.validate g.codec bs 0 with
      | () -> Alcobar.failf "%s validate passed with no env bound" label
      | exception Invalid_argument _ -> ()
      | exception Wire.Parse_error _ ->
          Alcobar.failf "%s validate reported a parse error with no env bound"
            label)

let check_positive_encode label g value bs env sz =
  let bs_str = string_of_bytes bs in
  let out = Bytes.create sz in
  (try Wire.Codec.encode ?env g.codec value out 0
   with Invalid_argument _ -> Alcobar.failf "%s positive encode raised" label);
  if string_of_bytes out <> bs_str then
    Alcobar.failf "%s positive reencode mismatch" label

let check_positive label g (value, bs) =
  let env = positive_env g in
  check_positive_decode label g value bs env;
  check_direct_codec_encode_agree label "positive" g value;
  let sz = check_positive_size label g value bs env in
  check_positive_size_metadata label g bs env;
  if Option.is_some env then check_env_required label g bs;
  check_positive_encode label g value bs env sz

let validate_one ?env g bs =
  try
    Wire.Codec.validate ?env g.codec bs 0;
    `Ok
  with
  | Invalid_argument _ -> `Crash
  | Wire.Parse_error e ->
      (* A parse error's offset is never negative. Most kinds point inside the
         input, but [Unexpected_eof] and [Missing_terminator] report where a
         read (or a field that does not fit) ran past the end, so their offset
         may exceed the buffer length. *)
      let len = Bytes.length bs in
      let bounded =
        e.at >= 0
        &&
        match e.kind with
        | Wire.Unexpected_eof _ | Wire.Missing_terminator -> true
        | _ -> e.at <= len
      in
      if not bounded then
        Alcobar.failf "parse error offset %d invalid for a %d-byte buffer" e.at
          len;
      (* Every field-path segment is a real (non-empty) field name. *)
      if List.exists (fun s -> String.length s = 0) e.field then
        Alcobar.failf "parse error field path has an empty segment";
      `Reject

let check_validate_positive label g bs =
  let env = positive_env g in
  match validate_one ?env g bs with
  | `Ok -> ()
  | `Reject -> Alcobar.failf "%s validate rejected a positive" label
  | `Crash -> Alcobar.failf "%s validate crashed on a positive" label

let check_validate_safety label kind ?env g bs =
  match validate_one ?env g bs with
  | `Ok | `Reject -> ()
  | `Crash -> Alcobar.failf "%s validate %s crashed" label kind

let check_decode_safety label kind ?env g bs =
  match Wire.Codec.decode ?env g.codec bs 0 with
  | Ok _ | Error _ -> ()
  | exception Invalid_argument _ ->
      Alcobar.failf "%s %s crashed decoder" label kind

(* The bytes a hostile value encodes to, or [None] when encode refused it, which
   is one of the two outcomes [check_encode_safety] allows. *)
let hostile_encoding label ?env g value =
  match Wire.Codec.size_of_value g.codec value with
  | exception Invalid_argument _ -> None
  | sz -> (
      match Bytes.create sz with
      | exception Invalid_argument _ ->
          Alcobar.failf "%s encode safety: size_of_value returned %d" label sz
      | buf -> (
          match Wire.Codec.encode ?env g.codec value buf 0 with
          | exception Invalid_argument _ -> None
          | () -> Some buf))

(* The encode-side twin of [check_decode_safety]. Decode is handed hostile bytes
   and must not crash; encode is handed a hostile value and must not put a frame
   on the wire that its own decoder throws out. Exactly two outcomes are
   allowed: encode refuses the value with [Invalid_argument], or the bytes it
   writes decode back to an equal value. Anything else means the OCaml encoder
   and the EverParse validator built from the same schema disagree on a frame
   this library produced itself. *)
let check_encode_safety label ?env g value =
  match hostile_encoding label ?env g value with
  | None -> ()
  | Some buf -> (
      match Wire.Codec.decode ?env g.codec buf 0 with
      | Ok back ->
          if not (g.equal back value) then
            Alcobar.failf
              "%s encode safety: encode wrote %a, which decodes to a different \
               value"
              label pp_hex (string_of_bytes buf)
      | Error e ->
          Alcobar.failf
            "%s encode safety: encode wrote bytes its own decoder rejects (%a)"
            label Wire.pp_parse_error e
      | exception Invalid_argument _ ->
          Alcobar.failf
            "%s encode safety: encode wrote bytes that crash the decoder" label)

(* One entry point's answer on one buffer: the value it built, the parse error
   it reported, or [Invalid_argument], which no entry point is allowed to raise
   ([check_decode_safety] / [check_validate_safety] own that). *)
type 'a verdict = Accepted of 'a | Rejected of Wire.parse_error | Crashed

let decode_at ?env g bs off =
  match Wire.Codec.decode ?env g.codec bs off with
  | Ok v -> Accepted v
  | Error e -> Rejected e
  | exception Invalid_argument _ -> Crashed

let validate_at ?env g bs off =
  match Wire.Codec.validate ?env g.codec bs off with
  | () -> Accepted ()
  | exception Wire.Parse_error e -> Rejected e
  | exception Invalid_argument _ -> Crashed

(* Two rejections of the same buffer, blamed on the same field, must give the
   same reason. Blamed on different fields they need not: a buffer can break
   several fields at once, and the two paths do not reach them in the same order
   (decode fills a record, so its field readers run in the compiler's argument
   order; validate walks the compiled field list in declaration order). Which of
   several genuine faults gets reported is not a property worth pinning; that a
   fault the other path found is a fault this one found too, is. *)
let same_rejection (a : Wire.parse_error) (b : Wire.parse_error) =
  List.compare String.compare a.Wire.field b.Wire.field <> 0
  || Wire.equal_error_kind a.Wire.kind b.Wire.kind

(* [Codec.validate] is the gate [codec.mli] tells a caller to run on untrusted
   input before reading fields zero-copy, so it must raise on exactly the bytes
   [Codec.decode] returns [Error] on, and for the same reason. Two entry points
   of one library that disagree about which bytes are valid are a parser
   differential inside it: what the caller validated is not what it goes on to
   read. Only the reason is compared, not the offset -- validate reaches a
   failing field through its own populate and may locate the same rejection at a
   different byte. *)
let check_decode_validate_agree label kind ?env g bs =
  match (decode_at ?env g bs 0, validate_at ?env g bs 0) with
  | Crashed, _ -> Alcobar.failf "%s %s crashed decoder" label kind
  | _, Crashed -> Alcobar.failf "%s %s crashed validator" label kind
  | Accepted _, Accepted () -> ()
  | Rejected d, Rejected v ->
      if not (same_rejection d v) then
        Alcobar.failf
          "%s %s: decode and validate rejected for different reasons (decode: \
           %a, validate: %a)"
          label kind Wire.pp_parse_error d Wire.pp_parse_error v
  | Accepted _, Rejected v ->
      Alcobar.failf "%s %s: decode accepted but validate rejected (%a)" label
        kind Wire.pp_parse_error v
  | Rejected d, Accepted () ->
      Alcobar.failf "%s %s: validate accepted what decode rejects (%a)" label
        kind Wire.pp_parse_error d

(* One sample of the hostile-value stream, or [None] for a shape with no
   refinement to violate. Kept as an [option] stream so every gen presents the
   same test-case arity whether or not it has hostile values. *)
let hostile_value_stream g =
  match g.adversarial_value with
  | None -> Alcobar.const None
  | Some gen -> Alcobar.map Alcobar.[ gen ] (fun v -> Some v)

(* {1 Per-field accessor properties}

   [Codec.get] and [Codec.set] are built by [build_staged_reader] and
   [build_staged_writer], which are separate constructions from the record
   populator [decode] runs and the record encoder [encode] runs. Addressing one
   field at a time is therefore a second, independent reading (and writing) of
   the same bytes rather than another call into the same code, and the two can
   drift: a width the encoder refuses and the staged writer truncates, or a
   write that spills into the next field, is invisible to any check phrased
   over the whole record. *)

(* The codec a per-field property addresses, paired with the fields it may
   name. A record built by [Codec.v] is addressed directly: it kept its own
   field list, so every checker names a slot that codec really declares. Any
   other gen is addressed through the one-field record [codec_of_typ] wraps its
   typ in -- the same wire format, since [typ] and [codec] are two spellings of
   one schema, and the only spelling whose single slot is guaranteed to carry
   the typ the checker was built from. Naming a slot of the gen's own codec
   instead would resolve the offset by name and then read it at whatever typ
   the checker happened to hold, which is not the same field. *)
module Schema_types = Wire.Private.Types

let rec typ_field_names : type a. a Schema_types.typ -> string list = function
  | Schema_types.Codec { codec_struct; _ } -> struct_field_names codec_struct
  | Schema_types.Struct st -> struct_field_names st
  | Schema_types.Casetype { cases; _ } ->
      List.concat_map
        (fun (Schema_types.Case_branch { cb_inner; _ }) ->
          typ_field_names cb_inner)
        cases
  | Schema_types.Array { elem; _ } -> typ_field_names elem
  | Schema_types.Repeat { elem; _ } -> typ_field_names elem
  | Schema_types.Single_elem { elem; _ } -> typ_field_names elem
  | Schema_types.Optional { inner; _ } -> typ_field_names inner
  | Schema_types.Optional_or { inner; _ } -> typ_field_names inner
  | Schema_types.Where { inner; _ } -> typ_field_names inner
  | Schema_types.Map { inner; _ } -> typ_field_names inner
  | Schema_types.Enum { base; _ } -> typ_field_names base
  | Schema_types.Apply { typ; _ } -> typ_field_names typ
  | _ -> []

and struct_field_names (st : Schema_types.struct_) =
  List.concat_map
    (fun (Schema_types.Field f) ->
      Option.to_list f.field_name @ typ_field_names f.field_typ)
    st.Schema_types.fields

type 'r addressed = {
  acodec : 'r Wire.Codec.t;
  achecks : 'r field_check list;
  anames : string list;
  ggeneral : 'r Wire.Codec.t;
      (* The gen's codec, and [acodec], with every fast path off. *)
  ageneral : 'r Wire.Codec.t;
}

(* [g.typ] is the whole schema for a [Codec.v] gen ([Wire.codec] of its own
   codec, so the walk reaches its field list); a leaf gen's codec is the
   one-slot record [codec_of_typ] wraps the typ in, whose slot [leaf_slot_name]
   names. Both spellings are admitted, so a leaf rejection may be attributed to
   either. *)
let addressed g =
  let anames = leaf_slot_name :: typ_field_names g.typ in
  let ggeneral = Wire.Private.Testing.general g.codec in
  match g.fields with
  | [] ->
      let field = leaf_field g.typ in
      let acodec = Wire.Codec.v leaf_codec_name Fun.id [ field ] in
      {
        acodec;
        achecks =
          [
            Field_check
              { name = leaf_slot_name; field; proj = Fun.id; equal = g.equal };
          ];
        anames;
        ggeneral;
        ageneral = Wire.Private.Testing.general acodec;
      }
  | fs ->
      { acodec = g.codec; achecks = fs; anames; ggeneral; ageneral = ggeneral }

(* Staging refuses on a field whose access shape has no staged accessor, which
   is a documented gap in the accessor API rather than a failure: skip. *)
let stage_get ?env a field =
  match Wire.Codec.get ?env a.acodec field with
  | staged -> Some (Wire.Staged.unstage staged)
  | exception Invalid_argument _ -> None

let stage_set a field =
  match Wire.Codec.set a.acodec field with
  | staged -> Some (Wire.Staged.unstage staged)
  | exception Invalid_argument _ -> None

(* [Codec.get] against the value the canonical bytes were encoded from: the
   staged reader and the encoder, over one buffer, with no shared code between
   them. *)
let check_field_get label ?env a value bs =
  List.iter
    (fun fc ->
      match fc with
      | Field_check f -> (
          match stage_get ?env a f.field with
          | None -> ()
          | Some get -> (
              match get bs 0 with
              | exception Invalid_argument m ->
                  Alcobar.failf
                    "%s field %s: Codec.get raised on a positive: %s" label
                    f.name m
              | exception Wire.Parse_error e ->
                  Alcobar.failf "%s field %s: Codec.get rejected a positive: %a"
                    label f.name Wire.pp_parse_error e
              | got ->
                  if not (f.equal got (f.proj value)) then
                    Alcobar.failf
                      "%s field %s: Codec.get reads back a value the encoder \
                       did not write"
                      label f.name)))
    a.achecks

(* [Codec.set] then [Codec.get]: the staged writer against the staged reader.
   [set] is documented to refuse a value the field cannot represent and to
   leave the buffer untouched when it does, so a refusal is one of the two
   accepted outcomes; what is not accepted is writing bytes that read back as a
   different value, which puts a number on the wire the caller never asked for
   and which no later [validate] can notice. [strict] says the value came from
   the positive stream, so the reader has to succeed as well as agree; a
   hostile value may legitimately be unreadable, since [set] is documented not
   to check enum membership and the reader does. *)
let check_field_set label ~strict a target bs =
  List.iter
    (fun (Field_check f) ->
      match (stage_get a f.field, stage_set a f.field) with
      | Some get, Some set -> (
          let buf = Bytes.copy bs in
          let want = f.proj target in
          match set buf 0 want with
          | exception Invalid_argument _ ->
              if not (Bytes.equal buf bs) then
                Alcobar.failf
                  "%s field %s: Codec.set refused a value after writing to the \
                   buffer"
                  label f.name
          | () -> (
              match get buf 0 with
              | exception Invalid_argument m ->
                  Alcobar.failf
                    "%s field %s: Codec.get raised on bytes Codec.set just \
                     wrote: %s"
                    label f.name m
              | exception Wire.Parse_error e ->
                  if strict then
                    Alcobar.failf
                      "%s field %s: Codec.get rejects the value Codec.set just \
                       wrote: %a"
                      label f.name Wire.pp_parse_error e
              | got ->
                  if not (f.equal got want) then
                    Alcobar.failf
                      "%s field %s: Codec.set wrote a value Codec.get reads \
                       back as a different one"
                      label f.name))
      | _ -> ())
    a.achecks

(* A value [Codec.set] will not put in a field is a value the record encoder
   must not put there either. The two width and length checks are written
   separately -- the staged writer's, and the per-field encoder's inside
   [Codec.encode] -- so one can gain a refusal the other does not have, and
   then the same frame is legal through one entry point and refused through the
   other. Fixed-size codecs only: with a runtime-sized field the value's own
   length changes the frame's size instead of overflowing a declared one, so
   the two are answering different questions. *)
let check_field_set_refusal label a record bs =
  if Option.is_some (Wire.Codec.wire_size_opt a.acodec) then
    List.iter
      (fun (Field_check f) ->
        match stage_set a f.field with
        | None -> ()
        | Some set -> (
            let buf = Bytes.copy bs in
            match set buf 0 (f.proj record) with
            | () -> ()
            | exception Invalid_argument _ -> (
                let out = Bytes.create (Bytes.length bs) in
                match Wire.Codec.encode a.acodec record out 0 with
                | exception Invalid_argument _ -> ()
                | () ->
                    Alcobar.failf
                      "%s field %s: Codec.set refuses this value but \
                       Codec.encode writes the same record"
                      label f.name)))
      a.achecks

(* Read a field now and hand back a predicate that re-reads it later. Lets a
   heterogeneous field list be compared before and after a write without the
   read values escaping their existential. *)
let field_snapshot ?env a buf fc =
  match fc with
  | Field_check f -> (
      match stage_get ?env a f.field with
      | None -> None
      | Some get -> (
          match get buf 0 with
          | (exception Invalid_argument _) | (exception Wire.Parse_error _) ->
              None
          | before ->
              Some
                ( f.name,
                  fun after_buf ->
                    match get after_buf 0 with
                    | (exception Invalid_argument _)
                    | (exception Wire.Parse_error _) ->
                        false
                    | after -> f.equal before after )))

(* Write locality, and its mirror, read independence: setting one field must
   not change what any other field reads, and reading a field must not depend
   on a sibling's value. Both directions are the same experiment. Restricted to
   fixed-size codecs, where every field has a static offset and size, because a
   variable layout makes the dependency real: setting a length field genuinely
   moves the span that follows it. *)
let check_field_locality label a target bs =
  let checks = a.achecks in
  if
    Option.is_some (Wire.Codec.wire_size_opt a.acodec)
    && List.compare_length_with checks 1 > 0
  then
    List.iter
      (fun (Field_check f as fc) ->
        match stage_set a f.field with
        | None -> ()
        | Some set -> (
            let buf = Bytes.copy bs in
            let others = List.filter (fun o -> o != fc) checks in
            let snaps = List.filter_map (field_snapshot a buf) others in
            match set buf 0 (f.proj target) with
            | exception Invalid_argument _ -> ()
            | () ->
                List.iter
                  (fun (other, same) ->
                    if not (same buf) then
                      Alcobar.failf
                        "%s: Codec.set on field %s changed what field %s reads"
                        label f.name other)
                  snaps))
      checks

let first_difference a b =
  let n = min (Bytes.length a) (Bytes.length b) in
  let rec go i =
    if i >= n then if Bytes.length a = Bytes.length b then -1 else n
    else if Bytes.get a i <> Bytes.get b i then i
    else go (i + 1)
  in
  go 0

(* Writing back exactly what the reader just returned must not move a byte. The
   positive stream's bytes are [encode]'s own output, so they are already the
   canonical spelling of every field and there is nothing for a re-write to
   normalise. This is the only check in the file that observes which bytes a
   write touched rather than what a later read makes of them, which is the
   channel a write that spills past its field shows up in. *)
let check_field_write_back label a bs =
  List.iter
    (fun (Field_check f) ->
      match (stage_get a f.field, stage_set a f.field) with
      | Some get, Some set -> (
          match get bs 0 with
          | (exception Invalid_argument _) | (exception Wire.Parse_error _) ->
              ()
          | v -> (
              let buf = Bytes.copy bs in
              match set buf 0 v with
              | exception Invalid_argument _ ->
                  if not (Bytes.equal buf bs) then
                    Alcobar.failf
                      "%s field %s: Codec.set refused a value after writing to \
                       the buffer"
                      label f.name
              | () ->
                  if not (Bytes.equal buf bs) then
                    Alcobar.failf
                      "%s field %s: writing back the value Codec.get just read \
                       changed the canonical encoding at byte %d"
                      label f.name (first_difference buf bs)))
      | _ -> ())
    a.achecks

(* Setting every field in turn into a zeroed buffer is an encode spelled with
   the staged writers, so it must land on the same bytes [encode] writes.
   Fixed-size codecs only: with a variable layout the fields have to be written
   in an order the offsets agree with, and a skipped field leaves a hole the
   comparison would blame on the wrong writer. Inconclusive, and so skipped,
   as soon as one field has no staged writer or refuses its own value. *)
let check_field_set_sweep label a value bs =
  let checks = a.achecks in
  if Option.is_some (Wire.Codec.wire_size_opt a.acodec) && checks <> [] then begin
    let buf = Bytes.make (Bytes.length bs) '\x00' in
    let wrote fc =
      match fc with
      | Field_check f -> (
          match stage_set a f.field with
          | None -> false
          | Some set -> (
              match set buf 0 (f.proj value) with
              | exception Invalid_argument _ -> false
              | () -> true))
    in
    if List.for_all wrote checks && not (Bytes.equal buf bs) then
      Alcobar.failf
        "%s: writing every field with Codec.set does not reproduce the bytes \
         Codec.encode writes (first difference at byte %d)"
        label (first_difference buf bs)
  end

(* All of the above on one positive sample. [other] is a second, independent
   positive: it supplies a value of each field's own type that differs from the
   one already in the buffer, which is what makes the write checks a write and
   not a no-op. [hostile] adds the values a field's own refinement rejects --
   an integer wider than its declared width, a value a closed enum does not
   list -- which is where [set] and [encode] are most likely to disagree. *)
let check_fields label a g (value, bs) other hostile =
  (* [get] threads the env the positive was encoded with; [set] takes none, so
     the write checks stay off a codec that binds one. *)
  check_field_get label ?env:(positive_env g) a value bs;
  if Option.is_none g.env then begin
    check_field_write_back label a bs;
    check_field_set_sweep label a value bs;
    check_field_set label ~strict:true a other bs;
    check_field_locality label a other bs;
    Option.iter
      (fun h ->
        check_field_set label ~strict:false a h bs;
        check_field_set_refusal label a h bs;
        check_field_locality label a h bs)
      hostile
  end

(* The accessors against the two things that are supposed to license them.

   [validate] is the gate [codec.mli] tells a caller to run before reaching for
   an accessor on untrusted input, so a buffer it accepts must be safe for every
   [get] the codec offers: the validator walks its own compiled layout and the
   staged readers compute theirs separately, and a frame one of them thinks is
   long enough and the other does not surfaces here as a read of whatever the
   caller's buffer holds past the end.

   [decode] builds the same record through a third construction again, the
   record populator. Where it accepts, every field it filled in must equal what
   the staged reader makes of the same bytes. On this stream the values come
   from the fuzzer's byte generators rather than from any encoder, so neither
   side is reading back something it wrote. *)
let check_field_reads label kind ?env a g bs =
  let validated =
    match validate_at ?env g bs 0 with Accepted () -> true | _ -> false
  in
  let decoded =
    match Wire.Codec.decode ?env g.codec bs 0 with
    | Ok record -> Some record
    | Error _ -> None
    | exception Invalid_argument _ -> None
  in
  if validated || Option.is_some decoded then
    List.iter
      (fun (Field_check f) ->
        match stage_get ?env a f.field with
        | None -> ()
        | Some get -> (
            match get bs 0 with
            | exception Invalid_argument m ->
                if validated then
                  Alcobar.failf
                    "%s %s field %s: Codec.get raised on a buffer validate \
                     accepted: %s"
                    label kind f.name m
            | exception Wire.Parse_error _ -> ()
            | got ->
                Option.iter
                  (fun record ->
                    if not (f.equal got (f.proj record)) then
                      Alcobar.failf
                        "%s %s field %s: Codec.get and Codec.decode read \
                         different values from the same bytes"
                        label kind f.name)
                  decoded))
      a.achecks

(* {1 Metamorphic properties}

   A transformation of the input the answer must not depend on. Reading a frame
   at offset [k] of a longer buffer is what every caller of this library
   actually does -- a frame inside a reassembly window, a payload behind a
   header the layer below already consumed -- and it is a different execution
   from reading at offset 0, where every [base + off] folds to [off]. The two
   runs share their source but not the arithmetic under test: a reader that
   resolves a position from the buffer's start rather than the frame's start
   agrees at base 0 and nowhere else. Prepending only; appending is a separate
   property ([check_frame_extent]) because a greedy tail is defined to run to
   the end of the buffer. *)

(* Bytes to prepend, and to append. Short, because every metamorphic check pays
   for them on every sample, and drawn rather than fixed so a read resolving a
   position from the buffer's start cannot agree with base 0 by coincidence. *)
let noise_gen =
  Alcobar.map
    Alcobar.[ Alcobar.bytes ]
    (fun s ->
      bytes_of_string (if String.length s > 8 then String.sub s 0 8 else s))

let size_at g bs off =
  match Wire.Codec.wire_size_at g.codec bs off with
  | n -> Some n
  | exception Invalid_argument _ -> None
  | exception Wire.Parse_error _ -> None

let pad_before pad bs =
  let k = Bytes.length pad in
  let out = Bytes.create (k + Bytes.length bs) in
  Bytes.blit pad 0 out 0 k;
  Bytes.blit bs 0 out k (Bytes.length bs);
  out

(* One verdict at base 0 against the same verdict at base [k], modulo the shift
   [k] adds to every offset the failure reports. A crash on both sides is not
   this property's business ([check_decode_safety] owns it); a crash on one
   side only is. *)
let check_shifted label what ~k ~same base0 basek =
  match (base0, basek) with
  | Crashed, Crashed -> ()
  | Crashed, _ ->
      Alcobar.failf "%s: %s crashed at base 0 but not at base %d" label what k
  | _, Crashed ->
      Alcobar.failf "%s: %s crashed at base %d but not at base 0" label what k
  | Accepted a, Accepted b ->
      if not (same a b) then
        Alcobar.failf "%s: %s at base %d read a different value than at base 0"
          label what k
  | Rejected a, Rejected b ->
      if not (Wire.equal_error_kind a.Wire.kind b.Wire.kind) then
        Alcobar.failf
          "%s: %s rejected for a different reason at base %d (%a at base 0, %a \
           at base %d)"
          label what k Wire.pp_parse_error a Wire.pp_parse_error b k
      else if b.Wire.at <> a.Wire.at + k then
        Alcobar.failf
          "%s: %s puts the failure at byte %d with the frame at base %d, where \
           base 0 puts it at byte %d"
          label what b.Wire.at k a.Wire.at
  | Accepted _, Rejected _ ->
      Alcobar.failf "%s: %s accepted at base 0 and rejected at base %d" label
        what k
  | Rejected _, Accepted _ ->
      Alcobar.failf "%s: %s rejected at base 0 and accepted at base %d" label
        what k

let check_offset_invariance label ?env a g pad bs =
  let k = Bytes.length pad in
  if k > 0 then begin
    let bs' = pad_before pad bs in
    check_shifted label "Codec.decode" ~k ~same:g.equal (decode_at ?env g bs 0)
      (decode_at ?env g bs' k);
    check_shifted label "Codec.validate" ~k
      ~same:(fun () () -> true)
      (validate_at ?env g bs 0) (validate_at ?env g bs' k);
    (* [wire_size_at] takes no env, so on a codec with an input param it would
       resolve the parametric size to 0 at both bases and compare two wrong
       answers. The question is not statable through that entry point. *)
    (if Option.is_none g.env then
       match (size_at g bs 0, size_at g bs' k) with
       | Some at0, Some atk when at0 <> atk ->
           Alcobar.failf
             "%s: Codec.wire_size_at is %d at base 0 and %d at base %d" label
             at0 atk k
       | Some _, None ->
           Alcobar.failf "%s: Codec.wire_size_at raised at base %d only" label k
       | None, Some _ ->
           Alcobar.failf "%s: Codec.wire_size_at raised at base 0 only" label
       | _ -> ());
    List.iter
      (fun fc ->
        match fc with
        | Field_check f -> (
            match stage_get ?env a f.field with
            | None -> ()
            | Some get ->
                let read buf off =
                  match get buf off with
                  | v -> Accepted v
                  | exception Wire.Parse_error e -> Rejected e
                  | exception Invalid_argument _ -> Crashed
                in
                check_shifted
                  (Fmt.str "%s field %s" label f.name)
                  "Codec.get" ~k ~same:f.equal (read bs 0) (read bs' k)))
      a.achecks
  end

(* [encode] at base [k] must write the frame [encode] at base 0 writes, and
   must leave every byte outside it alone: a caller encoding into a reused
   transmit buffer keeps what surrounds the frame. *)
let check_offset_encode label ?env g value bs pad =
  let k = Bytes.length pad in
  if k > 0 then
    match Wire.Codec.size_of_value g.codec value with
    | exception Invalid_argument _ -> ()
    | sz -> (
        let out = Bytes.make (k + sz + k) '\xa5' in
        match Wire.Codec.encode ?env g.codec value out k with
        | exception Invalid_argument m ->
            Alcobar.failf "%s: Codec.encode raised at base %d on a positive: %s"
              label k m
        | () ->
            if not (Bytes.equal (Bytes.sub out k sz) bs) then
              Alcobar.failf
                "%s: Codec.encode at base %d wrote different bytes than at \
                 base 0"
                label k;
            let untouched off len =
              let rec go i =
                i >= len || (Bytes.get out (off + i) = '\xa5' && go (i + 1))
              in
              go 0
            in
            if not (untouched 0 k) then
              Alcobar.failf "%s: Codec.encode at base %d wrote before the frame"
                label k;
            if not (untouched (k + sz) k) then
              Alcobar.failf "%s: Codec.encode at base %d wrote past the frame"
                label k)

(* {1 Framing}

   An accepted frame declares its own extent through [wire_size_at], and a
   reassembler advances by exactly that to find the next one. Two things have
   to hold for that loop to stay in step, and neither is checked anywhere but
   on a positive, where the extent is true by construction: the frame must not
   depend on bytes past what it declared, and bytes appended after it must not
   change it.

   The second has a real exception. [all_bytes], [all_zeros] and [rest_bytes]
   consume to the end of the buffer by design, so appending genuinely changes
   what they read. That is written here as a stronger clause rather than as a
   skip: a codec may let appended bytes change its answer only if its declared
   extent grew to cover them, which for a greedy tail means it consumed the
   whole buffer both before and after. A codec whose declared extent stays put
   while its value moves is reading bytes it does not claim. *)
(* Cutting the buffer to the size the frame declares must not change what it
   decodes to: the frame must not be reading bytes it did not claim. *)
let check_frame_fits_declared label ?env g v sz bs =
  match decode_at ?env g (Bytes.sub bs 0 sz) 0 with
  | Accepted v' ->
      if not (g.equal v v') then
        Alcobar.failf
          "%s: cutting the buffer to the frame's own wire_size_at (%d) changed \
           the decoded value"
          label sz
  | Rejected e ->
      Alcobar.failf
        "%s: the frame does not decode inside its own wire_size_at (%d): %a"
        label sz Wire.pp_parse_error e
  | Crashed ->
      Alcobar.failf
        "%s: cutting the buffer to the frame's own wire_size_at (%d) crashed \
         the decoder"
        label sz

(* The appended-bytes clause, with the greedy-tail carve-out written as its own
   test rather than as a skip: growing the declared size is allowed only when
   the codec consumed the whole buffer both before and after. *)
let check_frame_ignores_tail label ?env g v ~sz ~len tail bs =
  let n_tail = Bytes.length tail in
  let bs' = Bytes.cat bs tail in
  match size_at g bs' 0 with
  | None ->
      Alcobar.failf
        "%s: Codec.wire_size_at raised after %d bytes were appended to an \
         accepted frame"
        label n_tail
  | Some sz' when sz' = sz -> (
      match decode_at ?env g bs' 0 with
      | Accepted v' ->
          if not (g.equal v v') then
            Alcobar.failf
              "%s: appending %d bytes changed the value of a frame whose \
               declared size stayed %d"
              label n_tail sz
      | Rejected e ->
          Alcobar.failf
            "%s: appending %d bytes stopped a frame of declared size %d from \
             decoding: %a"
            label n_tail sz Wire.pp_parse_error e
      | Crashed ->
          Alcobar.failf "%s: appending %d bytes crashed the decoder" label
            n_tail)
  | Some sz' when sz' < sz ->
      Alcobar.failf
        "%s: appending %d bytes shrank the declared frame size from %d to %d"
        label n_tail sz sz'
  | Some sz' ->
      if sz <> len || sz' <> Bytes.length bs' then
        Alcobar.failf
          "%s: appending %d bytes grew the declared frame size from %d to %d \
           without consuming the buffer (%d then %d bytes available)"
          label n_tail sz sz' len (Bytes.length bs')

let check_frame_extent label kind ?env g tail bs =
  (* Same reason as in [check_offset_invariance]: with an input param bound,
     the size this codec declares is not reachable through [wire_size_at]. *)
  if Option.is_none g.env then
    match decode_at ?env g bs 0 with
    | Crashed | Rejected _ -> ()
    | Accepted v -> (
        let label = Fmt.str "%s %s" label kind in
        let len = Bytes.length bs in
        match size_at g bs 0 with
        | None ->
            Alcobar.failf
              "%s: Codec.wire_size_at raised on a frame decode accepted" label
        | Some sz ->
            let min_sz = Wire.Codec.min_wire_size g.codec in
            if sz < min_sz || sz > len then
              Alcobar.failf
                "%s: Codec.wire_size_at = %d on an accepted %d-byte frame, \
                 outside [%d, %d]"
                label sz len min_sz len;
            check_frame_fits_declared label ?env g v sz bs;
            if Bytes.length tail > 0 then
              check_frame_ignores_tail label ?env g v ~sz ~len tail bs)

(* Nothing shorter than [min_wire_size] may decode. A reassembler holds off
   until it has that many bytes, so a codec that accepts fewer has a minimum
   that does not mean what its caller reads it as. *)
let check_short_prefix label kind ?env g bs =
  let min_sz = Wire.Codec.min_wire_size g.codec in
  if min_sz > 0 && Bytes.length bs >= min_sz then
    match decode_at ?env g (Bytes.sub bs 0 (min_sz - 1)) 0 with
    | Rejected _ -> ()
    | Crashed ->
        Alcobar.failf
          "%s %s: decode crashed on %d bytes, below min_wire_size %d" label kind
          (min_sz - 1) min_sz
    | Accepted _ ->
        Alcobar.failf
          "%s %s: decode accepted %d bytes where min_wire_size is %d" label kind
          (min_sz - 1) min_sz

(* {1 Idempotence and non-interference} *)

(* Every read path leaves the buffer byte-identical. Callers of the bytes-based
   APIs retain that buffer, so a write anywhere on a read path corrupts their
   input. *)
let check_read_purity label kind ?env a g bs =
  let before = Bytes.copy bs in
  ignore (decode_at ?env g bs 0);
  ignore (validate_at ?env g bs 0);
  ignore (size_at g bs 0);
  List.iter (fun fc -> ignore (field_snapshot ?env a bs fc)) a.achecks;
  if not (Bytes.equal bs before) then
    Alcobar.failf "%s %s: a read path wrote to the buffer at byte %d" label kind
      (first_difference bs before)

let check_repeated label what ~same first second =
  match (first, second) with
  | Crashed, Crashed -> ()
  | Accepted a, Accepted b ->
      if not (same a b) then
        Alcobar.failf
          "%s: %s answered differently once another frame had been read" label
          what
  | Rejected a, Rejected b ->
      if
        (not (Wire.equal_error_kind a.Wire.kind b.Wire.kind))
        || a.Wire.at <> b.Wire.at
      then
        Alcobar.failf
          "%s: %s reported a different failure once another frame had been \
           read (%a, then %a)"
          label what Wire.pp_parse_error a Wire.pp_parse_error b
  | _ ->
      Alcobar.failf "%s: %s changed verdict once another frame had been read"
        label what

(* Reading one frame must not change the answer to a question about another.
   [decode], [validate] and the staged readers of a field carrying an action
   all share one domain-local scratch array, each clearing on entry as much of
   it as it believes it needs; a slot one of them leaves set and another reads
   before setting is one frame's field value answering for a different frame,
   which in this library is disclosure rather than a nit. The two things
   compared are a query answered with nothing else in flight, and the same
   query answered after a second frame has been through every read path the
   codec offers. *)
let check_frame_isolation label kind ?env a g bs interloper =
  let snaps = List.filter_map (field_snapshot ?env a bs) a.achecks in
  let alone_decode = decode_at ?env g bs 0 in
  let alone_validate = validate_at ?env g bs 0 in
  let alone_size = size_at g bs 0 in
  (* Push the second frame through every read path, the staged readers already
     in hand included, so it is one reader that sees both buffers. *)
  ignore (decode_at ?env g interloper 0);
  ignore (validate_at ?env g interloper 0);
  ignore (size_at g interloper 0);
  List.iter (fun (_, same) -> ignore (same interloper)) snaps;
  List.iter (fun fc -> ignore (field_snapshot ?env a interloper fc)) a.achecks;
  let label = Fmt.str "%s %s" label kind in
  check_repeated label "Codec.decode" ~same:g.equal alone_decode
    (decode_at ?env g bs 0);
  check_repeated label "Codec.validate"
    ~same:(fun () () -> true)
    alone_validate (validate_at ?env g bs 0);
  if alone_size <> size_at g bs 0 then
    Alcobar.failf
      "%s: Codec.wire_size_at answered differently once another frame had been \
       read"
      label;
  List.iter
    (fun (name, same) ->
      if not (same bs) then
        Alcobar.failf
          "%s: Codec.get on field %s reads a different value once the same \
           reader has been used on another frame"
          label name)
    snaps

(* {1 Fast paths against the general path}

   [Codec] compiles a codec on first use and picks fast paths from its shape:
   a plain decode and validate over a fixed frame, interval tests and byte
   tables, a layout pass over byte spans, a staged encode, [Codec.valid] and
   [Codec.read]. Each must answer exactly what the general path answers: the
   same value, the same error at the same offset for the same field, the same
   bytes, the same [Invalid_argument]. {!Wire.Private.Testing.general} is the
   same codec with every fast path off, the oracle for a performance rewrite.

   The first call on a codec compiles it and runs a different entry point
   from the later calls, so a codec not yet compiled
   ({!Wire.Private.Testing.fresh}) must answer like a warm one too. *)

module Testing = Wire.Private.Testing

(* One entry point's whole answer. Any other exception is a crash, which the
   property reports as it is. *)
type 'a answer = Value of 'a | Parse of Wire.parse_error | Invalid of string

let answer f =
  match f () with
  | v -> Value v
  | exception Wire.Parse_error e -> Parse e
  | exception Invalid_argument m -> Invalid m

let map_answer f = function
  | Value v -> Value (f v)
  | (Parse _ | Invalid _) as a -> a

let same_answer equal a b =
  match (a, b) with
  | Value x, Value y -> equal x y
  | Parse x, Parse y ->
      Wire.equal_error_kind x.Wire.kind y.Wire.kind
      && x.Wire.at = y.Wire.at
      && List.equal String.equal x.Wire.field y.Wire.field
  | Invalid x, Invalid y -> String.equal x y
  | (Value _ | Parse _ | Invalid _), _ -> false

let pp_answer pp_value ppf = function
  | Value v -> pp_value ppf v
  | Parse e ->
      Fmt.pf ppf "Parse_error (%a, field [%s])" Wire.pp_parse_error e
        (String.concat "." e.Wire.field)
  | Invalid m -> Fmt.pf ppf "Invalid_argument %S" m

let pp_some ppf _ = Fmt.string ppf "a value"

let check_answer label what ?(pp = pp_some) ~equal (na, a) (nb, b) =
  if not (same_answer equal a b) then
    Alcobar.failf "%s: %s: %s gives %a but %s gives %a" label what na
      (pp_answer pp) a nb (pp_answer pp) b

(* A codec compiled before the call under test, so the call takes the path a
   warm codec takes. *)
let warm c = ignore (Wire.Codec.min_wire_size c)

let decode_answer ?env c bs =
  answer (fun () -> Wire.Codec.decode_exn ?env c bs 0)

let validate_answer ?env c bs =
  answer (fun () -> Wire.Codec.validate ?env c bs 0)

let valid_answer ?env c bs = answer (fun () -> Wire.Codec.valid ?env c bs 0)
let unit_equal () () = true

let check_core label ?env ~equal ~names:((nf, fast), (ng, general)) bs =
  check_answer label "Codec.decode" ~equal
    (nf, decode_answer ?env fast bs)
    (ng, decode_answer ?env general bs);
  check_answer label "Codec.validate" ~equal:unit_equal
    (nf, validate_answer ?env fast bs)
    (ng, validate_answer ?env general bs)

(* [decode] and [validate] of a warm codec against the general path. Cheap,
   and for a composition drawn per sample the general codec is drawn with it. *)
let check_fast_general_core label kind ?env g general bs =
  warm g.codec;
  check_core
    (Fmt.str "%s %s" label kind)
    ?env ~equal:g.equal
    ~names:(("the fast path", g.codec), ("the general path", general))
    bs

let field_get ?env c f bs =
  answer (fun () -> Wire.Staged.unstage (Wire.Codec.get ?env c f) bs 0)

(* [valid] then [read] of each field, against [validate] then [get] of the
   general codec, and [get] of each field on both. *)
let check_accessors label ?env ~fast ~general checks bs =
  let fast_valid = valid_answer ?env fast bs in
  check_answer label "Codec.valid" ~equal:unit_equal
    ("the fast path", map_answer ignore fast_valid)
    ("the general path", map_answer ignore (valid_answer ?env general bs));
  List.iter
    (fun (Field_check f) ->
      let reference = field_get ?env general f.field bs in
      check_answer label
        ("Codec.get of field " ^ f.name)
        ~equal:f.equal
        ("the fast path", field_get ?env fast f.field bs)
        ("the general path", reference);
      match fast_valid with
      | Value v ->
          check_answer label
            ("Codec.read of field " ^ f.name)
            ~equal:f.equal
            ( "Codec.read",
              answer (fun () ->
                  Wire.Staged.unstage (Wire.Codec.read ?env fast f.field) v) )
            ("Codec.get on the general path", reference)
      | Parse _ | Invalid _ -> ())
    checks

let check_fast_general label kind ?env a g bs =
  check_fast_general_core label kind ?env g a.ggeneral bs;
  warm a.acodec;
  check_accessors
    (Fmt.str "%s %s" label kind)
    ?env ~fast:a.acodec ~general:a.ageneral a.achecks bs

(* Every entry point once on a codec not yet compiled, against the same entry
   point on the warm one. *)
let check_cold_warm label kind ?env a g bs =
  let label = Fmt.str "%s %s" label kind in
  warm g.codec;
  warm a.acodec;
  let names = ("the first call", "a later call") in
  let cold_vs_warm what ~equal f =
    check_answer label what ~equal
      (fst names, f (Testing.fresh g.codec))
      (snd names, f g.codec)
  in
  cold_vs_warm "Codec.decode" ~equal:g.equal (fun c -> decode_answer ?env c bs);
  cold_vs_warm "Codec.validate" ~equal:unit_equal (fun c ->
      validate_answer ?env c bs);
  check_answer label "Codec.valid" ~equal:unit_equal
    ( fst names,
      map_answer ignore (valid_answer ?env (Testing.fresh a.acodec) bs) )
    (snd names, map_answer ignore (valid_answer ?env a.acodec bs));
  match a.achecks with
  | Field_check f :: _ ->
      check_answer label
        ("Codec.get of field " ^ f.name)
        ~equal:f.equal
        (fst names, field_get ?env (Testing.fresh a.acodec) f.field bs)
        (snd names, field_get ?env a.acodec f.field bs)
  | [] -> ()

(* A value whose getters answer the same on successive calls. A flapping one
   has no single encoding to compare byte for byte; [check_encode_safety]
   holds its encoding to one of the answers instead. *)
let pure_value g v =
  List.for_all (fun (Field_check f) -> f.equal (f.proj v) (f.proj v)) g.fields

let encode_answer ?env c v ~len ~off =
  let buf = Bytes.make len '\xa5' in
  map_answer
    (fun () -> Bytes.to_string buf)
    (answer (fun () -> Wire.Codec.encode ?env c v buf off))

(* [encode] into a buffer of exactly the value's size, one short of it, and
   one behind a prefix, on the fast, the general and a cold codec. *)
let check_encode_paths label kind ?env a g value =
  if pure_value g value then
    match Wire.Codec.size_of_value ?env g.codec value with
    | exception Invalid_argument _ -> ()
    | n ->
        warm g.codec;
        let label = Fmt.str "%s %s" label kind in
        List.iter
          (fun (len, off) ->
            if len >= 0 then begin
              let what = Fmt.str "Codec.encode (%d bytes at %d)" len off in
              let fast = encode_answer ?env g.codec value ~len ~off in
              check_answer label what ~pp:pp_hex ~equal:String.equal
                ("the fast path", fast)
                ( "the general path",
                  encode_answer ?env a.ggeneral value ~len ~off );
              check_answer label what ~pp:pp_hex ~equal:String.equal
                ("a later call", fast)
                ( "the first call",
                  encode_answer ?env (Testing.fresh g.codec) value ~len ~off )
            end)
          [ (n, 0); (n - 1, 0); (n + 3, 3) ]

(* [set] of each field on the fast and the general codec, from the same
   buffer. *)
let check_set_paths label a value bs =
  List.iter
    (fun (Field_check f) ->
      let set c =
        let buf = Bytes.copy bs in
        map_answer
          (fun () -> Bytes.to_string buf)
          (answer (fun () ->
               Wire.Staged.unstage (Wire.Codec.set c f.field) buf 0
                 (f.proj value)))
      in
      check_answer label
        ("Codec.set of field " ^ f.name)
        ~pp:pp_hex ~equal:String.equal
        ("the fast path", set a.acodec)
        ("the general path", set a.ageneral))
    a.achecks
