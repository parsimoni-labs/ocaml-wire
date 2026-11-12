open Gen
open Compose
open Properties

(* Two positives from one slot of the test case: the per-field write checks
   need a second value of the record's own type to write over the first, and
   pairing them here keeps the case's generator arity unchanged. *)
let two_positives g =
  Alcobar.map Alcobar.[ g.positive; g.positive ] (fun a b -> (a, b))

let check_positive_stream ~validate label a g (sample, (other, other_bs))
    hostile noise =
  let value, bs = sample in
  check_positive label g sample;
  if validate then check_validate_positive label g bs;
  check_fields label a g sample other hostile;
  let env = positive_env g in
  check_offset_invariance (label ^ " positive") ?env a g noise bs;
  check_offset_encode (label ^ " positive") ?env g value bs noise;
  check_frame_extent label "positive" ?env g noise bs;
  check_short_prefix label "positive" ?env g bs;
  check_read_purity label "positive" ?env a g bs;
  check_frame_isolation label "positive" ?env a g bs other_bs

(* Every component of a rejection's root-to-leaf field path must name a field
   the codec declares somewhere in its schema. An empty path is valid for a
   top-level or anonymous failure; an invented name sends the caller to bytes
   that do not exist. *)
let check_rejection_names label kind ?env a g bs =
  match Wire.Codec.decode ?env g.codec bs 0 with
  | Ok _ | (exception Invalid_argument _) -> ()
  | Error e ->
      List.iter
        (fun name ->
          if not (List.mem name a.anames) then
            Alcobar.failf
              "%s %s: rejection names undeclared field %S (declared: %s)" label
              kind name
              (String.concat ", " a.anames))
        e.Wire.field

let check_safety_stream ~validate label a g kind ?env ~interloper ~noise bs =
  check_decode_safety label kind ?env g bs;
  check_rejection_names label kind ?env a g bs;
  check_direct_codec_agree label kind g bs;
  check_offset_invariance (Fmt.str "%s %s" label kind) ?env a g noise bs;
  check_frame_extent label kind ?env g noise bs;
  check_short_prefix label kind ?env g bs;
  check_read_purity label kind ?env a g bs;
  check_frame_isolation label kind ?env a g bs interloper;
  if validate then begin
    check_validate_safety label kind ?env g bs;
    check_decode_validate_agree label kind ?env g bs;
    check_field_reads label kind ?env a g bs
  end

let check_hostile_stream label g ?env = function
  | None -> ()
  | Some value ->
      check_direct_codec_encode_agree label "hostile" g value;
      check_encode_safety label ?env g value

(* The stream layout for a codec that binds no [Param.env]: one case, five
   generator slots, three streams plus the noise every metamorphic check
   transforms the sample with. Each safety stream stands in as the other's
   interloper for the non-interference check, so no extra slot is needed for
   the second frame. *)
let env_free_case ~validate label a g =
  let positive_stream = check_positive_stream ~validate label a g in
  let safety_stream = check_safety_stream ~validate label a g in
  let hostile_stream = check_hostile_stream label g in
  Alcobar.test_case (label ^ " all")
    Alcobar.
      [
        two_positives g;
        g.random;
        g.adversarial;
        hostile_value_stream g;
        noise_gen;
      ]
    (fun positives random adversarial hostile noise ->
      positive_stream positives hostile noise;
      safety_stream "random" ~interloper:adversarial ~noise random;
      safety_stream "adversarial" ~interloper:random ~noise adversarial;
      hostile_stream hostile)

(* The same, with a binder slot per safety stream for the codec's params. *)
let env_bound_case ~validate label a g (s : env_strategy) =
  let positive_stream = check_positive_stream ~validate label a g in
  let safety_stream = check_safety_stream ~validate label a g in
  let hostile_stream = check_hostile_stream label g in
  Alcobar.test_case (label ^ " all")
    Alcobar.
      [
        two_positives g;
        g.random;
        g.adversarial;
        s.fuzz;
        s.fuzz;
        hostile_value_stream g;
        noise_gen;
      ]
    (fun positives
         random
         adversarial
         random_env
         adversarial_env
         hostile
         noise
       ->
      positive_stream positives hostile noise;
      let env = Some (random_env (Wire.Codec.env g.codec)) in
      safety_stream "random" ?env ~interloper:adversarial ~noise random;
      let env = Some (adversarial_env (Wire.Codec.env g.codec)) in
      safety_stream "adversarial" ?env ~interloper:random ~noise adversarial;
      (* The hostile value is the thing under test, so bind the params the way a
         positive would: a random env would make encode fail for an unrelated
         reason. *)
      hostile_stream ?env:(positive_env g) hostile)

let test_cases ?(validate = true) label g =
  (* Built once per case, not once per sample: for a gen with no field list of
     its own [addressed] compiles a wrapper codec, which is far too expensive
     to repeat per draw. *)
  let a = addressed g in
  match g.env with
  | None -> [ env_free_case ~validate label a g ]
  | Some s -> [ env_bound_case ~validate label a g s ]

(* Cross-field sizes: a [byte_array] whose [~size] reads a preceding int field.
   The size-source field is drawn from several int-valued field types -- in
   particular [optional_or], whose present-or-default value the size expression
   reads. Each source builds a two-field [(len, data)] codec round-tripped
   through [test_cases], which checks decode, [size_of_value], and re-encode
   against canonical bytes assembled here (the length-field bytes followed by
   [len] data bytes). *)
let u8_size_bytes n =
  let b = Bytes.create 1 in
  Bytes.set_uint8 b 0 n;
  b

let u16be_size_bytes n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 n;
  b

let sized_source ~of_len name make_len bytes_of_len =
  let len_field = make_len () in
  let data_field =
    Wire.Field.v "Data" (Wire.byte_array ~size:(Wire.Field.ref len_field))
  in
  let codec =
    Wire.Codec.v ("Sized" ^ name)
      (fun len data -> (len, data))
      Wire.Codec.[ len_field $ fst; data_field $ snd ]
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 7 ]
      (fun len ->
        let data = String.make len 'a' in
        ((of_len len, data), Bytes.cat (bytes_of_len len) (bytes_of_string data)))
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
    fields = pair_fields ("Len", len_field) ("Data", data_field);
    adversarial_value = None;
  }

let sized_cases group =
  (* The u8 sources carry their length in a [Wire.UInt8.t] and the u16be one in
     a [Wire.UInt16.t], so the sources cannot sit in one list; each is turned
     into test cases where its own type is still known. *)
  let case slabel src = test_cases (group ^ " " ^ slabel) src in
  List.concat
    [
      case "u8"
        (sized_source ~of_len:Wire.UInt8.v "U8"
           (fun () -> Wire.Field.v "Len" Wire.uint8)
           u8_size_bytes);
      case "u16be"
        (sized_source ~of_len:Wire.UInt16.v "U16be"
           (fun () -> Wire.Field.v "Len" Wire.uint16be)
           u16be_size_bytes);
      case "map"
        (sized_source ~of_len:Wire.UInt8.v "Map"
           (fun () ->
             Wire.Field.v "Len"
               (Wire.map ~decode:Fun.id ~encode:Fun.id Wire.uint8))
           u8_size_bytes);
      case "optional_or"
        (sized_source ~of_len:Wire.UInt8.v "OptOr"
           (fun () ->
             Wire.Field.optional_or "Len" ~present:Wire.Expr.true_
               ~default:Wire.UInt8.zero Wire.uint8)
           u8_size_bytes);
      case "where"
        (sized_source ~of_len:Wire.UInt8.v "Where"
           (fun () ->
             Wire.Field.v "Len" (Wire.where Wire.Expr.true_ Wire.uint8))
           u8_size_bytes);
    ]

(* Drive a freshly generated nested composition per sample: the positive case
   asserts round-trip (and so [size_of_value] / [encode] consistency across the
   whole nesting), the other two assert the decoder never raises. Generated
   codecs never reference [Param.input], so no env is threaded. *)
type nested_sample = NS : 'a t * string * ('a * bytes) * 'a -> nested_sample
(* The trailing value is a second, independent positive of the same
         generated codec, which the per-field write checks need in order to
         write something other than what the buffer already holds. *)

type nested_bytes =
  | NB :
      'a t * string * bytes * (Wire.Param.env -> Wire.Param.env) option
      -> nested_bytes

let check_nested_positive label noise
    (NS (g, comp, ((value, bs) as pos), other)) =
  let label = Fmt.str "%s [%s]" label comp in
  try
    let a = addressed g in
    let env = positive_env g in
    check_positive label g pos;
    check_validate_positive label g bs;
    check_fields label a g pos other None;
    check_offset_invariance (label ^ " positive") ?env a g noise bs;
    check_offset_encode (label ^ " positive") ?env g value bs noise;
    check_frame_extent label "positive" ?env g noise bs;
    check_read_purity label "positive" ?env a g bs
  with Failure m -> Alcobar.failf "%s raised: %s" label m

let check_nested_bytes label kind noise (NB (g, comp, bs, mk)) =
  let label = Fmt.str "%s [%s]" label comp in
  let a = addressed g in
  let env = Option.map (fun f -> f (Wire.Codec.env g.codec)) mk in
  check_decode_safety label kind ?env g bs;
  check_validate_safety label kind ?env g bs;
  check_direct_codec_agree label kind g bs;
  check_field_reads label kind ?env a g bs;
  check_offset_invariance (Fmt.str "%s %s" label kind) ?env a g noise bs;
  check_frame_extent label kind ?env g noise bs;
  check_read_purity label kind ?env a g bs

(* An arbitrary nested codec paired with one buffer to read it at: the
   adversarial stream sits on the codec's own boundaries, the random one is
   arbitrary bytes. *)
let nested_bytes_gen depth ~adversarial =
  Alcobar.dynamic_bind (gen_any depth) (fun (Any a) ->
      let stream = if adversarial then a.g.adversarial else bytes_any in
      match a.g.env with
      | None ->
          Alcobar.map Alcobar.[ stream ] (fun bs -> NB (a.g, a.label, bs, None))
      | Some s ->
          Alcobar.map
            Alcobar.[ stream; s.fuzz ]
            (fun bs mk -> NB (a.g, a.label, bs, Some mk)))

let nested_cases label depth =
  let positive =
    Alcobar.dynamic_bind (gen_any depth) (fun (Any a) ->
        Alcobar.map
          Alcobar.[ a.g.positive; a.g.positive ]
          (fun pos (other, _) -> NS (a.g, a.label, pos, other)))
  in
  let with_bytes adversarial = nested_bytes_gen depth ~adversarial in
  [
    Alcobar.test_case (label ^ " all")
      Alcobar.[ positive; with_bytes false; with_bytes true; noise_gen ]
      (fun positive random adversarial noise ->
        check_nested_positive label noise positive;
        check_nested_bytes label "random" noise random;
        check_nested_bytes label "adversarial" noise adversarial);
  ]

(* The agreement property on generated compositions rather than curated ones.
   A check a combinator carries is enforced at every use site or at none, and a
   use site only exists once the combinator is nested inside something else, so
   the shapes that can drift are exactly the ones no curated list enumerates:
   the registry gens are each their own top-level codec, where decode and
   validate share the same compiled field list and cannot disagree. *)
let validate_agrees_cases label depth =
  let check kind (NB (g, comp, bs, mk)) =
    let env = Option.map (fun f -> f (Wire.Codec.env g.codec)) mk in
    check_decode_validate_agree (Fmt.str "%s [%s]" label comp) kind ?env g bs
  in
  [
    Alcobar.test_case
      (label ^ " validate agrees with decode")
      Alcobar.
        [
          nested_bytes_gen depth ~adversarial:false;
          nested_bytes_gen depth ~adversarial:true;
        ]
      (fun random adversarial ->
        check "random" random;
        check "adversarial" adversarial);
  ]

(* Round-trip through {!Wire.of_string} / {!Wire.to_string}: the typ-level
   API, distinct from [Codec.encode/decode]. Skipped for codecs that take a
   [Param.env] since the direct entry points don't thread one. *)
let direct_cases label g =
  if Option.is_some g.env then []
  else
    let pos_case =
      Alcobar.test_case
        (label ^ " of_string/to_string positive")
        Alcobar.[ g.positive ]
        (fun (value, bs) ->
          let s = string_of_bytes bs in
          (match Wire.of_string g.typ s with
          | Ok decoded ->
              if not (g.equal decoded value) then
                Alcobar.failf "%s direct decode mismatch" label
          | Error _ -> Alcobar.failf "%s direct decode failed" label);
          let out = Wire.to_string g.typ value in
          if out <> s then Alcobar.failf "%s direct reencode mismatch" label)
    in
    let safety kind stream =
      Alcobar.test_case
        (label ^ " of_string " ^ kind)
        Alcobar.[ stream ]
        (fun bs ->
          match Wire.of_string g.typ (string_of_bytes bs) with
          | Ok _ | Error _ -> ()
          | exception Invalid_argument _ ->
              Alcobar.failf "%s direct %s crashed decoder" label kind)
    in
    [ pos_case; safety "random" g.random; safety "adversarial" g.adversarial ]

(* Round-trip through {!Wire.of_reader} / {!Wire.to_writer}: streaming
   entry points. Same env restriction as {!direct_cases}. *)
let streaming_cases label g =
  if Option.is_some g.env then []
  else
    let pos_case =
      Alcobar.test_case
        (label ^ " of_reader/to_writer positive")
        Alcobar.[ g.positive ]
        (fun (value, bs) ->
          let reader = Bytesrw.Bytes.Reader.of_bytes bs in
          (match Wire.of_reader g.typ reader with
          | Ok decoded ->
              if not (g.equal decoded value) then
                Alcobar.failf "%s streaming decode mismatch" label
          | Error _ -> Alcobar.failf "%s streaming decode failed" label);
          let buf = Buffer.create (Bytes.length bs) in
          let writer = Bytesrw.Bytes.Writer.of_buffer buf in
          Wire.to_writer g.typ value writer;
          Bytesrw.Bytes.Writer.write_eod writer;
          let out = Buffer.contents buf in
          if out <> string_of_bytes bs then
            Alcobar.failf "%s streaming reencode mismatch" label)
    in
    [ pos_case ]

(* Exercises {!Wire.Codec.validate}: validation-only path that should
   accept positives and reject (without crashing) random/adversarial.
   When the codec references [Param.input], positives pull the env from
   [g.env.positive] and the safety streams fuzz the env. *)
let validate_cases label g =
  let pos_case =
    Alcobar.test_case
      (label ^ " validate positive")
      Alcobar.[ g.positive ]
      (fun (_value, bs) ->
        let env = positive_env g in
        match validate_one ?env g bs with
        | `Ok -> ()
        | `Reject -> Alcobar.failf "%s validate rejected a positive" label
        | `Crash -> Alcobar.failf "%s validate crashed on a positive" label)
  in
  let safety kind stream =
    match g.env with
    | None ->
        Alcobar.test_case
          (label ^ " validate " ^ kind)
          Alcobar.[ stream ]
          (fun bs ->
            match validate_one g bs with
            | `Ok | `Reject -> ()
            | `Crash -> Alcobar.failf "%s validate %s crashed" label kind)
    | Some s ->
        Alcobar.test_case
          (label ^ " validate " ^ kind)
          Alcobar.[ stream; s.fuzz ]
          (fun bs build_env ->
            let env = Some (build_env (Wire.Codec.env g.codec)) in
            match validate_one ?env g bs with
            | `Ok | `Reject -> ()
            | `Crash -> Alcobar.failf "%s validate %s crashed" label kind)
  in
  [ pos_case; safety "random" g.random; safety "adversarial" g.adversarial ]

(* Driver for codecs that always reject (e.g. [Action.abort]). Generates
   random bytes plus, optionally, a random env, and asserts that decode
   returns [Error _] (not [Ok], not [Invalid_argument]). *)
let reject_cases label g =
  let env_stream =
    match g.env with Some s -> s.fuzz | None -> Alcobar.const Fun.id
  in
  let check_reject bs build_env =
    let env = Some (build_env (Wire.Codec.env g.codec)) in
    match Wire.Codec.decode ?env g.codec bs 0 with
    | Ok _ -> Alcobar.failf "%s decoder accepted input it should reject" label
    | Error _ -> ()
    | exception Invalid_argument _ -> Alcobar.failf "%s decoder crashed" label
  in
  [
    Alcobar.test_case (label ^ " all")
      Alcobar.[ g.random; g.adversarial; env_stream; env_stream ]
      (fun random adversarial random_env adversarial_env ->
        check_reject random random_env;
        check_reject adversarial adversarial_env);
  ]
