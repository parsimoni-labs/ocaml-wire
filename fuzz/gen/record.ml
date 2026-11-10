open Gen
open Wrappers

(* {1 Record composition} *)

module Codec = struct
  type ('a, 'r) field = { name : string; gen : 'a t; getter : 'r -> 'a }

  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  let ( $ ) gen getter = { name = "_"; gen; getter }

  (* Assign each field a unique name by position: the [$] above leaves every
     field [_], which round-trips fine (getters, not names) but projects to
     clashing anonymous declarations in 3D. *)
  let field_name i f = if f.name = "_" then "f" ^ string_of_int i else f.name

  (* Build the codec fields and their checkers together, so both retain the
     identity of the same source [Field.t]. Accessors intentionally reject a
     separately declared lookalike field, even when its name and type match. *)
  let wire_codec_fields fields =
    let rec go : type f r.
        int -> (f, r) fields -> (f, r) Wire.Codec.fields * r field_check list =
     fun i -> function
       | [] -> (Wire.Codec.[], [])
       | f :: rest ->
           let name = field_name i f in
           let field =
             Wire.Codec.( $ ) (Wire.Field.v name f.gen.typ) f.getter
           in
           let wire_fields, checks = go (i + 1) rest in
           ( Wire.Codec.(field :: wire_fields),
             Field_check { name; field; proj = f.getter; equal = f.gen.equal }
             :: checks )
    in
    go 0 fields

  (* Walk the field list applying each field's positive sample to the
     partial builder and accumulating the bytes. Mirrors [Wire.Codec.v]'s
     application order. *)
  let rec positives_of : type f r.
      f -> bytes list -> (f, r) fields -> (r * bytes) Alcobar.gen =
   fun partial bytes_acc fields ->
    match fields with
    | [] ->
        let concat =
          let total =
            List.fold_left (fun n b -> n + Bytes.length b) 0 bytes_acc
          in
          let out = Bytes.create total in
          let _ =
            List.fold_left
              (fun off b ->
                let n = Bytes.length b in
                Bytes.blit b 0 out off n;
                off + n)
              0 (List.rev bytes_acc)
          in
          out
        in
        Alcobar.const (partial, concat)
    | f :: rest ->
        Alcobar.dynamic_bind f.gen.positive (fun (v, bytes) ->
            positives_of (partial v) (bytes :: bytes_acc) rest)

  (* Random byte stream for a compound: each field generates random
     bytes; concatenate. *)
  let rec random_of : type f r. (f, r) fields -> bytes Alcobar.gen = function
    | [] -> Alcobar.const Bytes.empty
    | f :: rest ->
        Alcobar.map
          Alcobar.[ f.gen.random; random_of rest ]
          (fun a b ->
            let out = Bytes.create (Bytes.length a + Bytes.length b) in
            Bytes.blit a 0 out 0 (Bytes.length a);
            Bytes.blit b 0 out (Bytes.length a) (Bytes.length b);
            out)

  (* Adversarial: one slot at a time uses its adversarial stream, every
     other slot uses a positive sample. The "one bad slot" shape is the
     adversarial input the decoder is most likely to mishandle; many
     concurrent bad slots devolves to [random]. *)
  let rec field_positives : type f r. (f, r) fields -> bytes Alcobar.gen list =
    function
    | [] -> []
    | f :: rest ->
        let pos = Alcobar.map Alcobar.[ f.gen.positive ] snd in
        pos :: field_positives rest

  let rec adversarial_at : type f r.
      int -> bytes Alcobar.gen list -> (f, r) fields -> bytes Alcobar.gen list =
   fun i positives fields ->
    match (fields, positives) with
    | [], _ | _, [] -> []
    | f :: rest, p :: ps ->
        let slot = if i = 0 then f.gen.adversarial else p in
        slot :: adversarial_at (i - 1) ps rest

  let rec concat_bytes_gens : bytes Alcobar.gen list -> bytes Alcobar.gen =
    function
    | [] -> Alcobar.const Bytes.empty
    | [ g ] -> g
    | g :: rest ->
        Alcobar.map
          Alcobar.[ g; concat_bytes_gens rest ]
          (fun a b ->
            let out = Bytes.create (Bytes.length a + Bytes.length b) in
            Bytes.blit a 0 out 0 (Bytes.length a);
            Bytes.blit b 0 out (Bytes.length a) (Bytes.length b);
            out)

  let rec field_count : type f r. (f, r) fields -> int = function
    | [] -> 0
    | _ :: rest -> 1 + field_count rest

  let rec field_envs : type f r. (f, r) fields -> env_strategy option list =
    function
    | [] -> []
    | f :: rest -> f.gen.env :: field_envs rest

  (* Hostile values compose the same "one bad slot" way the hostile bytes do:
     slot [i] draws from its own [adversarial_value], every other slot draws a
     positive value, so the record is well-formed apart from the one field
     under test. *)
  let rec hostile_value_at : type f r.
      int -> f -> (f, r) fields -> r Alcobar.gen =
   fun slot partial fields ->
    match fields with
    | [] -> Alcobar.const partial
    | f :: rest ->
        let gen =
          match f.gen.adversarial_value with
          | Some g when slot = 0 -> g
          | _ -> Alcobar.map Alcobar.[ f.gen.positive ] fst
        in
        Alcobar.dynamic_bind gen (fun v ->
            hostile_value_at (slot - 1) (partial v) rest)

  let rec hostile_slots : type f r. int -> (f, r) fields -> int list =
   fun i -> function
    | [] -> []
    | f :: rest ->
        let tail = hostile_slots (i + 1) rest in
        if Option.is_some f.gen.adversarial_value then i :: tail else tail

  let v : type f r.
      string -> ?equal:(r -> r -> bool) -> f -> (f, r) fields -> r t =
   fun name ?(equal = ( = )) builder fields ->
    let wire_fields, field_checks = wire_codec_fields fields in
    let codec = Wire.Codec.v name builder wire_fields in
    let typ = Wire.codec codec in
    let positives = field_positives fields in
    let n_fields = field_count fields in
    (* The canonical bytes for a record value are its encoding, not the
       concatenation of each field's sample bytes: adjacent bitfields coalesce
       into one base word, so concatenating their per-field bytes would place
       the second field in the wrong byte. Build the value, then encode it.
       A record carrying a param field ([param_size] / [rest_bytes]) needs the
       env threaded into [encode], else it raises and the concat fallback would
       ship the unpacked bytes; bind the field params first. Fall back to the
       test runner if no env makes encode succeed: that is either a library bug
       or a fuzzer-model bug, and hiding it would make later positives
       misleading. *)
    let env_strategy = combine_env_strategies (field_envs fields) in
    let positive =
      Alcobar.map
        Alcobar.[ positives_of builder [] fields ]
        (fun (v, _concat) ->
          let env =
            Option.map
              (fun (s : env_strategy) -> s.positive (Wire.Codec.env codec))
              env_strategy
          in
          let buf = Bytes.create (Wire.Codec.size_of_value codec v) in
          Wire.Codec.encode ?env codec v buf 0;
          (v, buf))
    in
    let random = random_of fields in
    let adversarial =
      let per_slot =
        List.init n_fields (fun i ->
            concat_bytes_gens (adversarial_at i positives fields))
      in
      if per_slot = [] then Alcobar.const Bytes.empty
      else Alcobar.choose per_slot
    in
    let adversarial_value =
      match hostile_slots 0 fields with
      | [] -> None
      | slots ->
          Some
            (Alcobar.choose
               (List.map (fun i -> hostile_value_at i builder fields) slots))
    in
    {
      codec;
      typ;
      positive;
      random;
      adversarial;
      equal;
      env = combine_env_strategies (field_envs fields);
      adversarial_value;
      fields = field_checks;
    }
end
