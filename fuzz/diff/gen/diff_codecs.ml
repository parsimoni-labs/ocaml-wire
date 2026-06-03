(** The codecs the differential fuzzer compares against the EverParse-generated
    C validator: a deterministic, well-distributed Boltzmann {!Fuzz_gen.sample}
    of the shape space (not a handpicked list), filtered to the ones that link
    into the single generated stub translation unit.

    A codec is includable iff it has a positive fixed wire size (variable-size
    codecs have no standalone validator; zero-size unit codecs hit a setter
    arity mismatch), binds no [Param.env], and is not a [casetype].
    Multi-typedef codecs (an array of a bounded lookup, an array of an enum) are
    included: the entrypoint struct and the [WireSet*] callbacks are uniquely
    prefixed per codec, so the only names two codecs can clash on in the shared
    translation unit are enum declarations and the synthesised refined-byte
    structs. We keep the first codec to claim each such name and report the rest
    as {!excluded}, so coverage stays explicit. *)

(* Sample size: each includable codec costs one EverParse run at generation
   time, so this is the knob trading differential breadth against CI time. *)
let sample_count = 64
let sample_seed = 1

let starts_with s p =
  let s = String.trim s in
  String.length s >= String.length p && String.sub s 0 (String.length p) = p

(* The leading C identifier of a token, dropping any trailing punctuation
   ([{], [;], ...) so "Color{" and "_RefByte_lk4;" yield the bare name. *)
let ident_prefix w =
  let is_id c =
    (c >= 'A' && c <= 'Z')
    || (c >= 'a' && c <= 'z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  let n = String.length w in
  let rec go i = if i < n && is_id w.[i] then go (i + 1) else i in
  String.sub w 0 (go 0)

(* Every top-level type name a schema declares, so two codecs that declare the
   same one (a synthesised element struct like [_bounded] or [_RefByte_lk4], an
   [enum]) are not both linked into the single translation unit, where the
   second would redefine it. Extracted generically as the name of each typedef
   close ("} Name;") plus each [enum Name], rather than a hand-listed set of
   patterns, so a new kind of synthesised element is covered automatically. The
   per-codec entrypoint name is harmless here (no two codecs share it) and the
   extern [WireCtx] has no close line, so neither causes a spurious drop. *)
let shared_names lines =
  List.concat_map
    (fun l ->
      let l = String.trim l in
      let from_close =
        if String.length l > 0 && l.[0] = '}' then
          let after = String.trim (String.sub l 1 (String.length l - 1)) in
          match ident_prefix after with "" -> [] | id -> [ id ]
        else []
      in
      let rec enums = function
        | "enum" :: w :: tl -> ident_prefix w :: enums tl
        | _ :: tl -> enums tl
        | [] -> []
      in
      from_close @ enums (String.split_on_char ' ' l))
    lines

let lines_of (Fuzz_gen.Pack g) =
  String.split_on_char '\n'
    (Wire.Everparse.Raw.to_3d
       (Wire.Everparse.schema (Fuzz_gen.codec g)).Wire.Everparse.module_)

type exclusion =
  | Variable_size
  | Zero_size
  | Parameterised
  | Casetype
  | Name_clash of string list

let string_of_exclusion = function
  | Variable_size -> "variable size, no standalone validator"
  | Zero_size -> "zero size, setter arity mismatch"
  | Parameterised -> "parameterised, binds Param.env"
  | Casetype -> "casetype, projects an embedded sub-validator"
  | Name_clash ns ->
      Fmt.str "shared-name duplicate (%s)" (String.concat ", " ns)

(* The scope filter: [None] is includable, [Some r] is out of the differential's
   scope for reason [r]. A casetype or variable/zero-size codec has no standalone
   fixed-size validator; a parameterised one needs an env the harness does not
   set up. *)
let scope_exclusion (Fuzz_gen.Pack g as p) =
  let s = Wire.Everparse.schema (Fuzz_gen.codec g) in
  match s.Wire.Everparse.wire_size with
  | None -> Some Variable_size
  | Some 0 -> Some Zero_size
  | Some _ when Fuzz_gen.binds_env p -> Some Parameterised
  | Some _ when List.exists (fun l -> starts_with l "casetype ") (lines_of p) ->
      Some Casetype
  | Some _ -> None

let candidates = Fuzz_gen.sample ~seed:sample_seed ~count:sample_count

(* Walk candidates in order, including the first to claim each shared name and
   recording later codecs that would redefine one as a [Name_clash], so every
   dropped codec carries an explicit reason. *)
let included, excluded =
  let used = Hashtbl.create 64 in
  let inc, exc =
    List.fold_left
      (fun (inc, exc) (label, p) ->
        match scope_exclusion p with
        | Some r -> (inc, (label, p, r) :: exc)
        | None -> (
            let names = shared_names (lines_of p) in
            match List.filter (Hashtbl.mem used) names with
            | _ :: _ as clash -> (inc, (label, p, Name_clash clash) :: exc)
            | [] ->
                List.iter (fun n -> Hashtbl.replace used n ()) names;
                ((label, p) :: inc, exc)))
      ([], []) candidates
  in
  (List.rev inc, List.rev exc)

let summary =
  let tally =
    List.fold_left
      (fun acc (_, _, r) ->
        let k =
          match r with
          | Variable_size -> "variable-size"
          | Zero_size -> "zero-size"
          | Parameterised -> "parameterised"
          | Casetype -> "casetype"
          | Name_clash _ -> "name-clash"
        in
        let n = try List.assoc k acc with Not_found -> 0 in
        (k, n + 1) :: List.remove_assoc k acc)
      [] excluded
  in
  let by_reason =
    String.concat ", " (List.rev_map (fun (k, n) -> Fmt.str "%d %s" n k) tally)
  in
  Fmt.str "%d candidates: %d included, %d excluded%s" (List.length candidates)
    (List.length included) (List.length excluded)
    (if by_reason = "" then "" else Fmt.str " (%s)" by_reason)
