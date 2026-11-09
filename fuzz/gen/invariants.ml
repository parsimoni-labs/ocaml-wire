open Gen
open Compose
open Properties
open Curated
open Codec_sampler

let any_labels xs = List.map (fun (Any a) -> a.label) xs

let duplicate_labels labels =
  let sorted = List.sort String.compare labels in
  let rec go acc = function
    | a :: (b :: _ as rest) when String.equal a b -> go (a :: acc) rest
    | _ :: rest -> go acc rest
    | [] -> List.rev acc
  in
  go [] sorted

let assert_no_duplicates group labels =
  match duplicate_labels labels with
  | [] -> ()
  | dups ->
      Alcobar.failf "%s duplicate labels: %s" group (String.concat ", " dups)

let assert_contains group labels expected =
  List.iter
    (fun label ->
      if not (List.mem label labels) then
        Alcobar.failf "%s missing %s" group label)
    expected

let expected_registry_labels =
  [
    "uint8";
    "uint16";
    "uint16be";
    "uint16(endian_edges)";
    "uint16be(endian_edges)";
    "uint32";
    "uint32be";
    "uint32(endian_edges)";
    "uint32be(endian_edges)";
    "uint64";
    "uint64be";
    "uint64(endian_edges)";
    "uint64be(endian_edges)";
    "int8";
    "int16";
    "int16be";
    "int16(endian_edges)";
    "int16be(endian_edges)";
    "int32";
    "int32be";
    "int32(endian_edges)";
    "int32be(endian_edges)";
    "int64";
    "int64be";
    "int64(endian_edges)";
    "int64be(endian_edges)";
    "float32";
    "float32be";
    "float64";
    "float64be";
    "empty";
    "uint_var(1,little)";
    "uint_var(2,little)";
    "uint_var(3,little)";
    "uint_var(3,big)";
    "uint_var(5,little)";
    "uint_var(5,big)";
    "uint_var(7,little)";
    "uint_var(7,big)";
    "uint_var(3,little_edges)";
    "uint_var(3,big_edges)";
    "uint_var(7,little_edges)";
    "uint_var(7,big_edges)";
    "bit(uint8)";
    "bit(uint16)";
    "bit(uint16be)";
    "bit(int8)";
    "bit(int16)";
    "bit(int16be)";
    "byte_array(0)";
    "byte_array(5)";
    "byte_slice(0)";
    "byte_slice(3)";
    "byte_array_where(4)";
    "all_bytes";
    "all_zeros";
    "zeroterm";
    "zeroterm_at_most(1)";
    "zeroterm_at_most(8)";
    "bits(1,U8)";
    "bits(1,U8,Lsb)";
    "bits(3,U8)";
    "bits(3,U8,Lsb)";
    "bits(7,U8)";
    "bits(7,U8,Lsb)";
    "bits(8,U8)";
    "bits(8,U8,Lsb)";
    "bits(1,U16)";
    "bits(15,U16)";
    "bits(16,U16)";
    "bits(16,U16,Lsb)";
    "bits(1,U16be)";
    "bits(10,U16be)";
    "bits(4,U16be)";
    "bits(15,U16be,Lsb)";
    "bits(16,U16be)";
    "bits(16,U16be,Lsb)";
    "bits(1,U32)";
    "bits(17,U32)";
    "bits(17,U32,Lsb)";
    "bits(32,U32,Lsb)";
    "bits(1,U32be)";
    "bits(31,U32be)";
    "bits(31,U32be,Lsb)";
    "bits(32,U32be)";
    "bits(32,U32be,Lsb)";
    "bits(4,U32,Lsb)";
    "bitpack_u8_msb(1,2,5)";
    "bitpack_u8_lsb(1,2,5)";
    "bitpack_u8_spill(5,5)";
    "bitpack_split_orders";
    "bitpack_u16_msb(3,2,11)";
    "bitpack_u16_lsb(3,2,11)";
    "bitpack_u16be_msb(3,2,11)";
    "bitpack_u16be_lsb(3,2,11)";
    "bitpack_u16be_spill(9,9)";
    "bitpack_u32_msb(6,10,16)";
    "bitpack_u32_lsb(6,10,16)";
    "bitpack_u32be_msb(6,10,16)";
    "bitpack_u32be_lsb(6,10,16)";
    "map(uint8)";
    "variants";
    "variants_u16be";
    "enum";
    "enum_u16be";
    "enum_open";
    "enum_open_u16be";
    "bounded_u8";
    "bounded_u16be";
    "bounded_u32be";
    "where(uint8)";
    "lookup";
    "optional(uint8)";
    "optional(false)";
    "optional_or(uint8)";
    "optional_or(false)";
    "optional_dynamic";
    "optional_or_dynamic";
    "nested(0,empty)";
    "nested(2,uint16be)";
    "nested(4,byte_array4)";
    "nested(3,byte_slice3)";
    "nested(4,byte_array_where4)";
    "nested(8,zeroterm_at_most8)";
    "nested_at_most(0,empty)";
    "nested_at_most(2,uint16be)";
    "nested_at_most(4,uint16be)";
    "array(0,uint8)";
    "array(3,uint16be)";
    "array_seq(3,uint16be)";
    "array_seq(0,uint8)";
    "array(2,enum_u16be)";
    "repeat(0,uint8)";
    "repeat(8,uint8)";
    "repeat(7,uint16be)";
    "repeat(6,enum)";
    "repeat_seq(8,uint8)";
    "repeat_seq(7,uint16be)";
    "record";
    "subcodec_last(record)";
    "subcodec_first(bits)";
    "subcodec_twice(record)";
    "subcodec_deep(record)";
    "subcodec_var(repeat)";
    "subcodec_ztonly(zeroterm)";
    "casetype_u8";
    "casetype_u16be_default";
    "field_anon";
    "action";
    "action_on_act";
    "expr_ops";
    "sizeof";
    "codec_where";
    "typ_where";
    "field_constraint";
    "field_int";
    "self_int64";
    "rest_bytes";
    "param_input";
    "param_size";
    "if_then_else";
    "finite_float64";
    "nan_float64";
  ]

let expected_fixed_labels =
  [
    "u8";
    "u16le";
    "u16";
    "u32le";
    "u32";
    "u64le";
    "u64";
    "i16le";
    "i16";
    "i32le";
    "i32";
    "i64le";
    "i64";
    "uv3le";
    "ba0";
    "bs0";
    "ba3";
    "bs4";
    "bits3lsb";
    "bp8m";
    "bp8l";
    "bp8spill";
    "bp16l";
    "bp16bem";
    "bp16bel";
    "bp32m";
    "bp32bel";
    "enum";
    "enum_open";
    "enum16";
    "enumopen16";
    "var";
    "var16";
    "fconstraint";
    "fint";
    "self64";
  ]

let expected_var_labels =
  [ "zt"; "zt1"; "zt6"; "az"; "optdyn"; "optordyn"; "ite" ]

let expected_env_labels = [ "param"; "param_size"; "rest"; "act"; "onact" ]

let expected_sampler_labels =
  [
    "u8";
    "u16";
    "u16be";
    "u32";
    "u32be";
    "i16";
    "i16be";
    "i32";
    "i32be";
    "i64";
    "i64be";
    "bits3lsb";
    "bits15u16";
    "bits15u16belsb";
    "bp8m";
    "bp8l";
    "bp16bel";
    "enum";
    "enum_open";
    "lkp";
    "ba3";
    "baw3";
    "uv3";
    "uv3le";
    "bnd16";
    "bnd32";
    "twhere";
    "fconstraint";
  ]
  @ any_labels [ flat_check_order; flat_table; flat_fixed_steps; wide 6 ]

let check_registry_invariants () =
  let labels = List.map fst registry in
  assert_no_duplicates "registry" labels;
  assert_contains "registry" labels expected_registry_labels

let check_vocabulary_group (group, labels) =
  if labels = [] then Alcobar.failf "%s is empty" group;
  assert_no_duplicates group labels

let check_composition_vocabulary () =
  let fixed = any_labels fixed_leaves
  and var = any_labels var_leaves
  and env = any_labels env_leaves
  and sampled = any_labels sampler_leaves
  and sampled_regular = any_labels sampler_regular_leaves
  and sampled_adversarial = any_labels sampler_adversarial_leaves in
  List.iter check_vocabulary_group
    [
      ("fixed_leaves", fixed);
      ("var_leaves", var);
      ("env_leaves", env);
      ("sampler_leaves", sampled);
      ("sampler_regular_leaves", sampled_regular);
      ("sampler_adversarial_leaves", sampled_adversarial);
    ];
  assert_contains "fixed_leaves" fixed expected_fixed_labels;
  assert_contains "var_leaves" var expected_var_labels;
  assert_contains "env_leaves" env expected_env_labels;
  assert_contains "sampler_leaves" sampled expected_sampler_labels;
  assert_contains "sampler_adversarial_leaves" sampled_adversarial
    [
      "bits3lsb";
      "bits15u16";
      "bits15u16belsb";
      "bp8m";
      "bp8l";
      "bp16bel";
      "bp32m";
      "bp32bel";
      "enum";
      "enum_open";
      "lkp";
    ]

let check_sampler_invariants () =
  let labels = List.map fst (sample ~seed:1 ~count:64) in
  if List.length labels <> 64 then
    Alcobar.failf "sample returned %d labels, expected 64" (List.length labels);
  assert_no_duplicates "sample" labels;
  List.iteri
    (fun i label ->
      let prefix = Fmt.str "smp%d_" i in
      if not (String.starts_with ~prefix label) then
        Alcobar.failf "sample label %S does not start with %S" label prefix)
    labels

(* The differential's code generator and its runner each call [sample] at the
   same seed and pair the results up by label; they are two processes, so
   nothing but reproducibility keeps them comparing the same codec. A draw
   sequence that depended on anything outside [rng] -- evaluation order, a
   counter shared between samples, a hash -- would have them silently compare
   mismatched shapes rather than fail. Redraw the differential's own seed and
   count, and pin both the labels and the shapes. *)
let check_sampler_reproducible () =
  let shapes () =
    List.map (fun (label, _, m) -> (label, m.shape)) (sampled ~seed:1 ~count:64)
  in
  let first = shapes () in
  let again = shapes () in
  let same (l1, s1) (l2, s2) = String.equal l1 l2 && equal_shape s1 s2 in
  if not (List.equal same first again) then
    Alcobar.failf "sample ~seed:1 is not reproducible across two calls"

let check_sampler_adversarial_bias () =
  let adversarial = any_labels sampler_adversarial_leaves in
  let is_adversarial l = List.mem l adversarial in
  let metas = List.map (fun (_, _, m) -> m) (sampled ~seed:7 ~count:256) in
  let adversarial_hits =
    List.fold_left
      (fun count m ->
        if List.exists is_adversarial m.leaves then count + 1 else count)
      0 metas
  in
  if adversarial_hits < 160 then
    Alcobar.failf
      "adversarial-biased sampler produced %d/256 weird/adversarial shapes"
      adversarial_hits

let shape_is_leaf = function Leaf -> true | Array _ | Record _ -> false
let shape_is_array = function Array _ -> true | Leaf | Record _ -> false

let shape_is_record_arity n = function
  | Record fs -> List.compare_length_with fs n = 0
  | Leaf | Array _ -> false

(* [p] holds somewhere in the tree, not only at its root: nesting is the point,
   so a record of arity 3 buried under an array still counts as one seen. *)
let rec shape_exists p s =
  p s
  ||
  match s with
  | Leaf -> false
  | Array e -> shape_exists p e
  | Record fs -> List.exists (shape_exists p) fs

(* Beyond the gross 3:1 adversarial-bias ratio ([check_sampler_adversarial_bias]),
   enforce that one deterministic sample actually spreads across the grammar:
   every shape (leaf, array, record of each arity), both endians and both bit
   orders, signed ints, floats, and a spread of bitpack sizes all appear. Reads
   the structured [sample_meta] each codec carries, not its debug label. The
   margins on a 256-codec sample are large, so this guards real distribution
   regressions, not RNG noise. *)
let check_sampler_distribution () =
  let metas = List.map (fun (_, _, m) -> m) (sampled ~seed:7 ~count:256) in
  let all_leaves = List.concat_map (fun m -> m.leaves) metas in
  let has l = List.mem l all_leaves in
  let has_any = List.exists has in
  let has_shape p = List.exists (fun m -> p m.shape) metas in
  let checks =
    [
      ("no bare leaves", has_shape shape_is_leaf);
      ("no arrays", has_shape (shape_exists shape_is_array));
      ( "no record of arity 2",
        has_shape (shape_exists (shape_is_record_arity 2)) );
      ( "no record of arity 3",
        has_shape (shape_exists (shape_is_record_arity 3)) );
      ( "no record of arity 4",
        has_shape (shape_exists (shape_is_record_arity 4)) );
      ( "no little-endian multibyte int",
        has_any [ "u16"; "u32"; "u64"; "i16"; "i32"; "i64" ] );
      ( "no big-endian multibyte int",
        has_any [ "u16be"; "u32be"; "u64be"; "i16be"; "i32be"; "i64be" ] );
      ( "no signed int",
        has_any [ "i8"; "i16"; "i16be"; "i32"; "i32be"; "i64"; "i64be" ] );
      ("no float", has_any [ "f32be"; "f64be" ]);
      ("no enum", has_any [ "enum"; "enum_open" ]);
      ("no lookup", has "lkp");
      ("no bounded", has "bnd");
      ("no msb-first bit field", has_any [ "bits3"; "bits15u16"; "bp8m" ]);
      ( "no lsb-first bit field",
        has_any [ "bits3lsb"; "bits15u16belsb"; "bp8l"; "bp16bel" ] );
      ("no 8-bit bitpack", has_any [ "bp8m"; "bp8l" ]);
      ("no 16-bit bitpack", has "bp16bel");
      ("no 32-bit bitpack", has_any [ "bp32m"; "bp32bel" ]);
      ("no byte span", has "ba3");
      ("no refined byte span", has "baw3");
      ("no arbitrary-width uint", has_any [ "uv3"; "uv3le" ]);
      ("no wide bounded int", has_any [ "bnd16"; "bnd32" ]);
      ("no refined sub-codec", has_any [ "twhere"; "fconstraint" ]);
    ]
  in
  List.iter
    (fun (what, ok) ->
      if not ok then Alcobar.failf "sampler distribution: %s" what)
    checks

(* The three shapes a flat sampler cannot reach, and the three the differential
   most wants: an array next to another field, an array inside an array's
   element, and a third level of structure over either. A mis-sized inner value
   shifts every field that follows it, and only a nested draw ever puts a field
   after one. Kept apart from [check_sampler_distribution] so a regression here
   names the nesting rather than the vocabulary. *)
let check_sampler_nesting () =
  let metas = List.map (fun (_, _, m) -> m) (sampled ~seed:7 ~count:256) in
  let has p = List.exists (fun m -> p m.shape) metas in
  let checks =
    [
      ("no array beside other record fields", has shape_has_array_beside_fields);
      ("no array of a repeating element", has shape_has_nested_repetition);
      ("no three-level shape", has (fun s -> shape_depth s >= 3));
    ]
  in
  List.iter
    (fun (what, ok) -> if not ok then Alcobar.failf "sampler nesting: %s" what)
    checks

(* How many registry entries the per-field properties actually reach. Staging an
   accessor is allowed to refuse -- a field shape with no staged accessor is a
   documented gap, not a failure -- so a change to [addressed], to the registry
   or to the accessor API could turn every per-field property into a silent
   no-op without one test going red. Pin the floor. *)
let field_reach () =
  let reach (readable, writable, multi) (_, Pack g) =
    let a = addressed g in
    let readable_field fc =
      match fc with Field_check f -> Option.is_some (stage_get a f.field)
    in
    let writable_field fc =
      match fc with Field_check f -> Option.is_some (stage_set a f.field)
    in
    (* [set] takes no env, so the write properties skip a codec that binds one;
       locality needs two fields to play one off against the other. *)
    let env_free = Option.is_none g.env in
    ( (if List.exists readable_field a.achecks then readable + 1 else readable),
      (if env_free && List.exists writable_field a.achecks then writable + 1
       else writable),
      if env_free && List.compare_length_with a.achecks 1 > 0 then multi + 1
      else multi )
  in
  List.fold_left reach (0, 0, 0) registry

let check_field_reach () =
  let readable, writable, multi = field_reach () in
  if readable < 120 || writable < 115 || multi < 15 then
    Alcobar.failf
      "per-field properties reach %d readable, %d writable and %d multi-field \
       registry entries out of %d"
      readable writable multi (List.length registry)

(* Every fast path [Codec] can select must be selected by some codec the
   fuzzer draws -- a registry entry, a composition leaf or a fixed-seed sample
   -- or the oracles comparing fast and general paths never run it. Computed
   once: the sample builds its codecs afresh. *)
let missing_paths =
  lazy
    (let paths g = Wire.Private.Testing.paths g.codec in
     let reached =
       List.concat_map (fun (_, Pack g) -> paths g) registry
       @ List.concat_map
           (fun (Any a) -> paths a.g)
           (fixed_leaves @ var_leaves @ env_leaves @ flat_leaves)
       @ List.concat_map
           (fun (_, Pack g) -> paths g)
           (sample ~seed:42 ~count:64)
     in
     List.filter
       (fun p -> not (List.mem p reached))
       Wire.Private.Testing.all_paths)

let check_path_coverage () =
  match Lazy.force missing_paths with
  | [] -> ()
  | missing ->
      Alcobar.failf "no codec the fuzzer draws selects the fast paths: %s"
        (String.concat ", " missing)

(* A check with no random input. Alcobar runs a case's body on every
   iteration, each under its per-iteration timeout, so such a check only
   repeated itself, and a slow one could cross the timeout on a slow runner:
   the sampler checks build hundreds of codecs. Run it once, when the suite is
   built, and let the case report that outcome. AFL runs keep the plain body. *)
let const_case name check =
  let outcome =
    if afl_mode () then None
    else
      Some
        (match check () with
        | () -> Ok ()
        | exception e -> Error (e, Printexc.get_raw_backtrace ()))
  in
  Alcobar.test_case name
    Alcobar.[ Alcobar.const () ]
    (fun () ->
      match outcome with
      | None -> check ()
      | Some (Ok ()) -> ()
      | Some (Error (e, bt)) -> Printexc.raise_with_backtrace e bt)

let invariant_cases label =
  [
    const_case (label ^ " registry") check_registry_invariants;
    const_case (label ^ " composition vocabulary") check_composition_vocabulary;
    const_case (label ^ " sampler") check_sampler_invariants;
    const_case (label ^ " sampler reproducible") check_sampler_reproducible;
    const_case
      (label ^ " sampler adversarial bias")
      check_sampler_adversarial_bias;
    const_case (label ^ " sampler distribution") check_sampler_distribution;
    const_case (label ^ " sampler nesting") check_sampler_nesting;
    const_case (label ^ " field access reach") check_field_reach;
    const_case (label ^ " fast path coverage") check_path_coverage;
  ]
