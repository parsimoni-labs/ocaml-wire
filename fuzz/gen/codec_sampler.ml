open Leaves
open Wrappers
open Compose
open Curated

(* {1 Boltzmann sampler over the projectable grammar}

   A well-distributed random codec sampler. Rather than enumerate a handpicked
   set, [sample] draws codecs whose shape follows a Boltzmann law: the arity of
   a record is geometric (the Boltzmann distribution for a sequence, tuned to a
   small expected size via [continue]), and each field is drawn either from a
   fixed leaf vocabulary that deliberately over-samples weird / adversarial
   shapes or, recursively, from the sampler itself. It stays in the fixed-size
   fragment (records and homogeneous arrays) so the differential fuzzer can
   compile every sample to a standalone C validator. Driven by a
   [Random.State.t], so a [seed] reproduces the exact set: the differential's
   generator and its runner both call [sample] with the same seed and agree on
   the shapes. *)

(* Fixed-size leaf vocabulary: every leaf has a standalone validator, so a flat
   record or array of them compiles to one C entrypoint. The sampler chooses
   from [sampler_adversarial_leaves] three times more often than from
   [sampler_regular_leaves]; bugs have clustered around these odd shapes. *)
let sampler_regular_leaves : any list =
  [
    Any { g = uint8; size = Some 1; label = "u8" };
    Any { g = uint16; size = Some 2; label = "u16" };
    Any { g = uint16be; size = Some 2; label = "u16be" };
    Any { g = uint32; size = Some 4; label = "u32" };
    Any { g = uint32be; size = Some 4; label = "u32be" };
    Any { g = uint64; size = Some 8; label = "u64" };
    Any { g = uint64be; size = Some 8; label = "u64be" };
    Any { g = int8; size = Some 1; label = "i8" };
    Any { g = int16; size = Some 2; label = "i16" };
    Any { g = int16be; size = Some 2; label = "i16be" };
    Any { g = int32; size = Some 4; label = "i32" };
    Any { g = int32be; size = Some 4; label = "i32be" };
    Any { g = int64; size = Some 8; label = "i64" };
    Any { g = int64be; size = Some 8; label = "i64be" };
    Any { g = float32be; size = Some 4; label = "f32be" };
    Any { g = float64be; size = Some 8; label = "f64be" };
  ]

let sampler_weird_leaves : any list =
  [
    Any { g = bits ~width:3 Wire.U8; size = Some 1; label = "bits3" };
    Any
      {
        g = bits ~bit_order:Wire.Lsb_first ~width:3 Wire.U8;
        size = Some 1;
        label = "bits3lsb";
      };
    Any { g = bits ~width:15 Wire.U16; size = Some 2; label = "bits15u16" };
    Any
      {
        g = bits ~bit_order:Wire.Lsb_first ~width:15 Wire.U16be;
        size = Some 2;
        label = "bits15u16belsb";
      };
    Any { g = bitpack_u8_msb; size = Some 1; label = "bp8m" };
    Any { g = bitpack_u8_lsb; size = Some 1; label = "bp8l" };
    Any { g = bitpack_u16be_lsb; size = Some 2; label = "bp16bel" };
    Any { g = bitpack_u32_msb; size = Some 4; label = "bp32m" };
    Any { g = bitpack_u32be_lsb; size = Some 4; label = "bp32bel" };
    Any { g = enum_open; size = Some 1; label = "enum_open" };
    Any { g = bounded_u8 ~min:10 ~max:100; size = Some 1; label = "bnd" };
    Any
      {
        g = enum "E" [ ("A", 1); ("B", 2); ("C", 3) ];
        size = Some 1;
        label = "enum";
      };
    Any
      { g = lookup [ 'a'; 'b'; 'c'; 'd' ] uint8; size = Some 1; label = "lkp" };
    (* Flat records on the fast paths: checks folded into intervals and a byte
       table, a general check step, and a staged encode wider than a pair. *)
    flat_check_order;
    flat_table;
    flat_fixed_steps;
    wide 6;
  ]

(* Refined and byte-span shapes. Every one is fixed-size and parameter-free, so
   the differential's scope filter takes them unchanged -- and they are where
   the refinements live: a per-byte span predicate, a range bound, a cross-field
   constraint, a variable-width integer. A decoder that drops one of those
   accepts bytes the generated C rejects, which is exactly what the differential
   is for. *)
let sampler_refined_leaves : any list =
  [
    Any { g = byte_array 3; size = Some 3; label = "ba3" };
    Any
      {
        g = byte_array_where 3 ~per_byte:printable_byte;
        size = Some 3;
        label = "baw3";
      };
    Any { g = uint_var ~endian:Wire.Big 3; size = Some 3; label = "uv3" };
    Any { g = uint_var ~endian:Wire.Little 3; size = Some 3; label = "uv3le" };
    Any { g = bounded_u16be; size = Some 2; label = "bnd16" };
    Any { g = bounded_u32be; size = Some 4; label = "bnd32" };
    Any { g = typ_where; size = Some 2; label = "twhere" };
    Any { g = field_constraint; size = Some 2; label = "fconstraint" };
  ]

(* Enum names 3D declares beside the module's types. Keywords, escaped and
   accepted by every lane; two cases that escape onto each other, and a
   case [enum] (and [enum_open]) also declares, both refused by the
   EverParse lane alone, the second only where the two meet in one record.
   The OCaml lane reads enums by value and compares all three. *)
let sampler_enum_name_leaves : any list =
  [
    Any
      {
        g = enum "end" [ ("in", 1); ("typeof", 2); ("int", 3) ];
        size = Some 1;
        label = "enum_kw";
      };
    Any
      {
        g = enum "Esc" [ ("end", 1); ("end_", 2) ];
        size = Some 1;
        label = "enum_esc";
      };
    Any
      {
        g = enum "E2" [ ("A", 4); ("D", 5) ];
        size = Some 1;
        label = "enum_dup";
      };
  ]

let sampler_adversarial_leaves =
  sampler_weird_leaves @ sampler_enum_name_leaves @ sampler_refined_leaves

let sampler_leaves = sampler_regular_leaves @ sampler_adversarial_leaves

(* Geometric arity in [1, max_arity]: keep adding a field with probability
   [continue]. This is the Boltzmann law for a sequence, truncated. *)
let sample_arity rng ~continue ~max_arity =
  let rec go k =
    if k >= max_arity || Random.State.float rng 1.0 >= continue then k
    else go (k + 1)
  in
  go 1

(* The shape of one sampled codec as a tree, paired with the leaf-vocabulary
   labels it draws (in draw order). An array element and a record field are each
   a shape in their own right, so an array can sit next to other fields, and a
   record or an array can sit under either. Exposed alongside each sample so
   coverage metrics read the structure directly rather than parsing the debug
   label. *)
type sample_shape =
  | Leaf
  | Array of sample_shape  (** element shape *)
  | Record of sample_shape list  (** field shapes, in declaration order *)

type sample_meta = { shape : sample_shape; leaves : string list }

let rec equal_shape a b =
  match (a, b) with
  | Leaf, Leaf -> true
  | Array x, Array y -> equal_shape x y
  | Record xs, Record ys -> List.equal equal_shape xs ys
  | (Leaf | Array _ | Record _), _ -> false

let rec shape_depth = function
  | Leaf -> 1
  | Array s -> 1 + shape_depth s
  | Record fs -> 1 + List.fold_left (fun d s -> max d (shape_depth s)) 0 fs

let rec shape_has_array = function
  | Leaf -> false
  | Array _ -> true
  | Record fs -> List.exists shape_has_array fs

(* An array that shares its record with at least one other field. This is the
   shape where a mis-sized element does the most damage: it shifts the offset of
   every field after it, which a record whose only field is the array cannot
   show. *)
let rec shape_has_array_beside_fields = function
  | Leaf -> false
  | Array s -> shape_has_array_beside_fields s
  | Record fs ->
      List.compare_length_with fs 1 > 0
      && List.exists (function Array _ -> true | Leaf | Record _ -> false) fs
      || List.exists shape_has_array_beside_fields fs

(* An array whose element repeats again inside itself. Wire rejects an array
   directly under an array (3D projects an array as a count of fixed-size
   elements, and a list is not one), so the reachable form is an array of a
   record that holds an array. *)
let rec shape_has_nested_repetition = function
  | Leaf -> false
  | Array s -> shape_has_array s || shape_has_nested_repetition s
  | Record fs -> List.exists shape_has_nested_repetition fs

(* How deep a sample may nest. The branching is subcritical on its own (a node
   composes with probability 1/2 and then has 1.875 children on average), so the
   budget only caps the tail; 3 is the floor that reaches an array of a record
   holding an array, which needs a record between the two arrays. *)
let sample_budget = 3
let sampler_regular_pool = Array.of_list sampler_regular_leaves
let sampler_adversarial_pool = Array.of_list sampler_adversarial_leaves

(* One leaf, adversarially biased 3:1, appended to [drawn] so the caller ends up
   with the vocabulary the sample actually holds. *)
let sample_leaf rng drawn =
  let pool =
    if Random.State.int rng 4 = 0 then sampler_regular_pool
    else sampler_adversarial_pool
  in
  let a = pool.(Random.State.int rng (Array.length pool)) in
  let (Any l) = a in
  drawn := l.label :: !drawn;
  a

(* One node of a sampled shape. Each draws its own arity: 1 stops at a leaf,
   otherwise the node is an array of one shape or a record of that many shapes,
   and the recursion repeats until [budget] runs out. The set of legal shapes is
   unchanged -- the same leaf vocabulary and the same three combinators, every
   intermediate node fixed-size.

   A composition Wire refuses to build (an array of arrays, an array of a
   bitfield) raises [Invalid_argument] or yields [None]; that node drops back to
   a bare leaf, and the leaves the abandoned subtree recorded are rolled back so
   the caller's list holds exactly what the returned codec does. [uid] names the
   sub-codecs apart, since a projection keys its typedefs by name. *)
(* Field names a code generator can trip on, in groups. Every generator
   escapes a keyword of its target with a trailing underscore and folds case
   where its target does, and refuses, naming both fields, two names that still
   spell one identifier (see [Wire.Private.Names]); which lane refuses a group
   is noted against it. The EverParse lane drops what it refuses with
   [Diff_codecs.Name_clash]; the OCaml lane counts a name clash as an expected
   refusal. *)
let sampler_name_groups =
  [|
    (* A name and the [_int] getter of a 32-bit field or the [_word] getter of
       a bitfield: refused by the OCaml lane alone. *)
    [ "x"; "x_int" ];
    [ "x_int"; "x" ];
    [ "x"; "x_word" ];
    [ "x_word"; "x" ];
    (* Keywords of OCaml and OxCaml, escaped by both lanes, and a name whose
       getter [get_failed] the OCaml lane's module defines for itself, refused
       by that lane alone. *)
    [ "done" ];
    [ "method"; "local_"; "exclave_"; "stack_" ];
    [ "failed" ];
    (* Keywords of 3D, F* and C, escaped apart: accepted by both lanes. *)
    [ "end"; "typeof"; "int"; "string" ];
    (* Two names one case fold spells alike: refused by both lanes (the plug's
       [_IDX_] macro, the stubs' and the OCaml lane's label). *)
    [ "X"; "x" ];
    (* A keyword next to its escape. [done]: refused by both lanes (the stubs'
       and the OCaml lane's label), not by 3D. [end]: refused by both (the 3D
       name and the labels). [restrict]: refused by the EverParse lane alone
       (the plug's struct member); the OCaml lane compares it. *)
    [ "done"; "done_" ];
    [ "end"; "end_" ];
    [ "restrict"; "restrict_" ];
    (* The name 3D gives the padding of a bit group reversed into its base's
       native order ([bits3] needs one): the padding takes the next free name,
       so every lane accepts it. *)
    [ "_anon_0"; "_anon_1" ];
  |]

(* The names of a record's [k] fields: half the time an adversarial group in
   front, the rest left to [Codec.field_name]. Drawn from their own stream, so
   the shapes a seed draws do not depend on them. *)
let sample_names names k =
  if Random.State.bool names then
    let g =
      sampler_name_groups.(Random.State.int names
                             (Array.length sampler_name_groups))
    in
    List.filteri (fun i _ -> i < k) g
  else []

let sample_node rng ~names ~drawn ~uid budget =
  let bare () = (sample_leaf rng drawn, Leaf) in
  let compose f =
    let saved = !drawn in
    let fallback () =
      drawn := saved;
      bare ()
    in
    match f () with
    | Some r -> r
    | None -> fallback ()
    | exception Invalid_argument _ -> fallback ()
  in
  (* A name with an upper-case letter past the first cannot collide with the
     entrypoint name [sampled] derives from the label, which [camel_of_label]
     lower-cases. *)
  let fresh k =
    incr uid;
    Fmt.str "R%dN%d" k !uid
  in
  let rec draw budget =
    let k = sample_arity rng ~continue:0.5 ~max_arity:4 in
    if budget <= 0 || k = 1 then bare ()
    else if Random.State.bool rng then draw_array budget
    else draw_record budget k
  and draw_array budget =
    let n = 1 + Random.State.int rng 6 in
    compose (fun () ->
        let element, inner = draw (budget - 1) in
        Option.map (fun a -> (a, Array inner)) (array_of n element))
  and draw_record budget k =
    compose (fun () ->
        let fields = draw_fields (budget - 1) k in
        let name = fresh k and names = sample_names names k in
        let shape = Record (List.map snd fields) in
        match List.map fst fields with
        | [ a; b ] -> Some (pair_of ~names name a b, shape)
        | [ a; b; c ] -> Some (rec3_of ~names name a b c, shape)
        | [ a; b; c; d ] -> Some (rec4_of ~names name a b c d, shape)
        | _ -> None)
  (* Explicit recursion rather than [List.init], so the fields are drawn left to
     right and the draw order is fixed by this code alone. *)
  and draw_fields budget k =
    if k <= 0 then []
    else
      let field = draw budget in
      field :: draw_fields budget (k - 1)
  in
  draw budget

(* One Boltzmann-distributed projectable codec, with the structural metadata
   needed for coverage metrics. The draw sequence is a pure function of [rng],
   so a seed reproduces the exact same set: the differential's generator and its
   runner rely on it to agree on the shapes. *)
let boltzmann_any rng ~names : any * sample_meta =
  let drawn = ref [] in
  let uid = ref 0 in
  let result, shape = sample_node rng ~names ~drawn ~uid sample_budget in
  (result, { shape; leaves = List.rev !drawn })

(* A nested shape's label nests too, so it grows with the tree. The label names
   the codec, and the codec name becomes a C identifier and a file name in the
   differential's generated schema directory, so cap the shape part: the
   [smp<i>_] prefix is what makes the name unique, and the rest is for reading. *)
let sample_label i shape =
  let max_shape = 64 in
  let shape =
    if String.length shape <= max_shape then shape
    else String.sub shape 0 max_shape
  in
  Fmt.str "smp%d_%s" i shape

let sampled ~seed ~count : (string * packed * sample_meta) list =
  let rng = Random.State.make [| seed |] in
  let names = Random.State.make [| seed; 1 |] in
  List.init count (fun i ->
      let Any a, meta = boltzmann_any rng ~names in
      let label = sample_label i a.label in
      (label, rename_case label (Pack a.g), meta))

let sample ~seed ~count : (string * packed) list =
  List.map (fun (label, p, _) -> (label, p)) (sampled ~seed ~count)

let flat_records ~seed ~count : (string * packed) list =
  List.filter_map
    (fun (label, p, meta) ->
      match meta.shape with
      | Record fields when List.for_all (equal_shape Leaf) fields ->
          Some (label, p)
      | Leaf | Array _ | Record _ -> None)
    (sampled ~seed ~count)
