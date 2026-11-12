open Gen
open Leaves
open Wrappers
open Record

(* {1 Recursive nested composition}

   [gen_any] generates an arbitrary nested codec by picking a combinator and
   recursively filling its inners, so the fuzzer exercises compositions no
   curated list enumerates (optional of a record of a repeat of a casetype,
   ...). Each node carries its byte [size] ([Some n] when fixed, [None] when
   variable) so the generator only builds compositions Wire actually accepts:
   [array] and [nested] take a fixed inner, everything else takes any inner.
   A trailing-consume leaf (all_bytes / all_zeros) is deliberately excluded
   since it only composes as the last field of a record. *)

type any = Any : { g : 'a t; size : int option; label : string } -> any

let fixed_size g = Wire.Codec.wire_size_opt g.codec

(* Fixed-width leaves: usable as [array] / [nested] elements. *)
let printable_byte b = Wire.Expr.(b >= Wire.int 0x20 && b <= Wire.int 0x7e)

(* Fixed-size leaves usable as any nested element or field. The set is broad on
   purpose: scalars of every width/endianness, packed bits, byte spans, an enum
   / variants / lookup / bounded / refined-byte / float-predicate leaf, so a
   composition can carry a constraint or mapping at the bottom. *)
let fixed_scalar_leaves : any list =
  [
    Any { g = uint8; size = Some 1; label = "u8" };
    Any { g = uint16; size = Some 2; label = "u16le" };
    Any { g = uint16be; size = Some 2; label = "u16" };
    Any { g = uint32; size = Some 4; label = "u32le" };
    Any { g = uint32be; size = Some 4; label = "u32" };
    Any { g = uint64; size = Some 8; label = "u64le" };
    Any { g = uint64be; size = Some 8; label = "u64" };
    Any { g = int8; size = Some 1; label = "i8" };
    Any { g = int16; size = Some 2; label = "i16le" };
    Any { g = int16be; size = Some 2; label = "i16" };
    Any { g = int32; size = Some 4; label = "i32le" };
    Any { g = int32be; size = Some 4; label = "i32" };
    Any { g = int64; size = Some 8; label = "i64le" };
    Any { g = int64be; size = Some 8; label = "i64" };
    Any { g = float32be; size = Some 4; label = "f32" };
    Any { g = float64be; size = Some 8; label = "f64" };
    Any { g = uint_var ~endian:Wire.Little 3; size = Some 3; label = "uv3le" };
    Any { g = uint_var ~endian:Wire.Big 3; size = Some 3; label = "uv3" };
    Any { g = empty; size = Some 0; label = "unit" };
  ]

let fixed_bytes_leaves : any list =
  [
    Any { g = byte_array 3; size = Some 3; label = "ba3" };
    Any { g = byte_slice 4; size = Some 4; label = "bs4" };
    Any { g = byte_array 0; size = Some 0; label = "ba0" };
    Any { g = byte_slice 0; size = Some 0; label = "bs0" };
    Any
      {
        g = byte_array_where 3 ~per_byte:printable_byte;
        size = Some 3;
        label = "baw3";
      };
    Any { g = bits ~width:3 Wire.U8; size = Some 1; label = "bits3" };
    Any
      {
        g = bits ~bit_order:Wire.Lsb_first ~width:3 Wire.U8;
        size = Some 1;
        label = "bits3lsb";
      };
    Any { g = bit uint8; size = Some 1; label = "bit" };
  ]

let fixed_bitpack_leaves : any list =
  [
    Any { g = bitpack_u8_msb; size = Some 1; label = "bp8m" };
    Any { g = bitpack_u8_lsb; size = Some 1; label = "bp8l" };
    Any { g = bitpack_u8_spill; size = Some 2; label = "bp8spill" };
    Any { g = bitpack_u16_lsb; size = Some 2; label = "bp16l" };
    Any { g = bitpack_u16be_msb; size = Some 2; label = "bp16bem" };
    Any { g = bitpack_u16be_lsb; size = Some 2; label = "bp16bel" };
    Any { g = bitpack_u32_msb; size = Some 4; label = "bp32m" };
    Any { g = bitpack_u32be_lsb; size = Some 4; label = "bp32bel" };
  ]

let fixed_enum_leaves : any list =
  [
    Any
      {
        g = enum "E" [ ("A", 1); ("B", 2); ("C", 3) ];
        size = Some 1;
        label = "enum";
      };
    Any { g = enum_open; size = Some 1; label = "enum_open" };
    Any { g = enum_u16be; size = Some 2; label = "enum16" };
    Any { g = enum_open_u16be; size = Some 2; label = "enumopen16" };
    Any
      {
        g = variants "V" [ ("X", `X); ("Y", `Y); ("Z", `Z) ];
        size = Some 1;
        label = "var";
      };
    Any { g = variants_u16be; size = Some 2; label = "var16" };
    Any
      { g = lookup [ 'a'; 'b'; 'c'; 'd' ] uint8; size = Some 1; label = "lkp" };
  ]

let fixed_constraint_leaves : any list =
  [
    Any { g = bounded_u8 ~min:10 ~max:100; size = Some 1; label = "bnd" };
    Any { g = field_constraint; size = Some 2; label = "fconstraint" };
    Any { g = field_int; size = Some 2; label = "fint" };
    Any { g = self_int64; size = Some 8; label = "self64" };
    Any { g = finite_float64; size = Some 8; label = "finf" };
    Any { g = nan_float64; size = Some 8; label = "nanf" };
    (* A sub-codec leaf with a [~where] over a projectable expression, so
       composing it exercises an embedded constrained sub-codec. ([expr_ops] and
       [sizeof] are deliberately not nested here: they use a negative literal /
       [field_pos], which have no 3D projection, so a composition embedding them
       would not project. They are still exercised standalone via the registry,
       where they are asserted to be rejected at projection.) *)
    Any { g = codec_where; size = Some 2; label = "cwhere" };
    Any { g = typ_where; size = Some 2; label = "twhere" };
  ]

let fixed_leaves : any list =
  fixed_scalar_leaves @ fixed_bytes_leaves @ fixed_enum_leaves
  @ fixed_bitpack_leaves @ fixed_constraint_leaves

(* Self-delimiting / trailing variable leaves. *)
let var_leaves : any list =
  [
    Any { g = zeroterm; size = None; label = "zt" };
    Any { g = zeroterm_at_most 1; size = None; label = "zt1" };
    Any { g = zeroterm_at_most 6; size = None; label = "zt6" };
    Any { g = all_zeros; size = None; label = "az" };
    (* Dynamic-gate optionals: a gate byte then a present/absent (optional) or
       value-driven (optional_or) payload. *)
    Any { g = optional_dynamic; size = None; label = "optdyn" };
    Any { g = optional_or_dynamic; size = None; label = "optordyn" };
    Any { g = optional_dyn_after_span; size = None; label = "optdynspan" };
    Any { g = union_after_span; size = None; label = "unionspan" };
    Any { g = if_then_else; size = None; label = "ite" };
  ]

(* Param/env-bearing leaves: sub-codecs whose [~where] / [action] reads a
   [Param.input]. Wrapping one carries its env up (see [with_env]); decoding
   the wrapper threads the env down to the embedded sub-codec. Composing these
   exposes any wrapper that drops the env on the way down. *)
let env_leaves : any list =
  [
    Any { g = param_input; size = Some 1; label = "param" };
    Any { g = param_size; size = None; label = "param_size" };
    Any { g = rest_bytes; size = None; label = "rest" };
    Any { g = action; size = Some 1; label = "act" };
    Any { g = action_on_act; size = Some 1; label = "onact" };
  ]

let nested_of (Any a) =
  match a.size with
  | Some s ->
      Some (Any { g = nested s a.g; size = Some s; label = "nest:" ^ a.label })
  | None -> None

(* Both array wrappers need a fixed-size element: [size] is [Some] iff the
   inner is fixed, and the array's size is [k] copies of it. [mk] is wrapped in
   a record to keep it polymorphic across the [Any] existential. *)
type arr_mk = { mk : 'x. int -> 'x t -> 'x list t }

let array_like prefix { mk } k (Any a) =
  match a.size with
  | Some s ->
      Some
        (Any
           {
             g = mk k a.g;
             size = Some (k * s);
             label = Fmt.str "%s%d:%s" prefix k a.label;
           })
  | None -> None

let array_of = array_like "arr" { mk = array }
let array_seq_of = array_like "arrs" { mk = array_seq }

let optional_of (Any a) =
  Any { g = optional a.g; size = None; label = "opt:" ^ a.label }

let repeat_of (Any a) =
  let label = "rep:" ^ a.label in
  Any
    {
      g = repeat ~name:("_rep" ^ shape_tag label) ~bytes:12 a.g;
      size = None;
      label;
    }

let repeat_seq_of (Any a) =
  let label = "reps:" ^ a.label in
  Any
    {
      g = repeat_seq ~name:("_rep_seq" ^ shape_tag label) ~bytes:12 a.g;
      size = None;
      label;
    }

let nested_at_most_of (Any a) =
  match a.size with
  | Some s ->
      Some
        (Any
           {
             g = nested_at_most (s + 2) a.g;
             size = None;
             label = "natm:" ^ a.label;
           })
  | None -> None

let map_of (Any a) =
  Any
    {
      g = map ~decode:Fun.id ~encode:Fun.id a.g;
      size = a.size;
      label = "map:" ^ a.label;
    }

let where_of (Any a) =
  Any { g = where a.g; size = a.size; label = "wh:" ^ a.label }

(* [name] is the sub-codec's 3D struct name. It is a parameter because a
   projection keys its typedefs by name: two records that share a name collapse
   onto whichever body projects first, so a composition holding more than one of
   them has to name each apart. *)
let pair_of name (Any a) (Any b) =
  let g =
    Codec.v name
      ~equal:(fun (x1, y1) (x2, y2) -> a.g.equal x1 x2 && b.g.equal y1 y2)
      (fun x y -> (x, y))
      Codec.[ a.g $ fst; b.g $ snd ]
  in
  Any
    {
      g;
      (* Ask the compiled codec: adjacent bitfields can coalesce, so summing
         their standalone widths overstates the record's actual wire size. *)
      size = fixed_size g;
      label = "(" ^ a.label ^ "," ^ b.label ^ ")";
    }

(* Records of arity 3 and 4: like [pair_of] but for more fields, so the
   Boltzmann sampler can emit records wider than a pair. Each unpacks its [Any]
   elements and seals one [Codec.v]. *)
let rec3_of name (Any a) (Any b) (Any c) =
  let g =
    Codec.v name
      ~equal:(fun (x1, y1, z1) (x2, y2, z2) ->
        a.g.equal x1 x2 && b.g.equal y1 y2 && c.g.equal z1 z2)
      (fun x y z -> (x, y, z))
      Codec.
        [
          (a.g $ fun (x, _, _) -> x);
          (b.g $ fun (_, y, _) -> y);
          (c.g $ fun (_, _, z) -> z);
        ]
  in
  Any
    {
      g;
      size = fixed_size g;
      label = Fmt.str "(%s,%s,%s)" a.label b.label c.label;
    }

let rec4_of name (Any a) (Any b) (Any c) (Any d) =
  let g =
    Codec.v name
      ~equal:(fun (x1, y1, z1, w1) (x2, y2, z2, w2) ->
        a.g.equal x1 x2 && b.g.equal y1 y2 && c.g.equal z1 z2 && d.g.equal w1 w2)
      (fun x y z w -> (x, y, z, w))
      Codec.
        [
          (a.g $ fun (x, _, _, _) -> x);
          (b.g $ fun (_, y, _, _) -> y);
          (c.g $ fun (_, _, z, _) -> z);
          (d.g $ fun (_, _, _, w) -> w);
        ]
  in
  Any
    {
      g;
      size = fixed_size g;
      label = Fmt.str "(%s,%s,%s,%s)" a.label b.label c.label d.label;
    }

let casetype_of (Any a) (Any b) =
  let g =
    casetype_u8 "CT2"
      [
        case ~index:1 a.g
          ~inject:(fun v -> `A v)
          ~project:(function `A v -> Some v | _ -> None);
        default_case ~tag:2 b.g
          ~inject:(fun v -> `B v)
          ~project:(function `B v -> Some v | _ -> None);
      ]
  in
  Any { g; size = None; label = "ct:" ^ a.label ^ "|" ^ b.label }

(* A wrapped codec inherits a [Param.input]-binding env from the inner it
   wraps, so a param-dependent leaf still decodes once nested (and exposes any
   gap where the env is not threaded down to it). *)
let env_of (Any a) = a.g.env

let with_env e (Any r) =
  match (r.g.env, e) with
  | None, Some _ -> Any { r with g = { r.g with env = e } }
  | _ -> Any r

(* Wrapping a composition that wire refuses to build (a bitfield as an array /
   nested / repeat element, an unprojectable repeat element, ...) raises
   [Invalid_argument] at construction. That is wire's projection ceiling, not a
   bug, so the composer falls back to the bare inner rather than crashing the
   generator. Everything that does build is still round-tripped and
   crash-tested. *)
let bind1 inner_gen f =
  Alcobar.dynamic_bind inner_gen (fun a ->
      match with_env (env_of a) (f a) with
      | x -> Alcobar.const x
      | exception Invalid_argument _ -> Alcobar.const a)

let bind1_opt inner_gen f =
  Alcobar.dynamic_bind inner_gen (fun a ->
      match f a with
      | Some x -> Alcobar.const (with_env (env_of a) x)
      | None -> Alcobar.const a
      | exception Invalid_argument _ -> Alcobar.const a)

let bind2 g1 g2 f =
  Alcobar.dynamic_bind g1 (fun a ->
      Alcobar.dynamic_bind g2 (fun b ->
          let e = match env_of a with Some _ as e -> e | None -> env_of b in
          match with_env e (f a b) with
          | x -> Alcobar.const x
          | exception Invalid_argument _ -> Alcobar.const a))

let bind3 g1 g2 g3 f =
  Alcobar.dynamic_bind g1 (fun a ->
      Alcobar.dynamic_bind g2 (fun b ->
          Alcobar.dynamic_bind g3 (fun c ->
              let e =
                match env_of a with
                | Some _ as e -> e
                | None -> (
                    match env_of b with Some _ as e -> e | None -> env_of c)
              in
              match with_env e (f a b c) with
              | x -> Alcobar.const x
              | exception Invalid_argument _ -> Alcobar.const a)))

let bind4 g1 g2 g3 g4 f =
  Alcobar.dynamic_bind g1 (fun a ->
      Alcobar.dynamic_bind g2 (fun b ->
          Alcobar.dynamic_bind g3 (fun c ->
              Alcobar.dynamic_bind g4 (fun d ->
                  let e =
                    match env_of a with
                    | Some _ as e -> e
                    | None -> (
                        match env_of b with
                        | Some _ as e -> e
                        | None -> (
                            match env_of c with
                            | Some _ as e -> e
                            | None -> env_of d))
                  in
                  match with_env e (f a b c d) with
                  | x -> Alcobar.const x
                  | exception Invalid_argument _ -> Alcobar.const a))))

(* [fixed_level] yields fixed-size nodes (for [array] / [nested] inners, which
   need a known element width); [any_level] yields any node. Both compose the
   full combinator set so the fuzzer reaches every nesting -- the point is to
   surface compositions Wire mishandles, not to pre-filter them. *)
(* The recursive samplers can build structurally different records at the same
   depth, so a record takes its name from its own shape rather than from its
   arity alone. *)
let pair_uniq (Any a as x) (Any b as y) =
  pair_of ("R2" ^ shape_tag (a.label ^ "," ^ b.label)) x y

let rec3_uniq (Any a as x) (Any b as y) (Any c as z) =
  rec3_of ("R3" ^ shape_tag (a.label ^ "," ^ b.label ^ "," ^ c.label)) x y z

let rec4_uniq (Any a as x) (Any b as y) (Any c as z) (Any d as w) =
  rec4_of
    ("R4" ^ shape_tag (a.label ^ "," ^ b.label ^ "," ^ c.label ^ "," ^ d.label))
    x y z w

let fixed_leaf_gens = List.map Alcobar.const fixed_leaves

let any_leaf_gens =
  List.map Alcobar.const (fixed_leaves @ var_leaves @ env_leaves)

(* One level over the fixed-size generator [fixed] for the level below. *)
let fixed_level fixed =
  Alcobar.choose
    (fixed_leaf_gens
    @ [
        bind1_opt fixed nested_of;
        bind1_opt fixed nested_at_most_of;
        bind1_opt fixed (array_of 2);
        bind1_opt fixed (array_seq_of 2);
        bind1 fixed map_of;
        bind1 fixed where_of;
        bind2 fixed fixed pair_uniq;
        bind3 fixed fixed fixed rec3_uniq;
        bind4 fixed fixed fixed fixed rec4_uniq;
      ])

(* One level over the generators [any] and [fixed] for the level below. *)
let any_level ~any ~fixed =
  Alcobar.choose
    (any_leaf_gens
    @ [
        bind1 any optional_of;
        bind1 any repeat_of;
        bind1 any repeat_seq_of;
        bind1 any map_of;
        bind1 any where_of;
        bind1_opt fixed nested_of;
        bind1_opt fixed nested_at_most_of;
        bind1_opt fixed (array_of 2);
        bind1_opt fixed (array_seq_of 2);
        bind2 any any pair_uniq;
        bind3 any any any rec3_uniq;
        bind4 any any any any rec4_uniq;
        bind2 any any casetype_of;
      ])

(* The fixed-size and the any generator at [depth], each level built once over
   the two below it and shared by every combinator that draws an inner from
   them. A generator is a description and a draw reads fresh choices at every
   position it occupies, so sharing changes nothing that is sampled, and the
   description stays linear in [depth]; a fuzz executable holds it for its
   whole run. *)
let rec levels depth =
  if depth <= 0 then
    (Alcobar.choose fixed_leaf_gens, Alcobar.choose any_leaf_gens)
  else
    let fixed, any = levels (depth - 1) in
    (fixed_level fixed, any_level ~any ~fixed)

let gen_any depth : any Alcobar.gen = snd (levels depth)
