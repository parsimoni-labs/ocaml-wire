(* Tests for everparse.ml: the documentation projection. *)

open Wire
open Wire.Everparse.Raw
open Test_helpers

let test_doc_drops_ffi_scaffolding () =
  (* The doc projection keeps the structure but not the FFI callbacks the
     codegen schema carries. *)
  let c =
    Codec.v "Pkt" (fun v -> v) Codec.[ (Field.v "v" uint8 $ fun v -> v) ]
  in
  let schema = to_3d (Everparse.project ~mode:`Ffi c).module_ in
  let doc =
    to_3d ~enum_as_type:true (Everparse.project ~mode:`Standalone c).module_
  in
  Alcotest.(check bool)
    "schema has WireCtx" true
    (contains ~sub:"WireCtx" schema);
  Alcotest.(check bool) "doc has no WireCtx" false (contains ~sub:"WireCtx" doc);
  Alcotest.(check bool) "doc has no WireSet" false (contains ~sub:"WireSet" doc);
  Alcotest.(check bool) "doc keeps the field" true (contains ~sub:"UINT8 v" doc)

let test_doc_enum_as_type () =
  (* An enum field renders as its named 3D enum type, so EverParse enforces
     membership through the type rather than an explicit refinement. *)
  let e = enum "Color" [ ("Red", 0); ("Green", 1); ("Blue", 2) ] uint8 in
  let c = Codec.v "Pix" (fun v -> v) Codec.[ (Field.v "Hue" e $ fun v -> v) ] in
  let doc =
    to_3d ~enum_as_type:true (Everparse.project ~mode:`Standalone c).module_
  in
  Alcotest.(check bool)
    "declares the enum" true
    (contains ~sub:"enum Color" doc);
  Alcotest.(check bool)
    "types the field as the enum" true
    (contains ~sub:"Color Hue" doc);
  Alcotest.(check bool)
    "drops the membership refinement" false (contains ~sub:"== 0" doc);
  Alcotest.(check bool)
    "codegen schema keeps membership" true
    (contains ~sub:"== 0" (to_3d (Everparse.project ~mode:`Ffi c).module_))

let test_doc_codec_citation () =
  (* [Codec.v ~doc] renders as a [/*++ ... --*/] comment on the typedef, so a
     spec can cite the RFC (or other source) it comes from. *)
  let c =
    Codec.v "Pkt" ~doc:"RFC 9999 section 1"
      (fun v -> v)
      Codec.[ (Field.v "v" uint8 $ fun v -> v) ]
  in
  Alcotest.(check (option string))
    "codec exposes its doc" (Some "RFC 9999 section 1") (Codec.doc c);
  let out =
    to_3d ~enum_as_type:true (Everparse.project ~mode:`Standalone c).module_
  in
  Alcotest.(check bool)
    "typedef carries the citation comment" true
    (contains ~sub:"/*++ RFC 9999 section 1 --*/" out)

let test_doc_comment_wraps_at_80 () =
  (* A long [?doc] note must not render as one comment line past 80 columns: the
     generated .3d doubles as readable protocol documentation. The body is
     word-wrapped across several comment lines. *)
  let long =
    "RFC 9999 section 4.2.1 the version field carries the protocol revision \
     and must equal the value negotiated during the handshake or the peer \
     rejects the frame as malformed and resets the connection immediately"
  in
  let c =
    Codec.v "Wrapped"
      (fun v -> v)
      Codec.[ (Field.v "v" uint8 ~doc:long $ fun v -> v) ]
  in
  let out = to_3d (Everparse.project ~mode:`Ffi c).module_ in
  let longest =
    String.split_on_char '\n' out
    |> List.fold_left (fun m l -> max m (String.length l)) 0
  in
  Alcotest.(check bool)
    (Fmt.str "no generated line exceeds 80 columns (longest=%d)" longest)
    true (longest <= 80);
  Alcotest.(check bool)
    "the long doc is still a comment" true
    (contains ~sub:"/* RFC 9999 section 4.2.1" out)

let test_doc_merge_dedup () =
  (* write_doc unions a family into one module, emitting a shared type once. *)
  let e = enum "Shared" [ ("A", 0); ("B", 1) ] uint8 in
  let c1 = Codec.v "One" (fun v -> v) Codec.[ (Field.v "x" e $ fun v -> v) ] in
  let c2 = Codec.v "Two" (fun v -> v) Codec.[ (Field.v "y" e $ fun v -> v) ] in
  let dir = Filename.get_temp_dir_name () in
  let name = "wire_doc_merge_test" in
  Everparse.write ~mode:`Standalone ~outdir:dir ~name
    [
      Everparse.project ~mode:`Standalone c1;
      Everparse.project ~mode:`Standalone c2;
    ];
  let path = Filename.concat dir (String.capitalize_ascii name ^ ".3d") in
  let content = In_channel.with_open_text path In_channel.input_all in
  Sys.remove path;
  let count sub = List.length (Re.all (Re.compile (Re.str sub)) content) in
  Alcotest.(check int) "shared enum declared once" 1 (count "enum Shared");
  Alcotest.(check bool)
    "first struct present" true
    (contains ~sub:"One" content);
  Alcotest.(check bool)
    "second struct present" true
    (contains ~sub:"Two" content)

let test_doc_merge_name_collision () =
  (* Two codecs declaring different types under one name cannot both be honoured
     by a single merged spec. Emitting the first and dropping the second would
     hand codec [Two] a C validator enforcing codec [One]'s enum, the exact
     inverse of what [Two] accepts in OCaml, so the merge is rejected. *)
  let one = enum "Shared" [ ("A", 0); ("B", 1) ] uint8 in
  let two = enum "Shared" [ ("X", 7); ("Y", 9) ] uint8 in
  let codec name e =
    Codec.v name (fun v -> v) Codec.[ (Field.v "x" e $ fun v -> v) ]
  in
  let dir = Filename.get_temp_dir_name () in
  let write name cs =
    Everparse.write ~mode:`Standalone ~outdir:dir ~name
      (List.map (Everparse.project ~mode:`Standalone) cs)
  in
  let msg =
    match
      write "wire_doc_merge_collision" [ codec "One" one; codec "Two" two ]
    with
    | () -> Alcotest.fail "expected a name-collision error"
    | exception Invalid_argument msg -> msg
  in
  Alcotest.(check bool)
    "names both schemas" true
    (contains ~sub:{|"One"|} msg && contains ~sub:{|"Two"|} msg);
  Alcotest.(check bool)
    "names the colliding declaration" true
    (contains ~sub:{|"Shared"|} msg);
  (* Two codecs building their own copy of the same declaration is the normal
     case for a protocol family and still collapses to one emitted type. *)
  let copy = enum "Shared" [ ("A", 0); ("B", 1) ] uint8 in
  let name = "wire_doc_merge_equal_copies" in
  write name [ codec "One" one; codec "Two" copy ];
  let path = Filename.concat dir (String.capitalize_ascii name ^ ".3d") in
  let content = In_channel.with_open_text path In_channel.input_all in
  Sys.remove path;
  Alcotest.(check int)
    "equal copies declared once" 1
    (List.length (Re.all (Re.compile (Re.str "enum Shared")) content))

(* A sub-codec packed as a codec of its own and also reached through another
   codec's field projects twice: once carrying [entrypoint] and the codec's doc,
   once carrying neither. Both describe the same type, so the merge must collapse
   them instead of reading the markers as a conflicting redeclaration -- and the
   surviving copy must keep the entrypoint whichever schema came first, or the
   shared type silently loses its validator. *)
let test_doc_merge_shared_sub_codec () =
  let shared =
    Codec.v "Chunk" ~doc:"The shared chunk header."
      (fun v -> v)
      Codec.[ (Field.v "len" uint8 $ fun v -> v) ]
  in
  let outer =
    Codec.v "Frame"
      (fun k c -> (k, c))
      Codec.
        [
          (Field.v "kind" uint8 $ fun (k, _) -> k);
          (Field.v "chunk" (codec shared) $ fun (_, c) -> c);
        ]
  in
  let dir = Filename.get_temp_dir_name () in
  let write name schemas =
    Everparse.write ~mode:`Standalone ~outdir:dir ~name schemas;
    let path = Filename.concat dir (String.capitalize_ascii name ^ ".3d") in
    let content = In_channel.with_open_text path In_channel.input_all in
    Sys.remove path;
    content
  in
  let sub = Everparse.project ~mode:`Standalone shared in
  let parent = Everparse.project ~mode:`Standalone outer in
  let check order content =
    Alcotest.(check int)
      (order ^ ": Chunk declared once")
      1
      (List.length (Re.all (Re.compile (Re.str "} Chunk;")) content));
    Alcotest.(check bool)
      (order ^ ": Chunk keeps its entrypoint")
      true
      (contains ~sub:"entrypoint\ntypedef struct WireChunk" content);
    Alcotest.(check bool)
      (order ^ ": Chunk keeps its doc")
      true
      (contains ~sub:"The shared chunk header." content)
  in
  check "sub first" (write "wire_doc_merge_shared_a" [ sub; parent ]);
  check "parent first" (write "wire_doc_merge_shared_b" [ parent; sub ])

let test_doc_field_citation () =
  (* [Field.v ~doc] renders as a plain [/* ... */] comment above the field --
     3d.exe rejects [/*++ --*/] at field position, so the per-field note uses
     the plain comment form. *)
  let f = Field.v "Length" ~doc:"RFC 9999 section 4.2" uint16be in
  Alcotest.(check (option string))
    "field exposes its doc" (Some "RFC 9999 section 4.2") (Field.doc f);
  let c = Codec.v "Pkt" (fun v -> v) Codec.[ (f $ fun v -> v) ] in
  let out =
    to_3d ~enum_as_type:true (Everparse.project ~mode:`Standalone c).module_
  in
  Alcotest.(check bool)
    "field carries the citation comment" true
    (contains ~sub:"/* RFC 9999 section 4.2 */" out)

let test_doc_bit_order_matches_schema () =
  (* Doc-mode emits a validator with no FFI, so its bitfield layout is never
     run through the differential C tests that check the schema (FFI)
     projection's bit order against [Codec.get]. Both projections must apply
     the same bit reordering; assert their bitfield sequence is identical so
     doc-mode's order stays pinned to the order those tests already cover.
     A,B,C share one [U8] word whose native order is [Lsb_first], so the
     default [Msb_first] declaration is reversed in the 3D -- a doc path that
     skipped the reorder would diverge here. *)
  let c =
    Codec.v "BitOrd"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (Field.v "A" (bits ~width:3 U8) $ fun (a, _, _, _) -> a);
          (Field.v "B" (bits ~width:1 U8) $ fun (_, b, _, _) -> b);
          (Field.v "C" (bits ~width:4 U8) $ fun (_, _, c, _) -> c);
          (Field.v "D" uint8 $ fun (_, _, _, d) -> d);
        ]
  in
  let bit_order_of schema =
    match Everparse.entrypoint_struct schema with
    | None -> Alcotest.fail "schema has no entrypoint struct"
    | Some s ->
        List.map
          (fun (name, is_bitfield, _) -> (name, is_bitfield))
          (Everparse.field_action_forms s)
  in
  Alcotest.(check (list (pair (option string) bool)))
    "doc reorders bitfields exactly as the schema projection does"
    (bit_order_of (Everparse.project ~mode:`Ffi c))
    (bit_order_of (Everparse.project ~mode:`Standalone c))

let suite =
  ( "everparse_doc",
    [
      Alcotest.test_case "doc: drops FFI scaffolding" `Quick
        test_doc_drops_ffi_scaffolding;
      Alcotest.test_case "doc: codec ~doc renders as citation comment" `Quick
        test_doc_codec_citation;
      Alcotest.test_case "doc: long doc comment wraps at 80 columns" `Quick
        test_doc_comment_wraps_at_80;
      Alcotest.test_case "doc: enum renders as named type" `Quick
        test_doc_enum_as_type;
      Alcotest.test_case "doc: merge dedups shared types" `Quick
        test_doc_merge_dedup;
      Alcotest.test_case "doc: merge rejects conflicting redeclaration" `Quick
        test_doc_merge_name_collision;
      Alcotest.test_case "doc: merge keeps a shared sub-codec's entrypoint"
        `Quick test_doc_merge_shared_sub_codec;
      Alcotest.test_case "doc: field ~doc renders as citation comment" `Quick
        test_doc_field_citation;
      Alcotest.test_case "doc: bit order matches schema projection" `Quick
        test_doc_bit_order_matches_schema;
    ] )
