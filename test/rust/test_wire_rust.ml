(* What Wire_rust accepts, refuses and names. The code it writes is compiled
   and compared with the codec by the Rust lane of the differential fuzzer
   (fuzz/diff/rust), and compiled here without std when BUILD_RUST=1. *)

open Wire
open Rust_src

(* A field's name becomes a Rust identifier, its record field's and its
   accessor's: lower-cased, made an identifier, escaped when a keyword of any
   edition, never the lone [_] pattern. *)
let test_ident () =
  List.iter
    (fun (label, ident) ->
      let src =
        rs (Codec.v "C" Fun.id Codec.[ Field.v label uint8 $ Fun.id ])
      in
      has label src (Fmt.str "    pub %s: u8," ident);
      has label src (Fmt.str "pub fn %s(&self) -> u8 {" ident))
    [
      ("Version", "version");
      ("a-b", "a_b");
      ("1x", "f_1x");
      ("_", "f__");
      ("", "f_");
      ("type", "type_");
      ("Self", "self_");
      ("async", "async_");
      ("gen", "gen_");
      ("union", "union");
    ]

let refused what ?folds c names =
  match Wire_rust.v ?folds c with
  | _ -> Alcotest.failf "%s: generated" what
  | exception Invalid_argument m ->
      List.iter
        (fun n ->
          Alcotest.(check bool)
            (Fmt.str "%s: names %s in %S" what n m)
            true (contains m n))
        names

(* What the OCaml writer refuses is refused with its message; a map calls
   OCaml functions and a name that is not UTF-8 has no Rust literal. *)
let test_refused () =
  refused "a map" Schemas.mapped
    [ "Wire_rust: cannot specialise"; "Len"; "a map" ];
  refused "a 64-bit scalar"
    (Codec.v "Wide" Fun.id Codec.[ Field.v "w" uint64be $ Fun.id ])
    [ "Wire_rust: cannot specialise"; "UINT64" ];
  refused "an unknown output"
    ~folds:[ Wire_rust.fold ~outputs:[ "nope" ] "fold" ]
    Packet_stack.codec [ "nope" ];
  refused "a name that is not UTF-8"
    (Codec.v "C" Fun.id Codec.[ Field.v "\xff" uint8 $ Fun.id ])
    [ "UTF-8" ]

(* Field names that spell one name twice are refused naming both: two
   functions of one name do not compile. *)
let test_clash () =
  let pair n1 t1 n2 t2 =
    Codec.v "C"
      (fun a b -> (a, b))
      Codec.[ Field.v n1 t1 $ fst; Field.v n2 t2 $ snd ]
  in
  refused "X and x" (pair "X" uint8 "x" uint8) [ "\"x\"" ];
  (* The keyword escape is injective: [type] and [type_] are [type_] and
     [type__]. *)
  has "type and type_"
    (rs (pair "type" uint8 "type_" uint8))
    "pub type_: u8,\n    pub type__: u8,";
  refused "two folds of one name"
    ~folds:Wire_rust.[ fold "f"; fold "f" ]
    Schemas.checked [ "\"f\"" ];
  (* A field may be called [buf], [off] or [b], the parameters of the
     module's functions. *)
  let c =
    Codec.v "C"
      (fun a b c -> (a, b, c))
      Codec.
        [
          (Field.v "buf" uint8 $ fun (x, _, _) -> x);
          (Field.v "off" uint8 $ fun (_, x, _) -> x);
          (Field.v "b" uint8 $ fun (_, _, x) -> x);
        ]
  in
  has "buf, off, b" (rs c) "pub buf: u8,\n    pub off: u8,\n    pub b: u8,"

(* The visitor a rule names is checked to implement the fold's trait, by its
   path or by its file. *)
let test_visitor () =
  let folds = [ Wire_rust.fold "count" ] in
  has "a path"
    (rs ~folds ~visitors:[ ("count", "crate::Count") ] Schemas.checked)
    "implements::<crate::Count>();";
  has "a file"
    (rs ~folds ~visitors:[ ("count", "flow_step.rs") ] Schemas.checked)
    "implements::<crate::flow_step::FlowStep>();";
  match
    Wire_rust.(
      to_rs ~visitors:[ ("nope", "crate::X") ] (v ~folds Schemas.checked))
  with
  | _ -> Alcotest.fail "a visitor of no fold"
  | exception Invalid_argument m -> has "a visitor of no fold" m "nope"

let suite =
  ( "wire_rust",
    [
      Alcotest.test_case "identifiers" `Quick test_ident;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
      Alcotest.test_case "visitors" `Quick test_visitor;
    ] )
