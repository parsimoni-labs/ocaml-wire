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

let refused what ?fold c names =
  match Wire_rust.v ?fold c with
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
  refused "an unknown field" ~fold:[ "nope" ] Packet_stack.codec [ "nope" ];
  refused "a field of bytes" ~fold:[ "dst" ] Packet_stack.codec [ "dst" ];
  refused "a field holding a record" ~fold:[ "ipv4" ] Packet_stack.codec
    [ "ipv4" ];
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

(* The fold hands [f] the fields its paths name, in order, each in its Rust
   type: an [Option] where the field is optional, and one more under a layer,
   whether the layer is optional or not. *)
let test_fold_fields () =
  has "paths"
    (rs
       ~fold:[ "ipv4.tcp.dst_port"; "ether_type"; "ipv4.protocol" ]
       Packet_stack.codec)
    "F: FnMut(A, Option<u16>, u16, Option<u8>) -> A,";
  has "an optional field"
    (rs ~fold:[ "a"; "x" ] Schemas.skipped)
    "F: FnMut(A, Option<u8>, u8) -> A,";
  has "no field" (rs ~fold:[] Schemas.checked) "F: FnMut(A) -> A,"

let suite =
  ( "wire_rust",
    [
      Alcotest.test_case "identifiers" `Quick test_ident;
      Alcotest.test_case "unsupported codecs are refused" `Quick test_refused;
      Alcotest.test_case "clashing names are refused" `Quick test_clash;
      Alcotest.test_case "the fields of the fold" `Quick test_fold_fields;
    ] )
