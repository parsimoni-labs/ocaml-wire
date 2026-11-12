open Wire

type inner = { tag : UInt8.t; value : UInt16.t }

let f_inner_tag = Field.v "Tag" uint8
let f_inner_value = Field.v "Value" uint16be

let inner_codec =
  Codec.v "Inner"
    (fun tag value -> ({ tag; value } : inner))
    Codec.
      [
        (f_inner_tag $ fun (r : inner) -> r.tag);
        (f_inner_value $ fun (r : inner) -> r.value);
      ]

type outer = { header : UInt8.t; inner : inner; trailer : UInt8.t }

let outer_codec =
  Codec.v "Outer"
    (fun header inner trailer -> ({ header; inner; trailer } : outer))
    Codec.
      [
        (Field.v "Header" uint8 $ fun (r : outer) -> r.header);
        (Field.v "Inner" (codec inner_codec) $ fun (r : outer) -> r.inner);
        (Field.v "Trailer" uint8 $ fun (r : outer) -> r.trailer);
      ]

type l2 = { x : UInt8.t }
type l1 = { inner : l2; y : UInt16.t }
type l0 = { inner : l1; z : UInt8.t }

let l2_codec =
  Codec.v "L2"
    (fun x -> ({ x } : l2))
    Codec.[ (Field.v "X" uint8 $ fun (r : l2) -> r.x) ]

let l1_codec =
  Codec.v "L1"
    (fun inner y -> ({ inner; y } : l1))
    Codec.
      [
        (Field.v "Inner" (codec l2_codec) $ fun (r : l1) -> r.inner);
        (Field.v "Y" uint16be $ fun (r : l1) -> r.y);
      ]

let l0_codec =
  Codec.v "L0"
    (fun inner z -> ({ inner; z } : l0))
    Codec.
      [
        (Field.v "Inner" (codec l1_codec) $ fun (r : l0) -> r.inner);
        (Field.v "Z" uint8 $ fun (r : l0) -> r.z);
      ]

type opt_record = { hdr : UInt8.t; payload : UInt16.t option; trail : UInt8.t }

let opt_codec ~present =
  Codec.v "OptRecord"
    (fun hdr payload trail -> ({ hdr; payload; trail } : opt_record))
    Codec.
      [
        (Field.v "Hdr" uint8 $ fun (r : opt_record) -> r.hdr);
        ( Field.optional "Payload" ~present:(bool present) uint16be
        $ fun (r : opt_record) -> r.payload );
        (Field.v "Trail" uint8 $ fun (r : opt_record) -> r.trail);
      ]

let opt_codec_present = opt_codec ~present:true
let opt_codec_absent = opt_codec ~present:false

type container = { length : UInt8.t; items : inner list }

let f_cnt_length = Field.v "Length" uint8

let repeat_codec =
  Codec.v "Container"
    (fun length items -> ({ length; items } : container))
    Codec.
      [
        (f_cnt_length $ fun (r : container) -> r.length);
        ( Field.repeat "Items" ~size:(Field.ref f_cnt_length) (codec inner_codec)
        $ fun (r : container) -> r.items );
      ]

type packet = { id : UInt8.t; data : UInt16.t }

let packet_codec =
  Codec.v "Packet"
    (fun id data -> ({ id; data } : packet))
    Codec.
      [
        (Field.v "Id" uint8 $ fun (r : packet) -> r.id);
        (Field.v "Data" uint16be $ fun (r : packet) -> r.data);
      ]

type multi_record = { x : UInt16.t; y : UInt16.t }

let multi_record_codec =
  Codec.v "MultiRecord"
    (fun x y -> ({ x; y } : multi_record))
    Codec.
      [
        (Field.v "x" uint16be $ fun (r : multi_record) -> r.x);
        (Field.v "y" uint16be $ fun (r : multi_record) -> r.y);
      ]

(* -- Decode assertions -- *)

let decode_ok = function
  | Ok v -> v
  | Error e -> Alcotest.failf "unexpected parse error: %a" pp_parse_error e

let expect_constraint_fail = function
  | Ok _ -> Alcotest.fail "expected a Constraint_failed parse error"
  | Error { kind = Constraint_failed _; _ } -> ()
  | Error e ->
      Alcotest.failf "expected Constraint_failed, got %a" pp_parse_error e

(* Assert a decode failed with a [kind] satisfying [pred]. *)
let expect_kind pred = function
  | Ok _ -> Alcotest.fail "expected a parse error"
  | Error { kind; _ } when pred kind -> ()
  | Error e -> Alcotest.failf "unexpected parse error: %a" pp_parse_error e

let roundtrip name typ testable v =
  Alcotest.check testable name v (decode_ok (of_string typ (to_string typ v)))

(* -- Codec test fixtures -- *)

open Wire.Everparse.Raw

let contains ~sub s = Re.execp (Re.compile (Re.str sub)) s

(* Project a codec to its 3D rendering for substring assertions. *)
let render_3d codec =
  to_3d (module_ [ typedef (Everparse.Raw.struct_of_codec codec) ])

(* Helper: encode record to string using Codec API *)
let encode_record codec v = Ok (Codec.to_string codec v)

(* Helper: decode record from string using Codec API *)
let decode_record codec s =
  let ws = Codec.wire_size codec in
  if String.length s < ws then
    Error
      {
        at = 0;
        field = [];
        kind = Unexpected_eof { expected = ws; got = String.length s };
      }
  else Codec.decode codec (Bytes.of_string s) 0

type simple_record = { a : UInt8.t; b : UInt16.t; c : UInt32.t }

let simple_record_codec =
  let open Codec in
  v "SimpleRecord"
    (fun a b c -> { a; b; c })
    [
      (Field.v "a" uint8 $ fun r -> r.a);
      (Field.v "b" uint16 $ fun r -> r.b);
      (Field.v "c" uint32 $ fun r -> r.c);
    ]

type meta_record = { x : UInt8.t }

let projection_limit = Param.input "limit" uint8
let projection_outx = Param.output "outx" uint8
let projection_f_x = Field.v "x" uint8

let projection_codec =
  let open Codec in
  v "ProjectionCodec"
    ~where:Expr.(Field.ref projection_f_x <= Param.expr projection_limit)
    (fun x -> { x })
    [
      ( Field.v "x"
          ~constraint_:Expr.(Field.ref projection_f_x <= int 8)
          ~action:
            (Action.on_success
               [ Action.assign projection_outx (Field.ref projection_f_x) ])
          uint8
      $ fun r -> r.x );
    ]

(* The allocation tests read [Gc.minor_words], which only counts under the
   native and bytecode runtimes; under wasm_of_ocaml or js_of_ocaml the
   counters stay at zero and the assertions would pass vacuously. *)
let skip_unless_gc_counters () =
  match Sys.backend_type with
  | Sys.Other _ -> Alcotest.skip ()
  | Sys.Native | Sys.Bytecode -> ()

let raises_invalid f =
  try
    ignore (f ());
    false
  with Invalid_argument _ -> true

let printable_byte b = Expr.(b >= int 0x20 && b <= int 0x7e)

module Bs = Bytesrw.Bytes.Slice
