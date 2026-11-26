(* The codecs the differential test of the stubs runs: each refusal the
   validator can make, a short buffer, a field constraint, a cross-field
   [~where], an enum, a byte span sized by a field and zero padding. The test
   compiles this file into the program that calls the stubs, so it links
   [wire] alone. *)

open Wire

module Header = struct
  type r = UInt8.t * UInt16.t

  let codec =
    Codec.v "DiffHeader"
      (fun version length -> (version, length))
      Codec.
        [
          (Field.v "version" uint8 $ fun (v, _) -> v);
          (Field.v "length" uint16be $ fun (_, l) -> l);
        ]
end

module Limits = struct
  type r = UInt8.t * UInt16.t

  let codec =
    Codec.v "DiffLimits"
      (fun x y -> (x, y))
      Codec.
        [
          Field.v "x" ~self_constraint:(fun x -> Expr.(x <= int 100)) uint8
          $ fst;
          Field.v "y" ~self_constraint:(fun y -> Expr.(y >= int 1)) uint16be
          $ snd;
        ]
end

module Bits = struct
  type r = int * int * UInt16.t

  let codec =
    Codec.v "DiffBits"
      (fun version ihl total -> (version, ihl, total))
      Codec.
        [
          ( Field.v "version"
              ~self_constraint:(fun v -> Expr.(v = int 4))
              (bits ~width:4 U8)
          $ fun (v, _, _) -> v );
          ( Field.v "ihl"
              ~self_constraint:(fun v -> Expr.(v >= int 5))
              (bits ~width:4 U8)
          $ fun (_, i, _) -> i );
          (Field.v "total" uint16be $ fun (_, _, t) -> t);
        ]
end

module Pair = struct
  type r = UInt8.t * UInt8.t

  let f_a = Field.v "a" uint8
  let f_b = Field.v "b" uint8

  let codec =
    Codec.v "DiffPair"
      ~where:Expr.(Field.ref f_a + Field.ref f_b <= int 200)
      (fun a b -> (a, b))
      Codec.[ f_a $ fst; f_b $ snd ]
end

module Kind = struct
  type r = UInt8.t * UInt16.t

  let codec =
    Codec.v "DiffKind"
      (fun kind value -> (kind, value))
      Codec.
        [
          Field.v "kind"
            (enum "DiffKindTag" [ ("Ping", 1); ("Pong", 2); ("Data", 7) ] uint8)
          $ fst;
          Field.v "value" uint16be $ snd;
        ]
end

module Sized = struct
  type r = UInt8.t * string * UInt8.t

  let f_len = Field.v "len" uint8

  let codec =
    Codec.v "DiffSized"
      (fun len payload tail -> (len, payload, tail))
      Codec.
        [
          (f_len $ fun (l, _, _) -> l);
          ( Field.v "payload" (byte_array ~size:(Field.ref f_len))
          $ fun (_, p, _) -> p );
          (Field.v "tail" uint8 $ fun (_, _, t) -> t);
        ]
end

module Padded = struct
  type r = UInt8.t * string

  let codec =
    Codec.v "DiffPadded"
      (fun x pad -> (x, pad))
      Codec.[ Field.v "x" uint8 $ fst; Field.v "pad" all_zeros $ snd ]
end

(* The codecs of the other end-to-end tests. *)

let named name =
  Codec.v name
    (fun version length -> (version, length))
    Codec.
      [
        (Field.v "version" uint8 $ fun (v, _) -> v);
        (Field.v "length" uint16be $ fun (_, l) -> l);
      ]

module Offset_header = struct
  type r = UInt8.t * UInt16.t

  let codec = named "OffsetHeader"
end

module Constrained = struct
  type r = UInt8.t

  let codec =
    Codec.v "Constrained"
      (fun x -> x)
      Codec.
        [
          ( Field.v "x" ~self_constraint:(fun x -> Expr.(x <= int 100)) uint8
          $ fun x -> x );
        ]
end

module Refused = struct
  type r = UInt8.t * UInt16.t

  let codec =
    Codec.v "Refused"
      (fun x y -> (x, y))
      Codec.
        [
          Field.v "x" ~self_constraint:(fun x -> Expr.(x <= int 100)) uint8
          $ fst;
          Field.v "y" uint16be $ snd;
        ]
end

module Bf_header = struct
  type r = int * int * UInt16.t

  let codec =
    Codec.v "BfHeader"
      (fun version flags length -> (version, flags, length))
      Codec.
        [
          (Field.v "version" (bits ~width:4 U8) $ fun (v, _, _) -> v);
          (Field.v "flags" (bits ~width:4 U8) $ fun (_, f, _) -> f);
          (Field.v "length" uint16be $ fun (_, _, l) -> l);
        ]
end

module Gc_quad = struct
  type r = UInt64.t * UInt64.t * UInt64.t * UInt64.t

  let f n = Field.v n uint64be

  let codec =
    Codec.v "GcQuad"
      (fun a b c d -> (a, b, c, d))
      Codec.
        [
          (f "a" $ fun (a, _, _, _) -> a);
          (f "b" $ fun (_, b, _, _) -> b);
          (f "c" $ fun (_, _, c, _) -> c);
          (f "d" $ fun (_, _, _, d) -> d);
        ]
end

module Test_parse = struct
  type r = UInt32.t * UInt16.t * UInt8.t

  let codec =
    Codec.v "TestParse"
      (fun id len tag -> (id, len, tag))
      Codec.
        [
          (Field.v "Id" uint32be $ fun (id, _, _) -> id);
          (Field.v "Length" uint16be $ fun (_, l, _) -> l);
          (Field.v "Tag" uint8 $ fun (_, _, t) -> t);
        ]
end
