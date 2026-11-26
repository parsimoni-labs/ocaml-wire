(* The codecs the differential test of the stubs runs: each refusal the
   validator can make, a short buffer, a field constraint, a cross-field
   [~where], an enum, a byte span sized by a field and zero padding. The test
   compiles this file into the program that calls the stubs, so it links
   [wire] alone. *)

open Wire

module Header = struct
  type r = int * int

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
  type r = int * int

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
  type r = int * int * int

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
  type r = int * int

  let f_a = Field.v "a" uint8
  let f_b = Field.v "b" uint8

  let codec =
    Codec.v "DiffPair"
      ~where:Expr.(Field.ref f_a + Field.ref f_b <= int 200)
      (fun a b -> (a, b))
      Codec.[ f_a $ fst; f_b $ snd ]
end

module Kind = struct
  type r = int * int

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
  type r = int * string * int

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
  type r = int * string

  let codec =
    Codec.v "DiffPadded"
      (fun x pad -> (x, pad))
      Codec.[ Field.v "x" uint8 $ fst; Field.v "pad" all_zeros $ snd ]
end
