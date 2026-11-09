(* Codecs covering what the generator specialises, and one it hands back to
   the codec. Each constructor is a top-level function, so the generated code
   calls it by name. *)

open Wire

type ipv4 =
  UInt8.t
  * UInt8.t
  * UInt16.t
  * UInt16.t
  * UInt16.t
  * UInt8.t
  * UInt8.t
  * UInt16.t
  * SInt32.t
  * UInt32.t

type checked = int * int * UInt16.t * UInt8.t * UInt8.t

type mixed =
  UInt16.t * SInt16.t * SInt8.t * SInt16.t * UInt32.t * SInt32.t * int * int

let ipv4_make vihl tos len id ff ttl proto sum src dst =
  (vihl, tos, len, id, ff, ttl, proto, sum, src, dst)

let ipv4 =
  Codec.v "IPv4" ipv4_make
    Codec.
      [
        (Field.v "VersionIHL" uint8 $ fun (x, _, _, _, _, _, _, _, _, _) -> x);
        (Field.v "TOS" uint8 $ fun (_, x, _, _, _, _, _, _, _, _) -> x);
        (Field.v "Length" uint16be $ fun (_, _, x, _, _, _, _, _, _, _) -> x);
        (Field.v "Id" uint16be $ fun (_, _, _, x, _, _, _, _, _, _) -> x);
        (Field.v "Frag" uint16be $ fun (_, _, _, _, x, _, _, _, _, _) -> x);
        (Field.v "TTL" uint8 $ fun (_, _, _, _, _, x, _, _, _, _) -> x);
        (Field.v "Proto" uint8 $ fun (_, _, _, _, _, _, x, _, _, _) -> x);
        (Field.v "Sum" uint16be $ fun (_, _, _, _, _, _, _, x, _, _) -> x);
        (Field.v "Src" int32be $ fun (_, _, _, _, _, _, _, _, x, _) -> x);
        (Field.v "Dst" uint32be $ fun (_, _, _, _, _, _, _, _, _, x) -> x);
      ]

let checked_make version ihl len ttl proto = (version, ihl, len, ttl, proto)

let checked =
  let ihl = Field.v "IHL" (bits ~width:4 U8) in
  let len = Field.v "Length" uint16be in
  Codec.v "Checked"
    ~where:Expr.(Field.ref len >= Field.ref ihl * int 4)
    checked_make
    Codec.
      [
        ( Field.v "Version" (bits ~width:4 U8) ~self_constraint:(fun self ->
              Expr.(self = int 4))
        $ fun (x, _, _, _, _) -> x );
        (ihl $ fun (_, x, _, _, _) -> x);
        (len $ fun (_, _, x, _, _) -> x);
        ( Field.v "TTL" uint8 ~self_constraint:(fun self -> Expr.(self > int 0))
        $ fun (_, _, _, x, _) -> x );
        ( Field.v "Proto"
            (enum "Proto" [ ("icmp", 1); ("tcp", 6); ("udp", 17) ] uint8)
        $ fun (_, _, _, _, x) -> x );
      ]

let mixed_make a b c d e f g h = (a, b, c, d, e, f, g, h)

let mixed =
  Codec.v "Mixed" mixed_make
    Codec.
      [
        (Field.v "A" uint16 $ fun (x, _, _, _, _, _, _, _) -> x);
        (Field.v "B" int16 $ fun (_, x, _, _, _, _, _, _) -> x);
        (Field.v "C" int8 $ fun (_, _, x, _, _, _, _, _) -> x);
        (Field.v "D" int16be $ fun (_, _, _, x, _, _, _, _) -> x);
        (Field.v "E" uint32 $ fun (_, _, _, _, x, _, _, _) -> x);
        (Field.v "F" int32 $ fun (_, _, _, _, _, x, _, _) -> x);
        (Field.v "G" (bits ~width:5 U16) $ fun (_, _, _, _, _, _, x, _) -> x);
        (Field.v "H" (bits ~width:11 U16) $ fun (_, _, _, _, _, _, _, x) -> x);
      ]

let small_make a b = (a, b)

let small =
  Codec.v "Small" small_make
    Codec.
      [
        (Field.v "A" uint8 $ fun (x, _) -> x);
        (Field.v "B" uint16be $ fun (_, x) -> x);
      ]

let eth_make dst src ty = (dst, src, ty)

let eth =
  Codec.v "Eth" eth_make
    Codec.
      [
        (Field.v "Dst" (byte_array ~size:(int 6)) $ fun (x, _, _) -> x);
        (Field.v "Src" (byte_array ~size:(int 6)) $ fun (_, x, _) -> x);
        (Field.v "Type" uint16be $ fun (_, _, x) -> x);
      ]

let variable_make n rest = (n, rest)

let variable =
  let n = Field.v "N" uint8 in
  Codec.v "Variable" variable_make
    Codec.
      [
        (n $ fun (x, _) -> x);
        (Field.v "Rest" (byte_array ~size:(Field.ref n)) $ fun (_, x) -> x);
      ]
