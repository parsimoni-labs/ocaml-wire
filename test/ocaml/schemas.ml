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

let ipv4_frag =
  Codec.(Field.v "Frag" uint16be $ fun (_, _, _, _, x, _, _, _, _, _) -> x)

let ipv4_proto =
  Codec.(Field.v "Proto" uint8 $ fun (_, _, _, _, _, _, x, _, _, _) -> x)

let ipv4_src =
  Codec.(Field.v "Src" int32be $ fun (_, _, _, _, _, _, _, _, x, _) -> x)

let ipv4_dst =
  Codec.(Field.v "Dst" uint32be $ fun (_, _, _, _, _, _, _, _, _, x) -> x)

let ipv4 =
  Codec.v "IPv4" ipv4_make
    Codec.
      [
        (Field.v "VersionIHL" uint8 $ fun (x, _, _, _, _, _, _, _, _, _) -> x);
        (Field.v "TOS" uint8 $ fun (_, x, _, _, _, _, _, _, _, _) -> x);
        (Field.v "Length" uint16be $ fun (_, _, x, _, _, _, _, _, _, _) -> x);
        (Field.v "Id" uint16be $ fun (_, _, _, x, _, _, _, _, _, _) -> x);
        ipv4_frag;
        (Field.v "TTL" uint8 $ fun (_, _, _, _, _, x, _, _, _, _) -> x);
        ipv4_proto;
        (Field.v "Sum" uint16be $ fun (_, _, _, _, _, _, _, x, _, _) -> x);
        ipv4_src;
        ipv4_dst;
      ]

let checked_make version ihl len ttl proto = (version, ihl, len, ttl, proto)

let checked_proto =
  Codec.(
    Field.v "Proto"
      (enum "Proto" [ ("icmp", 1); ("tcp", 6); ("udp", 17) ] uint8)
    $ fun (_, _, _, _, x) -> x)

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
        checked_proto;
      ]

let mixed_make a b c d e f g h = (a, b, c, d, e, f, g, h)
let mixed_b = Codec.(Field.v "B" int16 $ fun (_, x, _, _, _, _, _, _) -> x)
let mixed_e = Codec.(Field.v "E" uint32 $ fun (_, _, _, _, x, _, _, _) -> x)
let mixed_f = Codec.(Field.v "F" int32 $ fun (_, _, _, _, _, x, _, _) -> x)

let mixed_h =
  Codec.(Field.v "H" (bits ~width:11 U16) $ fun (_, _, _, _, _, _, _, x) -> x)

let mixed =
  Codec.v "Mixed" mixed_make
    Codec.
      [
        (Field.v "A" uint16 $ fun (x, _, _, _, _, _, _, _) -> x);
        mixed_b;
        (Field.v "C" int8 $ fun (_, _, x, _, _, _, _, _) -> x);
        (Field.v "D" int16be $ fun (_, _, _, x, _, _, _, _) -> x);
        mixed_e;
        mixed_f;
        (Field.v "G" (bits ~width:5 U16) $ fun (_, _, _, _, _, _, x, _) -> x);
        mixed_h;
      ]

let small_make a b = (a, b)

let small =
  Codec.v "Small" small_make
    Codec.
      [
        (Field.v "Type" uint8 $ fun (x, _) -> x);
        (Field.v "local_" uint16be $ fun (_, x) -> x);
      ]

let eth_make dst src ty = (dst, src, ty)

let eth_src =
  Codec.(Field.v "Src" (byte_array ~size:(int 6)) $ fun (_, x, _) -> x)

let eth =
  Codec.v "Eth" eth_make
    Codec.
      [
        (Field.v "Dst" (byte_array ~size:(int 6)) $ fun (x, _, _) -> x);
        eth_src;
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

type wide = int * int * int * int * int * int

let wide_make a b c d e f = (a, b, c, d, e, f)

let wide =
  Codec.v "Wide" wide_make
    Codec.
      [
        (Field.v "A" (bits ~width:1 U32be) $ fun (x, _, _, _, _, _) -> x);
        (Field.v "B" (bits ~width:8 U32be) $ fun (_, x, _, _, _, _) -> x);
        (Field.v "C" (bits ~width:23 U32be) $ fun (_, _, x, _, _, _) -> x);
        (Field.v "D" (bits ~width:3 U32) $ fun (_, _, _, x, _, _) -> x);
        (Field.v "E" (bits ~width:29 U32) $ fun (_, _, _, _, x, _) -> x);
        (Field.v "F" (bits ~width:31 U32be) $ fun (_, _, _, _, _, x) -> x);
      ]

let u16be_int = map ~decode:UInt16.to_int ~encode:UInt16.v uint16be
let i32be_int = map ~decode:SInt32.to_int32 ~encode:SInt32.of_int32 int32be
let proto_lookup = lookup [ `Icmp; `Tcp; `Udp ] uint8

let flag_bit =
  map
    ~decode:(fun x -> x = 1)
    ~encode:(fun b -> if b then 1 else 0)
    (bits ~width:1 U8)

type mapped = int * int32 * [ `Icmp | `Tcp | `Udp ] * bool * int

let mapped_make len addr proto flag rest = (len, addr, proto, flag, rest)
let mapped_len = Codec.(Field.v "Len" u16be_int $ fun (x, _, _, _, _) -> x)

let mapped =
  Codec.v "Mapped" mapped_make
    Codec.
      [
        mapped_len;
        (Field.v "Addr" i32be_int $ fun (_, x, _, _, _) -> x);
        (Field.v "Proto" proto_lookup $ fun (_, _, x, _, _) -> x);
        (Field.v "Flag" flag_bit $ fun (_, _, _, x, _) -> x);
        (Field.v "Rest" (bits ~width:7 U8) $ fun (_, _, _, _, x) -> x);
      ]

type signed = SInt8.t * SInt16.t

let signed_make s t = (s, t)

let signed =
  Codec.v "Signed" signed_make
    Codec.
      [
        Field.v "S" int8 ~self_constraint:(fun self -> Expr.(self >= int (-5)))
        $ fst;
        Field.v "T" int16be ~self_constraint:(fun self ->
            Expr.(self <> int (-1)))
        $ snd;
      ]

type refined = int * int * UInt8.t * UInt8.t

let refined_make hi lo kind code = (hi, lo, kind, code)

(* Checks that can fail together, so the error says which the codec runs
   first: a refinement over an enum, an enum over a refinement, and a bitfield
   after the first of its word. *)
let refined =
  let kind = Field.v "Kind" uint8 in
  let code = Field.v "Code" uint8 in
  Codec.v "Refined" refined_make
    Codec.
      [
        (Field.v "Hi" (bits ~width:4 U8) $ fun (x, _, _, _) -> x);
        ( Field.v "Lo" (bits ~width:4 U8) ~self_constraint:(fun self ->
              Expr.(self <> int 0))
        $ fun (_, x, _, _) -> x );
        ( Field.v "Kind"
            (where
               Expr.(Field.ref kind < int 3)
               (enum "K" [ ("a", 1); ("b", 2); ("e", 5) ] uint8))
            ~self_constraint:(fun self -> Expr.(self <> int 1))
        $ fun (_, _, x, _) -> x );
        ( Field.v "Code"
            (enum "C"
               [ ("x", 10); ("y", 20); ("z", 30) ]
               (where Expr.(Field.ref code > int 15) uint8))
        $ fun (_, _, _, x) -> x );
      ]

type overflow = UInt32.t * int * int * UInt32.t * UInt32.t

let overflow_make a hi lo b c = (a, hi, lo, b, c)

(* Arithmetic that leaves the native int: the sums for any [A] or [Lo] above
   5, the product for large [B] and [C]. *)
let overflow =
  let near_max = max_int - 5 in
  let overflows self = Expr.(self + int near_max <> int 0) in
  let b = Field.v "B" uint32be in
  let c = Field.v "C" uint32be in
  Codec.v "Overflow"
    ~where:Expr.(Field.ref b * Field.ref c >= int 0)
    overflow_make
    Codec.
      [
        ( Field.v "A" uint32be ~self_constraint:overflows
        $ fun (x, _, _, _, _) -> x );
        (Field.v "Hi" (bits ~width:4 U8) $ fun (_, x, _, _, _) -> x);
        ( Field.v "Lo" (bits ~width:4 U8) ~self_constraint:overflows
        $ fun (_, _, x, _, _) -> x );
        (b $ fun (_, _, _, x, _) -> x);
        (c $ fun (_, _, _, _, x) -> x);
      ]

type sized = UInt32.t * Bytesrw.Bytes.Slice.t option * UInt8.t

let sized =
  let n = Field.v "n" uint32be in
  Codec.v "Sized"
    (fun n body tail -> (n, body, tail))
    Codec.
      [
        (n $ fun (n, _, _) -> n);
        ( Field.optional "body"
            ~present:Expr.(Field.ref n * Field.ref n <> int 1)
            (byte_slice ~size:Expr.((Field.ref n * Field.ref n) - int 1))
        $ fun (_, b, _) -> b );
        (Field.v "tail" uint8 $ fun (_, _, t) -> t);
      ]

type skipped = UInt32.t * UInt8.t * UInt8.t option * UInt8.t option

let skipped =
  let n = Field.v "n" uint32be and x = Field.v "x" uint8 in
  Codec.v "Skipped"
    (fun n x a b -> (n, x, a, b))
    Codec.
      [
        (n $ fun (n, _, _, _) -> n);
        (x $ fun (_, x, _, _) -> x);
        ( Field.optional "a" ~present:Expr.(Field.ref x = int 4) uint8
        $ fun (_, _, a, _) -> a );
        ( Field.optional "b"
            ~present:
              Expr.(Field.ref n * Field.ref n > int 0 && Field.ref x = int 3)
            uint8
        $ fun (_, _, _, b) -> b );
      ]

let rec gated n =
  let tag = Field.v "tag" uint8 in
  let value =
    Field.optional "value" ~present:Expr.(Field.ref tag = int 1) uint16be
  in
  let tag = Codec.(tag $ fun () -> UInt8.v 0)
  and value = Codec.(value $ fun () -> None) in
  if n <= 1 then Codec.v "Gated" (fun _ _ -> ()) Codec.[ tag; value ]
  else
    let rest = Codec.(Field.v "rest" (codec (gated (n - 1))) $ fun () -> ()) in
    Codec.v
      ("Gated" ^ string_of_int n)
      (fun _ _ _ -> ())
      Codec.[ tag; value; rest ]

let rec nested n =
  let tag = Field.v "tag" uint8 in
  let tag_f = Codec.(tag $ fun () -> UInt8.v 0)
  and after = Codec.(Field.v "after" uint8 $ fun () -> UInt8.v 0) in
  if n <= 1 then Codec.v "Nested" (fun _ _ -> ()) Codec.[ tag_f; after ]
  else
    let inner =
      Codec.(
        Field.optional "inner"
          ~present:Expr.(Field.ref tag = int 1)
          (codec (nested (n - 1)))
        $ fun () -> None)
    in
    Codec.v
      ("Nested" ^ string_of_int n)
      (fun _ _ _ -> ())
      Codec.[ tag_f; inner; after ]
