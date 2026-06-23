(** Fuzz generators that mirror Wire's surface.

    Each ['a t] is the codec it tests paired with three Alcobar generators:
    [positive] produces a value together with bytes that decode to it, [random]
    produces arbitrary bytes the decoder must handle without raising, and
    [adversarial] produces bytes crafted at the boundary of every constraint the
    codec checks (lengths, gate flips, bit-pattern extremes). [run] turns one
    ['a t] into a set of Alcobar test cases that exercise round-trip on
    positives and crash-safety on the other two streams.

    [Codec.v] composes leaves into record gens the same way [Wire.Codec.v]
    composes typs into record codecs -- a record gen is itself an ['a t], so
    composition is recursive. *)

type 'a t = {
  codec : 'a Wire.Codec.t;
  positive : ('a * bytes) Alcobar.gen;
  random : bytes Alcobar.gen;
  adversarial : bytes Alcobar.gen;
  equal : 'a -> 'a -> bool;
  typ : 'a Wire.typ;
      (* Wire typ form, used when this gen slots into a parent record's
         field list. For leaves it is the bare typ; for records it is
         [Wire.codec built_codec], i.e. a sub-codec wrap. *)
  env : env_strategy option;
      (* When the codec references [Param.input], the test driver pulls
         a fresh env from [positive] for the round-trip case and from
         the [fuzz] gen for random and adversarial streams. *)
}

and env_strategy = {
  positive : Wire.Param.env -> Wire.Param.env;
      (* Binder for the positive stream: binds every [Param.input] (by name) on
         a fresh env built for the codec under test. Taking the env as an
         argument (rather than building it from a fixed codec) lets a leaf's
         param survive composition: when the leaf is nested in a bigger codec,
         the param is rebound on that codec's own env. *)
  fuzz : (Wire.Param.env -> Wire.Param.env) Alcobar.gen;
      (* Binder gen for the random and adversarial streams. *)
}

let bytes_of_string s = Bytes.unsafe_of_string s
let string_of_bytes b = Bytes.unsafe_to_string b

let corpus_generation_mode () =
  Array.exists (String.equal "--gen-corpus") Sys.argv

let file_input_mode () =
  let argv = Sys.argv in
  let n = Array.length argv in
  n > 1
  && (not (corpus_generation_mode ()))
  &&
  let path = argv.(n - 1) in
  Sys.file_exists path
  && try not (Sys.is_directory path) with Sys_error _ -> false

let slice_of_string s =
  let b = Bytes.of_string s in
  Bytesrw.Bytes.Slice.make_or_eod b ~first:0 ~length:(Bytes.length b)

let string_of_slice s =
  Bytes.sub_string
    (Bytesrw.Bytes.Slice.bytes s)
    (Bytesrw.Bytes.Slice.first s)
    (Bytesrw.Bytes.Slice.length s)

let bytes_of_octets xs =
  let b = Bytes.create (List.length xs) in
  List.iteri (fun i x -> Bytes.set_uint8 b i (x land 0xFF)) xs;
  b

let bytes_fixed n =
  Alcobar.map Alcobar.[ Alcobar.bytes_fixed n ] bytes_of_string

let bytes_any = Alcobar.map Alcobar.[ Alcobar.bytes ] bytes_of_string

let codec_of_typ typ =
  Wire.Codec.v "_leaf"
    (fun v -> v)
    Wire.Codec.[ (Wire.Field.v "v" typ $ fun v -> v) ]

(* The canonical bytes for [v]: a buffer sized by [size_of_value] then
   filled by [encode]. *)
let encode_via_codec codec v =
  let sz = Wire.Codec.size_of_value codec v in
  let buf = Bytes.create sz in
  Wire.Codec.encode codec v buf 0;
  buf

(* Build a leaf [t] from a typ and three Alcobar generators. *)
let leaf ~equal ~typ ~value_gen ~random ~adversarial =
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode_via_codec codec v))
  in
  let adversarial_bytes =
    Alcobar.map Alcobar.[ adversarial ] (fun v -> encode_via_codec codec v)
  in
  {
    codec;
    typ;
    positive;
    random;
    adversarial = adversarial_bytes;
    equal;
    env = None;
  }

(* A leaf whose positives draw a value from [value_gen] and encode it through
   the codec, with caller-chosen raw [random] / [adversarial] byte streams (so
   the adversarial stream can carry invalid tags the encoder would never
   produce). *)
let enum_like ~typ ~value_gen ~random ~adversarial =
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode_via_codec codec v))
  in
  { codec; typ; positive; random; adversarial; equal = ( = ); env = None }

(* {1 Leaves} *)

let scalar_int typ size value_gen boundaries =
  leaf ~equal:Int.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let scalar_int64 typ size value_gen boundaries =
  leaf ~equal:Int64.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let u8_boundaries = [ 0; 1; 0x7F; 0x80; 0xFE; 0xFF ]
let u16_boundaries = [ 0; 1; 0x7F; 0x80; 0x7FFF; 0x8000; 0xFFFE; 0xFFFF ]

let u32_boundaries =
  [ 0; 1; 0xFFFF; 0x7FFF_FFFF; 0x8000_0000; 0xFFFF_FFFE; 0xFFFF_FFFF ]

let u63_boundaries = [ 0; 1; 0xFFFF_FFFF; max_int - 1; max_int ]

let u64_boundaries_i64 =
  Int64.[ zero; one; of_int 0xFFFF_FFFF; max_int; min_int; -1L; sub max_int 1L ]

let s8_boundaries = [ -128; -1; 0; 1; 127 ]
let s16_boundaries = [ -0x8000; -1; 0; 1; 0x7FFF ]
let s32_boundaries = [ Int.min_int; -1; 0; 1; Int.max_int ]
let s64_boundaries_i64 = Int64.[ min_int; -1L; zero; one; max_int ]
let uint8 = scalar_int Wire.uint8 1 Alcobar.uint8 u8_boundaries
let uint16 = scalar_int Wire.uint16 2 Alcobar.uint16 u16_boundaries
let uint16be = scalar_int Wire.uint16be 2 Alcobar.uint16 u16_boundaries

let masked_u32 =
  Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land 0xFFFF_FFFF)

let masked_u63 = Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land max_int)
let uint32 = scalar_int Wire.uint32 4 masked_u32 u32_boundaries
let uint32be = scalar_int Wire.uint32be 4 masked_u32 u32_boundaries
let uint63 = scalar_int Wire.uint63 8 masked_u63 u63_boundaries
let uint63be = scalar_int Wire.uint63be 8 masked_u63 u63_boundaries
let uint64 = scalar_int64 Wire.uint64 8 Alcobar.int64 u64_boundaries_i64
let uint64be = scalar_int64 Wire.uint64be 8 Alcobar.int64 u64_boundaries_i64
let int8 = scalar_int Wire.int8 1 Alcobar.int8 s8_boundaries
let int16 = scalar_int Wire.int16 2 Alcobar.int16 s16_boundaries
let int16be = scalar_int Wire.int16be 2 Alcobar.int16 s16_boundaries

let int32 =
  scalar_int Wire.int32 4
    (Alcobar.map Alcobar.[ Alcobar.int32 ] Int32.to_int)
    s32_boundaries

let int32be =
  scalar_int Wire.int32be 4
    (Alcobar.map Alcobar.[ Alcobar.int32 ] Int32.to_int)
    s32_boundaries

let int64 = scalar_int64 Wire.int64 8 Alcobar.int64 s64_boundaries_i64
let int64be = scalar_int64 Wire.int64be 8 Alcobar.int64 s64_boundaries_i64

let float_boundaries =
  [
    0.0;
    -0.0;
    1.0;
    -1.0;
    Float.pi;
    Float.epsilon;
    Float.max_float;
    Float.min_float;
    Float.infinity;
    Float.neg_infinity;
    Float.nan;
  ]

(* Equality on floats compares bit patterns so NaN round-trips check
   bit-identity, not [Float.equal]. *)
let float64_bits_equal a b =
  Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b)

(* Float32 encodes through Int32 bits, which truncates a 64-bit float's
   mantissa. Round-trip equality has to compare the value as it would
   look after that truncation. *)
let float32_bits_equal a b =
  Int32.equal (Int32.bits_of_float a) (Int32.bits_of_float b)

let float32_truncate x = Int32.float_of_bits (Int32.bits_of_float x)
let float32_value_gen = Alcobar.map Alcobar.[ Alcobar.float ] float32_truncate

let float32 =
  leaf ~equal:float32_bits_equal ~typ:Wire.float32 ~value_gen:float32_value_gen
    ~random:(bytes_fixed 4)
    ~adversarial:
      (Alcobar.choose
         (List.map
            (fun v -> Alcobar.const (float32_truncate v))
            float_boundaries))

let float32be =
  leaf ~equal:float32_bits_equal ~typ:Wire.float32be
    ~value_gen:float32_value_gen ~random:(bytes_fixed 4)
    ~adversarial:
      (Alcobar.choose
         (List.map
            (fun v -> Alcobar.const (float32_truncate v))
            float_boundaries))

let float64 =
  leaf ~equal:float64_bits_equal ~typ:Wire.float64 ~value_gen:Alcobar.float
    ~random:(bytes_fixed 8)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const float_boundaries))

let float64be =
  leaf ~equal:float64_bits_equal ~typ:Wire.float64be ~value_gen:Alcobar.float
    ~random:(bytes_fixed 8)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const float_boundaries))

let empty =
  leaf ~equal:Unit.equal ~typ:Wire.empty ~value_gen:(Alcobar.const ())
    ~random:(bytes_fixed 0) ~adversarial:(Alcobar.const ())

let byte_array n =
  let typ = Wire.byte_array ~size:(Wire.int n) in
  let random = bytes_any in
  (* Adversarial: length boundaries -- 0, n-1, n, n+1, 2n. *)
  let adversarial =
    Alcobar.choose
      [
        Alcobar.const Bytes.empty;
        bytes_fixed (max 0 (n - 1));
        bytes_fixed n;
        bytes_fixed (n + 1);
        bytes_fixed (n * 2);
      ]
  in
  let codec = codec_of_typ typ in
  let encode v =
    let buf = Bytes.create n in
    Bytes.blit_string v 0 buf 0 (min n (String.length v));
    buf
  in
  let value_gen = Alcobar.bytes_fixed n in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun s -> (s, encode s)) in
  {
    codec;
    typ;
    positive;
    random;
    adversarial;
    equal = String.equal;
    env = None;
  }

let byte_slice n =
  let typ = Wire.byte_slice ~size:(Wire.int n) in
  let random = bytes_any in
  let adversarial =
    Alcobar.choose
      [
        Alcobar.const Bytes.empty;
        bytes_fixed (max 0 (n - 1));
        bytes_fixed n;
        bytes_fixed (n + 1);
      ]
  in
  let codec = codec_of_typ typ in
  let encode (s : Bytesrw.Bytes.Slice.t) =
    let buf = Bytes.create n in
    Bytes.blit
      (Bytesrw.Bytes.Slice.bytes s)
      (Bytesrw.Bytes.Slice.first s)
      buf 0
      (min n (Bytesrw.Bytes.Slice.length s));
    buf
  in
  let slice_of_string s =
    let b = Bytes.of_string s in
    Bytesrw.Bytes.Slice.make_or_eod b ~first:0 ~length:(Bytes.length b)
  in
  let value_gen = Alcobar.bytes_fixed n in
  let positive =
    Alcobar.map
      Alcobar.[ value_gen ]
      (fun s ->
        let slice = slice_of_string s in
        (slice, encode slice))
  in
  let slice_equal a b =
    let la = Bytesrw.Bytes.Slice.length a
    and lb = Bytesrw.Bytes.Slice.length b in
    la = lb
    &&
    let sa =
      Bytes.sub_string
        (Bytesrw.Bytes.Slice.bytes a)
        (Bytesrw.Bytes.Slice.first a)
        la
    in
    let sb =
      Bytes.sub_string
        (Bytesrw.Bytes.Slice.bytes b)
        (Bytesrw.Bytes.Slice.first b)
        lb
    in
    String.equal sa sb
  in
  { codec; typ; positive; random; adversarial; equal = slice_equal; env = None }

let all_bytes =
  let typ = Wire.all_bytes in
  let codec = codec_of_typ typ in
  let value_gen = Alcobar.bytes in
  let positive =
    Alcobar.map
      Alcobar.[ value_gen ]
      (fun s ->
        let sz = Wire.Codec.size_of_value codec s in
        let buf = Bytes.create sz in
        Wire.Codec.encode codec s buf 0;
        (s, buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = String.equal;
    env = None;
  }

let all_zeros =
  let typ = Wire.all_zeros in
  let codec = codec_of_typ typ in
  let zeros_gen =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 32 ]
      (fun n -> String.make n '\x00')
  in
  let positive =
    Alcobar.map
      Alcobar.[ zeros_gen ]
      (fun s ->
        let sz = Wire.Codec.size_of_value codec s in
        let buf = Bytes.create sz in
        Wire.Codec.encode codec s buf 0;
        (s, buf))
  in
  (* Adversarial: bytes with at least one non-zero -- decoder must
     surface [Constraint_failed], not raise. *)
  let adversarial =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:1 16; Alcobar.range ~min:1 256 ]
      (fun n byte -> Bytes.make n (Char.chr (byte mod 256)))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial;
    equal = String.equal;
    env = None;
  }

(* NUL-free string: a [zeroterm] value cannot contain its own terminator. *)
let nul_free_gen =
  Alcobar.map
    Alcobar.[ Alcobar.bytes ]
    (fun s -> String.concat "" (String.split_on_char '\x00' s))

let zeroterm =
  let typ = Wire.zeroterm in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ nul_free_gen ]
      (fun s ->
        let sz = Wire.Codec.size_of_value codec s in
        let buf = Bytes.create sz in
        Wire.Codec.encode codec s buf 0;
        (s, buf))
  in
  (* Adversarial: a run with no terminator -- decode must surface a clean
     error, not raise. *)
  let adversarial =
    Alcobar.map Alcobar.[ Alcobar.range ~min:0 32 ] (fun n -> Bytes.make n 'x')
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial;
    equal = String.equal;
    env = None;
  }

let zeroterm_at_most n =
  let typ = Wire.zeroterm_at_most ~size:(Wire.int n) in
  let codec = codec_of_typ typ in
  (* The terminator must fit in the region, so cap the data at [n - 1]. *)
  let value_gen =
    Alcobar.map
      Alcobar.[ nul_free_gen ]
      (fun s ->
        if String.length s > n - 1 then String.sub s 0 (max 0 (n - 1)) else s)
  in
  let positive =
    Alcobar.map
      Alcobar.[ value_gen ]
      (fun s ->
        let sz = Wire.Codec.size_of_value codec s in
        let buf = Bytes.create sz in
        Wire.Codec.encode codec s buf 0;
        (s, buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed n;
    adversarial = bytes_fixed n;
    equal = String.equal;
    env = None;
  }

let byte_array_where n ~per_byte =
  let typ = Wire.byte_array_where ~size:(Wire.int n) ~per_byte in
  let codec = codec_of_typ typ in
  let positive_gen =
    Alcobar.map
      Alcobar.[ Alcobar.const n ]
      (fun n -> String.init n (fun _ -> Char.chr 0x20))
  in
  let positive =
    Alcobar.map
      Alcobar.[ positive_gen ]
      (fun s ->
        let buf = Bytes.create n in
        Bytes.blit_string s 0 buf 0 n;
        (s, buf))
  in
  (* Adversarial: bytes that violate the predicate (control bytes). *)
  let adversarial = bytes_fixed n in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial;
    equal = String.equal;
    env = None;
  }

(* [nested] and [nested_at_most] differ only in the typ constructor; the
   payload-into-fixed-buffer generation is identical. *)
let nested_sized make_typ n inner =
  let typ = make_typ ~size:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  (* Zero-fill the region: the codec pads a [nested] region with zeros, so the
     bytes past the inner value must be zero, not the uninitialised contents of
     [Bytes.create]. *)
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, inner_bytes) ->
        let buf = Bytes.make n '\x00' in
        Bytes.blit inner_bytes 0 buf 0 (min n (Bytes.length inner_bytes));
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.[ inner.adversarial ]
      (fun b ->
        let buf = Bytes.make n '\x00' in
        Bytes.blit b 0 buf 0 (min n (Bytes.length b));
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed n;
    adversarial;
    equal = inner.equal;
    env = None;
  }

let nested n inner = nested_sized Wire.nested n inner

let map ~decode ~encode inner =
  let typ = Wire.map ~decode ~encode inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map Alcobar.[ inner.positive ] (fun (v, bytes) -> (decode v, bytes))
  in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = (fun a b -> inner.equal (encode a) (encode b));
    env = None;
  }

let uint_var ~endian size =
  let typ = Wire.uint ~endian (Wire.int size) in
  let codec = codec_of_typ typ in
  let max_v = (1 lsl (size * 8)) - 1 in
  let value_gen = Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land max_v) in
  let encode v =
    let buf = Bytes.create size in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  let boundaries = [ 0; 1; max_v; max_v - 1 ] in
  let adversarial =
    Alcobar.map
      Alcobar.[ Alcobar.choose (List.map Alcobar.const boundaries) ]
      encode
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed size;
    adversarial;
    equal = Int.equal;
    env = None;
  }

let exact_cases ~typ ~equal cases =
  let codec = codec_of_typ typ in
  let size =
    match cases with
    | (_, bs) :: _ -> Bytes.length bs
    | [] -> invalid_arg "exact_cases: empty"
  in
  {
    codec;
    typ;
    positive = Alcobar.choose (List.map Alcobar.const cases);
    random = bytes_fixed size;
    adversarial =
      Alcobar.choose (List.map (fun (_, bs) -> Alcobar.const bs) cases);
    equal;
    env = None;
  }

let exact_int typ cases = exact_cases ~typ ~equal:Int.equal cases
let exact_int64 typ cases = exact_cases ~typ ~equal:Int64.equal cases

let uint16_endian_edges =
  exact_int Wire.uint16
    [
      (0x0001, bytes_of_octets [ 0x01; 0x00 ]);
      (0x1234, bytes_of_octets [ 0x34; 0x12 ]);
      (0x8000, bytes_of_octets [ 0x00; 0x80 ]);
      (0xFFFF, bytes_of_octets [ 0xFF; 0xFF ]);
    ]

let uint16be_endian_edges =
  exact_int Wire.uint16be
    [
      (0x0001, bytes_of_octets [ 0x00; 0x01 ]);
      (0x1234, bytes_of_octets [ 0x12; 0x34 ]);
      (0x8000, bytes_of_octets [ 0x80; 0x00 ]);
      (0xFFFF, bytes_of_octets [ 0xFF; 0xFF ]);
    ]

let uint32_endian_edges =
  exact_int Wire.uint32
    [
      (0x01234567, bytes_of_octets [ 0x67; 0x45; 0x23; 0x01 ]);
      (0x80000000, bytes_of_octets [ 0x00; 0x00; 0x00; 0x80 ]);
      (0xFFFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF ]);
    ]

let uint32be_endian_edges =
  exact_int Wire.uint32be
    [
      (0x01234567, bytes_of_octets [ 0x01; 0x23; 0x45; 0x67 ]);
      (0x80000000, bytes_of_octets [ 0x80; 0x00; 0x00; 0x00 ]);
      (0xFFFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF ]);
    ]

let uint64_endian_edges =
  exact_int64 Wire.uint64
    [
      ( 0x0123456789ABCDEFL,
        bytes_of_octets [ 0xEF; 0xCD; 0xAB; 0x89; 0x67; 0x45; 0x23; 0x01 ] );
      (-2L, bytes_of_octets [ 0xFE; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]);
    ]

let uint64be_endian_edges =
  exact_int64 Wire.uint64be
    [
      ( 0x0123456789ABCDEFL,
        bytes_of_octets [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xAB; 0xCD; 0xEF ] );
      (-2L, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFE ]);
    ]

let int16_endian_edges =
  exact_int Wire.int16
    [
      (-0x8000, bytes_of_octets [ 0x00; 0x80 ]);
      (-2, bytes_of_octets [ 0xFE; 0xFF ]);
      (0x1234, bytes_of_octets [ 0x34; 0x12 ]);
      (0x7FFF, bytes_of_octets [ 0xFF; 0x7F ]);
    ]

let int16be_endian_edges =
  exact_int Wire.int16be
    [
      (-0x8000, bytes_of_octets [ 0x80; 0x00 ]);
      (-2, bytes_of_octets [ 0xFF; 0xFE ]);
      (0x1234, bytes_of_octets [ 0x12; 0x34 ]);
      (0x7FFF, bytes_of_octets [ 0x7F; 0xFF ]);
    ]

let int32_endian_edges =
  exact_int Wire.int32
    [
      (Int32.to_int Int32.min_int, bytes_of_octets [ 0x00; 0x00; 0x00; 0x80 ]);
      (-2, bytes_of_octets [ 0xFE; 0xFF; 0xFF; 0xFF ]);
      (0x01234567, bytes_of_octets [ 0x67; 0x45; 0x23; 0x01 ]);
      (Int32.to_int Int32.max_int, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0x7F ]);
    ]

let int32be_endian_edges =
  exact_int Wire.int32be
    [
      (Int32.to_int Int32.min_int, bytes_of_octets [ 0x80; 0x00; 0x00; 0x00 ]);
      (-2, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFE ]);
      (0x01234567, bytes_of_octets [ 0x01; 0x23; 0x45; 0x67 ]);
      (Int32.to_int Int32.max_int, bytes_of_octets [ 0x7F; 0xFF; 0xFF; 0xFF ]);
    ]

let int64_endian_edges =
  exact_int64 Wire.int64
    [
      ( Int64.min_int,
        bytes_of_octets [ 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x80 ] );
      (-2L, bytes_of_octets [ 0xFE; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ]);
      ( 0x0123456789ABCDEFL,
        bytes_of_octets [ 0xEF; 0xCD; 0xAB; 0x89; 0x67; 0x45; 0x23; 0x01 ] );
      ( Int64.max_int,
        bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0x7F ] );
    ]

let int64be_endian_edges =
  exact_int64 Wire.int64be
    [
      ( Int64.min_int,
        bytes_of_octets [ 0x80; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00; 0x00 ] );
      (-2L, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFE ]);
      ( 0x0123456789ABCDEFL,
        bytes_of_octets [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xAB; 0xCD; 0xEF ] );
      ( Int64.max_int,
        bytes_of_octets [ 0x7F; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ] );
    ]

let uint_var_endian_edges ~endian size cases =
  exact_int (Wire.uint ~endian (Wire.int size)) cases

let uint_var3_little_edges =
  uint_var_endian_edges ~endian:Wire.Little 3
    [
      (0x000001, bytes_of_octets [ 0x01; 0x00; 0x00 ]);
      (0x123456, bytes_of_octets [ 0x56; 0x34; 0x12 ]);
      (0xFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF ]);
    ]

let uint_var3_big_edges =
  uint_var_endian_edges ~endian:Wire.Big 3
    [
      (0x000001, bytes_of_octets [ 0x00; 0x00; 0x01 ]);
      (0x123456, bytes_of_octets [ 0x12; 0x34; 0x56 ]);
      (0xFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF ]);
    ]

let uint_var7_little_edges =
  uint_var_endian_edges ~endian:Wire.Little 7
    [
      ( 0x0123456789ABCD,
        bytes_of_octets [ 0xCD; 0xAB; 0x89; 0x67; 0x45; 0x23; 0x01 ] );
      ( 0x7FFFFFFFFFFFFF,
        bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0x7F ] );
    ]

let uint_var7_big_edges =
  uint_var_endian_edges ~endian:Wire.Big 7
    [
      ( 0x0123456789ABCD,
        bytes_of_octets [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xAB; 0xCD ] );
      ( 0x7FFFFFFFFFFFFF,
        bytes_of_octets [ 0x7F; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ] );
    ]

let optional ?(present = true) inner =
  let f =
    Wire.Field.optional
      ~present:(if present then Wire.Expr.true_ else Wire.Expr.false_)
      "v" inner.typ
  in
  let codec =
    Wire.Codec.v "_opt" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let wrap = if present then fun v -> Some v else fun _ -> None in
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, bs) -> (wrap v, if present then bs else Bytes.empty))
  in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = Option.equal inner.equal;
    env = None;
  }

let optional_or ?(present = true) ~default inner =
  let f =
    Wire.Field.optional_or
      ~present:(if present then Wire.Expr.true_ else Wire.Expr.false_)
      ~default "v" inner.typ
  in
  let codec =
    Wire.Codec.v "_opt_or" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, bs) -> if present then (v, bs) else (default, Bytes.empty))
  in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = inner.equal;
    env = None;
  }

let list_equal eq a b = List.length a = List.length b && List.for_all2 eq a b

(* [repeat] and [repeat_seq] differ only in the codec name and the items
   field constructor (list vs seq); the length-prefixed payload generation is
   identical. *)
let repeat_sized name make_items ~bytes:total_bytes inner =
  let f_total = Wire.Field.v "_total" Wire.uint16be in
  let f_items = make_items ~size:(Wire.Field.ref f_total) inner.typ in
  let codec =
    (* The length prefix is the actual encoded items length, not the budget:
       a budget that is not a multiple of the element size encodes fewer bytes
       than the budget, and the prefix must match what is written. *)
    let items_bytes xs =
      List.fold_left
        (fun acc v -> acc + Wire.Codec.size_of_value inner.codec v)
        0 xs
    in
    Wire.Codec.v name
      (fun _ xs -> xs)
      Wire.Codec.[ f_total $ items_bytes; (f_items $ fun xs -> xs) ]
  in
  let typ = Wire.codec codec in
  let positive =
    (* Independently assemble the canonical bytes (a uint16be length prefix
       then [count] copies of the element bytes), NOT via [Codec.encode] -- the
       point of the positive case is to check the decoder/encoder against bytes
       built outside the codec, so an encoder or size bug shows up rather than
       round-tripping through itself. A zero-count list is still a 2-byte
       prefix of 0, not empty. *)
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, bs) ->
        let n = Bytes.length bs in
        let count = if n = 0 || n > total_bytes then 0 else total_bytes / n in
        let payload = count * n in
        let buf = Bytes.create (2 + payload) in
        Bytes.set_uint16_be buf 0 payload;
        for i = 0 to count - 1 do
          Bytes.blit bs 0 buf (2 + (i * n)) n
        done;
        (List.init count (fun _ -> v), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal inner.equal;
    env = None;
  }

let repeat ~bytes inner =
  repeat_sized "_rep"
    (fun ~size typ -> Wire.Field.repeat "_items" ~size typ)
    ~bytes inner

let codec_wrap (c : 'a Wire.Codec.t) ~value_gen ~equal =
  let typ = Wire.codec c in
  let encode v =
    let sz = Wire.Codec.size_of_value c v in
    let buf = Bytes.create sz in
    Wire.Codec.encode c v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  {
    codec = c;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal;
    env = None;
  }

let nested_at_most n inner = nested_sized Wire.nested_at_most n inner

let variants name cases =
  let n = max 1 (List.length cases) in
  enum_like
    ~typ:(Wire.variants name cases Wire.uint8)
    ~value_gen:
      (Alcobar.map
         Alcobar.[ Alcobar.range ~min:0 n ]
         (fun i -> snd (List.nth cases (i mod n))))
    ~random:(bytes_fixed 1) ~adversarial:(bytes_fixed 1)

let rec sample_array_of_positives positive k =
  if k = 0 then Alcobar.const ([], [])
  else
    Alcobar.dynamic_bind positive (fun (v, bs) ->
        Alcobar.map
          Alcobar.[ sample_array_of_positives positive (k - 1) ]
          (fun (vs, bss) -> (v :: vs, bs :: bss)))

let concat_bytes_list bss =
  let total = List.fold_left (fun n b -> n + Bytes.length b) 0 bss in
  let out = Bytes.create total in
  let _ =
    List.fold_left
      (fun off b ->
        let nn = Bytes.length b in
        Bytes.blit b 0 out off nn;
        off + nn)
      0 bss
  in
  out

(* [array] and [array_seq] differ only in the typ constructor. *)
let array_sized make_typ n inner =
  let typ = make_typ ~len:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ sample_array_of_positives inner.positive n ]
      (fun (vs, bss) -> (vs, concat_bytes_list bss))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal inner.equal;
    env = None;
  }

let array n inner = array_sized Wire.array n inner

let enum name cases =
  let typ = Wire.enum name cases Wire.uint8 in
  let codec = codec_of_typ typ in
  let valid_values = List.map snd cases in
  let value_gen =
    let n = max 1 (List.length valid_values) in
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 n ]
      (fun i -> List.nth valid_values (i mod n))
  in
  let encode v =
    let buf = Bytes.create 1 in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Int.equal;
    env = None;
  }

(* [Wire.enum_open]: names known codes for documentation but accepts any value,
   with no membership refinement (unlike [enum], which rejects unlisted codes).
   Every byte is a valid positive, including unlisted codes, so a regression
   that started rejecting them would fail the positive round-trip here. *)
let enum_open =
  let typ = Wire.enum_open "Code" [ ("A", 1); ("B", 2); ("C", 3) ] Wire.uint8 in
  let codec = codec_of_typ typ in
  let encode v =
    let buf = Bytes.create 1 in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive =
    Alcobar.map Alcobar.[ Alcobar.uint8 ] (fun v -> (v, encode v))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Int.equal;
    env = None;
  }

let enum_base ~typ ~size name cases =
  let codec = codec_of_typ (Wire.enum name cases typ) in
  let valid_values = List.map snd cases in
  let value_gen =
    let n = max 1 (List.length valid_values) in
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 n ]
      (fun i -> List.nth valid_values (i mod n))
  in
  let positive =
    Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode_via_codec codec v))
  in
  {
    codec;
    typ = Wire.enum name cases typ;
    positive;
    random = bytes_fixed size;
    adversarial = bytes_fixed size;
    equal = Int.equal;
    env = None;
  }

let enum_open_base ~typ ~size name cases =
  let typ = Wire.enum_open name cases typ in
  let codec = codec_of_typ typ in
  let max_value =
    if size >= Sys.int_size / 8 then max_int else (1 lsl (size * 8)) - 1
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.int ]
      (fun n ->
        let v = n land max_value in
        (v, encode_via_codec codec v))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed size;
    adversarial = bytes_fixed size;
    equal = Int.equal;
    env = None;
  }

let enum_u16be =
  enum_base ~typ:Wire.uint16be ~size:2 "WideCode"
    [ ("Zero", 0); ("One", 1); ("High", 0xBEEF) ]

let enum_open_u16be =
  enum_open_base ~typ:Wire.uint16be ~size:2 "OpenWideCode"
    [ ("Zero", 0); ("One", 1); ("High", 0xBEEF) ]

let variants_u16be =
  let typ =
    Wire.variants "WideFlag"
      [ ("Zero", `Zero); ("One", `One); ("High", `High) ]
      Wire.uint16be
  in
  let codec = codec_of_typ typ in
  let values = [ `Zero; `One; `High ] in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 (List.length values) ]
      (fun i ->
        let v = List.nth values (i mod List.length values) in
        (v, encode_via_codec codec v))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial = bytes_fixed 2;
    equal = ( = );
    env = None;
  }

(* Single-field record holding a [Wire.uint8] constrained via the field's
   [~self_constraint]. Positives are in-range values; adversarials are
   boundary samples on both sides of the range. *)
let bounded_u8 ~min ~max =
  let f =
    Wire.Field.v "v" Wire.uint8 ~self_constraint:(fun r ->
        Wire.Expr.(r >= Wire.int min && r <= Wire.int max))
  in
  let codec =
    Wire.Codec.v "_bounded" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let value_gen =
    Alcobar.map Alcobar.[ Alcobar.range ~min (max - min + 1) ] (fun n -> n)
  in
  let encode v =
    let buf = Bytes.create 1 in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  let boundary_bytes =
    let boundaries =
      List.filter
        (fun n -> n >= 0 && n <= 0xFF)
        [ min - 1; min; min + 1; max - 1; max; max + 1 ]
    in
    Alcobar.map
      Alcobar.[ Alcobar.choose (List.map Alcobar.const boundaries) ]
      (fun n ->
        let buf = Bytes.create 1 in
        Bytes.set_uint8 buf 0 n;
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = boundary_bytes;
    equal = Int.equal;
    env = None;
  }

(* [bounded_u8] generalised to any unsigned integer [inner_typ] of [size] bytes
   with a range [self_constraint]. [set] writes a boundary value as raw bytes and
   [max_val] is the type's maximum (so out-of-type boundary samples are dropped).
   Covers the constraint path on wider and big-endian scalars, not just uint8. *)
let bounded_int ~inner_typ ~size ~set ~max_val ~min ~max =
  let f =
    Wire.Field.v "v" inner_typ ~self_constraint:(fun r ->
        Wire.Expr.(r >= Wire.int min && r <= Wire.int max))
  in
  let codec =
    Wire.Codec.v "_bounded_int" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let value_gen =
    Alcobar.map Alcobar.[ Alcobar.range ~min (max - min + 1) ] Fun.id
  in
  let encode v =
    let buf = Bytes.create size in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  let boundary_bytes =
    let bs =
      List.filter
        (fun n -> n >= 0 && n <= max_val)
        [ min - 1; min; min + 1; max - 1; max; max + 1 ]
    in
    Alcobar.map
      Alcobar.[ Alcobar.choose (List.map Alcobar.const bs) ]
      (fun n ->
        let buf = Bytes.create size in
        set buf 0 n;
        buf)
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_fixed size;
    adversarial = boundary_bytes;
    equal = Int.equal;
    env = None;
  }

let bounded_u16be =
  bounded_int ~inner_typ:Wire.uint16be ~size:2 ~set:Bytes.set_uint16_be
    ~max_val:0xFFFF ~min:1000 ~max:60000

let bounded_u32be =
  bounded_int ~inner_typ:Wire.uint32be ~size:4
    ~set:(fun b o n -> Bytes.set_int32_be b o (Int32.of_int n))
    ~max_val:0x7FFF_FFFF ~min:1000 ~max:1_000_000

(* Two-uint8 record with [Codec.v ~where:(a < b)]. Positives keep a < b;
   adversarials sit at the boundary (a = b, a = b+1) so the where clause
   fires exactly when crossing the comparison. *)
let codec_where =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b = Wire.Field.v "b" Wire.uint8 in
  let codec =
    Wire.Codec.v "_lt_pair"
      ~where:Wire.Expr.(Wire.Field.ref f_a < Wire.Field.ref f_b)
      (fun a b -> (a, b))
      Wire.Codec.[ (f_a $ fun (a, _) -> a); (f_b $ fun (_, b) -> b) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 0x100; Alcobar.range ~min:0 0x100 ]
      (fun a b ->
        let a, b =
          if a < b then (a, b) else if b < 0xFF then (b, b + 1) else (0, 1)
        in
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 b;
        ((a, b), buf))
  in
  let boundary =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 0x100 ]
      (fun a ->
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 a;
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial = boundary;
    equal = ( = );
    env = None;
  }

let u8_pair_bytes a b =
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 a;
  Bytes.set_uint8 buf 1 b;
  buf

let u8_pair_positive gen =
  Alcobar.map gen (fun a b -> ((a, b), u8_pair_bytes a b))

let u8_pair_adversarial gen = Alcobar.map gen (fun a b -> u8_pair_bytes a b)

let u8_pair_record name f_a f_b ~positive ~adversarial =
  let codec =
    Wire.Codec.v name (fun a b -> (a, b)) Wire.Codec.[ f_a $ fst; f_b $ snd ]
  in
  {
    codec;
    typ = Wire.codec codec;
    positive = u8_pair_positive positive;
    random = bytes_fixed 2;
    adversarial = u8_pair_adversarial adversarial;
    equal = ( = );
    env = None;
  }

(* A field whose typ carries a [Wire.where] cond ([d : where (len < 2) uint8]).
   The cond reaches the 3D refinement, so the EverParse validator rejects
   [len >= 2]; the OCaml side must reject it too. Unlike [codec_where] (a
   codec-level [~where]) this exercises the typ-level [Wire.where] path, which
   was projected but not enforced before. Adversarials sit at and above the
   boundary so the differential catches a cond that reaches 3D but not OCaml. *)
let typ_where =
  let f_len = Wire.Field.v "len" Wire.uint8 in
  let f_d =
    Wire.Field.v "d"
      (Wire.where Wire.Expr.(Wire.Field.ref f_len < Wire.int 2) Wire.uint8)
  in
  u8_pair_record "_typ_where" f_len f_d
    ~positive:Alcobar.[ Alcobar.range ~min:0 2; Alcobar.range ~min:0 0x100 ]
    ~adversarial:
      Alcobar.[ Alcobar.range ~min:2 0x100; Alcobar.range ~min:0 0x100 ]

(* [Field.v ~constraint_] is distinct from [~self_constraint] and codec-level
   [~where]: it is attached to one field but may reference earlier fields. *)
let field_constraint =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b =
    Wire.Field.v "b" Wire.uint8
      ~constraint_:Wire.Expr.(Wire.Field.ref f_a < Wire.int 4)
  in
  u8_pair_record "_field_constraint" f_a f_b
    ~positive:Alcobar.[ Alcobar.range ~min:0 4; Alcobar.uint8 ]
    ~adversarial:Alcobar.[ Alcobar.range ~min:4 0xFC; Alcobar.uint8 ]

(* [Field.int] is a second public way to reference an integer-valued field in
   expressions. Keep it separate from the [Field.ref]-heavy cases. *)
let field_int =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b =
    Wire.Field.v "b" Wire.uint8 ~self_constraint:(fun b ->
        Wire.Expr.(Wire.Field.int f_a + b < Wire.int 10))
  in
  u8_pair_record "_field_int" f_a f_b
    ~positive:Alcobar.[ Alcobar.range ~min:0 5; Alcobar.range ~min:0 5 ]
    ~adversarial:Alcobar.[ Alcobar.range ~min:10 0xF6; Alcobar.uint8 ]

(* Full-width int64 self constraints use [Field.v ~self_int64] and
   [Field.int64], separate from native-int [~self_constraint]. *)
let self_int64 =
  let f =
    Wire.Field.v "v" Wire.uint64be ~self_int64:(fun v ->
        Wire.Expr.(v >= int64 0L && v <= int64 1000L))
  in
  let codec =
    Wire.Codec.v "_self_int64" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 1001 ]
      (fun n ->
        let v = Int64.of_int n in
        let buf = Bytes.create 8 in
        Bytes.set_int64_be buf 0 v;
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.[ Alcobar.choose [ Alcobar.const 1001L; Alcobar.const (-1L) ] ]
      (fun v ->
        let buf = Bytes.create 8 in
        Bytes.set_int64_be buf 0 v;
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 8;
    adversarial;
    equal = Int64.equal;
    env = None;
  }

(* {1 More leaves and wrappers} *)

let lookup table inner =
  let n = max 1 (List.length table) in
  enum_like
    ~typ:(Wire.lookup table inner.typ)
    ~value_gen:
      (Alcobar.map
         Alcobar.[ Alcobar.range ~min:0 n ]
         (fun i -> List.nth table (i mod n)))
    ~random:inner.random ~adversarial:inner.adversarial

let where inner =
  let typ = Wire.where Wire.Expr.true_ inner.typ in
  let codec = codec_of_typ typ in
  let positive = inner.positive in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = inner.equal;
    env = None;
  }

let bits ?bit_order ~width base =
  let typ = Wire.bits ?bit_order ~width base in
  let base_size =
    match base with Wire.U8 -> 1 | U16 | U16be -> 2 | U32 | U32be -> 4
  in
  let mask = (1 lsl width) - 1 in
  let value_gen = Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land mask) in
  let boundaries = [ 0; 1; mask; mask - 1; mask lsr 1 ] in
  scalar_int typ base_size value_gen boundaries

let bitfield_total = function
  | Wire.U8 -> 8
  | U16 | U16be -> 16
  | U32 | U32be -> 32

let word_bytes base word =
  match base with
  | Wire.U8 -> bytes_of_octets [ word ]
  | U16 -> bytes_of_octets [ word; word lsr 8 ]
  | U16be -> bytes_of_octets [ word lsr 8; word ]
  | U32 -> bytes_of_octets [ word; word lsr 8; word lsr 16; word lsr 24 ]
  | U32be -> bytes_of_octets [ word lsr 24; word lsr 16; word lsr 8; word ]

let bit_mask width = (1 lsl width) - 1

let pack_bits ~bit_order ~total fields =
  let _, word =
    List.fold_left
      (fun (used, acc) (width, value) ->
        let shift =
          match bit_order with
          | Wire.Lsb_first -> used
          | Msb_first -> total - used - width
        in
        (used + width, acc lor ((value land bit_mask width) lsl shift)))
      (0, 0) fields
  in
  word

let bit_value width =
  Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land bit_mask width)

let bitpack3 name ~base ~bit_order (w_a, w_b, w_c) =
  let f_a = Wire.Field.v "a" (Wire.bits ~bit_order ~width:w_a base) in
  let f_b = Wire.Field.v "b" (Wire.bits ~bit_order ~width:w_b base) in
  let f_c = Wire.Field.v "c" (Wire.bits ~bit_order ~width:w_c base) in
  let codec =
    Wire.Codec.v name
      (fun a b c -> (a, b, c))
      Wire.Codec.
        [
          (f_a $ fun (a, _, _) -> a);
          (f_b $ fun (_, b, _) -> b);
          (f_c $ fun (_, _, c) -> c);
        ]
  in
  let total = bitfield_total base in
  let positive =
    Alcobar.map
      Alcobar.[ bit_value w_a; bit_value w_b; bit_value w_c ]
      (fun a b c ->
        let word =
          pack_bits ~bit_order ~total [ (w_a, a); (w_b, b); (w_c, c) ]
        in
        ((a, b, c), word_bytes base word))
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_fixed (total / 8);
    adversarial = bytes_fixed (total / 8);
    equal = ( = );
    env = None;
  }

let bitpack2_spill name ~base ~bit_order (w_a, w_b) =
  let f_a = Wire.Field.v "a" (Wire.bits ~bit_order ~width:w_a base) in
  let f_b = Wire.Field.v "b" (Wire.bits ~bit_order ~width:w_b base) in
  let codec =
    Wire.Codec.v name (fun a b -> (a, b)) Wire.Codec.[ f_a $ fst; f_b $ snd ]
  in
  let total = bitfield_total base in
  let one width value =
    word_bytes base (pack_bits ~bit_order ~total [ (width, value) ])
  in
  let positive =
    Alcobar.map
      Alcobar.[ bit_value w_a; bit_value w_b ]
      (fun a b -> ((a, b), Bytes.cat (one w_a a) (one w_b b)))
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_fixed (2 * (total / 8));
    adversarial = bytes_fixed (2 * (total / 8));
    equal = ( = );
    env = None;
  }

let bitpack_split_orders =
  let f_x =
    Wire.Field.v "x" (Wire.bits ~bit_order:Wire.Msb_first ~width:4 Wire.U8)
  in
  let f_y =
    Wire.Field.v "y" (Wire.bits ~bit_order:Wire.Lsb_first ~width:4 Wire.U8)
  in
  let codec =
    Wire.Codec.v "_bitpack_split_orders"
      (fun x y -> (x, y))
      Wire.Codec.[ f_x $ fst; f_y $ snd ]
  in
  let positive =
    Alcobar.map
      Alcobar.[ bit_value 4; bit_value 4 ]
      (fun x y -> ((x, y), bytes_of_octets [ x lsl 4; y ]))
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_fixed 2;
    adversarial = bytes_fixed 2;
    equal = ( = );
    env = None;
  }

let bitpack_u8_msb =
  bitpack3 "_bitpack_u8_msb" ~base:Wire.U8 ~bit_order:Wire.Msb_first (1, 2, 5)

let bitpack_u8_lsb =
  bitpack3 "_bitpack_u8_lsb" ~base:Wire.U8 ~bit_order:Wire.Lsb_first (1, 2, 5)

let bitpack_u8_spill =
  bitpack2_spill "_bitpack_u8_spill" ~base:Wire.U8 ~bit_order:Wire.Msb_first
    (5, 5)

let bitpack_u16_msb =
  bitpack3 "_bitpack_u16_msb" ~base:Wire.U16 ~bit_order:Wire.Msb_first (3, 2, 11)

let bitpack_u16_lsb =
  bitpack3 "_bitpack_u16_lsb" ~base:Wire.U16 ~bit_order:Wire.Lsb_first (3, 2, 11)

let bitpack_u16be_msb =
  bitpack3 "_bitpack_u16be_msb" ~base:Wire.U16be ~bit_order:Wire.Msb_first
    (3, 2, 11)

let bitpack_u16be_lsb =
  bitpack3 "_bitpack_u16be_lsb" ~base:Wire.U16be ~bit_order:Wire.Lsb_first
    (3, 2, 11)

let bitpack_u16be_spill =
  bitpack2_spill "_bitpack_u16be_spill" ~base:Wire.U16be
    ~bit_order:Wire.Lsb_first (9, 9)

let bitpack_u32_msb =
  bitpack3 "_bitpack_u32_msb" ~base:Wire.U32 ~bit_order:Wire.Msb_first
    (6, 10, 16)

let bitpack_u32_lsb =
  bitpack3 "_bitpack_u32_lsb" ~base:Wire.U32 ~bit_order:Wire.Lsb_first
    (6, 10, 16)

let bitpack_u32be_msb =
  bitpack3 "_bitpack_u32be_msb" ~base:Wire.U32be ~bit_order:Wire.Msb_first
    (6, 10, 16)

let bitpack_u32be_lsb =
  bitpack3 "_bitpack_u32be_lsb" ~base:Wire.U32be ~bit_order:Wire.Lsb_first
    (6, 10, 16)

let bit inner =
  let typ = Wire.bit inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.bool ]
      (fun b ->
        let sz = Wire.Codec.size_of_value codec b in
        let buf = Bytes.create sz in
        Wire.Codec.encode codec b buf 0;
        (b, buf))
  in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = Bool.equal;
    env = None;
  }

let array_seq n inner = array_sized (Wire.array_seq Wire.seq_list) n inner

let repeat_seq ~bytes inner =
  repeat_sized "_rep_seq"
    (fun ~size typ ->
      Wire.Field.repeat_seq "_items" ~seq:Wire.seq_list ~size typ)
    ~bytes inner

(* Two-uint8 record where the second field's [~constraint_] references
   the first via [Field.anon]. Exercises the anon constructor. *)
let field_anon =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let anon = Wire.Field.anon Wire.uint8 in
  let _ = anon in
  let codec =
    Wire.Codec.v "_anon"
      (fun a b -> (a, b))
      Wire.Codec.
        [
          (f_a $ fun (a, _) -> a);
          (Wire.Field.v "b" Wire.uint8 $ fun (_, b) -> b);
        ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8; Alcobar.uint8 ]
      (fun a b ->
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 b;
        ((a, b), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial = bytes_fixed 2;
    equal = ( = );
    env = None;
  }

(* Codec referencing [Param.input] in its [~where]. Each test run binds
   a fresh env with the input set, so adversarials can violate the
   constraint without spilling state. *)
let param_input =
  let limit = Wire.Param.input "limit" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  let codec =
    Wire.Codec.v "_param_in"
      ~where:Wire.Expr.(Wire.Field.ref f_v <= Wire.Param.expr limit)
      (fun v -> v)
      Wire.Codec.[ (f_v $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let strategy =
    {
      positive = (fun env -> Wire.Param.bind limit 100 env);
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint8 ]
          (fun lim env -> Wire.Param.bind limit lim env);
    }
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 101 ]
      (fun v ->
        let buf = Bytes.create 1 in
        Bytes.set_uint8 buf 0 v;
        (v, buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Int.equal;
    env = Some strategy;
  }

(* A 1-byte positive: a uint8 value written to a single-byte buffer. *)
let u8_positive =
  Alcobar.map
    Alcobar.[ Alcobar.uint8 ]
    (fun v ->
      let buf = Bytes.create 1 in
      Bytes.set_uint8 buf 0 v;
      (v, buf))

(* A single-uint8 codec whose field carries [action], with an env strategy
   that rebuilds the (param-free) env on demand. *)
let action_codec name action =
  let f = Wire.Field.v "v" ~action Wire.uint8 in
  let codec = Wire.Codec.v name (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ] in
  (* Output params only: nothing to bind, so the binder is the identity. *)
  {
    codec;
    typ = Wire.codec codec;
    positive = u8_positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Int.equal;
    env = Some { positive = Fun.id; fuzz = Alcobar.const Fun.id };
  }

(* Codec with [Action.on_success [assign out (Field.ref f); abort?]]
   exercising Action.assign, return_bool, abort, if_, var, on_act. *)
let action =
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  action_codec "_action"
    (Wire.Action.on_success
       [
         Wire.Action.var "local" (Wire.Field.ref f_v);
         Wire.Action.assign out (Wire.Field.ref f_v);
         Wire.Action.if_ Wire.Expr.true_
           [ Wire.Action.return_bool Wire.Expr.true_ ]
           None;
       ])

(* Codec whose field [~action] is [Action.abort]: every successful field
   parse triggers an unconditional failure. The driver uses
   {!reject_cases} since no input bytes can decode successfully. *)
let action_abort =
  let f =
    Wire.Field.v "v"
      ~action:(Wire.Action.on_success [ Wire.Action.abort ])
      Wire.uint8
  in
  let codec =
    Wire.Codec.v "_abort" (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  {
    codec;
    typ;
    (* Positive is unused by [reject_cases]; kept for type-shape parity. *)
    positive = u8_positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Int.equal;
    env = None;
  }

(* Same as [action] but using [Action.on_act] in place of
   [on_success]. *)
let action_on_act =
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  action_codec "_action_on_act"
    (Wire.Action.on_act
       [
         Wire.Action.assign out (Wire.Field.ref f_v);
         Wire.Action.return_bool Wire.Expr.true_;
       ])

(* Single-float codec whose [~where] is [Wire.is_nan f]. Positives are
   NaN bit patterns; non-NaN bytes are rejected. *)
let nan_float64 =
  let f = Wire.Field.v "v" Wire.float64 in
  let codec =
    Wire.Codec.v "_nan" ~where:(Wire.is_nan f)
      (fun v -> v)
      Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let nan_bit_patterns =
    [
      0x7FF8_0000_0000_0001L;
      0xFFF8_0000_0000_0001L;
      Int64.bits_of_float Float.nan;
    ]
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.choose (List.map Alcobar.const nan_bit_patterns) ]
      (fun bits ->
        let v = Int64.float_of_bits bits in
        let buf = Bytes.create 8 in
        Bytes.set_int64_le buf 0 bits;
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.
        [ Alcobar.choose (List.map Alcobar.const [ 0.0; 1.0; Float.infinity ]) ]
      (fun v ->
        let buf = Bytes.create 8 in
        Bytes.set_int64_le buf 0 (Int64.bits_of_float v);
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 8;
    adversarial;
    equal = float64_bits_equal;
    env = None;
  }

(* Single-field float codec with [~self_constraint:is_finite]. Adversarial
   bytes include NaN and infinity bit patterns. *)
let finite_float64 =
  let f = Wire.Field.v "v" Wire.float64 in
  let codec =
    Wire.Codec.v "_finite" ~where:(Wire.is_finite f)
      (fun v -> v)
      Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.float ]
      (fun v ->
        let v = if Float.is_finite v then v else 0.0 in
        let buf = Bytes.create 8 in
        Wire.Codec.encode codec v buf 0;
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.
        [
          Alcobar.choose
            (List.map Alcobar.const
               [ Float.nan; Float.infinity; Float.neg_infinity ]);
        ]
      (fun v ->
        let buf = Bytes.create 8 in
        Bytes.set_int64_be buf 0 (Int64.bits_of_float v);
        buf)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 8;
    adversarial;
    equal = float64_bits_equal;
    env = None;
  }

(* Codec whose constraint exercises every integer Expr operator
   (arithmetic, bitwise, comparison, logical, casts). Positives satisfy
   the chained predicate; adversarials probably do not. *)
let build_expr_ops_pred a b =
  let module E = Wire.Expr in
  let arith = E.((a + b - (a * Wire.int 1)) / Wire.int 1 mod Wire.int 256) in
  let bw = E.(a land Wire.int 0xFF lor (a lxor a)) in
  let shifted = E.((a lsl Wire.int 0) lsr Wire.int 0) in
  let casts = E.(to_uint8 a + to_uint16 b + to_uint32 a + to_uint64 b) in
  let lnotted = E.(lnot (lnot a)) in
  E.(
    arith >= Wire.int 0
    && bw >= Wire.int 0
    && shifted >= Wire.int 0
    && casts >= Wire.int 0
    && lnotted >= Wire.int 0
    && a = a
    && a <> Wire.int (-1)
    && a >= Wire.int 0
    && a > Wire.int (-1)
    && b <= Wire.int 0xFF
    && not (bool false)
    || true_)

let expr_ops =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b = Wire.Field.v "b" Wire.uint8 in
  let codec =
    Wire.Codec.v "_expr_ops"
      ~where:(build_expr_ops_pred (Wire.Field.ref f_a) (Wire.Field.ref f_b))
      (fun a b -> (a, b))
      Wire.Codec.[ (f_a $ fun (a, _) -> a); (f_b $ fun (_, b) -> b) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8; Alcobar.uint8 ]
      (fun a b ->
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 b;
        ((a, b), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial = bytes_fixed 2;
    equal = ( = );
    env = None;
  }

(* Codec ending in [rest_bytes] whose tail length is computed from a
   bound [Param.input "total"]. *)
let rest_bytes =
  let total = Wire.Param.input "total" Wire.uint16be in
  let f_hdr = Wire.Field.v "hdr" Wire.uint8 in
  let f_tail = Wire.Field.v "tail" (Wire.rest_bytes total) in
  let codec =
    Wire.Codec.v "_rest"
      (fun h t -> (h, t))
      Wire.Codec.[ (f_hdr $ fun (h, _) -> h); (f_tail $ fun (_, t) -> t) ]
  in
  let typ = Wire.codec codec in
  let tail_len = 5 in
  let strategy =
    {
      positive = (fun env -> Wire.Param.bind total (1 + tail_len) env);
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint16 ]
          (fun n env -> Wire.Param.bind total n env);
    }
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8; Alcobar.bytes_fixed tail_len ]
      (fun h tail ->
        let buf = Bytes.create (1 + tail_len) in
        Bytes.set_uint8 buf 0 h;
        Bytes.blit_string tail 0 buf 1 tail_len;
        ((h, tail), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed (1 + tail_len);
    adversarial = bytes_fixed (1 + tail_len);
    equal = ( = );
    env = Some strategy;
  }

(* A [byte_array] whose [~size] is a bound [Param.input], bound through
   [bind_by_name] (the by-name binder the differential harness uses) rather than
   the typed [Param.bind] every other param gen uses. A binder that does not
   reach the param's runtime cell resolves the size to 0 and truncates the field,
   so it shows up here as a positive decode mismatch. *)
let param_size =
  let n = Wire.Param.input "n" Wire.uint8 in
  let f = Wire.Field.v "data" (Wire.byte_array ~size:(Wire.Param.expr n)) in
  let codec =
    Wire.Codec.v "_param_size" (fun d -> d) Wire.Codec.[ (f $ fun d -> d) ]
  in
  let typ = Wire.codec codec in
  let data_len = 4 in
  let strategy =
    {
      positive = (fun env -> Wire.Param.bind_by_name "n" data_len env);
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint8 ]
          (fun k env -> Wire.Param.bind_by_name "n" k env);
    }
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.bytes_fixed data_len ]
      (fun b -> (b, bytes_of_string b))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = String.equal;
    env = Some strategy;
  }

(* [Wire.Expr.if_then_else] driving a field size: the [len] byte selects the
   payload width, with 0 meaning a fixed fallback (the documented "0 means N"
   shape). Exercises the ternary on the decode size path. *)
let if_then_else =
  let f_len = Wire.Field.v "len" Wire.uint8 in
  let f_data =
    Wire.Field.v "data"
      (Wire.byte_array
         ~size:
           Wire.Expr.(
             if_then_else
               (Wire.Field.ref f_len = Wire.int 0)
               (Wire.int 3) (Wire.Field.ref f_len)))
  in
  let codec =
    Wire.Codec.v "_ite"
      (fun len data -> (len, data))
      Wire.Codec.[ f_len $ fst; f_data $ snd ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.dynamic_bind (Alcobar.range ~min:0 6) (fun len ->
        let sz = if len = 0 then 3 else len in
        Alcobar.map
          Alcobar.[ Alcobar.bytes_fixed sz ]
          (fun b ->
            let buf = Bytes.create (1 + sz) in
            Bytes.set_uint8 buf 0 len;
            Bytes.blit_string b 0 buf 1 sz;
            ((len, b), buf)))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
  }

(* Two-field codec whose second field's [~self_constraint] references
   [Wire.sizeof_this], [Wire.field_pos], and [Wire.sizeof]. *)
let sizeof =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b =
    Wire.Field.v "b" Wire.uint8 ~self_constraint:(fun _ ->
        Wire.Expr.(
          Wire.sizeof_this = Wire.int 1
          && Wire.field_pos = Wire.int 1
          && Wire.sizeof Wire.uint8 = Wire.int 1))
  in
  let codec =
    Wire.Codec.v "_sizeof"
      (fun a b -> (a, b))
      Wire.Codec.[ (f_a $ fun (a, _) -> a); (f_b $ fun (_, b) -> b) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8; Alcobar.uint8 ]
      (fun a b ->
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 b;
        ((a, b), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial = bytes_fixed 2;
    equal = ( = );
    env = None;
  }

(* A uint8 [gate] field followed by a [mk_payload]-built payload field that
   reads the gate (the shared shape behind the dynamic optional leaves). *)
let gate_codec name mk_payload positive =
  let f_gate = Wire.Field.v "gate" Wire.uint8 in
  let f_payload = mk_payload f_gate in
  let codec =
    Wire.Codec.v name
      (fun gate payload -> (gate, payload))
      Wire.Codec.[ (f_gate $ fun (g, _) -> g); (f_payload $ fun (_, p) -> p) ]
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
  }

let gate_present f_gate = Wire.Expr.(Wire.Field.ref f_gate <> Wire.int 0)

(* The present-branch bytes shared by both dynamic optional leaves: gate byte
   set to 1, then the uint16be payload. *)
let gate_on_bytes v =
  let buf = Bytes.create 3 in
  Bytes.set_uint8 buf 0 1;
  Bytes.set_uint16_be buf 1 v;
  buf

let gate_positive ~present_value ~absent =
  Alcobar.map
    Alcobar.[ Alcobar.bool; Alcobar.uint16 ]
    (fun present v ->
      if present then (present_value v, gate_on_bytes v) else absent ())

(* Dynamic-gate optional: a uint8 [gate] field controls whether a uint16be
   payload is present via [Field.optional ~present:(Field.ref gate <> 0)]. *)
let optional_dynamic =
  gate_codec "_opt_dyn"
    (fun g ->
      Wire.Field.optional "payload" ~present:(gate_present g) Wire.uint16be)
    (gate_positive
       ~present_value:(fun v -> (1, Some v))
       ~absent:(fun () -> ((0, None), bytes_of_octets [ 0 ])))

(* Dynamic-gate optional_or: same shape, with a default value used when
   absent. *)
let optional_or_dynamic =
  gate_codec "_opt_or_dyn"
    (fun g ->
      Wire.Field.optional_or "payload" ~present:(gate_present g) ~default:0xCAFE
        Wire.uint16be)
    (gate_positive
       ~present_value:(fun v -> (1, v))
       ~absent:(fun () ->
         let buf = Bytes.create 3 in
         Bytes.set_uint8 buf 0 0;
         Bytes.set_uint16_be buf 1 0xCAFE;
         ((0, 0xCAFE), buf)))

(* {1 Casetype} *)

type 'a case =
  | Case : {
      index : int option;
      default_tag : int option;
      inner : 'w t;
      inject : 'w -> 'a;
      project : 'a -> 'w option;
    }
      -> 'a case

let case ~index inner ~inject ~project =
  Case { index = Some index; default_tag = None; inner; inject; project }

let default_case ~tag inner ~inject ~project =
  Case { index = None; default_tag = Some tag; inner; inject; project }

(* Combine the env strategies of a composite's parts: bind every part's params
   (by name) on the composite's own env. A record or casetype that contains a
   param leaf can then bind it, now that the composite codec surfaces the leaf's
   param ([Codec.v]'s param forwarding). The binders compose, so several param
   parts all bind on one env. *)
let combine_env_strategies (strategies : env_strategy option list) :
    env_strategy option =
  match List.filter_map Fun.id strategies with
  | [] -> None
  | ss ->
      let positive env =
        List.fold_left (fun e (s : env_strategy) -> s.positive e) env ss
      in
      let fuzz =
        List.fold_left
          (fun acc (s : env_strategy) ->
            Alcobar.map Alcobar.[ acc; s.fuzz ] (fun f g env -> g (f env)))
          (Alcobar.const Fun.id) ss
      in
      Some { positive; fuzz }

(* Convert one composer case to a Wire casetype branch. The composer's default
   branch ignores the matched tag and always re-encodes the fixed [t], so adapt
   to the tag-aware API. *)
let casetype_case_def (Case c) =
  match (c.index, c.default_tag) with
  | Some i, _ ->
      Wire.case ~index:i c.inner.typ ~inject:c.inject ~project:c.project
  | None, Some t ->
      Wire.default c.inner.typ
        ~inject:(fun _tag w -> c.inject w)
        ~project:(fun a -> Option.map (fun w -> (t, w)) (c.project a))
  | None, None -> invalid_arg "casetype_u8: case must supply ~index or ~tag"

let casetype_u8 name cases =
  let typ = Wire.casetype name Wire.uint8 (List.map casetype_case_def cases) in
  let codec = codec_of_typ typ in
  let n = max 1 (List.length cases) in
  let env_strategy =
    combine_env_strategies (List.map (fun (Case c) -> c.inner.env) cases)
  in
  let positive =
    Alcobar.dynamic_bind (Alcobar.range ~min:0 n) (fun i ->
        let (Case c) = List.nth cases (i mod n) in
        Alcobar.map
          Alcobar.[ c.inner.positive ]
          (fun (w, _inner_bytes) ->
            let v = c.inject w in
            let env =
              Option.map
                (fun (s : env_strategy) -> s.positive (Wire.Codec.env codec))
                env_strategy
            in
            let buf = Bytes.create (Wire.Codec.size_of_value codec v) in
            Wire.Codec.encode ?env codec v buf 0;
            (v, buf)))
  in
  (* Compare two casetype values through the case each projects to, using that
     case's own [equal]. Structural [( = )] is wrong when a case body is a
     [byte_slice], whose decoded value views a different buffer than the
     original even when the bytes match. *)
  let equal a b =
    List.exists
      (fun (Case c) ->
        match (c.project a, c.project b) with
        | Some wa, Some wb -> c.inner.equal wa wb
        | _ -> false)
      cases
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal;
    env = env_strategy;
  }

(* A direct [Wire.casetype] over a wider discriminator, with a default branch
   that preserves the actual unclaimed tag. This mirrors [Wire.default]'s API
   more closely than [casetype_u8]'s fixed re-encode tag helper. *)
let casetype_u16be_default =
  let typ =
    Wire.casetype "WideTagged" Wire.uint16be
      [
        Wire.case ~index:0x0102 Wire.uint8
          ~inject:(fun v -> `A v)
          ~project:(function `A v -> Some v | _ -> None);
        Wire.default Wire.uint16be
          ~inject:(fun tag v -> `Other (tag, v))
          ~project:(function `Other (tag, v) -> Some (tag, v) | _ -> None);
      ]
  in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.dynamic_bind Alcobar.bool (function
      | true ->
          Alcobar.map
            Alcobar.[ Alcobar.uint8 ]
            (fun v ->
              let buf = Bytes.create 3 in
              Bytes.set_uint16_be buf 0 0x0102;
              Bytes.set_uint8 buf 2 v;
              (`A v, buf))
      | false ->
          Alcobar.map
            Alcobar.[ Alcobar.uint16; Alcobar.uint16 ]
            (fun tag v ->
              let tag = if tag = 0x0102 then 0x0103 else tag in
              let buf = Bytes.create 4 in
              Bytes.set_uint16_be buf 0 tag;
              Bytes.set_uint16_be buf 2 v;
              (`Other (tag, v), buf)))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
  }

(* {1 Record composition} *)

module Codec = struct
  type ('a, 'r) field = { name : string; gen : 'a t; getter : 'r -> 'a }

  type ('f, 'r) fields =
    | [] : ('r, 'r) fields
    | ( :: ) : ('a, 'r) field * ('f, 'r) fields -> ('a -> 'f, 'r) fields

  let ( $ ) gen getter = { name = "_"; gen; getter }

  (* Assign each field a unique name by position: the [$] above leaves every
     field [_], which round-trips fine (getters, not names) but projects to
     clashing anonymous declarations in 3D. *)
  let wire_codec_fields fields =
    let rec go : type f r. int -> (f, r) fields -> (f, r) Wire.Codec.fields =
     fun i -> function
       | [] -> Wire.Codec.[]
       | f :: rest ->
           let name = if f.name = "_" then Fmt.str "f%d" i else f.name in
           Wire.Codec.( $ ) (Wire.Field.v name f.gen.typ) f.getter
           :: go (i + 1) rest
    in
    go 0 fields

  (* Walk the field list applying each field's positive sample to the
     partial builder and accumulating the bytes. Mirrors [Wire.Codec.v]'s
     application order. *)
  let rec positives_of : type f r.
      f -> bytes list -> (f, r) fields -> (r * bytes) Alcobar.gen =
   fun partial bytes_acc fields ->
    match fields with
    | [] ->
        let concat =
          let total =
            List.fold_left (fun n b -> n + Bytes.length b) 0 bytes_acc
          in
          let out = Bytes.create total in
          let _ =
            List.fold_left
              (fun off b ->
                let n = Bytes.length b in
                Bytes.blit b 0 out off n;
                off + n)
              0 (List.rev bytes_acc)
          in
          out
        in
        Alcobar.const (partial, concat)
    | f :: rest ->
        Alcobar.dynamic_bind f.gen.positive (fun (v, bytes) ->
            positives_of (partial v) (bytes :: bytes_acc) rest)

  (* Random byte stream for a compound: each field generates random
     bytes; concatenate. *)
  let rec random_of : type f r. (f, r) fields -> bytes Alcobar.gen = function
    | [] -> Alcobar.const Bytes.empty
    | f :: rest ->
        Alcobar.map
          Alcobar.[ f.gen.random; random_of rest ]
          (fun a b ->
            let out = Bytes.create (Bytes.length a + Bytes.length b) in
            Bytes.blit a 0 out 0 (Bytes.length a);
            Bytes.blit b 0 out (Bytes.length a) (Bytes.length b);
            out)

  (* Adversarial: one slot at a time uses its adversarial stream, every
     other slot uses a positive sample. The "one bad slot" shape is the
     adversarial input the decoder is most likely to mishandle; many
     concurrent bad slots devolves to [random]. *)
  let rec field_positives : type f r. (f, r) fields -> bytes Alcobar.gen list =
    function
    | [] -> []
    | f :: rest ->
        let pos = Alcobar.map Alcobar.[ f.gen.positive ] snd in
        pos :: field_positives rest

  let rec adversarial_at : type f r.
      int -> bytes Alcobar.gen list -> (f, r) fields -> bytes Alcobar.gen list =
   fun i positives fields ->
    match (fields, positives) with
    | [], _ | _, [] -> []
    | f :: rest, p :: ps ->
        let slot = if i = 0 then f.gen.adversarial else p in
        slot :: adversarial_at (i - 1) ps rest

  let rec concat_bytes_gens : bytes Alcobar.gen list -> bytes Alcobar.gen =
    function
    | [] -> Alcobar.const Bytes.empty
    | [ g ] -> g
    | g :: rest ->
        Alcobar.map
          Alcobar.[ g; concat_bytes_gens rest ]
          (fun a b ->
            let out = Bytes.create (Bytes.length a + Bytes.length b) in
            Bytes.blit a 0 out 0 (Bytes.length a);
            Bytes.blit b 0 out (Bytes.length a) (Bytes.length b);
            out)

  let rec field_count : type f r. (f, r) fields -> int = function
    | [] -> 0
    | _ :: rest -> 1 + field_count rest

  let rec field_envs : type f r. (f, r) fields -> env_strategy option list =
    function
    | [] -> []
    | f :: rest -> f.gen.env :: field_envs rest

  let v : type f r.
      string -> ?equal:(r -> r -> bool) -> f -> (f, r) fields -> r t =
   fun name ?(equal = ( = )) builder fields ->
    let codec = Wire.Codec.v name builder (wire_codec_fields fields) in
    let typ = Wire.codec codec in
    let positives = field_positives fields in
    let n_fields = field_count fields in
    (* The canonical bytes for a record value are its encoding, not the
       concatenation of each field's sample bytes: adjacent bitfields coalesce
       into one base word, so concatenating their per-field bytes would place
       the second field in the wrong byte. Build the value, then encode it.
       A record carrying a param field ([param_size] / [rest_bytes]) needs the
       env threaded into [encode], else it raises and the concat fallback would
       ship the unpacked bytes; bind the field params first. Fall back to the
       test runner if no env makes encode succeed: that is either a library bug
       or a fuzzer-model bug, and hiding it would make later positives
       misleading. *)
    let env_strategy = combine_env_strategies (field_envs fields) in
    let positive =
      Alcobar.map
        Alcobar.[ positives_of builder [] fields ]
        (fun (v, _concat) ->
          let env =
            Option.map
              (fun (s : env_strategy) -> s.positive (Wire.Codec.env codec))
              env_strategy
          in
          let buf = Bytes.create (Wire.Codec.size_of_value codec v) in
          Wire.Codec.encode ?env codec v buf 0;
          (v, buf))
    in
    let random = random_of fields in
    let adversarial =
      let per_slot =
        List.init n_fields (fun i ->
            concat_bytes_gens (adversarial_at i positives fields))
      in
      if per_slot = [] then Alcobar.const Bytes.empty
      else Alcobar.choose per_slot
    in
    {
      codec;
      typ;
      positive;
      random;
      adversarial;
      equal;
      env = combine_env_strategies (field_envs fields);
    }
end

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

let add_size a b =
  match (a, b) with Some x, Some y -> Some (x + y) | _ -> None

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
  Any { g = repeat ~bytes:12 a.g; size = None; label = "rep:" ^ a.label }

let repeat_seq_of (Any a) =
  Any { g = repeat_seq ~bytes:12 a.g; size = None; label = "reps:" ^ a.label }

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

let pair_of (Any a) (Any b) =
  let g =
    Codec.v "R2"
      ~equal:(fun (x1, y1) (x2, y2) -> a.g.equal x1 x2 && b.g.equal y1 y2)
      (fun x y -> (x, y))
      Codec.[ a.g $ fst; b.g $ snd ]
  in
  Any
    {
      g;
      size = add_size a.size b.size;
      label = "(" ^ a.label ^ "," ^ b.label ^ ")";
    }

(* Flat records of arity 3 and 4: like [pair_of] but for more fields, so the
   Boltzmann sampler can emit single-typedef records wider than a pair. Each
   unpacks its [Any] elements and seals a flat [Codec.v] (no nesting, so the 3D
   projection stays a single typedef). *)
let rec3_of (Any a) (Any b) (Any c) =
  let g =
    Codec.v "R3"
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
      size = add_size a.size (add_size b.size c.size);
      label = Fmt.str "(%s,%s,%s)" a.label b.label c.label;
    }

let rec4_of (Any a) (Any b) (Any c) (Any d) =
  let g =
    Codec.v "R4"
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
      size = add_size a.size (add_size b.size (add_size c.size d.size));
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

(* [gen_fixed] yields fixed-size nodes (for [array] / [nested] inners, which
   need a known element width); [gen_any] yields any node. Both compose the
   full combinator set so the fuzzer reaches every nesting -- the point is to
   surface compositions Wire mishandles, not to pre-filter them. *)
let rec gen_fixed depth : any Alcobar.gen =
  let leaves = List.map Alcobar.const fixed_leaves in
  if depth <= 0 then Alcobar.choose leaves
  else
    Alcobar.choose
      (leaves
      @ [
          bind1_opt (gen_fixed (depth - 1)) nested_of;
          bind1_opt (gen_fixed (depth - 1)) nested_at_most_of;
          bind1_opt (gen_fixed (depth - 1)) (array_of 2);
          bind1_opt (gen_fixed (depth - 1)) (array_seq_of 2);
          bind1 (gen_fixed (depth - 1)) map_of;
          bind1 (gen_fixed (depth - 1)) where_of;
          bind2 (gen_fixed (depth - 1)) (gen_fixed (depth - 1)) pair_of;
          bind3
            (gen_fixed (depth - 1))
            (gen_fixed (depth - 1))
            (gen_fixed (depth - 1))
            rec3_of;
          bind4
            (gen_fixed (depth - 1))
            (gen_fixed (depth - 1))
            (gen_fixed (depth - 1))
            (gen_fixed (depth - 1))
            rec4_of;
        ])

let rec gen_any depth : any Alcobar.gen =
  let leaves =
    List.map Alcobar.const (fixed_leaves @ var_leaves @ env_leaves)
  in
  if depth <= 0 then Alcobar.choose leaves
  else
    Alcobar.choose
      (leaves
      @ [
          bind1 (gen_any (depth - 1)) optional_of;
          bind1 (gen_any (depth - 1)) repeat_of;
          bind1 (gen_any (depth - 1)) repeat_seq_of;
          bind1 (gen_any (depth - 1)) map_of;
          bind1 (gen_any (depth - 1)) where_of;
          bind1_opt (gen_fixed (depth - 1)) nested_of;
          bind1_opt (gen_fixed (depth - 1)) nested_at_most_of;
          bind1_opt (gen_fixed (depth - 1)) (array_of 2);
          bind1_opt (gen_fixed (depth - 1)) (array_seq_of 2);
          bind2 (gen_any (depth - 1)) (gen_any (depth - 1)) pair_of;
          bind3
            (gen_any (depth - 1))
            (gen_any (depth - 1))
            (gen_any (depth - 1))
            rec3_of;
          bind4
            (gen_any (depth - 1))
            (gen_any (depth - 1))
            (gen_any (depth - 1))
            (gen_any (depth - 1))
            rec4_of;
          bind2 (gen_any (depth - 1)) (gen_any (depth - 1)) casetype_of;
        ])

(* {1 Test driver} *)

(* Wrap each of a gen's three streams in an Alcobar test_case. Positive
   asserts round-trip; the other two assert crash-safety. *)
let positive_env g =
  match g.env with
  | Some s -> Some (s.positive (Wire.Codec.env g.codec))
  | None -> None

(* Round-trip one positive sample: decode the canonical bytes back to the
   value, then re-encode into a buffer sized from [size_of_value] and require
   the two to agree. Sizing from [size_of_value] (rather than from the
   canonical bytes) drives the value-driven size for every combinator,
   including ones whose [positive] bytes are hand-assembled (casetype,
   nested_at_most) and so never otherwise exercise [encode] / [size_of_value].
   A wrong [size_of_value] then shows up here as a mismatch -- or, if it
   under-counts, as an [encode] overrun -- rather than silently. *)
let check_positive_decode label g value bs env =
  (match Wire.Codec.decode ?env g.codec bs 0 with
  | Ok decoded ->
      if not (g.equal decoded value) then
        Alcobar.failf "%s positive decode mismatch" label
  | Error _ -> Alcobar.failf "%s positive decode failed" label);
  match Wire.Codec.decode_exn ?env g.codec bs 0 with
  | decoded ->
      if not (g.equal decoded value) then
        Alcobar.failf "%s positive decode_exn mismatch" label
  | exception Wire.Parse_error e ->
      Alcobar.failf "%s positive decode_exn failed: %a" label
        Wire.pp_parse_error e

let check_positive_size label g value bs =
  let sz = Wire.Codec.size_of_value g.codec value in
  if sz <> Bytes.length bs then
    Alcobar.failf "%s size_of_value = %d but canonical encoding is %d" label sz
      (Bytes.length bs);
  sz

let check_positive_size_metadata label g bs =
  if Option.is_none g.env then begin
    let min_sz = Wire.Codec.min_wire_size g.codec in
    if min_sz > Bytes.length bs then
      Alcobar.failf "%s min_wire_size = %d but canonical encoding is %d" label
        min_sz (Bytes.length bs);
    (match Wire.Codec.wire_size_at g.codec bs 0 with
    | actual ->
        if actual <> Bytes.length bs then
          Alcobar.failf "%s wire_size_at = %d but canonical encoding is %d"
            label actual (Bytes.length bs)
    | exception Invalid_argument _ ->
        Alcobar.failf "%s wire_size_at raised on a positive" label);
    if Wire.Codec.is_fixed g.codec then
      match Wire.Codec.wire_size g.codec with
      | fixed ->
          if fixed <> Bytes.length bs then
            Alcobar.failf "%s wire_size = %d but canonical encoding is %d" label
              fixed (Bytes.length bs)
      | exception Invalid_argument _ ->
          Alcobar.failf "%s fixed codec raised in wire_size" label
  end

let check_positive_encode label g value bs env sz =
  let bs_str = string_of_bytes bs in
  let out = Bytes.create sz in
  (try Wire.Codec.encode ?env g.codec value out 0
   with Invalid_argument _ -> Alcobar.failf "%s positive encode raised" label);
  if string_of_bytes out <> bs_str then
    Alcobar.failf "%s positive reencode mismatch" label

let check_positive label g (value, bs) =
  let env = positive_env g in
  check_positive_decode label g value bs env;
  let sz = check_positive_size label g value bs in
  check_positive_size_metadata label g bs;
  check_positive_encode label g value bs env sz

let validate_one ?env g bs =
  try
    Wire.Codec.validate ?env g.codec bs 0;
    `Ok
  with
  | Wire.Validation_error _ -> `Reject
  | Invalid_argument _ -> `Crash
  | Wire.Parse_error _ -> `Reject

let check_validate_positive label g bs =
  let env = positive_env g in
  match validate_one ?env g bs with
  | `Ok -> ()
  | `Reject -> Alcobar.failf "%s validate rejected a positive" label
  | `Crash -> Alcobar.failf "%s validate crashed on a positive" label

let check_validate_safety label kind ?env g bs =
  match validate_one ?env g bs with
  | `Ok | `Reject -> ()
  | `Crash -> Alcobar.failf "%s validate %s crashed" label kind

let check_decode_safety label kind ?env g bs =
  try ignore (Wire.Codec.decode ?env g.codec bs 0)
  with Invalid_argument _ -> Alcobar.failf "%s %s crashed decoder" label kind

let decode_accepts ?env g bs =
  try
    match Wire.Codec.decode ?env g.codec bs 0 with
    | Ok _ -> `Accept
    | Error _ -> `Reject
  with Invalid_argument _ -> `Crash

let validate_accepts ?env g bs =
  match validate_one ?env g bs with
  | `Ok -> `Accept
  | `Reject -> `Reject
  | `Crash -> `Crash

let check_decode_validate_agree label kind ?env g bs =
  match (decode_accepts ?env g bs, validate_accepts ?env g bs) with
  | `Crash, _ -> Alcobar.failf "%s %s crashed decoder" label kind
  | _, `Crash -> Alcobar.failf "%s %s crashed validator" label kind
  | `Accept, `Accept | `Reject, `Reject -> ()
  | `Accept, `Reject ->
      Alcobar.failf "%s %s decode accepted but validate rejected" label kind
  | `Reject, `Accept ->
      Alcobar.failf "%s %s validate accepted but decode rejected" label kind

let test_cases ?(validate = true) label g =
  let check_positive_stream ((_, bs) as sample) =
    check_positive label g sample;
    if validate then check_validate_positive label g bs
  in
  let check_safety_stream kind ?env bs =
    check_decode_safety label kind ?env g bs;
    if validate then begin
      check_validate_safety label kind ?env g bs;
      check_decode_validate_agree label kind ?env g bs
    end
  in
  match g.env with
  | None ->
      [
        Alcobar.test_case (label ^ " all")
          Alcobar.[ g.positive; g.random; g.adversarial ]
          (fun positive random adversarial ->
            check_positive_stream positive;
            check_safety_stream "random" random;
            check_safety_stream "adversarial" adversarial);
      ]
  | Some s ->
      [
        Alcobar.test_case (label ^ " all")
          Alcobar.[ g.positive; g.random; g.adversarial; s.fuzz; s.fuzz ]
          (fun positive random adversarial random_env adversarial_env ->
            check_positive_stream positive;
            let env = Some (random_env (Wire.Codec.env g.codec)) in
            check_safety_stream "random" ?env random;
            let env = Some (adversarial_env (Wire.Codec.env g.codec)) in
            check_safety_stream "adversarial" ?env adversarial);
      ]

(* Cross-field sizes: a [byte_array] whose [~size] reads a preceding int field.
   The size-source field is drawn from several int-valued field types -- in
   particular [optional_or], whose present-or-default value the size expression
   reads. Each source builds a two-field [(len, data)] codec round-tripped
   through [test_cases], which checks decode, [size_of_value], and re-encode
   against canonical bytes assembled here (the length-field bytes followed by
   [len] data bytes). *)
let u8_size_bytes n =
  let b = Bytes.create 1 in
  Bytes.set_uint8 b 0 n;
  b

let u16be_size_bytes n =
  let b = Bytes.create 2 in
  Bytes.set_uint16_be b 0 n;
  b

let sized_source name make_len bytes_of_len =
  let len_field = make_len () in
  let data_field =
    Wire.Field.v "Data" (Wire.byte_array ~size:(Wire.Field.ref len_field))
  in
  let codec =
    Wire.Codec.v ("Sized" ^ name)
      (fun len data -> (len, data))
      Wire.Codec.[ len_field $ fst; data_field $ snd ]
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 7 ]
      (fun len ->
        let data = String.make len 'a' in
        ((len, data), Bytes.cat (bytes_of_len len) (bytes_of_string data)))
  in
  {
    codec;
    typ = Wire.codec codec;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
  }

let sized_cases group =
  List.concat_map
    (fun (slabel, src) -> test_cases (group ^ " " ^ slabel) src)
    [
      ( "u8",
        sized_source "U8"
          (fun () -> Wire.Field.v "Len" Wire.uint8)
          u8_size_bytes );
      ( "u16be",
        sized_source "U16be"
          (fun () -> Wire.Field.v "Len" Wire.uint16be)
          u16be_size_bytes );
      ( "map",
        sized_source "Map"
          (fun () ->
            Wire.Field.v "Len"
              (Wire.map ~decode:Fun.id ~encode:Fun.id Wire.uint8))
          u8_size_bytes );
      ( "optional_or",
        sized_source "OptOr"
          (fun () ->
            Wire.Field.optional_or "Len" ~present:Wire.Expr.true_ ~default:0
              Wire.uint8)
          u8_size_bytes );
      ( "where",
        sized_source "Where"
          (fun () -> Wire.Field.v "Len" (Wire.where Wire.Expr.true_ Wire.uint8))
          u8_size_bytes );
    ]

(* Drive a freshly generated nested composition per sample: the positive case
   asserts round-trip (and so [size_of_value] / [encode] consistency across the
   whole nesting), the other two assert the decoder never raises. Generated
   codecs never reference [Param.input], so no env is threaded. *)
type nested_sample = NS : 'a t * string * ('a * bytes) -> nested_sample

type nested_bytes =
  | NB :
      'a t * string * bytes * (Wire.Param.env -> Wire.Param.env) option
      -> nested_bytes

let nested_cases label depth =
  let positive =
    Alcobar.dynamic_bind (gen_any depth) (fun (Any a) ->
        Alcobar.map Alcobar.[ a.g.positive ] (fun pos -> NS (a.g, a.label, pos)))
  in
  let with_bytes adversarial =
    Alcobar.dynamic_bind (gen_any depth) (fun (Any a) ->
        let stream = if adversarial then a.g.adversarial else bytes_any in
        match a.g.env with
        | None ->
            Alcobar.map
              Alcobar.[ stream ]
              (fun bs -> NB (a.g, a.label, bs, None))
        | Some s ->
            Alcobar.map
              Alcobar.[ stream; s.fuzz ]
              (fun bs mk -> NB (a.g, a.label, bs, Some mk)))
  in
  [
    Alcobar.test_case (label ^ " all")
      Alcobar.[ positive; with_bytes false; with_bytes true ]
      (fun (NS (g, comp, ((value, bs) as pos))) random adversarial ->
        let nested_label = Fmt.str "%s [%s]" label comp in
        (try
           check_positive nested_label g pos;
           check_validate_positive nested_label g bs;
           ignore value
         with Failure m -> Alcobar.failf "%s raised: %s" nested_label m);
        let check kind (NB (g, comp, bs, mk)) =
          let nested_label = Fmt.str "%s [%s]" label comp in
          let env = Option.map (fun f -> f (Wire.Codec.env g.codec)) mk in
          check_decode_safety nested_label kind ?env g bs;
          check_validate_safety nested_label kind ?env g bs
        in
        check "random" random;
        check "adversarial" adversarial);
  ]

(* Round-trip through {!Wire.of_string} / {!Wire.to_string}: the typ-level
   API, distinct from [Codec.encode/decode]. Skipped for codecs that take a
   [Param.env] since the direct entry points don't thread one. *)
let direct_cases label g =
  if Option.is_some g.env then []
  else
    let pos_case =
      Alcobar.test_case
        (label ^ " of_string/to_string positive")
        Alcobar.[ g.positive ]
        (fun (value, bs) ->
          let s = string_of_bytes bs in
          (match Wire.of_string g.typ s with
          | Ok decoded ->
              if not (g.equal decoded value) then
                Alcobar.failf "%s direct decode mismatch" label
          | Error _ -> Alcobar.failf "%s direct decode failed" label);
          let out = Wire.to_string g.typ value in
          if out <> s then Alcobar.failf "%s direct reencode mismatch" label)
    in
    let safety kind stream =
      Alcobar.test_case
        (label ^ " of_string " ^ kind)
        Alcobar.[ stream ]
        (fun bs ->
          try ignore (Wire.of_string g.typ (string_of_bytes bs))
          with Invalid_argument _ ->
            Alcobar.failf "%s direct %s crashed decoder" label kind)
    in
    [ pos_case; safety "random" g.random; safety "adversarial" g.adversarial ]

(* Round-trip through {!Wire.of_reader} / {!Wire.to_writer}: streaming
   entry points. Same env restriction as {!direct_cases}. *)
let streaming_cases label g =
  if Option.is_some g.env then []
  else
    let pos_case =
      Alcobar.test_case
        (label ^ " of_reader/to_writer positive")
        Alcobar.[ g.positive ]
        (fun (value, bs) ->
          let reader = Bytesrw.Bytes.Reader.of_bytes bs in
          (match Wire.of_reader g.typ reader with
          | Ok decoded ->
              if not (g.equal decoded value) then
                Alcobar.failf "%s streaming decode mismatch" label
          | Error _ -> Alcobar.failf "%s streaming decode failed" label);
          let buf = Buffer.create (Bytes.length bs) in
          let writer = Bytesrw.Bytes.Writer.of_buffer buf in
          Wire.to_writer g.typ value writer;
          Bytesrw.Bytes.Writer.write_eod writer;
          let out = Buffer.contents buf in
          if out <> string_of_bytes bs then
            Alcobar.failf "%s streaming reencode mismatch" label)
    in
    [ pos_case ]

(* Exercises {!Wire.Codec.validate}: validation-only path that should
   accept positives and reject (without crashing) random/adversarial.
   When the codec references [Param.input], positives pull the env from
   [g.env.positive] and the safety streams fuzz the env. *)
let validate_cases label g =
  let pos_case =
    Alcobar.test_case
      (label ^ " validate positive")
      Alcobar.[ g.positive ]
      (fun (_value, bs) ->
        let env = positive_env g in
        match validate_one ?env g bs with
        | `Ok -> ()
        | `Reject -> Alcobar.failf "%s validate rejected a positive" label
        | `Crash -> Alcobar.failf "%s validate crashed on a positive" label)
  in
  let safety kind stream =
    match g.env with
    | None ->
        Alcobar.test_case
          (label ^ " validate " ^ kind)
          Alcobar.[ stream ]
          (fun bs ->
            match validate_one g bs with
            | `Ok | `Reject -> ()
            | `Crash -> Alcobar.failf "%s validate %s crashed" label kind)
    | Some s ->
        Alcobar.test_case
          (label ^ " validate " ^ kind)
          Alcobar.[ stream; s.fuzz ]
          (fun bs build_env ->
            let env = Some (build_env (Wire.Codec.env g.codec)) in
            match validate_one ?env g bs with
            | `Ok | `Reject -> ()
            | `Crash -> Alcobar.failf "%s validate %s crashed" label kind)
  in
  [ pos_case; safety "random" g.random; safety "adversarial" g.adversarial ]

(* Driver for codecs that always reject (e.g. [Action.abort]). Generates
   random bytes plus, optionally, a random env, and asserts that decode
   returns [Error _] (not [Ok], not [Invalid_argument]). *)
let reject_cases label g =
  let env_stream =
    match g.env with Some s -> s.fuzz | None -> Alcobar.const Fun.id
  in
  let check_reject bs build_env =
    let env = Some (build_env (Wire.Codec.env g.codec)) in
    match Wire.Codec.decode ?env g.codec bs 0 with
    | Ok _ -> Alcobar.failf "%s decoder accepted input it should reject" label
    | Error _ -> ()
    | exception Invalid_argument _ -> Alcobar.failf "%s decoder crashed" label
  in
  [
    Alcobar.test_case (label ^ " all")
      Alcobar.[ g.random; g.adversarial; env_stream; env_stream ]
      (fun random adversarial random_env adversarial_env ->
        check_reject random random_env;
        check_reject adversarial adversarial_env);
  ]

(* {1 Canonical registry and EverParse drivers}

   Every fuzz suite drives off [registry] rather than its own hand-written
   list, so a generated codec is exercised on the OCaml round-trip path
   ([test_cases]) and the 3D projection path ([everparse_cases] /
   [everparse_3d_cases]) from one source. *)

type packed = Pack : 'a t -> packed

let codec g = g.codec
let binds_env (Pack g) = Option.is_some g.env

let registry_record =
  Codec.v "RegRecord" (fun a b -> (a, b)) Codec.[ uint8 $ fst; uint16be $ snd ]

let registry_casetype =
  casetype_u8 "RegPayload"
    [
      case ~index:1 uint16be
        ~inject:(fun v -> `A v)
        ~project:(function `A v -> Some v | _ -> None);
      case ~index:2 uint32be
        ~inject:(fun v -> `B v)
        ~project:(function `B v -> Some v | _ -> None);
      default_case ~tag:0xFF uint8
        ~inject:(fun v -> `Other v)
        ~project:(function `Other v -> Some v | _ -> None);
    ]

let unsigned_scalar_gens =
  [
    ("uint8", Pack uint8);
    ("uint16", Pack uint16);
    ("uint16be", Pack uint16be);
    ("uint16(endian_edges)", Pack uint16_endian_edges);
    ("uint16be(endian_edges)", Pack uint16be_endian_edges);
    ("uint32", Pack uint32);
    ("uint32be", Pack uint32be);
    ("uint32(endian_edges)", Pack uint32_endian_edges);
    ("uint32be(endian_edges)", Pack uint32be_endian_edges);
    ("uint63", Pack uint63);
    ("uint63be", Pack uint63be);
    ("uint64", Pack uint64);
    ("uint64be", Pack uint64be);
    ("uint64(endian_edges)", Pack uint64_endian_edges);
    ("uint64be(endian_edges)", Pack uint64be_endian_edges);
  ]

let signed_scalar_gens =
  [
    ("int8", Pack int8);
    ("int16", Pack int16);
    ("int16be", Pack int16be);
    ("int16(endian_edges)", Pack int16_endian_edges);
    ("int16be(endian_edges)", Pack int16be_endian_edges);
    ("int32", Pack int32);
    ("int32be", Pack int32be);
    ("int32(endian_edges)", Pack int32_endian_edges);
    ("int32be(endian_edges)", Pack int32be_endian_edges);
    ("int64", Pack int64);
    ("int64be", Pack int64be);
    ("int64(endian_edges)", Pack int64_endian_edges);
    ("int64be(endian_edges)", Pack int64be_endian_edges);
  ]

let float_and_empty_gens =
  [
    ("float32", Pack float32);
    ("float32be", Pack float32be);
    ("float64", Pack float64);
    ("float64be", Pack float64be);
    ("empty", Pack empty);
  ]

let variable_width_scalar_gens =
  [
    ("uint_var(1,little)", Pack (uint_var ~endian:Wire.Little 1));
    ("uint_var(2,little)", Pack (uint_var ~endian:Wire.Little 2));
    ("uint_var(3,little)", Pack (uint_var ~endian:Wire.Little 3));
    ("uint_var(3,big)", Pack (uint_var ~endian:Wire.Big 3));
    ("uint_var(5,little)", Pack (uint_var ~endian:Wire.Little 5));
    ("uint_var(5,big)", Pack (uint_var ~endian:Wire.Big 5));
    ("uint_var(7,little)", Pack (uint_var ~endian:Wire.Little 7));
    ("uint_var(7,big)", Pack (uint_var ~endian:Wire.Big 7));
    ("uint_var(3,little_edges)", Pack uint_var3_little_edges);
    ("uint_var(3,big_edges)", Pack uint_var3_big_edges);
    ("uint_var(7,little_edges)", Pack uint_var7_little_edges);
    ("uint_var(7,big_edges)", Pack uint_var7_big_edges);
    ("bit(uint8)", Pack (bit uint8));
    ("bit(uint16)", Pack (bit uint16));
    ("bit(uint16be)", Pack (bit uint16be));
    ("bit(int8)", Pack (bit int8));
    ("bit(int16)", Pack (bit int16));
    ("bit(int16be)", Pack (bit int16be));
  ]

let scalar_gens =
  unsigned_scalar_gens @ signed_scalar_gens @ float_and_empty_gens
  @ variable_width_scalar_gens

let byte_gens =
  [
    ("byte_array(0)", Pack (byte_array 0));
    ("byte_array(5)", Pack (byte_array 5));
    ("byte_slice(0)", Pack (byte_slice 0));
    ("byte_slice(3)", Pack (byte_slice 3));
    ( "byte_array_where(4)",
      Pack
        (byte_array_where 4 ~per_byte:(fun b ->
             Wire.Expr.(b >= Wire.int 0x20 && b <= Wire.int 0x7e))) );
    ("all_bytes", Pack all_bytes);
    ("all_zeros", Pack all_zeros);
    ("zeroterm", Pack zeroterm);
    ("zeroterm_at_most(1)", Pack (zeroterm_at_most 1));
    ("zeroterm_at_most(8)", Pack (zeroterm_at_most 8));
  ]

let bits_gens =
  [
    ("bits(1,U8)", Pack (bits ~width:1 Wire.U8));
    ("bits(1,U8,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:1 Wire.U8));
    ("bits(3,U8)", Pack (bits ~width:3 Wire.U8));
    ("bits(3,U8,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:3 Wire.U8));
    ("bits(7,U8)", Pack (bits ~width:7 Wire.U8));
    ("bits(7,U8,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:7 Wire.U8));
    ("bits(8,U8)", Pack (bits ~width:8 Wire.U8));
    ("bits(8,U8,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:8 Wire.U8));
    ("bits(1,U16)", Pack (bits ~width:1 Wire.U16));
    ("bits(15,U16)", Pack (bits ~width:15 Wire.U16));
    ("bits(16,U16)", Pack (bits ~width:16 Wire.U16));
    ( "bits(16,U16,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:16 Wire.U16) );
    ("bits(1,U16be)", Pack (bits ~width:1 Wire.U16be));
    ("bits(10,U16be)", Pack (bits ~width:10 Wire.U16be));
    ("bits(4,U16be)", Pack (bits ~width:4 Wire.U16be));
    ( "bits(15,U16be,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:15 Wire.U16be) );
    ("bits(16,U16be)", Pack (bits ~width:16 Wire.U16be));
    ( "bits(16,U16be,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:16 Wire.U16be) );
    ("bits(1,U32)", Pack (bits ~width:1 Wire.U32));
    ("bits(17,U32)", Pack (bits ~width:17 Wire.U32));
    ( "bits(17,U32,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:17 Wire.U32) );
    ( "bits(32,U32,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:32 Wire.U32) );
    ("bits(1,U32be)", Pack (bits ~width:1 Wire.U32be));
    ("bits(31,U32be)", Pack (bits ~width:31 Wire.U32be));
    ( "bits(31,U32be,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:31 Wire.U32be) );
    ("bits(32,U32be)", Pack (bits ~width:32 Wire.U32be));
    ( "bits(32,U32be,Lsb)",
      Pack (bits ~bit_order:Wire.Lsb_first ~width:32 Wire.U32be) );
    ("bits(4,U32,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:4 Wire.U32));
  ]

let bitpack_gens =
  [
    ("bitpack_u8_msb(1,2,5)", Pack bitpack_u8_msb);
    ("bitpack_u8_lsb(1,2,5)", Pack bitpack_u8_lsb);
    ("bitpack_u8_spill(5,5)", Pack bitpack_u8_spill);
    ("bitpack_split_orders", Pack bitpack_split_orders);
    ("bitpack_u16_msb(3,2,11)", Pack bitpack_u16_msb);
    ("bitpack_u16_lsb(3,2,11)", Pack bitpack_u16_lsb);
    ("bitpack_u16be_msb(3,2,11)", Pack bitpack_u16be_msb);
    ("bitpack_u16be_lsb(3,2,11)", Pack bitpack_u16be_lsb);
    ("bitpack_u16be_spill(9,9)", Pack bitpack_u16be_spill);
    ("bitpack_u32_msb(6,10,16)", Pack bitpack_u32_msb);
    ("bitpack_u32_lsb(6,10,16)", Pack bitpack_u32_lsb);
    ("bitpack_u32be_msb(6,10,16)", Pack bitpack_u32be_msb);
    ("bitpack_u32be_lsb(6,10,16)", Pack bitpack_u32be_lsb);
  ]

let wrapper_gens =
  [
    ( "map(uint8)",
      Pack (map ~decode:(fun n -> n * 2) ~encode:(fun n -> n / 2) uint8) );
    ("variants", Pack (variants "Flag" [ ("A", `A); ("B", `B); ("C", `C) ]));
    ("variants_u16be", Pack variants_u16be);
    ("enum", Pack (enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ]));
    ("enum_u16be", Pack enum_u16be);
    ("enum_open", Pack enum_open);
    ("enum_open_u16be", Pack enum_open_u16be);
    ("bounded_u8", Pack (bounded_u8 ~min:10 ~max:100));
    ("bounded_u16be", Pack bounded_u16be);
    ("bounded_u32be", Pack bounded_u32be);
    ("where(uint8)", Pack (where uint8));
    ("lookup", Pack (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8));
    ("optional(uint8)", Pack (optional uint8));
    ("optional(false)", Pack (optional ~present:false uint8));
    ("optional_or(uint8)", Pack (optional_or ~default:0 uint8));
    ("optional_or(false)", Pack (optional_or ~present:false ~default:42 uint8));
    ("optional_dynamic", Pack optional_dynamic);
    ("optional_or_dynamic", Pack optional_or_dynamic);
    ("nested(0,empty)", Pack (nested 0 empty));
    ("nested(2,uint16be)", Pack (nested 2 uint16be));
    ("nested(4,uint16be)", Pack (nested 4 uint16be));
    ("nested_at_most(0,empty)", Pack (nested_at_most 0 empty));
    ("nested_at_most(2,uint16be)", Pack (nested_at_most 2 uint16be));
    ("nested_at_most(4,uint16be)", Pack (nested_at_most 4 uint16be));
  ]

let composite_gens =
  [
    ("array(0,uint8)", Pack (array 0 uint8));
    ("array(3,uint16be)", Pack (array 3 uint16be));
    ("array_seq(3,uint16be)", Pack (array_seq 3 uint16be));
    ("array_seq(0,uint8)", Pack (array_seq 0 uint8));
    ("array(2,record)", Pack (array 2 registry_record));
    ("array(3,bounded_u8)", Pack (array 3 (bounded_u8 ~min:10 ~max:100)));
    ("repeat(0,uint8)", Pack (repeat ~bytes:0 uint8));
    ("repeat(8,uint8)", Pack (repeat ~bytes:8 uint8));
    ("repeat(7,uint16be)", Pack (repeat ~bytes:7 uint16be));
    ("repeat_seq(8,uint8)", Pack (repeat_seq ~bytes:8 uint8));
    ("repeat_seq(7,uint16be)", Pack (repeat_seq ~bytes:7 uint16be));
    ("record", Pack registry_record);
    ("casetype_u8", Pack registry_casetype);
    ("casetype_u16be_default", Pack casetype_u16be_default);
    ("field_anon", Pack field_anon);
  ]

let param_action_gens =
  [
    ("action", Pack action);
    ("action_on_act", Pack action_on_act);
    ("expr_ops", Pack expr_ops);
    ("sizeof", Pack sizeof);
    ("codec_where", Pack codec_where);
    ("typ_where", Pack typ_where);
    ("field_constraint", Pack field_constraint);
    ("field_int", Pack field_int);
    ("self_int64", Pack self_int64);
    ("rest_bytes", Pack rest_bytes);
    ("param_input", Pack param_input);
    ("param_size", Pack param_size);
    ("if_then_else", Pack if_then_else);
    ("finite_float64", Pack finite_float64);
    ("nan_float64", Pack nan_float64);
  ]

(* A registry label rendered as a unique EverParse struct name: the
   alphanumeric characters with the first capitalized and the rest lowercased,
   e.g. ["uint_var(3,big)"] becomes ["Uintvar3big"]. EverParse lowercases all
   but the first character of a module name when it generates C, so keeping no
   internal capitals makes the generated validator symbol match what the FFI
   stubs reference. *)
let camel_of_label label =
  let buf = Buffer.create (String.length label) in
  let first = ref true in
  String.iter
    (fun c ->
      let alnum =
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
      in
      if alnum then begin
        Buffer.add_char buf
          (if !first then Char.uppercase_ascii c else Char.lowercase_ascii c);
        first := false
      end)
    label;
  let s = Buffer.contents buf in
  if s = "" then "Case" else s

(* Give each registry case a unique 3D struct name derived from its label, so
   every case projects to a distinct schema and the per-codec validators link
   without entrypoint-name collisions. The name is metadata only
   ({!Wire.Codec.rename}): it changes neither the wire encoding nor the
   generators, so the positive / random / adversarial streams are untouched. *)
let rename_case label (Pack g) =
  Pack { g with codec = Wire.Codec.rename (camel_of_label label) g.codec }

let registry : (string * packed) list =
  List.map
    (fun (label, p) -> (label, rename_case label p))
    (scalar_gens @ byte_gens @ bits_gens @ bitpack_gens @ wrapper_gens
   @ composite_gens @ param_action_gens)

let truncate_bytes ~max_len bs =
  if Bytes.length bs <= max_len then bs else Bytes.sub bs 0 max_len

type afl_input = { selector : int; mode : int; payload : bytes }

let afl_payload bs =
  let len = Bytes.length bs in
  if len <= 2 then Bytes.empty else Bytes.sub bs 2 (len - 2)

let parse_afl_input ?(max_len = 256) bs =
  {
    selector = (if Bytes.length bs = 0 then 0 else Bytes.get_uint8 bs 0);
    mode = (if Bytes.length bs <= 1 then 0 else Bytes.get_uint8 bs 1);
    payload = truncate_bytes ~max_len (afl_payload bs);
  }

let afl_input_gen =
  Alcobar.map
    Alcobar.[ Alcobar.uint8; Alcobar.uint8; bytes_any ]
    (fun selector mode payload ->
      let framed = Bytes.create (2 + Bytes.length payload) in
      Bytes.set_uint8 framed 0 selector;
      Bytes.set_uint8 framed 1 mode;
      Bytes.blit payload 0 framed 2 (Bytes.length payload);
      framed)

let afl_contract_cases ?(max_len = 256) label cases ~check =
  match cases with
  | [] -> []
  | _ ->
      [
        Alcobar.test_case (label ^ " afl")
          Alcobar.[ afl_input_gen ]
          (fun bs ->
            let input = parse_afl_input ~max_len bs in
            let name, item =
              List.nth cases (input.selector mod List.length cases)
            in
            check (label ^ " " ^ name) item input);
      ]

let afl_cases_for ?max_len cases label =
  afl_contract_cases ?max_len label cases ~check:(fun case (Pack g) input ->
      let env = positive_env g in
      match input.mode mod 4 with
      | 0 ->
          check_decode_safety case "afl" ?env g input.payload;
          check_validate_safety case "afl" ?env g input.payload
      | 1 -> check_decode_safety case "afl" ?env g input.payload
      | 2 -> check_validate_safety case "afl" ?env g input.payload
      | _ -> check_decode_validate_agree case "afl" ?env g input.payload)

(* AFL fuzzes the whole registry, not a curated subset: the [selector] byte
   picks one codec per input, so covering every combinator costs nothing
   per-exec and only spreads mutation across more shapes over a run. *)
let afl_cases ?max_len label = afl_cases_for ?max_len registry label

let afl_env_cases ?max_len label =
  let env_cases = List.filter (fun (_, p) -> binds_env p) registry in
  afl_cases_for ?max_len env_cases label

(* {1 Boltzmann sampler over the projectable grammar}

   A well-distributed random codec sampler. Rather than enumerate a handpicked
   set, [sample] draws codecs whose shape follows a Boltzmann law: the arity of
   a record is geometric (the Boltzmann distribution for a sequence, tuned to a
   small expected size via [continue]), and each field is drawn from a fixed
   leaf vocabulary that deliberately over-samples weird / adversarial shapes.
   It stays in the single-typedef, fixed-size fragment (flat records and
   homogeneous arrays of leaves) so the differential fuzzer can compile every
   sample to a standalone C validator. Driven by a
   [Random.State.t], so a [seed] reproduces the exact set: the differential's
   generator and its runner both call [sample] with the same seed and agree on
   the shapes. *)

(* Single-typedef, fixed-size leaf vocabulary: each projects to one EverParse
   field, so a flat record of them stays a single typedef. The sampler chooses
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

let sampler_adversarial_leaves : any list =
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
  ]

let sampler_leaves = sampler_regular_leaves @ sampler_adversarial_leaves

(* Geometric arity in [1, max_arity]: keep adding a field with probability
   [continue]. This is the Boltzmann law for a sequence, truncated. *)
let sample_arity rng ~continue ~max_arity =
  let rec go k =
    if k >= max_arity || Random.State.float rng 1.0 >= continue then k
    else go (k + 1)
  in
  go 1

(* The shape of one sampled codec, paired with the leaf-vocabulary labels it
   draws (in draw order). The sampler stays in the single-typedef, fixed-size
   fragment, so a composition is only a flat record of leaves or an array of one
   leaf -- "depth" here is record arity and array presence, not arbitrary
   nesting. Exposed alongside each sample so coverage metrics read the structure
   directly rather than parsing the debug label. *)
type sample_shape = Leaf | Array | Record of int
type sample_meta = { shape : sample_shape; leaves : string list }

(* One Boltzmann-distributed projectable codec, with the structural metadata
   needed for coverage metrics. A composition Wire refuses to build (e.g. a
   bitfield array element) raises [Invalid_argument]; fall back to a bare leaf so
   the sampler never crashes. The leaves drawn are recorded as a side effect, so
   the RNG draw sequence is byte-identical to a plain sampler and a given seed
   reproduces the exact same set. *)
let boltzmann_any rng : any * sample_meta =
  let regular = Array.of_list sampler_regular_leaves in
  let adversarial = Array.of_list sampler_adversarial_leaves in
  let pick xs = xs.(Random.State.int rng (Array.length xs)) in
  let drawn = ref [] in
  let leaf () =
    let a =
      if Random.State.int rng 4 = 0 then pick regular else pick adversarial
    in
    let (Any l) = a in
    drawn := l.label :: !drawn;
    a
  in
  let k = sample_arity rng ~continue:0.5 ~max_arity:4 in
  let shape = ref Leaf in
  let result =
    try
      if k = 1 then leaf ()
      else if Random.State.bool rng then
        match array_of (1 + Random.State.int rng 6) (leaf ()) with
        | Some a ->
            shape := Array;
            a
        | None -> leaf ()
      else
        match k with
        | 2 ->
            shape := Record 2;
            pair_of (leaf ()) (leaf ())
        | 3 ->
            shape := Record 3;
            rec3_of (leaf ()) (leaf ()) (leaf ())
        | _ ->
            shape := Record 4;
            rec4_of (leaf ()) (leaf ()) (leaf ()) (leaf ())
    with Invalid_argument _ -> leaf ()
  in
  (result, { shape = !shape; leaves = List.rev !drawn })

let sampled ~seed ~count : (string * packed * sample_meta) list =
  let rng = Random.State.make [| seed |] in
  List.init count (fun i ->
      let Any a, meta = boltzmann_any rng in
      let label = Fmt.str "smp%d_%s" i a.label in
      (label, rename_case label (Pack a.g), meta))

let sample ~seed ~count : (string * packed) list =
  List.map (fun (label, p, _) -> (label, p)) (sampled ~seed ~count)

let any_labels xs = List.map (fun (Any a) -> a.label) xs

let duplicate_labels labels =
  let sorted = List.sort String.compare labels in
  let rec go acc = function
    | a :: (b :: _ as rest) when String.equal a b -> go (a :: acc) rest
    | _ :: rest -> go acc rest
    | [] -> List.rev acc
  in
  go [] sorted

let assert_no_duplicates group labels =
  match duplicate_labels labels with
  | [] -> ()
  | dups ->
      Alcobar.failf "%s duplicate labels: %s" group (String.concat ", " dups)

let assert_contains group labels expected =
  List.iter
    (fun label ->
      if not (List.mem label labels) then
        Alcobar.failf "%s missing %s" group label)
    expected

let expected_registry_labels =
  [
    "uint8";
    "uint16";
    "uint16be";
    "uint16(endian_edges)";
    "uint16be(endian_edges)";
    "uint32";
    "uint32be";
    "uint32(endian_edges)";
    "uint32be(endian_edges)";
    "uint63";
    "uint63be";
    "uint64";
    "uint64be";
    "uint64(endian_edges)";
    "uint64be(endian_edges)";
    "int8";
    "int16";
    "int16be";
    "int16(endian_edges)";
    "int16be(endian_edges)";
    "int32";
    "int32be";
    "int32(endian_edges)";
    "int32be(endian_edges)";
    "int64";
    "int64be";
    "int64(endian_edges)";
    "int64be(endian_edges)";
    "float32";
    "float32be";
    "float64";
    "float64be";
    "empty";
    "uint_var(1,little)";
    "uint_var(2,little)";
    "uint_var(3,little)";
    "uint_var(3,big)";
    "uint_var(5,little)";
    "uint_var(5,big)";
    "uint_var(7,little)";
    "uint_var(7,big)";
    "uint_var(3,little_edges)";
    "uint_var(3,big_edges)";
    "uint_var(7,little_edges)";
    "uint_var(7,big_edges)";
    "bit(uint8)";
    "bit(uint16)";
    "bit(uint16be)";
    "bit(int8)";
    "bit(int16)";
    "bit(int16be)";
    "byte_array(0)";
    "byte_array(5)";
    "byte_slice(0)";
    "byte_slice(3)";
    "byte_array_where(4)";
    "all_bytes";
    "all_zeros";
    "zeroterm";
    "zeroterm_at_most(1)";
    "zeroterm_at_most(8)";
    "bits(1,U8)";
    "bits(1,U8,Lsb)";
    "bits(3,U8)";
    "bits(3,U8,Lsb)";
    "bits(7,U8)";
    "bits(7,U8,Lsb)";
    "bits(8,U8)";
    "bits(8,U8,Lsb)";
    "bits(1,U16)";
    "bits(15,U16)";
    "bits(16,U16)";
    "bits(16,U16,Lsb)";
    "bits(1,U16be)";
    "bits(10,U16be)";
    "bits(4,U16be)";
    "bits(15,U16be,Lsb)";
    "bits(16,U16be)";
    "bits(16,U16be,Lsb)";
    "bits(1,U32)";
    "bits(17,U32)";
    "bits(17,U32,Lsb)";
    "bits(32,U32,Lsb)";
    "bits(1,U32be)";
    "bits(31,U32be)";
    "bits(31,U32be,Lsb)";
    "bits(32,U32be)";
    "bits(32,U32be,Lsb)";
    "bits(4,U32,Lsb)";
    "bitpack_u8_msb(1,2,5)";
    "bitpack_u8_lsb(1,2,5)";
    "bitpack_u8_spill(5,5)";
    "bitpack_split_orders";
    "bitpack_u16_msb(3,2,11)";
    "bitpack_u16_lsb(3,2,11)";
    "bitpack_u16be_msb(3,2,11)";
    "bitpack_u16be_lsb(3,2,11)";
    "bitpack_u16be_spill(9,9)";
    "bitpack_u32_msb(6,10,16)";
    "bitpack_u32_lsb(6,10,16)";
    "bitpack_u32be_msb(6,10,16)";
    "bitpack_u32be_lsb(6,10,16)";
    "map(uint8)";
    "variants";
    "variants_u16be";
    "enum";
    "enum_u16be";
    "enum_open";
    "enum_open_u16be";
    "bounded_u8";
    "bounded_u16be";
    "bounded_u32be";
    "where(uint8)";
    "lookup";
    "optional(uint8)";
    "optional(false)";
    "optional_or(uint8)";
    "optional_or(false)";
    "optional_dynamic";
    "optional_or_dynamic";
    "nested(0,empty)";
    "nested(2,uint16be)";
    "nested(4,uint16be)";
    "nested_at_most(0,empty)";
    "nested_at_most(2,uint16be)";
    "nested_at_most(4,uint16be)";
    "array(0,uint8)";
    "array(3,uint16be)";
    "array_seq(3,uint16be)";
    "array_seq(0,uint8)";
    "repeat(0,uint8)";
    "repeat(8,uint8)";
    "repeat(7,uint16be)";
    "repeat_seq(8,uint8)";
    "repeat_seq(7,uint16be)";
    "record";
    "casetype_u8";
    "casetype_u16be_default";
    "field_anon";
    "action";
    "action_on_act";
    "expr_ops";
    "sizeof";
    "codec_where";
    "typ_where";
    "field_constraint";
    "field_int";
    "self_int64";
    "rest_bytes";
    "param_input";
    "param_size";
    "if_then_else";
    "finite_float64";
    "nan_float64";
  ]

let expected_fixed_labels =
  [
    "u8";
    "u16le";
    "u16";
    "u32le";
    "u32";
    "u64le";
    "u64";
    "i16le";
    "i16";
    "i32le";
    "i32";
    "i64le";
    "i64";
    "uv3le";
    "ba0";
    "bs0";
    "ba3";
    "bs4";
    "bits3lsb";
    "bp8m";
    "bp8l";
    "bp8spill";
    "bp16l";
    "bp16bem";
    "bp16bel";
    "bp32m";
    "bp32bel";
    "enum";
    "enum_open";
    "enum16";
    "enumopen16";
    "var";
    "var16";
    "fconstraint";
    "fint";
    "self64";
  ]

let expected_var_labels =
  [ "zt"; "zt1"; "zt6"; "az"; "optdyn"; "optordyn"; "ite" ]

let expected_env_labels = [ "param"; "param_size"; "rest"; "act"; "onact" ]

let expected_sampler_labels =
  [
    "u8";
    "u16";
    "u16be";
    "u32";
    "u32be";
    "i16";
    "i16be";
    "i32";
    "i32be";
    "i64";
    "i64be";
    "bits3lsb";
    "bits15u16";
    "bits15u16belsb";
    "bp8m";
    "bp8l";
    "bp16bel";
    "enum";
    "enum_open";
    "lkp";
  ]

let check_registry_invariants () =
  let labels = List.map fst registry in
  assert_no_duplicates "registry" labels;
  assert_contains "registry" labels expected_registry_labels

let check_vocabulary_group (group, labels) =
  if labels = [] then Alcobar.failf "%s is empty" group;
  assert_no_duplicates group labels

let check_composition_vocabulary () =
  let fixed = any_labels fixed_leaves
  and var = any_labels var_leaves
  and env = any_labels env_leaves
  and sampled = any_labels sampler_leaves
  and sampled_regular = any_labels sampler_regular_leaves
  and sampled_adversarial = any_labels sampler_adversarial_leaves in
  List.iter check_vocabulary_group
    [
      ("fixed_leaves", fixed);
      ("var_leaves", var);
      ("env_leaves", env);
      ("sampler_leaves", sampled);
      ("sampler_regular_leaves", sampled_regular);
      ("sampler_adversarial_leaves", sampled_adversarial);
    ];
  assert_contains "fixed_leaves" fixed expected_fixed_labels;
  assert_contains "var_leaves" var expected_var_labels;
  assert_contains "env_leaves" env expected_env_labels;
  assert_contains "sampler_leaves" sampled expected_sampler_labels;
  assert_contains "sampler_adversarial_leaves" sampled_adversarial
    [
      "bits3lsb";
      "bits15u16";
      "bits15u16belsb";
      "bp8m";
      "bp8l";
      "bp16bel";
      "bp32m";
      "bp32bel";
      "enum";
      "enum_open";
      "lkp";
    ]

let check_sampler_invariants () =
  let labels = List.map fst (sample ~seed:1 ~count:64) in
  if List.length labels <> 64 then
    Alcobar.failf "sample returned %d labels, expected 64" (List.length labels);
  assert_no_duplicates "sample" labels;
  List.iteri
    (fun i label ->
      let prefix = Fmt.str "smp%d_" i in
      if not (String.starts_with ~prefix label) then
        Alcobar.failf "sample label %S does not start with %S" label prefix)
    labels

let check_sampler_adversarial_bias () =
  let adversarial = any_labels sampler_adversarial_leaves in
  let is_adversarial l = List.mem l adversarial in
  let metas = List.map (fun (_, _, m) -> m) (sampled ~seed:7 ~count:256) in
  let adversarial_hits =
    List.fold_left
      (fun count m ->
        if List.exists is_adversarial m.leaves then count + 1 else count)
      0 metas
  in
  if adversarial_hits < 160 then
    Alcobar.failf
      "adversarial-biased sampler produced %d/256 weird/adversarial shapes"
      adversarial_hits

let shape_is_leaf = function Leaf -> true | _ -> false
let shape_is_array = function Array -> true | _ -> false
let shape_is_record_arity n = function Record k -> k = n | _ -> false

(* Beyond the gross 3:1 adversarial-bias ratio ([check_sampler_adversarial_bias]),
   enforce that one deterministic sample actually spreads across the grammar:
   every shape (leaf, array, record of each arity), both endians and both bit
   orders, signed ints, floats, and a spread of bitpack sizes all appear. Reads
   the structured [sample_meta] each codec carries, not its debug label. The
   margins on a 256-codec sample are large, so this guards real distribution
   regressions, not RNG noise. *)
let check_sampler_distribution () =
  let metas = List.map (fun (_, _, m) -> m) (sampled ~seed:7 ~count:256) in
  let all_leaves = List.concat_map (fun m -> m.leaves) metas in
  let has l = List.mem l all_leaves in
  let has_any = List.exists has in
  let has_shape p = List.exists (fun m -> p m.shape) metas in
  let checks =
    [
      ("no bare leaves", has_shape shape_is_leaf);
      ("no arrays", has_shape shape_is_array);
      ("no record of arity 2", has_shape (shape_is_record_arity 2));
      ("no record of arity 3", has_shape (shape_is_record_arity 3));
      ("no record of arity 4", has_shape (shape_is_record_arity 4));
      ( "no little-endian multibyte int",
        has_any [ "u16"; "u32"; "u64"; "i16"; "i32"; "i64" ] );
      ( "no big-endian multibyte int",
        has_any [ "u16be"; "u32be"; "u64be"; "i16be"; "i32be"; "i64be" ] );
      ( "no signed int",
        has_any [ "i8"; "i16"; "i16be"; "i32"; "i32be"; "i64"; "i64be" ] );
      ("no float", has_any [ "f32be"; "f64be" ]);
      ("no enum", has_any [ "enum"; "enum_open" ]);
      ("no lookup", has "lkp");
      ("no bounded", has "bnd");
      ("no msb-first bit field", has_any [ "bits3"; "bits15u16"; "bp8m" ]);
      ( "no lsb-first bit field",
        has_any [ "bits3lsb"; "bits15u16belsb"; "bp8l"; "bp16bel" ] );
      ("no 8-bit bitpack", has_any [ "bp8m"; "bp8l" ]);
      ("no 16-bit bitpack", has "bp16bel");
      ("no 32-bit bitpack", has_any [ "bp32m"; "bp32bel" ]);
    ]
  in
  List.iter
    (fun (what, ok) ->
      if not ok then Alcobar.failf "sampler distribution: %s" what)
    checks

let const_case name check =
  Alcobar.test_case name Alcobar.[ Alcobar.const () ] (fun () -> check ())

let invariant_cases label =
  [
    const_case (label ^ " registry") check_registry_invariants;
    const_case (label ^ " composition vocabulary") check_composition_vocabulary;
    const_case (label ^ " sampler") check_sampler_invariants;
    const_case
      (label ^ " sampler adversarial bias")
      check_sampler_adversarial_bias;
    const_case (label ^ " sampler distribution") check_sampler_distribution;
  ]

let expect_invalid label f =
  match f () with
  | _ -> Alcobar.failf "%s: expected Invalid_argument" label
  | exception Invalid_argument _ -> ()

let construction_guard_cases label =
  [
    const_case (label ^ " bits width 0") (fun () ->
        expect_invalid "bits width 0" (fun () -> Wire.bits ~width:0 Wire.U8));
    const_case (label ^ " bits width > U8") (fun () ->
        expect_invalid "bits width > U8" (fun () -> Wire.bits ~width:9 Wire.U8));
    const_case (label ^ " bits width > U16") (fun () ->
        expect_invalid "bits width > U16" (fun () ->
            Wire.bits ~width:17 Wire.U16be));
    const_case (label ^ " bits width > U32") (fun () ->
        expect_invalid "bits width > U32" (fun () ->
            Wire.bits ~width:33 Wire.U32be));
    const_case (label ^ " casetype dynamic tag") (fun () ->
        expect_invalid "casetype dynamic tag" (fun () ->
            Wire.casetype "BadDynTag"
              (Wire.uint ~endian:Wire.Big (Wire.int 2))
              [
                Wire.case ~index:0 Wire.uint8 ~inject:Fun.id ~project:(fun v ->
                    Some v);
              ]));
    const_case (label ^ " casetype enum tag") (fun () ->
        expect_invalid "casetype enum tag" (fun () ->
            Wire.casetype "BadEnumTag"
              (Wire.enum "BadTag" [ ("A", 0) ] Wire.uint16be)
              [
                Wire.case ~index:0 Wire.uint8 ~inject:Fun.id ~project:(fun v ->
                    Some v);
              ]));
    const_case (label ^ " repeat empty") (fun () ->
        expect_invalid "repeat empty" (fun () ->
            Wire.Field.repeat "Empty" ~size:(Wire.int 1) Wire.empty));
    const_case (label ^ " repeat_seq empty") (fun () ->
        expect_invalid "repeat_seq empty" (fun () ->
            Wire.Field.repeat_seq "Empty" ~seq:Wire.seq_list ~size:(Wire.int 1)
              Wire.empty));
  ]

let check_all_zeros_semantic label () =
  let pad = Wire.Field.v "Pad" Wire.all_zeros in
  let g =
    {
      codec = Wire.Codec.v "FuzzZeros" Fun.id Wire.Codec.[ pad $ Fun.id ];
      typ =
        Wire.codec
          (Wire.Codec.v "FuzzZerosTyp" Fun.id Wire.Codec.[ pad $ Fun.id ]);
      positive = Alcobar.const ("", Bytes.empty);
      random = bytes_any;
      adversarial = bytes_any;
      equal = String.equal;
      env = None;
    }
  in
  check_decode_validate_agree label "all_zeros" g
    (Bytes.of_string "\000\001\000")

let signed_slice_semantic_codec () =
  let len = Wire.Field.v "Len" Wire.int8 in
  let data = Wire.Field.v "Data" (Wire.byte_slice ~size:(Wire.Field.ref len)) in
  let cf_data = Wire.Codec.(data $ snd) in
  let codec =
    Wire.Codec.v "FuzzSignedSlice"
      (fun l d -> (l, d))
      Wire.Codec.[ len $ fst; cf_data ]
  in
  let g =
    {
      codec;
      typ = Wire.codec codec;
      positive = Alcobar.const ((0, slice_of_string ""), Bytes.of_string "\000");
      random = bytes_any;
      adversarial = bytes_any;
      equal =
        (fun (l1, s1) (l2, s2) ->
          Int.equal l1 l2
          && String.equal (string_of_slice s1) (string_of_slice s2));
      env = None;
    }
  in
  (g, codec, cf_data)

let check_negative_byte_slice_semantic label () =
  let g, codec, cf_data = signed_slice_semantic_codec () in
  let bad = Bytes.of_string "\255" in
  check_decode_validate_agree label "negative byte_slice" g bad;
  match validate_one g bad with
  | `Reject | `Crash -> ()
  | `Ok -> (
      match (Wire.Staged.unstage (Wire.Codec.get codec cf_data)) bad 0 with
      | s ->
          if Bytesrw.Bytes.Slice.length s < 0 then
            Alcobar.failf "%s validate-then-get produced negative slice" label
      | exception Invalid_argument m ->
          Alcobar.failf "%s validate-then-get crashed: %s" label m)

let semantic_invariant_cases label =
  [
    const_case
      (label ^ " decode/validate all_zeros")
      (check_all_zeros_semantic label);
    const_case
      (label ^ " decode/validate negative byte_slice")
      (check_negative_byte_slice_semantic label);
  ]

type api_access = {
  a : int;
  hi : int;
  lo : int;
  payload : Bytesrw.Bytes.Slice.t;
}

let api_access_codec () =
  let f_a = Wire.Field.v "A" Wire.uint8 in
  let f_hi = Wire.Field.v "Hi" (Wire.bits ~width:3 Wire.U8) in
  let f_lo = Wire.Field.v "Lo" (Wire.bits ~width:5 Wire.U8) in
  let f_payload = Wire.Field.v "Payload" (Wire.byte_slice ~size:(Wire.int 3)) in
  let bf_a = Wire.Codec.(f_a $ fun r -> r.a) in
  let bf_hi = Wire.Codec.(f_hi $ fun r -> r.hi) in
  let bf_lo = Wire.Codec.(f_lo $ fun r -> r.lo) in
  let bf_payload = Wire.Codec.(f_payload $ fun r -> r.payload) in
  let codec =
    Wire.Codec.v "ApiAccess"
      (fun a hi lo payload -> { a; hi; lo; payload })
      Wire.Codec.[ bf_a; bf_hi; bf_lo; bf_payload ]
  in
  (codec, bf_a, bf_hi, bf_lo, bf_payload)

let check_int label expected actual =
  if actual <> expected then
    Alcobar.failf "%s: expected %d, got %d" label expected actual

let check_string label expected actual =
  if not (String.equal expected actual) then
    Alcobar.failf "%s: expected %S, got %S" label expected actual

let check_api_getters codec bf_a bf_hi bf_lo bf_payload buf base a hi lo
    payload_s =
  let get_a = Wire.Codec.get codec bf_a |> Wire.Staged.unstage in
  let get_hi = Wire.Codec.get codec bf_hi |> Wire.Staged.unstage in
  let get_lo = Wire.Codec.get codec bf_lo |> Wire.Staged.unstage in
  let get_payload = Wire.Codec.get codec bf_payload |> Wire.Staged.unstage in
  check_int "Codec.get scalar" a (get_a buf base);
  check_int "Codec.get bitfield hi" hi (get_hi buf base);
  check_int "Codec.get bitfield lo" lo (get_lo buf base);
  check_string "Codec.get slice" payload_s
    (string_of_slice (get_payload buf base));
  let slice_offset =
    Wire.Codec.slice_offset codec bf_payload |> Wire.Staged.unstage
  in
  let slice_length =
    Wire.Codec.slice_length codec bf_payload |> Wire.Staged.unstage
  in
  check_int "Codec.slice_offset" (base + 2) (slice_offset buf base);
  check_int "Codec.slice_length" 3 (slice_length buf base)

let check_api_bitfields codec bf_hi bf_lo buf base hi lo =
  let bit_hi = Wire.Codec.bitfield codec bf_hi in
  let bit_lo = Wire.Codec.bitfield codec bf_lo in
  let load = Wire.Codec.load_word bit_hi |> Wire.Staged.unstage in
  let word = load buf base in
  check_int "Codec.extract hi" hi (Wire.Codec.extract bit_hi word);
  check_int "Codec.extract lo" lo (Wire.Codec.extract bit_lo word)

let check_api_setters codec bf_a bf_hi bf_lo bf_payload buf base next_a next_hi
    next_lo next_payload_s =
  let get_a = Wire.Codec.get codec bf_a |> Wire.Staged.unstage in
  let get_hi = Wire.Codec.get codec bf_hi |> Wire.Staged.unstage in
  let get_lo = Wire.Codec.get codec bf_lo |> Wire.Staged.unstage in
  let get_payload = Wire.Codec.get codec bf_payload |> Wire.Staged.unstage in
  let set_a = Wire.Codec.set codec bf_a |> Wire.Staged.unstage in
  let set_hi = Wire.Codec.set codec bf_hi |> Wire.Staged.unstage in
  let set_lo = Wire.Codec.set codec bf_lo |> Wire.Staged.unstage in
  let set_payload = Wire.Codec.set codec bf_payload |> Wire.Staged.unstage in
  set_a buf base next_a;
  set_hi buf base next_hi;
  set_lo buf base next_lo;
  set_payload buf base (slice_of_string next_payload_s);
  check_int "Codec.set scalar" next_a (get_a buf base);
  check_int "Codec.set bitfield hi" next_hi (get_hi buf base);
  check_int "Codec.set bitfield lo" next_lo (get_lo buf base);
  check_string "Codec.set slice" next_payload_s
    (string_of_slice (get_payload buf base))

let check_api_decode_after_set codec buf base next_a next_hi next_lo
    next_payload_s =
  match Wire.Codec.decode codec buf base with
  | Ok decoded ->
      check_int "Codec.decode after set scalar" next_a decoded.a;
      check_int "Codec.decode after set hi" next_hi decoded.hi;
      check_int "Codec.decode after set lo" next_lo decoded.lo;
      check_string "Codec.decode after set slice" next_payload_s
        (string_of_slice decoded.payload)
  | Error e ->
      Alcobar.failf "Codec.decode after set failed: %a" Wire.pp_parse_error e

let check_api_accessors a hi lo payload_s next_a next_hi next_lo next_payload_s
    =
  let hi = hi land 0x7 and lo = lo land 0x1F in
  let next_hi = next_hi land 0x7 and next_lo = next_lo land 0x1F in
  let codec, bf_a, bf_hi, bf_lo, bf_payload = api_access_codec () in
  let payload = slice_of_string payload_s in
  let value = { a; hi; lo; payload } in
  let base = 1 in
  let sz = Wire.Codec.wire_size codec in
  let buf = Bytes.make (sz + 2) '\xCC' in
  Wire.Codec.encode codec value buf base;
  check_api_getters codec bf_a bf_hi bf_lo bf_payload buf base a hi lo payload_s;
  check_api_bitfields codec bf_hi bf_lo buf base hi lo;
  check_api_setters codec bf_a bf_hi bf_lo bf_payload buf base next_a next_hi
    next_lo next_payload_s;
  check_api_decode_after_set codec buf base next_a next_hi next_lo
    next_payload_s

let custom_array_seq =
  Wire.Seq_map
    {
      empty = [];
      add = (fun acc v -> v :: acc);
      finish = (fun acc -> Array.of_list (List.rev acc));
      iter = (fun f arr -> Array.iter f arr);
    }

let check_custom_seq a b c d =
  let values = [| a; b; c; d |] in
  let typ = Wire.array_seq custom_array_seq ~len:(Wire.int 4) Wire.uint8 in
  let encoded = Wire.to_bytes typ values in
  (match Wire.of_bytes typ encoded with
  | Ok decoded ->
      if decoded <> values then Alcobar.failf "array_seq custom seq mismatch"
  | Error e ->
      Alcobar.failf "array_seq custom seq decode failed: %a" Wire.pp_parse_error
        e);
  let f_len = Wire.Field.v "Len" Wire.uint8 in
  let f_items =
    Wire.Field.repeat_seq "Items" ~seq:custom_array_seq
      ~size:(Wire.Field.ref f_len) Wire.uint8
  in
  let codec =
    Wire.Codec.v "ApiRepeatSeq"
      (fun _ items -> items)
      Wire.Codec.[ f_len $ Array.length; (f_items $ fun xs -> xs) ]
  in
  let buf = Bytes.create 5 in
  Wire.Codec.encode codec values buf 0;
  match Wire.Codec.decode codec buf 0 with
  | Ok decoded ->
      if decoded <> values then Alcobar.failf "repeat_seq custom seq mismatch"
  | Error e ->
      Alcobar.failf "repeat_seq custom seq decode failed: %a"
        Wire.pp_parse_error e

let raw_len_field = Wire.Private.Types.field "Len" Wire.uint8
let raw_data_field = Wire.Private.Types.field "Data" Wire.all_bytes

let raw_struct =
  Wire.Private.Types.struct_ "ApiRaw"
    [ raw_len_field; Wire.Private.Types.anon_field Wire.uint8; raw_data_field ]

let fst5 (a, _, _, _, _) = a

let check_struct_validator len data_s =
  let len = len land 0xFF in
  let bytes = Bytes.create (2 + String.length data_s) in
  Bytes.set_uint8 bytes 0 len;
  Bytes.set_uint8 bytes 1 0;
  Bytes.blit_string data_s 0 bytes 2 (String.length data_s);
  let validator = Wire.Codec.validator_of_struct raw_struct in
  check_int "Codec.struct_min_size" 2 (Wire.Codec.struct_min_size validator);
  check_int "Codec.struct_size_of" (Bytes.length bytes)
    (Wire.Codec.struct_size_of validator bytes 0);
  (match Wire.Codec.wire_size_info_of_validator validator with
  | `Fixed _ -> Alcobar.failf "raw all_bytes struct unexpectedly fixed"
  | `Variable size_of ->
      check_int "Codec.wire_size_info_of_validator" (Bytes.length bytes)
        (size_of bytes 0));
  (try Wire.Codec.validate_struct validator bytes 0 with
  | Wire.Validation_error e | Wire.Parse_error e ->
      Alcobar.failf "validate_struct rejected positive: %a" Wire.pp_parse_error
        e
  | Invalid_argument e -> Alcobar.failf "validate_struct crashed: %s" e);
  match Wire.of_bytes (Wire.Private.Types.struct_typ raw_struct) bytes with
  | Ok () -> ()
  | Error e ->
      Alcobar.failf "Raw.struct_typ direct decode failed: %a"
        Wire.pp_parse_error e

(* [Field.action] reports the action attached via [Field.v]'s [?action] (none on
   a plain field, [Some] on an annotated one), and [Action.pp] renders the
   block. *)
let check_field_action_pp f_plain f_acting action =
  (match Wire.Field.action f_plain with
  | None -> ()
  | Some _ -> Alcobar.failf "Field.action reported an action on a plain field");
  (match Wire.Field.action f_acting with
  | Some _ -> ()
  | None -> Alcobar.failf "Field.action missing on an annotated field");
  if Fmt.str "%a" Wire.Action.pp action = "" then
    Alcobar.failf "Action.pp produced empty output"

(* Build the input/output params and check their name / pp / [Private]
   projections; returns the pair for the codec [check_metadata_helpers] seals. *)
let check_param_metadata () =
  let open Wire in
  let p_in = Param.input "Limit" uint8 in
  let p_out = Param.output "Out" uint8 in
  check_string "Param.name" "Limit" (Param.name p_in);
  check_string "Param.pp" "Limit" (Fmt.str "%a" Param.pp p_in);
  check_string "Private.param_name" "Limit"
    (Private.param_name (Param.decl p_in));
  check_string "Private.param_c_type" "uint8_t"
    (Private.param_c_type (Param.decl p_in));
  check_string "Private.ml_type_of" "int" (Private.ml_type_of uint8);
  check_string "Private.c_type_of" "uint8_t" (Private.c_type_of uint8);
  if Private.param_is_mutable (Param.decl p_in) then
    Alcobar.failf "Param.input reported mutable";
  if not (Private.param_is_mutable (Param.decl p_out)) then
    Alcobar.failf "Param.output reported immutable";
  (p_in, p_out)

let check_metadata_helpers () =
  let open Wire in
  let f_meta =
    Field.v "Meta" ~doc:"metadata"
      ~constraint_:Expr.(bool true && not (bool false))
      uint8
  in
  check_string "Field.name" "Meta" (Field.name f_meta);
  check_int "Field.typ" 1 (Option.get (size (Field.typ f_meta)));
  (match Field.constraint_ f_meta with
  | Some _ -> ()
  | None -> Alcobar.failf "Field.constraint_ missing");
  (match Field.doc f_meta with
  | Some doc -> check_string "Field.doc" "metadata" doc
  | None -> Alcobar.failf "Field.doc missing");
  check_string "Field.pp" "Meta" (Fmt.str "%a" Field.pp f_meta);
  let _ = Field.decl_of_packed (Field.Named f_meta) in
  let _ = Field.decl_of_packed (Field.Anon (Field.anon uint8)) in
  let p_in, p_out = check_param_metadata () in
  let f_src = Field.v "Src" uint8 in
  let copy_action =
    Action.on_success [ Action.assign p_out (Field.ref f_src) ]
  in
  let f_copy = Field.v "Copy" ~action:copy_action uint8 in
  check_field_action_pp f_meta f_copy copy_action;
  let codec =
    Wire.Codec.v "ApiParam"
      ~where:Expr.(Field.ref f_src <= Param.expr p_in)
      (fun src copy -> (src, copy))
      Wire.Codec.[ f_src $ fst; f_copy $ snd ]
  in
  let env = Wire.Codec.env codec |> Param.bind p_in 9 in
  check_int "Param.get input" 9 (Param.get env p_in);
  let env_by_name = Wire.Codec.env codec |> Param.bind_by_name "Limit" 8 in
  check_int "Param.bind_by_name" 8 (Param.get env_by_name p_in);
  let buf = bytes_of_octets [ 7; 1 ] in
  (match Wire.Codec.decode ~env codec buf 0 with
  | Ok _ -> check_int "Param.get output" 7 (Param.get env p_out)
  | Error e -> Alcobar.failf "param/action decode failed: %a" pp_parse_error e);
  check_string "Codec schema name" "ApiParam"
    (Wire.Everparse.project ~mode:`Ffi codec).Wire.Everparse.name;
  let renamed = Wire.Codec.rename "ApiParamRenamed" codec in
  check_string "Codec.rename" "ApiParamRenamed"
    (Wire.Everparse.project ~mode:`Ffi renamed).Wire.Everparse.name;
  if Wire.Codec.doc codec <> None then
    Alcobar.failf "Codec.doc unexpectedly set";
  let pp_value = Fmt.str "%a" (pp_value codec) (7, 1) in
  if not (String.contains pp_value 'S') then
    Alcobar.failf "pp_value did not include field output: %S" pp_value;
  let _ = Wire.Codec.field_ref Wire.Codec.(f_src $ fst) in
  check_int "Wire.size fixed" 1 (Option.get (size uint8));
  match size all_bytes with
  | None -> ()
  | Some n -> Alcobar.failf "Wire.size all_bytes = Some %d" n

let public_raw_struct () =
  let len_field =
    Wire.Everparse.Raw.field "Len" ~constraint_:Wire.Expr.true_
      ~action:
        (Wire.Action.on_success [ Wire.Action.return_bool Wire.Expr.true_ ])
      Wire.uint8
  in
  let data_field = Wire.Everparse.Raw.field "Data" Wire.all_bytes in
  let raw_struct =
    Wire.Everparse.Raw.struct_ "ApiRaw"
      [ len_field; Wire.Everparse.Raw.anon_field Wire.uint8; data_field ]
  in
  (len_field, raw_struct)

let check_everparse_schema raw_struct =
  let schema = Wire.Everparse.Raw.project_struct ~mode:`Ffi raw_struct in
  check_string "Everparse.filename" "ApiRaw.3d" (Wire.Everparse.filename schema);
  if not (Wire.Everparse.uses_wire_ctx schema) then
    Alcobar.failf "schema_of_struct did not use WireCtx";
  (match Wire.Everparse.entrypoint_struct schema with
  | Some _ -> ()
  | None -> Alcobar.failf "schema_of_struct has no entrypoint");
  let plug_fields = Wire.Everparse.plug_fields schema in
  assert_contains "Everparse.plug_fields"
    (List.map (fun f -> f.Wire.Everparse.name) plug_fields)
    [ "Len"; "Data" ];
  if Wire.Everparse.plug_setters schema = [] then
    Alcobar.failf "Everparse.plug_setters empty";
  if Wire.Everparse.extern_fn_names schema = [] then
    Alcobar.failf "Everparse.extern_fn_names empty"

let check_raw_struct_metadata raw_struct len_field =
  let forms = Wire.Everparse.field_action_forms raw_struct in
  if List.length forms <> 3 then
    Alcobar.failf "field_action_forms returned %d fields" (List.length forms);
  check_string "Raw.struct_name" "ApiRaw"
    (Wire.Everparse.Raw.struct_name raw_struct);
  assert_contains "Raw.field_names"
    (Wire.Everparse.Raw.field_names raw_struct)
    [ "Len"; "Data" ];
  let projected =
    Wire.Everparse.Raw.struct_project raw_struct ~name:"ApiProjected"
      ~keep:[ len_field ]
  in
  let _ = Wire.Everparse.Raw.field_ref len_field in
  let _ = Wire.Everparse.Raw.struct_typ raw_struct in
  (match Wire.Everparse.Raw.struct_size raw_struct with
  | None -> ()
  | Some n -> Alcobar.failf "Raw.struct_size all_bytes = Some %d" n);
  assert_contains "Raw.struct_project"
    (Wire.Everparse.Raw.field_names projected)
    [ "Len" ]

let public_param_struct () =
  let p = Wire.Everparse.Raw.param "Limit" Wire.uint8 in
  let p_out = Wire.Everparse.Raw.mutable_param "Out" Wire.uint16be in
  let param_struct =
    Wire.Everparse.Raw.param_struct "ApiParamRaw" [ p; p_out ]
      [ Wire.Everparse.Raw.field "Value" Wire.uint8 ]
  in
  (p, param_struct)

let check_raw_param_metadata param_struct raw_struct =
  if List.length (Wire.Everparse.Raw.struct_params param_struct) <> 2 then
    Alcobar.failf "Raw.struct_params length mismatch";
  assert_contains "Raw.input_param_names"
    (Wire.Everparse.Raw.input_param_names param_struct)
    [ "Limit" ];
  if Wire.Everparse.Raw.input_param_c_types param_struct = [] then
    Alcobar.failf "Raw.input_param_c_types empty";
  if Wire.Everparse.Raw.field_kinds raw_struct = [] then
    Alcobar.failf "Raw.field_kinds empty";
  let app =
    Wire.Everparse.Raw.apply
      (Wire.Everparse.Raw.type_ref "ApiParamRaw")
      [ Wire.int 3 ]
  in
  let _ = Fmt.str "%a" Wire.Everparse.Raw.pp_typ app in
  let _ =
    Fmt.str "%a" Wire.Everparse.Raw.pp_typ
      (Wire.Everparse.Raw.qualified_ref "Api" "Other")
  in
  ()

let check_raw_module_helpers raw_struct p param_struct =
  let module_ =
    Wire.Everparse.Raw.module_ ~doc:"api raw module"
      [
        Wire.Everparse.Raw.define "ApiLimit" 255;
        Wire.Everparse.Raw.extern_probe ~init:true "ApiProbe";
        Wire.Everparse.Raw.extern_probe "ApiProbeNoInit";
        Wire.Everparse.Raw.extern_fn "ApiExtern" [ p ] Wire.uint8;
        Wire.Everparse.Raw.enum_decl "ApiEnum"
          [ ("Zero", 0); ("One", 1) ]
          Wire.uint8;
        Wire.Everparse.Raw.casetype_decl "ApiCase" [] Wire.uint8
          [
            Wire.Everparse.Raw.decl_case 1 Wire.uint8;
            Wire.Everparse.Raw.decl_default Wire.uint16be;
          ];
        Wire.Everparse.Raw.typedef ~entrypoint:true ~export:true ~output:true
          ~doc:"raw entry" raw_struct;
        Wire.Everparse.Raw.typedef param_struct;
      ]
  in
  let rendered = Wire.Everparse.Raw.to_3d module_ in
  if String.length rendered = 0 then Alcobar.failf "Raw.to_3d empty";
  let rendered_enum = Wire.Everparse.Raw.to_3d ~enum_as_type:true module_ in
  if String.length rendered_enum = 0 then
    Alcobar.failf "Raw.to_3d ~enum_as_type empty";
  let _ = Fmt.str "%a" Wire.Everparse.Raw.pp_module module_ in
  let module_schema =
    Wire.Everparse.Raw.of_module ~name:"ApiModule" ~module_ ~wire_size:1
  in
  check_string "Raw.of_module" "ApiModule" module_schema.Wire.Everparse.name;
  let codec = fst5 (api_access_codec ()) in
  let st = Wire.Everparse.Raw.struct_of_codec codec in
  let _ = Wire.Everparse.project ~mode:`Standalone codec in
  let _ = Fmt.str "%a" Wire.Ascii.pp_struct st in
  let _ = Fmt.str "%a" Wire.Ascii.pp_codec codec in
  ()

let check_everparse_helpers () =
  let len_field, raw_struct = public_raw_struct () in
  check_everparse_schema raw_struct;
  check_raw_struct_metadata raw_struct len_field;
  let p, param_struct = public_param_struct () in
  check_raw_param_metadata param_struct raw_struct;
  check_raw_module_helpers raw_struct p param_struct

let once_case name check =
  let ran = ref false in
  Alcobar.test_case name
    Alcobar.[ Alcobar.const () ]
    (fun () ->
      if not !ran then begin
        ran := true;
        check ()
      end)

let check_write_helpers_once () =
  let codec = fst5 (api_access_codec ()) in
  let schema = Wire.Everparse.project ~mode:`Ffi codec in
  let doc_schema = Wire.Everparse.project ~mode:`Standalone codec in
  let outdir =
    Filename.concat (Filename.get_temp_dir_name ()) "wire_fuzz_api"
  in
  (try Sys.mkdir outdir 0o700 with Sys_error _ -> ());
  Wire.Everparse.write ~mode:`Ffi ~outdir [ schema ];
  Wire.Everparse.write ~mode:`Standalone ~outdir ~name:"ApiDoc" [ doc_schema ];
  Wire.Everparse.Raw.to_3d_file ~enum_as_type:true
    (Filename.concat outdir "ApiRawDirect.3d")
    schema.module_

(* Direct checks for a representative slice of the public [Wire] API -- field /
   bitfield / slice accessors and setters, custom sequences, struct validation,
   metadata accessors, the EverParse projection helpers, and the write helpers --
   complementing the round-trip and projection coverage the registry gives. This
   is not a complete mirror of every public symbol: it targets the surfaces with
   non-trivial behaviour worth asserting directly, while the simple scalar
   round-trips stay in the registry suite. The internal codec helpers
   ([raw_decode]/[raw_encode], [embed_encode]/[embed_decode], [field_readers],
   [wire_size_info], the padded blit helpers) are not part of the public facade
   and are not counted as API coverage; they are only exercised transitively by
   the round-trip suite. *)
let api_cases label =
  [
    Alcobar.test_case (label ^ " accessors")
      Alcobar.
        [
          uint8;
          range ~min:0 8;
          range ~min:0 32;
          bytes_fixed 3;
          uint8;
          range ~min:0 8;
          range ~min:0 32;
          bytes_fixed 3;
        ]
      check_api_accessors;
    Alcobar.test_case (label ^ " custom seq")
      Alcobar.[ uint8; uint8; uint8; uint8 ]
      check_custom_seq;
    Alcobar.test_case
      (label ^ " struct validator")
      Alcobar.[ uint8; bytes ]
      check_struct_validator;
    const_case (label ^ " metadata") check_metadata_helpers;
    const_case (label ^ " everparse metadata") check_everparse_helpers;
    once_case (label ^ " write once") check_write_helpers_once;
  ]

(* Project a gen's codec to a 3D schema and pretty-print it: covers the whole
   projection + code-generation path without invoking 3d.exe. A projection that
   raises is a bug (every codec that builds must project). *)
let everparse_cases label g =
  [
    Alcobar.test_case
      (label ^ " projects+prints")
      Alcobar.[ Alcobar.const () ]
      (fun () ->
        match Wire.Everparse.project ~mode:`Ffi g.codec with
        | s -> ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
        | exception e ->
            Alcobar.failf "%s: 3D projection raised %s" label
              (Printexc.to_string e));
  ]

(* Same, over a freshly generated nested composition per sample: arbitrary
   nestings must all project and print. *)
let everparse_nested_cases label depth =
  let draw =
    Alcobar.map
      Alcobar.[ gen_any depth ]
      (fun (Any a) ->
        let comp = a.label and codec = a.g.codec in
        fun () ->
          match Wire.Everparse.project ~mode:`Ffi codec with
          | s -> ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
          | exception e ->
              Alcobar.failf "%s [%s]: 3D projection raised %s" label comp
                (Printexc.to_string e))
  in
  [
    Alcobar.test_case
      (label ^ " projects+prints")
      Alcobar.[ draw ]
      (fun run -> run ());
  ]

let everparse_projectable (_, Pack g) =
  (* A construction-rejected shape ([Invalid_argument]) raises during the full
     projection, and that rejection can fire in [to_3d] (rendering a [field_pos]
     or a non-projectable arithmetic constraint), not just in [schema]. Run both
     so such a codec is excluded from the sweep rather than crashing it. *)
  try
    let s = Wire.Everparse.project ~mode:`Ffi g.codec in
    ignore (Wire.Everparse.Raw.to_3d s.module_ : string);
    true
  with Invalid_argument _ -> false

(* A codec whose projection is rejected at construction ([Invalid_argument]) is
   deliberately not in the EverParse sweep; [afl_everparse_excluded] names them
   so the drop is visible (a future genuinely-unprojectable shape does not vanish
   silently), mirroring [Diff_codecs.excluded]. *)
let afl_everparse_registry, afl_everparse_excluded_pairs =
  List.partition everparse_projectable registry

let afl_everparse_excluded = List.map fst afl_everparse_excluded_pairs

let afl_everparse_cases ?max_len label =
  afl_contract_cases ?max_len label afl_everparse_registry
    ~check:(fun case (Pack g) _input ->
      match
        let s = Wire.Everparse.project ~mode:`Ffi g.codec in
        ignore (Wire.Everparse.Raw.to_3d s.module_ : string)
      with
      | () -> ()
      | exception e ->
          Alcobar.failf "%s: 3D projection raised %s" case
            (Printexc.to_string e))

(* Per sample, pick a random registry gen and round-trip it through the
   alternate entry points, so they cover the whole registry over many samples
   rather than a pinned subset. [of_string] / [of_reader] skip codecs that bind
   a [Param.env], which those entry points cannot thread. *)
let registry_pick =
  Alcobar.choose (List.map (fun (_, p) -> Alcobar.const p) registry)

let ep_direct label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (value, bs) () ->
          if Option.is_none g.env then begin
            let s = string_of_bytes bs in
            (match Wire.of_string g.typ s with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_string decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_string decode failed" label);
            (match Wire.of_string_exn g.typ s with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_string_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_string_exn decode failed" label);
            (match Wire.of_bytes g.typ bs with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_bytes decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_bytes decode failed" label);
            (match Wire.of_bytes_exn g.typ bs with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_bytes_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_bytes_exn decode failed" label);
            if Wire.to_string g.typ value <> s then
              Alcobar.failf "%s of_string reencode mismatch" label;
            if string_of_bytes (Wire.to_bytes g.typ value) <> s then
              Alcobar.failf "%s to_bytes reencode mismatch" label
          end))

let ep_direct_safety label kind adversarial =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      let stream = if adversarial then g.adversarial else g.random in
      Alcobar.map
        Alcobar.[ stream ]
        (fun bs () ->
          if Option.is_none g.env then
            try
              ignore (Wire.of_string g.typ (string_of_bytes bs));
              ignore (Wire.of_bytes g.typ bs)
            with Invalid_argument _ ->
              Alcobar.failf "%s direct %s crashed decoder" label kind))

let ep_streaming label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (value, bs) () ->
          if Option.is_none g.env then begin
            let reader = Bytesrw.Bytes.Reader.of_bytes bs in
            (match Wire.of_reader g.typ reader with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_reader decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_reader decode failed" label);
            let reader = Bytesrw.Bytes.Reader.of_bytes bs in
            (match Wire.of_reader_exn g.typ reader with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_reader_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_reader_exn decode failed" label);
            let buf = Buffer.create (Bytes.length bs) in
            let w = Bytesrw.Bytes.Writer.of_buffer buf in
            Wire.to_writer g.typ value w;
            Bytesrw.Bytes.Writer.write_eod w;
            if Buffer.contents buf <> string_of_bytes bs then
              Alcobar.failf "%s of_reader reencode mismatch" label
          end))

let ep_validate label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (_value, bs) () ->
          let env = positive_env g in
          match
            try
              Wire.Codec.validate ?env g.codec bs 0;
              `Ok
            with
            | Wire.Validation_error _ | Wire.Parse_error _ -> `Reject
            | Invalid_argument _ -> `Crash
          with
          | `Ok -> ()
          | `Reject -> Alcobar.failf "%s validate rejected a positive" label
          | `Crash -> Alcobar.failf "%s validate crashed on a positive" label))

let entry_point_cases label =
  [
    Alcobar.test_case (label ^ " of_string")
      Alcobar.[ ep_direct label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " direct random")
      Alcobar.[ ep_direct_safety label "random" false ]
      (fun run -> run ());
    Alcobar.test_case
      (label ^ " direct adversarial")
      Alcobar.[ ep_direct_safety label "adversarial" true ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " of_reader")
      Alcobar.[ ep_streaming label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " validate")
      Alcobar.[ ep_validate label ]
      (fun run -> run ());
  ]
