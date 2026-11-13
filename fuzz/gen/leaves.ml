open Gen

(* {1 Leaves} *)

let scalar_int ?hostile typ size value_gen boundaries =
  let g =
    leaf ~equal:Int.equal ~typ ~value_gen ~random:(bytes_fixed size)
      ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))
  in
  { g with adversarial_value = hostile }

(* [uint64] and [int64] are carried in an [int64], which is exactly the eight
   bytes written: every value of it is in range, so there is no hostile value to
   draw and no guard for one to exercise. *)
(* [uint64] is carried in a [Wire.UInt64.t]. Like [scalar_int64] every bit
   pattern is a legal value, so there is no hostile value to draw; it needs its
   own equality because the carrier is abstract. *)
let scalar_uint64 typ size value_gen boundaries =
  leaf ~equal:Wire.UInt64.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let scalar_int64 typ size value_gen boundaries =
  leaf ~equal:Int64.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

(* [int32] is carried in a [Wire.SInt32.t], whose only constructors are the
   4-byte pattern and a checked [of_int]: every value of it is a legal signed
   32-bit field, so there is no hostile value to draw and no guard for one to
   exercise. Same reasoning as [scalar_int64]. *)
let scalar_sint32 typ size value_gen boundaries =
  leaf ~equal:Wire.SInt32.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

(* [int8], [int16], [uint8] and [uint16] are carried in a [Wire.SInt8.t], a
   [Wire.SInt16.t], a [Wire.UInt8.t] and a [Wire.UInt16.t], whose only
   constructor refuses a number the field cannot hold: every value of them is a
   legal field value, so there is no hostile value to draw and no guard for one
   to exercise. Same reasoning as [scalar_sint32]. *)
let scalar_sint8 typ size value_gen boundaries =
  leaf ~equal:Wire.SInt8.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let scalar_sint16 typ size value_gen boundaries =
  leaf ~equal:Wire.SInt16.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let scalar_uint8 typ size value_gen boundaries =
  leaf ~equal:Wire.UInt8.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let scalar_uint16 typ size value_gen boundaries =
  leaf ~equal:Wire.UInt16.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

(* [uint32] is carried in a [Wire.UInt32.t], whose only constructors are the
   4-byte pattern and a checked [of_int]: every value of it is a legal unsigned
   32-bit field, so there is no hostile value to draw and no guard for one to
   exercise. Same reasoning as [scalar_int64] and [scalar_sint32]. *)
let scalar_optint typ size value_gen boundaries =
  leaf ~equal:Wire.UInt32.equal ~typ ~value_gen ~random:(bytes_fixed size)
    ~adversarial:(Alcobar.choose (List.map Alcobar.const boundaries))

let u8_boundaries = List.map Wire.UInt8.v [ 0; 1; 0x7F; 0x80; 0xFE; 0xFF ]

let u16_boundaries =
  List.map Wire.UInt16.v [ 0; 1; 0x7F; 0x80; 0x7FFF; 0x8000; 0xFFFE; 0xFFFF ]

let u32_boundaries =
  List.map Wire.UInt32.of_int
    [ 0; 1; 0xFFFF; 0x7FFF_FFFF; 0x8000_0000; 0xFFFF_FFFE; 0xFFFF_FFFF ]

let u64_boundaries_i64 =
  Int64.[ zero; one; of_int 0xFFFF_FFFF; max_int; min_int; -1L; sub max_int 1L ]

let s8_boundaries = List.map Wire.SInt8.v [ -128; -1; 0; 1; 127 ]
let s16_boundaries = List.map Wire.SInt16.v [ -0x8000; -1; 0; 1; 0x7FFF ]

let s32_boundaries =
  List.map Wire.SInt32.of_int32 Int32.[ min_int; minus_one; zero; one; max_int ]

let s64_boundaries_i64 = Int64.[ min_int; -1L; zero; one; max_int ]
let u8_value_gen = Alcobar.map Alcobar.[ Alcobar.uint8 ] Wire.UInt8.v
let uint8 = scalar_uint8 Wire.uint8 1 u8_value_gen u8_boundaries
let u16_value_gen = Alcobar.map Alcobar.[ Alcobar.uint16 ] Wire.UInt16.v
let uint16 = scalar_uint16 Wire.uint16 2 u16_value_gen u16_boundaries
let uint16be = scalar_uint16 Wire.uint16be 2 u16_value_gen u16_boundaries

let masked_u32 =
  Alcobar.map
    Alcobar.[ Alcobar.int ]
    (fun n -> Wire.UInt32.of_int (n land Wire.Private.UInt32.mask32))

let uint32 = scalar_optint Wire.uint32 4 masked_u32 u32_boundaries
let uint32be = scalar_optint Wire.uint32be 4 masked_u32 u32_boundaries
let u64_value_gen = Alcobar.map Alcobar.[ Alcobar.int64 ] Wire.UInt64.of_int64
let u64_boundaries = List.map Wire.UInt64.of_int64 u64_boundaries_i64
let uint64 = scalar_uint64 Wire.uint64 8 u64_value_gen u64_boundaries
let uint64be = scalar_uint64 Wire.uint64be 8 u64_value_gen u64_boundaries
let s8_value_gen = Alcobar.map Alcobar.[ Alcobar.int8 ] Wire.SInt8.v
let int8 = scalar_sint8 Wire.int8 1 s8_value_gen s8_boundaries
let s16_value_gen = Alcobar.map Alcobar.[ Alcobar.int16 ] Wire.SInt16.v
let int16 = scalar_sint16 Wire.int16 2 s16_value_gen s16_boundaries
let int16be = scalar_sint16 Wire.int16be 2 s16_value_gen s16_boundaries
let int32_value_gen = Alcobar.map Alcobar.[ Alcobar.int32 ] Wire.SInt32.of_int32
let int32 = scalar_sint32 Wire.int32 4 int32_value_gen s32_boundaries
let int32be = scalar_sint32 Wire.int32be 4 int32_value_gen s32_boundaries
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
    (* A fixed-size byte span is exact: any other length must be refused. *)
    fields = [];
    adversarial_value = Some (byte_lengths_around n);
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
  let value_gen = Alcobar.bytes_fixed n in
  let positive =
    Alcobar.map
      Alcobar.[ value_gen ]
      (fun s ->
        let slice = slice_of_string s in
        (slice, encode slice))
  in
  {
    codec;
    typ;
    positive;
    random;
    adversarial;
    equal = slice_equal;
    env = None;
    (* Same exact-span rule as [byte_array], through the zero-copy slice. *)
    fields = [];
    adversarial_value =
      Some (Alcobar.map Alcobar.[ byte_lengths_around n ] slice_of_string);
  }

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
    fields = [];
    adversarial_value = None;
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
     surface [Non_zero_padding], not raise. *)
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
    (* Padding decode only accepts as zeros: a non-zero byte must be refused. *)
    fields = [];
    adversarial_value =
      Some
        (Alcobar.map
           Alcobar.[ Alcobar.range ~min:1 16; Alcobar.range ~min:1 255 ]
           (fun n byte -> String.make n (Char.chr byte)));
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
    (* A value carrying its own terminator cannot round-trip. *)
    fields = [];
    adversarial_value =
      Some (Alcobar.map Alcobar.[ Alcobar.bytes ] (fun s -> s ^ "\000x"));
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
    (* Too long for the region once the terminator is counted, or carrying its
       own terminator. *)
    fields = [];
    adversarial_value =
      Some
        (Alcobar.choose
           [
             Alcobar.bytes_fixed n;
             Alcobar.bytes_fixed (n + 1);
             Alcobar.map Alcobar.[ Alcobar.bytes_fixed n ] (fun s -> s ^ "\000");
           ]);
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
    (* Arbitrary bytes at the declared length and around it: most violate the
       refinement, the rest exercise the exact-length rule. *)
    fields = [];
    adversarial_value = Some (byte_lengths_around n);
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
    fields = [];
    adversarial_value = None;
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
    (* [map] adds no encode path of its own, it hands the mapped value to the
       inner typ, so a value the inner typ refuses must still be refused here. *)
    fields = [];
    adversarial_value =
      Option.map
        (fun g -> Alcobar.map Alcobar.[ g ] decode)
        inner.adversarial_value;
  }

let uint_var ~endian size =
  let typ = Wire.uint ~endian size in
  let codec = codec_of_typ typ in
  let max_v = (1 lsl (size * 8)) - 1 in
  let value_gen =
    Alcobar.map
      Alcobar.[ Alcobar.int ]
      (fun n -> Optint.Int63.of_int (n land max_v))
  in
  let encode v =
    let buf = Bytes.create size in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  let boundaries = List.map Optint.Int63.of_int [ 0; 1; max_v; max_v - 1 ] in
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
    equal = ( = );
    env = None;
    fields = [];
    adversarial_value = Some (above_byte_width size);
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
    fields = [];
    adversarial_value = None;
  }

let exact_int64 typ cases = exact_cases ~typ ~equal:Int64.equal cases
let exact_uint64 typ cases = exact_cases ~typ ~equal:Wire.UInt64.equal cases
let exact_optint typ cases = exact_cases ~typ ~equal:Wire.UInt32.equal cases
let exact_sint32 typ cases = exact_cases ~typ ~equal:Wire.SInt32.equal cases
let exact_sint16 typ cases = exact_cases ~typ ~equal:Wire.SInt16.equal cases
let exact_uint16 typ cases = exact_cases ~typ ~equal:Wire.UInt16.equal cases
let u16 = Wire.UInt16.v

let uint16_endian_edges =
  exact_uint16 Wire.uint16
    [
      (u16 0x0001, bytes_of_octets [ 0x01; 0x00 ]);
      (u16 0x1234, bytes_of_octets [ 0x34; 0x12 ]);
      (u16 0x8000, bytes_of_octets [ 0x00; 0x80 ]);
      (u16 0xFFFF, bytes_of_octets [ 0xFF; 0xFF ]);
    ]

let uint16be_endian_edges =
  exact_uint16 Wire.uint16be
    [
      (u16 0x0001, bytes_of_octets [ 0x00; 0x01 ]);
      (u16 0x1234, bytes_of_octets [ 0x12; 0x34 ]);
      (u16 0x8000, bytes_of_octets [ 0x80; 0x00 ]);
      (u16 0xFFFF, bytes_of_octets [ 0xFF; 0xFF ]);
    ]

let uint32_endian_edges =
  exact_optint Wire.uint32
    [
      (Wire.UInt32.of_int 0x01234567, bytes_of_octets [ 0x67; 0x45; 0x23; 0x01 ]);
      (Wire.UInt32.of_int 0x80000000, bytes_of_octets [ 0x00; 0x00; 0x00; 0x80 ]);
      (Wire.UInt32.of_int 0xFFFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF ]);
    ]

let uint32be_endian_edges =
  exact_optint Wire.uint32be
    [
      (Wire.UInt32.of_int 0x01234567, bytes_of_octets [ 0x01; 0x23; 0x45; 0x67 ]);
      (Wire.UInt32.of_int 0x80000000, bytes_of_octets [ 0x80; 0x00; 0x00; 0x00 ]);
      (Wire.UInt32.of_int 0xFFFFFFFF, bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF ]);
    ]

let u64 = Wire.UInt64.of_int64

let uint64_endian_edges =
  exact_uint64 Wire.uint64
    [
      ( u64 0x0123456789ABCDEFL,
        bytes_of_octets [ 0xEF; 0xCD; 0xAB; 0x89; 0x67; 0x45; 0x23; 0x01 ] );
      ( u64 (-2L),
        bytes_of_octets [ 0xFE; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF ] );
    ]

let uint64be_endian_edges =
  exact_uint64 Wire.uint64be
    [
      ( u64 0x0123456789ABCDEFL,
        bytes_of_octets [ 0x01; 0x23; 0x45; 0x67; 0x89; 0xAB; 0xCD; 0xEF ] );
      ( u64 (-2L),
        bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFF; 0xFE ] );
    ]

let sint16 = Wire.SInt16.v

let int16_endian_edges =
  exact_sint16 Wire.int16
    [
      (sint16 (-0x8000), bytes_of_octets [ 0x00; 0x80 ]);
      (sint16 (-2), bytes_of_octets [ 0xFE; 0xFF ]);
      (sint16 0x1234, bytes_of_octets [ 0x34; 0x12 ]);
      (sint16 0x7FFF, bytes_of_octets [ 0xFF; 0x7F ]);
    ]

let int16be_endian_edges =
  exact_sint16 Wire.int16be
    [
      (sint16 (-0x8000), bytes_of_octets [ 0x80; 0x00 ]);
      (sint16 (-2), bytes_of_octets [ 0xFF; 0xFE ]);
      (sint16 0x1234, bytes_of_octets [ 0x12; 0x34 ]);
      (sint16 0x7FFF, bytes_of_octets [ 0x7F; 0xFF ]);
    ]

let sint32 n = Wire.SInt32.of_int32 (Int32.of_int n)

let int32_endian_edges =
  exact_sint32 Wire.int32
    [
      ( Wire.SInt32.of_int32 Int32.min_int,
        bytes_of_octets [ 0x00; 0x00; 0x00; 0x80 ] );
      (sint32 (-2), bytes_of_octets [ 0xFE; 0xFF; 0xFF; 0xFF ]);
      (sint32 0x01234567, bytes_of_octets [ 0x67; 0x45; 0x23; 0x01 ]);
      ( Wire.SInt32.of_int32 Int32.max_int,
        bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0x7F ] );
    ]

let int32be_endian_edges =
  exact_sint32 Wire.int32be
    [
      ( Wire.SInt32.of_int32 Int32.min_int,
        bytes_of_octets [ 0x80; 0x00; 0x00; 0x00 ] );
      (sint32 (-2), bytes_of_octets [ 0xFF; 0xFF; 0xFF; 0xFE ]);
      (sint32 0x01234567, bytes_of_octets [ 0x01; 0x23; 0x45; 0x67 ]);
      ( Wire.SInt32.of_int32 Int32.max_int,
        bytes_of_octets [ 0x7F; 0xFF; 0xFF; 0xFF ] );
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
  let cases = List.map (fun (v, bs) -> (Optint.Int63.of_int v, bs)) cases in
  exact_cases ~typ:(Wire.uint ~endian size) ~equal:( = ) cases

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

let optional ?(name = "_opt") ?(present = true) inner =
  let f =
    Wire.Field.optional
      ~present:(if present then Wire.Expr.true_ else Wire.Expr.false_)
      "v" inner.typ
  in
  let codec = Wire.Codec.v name (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ] in
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
    fields = [];
    adversarial_value = None;
  }

let optional_or ?(name = "_opt_or") ?(present = true) ~default inner =
  let f =
    Wire.Field.optional_or
      ~present:(if present then Wire.Expr.true_ else Wire.Expr.false_)
      ~default "v" inner.typ
  in
  let codec = Wire.Codec.v name (fun v -> v) Wire.Codec.[ (f $ fun v -> v) ] in
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
    fields = [];
    adversarial_value = None;
  }

let list_equal eq a b = List.length a = List.length b && List.for_all2 eq a b

(* [repeat] and [repeat_seq] differ only in the codec name and the items
   field constructor (list vs seq); the length-prefixed payload generation is
   identical. *)
(* A short stable tag for a shape, so two structurally different sub-codecs
   never take the same name. Sharing one would make the projection refuse the
   schema, and deduping them would leave both validated against whichever was
   reached first. Same shape yields the same tag, which is the sharing we do
   want. *)
let shape_tag label = Fmt.str "%06x" (Hashtbl.hash label land 0xffffff)

let repeat_sized name make_items ~bytes:total_bytes inner =
  let f_total = Wire.Field.v "_total" Wire.uint16be in
  let f_items = make_items ~size:(Wire.Field.ref f_total) inner.typ in
  let codec =
    (* The length prefix is the actual encoded items length, not the budget:
       a budget that is not a multiple of the element size encodes fewer bytes
       than the budget, and the prefix must match what is written. *)
    let items_bytes =
      match Wire.Codec.wire_size_opt inner.codec with
      | Some size -> fun xs -> Wire.UInt16.v (size * List.length xs)
      | None ->
          fun xs ->
            Wire.UInt16.v
              (List.fold_left
                 (fun acc v -> acc + Wire.Codec.size_of_value inner.codec v)
                 0 xs)
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
    fields = [];
    adversarial_value = None;
  }

let repeat ?(name = "_rep") ~bytes inner =
  repeat_sized name
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
    fields = [];
    adversarial_value = None;
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
  (* An [array ~len:n] is an exact cardinality, so a shorter or longer list must
     be refused rather than silently clipped or padded. *)
  let wrong_cardinality k =
    Alcobar.map
      Alcobar.[ sample_array_of_positives inner.positive k ]
      (fun (vs, _) -> vs)
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal inner.equal;
    env = None;
    fields = [];
    adversarial_value =
      Some
        (Alcobar.choose
           (List.map wrong_cardinality
              (List.sort_uniq Int.compare [ 0; max 0 (n - 1); n + 1 ])));
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
      (fun i -> Wire.UInt8.v (List.nth valid_values (i mod n)))
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
    equal = Wire.UInt8.equal;
    env = None;
    fields = [];
    adversarial_value =
      Some
        (Alcobar.map
           Alcobar.[ unlisted_enum_gen ~mask:0xFF valid_values ]
           Wire.UInt8.v);
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
    Alcobar.map Alcobar.[ u8_value_gen ] (fun v -> (v, encode v))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Wire.UInt8.equal;
    env = None;
    fields = [];
    adversarial_value = None;
  }

(* The case values are written as [int], but the base's carrier is whatever
   holds them on the wire, so [of_int] lifts a drawn case into it and [equal]
   compares two of them. *)
let enum_base ~typ ~size ~of_int ~equal name cases =
  let codec = codec_of_typ (Wire.enum name cases typ) in
  let valid_values = List.map snd cases in
  let value_gen =
    let n = max 1 (List.length valid_values) in
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 n ]
      (fun i -> of_int (List.nth valid_values (i mod n)))
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
    equal;
    env = None;
    fields = [];
    adversarial_value =
      Some
        (Alcobar.map
           Alcobar.
             [ unlisted_enum_gen ~mask:((1 lsl (size * 8)) - 1) valid_values ]
           of_int);
  }

let enum_open_base ~typ ~size ~of_int ~equal name cases =
  let typ = Wire.enum_open name cases typ in
  let codec = codec_of_typ typ in
  let max_value =
    if size >= Sys.int_size / 8 then max_int else (1 lsl (size * 8)) - 1
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.int ]
      (fun n ->
        let v = of_int (n land max_value) in
        (v, encode_via_codec codec v))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed size;
    adversarial = bytes_fixed size;
    equal;
    env = None;
    fields = [];
    adversarial_value = None;
  }

let enum_u16be =
  enum_base ~typ:Wire.uint16be ~size:2 ~of_int:Wire.UInt16.v
    ~equal:Wire.UInt16.equal "WideCode"
    [ ("Zero", 0); ("One", 1); ("High", 0xBEEF) ]

let enum_open_u16be =
  enum_open_base ~typ:Wire.uint16be ~size:2 ~of_int:Wire.UInt16.v
    ~equal:Wire.UInt16.equal "OpenWideCode"
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
    fields = [];
    adversarial_value = None;
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
    (* Named by its range: two ranges are two shapes, and a projection keys
       its typedefs by name. *)
    Wire.Codec.v
      (Fmt.str "_bounded_%d_%d" min max)
      (fun v -> v)
      Wire.Codec.[ (f $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let value_gen =
    Alcobar.map Alcobar.[ Alcobar.range ~min (max - min + 1) ] Wire.UInt8.v
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
    equal = Wire.UInt8.equal;
    env = None;
    (* Out-of-range on both sides of the field's [~self_constraint]. *)
    fields = [];
    adversarial_value =
      Some
        (Alcobar.choose
           (List.map
              (fun n -> Alcobar.const (Wire.UInt8.v n))
              (List.filter
                 (fun n -> n >= 0 && n <= 0xFF && (n < min || n > max))
                 [ min - 1; max + 1; 0; 0xFF ])));
  }

(* [bounded_u8] generalised to any unsigned integer [inner_typ] of [size] bytes
   with a range [self_constraint]. [set] writes a boundary value as raw bytes and
   [max_val] is the type's maximum (so out-of-type boundary samples are dropped).
   Covers the constraint path on wider and big-endian scalars, not just uint8. *)
let bounded_int ~inner_typ ~size ~set ~max_val ~min ~max ~of_int ~equal =
  let f =
    Wire.Field.v "v" inner_typ ~self_constraint:(fun r ->
        Wire.Expr.(r >= Wire.int min && r <= Wire.int max))
  in
  let codec =
    Wire.Codec.v
      (Fmt.str "_bounded_int%d_%d_%d" size min max)
      (fun v -> v)
      Wire.Codec.[ (f $ fun v -> v) ]
  in
  let value_gen =
    Alcobar.map Alcobar.[ Alcobar.range ~min (max - min + 1) ] of_int
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
    equal;
    env = None;
    fields = [];
    adversarial_value = None;
  }

let bounded_u16be =
  bounded_int ~inner_typ:Wire.uint16be ~size:2 ~set:Bytes.set_uint16_be
    ~max_val:0xFFFF ~min:1000 ~max:60000 ~of_int:Wire.UInt16.v
    ~equal:Wire.UInt16.equal

let bounded_u32be =
  bounded_int ~inner_typ:Wire.uint32be ~size:4
    ~set:(fun b o n -> Bytes.set_int32_be b o (Int32.of_int n))
    ~max_val:0x7FFF_FFFF ~min:1000 ~max:1_000_000 ~of_int:Wire.UInt32.of_int
    ~equal:Wire.UInt32.equal

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
        ((Wire.UInt8.v a, Wire.UInt8.v b), buf))
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
    (* [a = b] fails the codec-level [~where] [a < b]. *)
    fields = pair_fields ("a", f_a) ("b", f_b);
    adversarial_value =
      Some
        (Alcobar.map
           Alcobar.[ Alcobar.range ~min:0 0x100 ]
           (fun a ->
             let a = Wire.UInt8.v a in
             (a, a)));
  }

let u8_pair_bytes a b =
  let buf = Bytes.create 2 in
  Bytes.set_uint8 buf 0 a;
  Bytes.set_uint8 buf 1 b;
  buf

let u8_pair_positive gen =
  Alcobar.map gen (fun a b ->
      ((Wire.UInt8.v a, Wire.UInt8.v b), u8_pair_bytes a b))

let u8_pair_adversarial gen = Alcobar.map gen (fun a b -> u8_pair_bytes a b)

(* The same out-of-contract pair the decoder is handed, read back as a value so
   encode is put through it too. *)
let u8_pair_hostile bytes_gen =
  Alcobar.map
    Alcobar.[ bytes_gen ]
    (fun b -> (Wire.UInt8.get b 0, Wire.UInt8.get b 1))

let u8_pair_record name f_a f_b ~positive ~adversarial =
  let codec =
    Wire.Codec.v name (fun a b -> (a, b)) Wire.Codec.[ f_a $ fst; f_b $ snd ]
  in
  let adversarial_bytes = u8_pair_adversarial adversarial in
  {
    codec;
    typ = Wire.codec codec;
    positive = u8_pair_positive positive;
    random = bytes_fixed 2;
    adversarial = adversarial_bytes;
    equal = ( = );
    env = None;
    fields = pair_fields ("a", f_a) ("b", f_b);
    adversarial_value = Some (u8_pair_hostile adversarial_bytes);
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
        (Wire.UInt64.of_int64 v, buf))
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
    equal = Wire.UInt64.equal;
    env = None;
    fields = [];
    adversarial_value = None;
  }
