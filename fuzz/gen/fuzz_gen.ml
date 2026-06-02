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
    Bytesrw.Bytes.Slice.make b ~first:0 ~length:(Bytes.length b)
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

(* Dynamic-gate optional: a uint8 [gate] field controls whether a uint16be
   payload is present via [Field.optional ~present:(Field.ref gate <> 0)]. *)
let optional_dynamic =
  gate_codec "_opt_dyn"
    (fun g ->
      Wire.Field.optional "payload" ~present:(gate_present g) Wire.uint16be)
    (Alcobar.map
       Alcobar.[ Alcobar.bool; Alcobar.uint16 ]
       (fun present v ->
         if present then ((1, Some v), gate_on_bytes v)
         else
           let buf = Bytes.create 1 in
           Bytes.set_uint8 buf 0 0;
           ((0, None), buf)))

(* Dynamic-gate optional_or: same shape, with a default value used when
   absent. *)
let optional_or_dynamic =
  gate_codec "_opt_or_dyn"
    (fun g ->
      Wire.Field.optional_or "payload" ~present:(gate_present g) ~default:0xCAFE
        Wire.uint16be)
    (Alcobar.map
       Alcobar.[ Alcobar.bool; Alcobar.uint16 ]
       (fun present v ->
         if present then ((1, v), gate_on_bytes v)
         else
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
  let positive =
    Alcobar.dynamic_bind (Alcobar.range ~min:0 n) (fun i ->
        let (Case c) = List.nth cases (i mod n) in
        Alcobar.map
          Alcobar.[ c.inner.positive ]
          (fun (w, inner_bytes) ->
            let tag =
              match (c.index, c.default_tag) with
              | Some i, _ -> i
              | None, Some t -> t
              | None, None -> 0
            in
            let n_inner = Bytes.length inner_bytes in
            let buf = Bytes.create (1 + n_inner) in
            Bytes.set_uint8 buf 0 tag;
            Bytes.blit inner_bytes 0 buf 1 n_inner;
            (c.inject w, buf)))
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
    env = combine_env_strategies (List.map (fun (Case c) -> c.inner.env) cases);
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
       Fall back to the concatenation when the record needs a param env that is
       not threaded here (encode then raises). *)
    let positive =
      Alcobar.map
        Alcobar.[ positives_of builder [] fields ]
        (fun (v, concat) ->
          match
            let buf = Bytes.create (Wire.Codec.size_of_value codec v) in
            Wire.Codec.encode codec v buf 0;
            buf
          with
          | buf -> (v, buf)
          | exception Invalid_argument _ -> (v, concat))
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
let fixed_leaves : any list =
  [
    Any { g = uint8; size = Some 1; label = "u8" };
    Any { g = uint16be; size = Some 2; label = "u16" };
    Any { g = uint32be; size = Some 4; label = "u32" };
    Any { g = uint64be; size = Some 8; label = "u64" };
    Any { g = int8; size = Some 1; label = "i8" };
    Any { g = int16be; size = Some 2; label = "i16" };
    Any { g = int32be; size = Some 4; label = "i32" };
    Any { g = float32be; size = Some 4; label = "f32" };
    Any { g = float64be; size = Some 8; label = "f64" };
    Any { g = uint_var ~endian:Wire.Big 3; size = Some 3; label = "uv3" };
    Any { g = empty; size = Some 0; label = "unit" };
    Any { g = byte_array 3; size = Some 3; label = "ba3" };
    Any { g = byte_slice 4; size = Some 4; label = "bs4" };
    Any
      {
        g = byte_array_where 3 ~per_byte:printable_byte;
        size = Some 3;
        label = "baw3";
      };
    Any { g = bits ~width:3 Wire.U8; size = Some 1; label = "bits3" };
    Any { g = bit uint8; size = Some 1; label = "bit" };
    Any
      {
        g = enum "E" [ ("A", 1); ("B", 2); ("C", 3) ];
        size = Some 1;
        label = "enum";
      };
    Any
      {
        g = variants "V" [ ("X", `X); ("Y", `Y); ("Z", `Z) ];
        size = Some 1;
        label = "var";
      };
    Any
      { g = lookup [ 'a'; 'b'; 'c'; 'd' ] uint8; size = Some 1; label = "lkp" };
    Any { g = bounded_u8 ~min:10 ~max:100; size = Some 1; label = "bnd" };
    Any { g = finite_float64; size = Some 8; label = "finf" };
    Any { g = nan_float64; size = Some 8; label = "nanf" };
    (* Sub-codec leaves with a [~where] / [~self_constraint] over expressions,
       so composing them exercises an embedded constrained sub-codec. *)
    Any { g = expr_ops; size = Some 2; label = "expr" };
    Any { g = sizeof; size = Some 2; label = "sizeof" };
    Any { g = codec_where; size = Some 2; label = "cwhere" };
  ]

(* Self-delimiting / trailing variable leaves. *)
let var_leaves : any list =
  [
    Any { g = zeroterm; size = None; label = "zt" };
    Any { g = zeroterm_at_most 6; size = None; label = "zt6" };
    Any { g = all_zeros; size = None; label = "az" };
    (* Dynamic-gate optionals: a gate byte then a present/absent (optional) or
       value-driven (optional_or) payload. *)
    Any { g = optional_dynamic; size = None; label = "optdyn" };
    Any { g = optional_or_dynamic; size = None; label = "optordyn" };
  ]

(* Param/env-bearing leaves: sub-codecs whose [~where] / [action] reads a
   [Param.input]. Wrapping one carries its env up (see [with_env]); decoding
   the wrapper threads the env down to the embedded sub-codec. Composing these
   exposes any wrapper that drops the env on the way down. *)
let env_leaves : any list =
  [
    Any { g = param_input; size = Some 1; label = "param" };
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
let check_positive label g (value, bs) =
  let bs_str = string_of_bytes bs in
  let env = positive_env g in
  (match Wire.Codec.decode ?env g.codec bs 0 with
  | Ok decoded ->
      if not (g.equal decoded value) then
        Alcobar.failf "%s positive decode mismatch" label
  | Error _ -> Alcobar.failf "%s positive decode failed" label);
  let sz = Wire.Codec.size_of_value g.codec value in
  if sz <> Bytes.length bs then
    Alcobar.failf "%s size_of_value = %d but canonical encoding is %d" label sz
      (Bytes.length bs);
  let out = Bytes.create sz in
  (try Wire.Codec.encode ?env:(positive_env g) g.codec value out 0
   with Invalid_argument _ -> Alcobar.failf "%s positive encode raised" label);
  if string_of_bytes out <> bs_str then
    Alcobar.failf "%s positive reencode mismatch" label

let test_cases label g =
  let pos_case =
    Alcobar.test_case (label ^ " positive")
      Alcobar.[ g.positive ]
      (check_positive label g)
  in
  let safety_case kind stream =
    match g.env with
    | None ->
        Alcobar.test_case
          (label ^ " " ^ kind)
          Alcobar.[ stream ]
          (fun bs ->
            try ignore (Wire.Codec.decode g.codec bs 0)
            with Invalid_argument _ ->
              Alcobar.failf "%s %s crashed decoder" label kind)
    | Some s ->
        Alcobar.test_case
          (label ^ " " ^ kind)
          Alcobar.[ stream; s.fuzz ]
          (fun bs build_env ->
            let env = Some (build_env (Wire.Codec.env g.codec)) in
            try ignore (Wire.Codec.decode ?env g.codec bs 0)
            with Invalid_argument _ ->
              Alcobar.failf "%s %s crashed decoder" label kind)
  in
  [
    pos_case;
    safety_case "random" g.random;
    safety_case "adversarial" g.adversarial;
  ]

(* Cross-field sizes: a [byte_array] whose [~size] reads a preceding int field.
   The size-source field is drawn from several int-valued field types -- in
   particular [optional_or], whose value used to resolve to 0 in a size
   expression, decoding the span as empty and raising on encode. Each source
   builds a two-field [(len, data)] codec round-tripped through [test_cases],
   which checks decode, [size_of_value], and re-encode against canonical bytes
   assembled here (the length-field bytes followed by [len] data bytes). *)
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
  let safety kind stream =
    Alcobar.test_case
      (label ^ " " ^ kind)
      Alcobar.[ stream ]
      (fun (NB (g, comp, bs, mk)) ->
        let env = Option.map (fun f -> f (Wire.Codec.env g.codec)) mk in
        try ignore (Wire.Codec.decode ?env g.codec bs 0)
        with Invalid_argument _ ->
          Alcobar.failf "%s %s [%s] crashed decoder" label kind comp)
  in
  [
    Alcobar.test_case (label ^ " positive")
      Alcobar.[ positive ]
      (fun (NS (g, comp, pos)) ->
        try check_positive (Fmt.str "%s [%s]" label comp) g pos
        with Failure m -> Alcobar.failf "%s [%s] raised: %s" label comp m);
    safety "random" (with_bytes false);
    safety "adversarial" (with_bytes true);
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
  let validate_one ?env bs =
    try
      Wire.Codec.validate ?env g.codec bs 0;
      `Ok
    with
    | Wire.Validation_error _ -> `Reject
    | Invalid_argument _ -> `Crash
    | Wire.Parse_error _ -> `Reject
  in
  let pos_case =
    Alcobar.test_case
      (label ^ " validate positive")
      Alcobar.[ g.positive ]
      (fun (_value, bs) ->
        let env = positive_env g in
        match validate_one ?env bs with
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
            match validate_one bs with
            | `Ok | `Reject -> ()
            | `Crash -> Alcobar.failf "%s validate %s crashed" label kind)
    | Some s ->
        Alcobar.test_case
          (label ^ " validate " ^ kind)
          Alcobar.[ stream; s.fuzz ]
          (fun bs build_env ->
            let env = Some (build_env (Wire.Codec.env g.codec)) in
            match validate_one ?env bs with
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
    Alcobar.test_case (label ^ " random")
      Alcobar.[ g.random; env_stream ]
      check_reject;
    Alcobar.test_case (label ^ " adversarial")
      Alcobar.[ g.adversarial; env_stream ]
      check_reject;
  ]

(* {1 Canonical registry and EverParse drivers}

   Every fuzz suite drives off [registry] rather than its own hand-written
   list, so a generated codec is exercised on the OCaml round-trip path
   ([test_cases]) and the 3D projection path ([everparse_cases] /
   [everparse_3d_cases]) from one source. *)

type packed = Pack : 'a t -> packed

let codec g = g.codec

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

let scalar_gens =
  [
    ("uint8", Pack uint8);
    ("uint16", Pack uint16);
    ("uint16be", Pack uint16be);
    ("uint32", Pack uint32);
    ("uint32be", Pack uint32be);
    ("uint63", Pack uint63);
    ("uint63be", Pack uint63be);
    ("uint64", Pack uint64);
    ("uint64be", Pack uint64be);
    ("int8", Pack int8);
    ("int16", Pack int16);
    ("int16be", Pack int16be);
    ("int32", Pack int32);
    ("int32be", Pack int32be);
    ("int64", Pack int64);
    ("int64be", Pack int64be);
    ("float32", Pack float32);
    ("float32be", Pack float32be);
    ("float64", Pack float64);
    ("float64be", Pack float64be);
    ("empty", Pack empty);
    ("uint_var(3,big)", Pack (uint_var ~endian:Wire.Big 3));
    ("uint_var(7,big)", Pack (uint_var ~endian:Wire.Big 7));
    ("bit(uint8)", Pack (bit uint8));
    ("bit(uint16)", Pack (bit uint16));
  ]

let byte_gens =
  [
    ("byte_array(5)", Pack (byte_array 5));
    ("byte_slice(3)", Pack (byte_slice 3));
    ( "byte_array_where(4)",
      Pack
        (byte_array_where 4 ~per_byte:(fun b ->
             Wire.Expr.(b >= Wire.int 0x20 && b <= Wire.int 0x7e))) );
    ("all_bytes", Pack all_bytes);
    ("all_zeros", Pack all_zeros);
    ("zeroterm", Pack zeroterm);
    ("zeroterm_at_most(8)", Pack (zeroterm_at_most 8));
  ]

let bits_gens =
  [
    ("bits(3,U8)", Pack (bits ~width:3 Wire.U8));
    ("bits(10,U16be)", Pack (bits ~width:10 Wire.U16be));
    ("bits(4,U16be)", Pack (bits ~width:4 Wire.U16be));
    ("bits(4,U32,Lsb)", Pack (bits ~bit_order:Wire.Lsb_first ~width:4 Wire.U32));
  ]

let wrapper_gens =
  [
    ( "map(uint8)",
      Pack (map ~decode:(fun n -> n * 2) ~encode:(fun n -> n / 2) uint8) );
    ("variants", Pack (variants "Flag" [ ("A", `A); ("B", `B); ("C", `C) ]));
    ("enum", Pack (enum "Color" [ ("Red", 1); ("Green", 2); ("Blue", 3) ]));
    ("bounded_u8", Pack (bounded_u8 ~min:10 ~max:100));
    ("where(uint8)", Pack (where uint8));
    ("lookup", Pack (lookup [ 'a'; 'b'; 'c'; 'd' ] uint8));
    ("optional(uint8)", Pack (optional uint8));
    ("optional(false)", Pack (optional ~present:false uint8));
    ("optional_or(uint8)", Pack (optional_or ~default:0 uint8));
    ("optional_or(false)", Pack (optional_or ~present:false ~default:42 uint8));
    ("optional_dynamic", Pack optional_dynamic);
    ("optional_or_dynamic", Pack optional_or_dynamic);
    ("nested(4,uint16be)", Pack (nested 4 uint16be));
    ("nested_at_most(4,uint16be)", Pack (nested_at_most 4 uint16be));
  ]

let composite_gens =
  [
    ("array(3,uint16be)", Pack (array 3 uint16be));
    ("array_seq(3,uint16be)", Pack (array_seq 3 uint16be));
    ("array(2,record)", Pack (array 2 registry_record));
    ("repeat(8,uint8)", Pack (repeat ~bytes:8 uint8));
    ("repeat_seq(8,uint8)", Pack (repeat_seq ~bytes:8 uint8));
    ("record", Pack registry_record);
    ("casetype_u8", Pack registry_casetype);
    ("field_anon", Pack field_anon);
  ]

let param_action_gens =
  [
    ("action", Pack action);
    ("action_on_act", Pack action_on_act);
    ("expr_ops", Pack expr_ops);
    ("sizeof", Pack sizeof);
    ("codec_where", Pack codec_where);
    ("rest_bytes", Pack rest_bytes);
    ("param_input", Pack param_input);
    ("finite_float64", Pack finite_float64);
    ("nan_float64", Pack nan_float64);
  ]

let registry : (string * packed) list =
  scalar_gens @ byte_gens @ bits_gens @ wrapper_gens @ composite_gens
  @ param_action_gens

(* Project a gen's codec to a 3D schema and pretty-print it: covers the whole
   projection + code-generation path without invoking 3d.exe. A projection that
   raises is a bug (every codec that builds must project). *)
let everparse_cases label g =
  [
    Alcobar.test_case
      (label ^ " projects+prints")
      Alcobar.[ Alcobar.const () ]
      (fun () ->
        match Wire.Everparse.schema g.codec with
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
          match Wire.Everparse.schema codec with
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
            if Wire.to_string g.typ value <> s then
              Alcobar.failf "%s of_string reencode mismatch" label
          end))

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
    Alcobar.test_case (label ^ " of_reader")
      Alcobar.[ ep_streaming label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " validate")
      Alcobar.[ ep_validate label ]
      (fun run -> run ());
  ]
