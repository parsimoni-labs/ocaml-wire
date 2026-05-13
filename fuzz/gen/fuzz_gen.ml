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
  positive : unit -> Wire.Param.env;
      (* Builder used for the positive stream. Must bind every
         [Param.input] consistently with the positive value generator. *)
  fuzz : (unit -> Wire.Param.env) Alcobar.gen;
      (* Builder gen used for the random and adversarial streams. Fuzz
         [Param.input] values independently of the input bytes. *)
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

(* Build a leaf [t] from a typ and three Alcobar generators. *)
let leaf ~equal ~typ ~value_gen ~random ~adversarial =
  let codec = codec_of_typ typ in
  let encode v =
    let sz = Wire.Codec.size_of_value codec v in
    let buf = Bytes.create sz in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  let adversarial_bytes =
    Alcobar.map Alcobar.[ adversarial ] (fun v -> encode v)
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

let nested n inner =
  let typ = Wire.nested ~size:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, inner_bytes) ->
        let buf = Bytes.create n in
        Bytes.blit inner_bytes 0 buf 0 (min n (Bytes.length inner_bytes));
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.[ inner.adversarial ]
      (fun b ->
        let buf = Bytes.create n in
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

let repeat ~bytes:total_bytes inner =
  let f_total = Wire.Field.v "_total" Wire.uint16be in
  let f_items =
    Wire.Field.repeat "_items" ~size:(Wire.Field.ref f_total) inner.typ
  in
  let codec =
    Wire.Codec.v "_rep"
      (fun _ xs -> xs)
      Wire.Codec.[ (f_total $ fun _ -> total_bytes); (f_items $ fun xs -> xs) ]
  in
  let typ = Wire.codec codec in
  let positive =
    (* Generate a list of inner positives whose concatenated bytes have
       length <= total_bytes. *)
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, bs) ->
        let n = Bytes.length bs in
        if n = 0 || n > total_bytes then ([], Bytes.empty)
        else
          let count = total_bytes / n in
          let items = List.init count (fun _ -> v) in
          let payload = Bytes.create (count * n) in
          for i = 0 to count - 1 do
            Bytes.blit bs 0 payload (i * n) n
          done;
          let buf = Bytes.create (2 + (count * n)) in
          Bytes.set_uint16_be buf 0 (count * n);
          Bytes.blit payload 0 buf 2 (count * n);
          (items, buf))
  in
  let list_equal a b =
    List.length a = List.length b && List.for_all2 inner.equal a b
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal;
    env = None;
  }

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

let nested_at_most n inner =
  let typ = Wire.nested_at_most ~size:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, inner_bytes) ->
        let buf = Bytes.create n in
        Bytes.blit inner_bytes 0 buf 0 (min n (Bytes.length inner_bytes));
        (v, buf))
  in
  let adversarial =
    Alcobar.map
      Alcobar.[ inner.adversarial ]
      (fun b ->
        let buf = Bytes.create n in
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

let variants name cases =
  let typ = Wire.variants name cases Wire.uint8 in
  let codec = codec_of_typ typ in
  let n = List.length cases in
  let value_gen =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 (max 1 n) ]
      (fun i -> snd (List.nth cases (i mod max 1 n)))
  in
  let encode v =
    let sz = Wire.Codec.size_of_value codec v in
    let buf = Bytes.create sz in
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
    equal = ( = );
    env = None;
  }

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

let array n inner =
  let typ = Wire.array ~len:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ sample_array_of_positives inner.positive n ]
      (fun (vs, bss) -> (vs, concat_bytes_list bss))
  in
  let list_equal a b =
    List.length a = List.length b && List.for_all2 inner.equal a b
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal;
    env = None;
  }

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
  let typ = Wire.lookup table inner.typ in
  let codec = codec_of_typ typ in
  let n = max 1 (List.length table) in
  let value_gen =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 n ]
      (fun i -> List.nth table (i mod n))
  in
  let encode v =
    let sz = Wire.Codec.size_of_value codec v in
    let buf = Bytes.create sz in
    Wire.Codec.encode codec v buf 0;
    buf
  in
  let positive = Alcobar.map Alcobar.[ value_gen ] (fun v -> (v, encode v)) in
  {
    codec;
    typ;
    positive;
    random = inner.random;
    adversarial = inner.adversarial;
    equal = ( = );
    env = None;
  }

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

let array_seq n inner =
  let typ = Wire.array_seq Wire.seq_list ~len:(Wire.int n) inner.typ in
  let codec = codec_of_typ typ in
  let positive =
    Alcobar.map
      Alcobar.[ sample_array_of_positives inner.positive n ]
      (fun (vs, bss) -> (vs, concat_bytes_list bss))
  in
  let list_equal a b =
    List.length a = List.length b && List.for_all2 inner.equal a b
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal;
    env = None;
  }

let repeat_seq ~bytes:total_bytes inner =
  let f_total = Wire.Field.v "_total" Wire.uint16be in
  let f_items =
    Wire.Field.repeat_seq "_items" ~seq:Wire.seq_list
      ~size:(Wire.Field.ref f_total) inner.typ
  in
  let codec =
    Wire.Codec.v "_rep_seq"
      (fun _ xs -> xs)
      Wire.Codec.[ (f_total $ fun _ -> total_bytes); (f_items $ fun xs -> xs) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ inner.positive ]
      (fun (v, bs) ->
        let n = Bytes.length bs in
        if n = 0 || n > total_bytes then ([], Bytes.empty)
        else
          let count = total_bytes / n in
          let items = List.init count (fun _ -> v) in
          let payload = Bytes.create (count * n) in
          for i = 0 to count - 1 do
            Bytes.blit bs 0 payload (i * n) n
          done;
          let buf = Bytes.create (2 + (count * n)) in
          Bytes.set_uint16_be buf 0 (count * n);
          Bytes.blit payload 0 buf 2 (count * n);
          (items, buf))
  in
  let list_equal a b =
    List.length a = List.length b && List.for_all2 inner.equal a b
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = list_equal;
    env = None;
  }

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
      positive = (fun () -> Wire.Codec.env codec |> Wire.Param.bind limit 100);
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint8 ]
          (fun lim () -> Wire.Codec.env codec |> Wire.Param.bind limit lim);
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

(* Codec with [Action.on_success [assign out (Field.ref f); abort?]]
   exercising Action.assign, return_bool, abort, if_, var, on_act. *)
let action =
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  let action =
    Wire.Action.on_success
      [
        Wire.Action.var "local" (Wire.Field.ref f_v);
        Wire.Action.assign out (Wire.Field.ref f_v);
        Wire.Action.if_ Wire.Expr.true_
          [ Wire.Action.return_bool Wire.Expr.true_ ]
          None;
      ]
  in
  let f_with_action = Wire.Field.v "v" ~action Wire.uint8 in
  let codec =
    Wire.Codec.v "_action"
      (fun v -> v)
      Wire.Codec.[ (f_with_action $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let make_env () = Wire.Codec.env codec in
  let strategy = { positive = make_env; fuzz = Alcobar.const make_env } in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8 ]
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
  (* Positive is unused by [reject_cases]; kept for type-shape parity. *)
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8 ]
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
    env = None;
  }

(* Same as [action] but using [Action.on_act] in place of
   [on_success]. *)
let action_on_act =
  let out = Wire.Param.output "out" Wire.uint8 in
  let f_v = Wire.Field.v "v" Wire.uint8 in
  let action =
    Wire.Action.on_act
      [
        Wire.Action.assign out (Wire.Field.ref f_v);
        Wire.Action.return_bool Wire.Expr.true_;
      ]
  in
  let f_with_action = Wire.Field.v "v" ~action Wire.uint8 in
  let codec =
    Wire.Codec.v "_action_on_act"
      (fun v -> v)
      Wire.Codec.[ (f_with_action $ fun v -> v) ]
  in
  let typ = Wire.codec codec in
  let make_env () = Wire.Codec.env codec in
  let strategy = { positive = make_env; fuzz = Alcobar.const make_env } in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8 ]
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
      positive =
        (fun () -> Wire.Codec.env codec |> Wire.Param.bind total (1 + tail_len));
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint16 ]
          (fun n () -> Wire.Codec.env codec |> Wire.Param.bind total n);
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

(* Dynamic-gate optional: a uint8 [gate] field controls whether a uint16be
   payload is present via [Field.optional ~present:(Field.ref gate <> 0)]. *)
let optional_dynamic =
  let f_gate = Wire.Field.v "gate" Wire.uint8 in
  let f_payload =
    Wire.Field.optional "payload"
      ~present:Wire.Expr.(Wire.Field.ref f_gate <> Wire.int 0)
      Wire.uint16be
  in
  let codec =
    Wire.Codec.v "_opt_dyn"
      (fun gate payload -> (gate, payload))
      Wire.Codec.[ (f_gate $ fun (g, _) -> g); (f_payload $ fun (_, p) -> p) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.bool; Alcobar.uint16 ]
      (fun present v ->
        if present then (
          let buf = Bytes.create 3 in
          Bytes.set_uint8 buf 0 1;
          Bytes.set_uint16_be buf 1 v;
          ((1, Some v), buf))
        else
          let buf = Bytes.create 1 in
          Bytes.set_uint8 buf 0 0;
          ((0, None), buf))
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

(* Dynamic-gate optional_or: same shape, with a default value used when
   absent. *)
let optional_or_dynamic =
  let f_gate = Wire.Field.v "gate" Wire.uint8 in
  let f_payload =
    Wire.Field.optional_or "payload"
      ~present:Wire.Expr.(Wire.Field.ref f_gate <> Wire.int 0)
      ~default:0xCAFE Wire.uint16be
  in
  let codec =
    Wire.Codec.v "_opt_or_dyn"
      (fun gate payload -> (gate, payload))
      Wire.Codec.[ (f_gate $ fun (g, _) -> g); (f_payload $ fun (_, p) -> p) ]
  in
  let typ = Wire.codec codec in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.bool; Alcobar.uint16 ]
      (fun present v ->
        if present then (
          let buf = Bytes.create 3 in
          Bytes.set_uint8 buf 0 1;
          Bytes.set_uint16_be buf 1 v;
          ((1, v), buf))
        else
          let buf = Bytes.create 3 in
          Bytes.set_uint8 buf 0 0;
          Bytes.set_uint16_be buf 1 0xCAFE;
          ((0, 0xCAFE), buf))
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

let casetype_u8 name cases =
  let case_defs =
    List.map
      (fun (Case c) ->
        match (c.index, c.default_tag) with
        | Some i, _ ->
            Wire.case ~index:i c.inner.typ ~inject:c.inject ~project:c.project
        | None, Some t ->
            Wire.default ~tag:t c.inner.typ ~inject:c.inject ~project:c.project
        | None, None ->
            invalid_arg "casetype_u8: case must supply ~index or ~tag")
      cases
  in
  let typ = Wire.casetype name Wire.uint8 case_defs in
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

  let rec wire_codec_fields : type f r.
      (f, r) fields -> (f, r) Wire.Codec.fields = function
    | [] -> Wire.Codec.[]
    | f :: rest ->
        Wire.Codec.( $ ) (Wire.Field.v f.name f.gen.typ) f.getter
        :: wire_codec_fields rest

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

  let v : type f r.
      string -> ?equal:(r -> r -> bool) -> f -> (f, r) fields -> r t =
   fun name ?(equal = ( = )) builder fields ->
    let codec = Wire.Codec.v name builder (wire_codec_fields fields) in
    let typ = Wire.codec codec in
    let positives = field_positives fields in
    let n_fields = field_count fields in
    let positive = positives_of builder [] fields in
    let random = random_of fields in
    let adversarial =
      let per_slot =
        List.init n_fields (fun i ->
            concat_bytes_gens (adversarial_at i positives fields))
      in
      if per_slot = [] then Alcobar.const Bytes.empty
      else Alcobar.choose per_slot
    in
    { codec; typ; positive; random; adversarial; equal; env = None }
end

(* {1 Test driver} *)

(* Wrap each of a gen's three streams in an Alcobar test_case. Positive
   asserts round-trip; the other two assert crash-safety. *)
let positive_env g =
  match g.env with Some s -> Some (s.positive ()) | None -> None

let test_cases label g =
  let pos_case =
    Alcobar.test_case (label ^ " positive")
      Alcobar.[ g.positive ]
      (fun (value, bs) ->
        let bs_str = string_of_bytes bs in
        let env = positive_env g in
        (match Wire.Codec.decode ?env g.codec bs 0 with
        | Ok decoded ->
            if not (g.equal decoded value) then
              Alcobar.failf "%s positive decode mismatch" label
        | Error _ -> Alcobar.failf "%s positive decode failed" label);
        let out = Bytes.create (Bytes.length bs) in
        (try Wire.Codec.encode ?env:(positive_env g) g.codec value out 0
         with Invalid_argument _ ->
           Alcobar.failf "%s positive encode raised" label);
        if string_of_bytes out <> bs_str then
          Alcobar.failf "%s positive reencode mismatch" label)
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
            let env = Some (build_env ()) in
            try ignore (Wire.Codec.decode ?env g.codec bs 0)
            with Invalid_argument _ ->
              Alcobar.failf "%s %s crashed decoder" label kind)
  in
  [
    pos_case;
    safety_case "random" g.random;
    safety_case "adversarial" g.adversarial;
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
            let env = Some (build_env ()) in
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
    match g.env with
    | Some s -> s.fuzz
    | None -> Alcobar.const (fun () -> Wire.Codec.env g.codec)
  in
  let check_reject bs build_env =
    let env = Some (build_env ()) in
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
