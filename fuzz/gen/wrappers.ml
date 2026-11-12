open Gen
open Leaves

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
    (* The cond is [true_], so [where] narrows nothing: whatever the inner typ
       refuses it must still refuse through the wrapper. *)
    fields = [];
    adversarial_value = inner.adversarial_value;
  }

let bits ?bit_order ~width base =
  let typ = Wire.bits ?bit_order ~width base in
  let base_size =
    match base with Wire.U8 -> 1 | U16 | U16be -> 2 | U32 | U32be -> 4
  in
  let mask = (1 lsl width) - 1 in
  let value_gen = Alcobar.map Alcobar.[ Alcobar.int ] (fun n -> n land mask) in
  let boundaries = [ 0; 1; mask; mask - 1; mask lsr 1 ] in
  scalar_int ~hostile:(above_bit_width width) typ base_size value_gen boundaries

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
    fields = triple_fields ("a", f_a) ("b", f_b) ("c", f_c);
    adversarial_value = None;
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
    fields = pair_fields ("a", f_a) ("b", f_b);
    adversarial_value = None;
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
    fields = pair_fields ("x", f_x) ("y", f_y);
    adversarial_value = None;
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
    fields = [];
    adversarial_value = None;
  }

let array_seq n inner = array_sized (Wire.array_seq Wire.seq_list) n inner

let repeat_seq ?(name = "_rep_seq") ~bytes inner =
  repeat_sized name
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
        ((Wire.UInt8.v a, Wire.UInt8.v b), buf))
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
      positive = (fun env -> Wire.Param.bind limit (Wire.UInt8.v 100) env);
      fuzz =
        Alcobar.map
          Alcobar.[ u8_value_gen ]
          (fun lim env -> Wire.Param.bind limit lim env);
    }
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.range ~min:0 101 ]
      (fun v ->
        let buf = Bytes.create 1 in
        Bytes.set_uint8 buf 0 v;
        (Wire.UInt8.v v, buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 1;
    adversarial = bytes_fixed 1;
    equal = Wire.UInt8.equal;
    env = Some strategy;
    fields = [];
    adversarial_value = None;
  }

(* A 1-byte positive: a uint8 value written to a single-byte buffer. *)
let u8_positive =
  Alcobar.map
    Alcobar.[ Alcobar.uint8 ]
    (fun v ->
      let buf = Bytes.create 1 in
      Bytes.set_uint8 buf 0 v;
      (Wire.UInt8.v v, buf))

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
    equal = Wire.UInt8.equal;
    env = Some { positive = Fun.id; fuzz = Alcobar.const Fun.id };
    fields = [];
    adversarial_value = None;
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
    equal = Wire.UInt8.equal;
    env = None;
    fields = [];
    adversarial_value = None;
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
    fields = [];
    adversarial_value = None;
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
    fields = [];
    adversarial_value = None;
  }

(* Codec whose constraint exercises every integer Expr operator
   (arithmetic, bitwise, comparison, logical, casts). Positives satisfy
   the chained predicate. The selected arithmetic operation uses input values
   at the native-integer boundaries, so adversarials reach every checked
   overflow and zero-divisor path. *)
let build_expr_ops_pred a b =
  let module E = Wire.Expr in
  let selector = E.(a land Wire.int 7) in
  let arith =
    E.if_then_else
      E.(selector = Wire.int 0)
      E.(Wire.int max_int - a + b)
      (E.if_then_else
         E.(selector = Wire.int 1)
         E.(Wire.int min_int + a - b)
         (E.if_then_else
            E.(selector = Wire.int 2)
            E.((a - b) * Wire.int max_int)
            (E.if_then_else E.(selector = Wire.int 3) E.(a / b) E.(a mod b))))
  in
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
        let b = 1 + (b mod 255) in
        let a = if a land 1 = 0 || b = 255 then b else b + 1 in
        let buf = Bytes.create 2 in
        Bytes.set_uint8 buf 0 a;
        Bytes.set_uint8 buf 1 b;
        ((Wire.UInt8.v a, Wire.UInt8.v b), buf))
  in
  let adversarial =
    Alcobar.choose
      (List.map
         (fun octets -> Alcobar.const (bytes_of_octets octets))
         [
           [ 0; 1 ];
           (* addition overflow *)
           [ 1; 2 ];
           (* subtraction underflow *)
           [ 10; 1 ];
           (* multiplication overflow *)
           [ 3; 0 ];
           (* division by zero *)
           [ 4; 0 ];
           (* modulo by zero *)
         ])
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed 2;
    adversarial;
    equal = ( = );
    env = None;
    fields = [];
    adversarial_value = None;
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
        (fun env -> Wire.Param.bind total (Wire.UInt16.v (1 + tail_len)) env);
      fuzz =
        Alcobar.map
          Alcobar.[ Alcobar.uint16 ]
          (fun n env -> Wire.Param.bind total (Wire.UInt16.v n) env);
    }
  in
  let positive =
    Alcobar.map
      Alcobar.[ Alcobar.uint8; Alcobar.bytes_fixed tail_len ]
      (fun h tail ->
        let buf = Bytes.create (1 + tail_len) in
        Bytes.set_uint8 buf 0 h;
        Bytes.blit_string tail 0 buf 1 tail_len;
        ((Wire.UInt8.v h, tail), buf))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_fixed (1 + tail_len);
    adversarial = bytes_fixed (1 + tail_len);
    equal = ( = );
    env = Some strategy;
    fields = [];
    adversarial_value = None;
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
    fields = [];
    adversarial_value = None;
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
            ((Wire.UInt8.v len, b), buf)))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
    fields = [];
    adversarial_value = None;
  }

(* Two-field codec whose second field's [~self_constraint] references
   [Wire.sizeof_this], [Wire.field_pos], and [Wire.sizeof]. [sizeof_this] is
   the description's fixed prefix, so it is 2 here (both fields) wherever it is
   read, not the offset of the field reading it. *)
let sizeof =
  let f_a = Wire.Field.v "a" Wire.uint8 in
  let f_b =
    Wire.Field.v "b" Wire.uint8 ~self_constraint:(fun _ ->
        Wire.Expr.(
          Wire.sizeof_this = Wire.int 2
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
        ((Wire.UInt8.v a, Wire.UInt8.v b), buf))
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
    fields = [];
    adversarial_value = None;
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
      if present then (present_value (Wire.UInt16.v v), gate_on_bytes v)
      else absent ())

(* Dynamic-gate optional: a uint8 [gate] field controls whether a uint16be
   payload is present via [Field.optional ~present:(Field.ref gate <> 0)]. *)
let optional_dynamic =
  gate_codec "_opt_dyn"
    (fun g ->
      Wire.Field.optional "payload" ~present:(gate_present g) Wire.uint16be)
    (gate_positive
       ~present_value:(fun v -> (Wire.UInt8.v 1, Some v))
       ~absent:(fun () -> ((Wire.UInt8.zero, None), bytes_of_octets [ 0 ])))

(* Dynamic-gate optional_or: same shape, with a default value used when
   absent. *)
let optional_or_dynamic =
  gate_codec "_opt_or_dyn"
    (fun g ->
      Wire.Field.optional_or "payload" ~present:(gate_present g)
        ~default:(Wire.UInt16.v 0xCAFE) Wire.uint16be)
    (gate_positive
       ~present_value:(fun v -> (Wire.UInt8.v 1, v))
       ~absent:(fun () ->
         let buf = Bytes.create 3 in
         Bytes.set_uint8 buf 0 0;
         Bytes.set_uint16_be buf 1 0xCAFE;
         ((Wire.UInt8.zero, Wire.UInt16.v 0xCAFE), buf)))

(* Present/absent gate, a span of the drawn length, and a zero tail, encoded
   through the codec so the span's length byte and the payload's offset stay
   consistent. *)
let opt_dyn_span_positive codec =
  Alcobar.map
    Alcobar.
      [
        Alcobar.bool;
        Alcobar.uint16;
        Alcobar.range ~min:0 8;
        Alcobar.range ~min:0 4;
      ]
    (fun present v span_len tail_len ->
      let value =
        ( Wire.UInt8.v (if present then 1 else 0),
          String.make span_len 'x',
          (if present then Some (Wire.UInt16.v v) else None),
          String.make tail_len '\x00' )
      in
      let sz = Wire.Codec.size_of_value codec value in
      let buf = Bytes.create sz in
      Wire.Codec.encode codec value buf 0;
      (value, buf))

(* Dynamic-gate optional at a *runtime* offset. [optional_dynamic] keeps its
   gate and payload adjacent at static offsets, where the payload can never
   land past the end of the buffer. Here a length-prefixed span sits between
   them, so the payload's offset comes from the [len] byte, and the greedy
   tail lets the record's span resolve to the whole buffer -- the record-level
   bounds check passes and the optional's own read is the first thing that can
   leave the buffer. That is the shape whose read has no [field_wire_size] to
   bound-check it. *)
let optional_dyn_after_span =
  let f_gate = Wire.Field.v "gate" Wire.uint8 in
  let f_len = Wire.Field.v "len" Wire.uint8 in
  let f_span =
    Wire.Field.v "span" (Wire.byte_array ~size:(Wire.Field.ref f_len))
  in
  let f_payload =
    Wire.Field.optional "payload" ~present:(gate_present f_gate) Wire.uint16be
  in
  let f_tail = Wire.Field.v "tail" Wire.all_zeros in
  let gate_of (g, _, _, _) = g in
  let len_of (_, span, _, _) = Wire.UInt8.v (String.length span) in
  let span_of (_, span, _, _) = span in
  let payload_of (_, _, payload, _) = payload in
  let tail_of (_, _, _, tail) = tail in
  let codec =
    Wire.Codec.v "_opt_dyn_span"
      (fun gate _len span payload tail -> (gate, span, payload, tail))
      Wire.Codec.
        [
          f_gate $ gate_of;
          f_len $ len_of;
          f_span $ span_of;
          f_payload $ payload_of;
          f_tail $ tail_of;
        ]
  in
  (* Every slot of the shape #262 lived in: a span at a runtime size, an
     optional at a runtime offset behind it, and a greedy tail. Addressing them
     one at a time is what turns [Codec.get] loose on an attacker-supplied
     [len] and [Codec.set] loose next to a neighbour whose offset moves. *)
  let fields =
    [
      slot "gate" f_gate gate_of;
      slot "len" f_len len_of;
      slot "span" f_span span_of;
      slot "payload" f_payload payload_of;
      slot "tail" f_tail tail_of;
    ]
  in
  {
    codec;
    typ = Wire.codec codec;
    positive = opt_dyn_span_positive codec;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
    fields;
    adversarial_value = None;
  }

(* {1 Union} *)

type union_body =
  | Short of Wire.UInt16.t
  | Long of Wire.UInt32.t
  | Unknown of Wire.UInt8.t * Wire.UInt8.t

let union_body_field f_kind =
  Wire.Field.union "body" ~on:f_kind ~casetype:"_union_body"
    [
      Wire.case ~index:(Wire.UInt8.v 1) Wire.uint16be
        ~inject:(fun v -> Short v)
        ~project:(function Short v -> Some v | _ -> None);
      Wire.case ~index:(Wire.UInt8.v 2) Wire.uint32be
        ~inject:(fun v -> Long v)
        ~project:(function Long v -> Some v | _ -> None);
      Wire.default Wire.uint8
        ~inject:(fun tag v -> Unknown (tag, v))
        ~project:(function Unknown (tag, v) -> Some (tag, v) | _ -> None);
    ]

(* One of the three branches, a span of the drawn length and a tail byte,
   encoded through the codec so the tag and the length byte stay consistent with
   the value. The default's tag is drawn from the ones no case claims. *)
let union_span_positive codec =
  Alcobar.map
    Alcobar.
      [
        Alcobar.range ~min:0 3;
        Alcobar.uint16;
        Alcobar.range ~min:3 253;
        Alcobar.range ~min:0 8;
        Alcobar.uint8;
      ]
    (fun case v tag span_len tail ->
      let body =
        match case with
        | 0 -> Short (Wire.UInt16.v v)
        | 1 -> Long (Wire.UInt32.of_int (v * 0x10001))
        | _ -> Unknown (Wire.UInt8.v tag, Wire.UInt8.v (v land 0xff))
      in
      let value = (String.make span_len 'x', body, Wire.UInt8.v tail) in
      let buf = Bytes.create (Wire.Codec.size_of_value codec value) in
      Wire.Codec.encode codec value buf 0;
      (value, buf))

(* A union switching on a tag that is not next to it: a length-prefixed span
   sits between the two, so the union's offset comes from the [len] byte, and a
   fixed field follows it, so the union's own size places that one. The default
   branch takes every tag the two cases leave, which makes each buffer long
   enough a record. *)
let union_after_span =
  let f_kind = Wire.Field.v "kind" Wire.uint8 in
  let f_len = Wire.Field.v "len" Wire.uint8 in
  let f_span =
    Wire.Field.v "span" (Wire.byte_array ~size:(Wire.Field.ref f_len))
  in
  let f_body = union_body_field f_kind in
  let f_tail = Wire.Field.v "tail" Wire.uint8 in
  let kind_of (_, body, _) =
    match body with
    | Short _ -> Wire.UInt8.v 1
    | Long _ -> Wire.UInt8.v 2
    | Unknown (tag, _) -> tag
  in
  let len_of (span, _, _) = Wire.UInt8.v (String.length span) in
  let span_of (span, _, _) = span in
  let body_of (_, body, _) = body in
  let tail_of (_, _, tail) = tail in
  let codec =
    Wire.Codec.v "_union_span"
      (fun _kind _len span body tail -> (span, body, tail))
      Wire.Codec.
        [
          f_kind $ kind_of;
          f_len $ len_of;
          f_span $ span_of;
          f_body $ body_of;
          f_tail $ tail_of;
        ]
  in
  {
    codec;
    typ = Wire.codec codec;
    positive = union_span_positive codec;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
    fields =
      [
        slot "kind" f_kind kind_of;
        slot "len" f_len len_of;
        slot "span" f_span span_of;
        slot "body" f_body body_of;
        slot "tail" f_tail tail_of;
      ];
    adversarial_value = None;
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

(* Convert one composer case to a Wire casetype branch. The tag is written as
   an [int] and lifted into the discriminator's carrier here. The composer's
   default branch ignores the matched tag and always re-encodes the fixed [t],
   so adapt to the tag-aware API. *)
let casetype_case_def_with lift (Case c) =
  match (c.index, c.default_tag) with
  | Some i, _ ->
      Wire.case ~index:(lift i) c.inner.typ ~inject:c.inject ~project:c.project
  | None, Some t ->
      let t = lift t in
      Wire.default c.inner.typ
        ~inject:(fun _tag w -> c.inject w)
        ~project:(fun a -> Option.map (fun w -> (t, w)) (c.project a))
  | None, None -> invalid_arg "casetype_u8: case must supply ~index or ~tag"

(* The discriminator is a parameter because a casetype dispatches on any enum,
   and an enum takes any base with an integer view: the tag reaches the
   case-index projection wrapped in a [lookup]'s map, behind a [where], or over
   a 64-bit base. Generating only a bare [uint8] tag left every one of those
   carriers unreached. [lift] carries a case's integer index into whatever the
   tag's carrier turns out to be. *)
let casetype_tagged name tag lift cases =
  let typ =
    Wire.casetype name tag (List.map (casetype_case_def_with lift) cases)
  in
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
    fields = [];
    adversarial_value = None;
  }

let casetype_u8 name cases = casetype_tagged name Wire.uint8 Wire.UInt8.v cases

(* A direct [Wire.casetype] over a wider discriminator, with a default branch
   that preserves the actual unclaimed tag. This mirrors [Wire.default]'s API
   more closely than [casetype_u8]'s fixed re-encode tag helper. *)
let casetype_u16be_default =
  let typ =
    Wire.casetype "WideTagged" Wire.uint16be
      [
        Wire.case ~index:(Wire.UInt16.v 0x0102) Wire.uint8
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
              (`A (Wire.UInt8.v v), buf))
      | false ->
          Alcobar.map
            Alcobar.[ Alcobar.uint16; Alcobar.uint16 ]
            (fun tag v ->
              let tag = if tag = 0x0102 then 0x0103 else tag in
              let buf = Bytes.create 4 in
              Bytes.set_uint16_be buf 0 tag;
              Bytes.set_uint16_be buf 2 v;
              (`Other (Wire.UInt16.v tag, Wire.UInt16.v v), buf)))
  in
  {
    codec;
    typ;
    positive;
    random = bytes_any;
    adversarial = bytes_any;
    equal = ( = );
    env = None;
    fields = [];
    adversarial_value = None;
  }
