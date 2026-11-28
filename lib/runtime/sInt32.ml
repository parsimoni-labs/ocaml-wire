type t = Carrier.t

let pp = Carrier.pp
let zero = Carrier.zero

(* [Carrier.of_int32] keeps the signed view of the word, which is the view this
   type wants: on a wide native [int] a pattern with bit 31 set arrives as the
   negative number those four bytes spell, and where [Carrier.t] falls back to a
   boxed [int32] the carrier is the field exactly. This is the one place the
   unsigned sibling has to correct for, and the signed one does not. *)
let min_int = Carrier.of_int32 Int32.min_int
let max_int = Carrier.of_int32 Int32.max_int

(* Signed on both carriers: a native [int] compares as itself, and [Int32]
   compares signed, which is what a signed field means. *)
let compare a b = Carrier.compare a b
let equal a b = Carrier.equal a b
let mask16 = Carrier.of_int 0xFFFF

(* On a 64-bit host [Carrier.t] is the native [int], and matching on
   [Carrier.is_immediate] lets each conversion below use it as one: the
   compiler keeps the [int32] in a register, so a read is one load and a
   conversion no call. Elsewhere [Carrier.t] is the [int32] itself. *)
let le buf off : t =
  match Carrier.is_immediate with
  | True -> Int32.to_int (Bytes.get_int32_le buf off)
  | False -> Bytes.get_int32_le buf off

let be buf off : t =
  match Carrier.is_immediate with
  | True -> Int32.to_int (Bytes.get_int32_be buf off)
  | False -> Bytes.get_int32_be buf off

(* An int32 field owns exactly 32 bits of signed range. Where the native int is
   wide, [Carrier.t] is that int and holds numbers the field cannot; truncating
   one leaves a legal 32-bit value on the wire that no decoder, [validate] or
   generated C validator can tell from the number the caller meant. Where
   [Carrier.t] falls back to a boxed [int32] the carrier is exactly the field and
   the guard never fires. *)
let check_encode v =
  if compare v min_int < 0 || compare v max_int > 0 then
    invalid_arg
      ("Wire.encode: value " ^ Carrier.to_string v
     ^ " does not fit a signed 32-bit field")

(* Exact: every [t] holds a signed 32-bit value. *)
let[@inline] to_int32 (v : t) : int32 =
  match Carrier.is_immediate with True -> Int32.of_int v | False -> v

let[@inline] of_int32 (n : int32) : t =
  match Carrier.is_immediate with True -> Int32.to_int n | False -> n

(* Every encoder path, [Codec.encode] and [Codec.View.set] alike, writes an
   int32 through here, so the width check belongs here too. *)
let low16 v = Carrier.to_int (Carrier.logand v mask16)
let high16 v = low16 (Carrier.shift_right_logical v 16)

let set_le buf off v =
  check_encode v;
  Bytes.set_uint16_le buf off (low16 v);
  Bytes.set_uint16_le buf (off + 2) (high16 v)

let set_be buf off v =
  check_encode v;
  Bytes.set_uint16_be buf off (high16 v);
  Bytes.set_uint16_be buf (off + 2) (low16 v)

(* Optint's emulated conversions assume 31-bit ints. JavaScript has 32-bit
   ints, so use the exact Int32 conversion there instead of losing the sign. *)
let to_int v =
  if Sys.int_size = 32 then Int32.to_int (to_int32 v) else Carrier.to_int v

(* [Int32.to_int] truncates where the native [int] is narrower than 32 bits, so
   round-trip through it to tell a value it held from one it dropped bits of. *)
let to_int_opt v =
  let w = to_int32 v in
  let i = Int32.to_int w in
  if Int32.equal (Int32.of_int i) w then Some i else None

let of_int n =
  if Sys.int_size <= 32 then of_int32 (Int32.of_int n)
  else
    let v = Carrier.of_int n in
    if compare v min_int < 0 || compare v max_int > 0 then
      invalid_arg
        ("Wire.SInt32.of_int: " ^ string_of_int n
       ^ " is not a signed 32-bit value");
    v

let unsafe_le buf off : t =
  match Carrier.is_immediate with
  | True -> Int32.to_int (Unchecked.i32le buf off)
  | False -> Unchecked.i32le buf off

let unsafe_be buf off : t =
  match Carrier.is_immediate with
  | True -> Int32.to_int (Unchecked.i32be buf off)
  | False -> Unchecked.i32be buf off

let int_eq : (t, int) Type.eq option =
  match Carrier.is_immediate with True -> Some Type.Equal | False -> None
