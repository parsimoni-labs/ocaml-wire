type t = Optint.t

let pp = Optint.pp
let zero = Optint.zero

(* [Optint.of_int32] keeps the signed view of the word, which is the view this
   type wants: on a wide native [int] a pattern with bit 31 set arrives as the
   negative number those four bytes spell, and where [Optint.t] falls back to a
   boxed [int32] the carrier is the field exactly. This is the one place the
   unsigned sibling has to correct for, and the signed one does not. *)
let min_int = Optint.of_int32 Int32.min_int
let max_int = Optint.of_int32 Int32.max_int

(* Signed on both carriers: a native [int] compares as itself, and [Int32]
   compares signed, which is what a signed field means. *)
let compare = Optint.compare
let equal = Optint.equal
let mask16 = Optint.of_int 0xFFFF
let sign_bit = Optint.shift_left Optint.one 31

(* Sign-extend the 32-bit pattern in [w] through whatever the carrier is, with
   no branch on the platform. Where the native [int] is wide the subtraction
   turns a pattern with bit 31 set into the negative number it spells; where
   [Optint.t] is a boxed [int32] the same two steps wrap back to [w], which is
   already that number. *)
let of_word w = Optint.sub (Optint.logxor w sign_bit) sign_bit

(* Composed from two unboxed 16-bit reads rather than [Bytes.get_int32_*],
   which hands back a boxed [int32] on the way to [Optint.of_int32]. [Codec.get]
   on this field is a hot path the library keeps allocation-free, and the box
   showed up there. *)
let le buf off =
  let lo = Bytes.get_uint16_le buf off in
  let hi = Bytes.get_uint16_le buf (off + 2) in
  of_word Optint.(logor (of_int lo) (shift_left (of_int hi) 16))

let be buf off =
  let hi = Bytes.get_uint16_be buf off in
  let lo = Bytes.get_uint16_be buf (off + 2) in
  of_word Optint.(logor (shift_left (of_int hi) 16) (of_int lo))

(* An int32 field owns exactly 32 bits of signed range. Where the native int is
   wide, [Optint.t] is that int and holds numbers the field cannot; truncating
   one leaves a legal 32-bit value on the wire that no decoder, [validate] or
   generated C validator can tell from the number the caller meant. Where
   [Optint.t] falls back to a boxed [int32] the carrier is exactly the field and
   the guard never fires. *)
let check_encode v =
  if compare v min_int < 0 || compare v max_int > 0 then
    Fmt.invalid_arg "Wire.encode: value %a does not fit a signed 32-bit field"
      Optint.pp v

let to_int32 = Optint.to_int32
let of_int32 = Optint.of_int32

(* Every encoder path (typ-level, compiled field, [Codec.set]) writes an int32
   through here, so the width check belongs here too. *)
let low16 v = Optint.to_int (Optint.logand v mask16)
let high16 v = low16 (Optint.shift_right_logical v 16)

let set_le buf off v =
  check_encode v;
  Bytes.set_uint16_le buf off (low16 v);
  Bytes.set_uint16_le buf (off + 2) (high16 v)

let set_be buf off v =
  check_encode v;
  Bytes.set_uint16_be buf off (high16 v);
  Bytes.set_uint16_be buf (off + 2) (low16 v)

let to_int = Optint.to_int

(* [Int32.to_int] truncates where the native [int] is narrower than 32 bits, so
   round-trip through it to tell a value it held from one it dropped bits of. *)
let to_int_opt v =
  let w = to_int32 v in
  let i = Int32.to_int w in
  if Int32.equal (Int32.of_int i) w then Some i else None

let of_int n =
  let v = Optint.of_int n in
  if compare v min_int < 0 || compare v max_int > 0 then
    Fmt.invalid_arg "Wire.SInt32.of_int: %d is not a signed 32-bit value" n;
  v
