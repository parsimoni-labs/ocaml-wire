type t = Optint.t

(* [Optint.t] is the native [int] only where that holds more than 32 bits, and
   there every uint32 is a non-negative [int] whose natural order is already the
   unsigned one. Where it falls back to [Int32] the values with bit 31 set are
   negative, so the unsigned comparison has to be asked for by name: an
   inherited signed one ranks 0xFFFFFFFF, the largest uint32, below 1. Chosen
   once at module initialisation, not per call. *)
let compare =
  if Sys.int_size > 32 then Optint.compare
  else fun a b -> Int32.unsigned_compare (Optint.to_int32 a) (Optint.to_int32 b)

let equal = Optint.equal
let zero = Optint.zero
let pp = Optint.pp

(* Compose two unboxed 16-bit reads/writes through [Optint]. On a 64-bit host
   [Optint.t] is an unboxed native [int], so the shifts and [logor] stay in
   registers exactly like the old [int] code (measured identical, zero alloc);
   where the native [int] is narrower than 32 bits [Optint.t] falls back to a
   boxed [int32], which is what keeps a value with bit 31 set (a TCP sequence
   number) from being truncated. *)

let mask16 = Optint.of_int 0xFFFF

(* Computed at run time: an 0xFFFF_FFFF int literal lands in the bytecode as
   an out-of-range constant on a narrow-int target (wasm_of_ocaml warns). *)
let mask32 = Int64.to_int 0xFFFF_FFFFL
let field_mask = Optint.of_int mask32

(* A uint32 field owns exactly 32 bits. Where the native int is wide, [Optint.t]
   is that int and holds numbers the field cannot; masking one leaves a legal
   32-bit value on the wire that no decoder, [validate] or generated C validator
   can tell from the number the caller meant. Where [Optint.t] falls back to a
   boxed [int32] the carrier is exactly the field, [field_mask] is all-ones, and
   the guard never fires: every representable value is a legal uint32 there. *)
let check_encode v =
  if not (Optint.equal (Optint.logand v field_mask) v) then
    Fmt.invalid_arg
      "Wire.encode: value %a does not fit an unsigned 32-bit field" Optint.pp v

let le buf off =
  let lo = Bytes.get_uint16_le buf off in
  let hi = Bytes.get_uint16_le buf (off + 2) in
  Optint.(logor (of_int lo) (shift_left (of_int hi) 16))

let be buf off =
  let hi = Bytes.get_uint16_be buf off in
  let lo = Bytes.get_uint16_be buf (off + 2) in
  Optint.(logor (shift_left (of_int hi) 16) (of_int lo))

let low16 v = Optint.to_int (Optint.logand v mask16)

(* Every encoder path -- typ-level, compiled field, [Codec.set] -- writes a
   uint32 through here, so the width check belongs here too. *)
let set_le buf off v =
  check_encode v;
  Bytes.set_uint16_le buf off (low16 v);
  Bytes.set_uint16_le buf (off + 2) (low16 (Optint.shift_right_logical v 16))

let set_be buf off v =
  check_encode v;
  Bytes.set_uint16_be buf off (low16 (Optint.shift_right_logical v 16));
  Bytes.set_uint16_be buf (off + 2) (low16 v)

let to_int = Optint.to_int

(* Refuses rather than masks. Masking turns a number the field cannot hold into
   a legal 32-bit value on the wire that nothing downstream can tell from the
   one the caller meant, which is the failure this library exists to avoid.
   Only a native [int] wider than the field can supply such a number. *)
let of_int n =
  let v = Optint.of_int (n land mask32) in
  if not (Optint.equal (Optint.of_int n) v) then
    Fmt.invalid_arg "Wire.UInt32.of_int: %d is not an unsigned 32-bit value" n;
  v

let to_int32 = Optint.to_int32

(* [Optint.of_int32] keeps the signed view, so where [Optint.t] is a wide native
   int a word with bit 31 set arrives as a negative number: not what [le] and
   [be] decode those same bytes to, and not a value a uint32 field may hold.
   Masking back to the field brings it to the one canonical representation. *)
let of_int32 n = Optint.logand (Optint.of_int32 n) field_mask
