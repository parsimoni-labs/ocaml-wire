open Dsl

(* Encode-side guards. Decode rejects each of these values, so emitting one
   would put bytes on the wire that this library's own decoder, and the
   EverParse validator generated from the same schema, refuse. Each raises
   [Invalid_argument]: an unencodable value is a caller error, not malformed
   input. *)

(* A casetype value no branch projects has no encoding at all, so it has no size
   either. One raiser for the size path and both encode paths, so a caller
   sizing a buffer for such a value is told the same thing, in the same words,
   as one trying to write it. *)
let raise_no_matching_case () =
  invalid_arg "Wire.casetype: encoding a value no case matches"

(* Membership in a closed [enum]: the single rule both halves use, so decode and
   encode agree on which values a closed enum admits. An open enum names known
   codes without restricting the set, so it has no encode guard at all. *)
let enum_values cases = List.map snd cases

let rec enum_member valid (v : int) =
  match valid with [] -> false | x :: rest -> x = v || enum_member rest v

(* Decode-side membership scans the case list itself. The [valid] list an
   [Invalid_enum] carries is built on the failure path only: the scan runs once
   per decoded value, and a repeat's element count is the sender's choice
   through its byte budget, so building a list to succeed hands the sender a
   heap multiplier per element. *)
let rec enum_case_member cases (v : int) =
  match cases with
  | [] -> false
  | (_, x) :: rest -> x = v || enum_case_member rest v

let check_enum_decode ~at ~cases v =
  if not (enum_case_member cases v) then
    raise_invalid_enum ~at ~value:v ~valid:(enum_values cases)

let check_enum_encode ~name ~valid v =
  if not (enum_member valid v) then Errors.enum_encode ~name ~valid v

(* [all_zeros] decodes only zero bytes, so a padding value carrying anything
   else round-trips to [Non_zero_padding]. *)
let check_all_zeros_encode s =
  String.iteri
    (fun i c ->
      if c <> '\000' then
        Fmt.invalid_arg
          "Wire.encode: all_zeros expected a zero byte at offset %d, got 0x%02x"
          i (Char.code c))
    s

let check_where_encode cond ok =
  if not ok then
    Fmt.invalid_arg "Wire.encode: value violates its where constraint %a"
      Pp_3d.pp_expr cond

(* A [zeroterm] string ends at its first NUL, so a value carrying one decodes
   back truncated. Every encoder funnels through here, so the caller sees one
   message whatever path produced the bytes. *)
let check_zeroterm_encode s =
  if String.contains s '\000' then
    invalid_arg "Wire.encode: zeroterm string contains a NUL byte"

(* [zeroterm_at_most] writes the string, its NUL and zero padding into a fixed
   region, so the value has to leave room for the terminator. *)
let check_zeroterm_region ~region ~len =
  if len + 1 > region then
    Fmt.invalid_arg
      "Wire.encode: zeroterm string needs %d bytes but region is %d" (len + 1)
      region

(* A [bits ~width] slice owns exactly [bits] bits of a wider word and is carried
   in an OCaml [int], which holds far more than the slice does. Masking a wider
   value is the one truncation nothing downstream can catch, because the masked
   result is itself a legal field value that decode, [validate] and the
   EverParse validator all accept, and the number the caller meant is gone
   without trace. So each width accepts exactly what its decoder produces,
   [0, 2^bits - 1], and encode stays inverse to decode.

   Where the native [int] is no wider than the slice (a 32-bit slice under
   js_of_ocaml, anything from 31 bits up under wasm_of_ocaml) no [int] is out of
   range, so the guard narrows to what is still checkable there rather than
   rejecting a value the target can represent.

   No fixed-width scalar reaches here any more. {!UInt8.t}, {!UInt16.t},
   {!SInt8.t} and {!SInt16.t} carry the field's range in the type, so a value
   that reaches an encoder is in range by construction; {!UInt32.check_encode},
   {!SInt32.check_encode} and {!UInt63.check_encode} sit next to the
   representation that decides what fits; and [uint64] and [int64] need no guard
   at all, because an [int64] is exactly the eight bytes written. *)
let check_unsigned_encode ~bits v =
  let out_of_range = if bits >= Sys.int_size then v < 0 else v lsr bits <> 0 in
  if out_of_range then Errors.unsigned_too_wide ~bits v

(* The signed 32-bit writers, under the names the encode paths use. Every
   fixed-width scalar writes through its own carrier, whose range is already the
   field's. *)

let set_int32_le = SInt32.set_le
let set_int32_be = SInt32.set_be
