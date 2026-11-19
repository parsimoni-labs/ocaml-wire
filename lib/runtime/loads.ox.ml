module Int64_u = Stdlib_upstream_compatible.Int64_u
module Int32_u = Stdlib_upstream_compatible.Int32_u

type cell = { mutable cell : int64_u }

external checked16 : bytes -> int64_u -> int @@ portable
  = "%caml_bytes_get16_indexed_by_int64#"

external unchecked16 : bytes -> int64_u -> int @@ portable
  = "%caml_bytes_get16u_indexed_by_int64#"

external checked32 : bytes -> int64_u -> int32 @@ portable
  = "%caml_bytes_get32_indexed_by_int64#"

external unchecked32 : bytes -> int64_u -> int32 @@ portable
  = "%caml_bytes_get32u_indexed_by_int64#"

external checked64 : bytes -> int64_u -> int64 @@ portable
  = "%caml_bytes_get64_indexed_by_int64#"

external unchecked64 : bytes -> int64_u -> int64 @@ portable
  = "%caml_bytes_get64u_indexed_by_int64#"

external swap16 : int -> int @@ portable = "%bswap16"
external swap32 : int32 -> int32 @@ portable = "%bswap_int32"
external swap64 : int64 -> int64 @@ portable = "%bswap_int64"

(* No indexed primitive loads one byte: the paranoid profile checks the full
   offset before narrowing it to an [int]. *)
let[@inline always] u8 b off =
  if
    Paranoid.enabled
    && (Int64_u.compare off (Int64_u.of_int 0) < 0
       || Int64_u.compare off (Int64_u.of_int (Bytes.length b)) >= 0)
  then invalid_arg "index out of bounds";
  Char.code (Bytes.unsafe_get b (Int64_u.to_int off))

let[@inline always] u16 b o =
  if Paranoid.enabled then checked16 b o else unchecked16 b o

let[@inline always] i32 b o =
  if Paranoid.enabled then checked32 b o else unchecked32 b o

let[@inline always] i64 b o =
  if Paranoid.enabled then checked64 b o else unchecked64 b o

let[@inline always] u16be b o =
  if Sys.big_endian then u16 b o else swap16 (u16 b o)

let[@inline always] u16le b o =
  if Sys.big_endian then swap16 (u16 b o) else u16 b o

let[@inline always] i32be b o =
  Int32_u.of_int32 (if Sys.big_endian then i32 b o else swap32 (i32 b o))

let[@inline always] i32le b o =
  Int32_u.of_int32 (if Sys.big_endian then swap32 (i32 b o) else i32 b o)

let[@inline always] i64be b o =
  Int64_u.of_int64 (if Sys.big_endian then i64 b o else swap64 (i64 b o))

module Local = struct
  external get8 : bytes @ local -> int -> char @@ portable = "%bytes_unsafe_get"
  external set8 : bytes @ local -> int -> char -> unit @@ portable = "%bytes_unsafe_set"
  external safe_get8 : bytes @ local -> int -> char @@ portable = "%bytes_safe_get"
  external safe_set8 : bytes @ local -> int -> char -> unit @@ portable = "%bytes_safe_set"
  external get16 : bytes @ local -> int -> int @@ portable = "%caml_bytes_get16u"
  external get32 : bytes @ local -> int -> int32 @@ portable = "%caml_bytes_get32u"
  external get64 : bytes @ local -> int -> int64 @@ portable = "%caml_bytes_get64u"
  external safe_get16 : bytes @ local -> int -> int @@ portable = "%caml_bytes_get16"
  external safe_get32 : bytes @ local -> int -> int32 @@ portable = "%caml_bytes_get32"
  external safe_get64 : bytes @ local -> int -> int64 @@ portable = "%caml_bytes_get64"
  external set16 : bytes @ local -> int -> int -> unit @@ portable = "%caml_bytes_set16u"
  external set32 : bytes @ local -> int -> int32 -> unit @@ portable = "%caml_bytes_set32u"
  external set64 : bytes @ local -> int -> int64 -> unit @@ portable = "%caml_bytes_set64u"

  external safe_set16 : bytes @ local -> int -> int -> unit @@ portable
    = "%caml_bytes_set16"

  external safe_set32 : bytes @ local -> int -> int32 -> unit @@ portable
    = "%caml_bytes_set32"

  external safe_set64 : bytes @ local -> int -> int64 -> unit @@ portable
    = "%caml_bytes_set64"

  let[@inline always] g16 b o =
    if Paranoid.enabled then safe_get16 b o else get16 b o

  let[@inline always] g32 b o =
    if Paranoid.enabled then safe_get32 b o else get32 b o

  let[@inline always] g64 b o =
    if Paranoid.enabled then safe_get64 b o else get64 b o

  let[@inline always] s16 b o v =
    if Paranoid.enabled then safe_set16 b o v else set16 b o v

  let[@inline always] s32 b o v =
    if Paranoid.enabled then safe_set32 b o v else set32 b o v

  let[@inline always] s64 b o v =
    if Paranoid.enabled then safe_set64 b o v else set64 b o v

  let[@inline always] be16 v = if Sys.big_endian then v else swap16 v
  let[@inline always] le16 v = if Sys.big_endian then swap16 v else v
  let[@inline always] be32 v = if Sys.big_endian then v else swap32 v
  let[@inline always] le32 v = if Sys.big_endian then swap32 v else v
  let[@inline always] be64 v = if Sys.big_endian then v else swap64 v
  let[@inline always] le64 v = if Sys.big_endian then swap64 v else v

  let[@inline always] u8 b o =
    Char.code (if Paranoid.enabled then safe_get8 b o else get8 b o)

  let[@inline always] u16be b o = be16 (g16 b o)
  let[@inline always] u16le b o = le16 (g16 b o)
  let[@inline always] i32be b o = Int32_u.of_int32 (be32 (g32 b o))
  let[@inline always] i32le b o = Int32_u.of_int32 (le32 (g32 b o))
  let[@inline always] i64be b o = Int64_u.of_int64 (be64 (g64 b o))
  let[@inline always] i64le b o = Int64_u.of_int64 (le64 (g64 b o))

  let[@inline always] set_u8 b o v =
    let c = Char.unsafe_chr (v land 0xff) in
    if Paranoid.enabled then safe_set8 b o c else set8 b o c

  let[@inline always] set_u16be b o v = s16 b o (be16 v)
  let[@inline always] set_u16le b o v = s16 b o (le16 v)
  let[@inline always] set_i32be b o v = s32 b o (be32 (Int32_u.to_int32 v))
  let[@inline always] set_i32le b o v = s32 b o (le32 (Int32_u.to_int32 v))
  (* OxCaml fences every 64-bit store to memory on arm64 and no narrower one,
     so a 64-bit value goes out as two 32-bit stores. *)
  let[@inline always] set_i64be b o v =
    let v = Int64_u.to_int64 v in
    if Sys.big_endian then s64 b o v
    else begin
      s32 b o (be32 (Int64.to_int32 (Int64.shift_right_logical v 32)));
      s32 b (o + 4) (be32 (Int64.to_int32 v))
    end

  let[@inline always] set_i64le b o v =
    let v = Int64_u.to_int64 v in
    if Sys.big_endian then s64 b o (swap64 v)
    else begin
      s32 b o (Int64.to_int32 v);
      s32 b (o + 4) (Int64.to_int32 (Int64.shift_right_logical v 32))
    end

  (* The checked reads test the span against the length themselves, as one
     comparison the compiler shares with the caller's own use of the length,
     and raise what [Bytes]' do. *)
  let[@inline never] out_of_bounds () = invalid_arg "index out of bounds"

  let[@inline always] check b o n =
    if o < 0 || o > Bytes.length b - n then out_of_bounds ()

  let[@inline always] get_uint8 b o =
    check b o 1;
    Char.code (get8 b o)

  let[@inline always] get_uint16_be b o =
    check b o 2;
    be16 (get16 b o)

  let[@inline always] get_uint16_le b o =
    check b o 2;
    le16 (get16 b o)

  let[@inline always] get_int32_be b o =
    check b o 4;
    Int32_u.of_int32 (be32 (get32 b o))

  let[@inline always] get_int32_le b o =
    check b o 4;
    Int32_u.of_int32 (le32 (get32 b o))

  let sub_string b o n =
    if o < 0 || n < 0 || o > Bytes.length b - n then
      invalid_arg "String.sub / Bytes.sub";
    let r = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.unsafe_set r i (get8 b (o + i))
    done;
    Bytes.unsafe_to_string r

  let[@inline] unsafe_blit_string s so b o n =
    Bytes.unsafe_blit_string s so b o n

  let[@inline] blit_string s b o =
    let n = String.length s in
    if Paranoid.enabled then Bytes.blit_string s 0 b o n
    else Bytes.unsafe_blit_string s 0 b o n

  module Bigstring = struct
    type t =
      (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

    external length : t @ local -> int @@ portable = "%caml_ba_dim_1"
    external get : t @ local -> int -> char @@ portable = "%caml_ba_unsafe_ref_1"
    external set : t @ local -> int -> char -> unit @@ portable = "%caml_ba_unsafe_set_1"
    external get16 : t @ local -> int -> int @@ portable = "%caml_bigstring_get16u"
    external get32 : t @ local -> int -> int32 @@ portable = "%caml_bigstring_get32u"
    external get64 : t @ local -> int -> int64 @@ portable = "%caml_bigstring_get64u"
    external set16 : t @ local -> int -> int -> unit @@ portable = "%caml_bigstring_set16u"

    external set32 : t @ local -> int -> int32 -> unit @@ portable
      = "%caml_bigstring_set32u"

    external set64 : t @ local -> int -> int64 -> unit @@ portable
      = "%caml_bigstring_set64u"


    let[@inline never] out_of_bounds () = invalid_arg "index out of bounds"

    let[@inline always] check b o n =
      if Paranoid.enabled && (o < 0 || o > length b - n) then out_of_bounds ()

    let[@inline always] u8 b o =
      check b o 1;
      Char.code (get b o)

    let[@inline always] u16be b o =
      check b o 2;
      be16 (get16 b o)

    let[@inline always] u16le b o =
      check b o 2;
      le16 (get16 b o)

    let[@inline always] i32be b o =
      check b o 4;
      Int32_u.of_int32 (be32 (get32 b o))

    let[@inline always] i32le b o =
      check b o 4;
      Int32_u.of_int32 (le32 (get32 b o))

    let[@inline always] i64be b o =
      check b o 8;
      Int64_u.of_int64 (be64 (get64 b o))

    let[@inline always] i64le b o =
      check b o 8;
      Int64_u.of_int64 (le64 (get64 b o))

    let[@inline always] set_u8 b o v =
      check b o 1;
      set b o (Char.unsafe_chr (v land 0xff))

    let[@inline always] set_u16be b o v =
      check b o 2;
      set16 b o (be16 v)

    let[@inline always] set_u16le b o v =
      check b o 2;
      set16 b o (le16 v)

    let[@inline always] set_i32be b o v =
      check b o 4;
      set32 b o (be32 (Int32_u.to_int32 v))

    let[@inline always] set_i32le b o v =
      check b o 4;
      set32 b o (le32 (Int32_u.to_int32 v))

    let[@inline always] set_i64be b o v =
      let v = Int64_u.to_int64 v in
      check b o 8;
      if Sys.big_endian then set64 b o v
      else begin
        set32 b o (be32 (Int64.to_int32 (Int64.shift_right_logical v 32)));
        set32 b (o + 4) (be32 (Int64.to_int32 v))
      end

    let[@inline always] set_i64le b o v =
      let v = Int64_u.to_int64 v in
      check b o 8;
      if Sys.big_endian then set64 b o (swap64 v)
      else begin
        set32 b o (Int64.to_int32 v);
        set32 b (o + 4) (Int64.to_int32 (Int64.shift_right_logical v 32))
      end

    let[@inline always] bounds b o n =
      if o < 0 || o > length b - n then out_of_bounds ()

    let[@inline always] get_uint8 b o =
      bounds b o 1;
      Char.code (get b o)

    let[@inline always] get_uint16_be b o =
      bounds b o 2;
      be16 (get16 b o)

    let[@inline always] get_uint16_le b o =
      bounds b o 2;
      le16 (get16 b o)

    let[@inline always] get_int32_be b o =
      bounds b o 4;
      Int32_u.of_int32 (be32 (get32 b o))

    let[@inline always] get_int32_le b o =
      bounds b o 4;
      Int32_u.of_int32 (le32 (get32 b o))

    let sub_string b o n =
      if o < 0 || n < 0 || o > length b - n then
        invalid_arg "String.sub / Bytes.sub";
      let r = Bytes.create n in
      for i = 0 to n - 1 do
        Bytes.unsafe_set r i (get b (o + i))
      done;
      Bytes.unsafe_to_string r

    let unsafe_blit_string s so b o n =
      for i = 0 to n - 1 do
        set b (o + i) (String.unsafe_get s (so + i))
      done
  end
end
