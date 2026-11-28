(* The reads of the fields of a checked view, as data. *)

open Codec_repr
module Slice = Bytesrw.Bytes.Slice

(* A read is data: an unsigned field of a word of the frame reads as [Word],
   the 8-byte word at [q] shifted right by [s] and masked by [m], inline. Every
   other read is one direct call, so a read site holds one test and one load:
   in a frame of 4 to 7 bytes an unsigned field reads as [Word32], the same
   from the 4-byte word at [q], a byte slice of constant size in the frame
   ([Bytes]) is built only by the reads of that field, and a scalar the frame
   holds ([Scalar]) is one load. Each tests that its bytes lie in the buffer
   and answers its zero where they do not: a field of another codec of the
   same record type reaches past the frame its view was checked in. A second
   inline case costs every [Word] read an instruction. *)
type 'a read =
  | Word : { q : int; s : int; m : int; eq : ('a, int) Type.eq } -> 'a read
  | Word32 : { q : int; s : int; m : int; eq : ('a, int) Type.eq } -> 'a read
  | Bytes : { off : int; length : int } -> Slice.t read
  | Scalar : { s : 'a scalar; stop : int; zero : 'a } -> 'a read
  | Get : (bytes -> int -> 'a) -> 'a read

(* The loads of a word whose bounds the read has just tested, as primitives:
   no call boxes the word, whatever the build inlines across modules. *)
external get64u : bytes -> int -> int64 = "%caml_bytes_get64u"
external get32u : bytes -> int -> int32 = "%caml_bytes_get32u"
external swap64 : int64 -> int64 = "%bswap_int64"
external swap32 : int32 -> int32 = "%bswap_int32"

(* A 32-bit field as the [int] its carrier is where an [int] holds it. *)
let[@inline] i32 ~le buf off =
  let w = get32u buf off in
  Int32.to_int (if Sys.big_endian = le then swap32 w else w)

let[@inline] u32 ~le buf off = i32 ~le buf off land UInt32.mask32
let[@inline] cast : type a. (a, int) Type.eq -> int -> a = fun Type.Equal x -> x

let scalar_read : type a. a scalar -> bytes -> int -> a =
 fun s buf off ->
  match s with
  | Byte o -> UInt8.unsafe_get buf (off + o)
  | U16le o -> UInt16.unsafe_le buf (off + o)
  | U16be o -> UInt16.unsafe_be buf (off + o)
  | U32le o -> (
      match UInt32.int_eq with
      | Some eq -> cast eq (u32 ~le:true buf (off + o))
      | None -> UInt32.unsafe_le buf (off + o))
  | U32be o -> (
      match UInt32.int_eq with
      | Some eq -> cast eq (u32 ~le:false buf (off + o))
      | None -> UInt32.unsafe_be buf (off + o))
  | Sbyte o -> SInt8.unsafe_get buf (off + o)
  | I16le o -> SInt16.unsafe_le buf (off + o)
  | I16be o -> SInt16.unsafe_be buf (off + o)
  | I32le o -> (
      match SInt32.int_eq with
      | Some eq -> cast eq (i32 ~le:true buf (off + o))
      | None -> SInt32.unsafe_le buf (off + o))
  | I32be o -> (
      match SInt32.int_eq with
      | Some eq -> cast eq (i32 ~le:false buf (off + o))
      | None -> SInt32.unsafe_be buf (off + o))
  | Bits8 { off = o; shift; mask } ->
      (Unchecked.u8 buf (off + o) lsr shift) land mask
  | Bits16le { off = o; shift; mask } ->
      (Unchecked.u16le buf (off + o) lsr shift) land mask
  | Bits16be { off = o; shift; mask } ->
      (Unchecked.u16be buf (off + o) lsr shift) land mask

(* The bytes past the record's offset a scalar's read takes, and its zero. *)
let scalar_stop : type a. a scalar -> int = function
  | Byte o | Sbyte o -> o + 1
  | U16le o | U16be o | I16le o | I16be o -> o + 2
  | U32le o | U32be o | I32le o | I32be o -> o + 4
  | Bits8 { off; _ } -> off + 1
  | Bits16le { off; _ } | Bits16be { off; _ } -> off + 2

let scalar_zero : type a. a scalar -> a = function
  | Byte _ -> UInt8.zero
  | U16le _ -> UInt16.zero
  | U16be _ -> UInt16.zero
  | U32le _ -> UInt32.zero
  | U32be _ -> UInt32.zero
  | Sbyte _ -> SInt8.zero
  | I16le _ -> SInt16.zero
  | I16be _ -> SInt16.zero
  | I32le _ -> SInt32.zero
  | I32be _ -> SInt32.zero
  | Bits8 _ -> 0
  | Bits16le _ -> 0
  | Bits16be _ -> 0

let scalar s = Scalar { s; stop = scalar_stop s; zero = scalar_zero s }

let[@inline] word_read buf p q s m =
  let w = get64u buf (p + q) in
  let w = if Sys.big_endian then w else swap64 w in
  Int64.to_int (Int64.shift_right_logical w s) land m

(* [Int32.to_int] extends the sign, which the mask of a field of at most 32
   bits clears. *)
let[@inline] word32_read buf p q s m =
  let w = get32u buf (p + q) in
  let w = if Sys.big_endian then w else swap32 w in
  (Int32.to_int w lsr s) land m

let[@inline] bytes_read buf p off length =
  if Int.equal length 0 then Slice.eod
  else Slice.make buf ~first:(p + off) ~length

let[@inline never] read_other : type a. a read -> bytes -> int -> a =
 fun f buf r ->
  let len = Bytes.length buf in
  match f with
  | Word { q; s; m; eq = Type.Equal } ->
      if r + q > len - 8 then 0 else word_read buf r q s m
  | Word32 { q; s; m; eq = Type.Equal } ->
      if r + q > len - 4 then 0 else word32_read buf r q s m
  | Bytes { off; length } ->
      if r + off > len - length then Slice.eod else bytes_read buf r off length
  | Scalar { s; stop; zero } ->
      if r > len - stop then zero else scalar_read s buf r
  | Get get -> get buf r

let[@inline] read (type a) (f : a read) buf r : a =
  match f with
  | Word { q; s; m; eq = Type.Equal } ->
      if r + q > Bytes.length buf - 8 then 0 else word_read buf r q s m
  | f -> read_other f buf r

type 'a bits = { o : int; n : int; s : int; w : int; eq : ('a, int) Type.eq }

let bits_of_scalar : type a. a scalar -> a bits option = function
  | Byte o -> Some { o; n = 1; s = 0; w = 8; eq = UInt8.int_eq }
  | U16be o -> Some { o; n = 2; s = 0; w = 16; eq = UInt16.int_eq }
  | U32be o ->
      Option.map (fun eq -> { o; n = 4; s = 0; w = 32; eq }) UInt32.int_eq
  | Bits8 { off; shift; mask } ->
      Some
        {
          o = off;
          n = 1;
          s = shift;
          w = Codec_words.bit_width mask;
          eq = Type.Equal;
        }
  | Bits16be { off; shift; mask } ->
      Some
        {
          o = off;
          n = 2;
          s = shift;
          w = Codec_words.bit_width mask;
          eq = Type.Equal;
        }
  | Sbyte _ | I16be _ | I32be _ | U16le _ | U32le _ | I16le _ | I32le _
  | Bits16le _ ->
      None

let word_field ~size { o; n; s; w; eq } =
  if Sys.int_size < 63 || size < 4 then None
  else if size >= 8 then
    let { Codec_words.q; l; r; m; _ } =
      Codec_words.word ~size o ~bytes:n ~s ~w ~signed:false
    in
    Some (Word { q; s = r - l; m; eq })
  else
    let q = min o (size - 4) in
    Some (Word32 { q; s = (8 * (q + 4 - o - n)) + s; m = (1 lsl w) - 1; eq })
