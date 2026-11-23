(* The fields of a checked view as data, and their reads. *)

open Codec_repr
module Slice = Bytesrw.Bytes.Slice

(* A field is data: an unsigned field of a word of the frame reads as
   [Word], the 8-byte word at [q] shifted right by [s] and masked by [m],
   inline. Every other field is one direct call, so a read site holds one
   test and one load: in a frame of 4 to 7 bytes an unsigned field reads as
   [Word32], the same from the 4-byte word at [q], and a byte slice of
   constant size in the frame ([Bytes]) is built only by the reads of that
   field. A second inline case costs every [Word] read an instruction. *)
type ('a, 'c) field =
  | Word : {
      q : int;
      s : int;
      m : int;
      eq : ('a, int) Type.eq;
    }
      -> ('a, 'c) field
  | Word32 : {
      q : int;
      s : int;
      m : int;
      eq : ('a, int) Type.eq;
    }
      -> ('a, 'c) field
  | Bytes : { off : int; length : int } -> (Slice.t, 'c) field
  | Scalar : 'a scalar -> ('a, 'c) field
  | Get : (bytes -> int -> 'a) -> ('a, 'c) field

let scalar_read : type a. a scalar -> bytes -> int -> a =
 fun s buf off ->
  match s with
  | Byte o -> UInt8.unsafe_get buf (off + o)
  | U16le o -> UInt16.unsafe_le buf (off + o)
  | U16be o -> UInt16.unsafe_be buf (off + o)
  | U32le o -> UInt32.unsafe_le buf (off + o)
  | U32be o -> UInt32.unsafe_be buf (off + o)
  | Sbyte o -> SInt8.unsafe_get buf (off + o)
  | I16le o -> SInt16.unsafe_le buf (off + o)
  | I16be o -> SInt16.unsafe_be buf (off + o)
  | I32le o -> SInt32.unsafe_le buf (off + o)
  | I32be o -> SInt32.unsafe_be buf (off + o)
  | Bits8 { off = o; shift; mask } ->
      (Unchecked.u8 buf (off + o) lsr shift) land mask
  | Bits16le { off = o; shift; mask } ->
      (Unchecked.u16le buf (off + o) lsr shift) land mask
  | Bits16be { off = o; shift; mask } ->
      (Unchecked.u16be buf (off + o) lsr shift) land mask

let[@inline] word_read buf p q s m =
  Int64.to_int (Int64.shift_right_logical (Codec_words.load buf p q) s) land m

(* [Int32.to_int] extends the sign, which the mask of a field of at most 32
   bits clears. *)
let[@inline] word32_read buf p q s m =
  (Int32.to_int (Unchecked.i32be buf (p + q)) lsr s) land m

let[@inline] bytes_read buf p off length =
  if Int.equal length 0 then Slice.eod
  else Slice.make buf ~first:(p + off) ~length

let[@inline never] read_other : type a c. (a, c) field -> bytes -> int -> a =
 fun f buf off ->
  match f with
  | Word { q; s; m; eq = Type.Equal } -> word_read buf off q s m
  | Word32 { q; s; m; eq = Type.Equal } -> word32_read buf off q s m
  | Bytes { off = o; length } -> bytes_read buf off o length
  | Scalar s -> scalar_read s buf off
  | Get get -> get buf off

let[@inline] read (type a) (f : (a, _) field) b r : a =
  let buf = (b : _ Buf.t :> bytes) in
  match f with
  | Word { q; s; m; eq = Type.Equal } -> word_read buf r q s m
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
