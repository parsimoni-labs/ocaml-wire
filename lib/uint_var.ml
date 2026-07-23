(* A 7-byte value needs 56 bits: a 64-bit host accumulates in the unboxed
   native int, a narrow-int platform through [Int64], and both land in
   [UInt63.t], which holds the value everywhere. *)
let int_holds_7_bytes = Sys.int_size > 56

let read_int_be buf off size =
  let v = ref 0 in
  for i = 0 to size - 1 do
    v := (!v lsl 8) lor Bytes.get_uint8 buf (off + i)
  done;
  !v

let read_int_le buf off size =
  let v = ref 0 in
  for i = size - 1 downto 0 do
    v := (!v lsl 8) lor Bytes.get_uint8 buf (off + i)
  done;
  !v

let read_int64_be buf off size =
  let v = ref 0L in
  for i = 0 to size - 1 do
    v :=
      Int64.logor (Int64.shift_left !v 8)
        (Int64.of_int (Bytes.get_uint8 buf (off + i)))
  done;
  !v

let read_int64_le buf off size =
  let v = ref 0L in
  for i = size - 1 downto 0 do
    v :=
      Int64.logor (Int64.shift_left !v 8)
        (Int64.of_int (Bytes.get_uint8 buf (off + i)))
  done;
  !v

let read endian buf off size =
  match (endian : Types.endian) with
  | Big ->
      if int_holds_7_bytes then Optint.Int63.of_int (read_int_be buf off size)
      else Optint.Int63.of_int64 (read_int64_be buf off size)
  | Little ->
      if int_holds_7_bytes then Optint.Int63.of_int (read_int_le buf off size)
      else Optint.Int63.of_int64 (read_int64_le buf off size)

let byte_at v i =
  Int64.to_int (Int64.logand (Int64.shift_right_logical v (8 * i)) 0xFFL)

let write endian buf off size v =
  let v = Optint.Int63.to_int64 v in
  match (endian : Types.endian) with
  | Big ->
      for i = 0 to size - 1 do
        Bytes.set_uint8 buf (off + i) (byte_at v (size - 1 - i))
      done
  | Little ->
      for i = 0 to size - 1 do
        Bytes.set_uint8 buf (off + i) (byte_at v i)
      done
