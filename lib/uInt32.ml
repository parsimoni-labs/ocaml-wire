type t = int

let get_le buf off =
  Bytes.get_int32_le buf off |> Int32.to_int |> ( land ) 0xFFFF_FFFF

let get_be buf off =
  Bytes.get_int32_be buf off |> Int32.to_int |> ( land ) 0xFFFF_FFFF

let set_le buf off v = Bytes.set_int32_le buf off (Int32.of_int v)
let set_be buf off v = Bytes.set_int32_be buf off (Int32.of_int v)
let to_int t = t
let of_int t = t land 0xFFFF_FFFF
