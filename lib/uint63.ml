(* Unsigned 63-bit integer. Reads 8 bytes, masks to 63 bits (OCaml int). *)

type t = int

let get_le buf off =
  Bytes.get_int64_le buf off |> Int64.to_int |> ( land ) max_int

let get_be buf off =
  Bytes.get_int64_be buf off |> Int64.to_int |> ( land ) max_int

let set_le buf off v = Bytes.set_int64_le buf off (Int64.of_int v)
let set_be buf off v = Bytes.set_int64_be buf off (Int64.of_int v)
let to_int t = t
let of_int t = t
