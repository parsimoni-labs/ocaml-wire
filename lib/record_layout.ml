module T = Types

type store =
  | Int of { width : int; endian : T.endian; signed : bool }
  | Bits of { base : T.bitfield_base; shift : int; width : int }
  | Bytes of int

let leaf : type a. a T.typ -> store option = function
  | T.Uint8 -> Some (Int { width = 8; endian = T.Big; signed = false })
  | T.Int8 -> Some (Int { width = 8; endian = T.Big; signed = true })
  | T.Uint16 endian -> Some (Int { width = 16; endian; signed = false })
  | T.Int16 endian -> Some (Int { width = 16; endian; signed = true })
  | T.Uint32 endian -> Some (Int { width = 32; endian; signed = false })
  | T.Int32 endian -> Some (Int { width = 32; endian; signed = true })
  | T.Byte_array { size = T.Int n } when n >= 0 -> Some (Bytes n)
  | _ -> None

let width_bytes = function
  | Int { width; _ } -> width / 8
  | Bits { base; _ } -> Bitfield.byte_size base
  | Bytes n -> n

type word = {
  base : T.bitfield_base;
  order : T.bit_order;
  at : int;
  used : int;
}

let place_bits ~off ~word ~base ~order ~width =
  let total = Bitfield.total_bits base in
  let w =
    match word with
    | Some w
      when Bitfield.equal w.base base && w.order = order
           && w.used + width <= total ->
        w
    | _ -> { base; order; at = off; used = 0 }
  in
  let shift = Bitfield.shift ~bit_order:order ~total ~bits_used:w.used ~width in
  let next = if w.used = 0 then off + Bitfield.byte_size base else off in
  (w, shift, next)
