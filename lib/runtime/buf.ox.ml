type 'b t = bytes
type 'b length = int
type any = Any : 'b t -> any [@@unboxed]

let[@inline] v buf = Any buf
let[@inline] local (buf @ local) = exclave_ Any buf
let[@inline] length (b @ local) = Bytes.length b
let pp_length ppf n =
  Format.pp_print_char ppf '<';
  Format.pp_print_int ppf n;
  Format.pp_print_string ppf " bytes>"

let pp ppf b = pp_length ppf (Bytes.length b)
