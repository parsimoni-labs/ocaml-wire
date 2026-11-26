type t = int

let pp = Format.pp_print_int
let invalid n = n
let valid n ~ipv4_tcp_present = if ipv4_tcp_present then n + 1 else n
