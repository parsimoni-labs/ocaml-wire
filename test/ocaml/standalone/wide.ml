(* The modules that read 32-bit fields into an [int]: 64-bit targets only. *)

open Expect

let run () =
  round_trip "ipv4" ~size:S_ipv4.size ~decode:S_ipv4.decode
    ~encode:S_ipv4.encode ~big_decode:S_ipv4.Bigstring.decode
    ~big_encode:S_ipv4.Bigstring.encode
    (List.init 20 (fun i -> 255 - (i * 11)));
  round_trip "mixed" ~size:S_mixed.size ~decode:S_mixed.decode
    ~encode:S_mixed.encode ~big_decode:S_mixed.Bigstring.decode
    ~big_encode:S_mixed.Bigstring.encode
    (List.init S_mixed.size (fun i -> i * 29 land 0xff));
  round_trip "wide" ~size:S_wide.size ~decode:S_wide.decode
    ~encode:S_wide.encode ~big_decode:S_wide.Bigstring.decode
    ~big_encode:S_wide.Bigstring.encode
    (List.init S_wide.size (fun i -> 0xff - i))
