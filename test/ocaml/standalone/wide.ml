(* The modules that read 32-bit fields into an [int]: 64-bit targets only. *)

open Expect

(* On OxCaml a generated function takes its buffer [local], which the types of
   [Expect], shared with OCaml, cannot state: each is passed eta-expanded. *)

let run () =
  round_trip "ipv4" ~size:S_ipv4.size
    ~decode:(fun b o -> S_ipv4.decode b o)
    ~encode:(fun t b o -> S_ipv4.encode t b o)
    ~big_decode:(fun b o -> S_ipv4.Bigstring.decode b o)
    ~big_encode:(fun t b o -> S_ipv4.Bigstring.encode t b o)
    (List.init 20 (fun i -> 255 - (i * 11)));
  round_trip "mixed" ~size:S_mixed.size
    ~decode:(fun b o -> S_mixed.decode b o)
    ~encode:(fun t b o -> S_mixed.encode t b o)
    ~big_decode:(fun b o -> S_mixed.Bigstring.decode b o)
    ~big_encode:(fun t b o -> S_mixed.Bigstring.encode t b o)
    (List.init S_mixed.size (fun i -> i * 29 land 0xff));
  round_trip "wide" ~size:S_wide.size
    ~decode:(fun b o -> S_wide.decode b o)
    ~encode:(fun t b o -> S_wide.encode t b o)
    ~big_decode:(fun b o -> S_wide.Bigstring.decode b o)
    ~big_encode:(fun t b o -> S_wide.Bigstring.encode t b o)
    (List.init S_wide.size (fun i -> 0xff - i))
