(* Helpers over the Rust source Wire_rust writes. *)

let contains s sub =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s
    && (String.equal (String.sub s i n) sub || go (i + 1))
  in
  go 0

let has what src sub =
  Alcotest.(check bool) (Fmt.str "%s: %S" what sub) true (contains src sub)
