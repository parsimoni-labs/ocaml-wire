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

let rs ?folds ?visitors c = Wire_rust.(to_rs ?visitors (v ?folds c))

(* The function [name] of [src]: from its signature to the end of its body,
   the first line that closes a block at the start. *)
let section src name =
  let start =
    let n = String.length name in
    let rec go i =
      if i + n > String.length src then
        Alcotest.failf "no function %s in the module" name
      else if String.equal (String.sub src i n) name then i
      else go (i + 1)
    in
    go 0
  in
  let stop =
    let rec go i =
      if i + 3 > String.length src then String.length src
      else if String.equal (String.sub src i 3) "\n}\n" then i + 3
      else go (i + 1)
    in
    go start
  in
  String.sub src start (stop - start)
