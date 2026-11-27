open Wire.Private
module T = Types

(* Bitfields read through their word, by the variable holding it: a test of
   several bitfields of one word becomes one test of the word. *)
let rec masked words (e : int T.expr) =
  match e with
  | T.Ref (T.I, name) ->
      Option.map (fun mask -> (name, 0, mask)) (List.assoc_opt name words)
  | T.Land (a, T.Int n) | T.Land (T.Int n, a) ->
      Option.map
        (fun (word, shift, mask) -> (word, shift, mask land n))
        (masked words a)
  | T.Land (a, T.Lnot (T.Int n)) | T.Land (T.Lnot (T.Int n), a) ->
      Option.map
        (fun (word, shift, mask) -> (word, shift, mask land lnot n))
        (masked words a)
  | T.Lsr (a, T.Int n) when n >= 0 && n < Sys.int_size ->
      Option.bind (masked words a) (fun (word, shift, mask) ->
          let mask = mask lsr n in
          if mask = 0 then Some (word, 0, 0)
          else if shift + n < Sys.int_size then Some (word, shift + n, mask)
          else None)
  | T.Cast (`U8, a) -> masked words (T.Land (a, T.Int 255))
  | T.Cast (`U16, a) -> masked words (T.Land (a, T.Int 65535))
  | _ -> None

let zero_test words (e : bool T.expr) =
  let extract zero a =
    Option.map
      (fun (word, shift, mask) ->
        (word, (if mask = 0 then 0 else mask lsl shift), zero))
      (masked words a)
  in
  match e with
  | T.Eq (a, T.Int 0) -> extract true a
  | T.Eq (T.Int 0, a) -> extract true a
  | T.Ne (a, T.Int 0) -> extract false a
  | T.Ne (T.Int 0, a) -> extract false a
  | _ -> None

let mask_test word mask zero : bool T.expr =
  let bits = T.Land (T.Ref (T.I, word), T.Int mask) in
  if mask = 0 then T.Bool zero
  else if zero then T.Eq (bits, T.Int 0)
  else T.Ne (bits, T.Int 0)

let rec bit_predicate words (e : bool T.expr) : bool T.expr =
  let each = bit_predicate words in
  let combine want make a b =
    let a = each a and b = each b in
    match (zero_test words a, zero_test words b) with
    | Some (x, m, p), Some (y, n, q) when x = y && p = want && q = want ->
        mask_test x (m lor n) want
    | _ -> make a b
  in
  match e with
  | T.And (a, b) -> combine true (fun a b -> T.And (a, b)) a b
  | T.Or (a, b) -> combine false (fun a b -> T.Or (a, b)) a b
  | T.Not a -> (
      let a = each a in
      match zero_test words a with
      | Some (word, mask, zero) -> mask_test word mask (not zero)
      | None -> T.Not a)
  | _ -> (
      match zero_test words e with
      | Some (word, mask, zero) -> mask_test word mask zero
      | None -> e)
