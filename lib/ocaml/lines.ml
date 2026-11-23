(* {1 Lines}

   The generated code as text, one [let x = e in] binding per line: what
   reads and binds a variable, and the branches merlint counts. *)

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

(* The branches the code [s] opens, as merlint counts them: each [if], each
   [&&] and [||], each handler of an exception. *)
let decisions s =
  let n = String.length s in
  let word i k =
    i + String.length k <= n
    && String.sub s i (String.length k) = k
    && (i = 0 || not (is_ident_char s.[i - 1]))
    && (i + String.length k = n || not (is_ident_char s.[i + String.length k]))
  in
  let rec go i acc =
    if i >= n then acc
    else if word i "if" || word i "exception" then go (i + 2) (acc + 1)
    else if
      i + 1 < n
      && ((s.[i] = '&' && s.[i + 1] = '&') || (s.[i] = '|' && s.[i + 1] = '|'))
    then go (i + 2) (acc + 1)
    else go (i + 1) acc
  in
  go 0 0

let is_variable x =
  x <> ""
  && (match x.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all is_ident_char x

(* The variable a line [let x = e in] binds. *)
let binding line =
  let is_let =
    String.starts_with ~prefix:"let " line
    && String.ends_with ~suffix:" in" line
  in
  match if is_let then String.index_from_opt line 4 ' ' else None with
  | Some i when String.length line >= i + 3 ->
      let x = String.sub line 4 (i - 4) in
      if is_variable x && String.sub line i 3 = " = " then Some x else None
  | _ -> None

let local_prefix = "let[@local] "

(* The occurrences of [x] in [line] as a word: none qualified by a module,
   none as the name of a label. *)
let occurrences x line =
  let n = String.length line and k = String.length x in
  let rec from i acc =
    if i + k > n then List.rev acc
    else
      let word =
        String.sub line i k = x
        && (i = 0 || not (is_ident_char line.[i - 1] || line.[i - 1] = '.'))
        && (i + k = n || not (is_ident_char line.[i + k]))
      in
      let label =
        word && i > 0 && line.[i - 1] = '~' && i + k < n && line.[i + k] = ':'
      in
      from (i + 1) (if word && not label then i :: acc else acc)
  in
  from 0 []

(* Whether [line] binds [x]: as a [let], or as a parameter of a local
   function. *)
let binds x line =
  binding line = Some x
  || (String.starts_with ~prefix:local_prefix line && occurrences x line <> [])

(* Whether [line] reads [x], other than by binding it. *)
let reads x line =
  if String.starts_with ~prefix:local_prefix line then false
  else
    List.exists
      (fun i -> not (i = 4 && binding line = Some x))
      (occurrences x line)

(* {1 Dead bindings}

   A walk binds an offset, an end or a field as it reaches it, before it
   knows whether a later check, a call or the consumer reads it. Each binding
   is one line [let x = e in] whose [e] only loads or computes, so a line
   whose [x] no later line of its function reads is dropped, until none
   is. *)

(* A top-level function the scanner writes, inlined or not. *)
let is_function line =
  String.starts_with ~prefix:"let[@inline always]" line
  || String.starts_with ~prefix:"let[@inline never]" line
  || String.starts_with ~prefix:"unfold let " line

let rec prune lines =
  let lines = Array.of_list lines in
  let n = Array.length lines in
  let rec used x j =
    j < n
    && (not (is_function lines.(j)))
    && (reads x lines.(j) || used x (j + 1))
  in
  let dead i =
    match binding lines.(i) with
    | Some x -> not (used x (i + 1))
    | None -> false
  in
  let kept = List.filteri (fun i _ -> not (dead i)) (Array.to_list lines) in
  if List.length kept = n then kept else prune kept

(* Whether the code of [lines] reads [x] before it binds it. *)
let rec free x = function
  | [] -> false
  | line :: rest -> reads x line || ((not (binds x line)) && free x rest)
