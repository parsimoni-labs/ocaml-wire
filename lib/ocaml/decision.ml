module T = Wire.Private.Types

type pos = { var : string option; at : int }
type values = [ `Int | `Int64 | `Int64_u ]

type bound =
  | Next
  | Hoisted
  | Checked of { negative : bool; cancelled : (int T.expr * int T.expr) option }

type choice = Run of int option | If of bool T.expr * choice * choice
type join = Joined | Copied
type short_frames = Apart | Inline | Walked

type t =
  | Walk of { name : string; values : values; known : int }
  | Prefix_bound of { at : pos; window : string; bytes : int; tested : bool }
  | Span_bound of {
      size : int T.expr;
      at : pos;
      window : string;
      size_var : string;
      overflows : bool;
      bound : bound;
    }
  | Shared_words of {
      at : int;
      constant : bool;
      lo : int;
      hi : int;
      window : string;
      fields : (int * int) list;
      held : int list;
      loads : int list;
    }
  | Word_range of {
      at : pos;
      window : string;
      first : string;
      second : string;
      lo : int;
      hi : int;
    }
  | Dispatch of {
      at : pos;
      window : string;
      cases : int;
      conditions : bool T.expr list;
      choice : choice;
      join : join;
      hoisted : int T.expr option;
    }
  | Cut of { name : string; window : string; known : int }
  | Frame_loop of {
      name : string;
      need : int;
      frames : int;
      short : short_frames option;
      ahead : (string * int * int) list;
    }

let pp_pos ppf p =
  match p.var with
  | None -> Fmt.int ppf p.at
  | Some v -> if p.at = 0 then Fmt.string ppf v else Fmt.pf ppf "%s+%d" v p.at

let pp_values ppf (v : values) =
  Fmt.string ppf
    (match v with `Int -> "int" | `Int64 -> "int64" | `Int64_u -> "int64_u")

let pp_bound ppf = function
  | Next -> Fmt.string ppf "left to the next check"
  | Hoisted -> Fmt.string ppf "hoisted"
  | Checked { negative; cancelled } ->
      Fmt.pf ppf "checked%s%s"
        (if negative then ", negative" else "")
        (match cancelled with
        | Some (l, r) -> Fmt.str ", as %a > %a" T.pp_expr l T.pp_expr r
        | None -> "")

let rec pp_choice ppf = function
  | Run None -> Fmt.string ppf "none"
  | Run (Some i) -> Fmt.pf ppf "case %d" i
  | If (c, yes, no) ->
      Fmt.pf ppf "@[<hv 1>(if %a@ then %a@ else %a)@]" T.pp_expr c pp_choice yes
        pp_choice no

let pp_short ppf = function
  | Apart -> Fmt.string ppf "short frames apart"
  | Inline -> Fmt.string ppf "short frames inline"
  | Walked -> Fmt.string ppf "short frames walked"

let pp_ints = Fmt.(brackets (list ~sep:semi int))
let plural n = if n = 1 then "" else "s"

let pp ppf = function
  | Walk { name; values; known } ->
      Fmt.pf ppf "walk %s: %a values, %d bytes known" name pp_values values
        known
  | Prefix_bound { at; window; bytes; tested } ->
      Fmt.pf ppf "prefix at %a in %s: %d bytes, %s" pp_pos at window bytes
        (if tested then "tested" else "known")
  | Span_bound { size; at; window; size_var; overflows; bound } ->
      Fmt.pf ppf "span at %a in %s: %a bytes as %s%s, %a" pp_pos at window
        T.pp_expr size size_var
        (if overflows then " (checked arithmetic)" else "")
        pp_bound bound
  | Shared_words { at; constant; lo; hi; window; fields = _; held; loads } ->
      Fmt.pf ppf "words at %d%s in %s, within [%d, %d]: load %a, hold %a" at
        (if constant then "" else " from a variable")
        window lo hi pp_ints loads pp_ints held
  | Word_range { at; window; first; second; lo; hi } ->
      Fmt.pf ppf "word range at %a in %s: %s and %s in [%d, %d]" pp_pos at
        window first second lo hi
  | Dispatch { at; window; cases; conditions = _; choice; join; hoisted } ->
      Fmt.pf ppf "dispatch at %a in %s: %d case%s, %s%a: %a" pp_pos at window
        cases (plural cases)
        (match join with Joined -> "joined" | Copied -> "copied")
        Fmt.(option (any ", hoisted " ++ T.pp_expr))
        hoisted pp_choice choice
  | Cut { name; window; known } ->
      Fmt.pf ppf "cut %s in %s: %d bytes known" name window known
  | Frame_loop { name; need; frames; short; ahead } ->
      Fmt.pf ppf "frame loop %s: %d frame%s an iteration, %d bytes known%a%a"
        name frames (plural frames) need
        Fmt.(option (any ", " ++ pp_short))
        short
        Fmt.(
          if ahead = [] then nop
          else
            any ", reads ahead "
            ++ list ~sep:(any ", ") (fun ppf (v, k, n) ->
                pf ppf "%s (%d bytes at %d)" v n k))
        ahead
