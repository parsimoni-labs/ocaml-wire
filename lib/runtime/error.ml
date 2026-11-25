type predicate = Where | Field | Action | Per_byte

type kind =
  | Unexpected_eof of { expected : int; got : int }
  | Trailing_bytes of int
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | Missing_terminator
  | Non_zero_padding
  | Value_out_of_range of { value : int64 }
  | Zero_divisor
  | Constraint_failed of { which : predicate; value : int64 option }

type t = { at : int; field : string list; kind : kind }

let str = Format.pp_print_string

let comma ppf () =
  str ppf ",";
  Format.pp_print_space ppf ()

let dot ppf () = str ppf "."
let pp_ints ppf l = Format.pp_print_list ~pp_sep:comma Format.pp_print_int ppf l

let pp_predicate ppf (p : predicate) =
  match p with
  | Where -> str ppf "where clause"
  | Field -> str ppf "field constraint"
  | Action -> str ppf "field action"
  | Per_byte -> str ppf "per-byte refinement"

let pp_kind ppf = function
  | Unexpected_eof { expected; got } ->
      str ppf "unexpected EOF: expected ";
      Format.pp_print_int ppf expected;
      str ppf " bytes, got ";
      Format.pp_print_int ppf got
  | Trailing_bytes n ->
      Format.pp_print_int ppf n;
      str ppf (if n = 1 then " trailing byte" else " trailing bytes")
  | Invalid_enum { value; valid } ->
      str ppf "invalid enum value ";
      Format.pp_print_int ppf value;
      str ppf ", valid: [";
      pp_ints ppf valid;
      str ppf "]"
  | Invalid_tag tag ->
      str ppf "invalid tag: ";
      Format.pp_print_int ppf tag
  | Missing_terminator -> str ppf "missing NUL terminator"
  | Non_zero_padding -> str ppf "non-zero padding byte"
  | Value_out_of_range { value } ->
      str ppf "value out of range: ";
      str ppf (Int64.to_string value)
  | Zero_divisor -> str ppf "zero divisor in expression"
  | Constraint_failed { which; value } -> (
      pp_predicate ppf which;
      str ppf " violated";
      match value with
      | Some v ->
          str ppf " (value ";
          str ppf (Int64.to_string v);
          str ppf ")"
      | None -> ())

let pp ppf { at; field; kind } =
  (match field with
  | [] -> ()
  | _ -> Format.pp_print_list ~pp_sep:dot str ppf field);
  str ppf "@";
  Format.pp_print_int ppf at;
  str ppf ": ";
  pp_kind ppf kind

let predicate_rank (p : predicate) =
  match p with Where -> 0 | Field -> 1 | Action -> 2 | Per_byte -> 3

let compare_predicate a b = Int.compare (predicate_rank a) (predicate_rank b)

let kind_rank = function
  | Unexpected_eof _ -> 0
  | Trailing_bytes _ -> 1
  | Invalid_enum _ -> 2
  | Invalid_tag _ -> 3
  | Missing_terminator -> 4
  | Non_zero_padding -> 5
  | Value_out_of_range _ -> 6
  | Zero_divisor -> 7
  | Constraint_failed _ -> 8

let compare_kind a b =
  match (a, b) with
  | Unexpected_eof a, Unexpected_eof b ->
      let c = Int.compare a.expected b.expected in
      if c <> 0 then c else Int.compare a.got b.got
  | Trailing_bytes a, Trailing_bytes b -> Int.compare a b
  | Invalid_enum a, Invalid_enum b ->
      let c = Int.compare a.value b.value in
      if c <> 0 then c else List.compare Int.compare a.valid b.valid
  | Invalid_tag a, Invalid_tag b -> Int.compare a b
  | Value_out_of_range a, Value_out_of_range b -> Int64.compare a.value b.value
  | Constraint_failed a, Constraint_failed b ->
      let c = compare_predicate a.which b.which in
      if c <> 0 then c else Option.compare Int64.compare a.value b.value
  | Missing_terminator, Missing_terminator
  | Non_zero_padding, Non_zero_padding
  | Zero_divisor, Zero_divisor ->
      0
  | _ -> Int.compare (kind_rank a) (kind_rank b)

let compare a b =
  let c = compare_kind a.kind b.kind in
  if c <> 0 then c
  else
    let c = Int.compare a.at b.at in
    if c <> 0 then c else List.compare String.compare a.field b.field

let equal a b = compare a b = 0
