type predicate = Where | Field | Action | Per_byte

type error_kind =
  | Unexpected_eof of { expected : int; got : int }
  | Trailing_bytes of int
  | Invalid_enum of { value : int; valid : int list }
  | Invalid_tag of int
  | Missing_terminator
  | Non_zero_padding
  | Value_out_of_range of { value : int64 }
  | Zero_divisor
  | Constraint_failed of { which : predicate; value : int64 option }

type parse_error = { at : int; field : string list; kind : error_kind }

exception Parse_error of parse_error

let str = Format.pp_print_string

let comma ppf () =
  str ppf ",";
  Format.pp_print_space ppf ()

let dot ppf () = str ppf "."
let pp_ints = Format.pp_print_list ~pp_sep:comma Format.pp_print_int

let pp_predicate ppf (p : predicate) =
  match p with
  | Where -> str ppf "where clause"
  | Field -> str ppf "field constraint"
  | Action -> str ppf "field action"
  | Per_byte -> str ppf "per-byte refinement"

let pp_error_kind ppf = function
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

let pp_parse_error ppf { at; field; kind } =
  (match field with
  | [] -> ()
  | _ -> Format.pp_print_list ~pp_sep:dot str ppf field);
  str ppf "@";
  Format.pp_print_int ppf at;
  str ppf ": ";
  pp_error_kind ppf kind

let invalid parts = invalid_arg (String.concat "" parts)
let int = string_of_int

let to_string pp v =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* {1 Raising} *)

let fail ~at ~field kind = raise (Parse_error { at; field; kind })

let[@inline never] negative_offset off =
  invalid [ "Codec: negative offset "; int off ]

let[@inline never] eof ~at ~expected ~got =
  fail ~at ~field:[] (Unexpected_eof { expected; got })

let[@inline never] frame ~size ~len off =
  if off < 0 then negative_offset off;
  eof ~at:off ~expected:size ~got:(max 0 (len - off))

let[@inline never] field_constraint ~at ~field value =
  fail ~at ~field:[ field ]
    (Constraint_failed { which = Field; value = Some (Int64.of_int value) })

let[@inline never] invalid_enum ~at ~field ~value ~valid =
  fail ~at ~field (Invalid_enum { value; valid })

let[@inline never] invalid_tag ~at ~field tag =
  fail ~at ~field:[ field ] (Invalid_tag tag)

let[@inline never] where_clause ~at =
  fail ~at ~field:[] (Constraint_failed { which = Where; value = None })

(* {1 Encoding} *)

let[@inline never] encode_too_short ~name ~size ~len off =
  if off < 0 then
    invalid [ "Codec.encode "; name; ": negative offset "; int off ];
  invalid
    [
      "Codec.encode ";
      name;
      ": buffer too short (need ";
      int size;
      ", got ";
      int (len - off);
      ")";
    ]

let[@inline never] encode_rejected ~name e =
  invalid
    [
      "Codec.encode ";
      name;
      ": the encoded record is rejected by its own constraints (";
      to_string pp_parse_error e;
      ")";
    ]

let[@inline never] unsigned_too_wide ~bits v =
  invalid
    [
      "Wire.encode: value ";
      int v;
      " does not fit an unsigned ";
      int bits;
      "-bit field";
    ]

let[@inline never] byte_length ~expected ~actual =
  invalid
    [
      "Wire.encode: byte field expected ";
      int expected;
      " bytes, got ";
      int actual;
      " (a fixed-size byte field is exact; use zeroterm_at_most for a shorter \
       value)";
    ]

let[@inline never] enum_encode ~name ~valid v =
  invalid
    [
      "Wire.encode: enum ";
      name;
      " expected one of [";
      to_string pp_ints valid;
      "], got ";
      int v;
    ]
