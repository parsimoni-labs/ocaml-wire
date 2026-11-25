exception Parse_error of Error.t

let pp_ints ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () ->
      Format.pp_print_string ppf ",";
      Format.pp_print_space ppf ())
    Format.pp_print_int ppf l

let invalid parts = invalid_arg (String.concat "" parts)
let int = string_of_int

let to_string pp v =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  pp ppf v;
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* {1 Raising} *)

let fail ~at ~field kind = raise (Parse_error { Error.at; field; kind })

let[@inline never] negative_offset off =
  invalid [ "Codec: negative offset "; int off ]

let[@inline never] eof ~at ~expected ~got =
  fail ~at ~field:[] (Error.Unexpected_eof { expected; got })

let[@inline never] frame ~size ~len off =
  if off < 0 then negative_offset off;
  eof ~at:off ~expected:size ~got:(max 0 (len - off))

let[@inline never] field_constraint ~at ~field value =
  fail ~at ~field:[ field ]
    (Error.Constraint_failed
       { which = Field; value = Some (Int64.of_int value) })

let[@inline never] invalid_enum ~at ~field ~value ~valid =
  fail ~at ~field (Error.Invalid_enum { value; valid })

let[@inline never] invalid_tag ~at ~field tag =
  fail ~at ~field:[ field ] (Error.Invalid_tag tag)

let[@inline never] where_clause ~at =
  fail ~at ~field:[] (Error.Constraint_failed { which = Where; value = None })

let[@inline never] relocate ~at ~field e =
  raise (Parse_error { e with Error.at; field })

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
      to_string Error.pp e;
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

let check_unsigned ~bits v =
  let out_of_range = if bits >= Sys.int_size then v < 0 else v lsr bits <> 0 in
  if out_of_range then unsigned_too_wide ~bits v

let check_length ~expected s =
  let actual = String.length s in
  if actual <> expected then byte_length ~expected ~actual

let check_enum ~name ~valid v =
  if not (List.exists (Int.equal v) valid) then enum_encode ~name ~valid v
