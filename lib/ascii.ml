(* RFC-style ASCII bit layout diagrams.

   Renders wire structs as 32-bit-wide diagrams per RFC 791 conventions.
   Each row is 32 bits. Fields are placed left-to-right, wrapping at row
   boundaries. Variable-length and conditional fields are shown as full-width
   rows with dependent-size annotations. *)

let row_bits = 32
let bit_chars = 2

(* A field segment for layout purposes. *)
type segment =
  | Fixed of { name : string; bits : int }
  | Variable of { label : string }

(* Render an expression as a short annotation. *)
let rec pp_expr : type a. Buffer.t -> a Types.expr -> unit =
 fun buf -> function
  | Int n -> Buffer.add_string buf (string_of_int n)
  | Int64 n -> Buffer.add_string buf (Int64.to_string n)
  | Bool b -> Buffer.add_string buf (string_of_bool b)
  | Ref name -> Buffer.add_string buf name
  | Add (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " + ";
      pp_expr buf b
  | Sub (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " - ";
      pp_expr buf b
  | Mul (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " * ";
      pp_expr buf b
  | Eq (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " == ";
      pp_expr buf b
  | Le (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " <= ";
      pp_expr buf b
  | Lt (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " < ";
      pp_expr buf b
  | Ge (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " >= ";
      pp_expr buf b
  | Gt (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " > ";
      pp_expr buf b
  | Ne (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " != ";
      pp_expr buf b
  | And (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " && ";
      pp_expr buf b
  | Or (a, b) ->
      pp_expr buf a;
      Buffer.add_string buf " || ";
      pp_expr buf b
  | Not a ->
      Buffer.add_string buf "!";
      pp_expr buf a
  | _ -> Buffer.add_string buf "?"

let expr_to_string (type a) (e : a Types.expr) =
  let buf = Buffer.create 32 in
  pp_expr buf e;
  Buffer.contents buf

(* Extract segment info from a Types.field. *)
let field_segment (Types.Field { field_name; field_typ; constraint_; _ }) =
  let name = Option.value field_name ~default:"" in
  match field_typ with
  | Bits { width; _ } -> Fixed { name; bits = width }
  | _ -> (
      match Types.field_wire_size field_typ with
      | Some n -> Fixed { name; bits = n * 8 }
      | None ->
          let annotation =
            match field_typ with
            | Byte_array { size } | Byte_slice { size } ->
                Fmt.str "%s (%s bytes)" name (expr_to_string size)
            | Array { len; elem } ->
                let elem_info =
                  match Types.field_wire_size elem with
                  | Some n -> Fmt.str "%d-byte elems" n
                  | None -> "var elems"
                in
                Fmt.str "%s (%s x %s)" name (expr_to_string len) elem_info
            | Where { inner; cond } ->
                let inner_label =
                  match Types.field_wire_size inner with
                  | Some n -> Fmt.str "%s (%d)" name (n * 8)
                  | None -> name
                in
                Fmt.str "%s [%s]" inner_label (expr_to_string cond)
            | _ ->
                if name = "" then "(variable)" else Fmt.str "%s (variable)" name
          in
          let label =
            match constraint_ with
            | None -> annotation
            | Some cond -> Fmt.str "%s [%s]" annotation (expr_to_string cond)
          in
          Variable { label })

(* Centre a label inside [width] characters. *)
let centre label width =
  let len = String.length label in
  if len >= width then String.sub label 0 width
  else
    let pad = width - len in
    let left = pad / 2 in
    let right = pad - left in
    String.make left ' ' ^ label ^ String.make right ' '

(* Build the bit-position ruler lines. *)
let ruler () =
  let tens = Buffer.create 80 in
  let ones = Buffer.create 80 in
  Buffer.add_string tens "  ";
  Buffer.add_string ones "  ";
  for bit = 0 to row_bits - 1 do
    let decade = bit / 10 in
    let unit_ = bit mod 10 in
    if bit > 0 then (
      Buffer.add_char tens ' ';
      Buffer.add_char ones ' ');
    Buffer.add_string tens (string_of_int decade);
    Buffer.add_string ones (string_of_int unit_)
  done;
  (Buffer.contents tens, Buffer.contents ones)

(* Horizontal separator spanning the full row width. *)
let full_sep () =
  let buf = Buffer.create ((row_bits * bit_chars) + 2) in
  Buffer.add_string buf " +";
  for _ = 1 to row_bits do
    Buffer.add_char buf '-';
    Buffer.add_char buf '+'
  done;
  Buffer.contents buf

(* Render a row of fixed-width fields as "|...|...|". *)
let render_fixed_row fields =
  let buf = Buffer.create ((row_bits * bit_chars) + 2) in
  Buffer.add_string buf " |";
  List.iter
    (fun (name, bits) ->
      let content_width = (bits * bit_chars) - 1 in
      Buffer.add_string buf (centre name content_width);
      Buffer.add_char buf '|')
    fields;
  Buffer.contents buf

(* Render a variable-length field as a full-width labelled row. *)
let render_variable_row label =
  let total_width = (row_bits * bit_chars) - 1 in
  let content =
    if String.length label >= total_width then String.sub label 0 total_width
    else
      let padded = " " ^ label in
      let pad = total_width - String.length padded in
      if pad > 0 then padded ^ String.make pad ' ' else padded
  in
  Fmt.str " |%s|" content

(* Layout: split fixed segments into 32-bit rows. Variable segments
   always occupy their own full-width row. *)
type row = Fixed_row of (string * int) list | Variable_row of string

let layout segments =
  let rows = ref [] in
  let cur_row = ref [] in
  let cur_bits = ref 0 in
  let flush () =
    if !cur_row <> [] then (
      rows := Fixed_row (List.rev !cur_row) :: !rows;
      cur_row := [];
      cur_bits := 0)
  in
  List.iter
    (function
      | Fixed { name; bits } ->
          let remaining = ref bits in
          while !remaining > 0 do
            let avail = row_bits - !cur_bits in
            let take = min avail !remaining in
            let label =
              if take = bits then name
              else if !remaining = bits then name
              else ""
            in
            cur_row := (label, take) :: !cur_row;
            cur_bits := !cur_bits + take;
            remaining := !remaining - take;
            if !cur_bits = row_bits then flush ()
          done
      | Variable { label } ->
          flush ();
          rows := Variable_row label :: !rows)
    segments;
  flush ();
  List.rev !rows

let render_struct (s : Types.struct_) =
  let segments = List.map field_segment s.fields in
  let rows = layout segments in
  if rows = [] then ""
  else
    let buf = Buffer.create 512 in
    let tens, ones = ruler () in
    Buffer.add_string buf tens;
    Buffer.add_char buf '\n';
    Buffer.add_string buf ones;
    Buffer.add_char buf '\n';
    List.iter
      (fun row ->
        Buffer.add_string buf (full_sep ());
        Buffer.add_char buf '\n';
        (match row with
        | Fixed_row fields -> Buffer.add_string buf (render_fixed_row fields)
        | Variable_row label ->
            Buffer.add_string buf (render_variable_row label));
        Buffer.add_char buf '\n')
      rows;
    Buffer.add_string buf (full_sep ());
    Buffer.add_char buf '\n';
    Buffer.contents buf

let of_struct = render_struct
let of_codec t = render_struct (Codec.to_struct t)
let pp_struct ppf s = Format.pp_print_string ppf (render_struct s)
let pp_codec ppf t = pp_struct ppf (Codec.to_struct t)
