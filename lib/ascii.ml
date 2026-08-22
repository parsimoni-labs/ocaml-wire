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

(* Render an expression as a short annotation.

   Deliberately not shared with {!Types.pp_expr}, which prints the same GADT as
   3D source. That printer parenthesises every operator and raises
   [Invalid_argument] on the constructs 3D cannot express ([Field_pos], negative
   literals); neither behaviour is wanted here. An annotation is dropped when it
   outgrows its 32-bit-wide row, so the parentheses would cost the reader the
   whole annotation, and a diagram is documentation, so no input may raise. The
   match is exhaustive on purpose, so a new expression constructor is a compile
   error rather than a silent placeholder in the output, and every branch below
   is visibly total. *)
let rec pp_expr : type a. a Types.expr Fmt.t =
 fun ppf expr ->
  let infix : type b c. b Types.expr -> string -> c Types.expr -> unit =
   fun a op b -> Fmt.pf ppf "%a%s%a" pp_expr a op pp_expr b
  in
  match expr with
  | Int n -> Fmt.int ppf n
  | Int64 n -> Fmt.int64 ppf n
  | Bool b -> Fmt.bool ppf b
  | Ref (_, name) -> Fmt.string ppf name
  | Param_ref p -> Fmt.string ppf p.name
  (* A [sizeof] over a fixed-size description is a constant, and the byte count
     is what the diagram wants to show. Deferring to {!Types.pp_typ} would reach
     {!Types.pp_expr} again, which can raise. *)
  | Sizeof t -> (
      match Types.field_wire_size t with
      | Some n -> Fmt.int ppf n
      | None -> Fmt.string ppf "sizeof")
  | Sizeof_this -> Fmt.string ppf "sizeof(this)"
  | Field_pos -> Fmt.string ppf "field_pos"
  | Add (a, b) -> infix a " + " b
  | Sub (a, b) -> infix a " - " b
  | Mul (a, b) -> infix a " * " b
  | Div (a, b) -> infix a " / " b
  | Mod (a, b) -> infix a " % " b
  | Land (a, b) -> infix a " & " b
  | Land64 (a, b) -> infix a " & " b
  | Lor (a, b) -> infix a " | " b
  | Lxor (a, b) -> infix a " ^ " b
  | Lnot a -> Fmt.pf ppf "~%a" pp_expr a
  | Lsl (a, b) -> infix a " << " b
  | Lsr (a, b) -> infix a " >> " b
  | Lsr64 (a, b) -> infix a " >> " b
  | Eq (a, b) -> infix a " == " b
  | Ne (a, b) -> infix a " != " b
  | Lt (a, b) -> infix a " < " b
  | Le (a, b) -> infix a " <= " b
  | Gt (a, b) -> infix a " > " b
  | Ge (a, b) -> infix a " >= " b
  | And (a, b) -> infix a " && " b
  | Or (a, b) -> infix a " || " b
  | Not a -> Fmt.pf ppf "!%a" pp_expr a
  | Cast (t, e) ->
      let width =
        match t with
        | `U8 -> "u8"
        | `U16 -> "u16"
        | `U32 -> "u32"
        | `U64 -> "u64"
      in
      Fmt.pf ppf "(%s) %a" width pp_expr e
  (* Spelled out rather than as a C ternary: "?" is the one character a reader
     must be able to trust is never a rendering failure. *)
  | If_then_else (c, t, e) ->
      Fmt.pf ppf "if %a then %a else %a" pp_expr c pp_expr t pp_expr e

(* Annotate a fixed-field label with constraint and enum info. *)
let annotate_fixed name typ constraint_ bits =
  let base =
    if bits <= 8 then name
    else
      (* For wider fields, add enum/variant info if available *)
      let rec enum_info : type a. a Types.typ -> string option = function
        | Enum { cases; _ } ->
            let entries = List.map (fun (s, v) -> Fmt.str "%s=%d" s v) cases in
            Some (String.concat "," entries)
        | Map { inner; _ } -> enum_info inner
        | Where { inner; _ } -> enum_info inner
        | _ -> None
      in
      match enum_info typ with
      | Some info
        when String.length info + String.length name + 3
             <= (bits * bit_chars) - 1 ->
          Fmt.str "%s {%s}" name info
      | _ -> name
  in
  match constraint_ with
  | Some cond ->
      let ann = Fmt.str "%s [%a]" base pp_expr cond in
      if String.length ann <= (bits * bit_chars) - 1 then ann else base
  | None -> base

(* Extract segment info from a Types.field. *)
let variable_annotation : type a. string -> a Types.typ -> string =
 fun name typ ->
  match typ with
  | Types.Byte_array { size } -> Fmt.str "%s (%a bytes)" name pp_expr size
  | Types.Byte_slice { size } -> Fmt.str "%s (%a bytes)" name pp_expr size
  | Byte_array_where { size; _ } ->
      Fmt.str "%s (%a bytes, refined)" name pp_expr size
  | Array { len; elem; _ } ->
      let elem_info =
        match Types.field_wire_size elem with
        | Some n -> Fmt.str "%d-byte elems" n
        | None -> "var elems"
      in
      Fmt.str "%s (%a x %s)" name pp_expr len elem_info
  | Where { inner; cond } ->
      let inner_label =
        match Types.field_wire_size inner with
        | Some n -> Fmt.str "%s (%d)" name (n * 8)
        | None -> name
      in
      Fmt.str "%s [%a]" inner_label pp_expr cond
  | _ -> if name = "" then "(variable)" else Fmt.str "%s (variable)" name

let field_segment (Types.Field { field_name; field_typ; constraint_; _ }) =
  let name = Option.value field_name ~default:"" in
  match field_typ with
  | Bits { width; _ } ->
      Fixed
        { name = annotate_fixed name field_typ constraint_ width; bits = width }
  | _ -> (
      match Types.field_wire_size field_typ with
      | Some n ->
          Fixed
            {
              name = annotate_fixed name field_typ constraint_ (n * 8);
              bits = n * 8;
            }
      | None ->
          let annotation = variable_annotation name field_typ in
          let label =
            match constraint_ with
            | None -> annotation
            | Some cond -> Fmt.str "%s [%a]" annotation pp_expr cond
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

(* Build the bit-position ruler lines (RFC 791 style).
   Line 1: byte offsets at every 8th bit
   Line 2: bit positions 0-9 repeating *)
let ruler () =
  let bytes_line = Buffer.create 80 in
  let bits_line = Buffer.create 80 in
  Buffer.add_string bytes_line "  ";
  Buffer.add_string bits_line "  ";
  for bit = 0 to row_bits - 1 do
    if bit > 0 then (
      Buffer.add_char bytes_line ' ';
      Buffer.add_char bits_line ' ');
    if bit mod 8 = 0 then Buffer.add_string bytes_line (string_of_int (bit / 8))
    else Buffer.add_char bytes_line ' ';
    Buffer.add_string bits_line (string_of_int (bit mod 10))
  done;
  (Buffer.contents bytes_line, Buffer.contents bits_line)

(* Horizontal separator spanning [n] bits. *)
let sep n =
  let buf = Buffer.create ((n * bit_chars) + 2) in
  Buffer.add_string buf " +";
  for _ = 1 to n do
    Buffer.add_char buf '-';
    Buffer.add_char buf '+'
  done;
  Buffer.contents buf

let _full_sep () = sep row_bits

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

let row_bits_used = function
  | Fixed_row fields -> List.fold_left (fun acc (_, b) -> acc + b) 0 fields
  | Variable_row _ -> row_bits

let render_row buf last_bits row =
  let used = row_bits_used row in
  Buffer.add_string buf (sep (max !last_bits used));
  Buffer.add_char buf '\n';
  (match row with
  | Fixed_row fields -> Buffer.add_string buf (render_fixed_row fields)
  | Variable_row label -> Buffer.add_string buf (render_variable_row label));
  Buffer.add_char buf '\n';
  last_bits := used

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
    let last_bits = ref row_bits in
    List.iter (render_row buf last_bits) rows;
    Buffer.add_string buf (sep !last_bits);
    Buffer.add_char buf '\n';
    Buffer.contents buf

let of_struct = render_struct
let of_codec t = render_struct (Codec.to_struct t)
let pp_struct ppf s = Fmt.string ppf (render_struct s)
let pp_codec ppf t = pp_struct ppf (Codec.to_struct t)
