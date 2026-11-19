open Wire.Private
open Layout
module T = Types
open Planner
open Emitter

(* {1 The record module} *)

(* What the cold walk raises, as the codec does: the frame, then each field's
   checks in field order, each in the order the codec runs them, then the
   where clause. *)
let raise_check (f : field) c =
  let at = Fmt.str "(off + %d)" f.offset in
  match c.kind with
  | Constraint ->
      Fmt.str "%s.field_constraint ~at:%s ~field:%S %s" errors at f.name f.id
  | Enum valid ->
      Fmt.str "%s.invalid_enum ~at:%s ~field:[ %S ] ~value:%s ~valid:[%a]"
        errors at f.name f.id
        Fmt.(list ~sep:(any "; ") (using int_lit string))
        valid
  | Tag -> Fmt.str "%s.invalid_tag ~at:%s ~field:%S %s" errors at f.name f.id

let hot ?(ox = false) ?(raw32 = false) ~src ~live ~fail () =
  {
    num = Tagged;
    src;
    ox;
    cold = false;
    raw32;
    live;
    exported = Sset.empty;
    slots = Sset.empty;
    fail = (fun _ -> fail);
    overflowed = (fun _ _ -> fail);
  }

let cold ~src ~live =
  {
    num = Tagged;
    src;
    ox = false;
    cold = true;
    raw32 = false;
    live;
    exported = Sset.empty;
    slots = Sset.empty;
    fail =
      (function
      | Bounds -> "no_error ()"
      | Check (f, c) -> raise_check f c
      | Where -> errors ^ ".where_clause ~at:off");
    overflowed =
      (fun failure e ->
        match failure with
        | Check (f, _) ->
            Fmt.str "%s.relocate ~at:(off + %d) ~field:[ %S ] %s" errors
              f.offset f.name e
        | Where | Bounds -> Fmt.str "%s.relocate ~at:off ~field:[] %s" errors e);
  }

(* A fixed record's walk at [off], whose frame is checked before it. *)
let pp_fixed ppf ~m ~(plan : plan) ~size k =
  start ~m ~ppf ~ranges:plan.ranges ~base:"off" ~limit:"len" ~known:size
    plan.nodes (fun _ _ -> k ())

let pp_frame ~src ppf on_short =
  Fmt.pf ppf "off < 0 || off > %s buf - size then %s buf off" src.len on_short

let all_fields (plan : plan) = fields_of plan.nodes

let has_checks (plan : plan) =
  List.exists (fun f -> f.checks <> []) (all_fields plan)
  || List.exists (function Test _ -> true | _ -> false) plan.nodes

(* The [int] a test sees of a 32-bit field, read as an [int32]. *)
let int_view f =
  match f.store with
  | Int { width = 32; signed; _ } ->
      Fmt.str "(Int32.to_int %s_raw%s)" f.id
        (if signed then "" else " land mask32")
  | _ -> f.id

let map_var f = "map_" ^ f.id

(* A bitfield or a byte array with no map holds its value in a carrier of its
   own, [Value.<field>], whose constructor checks the width or the length once:
   a write then needs no check of its own. *)
let has_carrier f =
  match (f.map, f.store) with
  | None, (Bits _ | Bytes _) -> true
  | Some _, _ | None, Int _ -> false

(* The value handed to the constructor: through the field's map when it has
   one, the failure placed at the field. *)
let typed_value f =
  match (f.map, f.store) with
  | Some _, Int { width = 32; _ } ->
      Fmt.str "(%s.dec ~at:(off + %d) %s_raw)" (map_var f) f.offset f.id
  | Some _, _ -> Fmt.str "(%s.dec ~at:(off + %d) %s)" (map_var f) f.offset f.id
  | None, _ -> (
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.str "(cast %s.%sInt%d.int_eq %s)" rt
            (if signed then "S" else "U")
            w f.id
      | Int { signed; _ } ->
          Fmt.str "(%s.%s.of_int32 %s_raw)" rt
            (if signed then "SInt32" else "UInt32")
            f.id
      | Bits _ | Bytes _ -> Fmt.str "(cast Value.%s_eq %s)" f.label f.id)

(* The value handed to a constructor the caller wrote, which cannot name the
   module's carriers: a bitfield as its [int], a byte array as its [string]. *)
let plain_typed_value f =
  match (f.map, f.store) with
  | None, (Bits _ | Bytes _) -> f.id
  | _ -> typed_value f

(* The attributes the OxCaml target adds: an allocation check on a hot path,
   and on a cold one the promise that it only raises. *)
let zero_alloc = function `OxCaml -> "[@zero_alloc]" | `OCaml -> ""

let cold_attr = function
  | `OxCaml -> "[@inline never][@zero_alloc assume error]"
  | `OCaml -> "[@inline never]"

(* A decoder: the frame, the walk binding every field, then [call] on their
   [value]s, by default their typed values. *)
let pp_decode ?(src = bytes_src) ?(ox = false) ?(value = typed_value) ppf ~plan
    ~size ~head ~call =
  let fields = all_fields plan in
  Fmt.pf ppf "%s =@\n  if %a@\n  else@\n" head (pp_frame ~src) "fail";
  (* A 32-bit field is handed out as its [int32], bound whether live or not;
     its [int] is live only where a check reads it. *)
  let outputs =
    List.filter_map
      (fun f ->
        match f.store with Int { width = 32; _ } -> None | _ -> Some f.id)
      fields
  in
  let m =
    hot ~ox ~raw32:true ~src ~live:(live plan ~outputs) ~fail:"fail buf off" ()
  in
  pp_fixed ppf ~m ~plan ~size (fun () ->
      Fmt.pf ppf "%s@\n" (call (List.map value fields)));
  Fmt.pf ppf "@\n"

(* [is_valid] answers with a boolean, loading only the fields a test reads, so
   a caller that drops a bad record on the floor pays for no exception handler;
   [validate] raises what the codec raises, found off the hot path by [fail]. *)
let pp_validate ~src ppf ~target ~plan ~size =
  let attr = "[@inline]" ^ zero_alloc target in
  Fmt.pf ppf
    "let%s is_valid %s off =@\n\
    \  if off < 0 || off > %s buf - size then false@\n\
    \  else@\n"
    attr src.buf src.len;
  let m =
    hot ~ox:(target = `OxCaml) ~src ~live:(live plan ~outputs:[]) ~fail:"false"
      ()
  in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "true@\n");
  Fmt.pf ppf
    "@\n\
     let%s validate %s off =@\n\
    \  if not (is_valid buf off) then fail buf off@\n\
     @\n"
    attr src.buf

let pp_cold ~src ppf ~target ~plan ~size =
  Fmt.pf ppf
    "let%s fail %s off =@\n\
    \  if off < 0 || off > %s buf - size then@\n\
    \    %s.frame ~size ~len:(%s buf) off;@\n"
    (cold_attr target) src.buf src.len errors src.len;
  let m = cold ~src ~live:(live ~cold:true plan ~outputs:[]) in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "no_error ()@\n");
  Fmt.pf ppf
    "@\n\
     let%s too_short %s off =@\n\
    \  %s.encode_too_short ~name:codec_name ~size ~len:(%s buf) off@\n\
     @\n"
    (cold_attr target) src.buf errors src.len

let type_of f =
  let t = Fmt.str "%s.%s.t" rt in
  match f.store with
  | Int { width = 8; signed = false; _ } -> t "UInt8"
  | Int { width = 8; signed = true; _ } -> t "SInt8"
  | Int { width = 16; signed = false; _ } -> t "UInt16"
  | Int { width = 16; signed = true; _ } -> t "SInt16"
  | Int { signed = false; _ } -> t "UInt32"
  | Int { signed = true; _ } -> t "SInt32"
  | (Bits _ | Bytes _) when has_carrier f -> "Value." ^ f.label
  | Bits _ -> "int"
  | Bytes _ -> "string"

(* [Value.<field> x] is [x] in the field's carrier, after the check a write
   would make: its width for a bitfield, its length for a byte array. A
   carrier's [<field>_eq] proves it is that [int] or [string], for the
   module's own reads, whose values are in range by construction. *)
let pp_values ppf fields =
  match List.filter has_carrier fields with
  | [] -> ()
  | fields ->
      let plain f =
        match f.store with Bytes _ -> "string" | Bits _ | Int _ -> "int"
      in
      Fmt.pf ppf "module Value : sig@\n";
      List.iter
        (fun f ->
          Fmt.pf ppf
            "  type %s = private %s@\n\
            \  val %s : %s -> %s@\n\
            \  val %s_eq : (%s, %s) Type.eq@\n"
            f.label (plain f) f.label (plain f) f.label f.label f.label
            (plain f))
        fields;
      Fmt.pf ppf "end = struct@\n";
      List.iter
        (fun f ->
          Fmt.pf ppf "  type %s = %s@\n@\n  let[@inline] %s x =@\n" f.label
            (plain f) f.label;
          (match f.store with
          | Bits { width; _ } ->
              Fmt.pf ppf
                "    if x lsr %d <> 0 then %s.unsigned_too_wide ~bits:%d x \
                 else x@\n"
                width errors width
          | Bytes n ->
              Fmt.pf ppf
                "    if String.length x <> %d then@\n\
                \      %s.byte_length ~expected:%d ~actual:(String.length x)@\n\
                \    else x@\n"
                n errors n
          | Int _ -> Fmt.pf ppf "    x@\n");
          Fmt.pf ppf "@\n  let %s_eq = Type.Equal@\n@\n" f.label)
        fields;
      Fmt.pf ppf "end@\n@\n"

let pp_vars ppf fields = pp_list ppf (List.map (fun f -> f.id) fields)

(* Writes take each value in its plain form: an [int] for a field up to 16 bits
   or a bitfield, an [int32] for a 32-bit field, a [string] for a byte array. *)

(* The bits that put a plain value out of its field's range: the value, offset
   to zero for a signed field, shifted right by the field's width. Or-ing the
   values of one width before the shift makes one test, with no branch per
   field, that finds any. *)
let out_of_range f =
  let x = f.id in
  match f.store with
  | Int { width = (8 | 16) as w; signed = false; _ } -> Some (`Shift (w, x))
  | Int { width = (8 | 16) as w; signed = true; _ } ->
      Some (`Shift (w, Fmt.str "(%s + %d)" x (1 lsl (w - 1))))
  | Int _ -> None
  | Bits { width; _ } -> Some (`Shift (width, x))
  | Bytes n -> Some (`Other (Fmt.str "(String.length %s lxor %d)" x n))

let out_of_range_test bad =
  let widths =
    List.sort_uniq compare
      (List.filter_map
         (function `Shift (w, _) -> Some w | `Other _ -> None)
         bad)
  in
  List.map
    (fun w ->
      let xs =
        List.filter_map
          (function `Shift (w', x) when w' = w -> Some x | _ -> None)
          bad
      in
      match xs with
      | [ x ] -> Fmt.str "(%s lsr %d)" x w
      | _ -> Fmt.str "((%s) lsr %d)" (String.concat " lor " xs) w)
    widths
  @ List.filter_map (function `Other e -> Some e | `Shift _ -> None) bad

let member f valid =
  Fmt.str "(%a)"
    Fmt.(
      list ~sep:(any " || ") (fun ppf v -> pf ppf "%s = %s" f.id (int_lit v)))
    valid

(* [first_error] raises what the codec raises for these values: converting each
   to its field's type, in field order; then the frame; then each bitfield's
   width, byte array's length and enum case, in field order. *)
let pp_first_error ~src ppf ~target fields =
  (* A 32-bit value is in range as an [int32]: only an enum tests it. *)
  let param f =
    match (f.store, f.enum) with
    | Int { width = 32; _ }, None -> "_"
    | _ -> f.id
  in
  Fmt.pf ppf "let%s first_error %s off %a =@\n" (cold_attr target) src.buf
    pp_list (List.map param fields);
  List.iter
    (fun f ->
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.pf ppf "  ignore (%s.%sInt%d.v %s);@\n" rt
            (if signed then "S" else "U")
            w f.id
      | _ -> ())
    fields;
  Fmt.pf ppf "  if %a;@\n" (pp_frame ~src) "too_short";
  List.iter
    (fun f ->
      (match f.store with
      | Bits { width; _ } ->
          Fmt.pf ppf "  %s.check_unsigned ~bits:%d %s;@\n" errors width f.id
      | Bytes n ->
          Fmt.pf ppf "  %s.check_length ~expected:%d %s;@\n" errors n f.id
      | Int _ -> ());
      match f.enum with
      | Some (name, valid) ->
          Fmt.pf ppf "  %s.check_enum ~name:%S ~valid:[%a] %s;@\n" errors name
            Fmt.(list ~sep:(any "; ") (using int_lit string))
            valid f.id
      | None -> ())
    fields;
  Fmt.pf ppf "  no_error ()@\n@\n"

(* A store of [size] bytes at [at]: an [int] whose low [size] bytes it writes,
   or the [int32] of a 32-bit field. *)
type part = {
  at : int;
  size : int;
  endian : T.endian option;
  value : [ `Int of string | `I32 of string ];
}

(* A bitfield word's store: the values of its fields, each in range, shifted
   into place and or-ed. *)
let bit_word fields (o, base) =
  let bits =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { shift; _ } when f.offset = o ->
            Fmt.kstr Option.some "(%s lsl %d)" f.id shift
        | _ -> None)
      fields
  in
  let value = `Int (Fmt.str "(%s)" (String.concat " lor " bits)) in
  match base with
  | T.U8 -> { at = o; size = 1; endian = None; value }
  | T.U16 e -> { at = o; size = 2; endian = Some e; value }
  | T.U32 e -> { at = o; size = 4; endian = Some e; value }

(* An integer field's store. A signed value is masked to its width, so its sign
   does not reach the bytes beside it when stores merge. *)
let int_part f =
  let at = f.offset in
  let masked w signed =
    `Int
      (if signed then Fmt.str "(%s land 0x%x)" f.id ((1 lsl w) - 1) else f.id)
  in
  match f.store with
  | Int { width = 8; signed; _ } ->
      Some { at; size = 1; endian = None; value = masked 8 signed }
  | Int { width = 16; endian; signed } ->
      Some { at; size = 2; endian = Some endian; value = masked 16 signed }
  | Int { endian; _ } ->
      Some { at; size = 4; endian = Some endian; value = `I32 f.id }
  | Bits _ | Bytes _ -> None

(* The stores of a write, in offset order: each integer field and each
   bitfield word. *)
let parts fields =
  let words =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { base; _ } -> Some (f.offset, base)
        | _ -> None)
      fields
    |> List.sort_uniq compare
  in
  List.map (bit_word fields) words @ List.filter_map int_part fields
  |> List.sort (fun a b -> compare a.at b.at)

(* The parts from the head of [ps] that fill exactly [n] bytes from its first,
   all in one byte order, and the rest. *)
let take n ps =
  match ps with
  | [] -> None
  | p0 :: _ ->
      let base = p0.at in
      let rec go acc endian next ps =
        if next = base + n then Some (List.rev acc, endian, ps)
        else
          match ps with
          | p :: rest when p.at = next && p.at + p.size <= base + n -> (
              match (endian, p.endian) with
              | Some e, Some e' when e <> e' -> None
              | None, e | e, None -> go (p :: acc) e (next + p.size) rest
              | e, _ -> go (p :: acc) e (next + p.size) rest)
          | _ -> None
      in
      go [] None base ps

(* The position of a part's low bit in the [n]-byte word at [base]. *)
let shift endian ~n base p =
  let r = p.at - base in
  match endian with T.Big -> (n - r - p.size) * 8 | T.Little -> r * 8

(* A word as the or of its parts, each widened first: built in [int32] or
   [int64], it needs no [int] wider than one field. *)
let word ~m ~n endian base ps =
  let part p =
    let sh = shift endian ~n base p in
    let v =
      match p.value with
      | `Int v -> Fmt.str "(%s.of_int %s)" m v
      | `I32 x when n = 8 && sh + 32 < 64 ->
          Fmt.str "(Int64.logand (Int64.of_int32 %s) 0xffffffffL)" x
      | `I32 x -> Fmt.str "(%s.of_int32 %s)" m x
    in
    if sh = 0 then v else Fmt.str "(%s.shift_left %s %d)" m v sh
  in
  let rec ors = function
    | [] -> assert false
    | [ x ] -> x
    | x :: xs -> Fmt.str "(%s.logor %s %s)" m x (ors xs)
  in
  ors (List.map part ps)

(* Adjacent stores merge into one of 8 bytes, or of 4, so a record takes a few
   wide stores instead of one per field. *)
let stores ~src ppf fields =
  let e = function Some e -> endian_suffix e | None -> "be" in
  let one p =
    match (p.size, p.value) with
    | 1, `Int v -> Fmt.pf ppf "    %s.set_u8 buf (off + %d) %s;@\n" src.u p.at v
    | 2, `Int v ->
        Fmt.pf ppf "    %s.set_u16%s buf (off + %d) %s;@\n" src.u (e p.endian)
          p.at v
    | _, `Int v ->
        Fmt.pf ppf "    %s.set_i32%s buf (off + %d) (Int32.of_int %s);@\n" src.u
          (e p.endian) p.at v
    | _, `I32 v ->
        Fmt.pf ppf "    %s.set_i32%s buf (off + %d) %s;@\n" src.u (e p.endian)
          p.at v
  in
  let merged ~m ~n p ws endian =
    let endian = Option.value endian ~default:T.Big in
    Fmt.pf ppf "    %s.set_i%d%s buf (off + %d)@\n      %s;@\n" src.u (n * 8)
      (endian_suffix endian) p.at
      (word ~m ~n endian p.at ws)
  in
  let rec go = function
    | [] -> ()
    | p :: rest as ps -> (
        match take 8 ps with
        | Some (ws, endian, rest) ->
            merged ~m:"Int64" ~n:8 p ws endian;
            go rest
        | None -> (
            match take 4 ps with
            | Some ((_ :: _ :: _ as ws), endian, rest) ->
                merged ~m:"Int32" ~n:4 p ws endian;
                go rest
            | _ ->
                one p;
                go rest))
  in
  go (parts fields);
  List.iter
    (fun f ->
      match f.store with
      | Bytes n ->
          Fmt.pf ppf "    %s.unsafe_blit_string %s 0 buf (off + %d) %d;@\n"
            src.checked f.id f.offset n
      | _ -> ())
    fields

(* The checks and stores of a write, on the int values of [fields]; a field
   [in_range] says holds a value in range skips its range check. With [len],
   the frame is checked against that variable, a length the caller read once,
   rather than against the buffer's. *)
let pp_stores ?len ~src ppf ~plan ~in_range fields =
  let bad =
    List.filter_map
      (fun f -> if in_range f then None else out_of_range f)
      fields
  in
  let guard =
    (match bad with
      | [] -> []
      | _ ->
          [
            Fmt.str "(%s) <> 0" (String.concat " lor " (out_of_range_test bad));
          ])
    @ List.filter_map
        (fun f -> Option.map (fun (_, v) -> "not " ^ member f v) f.enum)
        fields
  in
  (match guard with
  | [] -> ()
  | _ ->
      Fmt.pf ppf "  if %s then first_error buf off %a@\n  else"
        (String.concat " || " guard)
        pp_vars fields);
  (match len with
  | None -> Fmt.pf ppf "  if %a@\n  else begin@\n" (pp_frame ~src) "too_short"
  | Some len ->
      Fmt.pf ppf
        "  if off < 0 || off > %s - size then too_short buf off@\n\
        \  else begin@\n"
        len);
  stores ~src ppf fields;
  if has_checks plan then Fmt.pf ppf "    validate_written buf off@\n"
  else Fmt.pf ppf "    ()@\n";
  Fmt.pf ppf "  end@\n@\n"

let pp_write_int ?(src = bytes_src) ppf ~target ~plan =
  let fields = all_fields plan in
  pp_first_error ~src ppf ~target fields;
  Fmt.pf ppf "let[@@inline]%s write_int %s off %a =@\n" (zero_alloc target)
    src.buf pp_vars fields;
  pp_stores ~src ppf ~plan ~in_range:(fun _ -> false) fields

(* The int a field's typed value stores. *)
let plain_value f =
  match (f.map, f.store) with
  | Some _, _ -> Fmt.str "(%s.enc %s)" (map_var f) f.id
  | None, _ -> (
      match f.store with
      | Int { width = 32; signed; _ } ->
          Fmt.str "(%s.%s.to_int32 %s)" rt
            (if signed then "SInt32" else "UInt32")
            f.id
      | Int _ -> Fmt.str "(%s : %s :> int)" f.id (type_of f)
      | Bits _ -> Fmt.str "(%s : %s :> int)" f.id (type_of f)
      | Bytes _ -> Fmt.str "(%s : %s :> string)" f.id (type_of f))

(* [write] takes the codec's typed values. An integer field's type holds it in
   range, as a bitfield's or a byte array's carrier does, so none is checked
   again. A map's functions are the caller's code, so its value is checked, and
   a write through one makes no promise about allocation. *)
let typed_in_range f =
  match (f.map, f.store) with
  | None, (Int _ | Bits _ | Bytes _) -> true
  | Some _, _ -> false

let pp_write ?(src = bytes_src) ppf ~target ~plan =
  let fields = all_fields plan in
  let target =
    if List.exists (fun f -> Option.is_some f.map) fields then `OCaml
    else target
  in
  Fmt.pf ppf "let[@@inline]%s write %s off %a =@\n" (zero_alloc target) src.buf
    pp_vars fields;
  List.iter
    (fun f -> Fmt.pf ppf "  let %s = %s in@\n" f.id (plain_value f))
    fields;
  pp_stores ~src ppf ~plan ~in_range:typed_in_range fields

(* [write_at b len off] is [write], and [write_int_at b len off] [write_int],
   into the buffer named ['b], whose length the caller read once: the frame is
   one comparison against [len], so a loop that writes many records measures
   no buffer. [write_at]'s carriers are in range already; [write_int_at] tests
   the ranges of all its values at once, which costs less than building a
   carrier for each value a loop computes. *)
let pp_write_at ?(src = bytes_src) ~ints ppf ~target ~plan =
  let fields = all_fields plan in
  let target =
    if (not ints) && List.exists (fun f -> Option.is_some f.map) fields then
      `OCaml
    else target
  in
  let b =
    match target with
    | `OxCaml -> Fmt.str "((b : 'b %s.t) @@ local)" src.brand
    | `OCaml -> Fmt.str "(b : 'b %s.t)" src.brand
  in
  Fmt.pf ppf
    "let[@@inline]%s %s %s (len : 'b %s.length) off %a =@\n\
    \  let buf = (b : _ %s.t :> %s) and len = (len : _ %s.length :> int) in@\n"
    (zero_alloc target)
    (if ints then "write_int_at" else "write_at")
    b src.brand pp_vars fields src.brand src.typ src.brand;
  if ints then
    pp_stores ~len:"len" ~src ppf ~plan ~in_range:(fun _ -> false) fields
  else begin
    List.iter
      (fun f -> Fmt.pf ppf "  let %s = %s in@\n" f.id (plain_value f))
      fields;
    pp_stores ~len:"len" ~src ppf ~plan ~in_range:typed_in_range fields
  end

(* A module whose code holds a value only an [int] of 63 bits holds refuses to
   load on a target whose ints have 32 bits or fewer. *)
let pp_wide ppf ~codec ~wide =
  if wide then
    Fmt.pf ppf
      "let () =@\n\
      \  if Sys.int_size <= 32 then@\n\
      \    invalid_arg@\n\
      \      %S@\n\
       @\n\
       let mask32 = Int64.to_int 0xFFFF_FFFFL@\n\
       @\n"
      (Fmt.str
         "Wire_ocaml: the code generated from %s holds integers of more than \
          31 bits, which needs ints wider than 32 bits"
         codec)

(* The module links wire.runtime and no more: it names the shape of its codec
   with [digest], which only {!bridge} checks, and raises each error itself. *)
let pp_header ppf ~codec ~name ~digest =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from %s. Do not edit: generate it again when \
     the codec changes. *)@\n\
     @\n\
     let digest = %S@\n\
     let codec_name = %S@\n\
     let[@@inline] cast : type a b. (a, b) Type.eq -> b -> a = fun Type.Equal \
     v -> v@\n\
     @\n\
     let[@@inline never] no_error () =@\n\
    \  invalid_arg (codec_name ^ \": the generated code found no error to \
     report\")@\n\
     @\n"
    codec digest name

(* Each named map, bound once. A map's functions are a [Wire] value, so a
   module with a map links wire; {!bridge} checks that the field still holds
   that very map. *)
let pp_maps ppf fields =
  List.iter
    (fun f ->
      match f.map with
      | Some path ->
          let make =
            match f.store with
            | Int { width = 32; _ } -> "map_int32"
            | _ -> "map_int"
          in
          Fmt.pf ppf "let %s = Wire.Private.Gen.%s %s@\n" (map_var f) make path
      | None -> ())
    fields;
  Fmt.pf ppf "@\n"

let pp_record ppf fields =
  Fmt.pf ppf "type t = {@\n";
  List.iter (fun f -> Fmt.pf ppf "  %s : %s;@\n" f.label (type_of f)) fields;
  Fmt.pf ppf "}@\n@\nlet v %a = { %a }@\n@\n" pp_vars fields
    Fmt.(list ~sep:(any "; ") string)
    (List.map (fun f -> Fmt.str "%s = %s" f.label f.id) fields)

let pp_encode ~src ppf fields =
  Fmt.pf ppf "let encode t %s off = write buf off %a@\n" src.buf pp_list
    (List.map (fun f -> "t." ^ f.label) fields)

(* [Get.<field> buf off] reads one field as [Codec.get] does, with the same
   bounds-checked load: the compiler's check jumps to a handler that does not
   return, so nothing is kept live across it, and an offset out of range raises
   what [Codec.get] raises. A closed enum's value is then checked to be a case. *)
let checked_load ~src ~off ~bytes ~endian =
  match bytes with
  | 1 -> Fmt.str "(%s.get_uint8 buf (off + %d))" src.checked off
  | 2 ->
      Fmt.str "(%s.get_uint16_%s buf (off + %d))" src.checked
        (endian_suffix endian) off
  | _ ->
      Fmt.str "(%s.get_int32_%s buf (off + %d))" src.checked
        (endian_suffix endian) off

let checked_read ~src f =
  match f.store with
  | Int { width = 32; endian; _ } ->
      checked_load ~src ~off:f.offset ~bytes:4 ~endian
  | Int { width; endian; signed } ->
      let v = checked_load ~src ~off:f.offset ~bytes:(width / 8) ~endian in
      if signed then
        Fmt.str "((%s lsl (Sys.int_size - %d)) asr (Sys.int_size - %d))" v width
          width
      else v
  | Bits { base; shift; width } ->
      let bytes = Bitfield.byte_size base in
      let word =
        checked_load ~src ~off:f.offset ~bytes ~endian:(bit_endian base)
      in
      let word =
        if bytes = 4 then Fmt.str "(Int32.to_int %s land mask32)" word else word
      in
      Fmt.str "((%s lsr %d) land %s)" word shift (mask width)
  | Bytes n ->
      Fmt.str "(%s.sub_string buf (off + %d) %d)" src.checked f.offset n

let read_attr target f =
  match (target, f.store) with
  | `OxCaml, (Int _ | Bits _) when Option.is_none f.map ->
      "[@inline][@zero_alloc]"
  | _ -> "[@inline]"

let pp_getter ~src ppf ~target f =
  let attr = read_attr target f in
  Fmt.pf ppf "let%s %s %s off =@\n" attr f.label src.buf;
  (match f.store with
  | Int { width = 32; _ } ->
      (* Its typed value is made from the [int32]; only an enum's test reads
         the [int]. *)
      Fmt.pf ppf "  let %s_raw = %s in@\n" f.id (checked_read ~src f);
      if Option.is_some f.enum then
        Fmt.pf ppf "  let %s = %s in@\n" f.id (int_view f)
  | _ -> Fmt.pf ppf "  let %s = %s in@\n" f.id (checked_read ~src f));
  (match f.enum with
  | Some (_, valid) ->
      Fmt.pf ppf "  if not %s then enum_failed (off + %d) %s [%a] else@\n"
        (member f valid) f.offset f.id
        Fmt.(list ~sep:(any "; ") (using int_lit string))
        valid
  | None -> ());
  Fmt.pf ppf "  %s@\n@\n" (typed_value f);
  match f.store with
  | Int { width = 32; _ } ->
      (* The [int] view of a 32-bit field, with no conversion through its
         typed form. *)
      Fmt.pf ppf "let%s %s_int %s off =@\n  let %s_raw = %s in@\n  %s@\n@\n"
        attr f.label src.buf f.id (checked_read ~src f) (int_view f)
  | _ -> ()

(* A bitfield's word, read once for all the fields packed in it:
   [Get.<field>_word] is the checked load of the word that holds the field, and
   [<field>_of_word], outside [Get] since a view's word goes through it too,
   takes the field out of it. *)
let word_attr = function
  | `OxCaml -> "[@inline][@zero_alloc]"
  | `OCaml -> "[@inline]"

let pp_word_getter ~src ppf ~target f =
  match f.store with
  | Bits { base; _ } ->
      let bytes = Bitfield.byte_size base in
      let word =
        checked_load ~src ~off:f.offset ~bytes ~endian:(bit_endian base)
      in
      let word =
        if bytes = 4 then Fmt.str "(Int32.to_int %s land mask32)" word else word
      in
      Fmt.pf ppf "let%s %s_word %s off = %s@\n@\n" (word_attr target) f.label
        src.buf word
  | _ -> ()

let pp_of_word ppf ~target f =
  match f.store with
  | Bits { shift; width; _ } ->
      Fmt.pf ppf "let%s %s_of_word w = (w lsr %d) land %s@\n@\n"
        (word_attr target) f.label shift (mask width)
  | _ -> ()

(* The checked reads are in [Get], named after their fields, so a field's name
   meets none of the module's own. *)
let pp_getters ?(src = bytes_src) ppf ~target fields =
  Fmt.pf ppf
    "let%s enum_failed at value valid =@\n\
    \  %s.invalid_enum ~at ~field:[] ~value ~valid@\n\
     @\n\
     module Get = struct@\n"
    (cold_attr target) errors;
  List.iter (pp_getter ~src ppf ~target) fields;
  List.iter (pp_word_getter ~src ppf ~target) fields;
  Fmt.pf ppf "end@\n@\n";
  List.iter (pp_of_word ppf ~target) fields

(* One field at [off] read with no check of its own, in a record of [size]
   bytes already checked to fit. A 32-bit field is bound as its [int32] only:
   both its typed value and its [int] view are made from that. *)
let pp_read ~src ~ox ?size ppf f =
  let m = hot ~ox ~raw32:true ~src ~live:Sset.empty ~fail:"" () in
  match f.store with
  | Int { width = 32; endian; _ } ->
      Fmt.pf ppf "  let %s_raw = %s in@\n" f.id
        (load m ~o:"off" ~k:f.offset ~bytes:4 ~endian)
  | _ ->
      Fmt.pf ppf "  let %s = %s in@\n" f.id
        (value m ?size ~o:"off" ~words:[] ~k:0 f)

(* [Valid.v buf] validates the record at offset 0. A buffer's length never
   changes, so each [read_<field>] after it is one load at a constant offset
   with no check of its own, and stays in the buffer even if its bytes change;
   only then may it read a value that breaks a constraint. This is EverParse's
   validate-then-read, with the proof a type. *)
let pp_valid ?(src = bytes_src) ppf ~target ~plan =
  let fields = all_fields plan in
  Fmt.pf ppf
    "module Valid : sig@\n\
    \  type t = private %s@\n\
     @\n\
    \  val v : %s -> t@\n\
     end = struct@\n\
    \  type t = %s@\n\
     @\n\
    \  let[@inline] v buf =@\n\
    \    if %s buf < size then fail buf 0@\n\
    \    else begin@\n"
    src.typ src.typ src.typ src.len;
  if has_checks plan then Fmt.pf ppf "      validate buf 0;@\n";
  Fmt.pf ppf "      buf@\n    end@\nend@\n@\n";
  List.iter
    (fun f ->
      let attr = read_attr target f in
      let read ~suffix ~result =
        Fmt.pf ppf
          "let%s read_%s%s (v : Valid.t) =@\n\
          \  let buf = (v :> %s) and off = 0 in@\n"
          attr f.label suffix src.typ;
        pp_read ~src ~ox:false ppf f;
        Fmt.pf ppf "  %s@\n@\n" result
      in
      read ~suffix:"" ~result:(typed_value f);
      match f.store with
      | Int { width = 32; _ } -> read ~suffix:"_int" ~result:(int_view f)
      | _ -> ())
    fields

(* The buffer [b] named as [buf], and the length [len] as an [int]. The checks'
   loads are the only reads of [buf]: a record with none binds only [len]. *)
let pp_bind_len ~src ppf ~live =
  if Sset.is_empty live then Fmt.pf ppf "    let "
  else
    Fmt.pf ppf "    let buf = (b : _ %s.t :> %s)@\n    and " src.brand src.typ;
  Fmt.pf ppf "len = (len : _ %s.length :> int) in@\n" src.brand

(* The body of [View.check]. A record with no check of its own loads nothing,
   so a negative [off] needs no test: it is the answer, which [View.ok]
   refuses. *)
let pp_view_check ~src ppf ~target ~plan ~size ~live =
  pp_bind_len ~src ppf ~live;
  if has_checks plan then begin
    Fmt.pf ppf "    if off < 0 || off > len - size then -1@\n    else@\n";
    let m = hot ~ox:(target = `OxCaml) ~src ~live ~fail:"-1" () in
    pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "off@\n")
  end
  else Fmt.pf ppf "    if off > len - size then -1 else off@\n"

(* A view is a record checked in a named buffer: [View.check b len off] runs
   the frame check against [len], the buffer's length read once by the caller,
   and the record's own checks, and answers the offset, or a negative [int].
   [View.get] turns an answer that passed into a ['b View.t], which only the
   reads of the buffer named ['b] accept, so each [view_<field> b v] is one
   load at a constant offset from [v] with no check of its own, and costs no
   allocation: [b] is the buffer and [v] an [int]. [View.get] tests the
   comparison [View.ok] makes, so that a compiler which shares the two
   (flambda2) drops the second test after [if View.ok c]. *)
let pp_view_module ~src ppf ~target ~plan ~size =
  let attr =
    match target with
    | `OxCaml -> "[@inline][@zero_alloc]"
    | `OCaml -> "[@inline]"
  in
  let param, local =
    match target with
    | `OxCaml -> ((fun b -> "(" ^ b ^ " @ local)"), " @ local")
    | `OCaml -> (Fun.id, "")
  in
  let b = param "b" in
  let live = live plan ~outputs:[] in
  (* A record whose checks load nothing reads neither [b] nor [buf]. *)
  let b_param = if Sset.is_empty live then param "_b" else b in
  Fmt.pf ppf
    "module View : sig@\n\
    \  type 'b t = private int@\n\
    \  type 'b check = private int@\n\
     @\n\
    \  val at : 'b %s.t%s -> 'b %s.length -> int -> 'b t@\n\
    \  val check : 'b %s.t%s -> 'b %s.length -> int -> 'b check@\n\
    \  val ok : 'b check -> bool@\n\
    \  val get : 'b check -> 'b t@\n\
     end = struct@\n\
    \  type 'b t = int@\n\
    \  type 'b check = int@\n\
     @\n\
    \  let%s check %s len off =@\n"
    src.brand local src.brand src.brand local src.brand attr b_param;
  pp_view_check ~src ppf ~target ~plan ~size ~live;
  Fmt.pf ppf
    "@\n\
    \  let%s at %s len off =@\n\
    \    let c = check b len off in@\n\
    \    if c < 0 then fail (b : _ %s.t :> %s) off else c@\n\
     @\n\
    \  let%s ok c = c >= 0@\n\
     @\n\
    \  let failed = Invalid_argument (codec_name ^ \".View.get: the check \
     failed\")@\n\
     @\n\
    \  let%s get c = if c >= 0 then c else raise failed@\n\
     end@\n\
     @\n"
    attr b src.brand src.typ attr attr

let pp_view ?(src = bytes_src) ppf ~target ~plan ~size =
  let fields = all_fields plan in
  let ox = target = `OxCaml in
  pp_view_module ~src ppf ~target ~plan ~size;
  let b =
    match target with
    | `OxCaml -> Fmt.str "((b : 'b %s.t) @@ local)" src.brand
    | `OCaml -> Fmt.str "(b : 'b %s.t)" src.brand
  in
  let head attr name =
    Fmt.pf ppf
      "let%s %s %s (v : 'b View.t) =@\n\
      \  let buf = (b : _ %s.t :> %s) and off = (v : _ View.t :> int) in@\n"
      attr name b src.brand src.typ
  in
  List.iter
    (fun f ->
      let attr = read_attr target f in
      let view ~suffix ~result =
        head attr (Fmt.str "view_%s%s" f.label suffix);
        pp_read ~src ~ox ~size ppf f;
        Fmt.pf ppf "  %s@\n@\n" result
      in
      view ~suffix:"" ~result:(typed_value f);
      match f.store with
      | Int { width = 32; _ } -> view ~suffix:"_int" ~result:(int_view f)
      | Bits { base; _ } ->
          (* The word that holds the field, for [<field>_of_word]: fields
             packed in one word share one load. *)
          let m = hot ~ox ~src ~live:Sset.empty ~fail:"" () in
          head attr (Fmt.str "view_%s_word" f.label);
          Fmt.pf ppf "  %s@\n@\n"
            (unsigned m ~size ~o:"off" ~words:[] ~k:f.offset
               ~bytes:(Bitfield.byte_size base) ~endian:(bit_endian base) ())
      | _ -> ())
    fields

(* A record the encoder wrote that fails its own constraints. On OxCaml the
   check is [is_valid], which the allocation checker follows, and only a record
   that fails it reaches [rejected], which raises what [validate] found as an
   encoding error and so only raises. *)
let pp_validate_written ~src ppf ~target =
  match target with
  | `OCaml ->
      Fmt.pf ppf
        "let[@inline never] validate_written %s off =@\n\
        \  match validate buf off with@\n\
        \  | () -> ()@\n\
        \  | exception %s.Parse_error e -> %s.encode_rejected ~name:codec_name \
         e@\n\
         @\n"
        src.buf errors errors
  | `OxCaml ->
      Fmt.pf ppf
        "let%s rejected %s off =@\n\
        \  match validate buf off with@\n\
        \  | () -> no_error ()@\n\
        \  | exception %s.Parse_error e -> %s.encode_rejected ~name:codec_name \
         e@\n\
         @\n\
         let[@inline][@zero_alloc] validate_written %s off =@\n\
        \  if not (is_valid buf off) then rejected buf off@\n\
         @\n"
        (cold_attr target) src.buf errors errors src.buf

(* [decode_local] builds the generated record in the caller's frame, so a
   decode of integer fields allocates nothing on the heap; a byte array is
   still a string on the heap, so with one it makes no such promise.
   [decode_with] takes the constructor as an argument, which flambda2 inlines
   with it. *)
let pp_oxcaml ~src ppf ~record ~plan ~size =
  let fields = all_fields plan in
  pp_decode ~src ~ox:true ppf ~plan ~size
    ~head:(Fmt.str "let[@inline] decode_with (make @@ local) %s off" src.buf)
    ~call:(fun args -> Fmt.str "make %a" pp_list args);
  let bytes =
    List.exists
      (fun f -> match f.store with Bytes _ -> true | _ -> false)
      fields
  in
  if record then
    pp_decode ~src ~ox:true ppf ~plan ~size
      ~head:
        (Fmt.str "let%s decode_local %s off"
           (if bytes then "" else "[@zero_alloc]")
           src.buf)
      ~call:(fun args ->
        Fmt.str "exclave_ stack_ { %a }"
          Fmt.(list ~sep:(any "; ") string)
          (List.map2 (fun f a -> Fmt.str "%s = %s" f.label a) fields args))

(* [decode] answers the codec's error as a value, as [Wire.Codec.decode]
   does; [decode_exn] raises it. *)
let pp_decode_result ~src ppf =
  Fmt.pf ppf
    "let decode %s off =@\n\
    \  match decode_exn buf off with@\n\
    \  | v -> Ok v@\n\
    \  | exception %s.Parse_error e -> Error e@\n\
     @\n"
    src.buf errors

(* [Field.<field>] names a field as a value, of the type its getter returns, so
   code written against [Wire.S] reads it the same way from the interpreted
   codec: [Field.get f] is [Get.<field>] and [Field.view f] [view_<field>]. A
   field under a map, whose type is the caller's, has none, nor has an
   anonymous one. *)
let field_fields fields =
  List.filter
    (fun f ->
      Option.is_none f.map && not (String.starts_with ~prefix:"_" f.label))
    fields

let pp_field_module ~src ppf ~target fields =
  let fields = field_fields fields in
  let b =
    match target with
    | `OxCaml -> Fmt.str "'b %s.t @@ local" src.brand
    | `OCaml -> Fmt.str "'b %s.t" src.brand
  in
  (* A handle is the field's functions, built once: [f.get buf off] of a known
     handle is a call to a known function, which the compiler inlines where it
     is written, on OCaml's Closure as on flambda2. *)
  Fmt.pf ppf
    "module Field = struct@\n\
    \  type 'a t = {@\n\
    \    get : bytes -> int -> 'a;@\n\
    \    view : 'b. %s -> 'b View.t -> 'a;@\n\
    \  }@\n\
     @\n"
    b;
  (* On OxCaml [Get.<field>] takes its buffer [local], a mode a handle's
     [get] does not state, so it is eta-expanded there. *)
  List.iter
    (fun f ->
      let get =
        match target with
        | `OxCaml -> Fmt.str "(fun buf off -> Get.%s buf off)" f.label
        | `OCaml -> "Get." ^ f.label
      in
      Fmt.pf ppf "  let %s : %s t = { get = %s; view = view_%s }@\n@\n" f.label
        (type_of f) get f.label)
    fields;
  Fmt.pf ppf "end@\n@\n"

(* [Global] is the module as a [Wire.S], for code that switches between it and
   [Wire.Codec.Make] by one line. On OCaml it is the module's own functions; on
   OxCaml, whose functions take their buffers [local], which an interpreted
   codec cannot, it states them for buffers on the heap. *)
let pp_global ~src ppf ~target fields =
  let fields = field_fields fields in
  Fmt.pf ppf
    "module Global = struct@\n\
    \  type nonrec t = t@\n\
     @\n\
    \  let min_size = min_size@\n";
  match target with
  | `OCaml ->
      Fmt.pf ppf
        "  let decode = decode@\n\
        \  let decode_exn = decode_exn@\n\
        \  let encode = encode@\n\
        \  let validate = validate@\n\
        \  let is_valid = is_valid@\n\
        \  module View = View@\n\
        \  module Field = Field@\n\
         end@\n\
         @\n"
  | `OxCaml ->
      Fmt.pf ppf
        "  let decode buf off = decode buf off@\n\
        \  let decode_exn buf off = decode_exn buf off@\n\
        \  let encode t buf off = encode t buf off@\n\
        \  let validate buf off = validate buf off@\n\
        \  let is_valid buf off = is_valid buf off@\n\
         @\n\
        \  module View = struct@\n\
        \    type 'b t = 'b View.t@\n\
        \    type 'b check = 'b View.check@\n\
         @\n\
        \    let at b len off = View.at b len off@\n\
        \    let check b len off = View.check b len off@\n\
        \    let ok = View.ok@\n\
        \    let get = View.get@\n\
        \  end@\n\
         @\n\
        \  module Field = struct@\n\
        \    type 'a t = {@\n\
        \      get : bytes -> int -> 'a;@\n\
        \      view : 'b. 'b %s.t -> 'b View.t -> 'a;@\n\
        \    }@\n\
         @\n"
        src.brand;
      List.iter
        (fun f ->
          Fmt.pf ppf
            "    let %s : %s t =@\n\
            \      { get = Field.%s.get; view = (fun b v -> Field.%s.view b v) \
             }@\n\
             @\n"
            f.label (type_of f) f.label f.label)
        fields;
      Fmt.pf ppf "  end@\nend@\n@\n"

(* The same reads and writes on a bigarray of bytes, with the same errors. *)
let pp_bigstring ppf ~target ~make ~record ~plan ~size =
  let src =
    match target with `OxCaml -> local_bigstring_src | `OCaml -> bigstring_src
  in
  let fields = all_fields plan in
  Fmt.pf ppf "module Bigstring = struct@\n";
  pp_cold ~src ppf ~target ~plan ~size;
  pp_validate ~src ppf ~target ~plan ~size;
  pp_validate_written ~src ppf ~target;
  let value = if record then typed_value else plain_typed_value in
  pp_decode ~src ~value ppf ~plan ~size
    ~head:(Fmt.str "let decode_exn %s off" src.buf) ~call:(fun args ->
      Fmt.str "%s %a" make pp_list args);
  pp_decode_result ~src ppf;
  pp_write_int ~src ppf ~target ~plan;
  pp_write ~src ppf ~target ~plan;
  pp_write_at ~src ~ints:true ppf ~target ~plan;
  pp_write_at ~src ~ints:false ppf ~target ~plan;
  pp_getters ~src ppf ~target fields;
  pp_valid ~src ppf ~target ~plan;
  pp_view ~src ppf ~target ~plan ~size;
  if record then pp_encode ~src ppf fields;
  Fmt.pf ppf "end@\n@\n"

let pp_body ppf ~target ~make ~plan ~size =
  let fields = all_fields plan in
  Fmt.pf ppf "let size = %d@\nlet min_size = size@\n@\n" size;
  pp_values ppf fields;
  let record = Option.is_none make in
  if record then pp_record ppf fields;
  let src =
    match target with `OxCaml -> local_bytes_src | `OCaml -> bytes_src
  in
  pp_cold ~src ppf ~target ~plan ~size;
  pp_validate ~src ppf ~target ~plan ~size;
  pp_validate_written ~src ppf ~target;
  let make = Option.value ~default:"v" make in
  let value = if record then typed_value else plain_typed_value in
  pp_decode ~src ~value ppf ~plan ~size
    ~head:(Fmt.str "let decode_exn %s off" src.buf) ~call:(fun args ->
      Fmt.str "%s %a" make pp_list args);
  pp_decode_result ~src ppf;
  if target = `OxCaml then pp_oxcaml ~src ppf ~record ~plan ~size;
  pp_write_int ~src ppf ~target ~plan;
  pp_write ~src ppf ~target ~plan;
  pp_write_at ~src ~ints:true ppf ~target ~plan;
  pp_write_at ~src ~ints:false ppf ~target ~plan;
  pp_getters ~src ppf ~target fields;
  pp_valid ~src ppf ~target ~plan;
  pp_view ~src ppf ~target ~plan ~size;
  pp_field_module ~src ppf ~target fields;
  if record then begin
    pp_encode ~src ppf fields;
    pp_global ~src ppf ~target fields
  end;
  pp_bigstring ppf ~target ~make ~record ~plan ~size

let refuse ~codec reason =
  Fmt.invalid_arg "Wire_ocaml: cannot specialise %s: %s" codec reason

(* The names the generated module defines for itself, at top level or in
   [Bigstring]. *)
let own_names =
  [
    "digest";
    "codec_name";
    "cast";
    "no_error";
    "fail";
    "too_short";
    "size";
    "is_valid";
    "validate";
    "validate_written";
    "rejected";
    "decode";
    "decode_exn";
    "decode_with";
    "decode_local";
    "first_error";
    "write_int";
    "write";
    "write_at";
    "write_int_at";
    "enum_failed";
    "Get";
    "v";
    "encode";
    "min_size";
    "t";
    "Valid";
    "View";
    "Value";
    "Field";
    "Global";
    "Bigstring";
    "mask32";
  ]

(* The names inside [Get] that its reads call: a getter of one of these
   names would shadow it for the getters after it. *)
let getters_own fields =
  "Get.cast" :: "Get.mask32" :: "Get.enum_failed"
  :: List.filter_map
       (fun f -> Option.map (fun _ -> "Get." ^ map_var f) f.map)
       fields

(* The names a field's functions and record field take, those in [Get]
   qualified. *)
let names_of f =
  let l = f.label in
  let derived =
    match f.store with
    | Int { width = 32; _ } ->
        [ "Get." ^ l ^ "_int"; "read_" ^ l ^ "_int"; "view_" ^ l ^ "_int" ]
    | Bits _ -> [ "Get." ^ l ^ "_word"; "view_" ^ l ^ "_word"; l ^ "_of_word" ]
    | Int _ | Bytes _ -> []
  in
  ("Get." ^ l) :: ("read_" ^ l) :: ("view_" ^ l) :: derived

(* Who defines a name: the generated module itself, or a field. *)
type source = Own | Field of string

(* A later definition would shadow an earlier one of the same name. Two fields
   of one label also define one getter, so this covers the record's fields
   too. *)
let check_names ~codec fields =
  let own = List.map (fun n -> (Own, n)) (own_names @ getters_own fields) in
  let defined =
    List.concat_map
      (fun f -> List.map (fun n -> (Field f.name, n)) (names_of f))
      fields
  in
  match Names.clash (own @ defined) with
  | Some (Own, Field f, n) | Some (Field f, Own, n) ->
      Fmt.kstr (refuse ~codec)
        "field %S defines %s, which the generated module defines for itself" f n
  | Some (Field g, Field f, n) ->
      Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
  | Some (Own, Own, _) | None -> ()

(* The record module covers a fixed record of named scalars, each one
   argument of [make]: a nested record, an optional field, a slice or an
   anonymous field has no place in it. *)
let record_scope (s : T.struct_) =
  if s.params <> [] then fail_unsupported "the codec has parameters";
  List.iter
    (fun (T.Field f) ->
      let rec scalar : type a. a T.typ -> bool = function
        | T.Byte_slice _ -> false
        | T.Where { inner; _ } -> scalar inner
        | T.Enum { base; _ } -> scalar base
        | T.Map { inner; _ } -> scalar inner
        | t -> is_leaf t
      in
      match f.field_name with
      | None -> fail_unsupported "an anonymous field"
      | Some name -> (
          if not (scalar f.field_typ) then
            match f.field_typ with
            | T.Optional _ ->
                fail_unsupported "field %s: an optional field" name
            | T.Codec _ -> fail_unsupported "field %s: a nested record" name
            | T.Single_elem _ ->
                fail_unsupported "field %s: a sized region" name
            | T.Byte_slice _ -> fail_unsupported "field %s: a byte slice" name
            | t -> fail_unsupported "field %s: its type, %a" name T.pp_typ t))
    s.fields

let record_plan ~maps c =
  let s = Gen.to_struct c in
  record_scope s;
  let plan = plan_of ~maps:(maps_of maps) s in
  match plan.nodes with
  | [] | [ Test _ ] -> (plan, 0)
  | [ Prefix (_, n) ] | [ Prefix (_, n); Test _ ] -> (plan, n)
  | _ -> fail_unsupported "its size is not fixed"

let pp ?(target = `OCaml) ?make ?(maps = []) ~codec ppf c =
  let digest = Gen.digest c in
  match record_plan ~maps c with
  | exception Unsupported reason -> refuse ~codec reason
  | _, size when Wire.Codec.wire_size_opt c <> Some size ->
      refuse ~codec "its size is not fixed"
  | plan, _
    when Option.is_none make
         && List.exists (fun f -> Option.is_some f.map) (all_fields plan) ->
      refuse ~codec
        "a field is a map, and the generated record cannot name its type; pass \
         ~make"
  | plan, size ->
      let fields = all_fields plan in
      check_names ~codec fields;
      pp_header ppf ~codec ~name:plan.record ~digest;
      (* On OxCaml the runtime's values are seen [portable], so a parallel
         task may call any function of the module. *)
      if target = `OxCaml then
        Fmt.pf ppf "module Wire_runtime = Wire_runtime.Target.Portable@\n@\n";
      pp_wide ppf ~codec ~wide:plan.wide;
      pp_maps ppf fields;
      pp_body ppf ~target ~make ~plan ~size

let ident = label

let ml ?target ?make ?maps ~codec c =
  Fmt.str "%a" (pp ?target ?make ?maps ~codec) c

let pp_bridge ?(maps = []) ~codec ~core ppf c =
  let fields =
    match record_plan ~maps c with
    | plan, _ -> all_fields plan
    | exception Unsupported reason -> refuse ~codec reason
  in
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from %s. Do not edit. Checks, when it loads, \
     that %s still has the shape %s was generated from. *)@\n\
     @\n\
     let () = Wire.Private.Gen.check %s %s.digest@\n"
    codec codec core codec core;
  List.iter
    (fun f ->
      Option.iter
        (fun path ->
          Fmt.pf ppf "let () = Wire.Private.Gen.check_map %s %S %s@\n" codec
            f.name path)
        f.map)
    fields

let bridge ?maps ~codec ~core c = Fmt.str "%a" (pp_bridge ?maps ~codec ~core) c
