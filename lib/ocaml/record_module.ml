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
    raw32;
    ranged = false;
    live;
    exported = Sset.empty;
    slots = Sset.empty;
    ahead = Sset.empty;
    fail = (fun _ -> fail);
    decide = ignore;
    note = ignore;
  }

let cold ~src ~live =
  {
    num = Tagged;
    src;
    ox = false;
    raw32 = false;
    ranged = false;
    live;
    exported = Sset.empty;
    slots = Sset.empty;
    ahead = Sset.empty;
    fail =
      (function
      | Bounds -> "no_error ()"
      | Check (f, c) -> raise_check f c
      | Where -> errors ^ ".where_clause ~at:off");
    decide = ignore;
    note = ignore;
  }

(* A fixed record's walk at [off], whose frame is checked before it. *)
let pp_fixed ppf ~m ~(plan : plan) ~size k =
  start ~m ~ppf ~ranges:plan.ranges ~base:"off" ~limit:"len" ~known:size
    plan.nodes (fun _ _ -> k ())

let pp_frame ~src ppf on_short =
  Fmt.pf ppf "off < 0 || off > %s buf - min_size then %s buf off" src.len
    on_short

let all_fields (plan : plan) = fields_of plan.nodes

let has_checks (plan : plan) =
  List.exists (fun f -> f.checks <> []) (all_fields plan)
  || List.exists (function Test _ -> true | _ -> false) plan.nodes

(* The [int] a test sees of a 32-bit field, read as an [int32]. *)
let int_view ~src f =
  match f.store with
  | Int { width = 32; signed; _ } ->
      Fmt.str "(%s.to_int %s_raw%s)" (i32 src) f.id
        (if signed then "" else " land mask32")
  | _ -> f.id

let map_var f = "map_" ^ f.id

(* The value handed to the constructor: through the field's map when it has
   one, the failure placed at the field. *)
let typed_value ~src f =
  match (f.map, f.store) with
  | Some _, Int { width = 32; _ } when src.unboxed ->
      Fmt.str "(%s.dec ~at:(off + %d) (Int32_u.to_int32 %s_raw))" (map_var f)
        f.offset f.id
  | Some _, Int { width = 32; _ } ->
      Fmt.str "(%s.dec ~at:(off + %d) %s_raw)" (map_var f) f.offset f.id
  | Some _, _ -> Fmt.str "(%s.dec ~at:(off + %d) %s)" (map_var f) f.offset f.id
  | None, _ -> (
      match f.store with
      | Int { width = (8 | 16) as w; signed; _ } ->
          Fmt.str "(cast %s.%sInt%d.int_eq %s)" rt
            (if signed then "S" else "U")
            w f.id
      | Int { signed; _ } when src.unboxed ->
          (* Through [int]: an [int32] would be boxed to cross the call. *)
          Fmt.str "(%s.%s.of_int %s)" rt
            (if signed then "SInt32" else "UInt32")
            (int_view ~src f)
      | Int { signed; _ } ->
          Fmt.str "(%s.%s.of_int32 %s_raw)" rt
            (if signed then "SInt32" else "UInt32")
            f.id
      | Bits _ | Bytes _ -> f.id)

(* The attributes the OxCaml target adds: an allocation check on a hot path,
   and on a cold one the promise that it only raises. *)
let zero_alloc = function `OxCaml -> "[@zero_alloc]" | `OCaml -> ""

let cold_attr = function
  | `OxCaml -> "[@inline never][@zero_alloc assume error]"
  | `OCaml -> "[@inline never]"

(* [decode]: the frame, the walk binding every field, then the record of
   their typed values; the record's error where a check fails. *)
let pp_decode ~src ppf ~plan ~size =
  let fields = all_fields plan in
  let fail = "error buf off" in
  Fmt.pf ppf
    "let decode %s ~off =@\n\
    \  if off < 0 || off > %s buf - min_size then %s@\n\
    \  else@\n"
    src.buf src.len fail;
  (* A 32-bit field is handed out as its [int32], bound whether live or not;
     its [int] is live only where a check reads it. *)
  let outputs =
    List.filter_map
      (fun f ->
        match f.store with Int { width = 32; _ } -> None | _ -> Some f.id)
      fields
  in
  let m = hot ~raw32:true ~src ~live:(live plan ~outputs) ~fail () in
  pp_fixed ppf ~m ~plan ~size (fun () ->
      Fmt.pf ppf "Ok { %a }@\n"
        Fmt.(list ~sep:(any "; ") string)
        (List.map
           (fun f -> Fmt.str "%s = %s" f.label (typed_value ~src f))
           fields));
  Fmt.pf ppf "@\n"

(* [is_valid] answers with a boolean, loading only the fields a test reads, so
   a caller that drops a bad record on the floor pays for no exception handler;
   [validate] answers what the codec answers, found off the hot path by
   [error]. *)
let pp_validate ~src ppf ~target ~plan ~size =
  let attr = "[@inline]" ^ zero_alloc target in
  Fmt.pf ppf
    "let%s is_valid %s off =@\n\
    \  if off < 0 || off > %s buf - min_size then false@\n\
    \  else@\n"
    attr src.buf src.len;
  let m =
    hot ~ox:(target = `OxCaml) ~src ~live:(live plan ~outputs:[]) ~fail:"false"
      ()
  in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "true@\n");
  Fmt.pf ppf
    "@\n\
     let validate %s ~off = if is_valid buf off then Ok () else error buf off@\n\
     @\n"
    src.buf

(* {2 A record that does not fit}

   3D checks each field as it reaches it: a record that does not fit reports
   the first field in order that fails a check where it fits, else the end of
   input. A [where] clause is checked with the last field it reads, as the
   refinement of that field the projection makes it, or before every field
   when it reads none. *)

(* Where each field of [fields] ends, by its variable. *)
let field_ends fields =
  List.fold_left
    (fun acc (f : field) -> Smap.add f.id (f.offset + width_bytes f.store) acc)
    Smap.empty fields

(* The nodes of a record that does not fit, with the [where] clause after the
   last field it reads, and where that field ends: the prefix is cut there. *)
let short_nodes ends (plan : plan) =
  let part n nodes = if n = 0 then [] else nodes in
  match plan.nodes with
  | [ Prefix (fields, size); Test e ] ->
      let at =
        List.fold_left
          (fun acc id ->
            match Smap.find_opt id ends with Some e -> max acc e | None -> acc)
          0 (T.collect_refs e)
      in
      let before, after = List.partition (fun f -> f.offset < at) fields in
      let after = List.map (fun f -> { f with offset = f.offset - at }) after in
      let prefix n = function [] -> Span (T.Int n) | fs -> Prefix (fs, n) in
      ( at,
        part at [ prefix at before ]
        @ (Test e :: part (size - at) [ prefix (size - at) after ]) )
  | nodes -> (0, nodes)

(* The checks of the walk of a record that does not fit, over [avail] of its
   bytes padded with zeros: each raises its error where its field fits, the
   [where] clause where the last field it reads fits, else the end of input. A
   field keeps its offset in the record in its error. *)
let short_cold ~src ~live ~size ~ends ~offsets ~where_end =
  let end_of (f : field) = Option.value ~default:size (Smap.find_opt f.id ends)
  and placed (f : field) =
    {
      f with
      offset = Option.value ~default:f.offset (Smap.find_opt f.id offsets);
    }
  in
  let fits n e =
    Fmt.str "(if %d > avail then %s.frame ~size:min_size ~len off else %s)" n
      errors e
  in
  let m = cold ~src ~live in
  {
    m with
    fail =
      (function
      | Bounds -> "no_error ()"
      | Check (f, c) -> fits (end_of f) (raise_check (placed f) c)
      | Where -> fits where_end (errors ^ ".where_clause ~at:off"));
  }

(* [short buf off] raises the error of a record that does not fit at [off]:
   its first check that fails, run on the bytes there are, else the frame's. *)
let pp_short ~src ppf ~target ~plan ~size =
  if not (has_checks plan) then
    Fmt.pf ppf
      "let%s short %s off =@\n  %s.frame ~size:min_size ~len:(%s buf) off@\n@\n"
      (cold_attr target) src.buf errors src.len
  else begin
    let tmp =
      match target with `OxCaml -> local_bytes_src | `OCaml -> bytes_src
    in
    Fmt.pf ppf
      "let%s short %s off =@\n\
      \  let len = %s buf in@\n\
      \  if off >= 0 then begin@\n\
      \    let avail = len - off in@\n\
      \    let tmp = Bytes.make min_size '\\000' in@\n\
      \    for i = 0 to avail - 1 do@\n\
      \      %s.set_u8 tmp i (%s.u8 buf (off + i))@\n\
      \    done;@\n\
      \    let buf = tmp in@\n"
      (cold_attr target) src.buf src.len tmp.u src.u;
    let fields = all_fields plan in
    let ends = field_ends fields in
    let offsets =
      List.fold_left
        (fun acc (f : field) -> Smap.add f.id f.offset acc)
        Smap.empty fields
    in
    let where_end, nodes = short_nodes ends plan in
    let m =
      short_cold ~src:tmp
        ~live:(live ~cold:true plan ~outputs:[])
        ~size ~ends ~offsets ~where_end
    in
    start ~m ~ppf ~ranges:plan.ranges ~base:"0" ~limit:"len" ~known:size nodes
      (fun _ _ -> Fmt.pf ppf "()@\n");
    Fmt.pf ppf "  end;@\n  %s.frame ~size:min_size ~len off@\n@\n" errors
  end

let pp_cold ~src ppf ~target ~plan ~size =
  pp_short ~src ppf ~target ~plan ~size;
  Fmt.pf ppf
    "let%s fail %s off =@\n\
    \  if off < 0 || off > %s buf - min_size then short buf off;@\n"
    (cold_attr target) src.buf src.len;
  let m = cold ~src ~live:(live ~cold:true plan ~outputs:[]) in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "no_error ()@\n");
  Fmt.pf ppf
    "@\n\
     let[@inline never] error %s off =@\n\
    \  match fail buf off with@\n\
    \  | r -> r@\n\
    \  | exception exn when %s.is_error exn -> Error (%s.error_of exn)@\n\
     @\n\
     let%s too_short %s off =@\n\
    \  %s.encode_too_short ~name:codec_name ~size:min_size ~len:(%s buf) off@\n\
     @\n"
    src.buf errors errors (cold_attr target) src.buf errors src.len

let type_of f =
  let t = Fmt.str "%s.%s.t" rt in
  match f.store with
  | Int { width = 8; signed = false; _ } -> t "UInt8"
  | Int { width = 8; signed = true; _ } -> t "SInt8"
  | Int { width = 16; signed = false; _ } -> t "UInt16"
  | Int { width = 16; signed = true; _ } -> t "SInt16"
  | Int { signed = false; _ } -> t "UInt32"
  | Int { signed = true; _ } -> t "SInt32"
  | Bits _ -> "int"
  | Bytes _ -> "string"

let pp_vars ppf fields = pp_list ppf (List.map (fun f -> f.id) fields)

(* The labelled parameters of a write: each field's value under its label. *)
let pp_labelled ppf fields =
  pp_list ppf (List.map (fun f -> Fmt.str "~%s:%s" f.label f.id) fields)

let labelled fields args =
  String.concat " "
    (List.map2 (fun f a -> Fmt.str "~%s:%s" f.label a) fields args)

(* Writes take each value in its plain form: an [int] for a field up to 16 bits
   or a bitfield, an [int32] for a 32-bit field, a [string] for a byte array. *)

(* The bits that put a plain value out of its field's range: the value, offset
   to zero for a signed field, above the field's width. Or-ing the values of
   one width before masking makes one test, with no branch per field, that
   finds any. The mask clears the low bits where a shift would move them out:
   on a tagged [int] the [land] keeps the tag and the shift must restore it. *)
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
      | [ x ] -> Fmt.str "(%s land lnot %s)" x (mask w)
      | _ -> Fmt.str "((%s) land lnot %s)" (String.concat " lor " xs) (mask w))
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
  (* A 32-bit value is in range as an [int32]: only an enum tests it. Its
     parameter is named, as every parameter the F* rendering compares is. *)
  let param f =
    match (f.store, f.enum) with
    | Int { width = 32; _ }, None -> "_" ^ f.id
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
   the values of a bitfield word's fields with their shifts, or the [int32] of
   a 32-bit field. *)
type part = {
  at : int;
  size : int;
  endian : T.endian option;
  value : [ `Int of string | `Bits of (string * int) list | `I32 of string ];
}

(* A bitfield word's store: the values of its fields, each in range, shifted
   into place and or-ed. *)
let bit_word fields (o, base) =
  let bits =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { shift; _ } when f.offset = o -> Some (f.id, shift)
        | _ -> None)
      fields
  in
  let value = `Bits bits in
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

(* The [int] an int part stores. *)
let int_value = function
  | `Int v -> v
  | `Bits bits ->
      Fmt.str "(%s)"
        (String.concat " lor "
           (List.map (fun (x, sh) -> Fmt.str "(%s lsl %d)" x sh) bits))
  | `I32 _ -> invalid_arg "int_value"

let shifted ~m v sh =
  if sh = 0 then v else Fmt.str "(%s.shift_left %s %d)" m v sh

let rec ors ~m = function
  | [] -> assert false
  | [ x ] -> x
  | x :: xs -> Fmt.str "(%s.logor %s %s)" m x (ors ~m xs)

(* A word as the or of its parts, each widened first: built in [int32] or
   [int64], it needs no [int] wider than one field. With [split], each
   bitfield is widened alone, so its untagging folds into its own shift. *)
let word ?(split = false) ~src ~m ~n endian base ps =
  let part p =
    let sh = shift endian ~n base p in
    let v =
      match p.value with
      | `Bits bits when split ->
          ors ~m
            (List.map
               (fun (x, s) ->
                 shifted ~m (Fmt.str "(%s.of_int %s)" m x) (sh + s))
               bits)
      | (`Int _ | `Bits _) as v -> Fmt.str "(%s.of_int %s)" m (int_value v)
      | `I32 x when n = 8 && sh + 32 < 64 && src.unboxed ->
          Fmt.str "(%s.logand (%s.of_int32_u %s) #0xffffffffL)" m m x
      | `I32 x when n = 8 && sh + 32 < 64 ->
          Fmt.str "(Int64.logand (Int64.of_int32 %s) 0xffffffffL)" x
      | `I32 x when src.unboxed && n = 4 -> x
      | `I32 x when src.unboxed -> Fmt.str "(%s.of_int32_u %s)" m x
      | `I32 x -> Fmt.str "(%s.of_int32 %s)" m x
    in
    match p.value with `Bits _ when split -> v | _ -> shifted ~m v sh
  in
  ors ~m (List.map part ps)

(* The int parts of a word of [n] bytes, or'ed in place. *)
let int_word ~n endian base ps =
  let part p =
    let v = int_value p.value in
    let sh = shift endian ~n base p in
    if sh = 0 then v else Fmt.str "(%s lsl %d)" v sh
  in
  Fmt.str "(%s)" (String.concat " lor " (List.map part ps))

(* One store of the [n]-byte word the parts [ws] make. On OxCaml a 4-byte word
   of ints is built untagged in an [int64_u], where the untagging of each value
   folds into the shift that computed it, and stored from there with no [int32]
   in between to normalise; elsewhere an int may hold only 31 bits. *)
let merged ~ox ~src ppf ~m ~n p ws endian =
  let endian = Option.value endian ~default:T.Big in
  let all_int =
    List.for_all
      (fun p -> match p.value with `Int _ | `Bits _ -> true | `I32 _ -> false)
      ws
  in
  if ox && src.unboxed && n = 4 && all_int then
    Fmt.pf ppf "    %s.set_u32%s buf (off + %d)@\n      %s;@\n" src.u
      (endian_suffix endian) p.at
      (word ~split:true ~src ~m:(i64 src) ~n endian p.at ws)
  else
    let w =
      if ox && n = 4 && all_int then
        Fmt.str "(%s.of_int %s)" (i32 src) (int_word ~n endian p.at ws)
      else word ~src ~m ~n endian p.at ws
    in
    Fmt.pf ppf "    %s.set_i%d%s buf (off + %d)@\n      %s;@\n" src.u (n * 8)
      (endian_suffix endian) p.at w

(* Adjacent stores merge into one of 8 bytes, or of 4, so a record takes a few
   wide stores instead of one per field. *)
let stores ?(ox = false) ~src ppf fields =
  let e = function Some e -> endian_suffix e | None -> "be" in
  let one p =
    match (p.size, p.value) with
    | 1, ((`Int _ | `Bits _) as v) ->
        Fmt.pf ppf "    %s.set_u8 buf (off + %d) %s;@\n" src.u p.at
          (int_value v)
    | 2, ((`Int _ | `Bits _) as v) ->
        Fmt.pf ppf "    %s.set_u16%s buf (off + %d) %s;@\n" src.u (e p.endian)
          p.at (int_value v)
    | _, (`Int _ | `Bits _) when ox && src.unboxed ->
        merged ~ox ~src ppf ~m:(i64 src) ~n:4 p [ p ] p.endian
    | _, ((`Int _ | `Bits _) as v) ->
        Fmt.pf ppf "    %s.set_i32%s buf (off + %d) (%s.of_int %s);@\n" src.u
          (e p.endian) p.at (i32 src) (int_value v)
    | _, `I32 v ->
        Fmt.pf ppf "    %s.set_i32%s buf (off + %d) %s;@\n" src.u (e p.endian)
          p.at v
  in
  let rec go = function
    | [] -> ()
    | p :: rest as ps -> (
        match take 8 ps with
        | Some (ws, endian, rest) ->
            merged ~ox ~src ppf ~m:(i64 src) ~n:8 p ws endian;
            go rest
        | None -> (
            match take 4 ps with
            | Some ((_ :: _ :: _ as ws), endian, rest) ->
                merged ~ox ~src ppf ~m:(i32 src) ~n:4 p ws endian;
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
   [in_range] says holds a value in range skips its range check. *)
let pp_stores ?ox ~src ppf ~plan ~in_range fields =
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
  Fmt.pf ppf "  if %a@\n  else begin@\n" (pp_frame ~src) "too_short";
  stores ?ox ~src ppf fields;
  if has_checks plan then Fmt.pf ppf "    validate_written buf off@\n"
  else Fmt.pf ppf "    ()@\n";
  Fmt.pf ppf "  end@\n@\n"

(* The int a field's typed value stores. *)
let plain_value ~src f =
  match (f.map, f.store) with
  | Some _, Int { width = 32; _ } when src.unboxed ->
      Fmt.str "(Int32_u.of_int32 (%s.enc %s))" (map_var f) f.id
  | Some _, _ -> Fmt.str "(%s.enc %s)" (map_var f) f.id
  | None, _ -> (
      match f.store with
      | Int { width = 32; signed; _ } when src.unboxed ->
          Fmt.str "(Int32_u.of_int (%s.%s.to_int %s))" rt
            (if signed then "SInt32" else "UInt32")
            f.id
      | Int { width = 32; signed; _ } ->
          Fmt.str "(%s.%s.to_int32 %s)" rt
            (if signed then "SInt32" else "UInt32")
            f.id
      | Int _ -> Fmt.str "(%s : %s :> int)" f.id (type_of f)
      | Bits _ | Bytes _ -> f.id)

(* [write] takes the values a record holds. An integer field's carrier holds
   it in range, so it is not checked again; a bitfield's [int] and a byte
   array's [string] are, all at once, which costs less than a check for each.
   A map's functions are the caller's code, so its value is checked, and a
   write through one makes no promise about allocation. *)
let in_range f =
  match (f.map, f.store) with
  | None, Int _ -> true
  | None, (Bits _ | Bytes _) | Some _, _ -> false

let pp_write ?(src = bytes_src) ppf ~target ~plan =
  let fields = all_fields plan in
  if
    List.exists
      (fun f ->
        ((not (in_range f)) && Option.is_some (out_of_range f))
        || Option.is_some f.enum)
      fields
  then pp_first_error ~src ppf ~target fields;
  let target =
    if List.exists (fun f -> Option.is_some f.map) fields then `OCaml
    else target
  in
  Fmt.pf ppf "let[@@inline]%s write %s ~off %a =@\n" (zero_alloc target) src.buf
    pp_labelled fields;
  List.iter
    (fun f -> Fmt.pf ppf "  let %s = %s in@\n" f.id (plain_value ~src f))
    fields;
  pp_stores ~ox:(target = `OxCaml) ~src ppf ~plan ~in_range fields

(* A module whose code holds a value only an [int] of 63 bits holds refuses to
   load on a target whose ints have 32 bits or fewer. *)
let pp_wide ppf ~codec ~wide ~uses =
  if wide then begin
    Fmt.pf ppf
      "let () =@\n\
      \  if Sys.int_size <= 32 then@\n\
      \    invalid_arg@\n\
      \      %S@\n\
       @\n"
      (Fmt.str
         "Wire_ocaml: the code generated from %s holds integers of more than \
          31 bits, which needs ints wider than 32 bits"
         codec);
    if uses "mask32" then
      Fmt.pf ppf "let mask32 = Int64.to_int 0xFFFF_FFFFL@\n@\n"
  end

(* The module links wire.runtime and no more, and raises each error itself. *)
let pp_header ppf ~codec ~name ~uses =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from the codec %s. Do not edit: generate it \
     again when the codec changes. *)@\n\
     @\n"
    codec;
  if uses "codec_name" || uses "no_error" then
    Fmt.pf ppf "let codec_name = %S@\n" name;
  (* [cast]'s parameters are variables, as the F* rendering's are: the
     witness is matched in its body. *)
  if uses "cast" then
    Fmt.pf ppf
      "let[@@inline] cast : type a b. (a, b) Type.eq -> b -> a = fun eq v ->@\n\
      \  match eq with Type.Equal -> v@\n";
  Fmt.pf ppf "@\n";
  if uses "no_error" then
    Fmt.pf ppf
      "let[@@inline never] no_error () =@\n\
      \  invalid_arg (codec_name ^ \": the generated code found no error to \
       report\")@\n\
       @\n"

(* [uses text name] is [true] iff the identifier [name] occurs in [text]. *)
let uses text name =
  let n = String.length name and len = String.length text in
  let ident c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec from i =
    match String.index_from_opt text i name.[0] with
    | None -> false
    | Some j when j + n > len -> false
    | Some j ->
        String.sub text j n = name
        && (j = 0 || not (ident text.[j - 1]))
        && (j + n = len || not (ident text.[j + n]))
        || from (j + 1)
  in
  n > 0 && from 0

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

(* The type of [f] in the record: its map's OCaml type for a field under a
   map, one of [maps]. *)
let record_type ~maps f =
  match f.map with Some path -> List.assoc path maps | None -> type_of f

let pp_record ~maps ppf fields =
  Fmt.pf ppf "type t = {@\n";
  List.iter
    (fun f -> Fmt.pf ppf "  %s : %s;@\n" f.label (record_type ~maps f))
    fields;
  Fmt.pf ppf "}@\n@\n"

(* [pp ppf r] prints [r] as an OCaml record, with [Format]'s primitives
   only: the module links no [Fmt]. A bitfield and a byte array print through
   [int] and [string], defined only when a field uses them. *)
let pp_printer ppf fields =
  let printer f =
    let base = Fmt.str "%s.%s.pp" rt in
    match (f.map, f.store) with
    | Some _, _ -> ("opaque", "")
    | None, store -> (
        match store with
        | Int { width = 8; signed = false; _ } -> (base "UInt8", "")
        | Int { width = 8; signed = true; _ } -> (base "SInt8", "")
        | Int { width = 16; signed = false; _ } -> (base "UInt16", "")
        | Int { width = 16; signed = true; _ } -> (base "SInt16", "")
        | Int { signed = false; _ } -> (base "UInt32", "")
        | Int { signed = true; _ } -> (base "SInt32", "")
        | Bits _ -> ("int", " :> int")
        | Bytes _ -> ("string", " :> string"))
  in
  let has p = List.exists (fun f -> fst (printer f) = p) fields in
  Fmt.pf ppf
    "let pp ppf (r : t) =@\n\
    \  let field name pp_v v =@\n\
    \    Format.pp_print_space ppf ();@\n\
    \    Format.pp_print_string ppf (name ^ \" = \");@\n\
    \    pp_v ppf v;@\n\
    \    Format.pp_print_char ppf ';'@\n\
    \  in@\n";
  if has "int" then Fmt.pf ppf "  let int = Format.pp_print_int in@\n";
  if has "string" then
    Fmt.pf ppf
      "  let string ppf s = Format.pp_print_string ppf (String.escaped s) in@\n";
  (* A map's value is the caller's type, which the module cannot print. *)
  if has "opaque" then
    Fmt.pf ppf "  let opaque ppf _ = Format.pp_print_string ppf \"_\" in@\n";
  Fmt.pf ppf
    "  Format.pp_open_hvbox ppf 2;@\n  Format.pp_print_char ppf '{';@\n";
  List.iter
    (fun f ->
      let pp_v, coerce = printer f in
      Fmt.pf ppf "  field %S %s (r.%s%s);@\n" f.label pp_v f.label coerce)
    fields;
  Fmt.pf ppf
    "  Format.pp_print_string ppf \" }\";@\n  Format.pp_close_box ppf ()@\n@\n"

let pp_encode ~src ppf fields =
  Fmt.pf ppf "let encode t %s ~off = write buf ~off %s@\n" src.buf
    (labelled fields (List.map (fun f -> "t." ^ f.label) fields))

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
      (* The mask takes the field from below bit 32: the word needs none. *)
      let word =
        if bytes = 4 then Fmt.str "(%s.to_int %s)" (i32 src) word else word
      in
      Fmt.str "((%s lsr %d) land %s)" word shift (mask width)
  | Bytes n ->
      Fmt.str "(%s.sub_string buf (off + %d) %d)" src.checked f.offset n

(* {2 Views}

   A module whose codec no walk covers, as one under a map, has the views of
   any other: [check] is [is_valid] answering a view, and an accessor reads
   its field with the load [decode] makes, through its map's [dec], which
   takes any value the check accepted. *)

let pp_view_check ~src ppf ~name =
  Fmt.pf ppf
    "type 'b view = int@\n\
     type 'b check = int@\n\
     @\n\
     let[@inline always] check (b : 'b Wire_runtime.Buf.t%s) (_ : 'b \
     Wire_runtime.Buf.length) ~off : 'b check =@\n\
    \  if is_valid (b : _ Wire_runtime.Buf.t :> bytes) off then off else -1@\n\
     @\n\
     let failed = Invalid_argument %S@\n\
     @\n\
     let[@inline always] is_ok (c : 'b check) = c >= 0@\n\
     @\n\
     let[@inline always] get (c : 'b check) : 'b view =@\n\
    \  if c >= 0 then c else raise failed@\n\
     @\n"
    (if src.unboxed then " @ local" else "")
    (name ^ ".get: the check failed")

(* The accessor of each field, named [accessor f]. *)
let pp_view_accessor ~src ppf ~accessor f =
  Fmt.pf ppf
    "let[@inline always] %s (b : 'b Wire_runtime.Buf.t%s) (v : 'b view) =@\n\
    \  let buf = (b : _ Wire_runtime.Buf.t :> bytes) in@\n\
    \  let off = v in@\n"
    (accessor f)
    (if src.unboxed then " @ local" else "");
  (match f.store with
  | Int { width = 32; _ } ->
      Fmt.pf ppf "  let %s_raw = %s in@\n" f.id (checked_read ~src f)
  | _ -> Fmt.pf ppf "  let %s = %s in@\n" f.id (checked_read ~src f));
  Fmt.pf ppf "  %s@\n@\n" (typed_value ~src f)

(* A record the encoder wrote that fails its own constraints. The check is
   [is_valid], which OxCaml's allocation checker follows, and only a record
   that fails it reaches [rejected], which raises what [fail] finds as an
   encoding error and so only raises. *)
let pp_validate_written ~src ppf ~target =
  Fmt.pf ppf
    "let%s rejected %s off =@\n\
    \  match fail buf off with@\n\
    \  | r -> r@\n\
    \  | exception exn when %s.is_error exn ->@\n\
    \      %s.encode_rejected ~name:codec_name (%s.error_of exn)@\n\
     @\n\
     let%s validate_written %s off =@\n\
    \  if not (is_valid buf off) then rejected buf off@\n\
     @\n"
    (cold_attr target) src.buf errors errors errors
    (match target with `OxCaml -> "[@inline][@zero_alloc]" | `OCaml -> "")
    src.buf

(* The bits of a 4-byte record a field occupies in a NEON lane, which holds the
   record's bytes in buffer order read as a little-endian word: [(shift,
   width)] when the field sits in one byte, unsigned, with no check, enum or
   map. Any other field keeps the record from having lanes. *)
let lane f =
  let in_byte ~byte ~bit ~width =
    if bit + width <= 8 then Some ((8 * byte) + bit, width) else None
  in
  match f with
  | { checks = _ :: _; _ } | { enum = Some _; _ } | { map = Some _; _ } -> None
  | { store = Int { width = 8; signed = false; _ }; offset; _ } ->
      Some (8 * offset, 8)
  | { store = Bits { base; shift; width }; offset; _ } -> (
      let n = Bitfield.byte_size base in
      match bit_endian base with
      | T.Big ->
          in_byte ~byte:(offset + n - 1 - (shift / 8)) ~bit:(shift mod 8) ~width
      | T.Little ->
          in_byte ~byte:(offset + (shift / 8)) ~bit:(shift mod 8) ~width)
  | _ -> None

let lanes plan ~size =
  let fields = all_fields plan in
  if size <> 4 || not (Sset.is_empty (live plan ~outputs:[])) then None
  else
    let ls = List.filter_map lane fields in
    if List.length ls = List.length fields then Some (List.combine fields ls)
    else None

(* [X4] reads and writes four records at once in a SIMD register (OxCaml on
   arm64 or amd64). [with_] checks the 16 bytes they take and loads them, and each
   [<field>] takes that field out of every lane. A [part] holds field bits at
   their place in each lane, every one in range: [<field>_bits] keeps the
   field's low bits of each lane, [<field>_of] checks they fit, [( ++ )] joins
   parts. Every field is unsigned with no check and the fields cover the
   record, so any part is four valid records, and [store] tests only that they
   fit. *)
(* A lane's value as [write] takes it: an 8-bit integer through its carrier's
   checked constructor, which raises for a value out of range. *)
let lane_value (f, _) =
  match f.store with
  | Int _ -> Fmt.str "~%s:(Wire_runtime.UInt8.v %s)" f.label f.id
  | Bits _ | Bytes _ -> Fmt.str "~%s:%s" f.label f.id

let pp_x4_sig ~src ppf fields =
  let v = "Wire_runtime.Vec.t" in
  Fmt.pf ppf
    "module X4 : sig@\n\
    \  type part@\n\
     @\n\
    \  val with_ :@\n\
    \    'b %s.t @@ local -> off:int -> ok:(%s -> 'a) @@ local ->@\n\
    \    fail:(unit -> 'a) @@ local -> 'a@\n\
     @\n"
    src.brand v;
  List.iter
    (fun (f, _) ->
      Fmt.pf ppf
        "  val %s : %s -> %s@\n\
        \  val %s_bits : %s -> part@\n\
        \  val %s_of : %s -> part@\n"
        f.label v v f.label v f.label v)
    fields;
  Fmt.pf ppf
    "  val zero : part@\n\
    \  val ( ++ ) : part -> part -> part@\n\
    \  val store : 'b %s.t @@ local -> off:int -> part -> unit@\n\
     @\n\
    \  val fill :@\n\
    \    'b %s.t @@ local -> off:int -> count:int -> f:(int -> part) @@ local \
     ->@\n\
    \    unit@\n\
     @\n\
    \  val write_int_at :@\n\
    \    'b %s.t @@ local -> off:int -> %s -> unit@\n\
     end = struct@\n\
    \  module V = Wire_runtime.Vec@\n\
     @\n\
    \  type part = V.t@\n\
     @\n\
    \  let[@inline] write_one ((b : 'b %s.t) @@ local) off %s =@\n\
    \    write (b : _ %s.t :> bytes) ~off %s@\n\
     @\n"
    src.brand src.brand src.brand
    (String.concat " -> " (List.map (fun _ -> v) fields))
    src.brand
    (String.concat " " (List.map (fun (f, _) -> f.id) fields))
    src.brand
    (String.concat " " (List.map lane_value fields))

let pp_x4_reads ~src ppf fields =
  Fmt.pf ppf
    "  let[@inline] with_ ((b : 'b %s.t) @@ local) ~off ~(ok @@ local)@\n\
    \      ~(fail @@ local) =@\n\
    \    let buf = (b : _ %s.t :> %s) in@\n\
    \    if off < 0 || off > Bytes.length buf - 16 then fail ()@\n\
    \    else ok (V.load buf off)@\n\
     @\n"
    src.brand src.brand src.typ;
  List.iter
    (fun (f, (shift, width)) ->
      let v =
        if shift = 0 then "w" else Fmt.str "(V.shift_right_logical %d w)" shift
      in
      if shift + width = 32 then
        Fmt.pf ppf "  let[@inline] %s w = %s@\n@\n" f.label v
      else
        Fmt.pf ppf "  let[@inline] %s w = V.logand %s (V.v %s)@\n@\n" f.label v
          (mask width))
    fields

(* The parts: a field's low bits moved to its place (no mask for the field at
   the top of the lane, whose higher bits the shift drops), the same after a
   range check that raises what [write_int_at] raises for the first lane out
   of range, and their union. *)
let pp_x4_parts ppf fields =
  Fmt.pf ppf
    "  let[@inline never] too_wide ~bits v =@\n\
    \    let rec first k =@\n\
    \      if k = 3 || V.get v k lsr bits <> 0 then V.get v k else first (k + \
     1)@\n\
    \    in@\n\
    \    Wire_runtime.Errors.unsigned_too_wide ~bits (first 0)@\n\
     @\n\
    \  let zero = V.v 0@\n\
    \  let[@inline] ( ++ ) a b = V.logor a b@\n\
     @\n";
  List.iter
    (fun (f, (shift, width)) ->
      (* Two shifts keep the low bits and place them, where a mask would need
         a constant built again at each call. *)
      let bits =
        if shift + width = 32 then Fmt.str "V.shift_left %d v" shift
        else
          Fmt.str "V.shift_right_logical %d (V.shift_left %d v)"
            (32 - width - shift)
            (32 - width)
      in
      Fmt.pf ppf
        "  let[@inline] %s_bits v = %s@\n\
         @\n\
        \  let[@inline] %s_of v =@\n\
        \    if V.is_all_zero (V.shift_right_logical %d v) then %s_bits v@\n\
        \    else too_wide ~bits:%d v@\n\
         @\n"
        f.label bits f.label width f.label width)
    fields

(* [store] writes four records with one store once they fit; if they do not,
   it writes them one at a time with [write], which writes the ones that fit
   and raises for the first that does not, as one record at a time does.
   [fill] checks the span of [count] registers once and stores two per turn,
   or falls back to [store] for each. *)
let pp_x4_store ~src ppf fields =
  let of_word =
    List.map
      (fun (_, (shift, width)) ->
        if shift + width = 32 then Fmt.str "(w lsr %d)" shift
        else Fmt.str "((w lsr %d) land %s)" shift (mask width))
      fields
  in
  Fmt.pf ppf
    "  let[@inline never] one_by_one ((b : 'b %s.t) @@ local) off p =@\n\
    \    for k = 0 to 3 do@\n\
    \      let w = V.get p k in@\n\
    \      write_one b (off + (4 * k)) %s@\n\
    \    done@\n\
     @\n\
    \  let[@inline] store ((b : 'b %s.t) @@ local) ~off p =@\n\
    \    let buf = (b : _ %s.t :> %s) in@\n\
    \    if off < 0 || off > Bytes.length buf - 16 then one_by_one b off p@\n\
    \    else V.store buf off p@\n\
     @\n\
    \  let[@inline] fill ((b : 'b %s.t) @@ local) ~off ~count ~(f @@ local) =@\n\
    \    let buf = (b : _ %s.t :> %s) in@\n\
    \    let n = Bytes.length buf in@\n\
    \    if off < 0 || off > n || count > (n - off) / 16 then@\n\
    \      for k = 0 to count - 1 do@\n\
    \        store b ~off:(off + (16 * k)) (f k)@\n\
    \      done@\n\
    \    else begin@\n\
    \      let k = ref 0 in@\n\
    \      while !k + 2 <= count do@\n\
    \        V.store buf (off + (16 * !k)) (f !k);@\n\
    \        V.store buf (off + (16 * (!k + 1))) (f (!k + 1));@\n\
    \        k := !k + 2@\n\
    \      done;@\n\
    \      if !k < count then V.store buf (off + (16 * !k)) (f !k)@\n\
    \    end@\n\
     @\n"
    src.brand
    (String.concat " " of_word)
    src.brand src.brand src.typ src.brand src.brand src.typ

(* [X4.write_int_at] takes plain lanes: one range test over the fields of each
   width, then [store]; lanes out of range are written one record at a time,
   which raises what [write] raises. *)
let pp_x4_write ~src ppf fields =
  let ids = List.map (fun (f, _) -> f.id) fields in
  let widths =
    List.sort_uniq compare (List.map (fun (_, (_, w)) -> w) fields)
  in
  let over =
    List.map
      (fun w ->
        let same =
          List.filter_map
            (fun (f, (_, w')) -> if w = w' then Some f.id else None)
            fields
        in
        Fmt.str "(V.shift_right_logical %d (%s))" w
          (String.concat " |> V.logor " same))
      widths
  in
  let word =
    List.map
      (fun (f, (shift, _)) ->
        if shift = 0 then f.id else Fmt.str "(V.shift_left %d %s)" shift f.id)
      fields
  in
  Fmt.pf ppf
    "  let[@inline never] lanes_one_by_one ((b : 'b %s.t) @@ local) off %s =@\n\
    \    for k = 0 to 3 do@\n\
    \      write_one b (off + (4 * k)) %s@\n\
    \    done@\n\
     @\n\
    \  let[@inline] write_int_at ((b : 'b %s.t) @@ local) ~off %s =@\n\
    \    let over = %s in@\n\
    \    if not (V.is_all_zero over) then lanes_one_by_one b off %s@\n\
    \    else store b ~off (%s)@\n\
     end@\n\
     @\n"
    src.brand (String.concat " " ids)
    (String.concat " " (List.map (Fmt.str "(V.get %s k)") ids))
    src.brand (String.concat " " ids)
    (String.concat "\n      |> V.logor " over)
    (String.concat " " ids)
    (String.concat "\n      |> V.logor " word)

let pp_x4 ~src ppf ~plan ~size =
  match lanes plan ~size with
  | None -> ()
  | Some fields ->
      pp_x4_sig ~src ppf fields;
      pp_x4_reads ~src ppf fields;
      pp_x4_parts ppf fields;
      pp_x4_store ~src ppf fields;
      pp_x4_write ~src ppf fields

(* The buffer a module reads, [bytes], held [local] on OxCaml. *)
let src_of = function `OxCaml -> local_bytes_src | `OCaml -> bytes_src

(* The checks, decoders and writes of a fixed record on [src], and with
   [views] its check and accessors. *)
let pp_codec ~src ?views ppf ~target ~plan ~size =
  pp_cold ~src ppf ~target ~plan ~size;
  pp_validate ~src ppf ~target ~plan ~size;
  if has_checks plan then pp_validate_written ~src ppf ~target;
  pp_decode ~src ppf ~plan ~size;
  pp_write ~src ppf ~target ~plan;
  Option.iter
    (fun (name, accessor) ->
      pp_view_check ~src ppf ~name;
      List.iter (pp_view_accessor ~src ppf ~accessor) (all_fields plan))
    views

let pp_body ?(simd = false) ?views ~maps ppf ~target ~plan ~size =
  let fields = all_fields plan in
  Fmt.pf ppf "let min_size = %d@\n@\n" size;
  pp_record ~maps ppf fields;
  pp_printer ppf fields;
  let src = src_of target in
  pp_codec ~src ?views ppf ~target ~plan ~size;
  if simd then pp_x4 ~src ppf ~plan ~size;
  pp_encode ~src ppf fields

let refuse ~codec reason =
  Fmt.invalid_arg "Wire_ocaml: cannot specialise %s: %s" codec reason

(* The names the generated module defines for itself. *)
let own_names =
  [
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
    "first_error";
    "error";
    "write";
    "check";
    "is_ok";
    "get";
    "failed";
    "encode";
    "min_size";
    "t";
    "Field";
    "Global";
    "mask32";
    "pp";
  ]

(* The accessor of each of [fields] in a module with no walk: its label,
   suffixed with ['_'] where it is a name the module defines. *)
let accessors ~reserved fields =
  let reserved = own_names @ reserved in
  List.map (fun f -> (f.label, Names.escape reserved f.label)) fields

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
            | T.Optional _ | T.Optional_or _ ->
                fail_unsupported "field %s: an optional field" name
            | T.Repeat _ -> fail_unsupported "field %s: a repeated field" name
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

let ident = label
