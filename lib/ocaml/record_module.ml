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
      | Bits _ | Bytes _ -> f.id)

(* The attributes the OxCaml target adds: an allocation check on a hot path,
   and on a cold one the promise that it only raises. *)
let zero_alloc = function `OxCaml -> "[@zero_alloc]" | `OCaml -> ""

let cold_attr = function
  | `OxCaml -> "[@inline never][@zero_alloc assume error]"
  | `OCaml -> "[@inline never]"

(* A decoder: the frame, the walk binding every field, then [call] on their
   typed values. *)
let pp_decode ?(src = bytes_src) ?(ox = false) ppf ~plan ~size ~head ~call =
  let fields = all_fields plan in
  Fmt.pf ppf "%s =@\n  if %a@\n  else@\n" head (pp_frame ~src) "fail";
  let m =
    hot ~ox ~raw32:true ~src
      ~live:(Sset.of_list (List.map (fun f -> f.id) fields))
      ~fail:"fail buf off" ()
  in
  pp_fixed ppf ~m ~plan ~size (fun () ->
      Fmt.pf ppf "%s@\n" (call (List.map typed_value fields)));
  Fmt.pf ppf "@\n"

(* [is_valid] answers with a boolean, loading only the fields a test reads, so
   a caller that drops a bad record on the floor pays for no exception handler;
   [validate] raises what the codec raises, found off the hot path by [fail]. *)
let pp_validate ~src ppf ~target ~plan ~size =
  let attr = "[@inline]" ^ zero_alloc target in
  Fmt.pf ppf
    "let%s is_valid buf off =@\n\
    \  if off < 0 || off > %s buf - size then false@\n\
    \  else@\n"
    attr src.len;
  let m =
    hot ~ox:(target = `OxCaml) ~src ~live:(live plan ~outputs:[]) ~fail:"false"
      ()
  in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "true@\n");
  Fmt.pf ppf
    "@\n\
     let%s validate buf off =@\n\
    \  if not (is_valid buf off) then fail buf off@\n\
     @\n"
    attr

let pp_cold ~src ppf ~target ~plan ~size =
  Fmt.pf ppf
    "let%s fail buf off =@\n\
    \  if off < 0 || off > %s buf - size then@\n\
    \    %s.frame ~size ~len:(%s buf) off;@\n"
    (cold_attr target) src.len errors src.len;
  let m = cold ~src ~live:(live ~cold:true plan ~outputs:[]) in
  pp_fixed ppf ~m ~plan ~size (fun () -> Fmt.pf ppf "no_error ()@\n");
  Fmt.pf ppf
    "@\n\
     let%s too_short buf off =@\n\
    \  %s.encode_too_short ~name:codec_name ~size ~len:(%s buf) off@\n\
     @\n"
    (cold_attr target) errors src.len

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

(* Writes take each value in its plain form: an [int] for a field up to 16 bits
   or a bitfield, an [int32] for a 32-bit field, a [string] for a byte array. *)

(* The bits that put a plain value out of its field's range, or-ed with the
   others' so one test, with no branch per field, finds any. *)
let out_of_range f =
  let x = f.id in
  match f.store with
  | Int { width = (8 | 16) as w; signed = false; _ } ->
      Fmt.kstr Option.some "(%s lsr %d)" x w
  | Int { width = (8 | 16) as w; signed = true; _ } ->
      Fmt.kstr Option.some "((%s + %d) lsr %d)" x (1 lsl (w - 1)) w
  | Int _ -> None
  | Bits { width; _ } -> Fmt.kstr Option.some "(%s lsr %d)" x width
  | Bytes n -> Fmt.kstr Option.some "(String.length %s lxor %d)" x n

let member f valid =
  Fmt.str "(%a)"
    Fmt.(
      list ~sep:(any " || ") (fun ppf v -> pf ppf "%s = %s" f.id (int_lit v)))
    valid

(* [first_error] raises what the codec raises for these values: converting each
   to its field's type, in field order; then the frame; then each bitfield's
   width, byte array's length and enum case, in field order. *)
let pp_first_error ~src ppf ~target fields =
  Fmt.pf ppf "let%s first_error buf off %a =@\n" (cold_attr target) pp_vars
    fields;
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
          Fmt.pf ppf
            "  if %s lsr %d <> 0 then %s.unsigned_too_wide ~bits:%d %s;@\n" f.id
            width errors width f.id
      | Bytes n ->
          Fmt.pf ppf
            "  if String.length %s <> %d then %s.byte_length ~expected:%d \
             ~actual:(String.length %s);@\n"
            f.id n errors n f.id
      | Int _ -> ());
      match f.enum with
      | Some (name, valid) ->
          Fmt.pf ppf
            "  if not %s then %s.enum_encode ~name:%S ~valid:[%a] %s;@\n"
            (member f valid) errors name
            Fmt.(list ~sep:(any "; ") (using int_lit string))
            valid f.id
      | None -> ())
    fields;
  Fmt.pf ppf "  no_error ()@\n@\n"

let store ~src ppf f =
  let o = f.offset in
  match f.store with
  | Int { width = 8; _ } ->
      Fmt.pf ppf "    %s.set_u8 buf (off + %d) %s;@\n" src.u o f.id
  | Int { width = 16; endian; _ } ->
      Fmt.pf ppf "    %s.set_u16%s buf (off + %d) %s;@\n" src.u
        (endian_suffix endian) o f.id
  | Int { endian; _ } ->
      Fmt.pf ppf "    %s.set_i32%s buf (off + %d) %s;@\n" src.u
        (endian_suffix endian) o f.id
  | Bits _ -> ()
  | Bytes n ->
      Fmt.pf ppf "    %s.unsafe_blit_string %s 0 buf (off + %d) %d;@\n"
        src.checked f.id o n

(* Each bitfield word, stored once: the values of its fields, each in range,
   shifted into place and or-ed. *)
let bit_words ~src ppf fields =
  let words =
    List.filter_map
      (fun f ->
        match f.store with
        | Bits { base; _ } -> Some (f.offset, base)
        | _ -> None)
      fields
    |> List.sort_uniq compare
  in
  List.iter
    (fun (o, base) ->
      let parts =
        List.filter_map
          (fun f ->
            match f.store with
            | Bits { shift; _ } when f.offset = o ->
                Fmt.kstr Option.some "(%s lsl %d)" f.id shift
            | _ -> None)
          fields
      in
      let value = String.concat " lor " parts in
      match base with
      | T.U8 -> Fmt.pf ppf "    %s.set_u8 buf (off + %d) (%s);@\n" src.u o value
      | T.U16 e ->
          Fmt.pf ppf "    %s.set_u16%s buf (off + %d) (%s);@\n" src.u
            (endian_suffix e) o value
      | T.U32 e ->
          Fmt.pf ppf "    %s.set_i32%s buf (off + %d) (Int32.of_int (%s));@\n"
            src.u (endian_suffix e) o value)
    words

let pp_write_int ?(src = bytes_src) ppf ~target ~plan =
  let fields = all_fields plan in
  pp_first_error ~src ppf ~target fields;
  Fmt.pf ppf "let[@@inline]%s write_int buf off %a =@\n" (zero_alloc target)
    pp_vars fields;
  let bad = List.filter_map out_of_range fields in
  let guard =
    (match bad with
      | [] -> []
      | _ -> [ Fmt.str "(%s) <> 0" (String.concat " lor " bad) ])
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
  bit_words ~src ppf fields;
  List.iter (store ~src ppf) fields;
  if has_checks plan then Fmt.pf ppf "    validate_written buf off@\n"
  else Fmt.pf ppf "    ()@\n";
  Fmt.pf ppf "  end@\n@\n"

(* [write] takes the codec's typed values, in range by construction, and
   stores them through [write_int]. *)
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
      | Bits _ | Bytes _ -> f.id)

(* A map's functions are the caller's code, so a write through one makes no
   promise about allocation. *)
let pp_write ppf ~target fields =
  let target =
    if List.exists (fun f -> Option.is_some f.map) fields then `OCaml
    else target
  in
  Fmt.pf ppf "let[@@inline]%s write buf off %a =@\n  write_int buf off %a@\n@\n"
    (zero_alloc target) pp_vars fields pp_list
    (List.map plain_value fields)

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
     [@@@@@@warning \"-32-26-27\"]@\n\
     @\n\
     let digest = %S@\n\
     let codec_name = %S@\n\
     let[@@inline] cast : type a. (a, int) Type.eq -> int -> a = fun \
     Type.Equal v -> v@\n\
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
  Fmt.pf ppf "}@\n@\nlet make %a = { %a }@\n@\n" pp_vars fields
    Fmt.(list ~sep:(any "; ") string)
    (List.map (fun f -> Fmt.str "%s = %s" f.label f.id) fields)

let pp_encode ppf fields =
  Fmt.pf ppf "let encode t buf off = write buf off %a@\n" pp_list
    (List.map (fun f -> "t." ^ f.label) fields)

(* [get_<field> buf off] reads one field as [Codec.get] does, with the same
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
  Fmt.pf ppf "let%s get_%s buf off =@\n" attr f.label;
  (match f.store with
  | Int { width = 32; _ } ->
      Fmt.pf ppf "  let %s_raw = %s in@\n  let %s = %s in@\n" f.id
        (checked_read ~src f) f.id (int_view f)
  | _ -> Fmt.pf ppf "  let %s = %s in@\n" f.id (checked_read ~src f));
  (match f.enum with
  | Some (_, valid) ->
      Fmt.pf ppf "  if not %s then get_failed (off + %d) %s [%a] else@\n"
        (member f valid) f.offset f.id
        Fmt.(list ~sep:(any "; ") (using int_lit string))
        valid
  | None -> ());
  Fmt.pf ppf "  %s@\n@\n" (typed_value f);
  match f.store with
  | Int { width = 32; _ } ->
      (* The [int] view of a 32-bit field, with no conversion through its
         typed form. *)
      Fmt.pf ppf
        "let%s get_%s_int buf off =@\n  let %s_raw = %s in@\n  %s@\n@\n" attr
        f.label f.id (checked_read ~src f) (int_view f)
  | _ -> ()

(* A bitfield's word, read once for all the fields packed in it:
   [get_<field>_word] is the checked load of the word that holds the field, and
   [<field>_of_word] takes the field out of it. *)
let pp_word_getters ~src ppf ~target f =
  match f.store with
  | Bits { base; shift; width } ->
      let attr =
        match target with
        | `OxCaml -> "[@inline][@zero_alloc]"
        | `OCaml -> "[@inline]"
      in
      let bytes = Bitfield.byte_size base in
      let word =
        checked_load ~src ~off:f.offset ~bytes ~endian:(bit_endian base)
      in
      let word =
        if bytes = 4 then Fmt.str "(Int32.to_int %s land mask32)" word else word
      in
      Fmt.pf ppf "let%s get_%s_word buf off = %s@\n@\n" attr f.label word;
      Fmt.pf ppf "let%s %s_of_word w = (w lsr %d) land %s@\n@\n" attr f.label
        shift (mask width)
  | _ -> ()

let pp_getters ?(src = bytes_src) ppf ~target fields =
  Fmt.pf ppf
    "let%s get_failed at value valid =@\n\
    \  %s.invalid_enum ~at ~field:[] ~value ~valid@\n\
     @\n"
    (cold_attr target) errors;
  List.iter (pp_getter ~src ppf ~target) fields;
  List.iter (pp_word_getters ~src ppf ~target) fields

(* One field at [off] read with no check of its own, in a record of [size]
   bytes already checked to fit. *)
let pp_read ~src ~ox ?size ppf f =
  let m = hot ~ox ~raw32:true ~src ~live:Sset.empty ~fail:"" () in
  match f.store with
  | Int { width = 32; signed; endian } ->
      Fmt.pf ppf
        "  let %s_raw = %s in@\n  let %s = (Int32.to_int %s_raw%s) in@\n" f.id
        (load m ~o:"off" ~k:f.offset ~bytes:4 ~endian)
        f.id f.id
        (if signed then "" else " land mask32")
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
      | Int { width = 32; _ } -> read ~suffix:"_int" ~result:f.id
      | _ -> ())
    fields

(* [View.at buf off] validates the record at [off] and stands for it, as
   [Valid] does at offset 0: each [view_<field>] is one load at a constant
   offset from [off] with no bounds check. A view that failed holds a buffer
   of no bytes, shared by every failed view since nothing can be written to
   it, and each read tests [ok] and gives zero without loading. On OxCaml a
   view is an unboxed record of the buffer and the offset, so it lives in
   registers and costs no allocation; on OCaml it is a small record. *)
let pp_view_module ~src ppf ~target =
  let attr =
    match target with
    | `OxCaml -> "[@inline][@zero_alloc]"
    | `OCaml -> "[@inline]"
  in
  let layout, hash =
    match target with
    | `OxCaml -> (" : value & value & value", "#")
    | `OCaml -> ("", "")
  in
  Fmt.pf ppf
    "module View : sig@\n\
    \  type t%s@\n\
     @\n\
    \  val at : %s -> int -> t@\n\
    \  val check : %s -> int -> t@\n\
    \  val ok : t -> bool@\n\
    \  val buf : t -> %s@\n\
    \  val off : t -> int@\n\
     end = struct@\n\
    \  type t = %s{ buf : %s; off : int; ok : bool }@\n\
     @\n\
    \  let empty = %s@\n\
     @\n\
    \  let%s at buf off =@\n\
    \    validate buf off;@\n\
    \    %s{ buf; off; ok = true }@\n\
     @\n\
    \  let%s check buf off =@\n\
    \    if is_valid buf off then %s{ buf; off; ok = true }@\n\
    \    else %s{ buf = empty; off = 0; ok = false }@\n\
     @\n\
    \  let%s ok (v : t) = v.%sok@\n\
    \  let%s buf (v : t) = v.%sbuf@\n\
    \  let%s off (v : t) = v.%soff@\n\
     end@\n\
     @\n"
    layout src.typ src.typ src.typ hash src.typ src.empty attr hash attr hash
    hash attr hash attr hash attr hash

(* What a read of a failed view binds in place of its loads. *)
let pp_zeros ppf f =
  match f.store with
  | Int { width = 32; _ } ->
      Fmt.pf ppf "  let %s_raw = 0l in@\n  let %s = 0 in@\n" f.id f.id
  | Bytes n -> Fmt.pf ppf "  let %s = String.make %d '\\000' in@\n" f.id n
  | Int _ | Bits _ -> Fmt.pf ppf "  let %s = 0 in@\n" f.id

let pp_view ?(src = bytes_src) ppf ~target ~plan ~size =
  let fields = all_fields plan in
  let ox = target = `OxCaml in
  pp_view_module ~src ppf ~target;
  List.iter
    (fun f ->
      let attr = read_attr target f in
      let view ~suffix ~result =
        Fmt.pf ppf
          "let%s view_%s%s (v : View.t) =@\n\
          \  let off = View.off v in@\n\
          \  if View.ok v then begin@\n\
          \  let buf = View.buf v in@\n"
          attr f.label suffix;
        pp_read ~src ~ox ~size ppf f;
        Fmt.pf ppf "  %s@\n  end@\n  else begin@\n" result;
        pp_zeros ppf f;
        Fmt.pf ppf "  %s@\n  end@\n@\n" result
      in
      view ~suffix:"" ~result:(typed_value f);
      match f.store with
      | Int { width = 32; _ } -> view ~suffix:"_int" ~result:f.id
      | Bits { base; _ } ->
          (* The word that holds the field, for [<field>_of_word]: fields
             packed in one word share one load. *)
          let m = hot ~ox ~src ~live:Sset.empty ~fail:"" () in
          Fmt.pf ppf
            "let%s view_%s_word (v : View.t) =@\n\
            \  if View.ok v then begin@\n\
            \    let buf = View.buf v and off = View.off v in@\n\
            \    %s@\n\
            \  end@\n\
            \  else 0@\n\
             @\n"
            attr f.label
            (unsigned m ~size ~o:"off" ~words:[] ~k:f.offset
               ~bytes:(Bitfield.byte_size base) ~endian:(bit_endian base) ())
      | _ -> ())
    fields

(* A record the encoder wrote that fails its own constraints. *)
let pp_validate_written ppf ~target =
  Fmt.pf ppf
    "let%s validate_written buf off =@\n\
    \  match validate buf off with@\n\
    \  | () -> ()@\n\
    \  | exception %s.Parse_error e -> %s.encode_rejected ~name:codec_name e@\n\
     @\n"
    (match target with
    | `OxCaml -> "[@inline never][@zero_alloc assume]"
    | `OCaml -> "[@inline never]")
    errors errors

(* [decode_local] builds the generated record in the caller's frame, so a
   decode of integer fields allocates nothing on the heap; a byte array is
   still a string on the heap, so with one it makes no such promise.
   [decode_with] takes the constructor as an argument, which flambda2 inlines
   with it. *)
let pp_oxcaml ppf ~record ~plan ~size =
  let fields = all_fields plan in
  pp_decode ~ox:true ppf ~plan ~size
    ~head:"let[@inline] decode_with make buf off" ~call:(fun args ->
      Fmt.str "make %a" pp_list args);
  let bytes =
    List.exists
      (fun f -> match f.store with Bytes _ -> true | _ -> false)
      fields
  in
  if record then
    pp_decode ~ox:true ppf ~plan ~size
      ~head:
        (Fmt.str "let%s decode_local buf off"
           (if bytes then "" else "[@zero_alloc]"))
      ~call:(fun args ->
        Fmt.str "exclave_ stack_ { %a }"
          Fmt.(list ~sep:(any "; ") string)
          (List.map2 (fun f a -> Fmt.str "%s = %s" f.label a) fields args))

(* The same reads and writes on a bigarray of bytes, with the same errors. *)
let pp_bigstring ppf ~target ~make ~record ~plan ~size =
  let src = bigstring_src in
  let fields = all_fields plan in
  Fmt.pf ppf "module Bigstring = struct@\n";
  pp_cold ~src ppf ~target ~plan ~size;
  pp_validate ~src ppf ~target ~plan ~size;
  pp_validate_written ppf ~target;
  pp_decode ~src ppf ~plan ~size ~head:"let decode buf off" ~call:(fun args ->
      Fmt.str "%s %a" make pp_list args);
  pp_write_int ~src ppf ~target ~plan;
  pp_write ppf ~target fields;
  pp_getters ~src ppf ~target fields;
  pp_valid ~src ppf ~target ~plan;
  pp_view ~src ppf ~target ~plan ~size;
  if record then pp_encode ppf fields;
  Fmt.pf ppf "end@\n@\n"

let pp_body ppf ~target ~make ~plan ~size =
  let fields = all_fields plan in
  Fmt.pf ppf "let size = %d@\n@\n" size;
  let record = Option.is_none make in
  if record then pp_record ppf fields;
  pp_cold ~src:bytes_src ppf ~target ~plan ~size;
  pp_validate ~src:bytes_src ppf ~target ~plan ~size;
  pp_validate_written ppf ~target;
  let make = Option.value ~default:"make" make in
  pp_decode ppf ~plan ~size ~head:"let decode buf off" ~call:(fun args ->
      Fmt.str "%s %a" make pp_list args);
  if target = `OxCaml then pp_oxcaml ppf ~record ~plan ~size;
  pp_write_int ppf ~target ~plan;
  pp_write ppf ~target fields;
  pp_getters ppf ~target fields;
  pp_valid ppf ~target ~plan;
  pp_view ppf ~target ~plan ~size;
  if record then pp_encode ppf fields;
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
    "decode";
    "decode_with";
    "decode_local";
    "first_error";
    "write_int";
    "write";
    "get_failed";
    "make";
    "encode";
    "t";
    "Valid";
    "View";
    "Bigstring";
    "mask32";
  ]

(* The names a field's functions and record field take. *)
let names_of f =
  let l = f.label in
  let derived =
    match f.store with
    | Int { width = 32; _ } ->
        [ "get_" ^ l ^ "_int"; "read_" ^ l ^ "_int"; "view_" ^ l ^ "_int" ]
    | Bits _ -> [ "get_" ^ l ^ "_word"; "view_" ^ l ^ "_word"; l ^ "_of_word" ]
    | Int _ | Bytes _ -> []
  in
  ("get_" ^ l) :: ("read_" ^ l) :: ("view_" ^ l) :: derived

(* Who defines a name: the generated module itself, or a field. *)
type source = Own | Field of string

(* A later definition would shadow an earlier one of the same name without a
   word, as the generated code silences unused values. Two fields of one label
   also define one getter, so this covers the record's fields too. *)
let check_names ~codec fields =
  let own = List.map (fun n -> (Own, n)) own_names in
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
