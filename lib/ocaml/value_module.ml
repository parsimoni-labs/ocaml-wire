open Wire.Private
open Layout
module T = Types
open Planner
module V = View_module

(* {1 The values of a codec}

   A codec the walk covers whose record holds more than named scalars at
   constant offsets has its value written out too: a record type per record
   of the codec, a decoder that checks the record with [check] and then reads
   each field through its accessor, an encoder and a size over the record's
   plan, and the function that finds the codec's error where [check] fails,
   walking the same plan and naming the first field that fails, as the codec
   does. Each record a field holds has its own, in its module. *)

let errors = Emitter.errors
let error = Emitter.error
let rt = Emitter.rt

(* Positions and sizes are [int]s, as the views hold them. *)
let mode = Record_module.cold ~src:Emitter.bytes_src ~live:Sset.empty
let text (r : V.record) e = Emitter.expr_text mode ~ranges:r.plan.ranges e

(* The field of [s] whose label is [label]. *)
let field_of (s : T.struct_) label =
  List.find_map
    (fun (T.Field f as fld) ->
      match f.field_name with
      | Some n when Names.ocaml_label n = label -> Some (n, fld)
      | _ -> None)
    s.fields

let label_of_presence (o : output) =
  let suffix = "_present" in
  let n = String.length o.name - String.length suffix in
  if n > 0 && String.ends_with ~suffix o.name then Some (String.sub o.name 0 n)
  else None

let store_type = function
  | Int { width = 8; signed; _ } ->
      Fmt.str "%s.%sInt8.t" rt (if signed then "S" else "U")
  | Int { width = 16; signed; _ } ->
      Fmt.str "%s.%sInt16.t" rt (if signed then "S" else "U")
  | Int { signed; _ } -> Fmt.str "%s.%sInt32.t" rt (if signed then "S" else "U")
  | Bits _ -> "int"
  | Bytes _ -> "string"

let sub_module (t : V.t) id =
  List.find_map
    (fun (r : V.record) -> if r.id = id then r.module_name else None)
    t.records
  |> Option.get

(* The OCaml type of a value of [typ], as the codec decodes it. *)
let rec typ_text : type a. V.t -> a T.typ -> string =
 fun t typ ->
  match typ with
  | T.Optional { inner; _ } -> typ_text t inner ^ " option"
  | T.Single_elem { elem; _ } -> typ_text t elem
  | T.Where { inner; _ } -> typ_text t inner
  | T.Enum { base; _ } -> typ_text t base
  | T.Codec { codec_id; _ } -> sub_module t codec_id ^ ".t"
  | T.Byte_slice _ -> "Bytesrw.Bytes.Slice.t"
  | T.Byte_array _ -> "string"
  | T.Unit -> "unit"
  | typ -> (
      match leaf typ with
      | Some s -> store_type s
      | None -> (
          match typ with
          | T.Bits _ -> "int"
          | _ -> fail_unsupported "its type, %a" T.pp_typ typ))

(* {2 The record and its decoder} *)

let named (s : T.struct_) =
  List.filter_map
    (fun (T.Field f as fld) ->
      Option.map (fun n -> (Names.ocaml_label n, fld)) f.field_name)
    s.fields

let pp_type ppf (t : V.t) (r : V.record) =
  match named r.struct_ with
  | [] -> Fmt.pf ppf "type t = unit@\n@\n"
  | fields ->
      Fmt.pf ppf "type t = {@\n";
      List.iter
        (fun (l, T.Field f) ->
          Fmt.pf ppf "  %s : %s;@\n" l (typ_text t f.field_typ))
        fields;
      Fmt.pf ppf "}@\n@\n"

let member (r : V.record) p = List.find_opt p r.members

let scalar r l =
  member r (function V.Scalar { field; _ } -> field.label = l | _ -> false)

let bytes r l = member r (function V.Bytes b -> b.label = l | _ -> false)

let sub r l =
  member r (function V.Sub { sub; _ } -> sub.path = Some [ l ] | _ -> false)

let present r l =
  member r (function
    | V.Present { presence; _ } -> label_of_presence presence = Some l
    | _ -> false)

let name m = V.member_name m

(* The expression of the value of the field [l] of type [typ] in the record
   at the view [v] of [b], through its accessors. *)
let rec value : type a. V.t -> V.record -> string -> a T.typ -> string =
 fun t r l typ ->
  let missing () = Fmt.invalid_arg "Value_module: no accessor for %s" l in
  match typ with
  | T.Optional { inner; _ } -> (
      match sub r l with
      | Some (V.Sub { name; sub; _ }) ->
          let m = sub_module t sub.codec_id in
          Fmt.str
            "(let c = %s b' v' in if %s.is_ok c then Some (%s.of_view b' \
             (%s.get c)) else None)"
            name m m m
      | _ -> (
          match present r l with
          | Some p ->
              Fmt.str "(if %s b' v' then Some %s else None)" (name p)
                (value t r l inner)
          | None -> missing ()))
  | T.Single_elem { elem; _ } -> value t r l elem
  | T.Where { inner; _ } -> value t r l inner
  | T.Enum { base; _ } -> value t r l base
  | T.Unit -> "()"
  | T.Codec { codec_id; _ } -> (
      match sub r l with
      | Some m ->
          let md = sub_module t codec_id in
          Fmt.str "(%s.of_view b' (%s.get (%s b' v')))" md md (name m)
      | None -> missing ())
  | T.Byte_slice _ | T.Byte_array _ -> (
      match bytes r l with
      | Some m -> Fmt.str "(%s b' v')" (name m)
      | None -> missing ())
  | _ -> (
      match scalar r l with
      | Some m -> Fmt.str "(%s b' v')" (name m)
      | None -> missing ())

(* The printer of a value of [typ], with [Format]'s primitives and the
   carriers' printers only. *)
let rec pp_text : type a. V.t -> a T.typ -> string =
 fun t typ ->
  match typ with
  | T.Optional { inner; _ } ->
      Fmt.str
        "(fun ppf -> function None -> Format.pp_print_string ppf \"None\" | \
         Some x -> Format.pp_print_string ppf \"Some (\"; %s ppf x; \
         Format.pp_print_char ppf ')')"
        (pp_text t inner)
  | T.Single_elem { elem; _ } -> pp_text t elem
  | T.Where { inner; _ } -> pp_text t inner
  | T.Enum { base; _ } -> pp_text t base
  | T.Codec { codec_id; _ } -> sub_module t codec_id ^ ".pp"
  | T.Byte_slice _ ->
      "(fun ppf s -> Format.pp_print_string ppf (String.escaped \
       (Bytesrw.Bytes.Slice.to_string s)))"
  | T.Byte_array _ ->
      "(fun ppf s -> Format.pp_print_string ppf (String.escaped s))"
  | T.Unit -> "(fun ppf () -> Format.pp_print_string ppf \"()\")"
  | typ -> (
      match leaf typ with
      | Some (Bits _) | None -> "Format.pp_print_int"
      | Some s ->
          let t = store_type s in
          String.sub t 0 (String.length t - 2) ^ ".pp")

(* [pp ppf r] prints [r] as an OCaml record, with [Format]'s primitives. *)
let pp_printer ppf t (r : V.record) =
  match named r.struct_ with
  | [] -> Fmt.pf ppf "let pp ppf () = Format.pp_print_string ppf \"()\"@\n@\n"
  | fields ->
      Fmt.pf ppf
        "let pp ppf (r : t) =@\n\
         let field name pp_v x =@\n\
         Format.pp_print_space ppf ();@\n\
         Format.pp_print_string ppf (name ^ \" = \");@\n\
         pp_v ppf x;@\n\
         Format.pp_print_char ppf ';'@\n\
         in@\n\
         Format.pp_open_hvbox ppf 2;@\n\
         Format.pp_print_char ppf '{';@\n";
      List.iter
        (fun (l, T.Field f) ->
          Fmt.pf ppf "field %S %s r.%s;@\n" l (pp_text t f.field_typ) l)
        fields;
      Fmt.pf ppf
        "Format.pp_print_string ppf \" }\";@\nFormat.pp_close_box ppf ()@\n@\n"

(* [of_view b v] is the record at the view [v], read through the
   accessors: only after a check that passed. *)
let pp_of_view ppf t (r : V.record) =
  let fields = named r.struct_ in
  let body =
    match fields with
    | [] -> "()"
    | fields ->
        Fmt.str "{ %s }"
          (String.concat "; "
             (List.map
                (fun (l, T.Field f) -> l ^ " = " ^ value t r l f.field_typ)
                fields))
  in
  (* Primed, the parameters are named apart from every accessor. *)
  let uses x = Record_module.uses body x in
  Fmt.pf ppf
    "let of_view (%s : 'b Wire_runtime.Buf.t) (%s : 'b view) : t =@\n%s@\n@\n"
    (if uses "b'" then "b'" else "_")
    (if uses "v'" then "v'" else "_")
    body

(* {2 Encoding}

   [encode_at r buf p] writes the record [r] at [p] and answers where it
   ends; [size r] is that many bytes. Each walks the record's plan with the
   plain value of every scalar bound first: an absent optional field is 0.
   A size, a presence or a where clause the values break is refused, as the
   codec's encoder refuses bytes its decoder would. *)

let rec plain : type a. store -> string -> a T.typ -> string =
 fun store e typ ->
  match typ with
  | T.Optional { inner; _ } ->
      Fmt.str "(match %s with Some x -> %s | None -> 0)" e
        (plain store "x" inner)
  | T.Single_elem { elem; _ } -> plain store e elem
  | T.Where { inner; _ } -> plain store e inner
  | T.Enum { base; _ } -> plain store e base
  | _ -> (
      match store with
      | Int { width = 8 | 16; _ } ->
          Fmt.str "(%s : %s :> int)" e (store_type store)
      | Int { signed; _ } ->
          Fmt.str "(%s.%sInt32.to_int %s)" rt (if signed then "S" else "U") e
      | Bits _ -> e
      | Bytes _ -> "0")

(* Each scalar of the plan, bound to its plain value. *)
let pp_bindings ppf (r : V.record) =
  List.iter
    (fun (f : field) ->
      match f.store with
      | Bytes _ -> ()
      | store -> (
          match field_of r.struct_ f.label with
          | Some (_, T.Field fd) when f.name <> "" ->
              Fmt.pf ppf "let %s = %s in@\n" f.id
                (plain store ("r." ^ f.label) fd.field_typ)
          | _ -> Fmt.pf ppf "let %s = 0 in@\n" f.id))
    (fields_of r.plan.nodes)

type enc = {
  t : V.t;
  r : V.record;
  fresh : unit -> string;
  defs : Buffer.t;
      (** The functions an optional field of the record is cut into. *)
  name : string;
}

let refuse_if cond what = Fmt.str "refuse_if (%s) %S" cond what

(* The bytes of a field of bytes, and its length. *)
let bytes_length slice x =
  if slice then Fmt.str "(Bytesrw.Bytes.Slice.length %s)" x
  else Fmt.str "(String.length %s)" x

let blit slice x ~at ~n =
  if slice then
    Fmt.str
      "Bytes.blit (Bytesrw.Bytes.Slice.bytes %s) (Bytesrw.Bytes.Slice.first \
       %s) buf %s %s;"
      x x at n
  else Fmt.str "Bytes.blit_string %s 0 buf %s %s;" x at n

(* The value of the field [l] in [env], through the region and refinement
   around it: an optional field binds its value where it is present. *)
let env_value env l = List.assoc_opt l env

let pp_word ppf ~p (o, base) bits =
  let v =
    String.concat " lor "
      (List.map (fun (id, sh) -> Fmt.str "(%s lsl %d)" id sh) bits)
  in
  match base with
  | T.U8 -> Fmt.pf ppf "Bytes.set_uint8 buf (%s + %d) (%s);@\n" p o v
  | T.U16 e ->
      Fmt.pf ppf "Bytes.set_uint16_%s buf (%s + %d) ((%s) land 0xffff);@\n"
        (Emitter.endian_suffix e) p o v
  | T.U32 e ->
      Fmt.pf ppf "Bytes.set_int32_%s buf (%s + %d) (Int32.of_int (%s));@\n"
        (Emitter.endian_suffix e) p o v

let pp_prefix ppf e env ~p fields n =
  begin
    Fmt.pf ppf "if write then begin@\n";
    let words = Hashtbl.create 4 in
    List.iter
      (fun (f : field) ->
        match f.store with
        | Int { width = 8; _ } ->
            Fmt.pf ppf "Bytes.set_uint8 buf (%s + %d) (%s land 0xff);@\n" p
              f.offset f.id
        | Int { width = 16; endian; _ } ->
            Fmt.pf ppf "Bytes.set_uint16_%s buf (%s + %d) (%s land 0xffff);@\n"
              (Emitter.endian_suffix endian)
              p f.offset f.id
        | Int { endian; _ } ->
            Fmt.pf ppf "Bytes.set_int32_%s buf (%s + %d) (Int32.of_int %s);@\n"
              (Emitter.endian_suffix endian)
              p f.offset f.id
        | Bits { base; shift; width } ->
            Fmt.pf ppf "%s.check_unsigned ~bits:%d %s;@\n" errors width f.id;
            let k = (f.offset, base) in
            Hashtbl.replace words k
              ((f.id, shift)
              :: Option.value ~default:[] (Hashtbl.find_opt words k))
        | Bytes k -> (
            match
              (f.name, env_value env f.label, V.slice_of e.r.struct_ f.label)
            with
            | n, Some x, Some slice when n <> "" ->
                Fmt.pf ppf
                  "if %s <> %d then %s.byte_length ~expected:%d ~actual:%s;@\n"
                  (bytes_length slice x) k errors k (bytes_length slice x);
                Fmt.pf ppf "%s@\n"
                  (blit slice x
                     ~at:(Fmt.str "(%s + %d)" p f.offset)
                     ~n:(string_of_int k))
            | _ ->
                Fmt.pf ppf "Bytes.fill buf (%s + %d) %d '\\000';@\n" p f.offset
                  k))
      fields;
    Hashtbl.iter (fun k bits -> pp_word ppf ~p k (List.rev bits)) words;
    Fmt.pf ppf "end;@\n"
  end;
  Fmt.str "(%s + %d)" p n

(* A span of bytes: a named field's, whose length must be the span's size,
   or an anonymous one, zeros. *)
let pp_span ppf e env ~p node size =
  let named =
    List.find_map
      (function
        | V.Bytes { label; at = Sized n; slice; _ } when n == node ->
            Option.map (fun x -> (x, slice)) (env_value env label)
        | _ -> None)
      e.r.members
  in
  let n = e.fresh () in
  Fmt.pf ppf "let %s = %s in@\n" n (text e.r size);
  Fmt.pf ppf "%s;@\n" (refuse_if (n ^ " < 0") "a negative size");
  (match named with
  | Some (x, slice) ->
      Fmt.pf ppf "%s;@\n"
        (refuse_if
           (Fmt.str "%s <> %s" (bytes_length slice x) n)
           "a field of bytes does not have its size");
      Fmt.pf ppf "if write then %s@\n" (blit slice x ~at:p ~n)
  | None -> Fmt.pf ppf "if write then Bytes.fill buf %s %s '\\000';@\n" p n);
  Fmt.str "(%s + %s)" p n

(* An optional field of the record's own is a function of its own, which
   binds the values it reads again, so each function of the encoder stays
   small. Its name has a prime, which no accessor's has. *)
let cut e ~p gate =
  let f = Fmt.str "%s'%s" e.name (e.fresh ()) in
  let b = Buffer.create 256 in
  let bppf = Format.formatter_of_buffer b in
  pp_bindings bppf e.r;
  Fmt.pf bppf "%s@\n" gate;
  Format.pp_print_flush bppf ();
  let body = Scan_module.without_dead_bindings (Buffer.contents b) in
  let param x = if Record_module.uses body x then x else "_" ^ x in
  Fmt.pf
    (Format.formatter_of_buffer e.defs)
    "let %s ~%s (r : t) %s %s =@\n%s@\n@\n%!" f
    (if Record_module.uses body "write" then "write" else "write:_")
    (param "buf") p body;
  Fmt.str "(%s ~write r buf %s)" f p

let rec pp_nodes ppf e env ~p nodes =
  List.fold_left
    (fun p node ->
      let q = e.fresh () in
      let next = pp_node ppf e env ~p node in
      Fmt.pf ppf "let %s = %s in@\n" q next;
      q)
    p nodes

and pp_node ppf e env ~p node =
  match node with
  | Prefix (fields, n) -> pp_prefix ppf e env ~p fields n
  | Test cond ->
      Fmt.pf ppf "%s;@\n"
        (refuse_if ("not " ^ text e.r cond) "the where clause does not hold");
      p
  | Span (T.Ref (T.I, var))
    when List.exists (fun (s : sub) -> s.var = var) e.r.subs -> (
      let s = List.find (fun (s : sub) -> s.var = var) e.r.subs in
      let m = sub_module e.t s.codec_id in
      match
        Option.bind s.path (function [ l ] -> env_value env l | _ -> None)
      with
      | Some x -> Fmt.str "(%s.encode_at ~write %s buf %s)" m x p
      | None -> invalid_arg "Value_module: a record of no field")
  | Span size -> pp_span ppf e env ~p node size
  | Gate (cond, presence, _, body) -> pp_gate e env ~p cond presence body
  | Region (size, at_most, body) ->
      let n = e.fresh () in
      Fmt.pf ppf "let %s = %s in@\n" n (text e.r size);
      Fmt.pf ppf "%s;@\n" (refuse_if (n ^ " < 0") "a negative size");
      let q = pp_nodes ppf e env ~p body in
      Fmt.pf ppf "%s;@\n"
        (refuse_if
           (Fmt.str "%s %s %s + %s" q (if at_most then ">" else "<>") p n)
           "a field does not fill its region");
      Fmt.pf ppf "if write then Bytes.fill buf %s (%s + %s - %s) '\\000';@\n" q
        p n q;
      Fmt.str "(%s + %s)" p n

(* An optional field: its value must be present exactly where its condition
   holds. *)
and pp_gate e env ~p cond presence body =
  let walk env =
    let b = Buffer.create 256 in
    let bppf = Format.formatter_of_buffer b in
    let q = pp_nodes bppf e env ~p body in
    Format.pp_print_flush bppf ();
    (Buffer.contents b, q)
  in
  match label_of_presence presence with
  | Some l when Option.is_some (env_value env l) ->
      let x = Option.get (env_value env l) in
      let inner = e.fresh () in
      let code, q = walk ((l, inner) :: env) in
      let gate =
        Fmt.str
          "(match %s with@\n| Some %s ->@\n%s;@\n%s%s@\n| None ->@\n%s;@\n%s)" x
          (if Record_module.uses code inner then inner else "_")
          (refuse_if
             ("not " ^ text e.r cond)
             (l ^ " is absent where its condition holds"))
          code q
          (refuse_if (text e.r cond)
             (l ^ " is present where its condition fails"))
          p
      in
      if x = "r." ^ l then cut e ~p gate else gate
  | _ ->
      let code, q = walk env in
      Fmt.str "(if %s then (%s%s) else %s)" (text e.r cond) code q p

let counter () =
  let n = ref 0 in
  fun () ->
    incr n;
    Fmt.str "q%d" !n

let env_of (r : V.record) =
  List.map (fun (l, _) -> (l, "r." ^ l)) (named r.struct_)

(* [encode_at ~write r buf p] walks [r] from [p] and answers where it ends,
   writing it with [write]: [size r] is the same walk with no write, so the
   two cannot disagree on where a field goes. *)
let pp_encode ppf t (r : V.record) =
  let b = Buffer.create 1024 in
  let bppf = Format.formatter_of_buffer b in
  pp_bindings bppf r;
  let defs = Buffer.create 256 in
  let e = { t; r; fresh = counter (); defs; name = "encode_at" } in
  let q = pp_nodes bppf e (env_of r) ~p:"p" r.plan.nodes in
  Fmt.pf bppf "%s@\n" q;
  Format.pp_print_flush bppf ();
  let body = Scan_module.without_dead_bindings (Buffer.contents b) in
  let param x = if Record_module.uses body x then x else "_" ^ x in
  Fmt.pf ppf "%slet encode_at ~%s %s %s p =@\n%s@\n@\n" (Buffer.contents defs)
    (if Record_module.uses body "write" then "write" else "write:_")
    (if Record_module.uses body "r" then "(r : t)" else "(_ : t)")
    (param "buf") body;
  Fmt.pf ppf "let size r = encode_at ~write:false r Bytes.empty 0@\n@\n"

(* {2 The codec's error}

   [error_at buf length p path] walks the record at [p] as the codec does and
   raises the first error it finds, as the codec names it: the record's
   fixed bytes, then each field in order, its bytes and then its checks, then
   the where clause. It answers where the record ends when it finds none. *)

(* The helpers the values' code raises through, one call each, those [uses]
   names. [fail] serves the others. *)
let helpers ~uses =
  let eof_if = uses "eof_if" and field_unless = uses "field_unless" in
  String.concat ""
    [
      (if uses "refuse_if" then
         "let[@inline never] refuse what = invalid_arg (codec_name ^ \": \" ^ \
          what)\n\n\
          let[@inline] refuse_if c what = if c then refuse what\n\n"
       else "");
      (if uses "fail_unless" || eof_if || field_unless then
         Fmt.str
           "let[@inline never] fail at field kind =\n\
            %s.fail { at; field; kind }\n\n"
           errors
       else "");
      (if uses "fail_unless" then
         "let[@inline] fail_unless c at field kind = if not c then fail at \
          field kind\n\n"
       else "");
      (if eof_if then
         Fmt.str
           "let[@inline] eof_if c at expected got field =@\n\
            if c then fail at field (%s.Unexpected_eof { expected; got })\n\n"
           error
       else "");
      (if field_unless then
         Fmt.str
           "let[@inline] field_unless c at field v =\n\
            if not c then@\n\
            fail at field (%s.Constraint_failed { which = %s.Field; value = \
            Some (Int64.of_int v) })\n\n"
           error error
       else "");
    ]

let path_text path name =
  if name = "" then path else Fmt.str "(%s @@ [ %S ])" path name

(* [eof ~at ~expected ~got ~field fmt ...] raises the codec's error for a
   short field where the condition [fmt ...] holds. *)
let eof ~at ~expected ~got ~field fmt =
  Fmt.kstr
    (fun cond -> Fmt.str "eof_if (%s) %s %s %s %s" cond at expected got field)
    fmt

let check_kind (f : field) (c : check) =
  match c.kind with
  | Constraint ->
      Fmt.str
        "%s.Constraint_failed { which = %s.Field; value = Some (Int64.of_int \
         %s) }"
        error error f.id
  | Enum valid ->
      Fmt.str "%s.Invalid_enum { value = %s; valid = [%s] }" error f.id
        (String.concat "; " (List.map int_lit valid))
  | Tag -> Fmt.str "%s.Invalid_tag %s" error f.id

let fail_check cond ~at ~field (f : field) (c : check) =
  match c.kind with
  | Constraint -> Fmt.str "field_unless %s %s %s %s" cond at field f.id
  | Enum _ | Tag ->
      Fmt.str "fail_unless %s %s %s (%s)" cond at field (check_kind f c)

let read (f : field) ~at =
  match f.store with
  | Int { width = 8; signed; _ } ->
      Fmt.str "Bytes.get_%s8 buf %s" (if signed then "int" else "uint") at
  | Int { width = 16; signed; endian } ->
      Fmt.str "Bytes.get_%s16_%s buf %s"
        (if signed then "int" else "uint")
        (Emitter.endian_suffix endian)
        at
  | Int { signed; endian; _ } ->
      let v =
        Fmt.str "Int32.to_int (Bytes.get_int32_%s buf %s)"
          (Emitter.endian_suffix endian)
          at
      in
      if signed then v else Fmt.str "(%s land %s.UInt32.mask32)" v rt
  | Bits { base; shift; width } ->
      let word =
        match base with
        | T.U8 -> Fmt.str "Bytes.get_uint8 buf %s" at
        | T.U16 e ->
            Fmt.str "Bytes.get_uint16_%s buf %s" (Emitter.endian_suffix e) at
        | T.U32 e ->
            Fmt.str
              "(Int32.to_int (Bytes.get_int32_%s buf %s) land %s.UInt32.mask32)"
              (Emitter.endian_suffix e) at rt
      in
      Fmt.str "((%s) lsr %d) land %s" word shift (mask width)
  | Bytes _ -> "0"

type err = {
  er : V.record;
  et : V.t;
  efresh : unit -> string;
  edefs : Buffer.t;  (** The functions its optional fields are cut into. *)
}

let name_in e label =
  match field_of e.er.struct_ label with Some (n, _) -> n | None -> ""

(* The label of the field a region holds. *)
let rec region_label e nodes =
  List.find_map
    (function
      | Prefix (f :: _, _) when f.name <> "" -> Some f.label
      | Span (T.Ref (T.I, var)) -> (
          match List.find_opt (fun (s : sub) -> s.var = var) e.er.subs with
          | Some { path = Some [ l ]; _ } -> Some l
          | _ -> None)
      | Span _ as node ->
          List.find_map
            (function
              | V.Bytes { label; at = Sized n; _ } when n == node -> Some label
              | _ -> None)
            e.er.members
      | Gate (_, _, _, body) | Region (_, _, body) -> region_label e body
      | Prefix _ | Test _ -> None)
    nodes

(* [check_pass ~all buf length p0 path] makes the checks of the fields at
   constant offsets of the record at [p0] whose bytes lie in the buffer, in
   order, those of the records and optional fields it holds at constant
   offsets included: the codec makes them before it checks the fixed bytes
   of the record it decodes ([all]), and of a record it enters at a variable
   offset those of the bitfields only. Each record's pass calls those of the
   records it holds, so the code is linear in the records. *)
(* The fields of [known] [ids] names, each at its offset from [p0], and the
   end of the last. *)
let read_all known ids =
  let fields = List.filter (fun ((g : field), _) -> List.mem g.id ids) known in
  let stop =
    List.fold_left
      (fun a ((g : field), at) -> max a (at + width_bytes g.store))
      0 fields
  in
  (stop, fields)

let bind ppf (fields : (field * int) list) =
  List.iter
    (fun ((g : field), at) ->
      Fmt.pf ppf "let %s = %s in@\n" g.id (read g ~at:(Fmt.str "(p0 + %d)" at)))
    fields

(* The checks of [f] at [at], made where its bytes and those its checks read
   lie in the buffer; of a bitfield always, of any other field with [all]. *)
let pp_pass_field ppf e known ~at (f : field) =
  if f.checks <> [] then begin
    let refs =
      f.id
      :: List.concat_map (fun (c : check) -> T.collect_refs c.cond) f.checks
    in
    let stop, bound = read_all known refs in
    let bits = match f.store with Bits _ -> true | _ -> false in
    Fmt.pf ppf "if %sp0 + %d <= length then begin@\n"
      (if bits then "" else "all && ")
      stop;
    bind ppf bound;
    List.iter
      (fun (c : check) ->
        Fmt.pf ppf "%s;@\n"
          (fail_check (text e.er c.cond)
             ~at:(Fmt.str "(p0 + %d)" (at + f.offset))
             ~field:(path_text "path" f.name) f c))
      f.checks;
    Fmt.pf ppf "end;@\n"
  end

let pp_check_pass ppf e =
  let rec go known at = function
    | Prefix (fields, n) :: rest ->
        let known =
          known @ List.map (fun (f : field) -> (f, at + f.offset)) fields
        in
        List.iter (pp_pass_field ppf e known ~at) fields;
        go known (at + n) rest
    | Span (T.Ref (T.I, var)) :: rest
      when List.exists (fun (s : sub) -> s.var = var) e.er.subs -> (
        let s = List.find (fun (s : sub) -> s.var = var) e.er.subs in
        let name = match s.path with Some [ l ] -> name_in e l | _ -> "" in
        Fmt.pf ppf "%s.check_pass ~all buf length (p0 + %d) %s;@\n"
          (sub_module e.et s.codec_id)
          at (path_text "path" name);
        (* After a record of variable size, no offset is constant. *)
        match s.fixed_size with
        | Some k -> go known (at + k) rest
        | None -> ())
    | Gate (cond, _, _, body) :: _ ->
        (* An optional field at a constant offset is checked where its
           condition holds; what follows it is at no constant offset. *)
        let stop, bound = read_all known (T.collect_refs cond) in
        Fmt.pf ppf "if p0 + %d <= length then begin@\n" stop;
        bind ppf bound;
        Fmt.pf ppf "if %s then begin@\n" (text e.er cond);
        go known at body;
        Fmt.pf ppf "end@\nend;@\n"
    | Test _ :: rest -> go known at rest
    | _ -> ()
  in
  go [] 0 e.er.plan.nodes

(* A field of a prefix: its bytes, then its checks. The codec names a field
   it finds short only when it checks it, and inside a region the region. *)
let pp_error_field ?short ppf e ~p ~path (f : field) =
  let at = Fmt.str "(%s + %d)" p f.offset in
  let w = width_bytes f.store in
  let field = path_text path f.name in
  Fmt.pf ppf "%s;@\n"
    (eof ~at ~expected:(string_of_int w)
       ~got:(Fmt.str "(length - %s)" at)
       ~field:
         (match short with
         | Some region -> region
         | None -> if f.checks = [] then path else field)
       "%s > length - %d" at w);
  match f.store with
  | Bytes _ -> ()
  | _ ->
      Fmt.pf ppf "let %s = %s in@\n" f.id (read f ~at);
      List.iter
        (fun (c : check) ->
          Fmt.pf ppf "%s;@\n" (fail_check (text e.er c.cond) ~at ~field f c))
        f.checks

(* An optional field is a function of its own, which takes the position it
   starts at and the values of the fields before it its walk reads, so each
   function of the walk stays small. Its name has a prime, which no
   accessor's has. *)
let cut_error e ~p code =
  let code = Scan_module.without_dead_bindings code in
  let f = Fmt.str "error_at'%s" (e.efresh ()) in
  let binds id = Record_module.uses code ("let " ^ id) in
  let free =
    List.filter_map
      (fun (fd : field) ->
        if Record_module.uses code fd.id && not (binds fd.id) then Some fd.id
        else None)
      (fields_of e.er.plan.nodes)
  in
  let param x = if Record_module.uses code x then x else "_" ^ x in
  Fmt.pf
    (Format.formatter_of_buffer e.edefs)
    "let[@inline never] %s %s %s %s %s %s%s =@\n%s@\n@\n%!" f (param "buf")
    (param "length") (param "p0") (param "path") p
    (String.concat "" (List.map (fun id -> " " ^ id) free))
    code;
  Fmt.str "(%s buf length p0 path %s%s)" f p
    (String.concat "" (List.map (fun id -> " " ^ id) free))

let rec pp_errors ?short ppf e ~p ~path nodes =
  List.fold_left
    (fun p node ->
      let q = e.efresh () in
      let next = pp_error ?short ppf e ~p ~path node in
      Fmt.pf ppf "let %s = %s in@\n" q next;
      q)
    p nodes

and pp_error ?short ppf e ~p ~path node =
  match node with
  | Prefix (fields, n) ->
      List.iter (pp_error_field ?short ppf e ~p ~path) fields;
      Fmt.str "(%s + %d)" p n
  | Test cond ->
      Fmt.pf ppf
        "fail_unless %s p0 %s (%s.Constraint_failed { which = %s.Where; value \
         = None });@\n"
        (text e.er cond) path error error;
      p
  | Span (T.Ref (T.I, var))
    when List.exists (fun (s : sub) -> s.var = var) e.er.subs ->
      let s = List.find (fun (s : sub) -> s.var = var) e.er.subs in
      let m = sub_module e.et s.codec_id in
      let name = match s.path with Some [ l ] -> name_in e l | _ -> "" in
      Fmt.str "(%s.error_at buf length %s %s)" m p (path_text path name)
  | Span size ->
      (* The codec names the record that holds a short span, not the span. *)
      let n = e.efresh () in
      Fmt.pf ppf "let %s = %s in@\n" n (text e.er size);
      Fmt.pf ppf "%s;@\n"
        (eof ~at:p ~expected:(Fmt.str "(max 0 %s)" n)
           ~got:(Fmt.str "(length - %s)" p)
           ~field:path "%s < 0 || %s > length - %s" n n p);
      Fmt.str "(%s + %s)" p n
  | Gate (cond, _, _, body) ->
      let b = Buffer.create 256 in
      let bppf = Format.formatter_of_buffer b in
      let q = pp_errors bppf e ~p ~path body in
      Format.pp_print_flush bppf ();
      cut_error e ~p
        (Fmt.str "(if %s then (%s%s) else %s)" (text e.er cond)
           (Buffer.contents b) q p)
  | Region (size, at_most, body) ->
      pp_error_region ppf e ~p ~path size at_most body

(* A region is measured to fit first, the error naming it, or nothing for a
   region of one scalar, which is then measured against the region before it
   is read; its contents are then walked in its window, a field too short
   for it naming the region, a record it holds naming its own fields; and
   they must fill it, or fit in it. *)
and pp_error_region ppf e ~p ~path size at_most body =
  let holds_record =
    match body with
    | [ Span (T.Ref (T.I, var)) ] ->
        List.exists (fun (s : sub) -> s.var = var) e.er.subs
    | _ -> false
  in
  let scalar =
    match body with
    | [ Prefix ([ { store = Int _ | Bits _; _ } ], w) ] -> Some w
    | _ -> None
  in
  let n = e.efresh () in
  let region =
    path_text path
      (Option.fold ~none:"" ~some:(name_in e) (region_label e body))
  in
  Fmt.pf ppf "let %s = %s in@\n" n (text e.er size);
  Fmt.pf ppf "%s;@\n"
    (eof ~at:p ~expected:(Fmt.str "(max 0 %s)" n)
       ~got:(Fmt.str "(length - %s)" p)
       ~field:(if Option.is_some scalar then path else region)
       "%s < 0 || %s > length - %s" n n p);
  let fill used =
    eof
      ~at:(Fmt.str "(%s + min %s %s)" p used n)
      ~expected:n ~got:used ~field:path "%s > %s%s" used n
      (if at_most then "" else Fmt.str " || %s <> %s" used n)
  in
  Option.iter (fun w -> Fmt.pf ppf "%s;@\n" (fill (string_of_int w))) scalar;
  let window = e.efresh () and used = e.efresh () in
  Fmt.pf ppf "let %s = min length (%s + %s) in@\n" window p n;
  Fmt.pf ppf "let %s =@\nlet length = %s in@\n" used window;
  let short = if holds_record then None else Some region in
  let q = pp_errors ?short ppf e ~p ~path body in
  Fmt.pf ppf "%s - %s@\nin@\n" q p;
  Fmt.pf ppf "%s;@\n" (fill used);
  Fmt.str "(%s + %s)" p n

let min_size (r : V.record) =
  Wire.Private.struct_min_size (Wire.Private.validator_of_struct r.struct_)

let pp_error_at ppf t (r : V.record) =
  let b = Buffer.create 1024 in
  let bppf = Format.formatter_of_buffer b in
  let e = { er = r; et = t; efresh = counter (); edefs = Buffer.create 256 } in
  let q = pp_errors bppf e ~p:"p0" ~path:"path" r.plan.nodes in
  Format.pp_print_flush bppf ();
  let pass =
    let b = Buffer.create 256 in
    let bppf = Format.formatter_of_buffer b in
    pp_check_pass bppf e;
    Format.pp_print_flush bppf ();
    Buffer.contents b
  in
  let param x = if Record_module.uses pass x then x else "_" ^ x in
  Fmt.pf ppf "let check_pass ~%s %s %s %s %s =@\n%s()@\n@\n"
    (if Record_module.uses pass "all" then "all" else "all:_all")
    (param "buf") (param "length") (param "p0") (param "path") pass;
  let body = Scan_module.without_dead_bindings (Buffer.contents b ^ q) in
  Fmt.pf ppf "%s" (Buffer.contents e.edefs);
  Fmt.pf ppf
    "let[@inline never] error_at %s length p0 path =@\n\
     check_pass ~all:%b buf length p0 path;@\n\
     %s;@\n\
     %s@\n\
     @\n"
    "buf" (r.module_name = None)
    (eof ~at:"p0"
       ~expected:(string_of_int (min_size r))
       ~got:"(max 0 (length - p0))" ~field:"path" "p0 > length - %d"
       (min_size r))
    body

(* {2 The members} *)

(* The members of the record of a sub-record's module. *)
let ml_record ppf t r =
  pp_type ppf t r;
  pp_printer ppf t r;
  pp_of_view ppf t r;
  pp_encode ppf t r;
  pp_error_at ppf t r

(* The check over the bytes from [off] to [stop], or over the record's own
   bytes for a codec of constant size. *)
let root_check t ~stop =
  match t.V.size with
  | Some _ -> "check b (Wire_runtime.Buf.length b) ~off"
  | None ->
      Fmt.str "check b (Wire_runtime.Buf.length b) ~off ~len:(%s - off)" stop

(* [encode], which checks what it wrote: to its end, or over the record's
   own bytes for a codec of constant size, whose end is not bound. *)
let pp_root_encode ppf t =
  Fmt.pf ppf
    "let encode (r : t) buf ~off =@\n\
     if off < 0 then %s.negative_offset off;@\n\
     %s@\n\
     match Wire_runtime.Buf.v buf with@\n\
     | Wire_runtime.Buf.Any b ->@\n\
     if not (is_ok (%s)) then@\n\
     invalid_arg (codec_name ^ \": the record does not pass its own checks\")@\n\
     @\n"
    errors
    (if Option.is_some t.V.size then
       "ignore (encode_at ~write:true r buf off : int);"
     else "let stop = encode_at ~write:true r buf off in")
    (root_check t ~stop:"stop")

(* The codec's own: its record, then [decode], [validate] and [encode] over
   a buffer, from its offset to its end. *)
let ml_root ppf t (r : V.record) =
  ml_record ppf t r;
  Fmt.pf ppf
    "let min_size = %d@\n\
     @\n\
     let[@inline never] first_error buf off =@\n\
     match error_at buf (Bytes.length buf) off [] with@\n\
     | _ -> no_error ()@\n\
     | exception exn when %s.is_error exn -> Error (%s.error_of exn)@\n\
     @\n\
     let is_valid buf off =@\n\
     off >= 0 &&@\n\
     match Wire_runtime.Buf.v buf with@\n\
     | Wire_runtime.Buf.Any b -> is_ok (%s)@\n\
     @\n\
     let decode buf ~off =@\n\
     if off < 0 then %s.negative_offset off;@\n\
     match Wire_runtime.Buf.v buf with@\n\
     | Wire_runtime.Buf.Any b ->@\n\
     let c = %s in@\n\
     if is_ok c then Ok (of_view b (get c)) else first_error buf off@\n\
     @\n\
     let validate buf ~off =@\n\
     if off < 0 then %s.negative_offset off;@\n\
     if is_valid buf off then Ok () else first_error buf off@\n\
     @\n"
    (min_size r) errors errors
    (root_check t ~stop:"Bytes.length buf")
    errors
    (root_check t ~stop:"Bytes.length buf")
    errors;
  pp_root_encode ppf t

(* {1 The interface} *)

(* The values read slices through Bytesrw, which states no portability: on
   OxCaml they are stated nonportable. *)
let pp_val ~target:_ ppf name typ doc =
  Fmt.pf ppf "val %s : %s@\n(** %s *)@\n@\n" name typ doc

let mli_type ppf t (r : V.record) =
  (match named r.struct_ with
  | [] -> Fmt.pf ppf "type t = unit@\n"
  | fields ->
      Fmt.pf ppf "type t = {@\n";
      List.iter
        (fun (l, T.Field f) ->
          Fmt.pf ppf "  %s : %s;@\n" l (typ_text t f.field_typ))
        fields;
      Fmt.pf ppf "}@\n");
  Fmt.pf ppf
    "(** The type for the records of [%s]: an optional field is an option, a \
     field of bytes a slice of the buffer it was decoded from or a string, a \
     record a field holds its module's. *)@\n\
     @\n"
    r.codec_name

let mli_record ppf ~target t r =
  mli_type ppf t r;
  pp_val ~target ppf "pp" "Format.formatter -> t -> unit"
    "[pp ppf r] prints an unspecified representation of [r] on [ppf].";
  pp_val ~target ppf "size" "t -> int"
    "[size r] is the number of bytes {!val-encode} writes for [r]."

let parse_error = rt ^ ".Error.t"

let mli_root ppf ~target t (r : V.record) =
  mli_record ppf ~target t r;
  pp_val ~target ppf "min_size" "int"
    (Fmt.str "[min_size] is the fewest bytes a record takes, [%d]." (min_size r));
  pp_val ~target ppf "validate"
    (Fmt.str "bytes -> off:int -> (unit, %s) result" parse_error)
    "[validate buf ~off] is [Ok ()] if the record at [off] in [buf] passes the \
     codec's checks, and the codec's error otherwise. Raises \
     [Invalid_argument] if [off] is negative.";
  pp_val ~target ppf "decode"
    (Fmt.str "bytes -> off:int -> (t, %s) result" parse_error)
    "[decode buf ~off] is the record at [off] in [buf], checked to the end of \
     [buf] as {!val-check} checks it, or the codec's error. Raises \
     [Invalid_argument] if [off] is negative.";
  pp_val ~target ppf "encode" "t -> bytes -> off:int -> unit"
    "[encode r buf ~off] writes [r] at [off] in [buf]. Raises \
     [Invalid_argument] if [r] does not fit, or if its values break a size, a \
     presence or a check of the record, as the codec's encoder does; the bytes \
     it wrote are then scrap."
