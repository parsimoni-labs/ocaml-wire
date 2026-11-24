open Wire.Private
module T = Types
open Planner
open Emitter

(* {1 The F* modules of a codec}

   [<M>.Scan] is the scanner's [fold] as the emitter writes it in F*.
   [<M>.Flat] reads the record field by field, as the 3D projection of the
   codec lays it out, and proves that reading equal to the parser 3d.exe
   writes for the projection, [<M>.parser____<S>]: it depends on the schema
   alone. [<M>.Proof] proves [fold] equal to that parser on the bytes of its
   window, through the reading. *)

(* {2 The record as 3D lays it out} *)

(* A field of the 3D struct, or a word of bitfields 3D packs into one: its
   variable in the reading, its size in bytes, its refinements and the
   scalars it holds, each as a name and the F* text of its value from the
   variable. A byte array has no variable. *)
type elem = {
  var : string option;
  size : int T.expr;
  conds : bool T.expr list;
  scalars : (string * (string -> string)) list;
  carrier : string;
}

(* An item of a 3D struct: a word or a byte array; a nested codec, a struct
   of its own; an optional codec, a casetype on its condition whose case
   holds the codec or the region it lies in; a region holding a codec; a
   presence, which 3D folds into the size of the bytes after it. *)
type item =
  | Word of elem
  | Sub of { name : string; fields : T.field list }
  | Opt of { name : string; present : bool T.expr; body : body }
  | Region of region
  | Flag of { name : string; cond : bool T.expr }

and body = Codec_body of T.field list | Region_body of region

and region = {
  size : int T.expr;
  at_most : bool;
  fields : T.field list;
  where : bool T.expr option;
}

let ident name =
  "f_"
  ^ String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c | _ -> '_')
      name

let base_bytes = function T.U8 -> 1 | T.U16 _ -> 2 | T.U32 _ -> 4

let base_carrier = function
  | T.U8 -> "P.of_u8"
  | T.U16 _ -> "P.of_u16"
  | T.U32 _ -> "P.of_u32"

let big_endian = function
  | T.U8 | T.U16 T.Big | T.U32 T.Big -> ()
  | T.U16 T.Little | T.U32 T.Little ->
      fail_unsupported "the F* reading of a little-endian bitfield"

(* A field's own type under the wrappers that do not change its bytes, and
   the refinement a [Where] adds. *)
let rec unwrap : type a. a T.typ -> T.packed_typ * bool T.expr list = function
  | T.Map { inner; _ } -> unwrap inner
  | T.Where { cond; inner } ->
      let t, conds = unwrap inner in
      (t, cond :: conds)
  | t -> (T.Pack_typ t, [])

let scalar (T.Pack_typ t) =
  match t with
  | T.Uint8 -> Some (1, "P.of_u8")
  | T.Uint16 T.Big -> Some (2, "P.of_u16")
  | T.Uint32 T.Big -> Some (4, "P.of_u32")
  | _ -> None

let codec_fields : type a. a T.typ -> T.field list option = function
  | T.Codec { codec_struct; _ } ->
      if codec_struct.params <> [] || Option.is_some codec_struct.where then
        fail_unsupported "the F* reading of a parameterised codec";
      Some codec_struct.fields
  | _ -> None

(* The region of [size] bytes holding a codec; one whose size is a
   difference states that the difference is not negative, as the wrapper
   struct the projection names it in does. *)
let region : type a. a T.typ -> region option = function
  | T.Single_elem { size; elem; at_most } -> (
      let T.Pack_typ elem, _ = unwrap elem in
      match codec_fields elem with
      | Some fields ->
          let where =
            match size with T.Sub (a, b) -> Some (T.Ge (a, b)) | _ -> None
          in
          Some { size; at_most; fields; where }
      | None ->
          fail_unsupported "the F* reading of a region of other than a codec")
  | _ -> None

(* The items of the struct's [fields]: consecutive bitfields of one base
   and bit order that fit its word share one, the first declared at the most
   significant bits for [Msb_first]. *)
let rec layout group fields =
  match (fields, group) with
  | [], None -> []
  | [], Some _ -> close group []
  | (T.Field f :: rest as fields), _ -> (
      let typ, wheres = unwrap f.field_typ in
      let conds = wheres @ Option.to_list f.constraint_ in
      let named = Option.map ident f.field_name in
      let name () =
        match f.field_name with
        | Some n -> n
        | None -> fail_unsupported "the F* reading of an anonymous field"
      in
      match typ with
      | T.Pack_typ (T.Bits { width; base; bit_order }) -> (
          big_endian base;
          let total = 8 * base_bytes base in
          match group with
          | Some (b, o, used, members, gconds)
            when T.equal_bitfield_base b base
                 && T.equal_bit_order o bit_order
                 && used + width <= total ->
              let member = (f.field_name, used, width) in
              layout
                (Some (b, o, used + width, member :: members, gconds @ conds))
                rest
          | Some _ -> close group fields
          | None ->
              layout
                (Some
                   (base, bit_order, width, [ (f.field_name, 0, width) ], conds))
                rest)
      | _ when Option.is_some group -> close group fields
      | _ when conds <> [] && Option.is_none (scalar typ) ->
          fail_unsupported "the F* reading of a refined composite field"
      | T.Pack_typ (T.Byte_array { size } | T.Byte_slice { size }) ->
          Word { var = None; size; conds = []; scalars = []; carrier = "" }
          :: layout None rest
      | T.Pack_typ (T.Codec _ as t) ->
          let fields = Option.get (codec_fields t) in
          Sub { name = name (); fields } :: layout None rest
      | T.Pack_typ (T.Single_elem _ as t) ->
          Region (Option.get (region t)) :: layout None rest
      | T.Pack_typ (T.Optional { present; inner }) -> (
          let name = name () in
          let T.Pack_typ inner, _ = unwrap inner in
          match (inner, codec_fields inner, region inner) with
          | T.Codec { codec_fixed_size = Some _; _ }, _, _ ->
              fail_unsupported
                "the F* reading of an optional codec of a fixed size, which 3D \
                 reads as a list"
          | _, Some fields, _ ->
              Opt { name; present; body = Codec_body fields }
              :: layout None rest
          | _, _, Some r ->
              Opt { name; present; body = Region_body r } :: layout None rest
          | (T.Byte_array { size } | T.Byte_slice { size }), _, _ ->
              Flag { name; cond = present }
              :: Word
                   {
                     var = None;
                     size = T.If_then_else (present, size, T.Int 0);
                     conds = [];
                     scalars = [];
                     carrier = "";
                   }
              :: layout None rest
          | _ -> fail_unsupported "the F* reading of the optional field %s" name
          )
      | t -> (
          match (scalar t, named, f.field_name) with
          | Some (size, carrier), Some var, Some name ->
              Word
                {
                  var = Some var;
                  size = T.Int size;
                  conds;
                  scalars = [ (name, fun v -> v) ];
                  carrier;
                }
              :: layout None rest
          | _ ->
              fail_unsupported "the F* reading of the field %s"
                (Option.value ~default:"_" f.field_name)))

and close group rest =
  match group with
  | None -> layout None rest
  | Some (base, order, _, members, conds) ->
      let total = 8 * base_bytes base in
      let first = List.find_map (fun (n, _, _) -> n) (List.rev members) in
      let var = "b" ^ ident (Option.value ~default:"word" first) in
      let scalars =
        List.filter_map
          (fun (name, used, width) ->
            let lo =
              match order with
              | T.Msb_first -> total - used - width
              | T.Lsb_first -> used
            in
            Option.map
              (fun n -> (n, fun v -> Fmt.str "(P.bits %s %d %d)" v lo width))
              name)
          (List.rev members)
      in
      Word
        {
          var = Some var;
          size = T.Int (base_bytes base);
          conds;
          scalars;
          carrier = base_carrier base;
        }
      :: layout None rest

(* {2 Printing} *)

let rec refs : type a. a T.expr -> string list = function
  | T.Ref (_, v) -> [ v ]
  | T.Add (a, b) | T.Sub (a, b) | T.Mul (a, b) -> refs a @ refs b
  | T.Land (a, b) | T.Lsl (a, b) | T.Lsr (a, b) -> refs a @ refs b
  | T.If_then_else (c, a, b) -> refs c @ refs a @ refs b
  | T.Eq (a, b) -> refs a @ refs b
  | T.Ne (a, b) -> refs a @ refs b
  | T.Lt (a, b) -> refs a @ refs b
  | T.Le (a, b) -> refs a @ refs b
  | T.Gt (a, b) -> refs a @ refs b
  | T.Ge (a, b) -> refs a @ refs b
  | T.And (a, b) | T.Or (a, b) -> refs a @ refs b
  | T.Not a -> refs a
  | _ -> []

(* The power of two [m + 1], when [m] is one less than it. *)
let mask m =
  if m > 0 && m < max_int && m land (m + 1) = 0 then Some (m + 1) else None

(* [e] as F* over the reading, each field [v] read as [env v]. *)
let rec fexpr : type a. (string -> string) -> a T.expr -> string =
 fun env e ->
  let p : type b. b T.expr -> string = fun x -> fexpr env x in
  let op o a b = Fmt.str "(%s %s %s)" (p a) o (p b) in
  match e with
  | T.Int n -> if n < 0 then Fmt.str "(%d)" n else string_of_int n
  | T.Bool b -> string_of_bool b
  | T.Ref (_, v) -> env v
  | T.Add (a, b) -> op "+" a b
  | T.Sub (a, b) -> op "-" a b
  | T.Mul (a, b) -> op "*" a b
  | T.Lsl (a, T.Int k) when k >= 0 && k < 62 ->
      Fmt.str "(%s * %d)" (p a) (1 lsl k)
  | T.Land (a, T.Int m) when Option.is_some (mask m) ->
      Fmt.str "(%s %% %d)" (p a) (Option.get (mask m))
  | T.If_then_else (c, a, b) ->
      Fmt.str "(if %s then %s else %s)" (p c) (p a) (p b)
  | T.Eq (a, b) -> op "=" a b
  | T.Ne (a, b) -> op "<>" a b
  | T.Lt (a, b) -> op "<" a b
  | T.Le (a, b) -> op "<=" a b
  | T.Gt (a, b) -> op ">" a b
  | T.Ge (a, b) -> op ">=" a b
  | T.And (a, b) -> op "&&" a b
  | T.Or (a, b) -> op "||" a b
  | T.Not a -> Fmt.str "(not %s)" (p a)
  | _ -> fail_unsupported "the F* reading of %a" T.pp_expr e

(* The name of an output under [path]. *)
let output_name path name = String.concat "_" (path @ [ label name ])

let body_fields = function
  | Codec_body fields -> fields
  | Region_body r -> r.fields

(* The outputs of [items] under [path], each with its value where the
   optional field they lie under is absent. *)
let rec absent path items =
  List.concat_map
    (function
      | Word e -> List.map (fun (n, _) -> (output_name path n, "0")) e.scalars
      | Flag { name; _ } ->
          [ (output_name (path @ [ label name ]) "present", "false") ]
      | Sub { name; fields } ->
          absent (path @ [ label name ]) (layout None fields)
      | Region r -> absent path (layout None r.fields)
      | Opt { name; body; _ } ->
          let path = path @ [ label name ] in
          (output_name path "present", "false")
          :: absent path (layout None (body_fields body)))
    items

(* {3 The reading} *)

(* The F* text of a position: a variable plus a constant. *)
let pos_text = function
  | None, at -> string_of_int at
  | Some v, 0 -> v
  | Some v, at -> Fmt.str "(%s + %d)" v at

(* The reading of [items] from the position [pos] of [s], in a window that
   ends at [lim], under [path]; [env] reads a field of the struct, [outs]
   holds each output read so far. [k] writes what follows, from where the
   items end. Each optional field writes [k] in both its branches. A size
   is not negative where 3D reads it, as an unsigned word; the reading
   refuses one that is, which its proof shows never happens, so that each
   position is plainly one. *)
type reader = { ppf : Format.formatter; fresh : string -> string }

let rec read r ~path ~env ~lim items pos outs k =
  let lookup n =
    match List.assoc_opt n env with
    | Some v -> v
    | None -> fail_unsupported "the F* reading of %s" n
  in
  match items with
  | [] -> k pos outs
  | Word e :: rest ->
      let next =
        match e.size with
        | T.Int n ->
            let var, at = pos in
            let next = (var, at + n) in
            Fmt.pf r.ppf "if %s < %s then invalid () else@\n" lim
              (pos_text next);
            next
        | size ->
            let p = r.fresh "p" in
            Fmt.pf r.ppf "let %s = %s + %s in@\n" p (pos_text pos)
              (fexpr lookup size);
            Fmt.pf r.ppf "if %s < %s || %s < %s then invalid () else@\n" p
              (pos_text pos) lim p;
            (Some p, 0)
      in
      let env, outs =
        match (e.var, e.size) with
        | Some v, T.Int n ->
            let v = r.fresh v in
            Fmt.pf r.ppf "let %s = be s %s %d in@\n" v (pos_text pos) n;
            let values = List.map (fun (n, f) -> (n, f v)) e.scalars in
            ( values @ env,
              outs @ List.map (fun (n, x) -> (output_name path n, x)) values )
        | _ -> (env, outs)
      in
      let lookup n =
        match List.assoc_opt n env with
        | Some v -> v
        | None -> fail_unsupported "the F* reading of %s" n
      in
      if e.conds <> [] then
        Fmt.pf r.ppf "if not (%s) then invalid () else@\n"
          (String.concat " && " (List.map (fexpr lookup) e.conds));
      read r ~path ~env ~lim rest next outs k
  | Flag { name; cond } :: rest ->
      let outs =
        outs
        @ [ (output_name (path @ [ label name ]) "present", fexpr lookup cond) ]
      in
      read r ~path ~env ~lim rest pos outs k
  | Sub { name; fields } :: rest ->
      read r
        ~path:(path @ [ label name ])
        ~env:[] ~lim (layout None fields) pos outs
        (fun pos outs -> read r ~path ~env ~lim rest pos outs k)
  | Region g :: rest ->
      read_region r ~path ~env ~lim g pos outs (fun pos outs ->
          read r ~path ~env ~lim rest pos outs k)
  | Opt { name; present; body } :: rest ->
      let inner = path @ [ label name ] in
      let after pos outs = read r ~path ~env ~lim rest pos outs k in
      Fmt.pf r.ppf "if %s then (@\n" (fexpr lookup present);
      let outs' = outs @ [ (output_name inner "present", "true") ] in
      (match body with
      | Codec_body fields ->
          read r ~path:inner ~env:[] ~lim (layout None fields) pos outs' after
      | Region_body g -> read_region r ~path:inner ~env ~lim g pos outs' after);
      Fmt.pf r.ppf ") else (@\n";
      after pos (outs @ absent path [ Opt { name; present; body } ]);
      Fmt.pf r.ppf ")@\n"

(* A region of [g.size] bytes at [pos], its codec read in a window that
   ends where the region does, the walk going on after it. *)
and read_region r ~path ~env ~lim g pos outs k =
  let lookup n =
    match List.assoc_opt n env with
    | Some v -> v
    | None -> fail_unsupported "the F* reading of %s" n
  in
  Option.iter
    (fun w -> Fmt.pf r.ppf "if not %s then invalid () else@\n" (fexpr lookup w))
    g.where;
  let e = r.fresh "w" in
  Fmt.pf r.ppf "let %s = %s + %s in@\n" e (pos_text pos) (fexpr lookup g.size);
  Fmt.pf r.ppf "if %s < %s || %s < %s then invalid () else@\n" e (pos_text pos)
    lim e;
  read r ~path ~env:[] ~lim:e (layout None g.fields) pos outs (fun cp outs ->
      if not g.at_most then
        Fmt.pf r.ppf "if %s <> %s then invalid () else@\n" (pos_text cp) e;
      k (Some e, 0) outs)

(* {3 The answer}

   Each output read from the value 3d.exe's parser builds: a struct is a
   pair for each item but the last, dependent where the item is refined or
   a later one reads it; a casetype is [t_ite], an optional field present
   in its second case; a region's codec is the first of a pair. *)

let names_read items =
  List.concat_map
    (function
      | Word e -> refs e.size @ List.concat_map refs e.conds
      | Flag { cond; _ } -> refs cond
      | Opt { present; body = Region_body g; _ } ->
          refs present @ refs g.size @ Option.fold ~none:[] ~some:refs g.where
      | Opt { present; _ } -> refs present
      | Region g -> refs g.size @ Option.fold ~none:[] ~some:refs g.where
      | Sub _ -> [])
    items

(* The items with a component in the value: a presence has none. *)
let components = List.filter (function Flag _ -> false | _ -> true)

(* The pattern of a struct of [items], binding each component to a fresh
   variable, with the variables. *)
let pattern fresh items =
  let items = components items in
  let vars = List.map (fun _ -> fresh "c") items in
  let rec go = function
    | [] -> "()"
    | [ (_, x) ] -> x
    | (i, x) :: rest ->
        let later = names_read (List.map fst rest) in
        let read =
          match i with
          | Word e ->
              e.conds <> []
              || List.exists (fun (n, _) -> List.mem n later) e.scalars
          | _ -> false
        in
        if read then Fmt.str "(| %s, %s |)" x (go rest)
        else Fmt.str "(%s, %s)" x (go rest)
  in
  (go (List.combine items vars), List.combine items vars)

(* Each output of [items], read from the struct value [x], as an
   expression that destructures [x]. *)
let rec answers fresh ~path x items =
  let pat, comps = pattern fresh items in
  let under e = Fmt.str "(let %s = %s in %s)" pat x e in
  let env = ref [] in
  let lookup n =
    match List.assoc_opt n !env with
    | Some v -> v
    | None -> fail_unsupported "the F* reading of %s" n
  in
  let comp i = List.assq i comps in
  List.concat_map
    (fun i ->
      match i with
      | Word { var = None; _ } -> []
      | Word e ->
          let read (n, f) =
            (n, Fmt.kstr (fun c -> under (f c)) "(%s %s)" e.carrier (comp i))
          in
          let values = List.map read e.scalars in
          env := values @ !env;
          List.map (fun (n, v) -> (output_name path n, v)) values
      | Flag { name; cond } ->
          [ (output_name (path @ [ label name ]) "present", fexpr lookup cond) ]
      | Sub { name; fields } ->
          List.map
            (fun (n, v) -> (n, under v))
            (answers fresh
               ~path:(path @ [ label name ])
               (comp i) (layout None fields))
      | Region g ->
          let v = fresh "v" in
          List.map
            (fun (n, e) ->
              (n, Fmt.kstr under "(let (%s, _) = %s in %s)" v (comp i) e))
            (answers fresh ~path v (layout None g.fields))
      | Opt { name; body; _ } ->
          optional fresh ~path ~under ~name ~body ~absent:(absent path [ i ])
            (comp i))
    items

(* The outputs of an optional field whose casetype value is [c]: present
   in its second case, whose value holds the codec or its region. *)
and optional fresh ~path ~under ~name ~body ~absent c =
  let inner = path @ [ label name ] in
  let b = fresh "b" in
  let v, fields, wrap =
    match body with
    | Codec_body fields -> (b, fields, Fun.id)
    | Region_body g ->
        let v = fresh "v" in
        let pat =
          if Option.is_some g.where then Fmt.str "(| _, (%s, _) |)" v
          else Fmt.str "(%s, _)" v
        in
        (v, g.fields, fun e -> Fmt.str "(let %s = %s in %s)" pat b e)
  in
  let case default e =
    Fmt.kstr under "(WireGen.Lib3d.ite_case %s (fun _ -> %s) (fun _ %s -> %s))"
      c default b (wrap e)
  in
  (output_name inner "present", case "false" "true")
  :: List.map
       (fun (n, e) -> (n, case (List.assoc n absent) e))
       (answers fresh ~path:inner v (layout None fields))

(* {3 The modules' text} *)

(* The valid arguments: each output, by its name. *)
let arguments (scan : Scan_module.fstar) values =
  List.map
    (fun (o : output) ->
      match List.assoc_opt o.name values with
      | Some v -> v
      | None -> fail_unsupported "the F* reading of the output %s" o.name)
    scan.handed_out

(* The type of [valid]: an [int] for each output, a [bool] for a
   presence. *)
let valid_type (scan : Scan_module.fstar) =
  match scan.handed_out with
  | [] -> "unit -> r"
  | outputs ->
      String.concat " -> "
        (List.map
           (fun (o : output) -> if o.is_bool then "bool" else "P.int")
           outputs
        @ [ "r" ])

let fresh () =
  let n = ref 0 in
  fun s ->
    incr n;
    Fmt.str "%s%d" s !n

let pp_flat ppf ~scan items =
  Fmt.pf ppf
    "let flat (#r:Type) (invalid:unit -> r) (valid:%s) (s:bytes) : r =@\n\
    \  let len = Seq.length s in@\n"
    (valid_type scan);
  let b = Buffer.create 4096 in
  let r = { ppf = Format.formatter_of_buffer b; fresh = fresh () } in
  read r ~path:[] ~env:[] ~lim:"len" items (None, 0) [] (fun _ outs ->
      Fmt.pf r.ppf "valid %s@\n"
        (match arguments scan outs with
        | [] -> "()"
        | args -> String.concat " " args));
  Format.pp_print_flush r.ppf ();
  List.iter
    (fun line -> if line <> "" then Fmt.pf ppf "  %s@\n" line)
    (String.split_on_char '\n' (Buffer.contents b))

let pp_answer ppf ~scan items =
  let outs = answers (fresh ()) ~path:[] "v" items in
  Fmt.pf ppf
    "let answer #r invalid valid o =@\n\
    \  match o with@\n\
    \  | None -> invalid ()@\n\
    \  | Some v ->@\n\
    \    valid %s@\n"
    (match arguments scan outs with
    | [] -> "()"
    | args -> String.concat "\n      " args)

(* {2 The modules} *)

let header ppf ~codec ~m what =
  Fmt.pf ppf
    "module %s@\n\
     @\n\
     (* Generated by Wire_ocaml from %s: %s. Do not edit: generate it again \
     when the codec changes. *)@\n\
     @\n"
    m codec what

let scan_module ~codec ~m (scan : Scan_module.fstar) =
  Fmt.str "%a%s"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Scan")
        "the fold of its scanner, written as F*";
      Fmt.pf ppf "module P = WireGen.OCaml@\n@\n";
      if Record_module.uses scan.body "mask32" then
        Fmt.pf ppf "let mask32 = P.to_int 0xFFFFFFFF@\n@\n";
      Fmt.pf ppf "type valid_t (r:Type) = %s@\n@\n" (valid_type scan))
    () scan.body

(* The steps that write out the type of the value 3d.exe's parser builds,
   but for the types of [EverParse3d.Prelude]. *)
let value_steps ~m =
  Fmt.str
    "[delta_only [`%%EverParse3d.Interpreter.as_type;\n\
    \         `%%EverParse3d.Interpreter.dtyp_as_type;\n\
    \         `%%EverParse3d.Interpreter.itype_as_type;\n\
    \         `%%EverParse3d.Interpreter.type_of_binding;\n\
    \         `%%EverParse3d.Interpreter.mk_dtyp_app;\n\
    \         `%%EverParse3d.Interpreter.coerce];\n\
    \        delta_namespace [\"%s\"]; zeta; iota; primops]"
    m

let flat_interface ~codec ~m ~tag ~scan items =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Flat")
        "the parser 3d.exe writes for its 3D projection, read field by field";
      Fmt.pf ppf
        "open WireGen.Lib@\n\
         module P = WireGen.OCaml@\n\
         @\n\
         (* The value 3d.exe's parser builds, its type written out but for \
         Prelude's types. *)@\n\
         let value : Type0 =@\n\
        \  norm %s@\n\
        \    (EverParse3d.Interpreter.as_type (%s.def'____%s ()))@\n\
         @\n\
         val spec (s:bytes) : GTot (option value)@\n\
         @\n\
         val answer (#r:Type) (invalid:unit -> r) (valid:%s) (o:option value) \
         : r@\n\
         @\n"
        (value_steps ~m) m tag (valid_type scan);
      pp_flat ppf ~scan items;
      Fmt.pf ppf
        "@\n\
         val flat_is_3d (#r:Type) (invalid:unit -> r) (valid:%s) (s:bytes)@\n\
        \  : Lemma (answer invalid valid (spec s) == flat invalid valid s)@\n"
        (valid_type scan))
    ()

(* The combinators of 3D a reading needs and WireGen.Lib does not state,
   which name the 3D parsers at their own kind: only a friend of
   [EverParse3d.Prelude] may, so each reading states them. A byte array of
   [n] bytes is [parse_nlist] of UINT8, which holds exactly when there are
   [n] bytes, and takes them; a region of at most [n] bytes parses its
   contents on its first [n] bytes, and takes them; a struct's precondition
   is a unit, which takes no byte. *)
let combinators =
  "let nlist_u8 (n:U32.t) (c:option nat{EP.memoizes_n_as_const c n}) \
   (s:LP.bytes)\n\
  \  : Lemma (let r = LP.parse (EP.parse_nlist n c #_ #EP.kind____UINT8 \
   EP.parse____UINT8) s in\n\
  \           (Some? r <==> Seq.length s >= U32.v n) /\\\n\
  \           (match r with Some (_, k) -> k == U32.v n | None -> True))\n\
  \          [SMTPat (LP.parse (EP.parse_nlist n c #_ #EP.kind____UINT8 \
   EP.parse____UINT8) s)]\n\
  \  = fldata_u8_list (U32.v n) s\n\n\
   let at_most_eq (n:U32.t) #nz #wk (#k:EP.parser_kind nz wk) #t (p:EP.parser \
   k t) (s:LP.bytes)\n\
  \  : Lemma ((EP.parse_t_at_most n p <: LP.bare_parser (EP.t_at_most n t)) s ==\n\
  \           (if Seq.length s < U32.v n then None\n\
  \            else match (p <: LP.bare_parser t) (Seq.slice s 0 (U32.v n)) with\n\
  \              | None -> None\n\
  \              | Some (v, c) ->\n\
  \                Some ((v, Seq.slice (Seq.slice s 0 (U32.v n)) c (U32.v n)), \
   U32.v n)))\n\
  \          [SMTPat ((EP.parse_t_at_most n p <: LP.bare_parser (EP.t_at_most \
   n t)) s)]\n\
  \  = if Seq.length s >= U32.v n then\n\
  \      LowParse.Spec.Combinators.nondep_then_eq p EP.parse_all_bytes \
   (Seq.slice s 0 (U32.v n))\n\n\
   let unit_eq (s:LP.bytes)\n\
  \  : Lemma ((EP.parse_unit <: LP.bare_parser unit) s == Some ((), 0))\n\
  \          [SMTPat ((EP.parse_unit <: LP.bare_parser unit) s)]\n\
  \  = ()\n"

let flat_implementation ~codec ~m ~tag ~scan items =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Flat")
        "the parser 3d.exe writes for its 3D projection, read field by field";
      Fmt.pf ppf
        "friend EverParse3d.Prelude@\n\
         friend %s@\n\
         @\n\
         open WireGen.Lib@\n\
         module P = WireGen.OCaml@\n\
         module LP = LowParse.Spec.Base@\n\
         module EP = EverParse3d.Prelude@\n\
         module U32 = FStar.UInt32@\n\
         @\n\
         %s@\n\
         let spec (s:bytes) : GTot (option value) =@\n\
        \  match LP.parse (%s.parser____%s ()) s with@\n\
        \  | None -> None@\n\
        \  | Some (v, _) ->@\n\
        \    norm_spec %s@\n\
        \      (EverParse3d.Interpreter.as_type (%s.def'____%s ()));@\n\
        \    Some (coerce_eq () v)@\n\
         @\n"
        m combinators m tag (value_steps ~m) m tag;
      pp_answer ppf ~scan items;
      Fmt.pf ppf
        "@\n\
         #push-options \"--z3rlimit 400 --fuel 1 --ifuel 1\"@\n\
         let flat_is_3d #r invalid valid s =@\n\
        \  assert (answer invalid valid (spec s) == flat invalid valid s)@\n\
        \    by (WireGen.Tac.unfold_flat [\"%s\"])@\n\
         #pop-options@\n"
        m)
    ()

let theorem =
  "(* The scanner's fold rejects a bad window, and on a good one answers what\n\
  \   3d.exe's parser answers on the window's bytes. *)\n\
   val fold_is_3d (#r:Type) (buf:P.buf) (off len:P.int) (invalid:unit -> r) \
   (valid:valid_t r)\n\
  \  : Lemma (fold buf off len invalid valid ==\n\
  \           (if off < 0 || len < 0 || off + len > P.length buf then invalid ()\n\
  \            else Flat.answer invalid valid (Flat.spec (Seq.slice buf off \
   (off + len)))))\n"

let proof_interface ~codec ~m =
  Fmt.str "%a%s"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Proof")
        "that the fold of its scanner is the parser 3d.exe writes";
      Fmt.pf ppf
        "open %s.Scan@\nmodule P = WireGen.OCaml@\nmodule Flat = %s.Flat@\n@\n"
        m m)
    () theorem

(* The identifiers of [text] that are not qualified by a module. *)
let identifiers text =
  let n = String.length text in
  let ident c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec go acc i =
    if i >= n then List.rev acc
    else if ident text.[i] && (i = 0 || not (ident text.[i - 1])) then begin
      let j = ref i in
      while !j < n && ident text.[!j] do
        incr j
      done;
      let x = String.sub text i (!j - i) in
      let qualified = i > 0 && text.[i - 1] = '.' in
      let module_ = !j < n && text.[!j] = '.' in
      go (if qualified || module_ then acc else x :: acc) !j
    end
    else go acc (i + 1)
  in
  go [] 0

(* A position the walk binds: its window's offset, a field's address, the
   end or the size of a span, a region's offset. A fact holds of any. *)
let is_position x =
  x = "base"
  || String.length x >= 2
     && (match x.[0] with 'o' | 'b' | 'e' | 'n' -> true | _ -> false)
     && String.for_all
          (function '0' .. '9' -> true | _ -> false)
          (String.sub x 1 (String.length x - 1))

(* The loads of [text]: each address, as a variable and a constant, and the
   bytes it reads. *)
let loads text =
  let widths = [ ("u8", 1); ("u16be", 2); ("i32be", 4); ("i64be", 8) ] in
  let n = String.length text in
  let rec go acc i =
    match String.index_from_opt text i '(' with
    | None -> List.rev acc
    | Some j ->
        let rest = String.sub text (j + 1) (n - j - 1) in
        let load (f, w) =
          let prefix = "P." ^ f ^ " buf " in
          if String.starts_with ~prefix rest then
            let a = String.length prefix in
            let addr = String.sub rest a (String.length rest - a) in
            if String.starts_with ~prefix:"(P.add " addr then
              Scanf.sscanf addr "(P.add %[a-z0-9_] %d)" (fun x k ->
                  Some (x, k, w))
            else Scanf.sscanf addr "%[a-z0-9_]" (fun x -> Some (x, 0, w))
          else None
        in
        go
          (match List.find_map load widths with
          | Some l -> l :: acc
          | None -> acc)
          (j + 1)
  in
  go [] 0

(* A fact Z3 proves on its own: for every position it reads at, each load
   inside the buffer, [fact] holds. It fires where [about] occurs. *)
let pp_fact ppf i ~about fact =
  let text = about ^ " " ^ fact in
  let roots =
    List.sort_uniq compare (List.filter is_position (identifiers text))
  in
  let requires =
    List.concat_map
      (fun (x, k, w) ->
        let a = if k = 0 then x else Fmt.str "%s + %d" x k in
        [ Fmt.str "0 <= %s" a; Fmt.str "%s + %d <= P.length buf" a w ])
      (List.sort_uniq compare (loads text))
  in
  Fmt.pf ppf "let fact%d (buf:P.buf) %s@\n  : Lemma (requires %s)@\n" i
    (String.concat " " (List.map (Fmt.str "(%s:P.int)") roots))
    (if requires = [] then "True" else String.concat " /\\ " requires);
  Fmt.pf ppf "    (ensures (%s))@\n    [SMTPat (%s)]@\n" fact about;
  Fmt.pf ppf "  = WireGen.Tables.tables ()@\n@\n"

(* The facts the walk notes: the bytes each binding that reads them holds,
   as the emitter states it, and the facts its tests rely on. *)
let pp_facts ppf (scan : Scan_module.fstar) =
  let facts =
    List.filter_map
      (function
        | Let { value; holds = Some h; _ } ->
            Some (value, Fmt.str "%s == %s" value h)
        | Let { holds = None; _ } -> None
        | Holds { fact; about } -> Some (about, fact))
      scan.notes
  in
  Fmt.pf ppf
    "(* What each binding of the walk holds, in bytes of the buffer, as the@\n\
    \   emitter states it: Z3 proves each on its own. *)@\n\
     #push-options \"--z3rlimit 50 --fuel 1 --ifuel 1\"@\n";
  List.iteri (fun i (about, fact) -> pp_fact ppf i ~about fact) facts;
  Fmt.pf ppf "#pop-options@\n@\n"

(* [fold] is [flat] on the window's bytes, then [fold_is_3d]. *)
let pp_theorem ppf ~m =
  Fmt.pf ppf
    "#push-options \"--z3rlimit 200 --fuel 1 --ifuel 1\"@\n\
     let fold_is_flat (#r:Type) (buf:P.buf) (off len:P.int)@\n\
    \    (invalid:unit -> r) (valid:valid_t r)@\n\
    \  : Lemma (fold buf off len invalid valid ==@\n\
    \           (if off < 0 || len < 0 || off + len > P.length buf then \
     invalid ()@\n\
    \            else Flat.flat invalid valid (Seq.slice buf off (off + len))))@\n\
    \  = assert (fold buf off len invalid valid ==@\n\
    \           (if off < 0 || len < 0 || off + len > P.length buf then \
     invalid ()@\n\
    \            else Flat.flat invalid valid (Seq.slice buf off (off + len))))@\n\
    \      by (WireGen.Tac.unfold_scan [`%%%s.Scan.fold; `%%%s.Flat.flat])@\n\
     #pop-options@\n\
     @\n\
     let fold_is_3d #r buf off len invalid valid =@\n\
    \  fold_is_flat buf off len invalid valid;@\n\
    \  if off < 0 || len < 0 || off + len > P.length buf then ()@\n\
    \  else Flat.flat_is_3d invalid valid (Seq.slice buf off (off + len))@\n"
    m m

let proof_implementation ~codec ~m scan =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Proof")
        "that the fold of its scanner is the parser 3d.exe writes";
      Fmt.pf ppf
        "open WireGen.Lib@\n\
         open %s.Scan@\n\
         module P = WireGen.OCaml@\n\
         module Flat = %s.Flat@\n\
         @\n"
        m m;
      pp_facts ppf scan;
      pp_theorem ppf ~m)
    ()

let modules ?fields ~codec c =
  let scan = Scan_module.fstar ?fields c in
  let st = Gen.to_struct c in
  if st.params <> [] || Option.is_some st.where then
    fail_unsupported "the F* reading of a parameterised struct";
  if not (Names.is_identifier st.name) then
    fail_unsupported "the F* reading of the struct %S" st.name;
  let items = layout None st.fields in
  let tag = "Wire" ^ st.name in
  [
    (codec ^ ".Scan.fst", scan_module ~codec ~m:codec scan);
    (codec ^ ".Flat.fsti", flat_interface ~codec ~m:codec ~tag ~scan items);
    (codec ^ ".Flat.fst", flat_implementation ~codec ~m:codec ~tag ~scan items);
    (codec ^ ".Proof.fsti", proof_interface ~codec ~m:codec);
    (codec ^ ".Proof.fst", proof_implementation ~codec ~m:codec scan);
  ]
