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
   variable in the reading, its offset and size, its refinements and the
   scalars it holds, each as a name and the F* text of its value from the
   variable. *)
type elem = {
  var : string option;
  offset : int;
  size : int;
  conds : bool T.expr list;
  scalars : (string * (string -> string)) list;
  carrier : string;
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

(* The elements of the struct's [fields] from [offset]: consecutive
   bitfields of one base and bit order that fit its word share one, the
   first declared at the most significant bits for [Msb_first]. *)
let rec elems offset group fields =
  match (fields, group) with
  | [], None -> []
  | [], Some _ -> close offset group []
  | (T.Field f :: rest as fields), _ -> (
      let typ, wheres = unwrap f.field_typ in
      let conds = wheres @ Option.to_list f.constraint_ in
      let named = Option.map ident f.field_name in
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
              elems offset
                (Some (b, o, used + width, member :: members, gconds @ conds))
                rest
          | Some _ -> close offset group fields
          | None ->
              elems offset
                (Some
                   (base, bit_order, width, [ (f.field_name, 0, width) ], conds))
                rest)
      | _ when Option.is_some group -> close offset group fields
      | T.Pack_typ (T.Byte_array { size = T.Int n }) ->
          if conds <> [] then
            fail_unsupported "the F* reading of a refined byte array";
          {
            var = None;
            offset;
            size = n;
            conds = [];
            scalars = [];
            carrier = "";
          }
          :: elems (offset + n) None rest
      | t -> (
          match (scalar t, named, f.field_name) with
          | Some (size, carrier), Some var, Some name ->
              {
                var = Some var;
                offset;
                size;
                conds;
                scalars = [ (name, fun v -> v) ];
                carrier;
              }
              :: elems (offset + size) None rest
          | _ ->
              fail_unsupported "the F* reading of the field %s"
                (Option.value ~default:"_" f.field_name)))

and close offset group rest =
  match group with
  | None -> elems offset None rest
  | Some (base, order, _, members, conds) ->
      let total = 8 * base_bytes base in
      let var = Fmt.str "bf%d" offset in
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
      {
        var = Some var;
        offset;
        size = base_bytes base;
        conds;
        scalars;
        carrier = base_carrier base;
      }
      :: elems (offset + base_bytes base) None rest

(* {2 Printing} *)

let rec refs : type a. a T.expr -> string list = function
  | T.Ref (_, v) -> [ v ]
  | T.Add (a, b) | T.Sub (a, b) | T.Mul (a, b) -> refs a @ refs b
  | T.Eq (a, b) -> refs a @ refs b
  | T.Ne (a, b) -> refs a @ refs b
  | T.Lt (a, b) -> refs a @ refs b
  | T.Le (a, b) -> refs a @ refs b
  | T.Gt (a, b) -> refs a @ refs b
  | T.Ge (a, b) -> refs a @ refs b
  | T.And (a, b) | T.Or (a, b) -> refs a @ refs b
  | T.Not a -> refs a
  | _ -> []

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

(* Each scalar's name, with the F* text of its value from the element's
   variable [read]. *)
let values ~read elems =
  List.concat_map
    (fun e ->
      match e.var with
      | None -> []
      | Some v -> List.map (fun (n, f) -> (n, f (read e v))) e.scalars)
    elems

(* The valid arguments: each output, as the field of the codec it is. *)
let arguments (scan : Scan_module.fstar) values =
  List.map
    (fun (o : output) ->
      let f =
        List.find
          (fun (f : field) ->
            match f.output with Some o' -> o'.id = o.id | None -> false)
          scan.prefix_fields
      in
      match List.assoc_opt f.name values with
      | Some v -> v
      | None -> fail_unsupported "the F* reading of the output %s" o.name)
    scan.handed_out

(* The type of [valid]: an [int] for each output. *)
let valid_type (scan : Scan_module.fstar) =
  match scan.handed_out with
  | [] -> "unit -> r"
  | outputs ->
      String.concat " -> " (List.map (fun _ -> "P.int") outputs @ [ "r" ])

let pp_flat ppf ~scan elems =
  let env = values ~read:(fun _ v -> v) elems in
  let lookup n =
    match List.assoc_opt n env with
    | Some v -> v
    | None -> fail_unsupported "the F* reading of %s" n
  in
  Fmt.pf ppf
    "let flat (#r:Type) (invalid:unit -> r) (valid:%s) (s:bytes) : r =@\n\
    \  let len = Seq.length s in@\n"
    (valid_type scan);
  List.iter
    (fun e ->
      Fmt.pf ppf "  if len < %d then invalid () else@\n" (e.offset + e.size);
      Option.iter
        (fun v -> Fmt.pf ppf "  let %s = be s %d %d in@\n" v e.offset e.size)
        e.var;
      if e.conds <> [] then
        Fmt.pf ppf "  if not (%s) then invalid () else@\n"
          (String.concat " && " (List.map (fexpr lookup) e.conds)))
    elems;
  Fmt.pf ppf "  valid %s@\n"
    (match arguments scan env with
    | [] -> "()"
    | args -> String.concat " " args)

(* The value 3d.exe's parser builds: a pair for each element but the last,
   dependent where the element is refined or a later one reads it. *)
let pattern elems =
  let rec go = function
    | [] -> "()"
    | [ e ] -> Option.value ~default:"_" e.var
    | e :: rest ->
        let later =
          List.concat_map (fun e -> List.concat_map refs e.conds) rest
        in
        let read = List.exists (fun (n, _) -> List.mem n later) e.scalars in
        let x = Option.value ~default:"_" e.var in
        if e.conds <> [] || read then Fmt.str "(| %s, %s |)" x (go rest)
        else Fmt.str "(%s, %s)" x (go rest)
  in
  go elems

let pp_answer ppf ~scan elems =
  let env = values ~read:(fun e v -> Fmt.str "(%s %s)" e.carrier v) elems in
  Fmt.pf ppf
    "let answer #r invalid valid o =@\n\
    \  match o with@\n\
    \  | None -> invalid ()@\n\
    \  | Some v ->@\n\
    \    let %s = v in@\n\
    \    valid %s@\n"
    (pattern elems)
    (match arguments scan env with
    | [] -> "()"
    | args -> String.concat " " args)

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
      Fmt.pf ppf
        "let window (buf:P.buf) (base:P.int) (stop:P.int) : prop =@\n\
        \  0 <= base /\\ stop <= P.length buf - base@\n\
         @\n\
         type valid_t (r:Type) = %s -> r@\n\
         @\n"
        (match scan.handed_out with
        | [] -> "unit"
        | outputs -> String.concat " -> " (List.map (fun _ -> "P.int") outputs)))
    () scan.body

let flat_interface ~codec ~m ~tag ~scan elems =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec ~m:(m ^ ".Flat")
        "the parser 3d.exe writes for its 3D projection, read field by field";
      Fmt.pf ppf
        "open WireGen.Lib@\n\
         module P = WireGen.OCaml@\n\
         @\n\
         let value = EverParse3d.Interpreter.as_type (%s.def'____%s ())@\n\
         @\n\
         val spec (s:bytes) : GTot (option value)@\n\
         @\n\
         val answer (#r:Type) (invalid:unit -> r) (valid:%s) (o:option value) \
         : r@\n\
         @\n"
        m tag (valid_type scan);
      pp_flat ppf ~scan elems;
      Fmt.pf ppf
        "@\n\
         val flat_is_3d (#r:Type) (invalid:unit -> r) (valid:%s) (s:bytes)@\n\
        \  : Lemma (answer invalid valid (spec s) == flat invalid valid s)@\n"
        (valid_type scan))
    ()

(* A byte array of [n] bytes, as 3d.exe denotes it: [parse_nlist] of
   UINT8, which holds exactly when there are [n] bytes, and takes them. It
   names the 3D parser at its own kind, which only a friend of
   [EverParse3d.Prelude] may, so each reading states it. *)
let nlist =
  "let nlist_u8 (n:U32.t) (c:option \
   nat{EverParse3d.Prelude.memoizes_n_as_const c n}) (s:LP.bytes)\n\
  \  : Lemma (let r = LP.parse (EverParse3d.Prelude.parse_nlist n c #_ \
   #EverParse3d.Prelude.kind____UINT8 EverParse3d.Prelude.parse____UINT8) s in\n\
  \           (Some? r <==> Seq.length s >= U32.v n) /\\\n\
  \           (match r with Some (_, k) -> k == U32.v n | None -> True))\n\
  \          [SMTPat (LP.parse (EverParse3d.Prelude.parse_nlist n c #_ \
   #EverParse3d.Prelude.kind____UINT8 EverParse3d.Prelude.parse____UINT8) s)]\n\
  \  = fldata_u8_list (U32.v n) s\n"

let flat_implementation ~codec ~m ~tag ~scan elems =
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
         module U32 = FStar.UInt32@\n\
         @\n\
         %s@\n\
         let spec (s:bytes) : GTot (option value) =@\n\
        \  match LP.parse (%s.parser____%s ()) s with@\n\
        \  | None -> None@\n\
        \  | Some (v, _) -> Some v@\n\
         @\n"
        m nlist m tag;
      pp_answer ppf ~scan elems;
      Fmt.pf ppf
        "@\n\
         #push-options \"--z3rlimit 400 --fuel 1 --ifuel 1\"@\n\
         let flat_is_3d #r invalid valid s =@\n\
        \  assert (answer invalid valid (spec s) == flat invalid valid s)@\n\
        \    by (WireGen.Tac.unfold_flat [`%%spec; `%%answer; `%%flat;@\n\
        \          `%%%s.parser____%s; `%%%s.def'____%s; `%%%s.def____%s])@\n\
         #pop-options@\n"
        m tag m tag m tag)
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

(* Whether [text] mentions the variable [x]. *)
let mentions x text = Record_module.uses text x

(* The bindings among [lets] that [text] needs, with those they need, in
   order. *)
let needed lets text =
  let rec go acc = function
    | [] -> acc
    | (var, rhs) :: earlier ->
        if List.exists (fun (_, t) -> mentions var t) acc || mentions var text
        then go ((var, rhs) :: acc) earlier
        else go acc earlier
  in
  go [] (List.rev lets)

let pp_lets ppf lets =
  List.iter (fun (var, rhs) -> Fmt.pf ppf "  let %s = %s in@\n" var rhs) lets

(* The facts the walk notes, each with the bindings it needs, as lemmas Z3
   proves one by one, then [lets], their conjunction under all the
   bindings. *)
let pp_facts ppf (scan : Scan_module.fstar) =
  let lets =
    List.filter_map
      (function Let { var; rhs; _ } -> Some (var, rhs) | Holds _ -> None)
      scan.notes
  in
  let facts =
    List.filter_map
      (function
        | Let { var; holds = Some h; _ } -> Some (var ^ " == " ^ h)
        | Let { holds = None; _ } -> None
        | Holds { fact; _ } -> Some fact)
      scan.notes
  in
  let window =
    Fmt.str "(buf:P.buf) (base:P.int{0 <= base /\\ base + %d <= P.length buf})"
      scan.need
  in
  Fmt.pf ppf
    "(* What each binding of the walk holds, in bytes of the window, as the@\n\
    \   emitter states it: Z3 proves each on its own. *)@\n\
     #push-options \"--z3rlimit 50 --fuel 1 --ifuel 1\"@\n";
  List.iteri
    (fun i fact ->
      Fmt.pf ppf "let fact%d %s@\n  : Lemma (@\n%a  %s)@\n" i window pp_lets
        (needed lets fact) fact;
      Fmt.pf ppf "  = WireGen.Tables.tables ()@\n@\n")
    facts;
  Fmt.pf ppf "let lets %s@\n  : Lemma (@\n%a  %s)@\n  = %s@\n#pop-options@\n@\n"
    window pp_lets lets
    (if facts = [] then "True" else String.concat " /\\\n    " facts)
    (if facts = [] then "()"
     else
       String.concat "; "
         (List.mapi (fun i _ -> Fmt.str "fact%d buf base" i) facts))

(* [fold_record] is [flat] on the window's bytes, then [fold_is_3d]. *)
let pp_theorem ppf ~m (scan : Scan_module.fstar) =
  let names =
    String.concat "; "
      (List.map (fun f -> Fmt.str "`%%%s.Scan.%s" m f) scan.functions
      @ [ Fmt.str "`%%%s.Flat.flat" m ])
  in
  Fmt.pf ppf
    "#push-options \"--z3rlimit 200 --fuel 1 --ifuel 1\"@\n\
     let record_is_flat (#r:Type) (buf:P.buf) (base:P.int) (stop:P.int{window \
     buf base stop /\\ stop >= 0})@\n\
    \    (invalid:unit -> r) (valid:valid_t r)@\n\
    \  : Lemma (fold_record buf base stop invalid valid ==@\n\
    \           Flat.flat invalid valid (Seq.slice buf base (base + stop)))@\n\
    \  = if stop < %d then@\n\
    \      assert (fold_record buf base stop invalid valid ==@\n\
    \              Flat.flat invalid valid (Seq.slice buf base (base + stop)))@\n\
    \        by (WireGen.Tac.unfold_scan [%s])@\n\
    \    else begin@\n\
    \      lets buf base;@\n\
    \      assert (fold_record buf base stop invalid valid ==@\n\
    \              Flat.flat invalid valid (Seq.slice buf base (base + stop)))@\n\
    \        by (WireGen.Tac.unfold_lets (`lets) [%s])@\n\
    \    end@\n\
     #pop-options@\n\
     @\n\
     let fold_is_3d #r buf off len invalid valid =@\n\
    \  if off < 0 || len < 0 || off + len > P.length buf then ()@\n\
    \  else begin@\n\
    \    record_is_flat buf off len invalid valid;@\n\
    \    Flat.flat_is_3d invalid valid (Seq.slice buf off (off + len))@\n\
    \  end@\n"
    scan.need names names

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
      pp_theorem ppf ~m scan)
    ()

let modules ?fields ~codec c =
  let scan = Scan_module.fstar ?fields c in
  if not (List.mem "fold_record" scan.functions) then
    fail_unsupported "the F* rendering of a fold with no record function";
  let st = Gen.to_struct c in
  if st.params <> [] || Option.is_some st.where then
    fail_unsupported "the F* reading of a parameterised struct";
  if not (Names.is_identifier st.name) then
    fail_unsupported "the F* reading of the struct %S" st.name;
  let elems = elems 0 None st.fields in
  let tag = "Wire" ^ st.name in
  [
    (codec ^ ".Scan.fst", scan_module ~codec ~m:codec scan);
    (codec ^ ".Flat.fsti", flat_interface ~codec ~m:codec ~tag ~scan elems);
    (codec ^ ".Flat.fst", flat_implementation ~codec ~m:codec ~tag ~scan elems);
    (codec ^ ".Proof.fsti", proof_interface ~codec ~m:codec);
    (codec ^ ".Proof.fst", proof_implementation ~codec ~m:codec scan);
  ]
