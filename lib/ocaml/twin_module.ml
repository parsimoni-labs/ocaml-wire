open Wire.Private
module T = Types
open Planner
module V = View_module

(* {1 The interpreted twin}

   The functor that reads a codec value as the generated module reads its
   bytes: the same views, each field read through the codec's own views
   ([Wire.Codec.View]) by its place in the codec's field list, the same
   [validate] and the same folds, each frame checked and its outputs read by
   the codec. *)

let gen = "Wire.Private.Gen"

(* The place of the field of [r] whose OCaml label is [label]. *)
let place_of (r : V.record) ~what pred =
  let rec go i = function
    | [] ->
        Fmt.invalid_arg "Wire_ocaml: no interpreted twin of %s in %s" what
          r.codec_name
    | T.Field f :: rest -> (
        match f.field_name with Some n when pred n -> i | _ -> go (i + 1) rest)
  in
  go 0 r.struct_.fields

let by_label (r : V.record) label =
  place_of r ~what:label (fun n -> Names.ocaml_label n = label)

let by_name (r : V.record) name = place_of r ~what:name (String.equal name)

(* The kind [Wire.Private.Gen] checks a scalar against. *)
let kind (f : field) =
  match f.store with
  | Wire.Private.Layout.Int { width = 8; signed = false; _ } -> "UInt8"
  | Int { width = 8; signed = true; _ } -> "SInt8"
  | Int { width = 16; signed = false; _ } -> "UInt16"
  | Int { width = 16; signed = true; _ } -> "SInt16"
  | Int { signed = false; _ } -> "UInt32"
  | Int { signed = true; _ } -> "SInt32"
  | Bits _ -> "Int"
  | Bytes _ -> "String"

(* The variable of the records of the codec [id]. *)
let var (r : V.record) = if r.id < 0 then "r'" else Fmt.str "r'%d" r.id

(* Each record a field holds, reached from the root through the first field
   that holds it: its variable bound to the record that field holds. *)
let pp_records ppf (t : V.t) =
  Fmt.pf ppf "let r' = %s.root C.codec@\n" gen;
  let seen = Hashtbl.create 8 in
  let rec walk (r : V.record) =
    List.iter
      (function
        | V.Sub { sub; _ } when not (Hashtbl.mem seen sub.codec_id) ->
            Hashtbl.replace seen sub.codec_id ();
            let label =
              match sub.path with
              | Some [ label ] -> label
              | _ ->
                  Fmt.invalid_arg
                    "Wire_ocaml: no interpreted twin of a record under %s"
                    r.codec_name
            in
            let held =
              List.find (fun (h : V.record) -> h.id = sub.codec_id) t.records
            in
            Fmt.pf ppf "let %s = %s.sub_at %s %d@\n" (var held) gen (var r)
              (by_label r label);
            walk held
        | _ -> ())
      r.members
  in
  walk t.root

(* The accessors of [r]: each reads through a handle bound beside it, named
   with a prime no accessor has. *)
let pp_members ppf (t : V.t) (r : V.record) =
  let v = var r in
  List.iter
    (function
      | V.Scalar { name; field } ->
          Fmt.pf ppf
            "let h'%s = %s.field_at %s %d %s.%s@\n\
             let %s b v = %s.read h'%s b v@\n"
            name gen v (by_name r field.name) gen (kind field) name gen name
      | V.Present { name; presence } ->
          let label =
            String.sub presence.name 0
              (String.length presence.name - String.length "_present")
          in
          Fmt.pf ppf
            "let h'%s = %s.present_at %s %d@\nlet %s b v = %s.read h'%s b v@\n"
            name gen v (by_label r label) name gen name
      | V.Sub { name; sub; _ } ->
          let label = match sub.path with Some [ l ] -> l | _ -> name in
          let m =
            Option.get
              (List.find (fun (h : V.record) -> h.id = sub.codec_id) t.records)
                .module_name
          in
          Fmt.pf ppf
            "let h'%s = %s.enter_at %s %d@\n\
             let %s b len v : 'b %s.check = %s.enter h'%s b len v@\n"
            name gen v (by_label r label) name m gen name
      | V.Bytes { name; label; slice; _ } ->
          Fmt.pf ppf
            "let h'%s = %s.field_at %s %d %s.%s@\n\
             let %s b v = %s.read h'%s b v@\n"
            name gen v (by_label r label) gen
            (if slice then "Slice" else "String")
            name gen name)
    r.members;
  List.iter
    (fun (name, (field : field)) ->
      Fmt.pf ppf
        "let s'%s = %s.set_at %s %d %s.%s@\nlet set_%s = s'%s.%s.write@\n" name
        gen v (by_name r field.name) gen (kind field) name name gen)
    (V.settable r)

let pp_types ppf name =
  Fmt.pf ppf
    "type 'b view = int@\n\
     type 'b check = int@\n\
     @\n\
     let ok (c : 'b check) = c >= 0@\n\
     @\n\
     let get (c : 'b check) : 'b view =@\n\
    \  if c >= 0 then c else invalid_arg %S@\n\
     @\n"
    (name ^ ".get: the check failed")

(* The arguments [valid] takes from the outputs [o]: each integer as an
   [int], or with a visitor as an [int64]. *)
let args ~visitor (selected : output list) =
  if selected = [] then "()"
  else
    String.concat " "
      (List.map
         (fun (o : output) ->
           if o.is_bool then
             Fmt.str "~%s:(%s.Outputs.bool o %S)" o.name gen o.name
           else if visitor then
             Fmt.str "~%s:(%s.Outputs.int o %S)" o.name gen o.name
           else
             Fmt.str "~%s:(Int64.to_int (%s.Outputs.int o %S))" o.name gen
               o.name)
         selected)

let pp_fold ppf (f : Scan_module.fold) =
  let n = f.name and o = if f.selected = [] then "_" else "o" in
  match f.visitor with
  | None ->
      Fmt.pf ppf
        "let %s buf off len ~invalid ~valid =@\n\
        \  match scan' buf off len with@\n\
        \  | None -> invalid ()@\n\
        \  | Some %s -> valid %s@\n\
         @\n\
         let %s_frames buf off len ~stride ~init ~invalid ~valid =@\n\
        \  %s.frames ~name:%S buf off len ~stride ~init (fun acc off len ->@\n\
        \      match scan' buf off len with@\n\
        \      | None -> invalid acc@\n\
        \      | Some %s -> valid acc %s)@\n\
         @\n"
        n o
        (args ~visitor:false f.selected)
        n gen (n ^ "_frames") o
        (args ~visitor:false f.selected)
  | Some m ->
      Fmt.pf ppf
        "let %s buf off len ~init =@\n\
        \  match scan' buf off len with@\n\
        \  | None -> %s.invalid init@\n\
        \  | Some %s -> %s.valid init %s@\n\
         @\n\
         let %s_frames buf off len ~stride ~init =@\n\
        \  %s.frames ~name:%S buf off len ~stride ~init (fun acc off len ->@\n\
        \      %s buf off len ~init:acc)@\n\
         @\n\
         let %s_index buf ~offs ~lens ~init =@\n\
        \  %s.index ~name:%S ~offs ~lens ~init (fun acc off len ->@\n\
        \      %s buf off len ~init:acc)@\n\
         @\n"
        n m o m
        (args ~visitor:true f.selected)
        n gen (n ^ "_frames") n n gen (n ^ "_index") n

let header ppf ~codec =
  Fmt.pf ppf
    "(* Generated by Wire_ocaml from the codec %s. Do not edit: generate it \
     again when the codec changes. *)@\n\
     @\n"
    codec

(* The functor's head, inlined where it is applied so that flambda reads its
   records and handles as constants, not through the instance's closures. *)
let pp_functor ppf ~digest =
  Fmt.pf ppf
    "module Make =@\n\
     functor@\n\
    \  [@inline]@\n\
    \  (C : sig@\n\
    \    type r@\n\
     @\n\
    \    val codec : r Wire.Codec.t@\n\
    \  end)@\n\
     ->@\n\
     struct@\n\
     let () = %s.check C.codec %S@\n"
    gen digest

let ml ~codec ~digest ~(views : V.t option) ~folds =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec;
      pp_functor ppf ~digest;
      (match views with
      | None -> ()
      | Some (t : V.t) ->
          pp_records ppf t;
          List.iter
            (fun (r : V.record) ->
              Fmt.pf ppf "@\nmodule %s = struct@\n" (Option.get r.module_name);
              pp_types ppf (Option.get r.module_name);
              pp_members ppf t r;
              Fmt.pf ppf "end@\n")
            t.records;
          Fmt.pf ppf "@\n";
          pp_types ppf codec;
          (match t.size with
          | Some _ ->
              Fmt.pf ppf
                "let check b length off : 'b check = %s.check_at r' b length \
                 off@\n"
                gen
          | None ->
              Fmt.pf ppf
                "let check b length ~off ~len : 'b check =@\n\
                \  %s.check_window r' b length ~off ~len@\n"
                gen);
          pp_members ppf t t.root);
      Fmt.pf ppf
        "@\n\
         let min_size = Wire.Codec.min_size C.codec@\n\
         let validate buf off = Wire.Codec.validate C.codec buf off@\n";
      if folds <> [] then begin
        Fmt.pf ppf "let scan' = %s.scan C.codec@\n@\n" gen;
        List.iter (pp_fold ppf) folds
      end;
      Fmt.pf ppf "end@\n")
    ()

let mli ~codec ~(plan : plan) ~(views : V.t option) ~folds =
  Fmt.str "%a"
    (fun ppf () ->
      header ppf ~codec;
      Fmt.pf ppf
        "(** The interpreted twin of the module generated from [%s].@\n\
         @\n\
        \    [Make (C)] reads the codec [C.codec] as the generated module \
         reads its bytes, through the codec's views ([Wire.Codec.View]) and \
         [Wire.Codec.validate]: the same views, accessors, [validate] and \
         folds, so code written against one runs on the other by changing one \
         line. It links [wire]. *)@\n\
         @\n\
         module Make (_ : sig@\n\
        \  type r@\n\
         @\n\
        \  val codec : r Wire.Codec.t@\n\
         end) : sig@\n"
        codec;
      Option.iter (V.mli ppf ~target:`OCaml) views;
      Fmt.pf ppf
        "@\n\
         val min_size : int@\n\
         (** [min_size] is the fewest bytes a record takes. *)@\n\
         @\n\
         val validate : bytes -> int -> (unit, Wire.Error.t) result@\n\
         (** [validate buf off] is [Wire.Codec.validate C.codec buf off]. *)@\n\
         @\n";
      Interface.scan_members ppf ~target:`OCaml ~plan ~folds;
      Fmt.pf ppf
        "end@\n\
         (** [Make (C)] is the twin of the codec [C.codec]. Raises \
         [Invalid_argument] unless [C.codec] has the shape the module was \
         generated from. *)@\n")
    ()
