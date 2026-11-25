open Wire.Private
open Planner
open Record_module

(* {1 The module of a codec}

   One module per codec, whatever its shape. Its parts are written from the
   codec's plan, each where the plan has it: the folds, the check and the
   views of any codec the walk covers ([Scan_module], [View_module]), and the
   members a fixed record of named scalars adds ([Record_module]). *)

type target = [ `OCaml | `OxCaml ]
type require = [ `Record | `Walk | `Any ]
type buffer = [ `Bytes | `Bigstring ]

type walk = {
  plan : plan;
  selected : output list;
  step : string option;
  views : View_module.t;
}

type t = {
  codec : string;
  name : string;
  digest : string;
  target : target;
  buffer : buffer;
  simd : bool;
  make : (string * string) option;
  maps : (string * string * typ) list;
  record : (plan * int) option;
  walk : walk option;
}

(* The record's members cover a record of named scalars at constant offsets,
   with its size: the plan's size is the codec's. *)
let record_part ~maps c =
  match record_plan ~maps c with
  | exception Unsupported reason -> Error reason
  | _, size when Wire.Codec.wire_size_opt c <> Some size ->
      Error "its size is not fixed"
  | plan, size -> Ok (plan, size)

(* The walk covers a codec of any shape in its scope, with no map: the folds
   hand out each field's value on the wire. *)
let walk_part ~maps c =
  if maps <> [] then Error "a map, which the folds do not take"
  else
    match Scan_module.scan_plan c with
    | exception Unsupported reason -> Error reason
    | plan -> Ok plan

(* The record's names: its own, those [Get]'s reads call, and each field's.
   A later definition would shadow an earlier one of the same name. *)
let record_names (plan, _) =
  let calls, fields = field_names (all_fields plan) in
  List.map (fun n -> (`Own, n)) (own_names @ calls)
  @ List.concat_map
      (fun (f, names) -> List.map (fun n -> (`Field f, n)) names)
      fields

(* The values the record's members define; a type or a module takes no
   accessor's name. *)
let record_values =
  List.filter (fun n -> n <> "t" && n = String.uncapitalize_ascii n) own_names

let clash ~codec names =
  match Names.clash names with
  | Some (`Own, `Field f, n) | Some (`Field f, `Own, n) ->
      Fmt.kstr (refuse ~codec)
        "field %S defines %s, which the generated module defines for itself" f n
  | Some (`Field g, `Field f, n) ->
      Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
  | Some (`Own, `Own, _) | None -> ()

(* A record whose members would define a name twice has none, unless they
   are what the caller needs; a map needs a constructor the caller names. *)
let checked_record ~codec ~needed ~make record =
  let record =
    match record with
    | Error _ -> None
    | Ok r when needed ->
        clash ~codec (record_names r);
        Some r
    | Ok r -> if Names.clash (record_names r) = None then Some r else None
  in
  (match record with
  | Some (plan, _)
    when Option.is_none make
         && List.exists (fun f -> Option.is_some f.map) (all_fields plan) ->
      refuse ~codec
        "a field is a map, and the generated record cannot name its type; pass \
         ~make"
  | _ -> ());
  record

(* The walk's outputs and views. Accessors are named apart from the module's
   values, but not from each other nor from the names a field defines in the
   record's members. *)
let checked_walk ~codec ~record ?fields ?step c = function
  | Error _ -> None
  | Ok plan ->
      let reserved = if Option.is_none record then [] else record_values in
      let selected, views =
        match
          (Scan_module.selected plan fields, View_module.of_codec ~reserved c)
        with
        | sv -> sv
        | exception Unsupported reason -> refuse ~codec reason
      in
      let accessors =
        List.map
          (fun m ->
            (`Field (View_module.member_name m), View_module.member_name m))
          views.root.members
      in
      let fields =
        match record with
        | None -> []
        | Some r ->
            List.filter
              (function `Own, _ -> false | _ -> true)
              (record_names r)
      in
      clash ~codec (accessors @ fields);
      Some { plan; selected; step; views }

let v ?(target = `OCaml) ?(buffer = `Bytes) ?(simd = false) ?make ?(maps = [])
    ?fields ?step ~require ~codec c =
  let typs = List.map (fun (path, _, typ) -> (path, typ)) maps in
  let record = record_part ~maps:typs c in
  (* The walk reads [bytes]: a module on a bigarray is a fixed record's. *)
  let require, walk =
    match buffer with
    | `Bytes -> (require, walk_part ~maps c)
    | `Bigstring -> (`Record, Error "the walk reads bytes")
  in
  (match (require, record, walk) with
  | `Record, Error reason, _ | `Walk, _, Error reason -> refuse ~codec reason
  | `Any, Error _, Error reason -> refuse ~codec reason
  | _ -> ());
  (* The record's members are needed where the caller asks for them, and
     for any module of a fixed record of named scalars but the walk's. *)
  let needed =
    match (require, record) with
    | `Record, _ | `Any, Ok _ -> true
    | `Walk, _ | `Any, Error _ -> false
  in
  let record = checked_record ~codec ~needed ~make record in
  if simd && target <> `OxCaml then
    refuse ~codec "~simd needs the OxCaml target";
  let walk = checked_walk ~codec ~record ?fields ?step c walk in
  let plan =
    match (record, walk) with
    | Some (plan, _), _ -> plan
    | None, Some w -> w.plan
    | None, None -> assert false
  in
  {
    codec;
    name = plan.record;
    digest = Gen.digest c;
    target;
    buffer;
    simd;
    make;
    maps;
    record;
    walk;
  }

let wide t =
  (match t.record with Some (plan, _) -> plan.wide | None -> false)
  || match t.walk with Some w -> w.plan.wide | None -> false

(* {1 The implementation} *)

let text f =
  let b = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer b in
  f ppf;
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* The record's members, then the walks and the views. The walks' functions
   are named apart from every name the record defines. *)
(* The accessor of each field of a fixed record in the module's views, found
   by the field's name: the views plan the record apart, under variables of
   their own. *)
let accessor t =
  Option.map
    (fun w (f : field) ->
      let found =
        List.find_map
          (function
            | View_module.Scalar { name; field } when field.name = f.name ->
                Some name
            | View_module.Bytes { name; label; _ } when label = f.label ->
                Some name
            | _ -> None)
          w.views.root.members
      in
      match found with
      | Some n -> n
      | None -> Fmt.invalid_arg "Writer: no accessor for %s" f.name)
    t.walk

let body ?decide t =
  let accessor = accessor t in
  let record, after =
    match t.record with
    | None -> ("", "")
    | Some (plan, size) -> (
        let make = Option.map fst t.make in
        ( text (fun ppf ->
              pp_maps ppf (all_fields plan);
              pp_body ~simd:t.simd ?accessor ~buffer:t.buffer ppf
                ~target:t.target ~make ~plan ~size),
          match accessor with
          | None -> ""
          | Some accessor ->
              text (fun ppf ->
                  pp_after_views ppf ~target:t.target ~make ~accessor ~plan
                    ~size) ))
  in
  let walk, views, views_mask32 =
    match t.walk with
    | None -> ("", "", false)
    | Some w ->
        let taken = Hashtbl.create 64 in
        let take n = Hashtbl.replace taken n () in
        if Option.is_some t.record then List.iter take own_names;
        List.iter take View_module.root_names;
        List.iter
          (fun m -> take (View_module.member_name m))
          w.views.root.members;
        let walk =
          Scan_module.walks ?decide ~target:t.target ~selected:w.selected
            ?step:w.step ?size:w.views.size ~taken w.plan
        in
        (* Without a record's members, the module decodes and encodes the
           codec's values through its views. *)
        let values =
          if Option.is_some t.record then None
          else
            Some
              (fun (r : View_module.record) ->
                text (fun ppf ->
                    if r.module_name = None then
                      Value_module.ml_root ppf w.views r
                    else Value_module.ml_record ppf w.views r))
        in
        let views, mask32 = View_module.ml ?values ~target:t.target w.views in
        let views =
          if Option.is_some values then
            Value_module.helpers ~uses:(uses views) ^ views
          else views
        in
        (walk, Scan_module.without_dead_bindings views, mask32)
  in
  (record ^ walk, views ^ after, views_mask32)

let ml ?decide t =
  let code, views, _ = body ?decide t in
  let all = code ^ views in
  (* No accessor takes a helper's name. *)
  let uses name = uses all name in
  text (fun ppf ->
      pp_header ppf ~codec:t.codec ~name:t.name ~digest:t.digest ~uses;
      (* On OxCaml a 32-bit or 64-bit value is held unboxed. *)
      List.iter
        (fun m ->
          if Scan_module.mentions (m ^ ".") all || Scan_module.mentions m all
          then Fmt.pf ppf "module %s = Wire_runtime.Target.%s@\n" m m)
        [ "Int32_u"; "Int64_u" ];
      Fmt.pf ppf "@\n";
      pp_wide ppf ~codec:t.codec ~wide:(wide t) ~uses;
      (* A 32-bit integer is an [int] where the module loads. *)
      List.iter
        (fun (name, m) ->
          if uses name then
            Fmt.pf ppf
              "let %s : (Wire_runtime.%s.t, int) Type.eq =@\n\
              \  match Wire_runtime.%s.int_eq with@\n\
              \  | Some eq -> eq@\n\
              \  | None -> invalid_arg \"%s: no 32-bit int\"@\n\
               @\n"
              name m m t.name)
        [ ("uint32_eq", "UInt32"); ("sint32_eq", "SInt32") ];
      Fmt.pf ppf "%s" all)

(* {1 The interface} *)

let mli t =
  text (fun ppf ->
      (match (t.record, t.walk) with
      | Some (plan, size), _ ->
          Interface.record_preamble ppf ~target:t.target ~simd:t.simd
            ~make:t.make ~maps:t.maps ~codec:t.codec ~plan ~size
      | None, Some w ->
          Interface.scan_preamble ppf ~codec:t.codec ~plan:w.plan
            ~selected:w.selected
      | None, None -> ());
      Interface.pp_digest t.target ppf;
      let views = Option.is_some t.walk in
      Option.iter
        (fun (plan, size) ->
          Interface.record_members ~views ~buffer:t.buffer ppf ~target:t.target
            ~simd:t.simd ~make:t.make ~maps:t.maps ~codec:t.codec ~plan ~size)
        t.record;
      Option.iter
        (fun w ->
          Interface.scan_members ppf ~target:t.target ~selected:w.selected
            ~step:w.step;
          let values =
            if Option.is_some t.record then None
            else
              Some
                (fun ppf (r : View_module.record) ->
                  if r.module_name = None then
                    Value_module.mli_root ppf ~target:t.target w.views r
                  else Value_module.mli_record ppf ~target:t.target w.views r)
          in
          View_module.mli ?values ppf ~target:t.target w.views;
          Option.iter
            (fun (plan, size) ->
              Interface.record_after_views ppf ~target:t.target ~simd:t.simd
                ~make:t.make ~maps:t.maps ~codec:t.codec ~plan ~size)
            t.record)
        t.walk)

let views t = Option.map (fun w -> w.views) t.walk
