open Wire.Private
open Planner
open Record_module

(* {1 The module of a codec}

   One module per codec, whatever its shape. Its parts are written from the
   codec's plan, each where the plan has it: the fold, the check and the
   views of any codec the walk covers ([Scan_module], [View_module]), and the
   members a fixed record of named scalars adds ([Record_module]). *)

type target = [ `OCaml | `OxCaml ]
type require = [ `Record | `Walk | `Any ]

(* The fold: the paths it was declared with, and what its walk hands [f]. *)
type fold = {
  paths : string list;
  scan : Scan_module.fold;
  types : string list;
}

type walk = { plan : plan; fold : fold option; views : View_module.t }

type t = {
  codec : string;
  name : string;
  target : target;
  simd : bool;
  maps : (string * string * typ) list;
  record : (plan * int) option;
  walk : walk option;
}

(* The record's members cover a record of named scalars at constant offsets,
   with its size: the plan's size is the codec's. *)
let record_part ~maps c =
  match record_plan ~maps c with
  | exception Unsupported reason -> Error reason
  | _, size when Wire.Codec.fixed_size c <> Some size ->
      Error "its size is not fixed"
  | plan, size -> Ok (plan, size)

(* The walk covers a codec of any shape in its scope, with no map: the fold
   hands out each field's value on the wire. *)
let walk_part ~maps c =
  if maps <> [] then Error "a map, which the walks do not take"
  else
    match Scan_module.scan_plan c with
    | exception Unsupported reason -> Error reason
    | plan -> Ok plan

(* Two fields whose labels are one OCaml name would define it twice, in the
   record type and as accessors. *)
let clash ~codec (plan, _) =
  let names =
    List.map (fun (f : field) -> (f.name, f.label)) (all_fields plan)
  in
  match Names.clash names with
  | Some (g, f, n) ->
      Fmt.kstr (refuse ~codec) "fields %S and %S both define %s" g f n
  | None -> ()

(* The values the record's members define; a type or a module takes no
   accessor's name. *)
let record_values =
  List.filter (fun n -> n <> "t" && n = String.uncapitalize_ascii n) own_names

(* The names the walks of a fold define, which no accessor takes. *)
let fold_names =
  let s = Scan_module.step in
  [ s; s ^ "_within"; s ^ "_index"; s ^ "_frames"; s ^ "_frames_short" ]

(* {2 The fold's fields}

   A path names a field by the accessor names from the codec down. The walk
   hands out its value, and the presences that make it an option: its own,
   when the field is optional, and one more through a layer, absent where the
   innermost optional layer of the path is. *)

(* Each output of [nodes] with its field and the presences of the optional
   fields around it, outermost first. *)
let rec placed around = function
  | [] -> []
  | Gate (_, p, _, body) :: rest ->
      placed (around @ [ p ]) body @ placed around rest
  | Region (_, _, body) :: rest -> placed around body @ placed around rest
  | Prefix (fields, _) :: rest ->
      List.filter_map
        (fun (f : field) ->
          Option.map (fun (o : output) -> (o.name, (o, f, around))) f.output)
        fields
      @ placed around rest
  | (Span _ | Test _) :: rest -> placed around rest

let fold_arg ~codec (plan : plan) (views : View_module.t) path =
  let refuse fmt = Fmt.kstr (refuse ~codec) fmt in
  let global prefix n = if prefix = "" then n else prefix ^ "_" ^ n in
  let member (r : View_module.record) c =
    List.find_opt (fun m -> View_module.member_name m = c) r.members
  in
  let rec go prefix r layers = function
    | [] -> refuse "the fold path %S names no field" path
    | [ leaf ] -> (
        match member r leaf with
        | Some (View_module.Scalar { field = { output = Some o; _ }; _ }) ->
            (global prefix o.name, layers)
        | Some _ -> refuse "the fold path %S names a field no fold reads" path
        | None -> refuse "the fold path %S names no field %S" path leaf)
    | c :: rest -> (
        match member r c with
        | Some
            (View_module.Sub { sub = { path = Some p; _ } as sub; presence; _ })
          ->
            let layer =
              Option.map (fun (o : output) -> global prefix o.name) presence
            in
            go
              (global prefix (String.concat "_" p))
              (View_module.record_of_sub views sub)
              (Option.to_list layer @ layers)
              rest
        | Some _ ->
            refuse "the fold path %S goes through %S, which holds no record"
              path c
        | None -> refuse "the fold path %S names no field %S" path c)
  in
  let steps = String.split_on_char '.' path in
  let name, layers = go "" views.root [] steps in
  let o, f, around =
    match List.assoc_opt name (placed [] plan.nodes) with
    | Some x -> x
    | None -> refuse "the fold path %S names a field no fold reads" path
  in
  let innermost p = List.find_opt p (List.rev around) in
  let is_layer (g : output) = List.mem g.name layers in
  let own =
    match List.rev around with
    | g :: _ when not (is_layer g) -> Some g
    | _ -> None
  in
  let layered = List.length steps > 1 in
  let arg =
    {
      Scan_module.value = o;
      typed = View_module.typed f;
      own;
      layered;
      layer = innermost is_layer;
    }
  in
  let typ =
    View_module.carrier_type f
    ^ (if Option.is_some own then " option" else "")
    ^ if layered then " option" else ""
  in
  (arg, typ)

let fold_of ~codec plan views paths =
  let args = List.map (fold_arg ~codec plan views) paths in
  let used =
    List.concat_map
      (fun ((a : Scan_module.arg), _) ->
        (a.value :: Option.to_list a.own) @ Option.to_list a.layer)
      args
  in
  let selected =
    List.filter
      (fun (o : output) -> List.exists (fun (u : output) -> u.id = o.id) used)
      plan.outputs
  in
  {
    paths;
    scan = { selected; args = List.map fst args };
    types = List.map snd args;
  }

(* The walk's fold and views. Accessors are named apart from the module's
   values and from the walks of its fold. *)
let checked_walk ~codec ~record ~fold c = function
  | Error _ -> None
  | Ok plan ->
      let reserved =
        (if Option.is_none record then [] else record_values)
        @ if Option.is_none fold then [] else fold_names
      in
      let views =
        match View_module.of_codec ~reserved c with
        | views -> views
        | exception Unsupported reason -> refuse ~codec reason
      in
      let fold = Option.map (fold_of ~codec plan views) fold in
      Some { plan; fold; views }

let v ?(target = `OCaml) ?(simd = false) ?(maps = []) ?fold ~require ~codec c =
  let typs = List.map (fun (path, _, typ) -> (path, typ)) maps in
  let record = record_part ~maps:typs c in
  let walk = walk_part ~maps c in
  let require = if Option.is_some fold then `Walk else require in
  (match (require, record, walk) with
  | `Record, Error reason, _ | `Walk, _, Error reason -> refuse ~codec reason
  | `Any, Error _, Error reason -> refuse ~codec reason
  | _ -> ());
  let record = Result.to_option record in
  Option.iter (clash ~codec) record;
  if simd && target <> `OxCaml then
    refuse ~codec "~simd needs the OxCaml target";
  let walk = checked_walk ~codec ~record ~fold c walk in
  let plan =
    match (record, walk) with
    | Some (plan, _), _ -> plan
    | None, Some w -> w.plan
    | None, None -> assert false
  in
  { codec; name = plan.record; target; simd; maps; record; walk }

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

(* The accessor of each field in a module with no walk. *)
let record_accessors plan =
  let names = accessors ~reserved:View_module.root_names (all_fields plan) in
  fun (f : field) -> List.assoc f.label names

(* The walks, then the record's members and the views, then the fold: the
   walks' functions come first, as in the F* rendering they are compared
   with, and are named apart from every name the record defines; the fold
   calls the module's [validate] and [check]. *)
let body ?decide t =
  (* A record's [size] is a function of the record, as for any shape, bound
     last: the module's own code reads the constant. *)
  let size =
    match t.record with Some _ -> "let size (_ : t) = min_size\n" | None -> ""
  in
  let record =
    match t.record with
    | None -> ""
    | Some (plan, size) ->
        let maps = List.map (fun (path, ocaml, _) -> (path, ocaml)) t.maps in
        let views =
          if Option.is_some t.walk then None
          else Some (t.name, record_accessors plan)
        in
        text (fun ppf ->
            pp_maps ppf (all_fields plan);
            pp_body ~simd:t.simd ?views ~maps ppf ~target:t.target ~plan ~size)
  in
  let walk, views, fold =
    match t.walk with
    | None -> ("", "", "")
    | Some w ->
        let taken = Hashtbl.create 64 in
        let take n = Hashtbl.replace taken n () in
        if Option.is_some t.record then List.iter take own_names;
        List.iter take View_module.root_names;
        List.iter
          (fun m -> take (View_module.member_name m))
          w.views.root.members;
        let walk =
          Scan_module.walks ?decide ~target:t.target
            ?fold:(Option.map (fun f -> f.scan) w.fold)
            ?size:w.views.size ~taken w.plan
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
        let ends = Option.is_some w.fold && Option.is_none w.views.size in
        let views, _ = View_module.ml ?values ~ends ~target:t.target w.views in
        let views =
          if Option.is_some values then
            Value_module.helpers ~uses:(uses views) ^ views
          else views
        in
        let fold =
          match w.fold with
          | None -> ""
          | Some _ ->
              text (fun ppf ->
                  Scan_module.pp_fold ppf ~name:t.name ~size:w.views.size)
        in
        (walk, Scan_module.without_dead_bindings views, fold)
  in
  walk ^ record ^ views ^ size ^ fold

let ml ?decide t =
  let all = body ?decide t in
  (* No accessor takes a helper's name. *)
  let uses name = uses all name in
  text (fun ppf ->
      pp_header ppf ~codec:t.codec ~name:t.name ~uses;
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
            ~maps:t.maps ~codec:t.codec ~plan ~size
      | None, Some w -> Interface.scan_preamble ppf ~codec:t.codec ~plan:w.plan
      | None, None -> ());
      Option.iter
        (fun (plan, size) ->
          let accessor =
            if Option.is_some t.walk then None else Some (record_accessors plan)
          in
          Interface.record_members ?accessor ppf ~target:t.target ~simd:t.simd
            ~maps:t.maps ~codec:t.codec ~plan ~size)
        t.record;
      Option.iter
        (fun w ->
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
            (fun f ->
              Interface.pp_fold ppf ~target:t.target ~paths:f.paths
                ~types:f.types)
            w.fold)
        t.walk)

let views t = Option.map (fun w -> w.views) t.walk
