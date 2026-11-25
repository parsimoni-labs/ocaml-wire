open Wire.Private
open Layout
module T = Types
open Planner
open Emitter
open Record_module
open Scan_module

(* The generator is built in phases, one module each, in dependency order:
   [Planner] (a codec read as a plan of nodes), [Emitter] (the checks and reads
   of a plan as OCaml text), then the parts of the one module of a codec,
   [Record_module] (a fixed record's members), [Scan_module] (the walks and
   folds) and [View_module] (the views), which [Writer] puts together with
   their interface ([Interface]). This module holds the interface. *)

type target = [ `OCaml | `OxCaml ]
type map = { path : string; ocaml : string; typ : Planner.typ }

let map path ~typ m = { path; ocaml = typ; typ = Typ m }
let ident = ident
let named = Option.map (List.map (fun m -> (m.path, m.ocaml, m.typ)))

type buffer = [ `Bytes | `Bigstring ]

let outputs = scanner_fields

type view = { module_name : string option; members : view_member list }

and view_member =
  | Int of { name : string; output : string; carrier : string option }
  | Bool of { name : string; output : string }
  | Record of { name : string; presence : string option; view : view }
  | Bytes of { name : string; slice : bool }

(* The outputs of a record reached by the path [prefix] are named after it,
   with the field's name as the folds hand it out, unescaped. *)
let views c =
  let t =
    Option.get
      (Writer.views
         (Writer.v
            ~folds:[ { Writer.name = "fold"; outputs = None; visitor = None } ]
            ~require:`Walk ~codec:(Gen.to_struct c).name c))
  in
  let global prefix name = if prefix = "" then name else prefix ^ "_" ^ name in
  let rec view prefix (r : View_module.record) =
    {
      module_name = r.module_name;
      members =
        List.map
          (function
            | View_module.Scalar { name; field } ->
                let o = Option.get field.output in
                Int
                  {
                    name;
                    output = global prefix o.name;
                    carrier = Option.map fst (View_module.carrier field);
                  }
            | View_module.Bytes { name; slice; _ } -> Bytes { name; slice }
            | Present { name; presence } ->
                Bool { name; output = global prefix presence.name }
            | Sub { name; sub; presence } ->
                let path = String.concat "_" (Option.get sub.path) in
                Record
                  {
                    name;
                    presence =
                      Option.map
                        (fun (o : output) -> global prefix o.name)
                        presence;
                    view =
                      view (global prefix path)
                        (List.find
                           (fun (r : View_module.record) -> r.id = sub.codec_id)
                           t.records);
                  })
          r.members;
    }
  in
  view "" t.root

let fstar ?fields ~codec c =
  try Fstar_module.modules ?fields ~codec c
  with Unsupported why -> refuse ~codec why

(* {1 The generator} *)

type fold = Writer.fold

let fold ?outputs name = { Writer.name; outputs; visitor = None }

(* The compiler the generator runs on, which is the one that builds what it
   writes. *)
let building : target =
  match Sys.ocaml_release.extra with Some (_, "ox") -> `OxCaml | _ -> `OCaml

type t =
  | T : {
      name : string;
      target : target;
      buffer : buffer;
      simd : bool;
      folds : fold list option;
      maps : map list option;
      codec : 'r Wire.Codec.t;
    }
      -> t

(* The module of a visitor's source file: [flow_step.ml] is [Flow_step]. *)
let visitor_module file =
  let base = Filename.basename file in
  let stem =
    match String.index_opt base '.' with
    | Some i -> String.sub base 0 i
    | None -> base
  in
  String.capitalize_ascii stem

let writer ?(visitors = []) (T t) =
  let folds =
    Option.map
      (List.map (fun (f : fold) ->
           match List.assoc_opt f.name visitors with
           | None -> f
           | Some file -> { f with visitor = Some (visitor_module file) }))
      t.folds
  in
  (match t.folds with
  | Some folds ->
      List.iter
        (fun (n, _) ->
          if not (List.exists (fun (f : fold) -> f.name = n) folds) then
            Fmt.invalid_arg "Wire_ocaml: %s declares no fold %S" t.name n)
        visitors
  | None -> ());
  let require =
    if t.simd || Option.is_some t.maps then `Record
    else match t.folds with Some (_ :: _) -> `Walk | _ -> `Any
  in
  Writer.v ~target:t.target ~buffer:t.buffer ~simd:t.simd ?maps:(named t.maps)
    ?folds ~require ~codec:(Gen.to_struct t.codec).name t.codec

let v ?name ?(target = building) ?(buffer = `Bytes) ?(simd = false) ?folds ?maps
    codec =
  let name =
    match name with
    | Some n -> n
    | None ->
        String.lowercase_ascii
          (View_module.module_of_codec (Gen.to_struct codec).name)
  in
  let t = T { name; target; buffer; simd; folds; maps; codec } in
  (* Refused when declared, not when printed. *)
  ignore (writer t : Writer.t);
  t

let name (T t) = t.name

let to_ml ?(twin = false) ?visitors t =
  let w = writer ?visitors t in
  if twin then Writer.twin_ml w else Writer.ml w

let to_mli ?(twin = false) ?visitors t =
  let w = writer ?visitors t in
  if twin then Writer.twin_mli w else Writer.mli w

(* {2 The generator's command line} *)

let usage =
  "usage: gen.exe [--visitor FOLD=FILE]... FILE...\n\
   Writes each FILE, <name>.ml or <name>.mli, the module or the interface of \
   the module <name>, and <name>_codec.ml or <name>_codec.mli, its interpreted \
   twin; FOLD of each written module calls the visitor of the source file FILE \
   by name."

let fail fmt =
  Fmt.kstr
    (fun msg ->
      Fmt.epr "%s: %s@.%s@." Sys.executable_name msg usage;
      exit 2)
    fmt

(* The visitors and the files a command line names. *)
let rec parse visitors files = function
  | [] -> (List.rev visitors, List.rev files)
  | "--visitor" :: v :: rest -> (
      match String.index_opt v '=' with
      | Some i ->
          let fold = String.sub v 0 i
          and file = String.sub v (i + 1) (String.length v - i - 1) in
          parse ((fold, file) :: visitors) files rest
      | None -> fail "--visitor %s: expected FOLD=FILE" v)
  | "--visitor" :: [] -> fail "--visitor needs FOLD=FILE"
  | file :: rest -> parse visitors (file :: files) rest

(* The module a file is written from and which of its texts it holds:
   [stack.mli] is the interface of [stack]. *)
let target_of file =
  let base = Filename.basename file in
  let twin = "_codec" in
  let unit stem =
    if String.ends_with ~suffix:twin stem then
      (String.sub stem 0 (String.length stem - String.length twin), true)
    else (stem, false)
  in
  match String.split_on_char '.' base with
  | stem :: "ml" :: _ -> (unit stem, `Ml)
  | stem :: "mli" :: _ -> (unit stem, `Mli)
  | _ -> fail "%s: not a .ml or .mli file" file

let write file text =
  let oc = open_out_bin file in
  output_string oc text;
  close_out oc

let main ts =
  let visitors, files = parse [] [] (List.tl (Array.to_list Sys.argv)) in
  if files = [] then fail "no file to write";
  let written =
    List.map
      (fun file ->
        let (stem, twin), kind = target_of file in
        match List.find_opt (fun t -> name t = stem) ts with
        | None ->
            fail "%s: no module %s among %s" file stem
              (String.concat ", " (List.map name ts))
        | Some t -> (file, (kind, twin), t))
      files
  in
  List.iter
    (fun (fold, file) ->
      if
        not
          (List.exists
             (fun (_, _, T t) ->
               List.exists
                 (fun (f : fold) -> f.name = fold)
                 (Option.value ~default:[] t.folds))
             written)
      then
        fail "--visitor %s=%s: no module written declares a fold %s" fold file
          fold)
    visitors;
  List.iter
    (fun (file, kind, (T t as m)) ->
      let visitors =
        List.filter
          (fun (fold, _) ->
            List.exists
              (fun (f : fold) -> f.name = fold)
              (Option.value ~default:[] t.folds))
          visitors
      in
      match kind with
      | `Ml, twin -> write file (to_ml ~twin ~visitors m)
      | `Mli, twin -> write file (to_mli ~twin ~visitors m))
    written

(* The module of a codec, as [Plan] reads it. *)
let declared = v

(* {1 The plan, for other backends} *)

module Plan = struct
  type nonrec kind = kind = Constraint | Enum of int list | Tag
  type nonrec check = check = { cond : bool T.expr; kind : kind }

  type nonrec output = output = {
    id : string;
    name : string;
    is_bool : bool;
    wire_width : int;
    signed : bool;
  }

  type nonrec field = field = {
    id : string;
    name : string;
    label : string;
    offset : int;
    store : store;
    checks : check list;
    enum : (string * int list) option;
    map : string option;
    output : output option;
  }

  type nonrec node = node =
    | Prefix of field list * int
    | Span of int T.expr
    | Gate of bool T.expr * output * output list * node list
    | Region of int T.expr * bool * node list
    | Test of bool T.expr

  type t = plan

  module Facts = struct
    type t = facts

    let empty = no_facts
    let assume = assume
    let simplify = simplify
  end

  let v c =
    match codec_plan c with
    | plan -> plan
    | exception Unsupported why -> invalid_arg why

  let nodes t = t.nodes
  let outputs t = t.outputs

  type nonrec pos = pos = { var : string option; at : int }
  type values = Decision.values

  type bound = Decision.bound =
    | Next
    | Hoisted
    | Checked of {
        negative : bool;
        cancelled : (int T.expr * int T.expr) option;
      }

  type choice = Decision.choice =
    | Run of int option
    | If of bool T.expr * choice * choice

  type join = Decision.join = Joined | Copied
  type short_frames = Decision.short_frames = Apart | Inline | Walked

  type decision = Decision.t =
    | Walk of { name : string; values : values; known : int }
    | Prefix_bound of { at : pos; window : string; bytes : int; tested : bool }
    | Span_bound of {
        size : int T.expr;
        at : pos;
        window : string;
        size_var : string;
        bound : bound;
      }
    | Shared_words of {
        at : int;
        constant : bool;
        lo : int;
        hi : int;
        window : string;
        fields : (int * int) list;
        held : int list;
        loads : int list;
      }
    | Word_range of {
        at : pos;
        window : string;
        first : string;
        second : string;
        lo : int;
        hi : int;
      }
    | Dispatch of {
        at : pos;
        window : string;
        cases : int;
        conditions : bool T.expr list;
        choice : choice;
        join : join;
        hoisted : int T.expr option;
      }
    | Cut of { name : string; window : string; known : int }
    | Frame_loop of {
        name : string;
        need : int;
        frames : int;
        short : short_frames option;
        ahead : (string * int * int) list;
      }

  let pp_decision = Decision.pp

  let decisions ?(target = `OCaml) ?(folds = [ fold "fold" ]) ?visitors c =
    let acc = ref [] in
    let decide d = acc := d :: !acc in
    ignore (Writer.ml ~decide (writer ?visitors (declared ~target ~folds c)));
    List.rev !acc

  let select t names =
    match selected t names with
    | outputs -> outputs
    | exception Unsupported why -> invalid_arg why

  let slot_depth = slot_depth
  let slots t outputs = slots t outputs

  let live t outputs =
    let live = live t ~outputs:(List.map (fun (o : output) -> o.id) outputs) in
    fun v -> Sset.mem v live

  module Scope = struct
    type nonrec t = { ranges : (int * int) Smap.t; facts : facts }

    let v (plan : plan) = { ranges = plan.ranges; facts = no_facts }
    let facts s = s.facts

    let refine s conditions =
      let ranges, facts = refined s.ranges s.facts conditions in
      { ranges; facts }

    let range s e = range s.ranges e
    let wide s e = past_native s.ranges e
    let nonnegative s e = nonnegative_in s.ranges s.facts e
  end

  let conditions f = List.map (fun c -> c.cond) f.checks
  let word_range = word_range
  let range (t : t) e = range t.ranges e

  type tree = Dispatch_tree.tree = { tid : int; node : tnode }

  and tnode = Dispatch_tree.tnode =
    | Leaf of int option
    | Branch of bool T.expr * tree * tree

  let gates facts nodes =
    Dispatch_tree.gate_run ~lower:(simplify facts) facts nodes

  let decide = Dispatch_tree.decide

  let shared tree =
    let shared, case_shared, _ = Dispatch_tree.reach tree in
    (shared, case_shared)
end
