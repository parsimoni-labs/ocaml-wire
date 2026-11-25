open Wire.Private
open Layout
module T = Types
open Planner
open Emitter

(* {1 The records}

   Each record a scan reaches has a view: the root's, at the top of the
   scanner, and one module per codec a field holds, named after the codec
   and written once however many fields hold it. A record is planned on its
   own, each record a field of it holds kept as a span of a variable standing
   for its size ([Planner.plan_apart]), so the plan of a record holds its own
   fields only. *)

type member =
  | Scalar of { name : string; field : field }
  | Present of { name : string; presence : output }
  | Sub of { name : string; sub : sub; presence : output option }
  | Bytes of { name : string; label : string; at : bytes_at; slice : bool }

and bytes_at = Fixed of field | Sized of node

type record = {
  id : int;
  codec_name : string;
  module_name : string option;
  struct_ : T.struct_;
  plan : plan;
  subs : sub list;
  fixed : int;
  fixed_size : int option;
  members : member list;
}

type t = { root : record; records : record list }

(* The values the module of every codec exports at its top level, and a
   view module in its own: an accessor of that name would be a second value
   of the module. *)
let root_names =
  [
    "check";
    "ok";
    "get";
    "digest";
    "fold";
    "fold_frames";
    "fold_step";
    "fold_frames_step";
    "fold_index_step";
    "fold_unboxed";
    "fold_frames_unboxed";
    "decode";
    "decode_exn";
    "validate";
    "validate_exn";
    "is_valid";
    "encode";
    "size";
    "min_size";
    "pp";
  ]

let sub_names = [ "ok"; "get"; "end_at"; "size"; "pp" ]

(* The helpers the module binds at its top level, which an accessor of that
   name would shadow for the code after it. *)
let helper_names =
  [
    "cast";
    "codec_name";
    "no_error";
    "mask32";
    "uint32_eq";
    "sint32_eq";
    "of_view";
    "encode_at";
    "error_at";
    "first_error";
    "check_pass";
    "refuse";
    "refuse_if";
    "fail";
    "fail_unless";
    "eof_if";
    "field_unless";
  ]

(* The modules the walks define on OxCaml, and those of a fixed record's
   members. *)
let reserved_modules =
  [
    "U";
    "B";
    "L";
    "Int32_u";
    "Int64_u";
    "Value";
    "Get";
    "Valid";
    "View";
    "Field";
    "Global";
    "Bigstring";
    "X4";
  ]

(* [CapturedIPv4] is [Captured_ipv4], [StackUDPHeader] [Stack_udp_header]: a
   word starts at a capital after a small letter or a digit, or at the last
   capital of a run followed by two small letters. A name that would start
   with [_] starts with [R_]. *)
let module_of_codec name =
  let n = String.length name in
  let is_upper c = c >= 'A' && c <= 'Z' and is_lower c = c >= 'a' && c <= 'z' in
  let is_digit c = c >= '0' && c <= '9' in
  let b = Buffer.create (n + 4) in
  String.iteri
    (fun i c ->
      if i > 0 && is_upper c then begin
        let prev = name.[i - 1] in
        let lowers_after =
          i + 2 < n && is_lower name.[i + 1] && is_lower name.[i + 2]
        in
        if is_lower prev || is_digit prev || (is_upper prev && lowers_after)
        then Buffer.add_char b '_'
      end;
      Buffer.add_char b c)
    name;
  let label = Names.ocaml_label (Buffer.contents b) in
  (* A module name starts with a letter. *)
  String.capitalize_ascii (if label.[0] = '_' then "r" ^ label else label)

let fixed_of (s : T.struct_) =
  match (plan_of s).nodes with Prefix (_, n) :: _ -> n | _ -> 0

(* The sub-record a node list is exactly, through a region. *)
let marker subs = function
  | [ Span (T.Ref (T.I, v)) ] | [ Region (_, _, [ Span (T.Ref (T.I, v)) ]) ] ->
      List.find_opt (fun (s : sub) -> s.var = v) subs
  | _ -> None

let path_name = function
  | Some (_ :: _ as p) -> Some (String.concat "_" p)
  | _ -> None

(* Whether the named field of bytes [label] of [s] is a slice, rather than a
   string: the type of the field of that label, through an optional field or
   a sized region. *)
let rec bytes_slice : type a. a T.typ -> bool option = function
  | T.Byte_slice _ -> Some true
  | T.Byte_array _ -> Some false
  | T.Optional { inner; _ } -> bytes_slice inner
  | T.Single_elem { elem; _ } -> bytes_slice elem
  | T.Where { inner; _ } -> bytes_slice inner
  | _ -> None

let slice_of (s : T.struct_) label =
  List.find_map
    (fun (T.Field f) ->
      match f.field_name with
      | Some n when Names.ocaml_label n = label -> bytes_slice f.field_typ
      | _ -> None)
    s.fields

let bytes_member s label at =
  Option.map
    (fun slice -> Bytes { name = label; label; at; slice })
    (slice_of s label)

let rec members_of s subs spans nodes =
  let members_of = members_of s subs spans in
  List.concat_map
    (function
      | Prefix (fields, _) ->
          List.filter_map
            (fun (f : field) ->
              match (f.output, f.store) with
              | Some o, _ when o.name <> "" ->
                  Some (Scalar { name = o.name; field = f })
              | None, Layout.Bytes _ when f.name <> "" && f.label <> "anon" ->
                  bytes_member s f.label (Fixed f)
              | _ -> None)
            fields
      | Span _ as span when List.exists (fun (_, n) -> n == span) spans -> (
          match List.find (fun (_, n) -> n == span) spans with
          | path, _ ->
              Option.to_list
                (bytes_member s (String.concat "_" path) (Sized span)))
      | Span (T.Ref (T.I, v)) -> (
          match List.find_opt (fun (s : sub) -> s.var = v) subs with
          | Some sub -> (
              match path_name sub.path with
              | Some name -> [ Sub { name; sub; presence = None } ]
              | None -> [])
          | None -> [])
      | Span _ | Test _ -> []
      | Region (_, _, body) -> members_of body
      | Gate (_, p, _, body) -> (
          match marker subs body with
          | Some sub -> (
              match path_name sub.path with
              | Some name -> [ Sub { name; sub; presence = Some p } ]
              | None -> [])
          | None ->
              (if p.name = "" then []
               else [ Present { name = p.name; presence = p } ])
              @ members_of body))
    nodes

let member_name = function
  | Scalar { name; _ }
  | Present { name; _ }
  | Sub { name; _ }
  | Bytes { name; _ } ->
      name

(* An accessor takes its field's name, suffixed with ['_'] where that is a
   name the module defines, as a keyword is: [size] is [size_] and [size_]
   [size__], so no two fields take one name. *)
let escape reserved = function
  | Scalar s -> Scalar { s with name = Names.escape reserved s.name }
  | Present p -> Present { p with name = Names.escape reserved p.name }
  | Sub s -> Sub { s with name = Names.escape reserved s.name }
  | Bytes b -> Bytes { b with name = Names.escape reserved b.name }

let record_of ~reserved ~id ~codec_name ~module_name ~fixed_size s =
  let plan, subs, spans = plan_apart s in
  let reserved =
    helper_names
    @ if module_name = None then reserved @ root_names else sub_names
  in
  let members =
    List.map (escape reserved) (members_of s subs spans plan.nodes)
  in
  (* A setter is named after its accessor: [set_x] for [x]. *)
  let names = List.map member_name members in
  List.iter
    (fun n ->
      if List.mem ("set_" ^ n) names then
        fail_unsupported "the fields %s and set_%s both define set_%s" n n n)
    names;
  {
    id;
    codec_name;
    module_name;
    struct_ = s;
    plan;
    subs;
    fixed = fixed_of s;
    fixed_size;
    members;
  }

(* The records under the root, each once, those a record holds before it,
   with a module name of its own. *)
let v ~reserved (s : T.struct_) =
  let root =
    record_of ~reserved ~id:(-1) ~codec_name:s.name ~module_name:None
      ~fixed_size:None s
  in
  let seen = Hashtbl.create 8 and names = Hashtbl.create 8 in
  List.iter (fun n -> Hashtbl.replace names n ()) reserved_modules;
  let order = ref [] in
  let rec visit (r : record) =
    List.iter
      (fun (sub : sub) ->
        if not (Hashtbl.mem seen sub.codec_id) then begin
          Hashtbl.replace seen sub.codec_id ();
          let base = module_of_codec sub.codec_name in
          let rec pick i =
            let n = if i = 1 then base else Fmt.str "%s_%d" base i in
            if Hashtbl.mem names n then pick (i + 1) else n
          in
          let name = pick 1 in
          Hashtbl.replace names name ();
          let r =
            record_of ~reserved ~id:sub.codec_id ~codec_name:sub.codec_name
              ~module_name:(Some name) ~fixed_size:sub.fixed_size sub.codec
          in
          visit r;
          order := r :: !order
        end)
      r.subs
  in
  visit root;
  { root; records = List.rev !order }

let record_of_sub t (sub : sub) =
  List.find (fun r -> r.id = sub.codec_id) t.records

let module_name t sub =
  match (record_of_sub t sub).module_name with
  | Some n -> n
  | None -> assert false

(* {1 Accessors}

   An accessor reads its field, or places its sub-record, from the view [v]:
   a field at a constant offset inside the bytes a view of the record is
   known to hold ([fixed]) is one load; anything else is placed again from
   the bytes that place it, read with a bound check against the buffer, as
   the check placed it, but with none of the record's checks: the bytes may
   have been written since. A presence or a tag is tested again for the same
   reason. A sub-record is placed, then its own fixed bytes are tested
   against the buffer's length: its view then reads them with no check. *)

type target =
  | Field of string
  | Gate of string
  | Marker of string
  | Bytes_span of node
  | End

let rec contains target node =
  match (target, node) with
  | Field id, Prefix (fields, _) ->
      List.exists (fun (f : field) -> f.id = id) fields
  | Marker v, Span (T.Ref (T.I, x)) -> x = v
  | Bytes_span n, (Span _ as m) -> n == m
  | Gate id, Gate (_, p, _, _) when p.id = id -> true
  | _, (Gate (_, _, _, body) | Region (_, _, body)) ->
      List.exists (contains target) body
  | _ -> false

(* The conditions of the optional fields around [target], or of [target]
   itself, outermost first. *)
let rec chain target nodes =
  match List.find_opt (contains target) nodes with
  | Some (Gate (c, p, _, body)) -> (
      match target with
      | Gate id when p.id = id -> [ c ]
      | _ -> c :: chain target body)
  | Some (Region (_, _, body)) -> chain target body
  | _ -> []

type pos = { var : string; at : int }

let ptext p = if p.at = 0 then p.var else Fmt.str "(%s + %d)" p.var p.at

exception Absent

type walk = {
  r : record;
  t : t;
  m : mode;
  target : target;
  out : Buffer.t;
  mutable n : int;
  needed : Sset.t;
  mutable used : Sset.t;
  mutable bound : Sset.t;
  mutable pending : bool T.expr list;
  mutable ranges : (int * int) Smap.t;
  mutable views : (string * int T.expr) list;
      (* A bitfield as an expression over its word. *)
  mutable words : (string * int) list; (* A bitfield word and its mask. *)
  mutable wide_words : int list;
      (* The 64-bit words bound at constant offsets from [v]. *)
  mutable known_at : (string * int) option;
      (* Bytes known to be in the buffer from a variable position. *)
  ends : (int, unit) Hashtbl.t;
  known : int;
  absent : string;
}

let fresh w s =
  w.n <- w.n + 1;
  Fmt.str "%s%d" s w.n

(* Positions are [int]s; values are held as the mode holds them, [int64_u]
   on OxCaml, where a load takes its position as one. *)
let origin w x = if w.m.num = Unboxed then Fmt.str "(U.of_int %s)" x else x
let to_int w v = if w.m.num = Unboxed then Fmt.str "(U.to_int %s)" v else v
let emit w fmt = Fmt.kstr (fun s -> Buffer.add_string w.out s) fmt
let refs e = Sset.of_list (T.collect_refs e)

let text w e =
  w.used <- Sset.union w.used (refs e);
  expr_text w.m ~ranges:w.ranges e

(* A condition, its bitfields read through their words, so that tests of
   bits of one word are one test of a mask, as the check makes them. *)
let cond_text w facts c =
  w.used <- Sset.union w.used (refs c);
  let c = T.subst_expr w.views c in
  expr_text w.m ~ranges:w.ranges
    (simplify facts (Bit_tests.bit_predicate w.words c))

(* Bytes known to be in the buffer from [p]'s variable, and from [p]. *)
let known_from w var =
  if var = "v" then w.known
  else match w.known_at with Some (x, n) when x = var -> n | _ -> 0

let known w p = known_from w p.var - p.at
let fixed_of_sub t sub = (record_of_sub t sub).fixed
let sub_of w v = List.find_opt (fun (s : sub) -> s.var = v) w.r.subs

(* Tests each pending condition whose variables are bound: the target is
   absent where one fails. *)
let flush w facts =
  let ready, later =
    List.partition (fun c -> Sset.subset (refs c) w.bound) w.pending
  in
  w.pending <- later;
  List.fold_left
    (fun facts c ->
      match simplify facts c with
      | T.Bool true -> facts
      | T.Bool false -> raise Absent
      | c' ->
          emit w "if not %s then %s else\n" (cond_text w facts c') w.absent;
          assume (assume facts c true) c' true)
    facts ready

(* The fields of a prefix a later test reads and that are not bound yet. *)
let todo w fields =
  List.filter
    (fun (f : field) -> Sset.mem f.id w.needed && not (Sset.mem f.id w.bound))
    fields

let extent_of fields =
  List.fold_left
    (fun a (f : field) -> max a (f.offset + width_bytes f.store))
    0 fields

(* The bytes from [o] a read of [need] bytes at [p] may load from, and the
   first of them: those known to be there, or else the [need] bytes, tested
   against the buffer here. *)
let room w ~bad p need =
  let kb = known_from w p.var in
  if need <= kb - p.at then (kb, 0)
  else begin
    emit w "if %s > len - %d then %s else\n" (ptext p) need
      (bad ~at:(ptext p) ~need:(string_of_int need));
    (p.at + need, p.at)
  end

(* The 64-bit words at constant offsets from [v] worth loading once for
   [fields] at [p], bound. *)
let wide_words w p ~size fields =
  if p.var <> "v" then []
  else
    let shifted =
      List.map (fun (f : field) -> { f with offset = p.at + f.offset }) fields
    in
    let ws = Wide_loads.words ~unboxed:(w.m.num = Unboxed) ~size shifted in
    List.iter
      (fun a ->
        if not (List.mem a w.wide_words) then begin
          emit w "let w%d = %s in\n" a
            (load w.m ~o:(origin w "v") ~k:a ~bytes:8 ~endian:T.Big);
          w.wide_words <- a :: w.wide_words
        end)
      ws;
    ws

(* Binds the fields of a prefix at [p] a later test reads: a bitfield
   through its word, bound once, and fields that share a 64-bit word at a
   constant offset through it. *)
let bind_prefix w ~bad fields p =
  match todo w fields with
  | [] -> ()
  | todo ->
      let size, lo = room w ~bad p (extent_of todo) in
      let ws = wide_words w p ~size todo in
      let bitwords = Hashtbl.create 4 in
      List.iter
        (fun (f : field) ->
          let value =
            match f.store with
            | Bits { base; shift; width } ->
                let word =
                  match Hashtbl.find_opt bitwords (f.offset, base) with
                  | Some word -> word
                  | None ->
                      let word = fresh w "bw" in
                      let bytes = Bitfield.byte_size base in
                      emit w "let %s = %s in\n" word
                        (unsigned w.m ~size ~lo ~o:(origin w p.var) ~words:ws
                           ~k:(p.at + f.offset) ~bytes ~endian:(bit_endian base)
                           ());
                      Hashtbl.replace bitwords (f.offset, base) word;
                      if 8 * bytes < Sys.int_size then begin
                        let mask = (1 lsl (8 * bytes)) - 1 in
                        w.ranges <- Smap.add word (0, mask) w.ranges;
                        w.words <- (word, mask) :: w.words
                      end;
                      word
                in
                if 8 * Bitfield.byte_size base < Sys.int_size then
                  w.views <-
                    ( f.id,
                      T.Land
                        ( T.Lsr (T.Ref (T.I, word), T.Int shift),
                          T.Int ((1 lsl width) - 1) ) )
                    :: w.views;
                value w.m ~bitword:word ~o:(origin w p.var) ~words:ws ~k:p.at f
            | _ -> value w.m ~size ~lo ~o:(origin w p.var) ~words:ws ~k:p.at f
          in
          emit w "let %s = %s in\n" f.id value;
          w.bound <- Sset.add f.id w.bound)
        todo

let at_var x = { var = x; at = 0 }

(* The bytes the first read at the position after [nodes] starts needs
   there, when that read tests its bound at that very position: one test
   then covers the span before it and the read. *)
let rec next_need w ~top facts nodes =
  match nodes with
  | Test _ :: rest -> next_need w ~top facts rest
  | node :: _ when contains w.target node -> target_need w facts node
  | Gate (c, _, _, _) :: rest when simplify facts c = T.Bool false ->
      next_need w ~top facts rest
  | Prefix (fields, _) :: _ when top -> (
      match todo w fields with [] -> None | todo -> Some (extent_of todo))
  | _ -> None

and target_need w facts = function
  | Gate (_, p, _, body) -> (
      match w.target with
      | Gate id when p.id = id -> None
      | _ -> next_need w ~top:false facts body)
  | Region (_, _, body) -> next_need w ~top:false facts body
  | Span (T.Ref (T.I, v)) -> Option.map (fixed_of_sub w.t) (sub_of w v)
  | Prefix (fields, _) -> (
      match w.target with
      | Field id -> (
          match List.find_opt (fun (f : field) -> f.id = id) fields with
          | Some f when not (Sset.mem f.id w.bound) ->
              Some (f.offset + width_bytes f.store)
          | _ -> None)
      | _ -> None)
  | _ -> None

(* [p] after [size] bytes, checked not negative unless its range proves it,
   as the check refuses a negative size, and to leave the [ahead] bytes a
   read needs right after it in the buffer. *)
let skip w ~bad ~ahead (size : int T.expr) p =
  match size with
  | T.Int k when k >= 0 -> { p with at = extent p.at k }
  | _ ->
      let s = fresh w "s" in
      let n = Option.value ~default:0 ahead in
      let room =
        if n = 0 then Fmt.str "(len - %s)" (ptext p)
        else Fmt.str "(len - %s - %d)" (ptext p) n
      in
      let fail = bad ~at:(ptext p) ~need:s in
      if past_native w.ranges size then begin
        w.used <- Sset.union w.used (refs size);
        emit w "let %s = %s in\n" s (wide_text w.m ~ranges:w.ranges size);
        emit w
          "if %s < 0 || Int64.unsigned_compare %s (Int64.of_int %s) > 0 then \
           %s else\n"
          room s room fail;
        emit w "let %s = Int64.to_int %s in\n" s s
      end
      else begin
        let lo, _ = range w.ranges size in
        emit w "let %s = %s in\n" s (to_int w (text w size));
        emit w "if %s%s > %s then %s else\n"
          (if lo >= 0 then "" else s ^ " < 0 || ")
          s room fail
      end;
      let x = fresh w "p" in
      emit w "let %s = %s + %s in\n" x (ptext p) s;
      if n > 0 then w.known_at <- Some (x, n);
      at_var x

(* [p] after the sub-record [sub] there: its size, or where its walk ends. *)
let sub_end w ~bad (sub : sub) p =
  match sub.fixed_size with
  | Some n -> { p with at = extent p.at n }
  | None ->
      Hashtbl.replace w.ends sub.codec_id ();
      let x = fresh w "p" in
      emit w "let %s = %s.end_at buf len %s in\n" x (module_name w.t sub)
        (ptext p);
      emit w "if %s < 0 then %s else\n" x (bad ~at:(ptext p) ~need:"0");
      at_var x

let rec advance w ~top ~bad facts nodes p =
  match nodes with
  | [] -> (facts, p)
  | node :: rest ->
      let facts, p = step w ~top ~bad ~rest facts node p in
      advance w ~top ~bad facts rest p

and step w ~top ~bad ~rest facts node p =
  match node with
  | Prefix (fields, n) ->
      let facts =
        if top then begin
          bind_prefix w ~bad fields p;
          flush w facts
        end
        else facts
      in
      (facts, { p with at = extent p.at n })
  | Test _ -> (facts, p)
  | Span (T.Ref (T.I, v)) when Option.is_some (sub_of w v) ->
      (facts, sub_end w ~bad (Option.get (sub_of w v)) p)
  | Span size | Region (size, _, _) ->
      (facts, skip w ~bad ~ahead:(next_need w ~top facts rest) size p)
  | Gate (c, _, _, body) -> (
      match simplify facts c with
      | T.Bool false -> (facts, p)
      | T.Bool true -> (facts, snd (advance w ~top:false ~bad facts body p))
      | c' ->
          let x = fresh w "p" in
          emit w "let %s =\nif %s then (\n" x (cond_text w facts c');
          let failed = ref false in
          let inner ~at:_ ~need:_ =
            failed := true;
            "-1"
          in
          let _, q =
            advance w ~top:false ~bad:inner (assume facts c' true) body p
          in
          emit w "%s) else %s in\n" (ptext q) (ptext p);
          (* Only a failure inside the gate answers a negative position. *)
          if !failed then
            emit w "if %s < 0 then %s else\n" x (bad ~at:(ptext p) ~need:"0");
          (facts, at_var x))

(* Writes the walk to the target and what [finish] makes of the position of
   the node that holds it. A presence is known once the conditions around it
   are tested. *)
let locate w ~bad ~finish =
  let presence = match w.target with Gate _ -> true | _ -> false in
  let rec inner facts nodes p =
    match nodes with
    | [] -> finish facts p
    | node :: _ when contains w.target node -> descend facts node p
    | node :: rest ->
        let facts, p = step w ~top:false ~bad ~rest facts node p in
        inner facts rest p
  and descend facts node p =
    match node with
    | Gate (c, _, _, body) ->
        let facts =
          match simplify facts c with
          | T.Bool true -> facts
          | T.Bool false -> raise Absent
          | c' ->
              emit w "if not %s then %s else\n" (cond_text w facts c') w.absent;
              assume facts c' true
        in
        if presence && w.pending = [] then finish facts p
        else inner facts body p
    | Region (_, _, body) -> inner facts body p
    | _ -> finish facts p
  in
  let rec top facts nodes p =
    match nodes with
    | _ when presence && w.pending = [] -> finish facts p
    | [] -> finish facts p
    | node :: _ when contains w.target node -> descend facts node p
    | node :: rest ->
        let facts, p = step w ~top:true ~bad ~rest facts node p in
        top facts rest p
  in
  top (flush w no_facts) w.r.plan.nodes { var = "v"; at = 0 }

(* The body of an accessor of [r], from the view [v] in [buf] of [len]
   bytes. *)
let body ~t ~m ~known ~absent ~bad ~target ~finish r =
  let all =
    Sset.of_list
      (List.concat_map
         (function
           | Prefix (fields, _) -> List.map (fun (f : field) -> f.id) fields
           | _ -> [])
         r.plan.nodes)
  in
  let run needed =
    let w =
      {
        r;
        t;
        m;
        target;
        out = Buffer.create 256;
        n = 0;
        needed;
        used = Sset.empty;
        bound = Sset.empty;
        pending = chain target r.plan.nodes;
        ranges = r.plan.ranges;
        views = [];
        words = [];
        wide_words = [];
        known_at = None;
        ends = Hashtbl.create 4;
        known;
        absent;
      }
    in
    (match locate w ~bad ~finish:(finish w) with
    | () -> ()
    | exception Absent ->
        Buffer.clear w.out;
        emit w "%s\n" absent);
    w
  in
  let first = run all in
  let w = run first.used in
  (Buffer.contents w.out, w.ends)

(* {1 Writing the module} *)

(* The value an accessor returns, read as an [int]: on OxCaml from a buffer
   the caller may hold [local], each load given its distance as a constant.
   flambda2 keeps it untagged where the caller computes on it, where an
   [int64_u] converted to [int] costs a sign extension of each operand. *)
let result_mode target =
  {
    num = Tagged;
    src = (match target with `OxCaml -> local_bytes_src | `OCaml -> bytes_src);
    ox = target = `OxCaml;
    raw32 = false;
    ranged = false;
    live = Sset.empty;
    exported = Sset.empty;
    slots = Sset.empty;
    ahead = Sset.empty;
    fail = (fun _ -> "");
    decide = ignore;
    note = ignore;
  }

(* On OxCaml an accessor holds the values its tests read as the check does,
   as [int64_u], read through the scanner's [L], [B] and [U]; elsewhere as
   [int]s. *)
let mode target =
  let num, src =
    match target with
    | `OxCaml -> (Unboxed, { local_bytes_src with u = "L"; checked = "L" })
    | `OCaml -> (Tagged, bytes_src)
  in
  {
    num;
    src;
    ox = false;
    raw32 = false;
    ranged = false;
    live = Sset.empty;
    exported = Sset.empty;
    slots = Sset.empty;
    ahead = Sset.empty;
    fail = (fun _ -> "");
    decide = ignore;
    note = ignore;
  }

let bad_raise ~at ~need =
  Fmt.str "%s.eof ~at:%s ~expected:%s ~got:(len - %s)" errors at need at

(* A field read with one load and no check: at a constant offset inside the
   bytes a view of [r] holds. *)
let direct (r : record) (f : field) =
  match r.plan.nodes with
  | Prefix (fields, _) :: _ ->
      List.memq f fields && f.offset + width_bytes f.store <= r.fixed
  | _ -> false

(* The function [name] over the buffer [b], with its length [len] when
   [with_len], and the view [v], returning [ret], whose body is [code]; a
   parameter the body does not read is written [_]-prefixed. *)
let pp_function ?(global = false) ppf ~target ~with_len ~name ~ret code =
  let lines =
    "let buf = (b : _ Wire_runtime.Buf.t :> bytes) in"
    :: (if with_len then "let len = (len : _ Wire_runtime.Buf.length :> int) in"
        else "let len = Bytes.length buf in")
    :: String.split_on_char '\n' code
    |> List.filter (fun l -> l <> "")
    |> Lines.prune
  in
  let reads x = List.exists (Lines.reads x) lines in
  let param x = if reads x then x else "_" ^ x in
  let b =
    match target with
    | `OxCaml when not global ->
        Fmt.str "(%s : 'b Wire_runtime.Buf.t @@ local)" (param "b")
    | `OxCaml | `OCaml -> Fmt.str "(%s : 'b Wire_runtime.Buf.t)" (param "b")
  in
  Fmt.pf ppf "let[@inline always] %s %s%s (%s : 'b view)%s =\n%s\n\n" name b
    (if with_len then Fmt.str " (%s : 'b Wire_runtime.Buf.length)" (param "len")
     else "")
    (param "v") ret (String.concat "\n" lines)

(* A presence whose last test is its answer answers that test. *)
let last_test code =
  match List.rev (String.split_on_char '\n' code) with
  | "true" :: test :: before
    when String.starts_with ~prefix:"if not " test
         && String.ends_with ~suffix:" then false else" test ->
      let c =
        String.sub test 7
          (String.length test - 7 - String.length " then false else")
      in
      String.concat "\n" (List.rev (c :: before))
  | _ -> code

(* An unsigned big-endian 32-bit field at [at] is the top half of the 64-bit
   word that starts on it, or else the bottom half of the one that ends on
   it, when that word is known to be there: one load, a shift or a mask,
   where a 32-bit load is sign-extended and then masked. The word is the one
   a test of the accessor may have loaded at that offset, of the same type
   on either target. *)
let result_words w m (f : field) ~at ~size ~lo p =
  let word a =
    if not (List.mem a w.wide_words) then
      emit w "let w%d = %s in\n" a (load m ~o:"v" ~k:a ~bytes:8 ~endian:T.Big);
    [ a ]
  in
  match f.store with
  | Int { width = 32; signed = false; endian = T.Big } when p.var = "v" ->
      if at + 8 <= size && lo <= at then word at
      else if at + 4 <= size && lo <= at - 4 then word (at - 4)
      else []
  | _ -> []

(* The type of a field's value, as the codec decodes it, and the proof that
   its values are [int]s: [Wire_runtime.UInt16.t] for a 16-bit integer, [int]
   for a bitfield. A 32-bit integer is an [int] where the module loads, which
   it refuses where ints have 32 bits or fewer; the header binds the proof.
   A narrower integer is typed by its carrier's [unsafe_of_int], a primitive,
   which OCaml without flambda compiles to nothing where a call to [cast]
   would leave a binding that tags the value. *)
let carrier (f : field) =
  match f.store with
  | Int { width = (8 | 16) as w; signed; _ } ->
      let m = Fmt.str "Wire_runtime.%sInt%d" (if signed then "S" else "U") w in
      Some (m, m ^ ".int_eq")
  | Int { width = 32; signed; _ } ->
      Some
        ( (if signed then "Wire_runtime.SInt32" else "Wire_runtime.UInt32"),
          if signed then "sint32_eq" else "uint32_eq" )
  | Int _ | Bits _ | Bytes _ -> None

let carrier_type f =
  match carrier f with Some (m, _) -> m ^ ".t" | None -> "int"

(* [x] in the carrier of [f]: the same [int], typed. *)
let typed f x =
  match (carrier f, f.store) with
  | Some (_, eq), Int { width = 32; _ } -> Fmt.str "(cast %s %s)" eq x
  | Some (m, _), _ -> Fmt.str "(%s.unsafe_of_int %s)" m x
  | None, _ -> x

(* The accessor of a field of bytes: a slice of the buffer, or a copy. A
   slice keeps the buffer, and a copy reads it with the stdlib's [Bytes], so
   both take it global. *)
let bytes_accessor ppf ~target ~run ~name ~at ~slice =
  let make pos n =
    if slice then
      Fmt.str "Bytesrw.Bytes.Slice.make_or_eod buf ~first:%s ~length:%s" pos n
    else Fmt.str "Bytes.sub_string buf %s %s" pos n
  in
  let absent = if slice then "Bytesrw.Bytes.Slice.eod" else "\"\"" in
  (match at with
    | Fixed f ->
        let n = width_bytes f.store in
        run ~absent ~bad:bad_raise ~target:(Field f.id) ~finish:(fun w _ p ->
            ignore (room w ~bad:bad_raise p (f.offset + n));
            emit w "%s"
              (make (ptext { p with at = p.at + f.offset }) (string_of_int n)))
    | Sized node ->
        let size = match node with Span e -> e | _ -> assert false in
        run ~absent ~bad:bad_raise ~target:(Bytes_span node)
          ~finish:(fun w _ p ->
            match size with
            | T.Int k when k >= 0 ->
                emit w "if %s > len - %d then %s else\n" (ptext p) k
                  (bad_raise ~at:(ptext p) ~need:(string_of_int k));
                emit w "%s" (make (ptext p) (string_of_int k))
            | _ ->
                let q = skip w ~bad:bad_raise ~ahead:None size p in
                emit w "%s"
                  (make (ptext p) (Fmt.str "(%s - %s)" (ptext q) (ptext p)))))
  |> pp_function ~global:true ppf ~target ~with_len:false ~name
       ~ret:(if slice then " : Bytesrw.Bytes.Slice.t" else " : string")

let accessor ppf ~target ~t ~ends (r : record) member =
  let m = mode target in
  let run ~absent ~bad ~target:tg ~finish =
    let code, e = body ~t ~m ~known:r.fixed ~absent ~bad ~target:tg ~finish r in
    Hashtbl.iter (fun k () -> Hashtbl.replace ends k ()) e;
    code
  in
  match member with
  | Scalar { name; field = f } ->
      run ~absent:(typed f "0") ~bad:bad_raise ~target:(Field f.id)
        ~finish:(fun w _ p ->
          if Sset.mem f.id w.bound then emit w "%s" (typed f (to_int w f.id))
          else
            let size, lo =
              room w ~bad:bad_raise p (f.offset + width_bytes f.store)
            in
            (* An unsigned big-endian 32-bit field is the top half of the
               64-bit word that starts on it, when that word is known to be
               there: a shift, and no mask. *)
            let at = p.at + f.offset in
            let m = result_mode target in
            let words = result_words w m f ~at ~size ~lo p in
            emit w "%s" (typed f (value m ~size ~lo ~o:p.var ~words ~k:p.at f)))
      |> pp_function ppf ~target ~with_len:false ~name
           ~ret:(" : " ^ carrier_type f)
  | Present { name; presence } ->
      run ~absent:"false" ~bad:bad_raise ~target:(Gate presence.id)
        ~finish:(fun w _ _ -> emit w "true")
      |> last_test
      |> pp_function ppf ~target ~with_len:false ~name ~ret:" : bool"
  | Sub { name; sub; _ } ->
      let sub_fixed = fixed_of_sub t sub in
      run ~absent:"(-1)"
        ~bad:(fun ~at:_ ~need:_ -> "(-1)")
        ~target:(Marker sub.var)
        ~finish:(fun w _ p ->
          if sub_fixed <= known w p then emit w "%s" (ptext p)
          else
            emit w "if %s > len - %d then -1 else %s" (ptext p) sub_fixed
              (ptext p))
      |> pp_function ppf ~target ~with_len:true ~name
           ~ret:(Fmt.str " : 'b %s.check" (module_name t sub))
  | Bytes { name; at; slice; _ } ->
      bytes_accessor ppf ~target ~run ~name ~at ~slice

(* [end_at buf len v] is where the record at [v] ends, or [-1]. *)
let end_at ppf ~target ~t ~ends (r : record) =
  let m = mode target in
  let code, e =
    body ~t ~m ~known:0 ~absent:"(-1)"
      ~bad:(fun ~at:_ ~need:_ -> "(-1)")
      ~target:End
      ~finish:(fun w _ p -> emit w "%s" (ptext p))
      r
  in
  Hashtbl.iter (fun k () -> Hashtbl.replace ends k ()) e;
  let lines =
    String.split_on_char '\n' code
    |> List.filter (fun l -> l <> "")
    |> Lines.prune
  in
  let param x = if List.exists (Lines.reads x) lines then x else "_" ^ x in
  Fmt.pf ppf "let[@inline never] end_at %s (%s : int) (v : int) =\n%s\n\n"
    (match target with
    | `OxCaml -> Fmt.str "(%s @@ local)" (param "buf")
    | `OCaml -> Fmt.str "(%s : bytes)" (param "buf"))
    (param "len") (String.concat "\n" lines)

(* [get] raises an exception built once: a call to [invalid_arg] would make
   the code around every [get] save what it holds in registers. *)
let pp_ok_get ppf ~name =
  Fmt.pf ppf
    "let failed = Invalid_argument %S\n\n\
     let[@inline always] ok (c : 'b check) = c >= 0\n\n\
     let[@inline always] get (c : 'b check) : 'b view =\n\
     if c >= 0 then c else raise failed\n\n"
    (name ^ ".get: the check failed")

(* The names the code of an accessor reads unqualified. An accessor of that
   name is written after the others, so that it shadows the name for no code
   that reads it. *)
let read_names = [ "mask32"; "not" ]

(* {2 Writing a field in place} *)

(* The fields of [r] a setter writes: an integer or a bitfield at a constant
   offset inside the bytes a view of [r] holds. *)
let settable (r : record) =
  List.filter_map
    (function
      | Scalar { name; field = f } when direct r f -> (
          match f.store with
          | Int _ | Bits _ -> Some (name, f)
          | Bytes _ -> None)
      | _ -> None)
    r.members

let setter_name name = "set_" ^ name

(* [set_<f> b v x] stores [x] as the field [f] of the record [v], in place:
   one store for an integer, a load and a store of its word for a bitfield,
   which keeps the word's other bits. A bitfield's [int] is tested against
   its width first; any other carrier holds its range by construction. *)
let setter ppf (name, (f : field)) =
  let u = "Wire_runtime.Unchecked" in
  let at = Fmt.str "(v + %d)" f.offset in
  let sfx e = endian_suffix e in
  Fmt.pf ppf
    "let[@inline] %s (b : 'b Wire_runtime.Buf.t) (v : 'b view) x =@\n\
     let buf = (b : _ Wire_runtime.Buf.t :> bytes) in@\n"
    (setter_name name);
  match f.store with
  | Int { width = 8; _ } ->
      Fmt.pf ppf "%s.set_u8 buf %s ((x : %s :> int) land 0xff)@\n@\n" u at
        (carrier_type f)
  | Int { width = 16; endian; _ } ->
      Fmt.pf ppf "%s.set_u16%s buf %s ((x : %s :> int) land 0xffff)@\n@\n" u
        (sfx endian) at (carrier_type f)
  | Int { endian; _ } ->
      Fmt.pf ppf "%s.set_i32%s buf %s (%s.to_int32 x)@\n@\n" u (sfx endian) at
        (fst (Option.get (carrier f)))
  | Bits { base; shift; width } -> (
      Fmt.pf ppf
        "if x lsr %d <> 0 then@\n\
         invalid_arg \"%s: the value does not fit in %d bits\";@\n"
        width (setter_name name) width;
      let mask = mask width in
      match base with
      | T.U8 ->
          Fmt.pf ppf
            "let w = %s.u8 buf %s in@\n\
             %s.set_u8 buf %s (w land lnot (%s lsl %d) lor (x lsl %d))@\n\
             @\n"
            u at u at mask shift shift
      | T.U16 e ->
          Fmt.pf ppf
            "let w = %s.u16%s buf %s in@\n\
             %s.set_u16%s buf %s@\n\
             (w land lnot (%s lsl %d) lor (x lsl %d) land 0xffff)@\n\
             @\n"
            u (sfx e) at u (sfx e) at mask shift shift
      | T.U32 e ->
          Fmt.pf ppf
            "let w = %s.i32%s buf %s in@\n\
             %s.set_i32%s buf %s@\n\
             (Int32.logor@\n\
             (Int32.logand w (Int32.lognot (Int32.shift_left (Int32.of_int %s) \
             %d)))@\n\
             (Int32.shift_left (Int32.of_int x) %d))@\n\
             @\n"
            u (sfx e) at u (sfx e) at mask shift shift)
  | Bytes _ -> ()

let pp_members ppf ~target ~t ~ends (r : record) =
  let late, early =
    List.partition (fun m -> List.mem (member_name m) read_names) r.members
  in
  List.iter (accessor ppf ~target ~t ~ends r) (early @ late);
  List.iter (setter ppf) (settable r)

(* The modules of the sub-records, then the root's members; the root's
   [check] is the scanner's. The writer binds [Int32_u] and [Int64_u] where
   the text reads them. A record's [end_at] is written when an accessor
   needs it, so the records are written to text parents first, then put
   children first. With the text, whether it reads [mask32]. *)
let ml ?(values = fun _ -> "") ~target t =
  let ends = Hashtbl.create 8 in
  let text f =
    let b = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer b in
    f ppf;
    Format.pp_print_flush ppf ();
    Buffer.contents b
  in
  let root =
    text (fun ppf ->
        pp_ok_get ppf ~name:t.root.codec_name;
        pp_members ppf ~target ~t ~ends t.root;
        Fmt.pf ppf "%s" (values t.root))
  in
  let modules =
    List.rev_map
      (fun (r : record) ->
        let name = Option.get r.module_name in
        text (fun ppf ->
            Fmt.pf ppf
              "module %s = struct\ntype 'b view = int\ntype 'b check = int\n\n"
              name;
            pp_ok_get ppf ~name;
            if Hashtbl.mem ends r.id then end_at ppf ~target ~t ~ends r;
            pp_members ppf ~target ~t ~ends r;
            Fmt.pf ppf "%send\n\n" (values r)))
      (List.rev t.records)
  in
  let text = String.concat "" (modules @ [ root ]) in
  let reads_mask32 =
    List.exists
      (fun l ->
        (not (String.starts_with ~prefix:"let[@inline" l))
        && Lines.reads "mask32" l)
      (String.split_on_char '\n' text)
  in
  (text, reads_mask32)

(* {1 The interface} *)

let pp_val ?(portable = true) ~target ppf name typ doc =
  Fmt.pf ppf "val %s : %s%s@\n(** %s *)@\n@\n" name typ
    (if target = `OxCaml && portable then " @@ portable" else "")
    doc

let named target =
  "'b Wire_runtime.Buf.t" ^ if target = `OxCaml then " @ local" else ""

let length = "'b Wire_runtime.Buf.length"

let pp_types ppf ~codec =
  Fmt.pf ppf
    "type 'b view = private int@\n\
     (** The type for the records of [%s] that passed a check in the buffer \
     named ['b]. *)@\n\
     @\n\
     type 'b check = private int@\n\
     (** The type for the answers of a check of a record of [%s] in the buffer \
     named ['b]. *)@\n\
     @\n"
    codec codec

let pp_ok_get_val ~target ppf =
  pp_val ~target ppf "ok" "'b check -> bool"
    "[ok c] is [true] iff the check [c] passed.";
  pp_val ~target ppf "get" "'b check -> 'b view"
    "[get c] is the record of the check [c]. Raises [Invalid_argument] if [ok \
     c] is [false]: test {!ok} first, a refused record is input, not misuse."

let moved =
  " If bytes of [b] written since the check move it out of [b], raises \
   [Wire_runtime.Errors.Parse_error]."

let pp_member_val ~target ~t ppf (r : record) = function
  | Scalar { name; field = f } ->
      let gated = chain (Field f.id) r.plan.nodes <> [] in
      pp_val ~target ppf name
        (String.concat " -> " [ named target; "'b view"; carrier_type f ])
        (Fmt.str "[%s b v] is the field [%s] of the record [v]%s.%s" name name
           (if gated then ", [0] when it is absent" else "")
           (if direct r f then "" else moved))
  | Present { name; _ } ->
      pp_val ~target ppf name
        (String.concat " -> " [ named target; "'b view"; "bool" ])
        (Fmt.str
           "[%s b v] is [true] iff the optional field it names is present in \
            the record [v].%s"
           name moved)
  | Sub { name; sub; _ } ->
      let m = module_name t sub in
      pp_val ~target ppf name
        (String.concat " -> "
           [ named target; length; "'b view"; Fmt.str "'b %s.check" m ])
        (Fmt.str
           "[%s b len v] is the record [%s] of [v], [len] the length of [b], \
            as a check of {!module-%s} that passes iff the record is present \
            and its fixed bytes lie in [b]. The check of [v] made the record's \
            checks and none runs again: if bytes of [b] were written since, a \
            read may return a value they break, never a byte outside [b]."
           name name m)
  | Bytes { name; slice; _ } ->
      (* Bytesrw states no portability, so a slice's accessor has none. *)
      pp_val ~portable:(not slice) ~target ppf name
        (String.concat " -> "
           [
             "'b Wire_runtime.Buf.t";
             "'b view";
             (if slice then "Bytesrw.Bytes.Slice.t" else "string");
           ])
        (Fmt.str "[%s b v] is the field [%s] of the record [v], %s.%s" name name
           (if slice then "a slice of [b], with no byte copied"
            else "a copy of its bytes")
           moved)

let pp_setter_val ~target ppf (name, (f : field)) =
  pp_val ~target ppf (setter_name name)
    (String.concat " -> "
       [ "'b Wire_runtime.Buf.t"; "'b view"; carrier_type f; "unit" ])
    (Fmt.str
       "[%s b v x] writes [x] in place as the field {!val-%s} reads in the \
        record [v]. It checks none of the record's constraints: check the \
        record again after a batch of writes.%s"
       (setter_name name) name
       (match f.store with
       | Bits { width; _ } ->
           Fmt.str " Raises [Invalid_argument] if [x] does not fit in %s."
             (if width = 1 then "1 bit" else Fmt.str "%d bits" width)
       | Int _ | Bytes _ -> ""))

let mli ?(values = fun _ _ -> ()) ppf ~target t =
  Fmt.pf ppf "(** {1:views Views} *)@\n@\n";
  List.iter
    (fun (r : record) ->
      Fmt.pf ppf "(** The views of the records of [%s]. *)@\nmodule %s : sig@\n"
        r.codec_name (Option.get r.module_name);
      pp_types ppf ~codec:r.codec_name;
      pp_ok_get_val ~target ppf;
      List.iter (pp_member_val ~target ~t ppf r) r.members;
      List.iter (pp_setter_val ~target ppf) (settable r);
      values ppf r;
      Fmt.pf ppf "end@\n@\n")
    t.records;
  pp_types ppf ~codec:t.root.codec_name;
  pp_val ~target ppf "check"
    (String.concat " -> "
       [ named target; length; "off:int"; "len:int"; "'b check" ])
    "[check b length ~off ~len] is the answer of the checks {!val-fold} makes \
     on the record at [off] in the [len] bytes there, in [b] of length \
     [length]. It never raises: bad bounds or a bad record give an answer \
     {!ok} refuses.";
  pp_ok_get_val ~target ppf;
  List.iter (pp_member_val ~target ~t ppf t.root) t.root.members;
  List.iter (pp_setter_val ~target ppf) (settable t.root);
  values ppf t.root

let of_codec ?(reserved = []) c = v ~reserved (Gen.to_struct c)

(* {1 The plan} *)

let pp_member ppf = function
  | Scalar { name; field } -> Fmt.pf ppf "%s (at %d)" name field.offset
  | Present { name; _ } -> Fmt.pf ppf "%s (presence)" name
  | Bytes { name; slice; _ } ->
      Fmt.pf ppf "%s (%s)" name (if slice then "slice" else "string")
  | Sub { name; sub; presence } ->
      Fmt.pf ppf "%s : %s%s" name sub.codec_name
        (if Option.is_some presence then " (optional)" else "")

let pp_record ppf r =
  Fmt.pf ppf "@[<v 2>%s (%s):@,%a@]"
    (Option.value ~default:"root" r.module_name)
    r.codec_name
    Fmt.(list ~sep:cut pp_member)
    r.members

let pp ppf t =
  Fmt.pf ppf "@[<v>%a@]" Fmt.(list ~sep:cut pp_record) (t.records @ [ t.root ])
