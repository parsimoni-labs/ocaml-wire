open Wire.Private
module T = Types
open Planner
open Emitter
open Record_module

(* {1 The scan module} *)

let codec_plan c =
  if Sys.int_size < 63 then
    fail_unsupported "a generator running with ints of fewer than 63 bits";
  plan_of (Gen.to_struct c)

let scan_plan c =
  let plan = codec_plan c in
  (match
     Names.clash
       (("ok", "ok")
       :: List.map (fun (o : output) -> (o.id, o.name)) plan.outputs)
   with
  | Some (_, _, n) -> fail_unsupported "the output name %S is ambiguous" n
  | None -> ());
  plan

let scanner_fields c =
  match scan_plan c with
  | plan -> List.map (fun (o : output) -> (o.name, o.is_bool)) plan.outputs
  | exception Unsupported why -> refuse ~codec:(Gen.to_struct c).name why

type scan = {
  plan : plan;
  selected : output list;
  m : mode;
  ppf : Format.formatter;
  taken : (string, unit) Hashtbl.t;
}

(* The outputs among [selected] that are slots. *)
let slots (plan : plan) selected =
  let deep =
    Sset.of_list (List.map (fun (o : output) -> o.id) (slots plan.nodes))
  in
  List.filter (fun (o : output) -> Sset.mem o.id deep) selected

let scan_mode (plan : plan) ~num selected =
  let ids = List.map (fun (o : output) -> o.id) in
  {
    num;
    src =
      (* On OxCaml it reads a buffer that may be [local] through [L], the
         runtime's [Target.Local]. *)
      (match num with
      | Unboxed -> { local_bytes_src with u = "L"; checked = "L" }
      | Tagged -> bytes_src);
    ox = false;
    cold = false;
    raw32 = false;
    live = live plan ~outputs:(ids selected);
    exported = Sset.of_list (ids selected);
    slots = Sset.of_list (ids (slots plan selected));
    fail = (fun _ -> "");
    overflowed = (fun _ _ -> "");
  }

(* The labelled arguments [valid] receives. *)
let pp_args ~conv ppf outputs =
  if outputs = [] then Fmt.pf ppf "()"
  else
    pp_list ppf
      (List.map (fun (o : output) -> Fmt.str "~%s:%s" o.name (conv o)) outputs)

let to_int (w : w) ~unboxed (o : output) =
  if w.m.num = Unboxed && (not unboxed) && not o.is_bool then
    match List.assoc_opt o.id w.settled with
    | Some _ -> "0"
    | None -> Fmt.str "(U.to_int %s)" (output_text w o)
  else output_text w o

(* The call of [valid] with the outputs, where the walk [w] ends. *)
let call s ~unboxed valid (w : w) =
  pf w "%s %a@\n" valid (pp_args ~conv:(to_int w ~unboxed)) s.selected

(* The functions cut out of [name] are named after it, without [fold_]. *)
let cut_prefix name =
  let fold = "fold_" in
  if String.starts_with ~prefix:fold name then
    String.sub name (String.length fold)
      (String.length name - String.length fold)
  else name

(* [write s] writes one function of the scanner, [name], with [cuts], the
   functions its walk is cut into, before it; none in a scanner with slots,
   whose walk stays one function. *)
let with_cuts ?(poly = "") s name write =
  Hashtbl.replace s.taken name ();
  if not (Sset.is_empty s.m.slots) then write s None
  else begin
    let main = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer main in
    let reserve =
      List.fold_left
        (fun n (o : output) ->
          n + String.length o.name + String.length o.id + 3)
        20 s.selected
    in
    let cuts =
      {
        prefix = cut_prefix name;
        defs = Buffer.create 4096;
        taken = s.taken;
        poly;
        reserve;
      }
    in
    write { s with ppf } (Some cuts);
    Format.pp_print_flush ppf ();
    Fmt.pf s.ppf "%s%s" (Buffer.contents cuts.defs) (Buffer.contents main)
  end

(* The variables a walk starts with, each with the type a function cut out
   of it gives it, when inference needs one: [base] and [stop], then
   [names]. *)
let scope s names =
  let num, buf =
    match s.m.num with
    | Unboxed -> (Some "int64_u", Some "bytes @ local")
    | Tagged -> (None, None)
  in
  List.rev ([ ("buf", buf); ("base", num); ("stop", num) ] @ names)

(* The walk over the [stop] bytes at [base], which starts by binding its
   slots: each rejection is the text [reject] and each acceptance what
   [accept] writes, in place, so a continuation the generator knows is never
   a closure. *)
let pp_scan ?cuts ?(known = 0) ~scope s ~reject ~accept =
  pp_slots s.m s.ppf s.selected;
  let m =
    { s.m with fail = (fun _ -> reject); overflowed = (fun _ _ -> reject) }
  in
  start ?cuts ~scope
    ?first:(Option.map (fun _ -> "record") cuts)
    ~m ~ppf:s.ppf ~ranges:s.plan.ranges ~base:"base" ~limit:"stop" ~known
    s.plan.nodes
    (fun w _ -> accept w)

(* [mentions x text] is [true] when the identifier [x] occurs in [text]. *)
let mentions x text =
  let n = String.length x and len = String.length text in
  let ident c =
    match c with
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
    | _ -> false
  in
  let rec from i =
    match String.index_from_opt text i x.[0] with
    | None -> false
    | Some j ->
        j + n <= len
        && String.sub text j n = x
        && (j = 0 || not (ident text.[j - 1]))
        && (j + n = len || not (ident text.[j + n]))
        || from (j + 1)
  in
  n > 0 && from 0

(* The bytes a frame needs before its first variable offset: the leading
   prefixes, then those of the first optional field. A batch whose stride is
   at least that tests none of their bounds per frame. *)
let rec fixed_need = function
  | Prefix (_, n) :: rest -> n + fixed_need rest
  | Gate (_, _, _, inner) :: _ -> fixed_need inner
  | _ -> 0

let pp_entry ?cuts ~scope s ~reject ~accept =
  Fmt.pf s.ppf
    "if off < 0 || len < 0 || off > Bytes.length buf || len > Bytes.length buf \
     - off then %s else@\n"
    reject;
  Fmt.pf s.ppf "let base = %s in@\nlet stop = %s in@\n"
    (if s.m.num = Tagged then "off" else "U.of_int off")
    (if s.m.num = Tagged then "len" else "U.of_int len");
  pp_scan ?cuts ~scope s ~reject ~accept

(* The types of the callbacks [fold_frames_unboxed] takes, whose accumulator
   has layout [bits64]. *)
let callback_types s =
  let fields =
    if s.selected = [] then [ "unit" ]
    else
      List.map
        (fun (o : output) ->
          o.name ^ ":" ^ if o.is_bool then "bool" else "int64_u")
        s.selected
  in
  ("a -> a", String.concat " -> " (("a" :: fields) @ [ "a" ]))

(* The callbacks [fold_frames] takes: on OxCaml, unboxed ones have their
   types written out. *)
let frames_callbacks s ~unboxed =
  if not unboxed then "~invalid ~valid"
  else
    let invalid, valid = callback_types s in
    Fmt.str "~(invalid : %s) ~(valid : %s) : a" invalid valid

(* The variables of a frame loop: how [acc] and [cursor] are read and
   assigned, and the stride added to [cursor]. *)
let frames_vars s =
  if s.m.num = Unboxed then ("", "<-", "stride_word") else ("!", ":=", "stride")

(* The loop of [fold_frames] over frames walked with [known] bytes of each
   known to be there. A walk that knows all its bounds may not read [stop]. *)
let pp_frames_loop ?cuts s ~names ~unboxed ~invalid ~valid ~known =
  let ppf = s.ppf and read, assign, stride = frames_vars s in
  let body = Buffer.create 4096 in
  let bppf = Format.formatter_of_buffer body in
  pp_scan ?cuts ~known ~scope:(scope s names) { s with ppf = bppf }
    ~reject:(invalid ^ " current")
    ~accept:(call s ~unboxed (valid ^ " current"));
  Format.pp_print_flush bppf ();
  let body = Buffer.contents body in
  Fmt.pf ppf
    "for _ = 1 to len / stride do@\n\
     let base = %scursor in@\n\
     let current = %sacc in@\n\
     let next = (@\n"
    read read;
  if mentions "stop" body then Fmt.pf ppf "let stop = %s in@\n" stride;
  Fmt.pf ppf "%s) in@\nacc %s next;@\ncursor %s %s@\ndone" body assign assign
    (binop s.m.num "+" "base" stride)

(* A batch of frames too short for the bounds a frame loop leaves out goes
   through the walk of one frame, already written, rather than a second copy
   of it. *)
let pp_frames_short ?step s =
  let read, assign, stride = frames_vars s in
  let at = if s.m.num = Unboxed then "(U.to_int base)" else "base" in
  let one =
    match step with
    | Some _ -> Fmt.str "fold_step buf %s stride ~init:current" at
    | None ->
        Fmt.str
          "fold buf %s stride ~invalid:(fun () -> invalid current) \
           ~valid:(valid current)"
          at
  in
  Fmt.pf s.ppf
    "for _ = 1 to len / stride do@\n\
     let base = %scursor in@\n\
     let current = %sacc in@\n\
     acc %s %s;@\n\
     cursor %s %s@\n\
     done"
    read read assign one assign
    (binop s.m.num "+" "base" stride)

(* [fold_frames], or with [step] [fold_frames_step], which calls the module
   [step]'s functions by name. The frame guard and a positive stride make
   [base + stride <= off + len] on every iteration, the last included. Each
   frame still gets all its checks: a short frame cannot borrow bytes from the
   next. *)
let pp_frames ?step s ~unboxed =
  let native = s.m.num = Unboxed in
  let binding, reference, read =
    if native then ("mutable ", "", "") else ("", "ref ", "!")
  in
  let name, callbacks, invalid, valid =
    match step with
    | Some m -> ("fold_frames_step", "", m ^ ".invalid", m ^ ".valid")
    | None ->
        ( (if unboxed then "fold_frames_unboxed" else "fold_frames"),
          frames_callbacks s ~unboxed,
          "invalid",
          "valid" )
  in
  let names, poly =
    match step with
    | Some _ -> ([ ("current", None) ], "")
    | None when unboxed ->
        let i, v = callback_types s in
        ( [ ("current", Some "a"); ("invalid", Some i); ("valid", Some v) ],
          "(type a : bits64) " )
    | None -> ([ ("current", None); ("invalid", None); ("valid", None) ], "")
  in
  with_cuts ~poly s name (fun s cuts ->
      let ppf = s.ppf in
      Fmt.pf ppf "let[@inline always] %s%s %s off len ~stride %s %s =@\n" name
        (if unboxed then " (type a : bits64)" else "")
        s.m.src.buf
        (if unboxed then "~(init : a)" else "~init")
        callbacks;
      Fmt.pf ppf
        "if stride <= 0 || off < 0 || len < 0 || off > Bytes.length buf || len \
         > Bytes.length buf - off then@\n\
         invalid_arg \"%s: invalid frame bounds or stride\";@\n"
        name;
      Fmt.pf ppf "let %sacc = %sinit in@\nlet %scursor = %soff in@\n" binding
        reference binding
        (if native then "U.of_int " else "ref ");
      if native then Fmt.pf ppf "let stride_word = U.of_int stride in@\n";
      let need = fixed_need s.plan.nodes in
      let loop known =
        pp_frames_loop ?cuts s ~names ~unboxed ~invalid ~valid ~known
      in
      if need > 0 then begin
        Fmt.pf ppf "if stride >= %d then (@\n" need;
        loop need;
        Fmt.pf ppf ")@\nelse (@\n";
        (* [fold_unboxed] is not polymorphic in the layout of its answer, so
           the unboxed loop keeps a walk of its own. *)
        if unboxed then loop 0 else pp_frames_short ?step s;
        Fmt.pf ppf ")"
      end
      else loop 0;
      Fmt.pf ppf ";@\n%sacc@\n@\n" read)

(* [fold], or [fold_unboxed], whose consumer takes the outputs unboxed. *)
let pp_fold s ~unboxed =
  let name = if unboxed then "fold_unboxed" else "fold" in
  with_cuts s name (fun s cuts ->
      Fmt.pf s.ppf "let[@inline always] %s %s off len ~invalid ~valid =@\n" name
        s.m.src.buf;
      pp_entry ?cuts
        ~scope:(scope s [ ("invalid", None); ("valid", None) ])
        s ~reject:"invalid ()" ~accept:(call s ~unboxed "valid");
      Fmt.pf s.ppf "@\n")

(* [fold_step], which calls the module [m]'s functions by name. *)
let pp_fold_step s m =
  with_cuts s "fold_step" (fun s cuts ->
      Fmt.pf s.ppf "let[@inline always] fold_step %s off len ~init =@\n"
        s.m.src.buf;
      pp_entry ?cuts
        ~scope:(scope s [ ("init", None) ])
        s ~reject:(m ^ ".invalid init")
        ~accept:(call s ~unboxed:false (m ^ ".valid init"));
      Fmt.pf s.ppf "@\n")

let pp_result_type s name hash =
  Fmt.pf s.ppf "type %s = %s{ ok : bool;@\n" name hash;
  List.iter
    (fun (o : output) ->
      Fmt.pf s.ppf "%s : %s;@\n" o.name
        (if o.is_bool then "bool"
         else if hash = "#" && s.m.num = Unboxed then "int64_u"
         else "int"))
    s.selected;
  Fmt.pf s.ppf "}@\n"

let pp_fields s ppf conv =
  List.iter
    (fun (o : output) -> Fmt.pf ppf "%s = %s;" o.name (conv o))
    s.selected

let output_id (o : output) = o.id

(* [decode], which folds into the record of a rejected frame or the one
   [decode_accepted] builds. *)
let pp_decoder s =
  let zero (o : output) = if o.is_bool then "false" else "0" in
  Hashtbl.replace s.taken "decode" ();
  Hashtbl.replace s.taken "decode_rejected" ();
  Hashtbl.replace s.taken "decode_accepted" ();
  Fmt.pf s.ppf "let decode_rejected : t = {ok = false;%a}@\n@\n" (pp_fields s)
    zero;
  Fmt.pf s.ppf
    "let[@inline always] decode_accepted %a : t = {ok = true;%a}@\n@\n"
    (pp_args ~conv:output_id) s.selected (pp_fields s) output_id;
  Fmt.pf s.ppf
    "let[@inline always] decode %s off len : t =@\n\
     fold buf off len ~invalid:(fun () -> decode_rejected) \
     ~valid:decode_accepted@\n\
     @\n"
    s.m.src.buf

(* [pp ppf v] prints the outputs [v] as an OCaml record, with [Format]'s
   primitives only. *)
let pp_printer s =
  Hashtbl.replace s.taken "pp" ();
  Fmt.pf s.ppf
    "let pp ppf (v : t) =@\n\
     let field name pp_v x =@\n\
     Format.pp_print_space ppf ();@\n\
     Format.pp_print_string ppf (name ^ \" = \");@\n\
     pp_v ppf x;@\n\
     Format.pp_print_char ppf ';'@\n\
     in@\n\
     Format.pp_open_hvbox ppf 2;@\n\
     Format.pp_print_char ppf '{';@\n\
     field \"ok\" Format.pp_print_bool v.ok;@\n";
  List.iter
    (fun (o : output) ->
      Fmt.pf s.ppf "field %S Format.pp_print_%s v.%s;@\n" o.name
        (if o.is_bool then "bool" else "int")
        o.name)
    s.selected;
  Fmt.pf s.ppf
    "Format.pp_print_string ppf \" }\";@\nFormat.pp_close_box ppf ()@\n@\n"

(* [decode_unboxed], which returns an unboxed record. Its walk stays one
   function and builds the record with local functions: built in a function
   the walk is cut into, or by a top-level function, the record costs
   flambda2 one spill and a permutation of register moves on each path,
   some two instructions a frame. *)
let pp_unboxed_decoder s =
  let zero (o : output) = if o.is_bool then "false" else lit s.m.num 0 in
  Hashtbl.replace s.taken "decode_unboxed" ();
  Fmt.pf s.ppf
    "let[@inline always][@zero_alloc] decode_unboxed %s off len : unboxed =@\n"
    s.m.src.buf;
  Fmt.pf s.ppf "let invalid () = #{ok = false;%a} in@\n" (pp_fields s) zero;
  Fmt.pf s.ppf "let valid %a = #{ok = true;%a} in@\n" (pp_args ~conv:output_id)
    s.selected (pp_fields s) output_id;
  pp_entry ~scope:(scope s []) s ~reject:"invalid ()"
    ~accept:(call s ~unboxed:true "valid")

let pp_box s =
  Fmt.pf s.ppf "let[@inline] box (v : unboxed) : t = {ok = v.#ok;";
  List.iter
    (fun (o : output) ->
      Fmt.pf s.ppf "%s = %s;" o.name
        (if s.m.num = Unboxed && not o.is_bool then "U.to_int v.#" ^ o.name
         else "v.#" ^ o.name))
    s.selected;
  Fmt.pf s.ppf "}@\n"

let selected (plan : plan) = function
  | None -> plan.outputs
  | Some wanted ->
      List.iter
        (fun name ->
          if not (List.exists (fun (o : output) -> o.name = name) plan.outputs)
          then fail_unsupported "unknown output %S" name)
        wanted;
      List.filter (fun (o : output) -> List.mem o.name wanted) plan.outputs

let without_dead_bindings text =
  String.concat "\n" (prune (String.split_on_char '\n' text))

(* The scanner after its header: the walks, the outputs and their printer. *)
let pp_body ?step s =
  let unboxed = s.m.num = Unboxed in
  if unboxed then
    Fmt.pf s.ppf
      "module U = Wire_runtime.Target.Int64_u@\n\
       module B = Wire_runtime.Target@\n\
       module L = Wire_runtime.Target.Local@\n\
       @\n";
  pp_fold s ~unboxed:false;
  pp_frames s ~unboxed:false;
  Option.iter
    (fun m ->
      pp_fold_step s m;
      pp_frames ~step:m s ~unboxed:false)
    step;
  if unboxed then begin
    pp_fold s ~unboxed:true;
    pp_frames s ~unboxed:true
  end;
  pp_result_type s "t" "";
  pp_decoder s;
  pp_printer s;
  if unboxed then begin
    pp_result_type s "unboxed" "#";
    pp_unboxed_decoder s;
    pp_box s
  end

let scanner ?(target = `OCaml) ?fields ?step ~codec c =
  try
    let plan = scan_plan c in
    let num = if target = `OxCaml then Unboxed else Tagged in
    let b = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer b in
    let selected = selected plan fields in
    let s =
      {
        plan;
        selected;
        m = scan_mode plan ~num selected;
        ppf;
        taken = Hashtbl.create 16;
      }
    in
    pp_body ?step s;
    Format.pp_print_flush ppf ();
    let body = without_dead_bindings (Buffer.contents b) in
    let uses = uses body in
    Fmt.str "%a%a%s"
      (fun ppf () ->
        pp_header ppf ~codec ~name:plan.record ~digest:(Gen.digest c) ~uses)
      ()
      (fun ppf () -> pp_wide ppf ~codec ~wide:plan.wide ~uses)
      () body
  with Unsupported why -> refuse ~codec why
