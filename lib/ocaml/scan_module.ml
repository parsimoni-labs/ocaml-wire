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

let scan_mode ?(decide = ignore) (plan : plan) ~num selected =
  let ids = List.map (fun (o : output) -> o.id) in
  {
    num;
    src =
      (* On OxCaml it reads a buffer that may be [local] through [L], the
         runtime's [Target.Local]. *)
      (match num with
      | Unboxed -> { local_bytes_src with u = "L"; checked = "L" }
      | Tagged | Boxed -> bytes_src);
    ox = false;
    cold = false;
    raw32 = false;
    ranged = true;
    live = live plan ~outputs:(ids selected);
    exported = Sset.of_list (ids selected);
    slots = Sset.of_list (ids (slots plan selected));
    ahead = Sset.empty;
    fail = (fun _ -> "");
    overflowed = (fun _ _ -> "");
    decide;
  }

(* The labelled arguments [valid] receives. *)
let pp_args ~conv ppf outputs =
  if outputs = [] then Fmt.pf ppf "()"
  else
    pp_list ppf
      (List.map (fun (o : output) -> Fmt.str "~%s:%s" o.name (conv o)) outputs)

(* An integer output as [valid] takes it. A value held as [int64_u] is
   converted for a [valid] that is not unboxed: to an [int64] with [step], to
   an [int] otherwise. *)
let to_int (w : w) ~unboxed ~step (o : output) =
  if w.m.num = Unboxed && (not unboxed) && not o.is_bool then
    match List.assoc_opt o.id w.settled with
    | Some _ -> if step then "0L" else "0"
    | None ->
        Fmt.str
          (if step then "(U.to_int64 %s)" else "(U.to_int %s)")
          (output_text w o)
  else output_text w o

(* The call of [valid] with the outputs, where the walk [w] ends. With
   [step], [valid] is the consumer of the step functions and takes [int64]. *)
let call ?(step = false) s ~unboxed valid (w : w) =
  pf w "%s %a@\n" valid (pp_args ~conv:(to_int w ~unboxed ~step)) s.selected

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
    | Tagged | Boxed -> (None, None)
  in
  List.rev ([ ("buf", buf); ("base", num); ("stop", num) ] @ names)

(* The walk over the [stop] bytes at [base], which starts by binding its
   slots: each rejection is the text [reject] and each acceptance what
   [accept] writes, in place, so a continuation the generator knows is never
   a closure. *)
let pp_scan ?cuts ?(known = 0) ~name ~scope s ~reject ~accept =
  pp_slots s.m s.ppf s.selected;
  let m =
    { s.m with fail = (fun _ -> reject); overflowed = (fun _ _ -> reject) }
  in
  start ?cuts ~scope
    ?first:(Option.map (fun _ -> "record") cuts)
    ~name ~m ~ppf:s.ppf ~ranges:s.plan.ranges ~base:"base" ~limit:"stop" ~known
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
  | Prefix (_, n) :: rest -> extent n (fixed_need rest)
  | Gate (_, _, _, inner) :: _ -> fixed_need inner
  | _ -> 0

(* [true] when the walk of [plan] starts with the bound check of a prefix,
   which runs before any read and rejects a negative length. *)
let checks_len plan =
  match plan.nodes with Prefix (_, n) :: _ -> n > 0 | _ -> false

(* The entry tests [len < 0] and [off > Bytes.length buf] unless
   {!checks_len} holds: then the walk's first check rejects a negative [len],
   and [len > Bytes.length buf - off] an [off] past the buffer. *)
let pp_entry ?cuts ~name ~scope s ~reject ~accept =
  Fmt.pf s.ppf "if %s then %s else@\n"
    (if checks_len s.plan then "off < 0 || len > Bytes.length buf - off"
     else
       "off < 0 || len < 0 || off > Bytes.length buf || len > Bytes.length buf \
        - off")
    reject;
  Fmt.pf s.ppf "let base = %s in@\nlet stop = %s in@\n"
    (if s.m.num = Unboxed then "U.of_int off" else "off")
    (if s.m.num = Unboxed then "U.of_int len" else "len");
  pp_scan ?cuts ~name ~scope s ~reject ~accept

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

(* The condition of a frame loop: [cursor] has not passed [last], the offset
   of the last frame the iteration walks. The cursor is the loop's only
   counter. *)
let frames_while ?(last = "last") s =
  if s.m.num = Unboxed then Fmt.str "U.compare cursor %s <= 0" last
  else Fmt.str "!cursor <= %s" last

(* The frames walked per iteration of the hot frame loop. OCaml polls for
   signals at each loop back edge (ocaml/ocaml#10039), so unrolling the loop
   amortises one poll over [unroll] frames. *)
let unroll = 4

(* The fields at constant offsets in the bytes {!fixed_need} counts that an
   optional field holds, each with its offset in the frame. *)
let rec gated_fields ~gated at = function
  | Prefix (fields, n) :: rest ->
      (if gated then
         List.map (fun (f : field) -> (f, extent at f.offset)) fields
       else [])
      @ gated_fields ~gated (extent at n) rest
  | Gate (_, _, _, inner) :: _ -> gated_fields ~gated:true at inner
  | _ -> []

(* The variables the conditions of the optional fields of [nodes] read. *)
let rec tested = function
  | [] -> []
  | Gate (e, _, _, body) :: rest -> T.collect_refs e @ tested body @ tested rest
  | Region (_, _, body) :: rest -> tested body @ tested rest
  | (Prefix _ | Span _ | Test _) :: rest -> tested rest

(* The fields an unrolled frame loop reads for every frame of an iteration
   before the walk of its first frame: those the walk tests to choose an
   optional field, in the [need] bytes every frame has, and inside an optional
   field itself, which the walk reaches after checks of its own (it loads the
   leading fields first anyway). Each is a variable and its value at a frame
   start. A frame's walk then dispatches on a value loaded before the previous
   frames' branches, so a mispredicted branch of one frame does not delay the
   next frame's choice by a load. *)
let read_aheads s ~need =
  let tested = tested s.plan.nodes in
  List.filter_map
    (fun ((f : field), k) ->
      if Sset.mem f.id s.m.live && List.mem f.id tested then
        Option.map (fun ahead -> (f.id, ahead)) (read_ahead s.m ~size:need ~k f)
      else None)
    (gated_fields ~gated:false 0 s.plan.nodes)

(* The loop of [fold_frames] over frames walked with [known] bytes of each
   known to be there. A walk that knows all its bounds may not read [stop].
   With [unrolled], and when the walk of a frame is a function of its own,
   the loop first walks [unroll] frames per iteration, then the rest one by
   one. An iteration computes the start of each of its frames, and loads each
   frame's [ahead] fields, before the walk of its first frame. The guard
   [stride <= len / unroll] keeps [off + len - unroll * stride] from
   overflowing or falling below [off]. *)
(* The walk of one frame of a loop of [fold_frames], with the read-ahead
   [ahead] fields bound as variables: the expression of the next
   accumulator. *)
let frames_body ?cuts ?step ~ahead s ~name ~names ~unboxed ~invalid ~valid
    ~known =
  let body = Buffer.create 4096 in
  let bppf = Format.formatter_of_buffer body in
  let m = { s.m with ahead = Sset.of_list (List.map fst ahead) } in
  let typ = if s.m.num = Unboxed then Some "int64_u" else None in
  let names = names @ List.map (fun (v, _) -> (v, typ)) ahead in
  pp_scan ?cuts ~known ~name ~scope:(scope s names) { s with ppf = bppf; m }
    ~reject:(invalid ^ " current")
    ~accept:(call ~step:(Option.is_some step) s ~unboxed (valid ^ " current"));
  Format.pp_print_flush bppf ();
  Buffer.contents body

(* A frame at [base] binds each read-ahead [v] of [used] to [loaded v], the
   variable its iteration bound, or else to its value at [base]. *)
let pp_frame ?loaded s ~body ~used base =
  let ppf = s.ppf and read, assign, stride = frames_vars s in
  if mentions "base" body || (used <> [] && Option.is_none loaded) then
    Fmt.pf ppf "let base = %s in@\n" base;
  List.iter
    (fun (v, value) ->
      Fmt.pf ppf "let %s = %s in@\n" v
        (match loaded with Some l -> l v | None -> value "base"))
    used;
  Fmt.pf ppf "let current = %sacc in@\nlet next = (@\n" read;
  if mentions "stop" body then Fmt.pf ppf "let stop = %s in@\n" stride;
  Fmt.pf ppf "%s) in@\nacc %s next;@\n" body assign

(* The loop that walks [unroll] frames per iteration, guarded by
   [stride <= len / unroll]. *)
let pp_frames_unrolled s ~body ~used =
  let ppf = s.ppf and read, assign, stride = frames_vars s in
  (* A position is an [int] where values are [int64]. *)
  let num = pos_num s.m.num in
  let int n = if num = Unboxed then Fmt.str "U.of_int (%s)" n else n in
  Fmt.pf ppf "if stride <= len / %d then (@\nlet last_%d = %s in@\n" unroll
    unroll
    (Fmt.kstr int "off + len - (%d * stride)" unroll);
  for j = 2 to unroll do
    Fmt.pf ppf "let stride_%d = %s in@\n" j (Fmt.kstr int "%d * stride" j)
  done;
  Fmt.pf ppf "while %s do@\n" (frames_while ~last:(Fmt.str "last_%d" unroll) s);
  let start j =
    if j = 0 then read ^ "cursor"
    else
      binop num "+" (read ^ "cursor")
        (if j = 1 then stride else Fmt.str "stride_%d" j)
  in
  let frames = List.init unroll Fun.id in
  List.iter
    (fun j -> if j > 0 then Fmt.pf ppf "let base_%d = %s in@\n" j (start j))
    frames;
  let at j = if j = 0 then read ^ "cursor" else Fmt.str "base_%d" j in
  List.iter
    (fun j ->
      List.iter
        (fun (v, value) -> Fmt.pf ppf "let %s_%d = %s in@\n" v j (value (at j)))
        used)
    frames;
  List.iter
    (fun j ->
      pp_frame s ~body ~used ~loaded:(fun v -> Fmt.str "%s_%d" v j) (at j))
    frames;
  Fmt.pf ppf "cursor %s %s@\ndone);@\n" assign
    (binop num "+" (read ^ "cursor") (Fmt.str "stride_%d" unroll))

(* The loop of [fold_frames] over frames walked with [known] bytes of each
   known to be there. A walk that knows all its bounds may not read [stop].
   With [unrolled], and when the walk of a frame is a function of its own,
   the loop first walks [unroll] frames per iteration, then the rest one by
   one. An iteration computes the start of each of its frames, and loads each
   frame's [ahead] fields, before the walk of its first frame. The guard
   [stride <= len / unroll] keeps [off + len - unroll * stride] from
   overflowing or falling below [off]. *)
let pp_frames_loop ?cuts ?step ?(ahead = []) s ~name ~names ~unboxed ~invalid
    ~valid ~known ~unrolled =
  let read, assign, stride = frames_vars s in
  let body =
    frames_body ?cuts ?step ~ahead s ~name ~names ~unboxed ~invalid ~valid
      ~known
  in
  let used = List.filter (fun (v, _) -> mentions v body) ahead in
  if unrolled && Option.is_some cuts then pp_frames_unrolled s ~body ~used;
  Fmt.pf s.ppf "while %s do@\n" (frames_while s);
  pp_frame s ~body ~used (read ^ "cursor");
  Fmt.pf s.ppf "cursor %s %s@\ndone" assign
    (binop (pos_num s.m.num) "+" (read ^ "cursor") stride)

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
    "while %s do@\n\
     let base = %scursor in@\n\
     let current = %sacc in@\n\
     acc %s %s;@\n\
     cursor %s %s@\n\
     done"
    (frames_while s) read read assign one assign
    (binop (pos_num s.m.num) "+" "base" stride)

(* The loop of [name] over a batch too short for its frame loop. On the stock
   compiler it is a function of its own, so the variables of the walk it
   inlines do not take registers from the frame loop. *)
let pp_frames_apart ?step s name =
  let short = name ^ "_short" in
  Hashtbl.replace s.taken short ();
  Fmt.pf s.ppf
    "let[@inline never] %s buf ~stride ~last ~off ~init%s =@\n\
     let acc = ref init in@\n\
     let cursor = ref off in@\n"
    short
    (if Option.is_some step then "" else " ~invalid ~valid");
  pp_frames_short ?step s;
  Fmt.pf s.ppf ";@\n!acc@\n@\n";
  Fmt.str "acc := %s buf ~stride ~last ~off ~init:!acc%s" short
    (if Option.is_some step then "" else " ~invalid ~valid")

(* The variables a frame loop's walk starts with besides its own, each with
   the type a function cut out of it gives it, and what goes before those
   functions' parameters. *)
let frames_scope ?step s ~unboxed =
  match step with
  | Some _ -> ([ ("current", None) ], "")
  | None when unboxed ->
      let i, v = callback_types s in
      ( [ ("current", Some "a"); ("invalid", Some i); ("valid", Some v) ],
        "(type a : bits64) " )
  | None -> ([ ("current", None); ("invalid", None); ("valid", None) ], "")

(* The start of the frame loop [name]: its parameters, the check of its
   bounds and stride, and its accumulator, cursor and last offset. *)
let pp_frames_head s ~name ~unboxed ~callbacks =
  let ppf = s.ppf and native = s.m.num = Unboxed in
  Fmt.pf ppf "let[@inline always] %s%s %s off len ~stride %s %s =@\n" name
    (if unboxed then " (type a : bits64)" else "")
    s.m.src.buf
    (if unboxed then "~(init : a)" else "~init")
    callbacks;
  Fmt.pf ppf
    "if stride <= 0 || off < 0 || len < 0 || off > Bytes.length buf || len > \
     Bytes.length buf - off then@\n\
     invalid_arg \"%s: invalid frame bounds or stride\";@\n"
    name;
  if native then
    Fmt.pf ppf
      "let mutable acc = init in@\n\
       let mutable cursor = U.of_int off in@\n\
       let stride_word = U.of_int stride in@\n\
       let last = U.of_int (off + len - stride) in@\n"
  else
    Fmt.pf ppf
      "let acc = ref init in@\n\
       let cursor = ref off in@\n\
       let last = off + len - stride in@\n"

(* The frame loop [name] as a decision: [pp_frames] below writes it. A walk
   with slots is not cut, so its loop is not unrolled. *)
let frame_loop s ~name ~need ~apart ~unboxed =
  let native = s.m.num = Unboxed in
  let frames = if native && Sset.is_empty s.m.slots then unroll else 1 in
  Decision.Frame_loop
    {
      name;
      need;
      frames;
      short =
        (if need = 0 then None
         else if Option.is_some apart then Some Decision.Apart
         else if unboxed then Some Decision.Walked
         else Some Decision.Inline);
      ahead = (if frames > 1 then List.map fst (read_aheads s ~need) else []);
    }

(* [fold_frames], or with [step] [fold_frames_step], which calls the module
   [step]'s functions by name. The frame guard and a positive stride keep
   [last = off + len - stride] from overflowing and every frame inside
   [off + len]. The loop runs [len / stride] times, and each frame gets all
   its checks, so a short frame cannot borrow bytes from the next. *)
let pp_frames ?step s ~unboxed =
  let native = s.m.num = Unboxed in
  let read = if native then "" else "!" in
  let name, callbacks, invalid, valid =
    match step with
    | Some m -> ("fold_frames_step", "", m ^ ".invalid", m ^ ".valid")
    | None ->
        ( (if unboxed then "fold_frames_unboxed" else "fold_frames"),
          frames_callbacks s ~unboxed,
          "invalid",
          "valid" )
  in
  let names, poly = frames_scope ?step s ~unboxed in
  let need = fixed_need s.plan.nodes in
  let apart =
    if need > 0 && not native then Some (pp_frames_apart ?step s name) else None
  in
  s.m.decide (frame_loop s ~name ~need ~apart ~unboxed);
  with_cuts ~poly s name (fun s cuts ->
      let ppf = s.ppf in
      pp_frames_head s ~name ~unboxed ~callbacks;
      let loop ~unrolled known =
        let ahead =
          if unrolled && Option.is_some cuts then
            List.map snd (read_aheads s ~need:known)
          else []
        in
        pp_frames_loop ?cuts ?step ~ahead s ~name ~names ~unboxed ~invalid
          ~valid ~known ~unrolled
      in
      (* The loop over frames that hold the leading prefixes comes last,
         where the compiler closes it with one conditional branch. On the
         stock compiler it walks one frame per iteration: four inlined walks
         cost more in spilled registers than the polls they save. *)
      if need > 0 then begin
        Fmt.pf ppf "if stride < %d then (@\n" need;
        (* [fold_unboxed] is not polymorphic in the layout of its answer, so
           the unboxed loop keeps a walk of its own. *)
        (match apart with
        | Some call -> Fmt.pf ppf "%s" call
        | None ->
            if unboxed then loop ~unrolled:false 0 else pp_frames_short ?step s);
        Fmt.pf ppf ")@\nelse (@\n";
        loop ~unrolled:native need;
        Fmt.pf ppf ")"
      end
      else loop ~unrolled:native 0;
      Fmt.pf ppf ";@\n%sacc@\n@\n" read)

(* [fold], or [fold_unboxed], whose consumer takes the outputs unboxed. *)
let pp_fold s ~unboxed =
  let name = if unboxed then "fold_unboxed" else "fold" in
  with_cuts s name (fun s cuts ->
      Fmt.pf s.ppf "let[@inline always] %s %s off len ~invalid ~valid =@\n" name
        s.m.src.buf;
      pp_entry ?cuts ~name
        ~scope:(scope s [ ("invalid", None); ("valid", None) ])
        s ~reject:"invalid ()" ~accept:(call s ~unboxed "valid");
      Fmt.pf s.ppf "@\n")

(* [fold_step], which calls the module [m]'s functions by name. *)
let pp_fold_step s m =
  with_cuts s "fold_step" (fun s cuts ->
      Fmt.pf s.ppf "let[@inline always] fold_step %s off len ~init =@\n"
        s.m.src.buf;
      pp_entry ?cuts ~name:"fold_step"
        ~scope:(scope s [ ("init", None) ])
        s ~reject:(m ^ ".invalid init")
        ~accept:(call ~step:true s ~unboxed:false (m ^ ".valid init"));
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
  pp_entry ~name:"decode_unboxed" ~scope:(scope s []) s ~reject:"invalid ()"
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
      (* On OCaml the step functions hold values as [int64]. *)
      let s =
        if unboxed then s
        else
          {
            s with
            m = scan_mode ~decide:s.m.decide s.plan ~num:Boxed s.selected;
          }
      in
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

let scanner ?decide ?(target = `OCaml) ?fields ?step ~codec c =
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
        m = scan_mode ?decide plan ~num selected;
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
