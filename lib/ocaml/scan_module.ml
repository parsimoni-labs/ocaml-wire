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
     Names.clash (List.map (fun (o : output) -> (o.id, o.name)) plan.outputs)
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
  fold : string;  (** The name of the fold being written. *)
  selected : output list;
  m : mode;
  ppf : Format.formatter;
  taken : (string, unit) Hashtbl.t;
  args : arg list option;
      (** The arguments the fold hands [f], which it takes with [invalid] as
          arguments; [None] for a fold that calls a visitor by name. *)
}

(* A field a fold hands its [f]: the label [f] takes it with, its output, the
   text of its value in its carrier from an [int], the presence of the field
   itself when it is optional, and through a layer one option more, [None]
   where the innermost optional layer of [layer] is absent. *)
and arg = {
  label : string;
  value : output;
  typed : string -> string;
  own : output option;
  layered : bool;
  layer : output option;
}

(* The outputs among [selected] that are slots. *)
let slots (plan : plan) selected =
  let deep =
    Sset.of_list (List.map (fun (o : output) -> o.id) (slots plan.nodes))
  in
  List.filter (fun (o : output) -> Sset.mem o.id deep) selected

let scan_mode ?(decide = ignore) ?(note = ignore) (plan : plan) ~num selected =
  let ids = List.map (fun (o : output) -> o.id) in
  {
    num;
    src =
      (* On OxCaml it reads a buffer that may be [local] through [L], the
         runtime's [Target.Local]. *)
      (match num with
      | Unboxed -> { local_bytes_src with u = "L"; checked = "L" }
      | Fstar | Fstar64 | Fstar_u -> fstar_src
      | Tagged | Boxed -> bytes_src);
    ox = false;
    raw32 = false;
    ranged = true;
    live = live plan ~outputs:(ids selected);
    exported = Sset.of_list (ids selected);
    slots = Sset.of_list (ids (slots plan selected));
    ahead = Sset.empty;
    fail = (fun _ -> "");
    decide;
    note;
  }

(* The labelled arguments the visitor's [valid] receives; an F* [valid]
   takes them in order. *)
let pp_args ~fstar ~conv ppf outputs =
  if outputs = [] then Fmt.pf ppf "()"
  else if fstar then pp_list ppf (List.map conv outputs)
  else
    pp_list ppf
      (List.map (fun (o : output) -> Fmt.str "~%s:%s" o.name (conv o)) outputs)

(* An integer output as the visitor takes it, an [int64]: a value held as
   [int64_u] is converted. *)
let to_int64 (w : w) (o : output) =
  if rep w.m.num = Unboxed && not o.is_bool then
    let u = if w.m.num = Fstar_u then "P" else "U" in
    match List.assoc_opt o.id w.settled with
    | Some _ -> if w.m.num = Unboxed then "0L" else "0"
    | None -> Fmt.str "(%s.to_int64 %s)" u (output_text w o)
  else output_text w o

(* An integer output as an [f] that takes [int]s takes it. *)
let to_int (w : w) (o : output) =
  if o.is_bool then output_text w o
  else
    match List.assoc_opt o.id w.settled with
    | Some _ -> "0"
    | None ->
        let u = if rep w.m.num = Unboxed then "U" else "Int64" in
        Fmt.str "(%s.to_int %s)" u (output_text w o)

(* [v] as an option, [None] where the presence [p] is false. *)
let opt w p v =
  match p with
  | None -> Fmt.str "(Some %s)" v
  | Some p -> (
      match output_text w p with
      | "false" -> "None"
      | "true" -> Fmt.str "(Some %s)" v
      | p -> Fmt.str "(if %s then Some %s else None)" p v)

(* The value of [a] as [f] takes it, labelled. *)
let arg_text w a =
  let v = a.typed (to_int w a.value) in
  let v = if Option.is_some a.own then opt w a.own v else v in
  Fmt.str "~%s:%s" a.label (if a.layered then opt w a.layer v else v)

(* The call of the visitor's [valid], applied to [acc], with the outputs,
   where the walk [w] ends; or of [f], with its labelled arguments. *)
let call s valid (w : w) =
  match s.args with
  | None ->
      pf w "%s %a@\n" valid
        (pp_args ~fstar:(is_fstar w.m.num) ~conv:(to_int64 w))
        s.selected
  | Some [] -> pf w "%s@\n" valid
  | Some args -> pf w "%s %a@\n" valid pp_list (List.map (arg_text w) args)

(* The rejection of a record by the visitor [m] from [acc]: a call of the
   argument [invalid] in a fold that takes its consumer as arguments. *)
let reject s m acc =
  if Option.is_some s.args then "invalid " ^ acc
  else Fmt.str "%s.invalid %s" m acc

(* The acceptance of a record by the visitor [m] from [acc], with the outputs
   labelled to the argument [f] in a fold that takes its consumer as
   arguments. *)
let accept s m acc =
  if Option.is_some s.args then call s ("f " ^ acc)
  else call s (Fmt.str "%s.valid %s" m acc)

(* The consumer a fold that takes it as arguments passes on to each function
   it calls. *)
let consumer s = if Option.is_some s.args then " ~f ~invalid" else ""

(* The fold a module writes, as the F* rendering names it. *)
let step = "fold_step"

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
let with_cuts ?(trace = Fun.id) s name write =
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
        reserve;
        trace;
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
    | Fstar | Fstar64 -> (Some "P.int", Some "P.buf")
    | Fstar_u -> (Some "P.int64", Some "P.buf")
    | Tagged | Boxed -> (None, None)
  in
  let consumer =
    if Option.is_some s.args then [ ("f", None); ("invalid", None) ] else []
  in
  List.rev ([ ("buf", buf); ("base", num); ("stop", num) ] @ names @ consumer)

(* The walk over the [stop] bytes at [base], which starts by binding its
   slots: each rejection is the text [reject] and each acceptance what
   [accept] writes, in place, so a continuation the generator knows is never
   a closure. *)
let pp_scan ?cuts ?(known = 0) ~name ~scope s ~reject ~accept =
  pp_slots s.m s.ppf s.selected;
  let m = { s.m with fail = (fun _ -> reject) } in
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

(* The bounds of an entry on [int64_u] positions, as the sign of one
   [logor]. *)
let pp_unboxed_guard s ~check ~off_tested ~length ~reject =
  let num = s.m.num in
  let sub a b = Num.binop num "-" a b and logor a b = Num.binop num "lor" a b in
  let rest =
    if check then sub (sub length "stop") "base"
    else sub (sub length "base") "stop"
  in
  let sign =
    if not off_tested then rest
    else logor (if checks_len s.plan then "base" else logor "base" "stop") rest
  in
  let of_int = int64_of num "int" in
  Fmt.pf s.ppf
    "let base = %s off in@\nlet stop = %s len in@\nif %s then %s else@\n" of_int
    of_int
    (if num = Fstar_u then Fmt.str "P.int64_lt %s 0" sign
     else Fmt.str "U.compare %s #0L < 0" sign)
    reject

(* The bounds of an entry on [int]s, or through [P] on [length] for an F*
   walk. *)
let pp_int_guard s ~check ~off_tested ~length ~reject =
  let past =
    if check then Fmt.str "off > %s - len" length
    else Fmt.str "len > %s - off" length
  in
  let fstar_length =
    if length = "Bytes.length buf" then "(P.length buf)" else length
  in
  Fmt.pf s.ppf "if %s then %s else@\n"
    (match (is_fstar s.m.num, checks_len s.plan) with
    | true, true -> Fmt.str "P.lt off 0 || P.gt len (P.sub %s off)" fstar_length
    | true, false ->
        Fmt.str "P.lt off 0 || P.lt len 0 || P.gt len (P.sub %s off)"
          fstar_length
    | _, _ when not off_tested -> past
    | _, true -> "off < 0 || " ^ past
    | _, false -> Fmt.str "off < 0 || len < 0 || %s" past)
    reject

(* The entry tests [len < 0] unless {!checks_len} holds, when the walk's
   first check rejects a negative [len]. With [len] not negative,
   [len > length - off] rejects an [off] past the buffer. [length] is the
   length of [buf], an [int64_u] with [unboxed_length]: the bounds are then
   tested on the [int64_u] position and length, as the sign of one [logor],
   where an [int] holds no 64-bit difference of them that could wrap. Without
   [loads], a walk that reads nothing and answers [off] with {!checks_len},
   a negative [off] needs no test of its own: the answer is negative, which
   [ok] refuses. [check], whose callers pass a constant [len], tests
   [off > length - len], which folds that constant into one subtraction:
   without [loads] on [int]s, [int_length] the length as an [int], as
   nothing reads at a position computed from them. *)
let pp_entry ?cuts ?(length = "Bytes.length buf") ?int_length
    ?(unboxed_length = false) ?(loads = true) ?(check = false) ~name ~scope s
    ~reject ~accept =
  let unboxed = rep s.m.num = Unboxed in
  let off_tested = loads || not (checks_len s.plan) in
  let on_ints = check && (not off_tested) && Option.is_some int_length in
  let length = if on_ints then Option.get int_length else length in
  if unboxed && unboxed_length && not on_ints then
    pp_unboxed_guard s ~check ~off_tested ~length ~reject
  else begin
    pp_int_guard s ~check ~off_tested ~length ~reject;
    let of_int = int64_of s.m.num "int" in
    Fmt.pf s.ppf "let base = %s in@\nlet stop = %s in@\n"
      (if unboxed then of_int ^ " off" else "off")
      (if unboxed then of_int ^ " len" else "len")
  end;
  pp_scan ?cuts ~name ~scope s ~reject ~accept

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

(* The bytes {!read_ahead} loads for the field [f]. *)
let loaded (f : field) =
  match f.store with Int { width; _ } -> width / 8 | _ -> 0

(* The fields an unrolled frame loop reads for every frame of an iteration
   before the walk of its first frame: those the walk tests to choose an
   optional field, in the [need] bytes every frame has, and inside an optional
   field itself, which the walk reaches after checks of its own (it loads the
   leading fields first anyway). Each is a variable, the field's offset and
   the bytes loaded, with its value at a frame start. A frame's walk then
   dispatches on a value loaded before the previous frames' branches, so a
   mispredicted branch of one frame does not delay the next frame's choice by
   a load. With [leading], the fields of the leading prefixes too, for a
   loop whose frames start at offsets it loads. *)
let read_aheads ?(leading = false) s ~need =
  let tested = tested s.plan.nodes in
  List.filter_map
    (fun ((f : field), k) ->
      if Sset.mem f.id s.m.live && List.mem f.id tested then
        Option.map
          (fun ahead -> ((f.id, k, loaded f), ahead))
          (read_ahead s.m ~size:need ~k f)
      else None)
    (gated_fields ~gated:leading 0 s.plan.nodes)

(* The walk of one frame of a frame loop, which calls the module [visitor]
   by name, with the read-ahead [ahead] fields bound as variables: the
   expression of the next accumulator. *)
let frames_body ?cuts ~ahead s ~visitor ~name ~known =
  let body = Buffer.create 4096 in
  let bppf = Format.formatter_of_buffer body in
  let m = { s.m with ahead = Sset.of_list (List.map fst ahead) } in
  let typ = if s.m.num = Unboxed then Some "int64_u" else None in
  let names = ("current", None) :: List.map (fun (v, _) -> (v, typ)) ahead in
  pp_scan ?cuts ~known ~name ~scope:(scope s names) { s with ppf = bppf; m }
    ~reject:(reject s visitor "current")
    ~accept:(accept s visitor "current");
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
let pp_frames_loop ?cuts ?(ahead = []) s ~visitor ~name ~known ~unrolled =
  let read, assign, stride = frames_vars s in
  let body = frames_body ?cuts ~ahead s ~visitor ~name ~known in
  let used = List.filter (fun (v, _) -> mentions v body) ahead in
  if unrolled && Option.is_some cuts then pp_frames_unrolled s ~body ~used;
  Fmt.pf s.ppf "while %s do@\n" (frames_while s);
  pp_frame s ~body ~used (read ^ "cursor");
  Fmt.pf s.ppf "cursor %s %s@\ndone" assign
    (binop (pos_num s.m.num) "+" (read ^ "cursor") stride)

(* A batch of frames too short for the bounds a frame loop leaves out goes
   through the walk of one frame, already written, rather than a second copy
   of it. *)
let pp_frames_short s =
  let read, assign, stride = frames_vars s in
  let at = if s.m.num = Unboxed then "(U.to_int base)" else "base" in
  let one = Fmt.str "%s buf %s stride ~init:current%s" s.fold at (consumer s) in
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
let pp_frames_apart s name =
  let short = name ^ "_short" in
  Hashtbl.replace s.taken short ();
  Fmt.pf s.ppf
    "let[@inline never] %s buf ~stride ~last ~off ~init%s =@\n\
     let acc = ref init in@\n\
     let cursor = ref off in@\n"
    short (consumer s);
  pp_frames_short s;
  Fmt.pf s.ppf ";@\n!acc@\n@\n";
  Fmt.str "acc := %s buf ~stride ~last ~off ~init:!acc%s" short (consumer s)

(* The start of the frame loop [name]: its parameters, the check of its
   bounds and stride, and its accumulator, cursor and last offset. With
   [len] not negative, [len > Bytes.length buf - off] rejects an [off] past
   the buffer. *)
let pp_frames_head s ~name =
  let ppf = s.ppf and native = s.m.num = Unboxed in
  Fmt.pf ppf "let[@inline always] %s %s off len ~stride ~init%s =@\n" name
    s.m.src.buf (consumer s);
  Fmt.pf ppf
    "if stride <= 0 || off < 0 || len < 0 || len > Bytes.length buf - off then@\n\
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
let frame_loop s ~name ~need ~apart =
  let frames = if Sset.is_empty s.m.slots then unroll else 1 in
  Decision.Frame_loop
    {
      name;
      need;
      frames;
      short =
        (if need = 0 then None
         else if Option.is_some apart then Some Decision.Apart
         else Some Decision.Inline);
      ahead = (if frames > 1 then List.map fst (read_aheads s ~need) else []);
    }

(* [<fold>_frames], which calls the module [visitor]'s functions by name.
   The frame guard and a positive stride keep [last = off + len - stride]
   from overflowing and every frame inside [off + len]. The loop runs
   [len / stride] times, and each frame gets all its checks, so a short
   frame cannot borrow bytes from the next. *)
let pp_frames s ~visitor =
  let native = s.m.num = Unboxed in
  let read = if native then "" else "!" in
  let name = s.fold ^ "_frames" in
  let need = fixed_need s.plan.nodes in
  let apart =
    if need > 0 && not native then Some (pp_frames_apart s name) else None
  in
  s.m.decide (frame_loop s ~name ~need ~apart);
  with_cuts s name (fun s cuts ->
      let ppf = s.ppf in
      pp_frames_head s ~name;
      let loop ~unrolled known =
        let ahead =
          if unrolled && Option.is_some cuts then
            List.map snd (read_aheads s ~need:known)
          else []
        in
        pp_frames_loop ?cuts ~ahead s ~visitor ~name ~known ~unrolled
      in
      (* The loop over frames that hold the leading prefixes comes last,
         where the compiler closes it with one conditional branch. *)
      if need > 0 then begin
        Fmt.pf ppf "if stride < %d then (@\n" need;
        (match apart with
        | Some call -> Fmt.pf ppf "%s" call
        | None -> pp_frames_short s);
        Fmt.pf ppf ")@\nelse (@\n";
        loop ~unrolled:true need;
        Fmt.pf ppf ")"
      end
      else loop ~unrolled:true 0;
      Fmt.pf ppf ";@\n%sacc@\n@\n" read)

(* The length of [buf] as the [_within] walk of a visitor's fold takes it:
   an [int64_u] where values are. *)
let length_of s =
  if s.m.num = Unboxed then "U.of_int (Bytes.length buf)"
  else "Bytes.length buf"

(* The loop of [fold_step_index] that {!pp_fold_index_ahead} writes to
   [out]: its name, its target, the bytes [need] its frames are known to
   hold, the fields [used] it reads ahead and their values at a frame start,
   the walk [body] of a frame that holds [need] bytes and [within], the walk
   of any other. *)
type index_loop = {
  out : Format.formatter;
  index : string;
  unboxed : bool;
  need : int;
  used : (string * (string -> string)) list;
  body : string;
  within : string;
  consumer : string;
}

(* On OCaml the variables the loop carries are [ref]s, which the compiler
   makes mutable variables, and a position is an [int]. *)
let index_read l v = if l.unboxed then v else "!" ^ v
let index_assign l = if l.unboxed then "<-" else ":="

let index_mutable l v e =
  if l.unboxed then Fmt.str "let mutable %s = %s in" v e
  else Fmt.str "let %s = ref %s in" v e

let index_zero l = if l.unboxed then "#0L" else "0L"

(* The fields read ahead of a frame: one function each, or on OxCaml one for
   all of several, which loads them under one test, as an unboxed tuple. *)
let ahead_tuple l =
  match l.used with _ :: _ :: _ when l.unboxed -> true | _ -> false

(* The function that reads ahead the [k]th field of [l]: [_ahead] when one
   reads them all. *)
let ahead_name l k =
  match l.used with
  | [ _ ] -> l.index ^ "_ahead"
  | _ when ahead_tuple l -> l.index ^ "_ahead"
  | _ -> Fmt.str "%s_ahead_%d" l.index k

(* The functions the loop of [l] calls on each frame, so that none of its
   functions tests more than a few conditions: [_known], whether a frame
   lies in the buffer and holds [need] bytes; the fields read ahead of a
   known frame, else zero; and [_frame], the walk of a frame, [body] where
   it is known and [within] otherwise. On OxCaml the bounds are tested as
   the sign of one [logor] of [int64_u]s, which hold the differences of two
   [int]s without wrapping. *)
let pp_index_functions l ~buf =
  let ppf = l.out and zero = index_zero l and index = l.index in
  let pos v = if l.unboxed then Fmt.str "(%s : int64_u)" v else v in
  if l.unboxed then
    Fmt.pf ppf
      "let[@inline always] %s_known ~(length : int64_u) %s %s =@\n\
       U.compare (U.logor (U.logor base (U.sub (U.sub length base) stop)) \
       (U.sub stop #%dL)) #0L >= 0@\n\
       @\n"
      index (pos "base") (pos "stop") l.need
  else
    Fmt.pf ppf
      "let[@inline always] %s_known ~length base stop =@\n\
       base >= 0 && stop >= %d && stop <= length - base@\n\
       @\n"
      index l.need;
  if ahead_tuple l then begin
    let list f = String.concat ", " (List.map f l.used) in
    Fmt.pf ppf
      "let[@inline always] %s_ahead %s ~known %s =@\n\
       if known then #(%s) else #(%s)@\n\
       @\n"
      index buf (pos "base")
      (list (fun (_, value) -> value "base"))
      (list (fun _ -> zero))
  end
  else
    List.iteri
      (fun k (_, value) ->
        Fmt.pf ppf
          "let[@inline always] %s %s ~known %s =@\n\
           if known then %s else %s@\n\
           @\n"
          (ahead_name l k) buf (pos "base") (value "base") zero)
      l.used;
  let int v = if l.unboxed then Fmt.str "(U.to_int %s)" v else v in
  Fmt.pf ppf
    "let[@inline always] %s_frame %s ~%s ~known ~%s ~%s ~current%s%s =@\n\
     if known then (@\n\
     %s) else %s buf ~length %s %s ~init:current%s@\n\
     @\n"
    index buf
    (if l.unboxed then "(length : int64_u)" else "length")
    (pos "base") (pos "stop")
    (String.concat "" (List.map (fun (v, _) -> " ~" ^ pos v) l.used))
    l.consumer l.body l.within (int "base") (int "stop") l.consumer

(* The frame at index [i] as the variables suffixed [x]: its start, its
   length, [known] when it lies in the buffer and holds [need] bytes, and
   then the fields read ahead, else zero. *)
let pp_index_next l x i =
  let ppf = l.out and index = l.index in
  let pos e = if l.unboxed then Fmt.str "U.of_int (%s)" e else e in
  Fmt.pf ppf
    "let base%s = %s in@\n\
     let stop%s = %s in@\n\
     let known%s = %s_known ~length base%s stop%s in@\n"
    x
    (Fmt.kstr pos "Array.unsafe_get offs %s" i)
    x
    (Fmt.kstr pos "Array.unsafe_get lens %s" i)
    x index x x;
  if ahead_tuple l then
    Fmt.pf ppf "let #(%s) = %s buf ~known:known%s base%s in@\n"
      (String.concat ", " (List.map (fun (v, _) -> v ^ x) l.used))
      (ahead_name l 0) x x
  else
    List.iteri
      (fun k (v, _) ->
        Fmt.pf ppf "let %s%s = %s buf ~known:known%s base%s in@\n" v x
          (ahead_name l k) x x)
      l.used

(* The walk of the frame suffixed [x], whose variables [carried] reads from
   the loop's references. *)
let pp_index_walk ?(carried = false) l x =
  let read = index_read l in
  let v name = if carried then read (name ^ x) else name ^ x in
  Fmt.pf l.out
    "acc %s %s_frame buf ~length ~known:%s ~base:%s ~stop:%s ~current:%s%s%s"
    (index_assign l) l.index (v "known") (v "base") (v "stop") (read "acc")
    (String.concat ""
       (List.map (fun (n, _) -> Fmt.str " ~%s:%s" n (v n)) l.used))
    l.consumer

(* The frames of the index, two an iteration, so the loop polls and tests
   its bound once per two frames, the next frame read before the current
   one is walked. *)
let pp_index_frames l =
  let ppf = l.out and read = index_read l in
  let vars = "base" :: "stop" :: "known" :: List.map fst l.used in
  pp_index_next l "_0" "0";
  List.iter
    (fun v -> Fmt.pf ppf "%s@\n" (index_mutable l (v ^ "_c") (v ^ "_0")))
    vars;
  Fmt.pf ppf "%s@\nwhile %s < n - 1 do@\n" (index_mutable l "i" "1") (read "i");
  pp_index_next l "_1" (read "i");
  pp_index_walk ~carried:true l "_c";
  Fmt.pf ppf ";@\n";
  pp_index_next l "_2" (Fmt.str "(%s + 1)" (read "i"));
  pp_index_walk l "_1";
  Fmt.pf ppf ";@\n";
  List.iter (fun v -> Fmt.pf ppf "%s_c %s %s_2;@\n" v (index_assign l) v) vars;
  Fmt.pf ppf "i %s %s + 2@\ndone;@\nif %s < n then begin@\n" (index_assign l)
    (read "i") (read "i");
  pp_index_next l "_1" (read "i");
  pp_index_walk ~carried:true l "_c";
  Fmt.pf ppf ";@\n";
  pp_index_walk l "_1";
  Fmt.pf ppf "@\nend@\nelse@\n";
  pp_index_walk ~carried:true l "_c"

(* [fold_step_index] for a codec whose frames need [need] bytes before
   their first variable offset. The loop is pipelined one frame deep:
   before it walks a frame, it loads the next frame's offset and length, tests
   that the frame lies in the buffer and holds [need] bytes, and if so loads
   its [ahead] fields. The walk of a frame then dispatches on values loaded
   before the previous frame's branches, so a branch of one frame that
   mispredicts does not delay the next frame's choice by the loads of its
   offset and fields. A frame that holds [need] bytes is walked knowing them,
   any other through [within]. *)
let pp_fold_index_ahead s ~within ~m ~need ~ahead =
  let index = s.fold ^ "_index" in
  with_cuts s index (fun s cuts ->
      let body =
        frames_body ?cuts ~ahead s ~visitor:m ~name:index ~known:need
      in
      let used = List.filter (fun (v, _) -> mentions v body) ahead in
      let l =
        {
          out = s.ppf;
          index;
          unboxed = s.m.num = Unboxed;
          need;
          used;
          body;
          within;
          consumer = consumer s;
        }
      in
      List.iter
        (fun n -> Hashtbl.replace s.taken n ())
        ((index ^ "_known") :: (index ^ "_frame")
        :: List.mapi (fun k _ -> ahead_name l k) used);
      pp_index_functions l ~buf:s.m.src.buf;
      Fmt.pf s.ppf
        "let[@inline always] %s %s ~(offs : int array) ~(lens : int array) \
         ~init%s =@\n\
         let n = Array.length offs in@\n\
         if Array.length lens <> n then@\n\
         invalid_arg \"%s: offs and lens differ in length\";@\n\
         let length = %s in@\n\
         %s@\n\
         if n > 0 then begin@\n"
        index s.m.src.buf (consumer s) index (length_of s)
        (index_mutable l "acc" "init");
      pp_index_frames l;
      Fmt.pf s.ppf "@\nend;@\n%s@\n@\n" (index_read l "acc"))

(* The plain [_index] of a visitor's fold, which folds [within] over the frames of an
   index. *)
let pp_fold_step_index_plain s ~within ~buf =
  let index = s.fold ^ "_index" in
  Hashtbl.replace s.taken index ();
  (* As in [fold_frames], OxCaml walks [unroll] frames an iteration, so the
     loop polls and tests its bound once per [unroll] frames. Both arrays have
     [n] elements, so every index of the loop is in range in each. *)
  let frames =
    if s.m.num = Unboxed && Sset.is_empty s.m.slots then unroll else 1
  in
  s.m.decide
    (Decision.Frame_loop
       { name = index; need = 0; frames; short = None; ahead = [] });
  let frame ?(acc = "acc") i =
    Fmt.str
      "%s buf ~length (Array.unsafe_get offs %s) (Array.unsafe_get lens %s) \
       ~init:%s%s"
      within i i acc (consumer s)
  in
  Fmt.pf s.ppf
    "let[@inline always] %s %s ~(offs : int array) ~(lens : int array) ~init%s \
     =@\n\
     let n = Array.length offs in@\n\
     if Array.length lens <> n then@\n\
     invalid_arg \"%s: offs and lens differ in length\";@\n\
     let length = %s in@\n"
    index buf (consumer s) index (length_of s);
  if s.m.num = Unboxed then begin
    (* The accumulator may have any layout, which a [ref] cannot hold. *)
    Fmt.pf s.ppf "let mutable acc = init in@\nlet mutable i = 0 in@\n";
    if frames > 1 then begin
      Fmt.pf s.ppf "while i < n - %d do@\n" (frames - 1);
      for k = 0 to frames - 1 do
        Fmt.pf s.ppf "acc <- %s;@\n"
          (frame (if k = 0 then "i" else Fmt.str "(i + %d)" k))
      done;
      Fmt.pf s.ppf "i <- i + %d@\ndone;@\n" frames
    end;
    Fmt.pf s.ppf "while i < n do@\nacc <- %s;@\ni <- i + 1@\ndone;@\nacc@\n@\n"
      (frame "i")
  end
  else
    Fmt.pf s.ppf
      "let acc = ref init in@\n\
       for i = 0 to n - 1 do@\n\
       acc := %s@\n\
       done;@\n\
       !acc@\n\
       @\n"
      (frame ~acc:"!acc" "i")

(* [fold_step_index]: pipelined when its frames need leading bytes, a plain
   loop otherwise. *)
let pp_fold_step_index s ~within ~m =
  let buf = s.m.src.buf in
  let need = fixed_need s.plan.nodes in
  if Sset.is_empty s.m.slots && need > 0 then begin
    let ahead = read_aheads ~leading:true s ~need in
    s.m.decide
      (Decision.Frame_loop
         {
           name = s.fold ^ "_index";
           need;
           frames = 2;
           short = None;
           ahead = List.map fst ahead;
         });
    pp_fold_index_ahead s ~within ~m ~need ~ahead:(List.map snd ahead)
  end
  else pp_fold_step_index_plain s ~within ~buf

(* The walk of one frame of a fold with a visitor, which the fold and its
   [_index] call. *)
let within s = s.fold ^ "_within"

(* {!length_of} in F*, of the buffer [buf]. *)
let fstar_length_of s buf =
  if rep s.m.num = Unboxed then Fmt.str "(P.int64_of_int (P.length %s))" buf
  else Fmt.str "(P.length %s)" buf

(* The [_within] walk of a visitor's fold, and the fold, which calls it with
   the length of [buf]. In F*, each parameter is typed and the accumulator
   is [m]'s [t]. *)
let pp_step_within ?trace s m =
  let buf = s.m.src.buf in
  let unboxed = rep s.m.num = Unboxed and within = within s in
  with_cuts ?trace s s.fold (fun s cuts ->
      Hashtbl.replace s.taken within ();
      let body = Buffer.create 4096 in
      let ppf = Format.formatter_of_buffer body in
      pp_entry ?cuts ~length:"length" ~unboxed_length:true ~name:s.fold
        ~scope:(scope s [ ("init", None) ])
        { s with ppf } ~reject:(reject s m "init") ~accept:(accept s m "init");
      Format.pp_print_flush ppf ();
      let body = Buffer.contents body in
      (* A walk that reads no byte takes [buf] for its type alone. *)
      let param =
        if mentions "buf" body then buf
        else
          let i = String.index buf 'b' in
          String.sub buf 0 i ^ "_" ^ String.sub buf i (String.length buf - i)
      in
      (* Its callers pass the length of [buf], which F* requires. *)
      if is_fstar s.m.num then
        Fmt.pf s.ppf
          "let %s (%s:P.buf) (length:%s{length == %s}) (off:P.int) (len:P.int) \
           (init:%s.t) : %s.t =@\n\
           %s@\n"
          within param
          (if unboxed then "P.int64" else "P.int")
          (fstar_length_of s param) m m body
      else
        Fmt.pf s.ppf "let[@inline always] %s %s ~%s off len ~init%s =@\n%s@\n"
          within param
          (if unboxed then "(length : int64_u)" else "length")
          (consumer s) body);
  if is_fstar s.m.num then
    Fmt.pf s.ppf
      "let %s (buf:P.buf) (off:P.int) (len:P.int) (init:%s.t) : %s.t =@\n\
       %s buf %s off len init@\n\
       @\n"
      s.fold m m within (fstar_length_of s "buf")
  else
    Fmt.pf s.ppf
      "let[@inline always] %s %s off len ~init%s =@\n\
       %s buf ~length:(%s) off len ~init%s@\n\
       @\n"
      s.fold buf (consumer s) within (length_of s) (consumer s)

(* A visitor's fold, which calls the module [m]'s functions by name, and its
   [_index], which folds it over the frames of an index. Both go through its
   [_within], whose [length] is that of [buf], so that the loop over the
   index loads it once. *)
let pp_fold_step s m =
  pp_step_within s m;
  pp_fold_step_index s ~within:(within s) ~m

(* [check b ~off ~len], the walk of [fold] with no output, which answers
   [off] for a record it accepts and [-1] otherwise, in the length of [b] it
   reads once. A codec of constant [size] takes no window: [check b ~off]
   walks the record over its [size] bytes, which the bounds fold into one
   test. *)
let pp_check ?size s =
  let unboxed = s.m.num = Unboxed in
  let s =
    {
      s with
      selected = [];
      m = scan_mode ~decide:s.m.decide s.plan ~num:s.m.num [];
    }
  in
  let pp ~loads s cuts =
    let body = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer body in
    pp_entry ?cuts
      ~length:(if unboxed then "(U.of_int length)" else "length")
      ~int_length:"length" ~unboxed_length:true ~loads ~check:true ~name:"check"
      ~scope:(scope s [ ("off", None) ])
      { s with ppf } ~reject:"(-1)"
      ~accept:(fun w -> pf w "off@\n");
    Format.pp_print_flush ppf ();
    let body = Buffer.contents body in
    let reads = mentions "buf" body || mentions "length" body in
    let b = if reads then "b" else "_b" in
    Fmt.pf s.ppf
      "let[@inline always] check (%s : 'b Wire_runtime.Buf.t%s) %s : 'b check =@\n"
      b
      (if unboxed then " @ local" else "")
      (if Option.is_some size then "~off" else "~off ~len");
    if reads then
      Fmt.pf s.ppf
        "let buf = (b : _ Wire_runtime.Buf.t :> bytes) in@\n\
         let length = Bytes.length buf in@\n";
    Option.iter (Fmt.pf s.ppf "let len = %d in@\n") size;
    Fmt.pf s.ppf "%s@\n" body
  in
  (* A walk that reads nothing answers a negative [off] itself: whether it
     reads is found by writing it once aside. *)
  let probe = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer probe in
  with_cuts
    { s with ppf; taken = Hashtbl.copy s.taken }
    "check" (pp ~loads:true);
  Format.pp_print_flush ppf ();
  let loads = mentions "buf" (Buffer.contents probe) in
  with_cuts s "check" (pp ~loads)

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
  String.concat "\n" (Lines.prune (String.split_on_char '\n' text))

type fold = { selected : output list; args : arg list }

(* The walks of a module: its [fold], which takes its consumer as arguments,
   on OCaml holding values as [int64], then [check]. The fold's functions are
   those of the visitor folds of the F* rendering, named [fold_step], with the
   arguments [f] and [invalid] where those call the visitor by name. *)
let pp_body ?fold ?size s =
  let unboxed = s.m.num = Unboxed in
  if unboxed then
    Fmt.pf s.ppf
      "module U = Wire_runtime.Target.Int64_u@\n\
       module B = Wire_runtime.Target@\n\
       module L = Wire_runtime.Target.Local@\n\
       @\n";
  Option.iter
    (fun f ->
      let num = if unboxed then Unboxed else Boxed in
      let s =
        {
          s with
          fold = step;
          selected = f.selected;
          m = scan_mode ~decide:s.m.decide s.plan ~num f.selected;
          args = Some f.args;
        }
      in
      pp_fold_step s "";
      pp_frames s ~visitor:"")
    fold;
  Fmt.pf s.ppf "type 'b view = int@\ntype 'b check = int@\n@\n";
  pp_check ?size s

let walks ?decide ~target ?fold ?size ~taken plan =
  let num = if target = `OxCaml then Unboxed else Tagged in
  let b = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer b in
  let s =
    {
      plan;
      fold = step;
      selected = [];
      m = scan_mode ?decide plan ~num [];
      ppf;
      taken;
      args = None;
    }
  in
  pp_body ?fold ?size s;
  Format.pp_print_flush ppf ();
  without_dead_bindings (Buffer.contents b)

(* {2 The fold}

   [fold] places the records as its arguments say and calls the walks above:
   back to back, a record of constant size through the frame loop over slots
   of that size, any other one record at a time, from where the one before
   ends. Without [invalid], a refused record stops the fold through a local
   exception, and the error of the first refused record is found again, off
   the hot path, by [check] and [validate] on its window alone. *)

(* The errors of the first refused window, slot or frame, which [fold]
   finds off its hot path. *)
let pp_fold_errors ppf ~size =
  Fmt.pf ppf
    "let[@inline never] window_error buf off len =@\n\
     let length = Bytes.length buf in@\n\
     let first = if off < 0 then 0 else if off > length then length else off in@\n\
     let stop =@\n\
     if len < 0 then first else if len > length - first then length else first \
     + len@\n\
     in@\n\
     match validate (Bytes.sub buf first (stop - first)) ~off:0 with@\n\
     | Ok () -> no_error ()@\n\
     | Error e -> { e with Wire_runtime.Error.at = e.Wire_runtime.Error.at + \
     first }@\n\
     @\n\
     let window_ok buf off len =@\n\
     off >= 0 && len >= 0 && len <= Bytes.length buf - off@\n\
     &&@\n\
     match Wire_runtime.Buf.v buf with@\n\
     | Wire_runtime.Buf.Any b -> %s@\n\
     @\n\
     let[@inline never] slot_error buf ~stride =@\n\
     let n = Bytes.length buf / stride in@\n\
     let rec go i =@\n\
     if i >= n then no_error ()@\n\
     else if window_ok buf (i * stride) stride then go (i + 1)@\n\
     else window_error buf (i * stride) stride@\n\
     in@\n\
     go 0@\n\
     @\n\
     let[@inline never] frame_error buf ~offs ~lens =@\n\
     let rec go i =@\n\
     if i >= Array.length offs then no_error ()@\n\
     else if window_ok buf offs.(i) lens.(i) then go (i + 1)@\n\
     else window_error buf offs.(i) lens.(i)@\n\
     in@\n\
     go 0@\n\
     @\n"
    (match size with
    | Some n -> Fmt.str "len >= %d && is_ok (check b ~off)" n
    | None -> "is_ok (check b ~off ~len)")

(* The fold of records back to back: a record of constant size through the
   frame loop over slots of that size, any other one at a time. *)
let fold_back ~name ~size =
  match size with
  | Some 0 ->
      Fmt.str "invalid_arg \"%s.fold: records of no bytes back to back\"" name
  | Some n ->
      Fmt.str
        "let length = Bytes.length buf in@\n\
         let whole = length / %d * %d in@\n\
         match %s_frames buf 0 whole ~stride:%d ~init ~f ~invalid:stop with@\n\
         | acc ->@\n\
         if whole = length then Ok acc@\n\
         else Error (window_error buf whole (length - whole), acc)@\n\
         | exception Refused acc -> Error (slot_error buf ~stride:%d, acc)"
        n n step n n
  | None ->
      Fmt.str
        "let length = Bytes.length buf in@\n\
         let rec go off acc =@\n\
         if off >= length then Ok acc@\n\
         else@\n\
         match %s buf off (length - off) ~init:acc ~f ~invalid:stop with@\n\
         | exception Refused acc ->@\n\
         Error (window_error buf off (length - off), acc)@\n\
         | acc ->@\n\
         let next = end_at buf length off in@\n\
         if next <= off then Ok acc else go next acc@\n\
         in@\n\
         go 0 init"
        step

let pp_fold ppf ~name ~size =
  pp_fold_errors ppf ~size;
  let back = fold_back ~name ~size in
  Fmt.pf ppf
    "let[@inline] fold (type a) ?stride ?offs ?lens ?invalid buf ~(init : a) \
     ~f : (a, Wire_runtime.Error.t * a) result =@\n\
     let exception Refused of a in@\n\
     let stop acc = raise_notrace (Refused acc) in@\n\
     match (stride, offs, lens) with@\n\
     | None, None, None -> (@\n\
     if Option.is_some invalid then@\n\
     invalid_arg \"%s.fold: ~invalid with records back to back\";@\n\
     %s)@\n\
     | Some stride, None, None -> (@\n\
     if stride <= 0 then invalid_arg \"%s.fold: ~stride is not positive\";@\n\
     let len = Bytes.length buf / stride * stride in@\n\
     match invalid with@\n\
     | Some invalid -> Ok (%s_frames buf 0 len ~stride ~init ~f ~invalid)@\n\
     | None -> (@\n\
     match %s_frames buf 0 len ~stride ~init ~f ~invalid:stop with@\n\
     | acc -> Ok acc@\n\
     | exception Refused acc -> Error (slot_error buf ~stride, acc)))@\n\
     | None, Some offs, Some lens -> (@\n\
     match invalid with@\n\
     | Some invalid -> Ok (%s_index buf ~offs ~lens ~init ~f ~invalid)@\n\
     | None -> (@\n\
     match %s_index buf ~offs ~lens ~init ~f ~invalid:stop with@\n\
     | acc -> Ok acc@\n\
     | exception Refused acc -> Error (frame_error buf ~offs ~lens, acc)))@\n\
     | Some _, Some _, _ | Some _, _, Some _ ->@\n\
     invalid_arg \"%s.fold: ~stride with ~offs and ~lens\"@\n\
     | None, Some _, None | None, None, Some _ ->@\n\
     invalid_arg \"%s.fold: ~offs without ~lens\"@\n\
     @\n"
    name back name step step step step name name

(* {1 The F* rendering} *)

(* The walk of a frame of a frame loop, rendered as a function: its name,
   its parameters with their types, and the bytes the loop knows. *)
type frame = { name : string; params : (string * string) list; need : int }

type fstar = {
  body : string;
  notes : note list;
  handed_out : output list;
  frame : frame option;
}

(* The F* rendering holds every value in a binding: an output under more
   than [slot_depth] optional fields is a slot, a mutable local, which F*
   has not. *)
let no_slots (plan : plan) =
  if Planner.slots plan.nodes <> [] then
    fail_unsupported "the F* rendering of a slot"

(* [text] with each local function defined as F* does: [let[@local]] is a
   hint to the OCaml compiler. *)
let unlocal text =
  let k = String.length Lines.local_prefix in
  String.concat "\n"
    (List.map
       (fun line ->
         if String.starts_with ~prefix:Lines.local_prefix line then
           "let " ^ String.sub line k (String.length line - k)
         else line)
       (String.split_on_char '\n' text))

(* The F* rendering of the function [write] writes, a walk of [plan] held as
   [num] whose OCaml holds values as [rep num]. *)

(* {2 The walks of the frame loops}

   The loops of a visitor's [_frames] and [_index] folds walk each frame
   that holds the bytes they test, [need], with the walk below: knowing
   those bytes, and on the index loop with the fields it loads ahead as
   variables. The OCaml cuts it out as [<f>_frames_record] and
   [<f>_index_record], the fold [fold_<f>] without its prefix; F* renders it as a function of the same name and
   parameters, each typed with what the loop ensures of it. *)

(* The walk of a frame of the loop of a visitor's [_frames] fold ([index]
   false) or of its [_index], which calls [m] by name. *)
let pp_frame_walk ?trace ~frame ~index s m =
  let name = s.fold ^ if index then "_index" else "_frames" in
  let need = fixed_need s.plan.nodes in
  let ahead =
    if index then List.map snd (read_aheads ~leading:true s ~need)
    else List.map snd (read_aheads s ~need)
  in
  with_cuts ?trace s name (fun s cuts ->
      let body = frames_body ?cuts ~ahead s ~visitor:m ~name ~known:need in
      let pos = if rep s.m.num = Unboxed then "P.int64" else "P.int" in
      let param (v, typ) = if mentions v body then Some (v, typ) else None in
      let params =
        List.filter_map param
          ([
             ("buf", "P.buf");
             ( "base",
               Fmt.str "%s{0 <= base /\\ base + %d <= P.length buf}" pos need );
             ( "stop",
               Fmt.str "%s{%d <= stop /\\ base + stop <= P.length buf}" pos need
             );
             ("current", m ^ ".t");
           ]
          @ List.map
              (fun (v, value) ->
                (v, Fmt.str "P.int64{%s == %s}" v (value "base")))
              ahead)
      in
      let cut = Fmt.str "%s_record" (cut_prefix name) in
      Fmt.pf s.ppf "let %s %s : %s.t =@\n%s@\n" cut
        (String.concat " "
           (List.map (fun (v, t) -> Fmt.str "(%s:%s)" v t) params))
        m body;
      frame := Some { name = cut; params; need })

(* The F* fold is named [fold_step], as the tree test reads it. *)
let rendered ?fields c ~num write =
  let plan = scan_plan c in
  no_slots plan;
  let selected = selected plan fields in
  let walk ~trace ~num ~note =
    let b = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer b in
    let s =
      {
        plan;
        fold = "fold_step";
        selected;
        m = scan_mode ~note plan ~num selected;
        ppf;
        taken = Hashtbl.create 16;
        args = None;
      }
    in
    let frame = ref None in
    write ~trace ~frame s;
    Format.pp_print_flush ppf ();
    (Buffer.contents b, !frame)
  in
  (* The F* walk writes in place what the OCaml walk cuts, at the places
     the OCaml walk, whose text merlint measures, cuts. *)
  let choices = Queue.create () in
  let record b =
    Queue.push b choices;
    b
  in
  ignore (walk ~trace:record ~num:(rep num) ~note:ignore);
  let replay _ =
    match Queue.take_opt choices with
    | Some b -> b
    | None -> invalid_arg "Wire_ocaml.fstar: the F* walk left the OCaml one"
  in
  let notes = ref [] in
  let text, frame =
    walk ~trace:replay ~num ~note:(fun n -> notes := n :: !notes)
  in
  {
    body = unlocal (without_dead_bindings text);
    notes = List.rev !notes;
    handed_out = selected;
    frame;
  }

let fstar_step ?fields ~step c =
  rendered ?fields c ~num:Fstar64 (fun ~trace ~frame:_ s ->
      pp_step_within ~trace s step)

let fstar_ox_step ?fields ~step c =
  rendered ?fields c ~num:Fstar_u (fun ~trace ~frame:_ s ->
      pp_step_within ~trace s step)

let fstar_frame ?fields ~ox ~index ~step c =
  rendered ?fields c
    ~num:(if ox then Fstar_u else Fstar64)
    (fun ~trace ~frame s -> pp_frame_walk ~trace ~frame ~index s step)
