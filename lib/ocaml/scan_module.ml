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

(* The labelled arguments [valid] receives; an F* [valid] takes them in
   order. *)
let pp_args ~fstar ~conv ppf outputs =
  if outputs = [] then Fmt.pf ppf "()"
  else if fstar then pp_list ppf (List.map conv outputs)
  else
    pp_list ppf
      (List.map (fun (o : output) -> Fmt.str "~%s:%s" o.name (conv o)) outputs)

(* An integer output as [valid] takes it. A value held as [int64_u] is
   converted: to an [int64] with [step], to an [int] otherwise. *)
let to_int (w : w) ~step (o : output) =
  if rep w.m.num = Unboxed && not o.is_bool then
    let u = if w.m.num = Fstar_u then "P" else "U" in
    match List.assoc_opt o.id w.settled with
    | Some _ -> if step && w.m.num = Unboxed then "0L" else "0"
    | None ->
        Fmt.str "(%s.%s %s)" u
          (if step then "to_int64" else "to_int")
          (output_text w o)
  else output_text w o

(* The call of [valid] with the outputs, where the walk [w] ends. With
   [step], [valid] is the consumer of the step functions and takes [int64]. *)
let call ?(step = false) s valid (w : w) =
  pf w "%s %a@\n" valid
    (pp_args ~fstar:(is_fstar w.m.num) ~conv:(to_int w ~step))
    s.selected

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
  List.rev ([ ("buf", buf); ("base", num); ("stop", num) ] @ names)

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
let frames_body ?cuts ?step ~ahead s ~name ~names ~invalid ~valid ~known =
  let body = Buffer.create 4096 in
  let bppf = Format.formatter_of_buffer body in
  let m = { s.m with ahead = Sset.of_list (List.map fst ahead) } in
  let typ = if s.m.num = Unboxed then Some "int64_u" else None in
  let names = names @ List.map (fun (v, _) -> (v, typ)) ahead in
  pp_scan ?cuts ~known ~name ~scope:(scope s names) { s with ppf = bppf; m }
    ~reject:(invalid ^ " current")
    ~accept:(call ~step:(Option.is_some step) s (valid ^ " current"));
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
let pp_frames_loop ?cuts ?step ?(ahead = []) s ~name ~names ~invalid ~valid
    ~known ~unrolled =
  let read, assign, stride = frames_vars s in
  let body =
    frames_body ?cuts ?step ~ahead s ~name ~names ~invalid ~valid ~known
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
    | Some _ -> Fmt.str "%s buf %s stride ~init:current" s.fold at
    | None ->
        Fmt.str
          "%s buf %s stride ~invalid:(fun () -> invalid current) ~valid:(valid \
           current)"
          s.fold at
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
let frames_scope ?step () =
  match step with
  | Some _ -> [ ("current", None) ]
  | None -> [ ("current", None); ("invalid", None); ("valid", None) ]

(* The start of the frame loop [name]: its parameters, the check of its
   bounds and stride, and its accumulator, cursor and last offset. With
   [len] not negative, [len > Bytes.length buf - off] rejects an [off] past
   the buffer. *)
let pp_frames_head s ~name ~callbacks =
  let ppf = s.ppf and native = s.m.num = Unboxed in
  Fmt.pf ppf "let[@inline always] %s %s off len ~stride ~init %s =@\n" name
    s.m.src.buf callbacks;
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

(* [fold_frames], or with [step] [fold_step_frames], which calls the module
   [step]'s functions by name. The frame guard and a positive stride keep
   [last = off + len - stride] from overflowing and every frame inside
   [off + len]. The loop runs [len / stride] times, and each frame gets all
   its checks, so a short frame cannot borrow bytes from the next. *)
let pp_frames ?step s =
  let native = s.m.num = Unboxed in
  let read = if native then "" else "!" in
  let name = s.fold ^ "_frames" in
  let callbacks, invalid, valid =
    match step with
    | Some m -> ("", m ^ ".invalid", m ^ ".valid")
    | None -> ("~invalid ~valid", "invalid", "valid")
  in
  let names = frames_scope ?step () in
  let need = fixed_need s.plan.nodes in
  let apart =
    if need > 0 && not native then Some (pp_frames_apart ?step s name) else None
  in
  s.m.decide (frame_loop s ~name ~need ~apart);
  with_cuts s name (fun s cuts ->
      let ppf = s.ppf in
      pp_frames_head s ~name ~callbacks;
      let loop ~unrolled known =
        let ahead =
          if unrolled && Option.is_some cuts then
            List.map snd (read_aheads s ~need:known)
          else []
        in
        pp_frames_loop ?cuts ?step ~ahead s ~name ~names ~invalid ~valid ~known
          ~unrolled
      in
      (* The loop over frames that hold the leading prefixes comes last,
         where the compiler closes it with one conditional branch. *)
      if need > 0 then begin
        Fmt.pf ppf "if stride < %d then (@\n" need;
        (match apart with
        | Some call -> Fmt.pf ppf "%s" call
        | None -> pp_frames_short ?step s);
        Fmt.pf ppf ")@\nelse (@\n";
        loop ~unrolled:true need;
        Fmt.pf ppf ")"
      end
      else loop ~unrolled:true 0;
      Fmt.pf ppf ";@\n%sacc@\n@\n" read)

(* [fold], which takes its consumer as arguments. *)
let pp_fold ?trace s =
  let name = s.fold in
  with_cuts ?trace s name (fun s cuts ->
      let callbacks =
        if is_fstar s.m.num then begin
          Fmt.pf s.ppf
            "let %s (#r:Type) (buf:P.buf) (off:P.int) (len:P.int) \
             (invalid:unit -> r) (valid:valid_t r) : r =@\n"
            name;
          [ ("invalid", Some "unit -> r"); ("valid", Some "valid_t r") ]
        end
        else begin
          Fmt.pf s.ppf "let[@inline always] %s %s off len ~invalid ~valid =@\n"
            name s.m.src.buf;
          [ ("invalid", None); ("valid", None) ]
        end
      in
      pp_entry ?cuts ~name ~scope:(scope s callbacks) s ~reject:"invalid ()"
        ~accept:(call s "valid");
      Fmt.pf s.ppf "@\n")

(* The length of [buf] as the [_within] walk of a visitor's fold takes it:
   an [int64_u] where values are. *)
let length_of s =
  if s.m.num = Unboxed then "U.of_int (Bytes.length buf)"
  else "Bytes.length buf"

(* The loop of [fold_step_index] that {!pp_fold_index_ahead} writes to
   [out]: its target, the bytes [need] its frames are known to hold, the fields [used]
   it reads ahead and their values at a frame start, the walk [body] of a
   frame that holds [need] bytes and [within], the walk of any other. *)
type index_loop = {
  out : Format.formatter;
  unboxed : bool;
  need : int;
  used : (string * (string -> string)) list;
  body : string;
  within : string;
}

(* On OCaml the variables the loop carries are [ref]s, which the compiler
   makes mutable variables, and a position is an [int]. *)
let index_read l v = if l.unboxed then v else "!" ^ v
let index_assign l = if l.unboxed then "<-" else ":="

let index_mutable l v e =
  if l.unboxed then Fmt.str "let mutable %s = %s in" v e
  else Fmt.str "let %s = ref %s in" v e

let index_zero l = if l.unboxed then "#0L" else "0L"

(* The frame at index [i] as the variables suffixed [x]: its start, its
   length, [known] when it lies in the buffer and holds [need] bytes, and
   then the fields read ahead, else zero. On OxCaml the bounds are tested as
   the sign of one [logor] of [int64_u]s, which hold the differences of two
   [int]s without wrapping. *)
let pp_index_next l x i =
  let ppf = l.out and zero = index_zero l in
  if l.unboxed then
    Fmt.pf ppf
      "let base%s = U.of_int (Array.unsafe_get offs %s) in@\n\
       let stop%s = U.of_int (Array.unsafe_get lens %s) in@\n\
       let known%s = U.compare (U.logor (U.logor base%s (U.sub (U.sub length \
       base%s) stop%s)) (U.sub stop%s #%dL)) #0L >= 0 in@\n"
      x i x i x x x x x l.need
  else
    Fmt.pf ppf
      "let base%s = Array.unsafe_get offs %s in@\n\
       let stop%s = Array.unsafe_get lens %s in@\n\
       let known%s = base%s >= 0 && stop%s >= %d && stop%s <= length - base%s \
       in@\n"
      x i x i x x x l.need x x;
  let one (v, value) =
    Fmt.pf ppf "let %s%s = if known%s then %s else %s in@\n" v x x
      (value ("base" ^ x))
      zero
  in
  match l.used with
  | _ :: _ :: _ when l.unboxed ->
      let list f = String.concat ", " (List.map f l.used) in
      Fmt.pf ppf "let #(%s) = if known%s then #(%s) else #(%s) in@\n"
        (list (fun (v, _) -> v ^ x))
        x
        (list (fun (_, value) -> value ("base" ^ x)))
        (list (fun _ -> zero))
  | used -> List.iter one used

(* The walk of the frame suffixed [x], whose variables [carried] reads from
   the loop's references. *)
let pp_index_walk ?(carried = false) l x =
  let ppf = l.out and read = index_read l in
  let v name = if carried then read (name ^ x) else name ^ x in
  let int name =
    if l.unboxed then Fmt.str "(U.to_int %s)" (v name) else v name
  in
  Fmt.pf ppf "acc %s (if %s then (@\nlet base = %s in@\n" (index_assign l)
    (v "known") (v "base");
  if mentions "stop" l.body then Fmt.pf ppf "let stop = %s in@\n" (v "stop");
  List.iter (fun (n, _) -> Fmt.pf ppf "let %s = %s in@\n" n (v n)) l.used;
  Fmt.pf ppf "let current = %s in@\n%s) else %s buf ~length %s %s ~init:%s)"
    (read "acc") l.body l.within (int "base") (int "stop") (read "acc")

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
      let names = frames_scope ~step:m () in
      let body =
        frames_body ?cuts ~step:m ~ahead s ~name:index ~names
          ~invalid:(m ^ ".invalid") ~valid:(m ^ ".valid") ~known:need
      in
      let used = List.filter (fun (v, _) -> mentions v body) ahead in
      let l =
        { out = s.ppf; unboxed = s.m.num = Unboxed; need; used; body; within }
      in
      Fmt.pf s.ppf
        "let[@inline always] %s %s ~(offs : int array) ~(lens : int array) \
         ~init =@\n\
         let n = Array.length offs in@\n\
         if Array.length lens <> n then@\n\
         invalid_arg \"%s: offs and lens differ in length\";@\n\
         let length = %s in@\n\
         %s@\n\
         if n > 0 then begin@\n"
        index s.m.src.buf index (length_of s)
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
       ~init:%s"
      within i i acc
  in
  Fmt.pf s.ppf
    "let[@inline always] %s %s ~(offs : int array) ~(lens : int array) ~init =@\n\
     let n = Array.length offs in@\n\
     if Array.length lens <> n then@\n\
     invalid_arg \"%s: offs and lens differ in length\";@\n\
     let length = %s in@\n"
    index buf index (length_of s);
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
        { s with ppf } ~reject:(m ^ ".invalid init")
        ~accept:(call ~step:true s (m ^ ".valid init"));
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
        Fmt.pf s.ppf "let[@inline always] %s %s ~%s off len ~init =@\n%s@\n"
          within param
          (if unboxed then "(length : int64_u)" else "length")
          body);
  if is_fstar s.m.num then
    Fmt.pf s.ppf
      "let %s (buf:P.buf) (off:P.int) (len:P.int) (init:%s.t) : %s.t =@\n\
       %s buf %s off len init@\n\
       @\n"
      s.fold m m within (fstar_length_of s "buf")
  else
    Fmt.pf s.ppf
      "let[@inline always] %s %s off len ~init =@\n\
       %s buf ~length:(%s) off len ~init@\n\
       @\n"
      s.fold buf within (length_of s)

(* A visitor's fold, which calls the module [m]'s functions by name, and its
   [_index], which folds it over the frames of an index. Both go through its
   [_within], whose [length] is that of [buf], so that the loop over the
   index loads it once. *)
let pp_fold_step s m =
  pp_step_within s m;
  pp_fold_step_index s ~within:(within s) ~m

(* [check b length ~off ~len], the walk of [fold] with no output, which
   answers [off] for a record it accepts and [-1] otherwise. A codec of
   constant [size] takes no window: [check b length off] walks the record
   over its [size] bytes, which the bounds fold into one test. *)
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
    let b = if mentions "buf" body then "b" else "_b" in
    Fmt.pf s.ppf
      "let[@inline always] check (%s : 'b Wire_runtime.Buf.t%s) (length : 'b \
       Wire_runtime.Buf.length) %s : 'b check =@\n"
      b
      (if unboxed then " @ local" else "")
      (if Option.is_some size then "off" else "~off ~len");
    if b = "b" then
      Fmt.pf s.ppf "let buf = (b : _ Wire_runtime.Buf.t :> bytes) in@\n";
    Fmt.pf s.ppf
      "let length = (length : _ Wire_runtime.Buf.length :> int) in@\n";
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

type fold = { name : string; selected : output list; visitor : string option }

(* The walks of a module: each of its [folds], then [check]. A fold with no
   visitor takes its consumer as arguments; one with a visitor calls that
   module by name, on OCaml holding values as [int64]. *)
let pp_body ~folds ?size s =
  let unboxed = s.m.num = Unboxed in
  if unboxed then
    Fmt.pf s.ppf
      "module U = Wire_runtime.Target.Int64_u@\n\
       module B = Wire_runtime.Target@\n\
       module L = Wire_runtime.Target.Local@\n\
       @\n";
  List.iter
    (fun f ->
      let mode num = scan_mode ~decide:s.m.decide s.plan ~num f.selected in
      let s =
        { s with fold = f.name; selected = f.selected; m = mode s.m.num }
      in
      match f.visitor with
      | None ->
          pp_fold s;
          pp_frames s
      | Some m ->
          let s = if unboxed then s else { s with m = mode Boxed } in
          pp_fold_step s m;
          pp_frames ~step:m s)
    folds;
  Fmt.pf s.ppf "type 'b view = int@\ntype 'b check = int@\n@\n";
  pp_check ?size s

let walks ?decide ~target ~folds ?size ~taken plan =
  let num = if target = `OxCaml then Unboxed else Tagged in
  let b = Buffer.create 4096 in
  let ppf = Format.formatter_of_buffer b in
  let s =
    {
      plan;
      fold = "fold";
      selected = [];
      m = scan_mode ?decide plan ~num [];
      ppf;
      taken;
    }
  in
  pp_body ~folds ?size s;
  Format.pp_print_flush ppf ();
  without_dead_bindings (Buffer.contents b)

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
      let names = frames_scope ~step:m () in
      let body =
        frames_body ?cuts ~step:m ~ahead s ~name ~names
          ~invalid:(m ^ ".invalid") ~valid:(m ^ ".valid") ~known:need
      in
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

let rendered ?(fold = "fold") ?fields c ~num write =
  let plan = scan_plan c in
  no_slots plan;
  let selected = selected plan fields in
  let walk ~trace ~num ~note =
    let b = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer b in
    let s =
      {
        plan;
        fold;
        selected;
        m = scan_mode ~note plan ~num selected;
        ppf;
        taken = Hashtbl.create 16;
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

let fstar ?fields c =
  rendered ?fields c ~num:Fstar (fun ~trace ~frame:_ s -> pp_fold ~trace s)

let fstar_step ?fields ~step c =
  rendered ~fold:"fold_step" ?fields c ~num:Fstar64 (fun ~trace ~frame:_ s ->
      pp_step_within ~trace s step)

let fstar_ox ?fields c =
  rendered ?fields c ~num:Fstar_u (fun ~trace ~frame:_ s -> pp_fold ~trace s)

let fstar_ox_step ?fields ~step c =
  rendered ~fold:"fold_step" ?fields c ~num:Fstar_u (fun ~trace ~frame:_ s ->
      pp_step_within ~trace s step)

let fstar_frame ?fields ~ox ~index ~step c =
  rendered ~fold:"fold_step" ?fields c
    ~num:(if ox then Fstar_u else Fstar64)
    (fun ~trace ~frame s -> pp_frame_walk ~trace ~frame ~index s step)
