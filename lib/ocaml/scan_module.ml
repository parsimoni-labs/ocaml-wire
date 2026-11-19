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
    src = bytes_src;
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

let to_int s ~unboxed (o : output) =
  let v = output_value s.m o in
  if s.m.num = Unboxed && (not unboxed) && not o.is_bool then
    Fmt.str "(U.to_int %s)" v
  else v

(* The call of [valid] with the outputs. *)
let call s ~unboxed valid () =
  Fmt.pf s.ppf "%s %a@\n" valid (pp_args ~conv:(to_int s ~unboxed)) s.selected

(* The walk over the [stop] bytes at [base], which starts by binding its
   slots: each rejection is the text [reject] and each acceptance what
   [accept] writes, in place, so a continuation the generator knows is never
   a closure. *)
let pp_scan s ~reject ~accept =
  pp_slots s.m s.ppf s.selected;
  let m =
    { s.m with fail = (fun _ -> reject); overflowed = (fun _ _ -> reject) }
  in
  start ~m ~ppf:s.ppf ~ranges:s.plan.ranges ~base:"base" ~limit:"stop" ~known:0
    s.plan.nodes (fun _ _ -> accept ())

let pp_entry s ~reject ~accept =
  Fmt.pf s.ppf
    "if off < 0 || len < 0 || off > Bytes.length buf || len > Bytes.length buf \
     - off then %s else@\n"
    reject;
  Fmt.pf s.ppf "let base = %s in@\nlet stop = %s in@\n"
    (if s.m.num = Tagged then "off" else "U.of_int off")
    (if s.m.num = Tagged then "len" else "U.of_int len");
  pp_scan s ~reject ~accept

(* The callbacks [fold_frames] takes: on OxCaml, unboxed ones have their
   types written out, the accumulator of layout [bits64]. *)
let frames_callbacks s ~unboxed =
  if not unboxed then "~invalid ~valid"
  else
    let fields =
      if s.selected = [] then [ "unit" ]
      else
        List.map
          (fun (o : output) ->
            o.name ^ ":" ^ if o.is_bool then "bool" else "int64_u")
          s.selected
    in
    "~(invalid : a -> a) ~(valid : "
    ^ String.concat " -> " (("a" :: fields) @ [ "a" ])
    ^ ") : a"

(* [fold_frames], or with [step] [fold_frames_step], which calls the module
   [step]'s functions by name. The frame guard and a positive stride make
   [base + stride <= off + len] on every iteration, the last included. Each
   frame still gets all its checks: a short frame cannot borrow bytes from the
   next. *)
let pp_frames ?step s ~unboxed =
  let ppf = s.ppf in
  let native = s.m.num = Unboxed in
  let binding, reference, read, assign =
    if native then ("mutable ", "", "", "<-") else ("", "ref ", "!", ":=")
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
  Fmt.pf ppf "let[@inline always] %s%s buf off len ~stride %s %s =@\n" name
    (if unboxed then " (type a : bits64)" else "")
    (if unboxed then "~(init : a)" else "~init")
    callbacks;
  Fmt.pf ppf
    "if stride <= 0 || off < 0 || len < 0 || off > Bytes.length buf || len > \
     Bytes.length buf - off then@\n\
     invalid_arg \"%s: invalid frame bounds or stride\";@\n"
    name;
  Fmt.pf ppf "let %sacc = %sinit in@\nlet %scursor = %soff in@\n" binding
    reference binding
    (if native then "U.of_int " else "ref ");
  if native then Fmt.pf ppf "let stride_word = U.of_int stride in@\n";
  Fmt.pf ppf
    "for _ = 1 to len / stride do@\n\
     let base = %scursor in@\n\
     let current = %sacc in@\n\
     let next = (@\n\
     let stop = %s in@\n"
    read read
    (if native then "stride_word" else "stride");
  pp_scan s ~reject:(invalid ^ " current")
    ~accept:(call s ~unboxed (valid ^ " current"));
  Fmt.pf ppf ") in@\nacc %s next;@\ncursor %s %s@\ndone;@\n%sacc@\n@\n" assign
    assign
    (binop s.m.num "+" "base" (if native then "stride_word" else "stride"))
    read

(* [fold_step], which calls the module [m]'s functions by name. *)
let pp_fold_step s m =
  Fmt.pf s.ppf "let[@inline always] fold_step buf off len ~init =@\n";
  pp_entry s ~reject:(m ^ ".invalid init")
    ~accept:(call s ~unboxed:false (m ^ ".valid init"));
  Fmt.pf s.ppf "@\n"

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

let pp_decoder s name typ hash attr =
  let ppf = s.ppf in
  Fmt.pf ppf "let[@inline always]%s %s buf off len : %s =@\n" attr name typ;
  let direct = hash = "#" in
  let zero (o : output) =
    if o.is_bool then "false" else if direct then lit s.m.num 0 else "0"
  in
  let fields ppf conv =
    List.iter
      (fun (o : output) -> Fmt.pf ppf "%s = %s;" o.name (conv o))
      s.selected
  in
  let id (o : output) = o.id in
  if direct then begin
    Fmt.pf ppf "let invalid () = #{ok = false;%a} in@\n" fields zero;
    Fmt.pf ppf "let valid %a = #{ok = true;%a} in@\n" (pp_args ~conv:id)
      s.selected fields id;
    pp_entry s ~reject:"invalid ()" ~accept:(call s ~unboxed:true "valid")
  end
  else
    Fmt.pf ppf
      "fold buf off len ~invalid:(fun () -> {ok = false;%a})@\n\
       ~valid:(fun %a -> {ok = true;%a})@\n"
      fields zero (pp_args ~conv:id) s.selected fields id

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

(* {2 Dead bindings}

   The walk binds an offset, an end or a field as it reaches it, before it
   knows whether a later check or the call of [valid] reads it: a lowered
   condition may read a bitfield's word instead of the field. Each binding is
   one line [let x = e in] whose [e] only loads or computes, so a line whose
   [x] no later line of its function reads is dropped, until none is. *)

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false

(* The variable a line [let x = e in] binds. *)
let is_variable x =
  x <> ""
  && (match x.[0] with 'a' .. 'z' | '_' -> true | _ -> false)
  && String.for_all is_ident_char x

let bound line =
  let is_let =
    String.starts_with ~prefix:"let " line
    && String.ends_with ~suffix:" in" line
  in
  match if is_let then String.index_from_opt line 4 ' ' else None with
  | Some i when String.length line >= i + 3 ->
      let x = String.sub line 4 (i - 4) in
      if is_variable x && String.sub line i 3 = " = " then Some x else None
  | _ -> None

(* Whether [line] reads [x], other than by binding it. *)
let reads x line =
  let n = String.length line and k = String.length x in
  let rec from i =
    i + k <= n
    &&
    let word =
      String.sub line i k = x
      && (i = 0 || not (is_ident_char line.[i - 1]))
      && (i + k = n || not (is_ident_char line.[i + k]))
    in
    (word && not (i = 4 && bound line = Some x)) || from (i + 1)
  in
  from 0

let is_function line = String.starts_with ~prefix:"let[@inline always]" line

let rec prune lines =
  let lines = Array.of_list lines in
  let n = Array.length lines in
  let rec used x j =
    j < n
    && (not (is_function lines.(j)))
    && (reads x lines.(j) || used x (j + 1))
  in
  let dead i =
    match bound lines.(i) with Some x -> not (used x (i + 1)) | None -> false
  in
  let kept = List.filteri (fun i _ -> not (dead i)) (Array.to_list lines) in
  if List.length kept = n then kept else prune kept

let without_dead_bindings text =
  String.concat "\n" (prune (String.split_on_char '\n' text))

let scanner ?(target = `OCaml) ?fields ?step ~codec c =
  try
    let plan = scan_plan c in
    let num = if target = `OxCaml then Unboxed else Tagged in
    let b = Buffer.create 4096 in
    let ppf = Format.formatter_of_buffer b in
    let selected = selected plan fields in
    let s = { plan; selected; m = scan_mode plan ~num selected; ppf } in
    pp_header ppf ~codec ~name:plan.record ~digest:(Gen.digest c);
    pp_wide ppf ~codec ~wide:plan.wide;
    if num = Unboxed then
      Fmt.pf ppf
        "module U = Wire_runtime.Target.Int64_u@\n\
         module B = Wire_runtime.Target@\n\
         @\n";
    Fmt.pf ppf "let[@inline always] fold buf off len ~invalid ~valid =@\n";
    pp_entry s ~reject:"invalid ()" ~accept:(call s ~unboxed:false "valid");
    Fmt.pf ppf "@\n";
    pp_frames s ~unboxed:false;
    Option.iter
      (fun m ->
        pp_fold_step s m;
        pp_frames ~step:m s ~unboxed:false)
      step;
    if num = Unboxed then begin
      Fmt.pf ppf
        "let[@inline always] fold_unboxed buf off len ~invalid ~valid =@\n";
      pp_entry s ~reject:"invalid ()" ~accept:(call s ~unboxed:true "valid");
      Fmt.pf ppf "@\n";
      pp_frames s ~unboxed:true
    end;
    pp_result_type s "t" "";
    pp_decoder s "decode" "t" "" "";
    if target = `OxCaml then begin
      pp_result_type s "unboxed" "#";
      pp_decoder s "decode_unboxed" "unboxed" "#" "[@zero_alloc]";
      pp_box s
    end;
    Format.pp_print_flush ppf ();
    without_dead_bindings (Buffer.contents b)
  with Unsupported why -> refuse ~codec why
