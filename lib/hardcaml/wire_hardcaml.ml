open Hardcaml
open Base.Int.O
module S = Signal
module T = Wire.Private.Types
module L = Wire.Private.Layout
module P = Wire_ocaml.Plan

(* The checked plan proves native arithmetic safe; shifts and bitwise values
   keep the exact 63-bit representation of OCaml native integers. *)
let integer_width = 63
let int n = S.of_int ~width:integer_width n
let native_zero = int 0
let bool b = if b then S.vdd else S.gnd
let get env id = List.assoc id env
let unsupported () = invalid_arg "Wire_hardcaml: unsupported checked expression"

let anchor name signal =
  let input =
    S.Type.is_wire signal && Option.is_none (S.Type.wire_driver signal)
  in
  if not (S.Type.is_const signal || input) then (
    let target =
      match S.Type.wire_driver signal with
      | Some driver when S.Type.is_reg driver -> driver
      | _ -> signal
    in
    S.Type.add_name target name;
    if not (S.Type.Uid.equal (S.Type.uid target) (S.Type.uid signal)) then
      S.Type.add_name signal (name ^ "_alias"));
  signal

module Private = struct
  let contiguous_keep keep =
    let width = S.width keep in
    if width = 1 then S.vdd
    else
      let higher = S.select keep (width - 1) 1 in
      let lower = S.select keep (width - 2) 0 in
      S.(higher &: ~:lower ==:. 0)
end

let compare op a b =
  let signed = S.width a <> 1 in
  match op with
  | `Eq -> S.(a ==: b)
  | `Ne -> S.(a <>: b)
  | `Lt -> if signed then S.(a <+ b) else S.(a <: b)
  | `Le -> if signed then S.(a <=+ b) else S.(a <=: b)
  | `Gt -> if signed then S.(a >+ b) else S.(a >: b)
  | `Ge -> if signed then S.(a >=+ b) else S.(a >=: b)

let rec expr : type a. (string * S.t) list -> a T.expr -> S.t =
 fun env e ->
  let emit : type a. a T.expr -> S.t = fun e -> expr env e in
  let comparison op a b = compare op (emit a) (emit b) in
  match e with
  | T.Int n -> int n
  | T.Bool b -> bool b
  | T.Ref (T.I, id) -> get env id
  | T.Add (a, b) -> S.(emit a +: emit b)
  | T.Sub (a, b) -> S.(emit a -: emit b)
  | T.Mul (a, b) -> S.uresize S.(emit a *+ emit b) integer_width
  | T.Land (a, b) -> S.(emit a &: emit b)
  | T.Lor (a, b) -> S.(emit a |: emit b)
  | T.Lxor (a, b) -> S.(emit a ^: emit b)
  | T.Lnot a -> S.(~:(emit a))
  | T.Lsl (a, b) -> S.log_shift S.sll (emit a) (S.select (emit b) 5 0)
  | T.Lsr (a, b) -> S.log_shift S.srl (emit a) (S.select (emit b) 5 0)
  | T.Eq (a, b) -> comparison `Eq a b
  | T.Ne (a, b) -> comparison `Ne a b
  | T.Lt (a, b) -> comparison `Lt a b
  | T.Le (a, b) -> comparison `Le a b
  | T.Gt (a, b) -> comparison `Gt a b
  | T.Ge (a, b) -> comparison `Ge a b
  | T.And (a, b) -> S.(emit a &: emit b)
  | T.Or (a, b) -> S.(emit a |: emit b)
  | T.Not a -> S.(~:(emit a))
  | T.Cast (`U8, a) -> S.uresize (S.select (emit a) 7 0) integer_width
  | T.Cast (`U16, a) -> S.uresize (S.select (emit a) 15 0) integer_width
  | T.If_then_else (c, a, b) -> S.mux2 (emit c) (emit a) (emit b)
  | _ -> unsupported ()

type position = { id : int; node : position_node }

and position_node =
  | At of S.t * int
  | After of position * S.t * int T.expr
  | Choice of bool T.expr * S.t * position * position

type state = { ok : S.t; pos : position; env : (string * S.t) list }

type t = {
  in_scope : S.t;
  accepted : S.t;
  consumed : S.t;
  raw_consumed : S.t;
  fields : (string * S.t) list;
  raw_fields : (string * S.t) list;
  read_prefix : int;
}

let in_scope t = t.in_scope
let accepted t = t.accepted
let consumed t = t.consumed
let fields t = t.fields
let read_prefix t = t.read_prefix

let outputs t =
  S.output "accepted" t.accepted
  :: S.output "in_scope" t.in_scope
  :: S.output "consumed" t.consumed
  :: List.map (fun (name, value) -> S.output ("field_" ^ name) value) t.fields

let to_circuit ~name t = Circuit.create_exn ~name (outputs t)

let masked_fields accepted fields =
  List.map
    (fun (name, value) ->
      (name, S.mux2 accepted value (S.zero (S.width value))))
    fields

let map_raw f t =
  let accepted = f t.accepted in
  let raw_consumed = f t.raw_consumed in
  let raw_fields =
    List.map (fun (name, value) -> (name, f value)) t.raw_fields
  in
  {
    in_scope = f t.in_scope;
    accepted;
    consumed = S.mux2 accepted raw_consumed native_zero;
    raw_consumed;
    fields = masked_fields accepted raw_fields;
    raw_fields;
    read_prefix = t.read_prefix;
  }

let register ?enable spec t = map_raw (fun value -> S.reg spec ?enable value) t

let reduce t ~f =
  let result = f t.raw_fields in
  S.mux2 t.accepted result (S.zero (S.width result))

let ceil_log2 n =
  let rec loop width power =
    if power >= n then width else loop (width + 1) (power * 2)
  in
  loop 0 1

let valid_bounds capacity max_length =
  capacity > 0 && capacity <= 256 && max_length >= capacity
  && max_length <= 1_048_576

type emitter = {
  pipeline : (Reg_spec.t * S.t) option;
  registers : (S.Type.Uid.t, S.t) Hashtbl.t;
  capacity : int;
  max_length : int;
  cursor_width : int;
  data : S.t;
  plan : P.t;
  next_position : int ref;
  positions : (int * P.Facts.t, S.t) Hashtbl.t;
  upper_cache : (int * P.Facts.t, int) Hashtbl.t;
  read_prefix : int ref;
}

let delay c value =
  match c.pipeline with
  | None -> value
  | Some (spec, enable) -> (
      if S.Type.is_const value then value
      else
        let id = S.uid value in
        match Hashtbl.find_opt c.registers id with
        | Some registered -> registered
        | None ->
            let registered =
              anchor
                ("wire_decode_extract_q_"
                ^ string_of_int (Hashtbl.length c.registers))
                (S.reg spec ~enable value)
            in
            Hashtbl.add c.registers id registered;
            registered)

let choose c condition yes no =
  let raw = S.mux2 condition yes no in
  (match c.pipeline with
  | Some _
    when (not (S.Type.is_const raw))
         && not (Hashtbl.mem c.registers (S.uid raw)) ->
      let selected = S.mux2 (delay c condition) (delay c yes) (delay c no) in
      Hashtbl.replace c.registers (S.uid raw) selected
  | _ -> ());
  raw

let checked_expr c env e =
  expr (List.map (fun (name, value) -> (name, delay c value)) env) e

let cursor c value = S.uresize value c.cursor_width

let position c node =
  let id = !(c.next_position) in
  incr c.next_position;
  { id; node }

let rec value c facts pos =
  match Hashtbl.find_opt c.positions (pos.id, facts) with
  | Some signal -> signal
  | None ->
      let signal =
        match pos.node with
        | At (signal, _) -> signal
        | After (base, size, _) ->
            S.(value c facts base +: uresize size c.cursor_width)
        | Choice (condition, signal, yes, no) -> (
            match P.Facts.simplify facts condition with
            | T.Bool true -> value c facts yes
            | T.Bool false -> value c facts no
            | _ -> S.mux2 signal (value c facts yes) (value c facts no))
      in
      Hashtbl.add c.positions (pos.id, facts) signal;
      signal

let bounded_add c a b =
  let b = Base.Int.max 0 b in
  if a > c.max_length || b > c.max_length - a then c.max_length + 1 else a + b

let rec upper c facts pos =
  match Hashtbl.find_opt c.upper_cache (pos.id, facts) with
  | Some bound -> bound
  | None ->
      let bound =
        match pos.node with
        | At (_, bound) -> bound
        | After (base, _, size) ->
            bounded_add c (upper c facts base) (snd (P.range c.plan size))
        | Choice (condition, _, yes, no) -> (
            match P.Facts.simplify facts condition with
            | T.Bool true -> upper c facts yes
            | T.Bool false -> upper c facts no
            | _ -> Base.Int.max (upper c facts yes) (upper c facts no))
      in
      Hashtbl.add c.upper_cache (pos.id, facts) bound;
      bound

(* Published cursors have passed their bounds checks. Truncation affects
   only failing execution paths, whose sticky rejection cannot become
   acceptance. DSL size arithmetic stays native-width until the check. *)
let unsigned_le a b =
  if S.Type.is_const a && S.to_int a = 0 then S.vdd else S.(a <=: b)

let fits c pos size limit =
  let pos = delay c pos and limit = delay c limit in
  let small = cursor c size in
  let ordered = unsigned_le pos limit in
  let room = unsigned_le small S.(limit -: pos) in
  S.(ordered &: (size ==: uresize small integer_width) &: room)

let within c facts pos size limit =
  fits c (value c facts pos) size (value c facts limit)

let advance c pos source size = position c (After (pos, size, source))

let prefix c pos =
  (* Out-of-scope indices may select arbitrary physical bus bits. [fits]
     makes those paths reject, and every rejected output is masked to zero. *)
  let shift = S.sll (S.uresize pos (ceil_log2 (c.capacity * 8) + 1)) 3 in
  S.log_shift S.srl c.data shift

let word c bits offset bytes endian =
  if offset < 0 || offset > c.capacity || bytes > c.capacity - offset then
    S.zero (bytes * 8)
  else
    let parts =
      List.init bytes (fun i ->
          S.select bits ((8 * (offset + i)) + 7) (8 * (offset + i)))
    in
    S.concat_msb
      (match endian with T.Big -> parts | T.Little -> List.rev parts)

let field c bits (f : P.field) =
  match f.store with
  | L.Int { width; _ } when width > 32 ->
      invalid_arg "Wire_hardcaml: scalar fields wider than 32 bits"
  | L.Int { width; endian; signed } ->
      let raw = word c bits f.offset (width / 8) endian in
      if signed then S.sresize raw integer_width
      else S.uresize raw integer_width
  | L.Bits { base; shift; width } ->
      let bytes, endian =
        match base with
        | T.U8 -> (1, T.Big)
        | T.U16 e -> (2, e)
        | T.U32 e -> (4, e)
      in
      let raw = word c bits f.offset bytes endian in
      S.uresize (S.select raw (shift + width - 1) shift) integer_width
  | L.Bytes _ -> native_zero

let check c state condition =
  { state with ok = S.(state.ok &: checked_expr c state.env condition) }

let scalar_read_end fields =
  List.fold_left
    (fun last (f : P.field) ->
      let bytes =
        match f.store with
        | L.Bytes _ -> 0
        | L.Int { width; _ } -> width / 8
        | L.Bits { base; _ } -> (
            match base with T.U8 -> 1 | T.U16 _ -> 2 | T.U32 _ -> 4)
      in
      if bytes = 0 then last else Base.Int.max last (f.offset + bytes))
    0 fields

let emit_prefix c facts state limit fields size =
  let read_end = scalar_read_end fields in
  if c.max_length > c.capacity && read_end > 0 then (
    let required = bounded_add c (upper c facts state.pos) read_end in
    c.read_prefix := Base.Int.max !(c.read_prefix) required;
    if required > c.capacity then
      invalid_arg
        "Wire_hardcaml: cannot prove captured prefix covers scalar reads");
  let physically_present =
    if c.max_length = c.capacity || read_end = 0 then S.vdd
    else
      within c facts state.pos (int read_end)
        (position c (At (cursor c (int c.capacity), c.capacity)))
  in
  let bits = prefix c (value c facts state.pos) in
  let state =
    {
      state with
      ok =
        S.(
          state.ok
          &: within c facts state.pos (int size) limit
          &: physically_present);
    }
  in
  let state =
    List.fold_left
      (fun state (f : P.field) ->
        let state =
          match f.store with
          | L.Bytes _ -> state
          | _ -> { state with env = (f.id, field c bits f) :: state.env }
        in
        List.fold_left (check c) state (P.conditions f))
      state fields
  in
  { state with pos = advance c state.pos (T.Int size) (int size) }

let emit_span c facts state limit size =
  let raw_size = expr state.env size in
  let checked_size = checked_expr c state.env size in
  {
    state with
    ok = S.(state.ok &: within c facts state.pos checked_size limit);
    pos = advance c state.pos size raw_size;
  }

let rec nodes c facts state limit = function
  | [] -> state
  | node :: rest -> nodes c facts (step c facts state limit node) limit rest

and step c facts state limit = function
  | P.Test condition -> check c state condition
  | P.Prefix (fields, size) -> emit_prefix c facts state limit fields size
  | P.Span size -> emit_span c facts state limit size
  | P.Region (size, at_most, body) ->
      emit_region c facts state limit size at_most body
  | P.Gate (predicate, present, children, body) ->
      emit_gate c facts state limit predicate present children body

and emit_region c facts state limit size at_most body =
  let raw_size = expr state.env size in
  let checked_size = checked_expr c state.env size in
  let stop = advance c state.pos size raw_size in
  let child =
    nodes c facts
      {
        state with
        ok = S.(state.ok &: within c facts state.pos checked_size limit);
      }
      stop body
  in
  let child_pos = delay c (value c facts child.pos)
  and checked_stop = delay c (value c facts stop) in
  let ok =
    if at_most then child.ok else S.(child.ok &: (child_pos ==: checked_stop))
  in
  { child with ok; pos = stop }

and emit_gate c facts state limit predicate present children body =
  let condition = expr state.env predicate in
  let branch_facts = P.Facts.assume facts predicate true in
  let child = nodes c branch_facts state limit body in
  let env =
    List.fold_left
      (fun env (o : P.output) ->
        let absent = if o.is_bool then S.gnd else native_zero in
        (o.id, choose c condition (get child.env o.id) absent) :: env)
      state.env children
  in
  {
    ok = S.mux2 (delay c condition) child.ok state.ok;
    pos = position c (Choice (predicate, condition, child.pos, state.pos));
    env = (present.id, condition) :: env;
  }

let check_interface ~capacity ~max_length ~data ~length =
  if
    Sys.int_size <> integer_width
    || (not (valid_bounds capacity max_length))
    || S.width data <> capacity * 8
    || S.width length < ceil_log2 (max_length + 1)
    || S.width length > integer_width
  then invalid_arg "Wire_hardcaml: invalid bounded input interface"

let v ?pipeline ?max_length ~capacity ~data ~length plan =
  let max_length = Option.value ~default:capacity max_length in
  check_interface ~capacity ~max_length ~data ~length;
  let c =
    {
      pipeline;
      registers = Hashtbl.create 32;
      capacity;
      max_length;
      cursor_width = ceil_log2 (max_length + 1);
      data;
      plan;
      next_position = ref 0;
      positions = Hashtbl.create 32;
      upper_cache = Hashtbl.create 32;
      read_prefix = ref 0;
    }
  in
  let length = S.uresize length integer_width in
  let in_scope =
    delay c (anchor "wire_decode_scope_next" S.(length <=: int max_length))
  in
  let result =
    nodes c P.Facts.empty
      {
        ok = in_scope;
        pos = position c (At (cursor c native_zero, 0));
        env = [];
      }
      (position c (At (cursor c length, max_length)))
      (P.nodes plan)
  in
  let raw_fields =
    List.map
      (fun (o : P.output) -> (o.name, delay c (get result.env o.id)))
      (P.outputs plan)
  in
  let fields = masked_fields result.ok raw_fields in
  let accepted = anchor "wire_decode_accept" result.ok in
  let raw_consumed =
    anchor "wire_decode_consumed"
      (S.uresize (delay c (value c P.Facts.empty result.pos)) integer_width)
  in
  {
    in_scope;
    accepted;
    consumed = S.mux2 result.ok raw_consumed native_zero;
    raw_consumed;
    fields;
    raw_fields;
    read_prefix = (if max_length = capacity then capacity else !(c.read_prefix));
  }

module Stream = struct
  type packing = Full_beats | Contiguous
  type decoder = t

  type proof_state = {
    reserved : S.t;
    queued : S.t;
    read_index : S.t;
    write_index : S.t;
    first_valid : S.t;
    second_valid : S.t;
    first_id : S.t;
    second_id : S.t;
    first_length : S.t;
    second_length : S.t;
    first_errors : S.t;
    second_errors : S.t;
    slots : (S.t * S.t * S.t) list;
    second_bounds : S.t;
    slot_bounds : S.t list;
  }

  type t = {
    input_ready : S.t;
    output_valid : S.t;
    frame_length : S.t;
    frame_id : S.t;
    completed_frames : S.t;
    current_length : S.t;
    current_error : S.t;
    transport_error : S.t;
    decoder : decoder;
    proof_state : proof_state;
  }

  let input_ready t = t.input_ready
  let output_valid t = t.output_valid
  let frame_length t = t.frame_length
  let frame_id t = t.frame_id
  let completed_frames t = t.completed_frames
  let current_length t = t.current_length
  let current_error t = t.current_error
  let transport_error t = t.transport_error
  let decoder t = t.decoder

  let rec sum word_width = function
    | [] -> S.zero word_width
    | [ x ] -> x
    | xs ->
        let half = List.length xs / 2 in
        let left = List.filteri (fun i _ -> i < half) xs in
        let right = List.filteri (fun i _ -> i >= half) xs in
        S.(sum word_width left +: sum word_width right)

  let v ?(packing = Full_beats) ~capacity ~max_length ~clock ~reset ~valid ~data
      ~keep ~last ~error ~output_ready plan =
    let bytes = S.width keep in
    if
      bytes < 1 || bytes > 64
      || S.width data <> bytes * 8
      || (not (valid_bounds capacity max_length))
      || List.exists
           (fun x -> S.width x <> 1)
           [ clock; reset; valid; last; error; output_ready ]
    then invalid_arg "Wire_hardcaml.Stream: invalid AXI stream interface";
    let count_width = ceil_log2 (max_length + 2) in
    let count n = S.of_int ~width:count_width n in
    let raw_width = ceil_log2 (max_length + bytes + 1) in
    let raw_count n = S.of_int ~width:raw_width n in
    let state_spec = Reg_spec.create ~clear:reset () ~clock in
    let data_spec = Reg_spec.create () ~clock in
    let reserved = S.wire 3 and queued = S.wire 3 in
    let output_valid = S.(queued <>:. 0 &: ~:reset) in
    (* Reserve a result slot at EOF. Registered credits isolate ingress and
       both decoder stages from the output READY signal. Four slots cover
       the two in-flight snapshots and the final result queue. *)
    let input_ready = S.(reserved <:. 4 &: ~:reset) in
    let transfer = anchor "wire_ingress_transfer" S.(valid &: input_ready) in
    let finished = anchor "wire_ingress_eof" S.(transfer &: last) in
    let deliver = S.(output_valid &: output_ready) in
    let balance added removed count =
      S.mux2
        S.(added ^: removed)
        (S.mux2 added S.(count +:. 1) S.(count -:. 1))
        count
    in
    S.assign reserved
      (anchor "wire_ingress_reserved_q"
         (S.reg state_spec (balance finished deliver reserved)));
    let collected = S.wire (capacity * 8) in
    let length = S.wire raw_width in
    let errors = S.wire 2 in
    let beat_count_width = ceil_log2 (bytes + 1) in
    let extended_width = 1 + Base.Int.max raw_width beat_count_width in
    let count_bytes =
      anchor "wire_ingress_beat_bytes"
        (sum beat_count_width
           (List.init bytes (fun i -> S.uresize (S.bit keep i) beat_count_width)))
    in
    let extended =
      anchor "wire_ingress_length_sum"
        S.(uresize length extended_width +: uresize count_bytes extended_width)
    in
    let overflow_seen =
      anchor "wire_ingress_overflow" S.(length >: raw_count max_length)
    in
    (* TKEEP is a contiguous low-byte mask; partial intermediate beats are
       packed without gaps. An empty beat is allowed only at TLAST. *)
    let contiguous =
      anchor "wire_ingress_keep_contiguous" (Private.contiguous_keep keep)
    in
    let bad_keep =
      match packing with
      | Full_beats -> S.(~:contiguous |: (keep <>: ones bytes &: ~:last))
      | Contiguous -> S.(~:contiguous |: (keep ==:. 0 &: ~:last))
    in
    let bad_keep = anchor "wire_ingress_keep_bad" bad_keep in
    let beat_errors = S.concat_lsb [ error; bad_keep ] in
    let next_errors = S.(errors |: beat_errors) in
    let byte_mask =
      S.concat_lsb (List.init bytes (fun i -> S.repeat (S.bit keep i) 8))
    in
    let incoming = S.uresize S.(data &: byte_mask) (capacity * 8) in
    let next_data =
      match packing with
      | Contiguous ->
          let shift =
            S.sll (S.uresize length (ceil_log2 (capacity * 8) + 1)) 3
          in
          let appended = S.(collected |: log_shift sll incoming shift) in
          S.mux2 S.(length <: raw_count capacity) appended collected
      | Full_beats ->
          let chunks = (capacity + bytes - 1) / bytes in
          let index_width = ceil_log2 (chunks + 1) in
          let index = S.wire index_width in
          let number n = S.of_int ~width:index_width n in
          let next = S.mux2 S.(index <: number chunks) S.(index +:. 1) index in
          S.assign index
            (anchor "wire_capture_chunk_index_q"
               (S.reg state_spec ~enable:transfer (S.mux2 last (number 0) next)));
          S.concat_lsb
            (List.init chunks (fun chunk ->
                 let low = chunk * bytes * 8 in
                 let width = Base.Int.min (bytes * 8) ((capacity * 8) - low) in
                 let previous = S.select collected (low + width - 1) low in
                 S.mux2
                   S.(index ==: number chunk)
                   (S.uresize incoming width) previous))
    in
    let next_data =
      anchor "wire_capture_prefix_next"
        (S.mux2 overflow_seen collected next_data)
    in
    S.assign collected
      (anchor "wire_capture_prefix_q"
         (S.reg state_spec ~enable:transfer
            (S.mux2 last (S.zero (capacity * 8)) next_data)));
    (* The raw counter can represent the first overflowing beat, then freezes.
       Its comparison runs in parallel with the next sum. The wider EOF sum
       also retains a final beat after an earlier overflow without wrapping. *)
    S.assign length
      (anchor "wire_ingress_length_q"
         (S.reg state_spec
            ~enable:S.(transfer &: (~:overflow_seen |: last))
            (S.mux2 last (raw_count 0) (S.uresize extended raw_width))));
    S.assign errors
      (anchor "wire_ingress_flags_q"
         (S.reg state_spec ~enable:transfer
            (S.mux2 last (S.zero 2) next_errors)));
    let snapshot name x = anchor name (S.reg data_spec ~enable:finished x) in
    let frame_data = snapshot "wire_frame_prefix_q" next_data in
    let frame_extended = snapshot "wire_frame_length_q" extended in
    let frame_error = snapshot "wire_frame_flags_q" next_errors in
    let sequence =
      anchor "wire_ingress_sequence_q"
        (S.reg_fb state_spec ~enable:finished ~width:64 ~f:(fun n ->
             S.(n +:. 1)))
    in
    let first_id = snapshot "wire_frame_id_q" sequence in
    let first_valid = anchor "wire_frame_token_q" (S.reg state_spec finished) in
    let second_valid =
      anchor "wire_decode_extract_q_token" (S.reg state_spec first_valid)
    in
    let push = S.(second_valid &: ~:reset) in
    S.assign queued
      (anchor "wire_result_bank_control_count_q"
         (S.reg state_spec (balance push deliver queued)));
    let write_index =
      anchor "wire_result_bank_control_write_index_q"
        (S.reg_fb state_spec ~enable:push ~width:2 ~f:(fun n -> S.(n +:. 1)))
    in
    let read_index =
      anchor "wire_result_bank_control_read_index_q"
        (S.reg_fb state_spec ~enable:deliver ~width:2 ~f:(fun n -> S.(n +:. 1)))
    in
    let write_enables =
      List.init 4 (fun i -> S.(push &: (write_index ==:. i)))
    in
    let labels = Hashtbl.create 32 in
    let label value name = Hashtbl.replace labels (S.uid value) name in
    let buffers = Hashtbl.create 32 in
    let buffered value =
      match Hashtbl.find_opt buffers (S.uid value) with
      | Some buffered -> buffered
      | None ->
          let name =
            match Hashtbl.find_opt labels (S.uid value) with
            | Some name -> name
            | None -> "value_" ^ string_of_int (Hashtbl.length buffers)
          in
          let slots =
            List.mapi
              (fun bank enable ->
                anchor
                  ("wire_result_bank_" ^ string_of_int bank ^ "_q_" ^ name)
                  (S.reg data_spec ~enable value))
              write_enables
          in
          let head =
            match slots with
            | [ a; b; c; d ] ->
                let low = S.bit read_index 0 in
                S.mux2 (S.bit read_index 1) (S.mux2 low d c) (S.mux2 low b a)
            | _ -> assert false
          in
          let head = anchor ("wire_result_head_" ^ name) head in
          let buffered = (head, slots) in
          Hashtbl.add buffers (S.uid value) buffered;
          buffered
    in
    let buffer value = fst (buffered value) in
    let slots value = snd (buffered value) in
    let decoded =
      v ~pipeline:(data_spec, first_valid) ~max_length ~capacity
        ~data:frame_data ~length:frame_extended plan
    in
    List.iteri
      (fun i (_, value) -> label value ("field_" ^ string_of_int i))
      decoded.raw_fields;
    label decoded.raw_consumed "consumed";
    label decoded.in_scope "scope";
    label decoded.accepted "accept";
    let decoder = map_raw buffer decoded in
    let load name x = anchor name (S.reg data_spec ~enable:first_valid x) in
    let second_length =
      load "wire_decode_extract_q_length" (S.uresize frame_extended count_width)
    in
    let second_errors = load "wire_decode_extract_q_flags" frame_error in
    let second_id = load "wire_decode_extract_q_id" first_id in
    label second_length "length";
    label second_errors "flags";
    label second_id "id";
    let report scope length = S.mux2 scope length (count (max_length + 1)) in
    let flags scope errors = S.concat_lsb [ errors; S.(~:scope) ] in
    let frame_length = report decoder.in_scope (buffer second_length) in
    let transport_error = flags decoder.in_scope (buffer second_errors) in
    let frame_id = buffer second_id in
    (* The proof view reuses actual registers; its combinational projections
       are unreachable from [to_circuit] and add no production ports. *)
    let first_scope = S.(frame_extended <=:. max_length) in
    let scopes = slots decoded.in_scope in
    let slot_lengths = slots second_length
    and slot_errors = slots second_errors in
    let bounded scope accepted consumed length =
      S.(~:accepted |: (scope &: (consumed <=: uresize length integer_width)))
    in
    let slot_accepted = slots decoded.accepted in
    let slot_consumed = slots decoded.raw_consumed in
    let proof_state =
      {
        reserved;
        queued;
        read_index;
        write_index;
        first_valid;
        second_valid;
        first_id;
        second_id;
        first_length = report first_scope (S.uresize frame_extended count_width);
        first_errors = flags first_scope frame_error;
        second_length = report decoded.in_scope second_length;
        second_errors = flags decoded.in_scope second_errors;
        slots =
          List.mapi
            (fun i id ->
              let scope = List.nth scopes i in
              ( id,
                report scope (List.nth slot_lengths i),
                flags scope (List.nth slot_errors i) ))
            (slots second_id);
        second_bounds =
          bounded decoded.in_scope decoded.accepted decoded.raw_consumed
            second_length;
        slot_bounds =
          List.mapi
            (fun i scope ->
              bounded scope (List.nth slot_accepted i)
                (List.nth slot_consumed i) (List.nth slot_lengths i))
            scopes;
      }
    in
    let accepted = S.(decoder.accepted &: (transport_error ==:. 0)) in
    let decoder =
      {
        decoder with
        accepted;
        consumed = S.mux2 accepted decoder.raw_consumed native_zero;
        fields = masked_fields accepted decoder.raw_fields;
      }
    in
    {
      input_ready;
      output_valid;
      frame_length;
      frame_id;
      completed_frames = sequence;
      current_length =
        S.mux2 overflow_seen
          (count (max_length + 1))
          (S.uresize length count_width);
      current_error = S.concat_lsb [ errors; overflow_seen ];
      transport_error;
      decoder;
      proof_state;
    }

  let to_circuit ~name t =
    Circuit.create_exn ~name
      (S.output "input_ready" t.input_ready
      :: S.output "output_valid" t.output_valid
      :: S.output "frame_id" t.frame_id
      :: S.output "frame_length" t.frame_length
      :: S.output "transport_error" t.transport_error
      :: outputs t.decoder)

  module Private = struct
    type state = proof_state

    let state t = t.proof_state
    let counts t = (t.reserved, t.queued)
    let indices t = (t.read_index, t.write_index)
    let first t = (t.first_valid, t.first_id, t.first_length, t.first_errors)

    let second t =
      (t.second_valid, t.second_id, t.second_length, t.second_errors)

    let slots t = t.slots
    let bounds t = (t.second_bounds, t.slot_bounds)
  end
end
