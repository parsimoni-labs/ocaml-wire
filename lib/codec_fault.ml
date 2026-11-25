open Types
open Codec_leaf
open Codec_repr

type faults = runtime -> Input_end.t -> bytes -> int -> unit

(* A record that does not fit its input reports, before the end of the input,
   the first check that fails on a field that fits, as the validator 3D
   generates does, which checks each field as it reaches it. [faults] runs the
   record's checks in that order and raises end of input at the first field
   that does not fit, which leaves the record's own end of input to report.
   Only a record that does not fit runs it. *)
let first_fault faults runtime input_end buf off =
  if off >= 0 && off <= Input_end.to_int input_end then
    match faults runtime input_end buf off with
    | () -> ()
    | exception Types.Parse_error { kind = Unexpected_eof _; _ } -> ()

(* The record of [min_size] bytes at [off] does not fit before [input_end]. *)
let[@inline never] unfit ~faults min_size runtime input_end buf off =
  first_fault faults runtime input_end buf off;
  frame_error min_size input_end off

let[@inline never] short ~faults min_size buf off =
  unfit ~faults min_size no_runtime (Input_end.of_bytes buf) buf off

(* The frame of a record of [min_size] bytes at [off] lies before
   [input_end]. *)
let check_frame ~faults min_size runtime input_end buf off =
  if off < 0 || off > Input_end.to_int input_end - min_size then
    unfit ~faults min_size runtime input_end buf off

(* The record ending at [end_off], measured before its checks, holds
   [min_size] bytes and ends before [input_end]. *)
let check_measured ~faults min_size runtime input_end buf off end_off =
  if end_off < off + min_size || end_off > Input_end.to_int input_end then begin
    first_fault faults runtime input_end buf off;
    check_end_bounds min_size input_end off end_off
  end

(* The walk for a record's first fault: each field's checks in field order,
   and the [where] clause where 3D checks it: before the fields when it reads
   no field, else with the last field it reads, as the refinement of that
   field the projection makes it. Its scratch is its own, and an action's
   assignment to a parameter stays in it: only a record that does not fit
   runs it. A record without checks has no fault but its end. *)
let record ~has_checks ~n_total ~param_base ~param_handles ~runtime_of ~where
    ~compiled_where faults_rev =
  if not has_checks then fun _ _ _ _ -> ()
  else
    let named = Array.of_list (List.rev faults_rev) in
    let where_at =
      match where with
      | None -> -2
      | Some w ->
          let refs = Types.collect_refs w in
          let last = Stdlib.ref (-1) in
          Array.iteri
            (fun i (names, _) ->
              if List.exists (fun n -> List.mem n refs) names then last := i)
            named;
          !last
    in
    let steps = Array.map snd named in
    let wide = params_wide param_handles in
    fun runtime input_end buf off ->
      let arr = alloc_slots n_total in
      seed_params arr runtime ~wide param_base 0 param_handles;
      let runtime = runtime_of arr in
      let where () =
        match compiled_where with
        | Some f when not (eval_arr_at ~at:off f arr) ->
            raise_constraint ~at:off ~which:Where ()
        | _ -> ()
      in
      if where_at = -1 then where ();
      Array.iteri
        (fun i step ->
          step arr runtime input_end buf off;
          if i = where_at then where ())
        steps
