open Gen
open Properties
open Curated

let truncate_bytes ~max_len bs =
  if Bytes.length bs <= max_len then bs else Bytes.sub bs 0 max_len

type afl_input = { selector : int; mode : int; payload : bytes }

let afl_payload bs =
  let len = Bytes.length bs in
  if len <= 2 then Bytes.empty else Bytes.sub bs 2 (len - 2)

let parse_afl_input ?(max_len = 256) bs =
  {
    selector = (if Bytes.length bs = 0 then 0 else Bytes.get_uint8 bs 0);
    mode = (if Bytes.length bs <= 1 then 0 else Bytes.get_uint8 bs 1);
    payload = truncate_bytes ~max_len (afl_payload bs);
  }

let afl_input_gen =
  Alcobar.map
    Alcobar.[ Alcobar.uint8; Alcobar.uint8; bytes_any ]
    (fun selector mode payload ->
      let framed = Bytes.create (2 + Bytes.length payload) in
      Bytes.set_uint8 framed 0 selector;
      Bytes.set_uint8 framed 1 mode;
      Bytes.blit payload 0 framed 2 (Bytes.length payload);
      framed)

let afl_contract_cases ?(max_len = 256) label cases ~check =
  match cases with
  | [] -> []
  | _ ->
      [
        Alcobar.test_case (label ^ " afl")
          Alcobar.[ afl_input_gen ]
          (fun bs ->
            let input = parse_afl_input ~max_len bs in
            let name, item =
              List.nth cases (input.selector mod List.length cases)
            in
            let label = label ^ " " ^ name in
            try check label item input
            with e when Lazy.force file_input ->
              if not (failure_reported ()) then
                Fmt.epr "FAIL %s: %s@." label (Printexc.to_string e);
              Fmt.epr "  case %s, mode %d, payload %a@." label input.mode pp_hex
                (string_of_bytes input.payload);
              raise e);
      ]

(* A registry codec for the AFL lane, with what the fast-path oracles compare
   it against, built on the first input that selects it. *)
type afl_target = Target : 'a t * 'a addressed Lazy.t -> afl_target

let afl_cases_for ?max_len cases label =
  let cases =
    List.map
      (fun (name, Pack g) -> (name, Target (g, lazy (addressed g))))
      cases
  in
  afl_contract_cases ?max_len label cases
    ~check:(fun case (Target (g, a)) input ->
      let env = positive_env g in
      match input.mode mod 6 with
      | 0 ->
          check_decode_safety case "afl" ?env g input.payload;
          check_validate_safety case "afl" ?env g input.payload
      | 1 -> check_decode_safety case "afl" ?env g input.payload
      | 2 -> check_validate_safety case "afl" ?env g input.payload
      | 3 -> check_decode_validate_agree case "afl" ?env g input.payload
      | 4 -> check_fast_general case "afl" ?env (Lazy.force a) g input.payload
      | _ -> check_cold_warm case "afl" ?env (Lazy.force a) g input.payload)

(* AFL fuzzes the whole registry, not a curated subset: the [selector] byte
   picks one codec per input, so covering every combinator costs nothing
   per-exec and only spreads mutation across more shapes over a run. *)
let afl_cases ?max_len label = afl_cases_for ?max_len registry label

let afl_env_cases ?max_len label =
  let env_cases = List.filter (fun (_, p) -> binds_env p) registry in
  afl_cases_for ?max_len env_cases label
