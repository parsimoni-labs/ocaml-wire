open Gen
open Properties
open Curated

(* A registry gen with what its per-field properties address, made on first
   use: [addressed] compiles a wrapper codec, too expensive to make per draw. *)
type picked = Picked : 'a t * 'a addressed Lazy.t -> picked

let picked =
  List.map
    (fun (name, Pack g) -> (name, Picked (g, lazy (addressed g))))
    registry

(* Per sample, pick a random registry gen and round-trip it through the
   alternate entry points, so they cover the whole registry over many samples
   rather than a pinned subset. *)
let registry_pick =
  Alcobar.choose (List.map (fun (_, p) -> Alcobar.const p) registry)

(* Same pick, keeping the entry name so a failure says which registry codec
   diverged. *)
let registry_pick_named =
  Alcobar.choose (List.map (fun (name, p) -> Alcobar.const (name, p)) picked)

(* The fresh-buffer encoders and the embedded decoder against the canonical
   bytes of a positive. *)
let ep_direct label =
  Alcobar.dynamic_bind registry_pick_named (fun (_, Picked (g, a)) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (value, bs) () ->
          if Option.is_none g.env then begin
            let s = string_of_bytes bs in
            if Wire.Codec.to_string g.codec value <> s then
              Alcobar.failf "%s Codec.to_string reencode mismatch" label;
            if string_of_bytes (Wire.Codec.to_bytes g.codec value) <> s then
              Alcobar.failf "%s Codec.to_bytes reencode mismatch" label;
            check_embedded_agree label "direct positive" (Lazy.force a) g bs
          end))

let ep_direct_safety label kind adversarial =
  Alcobar.dynamic_bind registry_pick_named (fun (name, Picked (g, a)) ->
      let stream = if adversarial then g.adversarial else g.random in
      Alcobar.map
        Alcobar.[ stream ]
        (fun bs () ->
          check_embedded_agree
            (Fmt.str "%s %s" label name)
            ("direct " ^ kind) (Lazy.force a) g bs))

let ep_validate label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (_value, bs) () ->
          let env = positive_env g in
          match Wire.Codec.validate ?env g.codec bs ~off:0 with
          | Ok () -> ()
          | Error _ -> Alcobar.failf "%s validate rejected a positive" label
          | exception Invalid_argument _ ->
              Alcobar.failf "%s validate crashed on a positive" label))

let entry_point_cases label =
  [
    Alcobar.test_case (label ^ " to_string")
      Alcobar.[ ep_direct label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " direct random")
      Alcobar.[ ep_direct_safety label "random" false ]
      (fun run -> run ());
    Alcobar.test_case
      (label ^ " direct adversarial")
      Alcobar.[ ep_direct_safety label "adversarial" true ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " validate")
      Alcobar.[ ep_validate label ]
      (fun run -> run ());
  ]
