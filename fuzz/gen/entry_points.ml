open Gen
open Properties
open Curated

(* Per sample, pick a random registry gen and round-trip it through the
   alternate entry points, so they cover the whole registry over many samples
   rather than a pinned subset. [of_string] / [of_reader] skip codecs that bind
   a [Param.env], which those entry points cannot thread. *)
let registry_pick =
  Alcobar.choose (List.map (fun (_, p) -> Alcobar.const p) registry)

(* Same pick, keeping the entry name so a failure says which registry codec
   diverged (and so the agreement quarantine can be looked up by name). *)
let registry_pick_named =
  Alcobar.choose (List.map (fun (name, p) -> Alcobar.const (name, p)) registry)

let ep_direct label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (value, bs) () ->
          if Option.is_none g.env then begin
            let s = string_of_bytes bs in
            (match Wire.of_string g.typ s with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_string decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_string decode failed" label);
            (match Wire.of_string_exn g.typ s with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_string_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_string_exn decode failed" label);
            (match Wire.of_bytes g.typ bs with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_bytes decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_bytes decode failed" label);
            (match Wire.of_bytes_exn g.typ bs with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_bytes_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_bytes_exn decode failed" label);
            if Wire.to_string g.typ value <> s then
              Alcobar.failf "%s of_string reencode mismatch" label;
            if string_of_bytes (Wire.to_bytes g.typ value) <> s then
              Alcobar.failf "%s to_bytes reencode mismatch" label
          end))

let ep_direct_safety label kind adversarial =
  Alcobar.dynamic_bind registry_pick_named (fun (name, Pack g) ->
      let stream = if adversarial then g.adversarial else g.random in
      Alcobar.map
        Alcobar.[ stream ]
        (fun bs () ->
          if Option.is_none g.env then begin
            (match Wire.of_bytes g.typ bs with
            | Ok _ | Error _ -> ()
            | exception Invalid_argument _ ->
                Alcobar.failf "%s %s direct %s crashed decoder" label name kind);
            check_direct_codec_agree name ("direct " ^ kind) g bs
          end))

let ep_streaming label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (value, bs) () ->
          if Option.is_none g.env then begin
            let reader = Bytesrw.Bytes.Reader.of_bytes bs in
            (match Wire.of_reader g.typ reader with
            | Ok d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_reader decode mismatch" label
            | Error _ -> Alcobar.failf "%s of_reader decode failed" label);
            let reader = Bytesrw.Bytes.Reader.of_bytes bs in
            (match Wire.of_reader_exn g.typ reader with
            | d ->
                if not (g.equal d value) then
                  Alcobar.failf "%s of_reader_exn decode mismatch" label
            | exception Wire.Parse_error _ ->
                Alcobar.failf "%s of_reader_exn decode failed" label);
            let buf = Buffer.create (Bytes.length bs) in
            let w = Bytesrw.Bytes.Writer.of_buffer buf in
            Wire.to_writer g.typ value w;
            Bytesrw.Bytes.Writer.write_eod w;
            if Buffer.contents buf <> string_of_bytes bs then
              Alcobar.failf "%s of_reader reencode mismatch" label
          end))

let ep_validate label =
  Alcobar.dynamic_bind registry_pick (fun (Pack g) ->
      Alcobar.map
        Alcobar.[ g.positive ]
        (fun (_value, bs) () ->
          let env = positive_env g in
          match
            try
              Wire.Codec.validate ?env g.codec bs 0;
              `Ok
            with
            | Wire.Parse_error _ -> `Reject
            | Invalid_argument _ -> `Crash
          with
          | `Ok -> ()
          | `Reject -> Alcobar.failf "%s validate rejected a positive" label
          | `Crash -> Alcobar.failf "%s validate crashed on a positive" label))

let entry_point_cases label =
  [
    Alcobar.test_case (label ^ " of_string")
      Alcobar.[ ep_direct label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " direct random")
      Alcobar.[ ep_direct_safety label "random" false ]
      (fun run -> run ());
    Alcobar.test_case
      (label ^ " direct adversarial")
      Alcobar.[ ep_direct_safety label "adversarial" true ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " of_reader")
      Alcobar.[ ep_streaming label ]
      (fun run -> run ());
    Alcobar.test_case (label ^ " validate")
      Alcobar.[ ep_validate label ]
      (fun run -> run ());
  ]
