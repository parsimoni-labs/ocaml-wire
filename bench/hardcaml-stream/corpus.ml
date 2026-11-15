let () =
  let file, prefixes =
    match Array.to_list Sys.argv with
    | [ _; file ] -> (file, [])
    | [ _; "--all"; file ] ->
        let prefixes =
          List.concat_map
            (fun (Diff_codecs.Scan c) ->
              if String.equal c.label "scan_packet" then c.corpus () else [])
            Diff_codecs.scan_cases
        in
        (file, prefixes)
    | _ -> invalid_arg "usage: corpus.exe [--all] OUTPUT.bin"
  in
  let frames = prefixes @ Diff_codecs.packet_payload_corpus () in
  Out_channel.with_open_bin file (fun output ->
      List.iter
        (fun frame ->
          let length = Bytes.create 4 in
          Bytes.set_int32_le length 0 (Int32.of_int (Bytes.length frame));
          output_bytes output length;
          output_bytes output frame)
        frames);
  Fmt.pr "wrote %d length-prefixed scan_packet frames@." (List.length frames)
