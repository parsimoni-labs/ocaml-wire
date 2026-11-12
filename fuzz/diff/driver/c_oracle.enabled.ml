let enabled = true

let oracle label =
  let fields, scan, standalone =
    List.assoc label Diff_scan_c.Scan_oracle_index.covered
  in
  let n = List.length fields in
  let scan buf off len =
    let out = Array.make n 0 in
    let end_ = scan buf off len out in
    (end_, List.mapi (fun i (name, _) -> (name, out.(i))) fields)
  in
  { Diff_codecs.fields; scan; standalone }
