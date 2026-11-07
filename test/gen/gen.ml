let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let () =
  let g name ?make codec c =
    write name (Wire_gen.ml ?make ~codec:("Schemas." ^ codec) c)
  in
  g "g_ipv4.ml" ~make:"Schemas.ipv4_make" "ipv4" Schemas.ipv4;
  g "g_ipv4_record.ml" "ipv4" Schemas.ipv4;
  g "g_checked.ml" ~make:"Schemas.checked_make" "checked" Schemas.checked;
  g "g_mixed.ml" ~make:"Schemas.mixed_make" "mixed" Schemas.mixed;
  g "g_small.ml" ~make:"Schemas.small_make" "small" Schemas.small;
  g "g_eth.ml" ~make:"Schemas.eth_make" "eth" Schemas.eth;
  g "g_wide.ml" ~make:"Schemas.wide_make" "wide" Schemas.wide;
  g "g_variable.ml" ~make:"Schemas.variable_make" "variable" Schemas.variable
