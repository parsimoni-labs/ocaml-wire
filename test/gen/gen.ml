let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* [--oxcaml] generates for the OxCaml target. *)
let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  let g name ?make ?maps codec c =
    write name (Wire_gen.ml ~target ?make ?maps ~codec:("Schemas." ^ codec) c)
  in
  g "g_ipv4.ml" ~make:"Schemas.ipv4_make" "ipv4" Schemas.ipv4;
  g "g_ipv4_record.ml" "ipv4" Schemas.ipv4;
  g "g_checked.ml" ~make:"Schemas.checked_make" "checked" Schemas.checked;
  g "g_mixed.ml" ~make:"Schemas.mixed_make" "mixed" Schemas.mixed;
  g "g_small.ml" ~make:"Schemas.small_make" "small" Schemas.small;
  g "g_eth.ml" ~make:"Schemas.eth_make" "eth" Schemas.eth;
  g "g_wide.ml" ~make:"Schemas.wide_make" "wide" Schemas.wide;
  g "g_mapped.ml" ~make:"Schemas.mapped_make"
    ~maps:
      Wire_gen.
        [
          ("Schemas.u16be_int", Typ Schemas.u16be_int);
          ("Schemas.i32be_int", Typ Schemas.i32be_int);
          ("Schemas.proto_lookup", Typ Schemas.proto_lookup);
          ("Schemas.flag_bit", Typ Schemas.flag_bit);
        ]
    "mapped" Schemas.mapped
