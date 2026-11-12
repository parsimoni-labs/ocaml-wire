let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* [--oxcaml] generates for the OxCaml target. *)
let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  (* Each module, and the bridge that checks it against its codec. *)
  let g name ?make ?maps codec c =
    let codec = "Schemas." ^ codec in
    write name (Wire_ocaml.ml ~target ?make ?maps ~codec c);
    let base = Filename.remove_extension name in
    write (base ^ "_wire.ml")
      (Wire_ocaml.bridge ?maps ~codec ~core:(String.capitalize_ascii base) c)
  in
  g "g_ipv4.ml" ~make:"Schemas.ipv4_make" "ipv4" Schemas.ipv4;
  g "g_ipv4_record.ml" "ipv4" Schemas.ipv4;
  g "g_checked.ml" ~make:"Schemas.checked_make" "checked" Schemas.checked;
  g "g_mixed.ml" ~make:"Schemas.mixed_make" "mixed" Schemas.mixed;
  g "g_small.ml" ~make:"Schemas.small_make" "small" Schemas.small;
  g "g_eth.ml" ~make:"Schemas.eth_make" "eth" Schemas.eth;
  g "g_wide.ml" ~make:"Schemas.wide_make" "wide" Schemas.wide;
  g "g_signed.ml" ~make:"Schemas.signed_make" "signed" Schemas.signed;
  g "g_refined.ml" ~make:"Schemas.refined_make" "refined" Schemas.refined;
  g "g_overflow.ml" ~make:"Schemas.overflow_make" "overflow" Schemas.overflow;
  g "g_mapped.ml" ~make:"Schemas.mapped_make"
    ~maps:
      Wire_ocaml.
        [
          ("Schemas.u16be_int", Typ Schemas.u16be_int);
          ("Schemas.i32be_int", Typ Schemas.i32be_int);
          ("Schemas.proto_lookup", Typ Schemas.proto_lookup);
          ("Schemas.flag_bit", Typ Schemas.flag_bit);
        ]
    "mapped" Schemas.mapped
