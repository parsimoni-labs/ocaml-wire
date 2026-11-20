(* Writes the generated modules the tests here compile without wire, each with
   its interface: each codec's own record, so no module names the codec or its
   schema. [--oxcaml] generates for the OxCaml target. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  let g name codec c =
    let codec = "Schemas." ^ codec in
    write name (Wire_ocaml.ml ~target ~codec c);
    write (name ^ "i") (Wire_ocaml.mli ~target ~codec c)
  in
  g "s_ipv4.ml" "ipv4" Schemas.ipv4;
  g "s_checked.ml" "checked" Schemas.checked;
  g "s_mixed.ml" "mixed" Schemas.mixed;
  g "s_eth.ml" "eth" Schemas.eth;
  g "s_wide.ml" "wide" Schemas.wide;
  g "s_signed.ml" "signed" Schemas.signed;
  g "s_refined.ml" "refined" Schemas.refined
