(* Writes the generated modules the tests here compile without wire, each with
   its interface: each codec's own record, so no module names the codec or its
   schema. [--oxcaml] generates for the OxCaml
   target. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* The module of [Schemas.stacked] with a fold of all its integer fields,
   those of the record it holds under [inner]. *)
let stacked target =
  let m =
    Wire_ocaml.v ~target
      ~fold:
        [
          "kind";
          "flags";
          "inner.version";
          "inner.ihl";
          "inner.length";
          "inner.ttl";
          "inner.proto";
        ]
      Schemas.stacked
  in
  write "s_stacked.ml" (Wire_ocaml.to_ml m);
  write "s_stacked.mli" (Wire_ocaml.to_mli m)

let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  let g name c =
    let m = Wire_ocaml.v ~target c in
    write name (Wire_ocaml.to_ml m);
    write (name ^ "i") (Wire_ocaml.to_mli m)
  in
  g "s_ipv4.ml" Schemas.ipv4;
  g "s_checked.ml" Schemas.checked;
  g "s_mixed.ml" Schemas.mixed;
  g "s_eth.ml" Schemas.eth;
  g "s_wide.ml" Schemas.wide;
  g "s_signed.ml" Schemas.signed;
  g "s_refined.ml" Schemas.refined;
  stacked target
