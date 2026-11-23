(* Writes the generated modules the tests here compile without wire, each with
   its interface: each codec's own record, so no module names the codec or its
   schema. [--oxcaml] generates for the OxCaml target. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* The scanner of [Schemas.stacked] with its step functions, after [Step],
   the consumer they call. [Step] adds up the outputs, which it takes as
   [int64]. *)
let stacked target =
  let fields = Wire_ocaml.scanner_fields Schemas.stacked in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name else "Int64.to_int " ^ name)
      fields
  and codec = "Schemas.stacked" in
  write "s_stacked.ml"
    ("module Step = struct\n\
      type t = int\n\n\
      let invalid acc = acc - 1\n\
      let[@inline] valid acc " ^ String.concat " " args ^ " =\nacc + "
   ^ String.concat " + " terms ^ "\nend\n\n"
    ^ Wire_ocaml.scanner ~target ~step:"Step" ~codec Schemas.stacked);
  write "s_stacked.mli"
    ("module Step : sig\n  type t = int\nend\n\n"
    ^ Wire_ocaml.scanner_mli ~target ~step:"Step" ~codec Schemas.stacked)

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
  g "s_refined.ml" "refined" Schemas.refined;
  stacked target
