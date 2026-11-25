(* Writes the generated modules the tests here compile without wire, each with
   its interface: each codec's own record, so no module names the codec or its
   schema, two of them also on a bigarray. [--oxcaml] generates for the OxCaml
   target. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* The scanner of [Schemas.stacked] with its step functions, after [Step],
   the consumer they call. [Step] adds up the outputs, which it takes as
   [int64]. *)
let stacked target =
  let fields = Wire_ocaml.outputs Schemas.stacked in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name else "Int64.to_int " ^ name)
      fields
  and m =
    Wire_ocaml.(
      v ~target ~folds:[ fold "fold"; fold "fold_step" ] Schemas.stacked)
  and visitors = [ ("fold_step", "step.ml") ] in
  write "s_stacked.ml"
    ("module Step = struct\n\
      type t = int\n\n\
      let invalid acc = acc - 1\n\
      let[@inline] valid acc " ^ String.concat " " args ^ " =\nacc + "
   ^ String.concat " + " terms ^ "\nend\n\n"
    ^ Wire_ocaml.to_ml ~visitors m);
  write "s_stacked.mli"
    ("module Step : sig\n  type t = int\nend\n\n"
    ^ Wire_ocaml.to_mli ~visitors m)

let () =
  let target =
    if Array.exists (String.equal "--oxcaml") Sys.argv then `OxCaml else `OCaml
  in
  let g ?buffer name c =
    let m = Wire_ocaml.v ~target ?buffer c in
    write name (Wire_ocaml.to_ml m);
    write (name ^ "i") (Wire_ocaml.to_mli m)
  in
  g "s_ipv4.ml" Schemas.ipv4;
  g ~buffer:`Bigstring "s_ipv4_big.ml" Schemas.ipv4;
  g "s_checked.ml" Schemas.checked;
  g ~buffer:`Bigstring "s_checked_big.ml" Schemas.checked;
  g "s_mixed.ml" Schemas.mixed;
  g "s_eth.ml" Schemas.eth;
  g "s_wide.ml" Schemas.wide;
  g "s_signed.ml" Schemas.signed;
  g "s_refined.ml" Schemas.refined;
  stacked target
