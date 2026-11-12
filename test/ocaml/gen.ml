let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* A consumer the scanner of [c] calls by name, which adds up the outputs,
   and that scanner with every output. *)
let summed ~target ~codec c =
  let fields = Wire_ocaml.scanner_fields c in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) -> if is_bool then "Bool.to_int " ^ name else name)
      fields
  in
  "module Step = struct\nlet invalid acc = acc - 1\nlet valid acc "
  ^ String.concat " " args ^ " =\nacc + " ^ String.concat " + " terms
  ^ "\nend\n\n"
  ^ Wire_ocaml.scanner ~target ~step:"Step" ~codec c

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
  write "s_sized.ml"
    (Wire_ocaml.scanner ~target ~codec:"Schemas.sized" Schemas.sized);
  write "s_overflow.ml"
    (Wire_ocaml.scanner ~target ~codec:"Schemas.overflow" Schemas.overflow);
  write "s_skipped.ml"
    (Wire_ocaml.scanner ~target ~codec:"Schemas.skipped" Schemas.skipped);
  write "s_nested.ml"
    (summed ~target ~codec:"Schemas.nested 12" (Schemas.nested 12));
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
