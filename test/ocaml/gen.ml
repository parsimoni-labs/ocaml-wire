let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* A consumer the scanner of [c] calls by name, which adds up the outputs. *)
let summed c =
  let fields = Wire_ocaml.scanner_fields c in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) -> if is_bool then "Bool.to_int " ^ name else name)
      fields
  in
  "type t = int\n\nlet invalid acc = acc - 1\nlet valid acc "
  ^ String.concat " " args ^ " =\nacc + " ^ String.concat " + " terms ^ "\n"

(* [--oxcaml] generates for the OxCaml target, and [--simd] adds the SIMD
   lanes of the records that have them (OxCaml on arm64 or amd64). Each module
   comes with its interface. *)
let () =
  let flag f = Array.exists (String.equal f) Sys.argv in
  let target = if flag "--oxcaml" then `OxCaml else `OCaml in
  let simd = flag "--simd" in
  let base name = Filename.remove_extension name in
  (* Each module, and the bridge that checks it against its codec. [make] is
     the constructor [Schemas.<codec>_make] and its type [Schemas.<codec>]. *)
  let g name ?(make = true) ?maps codec c =
    let make =
      if make then Some ("Schemas." ^ codec ^ "_make", "Schemas." ^ codec)
      else None
    in
    let codec = "Schemas." ^ codec
    and core = String.capitalize_ascii (base name) in
    write name (Wire_ocaml.ml ~target ~simd ?make ?maps ~codec c);
    write (name ^ "i") (Wire_ocaml.mli ~target ~simd ?make ?maps ~codec c);
    write (base name ^ "_wire.ml") (Wire_ocaml.bridge ?maps ~codec ~core c);
    write (base name ^ "_wire.mli") (Wire_ocaml.bridge_mli ?maps ~codec ~core c)
  in
  let s name ?step codec c =
    let codec = "Schemas." ^ codec in
    write name (Wire_ocaml.scanner ~target ?step ~codec c);
    write (name ^ "i") (Wire_ocaml.scanner_mli ~target ?step ~codec c)
  in
  g "g_ipv4.ml" "ipv4" Schemas.ipv4;
  g "g_ipv4_record.ml" ~make:false "ipv4" Schemas.ipv4;
  g "g_checked.ml" "checked" Schemas.checked;
  g "g_mixed.ml" "mixed" Schemas.mixed;
  g "g_small.ml" "small" Schemas.small;
  g "g_eth.ml" "eth" Schemas.eth;
  g "g_wide.ml" "wide" Schemas.wide;
  g "g_signed.ml" "signed" Schemas.signed;
  g "g_packed.ml" "packed" Schemas.packed;
  g "g_refined.ml" "refined" Schemas.refined;
  g "g_overflow.ml" "overflow" Schemas.overflow;
  g "g_lanes.ml" "lanes" Schemas.lanes;
  s "s_sized.ml" "sized" Schemas.sized;
  s "s_overflow.ml" "overflow" Schemas.overflow;
  s "s_skipped.ml" "skipped" Schemas.skipped;
  write "s_nested_step.ml" (summed (Schemas.nested 12));
  s "s_nested.ml" ~step:"S_nested_step" "nested 12" (Schemas.nested 12);
  g "g_mapped.ml" "mapped" Schemas.mapped
    ~maps:
      Wire_ocaml.
        [
          map "Schemas.u16be_int" ~typ:"int" Schemas.u16be_int;
          map "Schemas.i32be_int" ~typ:"int32" Schemas.i32be_int;
          map "Schemas.proto_lookup" ~typ:"[ `Icmp | `Tcp | `Udp ]"
            Schemas.proto_lookup;
          map "Schemas.flag_bit" ~typ:"bool" Schemas.flag_bit;
        ]
