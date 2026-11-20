let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* [Step], the consumer the scanner of [c] calls by name, adds up the outputs,
   which it takes as [int64] on OCaml. The scanner's module defines it first,
   so the compiler inlines it in every profile and no output is boxed. *)
let summed target c =
  let fields = Wire_ocaml.scanner_fields c in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name
        else if target = `OCaml then "Int64.to_int " ^ name
        else name)
      fields
  in
  "module Step = struct\n\
   type t = int\n\n\
   let invalid acc = acc - 1\n\
   let[@inline] valid acc " ^ String.concat " " args ^ " =\nacc + "
  ^ String.concat " + " terms ^ "\nend\n\n"

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
  let s name ?(step = false) codec c =
    let codec = "Schemas." ^ codec in
    let ml = Wire_ocaml.scanner ~target ~codec c
    and mli = Wire_ocaml.scanner_mli ~target ~codec c in
    if not step then begin
      write name ml;
      write (name ^ "i") mli
    end
    else begin
      let step = "Step" in
      write name (summed target c ^ Wire_ocaml.scanner ~target ~step ~codec c);
      write (name ^ "i")
        ("module Step : sig\n  type t = int\nend\n\n"
        ^ Wire_ocaml.scanner_mli ~target ~step ~codec c)
    end
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
  s "s_deep.ml" ~step:true "deep 32 (Sys.max_string_length - 1)"
    (Schemas.deep 32 (Sys.max_string_length - 1));
  s "s_huge.ml" ~step:true "huge" Schemas.huge;
  s "s_bigspan.ml" "bigspan" Schemas.bigspan;
  s "s_nested.ml" ~step:true "nested 12" (Schemas.nested 12);
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
