let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

(* [Step], the visitor the fold [fold_step] of [c] calls by name, adds up the
   outputs, which it takes as [int64]. The module defines it first, so the
   compiler inlines it in every profile and no output is boxed. *)
let summed c =
  let fields = Wire_ocaml.outputs c in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name else "Int64.to_int " ^ name)
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
  (* Each module, and its interpreted twin, but for a map, which a twin does
     not read. *)
  let g name ?maps c =
    let m = Wire_ocaml.v ~target ~simd ?maps c in
    write name (Wire_ocaml.to_ml m);
    write (name ^ "i") (Wire_ocaml.to_mli m);
    if Option.is_none maps then begin
      write (base name ^ "_codec.ml") (Wire_ocaml.to_ml ~twin:true m);
      write (base name ^ "_codec.mli") (Wire_ocaml.to_mli ~twin:true m)
    end
  in
  (* The same module on a bigarray. *)
  let big name c =
    let m = Wire_ocaml.v ~target ~buffer:`Bigstring c in
    write name (Wire_ocaml.to_ml m);
    write (name ^ "i") (Wire_ocaml.to_mli m)
  in
  (* A module of the walk alone: its fold [fold], and with [step] the fold
     [fold_step], whose visitor is [Step]. *)
  let s name ?(step = false) c =
    let folds =
      Wire_ocaml.(fold "fold" :: (if step then [ fold "fold_step" ] else []))
    in
    let m = Wire_ocaml.v ~target ~folds c
    and visitors = if step then [ ("fold_step", "step.ml") ] else [] in
    if not step then begin
      write name (Wire_ocaml.to_ml m);
      write (name ^ "i") (Wire_ocaml.to_mli m)
    end
    else begin
      write name (summed c ^ Wire_ocaml.to_ml ~visitors m);
      write (name ^ "i")
        ("module Step : sig\n  type t = int\nend\n\n"
        ^ Wire_ocaml.to_mli ~visitors m)
    end
  in
  g "g_ipv4.ml" Schemas.ipv4;
  g "g_ipv4_record.ml" Schemas.ipv4;
  g "g_mixed_record.ml" Schemas.mixed;
  g "g_checked.ml" Schemas.checked;
  g "g_mixed.ml" Schemas.mixed;
  g "g_small.ml" Schemas.small;
  g "g_eth.ml" Schemas.eth;
  g "g_wide.ml" Schemas.wide;
  g "g_signed.ml" Schemas.signed;
  g "g_packed.ml" Schemas.packed;
  g "g_refined.ml" Schemas.refined;
  g "g_overflow.ml" Schemas.overflow;
  g "g_lanes.ml" Schemas.lanes;
  big "g_ipv4_big.ml" Schemas.ipv4;
  big "g_checked_big.ml" Schemas.checked;
  big "g_mixed_big.ml" Schemas.mixed;
  big "g_eth_big.ml" Schemas.eth;
  big "g_wide_big.ml" Schemas.wide;
  s "s_sized.ml" Schemas.sized;
  s "s_overflow.ml" Schemas.overflow;
  s "s_skipped.ml" Schemas.skipped;
  s "s_huge.ml" ~step:true Schemas.huge;
  s "s_nested.ml" ~step:true (Schemas.nested 12);
  s "s_capture.ml" Packet_stack.capture;
  g "g_mapped.ml" Schemas.mapped
    ~maps:
      Wire_ocaml.
        [
          map "Schemas.u16be_int" ~typ:"int" Schemas.u16be_int;
          map "Schemas.i32be_int" ~typ:"int32" Schemas.i32be_int;
          map "Schemas.proto_lookup" ~typ:"[ `Icmp | `Tcp | `Udp ]"
            Schemas.proto_lookup;
          map "Schemas.flag_bit" ~typ:"bool" Schemas.flag_bit;
        ]
