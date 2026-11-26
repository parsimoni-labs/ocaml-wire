(* The modules under test, as {!Wire_ocaml.main} reads its command line, for
   the compiler that builds this generator: one dune rule writes each. With
   [WIRE_SIMD=1] in the environment, which the OxCaml rule sets on arm64 or
   amd64, g_lanes has the SIMD lanes of its records.

   With [--step FILE], the visitor FILE of a scanner's fold [fold_step]: for
   huge_step.ml the visitor of s_huge, for nested_step.ml that of s_nested. It
   adds up the outputs, which it takes as [int64]. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let summed c =
  let fields = Wire_ocaml.outputs c in
  let args = List.map (fun (name, _) -> "~" ^ name) fields
  and terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name else "Int64.to_int " ^ name)
      fields
  in
  "type t = int\n\nlet invalid (acc : t) = acc - 1\nlet[@inline] valid acc "
  ^ String.concat " " args ^ " =\nacc + " ^ String.concat " + " terms ^ "\n"

let huge = Schemas.huge
let nested = Schemas.nested 12

let () =
  match Sys.argv with
  | [| _; "--step"; file |] -> (
      match Filename.basename file with
      | "huge_step.ml" -> write file (summed huge)
      | "nested_step.ml" -> write file (summed nested)
      | f -> failwith ("gen: no visitor " ^ f))
  | _ ->
      let simd = Sys.getenv_opt "WIRE_SIMD" = Some "1" in
      let big name c = Wire_ocaml.v ~name ~buffer:`Bigstring c in
      let scanner name ?(step = false) c =
        let folds =
          Wire_ocaml.(
            fold "fold" :: (if step then [ fold "fold_step" ] else []))
        in
        Wire_ocaml.v ~name ~folds c
      in
      Wire_ocaml.(
        main
          [
            v ~name:"g_ipv4" Schemas.ipv4;
            v ~name:"g_ipv4_record" Schemas.ipv4;
            v ~name:"g_mixed_record" Schemas.mixed;
            v ~name:"g_checked" Schemas.checked;
            v ~name:"g_mixed" Schemas.mixed;
            v ~name:"g_small" Schemas.small;
            v ~name:"g_eth" Schemas.eth;
            v ~name:"g_wide" Schemas.wide;
            v ~name:"g_signed" Schemas.signed;
            v ~name:"g_packed" Schemas.packed;
            v ~name:"g_refined" Schemas.refined;
            v ~name:"g_overflow" Schemas.overflow;
            v ~name:"g_lanes" ~simd Schemas.lanes;
            big "g_ipv4_big" Schemas.ipv4;
            big "g_checked_big" Schemas.checked;
            big "g_mixed_big" Schemas.mixed;
            big "g_eth_big" Schemas.eth;
            big "g_wide_big" Schemas.wide;
            scanner "s_sized" Schemas.sized;
            scanner "s_overflow" Schemas.overflow;
            scanner "s_skipped" Schemas.skipped;
            scanner "s_huge" ~step:true huge;
            scanner "s_nested" ~step:true nested;
            scanner "s_capture" Packet_stack.capture;
            v ~name:"g_mapped" Schemas.mapped
              ~maps:
                [
                  map "Schemas.u16be_int" ~typ:"int" Schemas.u16be_int;
                  map "Schemas.i32be_int" ~typ:"int32" Schemas.i32be_int;
                  map "Schemas.proto_lookup" ~typ:"[ `Icmp | `Tcp | `Udp ]"
                    Schemas.proto_lookup;
                  map "Schemas.flag_bit" ~typ:"bool" Schemas.flag_bit;
                ];
          ])
