(* The modules under test, as {!Wire_ocaml.main} reads its command line, for
   the compiler that builds this generator: one dune rule writes each. With
   [WIRE_SIMD=1] in the environment, which the OxCaml rule sets on arm64 or
   amd64, g_lanes has the SIMD lanes of its records.

   With [--step FILE], the visitor FILE of the fold [fold] of a module under
   test. The visitor of s_nested adds up the outputs; each other one conses
   the outputs of a record, by name, onto the accumulator, or [None] for a
   record that fails. Both take the integer outputs as [int64]. *)

let write file s =
  let oc = open_out file in
  output_string oc s;
  close_out oc

let labels c = List.map (fun (name, _) -> "~" ^ name) (Wire_ocaml.outputs c)

let summed c =
  let terms =
    List.map
      (fun (name, is_bool) ->
        if is_bool then "Bool.to_int " ^ name else "Int64.to_int " ^ name)
      (Wire_ocaml.outputs c)
  in
  "type t = int\n\nlet invalid (acc : t) = acc - 1\nlet[@inline] valid acc "
  ^ String.concat " " (labels c)
  ^ " =\nacc + " ^ String.concat " + " terms ^ "\n"

let snapshot c =
  let pairs =
    List.map
      (fun (name, is_bool) ->
        Fmt.str "(%S, %s %s)" name
          (if is_bool then "Bool.to_int" else "Int64.to_int")
          name)
      (Wire_ocaml.outputs c)
  in
  "type t = (string * int) list option list\n\n\
   let invalid acc = None :: acc\n\
   let valid acc "
  ^ String.concat " " (labels c)
  ^ " =\n  Some [" ^ String.concat "; " pairs ^ "] :: acc\n"

let huge = Schemas.huge
let nested = Schemas.nested 12

let visitors =
  [
    ("checked_step.ml", fun () -> snapshot Schemas.checked);
    ("sized_step.ml", fun () -> snapshot Schemas.sized);
    ("overflow_step.ml", fun () -> snapshot Schemas.overflow);
    ("skipped_step.ml", fun () -> snapshot Schemas.skipped);
    ("huge_step.ml", fun () -> snapshot huge);
    ("nested_step.ml", fun () -> summed nested);
  ]

let () =
  match Sys.argv with
  | [| _; "--step"; file |] -> (
      match List.assoc_opt (Filename.basename file) visitors with
      | Some text -> write file (text ())
      | None -> failwith ("gen: no visitor " ^ file))
  | _ ->
      let simd = Sys.getenv_opt "WIRE_SIMD" = Some "1" in
      let scanner name c =
        Wire_ocaml.v ~name ~folds:[ Wire_ocaml.fold "fold" ] c
      in
      Wire_ocaml.(
        main
          [
            v ~name:"g_ipv4" Schemas.ipv4;
            v ~name:"g_ipv4_record" Schemas.ipv4;
            v ~name:"g_mixed_record" Schemas.mixed;
            v ~name:"g_checked" ~folds:[ fold "fold" ] Schemas.checked;
            v ~name:"g_mixed" Schemas.mixed;
            v ~name:"g_small" Schemas.small;
            v ~name:"g_eth" Schemas.eth;
            v ~name:"g_wide" Schemas.wide;
            v ~name:"g_signed" Schemas.signed;
            v ~name:"g_packed" Schemas.packed;
            v ~name:"g_refined" Schemas.refined;
            v ~name:"g_overflow" Schemas.overflow;
            v ~name:"g_lanes" ~simd Schemas.lanes;
            scanner "s_sized" Schemas.sized;
            scanner "s_overflow" Schemas.overflow;
            scanner "s_skipped" Schemas.skipped;
            scanner "s_huge" huge;
            scanner "s_nested" nested;
            v ~name:"s_capture" Packet_stack.capture;
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
