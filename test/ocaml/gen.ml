(* The modules under test, as {!Wire_ocaml.main} reads its command line, for
   the compiler that builds this generator: one dune rule writes each. With
   [WIRE_SIMD=1] in the environment, which the OxCaml rule sets on arm64 or
   amd64, g_lanes has the SIMD lanes of its records. A module with a fold
   names its fields by their paths. *)

(* Every field of [Schemas.nested n]: its tag and the field after its inner
   record, at each level. *)
let nested_paths n =
  let rec level prefix k =
    let path f = prefix ^ f in
    if k <= 1 then [ path "tag"; path "after" ]
    else (path "tag" :: level (prefix ^ "inner.") (k - 1)) @ [ path "after" ]
  in
  level "" n

let () =
  let simd = Sys.getenv_opt "WIRE_SIMD" = Some "1" in
  Wire_ocaml.(
    main
      [
        v ~name:"g_ipv4" Schemas.ipv4;
        v ~name:"g_ipv4_record" Schemas.ipv4;
        v ~name:"g_mixed_record" Schemas.mixed;
        v ~name:"g_checked"
          ~fold:[ "version"; "ihl"; "length"; "ttl"; "proto" ]
          Schemas.checked;
        v ~name:"g_mixed" Schemas.mixed;
        v ~name:"g_small" Schemas.small;
        v ~name:"g_eth" Schemas.eth;
        v ~name:"g_wide" Schemas.wide;
        v ~name:"g_signed" Schemas.signed;
        v ~name:"g_packed" Schemas.packed;
        v ~name:"g_refined" Schemas.refined;
        v ~name:"g_overflow" Schemas.overflow;
        v ~name:"g_lanes" ~simd Schemas.lanes;
        v ~name:"s_sized" ~fold:[ "n"; "tail" ] Schemas.sized;
        v ~name:"s_overflow"
          ~fold:[ "a"; "hi"; "lo"; "b"; "c" ]
          Schemas.overflow;
        v ~name:"s_skipped" ~fold:[ "n"; "x"; "a"; "b" ] Schemas.skipped;
        v ~name:"s_huge" ~fold:[ "flag"; "tail" ] Schemas.huge;
        v ~name:"s_nested" ~fold:(nested_paths 12) (Schemas.nested 12);
        v ~name:"s_capture" Packet_stack.capture;
        v ~name:"s_capture_step"
          ~fold:
            [
              "ipv4.protocol";
              "ipv4.src";
              "ipv4.dst";
              "ipv4.tcp.dst_port";
              "ipv4.udp.dst_port";
              "ipv6.next_header";
              "ipv6.dst3";
            ]
          Packet_stack.capture;
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
