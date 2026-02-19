(** Schema definitions for differential testing.

    These schemas are used to test that our OCaml parser produces the same
    results as the EverParse-generated C parser. *)

open D3t

(* Simple header schema: version (u8) + length (u16) + flags (u8) *)
type simple_header = { version : int; length : int; flags : int }

let simple_header_codec =
  let open Codec in
  record "SimpleHeader" (fun version length flags -> { version; length; flags })
  |+ field "version" uint8 (fun h -> h.version)
  |+ field "length" uint16 (fun h -> h.length)
  |+ field "flags" uint8 (fun h -> h.flags)
  |> seal

(* Generate 3D schema *)
let simple_header_struct = Codec.to_struct simple_header_codec

let simple_header_module =
  module_ ~doc:"Simple header for differential testing" "SimpleHeader"
    [ typedef ~entrypoint:true simple_header_struct ]

(* Constrained schema - constraints are applied in 3D generation,
   the OCaml parser doesn't validate constraints on individual fields.
   For differential testing, we validate manually or use the C parser. *)
type constrained_packet = { pkt_type : int; pkt_length : int }

let constrained_packet_codec =
  let open Codec in
  record "ConstrainedPacket" (fun pkt_type pkt_length ->
      { pkt_type; pkt_length })
  |+ field "pkt_type" uint8 (fun p -> p.pkt_type)
  |+ field "pkt_length" uint16 (fun p -> p.pkt_length)
  |> seal

let constrained_packet_module =
  module_ ~doc:"Constrained packet for differential testing" "ConstrainedPacket"
    [ typedef ~entrypoint:true (Codec.to_struct constrained_packet_codec) ]
