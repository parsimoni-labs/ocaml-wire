(** Schema definitions for differential testing.

    These schemas are used to test that our OCaml parser produces the same
    results as the EverParse-generated C parser. *)

open Wire
open Wire.Everparse.Raw

(* Simple header schema: version (u8) + length (u16) + flags (u8) *)
type simple_header = { version : int; length : int; flags : int }

let simple_header_codec =
  Codec.v "SimpleHeader"
    (fun version length flags -> { version; length; flags })
    Codec.
      [
        (Field.v "version" uint8 $ fun h -> h.version);
        (Field.v "length" uint16 $ fun h -> h.length);
        (Field.v "flags" uint8 $ fun h -> h.flags);
      ]

(* Generate 3D schema *)
let simple_header_struct = Everparse.struct_of_codec simple_header_codec

let simple_header_module =
  module_ ~doc:"Simple header for differential testing"
    [ typedef ~entrypoint:true simple_header_struct ]

(* Constrained schema - constraints are applied in 3D generation,
   the OCaml parser doesn't validate constraints on individual fields.
   For differential testing, we validate manually or use the C parser. *)
type constrained_packet = { pkt_type : int; pkt_length : int }

let constrained_packet_codec =
  Codec.v "ConstrainedPacket"
    (fun pkt_type pkt_length -> { pkt_type; pkt_length })
    Codec.
      [
        (Field.v "pkt_type" uint8 $ fun p -> p.pkt_type);
        (Field.v "pkt_length" uint16 $ fun p -> p.pkt_length);
      ]

let constrained_packet_module =
  module_ ~doc:"Constrained packet for differential testing"
    [
      typedef ~entrypoint:true
        (Everparse.struct_of_codec constrained_packet_codec);
    ]
