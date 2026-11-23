open Wire

type codec = Codec : string * 'a Codec.t -> codec

let tiny =
  Codec.v "Tiny"
    (fun a b -> (a, b))
    Codec.
      [
        (Field.v "a" uint8 $ fun (a, _) -> a);
        ( Field.v "b" uint16be ~self_constraint:(fun self ->
              Expr.(self >= int 4))
        $ fun (_, b) -> b );
      ]

let all =
  [
    Codec ("Tiny", tiny);
    Codec ("Ethernet", Headers.eth);
    Codec ("IPv4", Headers.ipv4);
    Codec ("Clcw", Space.clcw_codec);
  ]
