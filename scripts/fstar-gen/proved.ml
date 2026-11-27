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

(* A variable-size field between two fields: the second is read at a
   position the first gives. *)
let span =
  let len = Field.v "len" uint8 in
  Codec.v "Span"
    (fun len _ x -> (len, x))
    Codec.
      [
        (len $ fun (len, _) -> len);
        ( Field.v "data" (byte_slice ~size:(Field.ref len)) $ fun _ ->
          Bytesrw.Bytes.Slice.eod );
        (Field.v "x" uint8 $ fun (_, x) -> x);
      ]

(* A codec of a variable size, which 3D reads an optional field of as a
   casetype. *)
let inner =
  let a = Field.v "a" uint8 in
  Codec.v "Inner"
    (fun a _ -> a)
    Codec.
      [
        (a $ fun a -> a);
        ( Field.v "b" (byte_slice ~size:(Field.ref a)) $ fun _ ->
          Bytesrw.Bytes.Slice.eod );
      ]

let tag = Field.v "tag" uint8

(* One optional field, present on its tag. *)
let gate =
  Codec.v "Gate"
    (fun tag o x -> (tag, o, x))
    Codec.
      [
        (tag $ fun (tag, _, _) -> tag);
        ( Field.optional "o" ~present:Expr.(Field.ref tag = int 1) (codec inner)
        $ fun (_, o, _) -> o );
        (Field.v "x" uint8 $ fun (_, _, x) -> x);
      ]

(* Two optional fields on one tag, which the scanner dispatches on. *)
let dispatch =
  Codec.v "Dispatch"
    (fun tag o p x -> (tag, o, p, x))
    Codec.
      [
        (tag $ fun (tag, _, _, _) -> tag);
        ( Field.optional "o" ~present:Expr.(Field.ref tag = int 1) (codec inner)
        $ fun (_, o, _, _) -> o );
        ( Field.optional "p" ~present:Expr.(Field.ref tag = int 2) (codec inner)
        $ fun (_, _, p, _) -> p );
        (Field.v "x" uint8 $ fun (_, _, _, x) -> x);
      ]

(* An optional codec in a region of at most [len] bytes. *)
let region =
  let len = Field.v "len" uint8 in
  Codec.v "Region"
    (fun tag len o x -> (tag, len, o, x))
    Codec.
      [
        (tag $ fun (tag, _, _, _) -> tag);
        (len $ fun (_, len, _, _) -> len);
        ( Field.optional "o"
            ~present:Expr.(Field.ref tag = int 1)
            (nested_at_most ~size:(Field.ref len) (codec inner))
        $ fun (_, _, o, _) -> o );
        (Field.v "x" uint8 $ fun (_, _, _, x) -> x);
      ]

let all =
  [
    Codec ("Tiny", tiny);
    Codec ("Ethernet", Headers.eth);
    Codec ("IPv4", Headers.ipv4);
    Codec ("Clcw", Space.clcw_codec);
    Codec ("Span", span);
    Codec ("Gate", gate);
    Codec ("Dispatch", dispatch);
    Codec ("Region", region);
  ]
