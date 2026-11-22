A checked view of a codec: [Codec.check] answers without raising, a proof
comes only from a check that passed, and the types keep a proof to the
buffer and the view it was made for.

  $ cat > dune-project << 'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat > dune << 'EOF'
  > (executable (name probe) (libraries wire wire.runtime))
  > EOF

  $ cat > headers.ml << 'EOF'
  > open Wire
  > type ip = { version : int; ttl : UInt8.t }
  > let version = Field.v "version" ~self_constraint:Expr.(fun v -> v = int 4)
  >   (bits ~width:4 U8)
  > let ttl = Field.v "ttl" uint8
  > let ip_ttl = Codec.(ttl $ fun h -> h.ttl)
  > let ip =
  >   Codec.v "Ip"
  >     (fun version ttl -> { version; ttl })
  >     Codec.[ (version $ fun h -> h.version); ip_ttl ]
  > let lookalike_ttl = Codec.(Field.v "ttl" uint8 $ fun h -> h.ttl)
  > EOF

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module Ip = (val Codec.view ip)
  > let ttl = Codec.reader Ip.view ip_ttl
  > let pr = Printf.printf
  > let answer name buf off =
  >   match Wire_runtime.Buf.v (Bytes.of_string buf) with
  >   | Wire_runtime.Buf.Any b -> (
  >       let k = Codec.check Ip.view b (Wire_runtime.Buf.length b) off in
  >       if Codec.ok k then pr "%s: ttl %d\n" name (UInt8.to_int (Codec.read ttl b (Codec.valid k)))
  >       else
  >         match Codec.valid k with
  >         | _ -> pr "%s: a proof of a refused record\n" name
  >         | exception Invalid_argument _ -> pr "%s: refused, valid raises\n" name)
  > let () =
  >   answer "good" "\x40\x07" 0;
  >   answer "bad version" "\x50\x07" 0;
  >   answer "short buffer" "\x40" 0;
  >   answer "off = -1" "\x40\x07" (-1);
  >   answer "off past the end" "\x40\x07" 3;
  >   match Codec.reader Ip.view lookalike_ttl with
  >   | _ -> pr "a lookalike field read\n"
  >   | exception Invalid_argument _ -> pr "a lookalike field refused\n"
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  good: ttl 7
  bad version: refused, valid raises
  short buffer: refused, valid raises
  off = -1: refused, valid raises
  off past the end: refused, valid raises
  a lookalike field refused

A reader of one view takes no proof of another, even of the same codec:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module Ip = (val Codec.view ip)
  > module Ip2 = (val Codec.view ip)
  > let ttl = Codec.reader Ip.view ip_ttl
  > let read b len =
  >   let k = Codec.check Ip2.view b len 0 in
  >   Codec.read ttl b (Codec.valid k)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'This expression has type'
  1
  $ dune build probe.exe 2>&1 | grep -oE 'Ip2?\.c' | sort -u
  Ip.c
  Ip2.c

A proof made on one buffer reads no other:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module Ip = (val Codec.view ip)
  > let ttl = Codec.reader Ip.view ip_ttl
  > let read (type b1 b2) (b1 : b1 Wire_runtime.Buf.t)
  >     (len1 : b1 Wire_runtime.Buf.length) (b2 : b2 Wire_runtime.Buf.t) =
  >   let k = Codec.check Ip.view b1 len1 0 in
  >   (Codec.read ttl b2 (Codec.valid k) : UInt8.t)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'This expression has type'
  1
