A checked view of a codec: [Codec.View.check] answers without raising, a
view comes only from a check that passed, and the types keep a view to the
buffer and the codec it was checked for. The buffers are named by
[Wire.Buf], so the probe links [wire] alone.

  $ cat > dune-project << 'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat > dune << 'EOF'
  > (executable (name probe) (libraries wire))
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
  > module V = Codec.View
  > module Ip = (val V.codec ip)
  > let ttl = V.field Ip.codec ip_ttl
  > let pr = Printf.printf
  > let answer name buf off =
  >   let (Buf.Any b) = Buf.v (Bytes.of_string buf) in
  >   pr "%s: buffer of %d bytes\n" name (Buf.length b :> int);
  >   let k = V.check Ip.codec b (Buf.length b) off in
  >   if V.ok k then pr "%s: ttl %d\n" name (UInt8.to_int (V.read ttl b (V.get k)))
  >   else
  >     match V.get k with
  >     | _ -> pr "%s: a view of a refused record\n" name
  >     | exception Invalid_argument _ -> pr "%s: refused, get raises\n" name
  > let () =
  >   answer "good" "\x40\x07" 0;
  >   answer "bad version" "\x50\x07" 0;
  >   answer "short buffer" "\x40" 0;
  >   answer "off = -1" "\x40\x07" (-1);
  >   answer "off past the end" "\x40\x07" 3;
  >   match V.field Ip.codec lookalike_ttl with
  >   | _ -> pr "a lookalike field read\n"
  >   | exception Invalid_argument _ -> pr "a lookalike field refused\n"
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  good: buffer of 2 bytes
  good: ttl 7
  bad version: buffer of 2 bytes
  bad version: refused, get raises
  short buffer: buffer of 1 bytes
  short buffer: refused, get raises
  off = -1: buffer of 2 bytes
  off = -1: refused, get raises
  off past the end: buffer of 2 bytes
  off past the end: refused, get raises
  a lookalike field refused

A field of one named codec reads no view of another, even of the same
codec:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > module Ip = (val V.codec ip)
  > module Ip2 = (val V.codec ip)
  > let ttl = V.field Ip.codec ip_ttl
  > let read b len =
  >   let k = V.check Ip2.codec b len 0 in
  >   V.read ttl b (V.get k)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'This expression has type'
  1
  $ dune build probe.exe 2>&1 | grep -oE 'Ip2?\.c' | sort -u
  Ip.c
  Ip2.c

A view checked in one buffer reads no other:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > module Ip = (val V.codec ip)
  > let ttl = V.field Ip.codec ip_ttl
  > let read (type b1 b2) (b1 : b1 Buf.t) (len1 : b1 Buf.length) (b2 : b2 Buf.t) =
  >   let k = V.check Ip.codec b1 len1 0 in
  >   (V.read ttl b2 (V.get k) : UInt8.t)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'This expression has type'
  1

A check result is not an [int], so there is no second way to [ok]:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > module Ip = (val V.codec ip)
  > let accepted b len = (V.check Ip.codec b len 0 :> int) >= 0
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'Error'
  1

In a function polymorphic in the record type, the unpacking names the type:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let accepts (type r) (c : r Codec.t) bytes =
  >   let module C = (val Codec.View.codec c) in
  >   let (Buf.Any b) = Buf.v bytes in
  >   Codec.View.(ok (check C.codec b (Buf.length b) 0))
  > let () = Printf.printf "%b\n" (accepts Headers.ip (Bytes.of_string "\x40\x07"))
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  true
