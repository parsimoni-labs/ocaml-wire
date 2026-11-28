A checked view of a codec: [Codec.View.check] answers without raising, a
view comes only from a check that passed, and the types keep a view to the
buffer it was checked in and to its record type. The buffers are named by
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
  > let ttl = V.field ip ip_ttl
  > let pr = Printf.printf
  > let answer name buf off =
  >   let (Buf.Any b) = Buf.v (Bytes.of_string buf) in
  >   let k = V.check ip b ~off in
  >   if V.is_ok k then pr "%s: ttl %d\n" name (UInt8.to_int (V.read b (V.get k) ttl))
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
  >   match V.field ip lookalike_ttl with
  >   | _ -> pr "a lookalike field read\n"
  >   | exception Invalid_argument _ -> pr "a lookalike field refused\n"
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  good: ttl 7
  bad version: refused, get raises
  short buffer: refused, get raises
  off = -1: refused, get raises
  off past the end: refused, get raises
  a lookalike field refused

A field and a view are indexed by their record type, so a field of another
codec of the same record type is accepted with a view: its read stays in the
buffer and answers a wrong value, here the zero of a field that lies past
the end.

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > let padded_ttl = Codec.(Field.v "ttl" uint8 $ fun h -> h.ttl)
  > let padded =
  >   Codec.v "Padded"
  >     (fun _ ttl -> { version = 4; ttl })
  >     Codec.[ (Field.v "pad" (byte_array ~size:(int 4)) $ fun _ -> "\000\000\000\000"); padded_ttl ]
  > let ttl = V.field padded padded_ttl
  > let () =
  >   let (Buf.Any b) = Buf.v (Bytes.of_string "\x40\x07") in
  >   let k = V.check ip b ~off:0 in
  >   Printf.printf "ttl %d\n" (UInt8.to_int (V.read b (V.get k) ttl))
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  ttl 0

A view checked in one buffer reads no other:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > let ttl = V.field ip ip_ttl
  > let read (type b1 b2) (b1 : b1 Buf.t) (b2 : b2 Buf.t) =
  >   let k = V.check ip b1 ~off:0 in
  >   (V.read b2 (V.get k) ttl : UInt8.t)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'This expression has type'
  1

A check result is not an [int], so there is no second way to [is_ok]:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Headers
  > module V = Codec.View
  > let accepted b = (V.check ip b ~off:0 :> int) >= 0
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'Error'
  1

A path names a field under a layer, and only a layer holds a record. Its
type is an option however deep the path:

  $ cat > layers.ml << 'EOF'
  > open Wire
  > type inner = { x : UInt8.t }
  > type mid = { gate : UInt8.t; inner : inner option }
  > type outer = { tag : UInt8.t; mid : mid option }
  > let x = Codec.(Field.v "x" uint8 $ fun i -> i.x)
  > let inner_c = Codec.v "Inner" (fun x -> { x }) Codec.[ x ]
  > let f_gate = Field.v "gate" uint8
  > let gate = Codec.(f_gate $ fun m -> m.gate)
  > let inner =
  >   Codec.(Field.optional "inner" ~present:Expr.(Field.ref f_gate <> int 0)
  >     (codec inner_c) $ fun m -> m.inner)
  > let mid_c = Codec.v "Mid" (fun gate inner -> { gate; inner }) Codec.[ gate; inner ]
  > let f_tag = Field.v "tag" uint8
  > let tag = Codec.(f_tag $ fun o -> o.tag)
  > let mid =
  >   Codec.(Field.optional "mid" ~present:Expr.(Field.ref f_tag <> int 0)
  >     (codec mid_c) $ fun o -> o.mid)
  > let outer_c = Codec.v "Outer" (fun tag mid -> { tag; mid }) Codec.[ tag; mid ]
  > EOF
  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Layers
  > let path : (UInt8.t option, outer) Codec.field =
  >   Codec.(opt_layer mid // opt_layer inner / x)
  > let () =
  >   let count buf =
  >     Codec.View.fold outer_c Codec.[ path ] (Bytes.of_string buf) ~init:[]
  >       ~f:(fun acc x -> x :: acc)
  >   in
  >   List.iter
  >     (fun buf ->
  >       match count buf with
  >       | Ok [ Some x ] -> Printf.printf "x %d\n" (UInt8.to_int x)
  >       | Ok [ None ] -> print_endline "absent"
  >       | Ok _ -> print_endline "?"
  >       | Error (e, _) -> Format.printf "%a@." Error.pp e)
  >     [ "\x01\x01\x2a"; "\x01\x00"; "\x00" ]
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  x 42
  absent
  absent

A path through a field that holds no record is a type error:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Layers
  > let bad = Codec.(layer tag / x)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'Error:'
  1

and so is a path through a layer of another record:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Layers
  > let bad = Codec.(opt_layer mid / tag)
  > EOF
  $ dune build probe.exe 2>&1 | grep -c 'Error:'
  1

A layer of a field that holds no record, and a path in the fields of a
codec, are refused where they are built:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > open Layers
  > let () =
  >   (match Codec.layer tag with
  >   | _ -> print_endline "a layer of a byte"
  >   | exception Invalid_argument _ -> print_endline "a layer of a byte refused");
  >   match
  >     Codec.v "Bad"
  >       (fun _ -> { tag = UInt8.zero; mid = None })
  >       Codec.[ opt_layer mid // opt_layer inner / x ]
  >   with
  >   | _ -> print_endline "a codec of a path"
  >   | exception Invalid_argument _ -> print_endline "a codec of a path refused"
  > EOF
  $ dune build probe.exe 2>&1
  $ ./_build/default/probe.exe
  a layer of a byte refused
  a codec of a path refused
