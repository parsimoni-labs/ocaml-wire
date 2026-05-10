Field decorations -- [optional], [optional_or], [repeat], [repeat_seq]
-- are not exposed at the typ level. Anything that would project to
invalid 3D (nested decoration in [array], [where], [casetype], ...)
is therefore unreachable: the names simply do not exist as functions
returning a typ.

  $ cat > dune-project << 'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat > dune << 'EOF'
  > (executable (name probe) (libraries wire))
  > EOF

[Wire.optional] does not exist:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let _ = array ~len:(int 4) (Wire.optional Expr.true_ uint8)
  > EOF
  $ dune build probe.exe 2>&1
  File "probe.ml", line 2, characters 28-41:
  2 | let _ = array ~len:(int 4) (Wire.optional Expr.true_ uint8)
                                  ^^^^^^^^^^^^^
  Error: Unbound value Wire.optional
  [1]

[Wire.optional_or] does not exist:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let _ = where Expr.true_ (Wire.optional_or Expr.true_ ~default:0 uint8)
  > EOF
  $ dune build probe.exe 2>&1
  File "probe.ml", line 2, characters 26-42:
  2 | let _ = where Expr.true_ (Wire.optional_or Expr.true_ ~default:0 uint8)
                                ^^^^^^^^^^^^^^^^
  Error: Unbound value Wire.optional_or
  [1]

[Wire.repeat] does not exist:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let _ = casetype "Bad" uint8
  >   [ case ~index:0 (Wire.repeat ~size:(int 8) uint8)
  >       ~inject:(fun x -> x) ~project:(fun x -> Some x) ]
  > EOF
  $ dune build probe.exe 2>&1
  File "probe.ml", line 3, characters 19-30:
  3 |   [ case ~index:0 (Wire.repeat ~size:(int 8) uint8)
                         ^^^^^^^^^^^
  Error: Unbound value Wire.repeat
  [1]

[Wire.repeat_seq] does not exist:

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let _ = Wire.repeat_seq seq_list ~size:(int 8) uint8
  > EOF
  $ dune build probe.exe 2>&1
  File "probe.ml", line 2, characters 8-23:
  2 | let _ = Wire.repeat_seq seq_list ~size:(int 8) uint8
              ^^^^^^^^^^^^^^^
  Error: Unbound value Wire.repeat_seq
  [1]

The same operations are exposed at the field level and accept
fixed-size scalars as well as sized payloads (e.g. [byte_array {size}]):

  $ cat > probe.ml << 'EOF'
  > open Wire
  > let f_present = Field.v "present" uint8
  > let _ =
  >   Field.optional "Body"
  >     ~present:Expr.(Field.ref f_present <> int 0)
  >     (byte_array ~size:(int 8))
  > EOF
  $ dune build probe.exe 2>&1
