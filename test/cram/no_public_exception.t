No decoder raises on bad input, so the exception the interpreter and the
generated code stop at internally is no name a program can catch: bad
input is an [Error] value. The runtime hands out its raisers,
[Errors.is_error] and [Errors.error_of] for the code [Wire_ocaml] writes,
not the exception.

  $ cat > dune-project << 'EOF'
  > (lang dune 3.0)
  > EOF

  $ cat > dune << 'EOF'
  > (executable (name probe) (libraries wire wire.runtime))
  > EOF

The runtime's [Errors] has no [Parse_error]:

  $ cat > probe.ml << 'EOF'
  > let f g = try g () with Wire_runtime.Errors.Parse_error e -> ignore e
  > EOF
  $ dune build probe.exe 2>&1 | grep 'Unbound constructor' | tr -d '"'
  Error: Unbound constructor Wire_runtime.Errors.Parse_error

Nor has [Wire.Private.Types]:

  $ cat > probe.ml << 'EOF'
  > let f g = try g () with Wire.Private.Types.Parse_error e -> ignore e
  > EOF
  $ dune build probe.exe 2>&1 | grep 'Unbound constructor' | tr -d '"'
  Error: Unbound constructor Wire.Private.Types.Parse_error
