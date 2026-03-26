(lang dune 3.21)
(using directory-targets 0.1)

(name wire)

(generate_opam_files true)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (tangled gazagnaire.org/ocaml-wire))

(package
 (name wire)
 (synopsis "Binary wire format DSL with EverParse 3D output")
 (description
  "OCaml DSL for describing binary wire formats with EverParse 3D output. \
   Define your wire format once, then use it for OCaml parsing via bytesrw or \
   emit .3d files for verified C parser generation via EverParse.")
 (depends
  (ocaml (>= 5.1))
  (bytesrw (>= 0.1))
  (eio (>= 1.0))
  (fmt (>= 0.9))
  (memtrace :with-test)
  (alcotest :with-test)
  (alcobar :with-test)
  (re :with-test)
  (odoc :with-doc)))
