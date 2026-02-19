(lang dune 3.21)
(using directory-targets 0.1)

(name d3t)

(generate_opam_files true)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (tangled gazagnaire.org/ocaml-d3t))

(package
 (name d3t)
 (synopsis "Dependent Data Descriptions for binary wire formats")
 (description
  "GADT-based DSL for describing binary wire formats with EverParse 3D output. \
   Define your wire format once, then use it for OCaml parsing via bytesrw or \
   emit .3d files for verified C parser generation via EverParse.")
 (depends
  (ocaml (>= 5.1))
  (bytesrw (>= 0.1))
  (fmt (>= 0.9))
  (alcotest :with-test)
  (odoc :with-doc)))
