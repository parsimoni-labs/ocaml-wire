(lang dune 3.21)
(implicit_transitive_deps false)
(using directory-targets 0.1)
(using mdx 0.4)
(cram enable)

(name wire)

(generate_opam_files true)

(license ISC)
(authors "Thomas Gazagnaire <thomas@gazagnaire.org>")
(maintainers "Thomas Gazagnaire <thomas@gazagnaire.org>")

(source (github parsimoni-labs/ocaml-wire))

(package
 (name wire)
 (synopsis "One description of a binary format for OCaml, Rust and verified C")
 (description
  "Wire describes a binary format once, as an OCaml value, and derives from \
   that single description everything that reads and writes the bytes: an \
   interpreted OCaml codec that decodes, encodes and reads fields in place \
   without copying; standalone OCaml or OxCaml modules (wire.ocaml) and \
   dependency-free no_std Rust (wire.rust) that do the same work with every \
   offset a constant; and the EverParse 3D schema that compiles to a verified \
   C validator (wire.3d), with OCaml FFI stubs to call it (wire.stubs).")
 (depends
  (ocaml (>= 5.3))
  (bytesrw (>= 0.4.0))
  (fmt (>= 0.9))
  (optint (>= 0.3))
  (memtrace :with-test)
  (alcotest :with-test)
  (alcobar :with-test)
  (re (>= 1.11))
  (mdx :with-test)
  (odoc :with-doc)))
