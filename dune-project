(lang dune 3.0)
(name mirage-block-unikraft)
(generate_opam_files)

(source (github mirage/mirage-block-unikraft))
(license BSD-3)
(authors "Fabrice Buoro <fabrice@tarides.com>" "Samuel Hym <samuel@tarides.com>")
(maintainers "team@mirage.org")

(package
 (name mirage-block-unikraft)
 (synopsis "Unikraft implementation of MirageOS block interface")
 (description "This library implements the MirageOS block interface for Unikraft targets.")
 (tags ("org:mirage"))
 (depends
  (ocaml (>= "5.2.0"))
  (dune  (>= "3.0"))
  (lwt (>= "2.4.3"))
  (cstruct (>= "6.0.0"))
  (mirage-block (>= "2.0.0"))
  (mirage-unikraft (>= "1.0.0"))
  (fmt (>= "0.8.7"))))
