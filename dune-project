(lang dune 3.9)
(name eio)
(generate_opam_files true)
(source (github ocaml-multicore/eio_js))
(license ISC)
(authors "Jérôme Vouillon")
(maintainers "jerome@tarides.com")
(documentation "https://ocaml-multicore.github.io/eio_js/")
(package
 (name eio_js_backend)
 (synopsis "Simple Eio scheduler for JavaScript environments")
 (description "An Eio scheduler suitable for JavaScript environments.")
 (depends
  (mdx (and (>= 2.4.1) :with-test))
  (eio (>= 0.14))))
(package
 (name eio_brr)
 (synopsis "Eio support for Brr")
 (description "This package provides Eio variants of Brr functions, which returns directly instead of returning a future.")
 (depends
  (eio_js_backend (= :version))
  (brr (>= 0.0.4))))
(package
 (name js_of_ocaml-eio)
 (synopsis "Eio-based JavaScript bindings")
 (description "An Eio counterpart to package js_of_ocaml-lwt.")
 (depends
  (eio_js_backend (= :version))
  (js_of_ocaml-ppx (>= "5.0.1"))
  (js_of_ocaml (>= 5.0.1))))
(using mdx 0.2)
