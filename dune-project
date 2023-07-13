(lang dune 3.9)

(name absint)

(generate_opam_files true)

(source
 (github etrian-dev/ocaml-absint))

(authors "Nicola Vetrini")

(maintainers "Nicola Vetrini")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name absint)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (alcotest :with-test))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
