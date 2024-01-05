(lang dune 3.10)

(name scalpel_lang)

(using menhir 2.0)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Wenke")

(maintainers "Wenke")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name scalpel_lang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
