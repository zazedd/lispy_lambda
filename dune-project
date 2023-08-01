(lang dune 3.7)

(name lispy_lambda)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Francisco Santos" "Leonardo Santos")

(maintainers "Maintainer Name")

(using menhir 2.1)

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name lispy_lambda)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
