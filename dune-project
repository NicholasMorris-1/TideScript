(lang dune 3.6)

(name TideScript)

(using menhir 2.0)

(generate_opam_files true)


(source
 (github username/reponame))

(authors "Nicholas Morris")

(maintainers "Nicholas Morris")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name TideScript)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
