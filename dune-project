(lang dune 3.11)

(name monkeylang)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name monkeylang)
 (synopsis "Monkeylang")
 (description "Monkeylang, from Mr.Ball's book")
 (depends ocaml dune base)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
