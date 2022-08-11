(lang dune 3.0)

(name nqcc)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name nqcc)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  batteries
  bisect_ppx
  ppx_deriving
  ppx_inline_test
  cmdliner
  camlp-streams)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
