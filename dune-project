(lang dune 3.7)

(name nqcc)

(generate_opam_files true)

(map_workspace_root false)

(source
 (github nlsandler/nqcc2))

(authors "Nora Sandler <nora@norasandler.com>")

(maintainers "Nora Sandler <nora@norasandler.com>")

(license MIT)

(package
 (name nqcc)
 (synopsis "Reference implementation for the book 'Writing a C Compiler'")
 (description
  "`nqcc2` is the reference implementation for the book *Writing a C Compiler*. It's a compiler for a subset of C, targeting x86-64 assembly.")
 (depends
  (ocaml
   (>= 4.14.0))
  dune
  bisect_ppx
  ppx_deriving
  ppx_expect
  ppx_inline_test
  cmdliner
  camlp-streams
  re
  zarith
  integers))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
