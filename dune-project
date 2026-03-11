(lang dune 3.20)

(name eprol)

(generate_opam_files true)

(source
 (github de-alchmst/eprol))

(authors "Đe-Alchmst <PolakJ_37@proton.me>")

(maintainers "Đe-Alchmst <PolakJ_37@proton.me>")

(license "BSD 3-Clause")

(documentation https://url/to/documentation)

(package
 (name eprol)
 (allow_empty)
 (synopsis "The EPROL programming language")
 (description "The EPROL programming language")
 (depends ocaml menhirLib)
 (tags
  ("EPROL" "programming language" "WASM")))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
