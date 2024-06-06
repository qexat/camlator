(lang dune 3.15)

(name camlator)

(generate_opam_files true)

(source
 (github qexat/camlator))

(authors "Qexat")

(maintainers "Qexat")

(license LICENSE)

(documentation https://qexat.github.io/camlator)

(package
 (name camlator)
 (synopsis "An extremely simple calculator in OCaml")
 (description
  "A parser and tree-walk interpreter for basic math expressions, written in OCaml.")
 (depends ocaml dune ppx_deriving))
