(lang dune 3.15)

(name lua_interpreter)

(generate_opam_files true)

(source
 (github Rellikeht/lua_interpreter))

(authors "Rellikeht")

(maintainers "Rellikeht")

(license MIT)

; (documentation https://url/to/documentation)

(using menhir 3.0)

(package
 (name lua_interpreter)
 (synopsis "Simple lua interpreter")
 (description "Simple lua interpreter")
 ; (depends ocaml dune menhir base stdio utop ppx_deriving)
 (depends ocaml dune menhir base stdio utop)
 (tags
  (topics lua interpreter)))
