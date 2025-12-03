(lang dune 3.18)

(name advent)

(generate_opam_files true)

(source
 (github andrlime/aoc2025))

(authors "Andrew Li <andrewli@u.northwestern.edu>")

(maintainers "Andrew Li <andrewli@u.northwestern.edu>")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name advent)
 (synopsis "Advent of Code, 2025 in OCaml")
 (description "A longer description")
 (depends ocaml ppx_expect)
 (tags
  ("aoc2025")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
