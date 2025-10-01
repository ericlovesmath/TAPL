(lang dune 3.19)

(name TAPL)

(generate_opam_files true)

(source
 (github ericlovesmath/TAPL))

(authors "Eric Lee <me@ericchanlee.com>")

(maintainers "Eric Lee <me@ericchanlee.com>")

(documentation https://url/to/documentation)

(package
 (name TAPL)
 (synopsis "Implementation of Types and Programming Languages")
 (description "Implementation of Types and Programming Languages, following the book by Benjamin Pierce")
 (depends ocaml)
 (tags
  ("ocaml" "compiler" "types" "programming languages")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
