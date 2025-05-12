(lang dune 3.18)

(name type_inference)

(generate_opam_files true)

(source
 (github caden-parajulic/type-inference))

(authors "Caden Parajuli <caden.parajuli@gmail.com>")
(maintainers "Caden Parajuli <caden.parajuli@gmail.com>")

(license LICENSE)

; (documentation https://github.com/caden-parajuli/type-inference)

(package
 (name type_inference)
 (synopsis "An implementation of Hindley-Milner type inference")
 (description "An implementation of Hindley-Milner type inference based on Damas and Milner's 1982 paper, \"Principal type-schemes for functional programs\"")
 (depends ocaml))
