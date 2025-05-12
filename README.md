# Type Inference

This is my implementation of Hindley-Milner type inference, based on Damas and Milner's 1982 paper, "Principal type-schemes for functional programs." Inspired by the fact that the algorithm was originally developed for ML, I decided to implement the algorithm in OCaml. Implementing Algorithm W was mostly a 1-to-1 translation from the paper to OCaml, after implementing all the types and utility functions.

## Unification

The implementation of the unification algorithm is based on page 32 of Robinson's 1965 paper, "[A Machine-Orlented Logic Based on the Resolution Principle](https://doi.org/10.1145/321250.321253)." It is not the most efficient algorithm, but it is the same one referenced by Damas and Milner, and is fairly straight-forward to implement.

## Building

### Prerequisites

- Install [opam](https://opam.ocaml.org/doc/Install.html).
- Initialize opam: `opam init`
- Install [dune](https://dune.readthedocs.io/en/stable/howto/install-dune.html): `opam install dune`

### Running

Build and run the project with:

```sh
dune build
dune exec bin/main.exe
```

The main executable infers the types of a few small programs, including one requiring polymorphism in order to be well-typed.
