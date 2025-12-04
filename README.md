# Advent of Code 2025
I did these in OCaml. There is a functor that takes a single solution and creates a solver for it. The solver times and prints the solution. Each solution is registered in `registry.ml` and called via command line arguments. Expect tests exist for the solutions, but require setting the working directory of the project using an environment variable to get past the dune sandbox. I'm also not sure why the test files run in Z-A order, but the design of `ppx_expect` is not my job!

The design of this repository was _very loosely_ inspired by the repository, `fangyi-zhou/advent-of-code-ocaml-starter`. By that, I mean I saw that they used a functor and decided to as well, at which point I did some refactoring.
