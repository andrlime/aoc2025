open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = SimpleSolver.t

  let label = "Day 9, Part 1"
  let input = "./input/day9/full.txt"
  let parse_input input = input |> SimpleSolver.t_of_input_string |> SimpleSolver.sort
  let solver input = input |> SimpleSolver.get_maximum_area |> string_of_int
end

include Make (M)
