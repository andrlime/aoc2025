open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = Solver.t

  let label = "Day 9, Part 1"
  let input = "./input/day9/full.txt"
  let parse_input input = input |> Solver.t_of_input_string |> Solver.sort
  let solver input = input |> Solver.get_maximum_area |> string_of_int
end

include Make (M)
