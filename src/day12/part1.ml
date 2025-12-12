open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = FillShapesSolver.t

  let label = "Day 12, Part 1"
  let input = "./input/day12/full.txt"
  let parse_input = FillShapesSolver.t_of_input
  let solver input = input |> FillShapesSolver.count_solvable_puzzles |> string_of_int
end

include Make (M)
