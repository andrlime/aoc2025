open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = PathCounterSolver.t

  let label = "Day 11, Part 1"
  let input = "./input/day11/full.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> PathCounterSolver.create_graph
  ;;

  let solver input =
    PathCounterSolver.dfs_graph input "you" "out";
    input.counter |> string_of_int
  ;;
end

include Make (M)
