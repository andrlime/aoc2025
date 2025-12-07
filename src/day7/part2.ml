open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = Parser.u

  let label = "Day 7, Part 1"
  let input = "./input/day7/full.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> Parser.u_of_line_list
  ;;

  let solver input = TachyonDpSolver.solve_dp input |> string_of_int
end

include Make (M)
