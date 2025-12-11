open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = ElfMachine.t list

  let label = "Day 10, Part 2"
  let input = "./input/day10/tiny.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> List.map ElfMachine.t_of_string
  ;;

  let solver input =
    input
    |> List.map ElfMachine.get_minimum_solution_binary
    |> ListUtil.intsum
    |> string_of_int
  ;;
end

include Make (M)
