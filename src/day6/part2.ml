open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = NumberColumn.t list

  let label = "Day 6, Part 2"
  let input = "./input/day6/full.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> CephalopodRow.t_of_string_list
    |> NumberColumn.t_list_of_cephalopod_rows
  ;;

  let solver input =
    input |> List.map NumberColumn.solve |> ListUtil.intsum |> string_of_int
  ;;
end

include Make (M)
