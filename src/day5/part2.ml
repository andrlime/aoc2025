open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = u

  let label = "Day 5, Part 2"
  let input = "./input/day5/full.txt"
  let parse_input input = u_of_input input

  let solver (_, ranges) =
    ranges
    |> IngredientIdRange.sort
    |> IngredientIdRange.merge
    |> List.map IngredientIdRange.get_range_length
    |> ListUtil.intsum
    |> string_of_int
  ;;
end

include Make (M)
