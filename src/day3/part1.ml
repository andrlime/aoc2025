open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = bank list

  let label = "Day 3, Part 1"
  let input = "./input/day3/full.txt"

  let rec get_pair_from_battery_bank curmax curmaxfirst bank =
    match bank with
    | [] | _ :: [] -> curmax
    | first :: rest ->
      if first < curmaxfirst
      then get_pair_from_battery_bank curmax curmaxfirst rest
      else (
        let candidates = rest |> List.map (fun n2 -> (first * 10) + n2) in
        let newmax = ListUtil.intmax candidates in
        get_pair_from_battery_bank (max curmax newmax) first rest)
  ;;

  let parse_input input =
    input |> Io.split_string_into_lines |> List.filter_map bank_of_string
  ;;

  let solver input =
    input
    |> List.map (get_pair_from_battery_bank 0 0)
    |> List.fold_left (fun acc cur -> acc + cur) 0
    |> string_of_int
  ;;
end

include Make (M)
