open Shared.Util
open Shared.Solver
open Common

let is_invalid_id id =
  let firsthalf, secondhalf = string_of_int id |> StringUtil.slice_in_half in
  firsthalf = secondhalf
;;

let solver input =
  input
  |> String.split_on_char ','
  |> List.filter_map range_of_string
  |> List.fold_left
       (fun acc range ->
          { sum = acc.sum + sum_of_invalid_ids_in_range range is_invalid_id })
       initial_state
  |> fun acc -> acc.sum |> string_of_int
;;

let solution = { label = "Day 2, Part 1"; inputfile = "./input/day2/full.txt"; solver }
