open Shared.Util
open Shared.Solver
open Common

let apply_direction acc cur =
  let current_offset = acc.offset in
  let cur_number_of_zeros = acc.numzeros in
  let new_offset =
    match cur with
    | Left dx -> (current_offset - dx) % 100
    | Right dx -> (current_offset + dx) % 100
  in
  let new_number_of_zeros = if new_offset == 0 then 1 else 0 in
  { offset = new_offset; numzeros = cur_number_of_zeros + new_number_of_zeros }
;;

let solver input =
  input
  |> Io.split_string_into_lines
  |> List.filter_map direction_of_string
  |> List.fold_left apply_direction initial_state
  |> fun acc -> acc.numzeros |> string_of_int
;;

let solution = { label = "Day 1, Part 1"; inputfile = "./input/day1.txt"; solver }
