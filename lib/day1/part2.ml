open Shared.Util
open Shared.Solver
open Common

let get_num_zeros src direction =
  let steps_to_first_zero =
    match direction with
    | Left _ -> src % 100
    | Right _ -> -src % 100
  in
  let num_steps =
    match direction with
    | Left num_steps | Right num_steps -> num_steps
  in
  let num_steps_taken = num_steps - steps_to_first_zero in
  if num_steps_taken < 0
  then 0
  else (
    let is_started_at_zero = src == 0 in
    if is_started_at_zero then num_steps_taken / 100 else 1 + (num_steps_taken / 100))
;;

let apply_direction acc cur =
  let end_offset =
    match cur with
    | Left dx -> (acc.offset - dx) % 100
    | Right dx -> (acc.offset + dx) % 100
  in
  let num_newzeros = get_num_zeros acc.offset cur in
  { offset = end_offset; numzeros = acc.numzeros + num_newzeros }
;;

let solver input =
  input
  |> Io.split_string_into_lines
  |> List.filter_map direction_of_string
  |> List.fold_left apply_direction initial_state
  |> fun acc -> acc.numzeros |> string_of_int
;;

let solution = { label = "Day 1, Part 2"; inputfile = "./input/day1.txt"; solver }
