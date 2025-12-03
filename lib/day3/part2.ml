open! Shared.Util
open! Shared.Solver
open! Common

let get_first_largest_digit_and_index ~i0 ~i1 bank =
  ListUtil.range i0 i1
  |> List.fold_left
       (fun acc curindex ->
          let curnum = List.nth bank curindex in
          if curnum > acc.largestdigit
          then { largestdigit = curnum; largestindex = curindex }
          else acc)
       initial_state
;;

let rec get_joltage_from_bank startindex numdigits bank =
  if numdigits = 0
  then []
  else (
    let banklength = List.length bank in
    let endindex = banklength - numdigits in
    let { largestdigit; largestindex } =
      get_first_largest_digit_and_index ~i0:startindex ~i1:endindex bank
    in
    largestdigit :: get_joltage_from_bank (largestindex + 1) (numdigits - 1) bank)
;;

let int_of_bank bank = bank |> List.fold_left (fun acc cur -> (acc * 10) + cur) 0

let solver input =
  input
  |> String.split_on_char '\n'
  |> List.filter_map bank_of_string
  |> List.map (get_joltage_from_bank 0 12)
  |> List.map int_of_bank
  |> ListUtil.intsum
  |> string_of_int
;;

let solution = { label = "Day 3, Part 2"; inputfile = "./input/day3/full.txt"; solver }
