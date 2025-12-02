open Shared.Util
open Shared.Solver
open Common

let is_invalid_subsequence str chunksize =
  let strlen = String.length str in
  let can_evenly_divide_chunksize = strlen mod chunksize = 0 in
  if not can_evenly_divide_chunksize
  then false
  else (
    let numchunks = strlen / chunksize in
    let chunks =
      ListUtil.range 0 (numchunks - 1)
      |> List.map (fun c -> String.sub str (c * chunksize) chunksize)
    in
    ListUtil.all_elems_in_list_equal chunks)
;;

let is_invalid_id id =
  let idstr = string_of_int id in
  let idstr_length = String.length idstr in
  let range_subsequence_length = ListUtil.range 1 (idstr_length - 1) in
  List.fold_left
    (fun acc len -> acc || is_invalid_subsequence idstr len)
    false
    range_subsequence_length
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

let solution = { label = "Day 2, Part 2"; inputfile = "./input/day2/full.txt"; solver }
