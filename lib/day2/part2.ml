open! Shared.Exception
open! Shared.Util
open! Shared.Solver
open! Common

let get_nth_chunk str chunksize n = String.sub str (n * chunksize) chunksize

let check_chunk_matches str firstchunk chunksize n =
  let chunk = get_nth_chunk str chunksize n in
  if chunk <> firstchunk then raise (Break "chunks don't match")
;;

let is_invalid_subsequence str chunksize =
  let strlen = String.length str in
  let can_evenly_divide_chunksize = strlen mod chunksize = 0 in
  if not can_evenly_divide_chunksize
  then false
  else (
    try
      let firstchunk = get_nth_chunk str chunksize 0 in
      let numchunks = strlen / chunksize in
      ListUtil.range 1 (numchunks - 1)
      |> List.iter (check_chunk_matches str firstchunk chunksize);
      true
    with
    | Break _ -> false)
;;

let is_invalid_id id =
  let idstr = string_of_int id in
  let idstr_length = String.length idstr in
  let chunksize_range = ListUtil.range 1 (idstr_length - 1) in
  List.fold_left
    (fun acc len -> acc || is_invalid_subsequence idstr len)
    false
    chunksize_range
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
