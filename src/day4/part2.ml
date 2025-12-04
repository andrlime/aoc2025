open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = GridSquare.t array array

  let label = "Day 4, Part 2"
  let input = "./input/day4/full.txt"

  let parse_input input =
    input
    |> Io.split_string_into_lines
    |> ListUtil.remove_lines_shorter_than 1
    |> t_list_of_rawgrid
    |> List.map Array.of_list
    |> Array.of_list
  ;;

  let make_removed_set rows =
    let set = Hashtbl.create 31 in
    rows |> List.iter (fun (r, c, _) -> Hashtbl.add set (r, c) ());
    set
  ;;

  let make_new_grid original_grid removed_list =
    let removed_set = make_removed_set removed_list in
    original_grid
    |> ArrayUtil.iter2d (fun tile ->
      if Hashtbl.mem removed_set (tile.row, tile.col) then tile.square <- Empty else ());
    original_grid
  ;;

  let rec recursive_helper acc input =
    let removed = ref [] in
    input
    |> ArrayUtil.iter2d (fun tile ->
      match tile.square with
      | PaperRoll ->
        let nc = get_neighbors_count input tile in
        if nc < 4 then removed := (tile.row, tile.col, nc) :: !removed
      | _ -> ());
    match List.length !removed with
    | 0 -> acc
    | x -> recursive_helper (acc + x) (make_new_grid input !removed)
  ;;

  let solver input = recursive_helper 0 input |> string_of_int
end

include Make (M)
