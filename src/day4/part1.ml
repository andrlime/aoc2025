open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = GridSquare.t array array

  let label = "Day 4, Part 1"
  let input = "./input/day4/full.txt"

  let get_neighbors_count grid tile =
    let r, c = tile.row, tile.col in
    Neighbors.neighbors_2d
    |> List.filter_map (fun (dr, dc) ->
      try Some grid.(r + dr).(c + dc) with
      | _ -> None)
    |> List.filter_map (fun n -> if is_paper n.square then Some 1 else None)
    |> ListUtil.intsum
  ;;

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> t_list_of_rawgrid
    |> List.map Array.of_list
    |> Array.of_list
  ;;

  let solver input =
    let removed = ref [] in
    input
    |> ArrayUtil.iter2d (fun tile ->
      match tile.square with
      | PaperRoll ->
        let nc = get_neighbors_count input tile in
        if nc >= 4 then () else removed := nc :: !removed
      | Empty -> ());
    List.length !removed |> string_of_int
  ;;
end

include Make (M)
