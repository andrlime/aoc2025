open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = GridSquare.t array array

  let label = "Day 4, Part 1"
  let input = "./input/day4/full.txt"

  let parse_input input =
    input
    |> Io.split_string_into_lines
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
        if nc < 4 then removed := nc :: !removed
      | _ -> ());
    List.length !removed |> string_of_int
  ;;
end

include Make (M)
