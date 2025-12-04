open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = RawGrid.t

  let label = "Day 4, Part 1"
  let input = "./input/day4/full.txt"

  let get_neighbors_count grid tile =
    let r, c = tile.row, tile.col in
    Neighbors.neighbors_2d
    |> List.filter_map (fun (dr, dc) ->
      let rprime, cprime = r + dr, c + dc in
      try Some (List.nth (List.nth grid rprime) cprime) with
      | _ -> None)
    |> List.filter_map (fun n -> if RawGrid.is_paper n.square then Some 1 else None)
    |> ListUtil.intsum
  ;;

  let parse_input input =
    input |> String.split_on_char '\n' |> ListUtil.remove_lines_shorter_than 1
  ;;

  let solver input =
    let grid = input |> t_list_of_rawgrid in
    grid
    |> List.map
         (List.filter_map (fun tile ->
            match tile.square with
            | RawGrid.PaperRoll -> Some (get_neighbors_count grid tile)
            | RawGrid.Empty -> None))
    |> List.flatten
    |> List.filter_map (fun i -> if i >= 4 then None else Some i)
    |> List.length
    |> string_of_int
  ;;
end

include Make (M)
