open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = GridSquare.t list list

  let label = "Day 4, Part 2"
  let input = "./input/day4/full.txt"

  let get_neighbors_count grid tile =
    let r, c = tile.row, tile.col in
    Neighbors.neighbors_2d
    |> List.filter_map (fun (dr, dc) ->
      let rprime, cprime = r + dr, c + dc in
      try Some (List.nth (List.nth grid rprime) cprime) with
      | _ -> None)
    |> List.filter_map (fun n -> if is_paper n.square then Some 1 else None)
    |> ListUtil.intsum
  ;;

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> t_list_of_rawgrid
  ;;

  let make_removed_set rs =
    let set = Hashtbl.create 97 in
    rs |> List.iter (fun (r, c, _) -> Hashtbl.add set (r, c) ());
    set
  ;;

  let make_new_grid original_grid removed_list =
    let removed_set = make_removed_set removed_list in
    original_grid
    |> List.iter
         (List.iter (fun tile ->
            if Hashtbl.mem removed_set (tile.row, tile.col)
            then tile.square <- Empty
            else ()));
    original_grid
  ;;

  let rec recursive_helper acc input =
    let removed =
      input
      |> List.map
           (List.filter_map (fun tile ->
              match tile.square with
              | PaperRoll ->
                let r, c = tile.row, tile.col in
                let nc = get_neighbors_count input tile in
                if nc >= 4 then None else Some (r, c, nc)
              | Empty -> None))
      |> List.flatten
    in
    match List.length removed with
    | 0 -> acc
    | x -> recursive_helper (acc + x) (make_new_grid input removed)
  ;;

  let solver input = recursive_helper 0 input |> string_of_int
end

include Make (M)
