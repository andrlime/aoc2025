open! Shared.Util

module RawGrid = struct
  type u =
    | PaperRoll
    | Empty

  let u_of_char = function
    | '@' -> PaperRoll
    | '.' -> Empty
    | _ -> failwith "invalid char"
  ;;

  let string_of_u = function
    | PaperRoll -> "@"
    | Empty -> "."
  ;;

  let is_paper = function
    | PaperRoll -> true
    | Empty -> false
  ;;
end

module GridSquare = struct
  type t =
    { row : int
    ; col : int
    ; mutable square : RawGrid.u
    }

  let t_of_char char row col = { row; col; square = RawGrid.u_of_char char }

  let t_of_list_of_row rowi row =
    row |> List.mapi (fun coli colstr -> t_of_char colstr rowi coli)
  ;;

  let t_list_of_rawgrid slist =
    slist
    |> List.mapi (fun rowi rowstr ->
      rowstr |> StringUtil.charlist_of_string |> t_of_list_of_row rowi)
  ;;

  let get_neighbors_count grid tile =
    let r, c = tile.row, tile.col in
    Neighbors.neighbors_2d
    |> List.filter_map (fun (dr, dc) ->
      try Some grid.(r + dr).(c + dc) with
      | _ -> None)
    |> List.fold_left
         (fun count tile -> count + (RawGrid.is_paper tile.square |> Bool.to_int))
         0
  ;;
end

include GridSquare
include RawGrid
