open! Shared.Util
module GridPattern = struct end

module Shape = struct
  type t =
    { pattern : char array array
    ; width : int
    ; height : int
    ; filledcount : int
    }

  let t_of_input lines =
    (* ### \n ##. \n ##. *)
    let pattern =
      lines
      |> List.map StringUtil.charlist_of_string
      |> List.map Array.of_list
      |> Array.of_list
    in
    let filledcount =
      lines
      |> List.map StringUtil.charlist_of_string
      |> List.flatten
      |> List.fold_left (fun acc char -> if char = '#' then acc + 1 else acc) 0
    in
    { pattern
    ; height = List.length lines
    ; width = String.length (List.hd lines)
    ; filledcount
    }
  ;;
end

module Puzzle = struct
  type t =
    { width : int
    ; height : int
    ; shapecounts : int array
    ; raw : string
    }

  let t_of_input line =
    let segments = String.split_on_char ':' line in
    let dims =
      List.hd segments
      |> String.split_on_char 'x'
      |> List.map int_of_string
      |> Array.of_list
    in
    let nums =
      List.nth segments 1
      |> String.trim
      |> String.split_on_char ' '
      |> List.map int_of_string
      |> Array.of_list
    in
    { width = dims.(0); height = dims.(1); shapecounts = nums; raw = line }
  ;;
end

module FillShapesSolver = struct
  type t = Shape.t array * Puzzle.t list

  let t_of_input raw =
    let allshapes = ref [] in
    let allpuzzles = ref [] in
    let process_acc slist =
      match List.length slist with
      | 0 -> ()
      | 4 -> allshapes := Shape.t_of_input (List.tl slist) :: !allshapes
      | _ ->
        allpuzzles
        := slist |> ListUtil.remove_lines_shorter_than 1 |> List.map Puzzle.t_of_input
    in
    String.split_on_char '\n' raw
    |> List.fold_left
         (fun acc line ->
            if String.length line < 1
            then (
              process_acc (acc |> List.rev);
              [])
            else line :: acc)
         []
    |> process_acc;
    !allshapes |> List.rev |> Array.of_list, !allpuzzles
  ;;

  let count_solvable_puzzles (shapes, puzzles) =
    let open Puzzle in
    let open Shape in
    let can_solve_puzzle index p =
      let total_tile_cost =
        p.shapecounts
        |> Array.mapi (fun index count ->
          let shape = shapes.(index) in
          shape.filledcount * count)
        |> Array.fold_left ( + ) 0
      in
      let total_shape_count = p.shapecounts |> Array.fold_left ( + ) 0 in
      let total_number_tiles = p.width * p.height in
      let all_tiles_guaranteed_fit = p.width / 3 * (p.height / 3) >= total_shape_count in
      if all_tiles_guaranteed_fit
      then true
      else if total_tile_cost > total_number_tiles
      then false
      else (
        Printf.printf "WARN: Puzzle %d unsolvable using naive techniques!\n" index;
        false)
    in
    puzzles
    |> List.mapi can_solve_puzzle
    |> List.map (fun b -> if b then 1 else 0)
    |> List.fold_left ( + ) 0
  ;;
end
