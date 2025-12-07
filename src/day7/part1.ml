open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = Parser.t

  let label = "Day 7, Part 1"
  let input = "./input/day7/full.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> Parser.t_of_line_list
  ;;

  let solver input =
    let open TachyonState in
    if List.length input = 0 then failwith "not enough rows in input";
    let head = List.hd input in
    let rest = List.tl input in
    let state = { current_row = head; split_count = 0 } in
    rest |> List.fold_left folder state |> (fun acc -> acc.split_count) |> string_of_int
  ;;
end

include Make (M)
