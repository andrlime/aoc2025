open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = PathCounterSolver.t

  let label = "Day 11, Part 2"
  let input = "./input/day11/full.txt"

  let parse_input input =
    input
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> PathCounterSolver.create_graph
  ;;

  let solver input =
    let open PathCounterSolver in
    let p1 = dfs_graph_and_visit input "svr" "fft" in
    let p2 = dfs_graph_and_visit input "fft" "dac" in
    let p3 = dfs_graph_and_visit input "dac" "out" in
    p1 * p2 * p3 |> string_of_int
  ;;
end

include Make (M)
