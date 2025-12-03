open Advent.Registry

let%expect_test "day1 part1" =
  let module Soln = (val get_solution "1a") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 1105 |}]
;;

let%expect_test "day1 part2" =
  let module Soln = (val get_solution "1b") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 6599 |}]
;;
