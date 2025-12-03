open Advent.Registry

let%expect_test "day3 part1" =
  let module Soln = (val get_solution "3a") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 17332 |}]
;;

let%expect_test "day3 part2" =
  let module Soln = (val get_solution "3b") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 172516781546707 |}]
;;
