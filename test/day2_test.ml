open Advent.Registry

let%expect_test "day2 part1" =
  let module Soln = (val get_solution "2a") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 18952700150 |}]
;;

let%expect_test "day2 part2" =
  let module Soln = (val get_solution "2b") in
  Soln.get_soln_string () |> print_endline;
  [%expect {| 28858486244 |}]
;;
