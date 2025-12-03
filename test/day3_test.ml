open Common

let%expect_test "day3 part1" =
  get_solution_string "3a" |> print_endline;
  [%expect {| 17332 |}]
;;

let%expect_test "day3 part2" =
  get_solution_string "3b" |> print_endline;
  [%expect {| 172516781546707 |}]
;;
