open Common

let%expect_test "day6 part1" =
  get_solution_string "6a" |> print_endline;
  [%expect {| 4387670995909 |}]
;;

let%expect_test "day6 part2" =
  get_solution_string "6b" |> print_endline;
  [%expect {| 9625320374409 |}]
;;
