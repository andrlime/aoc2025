open Common

let%expect_test "day5 part1" =
  get_solution_string "5a" |> print_endline;
  [%expect {| 712 |}]
;;

let%expect_test "day5 part2" =
  get_solution_string "5b" |> print_endline;
  [%expect {| 332998283036769 |}]
;;
