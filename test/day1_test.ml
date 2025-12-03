open Common

let%expect_test "day1 part1" =
  get_solution_string "1a" |> print_endline;
  [%expect {| 1105 |}]
;;

let%expect_test "day1 part2" =
  get_solution_string "1b" |> print_endline;
  [%expect {| 6599 |}]
;;
