open Common

let%expect_test "day11 part1" =
  get_solution_string "11a" |> print_endline;
  [%expect {| 772 |}]
;;

let%expect_test "day11 part2" =
  get_solution_string "11b" |> print_endline;
  [%expect {| 423227545768872 |}]
;;
