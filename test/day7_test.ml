open Common

let%expect_test "day7 part1" =
  get_solution_string "7a" |> print_endline;
  [%expect {| 1535 |}]
;;

let%expect_test "day7 part2" =
  get_solution_string "7b" |> print_endline;
  [%expect {| 4404709551015 |}]
;;
