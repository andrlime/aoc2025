open Common

let%expect_test "day4 part1" =
  get_solution_string "4a" |> print_endline;
  [%expect {| 1437 |}]
;;

let%expect_test "day4 part2" =
  get_solution_string "4b" |> print_endline;
  [%expect {| 8765 |}]
;;
