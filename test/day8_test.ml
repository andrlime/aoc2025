open Common

let%expect_test "day8 part1" =
  get_solution_string "8a" |> print_endline;
  [%expect {| 330786 |}]
;;

let%expect_test "day8 part2" =
  get_solution_string "8b" |> print_endline;
  [%expect {| 3276581616 |}]
;;
