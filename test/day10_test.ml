open Common

let%expect_test "day10 part1" =
  get_solution_string "10a" |> print_endline;
  [%expect {| 488 |}]
;;

let%expect_test "day10 part2" =
  get_solution_string "10b" |> print_endline;
  [%expect {| 18771 |}]
;;
