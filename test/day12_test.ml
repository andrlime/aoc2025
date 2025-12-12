open Common

let%expect_test "day12 part1" =
  get_solution_string "12a" |> print_endline;
  [%expect {| 548 |}]
;;
