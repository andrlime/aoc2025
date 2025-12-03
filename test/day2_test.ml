open Common

let%expect_test "day2 part1" =
  get_solution_string "2a" |> print_endline;
  [%expect {| 18952700150 |}]
;;

let%expect_test "day2 part2" =
  get_solution_string "2b" |> print_endline;
  [%expect {| 28858486244 |}]
;;
