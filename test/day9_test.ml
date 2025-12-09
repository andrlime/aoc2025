open Common

let%expect_test "day9 part1" =
  get_solution_string "9a" |> print_endline;
  [%expect {| 4758121828 |}]
;;

(* let%expect_test "day9 part2" =
  get_solution_string "9b" |> print_endline;
  [%expect {| |}]
;; *)
