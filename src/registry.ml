let get_solution key : (module Shared.Solver.T) =
  match key with
  | "1a" -> (module Day1.Part1)
  | "1b" -> (module Day1.Part2)
  | "2a" -> (module Day2.Part1)
  | "2b" -> (module Day2.Part2)
  | "3a" -> (module Day3.Part1)
  | "3b" -> (module Day3.Part2)
  | "4a" -> (module Day4.Part1)
  | "4b" -> (module Day4.Part2)
  | "5a" -> (module Day5.Part1)
  | "5b" -> (module Day5.Part2)
  | "6a" -> (module Day6.Part1)
  | "6b" -> (module Day6.Part2)
  | "7a" -> (module Day7.Part1)
  | "7b" -> (module Day7.Part2)
  | "8a" -> (module Day8.Part1)
  | "8b" -> (module Day8.Part2)
  | "9a" -> (module Day9.Part1)
  | "9b" -> (module Day9.Part2)
  | "10a" ->
    (module Day10.Part1)
    (* | "10b" -> (module Day10.Part2) *)

    (* | "11a" -> (module Day11.Part1) *)
    (* | "11b" -> (module Day11.Part2) *)

    (* | "12a" -> (module Day12.Part1) *)
    (* | "12b" -> (module Day12.Part2) *)
  | "template" -> (module Template.Part1)
  | _ -> failwith (Printf.sprintf "solution %s not found" key)
;;
