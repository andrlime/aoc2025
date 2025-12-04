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
  | _ -> failwith (Printf.sprintf "solution %s not found" key)
;;
