open! Shared.Util
open! Shared.Solver
open! Common

let solver input =
  input
  |> ignore;
  "A"
;;

let solution = { label = "Day 4, Part 1"; inputfile = "./input/day4/full.txt"; solver }
