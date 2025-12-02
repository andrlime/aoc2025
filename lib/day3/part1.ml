open! Shared.Util
open! Shared.Solver
open! Common

let solver input =
  input |> ignore;
  "1"
;;

let solution = { label = "Day 3, Part 1"; inputfile = "./input/day1/full.txt"; solver }
