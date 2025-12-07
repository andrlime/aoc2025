open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = string

  let label = "Day 8, Part 1"
  let input = "./input/helloworld.txt"
  let parse_input input = input
  let solver input = input
end

include Make (M)
