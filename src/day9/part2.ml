open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = SimpleSolver.t

  let label = "Day 9, Part 2"
  let input = "./input/day9/full.txt"
  let parse_input input = input |> SimpleSolver.t_of_input_string

  let solver input =
    Gc.set { (Gc.get ()) with minor_heap_size = 128 * 1024 * 1024; space_overhead = 500 };
    input
    |> RedGreenSolver.create
    |> RedGreenSolver.add_all_edges
    |> RedGreenSolver.flood_fill ~start:(0, 0)
    |> RedGreenSolver.binarise_grid
    |> RedGreenSolver.build_prefix_sum
    |> RedGreenSolver.find_largest_area
    |> string_of_int
  ;;
end

include Make (M)
