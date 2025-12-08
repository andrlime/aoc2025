open! Shared.Exception
open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = Solver.t

  let label = "Day 8, Part 2"
  let input = "./input/day8/full.txt"
  let parse_input input = input |> Solver.t_of_input_string

  let solver input =
    let graph = Solver.create_graph input in
    let sorted_edges = Solver.get_all_graph_edges graph |> Solver.sort_edges in
    let u = UnionFind.create (List.length input) in
    let result =
      try
        sorted_edges
        |> List.iter (fun ((v1, v2), _) ->
          UnionFind.union u v1 v2;
          if u.components = 1
          then
            let open Circuit in
            raise (Result (get_x v1 * get_x v2)));
        failwith "could not connect all components"
      with
      | Result d -> d
    in
    result |> string_of_int
  ;;
end

include Make (M)
