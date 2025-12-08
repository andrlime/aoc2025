open! Shared.Exception
open! Shared.Util
open! Shared.Solver
open! Common

module M : Solution = struct
  type t = Solver.t

  let label = "Day 8, Part 1"
  let input = "./input/day8/full.txt"

  let max_iterations =
    match input with
    | "./input/day8/short.txt" -> 10
    | "./input/day8/full.txt" -> 1000
    | _ -> failwith "unknown input"
  ;;

  let parse_input input = input |> Solver.t_of_input_string

  let solver input =
    let graph = Solver.create_graph input in
    let sorted_edges = Solver.get_all_graph_edges graph |> Solver.sort_edges in
    let u = UnionFind.create (List.length sorted_edges) in
    let counter = ref 0 in
    let _ =
      try
        sorted_edges
        |> List.iter (fun ((v1, v2), _) ->
          UnionFind.union u v1 v2;
          counter := !counter + 1;
          if !counter = max_iterations then raise (Break "reached max iterations"));
        raise (Break "reached end")
      with
      | Break _ -> ()
    in
    (* Connect 1000 edges *)
    (* Get three largest circuits from the union find and multiply them *)
    u |> UnionFind.get_product_of_3_greatest_root_sizes |> string_of_int
  ;;
end

include Make (M)
