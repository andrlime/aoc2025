open! Shared.Util

module Circuit = struct
  (* Physics.Vector3 *)
  type t = Physics.quantity

  let get_x t =
    match t with
    | Physics.Vector3 { x; _ } -> x
    | _ -> failwith "unsupported type"
  ;;

  let t_of_string str =
    let coordinates = str |> String.split_on_char ',' in
    if List.length coordinates <> 3
    then None
    else (
      let coordinate_numbers = coordinates |> List.map int_of_string |> Array.of_list in
      Some
        (Physics.Vector3
           { x = coordinate_numbers.(0)
           ; y = coordinate_numbers.(1)
           ; z = coordinate_numbers.(2)
           }))
  ;;

  let print_circuit c =
    match c with
    | Physics.Vector3 { x; y; z } -> Printf.printf "Circuit at (%d, %d, %d)\n" x y z
    | _ -> failwith "unsupported type"
  ;;

  let to_string c =
    match c with
    | Physics.Vector3 { x; y; z } -> Printf.sprintf "(%d, %d, %d)" x y z
    | _ -> failwith "unsupported type"
  ;;

  let ( = ) c1 c2 =
    match c1, c2 with
    | ( Physics.Vector3 { x = x1; y = y1; z = z1 }
      , Physics.Vector3 { x = x2; y = y2; z = z2 } ) -> x1 = x2 && y1 = y2 && z1 = z2
    | _ -> failwith "unsupported type"
  ;;

  let get_min_max c1 c2 =
    match c1, c2 with
    | ( Physics.Vector3 { x = x1; y = y1; z = z1 }
      , Physics.Vector3 { x = x2; y = y2; z = z2 } ) ->
      if x1 + y1 + z1 < x2 + y2 + z2 then c1, c2 else c2, c1
    | _ -> failwith "unsupported type"
  ;;
end

module Solver = struct
  type t = Circuit.t list

  let t_of_input_string s =
    s
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> List.filter_map Circuit.t_of_string
  ;;

  let create_graph t =
    let g = WeightedGraph.create () in
    ListUtil.cross t t
    |> List.iter (fun (c1, c2) ->
      let d = Physics.distance_euclidean c1 c2 in
      if d > 0.
      then (
        let fromv, tov = Circuit.get_min_max c1 c2 in
        WeightedGraph.add_edge_directed g fromv tov d));
    g
  ;;

  let get_product_of_3_greatest_root_sizes u =
    let open UnionFind in
    let table = Hashtbl.create 100 in
    u.roots
    |> Array.iteri (fun node _ ->
      let root = find_wrapper u node in
      let curbinding =
        match Hashtbl.find_opt table root with
        | None -> 0
        | Some x -> x
      in
      Hashtbl.replace table root (curbinding + 1));
    table
    |> Hashtbl.to_seq
    |> List.of_seq
    |> List.map (fun (_, size) -> size)
    |> List.stable_sort Int.compare
    |> List.rev
    |> (fun list -> List.nth list 0, List.nth list 1, List.nth list 2)
    |> fun (d1, d2, d3) -> d1 * d2 * d3
  ;;

  let get_all_graph_edges g =
    let open WeightedGraph in
    g.all_edges |> Hashtbl.to_seq |> List.of_seq
  ;;

  let sort_edges e = e |> List.stable_sort WeightedGraph.edge_compare
end
