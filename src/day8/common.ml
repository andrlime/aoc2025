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

module WeightedGraph = struct
  (* a = node type (e.g. a coordinate), b = weight type (e.g. float) *)
  type ('a, 'b) t =
    { edges : ('a, ('a * 'b) list) Hashtbl.t
    ; vertices : ('a, unit) Hashtbl.t
    ; all_edges : ('a * 'a, 'b) Hashtbl.t
    }

  let create () =
    { edges = Hashtbl.create 100
    ; vertices = Hashtbl.create 100
    ; all_edges = Hashtbl.create 100
    }
  ;;

  let add_vertex g v = Hashtbl.add g.vertices v ()

  let add_edge_directed g v1 v2 w =
    let fromv, tov = Circuit.get_min_max v1 v2 in
    if Hashtbl.mem g.all_edges (fromv, tov)
    then ()
    else (
      let cur_edges =
        match Hashtbl.find_opt g.edges fromv with
        | Some list -> list
        | None -> []
      in
      Hashtbl.add g.edges fromv ((tov, w) :: cur_edges);
      Hashtbl.add g.all_edges (fromv, tov) w)
  ;;

  let add_edge g v1 v2 w =
    add_vertex g v1;
    add_vertex g v2;
    add_edge_directed g v1 v2 w;
    add_edge_directed g v2 v1 w
  ;;

  let edge_compare ((v1a, v1b), w1) ((v2a, v2b), w2) =
    if w1 = w2
    then if v1a = v2a then compare v1b v2b else compare v1a v2a
    else compare w1 w2
  ;;
end

module UnionFind = struct
  type 'a t =
    { vertices : ('a, int) Hashtbl.t
    ; roots : int array
    ; mutable nextkey : int
    ; mutable components : int
    }

  let create size =
    let roots = Array.make size 0 in
    roots |> Array.iteri (fun index _ -> roots.(index) <- index);
    { vertices = Hashtbl.create size; roots; nextkey = 0; components = size }
  ;;

  let rec find_wrapper u key =
    let root = u.roots.(key) in
    if root = key then key else find_wrapper u root
  ;;

  let find u a1 =
    let key = Hashtbl.find_opt u.vertices a1 in
    match key with
    | None ->
      let key = u.nextkey in
      Hashtbl.add u.vertices a1 key;
      let res = find_wrapper u key in
      u.nextkey <- key + 1;
      res
    | Some k -> find_wrapper u k
  ;;

  let union u a1 a2 =
    let root1 = find u a1 in
    let root2 = find u a2 in
    if root1 <> root2
    then (
      u.roots.(root1) <- root2;
      u.components <- u.components - 1)
  ;;

  let print_roots u =
    u.roots
    |> Array.iteri (fun node _ ->
      Printf.printf "node %d has root %d\n" node (find_wrapper u node))
  ;;

  let get_product_of_3_greatest_root_sizes u =
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
      if d > 0. then WeightedGraph.add_edge_directed g c1 c2 d);
    g
  ;;

  let get_all_graph_edges g =
    let open WeightedGraph in
    g.all_edges |> Hashtbl.to_seq |> List.of_seq
  ;;

  let sort_edges e = e |> List.stable_sort WeightedGraph.edge_compare
end
