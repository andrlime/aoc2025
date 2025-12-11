open! Shared.Util

module Visitability = struct
  type t =
    { graph : (string, int) WeightedGraph.t
    ; visitable : (string, (string, unit) Hashtbl.t) Hashtbl.t
    }

  let get_neighbors g node =
    let adjacency_list = WeightedGraph.(g.edges) in
    match Hashtbl.find_opt adjacency_list node with
    | None -> []
    | Some n -> n
  ;;

  let rec traverse g cur ~visited =
    let neighbors = get_neighbors g cur in
    neighbors
    |> List.iter (fun (n, _) ->
      if not (Hashtbl.mem visited n)
      then (
        Hashtbl.replace visited n ();
        traverse g n ~visited))
  ;;

  let mk g =
    let open WeightedGraph in
    (* Algorithm: for each v in g, dfs to all possible neighbors, and store into visitable *)
    let newt = { graph = g; visitable = Hashtbl.create 100 } in
    let allv = g.vertices |> Hashtbl.to_seq |> List.of_seq in
    allv
    |> List.iter (fun (v, _) ->
      let reachable = Hashtbl.create 100 in
      Hashtbl.replace reachable v ();
      traverse g v ~visited:reachable |> ignore;
      Hashtbl.replace newt.visitable v reachable);
    newt
  ;;
end

module PathCounterSolver = struct
  module StringSet = Set.Make (String)

  type t =
    { graph : (string, int) WeightedGraph.t
    ; mutable counter : int
    }

  let mk () = { graph = WeightedGraph.create (); counter = 0 }

  let create_graph lines =
    let g = mk () in
    let add_edge v = WeightedGraph.add_unweighted_edge_directed g.graph v in
    lines
    |> List.iter (fun l ->
      let segments = l |> String.split_on_char ':' in
      let v = List.hd segments in
      let edges = List.nth segments 1 |> String.trim |> String.split_on_char ' ' in
      edges |> List.iter (add_edge v));
    g
  ;;

  let get_neighbors t node =
    let adjacency_list = WeightedGraph.(t.graph.edges) in
    match Hashtbl.find_opt adjacency_list node with
    | None -> []
    | Some n -> n
  ;;

  let rec dfs_graph t cur dst ~order =
    if cur = dst
    then t.counter <- t.counter + 1
    else (
      let neighbors = get_neighbors t cur in
      neighbors |> List.iter (fun (n, _) -> dfs_graph t n dst ~order:(n :: order)))
  ;;

  let dfs_graph_and_visit t src dst =
    let v = Visitability.mk t.graph in
    let has_visited node ~path = StringSet.mem node path in
    let can_still_visit node ~from:n =
      let visitable_from_n = Hashtbl.find Visitability.(v.visitable) n in
      Hashtbl.mem visitable_from_n node
    in
    let rec loop cur ~order ~path =
      if cur = dst
      then t.counter <- t.counter + 1
      else (
        let neighbors = get_neighbors t cur in
        neighbors
        |> List.iter (fun (n, _) ->
          let notcycle = not (has_visited n ~path) in
          let canreach = can_still_visit dst ~from:n in
          if notcycle && canreach
          then loop n ~order:(n :: order) ~path:(StringSet.add n path)))
    in
    loop src ~order:[ src ] ~path:(StringSet.add src StringSet.empty);
    let rval = t.counter in
    t.counter <- 0;
    rval
  ;;
end
