module Io = struct
  let concat_with_project_root =
    try Filename.concat (Sys.getenv "PROJECT_ROOT") with
    | Not_found -> Filename.concat "."
  ;;

  let read_file path =
    let channel = path |> concat_with_project_root |> open_in in
    let len = in_channel_length channel in
    let content = really_input_string channel len in
    close_in channel;
    content
  ;;

  let split_string_into_lines str = str |> String.split_on_char '\n'
end

module ArrayUtil = struct
  let iter2d f grid = grid |> Array.iter (Array.iter f)
  let map2d f grid = grid |> Array.map (Array.map f)
end

module StringUtil = struct
  let slice str at =
    let firsthalf = String.sub str 0 at in
    let lasthalf = String.sub str at (String.length str - at) in
    firsthalf, lasthalf
  ;;

  let slice_in_half str =
    let halflength = String.length str / 2 in
    slice str halflength
  ;;

  let charlist_of_string str =
    let rec helper index accumulator =
      if index < 0
      then accumulator
      else (
        let curchar = String.get str index in
        helper (index - 1) (curchar :: accumulator))
    in
    helper (String.length str - 1) []
  ;;
end

module ListUtil = struct
  let cross l1 l2 =
    l1
    |> List.fold_left
         (fun acc1 c1 ->
            (l2 |> List.fold_left (fun acc2 c2 -> (c1, c2) :: acc2) []) @ acc1)
         []
  ;;

  let remove_lines_shorter_than length list =
    list |> List.filter_map (fun l -> if String.length l < length then None else Some l)
  ;;

  let rec uniform_zip lists =
    if lists |> List.exists (fun l -> l = [])
    then []
    else (
      let heads = lists |> List.map List.hd in
      let tails = lists |> List.map List.tl in
      heads :: uniform_zip tails)
  ;;

  let rec zip l1 l2 =
    if List.length l1 <> List.length l2
    then failwith "cannot zip two lists of unequal length"
    else (
      match l1, l2 with
      | f1 :: r1, f2 :: r2 -> (f1, f2) :: zip r1 r2
      | _ -> [])
  ;;

  let range lower upper =
    let rec f cur lowerbound lst =
      if cur < lowerbound then lst else f (cur - 1) lowerbound (cur :: lst)
    in
    f upper lower []
  ;;

  let all_elems_in_list_equal list =
    let rec all_equal list thing =
      match list with
      | first :: rst -> first = thing && all_equal rst thing
      | [] -> true
    in
    if List.length list = 0 then true else all_equal list (List.nth list 0)
  ;;

  let rec intmax list =
    match list with
    | first :: [] -> first
    | first :: rest -> max first (intmax rest)
    | [] -> failwith "cannot take max of empty list"
  ;;

  let intsum list = list |> List.fold_left (fun acc cur -> acc + cur) 0
  let iter2d f grid = grid |> List.iter (List.iter f)
  let map2d f grid = grid |> List.map (List.map f)
end

module Neighbors = struct
  let neighbors_2d = [ 1, 1; 1, 0; 1, -1; 0, 1; 0, -1; -1, -1; -1, 0; -1, 1 ]
end

(* TODO: Maybe extend this to floats using a functor *)
module Physics = struct
  type quantity =
    | Scalar of int
    | Vector2 of
        { x : int
        ; y : int
        }
    | Vector3 of
        { x : int
        ; y : int
        ; z : int
        }
    | VectorN of int list

  let dot q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> x * y
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } -> (x1 * x2) + (y1 * y2)
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      (x1 * x2) + (y1 * y2) + (z1 * z2)
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2 |> List.fold_left (fun acc (x, y) -> acc + (x * y)) 0
    | _ -> failwith "cannot dot two incompatible types"
  ;;

  let distance_euclidean q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> Float.of_int (abs x - y)
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      Float.sqrt ((Float.of_int dx ** 2.) +. (Float.of_int dy ** 2.))
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      Float.sqrt
        ((Float.of_int dx ** 2.) +. (Float.of_int dy ** 2.) +. (Float.of_int dz ** 2.))
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2
      |> List.fold_left (fun acc (x, y) -> acc +. (Float.of_int (x - y) ** 2.)) 0.0
      |> Float.sqrt
    | _ -> failwith "cannot compute euclidean distance between two incompatible types"
  ;;

  let distance_euclidean_squared q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> abs x - y
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      (dx * dx) + (dy * dy)
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      (dx * dx) + (dy * dy) + (dz * dz)
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2
      |> List.fold_left (fun acc (x, y) -> acc + ((x - y) * (x - y))) 0
    | _ ->
      failwith "cannot compute squared euclidean distance between two incompatible types"
  ;;

  let distance_taxicab q1 q2 =
    match q1, q2 with
    | Scalar x, Scalar y -> abs x - y
    | Vector2 { x = x1; y = y1 }, Vector2 { x = x2; y = y2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      abs dx + abs dy
    | Vector3 { x = x1; y = y1; z = z1 }, Vector3 { x = x2; y = y2; z = z2 } ->
      let dx = x1 - x2 in
      let dy = y1 - y2 in
      let dz = z1 - z2 in
      abs dx + abs dy + abs dz
    | VectorN list1, VectorN list2 ->
      ListUtil.zip list1 list2 |> List.fold_left (fun acc (x, y) -> acc + abs (x - y)) 0
    | _ -> failwith "cannot compute euclidean distance between two incompatible types"
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

  let add_edge_directed g fromv tov w =
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

  let add_unweighted_edge_directed g fromv tov =
    add_vertex g fromv;
    add_vertex g tov;
    add_edge_directed g fromv tov 1
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
end
