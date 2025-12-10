open! Shared.Util

module Coordinate = struct
  type t = Physics.quantity

  let t_of_string s =
    let parts = s |> String.split_on_char ',' |> Array.of_list in
    if Array.length parts <> 2
    then None
    else (
      let x = parts.(0) |> int_of_string in
      let y = parts.(1) |> int_of_string in
      Some (Physics.Vector2 { x; y }))
  ;;

  let print = function
    | Physics.Vector2 { x; y } -> Printf.printf "Coordinate at (%d, %d)\n" x y
    | _ -> failwith "unsupported type"
  ;;

  let to_string = function
    | Physics.Vector2 { x; y } -> Printf.sprintf "(%d, %d)" x y
    | _ -> failwith "unsupported type"
  ;;

  let get_x_y = function
    | Physics.Vector2 { x; y } -> x, y
    | _ -> failwith "unsupported type"
  ;;

  let compare c1 c2 =
    let x1, y1 = get_x_y c1 in
    let x2, y2 = get_x_y c2 in
    if x1 = x2 then compare y1 y2 else compare x1 x2
  ;;

  let compute_area c1 c2 =
    let x1, y1 = get_x_y c1 in
    let x2, y2 = get_x_y c2 in
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
  ;;
end

module SimpleSolver = struct
  type t = Coordinate.t list

  let t_of_input_string s =
    s
    |> String.split_on_char '\n'
    |> ListUtil.remove_lines_shorter_than 1
    |> List.filter_map Coordinate.t_of_string
  ;;

  let sort t =
    (* From top left to bottom right *)
    List.stable_sort Coordinate.compare t
  ;;

  let get_maximum_area t =
    ListUtil.cross t t
    |> List.fold_left
         (fun acc (c1, c2) ->
            let newarea = Coordinate.compute_area c1 c2 in
            if newarea > acc then newarea else acc)
         0
  ;;
end

module RedGreenSolver = struct
  type tile =
    | VisitedUnfilled
    | UnvisitedUnfilled
    | Filled

  type t =
    { mutable grid : tile array array
    ; coordinates : SimpleSolver.t
    ; compressed_x_mapping : (int, int) Hashtbl.t
    ; compressed_y_mapping : (int, int) Hashtbl.t
    ; mutable x_nextkey : int
    ; mutable y_nextkey : int
    ; size : int
    }

  let create coordinates =
    (* Invariant:
      Input has at most 500 entries, at most 250 unique x and 250 unique y after deduping.
      To account for holes between entries, each x is separated from the next by a hole of size 1
      Thus, the array is 500x500
    *)
    let size = 500 in
    { grid = Array.make_matrix size size UnvisitedUnfilled
    ; coordinates
    ; compressed_x_mapping = Hashtbl.create size
    ; compressed_y_mapping = Hashtbl.create size
    ; x_nextkey = 1
    ; y_nextkey = 1
    ; size
    }
  ;;

  let get_compressed_x_y t c =
    let rawx, rawy = Coordinate.get_x_y c in
    let compressed_x = Hashtbl.find t.compressed_x_mapping rawx in
    let compressed_y = Hashtbl.find t.compressed_y_mapping rawy in
    compressed_x, compressed_y
  ;;

  let create_x_hashset t =
    t.coordinates
    |> List.map (fun c ->
      let x, _ = Coordinate.get_x_y c in
      x)
    |> List.sort Int.compare
    |> List.iter (fun x0 ->
      let curmapping = Hashtbl.find_opt t.compressed_x_mapping x0 in
      match curmapping with
      | Some _ -> ()
      | None ->
        let key = t.x_nextkey in
        t.x_nextkey <- key + 2;
        Hashtbl.add t.compressed_x_mapping x0 key)
  ;;

  let create_y_hashset t =
    t.coordinates
    |> List.map (fun c ->
      let _, y = Coordinate.get_x_y c in
      y)
    |> List.sort Int.compare
    |> List.iter (fun y0 ->
      let curmapping = Hashtbl.find_opt t.compressed_y_mapping y0 in
      match curmapping with
      | Some _ -> ()
      | None ->
        let key = t.y_nextkey in
        t.y_nextkey <- key + 2;
        Hashtbl.add t.compressed_y_mapping y0 key)
  ;;

  let add_single_edge t prev cur =
    let xprev, yprev = get_compressed_x_y t prev in
    let xcur, ycur = get_compressed_x_y t cur in
    let xmin, xmax = if xprev < xcur then xprev, xcur else xcur, xprev in
    let ymin, ymax = if yprev < ycur then yprev, ycur else ycur, yprev in
    let xrange = ListUtil.range xmin xmax in
    let yrange = ListUtil.range ymin ymax in
    ListUtil.cross xrange yrange |> List.iter (fun (x, y) -> t.grid.(x).(y) <- Filled)
  ;;

  let add_all_edges t =
    create_x_hashset t;
    create_y_hashset t;
    let firstnode = List.hd t.coordinates in
    t.coordinates
    |> List.fold_left
         (fun prevnode curnode ->
            add_single_edge t prevnode curnode;
            curnode)
         firstnode
    |> fun lastnode ->
    add_single_edge t lastnode firstnode;
    t
  ;;

  let is_horizontal_edge_valid t y (x1, x2) =
    let range = ListUtil.range x1 x2 in
    range |> List.fold_left (fun result xcur -> result && t.grid.(xcur).(y) = Filled) true
  ;;

  let is_vertical_edge_valid t x (y1, y2) =
    let range = ListUtil.range y1 y2 in
    range |> List.fold_left (fun result ycur -> result && t.grid.(x).(ycur) = Filled) true
  ;;

  let compute_area t c1 c2 =
    let x1, y1 = get_compressed_x_y t c1 in
    let x2, y2 = get_compressed_x_y t c2 in
    let xmin, xmax = if x1 < x2 then x1, x2 else x2, x1 in
    let ymin, ymax = if y1 < y2 then y1, y2 else y2, y1 in
    let top = is_horizontal_edge_valid t ymin (xmin, xmax) in
    let bottom = is_horizontal_edge_valid t ymax (xmin, xmax) in
    let left = is_vertical_edge_valid t xmin (ymin, ymax) in
    let right = is_vertical_edge_valid t xmax (ymin, ymax) in
    let valid = top && bottom && left && right in
    if valid then Some (Coordinate.compute_area c1 c2) else None
  ;;

  let rec flood_fill ~start t =
    let x0, y0 = start in
    let empty_neighbors =
      Neighbors.neighbors_2d
      |> List.filter_map (fun (x, y) ->
        let x1, y1 = x + x0, y + y0 in
        if x1 < 0 || y1 < 0
        then None
        else if x1 >= t.size || y1 >= t.size
        then None
        else if t.grid.(x1).(y1) != UnvisitedUnfilled
        then None
        else Some (x1, y1))
    in
    empty_neighbors
    |> List.iter (fun (xn, yn) ->
      t.grid.(xn).(yn) <- VisitedUnfilled;
      flood_fill ~start:(xn, yn) t |> ignore);
    t
  ;;

  let binarise_grid t =
    let newgrid =
      t.grid
      |> ArrayUtil.map2d (function
        | VisitedUnfilled -> VisitedUnfilled
        | _ -> Filled)
    in
    t.grid <- newgrid;
    t
  ;;

  let find_largest_area t =
    ListUtil.cross t.coordinates t.coordinates
    |> List.fold_left
         (fun acc (c1, c2) ->
            let newarea = compute_area t c1 c2 in
            match newarea with
            | None -> acc
            | Some a -> if a > acc then a else acc)
         0
  ;;
end
