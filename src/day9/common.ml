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

module Solver = struct
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
